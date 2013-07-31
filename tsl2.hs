{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, RecordWildCards, UndecidableInstances, TupleSections, ImplicitParams #-}

module Main where

import qualified Data.Map as M
import Data.Tuple.Select
import Data.Maybe
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad
import Control.Error
import System.Environment
import qualified Text.PrettyPrint as P
import System.Console.GetOpt

import Util
import SpecInline
import PP
import Parse
import Spec
import SpecOps
import DbgGUI
import DbgTypes
import Cudd 
import SMTLib2
import SourceView
import StrategyView
import AbstractorIFace
import RefineCommon
import TermiteGame
import TSLAbsGame
import EqSMT
import Store
import SMTSolver
import Predicate
import Resource (evalResourceT)
import qualified ISpec    as I
import qualified TranSpec as I

data TOption = InputTSL String
             | ImportDir String
             | DoSynthesis
             | QBFSynthesis
             | NoBuiltins
             | NoFairness
        
options :: [OptDescr TOption]
options = [ Option ['i'] []             (ReqArg InputTSL "FILE")       "input TSL file"
          , Option ['I'] []             (ReqArg ImportDir "DIRECTORY") "additional import lookup directory"
          , Option ['s'] []             (NoArg DoSynthesis)            "perform synthesis"
          , Option ['q'] []             (NoArg QBFSynthesis)           "run QBF-based synthesis after normal synthesis"
          , Option []    ["nobuiltins"] (NoArg NoBuiltins)             "do not include TSL2 builtins"
          , Option []    ["nofairness"] (NoArg NoFairness)             "do not generate fairness constraints"]

data Config = Config { confTSLFile      :: FilePath
                     , confImportDirs   :: [FilePath]
                     , confDoSynthesis  :: Bool
                     , confQBFSynthesis :: Bool
                     , confNoBuiltins   :: Bool
                     , confNoFairness   :: Bool}

defaultConfig = Config { confTSLFile      = ""
                       , confImportDirs   = []
                       , confDoSynthesis  = False
                       , confQBFSynthesis = False
                       , confNoBuiltins   = False
                       , confNoFairness   = False}

addOption :: TOption -> Config -> Config
addOption (InputTSL f)  config = config{ confTSLFile      = f}
addOption (ImportDir d) config = config{ confImportDirs   = (confImportDirs config) ++ [d]}
addOption DoSynthesis   config = config{ confDoSynthesis  = True}
addOption QBFSynthesis  config = config{ confDoSynthesis  = True
                                       , confQBFSynthesis = True}
addOption NoBuiltins    config = config{ confNoBuiltins   = True}
addOption NoFairness    config = config{ confNoFairness   = True}

main = do
    args <- getArgs
    prog <- getProgName
    config <- case getOpt Permute options args of
                   (flags, [], []) -> return $ foldr addOption defaultConfig flags
                   _ -> fail $ usageInfo ("Usage: " ++ prog ++ " [OPTION...]") options 
    spec <- parseTSL (confTSLFile config) (confImportDirs config) (not $ confNoBuiltins config)
    writeFile "output.tsl" $ P.render $ pp spec
    case validateSpec spec of
         Left e  -> fail $ "validation error: " ++ e
         Right _ -> putStrLn "validation successful"
    spec' <- case flatten spec of
                  Left e  -> fail $ "flattening error: " ++ e
                  Right s -> return s
    writeFile "output2.tsl" $ P.render $ pp spec'
    case validateSpec spec' of
         Left e  -> fail $ "flattened spec validation error: " ++ e
         Right _ -> putStrLn "flattened spec validation successful"
    let ispecFull = spec2Internal spec' (not $ confNoFairness config)
        ispecDummy = ispecFull {I.specTran = (I.specTran ispecFull) { I.tsCTran = []
                                                                    , I.tsUTran = []}}
        ispec = if' (confDoSynthesis config) ispecFull ispecDummy
        solver = newSMTLib2Solver ispecFull z3Config
    writeFile "output3.tsl" $ P.render $ pp ispec
    (model, absvars, sfact) <- do (res, avars, model, mstrategy) <- synthesise config spec spec' ispec solver (confDoSynthesis config)
                                  putStrLn $ "Synthesis returned " ++ show res
                                  return (model, avars, if' (isJust mstrategy) [(strategyViewNew $ fromJust mstrategy, True)] [])
    when (confQBFSynthesis config) $ qbfSynth $ map ((absvars M.!) . sel1) $ mStateVars model
    putStrLn "starting debugger"
    let sourceViewFactory   = sourceViewNew spec spec' ispec absvars solver
    debugGUI ((sourceViewFactory, True):(if' (confDoSynthesis config) sfact [])) model

synthesise :: Config -> Spec -> Spec -> I.Spec -> SMTSolver -> Bool -> IO (Maybe Bool, M.Map String AbsVar, Model DdManager DdNode Store, Maybe (Strategy DdNode))
synthesise config inspec flatspec spec solver dostrat = runScript $ do
    hoistEither $ runST $ evalResourceT $ runEitherT $ do
        m <- lift $ lift $ RefineCommon.setupManager 
        let agame = tslAbsGame spec m (not $ confNoFairness config)
        let ts = eqTheorySolver spec m 
        sr <- lift $ do (win, ri) <- absRefineLoop m agame ts ()
                        mkSynthesisRes spec m (if' dostrat (Just win) Nothing, ri)
        let model = mkModel inspec flatspec spec solver sr
            strategy = mkStrategy spec sr
--        lift $ cuddAutodynDisable m
        return (srWin sr, srAbsVars sr, model, strategy)

qbfSynth :: [AbsVar] -> IO ()
qbfSynth avs = do
    putStrLn "Running QBF synthesis"
    
--tslUpdateAbsVarAST :: (?spec::Spec, ?pred::[Predicate]) => (AbsVar, f) -> TAST f e c
