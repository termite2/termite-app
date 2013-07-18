{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, RecordWildCards, UndecidableInstances, TupleSections, ImplicitParams #-}

module Main where

import qualified Data.Map as M
import Control.Monad.ST
import Control.Monad.Trans
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
import qualified ISpec    as I
import qualified TranSpec as I

data TOption = InputTSL String
             | ImportDir String
             | DoSynthesis

options :: [OptDescr TOption]
options = [ Option ['i'] [] (ReqArg InputTSL "FILE")       "input TSL file"
          , Option ['I'] [] (ReqArg ImportDir "DIRECTORY") "additional import lookup directory"
          , Option ['s'] [] (NoArg DoSynthesis)            "perform synthesis"]

data Config = Config { confTSLFile     :: FilePath
                     , confImportDirs  :: [FilePath]
                     , confDoSynthesis :: Bool}

defaultConfig = Config { confTSLFile     = ""
                       , confImportDirs  = []
                       , confDoSynthesis = False}

addOption :: TOption -> Config -> Config
addOption (InputTSL f)  config = config{confTSLFile = f}
addOption (ImportDir d) config = config{confImportDirs = (confImportDirs config) ++ [d]}
addOption DoSynthesis   config = config{confDoSynthesis = True}

main = do
    args <- getArgs
    prog <- getProgName
    config <- case getOpt Permute options args of
                   (flags, [], []) -> return $ foldr addOption defaultConfig flags
                   _ -> fail $ usageInfo ("Usage: " ++ prog ++ " [OPTION...]") options 
    spec <- parseTSL (confTSLFile config) (confImportDirs config)
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
    let solver = newSMTLib2Solver ispecFull z3Config
        ispecFull = let ?solver = solver in spec2Internal spec'
        ispecDummy = ispecFull {I.specTran = (I.specTran ispecFull) { I.tsCTran = []
                                                                    , I.tsUTran = []}}   
        ispec = if' (confDoSynthesis config) ispecFull ispecDummy
    writeFile "output3.tsl" $ P.render $ pp ispec
    (model, absvars, sfact) <- do (res, avars, model, strategy) <- synthesise spec spec' ispec solver
                                  putStrLn $ "Synthesis returned " ++ show res
                                  return (model, avars, [(strategyViewNew strategy, True)])
    putStrLn "starting debugger"
    let sourceViewFactory   = sourceViewNew spec spec' ispec absvars solver
    debugGUI ((sourceViewFactory, True):sfact) model

synthesise :: Spec -> Spec -> I.Spec -> SMTSolver -> IO (Bool, M.Map String AbsVar, Model DdManager DdNode Store, Strategy DdNode)
synthesise inspec flatspec spec solver = runScript $ do
    hoistEither $ runST $ runEitherT $ do
        m <- lift $ RefineCommon.setupManager 
        let agame = tslAbsGame spec m
        let ts = eqTheorySolver spec m 
        sr <- lift $ do (win, ri) <- absRefineLoop m agame ts ()
                        mkSynthesisRes spec m (win, ri)
        let model = mkModel inspec flatspec spec solver sr
            strategy = mkStrategy spec sr
--        lift $ cuddAutodynDisable m
        return (srWin sr, srAbsVars sr, model, strategy)
