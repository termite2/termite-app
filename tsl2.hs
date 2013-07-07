{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, RecordWildCards, UndecidableInstances, TupleSections #-}

module Main where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Error
import System.Environment
import Text.Parsec
import qualified Text.PrettyPrint as P
import System.Console.GetOpt
import System.Directory

import SpecInline
import PP
import Name
import Parse
import Spec
import SpecOps
import DbgGUI
import DbgTypes
import Cudd 
import SMTLib2
import SourceView
import StrategyView
import LogicClasses
import AbstractorIFace
import RefineCommon
import TermiteGame
import TSLAbsGame
import EqSMT
import Store
import SMTSolver
import Predicate
import qualified ISpec as I

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
    mods <- parseTSL M.empty (confTSLFile config) (confImportDirs config)
    let spec = mkSpec $ concat $ snd $ unzip $ M.toList mods
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
    let ispec = spec2Internal spec'
    writeFile "output3.tsl" $ P.render $ pp ispec
    let solver = newSMTLib2Solver ispec z3Config
    (model, absvars, sfact) <- if not $ confDoSynthesis config
                                  then liftM (,M.empty, []) concreteModel 
                                  else do (res, avars, model, strategy) <- synthesise spec' ispec solver
                                          putStrLn $ "Synthesis returned " ++ show res
                                          return (model, avars, [(strategyViewNew strategy, True)])
    putStrLn "starting debugger"
    let sourceViewFactory   = sourceViewNew spec spec' ispec absvars solver
    debugGUI ((sourceViewFactory, True):sfact) model

synthesise :: Spec -> I.Spec -> SMTSolver -> IO (Bool, M.Map String AbsVar, Model DdManager DdNode Store, Strategy DdNode)
synthesise flatspec spec solver = runScript $ do
    hoistEither $ runST $ runEitherT $ do
        m <- lift $ RefineCommon.setupManager 
        let agame = tslAbsGame spec m
        let ts = eqTheorySolver spec m 
        sr <- lift $ do (win, ri) <- absRefineLoop m agame ts ()
                        mkSynthesisRes spec m (win, ri)
        let model = mkModel flatspec spec solver sr
            strategy = mkStrategy spec sr
--        lift $ cuddAutodynDisable m
        return (srWin sr, srAbsVars sr, model, strategy)

-- Debugger model without any abstract state (suitable for source-level debugging only)
concreteModel :: IO (Model DdManager DdNode Store)
concreteModel = do
    -- start debugger
    let ddmanager = cuddInit
    return Model { mCtx                  = ddmanager
                 , mStateVars            = []
                 , mUntrackedVars        = []
                 , mLabelVars            = []
                 , mStateRels            = []
                 , mTransRels            = [("trans", topOp ddmanager)]
                 , mViews                = []
                 , mConcretiseState      = (\_ -> Nothing)
                 , mConcretiseTransition = (\_ -> Nothing)
                 , mAutoConcretiseTrans  = False
                 , mConstraints          = M.empty
                 , mTransRel             = botOp ddmanager 
                 }

--    let game = tslAbsGame ispec
--    result <- stToIO $ doEverything game {-((debugGUI [])::(Model DdManager DdNode () -> IO ()))-} newPDBPriv (eqSolver ispec)
--    putStrLn $ "result: " ++ show result

mkSpec :: [SpecItem] -> Spec
mkSpec is = Spec templates types consts
    where templates = mapMaybe (\i -> case i of 
                                           SpTemplate t -> Just t
                                           _            -> Nothing) is
          types = mapMaybe (\i -> case i of 
                                           SpType t     -> Just t
                                           _            -> Nothing) is
          consts = mapMaybe (\i -> case i of 
                                           SpConst c    -> Just c
                                           _            -> Nothing) is

instance PP [SpecItem] where
    pp is = P.vcat $ map ((P.$+$ P.text "") . pp) is

-- Recursively parse TSL file and all of its imports
-- Takes a map of already parsed files and the name of the file
-- to parse
parseTSL :: M.Map FilePath [SpecItem] -> FilePath -> [FilePath] -> IO (M.Map FilePath [SpecItem])
parseTSL modules f dirs = do
    tsl <- readFile f
    --putStr tsl
    spec <- case parse grammar f tsl of
                 Left e -> fail $ show e
                 Right st -> return st
    writeFile (f ++ ".out") $ P.render $ pp spec
    -- Parse imports
    foldM (\mods imp -> do imp' <- findImport imp dirs
                           if M.member imp' mods
                              then return mods
                              else parseTSL mods imp' dirs)
          (M.insert f spec modules) (imports spec)

findImport :: FilePath -> [FilePath] -> IO String
findImport f dirs = do
    let dirs' = "":(map (++"/") dirs)
    match <- filterM (\d -> doesFileExist (d ++ f)) dirs'
    case match of
         [] -> fail $ "File not found: " ++ f
         _  -> return $ head match ++ f

-- Extract the list of imports from parsed TSL spec
imports :: [SpecItem] -> [FilePath]
imports s = mapMaybe (\item -> case item of
                                    SpImport (Import _ (Ident _ n)) -> Just n
                                    _                               -> Nothing) s
