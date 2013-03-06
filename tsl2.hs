{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Main where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import Control.Monad.ST.Lazy
import Data.Maybe
import System.Environment
import Text.Parsec
import qualified Text.PrettyPrint as P
import System.Console.GetOpt
import System.Directory

import qualified ISpec as I
import SpecInline
import PP
import Pos
import Name
import Parse
import Type
import TypeOps
import Const   
import Var     
import Process
import ProcessOps
import Method
import MethodOps
import Template
import TemplateOps
import Spec
import SpecOps
import Expr
import ExprOps
import StatementOps
import NS
import Cascade
import TSLAbsGame
import EqSMT
import FCompile
import SetExplorer
import MultiSetExplorer
import VarView
import DbgGUI
import DbgTypes
import Cudd 
import SMTLib2

data TOption = InputTSL String
             | ImportDir String

options :: [OptDescr TOption]
options = [Option ['i'] [] (ReqArg InputTSL "FILE") "input TSL file",
           Option ['I'] [] (ReqArg ImportDir "DIRECTORY") "additional import lookup directory"]

data Config = Config {confTSLFile    :: FilePath,
                      confImportDirs :: [FilePath]}

defaultConfig = Config { confTSLFile    = ""
                       , confImportDirs = []}

addOption :: TOption -> Config -> Config
addOption (InputTSL f)  config = config{confTSLFile = f}
addOption (ImportDir d) config = config{confImportDirs = (confImportDirs config) ++ [d]}

instance Vals ()

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
                                    SpImport (Import _ (Ident _ name)) -> Just name
                                    _ -> Nothing) s
