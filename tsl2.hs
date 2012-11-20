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
import AbsGame
import TSLAbsGame
import EqSMT
import Adaptor
import FCompile

main = do
    args <- getArgs
    f <- case args of
             [] -> fail $ "File name required"
             _ -> return $ head args
    mods <- parseTSL M.empty f
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
    let game = tslAbsGame ispec
    let result = runST $ doEverything game newPDBPriv (eqSolver ispec)
    putStrLn $ "result: " ++ show result

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
parseTSL :: M.Map FilePath [SpecItem] -> FilePath -> IO (M.Map FilePath [SpecItem])
parseTSL modules f = do
    tsl <- readFile f
    --putStr tsl
    spec <- case parse grammar f tsl of
                 Left e -> fail $ show e
                 Right st -> return st
    writeFile (f ++ ".out") $ P.render $ pp spec
    -- Parse imports
    foldM (\mods imp -> if M.member imp mods
                           then return mods
                           else parseTSL mods imp)
          (M.insert f spec modules) (imports spec)

-- Extract the list of imports from parsed TSL spec
imports :: [SpecItem] -> [FilePath]
imports s = mapMaybe (\item -> case item of
                                    SpImport (Import _ (Ident _ name)) -> Just name
                                    _ -> Nothing) s
