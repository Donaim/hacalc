{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment
import Control.Monad
import System.Console.CmdArgs
import Data.Data
import System.IO

import PatternT.All
import Types
import Run
import Util

data ArgsT
	= Load
	{ rulefile :: String
	, exprfile :: String
	}
	deriving (Show, Data, Typeable)

load :: ArgsT
load = Load
	{ rulefile = def &= argPos 0 &= typ "rulefile"
	, exprfile = def &= argPos 1 &= typ "exprfile"
	}

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

withText :: String -> String -> IO ()
withText ruleText exprText =
	case interpretRulesAndText ruleText exprText of
		Left e -> putStrLn $ "Rules have syntax errors: " ++ show e
		Right results -> mapM_ mapf results

	where
	mapf (line, result) = case result of
		Left errors -> putErrLn $ line ++ " -> ERROR: " ++ show errors
		Right ok -> do
			r <- ok
			let showed = case r of [] -> line ; xs -> stringifyTree (fst3 (last xs))
			putStrLn $ line ++ " -> " ++ showed


main :: IO ()
main = do
	args <- cmdArgs $ modes [load]
		&= help "Example compiler for PatternT"
		&= program "hacalc"

	ruleText <- readFile (rulefile args)
	exprText <- readFile (exprfile args)
	withText ruleText exprText

