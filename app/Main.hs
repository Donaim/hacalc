{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment
import Control.Monad
import System.Console.CmdArgs
import Data.Data
import System.IO

import PatternT.All
import Hacalc.Types
import Hacalc.Run
import Hacalc.Util

data ArgsT
	= Load
	{ rulefile :: String
	, exprfile :: String
	, original :: Bool
	}
	deriving (Show, Data, Typeable)

load :: ArgsT
load = Load
	{ rulefile = def &= argPos 0 &= typ "rulefile"
	, exprfile = def &= argPos 1 &= typ "exprfile"
	, original = def &= help "Display the original expression in output. Looks like 'expr -> reduced'"
	}

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

withText :: Bool -> String -> String -> IO ()
withText originalQ ruleText exprText =
	case interpretRulesAndText ruleText exprText of
		Left e -> putStrLn $ "Rules have syntax errors: " ++ show e
		Right results -> mapM_ mapf results

	where
	mapf (line, result) = case result of
		Left errors -> putErrLn $ line ++ " -> ERROR: " ++ show errors
		Right ok -> do
			r <- ok
			let showed = case r of [] -> line ; xs -> stringifyTree (fst3 (last xs))
			let answer = if originalQ then line ++ " -> " ++ showed else showed
			putStrLn $ answer


main :: IO ()
main = do
	args <- cmdArgs $ modes [load]
		&= help "Example compiler for PatternT"
		&= program "hacalc"

	ruleText <- readFile (rulefile args)
	exprText <- readFile (exprfile args)
	withText (original args) ruleText exprText

