{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment
import Control.Monad
import System.Console.CmdArgs
import Data.Data
import System.IO
import System.Exit

import PatternT.All
import Hacalc.All

data ArgsT
	= Load
	{ rulefile :: String
	, exprfile :: Maybe String
	, original :: Bool
	, limit    :: Maybe Int
	}
	deriving (Show, Data, Typeable)

load :: ArgsT
load = Load
	{ rulefile = def &= argPos 0 &= typ "rulefile"
	, exprfile = def &= typ "exprfile"
	, original = def &= help "Display the original expression in output. Looks like 'expr -> reduced'"
	, limit    = def &= help "Limit the number of evaluations"
	}

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

runOptions :: InterpretOptions
runOptions = InterpretOptions
	{ textDelimiters                     = hacalcDelimitingSymbols
	, textDelimiterPreserveQuotesQ       = True
	, textEnableCommentsQ                = True
	, tokenizeRespectQuotesQ             = True
	, tokenizeSplitByNumbersQ            = True
	, parseFixMissingBracketsQ           = True
	, parseReportMissingEndquoteQ        = True
	, parseReportEmptyBracketsQ          = False
	, displayConcatByNumbersQ            = True
	, interpretStepLimit                 = Nothing
	, interpretTreeSizeLimit             = Nothing
	, interpretCondRecursionLimit        = Just 8
	}

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

interpretPureLine :: Rulesets HLeafType -> String -> String
interpretPureLine rules line = unliftIdentityMonad $
	case interpretLine runOptions rules () line of
		Left e -> return $ "Error: " ++ show e
		Right r -> do
			(answer, taken, droped) <- r
			return $ answer

getRules :: String -> IO (Rulesets HLeafType)
getRules text = case readPatterns text of
	Left e -> do
		die $ "Bad rules: " ++ show e
	Right p -> do
		return p

interactFunc :: Rulesets HLeafType -> Bool -> String -> String
interactFunc rules originalQ text = concatMap (++ "\n") output
	where
	filtered = filter (not . null) $ lines text
	answers = map (interpretPureLine rules) filtered
	output = map tf (zip answers filtered)
	tf (answer, original) = if originalQ then original ++ " -> " ++ answer else answer

main :: IO ()
main = do
	args <- cmdArgs $ modes [load]
		&= help "Example compiler for PatternT"
		&= program "hacalc"

	ruleText <- readFile (rulefile args)
	rules <- getRules ruleText
	case exprfile args of
		Just exprfile -> do
			exprText <- readFile exprfile
			putStr $ interactFunc rules (original args) exprText
		Nothing -> do
			interact (interactFunc rules (original args))

