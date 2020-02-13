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
import PatterntCommonFrontend.All

data ArgsT
	= Load
	{ rulefile :: String
	, exprfile :: Maybe String
	, original :: Bool
	, limit    :: Maybe Int
	, trace    :: Bool
	}
	deriving (Show, Data, Typeable)

load :: ArgsT
load = Load
	{ rulefile = def &= argPos 0 &= typ "rulefile"
	, exprfile = def &= typ "exprfile"
	, original = def &= help "Display the original expression in output. Looks like 'expr -> reduced'"
	, limit    = def &= help "Limit the number of evaluations"
	, trace    = def &= help "Show step by step process"
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

interpretPureLine :: Rulesets HLeafType -> InterpretOptions -> String -> (String, [String], Bool)
interpretPureLine rules opts line = unliftIdentityMonad $
	case interpretLine opts rules () line of
		Left e -> return ("Error: " ++ show e, [], False)
		Right r -> do
			(answer, taken, droped) <- r
			let hist =
				taken
				|> showHistory
				|> map (\(state, rule) -> "\t(using  " ++ rule ++ ")\n-> " ++ state)
			return (answer, hist, null droped)

getRules :: String -> IO (Rulesets HLeafType)
getRules text = case readPatterns text of
	Left e -> do
		die $ "Bad rules: " ++ show e
	Right p -> do
		return p

interactFunc :: Rulesets HLeafType -> InterpretOptions -> Bool -> Bool -> String -> String
interactFunc rules opts originalQ traceQ text = concatMap (++ "\n") output
	where
	filtered = filter (not . null) $ lines text
	answers = map (interpretPureLine rules opts) filtered
	output = map tf (zip answers filtered)
	tf ((answer, hist, finishedQ), original) =
		(if originalQ then "-> " ++ original ++ "\n" else "")
		++ (if traceQ then
				(concatMap (\s -> s ++ "\n") hist
				++
				if finishedQ then "\t(finished)\n" else "\t(not finished)\n")
			else "")
		++ answer

main :: IO ()
main = do
	args <- cmdArgs $ modes [load]
		&= help "Example compiler for PatternT"
		&= program "hacalc"

	ruleText <- readFile (rulefile args)
	rules <- getRules ruleText
	let opts = runOptions { interpretStepLimit = limit args }
	case exprfile args of
		Just exprfile -> do
			exprText <- readFile exprfile
			putStr $ interactFunc rules opts (original args) (trace args)  exprText
		Nothing -> do
			interact (interactFunc rules opts (original args) (trace args))

