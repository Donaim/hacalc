
module Run where

import PatternT.Types
import PatternT.SimplifyInterface
import PatternT.Parsing
import Types
import Builtins
import Util

simplifyCtxInitial :: SimplifyCtx
simplifyCtxInitial = ()

showCtx :: SimplifyCtx -> String
showCtx = show

pureRules :: [PureSimplificationF]
pureRules =
	[ ruleAdd "$add"
	, ruleMult "$mult"
	, ruleSub "$sub"
	, ruleDiv "$div"
	, rulePow "$pow"
	, ruleEqual "$equal"
	, ruleIsNum "$num?"
	, ruleLess "lt?"
	, ruleLessOrEq "le?"
	]

mixedRules :: Rulesets -> [[SimlifyFT]]
mixedRules patterns = map (\ ps -> map Tuple32 pureRules ++ map Tuple30 ps) patterns

type Stdout = SimplifyMonad [(Tree, Either SimplifyPattern String, SimplifyCtx)]
type Rulesets = [[SimplifyPattern]]

interpretOneTree :: Tree -> [[SimlifyFT]] -> Stdout
interpretOneTree = loop
	where
	loop tree [] = return []
	loop tree (ruleset : rest) = do
		history <- mixedApplySimplificationsWithPureUntil0Debug ruleset simplifyCtxInitial tree
		let newtree = if null history
			then tree
			else fst3 (last history)
		next <- loop newtree rest
		return (history ++ next)

interpretOneTree0 :: Tree -> Rulesets -> Stdout
interpretOneTree0 t rules = interpretOneTree t (mixedRules rules)

interpretLine :: String -> Rulesets -> Either ParseError Stdout
interpretLine line rules = case tokenize line of
	Left e -> Left e
	Right tokens -> Right $ interpretOneTree0 (makeTree (Group tokens)) rules
