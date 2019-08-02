
module Run where

import PatternT.Types
import PatternT.SimplifyInterface
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

mixedRules :: [[SimplifyPattern]] -> [[SimlifyFT]]
mixedRules patterns = map (\ ps -> map Tuple32 pureRules ++ map Tuple30 ps) patterns

interpretOneTree :: Tree -> [[SimlifyFT]] -> SimplifyMonad [(Tree, Either SimplifyPattern String, SimplifyCtx)]
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
