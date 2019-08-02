
module Run where

import PatternT.Types
import Types
import Builtins

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
