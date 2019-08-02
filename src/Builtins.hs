
module Builtins where

import PatternT.Types
import PatternT.Util
import PatternT.SimplifyInterface

ruleAdd :: String -> PureSimplificationF
ruleAdd name = (name, const $ stdNumberRule (+) name)

ruleMult :: String -> PureSimplificationF
ruleMult name = (name, const $ stdNumberRule (*) name)

ruleSub :: String -> PureSimplificationF
ruleSub name = (name, const $ stdNumberRule (-) name)

ruleDiv :: String -> PureSimplificationF
ruleDiv name = (name, const $ stdNumberRule (/) name)

rulePow :: String -> PureSimplificationF
rulePow name = (name, const $ stdNumberRule rationalPow name)

ruleEqual :: String -> PureSimplificationF
ruleEqual name = (name, func)
	where
	func simplifyF t = case t of
		(Branch (fname : x : xs)) -> -- ASSUMPTION: fname  == name
			let simplifiedxs = map (applySimplificationsUntil0LastF simplifyF) xs
			in let simplifiedx = applySimplificationsUntil0LastF simplifyF x
				in if all (== simplifiedx) simplifiedxs
					then Just $ Leaf "True"
					else Just $ Leaf "False"
		(_) -> Nothing

ruleIsNum :: String -> PureSimplificationF
ruleIsNum name = (name, func)
	where
	func simplifyF t = case t of
		(Branch (name : args)) -> case args of
			[x] -> case treeToMaybeNum x of
				Just n -> Just $ Leaf "True"
				Nothing -> Just $ Leaf "False"
			(_) -> Just $ Leaf "False"
		(_) -> Nothing

ruleLess :: String -> PureSimplificationF
ruleLess name = (name, func)
	where
	func simplifyF t = case t of
		(Branch [name, a, b]) -> Just $ Leaf $ show (a < b)
		(_) -> Nothing

ruleLessOrEq :: String -> PureSimplificationF
ruleLessOrEq name = (name, func)
	where
	func simplifyF t = case t of
		(Branch [name, a, b]) -> Just $ Leaf $ show (a <= b)
		(_) -> Nothing

---------------
-- ORDERING --
---------------

compareLeafs :: Symbol -> Symbol -> Ordering
compareLeafs a b =
	case symbolToMaybeNum a of
		Nothing -> case symbolToMaybeNum b of
			Nothing -> compare a b
			Just bn -> GT
		Just an -> case symbolToMaybeNum b of
			Nothing -> LT
			Just bn -> compare an bn

instance Ord Tree where
	compare a b =
		case a of
			(Leaf as) -> case b of
				(Leaf bs) ->
					compareLeafs as bs
				(Branch {}) ->
					LT -- ASSUMPTION: no singleton branches
			(Branch xs) -> case b of
				(Leaf {}) ->
					GT -- ASSUMPTION: no singleton branches
				(Branch ys) ->
					compare (reverse xs) (reverse ys) -- NOTE: the size of branch is the secondary thing, the most important is LAST element of branch

-----------
-- UTILS --
-----------

rationalPow :: Number -> Number -> Number
rationalPow a b = toRational $ (fromRational a) ** (fromRational b)

stdNumberRule :: (Number -> Number -> Number) -> String -> Tree -> Maybe Tree
stdNumberRule op name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp op failcase rargs
	where failcase = Leaf name

differentOrNothing :: Tree -> Tree -> Maybe Tree
differentOrNothing failcase t = case t of
	(Branch (x : xs)) ->
		if x == failcase
		then Nothing
		else Just t
	(_) -> Just t

numCast :: [Tree] -> [Either Tree Number]
numCast = map mapf
	where
	mapf t = case treeToMaybeNum t of
		Just x -> Right x
		Nothing -> Left t

withOp :: (Number -> Number -> Number) -> Tree -> [Tree] -> Tree
withOp op failcase rargs = case withOpOnMaybeNums op failcase numcasted of
	[] -> failcase
	[x] -> x
	xs -> (Branch xs)
	where numcasted = numCast rargs

withOpOnMaybeNums :: (Number -> Number -> Number) -> Tree -> [Either Tree Number] -> [Tree]
withOpOnMaybeNums op failcase mnums = loop Nothing mnums
	where
	loop :: Maybe Number -> [Either Tree Number] -> [Tree]
	loop macc [] = case macc of
		Nothing -> []
		Just acc -> [numToTree acc]
	loop macc (x : xs) =
		case x of
			Right num ->
				case macc of
					Just acc -> loop (Just $ op acc num) xs
					Nothing -> loop (Just num) xs
			Left t -> right
				where
				treeArgs = t : withOpOnMaybeNums op failcase xs
				allArgs = case macc of
					Nothing -> treeArgs
					Just acc -> (numToTree acc) : treeArgs
				right = [Branch (failcase : allArgs)]



