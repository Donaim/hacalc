
module Hacalc.Builtins where

import Data.Fixed (mod')
import Data.Ratio (denominator)

import PatternT.Types
import PatternT.Util
import PatternT.SimplifyInterface
import Hacalc.Types
import Hacalc.Util

ruleAdd :: String -> PureSimplificationF
ruleAdd name = (name, const $ stdNumberRule numberAdd name)

ruleMul :: String -> PureSimplificationF
ruleMul name = (name, const $ stdNumberRule numberMul name)

ruleSub :: String -> PureSimplificationF
ruleSub name = (name, const $ stdNumberRule numberSub name)

ruleDiv :: String -> PureSimplificationF
ruleDiv name = (name, const $ stdNumberRule numberDiv name)

rulePow :: String -> PureSimplificationF
rulePow name = (name, const $ stdNumberRule numberPow name)

ruleMod :: String -> PureSimplificationF
ruleMod name = (name, const $ stdNumberRule numberMod name)

ruleEqual :: String -> PureSimplificationF
ruleEqual = stdAnyRule func
	where
	func simplifyF args = case args of
		(x : xs) ->
			let simplifiedxs = map (applySimplificationsUntil0LastF simplifyF) xs
			in let simplifiedx = applySimplificationsUntil0LastF simplifyF x
				in if all (== simplifiedx) simplifiedxs
					then Just $ Leaf "True"
					else Just $ Leaf "False"
		(_) -> Nothing

ruleIsNum :: String -> PureSimplificationF
ruleIsNum = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ Leaf "True"
			Nothing -> Just $ Leaf "False"
		(_) -> Nothing

ruleIsInt :: String -> PureSimplificationF
ruleIsInt = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> Leaf "False"
				NumberFloat x -> Leaf (if x == fromInteger (round x) then "True" else "False")
				NumberFrac x -> Leaf (if denominator x == 1 then "True" else "False")
			Nothing -> Nothing
		(_) -> Nothing

ruleIsFrac :: String -> PureSimplificationF
ruleIsFrac = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> Leaf "False"
				NumberFloat x -> Leaf "False"
				NumberFrac x -> Leaf (if denominator x == 1 then "False" else "True")
			Nothing -> Nothing
		(_) -> Nothing

ruleIsFloat :: String -> PureSimplificationF
ruleIsFloat = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> Leaf "False"
				NumberFloat x -> Leaf (if x == fromInteger (round x) then "False" else "True")
				NumberFrac x -> Leaf "False"
			Nothing -> Nothing
		(_) -> Nothing

ruleFloat :: String -> PureSimplificationF
ruleFloat = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> x
				NumberFloat n -> x
				NumberFrac n -> numToTree (NumberFloat $ fromRational n)
			Nothing -> Nothing
		(_) -> Nothing

ruleLess :: String -> PureSimplificationF
ruleLess = stdAnyRule func
	where
	func simplifyF args = case args of
		[a, b] -> Just $ Leaf $ show (compareHacalc a b == LT)
		(_) -> Nothing

ruleLessOrEq :: String -> PureSimplificationF
ruleLessOrEq = stdAnyRule func
	where
	func simplifyF args = case args of
		[a, b] -> Just $ Leaf $ show (let x = compareHacalc a b in x == LT || x == EQ)
		(_) -> Nothing

---------------
-- ORDERING --
---------------

-- Comparison that is aware of numbers
compareHacalc :: Tree -> Tree -> Ordering
compareHacalc a b =
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
				compareHacalcList (reverse xs) (reverse ys) -- NOTE: the size of branch is the secondary thing, the most important is LAST element of branch

compareNumbers :: Number -> Number -> Ordering
compareNumbers a b = case a of
	NumberFrac a -> case b of
		NumberFrac b -> compare a b
		NumberFloat b -> compare a (toRationalPrecise b)
		NumberNaN {} -> LT
	NumberFloat a -> case b of
		NumberFrac b -> compare (toRationalPrecise a) b
		NumberFloat b -> compare a b
		NumberNaN {} -> EQ
	NumberNaN {} -> case b of
		NumberFrac {} -> GT
		NumberFloat {} -> GT
		NumberNaN {} -> EQ

compareLeafs :: Symbol -> Symbol -> Ordering
compareLeafs a b =
	case symbolToMaybeNum a of
		Nothing -> case symbolToMaybeNum b of
			Nothing -> compare a b
			Just bn -> GT
		Just an -> case symbolToMaybeNum b of
			Nothing -> LT
			Just bn -> compareNumbers an bn

compareHacalcList :: [Tree] -> [Tree] -> Ordering
compareHacalcList [] [] = EQ
compareHacalcList xs [] = GT
compareHacalcList [] ys = LT
compareHacalcList (x : xs) (y : ys) =
	let cmp = compareHacalc x y
	in if cmp == EQ
		then compareHacalcList xs ys
		else cmp


----------------
-- OPERATIONS --
----------------

numberAdd :: Number -> Number -> Number
numberAdd = numberDefaultOpTotal (+)

numberSub :: Number -> Number -> Number
numberSub = numberDefaultOpTotal (-)

numberMul :: Number -> Number -> Number
numberMul = numberDefaultOpTotal (*)

numberDiv :: Number -> Number -> Number
numberDiv = numberDefaultOp (\ a b -> if b == 0 then NumberNaN else NumberFrac $ a / b)

numberPow :: Number -> Number -> Number
numberPow = numberDefaultOp (\ a b -> NumberFloat (fromRational a ** fromRational b))

numberMod :: Number -> Number -> Number
numberMod = numberDefaultOp (\ a b -> if b == 0 then NumberNaN else NumberFrac $ mod' a b)

-----------
-- UTILS --
-----------

numberDefaultOpTotal :: (Rational -> Rational -> Rational) -> Number -> Number -> Number
numberDefaultOpTotal f = numberDefaultOp (\ a b -> NumberFrac $ f a b)

numberDefaultOp :: (Rational -> Rational -> Number) -> Number -> Number -> Number
numberDefaultOp op a b =
	case a of
		NumberNaN {} -> NumberNaN
		NumberFrac a -> case b of
			NumberNaN {} -> NumberNaN
			NumberFrac b -> op a b
			NumberFloat b -> op a (toRationalPrecise b)
		NumberFloat a -> case b of
			NumberNaN {} -> NumberNaN
			NumberFrac b -> op (toRationalPrecise a) b
			NumberFloat b -> op (toRationalPrecise a) (toRationalPrecise b)

stdNumberRule :: (Number -> Number -> Number) -> String -> Tree -> Maybe Tree
stdNumberRule op name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp numToTree treeToMaybeNum op failcase rargs
	where failcase = Leaf name

stdAnyRule :: ((Tree -> Maybe Tree) -> [Tree] -> Maybe Tree) -> String -> PureSimplificationF
stdAnyRule func name = (name, wrap)
	where
	wrap simplifyF t = case t of
		(Branch (name : args)) -> func simplifyF args
		(_) -> Nothing

differentOrNothing :: Tree -> Tree -> Maybe Tree
differentOrNothing failcase t = case t of
	(Branch (x : xs)) ->
		if x == failcase
		then Nothing
		else Just t
	(_) -> Just t

castAll :: (Tree -> Maybe a) -> [Tree] -> [Either Tree a]
castAll from = map mapf
	where
	mapf t = case from t of
		Just x -> Right x
		Nothing -> Left t

withOp :: (a -> Tree) -> (Tree -> Maybe a) -> (a -> a -> a) -> Tree -> [Tree] -> Tree
withOp to from op failcase rargs = case withOpOnMaybeNums to op failcase numcasted of
	[] -> failcase
	[x] -> x
	xs -> (Branch xs)
	where numcasted = castAll from rargs

withOpOnMaybeNums :: (a -> Tree) -> (a -> a -> a) -> Tree -> [Either Tree a] -> [Tree]
withOpOnMaybeNums to op failcase mnums = loop Nothing mnums
	where
	-- loop :: Maybe Number -> [Either Tree Number] -> [Tree]
	loop macc [] = case macc of
		Nothing -> []
		Just acc -> [to acc]
	loop macc (x : xs) =
		case x of
			Right num ->
				case macc of
					Just acc -> loop (Just $ op acc num) xs
					Nothing -> loop (Just num) xs
			Left t -> right
				where
				treeArgs = t : withOpOnMaybeNums to op failcase xs
				allArgs = case macc of
					Nothing -> treeArgs
					Just acc -> (to acc) : treeArgs
				right = [Branch (failcase : allArgs)]



