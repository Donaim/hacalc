
module Hacalc.Builtins where

import Data.Fixed (mod')
import Data.Ratio (numerator, denominator)

import PatternT.All
import Hacalc.Types
import Hacalc.Util

ruleAdd :: String -> PureSimplificationF
ruleAdd = ruleAddLim Nothing
ruleMul :: String -> PureSimplificationF
ruleMul = ruleMulLim Nothing
ruleSub :: String -> PureSimplificationF
ruleSub = ruleSubLim Nothing
ruleDiv :: String -> PureSimplificationF
ruleDiv = ruleDivLim Nothing
rulePow :: String -> PureSimplificationF
rulePow = rulePowLim Nothing
ruleMod :: String -> PureSimplificationF
ruleMod = ruleModLim Nothing

ruleAddLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
ruleAddLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberAdd) name)

ruleMulLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
ruleMulLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberMul) name)

ruleSubLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
ruleSubLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberSub) name)

ruleDivLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
ruleDivLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberDiv) name)

rulePowLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
rulePowLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberPow) name)

ruleModLim :: Maybe (Integer, Integer) -> String -> PureSimplificationF
ruleModLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberMod) name)

ruleEqual :: String -> PureSimplificationF
ruleEqual = stdAnyRule func
	where
	func simplifies args = case args of
		(x : xs) ->
			let simplifiedxs = map (applySimplificationsUntil0LastF (applyFirstSimplificationF simplifies)) xs
			in let simplifiedx = applySimplificationsUntil0LastF (applyFirstSimplificationF simplifies) x
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

ruleAlpha :: String -> PureSimplificationF
ruleAlpha = stdAnyRule func
	where
	func simplifyF args = case args of
		[abstractionPattern, abstractionProjection, body] ->
			case abstractionProjection of
				Branch {} -> Nothing
				Leaf projection ->
					case exprToMatchPattern (treeToExpr abstractionPattern) of
						Left e -> Nothing
						Right match -> Just $ tall match projection body
		(_) -> Nothing

	getLeafNames :: Tree -> [String]
	getLeafNames t = case t of
		Leaf s -> [s]
		Branch xs -> concat (map getLeafNames xs)

	getFreeNames :: Tree -> [String] -- TODO: optimize dis *angry face*
	getFreeNames t = filter (`notElem` taken) $ map (\ i -> 'x' : show i) [1 ..]
		where taken = getLeafNames t

	getAbstractionArgName :: PatternMatchPart -> String -> Tree -> Maybe String
	getAbstractionArgName match projection t = case matchGetDict match t of
		Nothing -> Nothing
		Just d -> case dictGet d projection of
			Just [Leaf s] -> Just s
			other -> Nothing -- NOTE: not nice because does not notify of error

	tall :: PatternMatchPart -> String -> Tree -> Tree
	tall match projection t = loop (getFreeNames t) emptyDict t
		where
		loop free scope t = case t of
			Leaf s -> case dictGet scope s of
				Just newname -> Leaf newname
				Nothing -> t
			Branch xs -> case getAbstractionArgName match projection t of
				Nothing -> Branch (map (loop free scope) xs)
				Just name -> Branch (map (loop (tail free) (dictAdd scope name (head free))) xs) -- ASSUMPTION: `free' is infinite

ruleBeta :: String -> PureSimplificationF
ruleBeta = stdAnyRule func
	where
	func simplifyF args = case args of
		[abstraction, var, body] -> case abstraction of
			Leaf s -> Just $ mapLeafs (rename s var) body
			Branch xs -> Nothing -- TODO: allow abstraction argument to be any match pattern
		(_) -> Nothing

	rename :: String -> Tree -> (String -> Tree -> Tree)
	rename name var = f
		where
		f leafName t =
			if leafName == name
			then var
			else t

	mapLeafs :: (String -> Tree -> Tree) -> Tree -> Tree
	mapLeafs f t = case t of
		(Leaf s) -> f s t
		(Branch xs) -> Branch (map (mapLeafs f) xs)

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

withChecker :: Maybe (Integer, Integer) -> (Number -> Number -> Number) -> (Number -> Number -> Number)
withChecker mbounds f = case mbounds of
	Nothing -> f
	Just (ma, mb) -> \ a b -> if checkBound2 a b ma dmaxa mb dmaxb then f a b else NumberNaN
		where
		dmaxa = fromIntegral ma -- ASSUMPTION: fromIntegral (2 ^ 99999) == Infinity, not a GHC error
		dmaxb = fromIntegral mb

checkBound2 :: Number -> Number -> Integer -> Double -> Integer -> Double -> Bool
checkBound2 a b imaxa dmaxa imaxb dmaxb = checkBound a imaxa dmaxa && checkBound b imaxb dmaxb

checkBound :: Number -> Integer -> Double -> Bool
checkBound x imax dmax = case x of
	NumberFrac x -> (abs (numerator x) < imax) && (denominator x < imax)
	NumberFloat x -> abs x < dmax -- NOTE: checkBound Infinity _ _ == False
	NumberNaN {} -> True

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

stdAnyRule :: ([Tree -> Maybe Tree] -> [Tree] -> Maybe Tree) -> String -> PureSimplificationF
stdAnyRule func name = (name, wrap)
	where
	wrap simplifies t = case t of
		(Branch (name : args)) -> func simplifies args
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



