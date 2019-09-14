
module Hacalc.Builtins where

import Data.Fixed (mod')
import Data.Ratio (numerator, denominator)

import PatternT.All
import Hacalc.Types
import Hacalc.Util

ruleAdd :: String -> HPureSimplificationF
ruleAdd = ruleAddLim Nothing
ruleMul :: String -> HPureSimplificationF
ruleMul = ruleMulLim Nothing
ruleSub :: String -> HPureSimplificationF
ruleSub = ruleSubLim Nothing
ruleDiv :: String -> HPureSimplificationF
ruleDiv = ruleDivLim Nothing
rulePow :: String -> HPureSimplificationF
rulePow = rulePowLim Nothing
ruleMod :: String -> HPureSimplificationF
ruleMod = ruleModLim Nothing

ruleAddLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleAddLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberAdd) name)

ruleMulLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleMulLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberMul) name)

ruleSubLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleSubLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberSub) name)

ruleDivLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleDivLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberDiv) name)

rulePowLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
rulePowLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberPow) name)

ruleModLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleModLim mlim name = (name, const $ stdNumberRule (withChecker mlim numberMod) name)

-- | Do not simplify at all
ruleEq :: String -> HPureSimplificationF
ruleEq = ruleEqualLim (Just 0)

-- | Normal form, careful - not decidable
ruleEqual :: String -> HPureSimplificationF
ruleEqual = ruleEqualLim Nothing

-- | Arbitrary depth equality
ruleEqualLim :: Maybe Integer -> String -> HPureSimplificationF
ruleEqualLim mlim = stdAnyRule (funcRuleEqual mlim)

-- | Dynamic arbitrary depth equality
ruleEqualDynLim :: String -> HPureSimplificationF
ruleEqualDynLim = stdAnyRule func
	where
	func simplifyF args = case args of
		(n : x : xs) -> case treeToMaybeNum n of
			Just n -> case numMaybeInt n of
				Just n -> funcRuleEqual (Just n) simplifyF (x : xs)
				Nothing -> Nothing
			Nothing -> Nothing
		other -> Nothing

ruleOr :: String -> HPureSimplificationF
ruleOr = stdAnyRule func
	where
	func simplifyF args = Just $
		if any (== trueLeaf) (map (applySimplificationsUntil0LastFLim 1 (applyFirstSimplificationF simplifyF)) args)
		then trueLeaf
		else falseLeaf

ruleIsNum :: String -> HPureSimplificationF
ruleIsNum = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just (NumberNaN {}) -> Just $ falseLeaf
			Just n -> Just $ trueLeaf
			Nothing -> Just $ falseLeaf
		(_) -> Nothing

ruleIsNan :: String -> HPureSimplificationF
ruleIsNan = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just (NumberNaN {}) -> Just $ trueLeaf
			other -> Just $ falseLeaf
		(_) -> Nothing

ruleIsInt :: String -> HPureSimplificationF
ruleIsInt = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> falseLeaf
				NumberFloat x -> if x == fromInteger (round x) then trueLeaf else falseLeaf
				NumberFrac x -> if denominator x == 1 then trueLeaf else falseLeaf
			Nothing -> Nothing
		(_) -> Nothing

ruleIsFrac :: String -> HPureSimplificationF
ruleIsFrac = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> falseLeaf
				NumberFloat x -> falseLeaf
				NumberFrac x -> if denominator x == 1 then falseLeaf else trueLeaf
			Nothing -> Nothing
		(_) -> Nothing

ruleIsFloat :: String -> HPureSimplificationF
ruleIsFloat = stdAnyRule func
	where
	func simplifyF args = case args of
		[x] -> case treeToMaybeNum x of
			Just n -> Just $ case n of
				NumberNaN {} -> falseLeaf
				NumberFloat x -> if x == fromInteger (round x) then falseLeaf else trueLeaf
				NumberFrac x -> falseLeaf
			Nothing -> Nothing
		(_) -> Nothing

ruleFloat :: String -> HPureSimplificationF
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

ruleLess :: String -> HPureSimplificationF
ruleLess = stdAnyRule func
	where
	func simplifyF args = case args of
		[a, b] -> Just $ if (compareHacalc a b == LT) then trueLeaf else falseLeaf
		(_) -> Nothing

ruleLessOrEq :: String -> HPureSimplificationF
ruleLessOrEq = stdAnyRule func
	where
	func simplifyF args = case args of
		[a, b] -> Just $ if (let x = compareHacalc a b in x == LT || x == EQ) then trueLeaf else falseLeaf
		(_) -> Nothing

ruleAlpha :: String -> HPureSimplificationF
ruleAlpha = stdAnyRule func
	where
	func simplifyF args = case args of
		[abstractionPattern, abstractionProjection, body] ->
			case abstractionProjection of
				Branch {} -> Nothing
				Leaf projection ->
					case exprToMatchPattern (treeToExpr abstractionPattern) of
						Left e -> Nothing
						Right match -> Just $ tall match (patternElemShow projection) body
		(_) -> Nothing

	getLeafNames :: HTree -> [String]
	getLeafNames t = case t of
		Leaf s -> [patternElemShow s]
		Branch xs -> concat (map getLeafNames xs)

	getFreeNames :: HTree -> [String] -- TODO: optimize dis *angry face*
	getFreeNames t = filter (`notElem` taken) $ map (\ i -> '$' : show i) [1 ..]
		where taken = getLeafNames t

	getAbstractionArgName :: PatternMatchPart HLeafType -> String -> HTree -> Maybe String
	getAbstractionArgName match projection t = case matchGetDict match t of
		Nothing -> Nothing
		Just d -> case dictGet d (patternElemRead projection) of
			Just [Leaf s] -> Just (patternElemShow s)
			other -> Nothing -- NOTE: not nice because does not notify of error

	tall :: PatternMatchPart HLeafType -> String -> HTree -> HTree
	tall match projection t = loop (getFreeNames t) emptyDict t
		where
		loop free scope t = case t of
			Leaf s -> case dictGet scope s of
				Just newname -> Leaf (patternElemRead newname)
				Nothing -> t
			Branch xs -> case getAbstractionArgName match projection t of
				Nothing -> Branch (map (loop free scope) xs)
				Just name -> Branch (map (loop (tail free) (dictAdd scope (patternElemRead name) (head free))) xs) -- ASSUMPTION: `free' is infinite

ruleBeta :: String -> HPureSimplificationF
ruleBeta = stdAnyRule func
	where
	func simplifyF args = case args of
		[abstraction, var, body] -> case abstraction of
			Leaf s -> Just $ mapLeafs (rename s var) body
			Branch xs -> Nothing -- TODO: allow abstraction argument to be any match pattern
		(_) -> Nothing

	rename :: HLeafType -> HTree -> (HLeafType -> HTree -> HTree)
	rename name var = f
		where
		f leafName t =
			if leafName == name
			then var
			else t

	mapLeafs :: (HLeafType -> HTree -> HTree) -> HTree -> HTree
	mapLeafs f t = case t of
		(Leaf s) -> f s t
		(Branch xs) -> Branch (map (mapLeafs f) xs)

---------------
-- ORDERING --
---------------

-- Comparison that is aware of numbers
compareHacalc :: HTree -> HTree -> Ordering
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

compareLeafs :: HLeafType -> HLeafType -> Ordering
compareLeafs a b =
	case leafElemToMaybeNum a of
		Nothing -> case leafElemToMaybeNum b of
			Nothing -> compare a b
			Just bn -> GT
		Just an -> case leafElemToMaybeNum b of
			Nothing -> LT
			Just bn -> compareNumbers an bn

compareHacalcList :: [HTree] -> [HTree] -> Ordering
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

trueLeaf :: HTree
trueLeaf = Leaf (patternElemRead "True")

falseLeaf :: HTree
falseLeaf = Leaf (patternElemRead "False")

funcRuleEqual :: Maybe Integer -> [HTree -> Maybe HTree] -> [HTree] -> Maybe HTree
funcRuleEqual mlim simplifies args = case args of
	(x : xs) ->
		let simplifiedxs = map (sloop (applyFirstSimplificationF simplifies)) xs
		in let simplifiedx = sloop (applyFirstSimplificationF simplifies) x
			in if all (== simplifiedx) simplifiedxs
				then Just $ trueLeaf
				else Just $ falseLeaf
	(_) -> Nothing

	where
	sloop :: (HTree -> Maybe HTree) -> HTree -> HTree
	sloop = maybe applySimplificationsUntil0LastF (applySimplificationsUntil0LastFLim) mlim

applySimplificationsUntil0LastFLim :: Integer -> (HTree -> Maybe HTree) -> HTree -> HTree
applySimplificationsUntil0LastFLim lim func t0 = loop 0 t0
	where
	loop n t =
		if n >= lim
		then t
		else case func t of
			Nothing -> t
			Just newt -> loop (n + 1) newt

numMaybeInt :: Number -> Maybe Integer
numMaybeInt n = case n of
	NumberNaN {} -> Nothing
	NumberFrac x -> if denominator x == 1 then Just (numerator x) else Nothing
	NumberFloat x -> if x == fromInteger (round x) then Just (round x) else Nothing

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

stdNumberRule :: (Number -> Number -> Number) -> String -> HTree -> Maybe HTree
stdNumberRule op name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp numToTree treeToMaybeNum op failcase rargs
	where failcase = Leaf (patternElemRead name)

stdAnyRule :: ([HTree -> Maybe HTree] -> [HTree] -> Maybe HTree) -> String -> HPureSimplificationF
stdAnyRule func name = (name, wrap)
	where
	wrap simplifies t = case t of
		(Branch (name : args)) -> func simplifies args
		(_) -> Nothing

differentOrNothing :: HTree -> HTree -> Maybe HTree
differentOrNothing failcase t = case t of
	(Branch (x : xs)) ->
		if x == failcase
		then Nothing
		else Just t
	(_) -> Just t

castAll :: (HTree -> Maybe a) -> [HTree] -> [Either HTree a]
castAll from = map mapf
	where
	mapf t = case from t of
		Just x -> Right x
		Nothing -> Left t

withOp :: (a -> HTree) -> (HTree -> Maybe a) -> (a -> a -> a) -> HTree -> [HTree] -> HTree
withOp to from op failcase rargs = case withOpOnMaybeNums to op failcase numcasted of
	[] -> failcase
	[x] -> x
	xs -> (Branch xs)
	where numcasted = castAll from rargs

withOpOnMaybeNums :: (a -> HTree) -> (a -> a -> a) -> HTree -> [Either HTree a] -> [HTree]
withOpOnMaybeNums to op failcase mnums = loop Nothing mnums
	where
	-- loop :: Maybe Number -> [Either HTree Number] -> [HTree]
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



