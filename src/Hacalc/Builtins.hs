
module Hacalc.Builtins where

import Data.Fixed (mod')
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (isJust, isNothing, maybe)
import Data.Bits

import PatternT.All
import Hacalc.Types
import Hacalc.Util
import Hacalc.UtilExternal
import Hacalc.Parsing

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
ruleLog :: String -> HPureSimplificationF
ruleLog = ruleLogLim Nothing
ruleMod :: String -> HPureSimplificationF
ruleMod = ruleModLim Nothing

ruleAddLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleAddLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberAdd) name)

ruleMulLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleMulLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberMul) name)

ruleSubLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleSubLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberSub) name)

ruleDivLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleDivLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberDiv) name)

rulePowLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
rulePowLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberPow) name)

ruleLogLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleLogLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberLog) name)

ruleModLim :: Maybe (Integer, Integer) -> String -> HPureSimplificationF
ruleModLim mlim name = (HVar name, const $ stdNumberRule (withChecker mlim numberMod) name)

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
		(n : x : xs) -> case n of
			(Leaf n) -> case numMaybeInt n of
				Just n -> funcRuleEqual (Just n) simplifyF (x : xs)
				Nothing -> Nothing
			other -> Nothing
		other -> Nothing

ruleOr :: String -> HPureSimplificationF
ruleOr = stdAnyRule func
	where
	func simplifyF args = Just $
		if any (== trueLeaf) (map (applySimplificationsUntil0LastFLim 1 (applyFirstSimplificationF simplifyF)) args)
		then trueLeaf
		else falseLeaf

ruleIsNum :: String -> HPureSimplificationF
ruleIsNum = stdUnaryRule func
	where
	func x = case x of
		(NumberNaN {}) -> Just $ falseLeaf
		(NumberFrac {}) -> Just $ trueLeaf
		(NumberIrr {}) -> Just $ trueLeaf
		HVar {} -> Nothing

ruleIsNan :: String -> HPureSimplificationF
ruleIsNan = stdUnaryRule func
	where
	func x = case x of
		(NumberNaN {}) -> Just $ trueLeaf
		other -> Just $ falseLeaf

ruleIsInt :: String -> HPureSimplificationF
ruleIsInt = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac x sf) -> Just $ if denominator x == 1 then trueLeaf else falseLeaf
		other -> Nothing

ruleIsFrac :: String -> HPureSimplificationF
ruleIsFrac = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac x sf) -> Just $ if isJust sf || denominator x == 1 then falseLeaf else trueLeaf -- NOTE: Int is not a Frac
		(NumberIrr x sf) -> Just $ if isNothing sf then trueLeaf else falseLeaf
		other -> Nothing

ruleIsFloat :: String -> HPureSimplificationF
ruleIsFloat = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac x sf) -> Just $ if isNothing sf || denominator x == 1 then falseLeaf else trueLeaf -- NOTE: Int is not a Frac
		(NumberIrr x sf) -> Just $ if isNothing sf then falseLeaf else trueLeaf
		other -> Nothing

ruleIsRational :: String -> HPureSimplificationF
ruleIsRational = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac {}) -> Just trueLeaf
		(NumberIrr {}) -> Just falseLeaf
		(NumberNaN {}) -> Just falseLeaf
		(HVar {}) -> Nothing

ruleIsReal :: String -> HPureSimplificationF
ruleIsReal = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac {}) -> Just trueLeaf
		(NumberIrr {}) -> Just trueLeaf
		(NumberNaN {}) -> Just falseLeaf
		(HVar {}) -> Nothing

ruleFloat :: String -> HPureSimplificationF
ruleFloat = stdAnyNormalRule func
	where
	func args = case args of
		[x] -> floatBase 10 x
		[x, base] -> case base of
			(NumberFrac b sf) ->
				if denominator b == 1
				then floatBase (numerator b) x
				else Nothing
			other -> Nothing
		(_) -> Nothing

	floatBase :: Integer -> HLeafType -> Maybe HTree
	floatBase b x =
		if isValidBase b
		then case x of
			(NumberFrac n sf) -> Just $ case sf of
				Just ob -> if ob == b then Leaf x else correct
				Nothing -> correct
				where correct = Leaf (NumberFrac n (Just b))
			other -> Nothing
		else Nothing

ruleFrac :: String -> HPureSimplificationF
ruleFrac = stdUnaryNumRule
	(\ x sf -> Just $ NumberFrac x Nothing)
	(\ x sf -> Just $ NumberIrr x Nothing)

ruleFloor :: String -> HPureSimplificationF
ruleFloor = stdUnaryNumRule
	(\ x sf -> Just $ NumberFrac ((floor x) % 1) sf)
	(\ x sf -> Just $ NumberFrac ((floor x) % 1) sf) -- NOTE: converts real to frac

ruleCeiling :: String -> HPureSimplificationF
ruleCeiling = stdUnaryNumRule
	(\ x sf -> Just $ NumberFrac ((ceiling x) % 1) sf)
	(\ x sf -> Just $ NumberFrac ((ceiling x) % 1) sf) -- NOTE: converts real to frac

ruleRound :: String -> HPureSimplificationF
ruleRound = stdUnaryNumRule
	(\ x sf -> Just $ NumberFrac ((round x) % 1) sf)
	(\ x sf -> Just $ NumberFrac ((round (iReal2Rat x)) % 1) sf)  -- NOTE: converts real to frac

ruleApprox :: String -> HPureSimplificationF
ruleApprox = stdUnaryNumRule
	(\ x sf -> Just $ NumberFrac x sf)
	(\ x sf -> Just $ NumberFrac (iReal2Rat x) sf)

ruleDigits :: String -> HPureSimplificationF
ruleDigits = stdAnyNormalRule func
	where
	func args = case args of
		[count, x] -> case count of
			(NumberFrac count sf) ->
				if denominator count == 1
				then case x of
					NumberFrac x b -> Just $ Leaf $ HVar
					 	(showHFloat (maybe 10 id b) (numerator count) x)  -- FIXME: leave it as number
					NumberIrr x b -> Just $ Leaf $ HVar
					 	(showHFloat (maybe 10 id b) (numerator count) (iReal2RatP (fromInteger (numerator count)) x))  -- FIXME: leave it as number
					other -> Nothing
				else Nothing
			other -> Nothing
		(_) -> Nothing

ruleSinus :: String -> HPureSimplificationF
ruleSinus = stdUnaryNumRule
	(\ x sf -> Just $ NumberIrr (sin (fromRational x)) (numberFormNotFraction sf)) -- TODO: add simple rational cases
	(\ x sf -> Just $ NumberIrr (sin x) (numberFormNotFraction sf))

ruleCosinus :: String -> HPureSimplificationF
ruleCosinus = stdUnaryNumRule
	(\ x sf -> Just $ NumberIrr (cos (fromRational x)) (numberFormNotFraction sf)) -- TODO: add simple rational cases
	(\ x sf -> Just $ NumberIrr (cos x) (numberFormNotFraction sf))

rulePi :: String -> HPureSimplificationF
rulePi = stdConstant (Leaf (NumberIrr pi (Just 10)))

-- | Euler number
ruleExp1 :: String -> HPureSimplificationF
ruleExp1 = stdConstant (Leaf (NumberIrr (exp 1) (Just 10)))

ruleBitOr :: String -> HPureSimplificationF
ruleBitOr = stdBinaryIntRule (.|.)

ruleBitAnd :: String -> HPureSimplificationF
ruleBitAnd = stdBinaryIntRule (.&.)

ruleBitXor :: String -> HPureSimplificationF
ruleBitXor = stdBinaryIntRule xor

ruleBitNot :: String -> HPureSimplificationF
ruleBitNot = stdUnaryNumRule func rfunc
	where
	func x sf =
		if denominator x /= 1
		then Nothing
		else Just (NumberFrac (complement (numerator x) % 1) sf)
	rfunc x sf = func (iReal2Rat x) sf

ruleBitShiftR :: String -> HPureSimplificationF
ruleBitShiftR = stdBinaryArgumentedRule shiftR

ruleBitShiftL :: String -> HPureSimplificationF
ruleBitShiftL = stdBinaryArgumentedRule shiftL

ruleBitRotateR :: String -> HPureSimplificationF
ruleBitRotateR = stdBinaryArgumentedRule rotateR

ruleBitRotateL :: String -> HPureSimplificationF
ruleBitRotateL = stdBinaryArgumentedRule rotateL

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

	getFreeNames :: HTree -> [String] -- TODO: optimize dis *angry face*
	getFreeNames t = filter (`notElem` taken) $ map (:[]) ['a' .. 'z'] ++ map ('x' :) (map show [2 ..])
		where
		taken = getLeafNames t

		getLeafNames :: HTree -> [String]
		getLeafNames t = case t of
			Leaf s -> [patternElemShow s]
			Branch xs -> concat (map getLeafNames xs)

	getAbstractionArgName :: PatternMatchPart HLeafType -> String -> HTree -> Maybe String
	getAbstractionArgName match projection t = case matchGetDict match t of
		Nothing -> Nothing
		Just d -> case dictGet d (patternElemReadUq projection) of
			Just [Leaf s] -> Just (patternElemShow s)
			other -> Nothing -- NOTE: not nice because does not notify of error

	tall :: PatternMatchPart HLeafType -> String -> HTree -> HTree
	tall match projection t = loop (getFreeNames t) emptyDict t
		where
		loop free scope t = case t of
			Leaf s -> case dictGet scope s of
				Just (Just newname) -> Leaf (patternElemReadUq newname)
				other -> t
			Branch xs -> case getAbstractionArgName match projection t of
				Nothing -> Branch (map (loop free scope) xs)
				Just name ->
					Branch $ flip map xs $
						case dictGet scope (patternElemReadUq name) of
							Just {} -> -- name already taken
								loop (tail free)
								     (dictAdd scope (patternElemReadUq name) (Just (head free)))
							Nothing ->
								loop free
								     (dictAdd scope (patternElemReadUq name) Nothing)

ruleBeta :: String -> HPureSimplificationF
ruleBeta = stdAnyRule func
	where
	func simplifyF args = case args of
		[whole] -> case whole of
			(Branch (body : var : vars)) -> case body of -- DIRTY!: projection
				Branch (x : dot : xs) -> case x of
					Leaf s -> Just $
						let first = mapLeafs (rename s var) (Branch xs)
						in case vars of
							[] -> first
							(y : ys) -> Branch ((Branch [first, y]) : ys)
					Branch {} -> Nothing -- TODO: allow lambda argument to be structured
				other -> Nothing
			other -> Nothing
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
		(Branch xs) -> case map (mapLeafs f) xs of
			[y] -> y
			ys -> Branch ys

---------------
-- ORDERING --
---------------

-- Comparison that is aware of numbers
compareHacalc :: HTree -> HTree -> Ordering
compareHacalc a b =
	case a of
		(Leaf as) -> case b of
			(Leaf bs) ->
				compare as bs
			(Branch {}) ->
				LT -- ASSUMPTION: no singleton branches
		(Branch xs) -> case b of
			(Leaf {}) ->
				GT -- ASSUMPTION: no singleton branches
			(Branch ys) ->
				compare xs ys -- NOTE: the size of branch is the secondary thing, the most important is first element of branch

----------------
-- OPERATIONS --
----------------

numberAdd :: HLeafType -> HLeafType -> HLeafType
numberAdd = numberDefaultOpTotal (+) (+)

numberSub :: HLeafType -> HLeafType -> HLeafType
numberSub = numberDefaultOpTotal (-) (-)

numberMul :: HLeafType -> HLeafType -> HLeafType
numberMul = numberDefaultOpTotal (*) (*)

numberDiv :: HLeafType -> HLeafType -> HLeafType
numberDiv ha hb = numberDefaultOp op opr ha hb
	where
	op a 0 = NumberNaN
	op a b = NumberFrac (a / b) (numberDefaultOpGetForm Nothing ha hb)

	opr a 0 = NumberNaN
	opr a b = NumberIrr (a / b) (numberDefaultOpGetForm (Just 10) ha hb)

numberPow :: HLeafType -> HLeafType -> HLeafType
numberPow ha hb = case ha of
	HVar {} -> NumberNaN -- FIXME: something more clever
	NumberNaN {} -> NumberNaN
	NumberFrac a af -> case hb of
		HVar {} -> NumberNaN -- FIXME: something more clever
		NumberNaN {} -> NumberNaN
		NumberFrac b bf ->
			if denominator b == 1 -- TODO: allow more exponents to be exact. Like Racket does
			then NumberFrac (a ^^ (numerator b)) preciseform
			else NumberIrr ((fromRational a) ** (fromRational b)) iform
		NumberIrr b bf ->
			NumberIrr ((fromRational a) ** b) iform
	NumberIrr a af -> case hb of
		HVar {} -> NumberNaN -- FIXME: something more clever
		NumberNaN {} -> NumberNaN
		NumberFrac b bf ->
			if denominator b == 1
			then NumberIrr (a ^^ (numerator b)) iform
			else NumberIrr (a ** (fromRational b)) iform
		NumberIrr b bf ->
			NumberIrr (a ** b) iform
	where
	preciseform = (numberDefaultOpGetForm Nothing ha hb)
	iform = numberDefaultOpGetForm (Just 10) ha hb

numberLog :: HLeafType -> HLeafType -> HLeafType
numberLog ha hb = numberDefaultOp logop logopr ha hb
	where
	-- TODO: do rationals for some cases
	logop a b = NumberIrr (logBase (fromRational a) (fromRational b)) iform
	logopr a b = NumberIrr (logBase a b) iform

	preciseform = numberDefaultOpGetForm Nothing ha hb
	iform = numberDefaultOpGetForm (Just 10) ha hb

numberMod :: HLeafType -> HLeafType -> HLeafType
numberMod ha hb = numberDefaultOp op opr ha hb
	where
	op a 0 = NumberNaN
	op a b = NumberFrac (mod' a b) preciseform

	opr a 0 = NumberNaN
	opr a b = NumberIrr (mod' a b) iform

	preciseform = numberDefaultOpGetForm Nothing ha hb
	iform = numberDefaultOpGetForm (Just 10) ha hb

-----------
-- UTILS --
-----------

numMaybeInt :: HLeafType -> Maybe Integer
numMaybeInt n = case n of
	NumberFrac x sf -> if denominator x == 1 then Just (numerator x) else Nothing
	other -> Nothing

withChecker :: Maybe (Integer, Integer) -> (HLeafType -> HLeafType -> HLeafType) -> (HLeafType -> HLeafType -> HLeafType)
withChecker mbounds f = case mbounds of
	Nothing -> f
	Just (ma, mb) -> \ a b -> if checkBound2 a b ma mb then f a b else NumberNaN

checkBound2 :: HLeafType -> HLeafType -> Integer -> Integer -> Bool
checkBound2 a b imaxa imaxb = checkBound a imaxa && checkBound b imaxb

checkBound :: HLeafType -> Integer -> Bool
checkBound x imax = case x of
	NumberFrac x sf -> (abs (numerator x) < imax) && (denominator x < imax)
	NumberIrr r sf -> let x = iReal2Rat r in (abs (numerator x) < imax) && (denominator x < imax) -- NOTE: poor performance
	NumberNaN {} -> True
	HVar {} -> True

numberDefaultOpTotal :: (Rational -> Rational -> Rational) -> (MyIreal -> MyIreal -> MyIreal) -> HLeafType -> HLeafType -> HLeafType
numberDefaultOpTotal f fr ha hb = numberDefaultOp op opr ha hb
	where
	op a b = NumberFrac (f a b) (numberDefaultOpGetForm Nothing ha hb)
	opr a b = NumberIrr (fr a b) (numberDefaultOpGetForm (Just 10) ha hb)

numberFormNotFraction :: Maybe Integer -> Maybe Integer
numberFormNotFraction sf = case sf of
	Nothing -> Just 10
	other -> other

numberDefaultOpGetForm :: Maybe Integer -> HLeafType -> HLeafType -> Maybe Integer
numberDefaultOpGetForm d a b = case a of
	NumberFrac x xf -> xf
	NumberIrr x xf -> xf
	HVar {} -> case b of
		HVar {} -> d
		NumberNaN {} -> d
		NumberFrac y yf -> yf
		NumberIrr y yf -> yf
	NumberNaN {} -> case b of
		HVar {} -> d
		NumberNaN {} -> d
		NumberFrac y yf -> yf
		NumberIrr y yf -> yf

numberDefaultOp :: (Rational -> Rational -> HLeafType) -> (MyIreal -> MyIreal -> HLeafType) -> HLeafType -> HLeafType -> HLeafType
numberDefaultOp op opr a b =
	case a of
		NumberNaN {} -> NumberNaN
		HVar {} -> NumberNaN -- FIXME: something more clever
		NumberIrr x xb -> case b of
			NumberIrr y yf -> opr x y
			NumberFrac y yf -> opr x (fromRational y)
			NumberNaN {} -> NumberNaN
			HVar {} -> NumberNaN -- FIXME: something more clever
		NumberFrac a sf -> case b of
			NumberNaN {} -> NumberNaN
			HVar {} -> NumberNaN -- FIXME: something more clever
			NumberFrac b sf -> op a b
			NumberIrr b sf -> opr (fromRational a) b

stdBinaryNumRule :: (Rational -> Rational -> Maybe Integer -> Maybe HLeafType) -> String -> HPureSimplificationF
stdBinaryNumRule op = stdBinaryRule func
	where
	func x y = case x of
		(NumberFrac n sf) -> case y of
			(NumberFrac w sf2) -> op n w sf >>= Just . Leaf
			other -> Nothing
		other -> Nothing

stdBinaryMaybeIntRule :: (Integer -> Integer -> Maybe Integer) -> String -> HPureSimplificationF
stdBinaryMaybeIntRule op = stdBinaryNumRule func
	where
	func a b sf =
		if denominator a /= 1 || denominator b /= 1
		then Nothing
		else case op (numerator a) (numerator b) of
			Nothing -> Nothing
			Just x -> Just (NumberFrac (x % 1) sf)

stdBinaryIntRule :: (Integer -> Integer -> Integer) -> String -> HPureSimplificationF
stdBinaryIntRule op = stdBinaryMaybeIntRule (\ a b -> Just (op a b))

-- for operations like `shiftL' `shiftR' `rotL' `rotR'
stdBinaryArgumentedRule :: (Integer -> Int -> Integer) -> String -> HPureSimplificationF
stdBinaryArgumentedRule op = stdBinaryMaybeIntRule func
	where
	func a b = do
		bi <- maybeIntegerToNonnegativeInt b
		Just (op a bi)

stdUnaryNumRule :: (Rational -> Maybe Integer -> Maybe HLeafType) -> (MyIreal -> Maybe Integer -> Maybe HLeafType) -> String -> HPureSimplificationF
stdUnaryNumRule op opr = stdUnaryRule func
	where
	func x = case x of
		(NumberFrac n sf) -> op n sf >>= Just . Leaf
		(NumberIrr n sf) -> opr n sf >>= Just . Leaf
		other -> Nothing

stdNumberRule :: (HLeafType -> HLeafType -> HLeafType) -> String -> HTree -> Maybe HTree
stdNumberRule op name t = case t of
	Leaf x -> Nothing
	(Branch []) -> Nothing
	(Branch (x : rargs)) -> -- ASSUMPTION: x == name
		differentOrNothing failcase $ withOp Leaf treeToMaybeNum op failcase rargs
	where
	failcase = Leaf (patternElemReadUq name)
	treeToMaybeNum t = case t of
		Leaf l -> case l of
			HVar {} -> Nothing
			other -> Just l
		Branch {} -> Nothing

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
	-- loop :: Maybe HLeafType -> [Either HTree HLeafType] -> [HTree]
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

-------------------
-- GENERAL UTILS --
-------------------

trueLeaf :: (PatternElement a) => Tree a
trueLeaf = Leaf $ patternElemReadUq "True"

falseLeaf :: (PatternElement a) => Tree a
falseLeaf = Leaf $ patternElemReadUq "False"

funcRuleEqual :: (PatternElement a, Show a) => Maybe Integer -> [Tree a -> Maybe (Tree a)] -> [Tree a] -> Maybe (Tree a)
funcRuleEqual mlim simplifies args = case args of
	(x : xs) ->
		let simplifiedxs = map (sloop (applyFirstSimplificationF simplifies)) xs
		in let simplifiedx = sloop (applyFirstSimplificationF simplifies) x
			in if all (== simplifiedx) simplifiedxs
				then Just $ trueLeaf
				else Just $ falseLeaf
	(_) -> Nothing

	where
	sloop :: (PatternElement a, Show a) => (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
	sloop = maybe applySimplificationsUntil0LastF (applySimplificationsUntil0LastFLim) mlim

stdBinaryRule :: (PatternElement a) => (a -> a -> Maybe (Tree a)) -> String -> PureSimplificationF a
stdBinaryRule func = stdAnyNormalRule wrap
	where
	wrap args = case args of
		[x, y] -> func x y
		other -> Nothing

stdUnaryRule :: (PatternElement a) => (a -> Maybe (Tree a)) -> String -> PureSimplificationF a
stdUnaryRule func = stdAnyNormalRule wrap
	where
	wrap args = case args of
		[x] -> func x
		other -> Nothing

stdAnyNormalRule :: (PatternElement a) => ([a] -> Maybe (Tree a)) -> String -> PureSimplificationF a
stdAnyNormalRule func = stdAnyRule wrap
	where
	wrap simplifies args = do
		c <- collect [] args
		func c

	collect buf [] = Just (reverse buf)
	collect buf (x : xs)= case x of
		Branch {} -> Nothing
		Leaf x -> collect (x : buf) xs

stdConstant :: (PatternElement a) => (Tree a) -> String -> PureSimplificationF a
stdConstant constTree name = (patternElemReadUq name, wrap)
	where
	wrap simplifies t = case t of
		(Leaf name) -> Just constTree
		(Branch {}) -> Nothing

stdAnyRule :: (PatternElement a) => ([(Tree a) -> Maybe (Tree a)] -> [Tree a] -> Maybe (Tree a)) -> String -> PureSimplificationF a
stdAnyRule func name = (patternElemReadUq name, wrap)
	where
	wrap simplifies t = case t of
		(Branch (name : args)) -> func simplifies args
		(_) -> Nothing

applySimplificationsUntil0LastFLim :: (PatternElement a) => Integer -> (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
applySimplificationsUntil0LastFLim lim func t0 = loop 0 t0
	where
	loop n t =
		if n >= lim
		then t
		else case func t of
			Nothing -> t
			Just newt -> loop (n + 1) newt
