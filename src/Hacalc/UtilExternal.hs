
module Hacalc.UtilExternal where

import Data.Maybe
import Data.Number.IReal
import Data.Number.IReal.IRealOperations
import Data.Number.IReal.IntegerInterval
import Data.Number.IReal.IReal
import PatterntCommonFrontend.All

iRealDefaultPrecision :: Integer
iRealDefaultPrecision = 2

iRealDefaultPrecisionI :: Int
iRealDefaultPrecisionI = fromIntegral iRealDefaultPrecision

iReal2RatP :: Int -> IReal -> Rational
iReal2RatP p x = case (readHRational $ showIReal p x) of -- TODO: improve performance-wise
	Just (r, b) -> r
	Nothing -> error "Impossible" -- ASSUMPTION: showIReal is always readable

iReal2Rat :: IReal -> Rational
iReal2Rat = iReal2RatP iRealDefaultPrecisionI

iRealCompareApprox :: IReal -> IReal -> Ordering
iRealCompareApprox x y =
	if lowerI dp >= 0 then GT
	else if upperI dp <= 0 then LT
	else EQ
	where dp = appr (x - y) iRealDefaultPrecisionI
