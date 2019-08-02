module Lib
	( someFunc
	) where

import Builtins
import Run

someFunc :: IO ()
someFunc = putStrLn "someFunc"
