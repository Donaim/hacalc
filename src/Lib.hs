module Lib
	( someFunc
	) where

import Types
import Builtins
import Run
import Parser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
