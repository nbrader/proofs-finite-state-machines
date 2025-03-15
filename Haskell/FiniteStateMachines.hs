#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck --package containers-0.6.5.1

import Test.QuickCheck
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad

data FSM = FSM {}

main :: IO ()
main = do
    putStrLn "Test"
