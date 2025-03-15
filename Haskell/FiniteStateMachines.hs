#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck --package containers-0.6.5.1

import Test.QuickCheck
import Data.List (nub, foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad

data FSM symbol state = FSM { initState :: state, transition :: symbol -> state -> state, acceptingStates :: [state] }

runFSM :: Eq state => FSM symbol state -> [symbol] -> Bool
runFSM fsm input = (`elem` acceptingStates fsm) $ foldr (transition fsm) (initState fsm) input

-- additionFSM
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq)
data IsCarrying = IsCarrying | NotCarrying | Fail deriving (Eq)

additionFSM =
    FSM {
            initState = NotCarrying,
            transition = f,
            acceptingStates = [NotCarrying]
        }
  where f :: (Digit, Digit, Digit) -> IsCarrying -> IsCarrying
        f (d1,d2,result) Fail = Fail
        f (d1,d2,result) carry
            | digitVal d1 + digitVal d2 + carryVal carry == digitVal result      = NotCarrying
            | digitVal d1 + digitVal d2 + carryVal carry == digitVal result + 10 = IsCarrying
            | otherwise = Fail
        
          where digitVal :: Digit -> Integer
                digitVal D0 = 0
                digitVal D1 = 1
                digitVal D2 = 2
                digitVal D3 = 3
                digitVal D4 = 4
                digitVal D5 = 5
                digitVal D6 = 6
                digitVal D7 = 7
                digitVal D8 = 8
                digitVal D9 = 9
                
                carryVal :: IsCarrying -> Integer
                carryVal NotCarrying = 0
                carryVal IsCarrying  = 1

toFSMInput :: Integer -> Integer -> Integer -> [(Digit,Digit,Digit)]
toFSMInput inputNum1 inputNum2 proposedResult = zip3 ds1 ds2 ds3
    
  where (paddedXs, paddedYs, paddedZs) = padToMax '0' (show inputNum1) (show inputNum2) (show proposedResult)
        
        (ds1, ds2, ds3) = (map charToDigit paddedXs, map charToDigit paddedYs, map charToDigit paddedZs)

padToMax padChar xs ys zs = go ([],[],[]) (reverse xs, reverse ys, reverse zs)
          where go (xs', ys', zs') ([], [], [])       = (xs', ys', zs')
                go (xs', ys', zs') (x:xs, [], [])     = go (x      :xs', padChar:ys', padChar:zs') (xs, [], [])
                go (xs', ys', zs') ([], y:ys, [])     = go (padChar:xs', y      :ys', padChar:zs') ([], ys, [])
                go (xs', ys', zs') ([], [], z:zs)     = go (padChar:xs', padChar:ys', z      :zs') ([], [], zs)
                go (xs', ys', zs') (x:xs, y:ys, [])   = go (x      :xs', y      :ys', padChar:zs') (xs, ys, [])
                go (xs', ys', zs') (x:xs, [], z:zs)   = go (x      :xs', padChar:ys', z      :zs') (xs, [], zs)
                go (xs', ys', zs') ([], y:ys, z:zs)   = go (padChar:xs', y      :ys', z      :zs') ([], ys, zs)
                go (xs', ys', zs') (x:xs, y:ys, z:zs) = go (x      :xs', y      :ys', z      :zs') (xs, ys, zs)

charToDigit :: Char -> Digit
charToDigit '0' = D0
charToDigit '1' = D1
charToDigit '2' = D2
charToDigit '3' = D3
charToDigit '4' = D4
charToDigit '5' = D5
charToDigit '6' = D6
charToDigit '7' = D7
charToDigit '8' = D8
charToDigit '9' = D9


main :: IO ()
main = do
    
    let inputNum1 = 51125478635350146540600
        inputNum2 = 64586789783213684604684684068
        proposedResult = inputNum1 + inputNum2
        
        fsmInput = toFSMInput inputNum1 inputNum2 proposedResult
        accept = runFSM additionFSM fsmInput
    
    print $ padToMax '0' (show inputNum1) (show inputNum2) (show proposedResult)
    
    putStrLn "inputNum1"
    print inputNum1
    putStrLn ""
    
    putStrLn "inputNum2"
    print inputNum2
    putStrLn ""
    
    putStrLn "proposedResult"
    print proposedResult
    putStrLn ""
    
    print $ if accept
             then "Accept"
             else "Reject"
