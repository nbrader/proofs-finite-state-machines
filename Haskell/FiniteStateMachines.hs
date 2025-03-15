#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck --package containers-0.6.5.1

import Test.QuickCheck
import Data.List (nub, foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Monad

data FSM symbol state = FSM { initState :: state, transition :: symbol -> state -> state, acceptingStates :: [state] }

runFSM :: Eq state => FSM symbol state -> [symbol] -> Bool
runFSM fsm input = (`elem` acceptingStates fsm) $ foldr (transition fsm) (initState fsm) input

-- additionFSM
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq, Show)
data IsCarrying = IsCarrying | NotCarrying | Fail deriving (Eq, Show)

additionFSM :: FSM (Digit, Digit, Digit) IsCarrying
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

toAdditionFSMInput :: Integer -> Integer -> Integer -> [(Digit,Digit,Digit)]
toAdditionFSMInput inputNum1 inputNum2 proposedResult = zip3 ds1 ds2 ds3
    
  where (paddedXs, paddedYs, paddedZs) = padToMax3OnLeft '0' (show inputNum1) (show inputNum2) (show proposedResult)
        
        (ds1, ds2, ds3) = (map charToDigit paddedXs, map charToDigit paddedYs, map charToDigit paddedZs)

padToMax3OnLeft padChar xs ys zs = go ([],[],[]) (reverse xs, reverse ys, reverse zs)
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


additionFSMExample :: IO ()
additionFSMExample = do
    
    let inputNum1 = 51125478635350146540600
        inputNum2 = 64586789783213684604684684068
        proposedResult = inputNum1 + inputNum2
        
        fsmInput = toAdditionFSMInput inputNum1 inputNum2 proposedResult
        accept = runFSM additionFSM fsmInput
    
    -- print fsmInput
    
    putStrLn "inputNum1"
    print inputNum1
    putStrLn ""
    
    putStrLn "inputNum2"
    print inputNum2
    putStrLn ""
    
    putStrLn "proposedResult"
    print proposedResult
    putStrLn ""
    
    putStrLn $ if accept
             then "Accept"
             else "Reject"
    putStrLn ""
    
    putStrLn ""

-- prefixFSM

prefixFSM :: FSM (Maybe Char, Maybe Char) Bool
prefixFSM =
    FSM {
            initState = True,
            transition = f,
            acceptingStates = [True]
        }
  where f :: (Maybe Char, Maybe Char) -> Bool -> Bool
        f (c1,c2) False = False
        f (c1,c2) notYetFailed
            | c1 == c2 = True
            | isNothing c1 = True
            | isNothing c2 = True
            | otherwise = False

toPrefixFSMInput :: String -> String -> [(Maybe Char, Maybe Char)]
toPrefixFSMInput input1 input2 = zip paddedXs paddedYs
    
  where (paddedXs, paddedYs) = padWithNothingToMax2OnRight input1 input2

padWithNothingToMax2OnRight xs ys = go ([],[]) (xs, ys)
          where go (xs', ys') ([], [])     = (xs', ys')
                go (xs', ys') (x:xs, [])   = go (Just x :xs', Nothing:ys') (xs, [])
                go (xs', ys') ([], y:ys)   = go (Nothing:xs', Just y :ys') ([], ys)
                go (xs', ys') (x:xs, y:ys) = go (Just x :xs', Just y :ys') (xs, ys)

prefixFSMExample :: IO ()
prefixFSMExample = do
    
    let input1 = "51125478635350146540600"
        input2 = take 5 input1
        
        fsmInput = toPrefixFSMInput input1 input2
        accept = runFSM prefixFSM fsmInput
    
    -- print fsmInput
    
    putStrLn "input1"
    print input1
    putStrLn ""
    
    putStrLn "input2"
    print input2
    putStrLn ""
    
    putStrLn $ if accept
             then "Accept"
             else "Reject"
    putStrLn ""
    
    putStrLn ""

main :: IO ()
main = do
    additionFSMExample
    prefixFSMExample