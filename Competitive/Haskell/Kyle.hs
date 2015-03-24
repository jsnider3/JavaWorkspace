import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

highOrder :: Int -> Int
highOrder n = getHighOrder n 0

getHighOrder :: Int -> Int -> Int
getHighOrder n shift = case (shift, testBit n  (63 - shift)) of
                        (64, _) -> (-1)
                        (_, False) -> getHighOrder n (shift + 1)
                        _   -> (63 - shift)

lowOrder :: Int -> Int
lowOrder n = getLowOrder n 0

getLowOrder :: Int -> Int -> Int
getLowOrder n shift = case (shift, testBit n shift) of
                        (64, _) -> (-1)
                        (_, False) -> getLowOrder n (shift + 1)
                        _   -> (shift)

winning :: Map Int Int -> Bool
winning pos = undefined {-case (highOrder pos - lowOrder pos) of
                0 -> True
                1 -> True
                _ -> not $ all winning (children pos)-}

findRuns :: [Int] -> Int -> Map Int Int -> Map Int Int
findRuns [] _ dict = dict
findRuns lst cnt dict = case (head lst, cnt) of
                          (1, _) -> findRuns (tail lst) (cnt + 1) dict  
                          (0, 0) -> findRuns (tail lst) (cnt) dict
                          _      -> findRuns (tail lst) 0 (Map.insertWith (+) cnt 1 dict)

children :: Map Int Int -> Map Int Int
--This equation is fundamentally wrong.
children pos = undefined --concat [knockdownOnes pos, knockdownTwos pos]

knockdownOnes :: Int -> [Int]
knockdownOnes n = catMaybes(map (knockdownOne n) [0..10])

knockdownOne n x = case testBit n x of
                        True -> Just (clearBit n x)
                        False -> Nothing

knockdownTwos :: Int -> [Int]
knockdownTwos n = catMaybes(map (knockdownTwo n) [0..10])

knockdownTwo n x = case and [testBit n x, testBit n (x-1)] of
                        True -> Just (clearBit (clearBit n x) x-1)
                        False -> Nothing

binaryListToInt :: [Int] -> Int
binaryListToInt = foldl1 (\ a b -> 2*a + b) 

testcase :: Int -> IO ()
testcase 0 = return ()
testcase n = do
         leng <- getLine
         test <- getLine
         let nums = map (\c -> if c=='X' then 0 else 1) test in
          case winning (findRuns nums 0 Map.empty) of
            True -> putStrLn "WIN"
            False -> putStrLn "LOSE"
         testcase (n-1)

main = do
  numtests <- getLine
  testcase (read numtests::Int)
