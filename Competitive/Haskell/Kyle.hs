import Data.Bits
import Debug.Trace

highOrder :: Int -> Int
highOrder n = getHighOrder n 0

getHighOrder :: Int -> Int -> Int
getHighOrder n shift = case (shift, testBit n  (63 - shift))) of
                        (64, _) -> (-1)
                        (_, 0) -> getHighOrder n (shift + 1)
                        _   -> (63 - shift)

lowOrder :: Int -> Int
lowOrder n = getLowOrder n 0

getLowOrder :: Int -> Int -> Int
getLowOrder n shift = case (shift, testBit n  shift) of
                        (64, _) -> (-1)
                        (_, 0) -> getLowOrder n (shift + 1)
                        _   -> (shift)

winning :: Int -> Bool
winning pos = case (highOrder pos - lowOrder pos) of
                0 -> True
                1 -> True
                _ -> not $ all winning (children pos)

children :: Int -> [Int]
--This equation is fundamentally wrong.
children pos = concat [knockdownOnes pos, knockdownTwos pos]

knockdownOnes :: Int -> [Int]
knockdownOnes n = concat(map (\x -> if testBit n x then [clearBit n x] else []) [0..10])

knockdownTwos :: Int -> [Int]
knockdownTwos n = concat(map (\x -> if and[testBit n x, testBit n (x-1)] then [clearBit (n-1) (clearBit n x)] else []) [0..10])

binaryListToInt :: [Int] -> Int
binaryListToInt = foldl1 (\ a b -> 2*a + b) 

testcase :: Int -> IO ()
testcase 0 = return ()
testcase n = do
         leng <- getLine
         test <- getLine
         let nums = map (\c -> if c=='X' then 0 else 1) test in
          case winning (binaryListToInt nums) of
            True -> putStrLn "WIN"
            False -> putStrLn "LOSE"
         testcase (n-1)

main = do
  numtests <- getLine
  testcase (read numtests::Int)
