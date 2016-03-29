import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map

merge m n = case m of
    Just a -> sortedMerge a n
    Nothing -> n

readInt :: String -> Int
readInt = read

recruit :: Int -> [Int] -> [Int]
recruit soldier ls = sortedMerge [soldier] ls

removeStrongest :: [Int] -> [Int]
removeStrongest [] = []
removeStrongest ls = tail ls

sortedMerge :: [Int] -> [Int] -> [Int]
sortedMerge [] b = b
sortedMerge a [] = a
sortedMerge (a:as) (b:bs) = case a > b of
    True  -> a : (sortedMerge as (b:bs))
    False -> b : (sortedMerge (a:as) bs)

strongest :: Maybe [Int] -> Int
strongest a = case a of
    Just [] -> 0
    Just ls -> head ls
    Nothing -> 0

processQueries :: [String] -> Map.Map Int [Int] -> [String]
processQueries [] _ = []
processQueries (quer:quers) armies = case words quer of
    ["1", ind] -> show (strongest (Map.lookup (readInt ind) armies)) : processQueries quers armies
    ["2", ind] -> processQueries quers (Map.adjust removeStrongest (readInt ind) armies)
    ["3", ind, skill] -> processQueries quers (Map.adjust (recruit (readInt skill)) (readInt ind) armies)
    ["4", first, second] -> processQueries quers
        (Map.adjust (merge (Map.lookup (readInt second) armies)) (readInt first) armies)
    _ -> show armies : processQueries quers armies

main = do
    ln <- getLine
    queries <- replicateM (readInt((words ln) !! 1)) getLine
    let zip_army = zip [1..readInt((words ln) !! 0)] (repeat []) in 
        mapM putStrLn (processQueries queries (Map.fromList zip_army))
