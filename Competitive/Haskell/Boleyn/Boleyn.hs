import Control.Monad
import Data.Array.IArray
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

{-
  Slowest test case takes me 30 times the cutoff to do.
  This almost certainly means that I need a better data
  structure.
  On the plus side, the results are 100% correct.
-}
createHierarchy :: [String] -> Map.Map Int (Set.Set Int)
createHierarchy links = 
  let supes = map makeIntList (map words links)
      dests = map (\x -> x !! 1) supes in
    addLeaves (edgesToTree(zip (map head supes)dests)) (map head supes)

isleaf :: Map.Map Int (Set.Set Int) -> Int -> Bool
isleaf orgChart emp = fromJust (Map.lookup emp orgChart) == Set.empty

toposort :: Map.Map Int (Set.Set Int) -> [Int]
toposort orgChart
  | Map.null orgChart = []
  | otherwise = let leaves = filter (isleaf orgChart) (Map.keys orgChart)
                    prunedOrg = Map.filterWithKey (\k _ -> not (isleaf orgChart k)) orgChart 
                    prunedOrg2 = Map.map (\v -> Set.difference v (Set.fromList leaves)) prunedOrg in
    leaves ++ (toposort prunedOrg2) 

makeSalmap :: Map.Map Int (Set.Set Int) -> [Int] -> (Int -> Int) -> Map.Map Int [Int] -> Map.Map Int [Int]
makeSalmap _ [] _ s = s
makeSalmap orgChart (emp:emps) sals salmap =
  let directs = sortOn sals (Set.toList (fromJust (Map.lookup emp orgChart)))
      indirects = map (\i -> fromJust (Map.lookup i salmap)) directs
      merged = merge2 sals (directs:indirects) in
    makeSalmap orgChart emps sals (Map.insert emp merged salmap)

merge2 _ [] = []
merge2 _ [a] = a
merge2 key ls = merge key (merge2 key (take (length ls `quot` 2) ls))
                          (merge2 key (drop (length ls `quot` 2) ls))

merge _ [] b = b
merge _ a [] = a
merge key (a:as) (b:bs) = case (a == b, key a < key b) of
  (True, _) -> a : merge key as bs
  (_, True) -> a : merge key as (b:bs)
  (_, False) -> b : merge key (a:as) bs

{-
  Get the subtree in tree anchored at the given root.
-}
descendents :: Array Int [Int] -> Int -> [Int]
descendents tree root = case (tree ! root ) of
                          [] -> []
                          a -> a ++ concat(map (descendents tree) a)

sortedDescendents :: Array Int [Int] -> Int -> Array Int Int -> [Int]
sortedDescendents tree root sals = sortOn (sals!) (descendents tree root)

sortOn :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOn f ls = map snd (sort (map (\i -> (f i, i)) ls))

addLeaves :: (Ord a) => (Map.Map a (Set.Set a)) -> [a] -> (Map.Map a (Set.Set a))
addLeaves a [] = a
addLeaves a (n:nodes) = addLeaves (Map.insertWith Set.union n Set.empty a) nodes

{-
  Represent a graph as an adjacency list given a list of (src, dest) pairs.
  TODO Add leaves to tree.
-}
edgesToTree :: (Ord a) => [(a, a)] -> (Map.Map a (Set.Set a))
edgesToTree [] = Map.empty
edgesToTree ((a, b):rest) = Map.insertWith (Set.union) b (Set.singleton a) (edgesToTree rest)

{-
  Convert the adjacency list to tree form.
-}
treeToArray :: Int -> (Map.Map Int [Int]) -> (Array Int [Int])
treeToArray n a = array (1, n)(Map.assocs a ++ zip (filter (\x -> Map.notMember x a)[1..n])(repeat []))

parseSalaries :: String -> Array Int Int
parseSalaries line = 
  let dat = makeIntList (words line) in
    array (1, length dat)(zip [1..] dat)

{-
  Read a list of strings to a list of ints.
-}
makeIntList :: [String] -> [Int]
makeIntList = map read

processQueries :: Map.Map Int [Int] -> [[Int]] -> [Int]
processQueries sortdecs [[a, b]] = [(fromJust(Map.lookup a sortdecs)) !! (b - 1)]
processQueries sortdecs ([a, b]:[x,y]:rest) =
  let hd = (fromJust(Map.lookup a sortdecs)) !! (b - 1) in
    hd:(processQueries sortdecs ([hd + x, y]:rest))

printQueries :: Map.Map Int (Set.Set Int) -> Array Int Int -> Int -> IO ()
printQueries tree sals n = do
  input <- replicateM n getLine
  let quers = map makeIntList (map words input) in do
    mapM_ print (processQueries (makeSalmap tree (toposort tree) (sals!) Map.empty) quers)

main :: IO ()
main = do
  nums <- getLine
  links <- replicateM (read(head (words nums)) - 1) getLine
  sals <- getLine
  printQueries (createHierarchy links) (parseSalaries sals) (read((words nums) !! 1)::Int)

