import Control.Monad
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

{-
  Slowest test case takes me 30 times the cutoff to do.
  On the plus side, the results are 100% correct.
  Currently, my code is bottlenecked by RAM.
-}
addLeaves :: (Ord a) => (Map.Map a (Set.Set a)) -> [a] -> (Map.Map a (Set.Set a))
addLeaves a [] = a
addLeaves a (n:nodes) = addLeaves (Map.insertWith Set.union n Set.empty a) nodes

createHierarchy :: [(Int, Int)] -> Map.Map Int (Set.Set Int)
createHierarchy edges = 
    addLeaves (edgesToTree(zip (map fst edges) (map snd edges))) (map fst edges)

{-
  Represent a graph as an adjacency list given a list of (src, dest) pairs.
-}
edgesToTree :: (Ord a) => [(a, a)] -> (Map.Map a (Set.Set a))
edgesToTree [] = Map.empty
edgesToTree ((a, b):rest) = Map.insertWith (Set.union) b (Set.singleton a) (edgesToTree rest)

isleaf :: (Ord a, Eq b) => Map.Map a (Set.Set b) -> a -> Bool
isleaf orgChart emp = fromJust (Map.lookup emp orgChart) == Set.empty

{-
  Read a list of strings to a list of ints.
-}
makeIntList :: [String] -> [Int]
makeIntList = map read

makeSalmap :: Map.Map Int (Set.Set Int) -> [Int] -> (Int -> Int) -> Map.Map Int [Int] -> Map.Map Int [Int]
makeSalmap _ [] _ s = s
makeSalmap orgChart (emp:emps) sals salmap =
  let directs = sortOn sals (Set.toList (fromJust (Map.lookup emp orgChart)))
      indirects = map (\i -> fromJust (Map.lookup i salmap)) directs
      merged = merge2 sals (directs:indirects) in
    makeSalmap orgChart emps sals (Map.insert emp merged salmap)

{-
  Call makeSalmap and convert it to a better performing data structure.
-}
makeVectorSalmap :: Map.Map Int (Set.Set Int) -> [Int] -> (Int -> Int) -> Map.Map Int [Int] -> Map.Map Int (UVector.Vector Int)
makeVectorSalmap orgChart emps sals salmap =
  Map.map UVector.fromList (makeSalmap orgChart emps sals salmap)

{-
  Merge n sorted sets into one given the key function.
-}
merge2 _ [] = []
merge2 _ [a] = a
merge2 key ls = merge key (merge2 key (take (length ls `quot` 2) ls))
                          (merge2 key (drop (length ls `quot` 2) ls))

{-
  Merge two sorted sets into one given the key function.
-}
merge _ [] b = b
merge _ a [] = a
merge key (a:as) (b:bs) = case (a == b, key a < key b) of
  (True, _) -> a : merge key as bs
  (_, True) -> a : merge key as (b:bs)
  (_, False) -> b : merge key (a:as) bs

readEdges :: [String] -> [(Int, Int)]
readEdges lines = map (tuplify2 . makeIntList . words) lines

{-
  Sort a list with the given value function.
-}
sortOn :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOn f ls = map snd (sort (map (\i -> (f i, i)) ls))

nub_ seen [] = []
nub_ seen (a:as) = case Set.member a seen of
  True -> nub_ seen as
  False -> a : (nub_ (Set.insert a seen) as)

toposort_dfs :: Map.Map Int (Set.Set Int) -> [Int]
toposort_dfs orgChart =
  let roots = Set.difference (Set.fromList (Map.keys orgChart)) (Map.foldl Set.union Set.empty orgChart) in
    (reverse.nub_ Set.empty.concat) (map (dfs orgChart) (Set.toList roots))

dfs :: Map.Map Int (Set.Set Int) -> Int -> [Int]
dfs orgChart source = case Map.lookup source orgChart of
  Nothing -> []
  Just a -> source:(Set.toList a ++ concat (map (dfs orgChart) (Set.toList a)))

{-
  Convert the adjacency list to tree form.
-}
treeToArray :: Int -> (Map.Map Int [Int]) -> (Vector.Vector [Int])
treeToArray n a = Vector.fromList (map snd $ sort $ Map.assocs a ++ zip (filter (\x -> Map.notMember x a)[1..n])(repeat []))

tuplify2 [a,b] = (a, b)

parseSalaries :: String -> Vector.Vector Int
parseSalaries line = 
  let dat = makeIntList (words line) in
    Vector.fromList dat

processQueries :: Map.Map Int (UVector.Vector Int) -> [[Int]] -> [Int]
processQueries sortdecs [[a, b]] = [(fromJust(Map.lookup a sortdecs)) UVector.! (b - 1)]
processQueries sortdecs ([a, b]:[x,y]:rest) =
  let hd = (fromJust(Map.lookup a sortdecs)) UVector.! (b - 1) in
    hd:(processQueries sortdecs ([hd + x, y]:rest))

main :: IO ()
main = do
  nums <- getLine
  links <- replicateM (read(head (words nums)) - 1) getLine
  sals <- getLine
  let edges = readEdges links
      tree = createHierarchy edges
      num_quers = read((words nums) !! 1)
      salaries = parseSalaries sals in do
    input <- replicateM num_quers getLine
    let quers = map (makeIntList . words) input
        salmap = makeVectorSalmap tree (toposort_dfs tree) (\x -> salaries Vector.! (x - 1)) Map.empty in do
      mapM_ print (processQueries salmap quers)
