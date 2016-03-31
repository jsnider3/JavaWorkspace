import Control.Monad
import Data.Array.IArray
import Data.List
import qualified Data.Map as Map
import Data.Maybe

{-
  Slowest test case takes me 30 times the cutoff to do.
  This almost certainly means that I need a better data
  structure.
  On the plus side, the results are 100% correct.
-}
createHierarchy :: Int -> IO (Array Int [Int])
createHierarchy n = do
  links <- replicateM (n - 1) getLine
  let supes = map makeIntList (map words links) in do
    return $ treeToArray n $ edgesToTree(zip (map head supes)(map (\x -> x !! 1) supes))

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

{-
  Represent a graph as an adjacency list given a list of (src, dest) pairs.
-}
edgesToTree :: (Ord a) => [(a, a)] -> (Map.Map a [a])
edgesToTree [] = Map.empty
edgesToTree ((a, b):rest) = Map.insertWith (++) b [a] (edgesToTree rest)

{-
  Convert the adjacency list to tree form.
-}
treeToArray :: Int -> (Map.Map Int [Int]) -> (Array Int [Int])
treeToArray n a = array (1, n)(Map.assocs a ++ zip (filter (\x -> Map.notMember x a)[1..n])(repeat []))

loadSalaries :: IO (Array Int Int)
loadSalaries = do
  line <- getLine
  let dat = makeIntList (words line) in
    let sals = array (1, length dat)(zip [1..] dat) in do
      return sals
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

printQueries :: Array Int [Int] -> Array Int Int -> Int -> IO [()]
printQueries tree sals n = do
  stuff <- mapM (const getLine) [1..n]
  let input = map makeIntList (map words stuff) in do
    mapM print $ processQueries (Map.fromList
      (map (\i -> (i, sortedDescendents tree i sals)) (indices tree))) input

main :: IO [()]
main = do
  nums <- getLine
  tree <- createHierarchy (read(head(words nums))::Int)
  sals <- loadSalaries
  printQueries tree sals (read((words nums) !! 1)::Int)

