import Data.Array.IArray
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import System.IO.Unsafe

{-
  Slowest test case takes me 30 times the cutoff to do.
  This almost certainly means that I need a better data
  structure.
  On the plus side, the results are 100% correct.
-}

createHierarchy :: Int -> IO (Array Int [Int])
createHierarchy n = do
  links <- mapM (const getLine) [1..n - 1]
  let supes = map makeIntList (map words links) in do
    return $ treeToArray n $ edgesToTree(zip (map head supes)(map (\x -> x !! 1) supes))

descendents tree root = case (tree ! root ) of
                          [] -> []
                          a -> a ++ concat(map (descendents tree) a)

edgesToTree :: (Ord a) => [(a, a)] -> (Map.Map a [a])
edgesToTree [] = Map.empty
edgesToTree ((a, b):rest) = Map.insertWith (++) b [a] (edgesToTree rest)

treeToArray :: Int -> (Map.Map Int [Int]) -> (Array Int [Int])
treeToArray n a = array (1, n)(Map.assocs a ++ zip (filter (\x -> Map.notMember x a)[1..n])(repeat []))

loadSalaries :: IO (Array Int Int)
loadSalaries = do
  line <- getLine
  let dat = makeIntList (words line) in
    let sals = array (1, length dat)(zip [1,2..] dat) in do
      return sals

makeIntList :: [String] -> [Int]
makeIntList = map (\x -> read x::Int)

processQueries :: Array Int [Int] -> Array Int Int -> [[Int]] -> [Int]
processQueries tree sals [[a, b]] = [quickSelectWithFunc (\x -> sals ! x) (descendents tree a) (b - 1)]
processQueries tree sals ([a, b]:[x,y]:rest) = 
  let hd = quickSelectWithFunc (\x -> sals ! x) (descendents tree a) (b - 1) in
    hd:(processQueries tree sals ([hd + x, y]:rest))

printQueries :: Array Int [Int] -> Array Int Int -> Int -> IO [()]
printQueries tree sals n = do
  stuff <- mapM (const getLine) [1..n]
  let input = map makeIntList (map words stuff) in do
    mapM print $ processQueries tree sals input

quickSelectWithFunc :: (Ord b) => (a-> b) -> [a] -> Int -> a
quickSelectWithFunc foo (x:xs) ind 
                     | ind < l     = quickSelectWithFunc foo ys ind
                     | ind > l     = quickSelectWithFunc foo zs (ind - l - 1)
                     | otherwise = x
  where (ys, zs) = partition (\n -> foo n < foo x) xs
        l = length ys

main :: IO [()]
main = do
  nums <- getLine
  tree <- createHierarchy (read(head(words nums))::Int)
  sals <- loadSalaries
  printQueries tree sals (read((words nums) !! 1)::Int)

