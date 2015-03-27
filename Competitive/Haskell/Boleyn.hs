import qualified Data.Map as Map
import Data.Maybe
import Data.List
import System.IO.Unsafe

createHierarchy :: Int -> IO (Map.Map Int [Int])
createHierarchy n = do
  links <- mapM (const getLine) [1..n - 1]
  let supes = map makeIntList (map words links) in do
    return $ edgesToTree(zip (map head supes)(map (\x -> x !! 1) supes))

descendents tree root = case (Map.lookup root tree) of
                          (Just a) -> a ++ concat(map (descendents tree) a)
                          _ -> []

edgesToTree :: (Ord a) => [(a, a)] -> (Map.Map a [a])
edgesToTree [] = Map.empty
edgesToTree ((a, b):rest) = Map.insertWith (++) b [a] (edgesToTree rest)

loadSalaries :: IO (Map.Map Int Int)
loadSalaries = do
  line <- getLine
  let dat = makeIntList (words line) in
    let sals = Map.fromList(zip [1, 2..] dat) in
      return sals

makeIntList :: [String] -> [Int]
makeIntList = map (\x -> read x::Int)

processQueries :: Map.Map Int [Int] -> Map.Map Int Int -> [[Int]] -> [Int]
processQueries tree sals [[a, b]] = [sortByMap sals (descendents tree a) !! (b - 1)]
processQueries tree sals ([a, b]:[x,y]:rest) = 
  let hd = sortByMap sals (descendents tree a) !! (b - 1) in
    hd:(processQueries tree sals ([hd+x,y]:rest))

printQueries :: Map.Map Int [Int] -> Map.Map Int Int -> Int -> IO [()]
printQueries tree sals n = do
--  print tree
--  print sals
  stuff <- mapM (const getLine) [1..n]
  let input = map makeIntList (map words stuff) in do
    mapM print $ processQueries tree sals input

sortByMap vals keys = sortBy (\x y -> compare (fromJust $Map.lookup x vals)(fromJust $Map.lookup y vals)) keys

main :: IO [()]
main = do
  nums <- getLine
  tree <- createHierarchy (read(head(words nums))::Int)
  sals <- loadSalaries
  printQueries tree sals (read((words nums) !! 1)::Int)

