import qualified Data.Map as Map
import System.IO.Unsafe

create_hierarchy :: Int -> IO (Map.Map Int [Int])
create_hierarchy n = do
  links <- mapM (const getLine) [1..n - 1]
  let supes = map makeIntList (map words links) in do
    return $ edgesToTree(zip (map head supes)(map (\x -> x !! 1) supes))

edgesToTree :: [(a, a)] -> (Map.Map a [a])
edgesToTree [] = Map.empty
edgesToTree ((a,b):rest) = Map.insertWith (++) b [a] (edgesToTree rest)

load_salaries :: IO (Map.Map Int Int)
load_salaries = do
  line <- getLine
  let dat = makeIntList (words line) in
    let sals = Map.fromList(zip [1, 2..] dat) in
      return sals

makeIntList :: [String] -> [Int]
makeIntList = map (\x -> read x::Int)

main :: IO ()
main = do
  nums <- getLine
  tree <- create_hierarchy (read(head(words nums))::Int)
  print tree
  sals <- load_salaries
  print sals
