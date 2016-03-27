import Control.Monad
import Data.List
import qualified Data.Map as Map

lookupEx k mp = fromJust (Map.lookup k mp)

merge m n = m ++ n

recruit soldier ls = soldier:ls)

removeStrongest ls = init (sort ls)

strongest :: [Int] -> Int
strongest = maximum

processQueries :: [String] -> Map.Map Int [Heap] -> [String]
processQueries [] _ = []
processQueries (quer:quers) armies = case words quer of
    ["1", ind] -> show (strongest (lookupEx (read ind) armies)) : processQueries quers armies
    ["2", ind] -> processQueries quers (Map.adjust removeStrongest (read ind) armies)
    ["3", ind, skill] -> processQueries quers (Map.adjust (recruit (read skill)) (read ind) armies)
    ["4", first, second] -> processQueries quers
        (Map.adjust (merge (lookupEx (read second) armies)) (read first) armies)
    _ -> show armies : processQueries quers armies

main = do
    ln <- getLine
    queries <- replicateM (read((words ln) !! 1)) getLine
    let zip_army = zip [1..read((words ln) !! 0)] (repeat []) in 
        mapM putStrLn (processQueries queries (Map.fromList zip_army))
