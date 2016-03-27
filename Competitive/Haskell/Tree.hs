import Control.Monad

data Tree = Node Int [Tree] deriving Show

change (Node val kids) [] num = Node num kids
change (Node val kids) (p:ps) num = Node val (take p kids ++ [change (kids !! p) ps num] ++ drop (p + 1) kids)

delete (Node val kids) [p] = Node val (take p kids ++ drop (p + 1) kids)
delete (Node val kids) (p:ps) = Node val (take p kids ++ [delete (kids !! p) ps] ++ drop (p + 1) kids)

getVal (Node v _) [] = show v
getVal (Node _ kids) (x:xs) = getVal (kids !! x) xs

insertChild (Node num kids) [] v = Node num (Node v [] : kids)
insertChild (Node num kids) (p:ps) v = Node num ((take p kids) ++ [insertChild (kids !! p) ps v] ++ (drop (p + 1) kids))

insertLeft (Node num kids) [p] v = Node num ((take p kids) ++ [Node v []] ++ (drop p kids))
insertLeft (Node num kids) (p:ps) v = Node num ((take p kids) ++ [insertLeft (kids !! p) ps v] ++ (drop (p + 1) kids))

insertRight (Node num kids) [p] v = Node num ((take (p + 1) kids) ++ [Node v []] ++ (drop (p + 1) kids))
insertRight (Node num kids) (p:ps) v = Node num ((take p kids) ++ [insertRight (kids !! p) ps v] ++ (drop (p + 1) kids))

moveLeft [p] = [p+1]
moveLeft (p:ps) = p:(moveLeft ps)

visitChild path ind = (path ++ [ind])

visitLeft [p] = [p-1]
visitLeft (p:ps) = p:(visitLeft ps)

visitRight [p] = [p+1]
visitRight (p:ps) = p:(visitRight ps)

visitParent = init

processQueries :: [String] -> Tree -> [Int] -> [String]
processQueries [] _ _ = []
processQueries (quer:quers) root path = case words quer of
    ["change", val] -> processQueries quers (change root path (read val)) path
    ["insert", "child", val] -> processQueries quers (insertChild root path (read val)) path
    ["visit", "child", val] ->  processQueries quers root (visitChild path (read val - 1))
    ["insert", "left", val] -> processQueries quers (insertLeft root path (read val)) (moveLeft path)
    ["visit", "left"] -> processQueries quers root (visitLeft path)
    ["insert", "right", val] -> processQueries quers (insertRight root path (read val)) path
    ["visit", "right"] -> processQueries quers root (visitRight path)
    ["visit", "parent"] -> processQueries quers root (visitParent path)
    ["delete"] -> processQueries quers (delete root path) (visitParent path)
    ["print"] -> (getVal root path) : processQueries quers root path
    _ -> show root : processQueries quers root path

main = do
    ln <- getLine
    queries <- replicateM (read ln) getLine
    mapM putStrLn (processQueries queries (Node 0 []) [])
