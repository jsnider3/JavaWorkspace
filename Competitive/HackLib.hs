{-
  arr_repl - Repeat the elements of arr n times
             and preserve order.
-}

arr_repl :: Int -> [Int] -> [Int]
arr_repl n arr = concat(map (replicate n) arr)

{-
  filter_less_than - Return the elements of arr less than n.
-}
filter_less_than :: Int -> [Int] -> [Int]
filter_less_than n [] = []
filter_less_than n arr = (if arr !! 0 < n then [arr !! 0] else [])
                           ++ filter_less_than n ( tail arr)
