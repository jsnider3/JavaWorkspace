{-
  arr_repl - Repeat the elements of arr n times
             and preserve order.
-}

arr_repl :: Int -> [Int] -> [Int]
arr_repl n arr = concat(map (replicate n) arr)-- Complete this function
