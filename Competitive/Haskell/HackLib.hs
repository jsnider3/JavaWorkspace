{-
  arr_repl - Repeat the elements of arr n times
             and preserve order.
-}

arr_repl :: Int -> [Int] -> [Int]
arr_repl n arr = concat(map (replicate n) arr)

{-
  count_down - Create a list counting down from n.
-}
count_down :: Int -> [Int]
count_down 0 = []
count_down n = n : (count_down (n-1))

{-
  euler_exp - Estimate e^x
-}
euler_exp :: Double -> Double
euler_exp x = foldl (+) 0 (map (\y -> (x ** y)/ (foldl (*) 1 [1..y])) [0..9])

{-
  filter_less_than - Return the elements of arr less than n.
-}
filter_less_than :: Int -> [Int] -> [Int]
filter_less_than n [] = []
filter_less_than n arr = (if arr !! 0 < n then [arr !! 0] else [])
                           ++ filter_less_than n ( tail arr)


{-
  list_len - Manually find the length of lst.
-}
list_len :: [a] -> Int
list_len [] = 0
list_len lst = 1 + list_len (tail lst)


{-
  odd_indices - Get the elements at the 1st, 3rd, 5th positions
                of a list.
-}
odd_indices :: [Int] -> [Int]
odd_indices [] = []
odd_indices [a] = []
odd_indices lst = [lst !! 1] ++ odd_indices (tail (tail lst))

{-
  rev - Reverse a list manually.
-}
rev [] = []
rev l = rev (tail l) ++ [head l]

{-
  sum_odds - Sum the odd elements of a list
-}
sum_odds arr = foldl (+) 0 $ filter odd arr
