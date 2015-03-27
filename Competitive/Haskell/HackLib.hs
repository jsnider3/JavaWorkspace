import Data.List
import Data.Numbers.Primes

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
count_down 0 = [0]
count_down n = n : (count_down (n-1))

{-
  euler_exp - Estimate e^x
-}
euler_exp :: Double -> Double
euler_exp x = foldl (+) 0 (map (\y -> (x ** y)/ (foldl (*) 1 [1..y])) [0..9])

{-
  factors - prime factors of n
-}
factors :: Int -> [Int]
factors 1 = []
factors n = let divisors = (takeWhile (\x ->  x <= (isqrt (fromIntegral n))) primes) ++ [n] in
            let small_div = filter (\x -> rem n x == 0) divisors !! 0 in
              small_div : factors (n `div` small_div) 

{-
  filter_less_than - Return the elements of arr less than n.
-}
filter_less_than :: Int -> [Int] -> [Int]
filter_less_than n [] = []
filter_less_than n arr 
                   | arr !! 0 < n = [arr !! 0] ++ filter_less_than n ( tail arr)
                   | otherwise = filter_less_than n ( tail arr)

{-
  list_len - Manually find the length of lst.
-}
list_len :: [a] -> Int
list_len [] = 0
list_len lst = 1 + list_len (tail lst)

{-
  isqrt - integer sqrt 
-}
isqrt n = floor (sqrt n)

{-
  merge_list - Merge two lists into one, preserving order.
-}
merge_list :: [a] -> [a] -> [a]
merge_list p q = concat(map (\(x,y) -> [x, y]) (zip p q))

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
rev :: [a] -> [a]
rev l = map (\x -> l !! x)(count_down ((length l) -1))

{-
  rle - Encode an alpha string using run_length_encoding.
-}
rle :: String -> String
rle str = rle_loop (tail str) (head str, 1)

rle_loop :: (Eq a, Show a) => [a] -> (a, Int) -> String
rle_loop [] (el, cnt) = case cnt of
                          1 -> show el
                          _ -> show el ++ show cnt
rle_loop lst (el, cnt) = case (el == head lst) of
                          True -> rle_loop (tail lst) (el, cnt + 1)
                          False -> case cnt of 
                                    1 -> show el ++ rle_loop (tail lst) (head lst, 1)
                                    _ -> show el ++ show cnt ++ rle_loop (tail lst) (head lst, 1)

{-
  sum_odds - Sum the odd elements of a list
-}
sum_odds :: [Int] -> Int
sum_odds arr = foldl (+) 0 $ filter odd arr

{-
  uniq - Return the list with only uniq elems.
         Order is determined by which came first.
-}
uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\a b -> if elem b a then a else a ++ [b]) []

