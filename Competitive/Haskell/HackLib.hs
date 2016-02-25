import Data.Ix
import Data.List
import Data.Numbers.Primes

{-
  area_under_curve - Get the area of a curve, using an
    integral and the left and right bound.
-}
area_under_curve :: [(Double, Double)] -> Int -> Int -> Double
area_under_curve integral left right =
    sum (map (\x -> eval_power x right - eval_power x left) integral)

{-
  arr_repl - Repeat the elements of arr n times
             and preserve order.
-}
arr_repl :: Int -> [Int] -> [Int]
arr_repl n arr = concat(map (replicate n) arr)

{-
  choose - Naive binomial choice function.
-}
choose :: Int -> Int -> Int
choose n k = factorial n `quot` ((factorial k) * (factorial (n - k)))

{-
  count - Count the number of times an element appears
          in a list.
-}
count :: Eq a => a -> [a] -> Int
count elm = length . filter (elm==)

{-
  count_down - Create a list counting down from n.
-}
count_down :: Int -> [Int]
count_down 0 = [0]
count_down n = n : (count_down (n-1))

{-
  eval_power - Evaluate coef*x^pow with x = at.
-}
eval_power :: (Double, Double) -> Int -> Double
eval_power (coef, pow) at = coef * ((fromIntegral at) ** pow)

{-
  euler_exp - Estimate e^x
-}
euler_exp :: Double -> Double
euler_exp x = foldl (+) 0 (map (\y -> (x ** y)/ (foldl (*) 1 [1..y])) [0..9])

{-
  factorial - nth factorial
-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

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
  lcm_factors - Return the prime factors of the lcm of two
                numbers expressed as prime factors.
-}
lcm_factors :: [Int] -> [Int] -> [Int]
lcm_factors a b = concat (map (\elm -> take_more_common elm a b)(nub (union a b)))

{-
  lcm - Get the least common multiple of a set of integers.
-}
lcm :: [Int] -> Int
lcm nums = let facs = map factors nums in
    product (foldl lcm_factors [] facs)

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
  pascal_row - Get the nth row of Pascal's triangle.
               Counting starts at 0.
-}
pascal_row :: Int -> [Int]
pascal_row row = map (choose row) (range(0, row))

{-
  pascal_triangle - Get the first n rows of Pascal's triangle.
-}
pascal_triangle :: Int -> [[Int]]
pascal_triangle rows = map pascal_row (range(0, rows - 1))

{-
  poly_coef_times - Multiply c0*x^p0 with c1*x^p1.
-}
poly_coef_times :: (Int, Int) -> (Int, Int) -> (Int, Int)
poly_coef_times (c0, p0) (c1, p1) = (c0 * c1, p0 + p1)

{-
  poly_square - Square a polynomial.
-}
poly_square :: [(Int, Int)] -> [(Int, Int)]
poly_square poly = [ poly_coef_times x y  | x<-poly, y<-poly ]

{-
  power_rule - Integrate of coef*x^pow.
-}
power_rule :: (Int, Int) -> (Double, Double)
power_rule (coef, pow) = let next_pow = (fromIntegral pow) + 1.0 in
                      ((fromIntegral coef) / (next_pow), next_pow)

{-
  purge_prefix - Remove the given prefix from a string that starts
                with it.
-}
purge_prefix :: String -> String -> String
purge_prefix a b = if a /= "" && head a == head b
    then purge_prefix (tail a) (tail b)
    else b

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
  rot_lst - Rotate a list until we wrap around.
-}
rot_lst :: (Eq a) => [a] -> [a] -> [[a]]
rot_lst start stop = case (start == stop) of
                      True -> [stop]
                      False -> start:(rot_lst (tail start ++ [head start])stop)
{-
  rot_n - Rotate a list n times.
-}
rot_n :: (Eq a, Eq b, Num b) => [a] -> b -> [[a]]
rot_n start 0 = []
rot_n start n = start:(rot_n (tail start++[head start]) (n-1))

{-
  shared_prefix - Get the longest prefix that two strings share.
-}
shared_prefix :: String -> String -> String
shared_prefix a b = if a == "" || b == "" || head a /= head b
    then ""
    else [head a] ++ (shared_prefix (tail a) (tail b))

{-
  sum_odds - Sum the odd elements of a list
-}
sum_odds :: [Int] -> Int
sum_odds arr = foldl (+) 0 $ filter odd arr

{-
  take_more_common - Used for my least common multiple code.
-}
take_more_common :: (Eq a) => a -> [a] -> [a] -> [a]
take_more_common elm a b = replicate (max (count elm a)(count elm b)) elm

{-
  uniq - Return the list with only uniq elems.
         Order is determined by which came first.
-}
uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\a b -> if elem b a then a else a ++ [b]) []

{-
  volume_under_curve - Get the volume of a solid of revolution,
    using an integral and the left and right bound.
-}
volume_under_curve :: [(Double, Double)] -> Int -> Int -> Double
volume_under_curve integral left right =
    pi * sum (map (\x -> eval_power x right - eval_power x left) integral)

main = do
  str <- getLine
  print $ rot_lst (tail str++ [head str])str

