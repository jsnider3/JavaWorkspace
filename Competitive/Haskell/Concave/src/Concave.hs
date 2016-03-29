import Control.Monad
import Data.List

makePair :: [a] -> (a, a)
makePair [hd, tl] = (hd, tl)

intWords :: String -> (Int, Int)
intWords line = makePair(map read (words line))

ynBool :: Bool -> String
ynBool True = "YES"
ynBool False = "NO"

{-
  dot :: (Int, Int) -> (Int, Int) -> Float
    Dot product of two 2D vectors.
-}
pointDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointDiff (sx, sy) (ex, ey) = (ex - sx, ey - sy)

{-
  dot :: (Int, Int) -> (Int, Int) -> Float
    Dot product of two 2D vectors.
-}
dot :: (Int, Int) -> (Int, Int) -> Float
dot (sx, sy) (ex, ey) = fromIntegral(ex * sx + ey * sy)

tern True _ b = b
tern False a _ = a

mag :: (Int, Int) -> Float
mag (a,b) = sqrt(fromIntegral(a)^2 + fromIntegral(b)^2)
{-
  perp :: (Int, Int) -> (Int, Int)
    Get a vector perpendicular to another.
-}
perp :: (Int, Int) -> (Int, Int)
perp (sx, sy) = (-sy, sx)

{-
  crossangle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Float
    Get the angle formed by two lines that meet at a point.
-}
crossangle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Float
crossangle (sx, sy) (mx, my) (ex, ey) =
  let x1 = mx - sx
      y1 = my - sy
      x2 = ex - mx
      y2 = ey - my in
    fromIntegral(x1 * y2) - fromIntegral(x2 * y1)
{-    let (x1, y1) = pointDiff start mid
        (x2, y2) = pointDiff mid end
        denom = mag(x1,y1) * mag(x2, y2)
    in acos ((dot (x1, y1)  (x2, y2)) / denom) -}

crossangles ::[(Int, Int)] -> [Float]
crossangles [a, b, c] = [crossangle a b c]
crossangles points = (crossangle (head $ tail $ reverse points) (last points) (points !! 0)) :
    ((crossangle (last points) (points !! 0) (points !! 1)) :
    ((crossangle (points !! 0) (points !! 1) (points !! 2)) :
    (map (\x -> crossangle (points !! x) (points !! (x+1)) (points !! (x+2))) [0..(length points - 3)])))

isConcave points = let angles = crossangles points in
    (length (filter (\x -> x > 0) angles) /= 0) &&
    (length (filter (\x -> x < 0) angles) /= 0)

assert True = 0
assert False = error "Failed assert"
main = do
    --print (dot (4, 6) (-2, 5) == 22.0)
    --print (False == isConcave [(0, 0), (0, 1), (1, 1), (1, 0)])
    --print (True == isConcave [(0, 0), (0, 1), (1, 1), (2, 2), (1, 0)])
    --print (crossangle (0, 0) (0, 1) (1, 1))
    --print (crossangles [(0, 0), (0, 1), (1, 1), (2, 2), (1, 0)])
    --print (isConcave [(0, 0), (0, 1), (1, 1), (2, 2), (1, 0)])
    --print (not $isConcave [(0, 0), (0, 1), (1, 1), (1, 0)])
    --print (not $isConcave [(522, 991), (1054, 665), (661, 485)])
    print (crossangles [(1028, 625), (1042, 943), (793, 1042), (404, 909),
                        (574, 474), (1077, 721), (392, 543), (572, 1005),
                        (963, 1020), (857, 390)])
    --print (crossangles [(522, 991), (1054, 665), (661, 485)])
    num_points <- getLine
    points <- (replicateM (read num_points) getLine)
    let intpoints = map intWords points in
        --print $ crossangles intpoints;
        putStrLn $ ynBool (isConcave intpoints)
