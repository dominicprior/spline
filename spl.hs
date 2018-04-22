-- B-Spline evaluator.

import Data.List

main = do
  putStrLn "hello world"

type T = Double  -- The knots
type X = Double  -- The one-dimensional points

-- Returns the result of evaluating the nth degree spline at value t.
-- The spline is given by the 2n knots and the n+1 points.
-- See, for example, http://www.uio.no/studier/emner/matnat/ifi/INF-MAT5340/v07/undervisningsmateriale/kap1.pdf

spl :: Int -> [T] -> [X] -> T -> X
spl 0 _ [x] _ = x
spl n ts xs t = let
  ts' = tail $ init ts
  xs' = zipWith4 w ts (drop n ts) xs (tail xs)
  w t1 t2 x1 x2 = ((t2-t) * x1 + (t-t1)*x2) / (t2 - t1)
  in spl (n-1) ts' xs' t
