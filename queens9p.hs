-- The raw solution for the derived problem of placing 1 pown and 9 queens on a 8X8 chessboard so that no two queens 
-- threaten each other. There are 16 independant solutions (queens9pind) and 128 dependent solutions (queens9p). 
-- This file also contain a set of auxiliary functions for detecting and filtering out the dependent solutions.

import Data.List 

type Pos = (Int,Int)
type Comb = (Pos,[Pos])


-------------------------------------------------------
-- only diagonal relation - without pown position
-- (enhance slightly performancies)
diag_related2 :: Pos -> Pos -> Bool
diag_related2 (a,b) (c,d) = (a-c == b-d) || (a-c == d-b)
             

-------------------------------------------------------
-- only diagonal relation - with pown position
-- (enhance slightly performancies)
diag_related3 :: Pos -> Pos -> Pos -> Bool
diag_related3 (x,y) (a,b) (c,d) = ( a-c == b-d && (a-x /= b-y || x > max a c || x < min a c) ) || 
                                  ( a-c == d-b && (a-x /= y-b || x > max a c || x < min a c) )
             

-------------------------------------------------------
-- recursive unrelate (without pown position argument) - 2 position argument
-- filters out related solutions 
unrelated2 :: (Pos -> Pos -> Bool) -> Pos -> [Pos] -> Bool
unrelated2 func y [] = True
unrelated2 func y (x:xs) =  if func y x then False else unrelated2 func y xs 


-------------------------------------------------------
-- recursive unrelate (with pown position argument) - 3 position argument
-- filters out related solutions 
unrelated3 :: (Pos -> Pos -> Pos -> Bool) -> Pos -> Pos -> [Pos] -> Bool
unrelated3 func p y [] = True
unrelated3 func p y (x:xs) =  if func p y x then False else unrelated3 func p y xs 


queens9p = nub queens
queens9pind = filterout $ nub queensi

-------------------------------------------------------
queens = [((x,y), sort [(x1,y1),(x2,y2),(x3,y3),(x4,y4), (x5,y5),(x6,y6),(x7,y7),(x8,y8),(x9,y9)]) | 
  x  <- [2..7], y  <- [2..7],
   
  x1 <- [x], y1 <- [1..y-1], 
  x2 <- [x], y2 <- [y+1..8], 
  
  y3 <- [y], x3 <- [1..x-1], not $ diag_related2 (x3,y3) (x1,y1), not $ diag_related2 (x3,y3) (x2,y2),
  y4 <- [y], x4 <- [x+1..8], not $ diag_related2 (x4,y4) (x1,y1), not $ diag_related2 (x4,y4) (x2,y2),

  
  x5 <- [1..8] \\ [x,x3,x4], 
  y5 <- [1..8] \\ [y,y1,y2], 
  unrelated2 diag_related2 (x5,y5) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  
  x6 <- [1..8] \\ [x,x3,x4, x5], 
  y6 <- [1..8] \\ [y,y1,y2, y5], 
  unrelated2 diag_related2 (x6,y6) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x6,y6) [(x5,y5)],
  
  x7 <- [1..8] \\ [x,x3,x4, x5,x6], 
  y7 <- [1..8] \\ [y,y1,y2, y5,y6], 
  unrelated2 diag_related2 (x7,y7) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x7,y7) [(x5,y5),(x6,y6)],
  
  x8 <- [1..8] \\ [x,x3,x4, x5,x6,x7], 
  y8 <- [1..8] \\ [y,y1,y2, y5,y6,y7], 
  unrelated2 diag_related2 (x8,y8) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x8,y8) [(x5,y5),(x6,y6),(x7,y7)],
  
  x9 <- [1..8] \\ [x,x3,x4, x5,x6,x7,x8], 
  y9 <- [1..8] \\ [y,y1,y2, y5,y6,y7,y8], 
  unrelated2 diag_related2 (x9,y9) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x9,y9) [(x5,y5),(x6,y6),(x7,y7),(x8,y8)]
         ]
          
-------------------------------------------------------
queensi = [((x,y), sort [(x1,y1),(x2,y2),(x3,y3),(x4,y4), (x5,y5),(x6,y6),(x7,y7),(x8,y8),(x9,y9)]) | 
  x  <- [2..4], y  <- [2..4],
   
  x1 <- [x], y1 <- [1..y-1], 
  x2 <- [x], y2 <- [y+1..8], 
  
  y3 <- [y], x3 <- [1..x-1], not $ diag_related2 (x3,y3) (x1,y1), not $ diag_related2 (x3,y3) (x2,y2),
  y4 <- [y], x4 <- [x+1..8], not $ diag_related2 (x4,y4) (x1,y1), not $ diag_related2 (x4,y4) (x2,y2),

  
  x5 <- [1..8] \\ [x,x3,x4], 
  y5 <- [1..8] \\ [y,y1,y2], 
  unrelated2 diag_related2 (x5,y5) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  
  x6 <- [1..8] \\ [x,x3,x4, x5], 
  y6 <- [1..8] \\ [y,y1,y2, y5], 
  unrelated2 diag_related2 (x6,y6) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x6,y6) [(x5,y5)],
  
  x7 <- [1..8] \\ [x,x3,x4, x5,x6], 
  y7 <- [1..8] \\ [y,y1,y2, y5,y6], 
  unrelated2 diag_related2 (x7,y7) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x7,y7) [(x5,y5),(x6,y6)],
  
  x8 <- [1..8] \\ [x,x3,x4, x5,x6,x7], 
  y8 <- [1..8] \\ [y,y1,y2, y5,y6,y7], 
  unrelated2 diag_related2 (x8,y8) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x8,y8) [(x5,y5),(x6,y6),(x7,y7)],
  
  x9 <- [1..8] \\ [x,x3,x4, x5,x6,x7,x8], 
  y9 <- [1..8] \\ [y,y1,y2, y5,y6,y7,y8], 
  unrelated2 diag_related2 (x9,y9) [(x1,y1),(x2,y2),(x3,y3),(x4,y4)],
  unrelated3 diag_related3 (x,y) (x9,y9) [(x5,y5),(x6,y6),(x7,y7),(x8,y8)]
         ]
          
          
          
-- =====================================================

-------------------------------------------------------
-- set of auxiliary functions for filtering out
-- the dependent (transformed) solutions 
-------------------------------------------------------
chesboard_size = 9  -- global constant

complement :: Int -> Int
complement x = chesboard_size + 1 - x


-------------------------------------------------------
-- not rotated for 90 (cw) filter
filter90 :: Comb -> [Comb] -> [Comb]
filter90 (p,qs) = filter (\(p2,qs2) -> p2 /= change90 p || qs2 /= rotate90 qs)
  where rotate90 [] = []
        rotate90 (x:xs) = sort $ change90 x : rotate90 xs
        change90 (a,b) = (b, complement a)

-------------------------------------------------------
-- not rotated for 180 filter
filter180 :: Comb -> [Comb] -> [Comb]
filter180 (p,qs) = filter (\(p2,qs2) -> p2 /= change180 p || qs2 /= rotate180 qs)
  where rotate180 [] = []
        rotate180 (x:xs) = sort $ change180 x : rotate180 xs
        change180 (a,b) = (complement a, complement b)


-------------------------------------------------------
-- not rotated for 270 (cw) filter
filter270 :: Comb -> [Comb] -> [Comb]
filter270 (p,qs) = filter (\(p2,qs2) -> p2 /= change270 p || qs2 /= rotate270 qs)
  where rotate270 [] = []
        rotate270 (x:xs) = sort $ change270 x : rotate270 xs
        change270 (a,b) = (complement b, a)


-------------------------------------------------------
-- not vertical mirrors filter
filterV :: Comb -> [Comb] -> [Comb]
filterV (p,qs) = filter (\(p2,qs2) -> p2 /= changemv p || qs2 /= mirrorv qs)
  where mirrorv [] = []
        mirrorv (x:xs) = sort $ changemv x : mirrorv xs
        changemv (a,b) = (complement a, b)


-------------------------------------------------------
-- not horizontal mirrors filter
filterH :: Comb -> [Comb] -> [Comb]
filterH (p,qs) = filter (\(p2,qs2) -> p2 /= changemh p || qs2 /= mirrorh qs)
  where mirrorh [] = []
        mirrorh (x:xs) = sort $ changemh x : mirrorh xs
        changemh (a,b) = (a, complement b)


-------------------------------------------------------
-- 11 - 88 diagonal mirrorr
filterD1 :: Comb -> [Comb] -> [Comb]
filterD1 (p,qs) = filter (\(p2,qs2) -> p2 /= changemd1 p || qs2 /= mirrord1 qs)
  where mirrord1 [] = []
        mirrord1 (x:xs) = sort $ changemd1 x : mirrord1 xs
        changemd1 (a,b) = (b, a)


-------------------------------------------------------
-- 81 - 18 diagonal mirrorr
filterD2 :: Comb -> [Comb] -> [Comb]
filterD2 (p,qs) = filter (\(p2,qs2) -> p2 /= changemd2 p || qs2 /= mirrord2 qs)
  where mirrord2 [] = []
        mirrord2 (x:xs) = sort $ changemd2 x : mirrord2 xs
        changemd2 (a,b) = (complement b, complement a)


-------------------------------------------------------
-- combine all transformation filters into one superfilter
-- (it doesn't have to be global but I've extracted just to 
-- emphasise the beauty of composition)
superfilter :: Comb -> [Comb] -> [Comb]
superfilter x = filter (/=x) . filterD1 x . filterD2 x . filterH x . filterV x . filter270 x . filter180 x . filter90 x 


-------------------------------------------------------
-- clear list in a way that there are not transfrom-related elements
filterout :: [Comb] -> [Comb]
filterout [] = []
filterout (x:[]) = [x]
filterout (x:xs) = x : superfilter x (filterout xs) 


