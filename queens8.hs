-- The raw solution for the old problem of placing 8 queens on a 8X8 chessboard so that no two queens threaten each other.

-- There are 12 independant solution and they can be listed by the "queens8ind" function. 

-- There are 92 dependent solutions. It's about solutions which are basicly same but isometrically transformed (rotation
-- or reflection). They can be listed by "queens8" function.

-- This file also contain a set of set of auxiliary functions for detecting and filtering out the dependent solutions.

import Data.List -- just because of \\

type Pos = (Int,Int)

-------------------------------------------------------
-- are two queens in same diagonal
diagrelate :: Pos -> Pos -> Bool
diagrelate a b = fst a - fst b == snd a - snd b || fst a - fst b == snd b - snd a
             

-------------------------------------------------------
-- recursive unrelate
-- filters out related solutions 
-- (less complex than usual filter-pattern)
unrelate :: Pos -> [Pos] -> Bool
unrelate y [] = True
unrelate y (x:xs) =  if y `diagrelate` x then False else unrelate y xs 


-------------------------------------------------------
-- list of all good permutations
-- (with a<-[1..4] we would get better performance but only 'lower' 46 of all 92 solutions. 
-- It would be correct, anyway i.e. it would yield the same set of independant solutions.)
perms8 = [[a,b,c,d,e,f,g,h] | a <- [1..8], 
                              b <- [1..8] \\ [a], unrelate (2,b) $ zip [1] [a],
                              c <- [1..8] \\ [a,b], unrelate (3,c) $ zip [1,2] [a,b],
                              d <- [1..8] \\ [a,b,c], unrelate (4,d) $ zip [1,2,3] [a,b,c],
                              e <- [1..8] \\ [a,b,c,d], unrelate (5,e) $ zip [1,2,3,4] [a,b,c,d],
                              f <- [1..8] \\ [a,b,c,d,e], unrelate (6,f) $ zip [1,2,3,4,5] [a,b,c,d,e], 
                              g <- [1..8] \\ [a,b,c,d,e,f], unrelate (7,g) $ zip [1,2,3,4,5,6] [a,b,c,d,e,f], 
                              h <- [1..8] \\ [a,b,c,d,e,f,g], unrelate (8,h) $ zip [1,2,3,4,5,6,7] [a,b,c,d,e,f,g]
         ]


-------------------------------------------------------
-- list of all queens positions i.e. (x,y) pairs
queens8 = map (([1..8]::[Int]) `zip`) perms8

-------------------------------------------------------
-- list of all queens positions i.e. (x,y) pairs
queens8ind = filterout queens8



-- =====================================================

-------------------------------------------------------
-- set of auxiliary functions for filtering out
-- the dependent solutions 
-------------------------------------------------------
chesboard_size = 8  -- global constant

complement :: Int -> Int
complement x = chesboard_size + 1 - x


-------------------------------------------------------
-- not rotated for 90 (cw) filter
filter90 :: [Pos] -> [[Pos]] -> [[Pos]]
filter90 x = filter (/= rotate90 x)
  where rotate90 [] = []
        rotate90 (x:xs) = sort $ change90 x : rotate90 xs
        change90 (a,b) = (b, complement a)


-------------------------------------------------------
-- not rotated for 180 filter
filter180 :: [Pos] -> [[Pos]] -> [[Pos]]
filter180 x = filter (/= rotate180 x)
  where rotate180 [] = []
        rotate180 (x:xs) = sort $ change180 x : rotate180 xs
        change180 (a,b) = (complement a, complement b)


-------------------------------------------------------
-- not rotated for 270 (cw) filter
filter270 :: [Pos] -> [[Pos]] -> [[Pos]]
filter270 x = filter (/= rotate270 x)
  where rotate270 [] = []
        rotate270 (x:xs) = sort $ change270 x : rotate270 xs
        change270 (a,b) = (complement b, a)


-------------------------------------------------------
-- not vertical mirrors filter
filterV :: [Pos] -> [[Pos]] -> [[Pos]]
filterV x = filter (/= mirrorv x)
  where mirrorv [] = []
        mirrorv (x:xs) = sort $ changemv x : mirrorv xs
        changemv (a,b) = (complement a, b)


-------------------------------------------------------
-- not horizontal mirrors filter
filterH :: [Pos] -> [[Pos]] -> [[Pos]]
filterH x = filter (/= mirrorh x)
  where mirrorh [] = []
        mirrorh (x:xs) = sort $ changemh x : mirrorh xs
        changemh (a,b) = (a, complement b)


-------------------------------------------------------
-- 11 - 88 diagonal mirrorr
filterD1 :: [Pos] -> [[Pos]] -> [[Pos]]
filterD1 x = filter (/= mirrord1 x)
  where mirrord1 [] = []
        mirrord1 (x:xs) = sort $ changemd1 x : mirrord1 xs
        changemd1 (a,b) = (b, a)

-------------------------------------------------------
-- 81 - 18 diagonal mirrorr
filterD2 :: [Pos] -> [[Pos]] -> [[Pos]]
filterD2 x = filter (/= mirrord2 x)
  where mirrord2 [] = []
        mirrord2 (x:xs) = sort $ changemd2 x : mirrord2 xs
        changemd2 (a,b) = (complement b, complement a)


-------------------------------------------------------
-- combine all transformation filters into one superfilter
-- (it doesn't have to be global but I've extracted just to 
-- emphasise the beauty of composition)
superfilter :: [Pos] -> [[Pos]] -> [[Pos]]
superfilter x = filter (/=x) . filterD1 x . filterD2 x . filterH x . filterV x . filter270 x . filter180 x . filter90 x 


-------------------------------------------------------
-- clear list in a way that there are not transfrom-related elements
filterout :: [[Pos]] -> [[Pos]]
filterout [] = []
filterout (x:[]) = [x]
filterout (x:xs) = x : superfilter x (filterout xs) 

-------------------------------------------------------
-- list of positions -> list of permutations (of rows)
poss2perms :: [[Pos]] -> [[Int]]
poss2perms [] = []
poss2perms (x:xs) = map snd x : poss2perms xs


