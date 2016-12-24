-- The generalized solution for the old problem of placing N queens on a NXN chessboard 
-- so that no two queens threaten each other.

-- The dependant solutions can be listed by the "queens n" function. 
-- The independant solutions can be listed by the "queensind n" function. 

-- This file also contain a set of set of auxiliary functions for detecting and filtering out the dependent solutions.

import Data.List 

type Pos = (Int,Int)

-------------------------------------------------------
-- are two queens in mutually attacking positions
-- (last two lines: left and  right diagonal i.e. comparing abs values)
relate :: Pos -> Pos -> Bool
relate a b = fst a == fst b || 
             snd a == snd b || 
             fst a - fst b == snd a - snd b || 
             fst a - fst b == snd b - snd a
             

-------------------------------------------------------
-- recursive unrelate
-- filters out related solutions 
-- (simplier than usual filter-pattern)
unrelate :: Pos -> [Pos] -> Bool
unrelate y [] = True
unrelate y (x:xs) =  if y `relate` x then False else unrelate y xs 

 

-------------------------------------------------------
-- list of all good permutations
perms :: Int -> [[Int]]
perms nn = foldr (\y acc -> ((y:) `map` sublist [y]) ++ acc) [] [1..hlf]
  where hlf = let h0 = quot nn 2 in if even nn then h0 else h0+1
        sublist lst = let lng = length lst 
                      in foldr (\x acc -> if (lng+1, x) `unrelate` zip [1..lng] lst 
                                          then ((x:) `map` if lng == nn-1 
                                                           then []:[] 
                                                           else sublist $ lst ++ [x]) ++ acc 
                                          else acc 
                               ) [] $ [1..nn] \\ lst


-------------------------------------------------------
-- list of all queens positions i.e. (x,y) pairs
queens :: Int -> [[Pos]]
queens n = map ([1..n] `zip`) $ perms n


-------------------------------------------------------
-- list of all queens positions i.e. (x,y) pairs
queensind :: Int -> [[Pos]]
queensind n = filterout $ queens n



-------------------------------------------------------
-- set of auxiliary functions for filtering out
-- the dependent solutions 
-------------------------------------------------------
complement :: Int -> Int -> Int
complement chesboard_size x = chesboard_size + 1 - x


-------------------------------------------------------
-- not rotated for 90 (cw) filter
filter90 :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filter90 n x = filter (/= rotate90 x)
  where rotate90 [] = []
        rotate90 (x:xs) = sort $ change90 x : rotate90 xs  -- For pairs, sorting is always in regards to the first member
        change90 (a,b) = (b, complement n a)               -- That's exactly what we need.


-------------------------------------------------------
-- not rotated for 180 filter
filter180 :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filter180 n x = filter (/= rotate180 x)
  where rotate180 [] = []
        rotate180 (x:xs) = sort $ change180 x : rotate180 xs
        change180 (a,b) = (complement n a, complement n b)


-------------------------------------------------------
-- not rotated for 270 (cw) filter
filter270 :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filter270 n x = filter (/= rotate270 x)
  where rotate270 [] = []
        rotate270 (x:xs) = sort $ change270 x : rotate270 xs
        change270 (a,b) = (complement n b, a)


-------------------------------------------------------
-- not vertical mirrors filter
filterV :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filterV n x = filter (/= mirrorv x)
  where mirrorv [] = []
        mirrorv (x:xs) = sort $ changemv x : mirrorv xs
        changemv (a,b) = (complement n a, b)


-------------------------------------------------------
-- not horizontal mirrors filter
filterH :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filterH n x = filter (/= mirrorh x)
  where mirrorh [] = []
        mirrorh (x:xs) = sort $ changemh x : mirrorh xs
        changemh (a,b) = (a, complement n b)


-------------------------------------------------------
-- 11 - 88 diagonal mirrorr
filterD1 :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filterD1 n x = filter (/= mirrord1 x)
  where mirrord1 [] = []
        mirrord1 (x:xs) = sort $ changemd1 x : mirrord1 xs
        changemd1 (a,b) = (b, a)


-------------------------------------------------------
-- 81 - 18 diagonal mirrorr
filterD2 :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
filterD2 n x = filter (/= mirrord2 x)
  where mirrord2 [] = []
        mirrord2 (x:xs) = sort $ changemd2 x : mirrord2 xs
        changemd2 (a,b) = (complement n b, complement n a)


-------------------------------------------------------
-- combine all transformation filters into one superfilter
-- (it doesn't have to be global but I've extracted just to 
-- emphasise the beauty of composition)
superfilter :: Int -> [Pos] -> [[Pos]] -> [[Pos]]
superfilter n x = filter (/=x) . filterD1 n x . filterD2 n x . filterH n x . filterV n x . filter270 n x . filter180 n x . filter90 n x 


-------------------------------------------------------
-- clear list in a way that there are not transfrom-related elements
filterout :: [[Pos]] -> [[Pos]]
filterout [] = []
filterout (x:[]) = [x]
filterout ls@(x:xs) = x : superfilter nn x (filterout xs) 
  where nn = length x


-------------------------------------------------------
-- This is redundant
-- First I had thought that combination of (h/v)mirroring + (90/270)rotation is not covered
-- Later, I realized that diagonal mirrorings cover these possibilities
-- filterout2 :: [[Pos]] -> [[Pos]]
-- filterout2 [] = []
-- filterout2 (x:[]) = [x]
-- filterout2 (x:xs) = x : superfilter y (superfilter x $ filterout xs)
--   where y = chh x


-------------------------------------------------------
-- list of positions -> list of permutations (of rows)
poss2perms :: [[Pos]] -> [[Int]]
poss2perms [] = []
poss2perms (x:xs) = map snd x : poss2perms xs


