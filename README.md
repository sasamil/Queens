# Queens
Haskell solution to an old problem

Since Max Bezzel published the eight queens puzzle in 1848 (Berliener Schachzeitung), many matematicians (including Gauss and Dijkstra) have been occupied by it. F. Nauck was the first who offered a complete solution containing 92 dependant possibilities.

I met this problem when I was a student (Занимљиви математички проблеми - М. Петковић) and I was a bit terrified by the complicated Fortran code which was given in the book. I didn't even try to realize it. These days I've been playing with this problem using Haskell. And, it's unusual how simple the solution can be. I've been playing with the related "Nine queens and one pown" problem... I still don't understand whether the complete solution is officially unknown or not, but - there are 16 independent solutions and my Haskell code is here!

File queens8.hs is for a raw solution of the problem - 8 queens on 8X8 chessboard. The code in filegen.hs is for a generalized solution (5 queens on 5X5 board, 6 queens on 6X6 board, etc.) The queens9p.hs is for a solution of derived "Nine queens and one pown" problem.
