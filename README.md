Since Max Bezzel published the eight queens puzzle in 1848 (<i>Berliener Schachzeitung</i>), many matematicians (including Gauss and Dijkstra) have been occupied by it. F. Nauck was the first who offered a complete solution containing 92 dependant possibilities.

I met this problem when I was a student (<i>Занимљиви математички проблеми - М. Петковић</i>) and I was a bit terrified by the complicated Fortran code which was given in the book, as a solution. I didn't even try to realize it. These days I've been playing with this problem using Haskell. And, it's unusual how simple the solution can be!? Furthermore, I've been playing with the derived "Nine queens and one pown" problem... After reading some articles, I cannot conclude whether the complete solution of this problem is still officially unknown or not?! Anyway, there are 128 dependent and 16 independent solutions and my Haskell code is here!  <img src="http://forum.srpskinacionalisti.com/images/smilies/eusa_whistle.gif" alt="I've succeeded" height="16" width="22">

File <i>queens8.hs</i> is for a raw solution of the problem - 8 queens on 8X8 chessboard. The code in <i>queensgen.hs</i> is for a generalized solution (N queens on N X N board; N>4) The <i>queens9p.h</i>s is given here as a solution of derived "Nine queens and one pown" problem
