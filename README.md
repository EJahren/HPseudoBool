Description
===========

This is a library for encoding pseudo boolean problems
in the format for the pseudo boolean competition 
(http://www.cril.univ-artois.fr/PB11/). Details about the 
format can be found here:

  http://www.cril.univ-artois.fr/PB11/format.pdf

usage
===


putStrLn (getEncoding test)

    test = do
      [a,b] <- mapM newVar ["a","b"]
      c <- newVar1
      add (2 |*| a |+| 3 |*| c |<=| (asSum 3))
      add (3 |*| b |+| 5 |*| c |>| (asSum 4))
      minimize (X 1 |+| X 2)

