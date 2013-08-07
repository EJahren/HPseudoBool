Description
===========

This is a library for encoding pseudo boolean problems
in the format for the pseudo boolean competition 
(http://www.cril.univ-artois.fr/PB11/). Details about the 
format can be found here:

  http://www.cril.univ-artois.fr/PB11/format.pdf

usage
===


    main = putStrLn (getEncoding test)

    test = do
      c <- newVar
      add ( c |<=| 1)
      [a,b] <- replicateM 2 newVar
      add (c |>=| a + b)
      minimize(a - b)
