module Main where

import Solver.ILP

pb :: Problem
pb = buildProblem $ do
  x1 <- namedVar RealVar "x1"
  x2 <- namedVar IntegerVar "x2"

  maximize $ 2 *. x1 +. x2
  suchThat $ 2 *. x1 +. x2 <=. 5
  suchThat $ x1 +. 4 *. x2 >=. 2

main :: IO ()
main = do
  putStrLn ""
  print pb
  let x = solveMILP pb
  print x
