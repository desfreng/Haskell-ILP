module Main where

import Solver.ILP

pb :: Problem
pb = buildProblem $ do
  x1 <- namedVar RealVar "x1"
  x2 <- namedVar RealVar "x2"
  x3 <- namedVar RealVar "x3"

  maximize $ x1 -. x2 +. x3
  suchThat $ 3 *. x1 -. 2 *. x2 +. 3 *. x3 ==. 3
  suchThat $ 0 *. x1 ==. 3

-- suchThat $ 3 *. x3 <=. -1

main :: IO ()
main = do
  putStrLn ""
  print pb
  let x = solveLinearProgram pb
  print x
