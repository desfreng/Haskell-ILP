{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Interface
  ( Var (..),
    Expr (),
    Constraint (),
    BuildProblem (),
    ObjectiveType (..),
    maximize,
    minimize,
    setObjective,
    suchThat,
    freshVar,
    namedVar,
    (+.),
    (-.),
    (*.),
    (==.),
    (>=.),
    (<=.),
    buildProblem,
    changeProblem,
  )
where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RatioInt (RatioInt (..))
import qualified Data.Set as S
import Solver.Data
import Prelude hiding (EQ)

newtype Var = Var VarID
  deriving (Eq, Ord)

data Expr = Expr {coeffs :: !(Map VarID RatioInt), constant :: !RatioInt}

data Constraint = LE !Expr | GE !Expr | EQ !Expr

varExpr :: Var -> Expr
varExpr (Var v) = Expr (M.singleton v 1) 0

addExpr :: Expr -> Expr -> Expr
addExpr (Expr a c1) (Expr b c2) =
  Expr (M.unionWith (+) a b) (c1 + c2)

scaleMap :: RatioInt -> Map a RatioInt -> Map a RatioInt
scaleMap 0 _ = M.empty
scaleMap k m = fmap (k *) m

scaleExpr :: RatioInt -> Expr -> Expr
scaleExpr k (Expr m c) =
  Expr (scaleMap k m) (k * c)

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Expr where
  toExpr :: Expr -> Expr
  toExpr = id

instance ToExpr Var where
  toExpr :: Var -> Expr
  toExpr = varExpr

instance ToExpr RatioInt where
  toExpr :: RatioInt -> Expr
  toExpr = Expr mempty

infixl 6 +., -.

(+.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a +. b = addExpr (toExpr a) (toExpr b)

(-.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a -. b = a +. scaleExpr (-1) (toExpr b)

infixl 7 *.

(*.) :: (ToExpr a) => RatioInt -> a -> Expr
k *. x = scaleExpr k (toExpr x)

infix 4 <=., >=., ==.

(<=.) :: (ToExpr a) => a -> RatioInt -> Constraint
a <=. b = LE (a -. b)

(>=.) :: (ToExpr a) => a -> RatioInt -> Constraint
a >=. b = GE (a -. b)

(==.) :: (ToExpr a) => a -> RatioInt -> Constraint
a ==. b = EQ (a -. b)

newtype BuildProblem a = BuildProblem (State Problem a)
  deriving (Functor, Applicative, Monad)

addVar :: VarType -> VarTag -> BuildProblem Var
addVar vKind vTag = BuildProblem $ state pbAddVar
  where
    pbAddVar st@Problem {nbVars, intVars, varTags} =
      let v = VarID nbVars
          newS =
            st
              { nbVars = nbVars + 1,
                varTags = M.insert v vTag varTags,
                intVars = case vKind of
                  IntegerVar -> S.insert v intVars
                  RealVar -> intVars
              }
       in (Var v, newS)

namedVar :: VarType -> String -> BuildProblem Var
namedVar vKind = addVar vKind . Tagged

freshVar :: VarType -> BuildProblem Var
freshVar vKind = addVar vKind NoTag

normExpr :: Map a RatioInt -> RatioInt -> (Map a RatioInt, RatioInt)
normExpr c k
  | k < 0 = (scaleMap (-1) c, -k)
  | otherwise = (c, k)

toEqualZero :: Constraint -> BuildProblem (Map VarID RatioInt, RatioInt)
toEqualZero (EQ Expr {coeffs, constant}) = pure $ normExpr coeffs (-constant)
toEqualZero (LE e) = do
  slack <- addVar RealVar SlackVar
  let Expr {coeffs, constant} = e +. slack
  return $ normExpr coeffs (-constant)
toEqualZero (GE e) = do
  slack <- addVar RealVar SlackVar
  let Expr {coeffs, constant} = e -. slack
  return $ normExpr coeffs (-constant)

suchThat :: Constraint -> BuildProblem ()
suchThat c = do
  cEqZ <- toEqualZero c
  BuildProblem $ modify' (\st@Problem {constraints} -> st {constraints = cEqZ : constraints})

setObjective :: ObjectiveType -> Expr -> BuildProblem ()
setObjective t e =
  BuildProblem $ modify' (\st -> st {objective = coeffs e, objectiveType = t})

maximize :: Expr -> BuildProblem ()
maximize = setObjective Maximize

minimize :: Expr -> BuildProblem ()
minimize = setObjective Minimize

initialProblem :: Problem
initialProblem =
  Problem
    { nbVars = 0,
      objectiveType = Minimize,
      varTags = M.empty,
      objective = M.empty,
      constraints = [],
      intVars = S.empty
    }

buildProblem :: BuildProblem () -> Problem
buildProblem (BuildProblem m) = execState m initialProblem

changeProblem :: Problem -> BuildProblem () -> Problem
changeProblem s (BuildProblem m) = execState m s
