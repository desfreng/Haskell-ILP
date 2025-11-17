{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Interface
  ( Var (),
    Expr (),
    Constraint (),
    BuildProblem (),
    maximize,
    minimize,
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
  )
where

import Control.Monad.ST (runST)
import Control.Monad.State
import Data.Bifunctor
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RatioInt (RatioInt)
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import Solver.Data
import Prelude hiding (EQ)

data Var = Var
  { vIdx :: !Int,
    vTag :: !VarTag
  }

instance Eq Var where
  (==) :: Var -> Var -> Bool
  (Var i _) == (Var j _) = i == j

instance Ord Var where
  compare :: Var -> Var -> Ordering
  compare (Var i _) (Var j _) = compare i j

data Expr = Expr {coeffs :: !(Map Var RatioInt), constant :: !RatioInt}

data Constraint = LE !Expr | GE !Expr | EQ !Expr

varExpr :: Var -> Expr
varExpr v = Expr (M.singleton v 1) 0

addExpr :: Expr -> Expr -> Expr
addExpr (Expr a c1) (Expr b c2) =
  Expr (M.unionWith (+) a b) (c1 + c2)

scaleMap :: RatioInt -> Map a RatioInt -> Map a RatioInt
scaleMap k = fmap (k *)

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

data PBState = PBState
  { nextVarId :: !Int,
    pbVarKind :: !(Map Var VarType),
    pbConstraints :: ![(Map Var RatioInt, RatioInt)],
    pbObjective :: !(Expr, ObjectiveType)
  }

newtype BuildProblem a = BuildProblem (State PBState a)
  deriving (Functor, Applicative, Monad)

addVar :: VarType -> VarTag -> BuildProblem Var
addVar vKind vTag = BuildProblem $ do
  st <- get
  let v = Var (nextVarId st) vTag
  put st {nextVarId = nextVarId st + 1, pbVarKind = M.insert v vKind (pbVarKind st)}
  return v

namedVar :: VarType -> String -> BuildProblem Var
namedVar vKind = addVar vKind . Tagged

freshVar :: VarType -> BuildProblem Var
freshVar vKind = addVar vKind NoTag

normExpr :: Map Var RatioInt -> RatioInt -> (Map Var RatioInt, RatioInt)
normExpr c k
  | k < 0 = (scaleMap (-1) c, -k)
  | otherwise = (c, k)

toEqualZero :: Constraint -> BuildProblem (Map Var RatioInt, RatioInt)
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
  BuildProblem $ modify' (\st -> st {pbConstraints = cEqZ : pbConstraints st})

setObjective :: ObjectiveType -> Expr -> BuildProblem ()
setObjective t e = BuildProblem $ do
  st <- get
  put st {pbObjective = (e, t)}

maximize :: Expr -> BuildProblem ()
maximize = setObjective Maximize

minimize :: Expr -> BuildProblem ()
minimize = setObjective Minimize

initialState :: PBState
initialState =
  PBState
    { nextVarId = 0,
      pbConstraints = [],
      pbVarKind = mempty,
      pbObjective = (toExpr (0 :: RatioInt), Minimize)
    }

buildProblem :: BuildProblem () -> Problem
buildProblem (BuildProblem m) =
  let vMap =
        M.foldMapWithKey
          ( \v kind ->
              let vInfo = VarInfo (vTag v) kind . TableauCol $ vIdx v
               in IM.singleton (vIdx v) vInfo
          )
          pbVarKind
      ctrMap = fmap (first exprToVect) pbConstraints
      objVect = exprToVect . coeffs $ fst pbObjective
   in Problem vMap objVect ctrMap (snd pbObjective)
  where
    PBState {nextVarId, pbConstraints, pbVarKind, pbObjective} =
      execState m initialState

    exprToVect :: Map Var RatioInt -> Vector RatioInt
    exprToVect vCoef = runST $
      do
        v <- MV.replicate nextVarId 0
        () <-
          mapM_ (\(var, val) -> MV.write v (vIdx var) val) $
            M.toList vCoef
        V.freeze v
