{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Data where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.RatioInt
import Data.Set (Set)
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Solver.Matrix

showRatio :: RatioInt -> String
showRatio r
  | denominator r == 1 = show $ numerator r
  | otherwise = show (numerator r) <> "/" <> show (denominator r)

newtype TableauCol = TableauCol Int
  deriving (Eq, Ord)

instance Show TableauCol where
  show :: TableauCol -> String
  show (TableauCol x) = show x

data VarType = IntegerVar | RealVar

data ObjectiveType = Maximize | Minimize

data VarTag = NoTag | SlackVar | Tagged !String

showTag :: TableauCol -> VarTag -> String
showTag (TableauCol idx) NoTag = "_" <> show idx
showTag (TableauCol idx) SlackVar = "_slack" <> show idx
showTag _ (Tagged x) = x

data VarInfo = VarInfo
  { varTag :: !VarTag,
    varType :: !VarType,
    varIndex :: !TableauCol
  }

data Problem = Problem
  { varInfo :: !(IntMap VarInfo),
    objective :: !(Vector RatioInt),
    constraints :: ![(Vector RatioInt, RatioInt)],
    objectiveType :: !ObjectiveType
  }

formatPoly :: IntMap VarInfo -> Vector RatioInt -> String
formatPoly vInfo l =
  let newL =
        catMaybes $
          V.ifoldl' (\acc vIndex c -> let v = vInfo IM.! vIndex in ppMult c v : acc) [] l
   in case newL of
        [] -> "0"
        ((_, s) : tl) -> s <> ppList tl
  where
    ppMult c v
      | c == 0 = Nothing
      | c == 1 = Just (Just "+ ", ppVar v)
      | c == -1 = Just (Nothing, "- " <> ppVar v)
      | c >= 0 = Just (Just "+ ", showRatio c <> "×" <> ppVar v)
      | otherwise = Just (Nothing, "- " <> showRatio (abs c) <> "×" <> ppVar v)

    ppVar VarInfo {varTag, varIndex} = showTag varIndex varTag

    ppList [] = ""
    ppList ((p, s) : xs) = " " <> fromMaybe "" p <> s <> ppList xs

formatConstraint :: IntMap VarInfo -> (Vector RatioInt, RatioInt) -> String
formatConstraint vInfo (coeffs, rhs) =
  let polyStr = formatPoly vInfo coeffs
   in "    " <> polyStr <> " = " <> showRatio rhs

instance Show Problem where
  show :: Problem -> String
  show Problem {objectiveType, varInfo, objective, constraints} =
    let objTypeStr = case objectiveType of
          Minimize -> "min"
          Maximize -> "max"
        objStr = objTypeStr <> " " <> formatPoly varInfo objective
        constraintsBlock = unlines $ formatConstraint varInfo <$> constraints
     in objStr <> "\n  with\n" <> constraintsBlock

data SimplexTableau = SimplexTableau
  { tableau :: !(Matrix RatioInt),
    basis :: !(Vector TableauCol)
  }

data SimplexData = SimplexData
  { artifVars :: !(Set TableauCol),
    origVars :: !(Vector TableauCol),
    origObj :: !(Vector RatioInt)
  }

showTableau :: SimplexTableau -> String
showTableau SimplexTableau {tableau, basis} =
  let fstLine = showRow 0 ""
      body = zipWith (\v rIdx -> showRow rIdx (showVar v)) (V.toList basis) [1 .. nrows tableau - 1]
   in "\n" <> unlines (pad varLine fstLine body)
  where
    nCol = ncols tableau
    nbVar = nCol - 1
    showVar = show
    varLine = ("", "", showVar . TableauCol <$> [0 .. nbVar - 1])
    showRow i c = (c, showRatio $ getElem i 0 tableau, [showRatio $ getElem i j tableau | j <- [1 .. nCol - 1]])
    pad x y z =
      let widest =
            (1 +) . maximum $
              maximum . fmap length . (\(a, b, c) -> a : b : c) <$> x : y : z

          fill str = replicate (widest - length str) ' ' <> str

          fillLine (a, b, c) =
            fill a
              <> " │ "
              <> fill b
              <> " │ "
              <> unwords (fmap fill c)

          sepLine =
            replicate (widest + 1) '─'
              <> "┼"
              <> replicate (widest + 2) '─'
              <> "┼"
              <> replicate ((nCol - 1) * (widest + 1)) '─'
       in fillLine x : sepLine : fillLine y : sepLine : fmap fillLine z

instance Show SimplexTableau where
  show :: SimplexTableau -> String
  show = showTableau

data VariableResult = VariableResult
  { resIndex :: !Int,
    resTag :: !VarTag,
    resVal :: !RatioInt,
    resType :: !VarType
  }

data OptimalResult = OptimalResult
  { variablesValues :: !(IntMap VariableResult),
    optimalCost :: !RatioInt
  }

data Result
  = Infeasible
  | Unbounded
  | Optimal !OptimalResult

instance Show VariableResult where
  show :: VariableResult -> String
  show VariableResult {resTag = NoTag, resIndex, resVal} =
    "_" <> show resIndex <> " = " <> showRatio resVal
  show VariableResult {resTag = SlackVar, resIndex, resVal} =
    "_slack" <> show resIndex <> " = " <> showRatio resVal
  show VariableResult {resTag = Tagged x, resVal} =
    x <> " = " <> showRatio resVal

instance Show Result where
  show :: Result -> String
  show Infeasible = "Infeasible"
  show Unbounded = "Unbounded"
  show (Optimal res) =
    let varValues = fmap (("    " <>) . show) . IM.elems . variablesValues $ res
     in unlines $ ("Optimal Cost: " <> showRatio (optimalCost res)) : "  with" : varValues
