{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Solver.Data where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RatioInt
import Data.Set (Set)
import Data.Vector.Strict (Vector)
import Solver.Matrix

newtype TableauCol = TableauCol Int
  deriving (Eq, Ord)

data VarType = IntegerVar | RealVar

data ObjectiveType = Maximize | Minimize

data VarTag = NoTag | SlackVar | Tagged !String

data Problem = Problem
  { nbVars :: Int,
    objectiveType :: ObjectiveType,
    varTags :: Map TableauCol VarTag,
    objective :: Map TableauCol RatioInt,
    constraints :: [(Map TableauCol RatioInt, RatioInt)],
    intVars :: Set TableauCol
  }

data SimplexTableau = SimplexTableau
  { tableau :: !(Matrix RatioInt),
    basis :: !(Vector TableauCol)
  }

data SimplexData = SimplexData
  { artifVars :: !(Set TableauCol),
    origVars :: !(Vector TableauCol),
    origObj :: !(Vector RatioInt)
  }

data VariableResult = VariableResult
  { resIndex :: Int,
    resTag :: VarTag,
    resVal :: RatioInt,
    resType :: VarType
  }

data OptimalResult = OptimalResult
  { variablesValues :: [VariableResult],
    optimalCost :: !RatioInt
  }

data Result
  = Infeasible
  | Unbounded
  | Optimal !OptimalResult

showRatio :: RatioInt -> String
showRatio r
  | denominator r == 1 = show $ numerator r
  | otherwise = show (numerator r) <> "/" <> show (denominator r)

showTag :: Int -> VarTag -> String
showTag idx NoTag = "_" <> show idx
showTag idx SlackVar = "_slack" <> show idx
showTag _ (Tagged x) = x

formatPoly :: Map TableauCol VarTag -> Map TableauCol RatioInt -> String
formatPoly vInfo l =
  let newL = map ppMult $ M.assocs l
   in case newL of
        [] -> "0"
        ((_, s) : tl) -> unwords $ s : fmap (uncurry (<>)) tl
  where
    ppMult (v, c)
      | c == 1 = ("+ ", ppVar v)
      | c == -1 = ("", "- " <> ppVar v)
      | c >= 0 = ("+ ", showRatio c <> "×" <> ppVar v)
      | otherwise = ("", "- " <> showRatio (abs c) <> "×" <> ppVar v)

    ppVar vCol@(TableauCol i) = showTag i $ vInfo M.! vCol

formatConstraint :: Map TableauCol VarTag -> (Map TableauCol RatioInt, RatioInt) -> String
formatConstraint vInfo (coeffs, rhs) =
  let polyStr = formatPoly vInfo coeffs
   in "    " <> polyStr <> " = " <> showRatio rhs

instance Show Problem where
  show :: Problem -> String
  show Problem {objectiveType, varTags, objective, constraints} =
    let objTypeStr = case objectiveType of
          Minimize -> "min"
          Maximize -> "max"
        objStr = objTypeStr <> " " <> formatPoly varTags objective
        constraintsBlock = unlines $ formatConstraint varTags <$> constraints
     in objStr <> "\n  with\n" <> constraintsBlock

-- showTableau :: SimplexTableau -> String
-- showTableau SimplexTableau {tableau, basis} =
--   let fstLine = showRow 0 ""
--       body = zipWith (\v rIdx -> showRow rIdx (showVar v)) (V.toList basis) [1 .. nrows tableau - 1]
--    in "\n" <> unlines (pad varLine fstLine body)
--   where
--     nCol = ncols tableau
--     nbVar = nCol - 1
--     showVar = show
--     varLine = ("", "", showVar . TableauCol <$> [0 .. nbVar - 1])
--     showRow i c = (c, showRatio $ getElem i 0 tableau, [showRatio $ getElem i j tableau | j <- [1 .. nCol - 1]])
--     pad x y z =
--       let widest =
--             (1 +) . maximum $
--               maximum . fmap length . (\(a, b, c) -> a : b : c) <$> x : y : z

--           fill str = replicate (widest - length str) ' ' <> str

--           fillLine (a, b, c) =
--             fill a
--               <> " │ "
--               <> fill b
--               <> " │ "
--               <> unwords (fmap fill c)

--           sepLine =
--             replicate (widest + 1) '─'
--               <> "┼"
--               <> replicate (widest + 2) '─'
--               <> "┼"
--               <> replicate ((nCol - 1) * (widest + 1)) '─'
--        in fillLine x : sepLine : fillLine y : sepLine : fmap fillLine z

-- instance Show SimplexTableau where
--   show :: SimplexTableau -> String
--   show = showTableau

instance Show VariableResult where
  show :: VariableResult -> String
  show VariableResult {resTag, resIndex, resVal} =
    showTag resIndex resTag <> " = " <> showRatio resVal

instance Show Result where
  show :: Result -> String
  show Infeasible = "Infeasible"
  show Unbounded = "Unbounded"
  show (Optimal res) =
    let varValues = fmap (("    " <>) . show) . variablesValues $ res
     in unlines $ ("Optimal Cost: " <> showRatio (optimalCost res)) : "  with" : varValues
