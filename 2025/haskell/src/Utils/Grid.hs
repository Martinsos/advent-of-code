module Utils.Grid
  ( Grid (..),
    GridIdx,
    GridRowIdx,
    GridColIdx,
    parseGrid,
    getAllGridIdxs,
    getNghbCells,
    tests,
  )
where

import Control.Arrow (second)
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import Data.Maybe (fromJust, isJust)
import Test.Hspec (hspec, shouldBe, specify)

--------------------------------------------------------------------------------

data Grid a = Grid
  { cellsMap :: !(M.HashMap GridIdx a),
    numCols :: !GridColIdx,
    numRows :: !GridRowIdx
  }
  deriving (Show, Eq)

type GridIdx = (GridRowIdx, GridColIdx)

type GridRowIdx = Int

type GridColIdx = Int

instance Functor Grid where
  fmap f grid = grid {cellsMap = fmap f grid.cellsMap}

parseGrid :: String -> Grid Char
parseGrid input =
  Grid
    { cellsMap = M.fromList $ zip [(r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]] (concat rows),
      numCols,
      numRows
    }
  where
    rows = lines input
    (numCols, numRows) = (length $ head rows, length rows)

-- | Returns an index for each cell in the grid, advancing from top (0) row till last row,
-- and in each row from left (0) column till last column.
getAllGridIdxs :: Grid a -> [GridIdx]
getAllGridIdxs g = [(r, c) | r <- [0 .. g.numRows - 1], c <- [0 .. g.numCols - 1]]

getNghbCells :: Grid a -> GridIdx -> [(GridIdx, a)]
getNghbCells grid (r, c) =
  let idxs = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
   in fmap (second fromJust) <$> filter (isJust . snd) $ (\idx -> (idx, grid.cellsMap M.!? idx)) <$> idxs

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  specify "parseGrid" $ do
    parseGrid (".@\n@@\n@." :: String)
      `shouldBe` Grid
        { cellsMap = M.fromList [((0, 0), '.'), ((0, 1), '@'), ((1, 0), '@'), ((1, 1), '@'), ((2, 0), '@'), ((2, 1), '.')],
          numCols = 2,
          numRows = 3
        }
  let mockGrid = parseGrid (".@\n@@\n@." :: String)
  specify "getAllGridIdxs" $ do
    getAllGridIdxs mockGrid `shouldBe` [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)]
  specify "getNghbCells" $ do
    sort (getNghbCells mockGrid (0, 0)) `shouldBe` sort [((0, 1), '@'), ((1, 0), '@'), ((1, 1), '@')]
    sort (getNghbCells mockGrid (2, 1)) `shouldBe` sort [((1, 0), '@'), ((1, 1), '@'), ((2, 0), '@')]
