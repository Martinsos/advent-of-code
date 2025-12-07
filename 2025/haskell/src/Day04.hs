{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day04 where

import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, it, shouldBe, specify)
import Utils.Grid (Grid)
import qualified Utils.Grid as G

day04 :: IO (Int, Int)
day04 = solve <$> (getDataFileName "day04-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input =
  ( length $ getAllImmediatellyAccessibleRolls rollGrid,
    countAllUltimatelyAccessibleRolls rollGrid
  )
  where
    rollGrid = charGridToRollGrid $ G.parseGrid input

newtype Roll = Roll {numNghbs :: Int}
  deriving (Show, Eq, Num)

charGridToRollGrid :: Grid Char -> Grid Roll
charGridToRollGrid charGrid =
  G.Grid
    { G.cellsMap = M.mapMaybeWithKey toRoll charGrid.cellsMap,
      G.numCols = charGrid.numCols,
      G.numRows = charGrid.numRows
    }
  where
    toRoll idx char = case char of
      '@' -> Just $ Roll $ length $ filter ((== '@') . snd) $ G.getNghbCells charGrid idx
      _notRoll -> Nothing

isRollImmediatellyAccessible :: Roll -> Bool
isRollImmediatellyAccessible (Roll numNghbs) = numNghbs < 4

getAllImmediatellyAccessibleRolls :: Grid Roll -> [G.GridIdx]
getAllImmediatellyAccessibleRolls grid = M.keys $ M.filter isRollImmediatellyAccessible grid.cellsMap

countAllUltimatelyAccessibleRolls :: Grid Roll -> Int
countAllUltimatelyAccessibleRolls initGrid = go initGrid (G.getAllGridIdxs initGrid)
  where
    go :: Grid Roll -> [G.GridIdx] -> Int
    go _ [] = 0
    go grid (candidateIdx : restCandidateIdxs) = case grid.cellsMap M.!? candidateIdx of
      Just roll
        | isRollImmediatellyAccessible roll ->
            let grid' = removeRoll grid candidateIdx
                candidateIdxs' = (fst <$> G.getNghbCells grid candidateIdx) <> restCandidateIdxs
             in 1 + go grid' candidateIdxs'
      _notAccessibleRoll -> go grid restCandidateIdxs

removeRoll :: Grid Roll -> G.GridIdx -> Grid Roll
removeRoll grid idxToRemove = deleteRoll idxToRemove $ reduceNumsNghbs nghbsIdxs grid
  where
    nghbsIdxs = fst <$> G.getNghbCells grid idxToRemove
    reduceNumsNghbs idxs g = g {G.cellsMap = foldl' reduceNumNghbForRoll g.cellsMap idxs}
    reduceNumNghbForRoll cells idx = M.adjust (subtract 1) idx cells
    deleteRoll idx g = g {G.cellsMap = M.delete idx g.cellsMap}

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let mockCharGrid = G.parseGrid ".@\n@@\n@@"
  specify "charGridToRollGrid" $ do
    charGridToRollGrid mockCharGrid
      `shouldBe` G.Grid
        { G.cellsMap = M.fromList [((0, 1), Roll 2), ((1, 0), Roll 4), ((1, 1), Roll 4), ((2, 0), Roll 3), ((2, 1), Roll 3)],
          G.numRows = 3,
          G.numCols = 2
        }
  let mockGrid = charGridToRollGrid mockCharGrid
  specify "getAllImmediatellyAccessibleRolls" $ do
    getAllImmediatellyAccessibleRolls mockGrid `shouldBe` [(0, 1), (2, 0), (2, 1)]
  specify "countAllUltimatelyAccessibleRolls" $ do
    countAllUltimatelyAccessibleRolls mockGrid `shouldBe` 5
  describe "solve" $ do
    it "should work for the example input" $
      (solve . unlines)
        [ "..@@.@@@@.",
          "@@@.@.@.@@",
          "@@@@@.@.@@",
          "@.@@@@..@.",
          "@@.@@@@.@@",
          ".@@@@@@@.@",
          ".@.@.@.@@@",
          "@.@@@.@@@@",
          ".@@@@@@@@.",
          "@.@.@@@.@."
        ]
        `shouldBe` (13, 43)
  specify "day04" $ do
    day04 >>= (`shouldBe` (1480, 8899))
