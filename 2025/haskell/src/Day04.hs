module Day04 where

import qualified Data.HashMap.Strict as M
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, it, shouldBe, specify)
import Utils.Grid (Grid)
import qualified Utils.Grid as G

day04 :: IO (Int, Int)
day04 = solve <$> (getDataFileName "day04-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input =
  ( length $ filter (cellHasAccessibleRoll . snd) $ M.toList rollGrid.cellsMap,
    undefined
  )
  where
    rollGrid = charGridToRollGrid $ G.parseGrid input
    cellHasAccessibleRoll = \case
      Just roll -> isRollAccessible roll
      Nothing -> False

newtype Roll = Roll {numNghbs :: Int} deriving (Show, Eq)

charGridToRollGrid :: Grid Char -> Grid (Maybe Roll)
charGridToRollGrid charGrid = charGrid {G.cellsMap = M.mapWithKey toRoll charGrid.cellsMap}
  where
    toRoll idx char = case char of
      '@' -> Just $ Roll $ length $ filter ((== '@') . snd) $ G.getNghbCells charGrid idx
      _notRoll -> Nothing

isRollAccessible :: Roll -> Bool
isRollAccessible (Roll numNghbs) = numNghbs < 4

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let mockCharGrid = G.parseGrid ".@\n@@\n@."
  specify "charGridToRollGrid" $ do
    charGridToRollGrid mockCharGrid
      `shouldBe` G.Grid
        { G.cellsMap =
            M.fromList
              [ ((0, 0), Nothing),
                ((0, 1), Just (Roll 2)),
                ((1, 0), Just (Roll 3)),
                ((1, 1), Just (Roll 3)),
                ((2, 0), Just (Roll 2)),
                ((2, 1), Nothing)
              ],
          G.numRows = 3,
          G.numCols = 2
        }
  describe "solve" $ do
    it "should work for the example input" $
      (fst . solve . unlines)
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
        `shouldBe` 13
  specify "day04" $ do
    day04 >>= ((`shouldBe` 1480) . fst)
