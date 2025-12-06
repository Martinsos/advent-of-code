module Day04 where

import Control.Arrow (second)
import Data.Maybe (fromJust, isJust, isNothing)
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, it, shouldBe, specify)
import Utils.Grid (Grid)
import qualified Utils.Grid as G

day04 :: IO (Int, Int)
day04 = solve <$> (getDataFileName "day04-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input =
  ( length $ filter id $ isAccessiblePaperRoll <$> allGridIdxs,
    undefined
  )
  where
    grid = parseGrid input
    allGridIdxs = [(r, c) | r <- [0 .. (grid.height - 1)], c <- [0 .. (grid.width - 1)]]
    isAccessiblePaperRoll idx = grid G.@!? idx == Just True && isAccessible grid idx == Just True

parseGrid :: String -> Grid Bool
parseGrid input = let rows = lines input in fmap (== '@') $ G.fromList (length $ head rows) $ concat rows

isAccessible :: Grid Bool -> (Int, Int) -> Maybe Bool
isAccessible grid (r, c) | isNothing (grid G.@!? (r, c)) = Nothing
isAccessible grid (r, c) = Just $ (< 4) $ length $ filter id $ snd <$> getAdjacentCells grid (r, c)

getAdjacentCells :: Grid a -> (Int, Int) -> [((Int, Int), a)]
getAdjacentCells grid (r, c) =
  let idxs = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
   in map (second fromJust) <$> filter (isJust . snd) $ zip idxs ((grid G.@!?) <$> idxs)

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let mockGrid = parseGrid "..@\n@@@\n@.@\n.@."
  specify "parseGrid" $ do
    parseGrid "..@\n@@@\n@.@\n.@."
      `shouldBe` G.fromList 3 [False, False, True, True, True, True, True, False, True, False, True, False]
  specify "getAdjacentCells" $ do
    getAdjacentCells mockGrid (0, 0) `shouldBe` [((0, 1), False), ((1, 0), True), ((1, 1), True)]
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
