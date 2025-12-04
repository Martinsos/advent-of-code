module Day03 where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)

day03 :: IO (Integer, Integer)
day03 = solve <$> (getDataFileName "day03-input.txt" >>= readFile)

solve :: String -> (Integer, Integer)
solve input =
  let banks = parseBanks input
   in ( sum $ findMaxJoltageForBank 2 <$> banks,
        sum $ findMaxJoltageForBank 12 <$> banks
      )

type Bank = [Battery]

type Battery = Integer

parseBanks :: String -> [Bank]
parseBanks = ((fromIntegral . digitToInt <$>) <$>) . lines

findMaxJoltageForBank :: Int -> Bank -> Integer
findMaxJoltageForBank 0 _ = 0
findMaxJoltageForBank numBatteries bank =
  let (maxFstBat, maxFstBatIdx) = findMax $ take (length bank - numBatteries + 1) bank
   in maxFstBat * 10 ^ (numBatteries - 1) + findMaxJoltageForBank (numBatteries - 1) (drop (maxFstBatIdx + 1) bank)

findMax :: (Ord a) => [a] -> (a, Int)
findMax xs = let maxX = maximum xs in (maxX, fromJust $ elemIndex maxX xs)

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  specify "findMax" $ do
    findMax [1, 2, 3, 2, 1] `shouldBe` (3 :: Int, 2)
    findMax [1, 2, 4] `shouldBe` (4 :: Int, 2)
  specify "findMaxJoltageForBank" $ do
    findMaxJoltageForBank 2 [1, 3, 1, 2, 1] `shouldBe` 32
    findMaxJoltageForBank 2 [1, 2, 3, 4] `shouldBe` 34
    findMaxJoltageForBank 3 [1, 3, 1, 2, 1] `shouldBe` 321
    findMaxJoltageForBank 3 [1, 2, 3, 4] `shouldBe` 234
  describe "solve" $ do
    specify "example test case" $
      ( (solve . unlines)
          [ "987654321111111",
            "811111111111119",
            "234234234234278",
            "818181911112111"
          ]
      )
        `shouldBe` (357, 3121910778619)
  specify "day03" $ do
    day03 >>= (`shouldBe` (17085, 169408143086082))
