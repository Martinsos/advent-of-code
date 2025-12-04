module Day03 where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)

day03 :: IO (Int, Int)
day03 = solve <$> (getDataFileName "day03-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input =
  let banks = parseBanks input
   in (sum $ findMaxJoltageForBank <$> banks, undefined)

type Bank = [Battery]

type Battery = Int

parseBanks :: String -> [Bank]
parseBanks = ((digitToInt <$>) <$>) . lines

findMaxJoltageForBank :: Bank -> Int
findMaxJoltageForBank bank =
  let (maxFstBat, maxFstBatIdx) = findMax $ init bank
      (maxSndBat, _maxSndBatIdx) = findMax $ drop (maxFstBatIdx + 1) bank
   in maxFstBat * 10 + maxSndBat

findMax :: (Ord a) => [a] -> (a, Int)
findMax xs = let maxX = maximum xs in (maxX, fromJust $ elemIndex maxX xs)

tests :: IO ()
tests = hspec $ do
  specify "findMax" $ do
    findMax [1, 2, 3, 2, 1] `shouldBe` (3 :: Int, 2)
    findMax [1, 2, 4] `shouldBe` (4 :: Int, 2)
  specify "findMaxJoltageForBank" $ do
    findMaxJoltageForBank [1, 3, 1, 2, 1] `shouldBe` 32
    findMaxJoltageForBank [1, 2, 3, 4] `shouldBe` 34
  describe "solve" $ do
    specify "example test case" $
      ( (fst . solve . unlines)
          [ "987654321111111",
            "811111111111119",
            "234234234234278",
            "818181911112111"
          ]
      )
        `shouldBe` 357
  specify "day03" $ do
    day03 >>= ((`shouldBe` 17085) . fst)
