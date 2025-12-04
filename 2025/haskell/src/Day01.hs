module Day01 where

import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)

day01 :: IO (Int, Int)
day01 = do
  rotations <- (parseRotation <$>) . lines <$> (getDataFileName "day01-input.txt" >>= readFile)

  let dialMovements = scanl (rotate . fst) (initialDial, 0) rotations

  let numTimesDialEndedAtZero = length $ filter ((== Dial 0) . fst) dialMovements
  let numTimesDialClickedAtZero = sum $ map snd dialMovements

  return (numTimesDialEndedAtZero, numTimesDialClickedAtZero)
  where
    initialDial = Dial 50

newtype Dial = Dial Int deriving (Eq, Show)

newtype Rotation = Rotation Int deriving (Show)

type NumZeroClicks = Int

parseRotation :: String -> Rotation
parseRotation =
  Rotation . \case
    'L' : distance -> (-1) * parseDistance distance
    'R' : distance -> 1 * parseDistance distance
    _invalid -> error "invalid rotation"
  where
    parseDistance = read

rotate :: Dial -> Rotation -> (Dial, NumZeroClicks)
rotate (Dial dial) (Rotation rotation) =
  ( Dial $ (dial + rotation) `mod` 100,
    abs ((dial + rotation) `div` 100) + numZeroClicksCorrection dial rotation
  )
  where
    numZeroClicksCorrection d r
      | r < 0 =
          sum
            [ if d == 0 then -1 else 0,
              if (d + r) `mod` 100 == 0 then 1 else 0
            ]
      | otherwise = 0

tests :: IO ()
tests = hspec $ do
  describe "rotate" $ do
    specify "positive rotations" $ do
      rotate (Dial 0) (Rotation 1) `shouldBe` (Dial 1, 0)
      rotate (Dial 0) (Rotation 100) `shouldBe` (Dial 0, 1)
      rotate (Dial 0) (Rotation 101) `shouldBe` (Dial 1, 1)
      rotate (Dial 99) (Rotation 1) `shouldBe` (Dial 0, 1)
      rotate (Dial 99) (Rotation 100) `shouldBe` (Dial 99, 1)
      rotate (Dial 99) (Rotation 101) `shouldBe` (Dial 0, 2)
    specify "negative rotations" $ do
      rotate (Dial 0) (Rotation (-1)) `shouldBe` (Dial 99, 0)
      rotate (Dial 0) (Rotation (-100)) `shouldBe` (Dial 0, 1)
      rotate (Dial 0) (Rotation (-101)) `shouldBe` (Dial 99, 1)
      rotate (Dial 1) (Rotation (-1)) `shouldBe` (Dial 0, 1)
      rotate (Dial 1) (Rotation (-100)) `shouldBe` (Dial 1, 1)
      rotate (Dial 1) (Rotation (-101)) `shouldBe` (Dial 0, 2)
  specify "solution" $ do
    day01 >>= (`shouldBe` (1048, 6498))
