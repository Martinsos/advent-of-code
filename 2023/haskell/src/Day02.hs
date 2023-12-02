module Day02 where

import Data.List.Split (splitOn)
import Paths_aoc2023 (getDataFileName)

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
-- Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
-- Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
-- Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
-- Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  print $ sum $ map fst $ filter (isGamePossible . snd) $ zip [1 :: Int ..] $ parseGame <$> inputLines

data CubeSet = CubeSet {red :: Int, green :: Int, blue :: Int}
  deriving (Show)

instance Monoid CubeSet where
  mempty = CubeSet 0 0 0

instance Semigroup CubeSet where
  (CubeSet a b c) <> (CubeSet a' b' c') = CubeSet (a + a') (b + b') (c + c')

parseGame :: String -> [CubeSet]
parseGame = (parseCubeSet <$>) . splitOn "; " . (!! 1) . splitOn ": "
  where
    parseCubeSet :: String -> CubeSet
    parseCubeSet = mconcat . (parseCubeColorPick <$>) . splitOn ", "

    parseCubeColorPick :: String -> CubeSet
    parseCubeColorPick str = case splitOn " " str of
      [num, color]
        | color == "red" -> mempty {red = read num}
        | color == "green" -> mempty {green = read num}
        | color == "blue" -> mempty {blue = read num}
      _ -> error "invalid cube color pick"

isSubsetOf :: CubeSet -> CubeSet -> Bool
isSubsetOf (CubeSet a b c) (CubeSet a' b' c') = a <= a' && b <= b' && c <= c'

isGamePossible :: [CubeSet] -> Bool
isGamePossible = all (`isSubsetOf` maxSet)
  where
    maxSet = CubeSet {red = 12, green = 13, blue = 14}
