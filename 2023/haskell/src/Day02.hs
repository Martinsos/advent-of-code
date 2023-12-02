module Day02 where

import Data.List.Split (splitOn)
import Paths_aoc2023 (getDataFileName)

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  print $ sum $ map fst $ filter (isGamePossible . snd) $ zip [1 :: Int ..] $ parseGame <$> inputLines
  print $ sum $ cubeSetPower . minSuperset . parseGame <$> inputLines

data CubeSet = CubeSet {red :: Int, green :: Int, blue :: Int}

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
isGamePossible = all (`isSubsetOf` CubeSet {red = 12, green = 13, blue = 14})

minSuperset :: [CubeSet] -> CubeSet
minSuperset = foldr1 (\(CubeSet a b c) (CubeSet a' b' c') -> CubeSet (max a a') (max b b') (max c c'))

cubeSetPower :: CubeSet -> Int
cubeSetPower (CubeSet a b c) = a * b * c
