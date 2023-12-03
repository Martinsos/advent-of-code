module Day03 where

import Data.Char (isDigit)
import Data.Foldable (foldl')
import Paths_aoc2023 (getDataFileName)

day03 :: IO ()
day03 = do
  schemaLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  let schemaLines' = repeat '.' : schemaLines ++ [repeat '.']
  print $
    (sum . concatMap extractPartNumbers) $
      zip3 schemaLines' (drop 1 schemaLines') (drop 2 schemaLines')

data FieldType = Symbol | Empty | Digit
  deriving (Eq)

extractPartNumbers :: (String, String, String) -> [Int]
extractPartNumbers (prevLine, line, nextLine) =
  fourth $
    foldl'
      ( \(afterSymbol, curNum, isCurNumPart, partNumbers) (fieldUp, field, fieldDown) ->
          let atSymbol = any ((== Symbol) . fieldType) [fieldUp, field, fieldDown]
              afterSymbol' = atSymbol
              curNum' =
                if fieldType field == Digit
                  then curNum ++ [field]
                  else ""
              partNumbers' =
                if fieldType field /= Digit && not (null curNum) && (isCurNumPart || atSymbol)
                  then partNumbers ++ [read curNum]
                  else partNumbers
              isCurNumPart' = not (null curNum') && (afterSymbol || atSymbol || isCurNumPart)
           in (afterSymbol', curNum', isCurNumPart', partNumbers')
      )
      (False, "" :: String, False, [])
      (zip3 prevLine line nextLine ++ [('.', '.', '.')])

fieldType :: Char -> FieldType
fieldType '.' = Empty
fieldType c | isDigit c = Digit
fieldType _ = Symbol

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x
