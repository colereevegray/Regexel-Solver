module RegexelVerifier (checkRegexle) where
import Data.List

import Types

checkRegexle :: String -> [LRegExp] -> [Bool]
checkRegexle hex expressions = returnList pairedWords
  where
    sideLength = hexStringToSideLength hex
    words = extractAllWords sideLength hex
    pairedWords = words2Regex words expressions

    returnList [] = []
    returnList (x:xs) = wordChecker x sideLength : returnList xs

{-
Convert from the flat hexagon string into a series of lists corresponding to
the expressions surrounding the hexagon
-}
extractAllWords :: Int -> String -> [String]
extractAllWords sideLength hex = rows ++ columns ++ reverse diagonals -- column being expressions 6-8, diagonals being 11-15
    where
        maxWidth = 2*sideLength-1
        rowLengths = [sideLength .. (2*sideLength-2)] ++ [maxWidth] ++ [2*sideLength-2, 2*sideLength-3 .. sideLength]

        padded = zipWith pad (splitBy rowLengths hex) [0..]
            where pad row rowIndex = let p = maxWidth - length row
                    in if rowIndex < sideLength -1
                        then replicate p '#' ++ row
                        else row ++ replicate p '#'

        rows = map (filter (/='#')) padded
        columns = map (filter (/='#')) (transpose padded)
        diagonals = map (reverse . filter (/='#')) (transpose (zipWith shear padded [0..]))

        shear row rowIndex = if rowIndex < sideLength -1
                                then filter (/='#') row ++ replicate (length (filter (=='#') row)) '#'
                                else replicate (length (filter (=='#') row)) '#' ++ filter (/='#') row

splitBy :: [Int] -> String -> [String]
splitBy [] _ = []
splitBy (x:xs) cs = take x cs : splitBy xs (drop x cs)

-- map words to regex
words2Regex :: [String] -> [LRegExp] -> [(String, LRegExp)]
words2Regex = zip

-- Take a String Regex pair and check if it is contained within the expressions generated
wordChecker :: (String, LRegExp) -> Int -> Bool
wordChecker (word, expresssion) sideLength = containsMatch word (boundedLang (2*sideLength-1) expresssion)

matches :: String -> String -> Bool
matches target pattern =
  length target == length pattern && all (\(x,y) -> x == y || y == '.') (zip target pattern)

containsMatch :: String -> [String] -> Bool
containsMatch target = any (matches target)

-- Get the side length of a hexagon
hexQuadHelper :: Int ->(Double -> Double -> Double) -> Int
hexQuadHelper h op = floor $ op 3 ( sqrt (-3 + (12*fromIntegral h)))/6

hexStringToSideLength :: String -> Int
hexStringToSideLength h = maximum ( hexQuadHelper len (-): [hexQuadHelper len (+)])
    where len = length h

-- Code from ex1 used to generate the possible matches for a given string:
boundedLang :: Int -> LRegExp -> [String]
boundedLang k (Let bindings mainExpression) = nub $ generateReg bindings k mainExpression

generateReg :: [(Int,  RegExp)] -> Int -> RegExp -> [String]
generateReg environment bound base = case base of
  S sreg -> generateSReg environment bound sreg
  Seq regexp -> regSequenceHelper environment bound regexp
  Star sreg -> "" : starHelper environment bound sreg
  Plus sreg -> starHelper environment bound sreg

regSequenceHelper :: [(Int, RegExp)] -> Int -> [RegExp] -> [String]
regSequenceHelper _ _ [] = [""]
regSequenceHelper environment bound (expression:restExpressions) =
  [ x++y | x <- generateReg environment bound expression,
           y <- regSequenceHelper environment (bound - length x) restExpressions ]

starHelper :: [(Int, RegExp)] -> Int -> SRegExp -> [String]
starHelper environment bound expression
  | bound <= 0 = []
  | otherwise =
      [ x++y |  x <- generateSReg environment bound expression
              , not (null x)
              , y <- "" : starHelper environment (bound - length x) expression ]

generateSReg :: [(Int, RegExp)] -> Int -> SRegExp -> [String]
generateSReg environment bound expression = case expression of
  Empty -> [""]
  Lit character -> if bound >= 1 then [[character]] else []
  Opt character -> if bound >= 1 then ["", [character]] else [""]
  Any           -> if bound >= 1 then ["."] else []
  OptAny        -> if bound >= 1 then ["", "."] else [""]

  Var i -> case lookup i environment of
    Just expression -> generateReg environment bound expression
    Nothing -> error $ "no instance of variable"++ show i

  Choice expressions -> concatMap (generateSReg environment bound) expressions

  SSeq expressions -> sRegSequenceHelper environment bound expressions

sRegSequenceHelper :: [(Int, RegExp)] -> Int -> [SRegExp] -> [String]
sRegSequenceHelper _ _ [] = [""]
sRegSequenceHelper environment bound (expression:restExpressions) =
  [ x++y | x <- generateSReg environment bound expression,
       y <- sRegSequenceHelper environment (bound - length x) restExpressions]