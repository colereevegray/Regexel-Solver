module RegexelSolver where

import Data.List
import Types

solveRegexle :: [LRegExp] -> String
solveRegexle expressions = concatMap head (finalBanks !! 0)
    where
        side = getSideLength expressions
        (table, rowIDs, colIDs, diaIDs) = createMappings side
        initialBanks = initializeWordBanks expressions
        finalBanks = solveLoop initialBanks table rowIDs colIDs diaIDs

solveStep :: [[[String]]] -> [((Int, Int), (Int, Int), (Int, Int))] -> [[Int]] -> [[Int]] -> [[Int]] -> [[[String]]]
solveStep banks table rowIds colIds diaIds= [newH, newC, newD]
    where
        size = length table
        allCellPoss = [getConstraints i banks table | i <- [0..size-1]]

        hConstraints = map (map (allCellPoss !!)) rowIds
        cConstraints = map (map (allCellPoss !!)) colIds
        dConstraints = map (map (allCellPoss !!)) diaIds

        newH = zipWith pruneBank hConstraints (banks !! 0)
        newC = zipWith pruneBank cConstraints (banks !! 1)
        newD = zipWith pruneBank dConstraints (banks !! 2)

solveLoop:: [[[String]]] -> [((Int, Int), (Int, Int), (Int, Int))] -> [[Int]] -> [[Int]] -> [[Int]] -> [[[String]]]
solveLoop banks table rIDs cIDs dIDs
    | banks == nextBanks = banks
    | otherwise          = solveLoop nextBanks table rIDs cIDs dIDs
    where nextBanks = solveStep banks table rIDs cIDs dIDs

createMappings :: Int -> ([((Int, Int), (Int, Int), (Int, Int))], [[Int]], [[Int]], [[Int]])
createMappings sideLength = (lookupTable, rows, columns, diagonals)
    where
        indexes = [0..sizeOfHexagon sideLength-1]
        maxWidth = 2*sideLength - 1
        rowLengths = [sideLength .. (2*sideLength-2)] ++ [maxWidth] ++ [2*sideLength-2, 2*sideLength-3 .. sideLength]


        padded = zipWith pad (splitBy rowLengths indexes) [0..]
            where pad row rowIndex = let p = maxWidth - length row
                    in if rowIndex < sideLength -1
                        then replicate p (-1) ++ row
                        else row ++ replicate p (-1)

        rows = map (filter (/=(-1))) padded
        columns = map (filter (/=(-1))) (transpose padded)
        diagonals = map (reverse . filter (/=(-1))) (transpose (zipWith shear padded [0..]))

        shear row rowIndex = if rowIndex < sideLength -1
                                then filter (/=(-1)) row ++ replicate (length (filter (==(-1)) row)) (-1)
                                else replicate (length (filter (==(-1)) row)) (-1) ++ filter (/=(-1)) row

        rowMappings = sortOn fst [(cellId, (lineId, posInLine)) |
                        (lineId, line) <- zip [0..] rows,
                        (posInLine, cellId) <- zip [0..] line ]

        columnMappings = sortOn fst [(cellId, (lineId, posInLine)) |
                        (lineId, line) <- zip [0..] columns,
                        (posInLine, cellId) <- zip [0..] line ]

        diagonalMappings = sortOn fst [(cellId, (lineId, posInLine)) |
                        (lineId, line) <- zip [0..] diagonals,
                        (posInLine, cellId) <- zip [0..] line ]
        lookupTable = zipWith3 (\r c d -> (r, c, d)) (map snd rowMappings) (map snd columnMappings) (map snd diagonalMappings)

splitBy :: [Int] -> [Int] -> [[Int]]
splitBy [] _ = []
splitBy (x:xs) cs = take x cs : splitBy xs (drop x cs)

possibleChoices :: [String] -> [String]
possibleChoices = map nub . transpose

initializeWordBanks :: [LRegExp] -> [[[String]]]
initializeWordBanks expressions = [hBank, cBank, dBank]
    where
        sideLength = getSideLength expressions
        bankSize = 2*sideLength-1
        rowLengths = [sideLength .. (2*sideLength-2)] ++ [bankSize] ++ [2*sideLength-2, 2*sideLength-3 .. sideLength]
        hBankExp = take bankSize expressions
        cBankExp = take bankSize $ drop bankSize expressions
        dBankExp = reverse $ drop (2*bankSize) expressions

        hBank = zipWith (\x y -> boundedLang y x) hBankExp rowLengths
        cBank = zipWith (\x y -> boundedLang y x) cBankExp rowLengths
        dBank = zipWith (\x y -> boundedLang y x) dBankExp rowLengths

getConstraints :: Int -> [[[String]]] -> [((Int, Int), (Int, Int), (Int, Int))] -> [Char]
getConstraints cellId [hBank, cBank, dBank] lookupTable =
    let ((rowId, rowPosition), (colId, colPosition), (diaId, diaPosition)) = lookupTable !! cellId
        rPoss = getPossibilities (hBank !! rowId) rowPosition
        cPoss = getPossibilities (cBank !! colId) colPosition
        dPoss = getPossibilities (dBank !! diaId) diaPosition
    in foldl1 intersector [rPoss, cPoss, dPoss]

getPossibilities :: [String] -> Int -> [Char]
getPossibilities bank pos =
    if null bank then [] else (possibleChoices bank) !! pos

intersector :: [Char] -> [Char] -> [Char]
intersector xs ys
    | '.' `elem` xs && '.' `elem` ys = nub (xs ++ ys)
    | '.' `elem` xs = ys
    | '.' `elem` ys = xs
    | otherwise = xs `intersect` ys

pruneBank :: [[Char]] -> [String] -> [String]
pruneBank constraints bank = concatMap (refineWord constraints) bank

refineWord :: [[Char]] -> String -> [String]
refineWord constraints word = sequence choices
  where
    choices = zipWith refineChar word constraints

    refineChar :: Char -> [Char] -> [Char]
    refineChar c constr
      | c == '.' = if '.' `elem` constr then ['.'] else constr
      | otherwise = if '.' `elem` constr || c `elem` constr then [c] else []

-- Geometry
getSideLength :: [LRegExp] -> Int
getSideLength expressions = ((length expressions `div` 3) + 1) `div` 2

sizeOfHexagon :: Int -> Int
sizeOfHexagon sideLength = 3*sideLength^2 -3*sideLength+1

-- The Below code is from exercise 1:
boundedLang :: Int -> LRegExp -> [String]
boundedLang k (Let bindings mainExpression) = filter (\s -> length s == k) $ nub $ generateReg bindings k mainExpression

-- Takes top level LRegExp, generated string max length, and root expression, returning the list of possible strings
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