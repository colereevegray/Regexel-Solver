module LangEnum (boundedLang) where
import Data.List
import Types
{-
Takes an integer and a regular expression and generates all the strings in that language up to the integer.

ToDo:
  - Closure handling
    - Create variable lookup table
    - When body or in our datatype RegExp is reached check for presence of that variable
    - Replace variable in body with S SRegExp

  - Char -> Output
    - Lit Char
    - Opt Char

  - Any -> Output (this should be easy, just return a . since it what character it is doesn't matter

  - Opt handling (return the corresponding string with either what its attached to, or without)

  - Sequences
    - RegExp idfk
    - SRegExp recurse to evaluate the simple reg expressions on their own, returning the appropriate strings.

  - Star and Plus - this is going to be painful I can already tell

  - Choice, recurse and evaluate on either choice, appending the results to the string.
-}
boundedLang :: Int -> LRegExp -> [String]
boundedLang k (Let bindings mainExpression) = nub $ generateReg bindings k mainExpression

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