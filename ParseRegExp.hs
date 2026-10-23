module ParseRegExp (parseRegExp) where
import Data.List (nub)
import Parsing
import Types
parseRegExp :: String -> Maybe LRegExp
parseRegExp string = case [r | (r,"") <- parse lParser string] of
                          (result : _) -> Just result
                          [] -> Nothing

environmentParser :: Parser (Int, RegExp)
environmentParser = do symbol "let"
                       v <- vParser
                       symbol "="
                       e <- rParser
                       symbol "in"
                       return (v, e)

lParser :: Parser LRegExp
lParser = do enviroment <- many environmentParser
             finalExpr <- rParser
             return (Let enviroment finalExpr)

rParser :: Parser RegExp
rParser =
  (do xs <- some rUnit
      return $ case xs of
                  [x] -> x
                  _   -> Seq (concatMap flattenRSeq xs))
  <|> (do s <- sParser
          return (S s))
  where
    flattenRSeq (Seq ss) = ss
    flattenRSeq other    = [other]

rUnit :: Parser RegExp
rUnit =
  (do u <- uParser
      char '*'
      return (Star u))
  <|> (do u <- uParser
          char '+'
          return (Plus u))
  <|> (do char '('
          r <- rParser
          char ')'
          return r)

sParser :: Parser SRegExp
sParser = do
  xs <- some uParser
  let nonEmpties = filter (/= Empty) xs
  let base = case nonEmpties of
              []  -> Empty
              [x] -> x
              _   -> SSeq (concatMap flattenSSeq nonEmpties)
  (do char '|'
      rest <- sParser
      return (Choice (nub (base: flattenChoice rest))))
   <|> return base
  where
    flattenChoice (Choice cs) = cs
    flattenChoice other = [other]
    flattenSSeq (SSeq ss) = ss
    flattenSSeq other = [other]

uParser :: Parser SRegExp
uParser = do
  v <- vParser
  return (Var v)
  <|> do
    c <- upper
    (do char '?'
        return (Opt c))
     <|> (do return (Lit c))
  <|> do
    opt <- char '.'
    (do char '?'
        return OptAny)
     <|> (do return Any)
  <|> do
    char 'e'
    return Empty
  <|> do char '('
         s <- sParser
         char ')'
         return s

vParser:: Parser Int
vParser = do
  char 'x'
  number <- nat
  return number