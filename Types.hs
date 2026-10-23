module Types where
data LRegExp =  Let [(Int, RegExp)] RegExp -- Top level, handles the ability to nest let expressions
  deriving (Eq,Show,Read)

{-
- Star, Plus are pretty self explanitory.
- Seq represents a grouped sub expression, * and + are ok here apparently
- S SRegExp acts as a wrapper to ensure the condition of the grammar where we cannot have nested stars.
-}
data RegExp = S SRegExp | Star SRegExp  | Plus SRegExp | Seq [RegExp]
  deriving (Eq,Show,Read)

{-
- Empty, Any, SSeq, Choice, Var are again pretty explanitory
- Lit Char is just the char on its own
- Opt char represents <C>? and OptAny .?
-}
data SRegExp =  Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
  deriving (Eq,Show,Read)
