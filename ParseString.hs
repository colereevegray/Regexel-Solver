module ParseString (unparseRegExp) where

import Data.List (intercalate, nub)
import Types ( SRegExp(..), RegExp(..), LRegExp(..) )

unparseRegExp :: LRegExp -> String
unparseRegExp = prettyPrintL

prettyPrintL :: LRegExp -> String
prettyPrintL (Let environment base) =
    concat [ prettyEnvironment e | e <- environment] ++ prettyPrintR base
    where
        prettyEnvironment (variable, expression) = "let x" ++ show variable ++ " = " ++ prettyPrintR expression ++ " in "

prettyPrintR :: RegExp -> String
prettyPrintR (S s)    = prettyPrintS s
prettyPrintR (Star s) = wrapForIterator s ++ "*"
prettyPrintR (Plus s) = wrapForIterator s ++ "+"
prettyPrintR (Seq rs) = concatMap wrapForSeq rs

prettyPrintS :: SRegExp -> String
prettyPrintS Empty       = "e"
prettyPrintS (Lit c)     = [c]
prettyPrintS (Opt c)     = [c] ++ "?"
prettyPrintS Any         = "."
prettyPrintS OptAny      = ".?"
prettyPrintS (Var i)     = "x" ++ show i
prettyPrintS (SSeq cs)   =
    let nonEmpties = filter (/= Empty) (concatMap flattenSSeq cs)
    in case nonEmpties of
        []  -> "e"
        _   -> concatMap wrapForSSeq nonEmpties
prettyPrintS (Choice cs) =
    let uniqueChoices = nub (concatMap flattenChoice cs)
    in intercalate "|" (map prettyPrintS uniqueChoices)

flattenSSeq :: SRegExp -> [SRegExp]
flattenSSeq (SSeq ss) = concatMap flattenSSeq ss
flattenSSeq s         = [s]


flattenChoice :: SRegExp -> [SRegExp]
flattenChoice (Choice cs) = concatMap flattenChoice cs
flattenChoice s           = [s]

wrapForIterator :: SRegExp -> String
wrapForIterator s@(SSeq _)   = "(" ++ prettyPrintS s ++ ")"
wrapForIterator s@(Choice _) = "(" ++ prettyPrintS s ++ ")"
wrapForIterator s            = prettyPrintS s

wrapForSeq :: RegExp -> String
wrapForSeq (S s@(Choice _)) = "(" ++ prettyPrintS s ++ ")"
wrapForSeq r                = prettyPrintR r

wrapForSSeq :: SRegExp -> String
wrapForSSeq s@(Choice _) = "(" ++ prettyPrintS s ++ ")"
wrapForSSeq s            = prettyPrintS s