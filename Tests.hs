import Types
import LangEnum (boundedLang)
import ParseRegExp ( parseRegExp )
import ParseString ( unparseRegExp )
import RegexelVerifier (checkRegexle)
import RegexelSolver (solveRegexle)
import Lambda (countReductions)
import Test.HUnit
import Data.List (sort)

{-Tests for Question 1-}
-- Precheck Tests (cover different variables interacting mainly, so did not include many variable tests other than
-- from the precheck)
-- boundedLang 1 (Let [] (S Empty))
q1TestCase0 :: LRegExp
q1TestCase0 = Let [] (S Empty)

q1TestResult0 :: [String]
q1TestResult0 = [""]

question1PrecheckTest0 :: Test
question1PrecheckTest0 = "Precheck Case 0" ~: sort q1TestResult0 @=? sort (boundedLang 10 q1TestCase0)

-- let x1 = ABC in let x2 = DEF in (x1|x2)+   Bounded to length 10
q1TestCase1 :: LRegExp
q1TestCase1 = Let [
  (1, S (SSeq[Lit 'A', Lit 'B', Lit 'C'])),
  (2, S (SSeq[Lit 'D', Lit 'E', Lit 'F']))
  ]
  (Plus (Choice[Var 1, Var 2]))

q1TestResult1 :: [String]
q1TestResult1 = ["ABC","ABCABC","ABCABCABC","ABCABCDEF","ABCDEF","ABCDEFABC","ABCDEFDEF","DEF","DEFABC","DEFABCABC","DEFABCDEF","DEFDEF","DEFDEFABC","DEFDEFDEF"]

question1PrecheckTest1 :: Test
question1PrecheckTest1 = "Precheck Case 1" ~: sort q1TestResult1 @=? sort (boundedLang 10 q1TestCase1)

-- let x1 = (AB)* in let x2 = (BC)* in let x3 = Bex2D? in x1x2.*x1x2(F.?|DE.?)x3 Bounded to length 10
q1TestCase2 :: LRegExp
q1TestCase2 = Let [
  (1, Star (SSeq[Lit 'A', Lit 'B'])),
  (2, Star (SSeq[Lit 'B', Lit 'C'])),
  (3, S (
    SSeq[
      Lit 'B',
      Empty,
      Var 2,
      Opt 'D'
      ]))
  ] (Seq [S(Var 1), S(Var 2), Star (Any), S (Var 1), S(Var 2), S(Choice[SSeq [Lit 'F', OptAny], SSeq [Lit 'D', Lit 'E', OptAny]]), S(Var 3)])

q1TestResult2 :: [String]
q1TestResult2 = ["........FB",".......DEB",".......F.B",".......FB",".......FBD","......ABFB","......BCFB","......DE.B","......DEB","......DEBD","......F.B","......F.BD","......FB","......FBBC","......FBD",".....ABDEB",".....ABF.B",".....ABFB",".....ABFBD",".....BCDEB",".....BCF.B",".....BCFB",".....BCFBD",".....DE.B",".....DE.BD",".....DEB",".....DEBBC",".....DEBD",".....F.B",".....F.BBC",".....F.BD",".....FB",".....FBBC",".....FBBCD",".....FBD","....ABABFB","....ABBCFB","....ABDE.B","....ABDEB","....ABDEBD","....ABF.B","....ABF.BD","....ABFB","....ABFBBC","....ABFBD","....BCBCFB","....BCDE.B","....BCDEB","....BCDEBD","....BCF.B","....BCF.BD","....BCFB","....BCFBBC","....BCFBD","....DE.B","....DE.BBC","....DE.BD","....DEB","....DEBBC","....DEBBCD","....DEBD","....F.B","....F.BBC","....F.BBCD","....F.BD","....FB","....FBBC","....FBBCBC","....FBBCD","....FBD","...ABABDEB","...ABABF.B","...ABABFB","...ABABFBD","...ABBCDEB","...ABBCF.B","...ABBCFB","...ABBCFBD","...ABDE.B","...ABDE.BD","...ABDEB","...ABDEBBC","...ABDEBD","...ABF.B","...ABF.BBC","...ABF.BD","...ABFB","...ABFBBC","...ABFBBCD","...ABFBD","...BCBCDEB","...BCBCF.B","...BCBCFB","...BCBCFBD","...BCDE.B","...BCDE.BD","...BCDEB","...BCDEBBC","...BCDEBD","...BCF.B","...BCF.BBC","...BCF.BD","...BCFB","...BCFBBC","...BCFBBCD","...BCFBD","...DE.B","...DE.BBC","...DE.BBCD","...DE.BD","...DEB","...DEBBC","...DEBBCBC","...DEBBCD","...DEBD","...F.B","...F.BBC","...F.BBCBC","...F.BBCD","...F.BD","...FB","...FBBC","...FBBCBC","...FBBCBCD","...FBBCD","...FBD","..ABABABFB","..ABABBCFB","..ABABDE.B","..ABABDEB","..ABABDEBD","..ABABF.B","..ABABF.BD","..ABABFB","..ABABFBBC","..ABABFBD","..ABBCBCFB","..ABBCDE.B","..ABBCDEB","..ABBCDEBD","..ABBCF.B","..ABBCF.BD","..ABBCFB","..ABBCFBBC","..ABBCFBD","..ABDE.B","..ABDE.BBC","..ABDE.BD","..ABDEB","..ABDEBBC","..ABDEBBCD","..ABDEBD","..ABF.B","..ABF.BBC","..ABF.BBCD","..ABF.BD","..ABFB","..ABFBBC","..ABFBBCBC","..ABFBBCD","..ABFBD","..BCBCBCFB","..BCBCDE.B","..BCBCDEB","..BCBCDEBD","..BCBCF.B","..BCBCF.BD","..BCBCFB","..BCBCFBBC","..BCBCFBD","..BCDE.B","..BCDE.BBC","..BCDE.BD","..BCDEB","..BCDEBBC","..BCDEBBCD","..BCDEBD","..BCF.B","..BCF.BBC","..BCF.BBCD","..BCF.BD","..BCFB","..BCFBBC","..BCFBBCBC","..BCFBBCD","..BCFBD","..DE.B","..DE.BBC","..DE.BBCBC","..DE.BBCD","..DE.BD","..DEB","..DEBBC","..DEBBCBC","..DEBBCBCD","..DEBBCD","..DEBD","..F.B","..F.BBC","..F.BBCBC","..F.BBCBCD","..F.BBCD","..F.BD","..FB","..FBBC","..FBBCBC","..FBBCBCBC","..FBBCBCD","..FBBCD","..FBD",".ABABABDEB",".ABABABF.B",".ABABABFB",".ABABABFBD",".ABABBCDEB",".ABABBCF.B",".ABABBCFB",".ABABBCFBD",".ABABDE.B",".ABABDE.BD",".ABABDEB",".ABABDEBBC",".ABABDEBD",".ABABF.B",".ABABF.BBC",".ABABF.BD",".ABABFB",".ABABFBBC",".ABABFBBCD",".ABABFBD",".ABBCBCDEB",".ABBCBCF.B",".ABBCBCFB",".ABBCBCFBD",".ABBCDE.B",".ABBCDE.BD",".ABBCDEB",".ABBCDEBBC",".ABBCDEBD",".ABBCF.B",".ABBCF.BBC",".ABBCF.BD",".ABBCFB",".ABBCFBBC",".ABBCFBBCD",".ABBCFBD",".ABDE.B",".ABDE.BBC",".ABDE.BBCD",".ABDE.BD",".ABDEB",".ABDEBBC",".ABDEBBCBC",".ABDEBBCD",".ABDEBD",".ABF.B",".ABF.BBC",".ABF.BBCBC",".ABF.BBCD",".ABF.BD",".ABFB",".ABFBBC",".ABFBBCBC",".ABFBBCBCD",".ABFBBCD",".ABFBD",".BCBCBCDEB",".BCBCBCF.B",".BCBCBCFB",".BCBCBCFBD",".BCBCDE.B",".BCBCDE.BD",".BCBCDEB",".BCBCDEBBC",".BCBCDEBD",".BCBCF.B",".BCBCF.BBC",".BCBCF.BD",".BCBCFB",".BCBCFBBC",".BCBCFBBCD",".BCBCFBD",".BCDE.B",".BCDE.BBC",".BCDE.BBCD",".BCDE.BD",".BCDEB",".BCDEBBC",".BCDEBBCBC",".BCDEBBCD",".BCDEBD",".BCF.B",".BCF.BBC",".BCF.BBCBC",".BCF.BBCD",".BCF.BD",".BCFB",".BCFBBC",".BCFBBCBC",".BCFBBCBCD",".BCFBBCD",".BCFBD",".DE.B",".DE.BBC",".DE.BBCBC",".DE.BBCBCD",".DE.BBCD",".DE.BD",".DEB",".DEBBC",".DEBBCBC",".DEBBCBCBC",".DEBBCBCD",".DEBBCD",".DEBD",".F.B",".F.BBC",".F.BBCBC",".F.BBCBCBC",".F.BBCBCD",".F.BBCD",".F.BD",".FB",".FBBC",".FBBCBC",".FBBCBCBC",".FBBCBCBCD",".FBBCBCD",".FBBCD",".FBD","AB......FB","AB.....DEB","AB.....F.B","AB.....FB","AB.....FBD","AB....ABFB","AB....BCFB","AB....DE.B","AB....DEB","AB....DEBD","AB....F.B","AB....F.BD","AB....FB","AB....FBBC","AB....FBD","AB...ABDEB","AB...ABF.B","AB...ABFB","AB...ABFBD","AB...BCDEB","AB...BCF.B","AB...BCFB","AB...BCFBD","AB...DE.B","AB...DE.BD","AB...DEB","AB...DEBBC","AB...DEBD","AB...F.B","AB...F.BBC","AB...F.BD","AB...FB","AB...FBBC","AB...FBBCD","AB...FBD","AB..ABABFB","AB..ABBCFB","AB..ABDE.B","AB..ABDEB","AB..ABDEBD","AB..ABF.B","AB..ABF.BD","AB..ABFB","AB..ABFBBC","AB..ABFBD","AB..BCBCFB","AB..BCDE.B","AB..BCDEB","AB..BCDEBD","AB..BCF.B","AB..BCF.BD","AB..BCFB","AB..BCFBBC","AB..BCFBD","AB..DE.B","AB..DE.BBC","AB..DE.BD","AB..DEB","AB..DEBBC","AB..DEBBCD","AB..DEBD","AB..F.B","AB..F.BBC","AB..F.BBCD","AB..F.BD","AB..FB","AB..FBBC","AB..FBBCBC","AB..FBBCD","AB..FBD","AB.ABABDEB","AB.ABABF.B","AB.ABABFB","AB.ABABFBD","AB.ABBCDEB","AB.ABBCF.B","AB.ABBCFB","AB.ABBCFBD","AB.ABDE.B","AB.ABDE.BD","AB.ABDEB","AB.ABDEBBC","AB.ABDEBD","AB.ABF.B","AB.ABF.BBC","AB.ABF.BD","AB.ABFB","AB.ABFBBC","AB.ABFBBCD","AB.ABFBD","AB.BCBCDEB","AB.BCBCF.B","AB.BCBCFB","AB.BCBCFBD","AB.BCDE.B","AB.BCDE.BD","AB.BCDEB","AB.BCDEBBC","AB.BCDEBD","AB.BCF.B","AB.BCF.BBC","AB.BCF.BD","AB.BCFB","AB.BCFBBC","AB.BCFBBCD","AB.BCFBD","AB.DE.B","AB.DE.BBC","AB.DE.BBCD","AB.DE.BD","AB.DEB","AB.DEBBC","AB.DEBBCBC","AB.DEBBCD","AB.DEBD","AB.F.B","AB.F.BBC","AB.F.BBCBC","AB.F.BBCD","AB.F.BD","AB.FB","AB.FBBC","AB.FBBCBC","AB.FBBCBCD","AB.FBBCD","AB.FBD","ABAB....FB","ABAB...DEB","ABAB...F.B","ABAB...FB","ABAB...FBD","ABAB..ABFB","ABAB..BCFB","ABAB..DE.B","ABAB..DEB","ABAB..DEBD","ABAB..F.B","ABAB..F.BD","ABAB..FB","ABAB..FBBC","ABAB..FBD","ABAB.ABDEB","ABAB.ABF.B","ABAB.ABFB","ABAB.ABFBD","ABAB.BCDEB","ABAB.BCF.B","ABAB.BCFB","ABAB.BCFBD","ABAB.DE.B","ABAB.DE.BD","ABAB.DEB","ABAB.DEBBC","ABAB.DEBD","ABAB.F.B","ABAB.F.BBC","ABAB.F.BD","ABAB.FB","ABAB.FBBC","ABAB.FBBCD","ABAB.FBD","ABABAB..FB","ABABAB.DEB","ABABAB.F.B","ABABAB.FB","ABABAB.FBD","ABABABABFB","ABABABBCFB","ABABABDE.B","ABABABDEB","ABABABDEBD","ABABABF.B","ABABABF.BD","ABABABFB","ABABABFBBC","ABABABFBD","ABABBC..FB","ABABBC.DEB","ABABBC.F.B","ABABBC.FB","ABABBC.FBD","ABABBCABFB","ABABBCBCFB","ABABBCDE.B","ABABBCDEB","ABABBCDEBD","ABABBCF.B","ABABBCF.BD","ABABBCFB","ABABBCFBBC","ABABBCFBD","ABABDE.B","ABABDE.BBC","ABABDE.BD","ABABDEB","ABABDEBBC","ABABDEBBCD","ABABDEBD","ABABF.B","ABABF.BBC","ABABF.BBCD","ABABF.BD","ABABFB","ABABFBBC","ABABFBBCBC","ABABFBBCD","ABABFBD","ABBC....FB","ABBC...DEB","ABBC...F.B","ABBC...FB","ABBC...FBD","ABBC..ABFB","ABBC..BCFB","ABBC..DE.B","ABBC..DEB","ABBC..DEBD","ABBC..F.B","ABBC..F.BD","ABBC..FB","ABBC..FBBC","ABBC..FBD","ABBC.ABDEB","ABBC.ABF.B","ABBC.ABFB","ABBC.ABFBD","ABBC.BCDEB","ABBC.BCF.B","ABBC.BCFB","ABBC.BCFBD","ABBC.DE.B","ABBC.DE.BD","ABBC.DEB","ABBC.DEBBC","ABBC.DEBD","ABBC.F.B","ABBC.F.BBC","ABBC.F.BD","ABBC.FB","ABBC.FBBC","ABBC.FBBCD","ABBC.FBD","ABBCABABFB","ABBCABBCFB","ABBCABDE.B","ABBCABDEB","ABBCABDEBD","ABBCABF.B","ABBCABF.BD","ABBCABFB","ABBCABFBBC","ABBCABFBD","ABBCBC..FB","ABBCBC.DEB","ABBCBC.F.B","ABBCBC.FB","ABBCBC.FBD","ABBCBCABFB","ABBCBCBCFB","ABBCBCDE.B","ABBCBCDEB","ABBCBCDEBD","ABBCBCF.B","ABBCBCF.BD","ABBCBCFB","ABBCBCFBBC","ABBCBCFBD","ABBCDE.B","ABBCDE.BBC","ABBCDE.BD","ABBCDEB","ABBCDEBBC","ABBCDEBBCD","ABBCDEBD","ABBCF.B","ABBCF.BBC","ABBCF.BBCD","ABBCF.BD","ABBCFB","ABBCFBBC","ABBCFBBCBC","ABBCFBBCD","ABBCFBD","ABDE.B","ABDE.BBC","ABDE.BBCBC","ABDE.BBCD","ABDE.BD","ABDEB","ABDEBBC","ABDEBBCBC","ABDEBBCBCD","ABDEBBCD","ABDEBD","ABF.B","ABF.BBC","ABF.BBCBC","ABF.BBCBCD","ABF.BBCD","ABF.BD","ABFB","ABFBBC","ABFBBCBC","ABFBBCBCBC","ABFBBCBCD","ABFBBCD","ABFBD","BC......FB","BC.....DEB","BC.....F.B","BC.....FB","BC.....FBD","BC....ABFB","BC....BCFB","BC....DE.B","BC....DEB","BC....DEBD","BC....F.B","BC....F.BD","BC....FB","BC....FBBC","BC....FBD","BC...ABDEB","BC...ABF.B","BC...ABFB","BC...ABFBD","BC...BCDEB","BC...BCF.B","BC...BCFB","BC...BCFBD","BC...DE.B","BC...DE.BD","BC...DEB","BC...DEBBC","BC...DEBD","BC...F.B","BC...F.BBC","BC...F.BD","BC...FB","BC...FBBC","BC...FBBCD","BC...FBD","BC..ABABFB","BC..ABBCFB","BC..ABDE.B","BC..ABDEB","BC..ABDEBD","BC..ABF.B","BC..ABF.BD","BC..ABFB","BC..ABFBBC","BC..ABFBD","BC..BCBCFB","BC..BCDE.B","BC..BCDEB","BC..BCDEBD","BC..BCF.B","BC..BCF.BD","BC..BCFB","BC..BCFBBC","BC..BCFBD","BC..DE.B","BC..DE.BBC","BC..DE.BD","BC..DEB","BC..DEBBC","BC..DEBBCD","BC..DEBD","BC..F.B","BC..F.BBC","BC..F.BBCD","BC..F.BD","BC..FB","BC..FBBC","BC..FBBCBC","BC..FBBCD","BC..FBD","BC.ABABDEB","BC.ABABF.B","BC.ABABFB","BC.ABABFBD","BC.ABBCDEB","BC.ABBCF.B","BC.ABBCFB","BC.ABBCFBD","BC.ABDE.B","BC.ABDE.BD","BC.ABDEB","BC.ABDEBBC","BC.ABDEBD","BC.ABF.B","BC.ABF.BBC","BC.ABF.BD","BC.ABFB","BC.ABFBBC","BC.ABFBBCD","BC.ABFBD","BC.BCBCDEB","BC.BCBCF.B","BC.BCBCFB","BC.BCBCFBD","BC.BCDE.B","BC.BCDE.BD","BC.BCDEB","BC.BCDEBBC","BC.BCDEBD","BC.BCF.B","BC.BCF.BBC","BC.BCF.BD","BC.BCFB","BC.BCFBBC","BC.BCFBBCD","BC.BCFBD","BC.DE.B","BC.DE.BBC","BC.DE.BBCD","BC.DE.BD","BC.DEB","BC.DEBBC","BC.DEBBCBC","BC.DEBBCD","BC.DEBD","BC.F.B","BC.F.BBC","BC.F.BBCBC","BC.F.BBCD","BC.F.BD","BC.FB","BC.FBBC","BC.FBBCBC","BC.FBBCBCD","BC.FBBCD","BC.FBD","BCABABABFB","BCABABBCFB","BCABABDE.B","BCABABDEB","BCABABDEBD","BCABABF.B","BCABABF.BD","BCABABFB","BCABABFBBC","BCABABFBD","BCABBCBCFB","BCABBCDE.B","BCABBCDEB","BCABBCDEBD","BCABBCF.B","BCABBCF.BD","BCABBCFB","BCABBCFBBC","BCABBCFBD","BCABDE.B","BCABDE.BBC","BCABDE.BD","BCABDEB","BCABDEBBC","BCABDEBBCD","BCABDEBD","BCABF.B","BCABF.BBC","BCABF.BBCD","BCABF.BD","BCABFB","BCABFBBC","BCABFBBCBC","BCABFBBCD","BCABFBD","BCBC....FB","BCBC...DEB","BCBC...F.B","BCBC...FB","BCBC...FBD","BCBC..ABFB","BCBC..BCFB","BCBC..DE.B","BCBC..DEB","BCBC..DEBD","BCBC..F.B","BCBC..F.BD","BCBC..FB","BCBC..FBBC","BCBC..FBD","BCBC.ABDEB","BCBC.ABF.B","BCBC.ABFB","BCBC.ABFBD","BCBC.BCDEB","BCBC.BCF.B","BCBC.BCFB","BCBC.BCFBD","BCBC.DE.B","BCBC.DE.BD","BCBC.DEB","BCBC.DEBBC","BCBC.DEBD","BCBC.F.B","BCBC.F.BBC","BCBC.F.BD","BCBC.FB","BCBC.FBBC","BCBC.FBBCD","BCBC.FBD","BCBCABABFB","BCBCABBCFB","BCBCABDE.B","BCBCABDEB","BCBCABDEBD","BCBCABF.B","BCBCABF.BD","BCBCABFB","BCBCABFBBC","BCBCABFBD","BCBCBC..FB","BCBCBC.DEB","BCBCBC.F.B","BCBCBC.FB","BCBCBC.FBD","BCBCBCABFB","BCBCBCBCFB","BCBCBCDE.B","BCBCBCDEB","BCBCBCDEBD","BCBCBCF.B","BCBCBCF.BD","BCBCBCFB","BCBCBCFBBC","BCBCBCFBD","BCBCDE.B","BCBCDE.BBC","BCBCDE.BD","BCBCDEB","BCBCDEBBC","BCBCDEBBCD","BCBCDEBD","BCBCF.B","BCBCF.BBC","BCBCF.BBCD","BCBCF.BD","BCBCFB","BCBCFBBC","BCBCFBBCBC","BCBCFBBCD","BCBCFBD","BCDE.B","BCDE.BBC","BCDE.BBCBC","BCDE.BBCD","BCDE.BD","BCDEB","BCDEBBC","BCDEBBCBC","BCDEBBCBCD","BCDEBBCD","BCDEBD","BCF.B","BCF.BBC","BCF.BBCBC","BCF.BBCBCD","BCF.BBCD","BCF.BD","BCFB","BCFBBC","BCFBBCBC","BCFBBCBCBC","BCFBBCBCD","BCFBBCD","BCFBD","DE.B","DE.BBC","DE.BBCBC","DE.BBCBCBC","DE.BBCBCD","DE.BBCD","DE.BD","DEB","DEBBC","DEBBCBC","DEBBCBCBC","DEBBCBCBCD","DEBBCBCD","DEBBCD","DEBD","F.B","F.BBC","F.BBCBC","F.BBCBCBC","F.BBCBCBCD","F.BBCBCD","F.BBCD","F.BD","FB","FBBC","FBBCBC","FBBCBCBC","FBBCBCBCBC","FBBCBCBCD","FBBCBCD","FBBCD","FBD"]

question1PrecheckTest2 :: Test
question1PrecheckTest2 = "Precheck Case 2" ~: sort q1TestResult2 @=? sort (boundedLang 10 q1TestCase2)

-- let x1 = ABC in let x2 = DeF? in (x1|x2)+   Bounded by 5
q1TestCase3 :: LRegExp
q1TestCase3 = Let [
  (1, S(SSeq [Lit 'A', Lit 'B', Lit 'C'])),
  (2, S(SSeq [Lit 'D', Empty, Opt 'F']))
  ] (Plus (Choice[Var 1, Var 2]))

q1TestResult3 :: [String]
q1TestResult3 = ["ABC","ABCD","ABCDD","ABCDF","D","DABC","DABCD","DD","DDABC","DDD","DDDD","DDDDD","DDDDF","DDDF","DDDFD","DDF","DDFD","DDFDD","DDFDF","DF","DFABC","DFD","DFDD","DFDDD","DFDDF","DFDF","DFDFD"]

question1PrecheckTest3 :: Test
question1PrecheckTest3 = "Precheck Case 3" ~: sort q1TestResult3 @=? sort (boundedLang 5 q1TestCase3)

-- basic choice enumeration:
q1TestCase4 :: LRegExp
q1TestCase4 = Let [] (S (Choice [ SSeq [ (Lit 'A') , (Lit 'B'), (Lit 'C') ] , SSeq [ (Lit 'D') , (Lit 'E'), (Lit 'F') ] , SSeq [ (Lit 'G') , (Lit 'H'), (Lit 'I') ] ]))

q1TestResult4 :: [String]
q1TestResult4 = ["ABC", "DEF", "GHI"]

question1Test1 :: Test
question1Test1 = "Test Case 1, basic choices" ~: sort q1TestResult4 @=? sort (boundedLang 3 q1TestCase4)

-- basic literal
q1TestCase5 :: LRegExp
q1TestCase5 = Let [] (S (Lit 'A'))

q1TestResult5 :: [String]
q1TestResult5 = ["A"]

question1Test2 :: Test
question1Test2 = "Test Case 2, basic literal" ~: sort q1TestResult5 @=? sort (boundedLang 1 q1TestCase5)

-- basic wildcard
q1TestCase6 :: LRegExp
q1TestCase6 = Let [] (S Any)

q1TestResult6 :: [String]
q1TestResult6 = ["."]

question1Test3 :: Test
question1Test3 = "Test Case 3, basic wildcard" ~: sort q1TestResult6 @=? sort (boundedLang 1 q1TestCase6)

-- basic variable lookup
q1TestCase7 :: LRegExp
q1TestCase7 = Let [(1, S (Lit 'A'))] (S (Var 1))

q1TestResult7 :: [String]
q1TestResult7 = ["A"]

question1Test4 :: Test
question1Test4 = "Test Case 4, basic variable lookup" ~: sort q1TestResult7 @=? sort (boundedLang 1 q1TestCase7)

-- Testing empty sequences
q1TestCase8a :: LRegExp
q1TestCase8a = Let [] (Seq [])

q1TestCase8b :: LRegExp
q1TestCase8b = Let [] (S (SSeq []))

q1TestResult8 :: [String]
q1TestResult8 = [""]

-- Test for Seq []
question1Test5a :: Test
question1Test5a = "Test Case 5a, Seq empty == empty string" ~: sort q1TestResult8 @=? sort (boundedLang 1 q1TestCase8a)

-- Test for SSeq []
question1Test5b :: Test
question1Test5b = "Test Case 5b, SSeq empty == empty string" ~: sort q1TestResult8 @=? sort (boundedLang 1 q1TestCase8b)

-- wildcards, but not enough space
q1TestCase9 :: LRegExp
q1TestCase9 = Let [] (S(SSeq[Any, Any, OptAny]))

q1TestResult9 :: [String]
q1TestResult9 = [".."]

question1Test6 :: Test
question1Test6 = "Test Case 6, Wildcard should not be able to boundedLang bounds, and should be displayed correctly"  ~: sort q1TestResult9 @=? sort (boundedLang 2 q1TestCase9)

{-Tests for Question 2-}

-- Precheck Tests
-- Empty
q2TestCase0 :: String
q2TestCase0 = ""

q2TestResult0 :: Maybe LRegExp
q2TestResult0 = Nothing

question2Test0 :: Test
question2Test0 = "Precheck 2 Test Case 1, No input gives no expression"  ~: q2TestResult0 @=? parseRegExp q2TestCase0

-- Complicated expression
q2TestCase1 :: String
q2TestCase1 = "(AB(CD)(EF|(GH|KL|GH)))*"

q2TestResult1 :: Maybe LRegExp
q2TestResult1 = Just (Let [] (Star (SSeq [Lit 'A',Lit 'B',Lit 'C',Lit 'D',Choice [SSeq [Lit 'E',Lit 'F'],SSeq [Lit 'G',Lit 'H'],SSeq [Lit 'K',Lit 'L']]])))

question2Test1 :: Test
question2Test1 = "Precheck 2 Test Case 2, Sequences get collapsed correctly, and choices get collapsed into their simplest form"  ~: q2TestResult1 @=? parseRegExp q2TestCase1

-- Simple Sequence Gets collapsed
q2TestCase2 :: String
q2TestCase2 = "AB"

q2TestResult2 :: Maybe LRegExp
q2TestResult2 = Just (Let [] (S (SSeq [Lit 'A',Lit 'B'])))

question2Test2 :: Test
question2Test2 = "2 Test 1, Simple sequence gets collapsed so there are no nested S" ~: q2TestResult2 @=? parseRegExp q2TestCase2

-- No Singleton sequences
q2TestCase3 :: String
q2TestCase3 = "A"

q2TestResult3 :: Maybe LRegExp
q2TestResult3 = Just (Let [] (S(Lit 'A')))

question2Test3 :: Test
question2Test3 = "2 Test 2, Singleton should be kept a literal, wrapped in choice or sequence" ~: q2TestResult3 @=? parseRegExp q2TestCase3

-- Choice duplicates get removed
q2TestCase4 :: String
q2TestCase4 = "A|A|B"

q2TestResult4 :: Maybe LRegExp
q2TestResult4 = Just (Let [] (S(Choice [Lit 'A', Lit 'B'])))

question2Test4 :: Test
question2Test4 = "2 Test 3, No duplicates in a choice" ~: q2TestResult4 @=? parseRegExp q2TestCase4

-- empty gets removed from sequence
q2TestCase5 :: String
q2TestCase5 = "AeAeAe"

q2TestResult5 :: Maybe LRegExp
q2TestResult5 = Just (Let [] (S (SSeq [Lit 'A', Lit 'A', Lit 'A'])))

question2Test5 :: Test
question2Test5 = "2 Test 4, Empty characters get removed from sequence" ~: q2TestResult5 @=? parseRegExp q2TestCase5

-- empty doesn't get removed from choice
q2TestCase6 :: String
q2TestCase6  = "A|e"

q2TestResult6 :: Maybe LRegExp
q2TestResult6 = Just (Let [] (S (Choice [Lit 'A', Empty])))

question2Test6 :: Test
question2Test6 = "2 Test 5, Empty doesn't get removed from choice" ~: q2TestResult6 @=? parseRegExp q2TestCase6

-- alternating sequence and choice
q2TestCase7 :: String
q2TestCase7 = "AB(A|C)"

q2TestResult7 :: Maybe LRegExp
q2TestResult7 = Just (Let [] (S (SSeq [Lit 'A',Lit 'B',Choice [Lit 'A',Lit 'C']])))

question2Test7 :: Test
question2Test7 = "2 Test 6, Sequence and Choice in a sequence" ~: q2TestResult7 @=? parseRegExp q2TestCase7

-- let sequence (and by extension let) works correctly
q2TestCase8 :: String
q2TestCase8 = "let x1 = (AB)* in let x2 = FGH in x1x2"

q2TestResult8 :: Maybe LRegExp
q2TestResult8 = Just (Let [(1,Star (SSeq [Lit 'A',Lit 'B'])),(2,S (SSeq [Lit 'F',Lit 'G',Lit 'H']))] (S (SSeq [Var 1,Var 2])))

question2Test8 :: Test
question2Test8 = "2 Test 7, Sequenced lets" ~: q2TestResult8 @=? parseRegExp q2TestCase8

-- I did at this point realize that the example from question 1 let x1 = ABC|D.E in let x2 = FG.*H in let x3 = x2.? in x1x2x3x1 does not work, But it seems like the concrete grammar should
-- prevent this so I've decided to leave it be.

{-Tests for Question 3-}
-- Prechecks
-- Empty pretty printing
q3TestCase0 :: LRegExp
q3TestCase0 = Let [] (S Empty)

q3TestResult0 :: String
q3TestResult0 = "e"

question3Test0 :: Test
question3Test0 = "Precheck 3 Test Case 0 empty string"  ~: q3TestResult0 @=? unparseRegExp q3TestCase0

-- Long complex sequecne
q3TestCase1 :: LRegExp
q3TestCase1 = Let [] (Star (SSeq [Lit 'A',Lit 'B',Lit 'C',Lit 'D',Choice [SSeq [Lit 'E',Lit 'F'],SSeq [Lit 'G',Lit 'H'],SSeq [Lit 'K',Lit 'L'],Choice [SSeq [Lit 'A']]]]))

q3TestResult1 :: String
q3TestResult1 = "(ABCD(EF|GH|KL|A))*"

question3Test1 :: Test
question3Test1 = "Precheck 3 Test Case 1 empty string"  ~: q3TestResult1 @=? unparseRegExp q3TestCase1

-- basic literal
q3TestCase2 :: LRegExp
q3TestCase2 = Let [] (S (Lit 'A'))

q3TestResult2 :: String
q3TestResult2 = "A"

question3Test2 :: Test
question3Test2 = "3 Test 2: Basic Literal" ~: q3TestResult1 @=? unparseRegExp q3TestCase1

-- minimal brackets
q3TestCase3 :: LRegExp
q3TestCase3 = Let [] (S (Choice [SSeq [Lit 'A', Lit 'B'], SSeq [Lit 'C', Lit 'D']]))

q3TestResult3 :: String
q3TestResult3 = "AB|CD"

question3Test3 :: Test
question3Test3 = "3 Test 3: Minimal brackets - Choice of Sequences" ~: q3TestResult3 @=? unparseRegExp q3TestCase3

-- necessary brackets, choice inside sequence
q3TestCase4 :: LRegExp
q3TestCase4 = Let [] (S (SSeq [Lit 'A', Choice [Lit 'B', Lit 'C'], Lit 'D']))

q3TestResult4 :: String
q3TestResult4 = "A(B|C)D"

question3Test4 :: Test
question3Test4 = "3 Test 4: Necessary brackets - Choice in Sequence" ~: q3TestResult4 @=? unparseRegExp q3TestCase4

-- Empty removed from sequences
q3TestCase5 :: LRegExp
q3TestCase5 = Let [] (S (SSeq [Lit 'A', SSeq [Empty, Lit 'B', Empty], Lit 'C']))

q3TestResult5 :: String
q3TestResult5 = "ABC"

question3Test5 :: Test
question3Test5 = "3 Test 5: Empty removed from sequences" ~: q3TestResult5 @=? unparseRegExp q3TestCase5

-- Choice with Empty and Duplicates
q3TestCase6 :: LRegExp
q3TestCase6 = Let [] (S (Choice [Lit 'A', Empty, Lit 'A']))

q3TestResult6 :: String
q3TestResult6 = "A|e"

question3Test6 :: Test
question3Test6 = "3 Test 6: Choice removes duplicates but keeps e" ~: q3TestResult6 @=? unparseRegExp q3TestCase6

-- Let sequence
q3TestCase7 :: LRegExp
q3TestCase7 = Let [(1, Star Any), (2, S (Choice [Lit 'A', Lit 'B']))] (S (SSeq [Var 1, Var 2]))

q3TestResult7 :: String
q3TestResult7 = "let x1 = .* in let x2 = A|B in x1x2"

question3Test7 :: Test
question3Test7 = "3 Test 8: Let environment with multiple variables" ~: q3TestResult7 @=? unparseRegExp q3TestCase7

{-Tests for Question 4-}
-- Prechecks
q4TestCase0 :: (String, [LRegExp])
q4TestCase0 = ("POUELUCKWOOLESOEKLY", (map (Let []) [ Star Any, Plus (Choice [SSeq [Lit 'E',Lit 'L'],SSeq [Lit 'G',Lit 'N'],SSeq [Lit 'U',Lit 'C'],Lit 'M']), Plus (Choice [Lit 'K',SSeq [Lit 'Y',Lit 'P',Lit 'T',Lit 'Y'],SSeq [Lit 'W',Lit 'O',Lit 'O'],Lit 'L']), Plus (Choice [SSeq [Opt 'S',Lit 'O'],Lit 'E']), Plus (Choice [SSeq [Opt 'L',Lit 'Y'],Lit 'B',Lit 'K']), Plus (Choice [Lit 'A',SSeq [Lit 'Z',Lit 'D'],SSeq [Opt 'E',Lit 'K']]), Plus (Choice [Lit 'K',SSeq [Lit 'Q',Lit 'X'],SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'S',Lit 'L']]), Seq [Star Any,S (SSeq [Lit 'O',Lit 'O',Lit 'Y']), Star Any], Plus (Choice [Lit 'E',SSeq [Opt 'U',Lit 'O'],SSeq [Lit 'X',Lit 'X']]), Plus (Choice [SSeq [Lit 'Y',Lit 'F'],SSeq [Opt 'C',Lit 'L'],Lit 'U']), Plus (Choice [Lit 'U',Lit 'Y',SSeq [Lit 'E',Opt 'L']]), Plus (Choice [SSeq [Lit 'G',Lit 'C',Lit 'M'],SSeq [Lit 'O',Lit 'C'],Lit 'Q',SSeq [Lit 'L',Lit 'O']]), Plus (Choice [SSeq [Lit 'I',Lit 'T',Lit 'N',Lit 'H'],Lit 'E',SSeq [Lit 'K',Lit 'S'],SSeq [Lit 'O',Lit 'U',Lit 'U']]), Plus (Choice [SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'L',Lit 'O'],Lit 'P',SSeq [Lit 'R',Lit 'R',Lit 'H']]), Plus (Choice [SSeq [Opt 'E',Lit 'P'],Lit 'C',Lit 'K'])]))

q4TestResult0 :: [Bool]
q4TestResult0 = [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]

question4Test0 :: Test
question4Test0 = "Precheck 4 Test Case 1:" ~: q4TestResult0 @=? uncurry checkRegexle q4TestCase0

q4TestCase1 :: (String, [LRegExp])
q4TestCase1 = ("POUEAUCKHOOLEKOEKLY", (map (Let []) [ Star Any, Plus (Choice [SSeq [Lit 'E',Lit 'L'],SSeq [Lit 'G',Lit 'N'],SSeq [Lit 'U',Lit 'C'],Lit 'M']), Plus (Choice [Lit 'K',SSeq [Lit 'Y',Lit 'P',Lit 'T',Lit 'Y'],SSeq [Lit 'W',Lit 'O',Lit 'O'],Lit 'L']), Plus (Choice [SSeq [Opt 'S',Lit 'O'],Lit 'E']), Plus (Choice [SSeq [Opt 'L',Lit 'Y'],Lit 'B',Lit 'K']), Plus (Choice [Lit 'A',SSeq [Lit 'Z',Lit 'D'],SSeq [Opt 'E',Lit 'K']]), Plus (Choice [Lit 'K',SSeq [Lit 'Q',Lit 'X'],SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'S',Lit 'L']]), Seq [Star Any,S (SSeq [Lit 'O',Lit 'O',Lit 'Y']), Star Any], Plus (Choice [Lit 'E',SSeq [Opt 'U',Lit 'O'],SSeq [Lit 'X',Lit 'X']]), Plus (Choice [SSeq [Lit 'Y',Lit 'F'],SSeq [Opt 'C',Lit 'L'],Lit 'U']), Plus (Choice [Lit 'U',Lit 'Y',SSeq [Lit 'E',Opt 'L']]), Plus (Choice [SSeq [Lit 'G',Lit 'C',Lit 'M'],SSeq [Lit 'O',Lit 'C'],Lit 'Q',SSeq [Lit 'L',Lit 'O']]), Plus (Choice [SSeq [Lit 'I',Lit 'T',Lit 'N',Lit 'H'],Lit 'E',SSeq [Lit 'K',Lit 'S'],SSeq [Lit 'O',Lit 'U',Lit 'U']]), Plus (Choice [SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'L',Lit 'O'],Lit 'P',SSeq [Lit 'R',Lit 'R',Lit 'H']]), Plus (Choice [SSeq [Opt 'E',Lit 'P'],Lit 'C',Lit 'K'])]))

q4TestResult1 :: [Bool]
q4TestResult1 = [True,False,False,False,True,True,False,True,True,True,True,True,False,False,True]

question4Test1 :: Test
question4Test1 = "Precheck 4 Test Case 1: " ~: q4TestResult1 @=? uncurry checkRegexle q4TestCase1

-- grid size one match
q4TestCase2 :: (String, [LRegExp])
q4TestCase2 = ("A", map (Let []) [ S (Lit 'A'), S Any, S (Lit 'B')])

q4TestResult2 :: [Bool]
q4TestResult2 = [True, True, False]

question4Test2 :: Test
question4Test2 = "4 Test Case 1: Single cell match" ~: q4TestResult2 @=? uncurry checkRegexle q4TestCase2

{-Tests for Question 5-}
-- Prechecks
q5TestCase0 :: [LRegExp]
q5TestCase0 = []

q5TestResult0 :: String
q5TestResult0 = ""

question5Test0 :: Test
question5Test0 = "Precheck Test Case 0: Empty grid " ~: q5TestResult0 @=? solveRegexle q5TestCase0

q5TestCase1 :: [LRegExp]
q5TestCase1 = map (Let []) [ Plus (Choice [Lit 'H', Lit 'T', Lit 'U', Lit 'V']), Seq [ S (Lit 'D'), S (SSeq [Lit 'A', Lit 'W']), Star Any ], Plus (Choice [Lit 'Z', Lit 'F', Lit 'M', Lit 'S']), Seq [ Star Any, S (Lit 'S'), Star Any ], Seq [ Star Any, S (SSeq [Lit 'A', Lit 'Z']), Star Any ], Plus (Choice [Lit 'T', Lit 'W', Lit 'M', Lit 'L']), Plus (Choice [Lit 'W', Lit 'R', Lit 'Z', Lit 'G']), Plus (Choice [SSeq [Lit 'S', Lit 'A'], Lit 'T', SSeq [Lit 'P', Lit 'N'], Lit 'X']), Plus (Choice [Lit 'O', Lit 'N', Lit 'D', Lit 'H'])]

q5TestResult1 :: String
q5TestResult1 = "HTDAWSZ"

question5Test1 :: Test
question5Test1 = "Precheck Test Case 1: 2x2 grid " ~: q5TestResult1 @=? solveRegexle q5TestCase1

-- Test on larger input known to be correct from q4
q5TestCase2 :: [LRegExp]
q5TestCase2 = (map (Let []) [ Star Any, Plus (Choice [SSeq [Lit 'E',Lit 'L'],SSeq [Lit 'G',Lit 'N'],SSeq [Lit 'U',Lit 'C'],Lit 'M']), Plus (Choice [Lit 'K',SSeq [Lit 'Y',Lit 'P',Lit 'T',Lit 'Y'],SSeq [Lit 'W',Lit 'O',Lit 'O'],Lit 'L']), Plus (Choice [SSeq [Opt 'S',Lit 'O'],Lit 'E']), Plus (Choice [SSeq [Opt 'L',Lit 'Y'],Lit 'B',Lit 'K']), Plus (Choice [Lit 'A',SSeq [Lit 'Z',Lit 'D'],SSeq [Opt 'E',Lit 'K']]), Plus (Choice [Lit 'K',SSeq [Lit 'Q',Lit 'X'],SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'S',Lit 'L']]), Seq [Star Any,S (SSeq [Lit 'O',Lit 'O',Lit 'Y']), Star Any], Plus (Choice [Lit 'E',SSeq [Opt 'U',Lit 'O'],SSeq [Lit 'X',Lit 'X']]), Plus (Choice [SSeq [Lit 'Y',Lit 'F'],SSeq [Opt 'C',Lit 'L'],Lit 'U']), Plus (Choice [Lit 'U',Lit 'Y',SSeq [Lit 'E',Opt 'L']]), Plus (Choice [SSeq [Lit 'G',Lit 'C',Lit 'M'],SSeq [Lit 'O',Lit 'C'],Lit 'Q',SSeq [Lit 'L',Lit 'O']]), Plus (Choice [SSeq [Lit 'I',Lit 'T',Lit 'N',Lit 'H'],Lit 'E',SSeq [Lit 'K',Lit 'S'],SSeq [Lit 'O',Lit 'U',Lit 'U']]), Plus (Choice [SSeq [Lit 'E',Lit 'W'],SSeq [Lit 'L',Lit 'O'],Lit 'P',SSeq [Lit 'R',Lit 'R',Lit 'H']]), Plus (Choice [SSeq [Opt 'E',Lit 'P'],Lit 'C',Lit 'K'])])

q5TestResult2 :: String
q5TestResult2 = "POUELUCKWOOLESOEKLY"

question5Test2 :: Test
question5Test2 = "5 Test Case 1: 3x3 grid "  ~: q5TestResult2 @=? solveRegexle q5TestCase2

-- single match 1x1 grid
q5TestCase3 :: [LRegExp]
q5TestCase3 =  [Let [] (S (Lit 'A')), Let [] (S Any), Let [] (S Any)]

q5TestResult3 :: String
q5TestResult3 = "A"

question5Test3 :: Test
question5Test3 = "5 Test Case 2: 1x1 with single match "  ~: q5TestResult3 @=? solveRegexle q5TestCase3

{-Tests for Question 6-}
-- These were originally in a different file due to Var being conflicted in types.

-- -- Precheck Tests
-- q6TestCase0 :: Exp
-- q6TestCase0 = Abs 0 (Var 0)

-- q6TestResult0 :: (Int, Int)
-- q6TestResult0 = (0, 0)

-- question6Test0 :: Test
-- question6Test0 = "Precheck Test Case 0:  Identity lambda (already normal form)" ~: q6TestResult0 @=? countReductions 10 q6TestCase0

-- -- complicated expression
-- q6TestCase1 :: Exp
-- q6TestCase1 = (Catch (App (Abs 1 (Var 1)) (Abs 1 (Var 1))) (App (Abs 3 (Abs 2 (Var 2))) (App (Abs 4 (App Exn (Abs 2 (Var 2)))) (Abs 1 (Var 1)))))

-- q6TestResult1 :: (Int, Int)
-- q6TestResult1 = (4, 3)

-- question6Test1 :: Test
-- question6Test1 = "Precheck Test Case 1" ~: q6TestResult1 @=? countReductions 4 q6TestCase1

-- -- beta reduction works for app
-- q6TestCase2 :: Exp
-- q6TestCase2 = App (Abs 0 (Var 0)) (Abs 1 (Var 1))

-- q6TestResult2 :: (Int, Int)
-- q6TestResult2 = (1, 1)

-- question6Test2 :: Test
-- question6Test2 = "6 Test Case 2: Simple Beta Reduction" ~: q6TestResult2 @=? countReductions 10 q6TestCase2

-- -- Exceptions get propogated
-- q6TestCase3 :: Exp
-- q6TestCase3 = App Exn (Abs 0 (Var 0))

-- q6TestResult3 :: (Int, Int)
-- q6TestResult3 = (1, 1)

-- question6Test3 :: Test
-- question6Test3 = "6 Test Case 3: Exception Propagation " ~: q6TestResult3 @=? countReductions 10 q6TestCase3


-- -- catch handles exceptions
-- q6TestCase4 :: Exp
-- q6TestCase4 = Catch (Abs 0 (Var 0)) Exn

-- q6TestResult4 :: (Int, Int)
-- q6TestResult4 = (1, 1)

-- question6Test4 :: Test
-- question6Test4 = "6 Test Case 4: Catch handling an exception" ~: q6TestResult4 @=? countReductions 10 q6TestCase4

-- -- bound actually stops expansion
-- q6TestCase5 :: Exp
-- q6TestCase5 = App (Abs 0 (Var 0)) (App (Abs 1 (Var 1)) (Abs 2 (Var 2)))

-- q6TestResult5 :: (Int, Int)
-- q6TestResult5 = (1, 1)

-- question6Test5 :: Test
-- question6Test5 = "6 Test Case 5: Integer bound constraint" ~: q6TestResult5 @=? countReductions 1 q6TestCase5

{-HUnit Test List -}
q1Tests :: Test
q1Tests = TestList [question1PrecheckTest0, question1PrecheckTest1, question1PrecheckTest2, question1PrecheckTest3, question1Test1, question1Test2, question1Test3, question1Test4,
                    question1Test5a, question1Test5b, question1Test6]

q2Tests :: Test
q2Tests = TestList [question2Test0, question2Test1, question2Test2, question2Test3,  question2Test4, question2Test5, question2Test6, question2Test7, question2Test8]

q3Tests :: Test
q3Tests = TestList [question3Test0, question3Test1, question3Test2, question3Test3, question3Test4, question3Test5, question3Test6, question3Test7]

q4Tests :: Test
q4Tests = TestList [question4Test0, question4Test1, question4Test2]

q5Tests :: Test
q5Tests = TestList [question5Test0, question5Test1, question5Test2, question5Test3]

-- q6Tests :: Test
-- q6Tests = TestList [question6Test0, question6Test1, question6Test2, question6Test3, question6Test4, question6Test5]

main :: IO Counts
main = runTestTT q4Tests