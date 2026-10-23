module Lambda where

data Exp = Var Int | Abs Int Exp | App Exp Exp | Exn | Catch Exp Exp
  deriving (Eq,Show,Read)

countReductions :: Int -> Exp -> (Int,Int)
countReductions bound expression = (countByValue bound expression, countByName bound expression)

countByValue :: Int -> Exp -> Int
countByValue bound expression = driver expression 0
  where
    driver exp step
      | step >= bound = bound
      | otherwise    = case stepByValue exp of
        Nothing      -> step
        Just nextExp -> driver nextExp (step+1)

stepByValue :: Exp -> Maybe Exp
stepByValue (Catch e1 e2)
  | not (isValue e1) = fmap (\e1' -> Catch e1' e2) (stepByValue e1)
  | e2 == Exn        = Just e1
  | isLambda e2      = Just e2
  | otherwise        = fmap (\e2' -> Catch e1 e2') (stepByValue e2)

stepByValue (App e1 e2)
  | not (isValue e1) = fmap (\e1' -> App e1' e2) (stepByValue e1)
  | not (isValue e2) = fmap (\e2' -> App e1 e2') (stepByValue e2)
  | otherwise        = case e1 of
    Abs x body -> Just (substitute body e2 x)
    Exn          -> Just Exn
    _            -> if e2 == Exn then Just Exn else Nothing
stepByValue _ = Nothing

countByName :: Int -> Exp -> Int
countByName bound expression = driver expression 0
  where
    driver exp step
      | step >= bound = bound
      | otherwise    = case stepByName exp of
        Nothing      -> step
        Just nextExp -> driver nextExp (step+1)

stepByName :: Exp -> Maybe Exp
stepByName (Catch e1 e2)
  | not (isValue e1) = fmap (\e1' -> Catch e1' e2) (stepByName e1)
  | e2 == Exn        = Just e1
  | isLambda e2      = Just e2
  | otherwise        = fmap (\e2' -> Catch e1 e2') (stepByName e2)
stepByName (App e1 e2) = case e1 of
  (Abs x body) -> Just (substitute body e2 x)
  Exn          -> Just Exn
  _            -> fmap (\e1' -> App e1' e2) (stepByName e1)
stepByName _ = Nothing --Termination condition

-- Utility functions
substitute :: Exp -> Exp -> Int -> Exp
substitute body replacement target = case body of
  Exn -> Exn
  (Abs int exp )-> if int == target then Abs int exp else Abs int (substitute exp replacement target)
  (Var int) -> if int == target then replacement else Var int

  (App e1 e2) -> App (substitute e1 replacement target) (substitute e2 replacement target)
  (Catch e1 e2) -> Catch (substitute e1 replacement target) (substitute e2 replacement target)

isValue :: Exp -> Bool
isValue exp = case exp of
  (Abs _ _) -> True
  Exn     -> True
  _       -> False

isLambda :: Exp -> Bool
isLambda (Abs _ _) = True
isLambda _ = False