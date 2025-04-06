{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task2 where

import Task1 (Parse, Parse(..), Stack(..), pop, push, len, isNumeric)
-- import Task3 (BoolOp)

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show


-- * Parsing

polka2 :: (Parse a, Parse op) => Maybe (Stack (Expr a op)) -> String -> Maybe(Stack (Expr a op))
polka2 Nothing _ = Nothing
polka2 (Just stack) e
    | isNumeric e = case parse e  of -- it is not general !!!
      Just num -> Just(push (Lit num) stack)
      _        -> Nothing
    | otherwise = case parse e of
      Just res -> case (s1, s2) of
                    (Just x1, Just x2) -> Just(push (BinOp res x2 x1) stack2)
                    _        -> Nothing
      _ -> Just (push (Var e) stack)
      where (s1, stack1) = pop stack
            (s2, stack2) = pop stack1

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
-- 
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse s = case foldl polka2 (Just(Stack [])) (words s) of
    Just smth -> if len smth == 1 then fst (pop smth) else Nothing
    _ -> Nothing


instance Parse IntOp where
  parse s = case s of
    "+" -> Just Add
    "*" -> Just Mul
    "-" -> Just Sub
    _   -> Nothing 

instance Parse Integer where
  parse s = case reads s of
    [(num, "")] -> Just num
    _           -> Nothing
    
-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--

instance Eval Integer IntOp where
  evalBinOp op l r = case op of
    Add -> l + r
    Mul -> l * r
    Sub -> l - r
     

evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr vars expr = case expr of
  Lit n -> Just n
  Var x -> lookup x vars
  BinOp op l r -> 
    let lRes = evalExpr vars l
        rRes = evalExpr vars r
    in case (lRes, rRes) of
      (Just x1, Just x2) -> Just (evalBinOp op x1 x2)
      _ -> Nothing


-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate reifyInteger 

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'Reify' function is required to reconcile generic type
-- of intermediate 'Expr' expression with concrete type using 'a' and 'op'.
--
evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e -> evalExpr m (reify e)
  Nothing -> Nothing

-- * Helpers

-- | Helper type for specifying 'Expr' with
-- concrete 'a' and 'op' in generic context
type Reify a op = Expr a op -> Expr a op

-- | Helper for specifying 'Expr' with 'Integer' and 'IntOp' in generic context
reifyInteger :: Reify Integer IntOp
reifyInteger = id
