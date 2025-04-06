{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Data.Char (isDigit)
-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

newtype Stack a = Stack [a]

push :: a -> Stack a -> Stack a
push e (Stack s) = Stack (e : s)

pop :: Stack a -> (Maybe a, Stack a)
pop s@(Stack []) = (Nothing, s)
pop (Stack (x:xs)) = (Just x, Stack xs)

len :: Stack a -> Int
len (Stack s) = length s

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr expr = case expr of
   Lit n   -> n
   Add l r -> evalIExpr l + evalIExpr r
   Mul l r -> evalIExpr l * evalIExpr r 

isNumeric::String -> Bool
isNumeric = all isDigit -- + check negative numbers!!!

polka :: Maybe (Stack IExpr) -> String -> Maybe(Stack IExpr)
polka Nothing _ = Nothing
polka (Just stack) e
    | isNumeric e = Just(push (Lit (read e::Integer)) stack)
    | otherwise = 
      let (s1, stack1) = pop stack
          (s2, stack2) = pop stack1
      in case (s1, s2) of
        (Just x1, Just x2) -> case e of
                                "+" -> Just(push (Add x1 x2) stack2)
                                "*" -> Just(push (Mul x1 x2) stack2)
                                _   -> Nothing
        _ -> Nothing

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a
  
-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
---- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--

instance Parse IExpr where
  parse s = case foldl polka (Just(Stack [])) (words s) of
    Just smth -> if len smth == 1 then fst (pop smth) else Nothing
    _ -> Nothing     

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = case parse s of
  Just expr -> Just(evalIExpr expr)
  Nothing -> Nothing
