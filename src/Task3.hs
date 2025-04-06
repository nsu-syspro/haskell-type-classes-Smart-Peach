{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- The above pragma enables all warnings

module Task3 where

import Task1 (Parse, Parse(..))
import Task2 (Eval, Eval(..), evaluate, Reify)
import Prelude hiding ((^^))

(^^) :: Bool -> Bool -> Bool
False ^^ True = True
True ^^ False = True
_ ^^ _ = False


data BoolOp = And | Or | Xor
  deriving Show


instance Parse BoolOp where
    parse s = case s of
        "and" -> Just And
        "or"  -> Just Or
        "xor" -> Just Xor
        _     -> Nothing


instance Eval Bool BoolOp where
    evalBinOp op l r = case op of
        And -> l && r
        Or -> l || r
        Xor -> l ^^ r


instance Parse Bool where
  parse s = case s of
    "True" -> Just True
    "False" -> Just False
    _ -> Nothing

isVar:: String -> Bool
isVar s = case s of
    "and" -> False
    "or" -> False
    "xor" -> False
    _ -> True

evaluateBool :: [(String, Bool)] -> String -> Maybe Bool
evaluateBool = evaluate reifyBool

getVariables :: [String] -> [String]
getVariables s = [x | x <- s, isVar x]

getAllPossibilities :: [String] -> [[(String, Bool)]]
-- getAllPossibilities vars = [[(var, val)] | var <- vars , val <- [True, False]]
getAllPossibilities vars = sequence [ [(var, True), (var, False)] | var <- vars ]

reifyBool :: Reify Bool BoolOp
reifyBool = id

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
-- solveSAT s =
--     let res = [evaluateBool vars s | vars <- getAllPossibilities (getVariables (words s))]
--     in
--     if Just True `elem` res then Just True
--     else if Nothing `elem` res then Nothing
--     else Just False
solveSAT = error "lalala"
