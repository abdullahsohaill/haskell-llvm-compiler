module Parser where

import AST (Expr) 

import Data.Char (isDigit, digitToInt)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

pChar :: Parser Char
pChar = Parser $ \input ->
  case input of
    []     -> Nothing 
    (x:xs) -> Just (x, xs)

instance Functor Parser where
    fmap f p = Parser $ \input ->
        case runParser p input of
            Nothing         -> Nothing
            Just (result, rest) -> Just (f result, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
    case input of 
        []     -> Nothing
        (x:xs) -> if predicate x
                    then Just (x, xs)
                    else Nothing

pDigit :: Parser Char
pDigit = satisfy isDigit

-- parses single digit and returns it as an Integer
pDigitInt :: Parser Integer
pDigitInt = fmap (toInteger . digitToInt) pDigit