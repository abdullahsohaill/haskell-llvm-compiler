module Parser where

import AST (Expr) 

import Data.Char (isDigit, digitToInt, isSpace)

import Control.Applicative (Alternative(empty, (<|>)), Applicative(..))

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

pChar :: Parser Char
pChar = Parser $ \input ->
  case input of
    []     -> Nothing 
    (x:xs) -> Just (x, xs)

-- functor instance for Parser
instance Functor Parser where
    fmap f p = Parser $ \input ->
        case runParser p input of
            Nothing         -> Nothing
            Just (result, rest) -> Just (f result, rest)

-- parses a character that satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
    case input of 
        []     -> Nothing
        (x:xs) -> if predicate x
                    then Just (x, xs)
                    else Nothing

-- parses a single digit character
pDigit :: Parser Char
pDigit = satisfy isDigit

-- parses single digit and returns it as an Integer
pDigitInt :: Parser Integer
pDigitInt = fmap (toInteger . digitToInt) pDigit

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)

    -- sequencing two parsers
    pFun <*> pValue = Parser $ \input ->
        case runParser pFun input of
            Nothing -> Nothing

            -- gives a function and the remaining input
            Just (f, rest1) ->
                case runParser pValue rest1 of
                    Nothing -> Nothing
                    Just (val, rest2) -> Just (f val, rest2)

instance Alternative Parser where
    -- parser that always fails
    empty = Parser $ \_ -> Nothing

    -- run the first parser on input, if it fails, run the second parser
    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Nothing -> runParser p2 input
            Just res -> Just res

-- parses one or more occurrences of p
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

-- parses zero or more occurrences of p
many :: Parser a -> Parser [a]
many p = some p <|> pure []

-- parses multi-digit integer
pNumber :: Parser Integer
pNumber = read <$> some pDigit

-- issue is that the string could contain white spaces
pWhitespace :: Parser String
pWhitespace =  many (satisfy isSpace)

lexeme :: Parser a -> Parser a
lexeme p = (pWhitespace *> p) <* pWhitespace 

pNumberLexeme = lexeme pNumber

pString :: String -> Parser String
pString [] = pure []
pString (x:xs) = Parser $ \input ->
    case runParser pChar input of
        Nothing -> Nothing

        Just (f, rest1) ->
            case f==x of
                Nothing -> Nothing
                Just (val, rest2) -> runParser xs rest2