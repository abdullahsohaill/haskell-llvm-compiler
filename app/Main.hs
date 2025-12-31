module Main (main) where

import Parser 
-- (Parser, runParser, pDigitInt, pChar, pNumber, pWhitespace, many, some)
import Data.Char 


testParser :: Show a => Parser a -> String -> IO ()
testParser parser input =
  case runParser parser input of
    Just (result, rest) -> do
      putStrLn $ "Success! Parsed: " ++ show result
      putStrLn $ "  Remaining input: \"" ++ rest ++ "\""
    Nothing ->
      putStrLn "Parser failed."

main :: IO ()
main = do
    putStrLn "running parser"

    -- putStrLn "--- Testing Applicative Number Parser ---"

    -- putStrLn "\n> Parsing a valid multi-digit number:"
    -- testParser pNumber "12345hello"

    -- putStrLn "\n> Parsing a single-digit number:"
    -- testParser pNumber "7"

    -- putStrLn "\n> Attempting to parse non-numeric input:"
    -- testParser pNumber "world"

    -- putStrLn "\n> Attempting to parse an empty string:"
    -- testParser pNumber ""

    -- putStrLn "\n> Attempting to parse WHITE SPACE:"
    -- testParser pWhitespace " 1 2 3 "

    putStrLn "\n> Attempting to parse using lexeme on:"
    testParser pNumberLexeme " 123 "
