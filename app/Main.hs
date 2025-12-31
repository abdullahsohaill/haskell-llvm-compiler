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

    let input1 = "abc"
    putStrLn $ "Parsing input: " ++ input1
    case runParser pChar input1 of
        Just (result, rest) -> putStrLn $ "parsed: " ++ [result] ++ ", remaining: " ++ rest
        Nothing             -> putStrLn "parsing failed."

    putStrLn ""

    let input2 = ""
    putStrLn $ "Parsing input: " ++ input2
    case runParser pChar input2 of
        Just (result, rest) -> putStrLn $ "parsed: " ++ [result] ++ ", remaining: " ++ rest
        Nothing             -> putStrLn "parsing failed"

    let input3 = "543"
    putStrLn $ "Running on input: \"" ++ input3 ++ "\""
    case runParser pDigitInt input3 of
        Just (result, rest) -> do
            putStrLn $ "  Success! Parsed: " ++ show result
            putStrLn $ "  The result's type is Integer: " ++ show (result + 1)
            putStrLn $ "  Remaining input: \"" ++ rest ++ "\""
        Nothing -> putStrLn "  Parser failed."

    putStrLn "" 

    let input4 = "hello"
    putStrLn $ "Running on input: \"" ++ input4 ++ "\""
    case runParser pDigitInt input4 of
        Just (result, rest) -> do
            putStrLn $ "  Success! Parsed: " ++ show result
            putStrLn $ "  Remaining input: \"" ++ rest ++ "\""
        Nothing -> putStrLn "  Parser failed."

    putStrLn "--- Testing the Applicative Number Parser ---"

    putStrLn "\n> Parsing a valid multi-digit number:"
    testParser pNumber "12345hello"

    putStrLn "\n> Parsing a single-digit number:"
    testParser pNumber "7"

    putStrLn "\n> Attempting to parse non-numeric input:"
    testParser pNumber "world"

    putStrLn "\n> Attempting to parse an empty string:"
    testParser pNumber ""

    putStrLn "\n> Attempting to parse WHITE SPACE:"
    testParser pWhitespace " 1 2 3 "
