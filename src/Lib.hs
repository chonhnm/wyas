{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc,
  )
where

import Control.Exception (throw)
import Data.Char (digitToInt, isHexDigit, toLower, toUpper)
import Data.Complex (Complex ((:+)))
import Data.Foldable (Foldable (foldl'))
import Data.Ratio (Ratio)
import GHC.Arr (Array, listArray)
import GHC.Real ((%))
import GHC.Unicode (isHexDigit)
import Numeric (readFloat, readHex, readOct)
import System.Environment (getArgs)
import Text.Parsec (alphaNum, anyChar, digit, optional, sepBy, sepEndBy, skipMany)
import Text.Parsec.Char (digit)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    digit,
    endBy,
    hexDigit,
    letter,
    many,
    many1,
    noneOf,
    notFollowedBy,
    octDigit,
    oneOf,
    parse,
    sepBy,
    skipMany1,
    space,
    string,
    try,
    (<?>),
    (<|>),
  )
import Text.ParserCombinators.Parsec.Char (digit)

someFunc :: IO ()
someFunc = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

spaces1 :: Parser ()
spaces1 = skipMany space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | Vector (Array Int LispVal)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChar <|> noneOf "\\\""
  char '"'
  return $ String x

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  oneOf "\\\"\n\r\t"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber =
  parseDecimal1
    <|> parseDecimal2
    <|> parseHex
    <|> parseOct
    <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <- many1 digit
  return $ (Number . read) x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number $ hex2dig x

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number $ oct2dig x

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number $ bin2dig x

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ head $ readOct x

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head $ readHex x

bin2dig :: [Char] -> Integer
bin2dig = foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True))
    <|> (char 'f' >> return (Bool False))

parseChar :: Parser LispVal
parseChar = try parseCharacter <|> parseCharacterName

parseCharacter :: Parser LispVal
parseCharacter =
  do
    string "#\\"
    x <- anyChar
    notFollowedBy alphaNum
    return $ Character x

parseCharacterName :: Parser LispVal
parseCharacterName =
  do
    string "#\\"
    val <- caseInsensitiveString "newline" <|> caseInsensitiveString "space"
    return $
      Character $ case val of
        "space" -> ' '
        "newline" -> '\n'
        _ -> ' '

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = mapM caseInsensitiveChar s <?> "\"" ++ s ++ "\""

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat $ x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseNumber
  char '+'
  y <- try parseFloat <|> parseNumber
  char 'i'
  return $ Complex $ toDouble x :+ toDouble y

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

-- parse list
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseListAll :: Parser LispVal
parseListAll = do
  char '('
  spaces1
  x <- try parseList <|> parseDottedList
  spaces1
  char ')'
  return x

parseListAllWithoutTry :: Parser LispVal
parseListAllWithoutTry = do
  char '('
  spaces1
  head <- sepEndBy parseExpr spaces
  do
    char '.' >> spaces
    tail <- parseExpr
    spaces1 >> char ')'
    return $ DottedList head tail
    <|> (char ')' >> return (List head))

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBackQuoted :: Parser LispVal
parseBackQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  try $ char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  try $ string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  arrayVal <- sepBy parseExpr spaces
  char ')'
  return $ Vector $ listArray (0, length arrayVal - 1) arrayVal

parseNumber1 :: Parser LispVal
parseNumber1 = do
  num <- many1 digit
  return $ (Number . read) num

parseNumber2 :: Parser LispVal
parseNumber2 = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseNumber -- we need the 'try' because these can all
    <|> try parseBool -- start with  the '#' char
    <|> try parseChar
    <|> parseQuoted
    <|> parseBackQuoted
    <|> parseUnquoteSplicing
    <|> parseUnquote
    <|> try parseVector
    <|> parseListAllWithoutTry
