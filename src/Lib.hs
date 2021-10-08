{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc,
  )
where

import Control.Exception (throw)
import Data.Char (digitToInt, isHexDigit, toLower, toUpper)
import Data.Complex (Complex ((:+)))
import Data.Foldable (Foldable (foldl'))
import Data.Ratio (Ratio, numerator, denominator)
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
import GHC.Float (rationalToDouble)
import qualified Data.Functor

someFunc :: IO ()
someFunc = do
  (expr : _) <- getArgs
  print $ eval $ readExpr expr

symbol :: Parser Char
symbol = oneOf "!$%|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

spaces1 :: Parser ()
spaces1 = skipMany space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | String String
  | Bool Bool
  | Character Char
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | Vector (Array Int LispVal)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "\'" ++ show c ++ "\'"
showVal (Float f) = show f
showVal (Ratio r) = show r
showVal (Complex c) = show c
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . "
    ++ showVal tail
    ++ ")"
showVal (Vector array) = show array

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
toDouble (Ratio r) = rationalToDouble  (numerator r) (denominator r)
toDouble _ = error "not implement"

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
parseNumber2 = many1 digit Data.Functor.<&> (Number . read)

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

{-
boolean? --Boolean? returns #t if obj is either #t or #f and returns #f otherwise.
pair? --Pair? returns #t if obj is a pair, and otherwise returns #f.
null? --Returns #t if obj is the empty list, otherwise returns #f.
list? --Returns #t if obj is a list, otherwise returns #f.
symbol? --Returns #t if obj is a symbol, otherwise returns #f.
char? --Returns #t if obj is a character, otherwise returns #f.
string? --Returns #t if obj is a string, otherwise returns #f.
vector? --Returns #t if obj is a vector, otherwise returns #f.

number?
complex?
real?
rational?
integer?
-}
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("boolean?", unaryOp booleanOp),
    ("pair?", unaryOp pairOp),
    ("null?", unaryOp nullOp),
    ("list?", unaryOp listOp),
    ("symbol?", unaryOp symbolOp),
    ("char?", unaryOp charOp),
    ("string?", unaryOp stringOp),
    ("vector?", unaryOp vectorOp),
    ("integer?", unaryOp integerOp),
    ("rational?", unaryOp rationalOp),
    ("real?", unaryOp realOp),
    ("complex?", unaryOp complexOp),
    ("number?", unaryOp numberOp),
    -- symbol handler
    ("symbol->string", unaryOp symbol2stringOp),
    ("string->symbol", unaryOp string2symbolOp)
  ]

string2symbolOp :: LispVal -> LispVal
string2symbolOp (String s) = Atom s
string2symbolOp n = error $ show n ++ " is not string."

symbol2stringOp :: LispVal -> LispVal
symbol2stringOp (Atom a) = String a
symbol2stringOp n = error $ show n ++ " is not atom."

integerOp :: LispVal -> LispVal
integerOp (Number _) = Bool True
integerOp _ = Bool False


rationalOp :: LispVal -> LispVal
rationalOp (Number _) = Bool True
rationalOp (Ratio _) = Bool True
rationalOp _ = Bool False

realOp :: LispVal -> LispVal
realOp (Number _) = Bool True
realOp (Ratio _) = Bool True
realOp (Float _ ) = Bool True
realOp _ = Bool False

complexOp :: LispVal -> LispVal
complexOp (Number _) = Bool True
complexOp (Ratio _) = Bool True
complexOp (Float _ ) = Bool True
complexOp (Complex _ ) = Bool True
complexOp _ = Bool False

numberOp :: LispVal -> LispVal
numberOp = complexOp

vectorOp :: LispVal -> LispVal
vectorOp (Vector _) = Bool True
vectorOp _ = Bool False

stringOp :: LispVal -> LispVal
stringOp (String _) = Bool True
stringOp _ = Bool False

charOp :: LispVal -> LispVal
charOp (Character _) = Bool True
charOp _ = Bool False

symbolOp :: LispVal -> LispVal
symbolOp (Atom _) = Bool True
symbolOp _ = Bool False

listOp :: LispVal -> LispVal
listOp (List _) = Bool True
listOp _ = Bool False

nullOp :: LispVal -> LispVal
nullOp (List []) = Bool True
nullOp _ = Bool False

pairOp :: LispVal -> LispVal
pairOp (List (a:_)) = Bool True
pairOp (DottedList _ _) = Bool True
pairOp _ = Bool False

booleanOp :: LispVal -> LispVal
booleanOp (Bool _) = Bool True
booleanOp _ = Bool False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v
unaryOp _ _   = error "only support one argument."

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

{-
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
-}