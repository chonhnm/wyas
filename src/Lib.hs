{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Control.Arrow (ArrowChoice (right))
import Control.Exception (throw)
import Control.Monad.Except (catchError, throwError)
import Data.Char (digitToInt, isAlpha, isHexDigit, toLower, toUpper)
import Data.Complex (Complex ((:+)))
import Data.Foldable (Foldable (foldl'))
import qualified Data.Functor
import Data.Ratio (Ratio, denominator, numerator)
import Foreign.C (isValidErrno)
import GHC.Arr (Array, elems, listArray, numElements)
import GHC.Float (rationalToDouble)
import GHC.Real ((%))
import GHC.Unicode (isHexDigit)
import Numeric (readFloat, readHex, readOct)
import System.Environment (getArgs)
import Text.Parsec (ParseError, alphaNum, anyChar, digit, optional, sepBy, sepEndBy, skipMany)
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
import Text.Show (Show)

someFunc :: IO ()
someFunc = do
  (expr : _) <- getArgs
  let evaled = fmap show $ readExpr expr >>= eval
  putStrLn $ extractValue $ trapError evaled

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

symbol :: Parser Char
symbol = oneOf "!$%|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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
  deriving Eq

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
-- parseDecimal1 = Number . read <$> many1 digit
parseDecimal1 = fmap (Number . read) (many1 digit)

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
toDouble (Ratio r) = rationalToDouble (numerator r) (denominator r)
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
  arrayVal <- sepEndBy parseExpr spaces
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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Vector _) = return val
eval val@(DottedList _ _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval pred
    case result of
      Bool True -> eval conseq
      Bool False -> eval alt
      _ -> throwError $ TypeMismatch "boolean" pred
eval (List (Atom "cond" : alts)) = cond alts
eval form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
    List (List datums : exprs) -> do
      result <- eval key
      equality <- mapM (\x -> eqv [result, x]) datums
      if Bool True `elem` equality
        then mapM eval exprs >>= return . last
        else eval $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(Atom _) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

cond :: [LispVal] -> ThrowsError LispVal
cond [List [Atom "else", value]] = eval value
cond (List [condition, value] : alts) =
  do
    result <- eval condition
    boolResult :: Bool <- unpackBool result
    if boolResult
      then eval value
      else cond alts
cond (List a : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

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
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    ("string->symbol", unaryOp string2symbolOp),
    -- boolean operators
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    -- car
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

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
realOp (Float _) = Bool True
realOp _ = Bool False

complexOp :: LispVal -> LispVal
complexOp (Number _) = Bool True
complexOp (Ratio _) = Bool True
complexOp (Float _) = Bool True
complexOp (Complex _) = Bool True
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
pairOp (List (a : _)) = Bool True
pairOp (DottedList _ _) = Bool True
pairOp _ = Bool False

booleanOp :: LispVal -> LispVal
booleanOp (Bool _) = Bool True
booleanOp _ = Bool False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp _ _ = error "only support one argument."

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- error checking and exceptions

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default str) = show str

instance Show LispError where show = showError

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "error extract value"

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pari" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2] = return $ Bool $ arg1 == arg2
eqv [Ratio arg1, Ratio arg2] = return $ Bool $ arg1 == arg2
eqv [Complex arg1, Complex arg2] = return $ Bool $ arg1 == arg2
eqv [Character arg1, Character arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv listPair@[List _, List _] = eqvList eqv listPair
eqv [Vector arg1, Vector arg2] = eqv [List $ elems arg1, List $ elems arg2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnPacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnPacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal listPair@[List _, List _] = eqvList equal listPair
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [Vector arg1, Vector arg2] = equal [List $ elems arg1, List $ elems arg2]
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnPacker unpackNum, AnyUnPacker unpackStr, AnyUnPacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFun [List arg1, List arg2] =
  return $
    Bool $
      (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqvFun [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
      Right _ -> False
eqvList _ _ = throwError $ Default "not supported"
