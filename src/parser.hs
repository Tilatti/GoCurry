-- A cool way to build a parser ! Thanks to Lennart Andersson
-- This version is based on Stat Monad
-- Reference :  www.cs.lth.se/EDA120/assignment4/parser.pdf
--
-- TODO : Implement Location

module Parsing where

import Data.Maybe
import Data.Either
import Data.Char (isDigit, digitToInt, isLetter, isSpace, ord)

import Control.Monad.State
import Control.Monad.Error

import Debug.Trace

data Location = Location Int | NoLocation
instance Error Location where
	noMsg = NoLocation
	strMsg s = NoLocation

type Parser a = ErrorT Location (State String) a

--Parsing without any fail
parse_return :: a -> Parser a
parse_return a = lift (State $ \cs -> (a, cs))

sizeStr :: String -> Int
sizeStr (c:cs) = 1 + sizeStr cs
sizeStr [] = 0

infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(?) parser f =
  do
    val <- parser
    if f val
      then return val
      else throwError NoLocation

infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(!) p1 p2 =
  catchError (p1)  -- Then
	(\_ -> p2) -- Else

infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(#) p1 p2 =
  do
    val1 <- p1
    val2 <- p2
    return (val1, val2)

(##) :: Parser a -> Parser a -> Parser [a]
(##) p1 p2 =
  do
    val1 <- p1
    val2 <- p2
    return (val1:val2:[])

infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(>->) p f =
  do
    val <- p
    return (f val)

(-#) :: Parser a -> Parser b -> Parser b
(-#) p1 p2  =
  do
    val1 <- p1
    val2 <- p2
    return (val2)

(#-) :: Parser a -> Parser b -> Parser a
(#-) p1 p2 =
  do
    val1 <- p1
    val2 <- p2
    return (val1)

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

--
parse_iterate :: Parser a -> Int -> Parser [a]
parse_iterate m 0 = parse_return []
parse_iterate m i = m # parse_iterate m (i-1) >-> cons

parse_iter :: Parser a -> Parser [a]
parse_iter m = m # parse_iter m >-> cons ! parse_return []


char' :: String -> (Char, String)
char' (c:cs) = (c, cs)
char' [] = (' ', []) -- Impossible case

-- Parsing a char
-- The most import function
char :: Parser Char
char =
  do
    str <- get
    if (not (str == []))
      then lift (State $ char')
      else throwError NoLocation


--Parsing a char and verifiy the value
lit :: Char -> Parser Char
lit c = char ? (==c)

digit :: Parser Char
digit = (char ? isDigit)

digitVal :: Parser Int
digitVal = digit >-> digitToInt

letter :: Parser Char
letter = char ? isLetter

space :: Parser Char
space = char ? isSpace

--We do not accept empty string
letters :: Parser String
letters = parse_iter letter --letter # parse_iter >-> cons

--Return a new parser accepting space after the token
token :: Parser a -> Parser a
token m = m #- parse_iter space

token_sep :: Parser a -> Parser a -> Parser a
token_sep m sep = m #- parse_iter sep

word :: Parser String
word = token letters

-- Test if parsed string is equal to w String
accept :: String -> Parser String
accept w = (parse_iterate char (sizeStr w) ? (==w))

infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(#>) p f = p >>= f

--Exemple use of #> : detect two identicals characteres
double :: Parser Char
double = char #> lit

bldNumber :: Int -> Int -> Int
bldNumber n d = 10 * n + d

number' :: Int -> Parser Int
number' n =
  digitVal >-> bldNumber n #> number'
  ! parse_return n

number :: Parser Int
number = token (digitVal #> number')

isTab :: Char -> Bool
isTab c = (ord c) == 9

isNotTab :: Char -> Bool
isNotTab c = (ord c) /= 9

tab :: Parser Char
tab = char ? isTab

parseFilePath :: Parser String
--parseFilePath = parse_iter (char ? isLetter)
parseFilePath = parse_iter char

-- The UnWrapper function, apply a parser to string, and return result
applyParser :: Parser a -> String -> Maybe a
applyParser parser str =
  let
    result = runState (runErrorT parser) str
  in
    case result of
			(Right val, "") -> Just val
			(Right val, str) -> Just val
			(Left loc, str)  -> Nothing

-- ugly gluing functions

glue3Parser :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
glue3Parser p1 p2 p3 =
  do
    val1 <- p1
    val2 <- p2
    val3 <- p3
    return (val1, val2, val3)

glue4Parser :: Parser a -> Parser b -> Parser c -> Parser d ->
		Parser (a, b, c, d)
glue4Parser p1 p2 p3 p4 =
  do
    val1 <- p1
    val2 <- p2
    val3 <- p3
    val4 <- p4
    return (val1, val2, val3, val4)

glue5Parser :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e ->
		Parser (a, b, c, d, e)
glue5Parser p1 p2 p3 p4 p5 =
  do
    val1 <- p1
    val2 <- p2
    val3 <- p3
    val4 <- p4
    val5 <- p5
    return (val1, val2, val3, val4, val5)

glue6Parser :: Parser a -> Parser b -> Parser c ->
		Parser d -> Parser e -> Parser f ->
		Parser (a, b, c, d, e, f)
glue6Parser p1 p2 p3 p4 p5 p6 =
  do
    val1 <- p1
    val2 <- p2
    val3 <- p3
    val4 <- p4
    val5 <- p5
    val6 <- p6
    return (val1, val2, val3, val4, val5, val6)
