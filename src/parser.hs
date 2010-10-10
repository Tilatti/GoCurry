-- A cool way to build a parser ! Thanks to Lennart Andersson
-- Reference :  www.cs.lth.se/EDA120/assignment4/parser.pdf

module Parsing where

import Data.Maybe
import Data.Char (isDigit, digitToInt, isLetter, isSpace, ord)

import Control.Applicative

type Parser a = String -> Maybe (a, String)

--Parsing without any fail
parse_return :: a -> Parser a
parse_return a cs = Just(a,cs)

sizeStr :: String -> Int
sizeStr (c:cs) = 1 + sizeStr cs
sizeStr [] = 0

infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
  case m cs of
    Nothing -> Nothing
    Just(a,cs) -> if p a then Just(a,cs) else Nothing

infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs =
  case m cs of
    Nothing -> n cs
    mcs -> mcs

infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
  case m cs of
    Nothing -> Nothing
    Just(p, cs) ->
      case n cs of
	Nothing -> Nothing
	Just(q, cs) -> Just((p,q), cs)

(##) :: Parser a -> Parser a -> Parser [a]
(m ## n) cs =
  case m cs of
   Nothing -> Nothing
   Just (p, cs) ->
     case n cs of
       Nothing -> Nothing
       Just(q, cs) -> Just((p:q:[]), cs)

-- (##) :: Parser (a, b) -> Parser c -> Parser (a, b, c)

infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs =
  case m cs of
    Just(a,cs) -> Just(k a, cs)
    Nothing -> Nothing

(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs =
  case m cs of
     Just (p, cs) ->
       case n cs of
	 Nothing -> Nothing
	 Just (q, cs) -> Just (q, cs)
     Nothing -> Nothing

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs =
  case m cs of
     Just (p, cs) ->
       case n cs of
	 Nothing -> Nothing
	 Just (q, cs) -> Just (p, cs)
     Nothing -> Nothing

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

parse_iterate :: Parser a -> Int -> Parser [a]
parse_iterate m 0 = parse_return []
parse_iterate m i = m # parse_iterate m (i-1) >-> cons

parse_iter :: Parser a -> Parser [a]
parse_iter m = m # parse_iter m >-> cons ! parse_return []

--Parsing a char
char :: Parser Char
char (c:cs) = Just(c,cs)
char [] = Nothing

--Parsing a char and verifiy the value
lit :: Char -> Parser Char
lit c = char ? (==c)

digit :: Parser Char
digit cs = (char ? isDigit) cs

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

accept :: String -> Parser String
accept w = token (parse_iterate char (sizeStr w) ? (==w))

infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> k) cs =
  case m cs of
    Nothing -> Nothing
    Just(a,cs) -> k a cs

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
parseFilePath = parse_iter (char ? isLetter)

{-
newtype SeqParser a = SeqParser { getParser :: Parser a }
  deriving (Applicative)

instance Applicative SeqParser where
  pure p = parse_return p
  p1 <*> p2 = p1 # p2
    -}
