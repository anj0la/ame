module Lexer (Lexer(..)) where

import Error
import Position
import Token

data Lexer = Lexer 
    { source :: String 
    , start :: Int
    , curr :: Int 
    , lex_pos :: Position
    , tokens :: [Token]
    , rep :: [Error]
    }

init :: String -> Lexer 
init src = Lexer { source = src, start = 0, curr = 0, lex_pos = Position 1 1, tokens = [], rep = [] }

isDigit :: Char -> Bool 
isDigit c = c >= '0' && c <= '9'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

isBinaryDigit :: Char -> Bool 
isBinaryDigit c = c == '0' || c == '1'

isOctalDigit :: Char -> Bool 
isOctalDigit c = c >= '0' && c <= '7'

isAlpha :: Char -> Bool 
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlnum :: Char -> Bool
isAlnum c = isAlpha c || isDigit c

isIdentiferStart :: Char -> Bool
isIdentiferStart c = isAlpha c || c == '_'

isIdentiferChar :: Char -> Bool
isIdentiferChar c = isIdentiferStart c || isDigit c

isSymbol :: Char -> Bool 
isSymbol c = (c >= '!' && c <= '/') || (c >= ':' && c <= '@') || (c >= '[' && c <= '`') || (c >= '{' && c <= '~')

isSpace :: Char -> Bool 
isSpace c = c == ' '

isAtEnd :: Lexer -> Bool
isAtEnd lex = (curr lex) >= length (source lex)

bump :: Lexer -> Lexer -- only returns updated lexer state
bump lex = lex { curr = curr lex + 1 }


