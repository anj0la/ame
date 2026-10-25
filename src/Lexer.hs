module Lexer (Lexer(..)) where

import Error
import Position
import Token

-- LEXER TYPE 

data Lexer = Lexer 
    { source :: String 
    , lexPos :: Position
    , tokens :: [Token]
    , rep :: [Error]
    }

init :: String -> Lexer 
init src = Lexer { source = src, lexPos = Position 1 1, tokens = [], rep = [] }

-- HELPER FUNCTIONS

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

peek :: String -> Maybe Char 
peek [] = Nothing 
peek (x : _) = Just x

peekNext :: String -> Maybe Char 
peekNext [] = Nothing 
peekNext (_ : x : _) = Just x 

updatePos :: Position -> Int -> Int -> Position 
updatePos oldPos newLine newCol = 
    oldPos 
        { line = newLine
        , col = newCol
        }

advance :: Lexer -> (Char, Lexer)
advance lexer = 
    let char = head (source lexer)
        oldPos = lexPos lexer
    in case char of
        '\n' -> (char, lexer 
                        { source = tail (source lexer) 
                        , lexPos = updatePos oldPos (line oldPos + 1) (1)
                        }) 
        '\t' -> (char, lexer 
                        { source = tail (source lexer)
                        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 4)
                        })
        '\r' -> (char, lexer 
                        { source = tail (source lexer)
                        , lexPos = updatePos oldPos (line oldPos) (1)
                        }) 
        _ -> (char, lexer 
                        { source = tail (source lexer)
                        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 1)
                        })

bump :: Lexer -> Lexer -- only returns updated lexer state
bump lexer = 
    let oldPos = lexPos lexer 
    in lexer 
        { source = tail (source lexer)
        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 1) 
        }

-- READ FUNCTIONS 

readLineComment :: Lexer -> Lexer 
readLineComment lexer =
    case peek (source lexer) of 
        Just '\n'   -> lexer 
        Just _      -> 
            let (_, lexer') = advance lexer 
            in readLineComment lexer'
        Nothing     -> lexer -- reached the end of the lexer