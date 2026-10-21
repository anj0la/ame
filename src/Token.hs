module Token(Token(..), TokenKind(..)) where

import Position ( Position(..) ) 
    
data TokenKind 
    = Identifier String 
    -- Literals
    | IntLiteral Int 
    | FloatLiteral Double
    | CharLiteral Char
    | StringLiteral String 
    | True
    | False
    | And 
    | Or 
    | Not
    -- Keywords
    | Fun 
    | Do
    | End
    | Var
    | If 
    | Then 
    | Else 
    | While
    | Return
    -- Punctuation
    | LeftParen
    | RightParen
    | Comma
    | Hash -- Line comment: #
    -- Binary Operators
    | EqualEqual
    | BangEqual
    | LessEqual
    | GreaterEqual 
    | LessLess
    | GreaterGreater
    | Less
    | Greater
    | Plus 
    | Minus 
    | Star 
    | Slash
    | Percent
    -- Assign Operators
    | Equal
    -- Bitwise Operators
    | Amp
    | Pipe 
    | Caret 
    | Tilde
    -- Other 
    | Illegal
    | Eof
    deriving (Show, Eq) -- have to remember, if Token implements Show and Eq type classes, then TokenKind must do so

data Token = Token 
    { kind :: TokenKind
    , lexeme :: String
    , pos :: Position
    } deriving (Show, Eq)