module Position (Position(..)) where

data Position = Position
    { line :: Int
    , col :: Int
    } deriving(Show, Eq) -- we want to be able to print the position, and compare Position types