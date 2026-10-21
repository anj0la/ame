module Main (main) where

import Position

main :: IO ()
main = do
    let pos = Position 1 1
    print pos 
    putStrLn "Hello, Haskell!"