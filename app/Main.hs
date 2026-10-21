module Main (main) where

import Position ( Position(..) ) -- importing just Position (Haskell infers the fields)

main :: IO ()
main = do
    let pos = Position 1 1
    print pos 
    putStrLn "Hello, Haskell!"