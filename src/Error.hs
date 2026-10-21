module Error (Error(..)) where 

import Position (Position(..))

data Error = Error 
    { position :: Position
    , message :: String
    } deriving (Show)

reporter :: [Error]
reporter = []

addError :: Position -> String -> [Error] -> [Error]
addError pos msg rep =
    let err = Error pos msg
    in err : rep -- prepends error to the reporter

hasErrors :: [Error] -> Bool 
hasErrors rep =
    null rep

formatError :: Position -> String -> String 
formatError pos msg = 
    "error at" ++ show (line pos) ++ ":" ++ show (col pos) ++ ": " ++ msg

printErrors :: [Error] -> IO ()
printErrors rep = do 
    mapM_ (\x -> do
        putStrLn (formatError (position x) (message x))
        ) rep