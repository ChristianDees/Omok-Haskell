module Main where

import System.IO
import Board

-- map player number to a character
playerToChar :: Int -> Char
playerToChar 1 = 'X'
playerToChar 2 = 'O'
playerToChar _ = '.'

-- check if input can be split into two valid integers
checkTwoInts :: String -> Maybe (Int, Int)
checkTwoInts input =
    case words input of
        [xStr, yStr] -> case (readInt xStr, readInt yStr) of
            (Just x, Just y) -> Just (x, y)  -- both are valid integers
            _                -> Nothing      -- invalid if any read fails
        [aStr] -> case readInt aStr of
            Just a | a == -1 -> Just (a, -1) -- special case for -1, return (a, -1)
            _                -> Nothing      -- invalid if not -1
        _  -> Nothing                        -- otherwise, return nothing

-- helper function to safely read an integer
readInt :: String -> Maybe Int
readInt str = case reads str of
    [(n, "")] -> Just n   -- parsed int successfuly
    _         -> Nothing  -- parsed int failed

-- get cords from input
getCord :: String -> IO (Int, Int)
getCord prompt = do
    hSetBuffering stdout NoBuffering
    putStr prompt
    line <- getLine
    case checkTwoInts line of
        Just (x, y) | x == -1 || y == -1 -> do  -- exit if -1 entered at all
            return (-1,-1)                      -- indicate goodbye
        Just (x, y) -> return (x, y)            -- return x and y if valid
        Nothing -> do
            putStrLn $ "Invalid input: " ++ line
            getCord prompt                      -- retry on invalid input

-- read x and y from player input
readXY :: [[Int]] -> Int -> IO (Int, Int)
readXY bd p = do
    let bdSize = length bd
    let playerPrompt = [playerToChar p] ++ "'s turn: enter x and y (1-" ++ show bdSize ++ " or -1 to quit)? "
    (x, y) <- getCord playerPrompt
    if x == -1 && y == -1 then do
        return (-1,-1)
    else if x < 1 || x > bdSize || y < 1 || y > bdSize then do
        putStrLn $ "Invalid input: " ++ show x ++ " " ++ show y
        readXY bd p     -- retry if cords outside of bounds
    else if isEmpty x y bd then do
        return (x, y)   -- return cords if valid
    else if isMarked x y bd then do
        putStrLn "Already marked!"
        readXY bd p     -- retry if spot taken
    else do
        putStrLn "Error: Unexpected condition!"
        return (-1,-1)

-- main game loop
playOmok :: Int -> [[Int]] -> IO ()
playOmok p bd = do
    putStr (boardToStr playerToChar bd)     -- display board
    (x, y) <- readXY bd p                   -- get cords
    let bd' = mark x y bd p                 -- mark cords
    if x == -1 && y == -1 then do return () -- quit
    else if isGameOver bd' then do      
        putStr (boardToStr playerToChar bd')                          -- check over
        if isWonBy bd' p then do putStrLn $ playerToChar p : " won!"  -- check won
        else if isDraw bd' then do putStrLn "Draw!"                   -- check draw
        else putStrLn "Game Over!"                                      
        return()
    else do         -- continue to next turn
        let nextPlayer = if p == 1 then 2 else 1
        playOmok nextPlayer bd'

-- main
main :: IO ()
main = do
    let board = mkBoard 15          -- setup game board
    playOmok 2 board                -- begin the game with 'O' first
    putStrLn "Exiting..."           -- exit when finished