module Main where

import System.IO 
import Text.Read (readMaybe)
import System.Exit 
import Board
import System.Random

-- check if input can be split into two ints
checkTwoInts :: String -> Maybe (Int, Int)
checkTwoInts input = case mapM readMaybe (words input) of
    Just [a, b] -> Just (a, b)          -- if both are ints
    Just [a] | a == -1 -> Just (a, -2)  -- in case only one "-1" is specified
    _  -> Nothing                       -- otherwise nothing

-- get cords from input
getCord :: String -> IO (Int, Int)
getCord prompt = do
    hSetBuffering stdout NoBuffering
    putStr prompt
    line <- getLine
    case checkTwoInts line of
        Just (x, y) | x == -1 || y == -1 -> do  -- exit if -1 entered at all
            putStrLn "Goodbye!"
            exitFailure
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
    if x < 1 || x > bdSize || y < 1 || y > bdSize then do
        putStrLn $ "Invalid input: " ++ show x ++ " " ++ show y
        readXY bd p     -- retry if cords outside of bounds
    else if isEmpty x y bd then do
        return (x, y)   -- return cords if valid
    else if isMarked x y bd then do
        putStrLn "Already marked!"
        readXY bd p     -- retry if spot taken
    else do
        putStrLn "Error: Unexpected condition!"
        exitFailure

-- main game loop
playOmok :: Int -> [[Int]] -> IO ()
playOmok p bd = do
    putStr (boardToStr playerToChar bd)
    (x, y) <- readXY bd p
    let bd' = mark x y bd p
    -- checkIfWon bd p
    -- checkIfDraw bd
    let nextPlayer = if p == 1 then 2 else 1
    playOmok nextPlayer bd'  

-- main
main :: IO ()
main = do
    let board = mkBoard 15          -- setup game board
    firstPlayer <- randomRIO (1, 2) -- randomly choose first player
    playOmok firstPlayer board      -- begin the game