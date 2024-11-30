module Main where

import System.IO
import Board

import System.Random

-- map player number to a character
playerToChar :: Int -> Char
playerToChar 1 = 'X'
playerToChar 2 = 'O'
playerToChar _ = '.'

-- check if input can be split into two valid integers
checkTwoInts :: String -> Maybe (Int, Int)
checkTwoInts input =
    case words input of
        ["-6", "-9"] -> Just (-6, -9)        -- activate computer mode
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
    if x == -1 && y == -1 then return (-1,-1)
    else if x == -6 && y == -9 then return (-6, -9)
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

-- generate random valid x and y cords 
generateComputerMove :: [[Int]] -> IO (Int, Int)
generateComputerMove bd = do
    let bdLen = length bd       -- size of the board
    x <- randomRIO (1, bdLen)   -- random x between 1 and n
    y <- randomRIO (1, bdLen)   -- random y between 1 and n
    if isEmpty x y bd           -- if spot is valid, return it
        then return (x, y)
        else generateComputerMove bd 

-- main game loop
playOmok :: Int -> [[Int]] -> Bool -> IO ()
playOmok p bd compMode = do
    putStrLn (boardToStr playerToChar bd)
    (x, y) <- if compMode && p == 1      -- get moves          
              then do                    -- computer turn (X)
                (compX, compY) <- generateComputerMove bd
                putStrLn $ "Computer's move: " ++ show (compX, compY)
                return (compX, compY)
              else do                    -- user turn (O)
                (usrX, usrY) <- readXY bd p
                if usrX == -1 && usrY == -1 then return (-1, -1)
                else return (usrX, usrY) -- return user move
    if x == -1 && y == -1 then do        -- exit game if requested
        putStrLn "Exiting..." 
        return ()
    else do
        if x == -6 && y == -9 then do    -- toggle comp mode with -6 -9
            putStrLn "Computer mode activated."
            playOmok p bd True           -- switch to comp mode, user goes first
        else do
            let bd' = mark x y bd p      -- update board
            if isGameOver bd' then do    -- check game data
                putStrLn (boardToStr playerToChar bd')
                if isWonBy bd' p then putStrLn (playerToChar p : " won!")
                else if isDraw bd' then putStrLn "It's a draw!"
                else putStrLn "Game Over!"
                return ()
            else do                      -- continue playing
                let nextPlayer = if p == 1 then 2 else 1   
                playOmok nextPlayer bd' compMode

-- main
main :: IO ()
main = do
    let board = mkBoard 15   -- setup game board
    playOmok 2 board False   -- begin the game with 'O' first
    putStrLn "Exiting..."    -- exit when finished