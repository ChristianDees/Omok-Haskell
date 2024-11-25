module Board where

-- create 2d array, 0's = empty 
mkBoard :: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0)

-- represents player1 piece
mkPlayer :: Int
mkPlayer = 1

-- represents player2 piece
mkOpponent :: Int
mkOpponent = 2

-- get size of board
size :: [[Int]] -> Int
size = length

-- get row at y-1
row :: Int -> [[Int]] -> [Int]
row y bd = bd !! (y - 1)

-- get col at x-1
column :: Int -> [[Int]] -> [Int]
column x bd = [ r !! (x - 1) | r <- bd ]

-- mark board with player move
mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
mark x y board player =
  let currentRow = row y board
      newRow = take (x - 1) currentRow ++ [player] ++ drop x currentRow
  in take (y - 1) board ++ [newRow] ++ drop y board

-- check if cords are empty 
isEmpty :: Int -> Int -> [[Int]] -> Bool
isEmpty x y bd = (bd !! (y - 1)) !! (x - 1) == 0

-- check if cords are taken
isMarked :: Int -> Int -> [[Int]] -> Bool
isMarked x y bd = not (isEmpty x y bd)

-- return what player is at specific cord
marker :: Int -> Int -> [[Int]] -> Int
marker x y bd = (bd !! (y - 1)) !! (x - 1)

-- check if cords are taken by specific player (p)
isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
isMarkedBy x y bd p = marker x y bd == p

-- check if board is full
isFull :: [[Int]] -> Bool
isFull = all (notElem 0)

-- map player number to a character
playerToChar :: Int -> Char
playerToChar 1 = 'X'
playerToChar 2 = 'O'
playerToChar _ = '.'

-- represent board with string
boardToStr :: (Int -> Char) -> [[Int]] -> String
boardToStr charConverter bd =
  let sizeBd = length (head bd)
      columnHeader = " x " ++ unwords [show (i `mod` 10) | i <- [1..sizeBd]] ++ "\n" ++ "y " ++ replicate (sizeBd * 2) '-' ++ "\n"
  in columnHeader ++ concat [show (y `mod` 10) ++ "|" ++ concatMap (\x -> " " ++ [charConverter (r !! x)]) [0..sizeBd-1] ++ "\n" | (y, r) <- zip ([1 :: Int ..] :: [Int]) bd]

-- check if the board has a winning row, column, or diagonal for player p
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = any (fiveInRow p) bd || any (fiveInRow p) (columns bd) || any (fiveInRow p) (diagonalsTLBR bd) ||any (fiveInRow p) (diagonalsBLTR bd)

-- Helper function to check for 5 consecutive pieces
fiveInRow :: Int -> [Int] -> Bool
fiveInRow p r = any (all (== p)) (sliding 5 r)

-- Generate sliding windows of size n over a list
sliding :: Int -> [a] -> [[a]]
sliding n xs = [take n (drop i xs) | i <- [0..length xs - n]]

-- get all cols
columns :: [[Int]] -> [[Int]]
columns bd = [column (x + 1) bd | x <- [0..size bd - 1]]

-- diagnoal: top left to bottom right 
diagonalsTLBR :: [[Int]] -> [[Int]]
diagonalsTLBR bd = let bdSize = length bd in [ [bd !! (i + k) !! k | k <- [0..min (bdSize - 1 - i) (bdSize - 1)]] | i <- [0..bdSize - 1]]

-- diagonal: bottom left to top right
diagonalsBLTR :: [[Int]] -> [[Int]]
diagonalsBLTR bd = let bdSize = length bd in [ [bd !! (i - k) !! k | k <- [0..min i (bdSize - 1 - j)]] | i <- [bdSize - 1, bdSize - 2..0], j <- [0..bdSize - 1] ]

-- check if game is a draw
isDraw :: [[Int]] -> Bool
isDraw bd = isFull bd && not (isWonBy bd 1) && not (isWonBy bd 2)

-- check if game is a draw
isGameOver :: [[Int]] -> Bool
isGameOver bd = isDraw bd || isWonBy bd 1 || isWonBy bd 2