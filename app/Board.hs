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
  let -- get row
      currentRow = row y board
      -- make new row
      newRow = take (x - 1) currentRow ++ [player] ++ drop x currentRow
      -- replace old row with new row
  in take (y - 1) board ++ [newRow] ++ drop y board

-- check if cords are empty 
isEmpty :: Int -> Int -> [[Int]] -> Bool
isEmpty x y bd = (bd !! (x - 1)) !! (y - 1) == 0

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
  let sizeBd = length (head bd)  -- size of the board (assumed square or uniform rows)
      -- column header (1-size of board, wrap at 9)
      columnHeader = " x " ++ unwords [show (i `mod` 10) | i <- [1..sizeBd]] ++ "\n" ++ "y " ++ replicate (sizeBd * 2) '-' ++ "\n"
      -- each row header (1-size of board, wrap at 9)
  in columnHeader ++ concat [show (y `mod` 10) ++ "|" ++ concatMap (\x -> " " ++ [charConverter (r !! x)]) [0..sizeBd-1] ++ "\n" | (y, r) <- zip ([1 :: Int ..] :: [Int]) bd]