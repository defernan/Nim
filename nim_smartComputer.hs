module NimSmart where

import Numeric
import Data.Char
{-
-
-
-----------------------BOARD---------------------------------
-
-
-}
--DISPLAY BOARD
showBoard board = "Row 1: " ++ (showSticks (fst3 board)) ++ "\n" ++
	"Row 2: " ++ (showSticks (snd3 board)) ++ "\n" ++
	"Row 3: " ++ (showSticks (thrd3 board)) ++ "\n"

--show 'X' n times
showSticks n = concat ["X" | r <- [0..n-1] ] 


--RETRIEVING BOARD VALUES
--for tuple use
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) = c

--move second param = player
move board player = player board

{-
-
-
-----------------COMPUTER PLAYER-------------------
-
-
-}

compPlayer board = do
	putStrLn "5"


--HELPER 
--convert board to binary
boardToBinary:: (Show t1, Show t, Show a, Integral t1, Integral t, Integral a) => (t, t1, a) -> ([Char], [Char], [Char])
boardToBinary board = (intToBinary (fst3 board), intToBinary (snd3 board), intToBinary (thrd3 board))

intToBinary :: (Show a, Integral a) => a -> [Char]
intToBinary n
	| n < 4 = "0" ++ (showIntAtBase 2 intToDigit n "")
	| n < 2 = "00" ++ (showIntAtBase 2 intToDigit n "")
	| otherwise = (showIntAtBase 2 intToDigit n "")

--get kernal state
{-
-
-
----------------HUMAN PLLAYER--------------------
-
-
-}
--HELPER FUNCTIONS
--ROW SELECT
--logic for row select
isValidRow board num 
	| num == 1 && fst3 board > 0 = True
	| num == 2 && snd3 board > 0 = True
	| num == 3 && thrd3 board > 0 = True
	| otherwise = False

validRows board = [x | x <- [1..3], (isValidRow board x)]

--only returns when valid selection has been made
getRow board= do 
	putStr "Enter one of the following row numbers: "
	putStrLn(show (validRows board))
	row <- getLine
	if (isValidRow board (read row))
		then do 
			return row
		else do
			putStrLn "\nInvalid Row Number"
			getRow board

--STICK SELECT
--logic for stick select
isValidSticks board row num 
	| row == 1 && num > 0 && num <= fst3 board = True
	| row == 2 && num > 0 && num <= snd3 board = True
	| row == 3 && num > 0 && num <= thrd3 board = True
	| otherwise = False

validSticks board row 
	| row == 1 = "1-" ++ (show (fst3 board))
	| row == 2 = "1-" ++ (show (snd3 board))
	| row == 3 = "1-" ++ (show (thrd3 board))
	| otherwise = "error"

--only returns when valid selection has been made
getSticks board row= do 
	putStr "Select number of sticks to remove:"
	putStrLn (validSticks board row)
	sticks <- getLine
	if (isValidSticks board row (read sticks))
		then do 
			return sticks
		else do
			putStrLn "\nInvalid Stick Number"
			getSticks board row

{-
-
-
-
----------------------------GAME PLAY-----------------------------
-
-
-}

makeMove board row sticks
	| row == 1 = ((fst3 board) - sticks, snd3 board, thrd3 board)
	| row == 2 = (fst3 board, (snd3 board) - sticks, thrd3 board)
	| row == 3 = (fst3 board, snd3 board, (thrd3 board) - sticks)

--play
play board= do
	putStr (showBoard board)
	row <- getRow board
	sticks <- getSticks board (read row)
	putStrLn "TEMP"
	play (makeMove board (read row) (read sticks))




main = do
	play (4,3,7)
