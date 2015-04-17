module Nim where

showBoard board = "Row 1: " ++ (showSticks (fst3 board)) ++ "\n" ++
	"Row 2: " ++ (showSticks (snd3 board)) ++ "\n" ++
	"Row 3: " ++ (showSticks (thrd3 board)) ++ "\n"

--show 'X' n times
showSticks n = concat ["X" | r <- [0..n-1] ] 

--prompUser =

play board = do
	putStrLn (showBoard board)
	move board humanPlay
	move board compPlayer

--for tuple use
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) = c

--move second param = player
move board player = player board

humanPlay board= do
	putStr "Enter one of the following row numbers: "
	putStrLn(show (validRows board))
	row <- getLine
	if (isValidRow board (read row))
		then do
			putStr "Select number of sticks to remove:"
			putStrLn (validSticks board (read row))
		else do 
			putStrLn "Invalid row number"
			humanPlay board

compPlayer board = do
	putStrLn "5"

--logic for row select
isValidRow board num 
	| num == 1 && fst3 board > 0 = True
	| num == 2 && snd3 board > 0 = True
	| num == 3 && thrd3 board > 0 = True
	| otherwise = False

validRows board = [x | x <- [1..3], (isValidRow board x)]

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
--code for humanPlay
--returns list of valid rows
--validRows board = []
{-
isValid num =



--returns list of valid sticks to take from row
validSticks row =


humanMove = do
	showBoard 4 3 7
-}


main = do
	play (4,3,7)