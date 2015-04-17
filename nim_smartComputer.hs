module NimSmart where

showBoard board = "Row 1: " ++ (showSticks (fst3 board)) ++ "\n" ++
	"Row 2: " ++ (showSticks (snd3 board)) ++ "\n" ++
	"Row 3: " ++ (showSticks (thrd3 board)) ++ "\n"

--show 'X' n times
showSticks n = concat ["X" | r <- [0..n-1] ] 

--prompUser =



--for tuple use
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) = c

--move second param = player
move board player = player board



compPlayer board = do
	putStrLn "5"
--remove sticks

--logic for row select
isValidRow board num 
	| num == 1 && fst3 board > 0 = True
	| num == 2 && snd3 board > 0 = True
	| num == 3 && thrd3 board > 0 = True
	| otherwise = False

validRows board = [x | x <- [1..3], (isValidRow board x)]

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
--play
play board= do
	putStr (showBoard board)
	row <- getRow board
	sticks <- getSticks board (read row)
	putStrLn "TEMP"
	play board




main = do
	play (4,3,7)
