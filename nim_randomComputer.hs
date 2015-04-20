module NimRandom where

import Numeric
import Data.Char
import System.Random
{-
-
-
-----------------------BOARD---------------------------------
-
-
-}
--DISPLAY BOARD
showBoard :: (Num t2, Num t1, Num t, Enum t2, Enum t1, Enum t) => (t1, t2, t) -> [Char]
showBoard board = "Row 1: " ++ (showSticks (fst3 board)) ++ "\n" ++
	"Row 2: " ++ (showSticks (snd3 board)) ++ "\n" ++
	"Row 3: " ++ (showSticks (thrd3 board)) ++ "\n"

--show 'X' n times
showSticks :: (Num t, Enum t) => t -> [Char]
showSticks n = concat ["X" | r <- [0..n-1] ] 


--RETRIEVING BOARD VALUES
--for tuple 
fst3 :: (t, t1, t2) -> t
fst3 (a,_,_) = a

snd3 :: (t, t1, t2) -> t1
snd3 (_,b,_) = b

thrd3 :: (t, t1, t2) -> t2
thrd3 (_,_,c) = c

{-
-
-
-----------------COMPUTER PLAYER-------------------
-
-
-}
compPlayerMove :: (Int, Int, Int) -> StdGen -> ((Int, Int, Int), StdGen)
compPlayerMove board gen
	| row == 1 && isValidRow board row = (((fst3 board) - numSticks, snd3 board, thrd3 board),gen'')
	| row == 2 && isValidRow board row = ((fst3 board, (snd3 board) - numSticks, thrd3 board),gen'')
	| row == 3 && isValidRow board row = ((fst3 board, snd3 board, (thrd3 board - numSticks)),gen'')	
	| otherwise = compPlayerMove board gen''
	where
		rowGen = getRandomRow gen
		row = fst rowGen
		gen' = snd rowGen
		maxSticks = getSticksInRow board row
		sticks = getRandomSticks maxSticks gen'
		numSticks = fst sticks
		gen'' = snd sticks

getSticksInRow::(Int, Int, Int) -> Int -> Int
getSticksInRow board row
	|row == 1 = fst3 board
	|row == 2 = snd3 board
	|row == 3 = thrd3 board
		
getRandomRow::StdGen -> (Int, StdGen)
getRandomRow gen = randomR (1,3) gen

getRandomSticks::Int -> StdGen -> (Int, StdGen)
getRandomSticks maxSticks gen = randomR (1, maxSticks) gen
	

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
isValidRow :: (Ord t1, Ord t, Ord a1, Num t1, Num t, Num a1, Num a, Eq a) => (t, t1, a1) -> a -> Bool
isValidRow board num 
	| num == 1 && fst3 board > 0 = True
	| num == 2 && snd3 board > 0 = True
	| num == 3 && thrd3 board > 0 = True
	| otherwise = False

validRows :: (Ord t2, Ord t1, Ord a, Num t2, Num t1, Num a, Num t, Eq t, Enum t) => (t1, t2, a) -> [t]
validRows board = [x | x <- [1..3], (isValidRow board x)]

--only returns when valid selection has been made
getRow :: (Ord t1, Ord t, Ord a, Num t1, Num t, Num a) => (t, t1, a) -> IO String
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
isValidSticks :: (Ord t, Num t, Num a, Eq a) => (t, t, t) -> a -> t -> Bool
isValidSticks board row num 
	| row == 1 && num > 0 && num <= fst3 board = True
	| row == 2 && num > 0 && num <= snd3 board = True
	| row == 3 && num > 0 && num <= thrd3 board = True
	| otherwise = False

validSticks :: (Show t1, Show t, Show a1, Num a, Eq a) => (t, t1, a1) -> a -> [Char]
validSticks board row 
	| row == 1 = "1-" ++ (show (fst3 board))
	| row == 2 = "1-" ++ (show (snd3 board))
	| row == 3 = "1-" ++ (show (thrd3 board))
	| otherwise = "error"

--only returns when valid selection has been made
getSticks :: (Show t, Read t, Ord t, Num t, Num a, Eq a) => (t, t, t) -> a -> IO String
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
checkWin :: (Num t1, Num t, Num a, Eq t1, Eq t, Eq a) => (t, t1, a) -> Bool
checkWin board
	| fst3 board == 0 && snd3 board == 0 && thrd3 board == 0 = True
	| otherwise = False

makeMove :: (Num t, Num a, Eq a) => (t, t, t) -> a -> t -> (t, t, t)
makeMove board row sticks
	| row == 1 = ((fst3 board) - sticks, snd3 board, thrd3 board)
	| row == 2 = (fst3 board, (snd3 board) - sticks, thrd3 board)
	| row == 3 = (fst3 board, snd3 board, (thrd3 board) - sticks)

--play
play :: (Int, Int, Int)->StdGen -> IO ()
play board gen= do
	putStrLn "Human's Move"
	putStr (showBoard board)
	row <- getRow board
	sticks <- getSticks board (read row)
	let board1 = (makeMove board (read row) (read sticks))
	if (checkWin board1)
		then do
			putStrLn "Human won!"
			putStrLn (showBoard board1)
		else do
			let (board2, gen') = compPlayerMove board1 gen
			print gen'
			putStrLn "Computer's Move"
			putStrLn (showBoard board1)
			if (checkWin board2)
				then do
					putStrLn "Computer won!"
					putStrLn (showBoard board2)
				else do
					play (board2) gen'


createInitRandom::StdGen -> StdGen
createInitRandom gen = snd $ (randomR (1,100000) gen :: (Int, StdGen))

main :: IO ()
main = do
	gen <- newStdGen
	putStrLn "NIM!"
	let sGen = createInitRandom gen
	print sGen
	play (4,3,7) sGen