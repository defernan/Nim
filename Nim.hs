module Nim where

showBoard row1 row2 row3 = "Row 1: " ++ (showSticks row1) ++ "\n" ++
	"Row 2: " ++ (showSticks row2) ++ "\n" ++
	"Row 3: " ++ (showSticks row3) ++ "\n"

--show 'X' n times
showSticks n = concat ["X" | r <- [0..n-1] ] 

main = putStrLn (showBoard 4 3 7)