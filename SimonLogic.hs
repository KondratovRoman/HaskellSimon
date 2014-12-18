module SimonLogic where

import System.Random
import Data.List

data Color = Red | Yellow | Blue | Green
	deriving (Show, Eq)

--генерация случайной доски с четырьмя цветами, которые могут повторяться
--список представляет собой последовательность их отображения
--n - длина списка (зависит от уровня сложности)

{-generateLevel :: Int -> [Color]
generateLevel n = -}

randomColor :: IO Color
randomColor = do
	number <- randomRIO (1,4) :: IO Int
	let col = getColorOnNumber number
	return col
	

getColorOnNumber :: Int -> Color
getColorOnNumber n
	| n==1 = Red
	| n==2 = Yellow
	| n==3 = Blue
	| otherwise = Green 
