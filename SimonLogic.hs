module SimonLogic where

import System.Random
import Data.List
import System.IO
import Control.Monad

data Color = Red | Yellow | Blue | Green
	deriving (Show, Eq)

--состояние хранит список последовательно появившихся цветов
type State = [Color] 

{-Функция, генерирующая уровень. 
n - номер уровня и количество зажигающихся цветов на этом уровне-}
generateLevel :: Int -> IO State
generateLevel n = forM [1..n] $ (\a -> do
			x <- randomColor
			return x)

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


{-Функция, проверяющая правильность нажатых пользователем цветов.
Получает на вход пользовательское состояние и правильное состояние-}
compareStates :: State -> State -> Bool
compareStates userState trueState = userState == trueState 

win :: State -> State -> IO()
win st1 st2
	| compareStates st1 st2 = print "You win!"
	| otherwise = print "You fail!"

{-Таймер-}

{-Консольная реализация нашей игры в ее простейшем виде-
напишу чуть позже}
