module SimonLogic where

import System.Random
import Data.List
import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

data GameColor = Red | Yellow | Blue | Green
	deriving (Show, Eq)

--состояние хранит список последовательно появившихся цветов
type State = [GameColor] 

{-Функция, генерирующая уровень. 
n - номер уровня и количество зажигающихся цветов на этом уровне-}
generateLevel :: Int -> IO State
generateLevel n = forM [1..n] $ (\a -> do
			x <- randomColor
			return x)

randomColor :: IO GameColor
randomColor = do
	number <- randomRIO (1,4) :: IO Int
	let col = getColorOnNumber number
	return col
	

getColorOnNumber :: Int -> GameColor
getColorOnNumber n
	| n==1 = Red
	| n==2 = Yellow
	| n==3 = Blue
	| otherwise = Green 

  {-Функция, которая берет первые несколько элементов списка-}
getNfromList :: Int -> State -> State
getNfromList n list = take n list


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
напишу чуть позже-}
parseStr :: String -> [GameColor]
parseStr str = parseList $ words str 

parseList :: [String] -> [GameColor]
parseList xs = map (\x -> prs x) xs
	where 	
		prs :: String -> GameColor
		prs x 
			| x=="Red" = Red
			| x=="Blue" = Blue
			| x=="Yellow" = Yellow
			|otherwise = Green 

stringLevel :: State -> String
stringLevel st = foldl(\acc x -> acc ++ " " ++ show x) "" st  
	
	
action n = do
	state <- generateLevel n
	putStrLn $ stringLevel state
	str <- getLine
    	let userList = parseStr str
	win userList state
	if compareStates userList state then action (n+1) else win userList state






