module Main where
import Graphics.UI.WX
import SimonLogic
import System.IO
import Control.Monad
import Data.IORef
import Data.List
import Control.Concurrent (threadDelay)

bugtext = unlines [ "Former bug: these buttons should react when clicked"
                  , "but the boxed one does not"
                  , ""
                  , "Buggy   in: MacOS X [now fixed!]"
                  , "Working in: Linux and Windows"
                  ]


about :: Window a -> IO ()
about w
  = infoDialog w "About Simon" "Simon \n\nby sfedu\n\n Simon was written using wxHaskell"

endOfGame :: Window a -> IO ()
endOfGame w
  = infoDialog w "Game over!" "You made error in the sequence!"

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Simon Help"
  (  "  Kill me,please \n\n"  )

main :: IO ()
main = start $ gui

--Если пользователь проиграл, то обнуляем все и запускаем игру заново
gameOver :: Window a -> IORef State -> IORef Int -> IORef State -> TextCtrl () -> TextCtrl() -> IO()
gameOver f userList n levelList label levelInfo = do
    endOfGame f --выводим сообщение о проигрыше
    let tempList = [] --обнуляем пользовательскую последовательность
    writeIORef userList tempList 
    writeIORef n 1 --генерируем заново 1ый уровень
    level <- generateLevel 1
    writeIORef levelList level
    set label [ text := stringLevel level]
    set levelInfo [ text := "Удачи в игре! "]
    return()



--проверяет верно ли пользователь нажал кнопки и генерирует новый уровень
actionGUI :: Window a -> IORef Int -> TextCtrl () -> IORef State -> IO()
actionGUI f ref txtTitle  constState = do
    st <- readIORef ref
    --let equal = compareStates st1 st2
    state <- generateLevel st
    writeIORef constState state
    --let level = stringLevel state
    showColorsListWithDelay f constState txtTitle 
    writeIORef ref (st+1)
    return ()

-- Функция, которая выводит в лейбл один элемент сгенерированной последовательности
showOneColor :: IORef State -> TextCtrl ()-> IO()
showOneColor colors label = do
    currState <- readIORef colors
    if currState == [] then return() else showOneColor_ colors label

showOneColor_ :: IORef State -> TextCtrl ()-> IO()
showOneColor_ colors label = do
    currState <- readIORef colors
    let color = head currState
    let str = colorToString color
    set label [ text := str]
    threadDelay $ 100000 * 5
    set label [ text := ""]
    if (currState == []) then return() else (writeIORef colors (tail currState))

-- Функция, которая показывает пользователю сгенерированную последовательность поэлементно с задержкой по таймеру
showColorsListWithDelay :: Window a -> IORef State -> TextCtrl () -> IO()
showColorsListWithDelay f generatedList label = do
  --копируем generated list
    stateGenList <- readIORef generatedList
    generatedListCopy <- newIORef (stateGenList)
    t <- timer f [interval := 1000, on command := showOneColor generatedListCopy label]
    --t <- timer f [interval := 1030, on command := showOneColor generatedListCopy label]
    set label [ text := ""]
    return ()


-- Обнуляет пользовательский список перед генерацией нового игрового уровня и запускает новый уровень
nullUsersList :: Window a -> IORef State -> IORef Int -> TextCtrl () -> IORef State -> TextCtrl() -> IO()
nullUsersList f userList n textField st1 levelInfo = do
    nn <- readIORef n
    set levelInfo [ text := "Вы прошли уровень " ++ show (nn-1) ++ " Ура!"]
    let tempList = []
    writeIORef userList tempList
    actionGUI f n textField st1
    return()

actionUserButtons :: TextCtrl () -> IORef Int -> Window a -> IORef State -> IORef State -> GameColor -> TextCtrl () -> IO()
actionUserButtons textField n w refUser st1 butColor levelInfo = do 
    st <- readIORef refUser
    constState <- readIORef st1
    let modifiedUserList = st ++ [butColor]
    writeIORef refUser modifiedUserList
    let partOfGenList = getNfromList (length modifiedUserList) constState  
    let equal = compareStates modifiedUserList partOfGenList  -- Сравнивает текущий подсписок с соотв. по длине подскиском программы лексико-графически
    if (equal == False) then (gameOver w refUser n st1 textField levelInfo)  else  -- Если подпоследовательность не равна соответствующей подпоследовательности списка программы, 
                                                --то была допущена ошибка, программа завершается
                if (length modifiedUserList == length constState) then nullUsersList w refUser n textField st1 levelInfo else return()



gui :: IO ()
gui =  do
  -- форма, на которой будут лежать все наши контролы
  f <- frame [ text := "Simon" ]
  
  let n = 1
  state <- generateLevel n
  let level = stringLevel state 
  ref <- newIORef (n+1)

   -- Это список, в который записивается сгенерированная программой последовательность для определенного игрового уровня
  refState <- newIORef state

  -- Это список, который накапливает значения на определенном уровне по нажатию цветных кнопок пользователя
  let userList = []
  refUserList <- newIORef userList
 
  -- Task text 
  txtTitle <- entry f [text := level , enabled := False ]
  --заглушка для таймера,показывающего, сколько осталось для закрытия задания.
  -- t <- timer f [interval := 20, on command := nextBalls vballs p]
  --Заглушка, для таймера отсчитывающего, сколько осталось времени до конца выполнения задания
   --howManyTimer <- entry f [text := "01:29" , enabled := False ]
  congr <- entry f [text := "Удачи в игре!" , enabled := False ]
  b1 <- button f [ text := " ", bgcolor  := yellow, clientSize := sz 100 100  ]
  b2 <- button f [ text := " " , bgcolor  := green, clientSize := sz 100 100 ]
  b3 <- button f [ text := " " , bgcolor  := blue, clientSize := sz 100 100]
  b4 <- button f [ text := " " , bgcolor  := red, clientSize := sz 100 100 ]
  q <- button f [ text := "quit" , on command := close f ]
  h <- button f [ text := "help" , on command := chelp f ]
  a <- button f [ text := "about", on command := about f ]


 -- t <- timer f [interval := 1000, on command := showOneColor refState txtTitle]
  set b1 [ on command := actionUserButtons txtTitle ref f refUserList refState Yellow congr]
  set b2 [ on command := actionUserButtons txtTitle ref f refUserList refState Green congr]
  set b3 [ on command := actionUserButtons txtTitle ref f refUserList refState Blue congr]
  set b4 [ on command := actionUserButtons txtTitle ref f refUserList refState Red congr]
 
  set f [ layout := column 0 [
	    -- fill $ widget p
        {- верхний ряд -}
        margin 1 $ row 1 [
	     -- само задание
        hfill $ minsize (sz 200 25) $ widget txtTitle
		--,minsize (sz 150 40) $ widget  howManyTimer 
        -- Таймер до закрытия
	     --hfill $ minsize (sz 150 25) $ widget taskShowTimer   
        ],
		
		--переход уровня
        margin 1 $ row 1 [
	     -- само задание
        hfill $ minsize (sz 150 25) $  widget congr
        ],
		
        {- средний ряд -}
        margin 5 $ row 5 [
        -- слева: кнопки
          row 5 [
            widget b1, 
			widget b2
           ]
		  ], 
	    margin 5 $ row 5 [
		   row 5 [
		    widget b3,
	        widget b4
		   ]
	    --,minsize (sz 150 40) $ widget  howManyTimer 
        ],
	{- нижний ряд -}
        hfloatCentre $  margin 1 $ row 1 [ 
	  widget q,
	  widget h,
	  widget a
        ] 
	--  defaultButton := q
     ]
   ] 

  return() 