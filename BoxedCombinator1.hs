module Main where
import Graphics.UI.WX
import SimonLogic
import Data.IORef
import Data.List

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

--проверяет верно ли пользователь нажал кнопки и генерирует новый уровень
actionGUI :: IORef Int -> TextCtrl () -> IORef State -> IO()
actionGUI ref txtTitle  constState = do
    st <- readIORef ref
    --let equal = compareStates st1 st2
    state <- generateLevel st
    writeIORef constState state
    let level = stringLevel state
    set txtTitle [ text := level ]
    writeIORef ref (st+1)
    return ()

-- Обнуляет пользовательский список перед генерацией нового игрового уровня
nullUrersList :: IORef State -> IORef Int -> TextCtrl () -> IORef State -> IO()
nullUrersList userList n textField st1 = do
    let tempList = []
    writeIORef userList tempList
    actionGUI n textField st1
    return()

actionUserButtons :: TextCtrl () -> IORef Int -> Window a -> IORef State -> IORef State -> GameColor -> IO()
actionUserButtons textField n w refUser st1 butColor = do 
    st <- readIORef refUser
    constState <- readIORef st1
    let modifiedUserList = st ++ [butColor]
    writeIORef refUser modifiedUserList
    let partOfGenList = getNfromList (length modifiedUserList) constState  
    let equal = compareStates modifiedUserList partOfGenList  -- Сравнивает текущий подсписок с соотв. по длине подскиском программы лексико-графически
    if (equal == False) then endOfGame w  else  -- Если подпоследовательность не равна соответствующей подпоследовательности списка программы, 
                                            --то была допущена ошибка, программа завершается
                if (length modifiedUserList == length constState) then nullUrersList refUser n textField st1 else return()



{-updateBtns :: [Button ()] -> Board -> IO ()
updateBtns btns brd = do
  let z = zip (elems brd) btns
  forM_ z $ \p -> set (snd p) [text := (btnLabel (fst p))]
-}
gui :: IO ()
gui =  do
  state <- generateLevel 1
  let level = stringLevel state
  -- форма, на которой будут лежать все наши контролы
  f <- frame [ text := "Simon" ]

  let n = 1
  ref <- newIORef (n+1)

  -- Это список, который накапливает значения на определенном уровне по нажатию цветных кнопок пользователя
  let userList = []
  refUserList <- newIORef userList
  -- Это список, в который записивается сгенерированная программой последовательность для определенного игрового уровня
  refState <- newIORef state
  -- Task text 
  txtTitle <- entry f [text := level , enabled := False ]
  --заглушка для таймера,показывающего, сколько осталось для закрытия задания.
  taskShowTimer <- entry f [text := "01:29" , enabled := False ]
  --Заглушка, для таймера отсчитывающего, сколько осталось времени до конца выполнения задания
  howManyTimer <- entry f [text := "01:29" , enabled := False ]
  b1 <- button f [ text := "yellow", bgcolor  := yellow  ]
  b2 <- button f [ text := "green" , bgcolor  := green ]
  b3 <- button f [ text := "blue" , bgcolor  := blue]
  b4 <- button f [ text := "red" , bgcolor  := red ]
  q <- button f [ text := "quit" , on command := close f ]
  h <- button f [ text := "help" , on command := chelp f ]
  a <- button f [ text := "about", on command := about f ]

  set b1 [ on command := actionUserButtons txtTitle ref f refUserList refState Yellow]
  set b2 [ on command := actionUserButtons txtTitle ref f refUserList refState Green]
  set b3 [ on command := actionUserButtons txtTitle ref f refUserList refState Blue]
  set b4 [ on command := actionUserButtons txtTitle ref f refUserList refState Red]
 
  set f [ layout := column 0 [
	-- fill $ widget p
        {- верхний ряд -}
        margin 1 $ row 1 [
	-- само задание
        hfill $ minsize (sz 150 25) $ widget txtTitle,
        -- Таймер до закрытия
	    hfill $ minsize (sz 150 25) $ widget taskShowTimer   
        ],
        {- средний ряд -}
        hfloatCentre $  margin 1 $ row 2 [
         -- слева: кнопки
           row 2 [
             widget b1, 
	     widget b2,
	     widget b3,
	     widget b4
           ],
	    minsize (sz 150 40) $ widget  howManyTimer 
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


  

  --set a [ on command := actionGUI ref txtTitle state state n]
  return() 
  



