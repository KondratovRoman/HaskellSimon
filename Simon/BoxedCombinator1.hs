module Main where
import Graphics.UI.WX
import SimonLogic
import System.IO
import Control.Monad
import Data.IORef
import Data.List
import Control.Concurrent (threadDelay)

about :: Window a -> IO ()
about w
  = infoDialog w "About Simon" "Simon \n\n Авторы: Рындеева, Кондратов, Карпенко \n\n В рамках курса \"Функциональное Программирование\"  \n Simon was written using wxHaskell \n Институт математики, механики и компьютерных наук ЮФУ, 2014\n "

endOfGame :: Window a -> IO ()
endOfGame w
  = infoDialog w "Game over!" "You made error in the sequence!"

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Simon Help"
  (  "How to play Simon\n\n"
  ++ " Four colored buttons light up in a specific pattern.\n"
  ++ " After displaying the pattern, the player must repeat the pattern by \n" 
  ++ " clicking the buttons in proper order. The pattern gets longer each time \n"
  ++ " the player completes the pattern. If the player presses a wrong button, the game ends. \n\n"  
  ++ " Good luck!\n"
  )
  

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
actionGUI :: [Button ()] -> [(Button(), GameColor)] -> Window a -> IORef Int -> TextCtrl () -> IORef State -> IO()
actionGUI buttons pairs f ref txtTitle  constState = do
    st <- readIORef ref
    state <- generateLevel st
    writeIORef constState state
    showColorsListWithDelay buttons pairs f constState txtTitle
    writeIORef ref (st+1)
    return ()

-- Функция, которая выводит в лейбл один элемент сгенерированной последовательности
showOneColor :: IORef Bool -> Window a -> [Button ()] -> [(Button(), GameColor)] -> IORef State -> TextCtrl ()-> IO()
showOneColor refFlag f buttons pairs colors label = do
    currState <- readIORef colors
    if currState == [] then (do enableButs buttons; return()) else showOneColor_ refFlag f buttons pairs colors label
  
showOneColor_ :: IORef Bool -> Window a -> [Button ()] -> [(Button(), GameColor)] -> IORef State -> TextCtrl ()-> IO()
showOneColor_ refFlag f buttons pairs colors label = do
  currState <- readIORef colors
  fl <- readIORef refFlag
  let c = head currState
  let str = colorToString c
  let ourButton = findButtonOnColor c pairs
  if fl == True then setColor refFlag ourButton c label else offColor refFlag ourButton label
  if (currState == []) then return() else (if fl == False then (writeIORef colors (tail currState)) else (writeIORef colors currState))
  
--функция для нахождения кнопки по цвету
findButtonOnColor :: GameColor -> [(Button(), GameColor)] -> Button()
findButtonOnColor color list = head $ foldl (\acc x -> if (snd x == color) then fst x : acc else acc) [] list

--Красим кнопку
setColor :: IORef Bool -> Button() -> GameColor -> TextCtrl () -> IO()
setColor refFlag but color label
  | color == Yellow = do
              set but [bgcolor  := yellow ]
              set label [ text := "Yellow" ]
              writeIORef refFlag False
  | color == Blue = do 
              set but [bgcolor := blue]
              set label [ text := "Blue" ]
              writeIORef refFlag False
  | color == Red = do 
              set but [bgcolor := red]
              set label [ text := "Red" ]
              writeIORef refFlag False
  | color == Green = do 
              set but [bgcolor := green]
              set label [ text := "Green" ]
              writeIORef refFlag False
  | otherwise = do
              set but [bgcolor := black]

--Убираем цвет у кнокпи
offColor :: IORef Bool -> Button() -> TextCtrl () -> IO()
offColor refFlag but label = do
              set but [bgcolor := white ]
              set label [ text := " " ]
              writeIORef refFlag True

-- Функция, которая показывает пользователю сгенерированную последовательность поэлементно с задержкой по таймеру
showColorsListWithDelay :: [Button ()] -> [(Button(), GameColor)] -> Window a -> IORef State -> TextCtrl () -> IO()
showColorsListWithDelay buttons pairs f generatedList label = do
  --копируем generated list
    stateGenList <- readIORef generatedList
    generatedListCopy <- newIORef (stateGenList)
    flag <- newIORef True
    t <- timer f [interval := 800, on command := showOneColor flag f buttons pairs generatedListCopy label]
    set label [ text := ""]
    return () 

-- Обнуляет пользовательский список перед генерацией нового игрового уровня и запускает новый уровень
nullUsersList :: [Button()] -> [(Button(), GameColor)] -> Window a -> IORef State -> IORef Int -> TextCtrl () -> IORef State -> TextCtrl() -> IO()
nullUsersList buttons pairs f userList n textField st1 levelInfo = do
    nn <- readIORef n
    set levelInfo [ text := "Вы прошли уровень " ++ show (nn-1) ++ " Ура!"]
    let tempList = []
    writeIORef userList tempList
    actionGUI buttons pairs f n textField st1
    return()

--Функция, вызываемая по нажатию на кнопки
actionUserButtons :: [Button ()] -> [(Button(), GameColor)] -> TextCtrl () -> IORef Int -> Window a -> IORef State -> IORef State -> GameColor -> TextCtrl () -> IO()
actionUserButtons buttons pairs textField n w refUser st1 butColor levelInfo = do 
    st <- readIORef refUser
    constState <- readIORef st1
    let modifiedUserList = st ++ [butColor]
    writeIORef refUser modifiedUserList
    let partOfGenList = getNfromList (length modifiedUserList) constState  
    let equal = compareStates modifiedUserList partOfGenList  -- Сравнивает текущий подсписок с соотв. по длине подскиском программы лексико-графически
    if (equal == False) then (gameOver w refUser n st1 textField levelInfo)  else  -- Если подпоследовательность не равна соответствующей подпоследовательности списка программы, 
                                                --то была допущена ошибка, программа завершается
                if (length modifiedUserList == length constState) then (do disableButs buttons; nullUsersList buttons pairs w refUser n textField st1 levelInfo ; return()) else return()

--Делает кнопки недоступными для пользователя
disableButs :: [Button ()] -> IO ()
disableButs buttons = do 
  set (getIfromList buttons 0) [enabled := False]
  set (getIfromList buttons 1) [enabled := False]
  set (getIfromList buttons 2) [enabled := False]
  set (getIfromList buttons 3) [enabled := False]
  return()

--Делает кнопки доступными для пользователя
enableButs :: [Button ()] -> IO ()
enableButs buttons = do 
  set (getIfromList buttons 0) [enabled := True]
  set (getIfromList buttons 1) [enabled := True]
  set (getIfromList buttons 2) [enabled := True]
  set (getIfromList buttons 3) [enabled := True]
  return()

gui :: IO ()
gui =  do
  -- форма, на которой будут лежать все наши контролы
  f <- frame [ text := "Simon", picture := "../some/image.ico" ]
  --set f [ image := "/some/image.ico"]
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
  congr <- entry f [text := "Удачи в игре!" , enabled := False ]
      
    -- tsakButton, мигающие кнопочки
  taskb1 <- button f [ text := " ", bgcolor  := white, clientSize := sz 80 80, enabled := False  ]
  taskb2 <- button f [ text := " " , bgcolor  := white, clientSize := sz 80 80, enabled := False ]
  taskb3 <- button f [ text := " " , bgcolor  := white, clientSize := sz 80 80, enabled := False ]
  taskb4 <- button f [ text := " " , bgcolor  := white, clientSize := sz 80 80, enabled := False ]
  
  let listOfPairTaskButtons = (taskb1, Yellow):(taskb2, Green):(taskb3, Blue):(taskb4, Red):[]
  
  p <- panel f [clientSize := sz 320 70] 
  
  -- gameButton, кнопочки на которые непосредственно жмакает юзерь
  b1 <- button f [ text := " ", bgcolor  := yellow, clientSize := sz 100 100  ]
  b2 <- button f [ text := " " , bgcolor  := green, clientSize := sz 100 100 ]
  b3 <- button f [ text := " " , bgcolor  := blue, clientSize := sz 100 100]
  b4 <- button f [ text := " " , bgcolor  := red, clientSize := sz 100 100 ]

  --список, куда мы помещаем кнопки, на каторые жмакает юзер
  let listUserButtons = b1:b2:b3:b4:[]
  
  -- Кнопки с помошщью, выходом и восхвалением 
  q <- button f [ text := "quit" , on command := close f ]
  h <- button f [ text := "help" , on command := chelp f ]
  a <- button f [ text := "about", on command := about f ]


 -- t <- timer f [interval := 1000, on command := showOneColor refState txtTitle]
  set b1 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f refUserList refState Yellow congr ]
  set b2 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f refUserList refState Green congr]
  set b3 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f refUserList refState Blue congr ]
  set b4 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f refUserList refState Red congr ]
 
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
    
        -- Кнопки для мигания с заданием
      --Первый ряд
        hfloatCentre $ margin 5 $ row 5 [
          row 5 [ widget taskb1, widget taskb2]
    ], 
      
      hfloatCentre $ margin 5 $ row 5 [
       row 5 [ widget taskb3, widget taskb4]
      --,minsize (sz 150 40) $ widget  howManyTimer 
        ],  
    
        hfloatCentre $ margin 5 $ row 5 [ 
      row 5 [ widget p ]
     ] ,
  
        -- Непосредственно сами кнопки
        hfloatCentre $ margin 5 $ row 5 [
          row 5 [ widget b1, widget b2]
    ], 
      hfloatCentre $margin 5 $ row 5 [
       row 5 [ widget b3, widget b4]
      --,minsize (sz 150 40) $ widget  howManyTimer 
        ],
  {- нижний ряд -}
        hfloatCentre $  margin 1 $ row 1 [ widget q, widget h, widget a ] 
  --  defaultButton := q
     ]
   ] 

  return() 