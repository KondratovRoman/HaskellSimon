module Main where
import Graphics.UI.WX
import SimonLogic

bugtext = unlines [ "Former bug: these buttons should react when clicked"
                  , "but the boxed one does not"
                  , ""
                  , "Buggy   in: MacOS X [now fixed!]"
                  , "Working in: Linux and Windows"
                  ]


about :: Window a -> IO ()
about w
  = infoDialog w "About Simon" "Simon \n\nby sfedu\n\n Simon was written using wxHaskell"

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Simon Help"
  (  "  Kill me,please \n\n"  )

main :: IO ()
main = start $ gui 1

--проверяет верно ли пользователь нажал кнопки и генерирует новый уровень
actionGUI :: State -> State -> Int -> IO()
actionGUI st1 st2 n = do
    let equal = compareStates st1 st2
    state <- generateLevel n
    let level = stringLevel state
    set txtTitle [ text := level]
    return ()


gui :: Int -> IO ()
gui n =  do
  state <- generateLevel n
  let level = stringLevel state
  -- форма, на которой будут лежать все наши контролы
  f <- frame [ text := "Simon" ]
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

  set b1 [ on command := set b1 [ text := "thanks!" ] ]
  set b2 [ on command := set b2 [ text := "thanks!" ] ]
  set b3 [ on command := set b3 [ text := "thanks!" ] ]
  set b4 [ on command := set b3 [ text := "thanks!" ] ]
 
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

  set a [ on command := actionGUI state state n]
  return() 
  



