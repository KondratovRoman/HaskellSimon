module Main where
import Graphics.UI.WX

bugtext = unlines [ "Former bug: these buttons should react when clicked"
                  , "but the boxed one does not"
                  , ""
                  , "Buggy   in: MacOS X [now fixed!]"
                  , "Working in: Linux and Windows"
                  ]


main :: IO ()
main = start gui

gui :: IO ()
gui
   =  do
         f <- frame [ text := "program" ]
         b1 <- button f [ text := "yellow", clientSize := sz 500 500]
         b2 <- button f [ text := "green" ]
         b3 <- button f [ text := "blue" ]
         b4 <- button f [ text := "red" ]
         q <- button f [ text := "quit" , on command := close f ]
         h <- button f [ text := "help" , on command := chelp f ]
         a <- button f [ text := "about", on command := about f ]
         p <- panel  f [ clientSize := sz 1280 680 ]
         set b1 [ on command := set b1 [ text := "thanks!" ] ]
         set b2 [ on command := set b2 [ text := "thanks!" ] ]
         set b3 [ on command := set b3 [ text := "thanks!" ] ]
         set b4 [ on command := set b4 [ text := "thanks!" ] ]

         set f [ layout := column 10
		           [ fill $ widget p
                           , hfloatCentre $ margin 5 $ row 5 [widget b1, widget b2, widget b3,widget b4],
                            hfloatCentre $ margin 5 $ row 5 [  widget q, widget h, widget a]
                           ]
             , defaultButton := q
             ]   
 	 return ()
                  {-(boxed "" $ column 2 [ (boxed "hey" $ widget b2)
                                                                   , widget b3 ]) ]-}
               


about :: Window a -> IO ()
about w
  = infoDialog w "About Simon" "Simon\n\nby sfedu\n\n Simon was written using wxHaskell"

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Simon Help"
  (  "  Kill me,please \n\n"
  )
