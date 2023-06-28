#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test11" ;
      MAIN

      DEFINE TOOLBAR ToolBar BUTTONSIZE 48,48 FLAT BORDER

         BUTTON Button_1 ;
         CAPTION "Button &1" ;
         PICTURE "button.bmp" ;
         ACTION MsgInfo("Button 1 clicked") ;
         TOOLTIP "Tooltip Button 1"

         BUTTON Button_2 ;
         CAPTION "Button &2" ;
         PICTURE "button.bmp" ;
         ACTION MsgInfo("Button 2 clicked") ;
         TOOLTIP "Tooltip Button 2"

         BUTTON Button_3 ;
         CAPTION "Button &3" ;
         PICTURE "button.bmp" ;
         ACTION MsgInfo("Button 3 clicked") ;
         TOOLTIP "Tooltip Button 3"

         BUTTON Button_4 ;
         CAPTION "Button &4" ;
         PICTURE "button.bmp" ;
         ACTION MsgInfo("Button 4 clicked") ;
         TOOLTIP "Tooltip Button 4"

         BUTTON Button_5 ;
         CAPTION "Button &5" ;
         PICTURE "button.bmp" ;
         ACTION MsgInfo("Button 5 clicked") ;
         TOOLTIP "Tooltip Button 5"

      END TOOLBAR

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
