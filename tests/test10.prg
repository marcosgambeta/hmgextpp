#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test10" ;
      MAIN

      @ 20, 20 BUTTON Button1 ;
               PICTURE "button.bmp" ;
               ACTION MsgInfo( "Button1 clicked" ) ;
               WIDTH 30 ;
               HEIGHT 30 ;
               TOOLTIP "Button1 tooltip"

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
