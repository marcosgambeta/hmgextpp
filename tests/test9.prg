#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test9" ;
      MAIN

      @ 20, 20 BUTTON Button1 CAPTION "Button1" ACTION MsgInfo( "Button1 clicked" )

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
