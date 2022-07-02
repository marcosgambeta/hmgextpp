#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test2" ;
      MAIN

      DEFINE MAIN MENU
         POPUP "Menu"
            ITEM "Option 1" ACTION MsgInfo( "Option 1" )
            ITEM "Option 2" ACTION MsgInfo( "Option 2" )
         END POPUP
      END MENU

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
