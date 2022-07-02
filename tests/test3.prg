#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test3" ;
      MAIN

      @ 20, 20 LABEL Label1 VALUE "Label1"

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
