#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test6" ;
      MAIN

      @ 20, 20 RADIOGROUP RadioGroup1 OPTIONS { "Option 1", "Option 2", "Option 3" }

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
