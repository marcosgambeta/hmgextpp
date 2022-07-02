#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test5" ;
      MAIN

      @ 20, 20 CHECKBOX CheckBox1 CAPTION "CheckBox1"

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
