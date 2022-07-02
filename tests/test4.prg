#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test4" ;
      MAIN

      @  20, 20 TEXTBOX Text1
      @  60, 20 TEXTBOX Text2 NUMERIC
      @ 100, 20 TEXTBOX Text3 NUMERIC INPUTMASK "9999.99"

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
