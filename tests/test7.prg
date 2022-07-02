#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test7" ;
      MAIN

      @ 20, 20 LISTBOX ListBox1 ITEMS { "Item 1", "Item 2", "Item 3" }

   END WINDOW

   ACTIVATE WINDOW MainWindow

RETURN
