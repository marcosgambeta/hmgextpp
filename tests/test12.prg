#include "minigui.ch"

PROCEDURE Main()

   LOCAL aRows[1000][2]
   LOCAL n

   FOR n := 1 TO 1000
      aRows[n]  := {"NAME" + alltrim(str(n)), n}
   NEXT n

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Grid Test" ;
      MAIN

      @ 10,10 GRID Grid_1 ;
         WIDTH 600 ;
         HEIGHT 420 ;
         HEADERS {"Name", "Value"} ;
         WIDTHS {280, 280};
         ITEMS aRows ;
         VALUE 1 ;
         JUSTIFY {BROWSE_JTFY_LEFT, BROWSE_JTFY_RIGHT}

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN
