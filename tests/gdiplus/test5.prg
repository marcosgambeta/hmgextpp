/*
 * HMGEXT++/GDI+ test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "minigui.ch"

PROCEDURE Main()

   LOCAL a := {}

   waGdiplusStartup()

   aadd(a, waGpPoint():new(0 + 20, 0 + 20))
   aadd(a, waGpPoint():new(200 + 20, 0 + 20))
   aadd(a, waGpPoint():new(200 + 20, 200 + 20))
   aadd(a, waGpPoint():new(0 + 20, 200 + 20))
   aadd(a, waGpPoint():new(0 + 20, 0 + 20))

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test5" ;
      MAIN ;
      ON PAINT {||
         LOCAL pGraphics
         LOCAL pPen
         LOCAL pBrush
         waGdipCreateFromHWND(NTOP(MainWindow.Handle), @pGraphics)
         waGdipCreatePen1(0xFF00FFFF, 5, 2 /* pixel */, @pPen)
         waGdipCreateSolidFill(0xFFFF8C00, @pBrush)
         waGdipDrawLinesI(pGraphics, pPen, a, len(a))
         waGdipFillRectangleI(pGraphics, pBrush, 0 + 20 + 10, 0 + 20 + 10, 200 + 20 - 40, 200 + 20 - 40)
         waGdipDeleteBrush(pBrush)
         waGdipDeletePen(pPen)
         waGdipDeleteGraphics(pGraphics)
      }

   END WINDOW

   ACTIVATE WINDOW MainWindow

   waGdiplusShutdown()

RETURN
