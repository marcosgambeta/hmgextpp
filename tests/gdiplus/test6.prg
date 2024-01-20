/*
 * HMGEXT++/GDI+ test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "minigui.ch"

PROCEDURE Main()

   waGdiplusStartup()

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test6" ;
      MAIN ;
      ON SIZE waInvalidateRgn(NTOP(MainWindow.Handle), NIL, .T.) ;
      ON MAXIMIZE waInvalidateRgn(NTOP(MainWindow.Handle), NIL, .T.) ;
      ON RESTORE waInvalidateRgn(NTOP(MainWindow.Handle), NIL, .T.) ;
      ON PAINT {||
         LOCAL oPS
         LOCAL pDC
         LOCAL pGraphics
         LOCAL pPen
         LOCAL pBrush
         LOCAL pWND
         LOCAL oRect
         LOCAL nWidth
         LOCAL nHeight

         pWND := NTOP(MainWindow.Handle)

         oRect := wasRECT():new()
         waGetWindowRect(pWND, oRect)
         nWidth := oRect:right - oRect:left
         nHeight := oRect:bottom - oRect:top

         oPS := wasPAINTSTRUCT():new()
         pDC := waBeginPaint(pWND, oPS)
         waGdipCreateFromHDC(pDC, @pGraphics)
         waGdipCreateLineBrushI(waGpPoint():new(0, 0), waGpPoint():new(0, nHeight), 0xFFFF0000, 0xFF0000FF, NIL, @pBrush)
         waGdipFillRectangleI(pGraphics, pBrush, 0, 0, nWidth, nHeight)
         waGdipDeleteBrush(pBrush)
         waGdipDeleteGraphics(pGraphics)
         waEndPaint(pWND, oPS)
      }

   END WINDOW

   ACTIVATE WINDOW MainWindow

   waGdiplusShutdown()

RETURN
