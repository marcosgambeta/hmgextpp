/*
 * HMGEXT++/GDI+ test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "minigui.ch"

PROCEDURE Main()

   LOCAL pImage

   waGdiplusStartup()

   waGdipLoadImageFromFile("harbour.gif", @pImage)

   DEFINE WINDOW MainWindow ;
      AT 100, 100 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE "Test4" ;
      MAIN ;
      ON PAINT {||
         LOCAL pGraphics
         waGdipCreateFromHWND(waNToP(MainWindow.Handle), @pGraphics)
         waGdipDrawImageRectI(pGraphics, pImage, 0, 0, MainWindow.Width, MainWindow.Height)
         waGdipDeleteGraphics(pGraphics)
      }

   END WINDOW

   ACTIVATE WINDOW MainWindow

   waGdipDisposeImage(pImage)

   waGdiplusShutdown()

RETURN
