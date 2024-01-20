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
      TITLE "Test3" ;
      MAIN ;
      ON PAINT {||
         LOCAL pGraphics
         LOCAL pImage
         waGdipCreateFromHWND(NTOP(MainWindow.Handle), @pGraphics)
         waGdipLoadImageFromFile("harbour.gif", @pImage)
         waGdipDrawImageRectI(pGraphics, pImage, 0, 0, MainWindow.Width, MainWindow.Height)
         waGdipDisposeImage(pImage)
         waGdipDeleteGraphics(pGraphics)
      }

   END WINDOW

   ACTIVATE WINDOW MainWindow

   waGdiplusShutdown()

RETURN
