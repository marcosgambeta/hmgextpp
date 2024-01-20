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
      TITLE "Test2" ;
      MAIN ;
      ON PAINT {||
         LOCAL pGraphics
         LOCAL pImage
         LOCAL nWidth
         LOCAL nHeight
         waGdipCreateFromHWND(NTOP(MainWindow.handle), @pGraphics)
         waGdipLoadImageFromFile("harbour.gif", @pImage)
         waGdipGetImageDimension(pImage, @nWidth, @nHeight)
         waGdipDrawImageI(pGraphics, pImage, (MainWindow.Width - nWidth) / 2, (MainWindow.Height - nHeight) / 2)
         waGdipDisposeImage(pImage)
         waGdipDeleteGraphics(pGraphics)
     }

   END WINDOW

   ACTIVATE WINDOW MainWindow

   waGdiplusShutdown()

RETURN
