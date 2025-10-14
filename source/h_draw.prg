//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

// $BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this software; see the file COPYING. If not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
// visit the web site http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of Harbour Minigui.
//
// The exception is that, if you link the Harbour Minigui library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// Harbour-Minigui library code into it.
// $END_LICENSE$

// Parts of this project are based upon:
//
// "Harbour GUI framework for Win32"
// Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2022, https://harbour.github.io/
//
// "WHAT32"
// Copyright 2002 AJ Wos <andrwos@aust1.net>
//
// "HWGUI"
// Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

#include "minigui.ch"

//---------------------------------------------------------------------------//
FUNCTION drawtextout(window, row, col, string, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle, once)
//---------------------------------------------------------------------------//

   LOCAL FormHandle
   LOCAL FontHandle
   LOCAL torow
   LOCAL tocol
   LOCAL i

   IF hb_IsString(window)
      IF (i := GetFormIndex(window)) > 0
         FormHandle := _HMG_aFormHandles[i]
      ENDIF
   ELSE
      FormHandle := window
   ENDIF

   IF hmg_IsWindowHandle(FormHandle) .OR. ( hmg_GetObjectType(FormHandle) == OBJ_DC )
      IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
         GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout, @angle )
      ENDIF

      __defaultNIL(@fontname, _HMG_DefaultFontName)
      __defaultNIL(@fontsize, _HMG_DefaultFontSize)
      hb_default(@backcolor, {255, 255, 255})
      hb_default(@fontcolor, {0, 0, 0})
      hb_default(@angle, 0)
      hb_default(@once, .F.)

      torow := row + iif(transparent .OR. !Empty(angle), 0, fontsize + 4)
      tocol := col + ( Len(string) - 1 ) * fontsize
      hmg_textdraw(FormHandle, row, col, string, torow, tocol, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle)
      IF !once
         AAdd(_HMG_aFormGraphTasks[i], {||hmg_textdraw(FormHandle, row, col, string, torow, tocol, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle)})
      ENDIF
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawline(window, row, col, row1, col1, penrgb, penwidth)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)

      hmg_linedraw(FormHandle, row, col, row1, col1, penrgb, penwidth)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_linedraw(FormHandle, row, col, row1, col1, penrgb, penwidth)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawrect(window, row, col, row1, col1, penrgb, penwidth, fillrgb)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)
      IF !( fill := hb_IsArray(fillrgb) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      hmg_rectdraw(FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_rectdraw(FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawroundrect(window, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)
      IF !( fill := hb_IsArray(fillrgb) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      hmg_roundrectdraw(FormHandle, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb, fill)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_roundrectdraw(FormHandle, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb, fill)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawellipse(window, row, col, row1, col1, penrgb, penwidth, fillrgb)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)
      IF !( fill := hb_IsArray(fillrgb) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      hmg_ellipsedraw(FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_ellipsedraw(FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawarc(window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)

      hmg_arcdraw(FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_arcdraw(FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawpie(window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)
      IF !( fill := hb_IsArray(fillrgb) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      hmg_piedraw(FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb, fill)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_piedraw(FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb, fill)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawpolygon(window, apoints, penrgb, penwidth, fillrgb)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL i
   LOCAL fill
   LOCAL xarr := {}
   LOCAL yarr := {}

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)
      IF !( fill := hb_IsArray(fillrgb) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      AEval(apoints, {|x|AAdd(yarr, x[1]), AAdd(xarr, x[2])})
      hmg_polygondraw(FormHandle, xarr, yarr, penrgb, penwidth, fillrgb, fill)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_polygondraw(FormHandle, xarr, yarr, penrgb, penwidth, fillrgb, fill)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION drawpolybezier(window, apoints, penrgb, penwidth)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL xarr := {}
   LOCAL yarr := {}
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@penrgb, {0, 0, 0})
      hb_default(@penwidth, 1)

      AEval(apoints, {|x|AAdd(yarr, x[1]), AAdd(xarr, x[2])})
      hmg_polybezierdraw(FormHandle, xarr, yarr, penrgb, penwidth)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_polybezierdraw(FormHandle, xarr, yarr, penrgb, penwidth)})
   ENDIF

RETURN NIL

#define COLOR_BTNFACE        15
//---------------------------------------------------------------------------//
FUNCTION HMG_DrawIcon(window, icon, row, col, w, h, rgb, transparent)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL backcolor
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@w, 0)
      hb_default(@h, 0)
      hb_default(@transparent, .F.)
      IF transparent
         backcolor := _HMG_aFormBkColor[i]
         IF IsArrayRGB(backcolor)
            rgb := RGB(backcolor[1], backcolor[2], backcolor[3])
         ENDIF
      ELSE
         IF IsArrayRGB(rgb)
            rgb := RGB(rgb[1], rgb[2], rgb[3])
         ENDIF
      ENDIF
      hb_default(@rgb, waGetSysColor(COLOR_BTNFACE))

      IF hb_IsNumeric(icon)
         hmg_DrawIconEx(FormHandle, Col, Row, icon, w, h, rgb, .F.)
         AAdd(_HMG_aFormGraphTasks[i], {||hmg_DrawIconEx(FormHandle, Col, Row, icon, w, h, rgb, .F.)})
      ELSEIF hb_IsString(icon)
         hmg_DrawIconEx(FormHandle, Col, Row, hmg_LoadIconByName(icon, w, h), w, h, rgb, .T.)
         AAdd(_HMG_aFormGraphTasks[i], {||hmg_DrawIconEx(FormHandle, Col, Row, hmg_LoadIconByName(icon, w, h), w, h, rgb, .T.)})
      ENDIF
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION HMG_DrawSysIcon(window, cIconDll, icon, row, col, w, h, rgb, transparent)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL backcolor
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hb_default(@w, 0)
      hb_default(@h, 0)
      hb_default(@transparent, .F.)
      IF transparent
         backcolor := _HMG_aFormBkColor[i]
         IF IsArrayRGB(backcolor)
            rgb := RGB(backcolor[1], backcolor[2], backcolor[3])
         ENDIF
      ELSE
         IF IsArrayRGB(rgb)
            rgb := RGB(rgb[1], rgb[2], rgb[3])
         ENDIF
      ENDIF
      hb_default(@rgb, waGetSysColor(COLOR_BTNFACE))
      hb_default(@cIconDll, System.SystemFolder + hb_ps() + "imageres.dll")

      IF hb_IsNumeric(icon)
         AAdd(_HMG_aFormGraphTasks[i], {||hmg_DrawIconEx(FormHandle, Col, Row, hmg_ExtractIcon(cIconDll, icon), w, h, rgb, .T.)})
      ENDIF
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION EraseWindow(window)
//---------------------------------------------------------------------------//
   
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      IF !_HMG_aFormDeleted[i]
         IF hb_IsArray(_HMG_aFormGraphTasks[i])
            ASize(_HMG_aFormGraphTasks[i], 0)
            hmg_RedrawWindow(_HMG_aFormHandles[i])
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION DrawWindowBoxIn(window, row, col, rowr, colr)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL hDC
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hDC := hmg_GetDC(FormHandle)
      hmg_WndBoxIn(hDC, row, col, rowr, colr)
      hmg_ReleaseDC(FormHandle, hDC)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_WndBoxIn((hDC := hmg_GetDC(FormHandle)), row, col, rowr, colr), hmg_ReleaseDC(FormHandle, hDC)})
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION DrawWindowBoxRaised(window, row, col, rowr, colr)
//---------------------------------------------------------------------------//
   
   LOCAL FormHandle
   LOCAL hDC
   LOCAL i

   IF (i := GetFormIndex(Window)) > 0
      FormHandle := _HMG_aFormHandles[i]
      hDC := hmg_GetDC(FormHandle)
      hmg_WndBoxRaised(hDC, row, col, rowr, colr)
      hmg_ReleaseDC(FormHandle, hDC)
      AAdd(_HMG_aFormGraphTasks[i], {||hmg_WndBoxRaised((hDC := hmg_GetDC(FormHandle)), row, col, rowr, colr), hmg_ReleaseDC(FormHandle, hDC)})
   ENDIF

RETURN NIL
