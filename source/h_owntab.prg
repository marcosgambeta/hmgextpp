//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2008 Walter Formigoni <walter.formigoni@uol.com.br>
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

#include "i_winuser.ch"
#include "minigui.ch"

#define DT_CENTER     1

//----------------------------------------------------------------------------//
FUNCTION OwnTabPaint(lParam)
//----------------------------------------------------------------------------//
   
   LOCAL hDC
   LOCAL hBrush
   LOCAL hOldFont
   LOCAL hImage
   LOCAL aBkColor
   LOCAL aForeColor
   LOCAL aInactiveColor
   LOCAL aBmp
   LOCAL aMetr
   LOCAL aBtnRc
   LOCAL oldTextColor
   LOCAL oldBkMode
   LOCAL nTextColor
   LOCAL bkColor
   LOCAL i
   LOCAL nItemId
   LOCAL x1
   LOCAL y1
   LOCAL x2
   LOCAL y2
   LOCAL xp1
   LOCAL yp1
   LOCAL xp2
   LOCAL yp2
   LOCAL lSelected
   LOCAL lBigFsize
   LOCAL lBigFsize2
   LOCAL lBigFsize3

   hDC := hmg_GETOWNBTNDC(lParam)

   i := AScan(_HMG_aControlHandles, hmg_GETOWNBTNHANDLE(lParam))

   IF Empty(hDC) .OR. i == 0
      RETURN 1
   ENDIF

   nItemId    := hmg_GETOWNBTNITEMID(lParam) + 1
   aBtnRc     := hmg_GETOWNBTNRECT(lParam)
   lSelected  := ( hb_bitand(hmg_GETOWNBTNSTATE(lParam), ODS_SELECTED) == ODS_SELECTED )
   lBigFsize  := ( _HMG_aControlFontSize[i] >= 12 )
   lBigFsize2 := ( _HMG_aControlFontSize[i] >= 18 )
   lBigFsize3 := ( _HMG_aControlFontSize[i] >= 24 )

   _HMG_aControlMiscData1[i][1] := aBtnRc[4] - aBtnRc[2]  // store a bookmark height

   hOldFont     := hmg_SelectObject(hDC, _HMG_aControlFontHandle[i])
   aMetr        := hmg_GetTextMetric(hDC)
   oldBkMode    := hmg_SetBkMode(hDC, TRANSPARENT)
   nTextColor   := waGetSysColor(COLOR_BTNTEXT)
   oldTextColor := hmg_SetTextColor(hDC, waGetRValue(nTextColor), waGetGValue(nTextColor), waGetBValue(nTextColor))

   IF hb_IsArray(_HMG_aControlMiscData2[i]) .AND. nItemId <= Len(_HMG_aControlMiscData2[i]) .AND. ;
      IsArrayRGB(_HMG_aControlMiscData2[i][nItemId])
      aBkColor := _HMG_aControlMiscData2[i][nItemId]
   ELSE
      aBkColor := _HMG_aControlBkColor[i]
   ENDIF

   bkColor := RGB(aBkColor[1], aBkColor[2], aBkColor[3])
   hmg_SetBkColor(hDC, bkColor)

   hBrush := hmg_CreateSolidBrush(aBkColor[1], aBkColor[2], aBkColor[3])
   hmg_FillRect(hDC, aBtnRc[1], aBtnRc[2], aBtnRc[3], aBtnRc[4], hBrush)
   hmg_DeleteObject(hBrush)

   x1 := aBtnRc[1]
   y1 := Round(aBtnRc[4] / 2, 0) - ( aMetr[1] - 10 )
   x2 := aBtnRc[3] - 2
   y2 := y1 + aMetr[1]

   IF _HMG_aControlMiscData1[i][2]  // ImageFlag

      nItemId := Min(nItemId, Len(_HMG_aControlPicture[i]))

      hImage := hmg_LoadBitmap(_HMG_aControlPicture[i][nItemId])
      IF Empty(hImage)
         hImage := hmg_LoadImage(_HMG_aControlPicture[i][nItemId], , , , , , bkColor)
      ENDIF

      aBmp := hmg_GetBitmapSize(hImage)

      xp1 := 4
      xp2 := aBmp[1]
      yp2 := aBmp[2]
      yp1 := Round(aBtnRc[4] / 2 - yp2 / 2, 0)
      x1  += 2 * xp1 + xp2

      IF _HMG_aControlMiscData1[i][4]  // Bottom Tab

         IF lSelected
            hmg_DrawGlyph(hDC, aBtnRc[1] + 2 * xp1, 2 * yp1 - iif(lBigFsize, 8, 5), xp2, 2 * yp2 - iif(lBigFsize, 8, 5), hImage, bkColor, .F., .F.)
         ELSE
            hmg_DrawGlyph(hDC, aBtnRc[1] + xp1, 2 * yp1 - iif(lBigFsize, 8, 5), xp2, 2 * yp2 - iif(lBigFsize, 8, 5), hImage, bkColor, .F., .F.)
         ENDIF

      ELSE

         IF lSelected
            hmg_DrawGlyph(hDC, aBtnRc[1] + 2 * xp1, yp1 - 2, xp2, yp2, hImage, bkColor, .F., .F.)
         ELSE
            hmg_DrawGlyph(hDC, aBtnRc[1] + xp1, yp1 + 2, xp2, yp2, hImage, bkColor, .F., .F.)
         ENDIF

      ENDIF

      hmg_DeleteObject(hImage)

   ENDIF

   IF lSelected

      IF _HMG_aControlMiscData1[i][5]  // HotTrack

         IF IsArrayRGB(aForeColor := _HMG_aControlMiscData1[i][6])
            hmg_SetTextColor(hDC, aForeColor[1], aForeColor[2], aForeColor[3])
         ELSEIF bkColor == waGetSysColor(COLOR_BTNFACE)
            hmg_SetTextColor(hDC, 0, 0, 128)
         ELSE
            hmg_SetTextColor(hDC, 255, 255, 255)
         ENDIF

      ENDIF

   ELSE

      IF IsArrayRGB(aInactiveColor := _HMG_aControlMiscData1[i][7])
         hmg_SetTextColor(hDC, aInactiveColor[1], aInactiveColor[2], aInactiveColor[3])
      ENDIF

   ENDIF

   IF _HMG_aControlMiscData1[i][4]  // Bottom Tab

      IF lSelected
         hmg_DrawText(hDC, _HMG_aControlCaption[i][nItemId], x1, 2 * y1 - iif(lBigFsize3, -12, iif(lBigFsize2, -3, iif(lBigFsize, 6, 12))), x2, 2 * y2 - iif(lBigFsize3, -12, iif(lBigFsize2, -3, iif(lBigFsize, 6, 12))), DT_CENTER)
      ELSE
         hmg_DrawText(hDC, _HMG_aControlCaption[i][nItemId], x1, 2 * y1 - iif(lBigFsize3, -18, iif(lBigFsize2, -8, iif(lBigFsize, 0, 8))), x2, 2 * y2 - iif(lBigFsize3, -18, iif(lBigFsize2, -8, iif(lBigFsize, 0, 8))), DT_CENTER)
      ENDIF

   ELSE

      IF lSelected
         hmg_DrawText(hDC, _HMG_aControlCaption[i][nItemId], x1, y1 - iif(lBigFsize3, -9, iif(lBigFsize2, -5, iif(lBigFsize, 0, 4))), x2, y2 - iif(lBigFsize3, -9, iif(lBigFsize2, -5, iif(lBigFsize, 0, 4))), DT_CENTER)
      ELSE
         hmg_DrawText(hDC, _HMG_aControlCaption[i][nItemId], x1, y1 + iif(lBigFsize3, 14, iif(lBigFsize2, 8, iif(lBigFsize, 4, 0))), x2, y2 + iif(lBigFsize3, 14, iif(lBigFsize2, 8, iif(lBigFsize, 4, 0))), DT_CENTER)
      ENDIF

   ENDIF

   hmg_SelectObject(hDC, hOldFont)
   hmg_SetBkMode(hDC, oldBkMode)
   hmg_SetTextColor(hDC, oldTextColor)

RETURN 0
