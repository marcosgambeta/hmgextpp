//
// BOS TAURUS - Graphic Library for HMG
//
// Copyright 2012-2016 by Dr. Claudio Soto (from Uruguay).
// mail: <srvet@adinet.com.uy>
// blog: http://srvet.blogspot.com
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
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301, USA
// (or visit their web site at http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of BOS TAURUS.
//
// The exception is that, if you link the BOS TAURUS library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// BOS TAURUS library code into it.
// $END_LICENSE$

//******************************************************************************
// ARCHIVO:  h_BosTaurus.prg
// LENGUAJE: HMG
// FECHA:    Setiembre 2012
// AUTOR:    Dr. CLAUDIO SOTO
// PAIS:     URUGUAY
// E-MAIL:   srvet@adinet.com.uy
// BLOG:     http://srvet.blogspot.com
//******************************************************************************

#include "hmg.ch"
#include "i_winuser.ch"

// **********************************************************************************************************************************
// INTERNAL Functions PRG
// **********************************************************************************************************************************

FUNCTION bt_WinHandle(Win)

   LOCAL hWnd := IIF(hb_IsNumeric(Win), Win, GetFormHandle(Win))

RETURN hWnd

FUNCTION bt_FillRectIsNIL(Row, Col, Width, Height, Row_value, Col_value, Width_value, Height_value)

   Row := IIF(Row == NIL, Row_value, Row)
   Col := IIF(Col == NIL, Col_value, Col)
   Width := IIF(Width == NIL, Width_value, Width)
   Height := IIF(Height == NIL, Height_value, Height)

RETURN NIL

FUNCTION bt_AdjustWidthHeightRect(Row, Col, Width, Height, Max_Width, Max_Height)

   Width := IIF((Col + Width > Max_Width), (Max_Width - Col), Width)
   Height := IIF((Row + Height > Max_Height), (Max_Height - Row), Height)

RETURN NIL

FUNCTION bt_ListCalledFunctions(nActivation)

   LOCAL cMsg := ""

   nActivation := IIF(!hb_isNumeric(nActivation), 1, nActivation)
   DO WHILE !(ProcName(nActivation) == "")
      cMsg := cMsg + "Called from:" + ProcName(nActivation) + "(" + LTrim(Str(ProcLine(nActivation))) + ")" + CRLF
      nActivation++
   ENDDO

RETURN cMsg

// **********************************************************************************************************************************
// * BT INFO
// **********************************************************************************************************************************

FUNCTION BT_InfoName()
RETURN AllTrim(_BT_INFO_NAME_)

FUNCTION BT_InfoVersion()
RETURN AllTrim(Str(_BT_INFO_MAJOR_VERSION_)) + "." + AllTrim(Str(_BT_INFO_MINOR_VERSION_)) + "." + AllTrim(Str(_BT_INFO_PATCHLEVEL_))

FUNCTION BT_InfoAuthor()
RETURN AllTrim(_BT_INFO_AUTHOR_)

// **********************************************************************************************************************************
// * Handle DC
// **********************************************************************************************************************************

FUNCTION BT_CreateDC(Win_or_hBitmap, Type, BTstruct)

   LOCAL Handle
   LOCAL hDc

   DO CASE
   CASE Type == BT_HDC_DESKTOP
      Handle := 0
   CASE Type == BT_HDC_BITMAP
      Handle := Win_or_hBitmap
   OTHERWISE
      Handle := bt_WinHandle(Win_or_hBitmap)
   END CASE
   BTstruct := BT_DC_CREATE(Type, Handle)
   hDC := BTstruct[3]

RETURN hDC

FUNCTION BT_DeleteDC(BTstruct)

   LOCAL lRet

   IF !hb_isArray(BTstruct)
      MsgBox("Error in call to " + ProcName() + ": The second parameter is not an array" + CRLF + bt_ListCalledFunctions(2), "BT Fatal Error")
      RELEASE WINDOW ALL
   ELSEIF Len(BTstruct) != 50
      MsgBox("Error in call to " + ProcName() + ": The second parameter is an corrupted array " + CRLF + bt_ListCalledFunctions(2), "BT Fatal Error")
      RELEASE WINDOW ALL
   ENDIF
   lRet := BT_DC_DELETE(BTstruct)

RETURN lRet

// **********************************************************************************************************************************
// DRAW Functions
// **********************************************************************************************************************************

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawGetPixel(hDC, Row, Col)

   LOCAL aRGBcolor

   aRGBcolor := BT_DRAW_HDC_PIXEL(hDC, Col, Row, BT_HDC_GETPIXEL, 0)

RETURN aRGBcolor

FUNCTION BT_DrawSetPixel(hDC, Row, Col, aRGBcolor)

   LOCAL aRGBcolor_Old

   aRGBcolor_Old := BT_DRAW_HDC_PIXEL(hDC, Col, Row, BT_HDC_SETPIXEL, ArrayRGB_TO_COLORREF(aRGBcolor))

RETURN aRGBcolor_Old

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawBitmap(hDC, Row, Col, Width, Height, Mode_Stretch, hBitmap)

   LOCAL Width2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_WIDTH)
   LOCAL Height2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_HEIGHT)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Width2, Height2)
   BT_DRAW_HDC_BITMAP(hDC, Col, Row, Width, Height, hBitmap, 0, 0, Width2, Height2, Mode_Stretch, BT_BITMAP_OPAQUE, 0)

RETURN NIL

FUNCTION BT_DrawBitmapTransparent(hDC, Row, Col, Width, Height, Mode_Stretch, hBitmap, aRGBcolor_transp)

   LOCAL ColorRef_Transp := IIF(aRGBcolor_transp == NIL, BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_GETCOLORPIXEL, 0, 0), ArrayRGB_TO_COLORREF(aRGBcolor_transp))
   LOCAL Width2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_WIDTH)
   LOCAL Height2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_HEIGHT)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Width2, Height2)
   BT_DRAW_HDC_BITMAP(hDC, Col, Row, Width, Height, hBitmap, 0, 0, Width2, Height2, Mode_Stretch, BT_BITMAP_TRANSPARENT, ColorRef_Transp)

RETURN NIL

FUNCTION BT_DrawBitmapAlphaBlend(hDC, Row, Col, Width, Height, Alpha, Mode_Stretch, hBitmap)

   LOCAL Width2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_WIDTH)
   LOCAL Height2 := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_HEIGHT)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Width2, Height2)
   BT_DRAW_HDC_BITMAPALPHABLEND(hDC, Col, Row, Width, Height, hBitmap, 0, 0, Width2, Height2, Alpha, Mode_Stretch)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawDCtoDC(hDC1, Row1, Col1, Width1, Height1, Mode_Stretch, hDC2, Row2, Col2, Width2, Height2)

   BT_DRAW_HDC_TO_HDC(hDC1, Col1, Row1, Width1, Height1, hDC2, Col2, Row2, Width2, Height2, Mode_Stretch, BT_HDC_OPAQUE, 0)

RETURN NIL

FUNCTION BT_DrawDCtoDCTransparent(hDC1, Row1, Col1, Width1, Height1, Mode_Stretch, hDC2, Row2, Col2, Width2, Height2, aRGBcolor_transp)

   LOCAL ColorRef_Transp := IIF(aRGBcolor_transp == NIL, ArrayRGB_TO_COLORREF(BT_DrawGetPixel(hDC2, 0, 0)), ArrayRGB_TO_COLORREF(aRGBcolor_transp))

   BT_DRAW_HDC_TO_HDC(hDC1, Col1, Row1, Width1, Height1, hDC2, Col2, Row2, Width2, Height2, Mode_Stretch, BT_HDC_TRANSPARENT, ColorRef_Transp)

RETURN NIL

FUNCTION BT_DrawDCtoDCAlphaBlend(hDC1, Row1, Col1, Width1, Height1, Alpha, Mode_Stretch, hDC2, Row2, Col2, Width2, Height2)

   BT_DRAW_HDC_TO_HDC_ALPHABLEND(hDC1, Col1, Row1, Width1, Height1, hDC2, Col2, Row2, Width2, Height2, Alpha, Mode_Stretch)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawGradientFillHorizontal(hDC, Row, Col, Width, Height, aColorRGBstart, aColorRGBend)

   aColorRGBstart := IIF(aColorRGBstart == NIL, BLACK, aColorRGBstart)
   aColorRGBend := IIF(aColorRGBend == NIL, WHITE, aColorRGBend)
   BT_DRAW_HDC_GRADIENTFILL(hDC, Col, Row, Width, Height, ArrayRGB_TO_COLORREF(aColorRGBstart), ArrayRGB_TO_COLORREF(aColorRGBend), BT_GRADIENTFILL_HORIZONTAL)

RETURN NIL

FUNCTION BT_DrawGradientFillVertical(hDC, Row, Col, Width, Height, aColorRGBstart, aColorRGBend)

   aColorRGBstart := IIF(aColorRGBstart == NIL, WHITE, aColorRGBstart)
   aColorRGBend := IIF(aColorRGBend == NIL, BLACK, aColorRGBend)
   BT_DRAW_HDC_GRADIENTFILL(hDC, Col, Row, Width, Height, ArrayRGB_TO_COLORREF(aColorRGBstart), ArrayRGB_TO_COLORREF(aColorRGBend), BT_GRADIENTFILL_VERTICAL)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawText(hDC, Row, Col, cText, cFontName, nFontSize, aFontColor, aBackColor, nTypeText, nAlingText, nOrientation)

   aFontColor := IIF(aFontColor == NIL, BLACK, aFontColor)
   aBackColor := IIF(aBackColor == NIL, WHITE, aBackColor)
   nTypeText := IIF(nTypeText == NIL, BT_TEXT_OPAQUE, nTypeText)
   nAlingText := IIF(nAlingText == NIL, (BT_TEXT_LEFT + BT_TEXT_TOP), nAlingText)
   nOrientation := IIF(nOrientation == NIL, BT_TEXT_NORMAL_ORIENTATION, nOrientation)
   BT_DRAW_HDC_TEXTOUT(hDC, Col, Row, cText, cFontName, nFontSize, ArrayRGB_TO_COLORREF(aFontColor), ArrayRGB_TO_COLORREF(aBackColor), nTypeText, nAlingText, nOrientation)

RETURN NIL

FUNCTION BT_DrawTextEx(hDC, Row, Col, Width, Height, cText, cFontName, nFontSize, aFontColor, aBackColor, nTypeText, nAlingText, nOrientation)

   aFontColor := IIF(aFontColor == NIL, BLACK, aFontColor)
   aBackColor := IIF(aBackColor == NIL, WHITE, aBackColor)
   nTypeText := IIF(nTypeText == NIL, BT_TEXT_OPAQUE, nTypeText)
   nAlingText := IIF(nAlingText == NIL, (BT_TEXT_LEFT + BT_TEXT_TOP + BT_TEXT_WORDBREAK + BT_TEXT_NOCLIP), nAlingText)
   nOrientation := IIF(nOrientation == NIL, BT_TEXT_NORMAL_ORIENTATION, nOrientation)
   BT_DRAW_HDC_DRAWTEXT(hDC, Col, Row, Width, Height, cText, cFontName, nFontSize, ArrayRGB_TO_COLORREF(aFontColor), ArrayRGB_TO_COLORREF(aBackColor), nTypeText, nAlingText, nOrientation)

RETURN NIL

FUNCTION BT_DrawTextSize(hDC, cText, cFontName, nFontSize, nTypeText)

   LOCAL aSize := BT_DRAW_HDC_TEXTSIZE(hDC, cText, cFontName, nFontSize, nTypeText)

RETURN aSize

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawPolyLine(hDC, aPointY, aPointX, aColorRGBLine, nWidthLine)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_POLY(hDC, aPointX, aPointY, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, 0, BT_DRAW_POLYLINE)

RETURN NIL

FUNCTION BT_DrawPolygon(hDC, aPointY, aPointX, aColorRGBLine, nWidthLine, aColorRGBFill)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_POLY(hDC, aPointX, aPointY, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, ArrayRGB_TO_COLORREF(aColorRGBFill), BT_DRAW_POLYGON)

RETURN NIL

FUNCTION BT_DrawPolyBezier(hDC, aPointY, aPointX, aColorRGBLine, nWidthLine)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_POLY(hDC, aPointX, aPointY, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, 0, BT_DRAW_POLYBEZIER)

RETURN NIL

FUNCTION BT_DrawArc(hDC, Row1, Col1, Row2, Col2, RowStartArc, ColStartArc, RowEndArc, ColEndArc, aColorRGBLine, nWidthLine)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_ARCX(hDC, Col1, Row1, Col2, Row2, ColStartArc, RowStartArc, ColEndArc, RowEndArc, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, 0, BT_DRAW_ARC)

RETURN NIL

FUNCTION BT_DrawChord(hDC, Row1, Col1, Row2, Col2, RowStartArc, ColStartArc, RowEndArc, ColEndArc, aColorRGBLine, nWidthLine, aColorRGBFill)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_ARCX(hDC, Col1, Row1, Col2, Row2, ColStartArc, RowStartArc, ColEndArc, RowEndArc, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, ArrayRGB_TO_COLORREF(aColorRGBFill), BT_DRAW_CHORD)

RETURN NIL

FUNCTION BT_DrawPie(hDC, Row1, Col1, Row2, Col2, RowStartArc, ColStartArc, RowEndArc, ColEndArc, aColorRGBLine, nWidthLine, aColorRGBFill)

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_ARCX(hDC, Col1, Row1, Col2, Row2, ColStartArc, RowStartArc, ColEndArc, RowEndArc, ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, ArrayRGB_TO_COLORREF(aColorRGBFill), BT_DRAW_PIE)

RETURN NIL

FUNCTION BT_DrawLine(hDC, Row1, Col1, Row2, Col2, aColorRGBLine, nWidthLine)

   LOCAL aPointX := { Col1, Col2 }
   LOCAL aPointY := { Row1, Row2 }

   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DrawPolyLine(hDC, aPointY, aPointX, aColorRGBLine, nWidthLine)

RETURN NIL

FUNCTION BT_DrawRectangle(hDC, Row, Col, Width, Height, aColorRGBLine, nWidthLine)

   LOCAL aPointX := Array(5)
   LOCAL aPointY := Array(5)

   aPointX[1] := Col ;          aPointY[1] := Row
   aPointX[2] := Col + Width ;  aPointY[2] := Row
   aPointX[3] := Col + Width ;  aPointY[3] := Row + Height
   aPointX[4] := Col ;          aPointY[4] := Row + Height
   aPointX[5] := Col ;          aPointY[5] := Row
   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DrawPolyLine(hDC, aPointY, aPointX, aColorRGBLine, nWidthLine)

RETURN NIL

FUNCTION BT_DrawEllipse(hDC, Row1, Col1, Width, Height, aColorRGBLine, nWidthLine)

   LOCAL ColStartArc
   LOCAL ColEndArc
   LOCAL RowStartArc
   LOCAL RowEndArc
   LOCAL Col2
   LOCAL Row2

   Col2 := Col1 + Width
   Row2 := Row1 + Height
   ColStartArc := ColEndArc := Col1
   RowStartArc := RowEndArc := Row1
   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DrawArc(hDC, Row1, Col1, Row2, Col2, RowStartArc, ColStartArc, RowEndArc, ColEndArc, aColorRGBLine, nWidthLine)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_DrawFillRectangle(hDC, Row, Col, Width, Height, aColorRGBFill, aColorRGBLine, nWidthLine)

   aColorRGBLine := IIF(nWidthLine == NIL, aColorRGBFill, aColorRGBLine)
   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_FILLEDOBJECT(hDC, Col, Row, Width, Height, ArrayRGB_TO_COLORREF(aColorRGBFill), ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, BT_FILLRECTANGLE, 0, 0)

RETURN NIL

FUNCTION BT_DrawFillEllipse(hDC, Row, Col, Width, Height, aColorRGBFill, aColorRGBLine, nWidthLine)

   aColorRGBLine := IIF(nWidthLine == NIL, aColorRGBFill, aColorRGBLine)
   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_FILLEDOBJECT(hDC, Col, Row, Width, Height, ArrayRGB_TO_COLORREF(aColorRGBFill), ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, BT_FILLELLIPSE, 0, 0)

RETURN NIL

FUNCTION BT_DrawFillRoundRect(hDC, Row, Col, Width, Height, RoundWidth, RoundHeight, aColorRGBFill, aColorRGBLine, nWidthLine)

   aColorRGBLine := IIF(nWidthLine == NIL, aColorRGBFill, aColorRGBLine)
   nWidthLine := IIF(nWidthLine == NIL, 1, nWidthLine)
   BT_DRAW_HDC_FILLEDOBJECT(hDC, Col, Row, Width, Height, ArrayRGB_TO_COLORREF(aColorRGBFill), ArrayRGB_TO_COLORREF(aColorRGBLine), nWidthLine, BT_FILLROUNDRECT, RoundWidth, RoundHeight)

RETURN NIL

FUNCTION BT_DrawFillFlood(hDC, Row, Col, aColorRGBFill)

   BT_DRAW_HDC_FILLEDOBJECT(hDC, Col, Row, NIL, NIL, ArrayRGB_TO_COLORREF(aColorRGBFill), NIL, NIL, BT_FILLFLOOD, NIL, NIL)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_GetDesktopHandle()
RETURN BT_SCR_GETDESKTOPHANDLE()

FUNCTION BT_DesktopWidth()

   LOCAL Width := BT_SCR_GETINFO(0, BT_SCR_DESKTOP, BT_SCR_INFO_WIDTH)

RETURN Width

FUNCTION BT_DesktopHeight()

   LOCAL Height := BT_SCR_GETINFO(0, BT_SCR_DESKTOP, BT_SCR_INFO_HEIGHT)

RETURN Height

FUNCTION BT_WindowWidth(Win)

   LOCAL Width := BT_SCR_GETINFO(bt_WinHandle(Win), BT_SCR_WINDOW, BT_SCR_INFO_WIDTH)

RETURN Width

FUNCTION BT_WindowHeight(Win)

   LOCAL Height := BT_SCR_GETINFO(bt_WinHandle(Win), BT_SCR_WINDOW, BT_SCR_INFO_HEIGHT)

RETURN Height

FUNCTION BT_ClientAreaWidth(Win)

   LOCAL Width := BT_SCR_GETINFO(bt_WinHandle(Win), BT_SCR_CLIENTAREA, BT_SCR_INFO_WIDTH)

RETURN Width

FUNCTION BT_ClientAreaHeight(Win)

   LOCAL Height := BT_SCR_GETINFO(bt_WinHandle(Win), BT_SCR_CLIENTAREA, BT_SCR_INFO_HEIGHT)

RETURN Height

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION bt_StatusBarHandle(Win)

   LOCAL hWnd := bt_WinHandle(Win)
   LOCAL k
   LOCAL hWndStatusBar := HMG_NULLHANDLE

   FOR k := 1 TO Len(_HMG_aControlType)
      IF _HMG_aControlType[k] == CONTROL_TYPE_MESSAGEBAR .AND. _HMG_aControlParenthandles[k] == hWnd
         hWndStatusBar := _HMG_aControlHandles[k]
      ENDIF
   NEXT

RETURN hWndStatusBar

FUNCTION BT_StatusBarWidth(Win)

   LOCAL hWnd := bt_StatusBarHandle(Win)
   LOCAL Width := 0

   IF hWnd != 0
      Width := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_WIDTH)
   ENDIF

RETURN Width

FUNCTION BT_StatusBarHeight(Win)

   LOCAL hWnd := bt_StatusBarHandle(Win)
   LOCAL Height := 0

   IF hWnd > 0
      Height := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_HEIGHT)
   ENDIF

RETURN Height

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION bt_ToolBarBottomHandle(Win)

   LOCAL hWnd := bt_WinHandle(Win)
   LOCAL k
   LOCAL hWndToolBar := HMG_NULLHANDLE

   FOR k := 1 TO Len(_HMG_aControlType)
      IF _HMG_aControlType[k] == CONTROL_TYPE_TOOLBAR .AND. _HMG_aControlParenthandles[k] == hWnd .AND. hb_bitand(hmg_GetWindowLong(_HMG_aControlHandles[k], GWL_STYLE), CCS_BOTTOM) == CCS_BOTTOM
         hWndToolBar := _HMG_aControlHandles[k]
      ENDIF
   NEXT

RETURN hWndToolBar

FUNCTION BT_ToolBarBottomHeight(Win)

   LOCAL hWnd := bt_ToolBarBottomHandle(bt_WinHandle(Win))
   LOCAL nHeight := 0

   IF hWnd != 0
      nHeight := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_HEIGHT)
   ENDIF

RETURN nHeight

FUNCTION BT_ToolBarBottomWidth(Win)

   LOCAL hWnd := bt_ToolBarBottomHandle(bt_WinHandle(Win))
   LOCAL nWidth := 0

   IF hWnd != 0
      nWidth := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_WIDTH)
   ENDIF

RETURN nWidth

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION bt_ToolBarTopHandle(Win)

   LOCAL hWnd := bt_WinHandle(Win)
   LOCAL k
   LOCAL hWndToolBar := HMG_NULLHANDLE

   FOR k := 1 TO Len(_HMG_aControlType)
      IF _HMG_aControlType[k] == CONTROL_TYPE_TOOLBAR .AND. _HMG_aControlParenthandles[k] == hWnd .AND. hb_bitand(hmg_GetWindowLong(_HMG_aControlHandles[k], GWL_STYLE), CCS_BOTTOM) != CCS_BOTTOM
         hWndToolBar := _HMG_aControlHandles[k]
      ENDIF
   NEXT

RETURN hWndToolBar

FUNCTION BT_ToolBarTopHeight(Win)

   LOCAL hWnd := bt_ToolBarTopHandle(bt_WinHandle(Win))
   LOCAL nHeight := 0

   IF hWnd != 0
      nHeight := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_HEIGHT)
   ENDIF

RETURN nHeight

FUNCTION BT_ToolBarTopWidth(Win)

   LOCAL hWnd := bt_ToolBarTopHandle(bt_WinHandle(Win))
   LOCAL nWidth := 0

   IF hWnd != 0
      nWidth := BT_SCR_GETINFO(hWnd, BT_SCR_WINDOW, BT_SCR_INFO_WIDTH)
   ENDIF

RETURN nWidth

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_ClientAreaInvalidateAll(Win, lErase)

   lErase := IIF(lErase == NIL, .F., lErase)
   BT_SCR_INVALIDATERECT(bt_WinHandle(Win), NIL, lErase)

RETURN NIL

FUNCTION BT_ClientAreaInvalidateRect(Win, Row, Col, Width, Height, lErase)

   lErase := IIF(lErase == NIL, .F., lErase)
   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, BT_ClientAreaWidth(Win), BT_ClientAreaHeight(Win))
   BT_SCR_INVALIDATERECT(bt_WinHandle(Win), {Col, Row, Col + Width, Row + Height}, lErase)

RETURN NIL

// **********************************************************************************************************************************
// BITMAP  Functions
// **********************************************************************************************************************************

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapLoadFile(cFileName)

   LOCAL hBitmap := BT_BMP_LOADFILE(cFileName)

RETURN hBitmap

FUNCTION BT_BitmapSaveFile(hBitmap, cFileName, nTypePicture)

   LOCAL lRet

   nTypePicture := IIF(nTypePicture == NIL, BT_FILEFORMAT_BMP, nTypePicture)
   lRet := BT_BMP_SAVEFILE(hBitmap, cFileName, nTypePicture)

RETURN lRet

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapCreateNew(Width, Height, aRGBcolor_Fill_Bk)

   LOCAL New_hBitmap

   aRGBcolor_Fill_Bk := IIF(aRGBColor_Fill_Bk == NIL, BLACK, aRGBcolor_Fill_Bk)
   New_hBitmap := BT_BMP_CREATE(Width, Height, ArrayRGB_TO_COLORREF(aRGBColor_Fill_Bk))

RETURN New_hBitmap

FUNCTION BT_BitmapRelease(hBitmap)

   BT_BMP_RELEASE(hBitmap)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapWidth(hBitmap)

   LOCAL Width := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_WIDTH)

RETURN Width

FUNCTION BT_BitmapHeight(hBitmap)

   LOCAL Height := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_HEIGHT)

RETURN Height

FUNCTION BT_BitmapBitsPerPixel(hBitmap)

   LOCAL BitsPixel := BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_BITSPIXEL)

RETURN BitsPixel

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapInvert(hBitmap)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_INVERT)

RETURN NIL

FUNCTION BT_BitmapGrayness(hBitmap, Gray_Level)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_GRAYNESS, Gray_Level)

RETURN NIL

FUNCTION BT_BitmapBrightness(hBitmap, Light_Level)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_BRIGHTNESS, Light_Level)

RETURN NIL

FUNCTION BT_BitmapContrast(hBitmap, ContrastAngle)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_CONTRAST, ContrastAngle)

RETURN NIL

FUNCTION BT_BitmapModifyColor(hBitmap, RedLevel, GreenLevel, BlueLevel)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_MODIFYCOLOR, {RedLevel, GreenLevel, BlueLevel})

RETURN NIL

FUNCTION BT_BitmapGammaCorrect(hBitmap, RedGamma, GreenGamma, BlueGamma)

   BT_BMP_PROCESS(hBitmap, BT_BMP_PROCESS_GAMMACORRECT, {RedGamma, GreenGamma, BlueGamma})

RETURN NIL

FUNCTION BT_BitmapConvolutionFilter3x3(hBitmap, aFilter)

   BT_BMP_FILTER3x3(hBitmap, aFilter)

RETURN NIL

FUNCTION BT_BitmapTransform(hBitmap, Mode, Angle, aRGBColor_Fill_Bk)

   LOCAL New_hBitmap
   LOCAL ColorRef_Fill_Bk := IIF(aRGBColor_Fill_Bk == NIL, BT_BMP_GETINFO(hBitmap, BT_BITMAP_INFO_GETCOLORPIXEL, 0, 0), ArrayRGB_TO_COLORREF(aRGBColor_Fill_Bk))

   New_hBitmap := BT_BMP_TRANSFORM(hBitmap, Mode, Angle, ColorRef_Fill_Bk)

RETURN New_hBitmap

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapClone(hBitmap, Row, Col, Width, Height)

   LOCAL New_hBitmap
   LOCAL Max_Width := BT_BitmapWidth(hBitmap)
   LOCAL Max_Height := BT_BitmapHeight(hBitmap)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Max_Width, Max_Height)
   bt_AdjustWidthHeightRect(Row, Col, @Width, @Height, Max_Width, Max_Height)
   New_hBitmap := BT_BMP_CLONE(hBitmap, Col, Row, Width, Height)

RETURN New_hBitmap

FUNCTION BT_BitmapCopyAndResize(hBitmap, New_Width, New_Height, Mode_Stretch, Algorithm)

   LOCAL New_hBitmap

   Mode_Stretch := IIF(Mode_Stretch == NIL, BT_STRETCH, Mode_Stretch)
   Algorithm := IIF(Algorithm == NIL, BT_RESIZE_HALFTONE, Algorithm)
   New_hBitmap := BT_BMP_COPYANDRESIZE(hBitmap, New_Width, New_Height, Mode_Stretch, Algorithm)

RETURN New_hBitmap

FUNCTION BT_BitmapPaste(hBitmap_D, Row_D, Col_D, Width_D, Height_D, Mode_Stretch, hBitmap_O)

   LOCAL Max_Width_D := BT_BitmapWidth(hBitmap_D)
   LOCAL Max_Height_D := BT_BitmapHeight(hBitmap_D)
   LOCAL Width_O := BT_BitmapWidth(hBitmap_O)
   LOCAL Height_O := BT_BitmapHeight(hBitmap_O)

   bt_FillRectIsNIL(@Row_D, @Col_D, @Width_D, @Height_D, 0, 0, Max_Width_D, Max_Height_D)
   BT_BMP_PASTE(hBitmap_D, Col_D, Row_D, Width_D, Height_D, hBitmap_O, 0, 0, Width_O, Height_O, Mode_Stretch, BT_BITMAP_OPAQUE, 0)

RETURN NIL

FUNCTION BT_BitmapPasteTransparent(hBitmap_D, Row_D, Col_D, Width_D, Height_D, Mode_Stretch, hBitmap_O, aRGBcolor_transp)

   LOCAL Max_Width_D := BT_BitmapWidth(hBitmap_D)
   LOCAL Max_Height_D := BT_BitmapHeight(hBitmap_D)
   LOCAL Width_O := BT_BitmapWidth(hBitmap_O)
   LOCAL Height_O := BT_BitmapHeight(hBitmap_O)
   LOCAL ColorRef_Transp := IIF(aRGBcolor_transp == NIL, BT_BMP_GETINFO(hBitmap_O, BT_BITMAP_INFO_GETCOLORPIXEL, 0, 0), ArrayRGB_TO_COLORREF(aRGBcolor_transp))

   bt_FillRectIsNIL(@Row_D, @Col_D, @Width_D, @Height_D, 0, 0, Max_Width_D, Max_Height_D)
   BT_BMP_PASTE(hBitmap_D, Col_D, Row_D, Width_D, Height_D, hBitmap_O, 0, 0, Width_O, Height_O, Mode_Stretch, BT_BITMAP_TRANSPARENT, ColorRef_Transp)

RETURN NIL

FUNCTION BT_BitmapPasteAlphaBlend(hBitmap_D, Row_D, Col_D, Width_D, Height_D, Alpha, Mode_Stretch, hBitmap_O)

   LOCAL Max_Width_D := BT_BitmapWidth(hBitmap_D)
   LOCAL Max_Height_D := BT_BitmapHeight(hBitmap_D)
   LOCAL Width_O := BT_BitmapWidth(hBitmap_O)
   LOCAL Height_O := BT_BitmapHeight(hBitmap_O)

   bt_FillRectIsNIL(@Row_D, @Col_D, @Width_D, @Height_D, 0, 0, Max_Width_D, Max_Height_D)
   BT_BMP_PASTE_ALPHABLEND(hBitmap_D, Col_D, Row_D, Width_D, Height_D, hBitmap_O, 0, 0, Width_O, Height_O, Alpha, Mode_Stretch)

RETURN NIL

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapCaptureDesktop(Row, Col, Width, Height)

   LOCAL New_hBitmap
   LOCAL Win := BT_GetDesktopHandle()
   LOCAL Max_Width := BT_DesktopWidth()
   LOCAL Max_Height := BT_DesktopHeight()

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Max_Width, Max_Height)
   bt_AdjustWidthHeightRect(Row, Col, @Width, @Height, Max_Width, Max_Height)
   New_hBitmap := BT_BMP_CAPTURESCR(bt_WinHandle(Win), Col, Row, Width, Height, BT_BITMAP_CAPTURE_DESKTOP)

RETURN New_hBitmap

FUNCTION BT_BitmapCaptureWindow(Win, Row, Col, Width, Height)

   LOCAL New_hBitmap
   LOCAL Max_Width := BT_WindowWidth(Win)
   LOCAL Max_Height := BT_WindowHeight(Win)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Max_Width, Max_Height)
   bt_AdjustWidthHeightRect(Row, Col, @Width, @Height, Max_Width, Max_Height)
   New_hBitmap := BT_BMP_CAPTURESCR(bt_WinHandle(Win), Col, Row, Width, Height, BT_BITMAP_CAPTURE_WINDOW)

RETURN New_hBitmap

FUNCTION BT_BitmapCaptureClientArea(Win, Row, Col, Width, Height)

   LOCAL New_hBitmap
   LOCAL Max_Width := BT_ClientAreaWidth(Win)
   LOCAL Max_Height := BT_ClientAreaHeight(Win)

   bt_FillRectIsNIL(@Row, @Col, @Width, @Height, 0, 0, Max_Width, Max_Height)
   bt_AdjustWidthHeightRect(Row, Col, @Width, @Height, Max_Width, Max_Height)
   New_hBitmap := BT_BMP_CAPTURESCR(bt_WinHandle(Win), Col, Row, Width, Height, BT_BITMAP_CAPTURE_CLIENTAREA)

RETURN New_hBitmap

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FUNCTION BT_BitmapClipboardGet(Win)

   LOCAL hBitmap

   hBitmap := BT_BMP_GET_CLIPBOARD(bt_WinHandle(Win))

RETURN hBitmap

FUNCTION BT_BitmapClipboardPut(Win, hBitmap)

   LOCAL lRet := BT_BMP_PUT_CLIPBOARD(bt_WinHandle(Win), hBitmap)

RETURN lRet

FUNCTION BT_BitmapClipboardClean(Win)

   LOCAL lRet := BT_BMP_CLEAN_CLIPBOARD(bt_WinHandle(Win))

RETURN lRet

FUNCTION BT_BitmapClipboardIsEmpty()

   LOCAL lRet := BT_BMP_CLIPBOARD_ISEMPTY()

RETURN lRet

// **********************************************************************************************************************************
// HMG Functions
// **********************************************************************************************************************************

FUNCTION BT_HMGGetImage(cFormName, cControlName)

   LOCAL k
   LOCAL hBitmap := 0

#ifndef __HMG__
   MEMVAR _HMG_SYSDATA
#endif

   k := GetControlIndex(cControlName, cFormName)

   IF k > 0 .AND. GetControlType(cControlName, cFormName) == CONTROL_TYPE_IMAGE
#ifdef __HMG__ // HMG Extended
      hBitmap := _HMG_aControlContainerHandle[k]

#else          // HMG Official
      hBitmap := _HMG_SYSDATA[37, k]
#endif
   ENDIF

RETURN hBitmap

FUNCTION BT_HMGCloneImage(cFormName, cControlName)

   LOCAL hBitmap := BT_HMGGetImage(cFormName, cControlName)

RETURN BT_BitmapClone(hBitmap)

FUNCTION BT_HMGSetImage(cFormName, cControlName, hBitmap, lReleasePreviousBitmap)

   LOCAL hWnd
   LOCAL k

#ifndef __HMG__
   MEMVAR _HMG_SYSDATA
#endif

   IF !hb_isLogical(lReleasePreviousBitmap)
      lReleasePreviousBitmap := .T.
   ENDIF

   k := GetControlIndex(cControlName, cFormName)

   IF k > 0 .AND. GetControlType(cControlName, cFormName) == CONTROL_TYPE_IMAGE
#ifdef __HMG__ // HMG Extended
      IF _HMG_aControlContainerHandle[k] != 0 .AND. lReleasePreviousBitmap
         BT_BitmapRelease(_HMG_aControlContainerHandle[k])
      ENDIF
      _HMG_aControlContainerHandle[k] := hBitmap
      _HMG_aControlWidth[k] := BT_BitmapWidth(hBitmap)
      _HMG_aControlHeight[k] := BT_BitmapHeight(hBitmap)
#else          // HMG Official
      IF _HMG_SYSDATA[37, k] != 0 .AND. lReleasePreviousBitmap
         BT_BitmapRelease(_HMG_SYSDATA[37, k])
      ENDIF
      _HMG_SYSDATA[37, k] := hBitmap
      _HMG_SYSDATA[20, k] := BT_BitmapWidth(hBitmap)
      _HMG_SYSDATA[21, k] := BT_BitmapHeight(hBitmap)
#endif
      hWnd := GetControlHandle(cControlName, cFormName)
      #define _STM_SETIMAGE_ 0x0172
      #define _IMAGE_BITMAP_ 0
      hmg_SendMessage(hWnd, _STM_SETIMAGE_, _IMAGE_BITMAP_, hBitmap)
      #undef _IMAGE_BITMAP_
      #undef _STM_SETIMAGE_
   ENDIF

RETURN NIL
