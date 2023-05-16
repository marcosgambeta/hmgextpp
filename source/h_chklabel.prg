/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software; see the file COPYING. If not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 * visit the web site http://www.gnu.org/).
 *
 * As a special exception, you have permission for additional uses of the text
 * contained in this release of Harbour Minigui.
 *
 * The exception is that, if you link the Harbour Minigui library with other
 * files to produce an executable, this does not by itself cause the resulting
 * executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of linking the
 * Harbour-Minigui library code into it.
 *
 * Parts of this project are based upon:
 *
 * "Harbour GUI framework for Win32"
 * Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - https://harbour.github.io/
 *
 * "Harbour Project"
 * Copyright 1999-2022, https://harbour.github.io/
 *
 * "WHAT32"
 * Copyright 2002 AJ Wos <andrwos@aust1.net>
 *
 * "HWGUI"
 * Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
 */

#include "minigui.ch"
#include "i_winuser.ch"

FUNCTION _DefineChkLabel(ControlName, ParentFormName, x, y, Caption, w, h, ;
      fontname, fontsize, bold, BORDER, CLIENTEDGE, HSCROLL, VSCROLL, ;
      TRANSPARENT, aRGB_bk, aRGB_font, ProcedureName, tooltip, HelpId, invisible, ;
      italic, underline, strikeout, field, autosize, rightalign, centeralign, ;
      blink, mouseover, mouseleave, abitmap, leftcheck, lChecked, VCenterAlign, nId, bInit)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL WorkArea
   LOCAL cBmp
   LOCAL mVar
   LOCAL k := 0
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory
   LOCAL oc := NIL
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif
   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@ProcedureName, {|| SetProperty(ParentFormName, ControlName, "Checked", NIL) })
   hb_default(@invisible, .F.)
   hb_default(@bold, .F.)
   hb_default(@italic, .F.)
   hb_default(@underline, .F.)
   hb_default(@strikeout, .F.)
   hb_default(@VCenterAlign, .F.)

   IF (FontHandle := GetFontHandle(FontName)) != 0
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF Field != NIL
      IF hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select (WorkArea) != 0
            lChecked := &(Field)
         ENDIF
      ENDIF
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
      IF _HMG_IsThemed .AND. aRGB_bk == NIL
         Transparent := .T.
      ENDIF
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Window: " + IFNIL(ParentFormName, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   IF autosize .AND. !ISCHARACTER(Caption)
      Caption := cValToChar(Caption)
   ENDIF

   IF ValType(aBitmap) != "A"
      cBmp := aBitmap
      aBitmap := Array(2)
      aBitmap[1] := iif(Empty(cBmp), GetCheckBmp(), cBmp)
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_CHILD + SS_NOTIFY

      IF border
         Style += WS_BORDER
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF HSCROLL
         Style += WS_HSCROLL
      ENDIF

      IF VSCROLL
         Style += WS_VSCROLL
      ENDIF

      IF rightalign
         Style += ES_RIGHT
      ENDIF

      IF centeralign
         Style += ES_CENTER
      ENDIF

      IF VCenterAlign
         Style += SS_CENTERIMAGE
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogLabel(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "static", style, 0, x, y, w, h, caption, HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         IF caption != NIL
            SetWindowText(ControlHandle, caption)
         ENDIF

         SetWindowStyle(ControlHandle, Style, .T.)

/*          TODO

         if (hb_parl(12))
            {
            ExStyle = ExStyle | WS_EX_CLIENTEDGE;
               }

            if (hb_parl(15))
               {
               ExStyle = ExStyle | WS_EX_TRANSPARENT;
                  }
*/
      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      Controlhandle := InitChkLabel(ParentFormHandle, Caption, 0, x, y, w, h, "", 0, ;
         (hb_IsBlock(mouseover) .OR. hb_IsBlock(mouseleave)), border, clientedge, ;
         HSCROLL, VSCROLL, TRANSPARENT, invisible, rightalign, centeralign, ;
         abitmap[1], abitmap[2], leftcheck, lChecked, VCenterAlign)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         _SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         FontHandle := _SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
      ENDIF

      IF tooltip != NIL
         SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_CHECKLABEL
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := NIL
   _HMG_aControlInputMask          [k] := transparent
   _HMG_aControllostFocusProcedure [k] := mouseleave
   _HMG_aControlGotFocusProcedure  [k] := mouseover
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := aRGB_bk
   _HMG_aControlFontColor          [k] := aRGB_font
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := iif(autosize == .T., 1, 0)
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := abitmap
   _HMG_aControlContainerHandle    [k] := 0
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := h
   _HMG_aControlRangeMax           [k] := leftcheck
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := 0
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := {0, blink, .T.}
   _HMG_aControlMiscData2          [k] := ""

   IF blink == .T. .AND. !lDialogInMemory
      _DefineTimer("BlinkTimer" + hb_ntos(k), ParentFormName, 500, {||_HMG_aControlMiscData1[k][3] := !_HMG_aControlMiscData1[k][3], ;
         iif(_HMG_aControlMiscData1[k][3] == .T., _ShowControl(ControlName, ParentFormName), _HideControl(ControlName, ParentFormName))})
   ENDIF

   IF autosize == .T. .AND. !lDialogInMemory
      _SetControlWidth(ControlName, ParentFormName, GetTextWidth(NIL, Caption, FontHandle) + ;
         iif(bold == .T. .OR. italic == .T., GetTextWidth(NIL, " ", FontHandle), 0) + h + iif(Len(Caption) > 0 .AND. leftcheck == .F., GetBorderWidth(), iif(leftcheck, GetBorderWidth() / 2, 0)))
      _SetControlHeight(ControlName, ParentFormName, iif(FontSize < 13, 22, FontSize + 16))
      RedrawWindow(ControlHandle)
   ENDIF

   IF Field != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
   ENDIF
/*
   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF
*/
   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN NIL

STATIC FUNCTION GetCheckBmp()

   LOCAL uAnsi
   LOCAL cBmp
   LOCAL nHandle
   LOCAL nWrite
   LOCAL cBmpFile
   LOCAL cStock := ; // check bmp
      "42 4D F6 00 00 00 00 00 00 00 76 00 00 00 28 00" + ;
      "00 00 10 00 00 00 10 00 00 00 01 00 04 00 00 00" + ;
      "00 00 80 00 00 00 C4 0E 00 00 C4 0E 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 FF FF FF FF FF FF FF FF FF FF" + ;
      "FF 7F FF FF FF FF FF FF F8 07 FF FF FF FF FF FF" + ;
      "80 00 7F FF FF FF FF FF 80 00 78 FF FF FF FF F8" + ;
      "00 70 08 FF FF FF FF 80 07 80 07 8F FF FF F8 00" + ;
      "7F F7 00 8F FF FF F8 07 8F F8 00 78 FF FF F8 8F" + ;
      "FF FF 70 07 8F FF FF FF FF FF 87 00 7F FF FF FF" + ;
      "FF FF F8 70 07 FF FF FF FF FF FF 87 00 8F FF FF" + ;
      "FF FF FF F8 70 8F FF FF FF FF FF FF 88 8F FF FF" + ;
      "FF FF FF FF FF FF"

   uAnsi := StrTran(cStock, " ")
   cBmp := cAnsi2Bmp(uAnsi)
   cBmpFile := TempFile(GetTempFolder(), "BMP")

   IF File(cBmpFile)
      FErase(cBmpFile)
   ENDIF

   IF (nHandle := FCreate(cBmpFile)) < 0
      RETURN ""
   ENDIF

   nWrite := Len(cBmp)

   IF FWrite(nHandle, cBmp, nWrite) < nWrite
      cBmpFile := ""
   ENDIF

   FClose(nHandle)

RETURN cBmpFile

// ============================================================================
// FUNCTION cAnsi2Bmp() Version 9.0 Nov/30/2009
// ============================================================================

STATIC FUNCTION cAnsi2Bmp(cAnsi)

   LOCAL cLong
   LOCAL cBmp := ""

   WHILE Len(cAnsi) >= 8
      cLong := Left(cAnsi, 8)
      cBmp += cHex2Bin(cAnsi2Hex(cLong))
      cAnsi := Stuff(cAnsi, 1, 8, "")
   ENDDO

   IF !Empty(cAnsi)
      cBmp += cHex2Bin(cAnsi2Hex(PadR(cAnsi, 4, "0")))
   ENDIF

RETURN cBmp

// ============================================================================
// FUNCTION cAnsi2Hex() Version 9.0 Nov/30/2009
// ============================================================================

STATIC FUNCTION cAnsi2Hex(cAnsi)

   LOCAL cDig
   LOCAL cHex := ""

   cAnsi := AllTrim(cAnsi)

   WHILE Len(cAnsi) >= 2
      cDig := Left(cAnsi, 2)
      cHex := cDig + cHex
      cAnsi := Stuff(cAnsi, 1, 2, "")
   ENDDO

RETURN cHex

// ============================================================================
// FUNCTION cHex2Bin() Version 9.0 Nov/30/2009
// ============================================================================

STATIC FUNCTION cHex2Bin(cHex)

   LOCAL nPos
   LOCAL nEle
   LOCAL nExp := 0
   LOCAL nDec := 0
   LOCAL aHex := {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"}

   cHex := AllTrim(cHex)

   FOR nPos := Len(cHex) TO 1 Step - 1
      nEle := Max(0, AScan(aHex, SubStr(cHex, nPos, 1)) - 1)
      nDec += (nEle * (16 ** nExp))
      nExp ++
   NEXT

RETURN iif(Len(cHex) > 4, L2Bin(Int(nDec)), iif(Len(cHex) > 2, I2Bin(Int(nDec)), Chr(Int(nDec))))

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbwinuni.h>

#ifndef WC_STATIC
#define WC_STATIC  "Static"
#endif

LRESULT APIENTRY ChkLabelFunc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam);
static WNDPROC LabelOldWndProc;

struct INSCHK
{
   BOOL    lCheck;                        // is checked ?
   WNDPROC oldproc;                       // need to remember the old window procedure
   int     cxLeftEdge, cxRightEdge;       // size of the current window borders.
   int     cxButton;
   int     cxSpace;
   BOOL    lLeftCheck;
   HBITMAP himage;
   HBITMAP himagemask;
   HBITMAP himage2;
   HBITMAP himagemask2;
};

using PINSCHK = INSCHK *;

HBITMAP CreateBitmapMask(HBITMAP hbmColour, COLORREF crTransparent)
{
   HDC hdcMem;
   HDC hdcMem2;
   HBITMAP hbmMask;
   BITMAP bm;

   GetObject(hbmColour, sizeof(BITMAP), &bm);
   hbmMask = CreateBitmap(bm.bmWidth, bm.bmHeight, 1, 1, nullptr);

   hdcMem = CreateCompatibleDC(0);
   hdcMem2 = CreateCompatibleDC(0);

   SelectObject(hdcMem, hbmColour);
   SelectObject(hdcMem2, hbmMask);

   SetBkColor(hdcMem2, crTransparent);

   BitBlt(hdcMem2, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCCOPY);

   BitBlt(hdcMem, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem2, 0, 0, SRCINVERT);

   DeleteDC(hdcMem);
   DeleteDC(hdcMem2);

   return hbmMask;
}

void GetCheck(INSCHK * pbtn, RECT * rect)
{
   if( !(pbtn->lLeftCheck) )
   {
      rect->left = rect->right - pbtn->cxButton;
   }
   else
   {
      rect->right = rect->left + pbtn->cxButton;
   }

   if( pbtn->cxRightEdge > pbtn->cxLeftEdge )
   {
      OffsetRect(rect, pbtn->cxRightEdge - pbtn->cxLeftEdge, 0);
   }
}

BOOL InsertCheck(HWND hWnd, HBITMAP himage, HBITMAP himage2, int BtnWidth, BOOL lCheck, BOOL lLeftCheck)
{
   INSCHK * pbtn;

   pbtn = static_cast<INSCHK*>(HeapAlloc(GetProcessHeap(), 0, sizeof(INSCHK)));

   if( !pbtn )
   {
      return FALSE;
   }

   pbtn->lCheck     = lCheck;
   pbtn->lLeftCheck = lLeftCheck;
   pbtn->cxButton   = HB_MAX(BtnWidth, GetSystemMetrics(SM_CXVSCROLL));
   pbtn->himage     = himage;
   pbtn->himage2    = himage2;
   pbtn->cxSpace    = GetSystemMetrics(SM_CXSIZEFRAME) / 4;

   if( himage != nullptr )
   {
      pbtn->himagemask = CreateBitmapMask(himage, RGB(0, 0, 0));
   }
   else
   {
      pbtn->himagemask = nullptr;
   }

   if( himage2 != nullptr )
   {
      pbtn->himagemask2 = CreateBitmapMask(himage2, RGB(0, 0, 0));
   }
   else
   {
      pbtn->himagemask2 = nullptr;
   }

   // associate our button state structure with the window

   SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pbtn));

   // force the edit control to update its non-client area

   SetWindowPos(hWnd, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER);

   return TRUE;
}

static void DrawCheck(HWND hWnd, INSCHK * pbtn, RECT * prect)
{
   HDC     hdc;
   HBITMAP hBitmap      = pbtn->himage;
   HBITMAP hBitmapMask  = pbtn->himagemask;
   HBITMAP hBitmap2     = pbtn->himage2;
   HBITMAP hBitmapMask2 = pbtn->himagemask2;
   BITMAP  bm;

   hdc = GetWindowDC(hWnd);

   if( hBitmap == nullptr )
   {
      FillRect(hdc, prect, GetSysColorBrush(COLOR_WINDOW));
      SetBkMode(hdc, TRANSPARENT);
      DrawText(hdc, TEXT("V"), 1, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
   }
   else
   {
      int wRow = prect->top;
      int wCol = prect->left;

      HDC hdcMem = CreateCompatibleDC(hdc);

      if( pbtn->lCheck )
      {
         HBITMAP hbmOld = static_cast<HBITMAP>(SelectObject(hdcMem, hBitmapMask));
         GetObject(hBitmap, sizeof(bm), &bm);

         BitBlt(hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCAND);

         SelectObject(hdcMem, hBitmap);
         BitBlt(hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCPAINT);
         SelectObject(hdcMem, hbmOld);
      }
      else if( hBitmap2 != nullptr )
      {
         HBITMAP hbmOld = static_cast<HBITMAP>(SelectObject(hdcMem, hBitmapMask2));
         GetObject(hBitmap2, sizeof(bm), &bm);

         BitBlt(hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCAND);

         SelectObject(hdcMem, hBitmap2);
         BitBlt(hdc, wCol, wRow, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCPAINT);
         SelectObject(hdcMem, hbmOld);
      }
      DeleteDC(hdcMem);

   }

   ReleaseDC(hWnd, hdc);
}

HB_FUNC_STATIC( INITCHKLABEL )
{
   HWND hwnd;
   HWND hbutton;
   HBITMAP himage;
   HBITMAP himage2;
   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);
   int BtnWidth = hb_parni(7);
   int style = WS_CHILD | SS_NOTIFY;
   int ExStyle = 0;

   hwnd = hmg_par_HWND(1);

   if( hb_parl(12) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   if( hb_parl(11) )
   {
      style |= WS_BORDER;
   }

   if( hb_parl(13) )
   {
      style |= WS_HSCROLL;
   }

   if( hb_parl(14) )
   {
      style |= WS_VSCROLL;
   }

   if( hb_parl(15) )
   {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   if( !hb_parl(16) )
   {
      style |= WS_VISIBLE;
   }

   if( hb_parl(17) )
   {
      style |= ES_RIGHT;
   }

   if( hb_parl(18) )
   {
      style |= ES_CENTER;
   }

   if( hb_parl(23) )
   {
      style |= SS_CENTERIMAGE;
   }

   hbutton = CreateWindowEx(ExStyle,
                            WC_STATIC,
                            lpWindowName,
                            style,
                            hmg_par_int(4),
                            hmg_par_int(5),
                            hmg_par_int(6),
                            hmg_par_int(7),
                            hwnd,
                            hmg_par_HMENU(3),
                            GetInstance(),
                            nullptr);

   if( hb_parc(19) != nullptr )
   {
      himage = HMG_LoadPicture(hb_parc(19), -1, -1, nullptr, 0, 0, -1, 0, false, 255);
   }
   else
   {
      himage = nullptr;
   }

   if( hb_parc(20) != nullptr )
   {
      himage2 = HMG_LoadPicture(hb_parc(20), -1, -1, nullptr, 0, 0, -1, 0, false, 255);
   }
   else
   {
      himage2 = nullptr;
   }

   InsertCheck(hbutton, himage, himage2, BtnWidth, hb_parl(22), hb_parl(21));

   LabelOldWndProc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(ChkLabelFunc)));
   SetWindowPos(hbutton, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER);

   hmg_ret_HWND(hbutton);

   hb_strfree(WindowName);
}

HB_FUNC( SETCHKLABEL )
{
   HWND hWnd = hmg_par_HWND(1);
   INSCHK * pbtn = reinterpret_cast<INSCHK*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));
   RECT rect;

   pbtn->lCheck = hb_parl(2);

   GetWindowRect(hWnd, &rect);
   OffsetRect(&rect, -rect.left, -rect.top);
   ShowWindow(hWnd, SW_HIDE);
   InvalidateRect(hWnd, &rect, TRUE);

   GetCheck(pbtn, &rect);
   DrawCheck(hWnd, pbtn, &rect);
   ShowWindow(hWnd, SW_SHOW);
}

HB_FUNC( GETCHKLABEL )
{
   HWND hWnd = hmg_par_HWND(1);
   INSCHK * pbtn = reinterpret_cast<INSCHK*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));

   hb_retl(pbtn->lCheck);
}

LRESULT APIENTRY ChkLabelFunc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;
   long int r;
   RECT * prect;
   RECT oldrect;
   RECT rect;
   TRACKMOUSEEVENT tme;

   INSCHK * pbtn = reinterpret_cast<INSCHK*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));

   switch( Msg )
   {
      case WM_NCCALCSIZE:
         prect = reinterpret_cast<RECT*>(lParam);
         oldrect = *prect;

         CallWindowProc(LabelOldWndProc, hWnd, Msg, wParam, lParam);
         SendMessage(hWnd, WM_SETREDRAW, 1, 0);
         if( !pbtn )
         {
            return 0;
         }

         if( pbtn->lLeftCheck )
         {
            pbtn->cxLeftEdge = prect->left - oldrect.left;
            pbtn->cxRightEdge = oldrect.right - prect->right;
            prect->left += pbtn->cxButton + pbtn->cxSpace;
         }
         else
         {
            pbtn->cxLeftEdge = prect->left - oldrect.left;
            pbtn->cxRightEdge = oldrect.right - prect->right;
            prect->right -= pbtn->cxButton + pbtn->cxSpace;
         }

         return 0;

      case WM_NCPAINT:
         CallWindowProc(LabelOldWndProc, hWnd, Msg, wParam, lParam);
         if( pbtn->lCheck )
         {
            GetWindowRect(hWnd, &rect);
            OffsetRect(&rect, -rect.left, -rect.top);
            GetCheck(pbtn, &rect);
            DrawCheck(hWnd, pbtn, &rect);
         }
         else if( pbtn->himage2 != nullptr )
         {
            GetWindowRect(hWnd, &rect);
            OffsetRect(&rect, -rect.left, -rect.top);
            GetCheck(pbtn, &rect);
            DrawCheck(hWnd, pbtn, &rect);
         }
         return 0;

      case WM_MOUSEMOVE:
         tme.cbSize      = sizeof(TRACKMOUSEEVENT);
         tme.dwFlags     = TME_LEAVE;
         tme.hwndTrack   = hWnd;
         tme.dwHoverTime = HOVER_DEFAULT;
         _TrackMouseEvent(&tme);

         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OLABELEVENTS"));
         }

         if( pSymbol )
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hWnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         r = hb_parnl(-1);

         return (r != 0) ? r : CallWindowProc(LabelOldWndProc, hWnd, 0, 0, 0);

      case WM_MOUSELEAVE:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OLABELEVENTS"));
         }

         if( pSymbol )
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hWnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         r = hb_parnl(-1);

         return (r != 0) ? r : CallWindowProc(LabelOldWndProc, hWnd, 0, 0, 0);
   }

   return CallWindowProc(LabelOldWndProc, hWnd, Msg, wParam, lParam);
}

#pragma ENDDUMP
