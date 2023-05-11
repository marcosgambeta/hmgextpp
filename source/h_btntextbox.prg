/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * BTNTEXTBOX control source code
 * (C)2006-2011 Janusz Pora <januszpora@onet.eu>
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

#define EM_SETCUEBANNER       0x1501

#define TBB1   2
#define TBB2   3

FUNCTION _DefineBtnTextBox(ControlName, ParentFormName, x, y, w, h, ;
      cValue, ProcedureName, ProcedureName2, abitmap, BtnWidth, FontName, FontSize, ;
      aToolTip, nMaxLength, lUpper, lLower, lNumeric, lPassword, ;
      uLostFocus, uGotFocus, uChange, uEnter, right, HelpId, ;
      bold, italic, underline, strikeout, field, backcolor, fontcolor, ;
      invisible, notabstop, nId, disableedit, lDefault, cuetext, keepfocus, bInit)

   LOCAL ParentFormHandle
   LOCAL aControlHandle := 0
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL lBtn2 := ISBLOCK(ProcedureName2)
   LOCAL FontHandle
   LOCAL cBmp
   LOCAL tmp
   LOCAL WorkArea
   LOCAL blInit
   LOCAL lDialogInMemory
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   // Assign STANDARD values to optional params.
   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@cValue, "")
   __defaultNIL(@uChange, "")
   __defaultNIL(@uGotFocus, "")
   __defaultNIL(@uLostFocus, "")
   __defaultNIL(@uEnter, "")
   __defaultNIL(@nMaxLength, 255)
   hb_default(@lUpper, .F.)
   hb_default(@lLower, .F.)
   hb_default(@lNumeric, .F.)
   hb_default(@lPassword, .F.)
   hb_default(@lDefault, .F.)
   hb_default(@keepfocus, .T.)

   IF ValType(aBitmap) != "A"
      cBmp := aBitmap
      aBitmap := Array(2)
      aBitmap[1] := cBmp
   ENDIF

   IF Field != NIL
      IF hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select (WorkArea) != 0
            cValue := &(Field)
         ENDIF
      ENDIF
   ENDIF

   IF ValType(aToolTip) != "A"
      tmp := aToolTip
      aToolTip := Array(3)
      aToolTip[1] := tmp
   ELSE
      IF Len(aToolTip) < 3
         aToolTip := ASize(aToolTip, 3)
      ENDIF
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != 0
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
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
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

// Check if the window/form is defined.
   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Window " + IFNIL(ParentFormName, "Parent", ParentFormName) + " is not defined.")
   ENDIF

// Check if the control is already defined.
   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control " + ControlName + " of " + ParentFormName + " already defined.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive
      ParentFormHandle  := _HMG_ActiveDialogHandle

      Style := WS_CHILD + ES_AUTOHSCROLL + WS_CLIPCHILDREN

      IF lNumeric
         Style += ES_NUMBER
      ELSE
         IF lUpper
            Style += ES_UPPERCASE
         ENDIF
         IF lLower
            Style += ES_LOWERCASE
         ENDIF
      ENDIF

      IF lPassword
         Style += ES_PASSWORD
      ENDIF

      IF right
         Style += ES_RIGHT
      ENDIF

      IF disableedit
         Style += ES_READONLY
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF  !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF Len(_HMG_aDialogTemplate) > 0        //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,aToolTip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogTextBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "EDIT", style, 0, x, y, w, h, cValue, HelpId, aToolTip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         aControlHandle := GetDialogItemHandle(ParentFormHandle, nId)
         SetWindowStyle(aControlHandle, Style, .T.)

         x := GetWindowCol(aControlHandle)
         y := GetWindowRow(aControlHandle)
         w := GetWindowWidth(aControlHandle)
         h := GetWindowHeight(aControlHandle)

         aControlHandle := ReDefBtnTextBox(aControlHandle, abitmap[1], BtnWidth, abitmap[2], lBtn2, w, h)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)
      // Creates the control window
      aControlHandle := InitBtnTextBox(ParentFormHandle, 0, x, y, w, h, "", 0, nMaxLength, ;
         lUpper, lLower, .F., lPassword, right, invisible, notabstop, ;
         abitmap[1], BtnWidth, abitmap[2], lBtn2, disableedit, lDefault)

   ENDIF

   IF !lDialogInMemory

      IF FontHandle != 0
         _SetFontHandle(aControlHandle[1], FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(aControlHandle[1])
            FontHandle := _SetFont(aControlHandle[1], FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
         SetTbBtnMargin(aControlHandle[1], BtnWidth, .T., lBtn2)
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, aControlHandle)
      ENDIF

      // Add a ToolTip if param has value
      FOR tmp := 1 TO 3
         IF aToolTip[tmp] != NIL
            SetToolTip(aControlHandle[tmp], aToolTip[tmp], GetFormToolTipHandle(ParentFormName))
         ENDIF
      NEXT

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := iif(lNumeric, CONTROL_TYPE_BTNNUMTEXT, CONTROL_TYPE_BTNTEXT)
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := aControlHandle[1]
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := cValue
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControlLostFocusProcedure [k] := uLostFocus
   _HMG_aControlGotFocusProcedure  [k] := uGotFocus
   _HMG_aControlChangeProcedure    [k] := uChange
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := uEnter
   _HMG_aControlHeadClick          [k] := ProcedureName2
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := aControlHandle
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := abitmap
   _HMG_aControlContainerHandle    [k] := 0
   _HMG_aControlFontName           [k] := FontName
   _HMG_aControlFontSize           [k] := FontSize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := aToolTip
   _HMG_aControlRangeMin           [k] := BtnWidth
   _HMG_aControlRangeMax           [k] := nMaxLength
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := !invisible
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := 0
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := {0, lBtn2, disableedit, lDefault, keepfocus}
   _HMG_aControlMiscData2          [k] := ""

   IF !lDialogInMemory
      // With NUMERIC clause, transform numeric value into a string.
      IF lNumeric
         IF ValType(cValue) != "C"
            cValue := hb_ntos(cValue)
         ENDIF
      ENDIF

      // Fill the TEXTBOX with the text given.
      IF Len(cValue) > 0
         SetWindowText(aControlHandle[1], cValue)
      ENDIF

      IF !Empty(cuetext) .AND. IsVistaOrLater()
         SendMessageWideString(aControlHandle[1], EM_SETCUEBANNER, .T. /*show on focus*/, cuetext)
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(aControlHandle[1])
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN nil

FUNCTION InitDialogBtnTextBox(ParentName, ControlHandle, k)

   LOCAL nMaxLength
   LOCAL Field
   LOCAL cValue
   LOCAL lNumeric
   LOCAL abitmap
   LOCAL BtnWidth
   LOCAL lBtn2
   LOCAL aControlHandle

   Field          := _HMG_aControlPageMap[k]
   nMaxLength     := _HMG_aControlRangeMax[k]
   cValue         := _HMG_aControlValue[k]
   lNumeric       := (_HMG_aControlType[k] == CONTROL_TYPE_BTNNUMTEXT)
   abitmap        := _HMG_aControlPicture[k]
   BtnWidth       := _HMG_aControlRangeMin[k]
   lBtn2          := _HMG_aControlMiscData1[k, 2]
   aControlHandle := _HMG_aControlSpacing[k]

   IF nMaxLength != NIL
      SendMessage(aControlHandle[1], EM_LIMITTEXT, nMaxLength, 0)
   ENDIF

// With NUMERIC clause, transform numeric value into a string.
   IF lNumeric
      IF ValType(cValue) != "C"
         cValue := hb_ntos(cValue)
      ENDIF
   ENDIF

// Fill the TEXTBOX with the text given.
   IF Len(cValue) > 0
      SetWindowText(aControlHandle[1], cValue)
   ENDIF

   IF Field != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentName)], k)
   ENDIF

   ReDefBtnTextBox(ControlHandle, abitmap[1], BtnWidth, abitmap[2], lBtn2)
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN Nil

FUNCTION TBBtnEvents(hwndEdit, HwndBtn, nMsg)

   LOCAL ParentForm
   LOCAL i
   LOCAL aHandle

   i := AScan(_HMG_aControlSpacing, {|x|HB_ISARRAY(x) .AND. Len(x) > 0 .AND. HB_ISNUMERIC(x[1]) .AND. x[1] == hwndEdit})

   IF i > 0 .AND. HwndBtn > 0

      aHandle := _HMG_aControlSpacing[i]

      SWITCH AScan(aHandle, HwndBtn)
      CASE TBB1
         IF _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            IF HB_ISARRAY(_HMG_aControlMiscData1[i]) .AND. Len(_HMG_aControlMiscData1[i]) >= 4 .AND. !_HMG_aControlMiscData1[i][4]
               SendMessage(HwndBtn, BM_SETSTYLE, LOWORD(BS_PUSHBUTTON), 1)
            ENDIF
         ENDIF
         EXIT

      CASE TBB2
         IF _DoControlEventProcedure(_HMG_aControlHeadClick[i], i)
            IF HB_ISARRAY(_HMG_aControlMiscData1[i]) .AND. Len(_HMG_aControlMiscData1[i]) >= 4 .AND. !_HMG_aControlMiscData1[i][4]
               SendMessage(HwndBtn, BM_SETSTYLE, LOWORD(BS_PUSHBUTTON), 1)
            ENDIF
         ENDIF
      END SWITCH

      IF nMsg == WM_CONTEXTMENU
         ParentForm := _HMG_aControlParentHandles[i]
         IF (i := AScan(_HMG_aControlsContextMenu, {|x|x[1] == aHandle[1]})) > 0
            IF _HMG_aControlsContextMenu[i][4] == .T.
               setfocus(aHandle[1])
               _HMG_xControlsContextMenuID := _HMG_aControlsContextMenu[i][3]
               TrackPopupMenu(_HMG_aControlsContextMenu[i][2], LOWORD(HwndBtn), HIWORD(HwndBtn), ParentForm)
               RETURN 1
            ENDIF
         ENDIF
      ELSE
         IF HB_ISARRAY(_HMG_aControlMiscData1[i]) .AND. Len(_HMG_aControlMiscData1[i]) > 4 .AND. _HMG_aControlMiscData1[i][5]
            SetFocus(aHandle[1])
         ENDIF
      ENDIF

   ENDIF

RETURN 0

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <hbapiitm.h>
#include <hbstack.h>
#include <hbvm.h>
#include <hbwinuni.h>

#ifndef WC_EDIT
#define WC_EDIT    "Edit"
#define WC_BUTTON  "Button"
#endif

#define TBB1       2
#define TBB2       3

LRESULT CALLBACK OwnBtnTextProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

/*
INITBTNTEXTBOX(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) --> array
*/
HB_FUNC_STATIC( INITBTNTEXTBOX )
{
   HWND himage, himage2;
   BOOL fBtn2 = hb_parl(20);
   int  BtnWidth2;
   int  BtnWidth = HB_ISNIL(18) ? 0 : hb_parni(18);

   // Get the handle of the parent window/form.
   HWND hwnd = hmg_par_HWND(1);

   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   DWORD style = WS_CHILD | ES_AUTOHSCROLL | WS_CLIPCHILDREN; // TEXTBOX window base style

   if( hb_parl(12) )  // if <lNumeric> is TRUE, then ES_NUMBER style is added.
   {
      style |= ES_NUMBER;
   }
   else
   {
      if( hb_parl(10) ) // if <lUpper> is TRUE, then ES_UPPERCASE style is added.
      {
         style |= ES_UPPERCASE;
      }

      if( hb_parl(11) ) // if <lLower> is TRUE, then ES_LOWERCASE style is added.
      {
         style |= ES_LOWERCASE;
      }
   }

   if( hb_parl(13) )  // if <lPassword> is TRUE, then ES_PASSWORD style is added.
   {
      style |= ES_PASSWORD;
   }

   if( hb_parl(14) )
   {
      style |= ES_RIGHT;
   }

   if( !hb_parl(15) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(16) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(21) )
   {
      style |= ES_READONLY;
   }

   // Creates the child Frame control.
   HWND hedit = CreateWindowEx(WS_EX_CLIENTEDGE, WC_EDIT, "", style,
      hmg_par_int(3), hmg_par_int(4), hmg_par_int(5), hmg_par_int(6),
      hwnd, nullptr, GetInstance(), nullptr);

   SetProp(hedit, "OldWndProc", reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnBtnTextProc));

   SendMessage(hedit, EM_LIMITTEXT, hmg_par_WPARAM(9), 0);

   if( hb_parc(17) != nullptr )
   {
      void * ImageName;
      LPCTSTR lpImageName = HB_PARSTR(17, &ImageName, nullptr);

      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage == nullptr )
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage != nullptr )
      {
         BITMAP bm;
         GetObject(himage, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > hb_parni(6) - 5 )
         {
            DeleteObject(himage);
            himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4, hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            if( himage == nullptr )
            {
               himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, BtnWidth - 4, hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName);
   }
   else
   {
      himage = nullptr;
   }

   if( hb_parc(19) != nullptr )
   {
      void * ImageName2;
      LPCTSTR lpImageName2 = HB_PARSTR(19, &ImageName2, nullptr);

      himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage2 == nullptr )
      {
         himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage2 != nullptr )
      {
         BITMAP bm;
         GetObject(himage2, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > hb_parni(6) - 5 )
         {
            DeleteObject(himage2);
            himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

            if( himage2 == nullptr )
            {
               himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName2);
   }
   else
   {
      himage2 = nullptr;
   }

   DWORD ibtnStyle1 = BS_NOTIFY | WS_CHILD | WS_VISIBLE | (hb_parl(22) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON);

   if( himage != nullptr )
   {
      ibtnStyle1 |= BS_BITMAP;
   }

   DWORD ibtnStyle2 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE; // BUTTON window base style.

   if( himage2 != nullptr )
   {
      ibtnStyle2 |= BS_BITMAP;
   }

   HWND hBtn1 = CreateWindowEx(0, WC_BUTTON, "...", ibtnStyle1,
      hmg_par_int(5) - BtnWidth - 3, -1, BtnWidth, hmg_par_int(6) - 2,
      hedit, reinterpret_cast<HMENU>(TBB1), GetInstance(), nullptr);

   HWND hBtn2 = fBtn2 ? CreateWindowEx(0, WC_BUTTON, "...", ibtnStyle2,
      hmg_par_int(5) - BtnWidth - BtnWidth2 - 3, -1, BtnWidth, hmg_par_int(6) - 2,
      hedit, reinterpret_cast<HMENU>(TBB2), GetInstance(), nullptr) : nullptr;

   if( himage != nullptr )
   {
      SendMessage(hBtn1, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));
   }

   if( himage2 != nullptr )
   {
      SendMessage(hBtn2, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage2));
   }

   hb_reta(5);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hedit), -1, 1);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn1), -1, 2);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn2), -1, 3);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 4);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage2), -1, 5);
}

/*
REDEFBTNTEXTBOX(p1, p2, p3, p4, p5, p6, p7) --> array
*/
HB_FUNC_STATIC( REDEFBTNTEXTBOX )
{
   HWND himage, himage2;
   int  width, height, BtnWidth2;
   int  BtnWidth = HB_ISNIL(3) ? 0 : hb_parni(3);

   HWND hedit = hmg_par_HWND(1);
   BOOL fBtn2 = hb_parl(5);
   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth2 = fBtn2 ?  BtnWidth : 0;
   width     = hb_parni(6);
   height    = hb_parni(7);

   SetProp(hedit, "OldWndProc", reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnBtnTextProc));

   if( hb_parc(2) != nullptr )
   {
      void * ImageName;
      LPCTSTR lpImageName = HB_PARSTR(2, &ImageName, nullptr);

      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage == nullptr )
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage != nullptr )
      {
         BITMAP bm;
         GetObject(himage, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > height - 5 )
         {
            DeleteObject(himage);
            himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4, height - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            if( himage == nullptr )
            {
               himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, BtnWidth - 4, height - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName);
   }
   else
   {
      himage = nullptr;
   }

   if( hb_parc(4) != nullptr )
   {
      void * ImageName2;
      LPCTSTR lpImageName2 = HB_PARSTR(4, &ImageName2, nullptr);

      himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage2 == nullptr )
      {
         himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage2 != nullptr )
      {
         BITMAP bm;
         GetObject(himage2, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > height - 5 )
         {
            DeleteObject(himage2);
            himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, height - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

            if( himage2 == nullptr )
            {
               himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, height - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName2);
   }
   else
   {
      himage2 = nullptr;
   }

   HWND hBtn1 = CreateWindowEx(0, WC_BUTTON, "...", BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE | BS_BITMAP,
      width - BtnWidth - 4, -1, BtnWidth, height - 2,
      hedit, reinterpret_cast<HMENU>(TBB1), GetInstance(), nullptr);

   HWND hBtn2 = fBtn2 ? CreateWindowEx(0, WC_BUTTON, "...", BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE | BS_BITMAP,
      width - BtnWidth - BtnWidth2 - 4, -1, BtnWidth, height - 2,
      hedit, reinterpret_cast<HMENU>(TBB2), GetInstance(), nullptr) : nullptr;

   if( himage != nullptr )
   {
      SendMessage(hBtn1, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));
   }

   if( himage2 != nullptr )
   {
      SendMessage(hBtn2, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage2));
   }

   SendMessage(hedit, EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG(0, BtnWidth + BtnWidth2 + 2));

   hb_reta(5);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hedit), -1, 1);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn1), -1, 2);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn2), -1, 3);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 4);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage2), -1, 5);
}

/*
SETTBBTNMARGIN(hedit, nBtnWidth, lBtns, lBtn2) --> NIL
*/
HB_FUNC( SETTBBTNMARGIN )
{
   int  BtnWidth = hb_parni(2);
   BOOL fBtns    = hb_parl(3);
   BOOL fBtn2    = hb_parl(4);
   int  BtnWidth2;

   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth  = fBtns ? BtnWidth : 0;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   SendMessage(hmg_par_HWND(1), EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG(0, BtnWidth + BtnWidth2 + 2));
}

LRESULT CALLBACK OwnBtnTextProc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;
   long int r;

   WNDPROC OldWndProc = reinterpret_cast<WNDPROC>(GetProp(hwnd, "OldWndProc"));

   switch( Msg )
   {
      case WM_CONTEXTMENU:
      case WM_COMMAND:

         if( lParam != 0 && (HIWORD(wParam) == BN_CLICKED || Msg == WM_CONTEXTMENU) )
         {
            if( !pSymbol )
            {
               pSymbol = hb_dynsymSymbol(hb_dynsymGet("TBBTNEVENTS"));
            }

            if( pSymbol )
            {
               hb_vmPushSymbol(pSymbol);
               hb_vmPushNil();
               hmg_vmPushHandle(hwnd);
               hb_vmPushNumInt(lParam);
               hb_vmPushLong(Msg);
               hb_vmDo(3);
            }

            r = hb_parnl(-1);

            return r != 0 ? r : CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
         }
   }

   return CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
}

#pragma ENDDUMP
