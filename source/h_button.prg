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
 *
 * Parts of this code is contributed and used here under permission of his author:
 * Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 */

#include "i_winuser.ch"
#include "minigui.ch"

/*
_DefineButton(...) --> NIL
*/
FUNCTION _DefineButton(ControlName, ParentFormName, x, y, Caption, ;
   ProcedureName, w, h, fontname, fontsize, tooltip, gotfocus, lostfocus, flat, NoTabStop, HelpId, ;
   invisible, bold, italic, underline, strikeout, multiline, default, key, nId)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL blInit
   LOCAL k
   LOCAL Style
   LOCAL lDialogInMemory

   hb_default(@w, 100)
   hb_default(@h, 28)
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   hb_default(@invisible, .F.)
   hb_default(@flat, .F.)
   hb_default(@NoTabStop, .F.)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
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

   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Window: " + iif(ParentFormName == NIL, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := BS_NOTIFY + WS_CHILD + BS_PUSHBUTTON
      IF flat
         style += BS_FLAT
      ENDIF
      IF !NoTabStop
         style += WS_TABSTOP
      ENDIF
      IF !invisible
         style += WS_VISIBLE
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //           {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogButtonImage(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "button", style, 0, x, y, w, h, caption, HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         IF !empty(FontHandle)
            _SetFontHandle(ControlHandle, FontHandle)
         ELSE
            __defaultNIL(@FontName, _HMG_DefaultFontName)
            __defaultNIL(@FontSize, _HMG_DefaultFontSize)
            IF IsWindowHandle(ControlHandle)
               FontHandle := _SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
            ENDIF
         ENDIF
         IF caption != NIL
            SetWindowText(ControlHandle, caption)
         ENDIF

         SetWindowStyle(ControlHandle, style, .T.)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      ControlHandle := InitButton(ParentFormHandle, Caption, 0, x, y, w, h, "", 0, flat, NoTabStop, invisible, multiline, default)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         _SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(ControlHandle)
            FontHandle := _SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_BUTTON
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := NIL
   _HMG_aControlInputMask          [k] := iif(hb_IsString(key), key, "")
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF !lDialogInMemory

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      IF tooltip != NIL
         SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

      _SetHotKeyByName(ParentFormName, key, ProcedureName)

   ENDIF

RETURN NIL

/*
*/
FUNCTION _DefineImageButton(ControlName, ParentFormName, x, y, Caption, ;
   ProcedureName, w, h, image, tooltip, gotfocus, lostfocus, flat, notrans, HelpId, ;
   invisible, notabstop, default, icon, extract, nIdx, noxpstyle, key, nId)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL aRet
   LOCAL mVar
   LOCAL cPicture
   LOCAL blInit
   LOCAL k
   LOCAL Style
   LOCAL nhImage
   LOCAL lDialogInMemory

   hb_default(@flat, .F.)
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@noxpstyle, .F.)
   hb_default(@nIdx, 0)

   IF _HMG_ToolBarActive
      RETURN NIL
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Window: " + iif(ParentFormName == NIL, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   IF !Empty(image) .AND. !Empty(icon)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + ". Either bitmap or icon must be specified.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   cPicture := iif(empty(icon), image, icon)
   IF hb_IsArray(cPicture)
      image := cPicture[1]
   ENDIF

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := BS_NOTIFY + BS_BITMAP + WS_CHILD + BS_PUSHBUTTON
      IF flat
         style += BS_FLAT
      ENDIF
      IF !NoTabStop
         style += WS_TABSTOP
      ENDIF
      IF !invisible
         style += WS_VISIBLE
      ENDIF

      IF lDialogInMemory // Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,ToolTip,FontName,FontSize,bold,italic,,}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogButtonImage(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "button", style, 0, x, y, w, h, caption, HelpId, tooltip, , , , , , , blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         SetWindowStyle(ControlHandle, style, .T.)

         _SetBtnPicture(ControlHandle, image)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      aRet := InitImageButton(ParentFormHandle, Caption, 0, x, y, w, h, image, flat, notrans, invisible, notabstop, default, icon, extract, nIdx, (_HMG_IsThemed .AND. !noxpstyle))

      ControlHandle := aRet[1]
      nhImage := aRet[2]

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_BUTTON
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := NIL
   _HMG_aControlInputMask          [k] := iif(hb_IsString(key), key, "")
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := noxpstyle
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := cPicture
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
   _HMG_aControlBrushHandle        [k] := nhImage
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := iif(empty(icon), 0, 1)  // 0 - bitmap  1 - icon
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF !lDialogInMemory

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
      ENDIF

      IF tooltip != NIL
         SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

      _SetHotKeyByName(ParentFormName, key, ProcedureName)

   ENDIF

RETURN NIL

/*
InitDialogButtonImage(ParentFormName, ControlHandle, k) --> NIL
*/
FUNCTION InitDialogButtonImage(ParentFormName, ControlHandle, k)

   LOCAL image

   image := _HMG_aControlPicture[k]
   IF !Empty(image) .AND. ParentFormName != NIL
      _SetBtnPicture(ControlHandle, image)
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]  // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <shellapi.h>
#include <commctrl.h>
#include <math.h>
#include <hbapiitm.hpp>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

#ifndef BCM_FIRST
#define BCM_FIRST         0x1600
#define BCM_SETIMAGELIST  (BCM_FIRST + 0x0002)
#endif

HIMAGELIST HMG_SetButtonImageList(HWND hButton, const char * FileName, int Transparent, UINT uAlign);

#if (defined(__BORLANDC__) && __BORLANDC__ < 1410) || (defined(__MINGW32__) && defined(__MINGW32_VERSION))
struct BUTTON_IMAGELIST
{
   HIMAGELIST himl;
   RECT       margin;
   UINT       uAlign;
};
using PBUTTON_IMAGELIST = BUTTON_IMAGELIST *;
#endif

/*
INITBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> HWND
*/
HB_FUNC_STATIC( INITBUTTON )
{
   void * WindowName;

   DWORD style = BS_NOTIFY | WS_CHILD | (hb_parl(14) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON); // JK

   if( hb_parl(10) ) {
      style |= BS_FLAT;
   }

   if( !hb_parl(11) ) {
      style |= WS_TABSTOP;
   }

   if( !hb_parl(12) ) {
      style |= WS_VISIBLE;
   }

   if( hb_parl(13) ) {
      style |= BS_MULTILINE;
   }

   hmg_ret_HWND(CreateWindowEx(0,
                               WC_BUTTON,
                               HB_PARSTR(2, &WindowName, nullptr),
                               style,
                               hmg_par_int(4),
                               hmg_par_int(5),
                               hmg_par_int(6),
                               hmg_par_int(7),
                               hmg_par_HWND(1),
                               hmg_par_HMENU(3),
                               GetInstance(),
                               nullptr));

   hb_strfree(WindowName);
}

/*
INITIMAGEBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> array
*/
HB_FUNC_STATIC( INITIMAGEBUTTON )
{
   HICON hIcon;
   int Transparent = hb_parl(10) ? 0 : 1;
   HIMAGELIST himl;
   BUTTON_IMAGELIST bi;

   void * WindowName;
   void * IconName;

   LPCTSTR lpIconName = HB_PARSTR(14, &IconName, nullptr);

   auto hwnd = hmg_par_HWND(1);

   DWORD style = BS_NOTIFY | WS_CHILD | (hb_parl(13) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON) | (hb_parc(14) == nullptr ? BS_BITMAP : BS_ICON); // JK

   if( hb_parl(9) ) {
      style |= BS_FLAT;
   }

   if( !hb_parl(11) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(12) ) {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(0,
                                 WC_BUTTON,
                                 HB_PARSTR(2, &WindowName, nullptr),
                                 style,
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(6),
                                 hmg_par_int(7),
                                 hwnd,
                                 hmg_par_HMENU(3),
                                 GetInstance(),
                                 nullptr);

   if( HB_ISNIL(14) ) {
      if( !hb_parl(17) ) {
         auto himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(8), -1, -1, hwnd, 0, Transparent, -1, 0, false, 255));
         SendMessage(hbutton, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));
         hb_reta(2);
         hmg_storvhandle(hbutton, -1, 1);
         hmg_storvhandle(himage, -1, 2);
      } else {
         himl = HMG_SetButtonImageList(hbutton, hb_parc(8), Transparent, BUTTON_IMAGELIST_ALIGN_CENTER);

         hb_reta(2);
         hmg_storvhandle(hbutton, -1, 1);
         hmg_storvhandle(himl, -1, 2);
      }
   } else {
      if( !hb_parl(15) ) {
         hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

         if( hIcon == nullptr ) {
            hIcon = static_cast<HICON>(LoadImage(0, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
         }
      } else {
         hIcon = static_cast<HICON>(ExtractIcon(GetInstance(), lpIconName, hb_parni(16)));

         if( hIcon == nullptr ) {
            hIcon = static_cast<HICON>(ExtractIcon(GetInstance(), TEXT("user.exe"), 0));
         }
      }

      if( hb_parl(17) ) {
         ICONINFO sIconInfo;
         GetIconInfo(hIcon, &sIconInfo);
         BITMAP bm;
         GetObject(sIconInfo.hbmColor, sizeof(BITMAP), static_cast<LPVOID>(&bm));

         himl = ImageList_Create(bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0);

         bi.himl          = himl;
         bi.margin.left   = 10;
         bi.margin.top    = 10;
         bi.margin.bottom = 10;
         bi.margin.right  = 10;
         bi.uAlign        = 4;

         ImageList_AddIcon(bi.himl, hIcon);

         SendMessage(hbutton, BCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(&bi));

         DeleteObject(sIconInfo.hbmMask);
         DeleteObject(sIconInfo.hbmColor);
         DestroyIcon(hIcon);

         hb_reta(2);
         hmg_storvhandle(hbutton, -1, 1);
         hmg_storvhandle(himl, -1, 2);
      } else {
         SendMessage(hbutton, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_ICON), reinterpret_cast<LPARAM>(hIcon));

         hb_reta(2);
         hmg_storvhandle(hbutton, -1, 1);
         hmg_storvhandle(hIcon, -1, 2);
      }
   }

   hb_strfree(WindowName);
   hb_strfree(IconName);
}

#pragma ENDDUMP
