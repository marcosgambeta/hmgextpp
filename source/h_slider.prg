//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

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
#include "i_winuser.ch"

FUNCTION _DefineSlider(ControlName, ParentFormName, x, y, w, h, lo, hi, value, ;
   tooltip, scroll, change, vertical, noticks, both, top, left, HelpId, invisible, ;
   notabstop, backcolor, nId, enableselrange, nSelMin, nSelMax, bInit)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL blInit
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL lDialogInMemory
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, iif(vertical, 35 + iif(both, 5, 0), 120))
   hb_default(@h, iif(vertical, 120, 35 + iif(both, 5, 0)))
   hb_default(@value, Int((hi - lo) / 2))

   hb_default(@enableselrange, .F.) /* P.Ch. 16.10. */
   hb_default(@nSelMin, 0)
   hb_default(@nSelMax, 0)

   __defaultNIL(@scroll, "")
   __defaultNIL(@change, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)

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

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_CHILD

      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF vertical
         Style += TBS_VERT
      ENDIF

      IF !noticks
         Style += TBS_AUTOTICKS
      ELSE
         Style += TBS_NOTICKS
      ENDIF

      IF both
         Style += TBS_BOTH
      ENDIF
      IF top
         Style += TBS_TOP
      ENDIF
      IF left
         Style += TBS_LEFT
      ENDIF

      IF enableselrange
         Style += TBS_ENABLESELRANGE
      ENDIF

      IF Len(_HMG_aDialogTemplate) > 0 //Dialog Template
         /* TODO */ /* P.Ch. 16.10. */

         // {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogSlider(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "msctls_trackbar32", style, 0, x, y, w, h, "", HelpId, tooltip, "", 0, , , , , blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE
         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         hmg_SetWindowStyle(ControlHandle, Style, .T.)
      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)
      /* P.Ch. 16.10. */
      ControlHandle := hmg_InitSlider(ParentFormHandle, 0, x, y, w, h, lo, hi, vertical, noticks, both, top, left, invisible, notabstop, enableselrange, nSelMin, nSelMax)

   ENDIF

   IF !lDialogInMemory
      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      hmg_SendMessage(ControlHandle, TBM_SETPOS, 1, value)

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_SLIDER
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := scroll
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControlLostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := _HMG_ActiveTabButtons
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
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := Lo
   _HMG_aControlRangeMax           [k] := Hi
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveTabName, "")
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameParentFormName[_HMG_FrameLevel], "")
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN NIL

FUNCTION InitDialogSlider(ParentName, ControlHandle, k)

   IF ParentName != NIL
      hmg_SendMessage(ControlHandle, TBM_SETPOS, 1, _HMG_aControlValue[k])
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"
#include <commctrl.h>

/*
HMG_INITSLIDER(p1, p2, nX, nY, nWidth, nHeight, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) --> HWND
*/
HB_FUNC_STATIC( HMG_INITSLIDER )
{
   auto iSelMin = 0;
   auto iSelMax = 0;

   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_BAR_CLASSES;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD;

   if( hb_parl(9) )
   {
      style |= TBS_VERT;
   }

   style |= hb_parl(10) ? TBS_NOTICKS : TBS_AUTOTICKS;

   if( hb_parl(11) )
   {
      style |= TBS_BOTH;
   }

   if( hb_parl(12) )
   {
      style |= TBS_TOP;
   }

   if( hb_parl(13) )
   {
      style |= TBS_LEFT;
   }

   if( !hb_parl(14) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(15) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(16) )
   { /* P.Ch. 16.10 */
      style |= TBS_ENABLESELRANGE;
      iSelMin = HB_MIN(hb_parnidef(17, 0), hb_parnidef(18, 0));
      iSelMax = HB_MAX(hb_parnidef(17, 0), hb_parnidef(18, 0));
   }

   auto hTrackBar = CreateWindowEx(0,
                                   TRACKBAR_CLASS,
                                   nullptr,
                                   style,
                                   hmg_par_int(3),
                                   hmg_par_int(4),
                                   hmg_par_int(5),
                                   hmg_par_int(6),
                                   hmg_par_HWND(1),
                                   hmg_par_HMENU(2),
                                   GetInstance(),
                                   nullptr);

   SendMessage(hTrackBar, TBM_SETRANGE, static_cast<WPARAM>(TRUE), MAKELONG(hb_parni(7), hb_parni(8)));

   if( hb_parl(16) && (iSelMin != iSelMax) )
   {
      SendMessage(hTrackBar, TBM_SETSEL, static_cast<WPARAM>(TRUE), MAKELONG(iSelMin, iSelMax));  /* P.Ch. 16.10 */
   }

   hmg_ret_HWND(hTrackBar);
}

#pragma ENDDUMP
