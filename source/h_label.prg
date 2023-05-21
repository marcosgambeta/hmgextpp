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

FUNCTION _DefineLabel(ControlName, ParentFormName, x, y, Caption, w, h, ;
      fontname, fontsize, bold, BORDER, CLIENTEDGE, HSCROLL, VSCROLL, ;
      TRANSPARENT, aRGB_bk, aRGB_font, ProcedureName, tooltip, HelpId, invisible, ;
      italic, underline, strikeout, autosize, rightalign, centeralign, ;
      blink, mouseover, mouseleave, VCenterAlign, NoPrefix, nId, bInit, dblclick, rclick)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k := 0
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@ProcedureName, "")
   hb_default(@invisible, .F.)
   hb_default(@bold, .F.)
   hb_default(@italic, .F.)
   hb_default(@underline, .F.)
   hb_default(@strikeout, .F.)
   hb_default(@VCenterAlign, .F.)
   __defaultNIL(@rclick, "")

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

   IF hb_IsArray(Caption)
      mVar := ""
      AEval(Caption, {|v|mVar += cValToChar(v)})
      Caption := mVar
   ELSEIF hb_IsBlock(Caption)
      Caption := cValToChar(Eval(Caption))
   ELSE
      Caption := cValToChar(Caption)
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

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      Controlhandle := InitLabel(ParentFormHandle, Caption, 0, x, y, w, h, "", (hb_IsBlock(ProcedureName) .OR. hb_IsBlock(dblclick) .OR. hb_IsBlock(rclick) .OR. hb_IsString(tooltip)), (hb_IsBlock(mouseover) .OR. hb_IsBlock(mouseleave)), border, clientedge, HSCROLL, VSCROLL, TRANSPARENT, invisible, rightalign, centeralign, VCenterAlign, NoPrefix)

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

   _HMG_aControlType               [k] := CONTROL_TYPE_LABEL
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Nil
   _HMG_aControlInputMask          [k] := transparent
   _HMG_aControllostFocusProcedure [k] := mouseleave
   _HMG_aControlGotFocusProcedure  [k] := mouseover
   _HMG_aControlChangeProcedure    [k] := rclick
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := aRGB_bk
   _HMG_aControlFontColor          [k] := aRGB_font
   _HMG_aControlDblClick           [k] := _HMG_ActiveTabButtons
   _HMG_aControlHeadClick          [k] := dblclick
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := iif(autosize == .T., 1, 0)
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveTabName, "")
   _HMG_aControlRangeMax           [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameParentFormName[_HMG_FrameLevel], "")
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := {0, blink, .T.}
   _HMG_aControlMiscData2          [k] := ""

   IF blink == .T. .AND. !lDialogInMemory
      _DefineTimer("BlinkTimer" + hb_ntos(k), ParentFormName, 500, {||_HMG_aControlMiscData1[k][3] := !_HMG_aControlMiscData1[k][3], ;
         iif(_HMG_aControlMiscData1[k][3] == .T., _ShowControl(ControlName, ParentFormName), _HideControl(ControlName, ParentFormName))})
   ENDIF

   IF autosize == .T. .AND. !lDialogInMemory
      _SetControlWidth(ControlName, ParentFormName, GetTextWidth(NIL, Caption, FontHandle) + ;
         iif(bold == .T. .OR. italic == .T., GetTextWidth(NIL, " ", FontHandle), 0))
      _SetControlHeight(ControlName, ParentFormName, FontSize + iif(FontSize < 14, 12, 16))
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN Nil

FUNCTION InitDialogLabel(ParentFormName, ControlHandle, k)

   LOCAL ControlName := _HMG_aControlNames[k]

   IF _HMG_aControlMiscData1[k][2] == .T.
      _DefineTimer("BlinkTimer" + hb_ntos(k), ParentFormName, 500, {||_HMG_aControlMiscData1[k][3] := !_HMG_aControlMiscData1[k][3], ;
         iif(_HMG_aControlMiscData1[k][3] == .T., _ShowControl(ControlName, ParentFormName), _HideControl(ControlName, ParentFormName))})
   ENDIF

   IF _HMG_aControlSpacing[k] == 1
      _SetControlWidth(ControlName, ParentFormName, GetTextWidth(NIL, _HMG_aControlCaption[k], _HMG_aControlFontHandle[k]) + ;
         iif(_HMG_aControlFontAttributes[k][1] == .T. .OR. _HMG_aControlFontAttributes[k][2] == .T., ;
         GetTextWidth(NIL, " ", _HMG_aControlFontHandle[k]), 0))
      _SetControlHeight(ControlName, ParentFormName, _HMG_aControlFontSize[k] + iif(_HMG_aControlFontSize[k] < 14, 12, 16))
      RedrawWindow(ControlHandle)
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN Nil

FUNCTION OLABELEVENTS(hWnd, nMsg, wParam, lParam)

   LOCAL i := AScan(_HMG_aControlHandles, hWnd)

   HB_SYMBOL_UNUSED(wParam)
   HB_SYMBOL_UNUSED(lParam)

   IF i > 0

      SWITCH nMsg

      CASE WM_MOUSEMOVE
        _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
        EXIT

      CASE WM_MOUSELEAVE
        _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)

      ENDSWITCH

   ENDIF

RETURN 0

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbwinuni.h>

#ifndef WC_STATIC
#define WC_STATIC  "Static"
#endif

LRESULT APIENTRY LabelSubClassFunc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam);
static WNDPROC LabelOldWndProc;

HB_FUNC_STATIC( INITLABEL )
{
   HWND hWnd;
   HWND hWndParent = hmg_par_HWND(1);

   int style = WS_CHILD;
   int ExStyle = 0;

   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   if( hb_parl(9) || hb_parl(10) )
   {
      style |= SS_NOTIFY;
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

   if( hb_parl(19) )
   {
      style |= SS_CENTERIMAGE;
   }

   if( hb_parl(20) )
   {
      style |= SS_NOPREFIX;
   }

   if( hb_parl(12) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   if( hb_parl(15) )
   {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   hWnd = CreateWindowEx(ExStyle,
                         WC_STATIC,
                         lpWindowName,
                         style,
                         hmg_par_int(4),
                         hmg_par_int(5),
                         hmg_par_int(6),
                         hmg_par_int(7),
                         hWndParent,
                         hmg_par_HMENU(3),
                         GetInstance(),
                         nullptr);

   if( hb_parl(10) )
   {
      LabelOldWndProc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(LabelSubClassFunc)));
   }

   hmg_ret_HWND(hWnd);

   hb_strfree(WindowName);
}

#define _OLD_STYLE 0

LRESULT APIENTRY LabelSubClassFunc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   TRACKMOUSEEVENT tme;
   static PHB_SYMB pSymbol = nullptr;
   static bool bMouseTracking = false;
   long r = 0;

#if _OLD_STYLE
   bool bCallUDF = false;
#endif

   if( Msg == WM_MOUSEMOVE || Msg == WM_MOUSELEAVE )
   {
      if( Msg == WM_MOUSEMOVE )
      {
         if( bMouseTracking == false )
         {
            tme.cbSize      = sizeof(TRACKMOUSEEVENT);
            tme.dwFlags     = TME_LEAVE;
            tme.hwndTrack   = hWnd;
            tme.dwHoverTime = HOVER_DEFAULT;

            if( _TrackMouseEvent(&tme) == TRUE )
            {
#if _OLD_STYLE
               bCallUDF = true;
#endif
               bMouseTracking = true;
            }
#if _OLD_STYLE
         }
         else
         {
            bCallUDF = false;
#endif
         }
      }
      else
      {
#if _OLD_STYLE
         bCallUDF = true;
#endif
         bMouseTracking = false;
      }
#if _OLD_STYLE
      if( bCallUDF == true )
      {
#endif
      if( !pSymbol )
      {
         pSymbol = hb_dynsymSymbol(hb_dynsymGet("OLABELEVENTS"));
      }

      if( pSymbol && hb_vmRequestReenter() )
      {
         hb_vmPushSymbol(pSymbol);
         hb_vmPushNil();
         hmg_vmPushHandle(hWnd);
         hb_vmPushLong(Msg);
         hb_vmPushNumInt(wParam);
         hb_vmPushNumInt(lParam);
         hb_vmDo(4);

         r = hb_parnl(-1);

         hb_vmRequestRestore();
      }
#if _OLD_STYLE
   }
#endif
      return (r != 0) ? r : CallWindowProc(LabelOldWndProc, hWnd, 0, 0, 0);
   }

   bMouseTracking = false;

   return CallWindowProc(LabelOldWndProc, hWnd, Msg, wParam, lParam);
}
#undef _OLD_STYLE

#pragma ENDDUMP
