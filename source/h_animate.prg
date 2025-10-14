//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// ANIMATERES Control Source Code
// Copyright 2011 Grigory Filatov <gfilatov@gmail.com>
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

#ifdef _USERINIT_

INIT PROCEDURE _InitAnimateRes()

   InstallMethodHandler("Release", "ReleaseAnimateRes")
   InstallPropertyHandler("File", "SetAnimateResFile", "GetAnimateResFile")
   InstallPropertyHandler("ResId", "SetAnimateResId", "GetAnimateResId")

RETURN

FUNCTION _DefineAnimateRes(ControlName, ParentForm, x, y, w, h, cFile, nRes, tooltip, HelpId, invisible)

   LOCAL ControlHandle
   LOCAL hAvi
   LOCAL cParentForm
   LOCAL mVar
   LOCAL k

   hb_default(@w, 200)
   hb_default(@h, 50)
   hb_default(@invisible, .F.)

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentForm := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   IF !_IsWindowDefined(ParentForm)
      MsgMiniGuiError("Window: " + ParentForm + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentForm)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + " Already defined.")
   ENDIF

   mVar := "_" + ParentForm + "_" + ControlName
   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   cParentForm := ParentForm

   ParentForm := GetFormHandle(ParentForm)

   ControlHandle := hmg_InitAnimateRes(ParentForm, @hAvi, x, y, w, h, cFile, nRes, invisible)

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
   ENDIF

   IF tooltip != NIL
      hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(cParentForm))
   ENDIF

   _HMG_aControlType               [k] := CONTROL_TYPE_ANIMATERES
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentForm
   _HMG_aControlIds                [k] := nRes
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := cFile
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
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
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := hAvi
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, Len(_HMG_aControlNames), mVar)
   ENDIF

RETURN NIL

FUNCTION SetAnimateResFile(cWindow, cControl, cProperty, cValue)

   IF GetControlType(cControl, cWindow) == CONTROL_TYPE_ANIMATERES .AND. Upper(cProperty) == "FILE"

      _HMG_UserComponentProcess := .T.

      _HMG_aControlValue[GetControlIndex(cControl, cWindow)] := cValue

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

FUNCTION GetAnimateResFile(cWindow, cControl)

   LOCAL RetVal

   IF GetControlType(cControl, cWindow) == CONTROL_TYPE_ANIMATERES

      _HMG_UserComponentProcess := .T.

      RetVal := _HMG_aControlValue[GetControlIndex(cControl, cWindow)]

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

FUNCTION SetAnimateResId(cWindow, cControl, cProperty, cValue)

   IF GetControlType(cControl, cWindow) == CONTROL_TYPE_ANIMATERES .AND. Upper(cProperty) == "RESID"

      _HMG_UserComponentProcess := .T.

      _HMG_aControlIds[GetControlIndex(cControl, cWindow)] := cValue

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

FUNCTION GetAnimateResId(cWindow, cControl)

   LOCAL RetVal

   IF GetControlType(cControl, cWindow) == CONTROL_TYPE_ANIMATERES

      _HMG_UserComponentProcess := .T.

      RetVal := _HMG_aControlIds[GetControlIndex(cControl, cWindow)]

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

PROCEDURE ReleaseAnimateRes(cWindow, cControl)

   IF _IsControlDefined(cControl, cWindow) .AND. GetControlType(cControl, cWindow) == CONTROL_TYPE_ANIMATERES

      hmg_UnloadAnimateLib(_GetControlObject(cControl, cWindow))

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

// Low Level C++ Routines

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <hbwinuni.h>
#include <mmsystem.h>
#include <commctrl.h>

/*
HMG_INITANIMATERES(p1, p2, nX, nY, nWidth, nHeight, p7, p8, p9) --> HWND
*/
HB_FUNC_STATIC(HMG_INITANIMATERES)
{
   void * DllName;

   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_ANIMATE_CLASS;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD | WS_VISIBLE | ACS_TRANSPARENT | ACS_CENTER | ACS_AUTOPLAY;

   if (!hb_parl(9))
   {
      style |= WS_VISIBLE;
   }

   HINSTANCE avi = LoadLibrary(HB_PARSTR(7, &DllName, nullptr));

   auto AnimationCtrl = CreateWindowEx(0,
                                       ANIMATE_CLASS,
                                       nullptr,
                                       style,
                                       hmg_par_int(3),
                                       hmg_par_int(4),
                                       hmg_par_int(5),
                                       hmg_par_int(6),
                                       hmg_par_HWND(1),
                                       hmg_par_HMENU(2),
                                       avi,
                                       nullptr);

   Animate_OpenEx(AnimationCtrl, avi, MAKEINTRESOURCE(hb_parni(8)));

   HB_STORNL(reinterpret_cast<LONG_PTR>(avi), 2);

   hmg_ret_HWND(AnimationCtrl);

   hb_strfree(DllName);
}

/*
HMG_UNLOADANIMATELIB(HINSTANCE) --> NIL
*/
HB_FUNC_STATIC(HMG_UNLOADANIMATELIB)
{
   HINSTANCE hLib = hmg_par_HINSTANCE(1);

   if (hLib != nullptr)
   {
      FreeLibrary(hLib);
   }
}

#pragma ENDDUMP

#endif
