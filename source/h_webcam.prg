/*
  MINIGUI - Harbour Win32 GUI library source code

  Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
  http://harbourminigui.googlepages.com/

  WEBCAM Control Source Code
  Copyright 2012 Grigory Filatov <gfilatov@gmail.com>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this software; see the file COPYING. If not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
  visit the web site http://www.gnu.org/).

  As a special exception, you have permission for additional uses of the text
  contained in this release of Harbour Minigui.

  The exception is that, if you link the Harbour Minigui library with other
  files to produce an executable, this does not by itself cause the resulting
  executable to be covered by the GNU General Public License.
  Your use of that executable is in no way restricted on account of linking the
  Harbour-Minigui library code into it.

  Parts of this project are based upon:

  "Harbour GUI framework for Win32"
  Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
  Copyright 2001 Antonio Linares <alinares@fivetech.com>
  www - https://harbour.github.io/

  "Harbour Project"
  Copyright 1999-2022, https://harbour.github.io/

  "WHAT32"
  Copyright 2002 AJ Wos <andrwos@aust1.net>

  "HWGUI"
  Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
*/

#include "minigui.ch"

#ifdef _USERINIT_
#include "i_winuser.ch"

INIT PROCEDURE _InitWebCam

   InstallMethodHandler("Start", "_StartWebCam")
   InstallMethodHandler("Release", "_ReleaseWebCam")

RETURN

FUNCTION _DefineWebCam(ControlName, ParentForm, x, y, w, h, lStart, nRate, TOOLTIP, HelpId)

   LOCAL ControlHandle
   LOCAL cParentForm
   LOCAL mVar
   LOCAL k

   hb_default(@w, 320)
   hb_default(@h, 240)
   hb_default(@nRate, 30)

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x := x + _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y := y + _HMG_ActiveFrameRow[_HMG_FrameLevel]
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
   PUBLIC &mVar. := k
#endif

   cParentForm := ParentForm

   ParentForm := GetFormHandle(ParentForm)

   ControlHandle := _CreateWebCam(ParentForm, x, y, w, h)

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
   ENDIF

   IF ValType(tooltip) != "U"
      SetToolTip(ControlHandle, TOOLTIP, GetFormToolTipHandle(cParentForm))
   ENDIF

   _HMG_aControlType               [k] := CONTROL_TYPE_WEBCAM
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentForm
   _HMG_aControlIds                [k] := 0
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := nRate
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := {}
   _HMG_aControlFontColor          [k] := {}
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
   _HMG_aControlContainerHandle    [k] := 0
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := .F.
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := 0
   _HMG_aControlBrushHandle        [k] := 0
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

   IF lStart
      IF !_StartWebCam(cParentForm, ControlName)
         MsgAlert("Webcam service is unavailable!", "Alert")
      ENDIF
   ENDIF

RETURN NIL

FUNCTION _CreateWebCam(ParentForm, x, y, w, h)
RETURN cap_CreateCaptureWindow("WebCam", hb_bitOr(WS_CHILD, WS_VISIBLE), y, x, w, h, ParentForm, 0)

FUNCTION _StartWebCam(cWindow, cControl)

   LOCAL hWnd
   LOCAL w
   LOCAL h
   LOCAL nTry := 1
   LOCAL lSuccess

   hWnd := GetControlHandle(cControl, cWindow)

   REPEAT
      lSuccess := cap_DriverConnect(hWnd, 0)
      DO EVENTS
   UNTIL (lSuccess != .T. .AND. nTry++ < 3)

   IF lSuccess
      w := _GetControlWidth(cControl, cWindow)
      h := _GetControlHeight(cControl, cWindow)
      cap_SetVideoFormat(hWnd, Min(w, 320), Min(h, 240))
      lSuccess := (cap_PreviewScale(hWnd, .T.) .AND. cap_PreviewRate(hWnd, GetControlValue(cControl, cWindow)) .AND. cap_Preview(hWnd, .T.))
   ELSE
      // error connecting to video source
      DestroyWindow(hWnd)
   ENDIF

   _HMG_aControlVisible[ GetControlIndex(cControl, cWindow) ] := lSuccess

RETURN lSuccess

PROCEDURE _ReleaseWebCam(cWindow, cControl)

   LOCAL hWnd

   IF _IsControlDefined(cControl, cWindow) .AND. GetControlType(cControl, cWindow) == CONTROL_TYPE_WEBCAM

      hWnd := GetControlHandle(cControl, cWindow)

      IF !Empty(hWnd)
         cap_DriverDisconnect(hWnd)
         DestroyWindow(hWnd)
         _EraseControl(GetControlIndex(cControl, cWindow), GetFormIndex(cWindow))
      ENDIF

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

#endif
