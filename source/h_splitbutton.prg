/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2017 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"

#ifdef _USERINIT_
//----------------------------------------------------------------------------//
INIT PROCEDURE _InitSPButton
//----------------------------------------------------------------------------//

   InstallEventHandler("SPButtonEventHandler")
   InstallMethodHandler("SetFocus", "SPButtonSetFocus")
   InstallMethodHandler("Enable", "SPButtonEnable")
   InstallMethodHandler("Disable", "SPButtonDisable")

RETURN

//----------------------------------------------------------------------------//
PROCEDURE _DefineSplitButton(cName, nRow, nCol, cCaption, bAction, cParent, ;
   lDefault, w, h, tooltip, fontname, fontsize, bold, italic, underline, strikeout)
//----------------------------------------------------------------------------//
   
   LOCAL hControlHandle
   LOCAL hParentFormHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL nId
   LOCAL k

   IF _HMG_BeginWindowActive
      cParent := _HMG_ActiveFormName
   ENDIF

   // If defined inside a Tab structure, adjust position and determine cParent

   IF _HMG_FrameLevel > 0
      nCol += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      nRow += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      cParent := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   IF !_IsWindowDefined(cParent)
      MsgMiniGuiError("Window: " + cParent + " is not defined.")
   ENDIF

   IF _IsControlDefined(cName, cParent)
      MsgMiniGuiError("Control: " + cName + " Of " + cParent + " Already defined.")
   ENDIF

   hb_default(@w, 148)
   hb_default(@h, 38)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   mVar := "_" + cParent + "_" + cName

   k := _GetControlFree()
   nId := _GetId()

   hParentFormHandle := GetFormHandle(cParent)

   hControlHandle := InitSplitButton(hParentFormHandle, nRow, nCol, cCaption, lDefault, w, h, nId)

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, hControlHandle)
   ENDIF

   IF !empty(FontHandle)
      _SetFontHandle(hControlHandle, FontHandle)
   ELSE
      __defaultNIL(@FontName, _HMG_DefaultFontName)
      __defaultNIL(@FontSize, _HMG_DefaultFontSize)
      FontHandle := _SetFont(hControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_SPBUTTON
   _HMG_aControlNames              [k] := cName
   _HMG_aControlHandles            [k] := hControlHandle
   _HMG_aControlParenthandles      [k] := hParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := bAction
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := NIL
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := nRow
   _HMG_aControlCol                [k] := nCol
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := NIL
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := cCaption
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := 0
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF tooltip != NIL
      SetToolTip(hControlHandle, tooltip, GetFormToolTipHandle(cParent))
   ENDIF

RETURN

#define WM_COMMAND   0x0111
#define BN_CLICKED   0
#define WM_NOTIFY    0x004E
#define BCN_FIRST    -1250
#define BCN_DROPDOWN (BCN_FIRST + 0x0002)
//----------------------------------------------------------------------------//
FUNCTION SPButtonEventHandler(hWnd, nMsg, wParam, lParam)
//----------------------------------------------------------------------------//
   
   LOCAL xRetVal // := NIL
   LOCAL i

   HB_SYMBOL_UNUSED(hWnd)

   IF nMsg == WM_NOTIFY

      IF GetNotifyCode(lParam) == BCN_DROPDOWN  // Notify for dropdown button
         xRetVal := 0
         LaunchDropdownMenu(GetHwndFrom(lParam))
      ENDIF

   ELSEIF nMsg == WM_COMMAND

      i := AScan(_HMG_aControlHandles, hmg_numbertohandle(lParam))

      IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_SPBUTTON

         IF HiWord(wParam) == BN_CLICKED
            xRetVal := 0
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
         ENDIF

      ENDIF

   ENDIF

RETURN xRetVal

#define BM_SETSTYLE        244
#define BS_SPLITBUTTON     0x0000000C
#define BS_DEFSPLITBUTTON  0x0000000D
//----------------------------------------------------------------------------//
PROCEDURE SPButtonSetFocus(cWindow, cControl)
//----------------------------------------------------------------------------//
   
   LOCAL hWnd
   LOCAL ParentFormHandle
   LOCAL ControlCount
   LOCAL x

   IF GetControlType(cControl, cWindow) == "SPBUTTON"

      _HMG_UserComponentProcess := .T.

      hWnd := GetControlHandle(cControl, cWindow)

      ControlCount := Len(_HMG_aControlNames)
      ParentFormHandle := _HMG_aControlParentHandles[GetControlIndex(cControl, cWindow)]
      FOR x := 1 TO ControlCount
         IF _HMG_aControlType[x] == CONTROL_TYPE_SPBUTTON
            IF _HMG_aControlParentHandles[x] == ParentFormHandle
               SendMessage(_HMG_aControlHandles[x], BM_SETSTYLE, BS_SPLITBUTTON, LOWORD(1))
            ENDIF
         ENDIF
      NEXT

      SetFocus(hWnd)
      SendMessage(hWnd, BM_SETSTYLE, BS_DEFSPLITBUTTON, LOWORD(1))

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

//----------------------------------------------------------------------------//
PROCEDURE SPButtonEnable(cWindow, cControl)
//----------------------------------------------------------------------------//

   IF GetControlType(cControl, cWindow) == "SPBUTTON"

      EnableWindow(GetControlHandle(cControl, cWindow))

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

//----------------------------------------------------------------------------//
PROCEDURE SPButtonDisable(cWindow, cControl)
//----------------------------------------------------------------------------//

   IF GetControlType(cControl, cWindow) == "SPBUTTON"

      DisableWindow(GetControlHandle(cControl, cWindow))

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

//----------------------------------------------------------------------------//
STATIC FUNCTION LaunchDropdownMenu(nHwnd)
//----------------------------------------------------------------------------//
   
   LOCAL aPos := {0, 0, 0, 0}
   LOCAL nIdx

   nIdx := AScan(_HMG_aControlHandles, nHwnd)

   IF nIdx > 0

      GetWindowRect(nHwnd, aPos)

      TrackPopupMenu(_HMG_aControlRangeMax[nIdx], aPos[1] + 1, aPos[2] + _HMG_aControlHeight[nIdx], _HMG_aControlParentHandles[nIdx])

   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
// Low Level C Routines
//----------------------------------------------------------------------------//

#pragma BEGINDUMP

#define BS_SPLITBUTTON     0x0000000C
#define BS_DEFSPLITBUTTON  0x0000000D

#include "mgdefs.hpp"
#include "hbwinuni.hpp"

/*
INITSPLITBUTTON(HWND, np2, np3, cp4, lp5, np6, np7, p8) --> HWND
*/
HB_FUNC( INITSPLITBUTTON )
{
   DWORD style = hb_parl(5) ? BS_DEFSPLITBUTTON : BS_SPLITBUTTON;

   void * str;
   auto hbutton = CreateWindowEx(0,
                                 "button",
                                 HB_PARSTR(4, &str, nullptr),
                                 style | WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | BS_PUSHBUTTON | WS_VISIBLE | WS_TABSTOP,
                                 hmg_par_int(3),
                                 hmg_par_int(2),
                                 hmg_par_int(6),
                                 hmg_par_int(7),
                                 hmg_par_HWND(1),
                                 hmg_par_HMENU(8),
                                 GetModuleHandle(nullptr),
                                 nullptr);

   hb_strfree(str);

   hmg_ret_HWND(hbutton);
}

#pragma ENDDUMP

#endif
