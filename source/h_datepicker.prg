//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
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
#include "i_winuser.ch"

#define DTS_UPDOWN          0x0001 // use UPDOWN instead of MONTHCAL
#define DTS_SHOWNONE        0x0002 // allow a NONE selection
#define DTS_RIGHTALIGN      0x0020 // right-align popup instead of left-align it

//---------------------------------------------------------------------------//
FUNCTION _DefineDatePick(ControlName, ParentFormName, x, y, w, h, value, ;
      fontname, fontsize, tooltip, change, lostfocus, ;
      gotfocus, shownone, updown, rightalign, HelpId, ;
      invisible, notabstop, bold, italic, underline, strikeout, ;
      Field, Enter, backcolor, fontcolor, titlebkclr, titlefrclr, trlfontclr, ;
      cDateFormat, dRangeMin, dRangeMax, nId, bInit)
//---------------------------------------------------------------------------//
   
   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL blInit
   LOCAL WorkArea
   LOCAL lDialogInMemory
   LOCAL oc // := NIL
   LOCAL ow // := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@value, BLANK_DATE)
   __defaultNIL(@change, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF Field != NIL
      IF hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified Field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            value := &(Field)
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

      IF shownone
         Style += DTS_SHOWNONE
      ENDIF

      IF updown
         Style += DTS_UPDOWN
      ENDIF

      IF rightalign
         Style += DTS_RIGHTALIGN
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF lDialogInMemory // Dialog Template
         hmg_InitExCommonControls(1)

         // {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogDatePicker(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "SysDateTimePick32", style, 0, x, y, w, h, "", HelpId, tooltip, fontname, fontsize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

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

      ControlHandle := hmg_InitDatePick(ParentFormHandle, 0, x, y, w, h, "", 0, shownone, updown, rightalign, invisible, notabstop)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF hmg_IsWindowHandle(ControlHandle)
            FontHandle := hmg__SetFont(ControlHandle, fontname, fontsize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
      ENDIF

      IF Empty(Value)
         hmg_SetDatePickNull(ControlHandle)
      ELSE
         hmg_SetDatePick(ControlHandle, Year(value), Month(value), Day(value))
      ENDIF

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_DATEPICK
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := Enter
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
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
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { backcolor, fontcolor, TitleBkClr, TitleFrClr, TrlFontClr }
   _HMG_aControlMiscData2          [k] := ""

   IF IsArrayRGB(BackColor)
      SetDatePickBkColor(ControlHandle, backcolor[1], backcolor[2], backcolor[3])
   ENDIF

   IF IsArrayRGB(FontColor)
      SetDatePickFontColor(ControlHandle, fontcolor[1], fontcolor[2], fontcolor[3])
   ENDIF

   IF hb_IsString(cDateFormat)

      IF hmg_SetDatePickerDateFormat(ControlHandle, cDateFormat)
         _HMG_aControlSpacing[k] := cDateFormat
      ELSE
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + ": Wrong format string.")
      ENDIF

   ELSE
      _HMG_aControlSpacing[k] := ""
   ENDIF

   IF hb_IsDate(dRangeMin) .OR. hb_IsDate(dRangeMax)
      IF !_SetDatePickerRange(ControlHandle, dRangeMin, dRangeMax, k)
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + ": Wrong date range.")
      ENDIF
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)

#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _DefineTimePick(ControlName, ParentFormName, x, y, w, h, value, ;
      fontname, fontsize, tooltip, change, lostfocus, ;
      gotfocus, shownone, HelpId, ;
      invisible, notabstop, bold, italic, underline, strikeout, ;
      Field, Enter, cTimeFormat, nId)
//---------------------------------------------------------------------------//
   
   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL blInit
   LOCAL WorkArea
   LOCAL lDialogInMemory

   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@value, iif(shownone, "", Time()))
   __defaultNIL(@change, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@cTimeFormat, "HH:mm:ss")

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF Field != NIL
      IF hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified Field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            value := &(Field)
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

      IF shownone
         Style += DTS_SHOWNONE
      ENDIF

      Style += DTS_UPDOWN

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF lDialogInMemory // Dialog Template
         hmg_InitExCommonControls(1)

         // {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogDatePicker(x, y, z )}
         AAdd(_HMG_aDialogItems, {nId, k, "SysDateTimePick32", style, 0, x, y, w, h, "", HelpId, tooltip, fontname, fontsize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

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

      ControlHandle := hmg_InitTimePick(ParentFormHandle, 0, x, y, w, h, "", 0, shownone, invisible, notabstop)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         FontHandle := hmg__SetFont(ControlHandle, fontname, fontsize, bold, italic, underline, strikeout)
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
      ENDIF

      IF Empty(Value)
         IF shownone
            hmg_SetDatePickNull(ControlHandle)
         ELSE
            hmg_SetTimePick(ControlHandle, Val(Left(Time(), 2)), Val(SubStr(Time(), 4, 2)), Val(SubStr(Time(), 7, 2)))
         ENDIF
      ELSE
         hmg_SetTimePick(ControlHandle, Val(Left(value, 2)), Val(SubStr(value, 4, 2)), Val(SubStr(value, 7, 2)))
      ENDIF

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_TIMEPICK
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := Enter
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := change
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
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
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

   IF hmg_SetDatePickerDateFormat(ControlHandle, cTimeFormat)
      _HMG_aControlSpacing[k] := cTimeFormat
      IF AScan(_HMG_aControlFontAttributes[k], .T.) > 0 .OR. ;
            fontname != _HMG_DefaultFontName .OR. fontsize != _HMG_DefaultFontSize
         _SetFontName(ControlName, ParentFormName, fontname)
      ENDIF
   ELSE
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + ": Wrong format string.")
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION InitDialogDatePicker(ParentFormName, ControlHandle, k)
//---------------------------------------------------------------------------//
   ParentFormName := NIL
   ControlHandle := NIL

   _SetValue(, , _HMG_aControlValue[k], k)
   // JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3] // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _SetDatePickerRange(ControlHandle, dRangeMin, dRangeMax, Index)
//---------------------------------------------------------------------------//
   
   LOCAL lOK

   hb_default(@dRangeMin, BLANK_DATE)
   hb_default(@dRangeMax, BLANK_DATE)

   IF (lOK := hmg_SetDatePickRange(ControlHandle, dRangeMin, dRangeMax))
      _HMG_aControlRangeMin[Index] := dRangeMin
      _HMG_aControlRangeMax[Index] := dRangeMax
   ENDIF

RETURN lOK

//---------------------------------------------------------------------------//
FUNCTION OPICKEVENTS(hWnd, nMsg, wParam, lParam)
//---------------------------------------------------------------------------//
   
   LOCAL i := AScan(_HMG_aControlHandles, hWnd)
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL hDC
   LOCAL hBrush

   HB_SYMBOL_UNUSED(lParam)

   hDC := wParam

   SWITCH nMsg

   CASE WM_ERASEBKGND

      IF i > 0
         IF _HMG_aControlBkColor[i] != NIL
            hmg_GetClientRect(_HMG_aControlHandles[i], /*@*/aRect)
            hBrush := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
            hmg_FillRect(hDC, aRect[1], aRect[2], aRect[3] - GETVSCROLLBARWIDTH(), aRect[4], hBrush)
            hmg_DeleteObject(hBrush)
            RETURN 1
         ENDIF
      ENDIF

   ENDSWITCH

RETURN 0
