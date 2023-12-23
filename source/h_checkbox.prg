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

FUNCTION _DefineCheckBox(ControlName, ParentFormName, x, y, Caption, Value, ;
      fontname, fontsize, tooltip, changeprocedure, w, h, lostfocus, gotfocus, ;
      HelpId, invisible, notabstop, bold, italic, underline, strikeout, field, ;
      backcolor, fontcolor, transparent, leftjustify, threestate, Enter, autosize, multiline, nId, bInit)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL WorkArea
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 100)
   hb_default(@h, 28)
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@changeprocedure, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@transparent, .F.)
   hb_default(@leftjustify, .F.)
   hb_default(@multiline, .F.)
   hb_default(@threestate, .F.)
   IF !threestate
      hb_default(@value, .F.)
   ENDIF
   hb_default(@autosize, .F.)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF Field != NIL
      IF hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            Value := &(Field)
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

   IF transparent .AND. _HMG_FrameLevel == 0 .AND. _HMG_IsThemed // Fixed for transparent problem at themed WinXP and later
      transparent := .F.
      mVar := _HMG_aFormBkColor[GetFormIndex(ParentFormName)]
      IF backcolor == NIL .AND. mVar[1] < 0 .AND. mVar[2] < 0 .AND. mVar[3] < 0
         k := GetSysColor(COLOR_BTNFACE)
         backcolor := nRGB2Arr(k)
      ELSE
         backcolor := mVar
      ENDIF
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle
      Style := BS_NOTIFY + WS_CHILD  // + BS_LEFT

      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF
      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF threestate
         Style += BS_AUTO3STATE
      ELSE
         Style += BS_AUTOCHECKBOX
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //           {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogCheckButton(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "button", style, 0, x, y, w, h, caption, HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

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
      Controlhandle := InitCheckBox(ParentFormHandle, Caption, 0, x, y, multiline, threestate, w, h, invisible, notabstop, leftjustify, transparent)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(ControlHandle)
            FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_IsThemed .AND. IsArrayRGB(fontcolor)
         SetWindowTheme(ControlHandle, "", "")
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
      ENDIF

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_CHECKBOX
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := Enter
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := transparent
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := changeprocedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := _HMG_ActiveTabButtons
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := threestate
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
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

   IF !lDialogInMemory
      IF threestate .AND. value == NIL
         SendMessage(Controlhandle, BM_SETCHECK, BST_INDETERMINATE, 0)
      ELSEIF value
         SendMessage(Controlhandle, BM_SETCHECK, BST_CHECKED, 0)
      ENDIF
      IF autosize
         _SetControlWidth(ControlName, ParentFormName, hmg_GetTextWidth(NIL, Caption, FontHandle) + ;
            iif(bold .OR. italic, hmg_GetTextWidth(NIL, " ", FontHandle), 0) + 20)
         _SetControlHeight(ControlName, ParentFormName, FontSize + iif(FontSize < 14, 12, 16))
         hmg_RedrawWindow(ControlHandle)
      ENDIF
   ENDIF

   IF Field != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
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

FUNCTION _DefineCheckButton(ControlName, ParentFormName, x, y, Caption, Value, ;
      fontname, fontsize, tooltip, changeprocedure, ;
      w, h, lostfocus, gotfocus, HelpId, invisible, ;
      notabstop, bold, italic, underline, strikeout, nId)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory

   hb_default(@value, .F.)
   hb_default(@w, 100)
   hb_default(@h, 28)
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@changeprocedure, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)

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

      Style := BS_NOTIFY + WS_CHILD + BS_AUTOCHECKBOX + BS_PUSHLIKE

      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //          {{ID,k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogCheckButton(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "button", style, 0, x, y, w, h, "", HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         SetWindowStyle(ControlHandle, Style, .T.)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      Controlhandle := InitCheckButton(ParentFormHandle, Caption, 0, x, y, "", 0, w, h, invisible, notabstop)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(ControlHandle)
            FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_CHECKBOX
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := changeprocedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := .F.
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
   _HMG_aControlMiscData1          [k] := 2
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF value .AND. !lDialogInMemory
      SendMessage(Controlhandle, BM_SETCHECK, BST_CHECKED, 0)
   ENDIF

RETURN NIL

FUNCTION InitDialogCheckButton(ParentName, ControlHandle, k)

   LOCAL Value
   LOCAL BitMap
   LOCAL threestate

   BitMap := _HMG_aControlPicture[k]
   Value := _HMG_aControlValue[k]
   threestate := _HMG_aControlSpacing[k]
   IF !Empty(BitMap) .AND. ParentName != NIL
      hmg__SetBtnPicture(ControlHandle, BitMap)
   ENDIF
   IF value
      SendMessage(Controlhandle, BM_SETCHECK, BST_CHECKED, 0)
   ELSEIF threestate .AND. value == NIL
      SendMessage(Controlhandle, BM_SETCHECK, BST_INDETERMINATE, 0)
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

FUNCTION _DefineImageCheckButton(ControlName, ParentFormName, x, y, BitMap, ;
      Value, fontname, fontsize, tooltip, ;
      changeprocedure, w, h, lostfocus, gotfocus, ;
      HelpId, invisible, notabstop, nId, notrans)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL aRet
   LOCAL nhImage
   LOCAL mVar
   LOCAL k
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory

   hb_default(@value, .F.)
   hb_default(@w, 100)
   hb_default(@h, 28)
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@changeprocedure, "")
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@notrans, .F.)

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

      Style := BS_NOTIFY + BS_BITMAP + WS_CHILD + BS_AUTOCHECKBOX + BS_PUSHLIKE
      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF
      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF lDialogInMemory         //Dialog Template
         //          {{ID,k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogCheckButton(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "button", style, 0, x, y, w, h, "", HelpId, tooltip, FontName, FontSize, , , , , blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})
      ELSE
         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         SetWindowStyle(ControlHandle, Style, .T.)

         hmg__SetBtnPicture(ControlHandle, bitmap)
      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      aRet := InitImageCheckButton(ParentFormHandle, "", 0, x, y, "", notrans, bitmap, w, h, invisible, notabstop, _HMG_IsThemed)

      ControlHandle := aRet[1]
      nhImage := aRet[2]

   ENDIF

   IF !lDialogInMemory
      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_CHECKBOX
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := changeprocedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := notrans
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := BitMap
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
   _HMG_aControlBrushHandle        [k] := nhImage
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 1
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF value .AND. !lDialogInMemory
      SendMessage(Controlhandle, BM_SETCHECK, BST_CHECKED, 0)
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbwinuni.h>

#ifndef WC_BUTTON
#define WC_BUTTON         "Button"
#endif

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
INITCHECKBOX(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11, par12, par13) --> HWND
*/
HB_FUNC_STATIC( INITCHECKBOX )
{
   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   DWORD style = BS_NOTIFY | WS_CHILD;

   if( !hb_parl(10) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(11) ) {
      style |= WS_TABSTOP;
   }

   if( hb_parl(12) ) {
      style |= BS_LEFTTEXT;
   }

   if( hb_parl(6) ) {
      style |= BS_MULTILINE;
   }

   style |= hb_parl(7) ? BS_AUTO3STATE : BS_AUTOCHECKBOX;

   DWORD ExStyle = 0;

   if( hb_parl(13) ) {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   hmg_ret_HWND(CreateWindowEx(ExStyle,
                               WC_BUTTON,
                               lpWindowName,
                               style,
                               hmg_par_int(4),
                               hmg_par_int(5),
                               hmg_par_int(8),
                               hmg_par_int(9),
                               hmg_par_HWND(1),
                               hmg_par_HMENU(3),
                               GetInstance(),
                               nullptr));

   hb_strfree(WindowName);
}

/*
INITCHECKBUTTON(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11) --> HWND
*/
HB_FUNC_STATIC( INITCHECKBUTTON )
{
   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   DWORD style = BS_NOTIFY | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl(10) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(11) ) {
      style |= WS_TABSTOP;
   }

   hmg_ret_HWND(CreateWindowEx(0,
                               WC_BUTTON,
                               lpWindowName,
                               style,
                               hmg_par_int(4),
                               hmg_par_int(5),
                               hmg_par_int(8),
                               hmg_par_int(9),
                               hmg_par_HWND(1),
                               hmg_par_HMENU(3),
                               GetInstance(),
                               nullptr));

   hb_strfree(WindowName);
}

/*
INITIMAGECHECKBUTTON(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11, par12, par13) --> HWND
*/
HB_FUNC_STATIC( INITIMAGECHECKBUTTON )
{
   HWND       himage;
   HIMAGELIST himl;

   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   int Transparent = hb_parl(7) ? 0 : 1;

   auto hwnd = hmg_par_HWND(1);

   DWORD style = BS_NOTIFY | BS_BITMAP | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl(11) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(12) ) {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(0,
                                 WC_BUTTON,
                                 lpWindowName,
                                 style,
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(9),
                                 hmg_par_int(10),
                                 hwnd,
                                 hmg_par_HMENU(3),
                                 GetInstance(),
                                 nullptr);

   if( !hb_parl(13) ) {
      himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(8), -1, -1, hwnd, 0, Transparent, -1, 0, false, 255));

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

   hb_strfree(WindowName);
}

#pragma ENDDUMP
