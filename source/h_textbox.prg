//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// Modified 02.10.2005 to recognize insert/overwrite INS key status
// (C) Jacek Kubica <kubica@wssk.wroc.pl>
// HMG Experimental Build 10d
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

#include "SET_COMPILE_HMG_UNICODE.ch"

#include "minigui.ch"
#include "i_winuser.ch"

#define EM_SETCUEBANNER       0x1501

//---------------------------------------------------------------------------//
FUNCTION _DefineTextBox(ControlName, ParentFormName, x, y, w, h, ;
      cValue, FontName, FontSize, ToolTip, nMaxLength, lUpper, lLower, ;
      lNumeric, lPassword, uLostFocus, uGotFocus, uChange, uEnter, right, ;
      HelpId, readonly, bold, italic, underline, strikeout, field, ;
      backcolor, fontcolor, invisible, notabstop, noborder, cuetext, nId, bInit)
//---------------------------------------------------------------------------//

   LOCAL ParentFormHandle
   LOCAL ControlHandle := 0
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL WorkArea
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory

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

   IF Field != NIL
      IF  hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            cValue := &(Field)
         ENDIF
      ENDIF
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
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
      MsgMiniGuiError("Window: " + iif(ParentFormName == NIL, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   // Check if the control is already defined.
   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " of " + ParentFormName + " already defined.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive
      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_CHILD + ES_AUTOHSCROLL + BS_FLAT + iif(noborder, 0, WS_BORDER)

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

      IF readonly
         Style += ES_READONLY
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF  !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF Len(_HMG_aDialogTemplate) > 0        //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogTextBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "EDIT", style, 0, x, y, w, h, cValue, HelpId, Tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

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
      // Creates the control window
      ControlHandle := hmg_InitTextBox(ParentFormHandle, 0, x, y, w, h, "", 0, nMaxLength, lUpper, lLower, .F. , lPassword, right, readonly, invisible, notabstop, noborder)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF hmg_IsWindowHandle(ControlHandle)
            FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      // Add a tooltip if param has value
      IF ToolTip != NIL
         hmg_SetToolTip(ControlHandle, ToolTip, GetFormToolTipHandle(ParentFormName))
      ENDIF
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := iif(lNumeric, CONTROL_TYPE_NUMTEXT, CONTROL_TYPE_TEXT)
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
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
   _HMG_aControlFontName           [k] := FontName
   _HMG_aControlFontSize           [k] := FontSize
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := ToolTip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := nMaxLength
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := !invisible
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { 0, readonly }
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF !lDialogInMemory
      // With NUMERIC clause, transform numeric value into a string.
      IF lNumeric
         IF !hb_isChar(cValue)
            cValue := hb_ntos(cValue)
         ENDIF
      ENDIF

      // Fill the TEXTBOX with the text given.
      IF Len(cValue) > 0
         hmg_SetWindowText(ControlHandle, cValue)
      ENDIF

      IF !Empty(cuetext) .AND. IsVistaOrLater()
         SendMessageWideString(ControlHandle, EM_SETCUEBANNER, .T. /*show on focus*/, cuetext)
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF
   ENDIF

   Do_ControlEventProcedure(bInit, k)

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION InitDialogTextBox(ParentName, ControlHandle, k)
//---------------------------------------------------------------------------//

   LOCAL Field
   LOCAL cValue
   LOCAL nMaxLength
   LOCAL readonly
   LOCAL lNumeric

   Field       := _HMG_aControlPageMap[k]
   nMaxLength  := _HMG_aControlRangeMax[k]
   readonly    := _HMG_aControlMiscData1[k, 2]
   cValue      := _HMG_aControlValue[k]
   lNumeric    := ( _HMG_aControlType[k] == CONTROL_TYPE_NUMTEXT )

   IF hb_IsLogical(readonly)
      hmg_SendMessage(ControlHandle, EM_SETREADONLY, iif(readonly, 1, 0), 0)
   ENDIF
   IF nMaxLength != NIL
      hmg_SendMessage(ControlHandle, EM_LIMITTEXT, nMaxLength, 0)
   ENDIF

   // With NUMERIC clause, transform numeric value into a string.
   IF lNumeric
      IF !hb_isChar(cValue)
         cValue := hb_ntos(cValue)
      ENDIF
   ENDIF

   // Fill the TEXTBOX with the text given.
   IF Len(cValue) > 0
      hmg_SetWindowText(ControlHandle, cValue)
   ENDIF

   IF Field != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentName)], k)
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _DefineMaskedTextbox(ControlName, ParentFormName, x, y, inputmask, w, ;
      value, fontname, fontsize, tooltip, lostfocus, gotfocus, change, h, enter, ;
      rightalign, HelpId, Format, bold, italic, underline, strikeout, field, ;
      backcolor, fontcolor, readonly, invisible, notabstop, noborder, cuetext, nId, bInit)
//---------------------------------------------------------------------------//
   
   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL WorkArea
   LOCAL Style
   LOCAL blInit
   LOCAL c
   LOCAL i
   LOCAL lDialogInMemory

   HB_SYMBOL_UNUSED(RightAlign)

   IF Field != NIL
      IF  hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            Value := &(Field)
         ENDIF
      ENDIF
   ENDIF

   __defaultNIL(@Format, "")

   FOR i := 1 TO hb_ULen(InputMask)

      c := hb_USubStr(InputMask, i, 1)

      IF c != "9" .AND. c != "$" .AND. c != "*" .AND. c != "." .AND. c != "," .AND. c != " " .AND. c != "�"
         MsgMiniGuiError("@...TEXTBOX: Wrong InputMask Definition.")
      ENDIF

   NEXT i

   FOR i := 1 TO hb_ULen(Format)

      c := hb_USubStr(Format, i, 1)

      IF c != "C" .AND. c != "X" .AND. c != "(" .AND. c != "E"
         MsgMiniGuiError("@...TEXTBOX: Wrong Format Definition.")
      ENDIF

   NEXT i

   hb_default(@w, 120)
   hb_default(@h, 24)
   __defaultNIL(@Value, "")
   __defaultNIL(@change, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@enter, "")

   IF !Empty(Format)
      Format := "@" + AllTrim(Format)
   ENDIF

   InputMask := Format + " " + InputMask

   Value := Transform(value, InputMask)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
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

      Style := WS_CHILD + ES_AUTOHSCROLL + iif(noborder, 0, WS_BORDER)

      IF readonly
         Style += ES_READONLY
      ENDIF

      IF invisible
         Style += WS_VISIBLE
      ENDIF

      IF  !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogMaskedTextBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "EDIT", style, 0, x, y, w, h, Value, HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

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
      ControlHandle := hmg_InitMaskedTextBox(ParentFormHandle, 0, x, y, w, "", 0, 255, .F., .F., h, .T., readonly, invisible, notabstop, noborder)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF hmg_IsWindowHandle(ControlHandle)
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

   _HMG_aControlType               [k] := CONTROL_TYPE_MASKEDTEXT
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := InputMask
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := GetNumMask(InputMask)
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := Change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := enter
   _HMG_aControlHeadClick          [k] := Field
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
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := !invisible
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { 0, readonly }
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF !lDialogInMemory
      IF !Empty(cuetext) .AND. IsVistaOrLater()
         SendMessageWideString(ControlHandle, EM_SETCUEBANNER, .T. /*show on focus*/, cuetext)
      ENDIF

      hmg_SetWindowText(ControlHandle, value)

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF
   ENDIF

   Do_ControlEventProcedure(bInit, k)

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION InitDialogMaskedTextBox(ParentName, ControlHandle, k)
//---------------------------------------------------------------------------//
   
   LOCAL Field
   LOCAL cValue
   LOCAL date

   Field  := _HMG_aControlPageMap[k]
   date   := _HMG_aControlMiscData1[k, 2]
   cValue := _HMG_aControlValue[k]

   IF !date
      hmg_SetWindowText(ControlHandle, cValue)
   ELSE
      hmg_SetWindowText(ControlHandle, DToC(cValue))
   ENDIF

   IF Field != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentName)], k)
   ENDIF

// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION GetNumFromText(Text, i)
//---------------------------------------------------------------------------//
   
   LOCAL s As String
   LOCAL c
   LOCAL x

   FOR x := 1 TO hb_ULen(Text)

      c := hb_USubStr(Text, x, 1)

      IF hmg_IsDigit(c) .OR. c = "." .OR. c = "-"
         s += c
      ENDIF

   NEXT x

   IF hb_ULeft(AllTrim(Text), 1) == "(" .OR. hb_URight(AllTrim(Text), 2) == "DB"
      s := "-" + s
   ENDIF

   s := Transform(Val(s), _HMG_aControlInputMask[i])

RETURN Val(s)

//---------------------------------------------------------------------------//
STATIC FUNCTION GetNumMask(Text)
//---------------------------------------------------------------------------//
   
   LOCAL s As String
   LOCAL c
   LOCAL i

   FOR i := 1 TO hb_ULen(Text)

      c := hb_USubStr(Text, i, 1)

      IF c == "9" .OR. c == "."
         s += c
      ENDIF

      IF c == "$" .OR. c == "*"
         s += "9"
      ENDIF

   NEXT i

RETURN s

//---------------------------------------------------------------------------//
FUNCTION _DefineCharMaskTextbox(ControlName, ParentFormName, x, y, inputmask, ;
      w, value, fontname, fontsize, tooltip, lostfocus, gotfocus, change, ;
      h, enter, rightalign, HelpId, bold, italic, underline, strikeout, field, ;
      backcolor, fontcolor, date, readonly, invisible, notabstop, noborder, cuetext, nId, bInit)
//---------------------------------------------------------------------------//
   
   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL WorkArea
   LOCAL cChar
   LOCAL Style
   LOCAL blInit
   LOCAL lDialogInMemory

   IF Field != NIL
      IF  hb_UAt(">", Field) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " : You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(Field, hb_UAt(">", Field) - 2)
         IF Select(WorkArea) != 0
            Value := &(Field)
         ENDIF
      ENDIF
   ENDIF

   hb_default(@w, 120)
   hb_default(@h, 24)
   hb_default(@date, .F.)
   __defaultNIL(@change, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@enter, "")

   IF Value == NIL
      Value := iif(date, CToD("  /  /  "), "")
   ENDIF

   IF date
      InputMask := Set(_SET_DATEFORMAT)
      FOR EACH cChar IN "yYmMdD"
         InputMask := StrTran(InputMask, cChar, "9")
      NEXT 
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
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

      Style := WS_CHILD + ES_AUTOHSCROLL + iif(noborder, 0, WS_BORDER)

      IF readonly
         Style += ES_READONLY
      ENDIF

      IF invisible
         Style += WS_VISIBLE
      ENDIF

      IF !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogMaskedTextBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "EDIT", style, 0, x, y, w, h, Value, HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

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
      ControlHandle := hmg_InitCharMaskTextBox(ParentFormHandle, 0, x, y, w, "", 0, 255, .F., .F., h, rightalign, readonly, invisible, notabstop, noborder)

   ENDIF

   IF !lDialogInMemory

      IF !empty(FontHandle)
         hmg__SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF hmg_IsWindowHandle(ControlHandle)
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

   _HMG_aControlType               [k] := CONTROL_TYPE_CHARMASKTEXT
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := InputMask
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := Change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := enter
   _HMG_aControlHeadClick          [k] := date
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
   _HMG_aControlVisible            [k] := !invisible
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { 0, readonly }
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF !lDialogInMemory

      IF !Empty(cuetext) .AND. IsVistaOrLater()
         IF Empty(Value)
            Value := NIL
         ENDIF
         SendMessageWideString(ControlHandle, EM_SETCUEBANNER, .T. /*show on focus*/, cuetext)
      ENDIF

      IF Value != NIL
         IF !date
            hmg_SetWindowText(ControlHandle, Value)
         ELSE
            hmg_SetWindowText(ControlHandle, DToC(Value))
         ENDIF
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF
   ENDIF

   Do_ControlEventProcedure(bInit, k)

RETURN NIL

//---------------------------------------------------------------------------//
PROCEDURE ProcessCharMask(i, d)
//---------------------------------------------------------------------------//
   
   LOCAL OutBuffer As String
   LOCAL pc As Numeric
   LOCAL fnb As Numeric
   LOCAL dc As Numeric
   LOCAL ol As Numeric
   LOCAL BadEntry As Logical
   LOCAL pFlag As Logical
   LOCAL NegativeZero As Logical
   LOCAL InBuffer
   LOCAL BackInbuffer
   LOCAL InBufferLeft
   LOCAL InBufferRight
   LOCAL icp
   LOCAL CB
   LOCAL CM
   LOCAL Mask
   LOCAL OldChar
   LOCAL Output
   LOCAL ncp
   LOCAL x

   IF hb_IsLogical(_HMG_aControlSpacing[i])
      IF !_HMG_aControlSpacing[i]
         RETURN
      ENDIF
   ENDIF

   Mask := _HMG_aControlInputMask[i]

   // Store Initial CaretPos
   icp := hmg_HiWord(hmg_SendMessage(_HMG_aControlHandles[i], EM_GETSEL, 0, 0))

   // Get Current Content
   InBuffer := hmg_GetWindowText(_HMG_aControlHandles[i])

   // RL 104
   IF Left(AllTrim(InBuffer), 1) == "-" .AND. Val(InBuffer) == 0
      NegativeZero := .T.
   ENDIF

   HB_SYMBOL_UNUSED(d)

   IF PCount() > 1

      // Point Count For Numeric InputMask
      FOR x := 1 TO hb_ULen(InBuffer)
         CB := hb_USubStr(InBuffer, x, 1)
         IF CB == "." .OR. CB == ","
            pc++
         ENDIF
      NEXT x

      // RL 89
      IF hb_ULeft(InBuffer, 1) == "." .OR. hb_ULeft(InBuffer, 1) == ","
         pFlag := .T.
      ENDIF

      // Find First Non-Blank Position
      FOR x := 1 TO hb_ULen(InBuffer)
         CB := hb_USubStr(InBuffer, x, 1)
         IF CB != " "
            fnb := x
            EXIT
         ENDIF
      NEXT x

   ENDIF

   BackInBuffer := InBuffer

   OldChar := hb_USubStr(InBuffer, icp + 1, 1)

   IF Len(InBuffer) < Len(Mask)

      InBufferLeft := hb_ULeft(InBuffer, icp)

      InBufferRight := hb_URight(InBuffer, hb_ULen(InBuffer) - icp)

      IF CharMaskTekstOK(InBufferLeft + " " + InBufferRight, Mask) .AND. !CharMaskTekstOK(InBufferLeft + InBufferRight, Mask)
         InBuffer := InBufferLeft + " " + InBufferRight
      ELSE
         InBuffer := InBufferLeft + InBufferRight
      ENDIF

   ENDIF

   IF Len(InBuffer) > Len(Mask)

      InBufferLeft := hb_ULeft(InBuffer, icp)

      InBufferRight := hb_URight(InBuffer, hb_ULen(InBuffer) - icp - 1)

      InBuffer := InBufferLeft + InBufferRight

   ENDIF

   // Process Mask
   FOR x := 1 TO hb_ULen(Mask)

      CB := hb_USubStr(InBuffer, x, 1)
      CM := hb_USubStr(Mask, x, 1)

      SWITCH CM
      CASE "A"
      CASE "N"
      CASE "!"
         IF hmg_IsAlpha(CB) .OR. CB == " " .OR. ( ( CM == "N" .OR. CM == "!" ) .AND. hmg_IsDigit(CB) )
            IF CM == "!" .AND. !hmg_IsDigit(CB)
               OutBuffer += hmg_Upper(CB)
            ELSE
               OutBuffer += CB
            ENDIF
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT
      CASE "9"
         IF hmg_IsDigit(CB) .OR. CB == " " .OR. ( CB == "-" .AND. x == fnb .AND. PCount() > 1 )
            OutBuffer += CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT
      CASE " "
         IF CB == " "
            OutBuffer += CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT
      OTHERWISE
         IF CM == "X"  // GF 07/04/2022
            OutBuffer += CB
         ELSE
            OutBuffer += CM
         ENDIF
      ENDSWITCH

   NEXT x

   // Replace Content
   IF !( BackInBuffer == OutBuffer )
      hmg_SetWindowText(_HMG_aControlHandles[i], OutBuffer)
   ENDIF

   IF pc > 1

      pc := hb_UAt(".", OutBuffer)
      // RL 104
      IF NegativeZero

         Output := Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), Mask)

         Output := hb_URight(Output, ol - 1)

         Output := "-" + Output

         // Replace Text
         hmg_SetWindowText(_HMG_aControlhandles[i], Output)
         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, pc + dc, pc + dc)

      ELSE

         hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), Mask))
         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, pc + dc, pc + dc)

      ENDIF

   ELSE

      IF pFlag

         ncp := hb_UAt("." , hmg_GetWindowText(_HMG_aControlHandles[i]))
         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, ncp, ncp)

      ELSE

         // Restore Initial CaretPos
         IF BadEntry
            icp--
         ENDIF

         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, icp, icp)

         pc := hb_ULen(OutBuffer)

         // Skip Protected Characters
         FOR x := 1 TO pc

            CB := hb_USubStr(OutBuffer, icp + x, 1)
            CM := hb_USubStr(Mask, icp + x, 1)

            IF CM == "X"
               EXIT
            ENDIF
            IF !hmg_IsDigit(CB) .AND. !hmg_IsAlpha(CB) .AND. ( !( CB == " " ) .OR. ( CB == " " .AND. CM == " " ) )
               hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, icp + x, icp + x)
            ELSE
               EXIT
            ENDIF

         NEXT x

      ENDIF

   ENDIF

RETURN

//---------------------------------------------------------------------------//
STATIC FUNCTION CharMaskTekstOK(cString, cMask)
//---------------------------------------------------------------------------//
   
   LOCAL nCount := Min(hb_ULen(cString), hb_ULen(cMask))
   LOCAL lPassed := .F.
   LOCAL CB
   LOCAL CM
   LOCAL x

   FOR x := 1 TO nCount
      CB := hb_USubStr(cString, x, 1)
      CM := hb_USubStr(cMask, x, 1)
      SWITCH CM // JK
      CASE "A"
         lPassed := ( hmg_IsAlpha(CB) .OR. CB == " " )
         EXIT
      CASE "N"
      CASE "!"
         lPassed := ( hmg_IsDigit(CB) .OR. hmg_IsAlpha(CB) .OR. CB == " " )
         EXIT
      CASE "9"
         lPassed := ( hmg_IsDigit(CB) .OR. CB == " " )
         EXIT
      CASE " "
         lPassed := ( CB == " " )
         EXIT
      OTHERWISE
         lPassed := !( CM == "X" )  // GF 07/04/2022
      ENDSWITCH
      IF !lPassed
         EXIT
      ENDIF
   NEXT

RETURN lPassed

//---------------------------------------------------------------------------//
PROCEDURE _DataTextBoxRefresh(i)
//---------------------------------------------------------------------------//

   LOCAL Field

   IF _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT
      Field := _HMG_aControlHeadClick[i]
   ELSE
      Field := _HMG_aControlPageMap[i]
   ENDIF

   IF Field != NIL
      _SetValue(, , iif(Type(Field) == "C" , RTrim(&Field) , &Field) , i)
   ELSE
      hmg_RedrawWindow(_HMG_aControlHandles[i])
   ENDIF

RETURN

//---------------------------------------------------------------------------//
PROCEDURE _DataTextBoxSave(ControlName, ParentForm)
//---------------------------------------------------------------------------//
   
   LOCAL Field
   LOCAL i

   i := GetControlIndex(ControlName, ParentForm)

   IF _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT
      Field := _HMG_aControlHeadClick[i]
   ELSE
      Field := _HMG_aControlPageMap[i]
   ENDIF

   IF _IsFieldExists(Field)
      REPLACE &Field WITH _GetValue(Controlname, ParentForm)
   ENDIF

RETURN

//---------------------------------------------------------------------------//
PROCEDURE ProcessNumText(i)
//---------------------------------------------------------------------------//
   
   LOCAL BadEntry As Logical
   LOCAL OutBuffer As String
   LOCAL InBuffer
   LOCAL BackInBuffer
   LOCAL icp
   LOCAL x
   LOCAL CB
   LOCAL fnb

   // Store Initial CaretPos
   icp := hmg_HiWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0))

   // Get Current Content
   InBuffer := hmg_GetWindowText(_HMG_aControlHandles[i])

   BackInBuffer := InBuffer

   // Find First Non-Blank Position
   FOR x := 1 TO hb_ULen(InBuffer)

      CB := hb_USubStr(InBuffer, x, 1)

      IF CB != " "
         fnb := x
         EXIT
      ENDIF

   NEXT x

   // Process Mask
   FOR x := 1 TO hb_ULen(InBuffer)

      CB := hb_USubStr(InBuffer, x, 1)

      IF IsDigit(CB) .OR. ( CB == "-" .AND. x == fnb ) .OR. ;
         ( ( CB == "." .OR. CB == "," ) .AND. hb_UAt(".", OutBuffer) == 0 )

         OutBuffer += CB

      ELSE

         BadEntry := .T.

      ENDIF

   NEXT x

   IF BadEntry
      icp--
   ENDIF

   // JK Replace Content
   IF !( BackInBuffer == OutBuffer )
      hmg_SetWindowText(_HMG_aControlHandles[i], OutBuffer)
   ENDIF

   // Restore Initial CaretPos
   hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, icp, icp)

RETURN

//---------------------------------------------------------------------------//
FUNCTION GETNumFromTextSP(Text, i)
//---------------------------------------------------------------------------//

   LOCAL s As String
   LOCAL x
   LOCAL c

   FOR x := 1 TO hb_ULen(Text)

      c := hb_USubStr(Text, x, 1)

      IF hmg_IsDigit(c) .OR. c = "," .OR. c = "-" .OR. c = "."

         IF c == "."
            c := ""
         ENDIF

         IF c == ","
            c := "."
         ENDIF

         s += c

      ENDIF

   NEXT x

   IF hb_ULeft(AllTrim(Text), 1) == "(" .OR. hb_URight(AllTrim(Text), 2) == "DB"
      s := "-" + s
   ENDIF

   s := Transform(Val(s), _HMG_aControlInputMask[i])

RETURN Val(s)

//---------------------------------------------------------------------------//
FUNCTION OEDITEVENTS(hWnd, nMsg, wParam, lParam)
//---------------------------------------------------------------------------//

   LOCAL hTextBox
   LOCAL ParentForm
   LOCAL inbuffer
   LOCAL icp
   LOCAL icpe
   LOCAL i

   i := AScan(_HMG_aControlHandles, hWnd)

   SWITCH nMsg

   CASE WM_CHAR

      hTextBox := _HMG_aControlHandles[i]
      icp  := hmg_HiWord(hmg_SendMessage(hTextBox, EM_GETSEL, 0, 0))
      icpe := hmg_LoWord(hmg_SendMessage(hTextBox, EM_GETSEL, 0, 0))
      InBuffer := hmg_GetWindowText(hTextBox)

      // simulate overwrite mode
      IF !IsInsertActive() .AND. wParam != 13 .AND. wParam != 8 .AND. hb_USubStr(inBuffer, icp + 1, 1) != Chr(13)

#ifdef UNICODE
         IF hmg_IsAlpha(hb_UChar(wParam)) .OR. hmg_IsDigit(hb_UChar(wParam))
#else
         IF hmg_IsAlpha(Chr(wParam)) .OR. hmg_IsDigit(Chr(wParam))
#endif
            IF icp != icpe
               hmg_SendMessage(hTextBox, WM_CLEAR, 0, 0)
               hmg_SendMessage(hTextBox, EM_SETSEL, icpe, icpe)
            ELSE
               hmg_SendMessage(hTextBox, EM_SETSEL, icp, icp + 1)
               hmg_SendMessage(hTextBox, WM_CLEAR, 0, 0)
               hmg_SendMessage(hTextBox, EM_SETSEL, icp, icp)
            ENDIF

         ELSE

            IF wParam == 1
               hmg_SendMessage(hTextBox, EM_SETSEL, 0, -1)
            ENDIF

         ENDIF

      ELSE

         IF wParam == 1
            hmg_SendMessage(hTextBox, EM_SETSEL, 0, -1)
         ENDIF

      ENDIF
      EXIT

   CASE WM_CONTEXTMENU

      ParentForm := _HMG_aControlParentHandles[i]

      IF (i := AScan(_HMG_aControlsContextMenu, {|x|x[1] == hWnd})) > 0

         IF _HMG_aControlsContextMenu[i][4]
            hmg_setfocus(wParam)

            _HMG_xControlsContextMenuID := _HMG_aControlsContextMenu[i][3]

            hmg_TrackPopupMenu(_HMG_aControlsContextMenu[i][2] , hmg_LOWORD(lParam) , hmg_HIWORD(lParam) , ParentForm)

            RETURN 1
         ENDIF

      ENDIF

   ENDSWITCH

RETURN 0
