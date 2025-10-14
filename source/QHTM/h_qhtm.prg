//
//  MINIGUI - Harbour Win32 GUI library source code
//
//  Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
//  http://harbourminigui.googlepages.com/
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
// Copyright 2001 Alexander S.Kresin <alex@belacy.ru>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2020, https://harbour.github.io/

#include "minigui.ch"
#include "i_QHTM.ch"

#define WS_BORDER           0x00800000
#define WM_SETREDRAW        0x0b

/******
*
*       _DefineQhtm(ControlName, ParentForm, x, y, w, h, Value, fname, resname, ;
*                   fontname, fontsize, Change, lBorder, nId)
*
*       Define QHTM control
*
*/
FUNCTION _DefineQhtm(ControlName, ParentForm, x, y, w, h, Value, fname, resname, fontname, fontsize, Change, lBorder, nId)

   LOCAL mVar
   LOCAL k := 0
   LOCAL ControlHandle
   LOCAL ParentFormHandle
   LOCAL FontHandle
   LOCAL bold
   LOCAL italic
   LOCAL underline
   LOCAL strikeout

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF _HMG_BeginWindowActive

      ParentForm := _HMG_ActiveFormName

      IF !Empty(ParentForm) .AND. FontName == NIL
         FontName := _HMG_ActiveFontName
      ENDIF

      IF !Empty(ParentForm) .AND. FontSize == NIL
         FontSize := _HMG_ActiveFontSize
      ENDIF

   ENDIF

   IF !_IsWindowDefined(ParentForm)
      MsgMiniGuiError("Window: " + ParentForm + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentForm)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + " Already defined.")
   ENDIF

   hb_default(@nId, _GetId())

   mVar := "_" + ParentForm + "_" + ControlName

   ParentFormHandle := GetFormHandle(ParentForm)

   ControlHandle := CreateQHTM(ParentFormHandle, nId, Iif(lBorder, WS_BORDER, 0), y, x, w, h)

   IF FontHandle != 0
      hmg__SetFontHandle(ControlHandle, FontHandle)
   ELSE
      __defaultNIL(@FontName, _HMG_DefaultFontName)
      __defaultNIL(@FontSize, _HMG_DefaultFontSize)
      FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
   ENDIF

   IF hb_IsChar(Value)
      hmg_SetWindowText(ControlHandle, Value)   // define from a variable
   ELSEIF hb_IsChar(fname)
      QHTM_LoadFile(ControlHandle, fname)   // loading from a file
   ELSEIF hb_IsChar(resname)
      QHTM_LoadRes(ControlHandle, resname)  // loading from a resource
   ENDIF

   QHTM_FormCallBack(ControlHandle)         // set a handling procedure of the web-form

   k := _GetControlFree()

   PUBLIC &mVar. := k

   _HMG_aControlType               [k] := CONTROL_TYPE_QHTM
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := Change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := x
   _HMG_aControlCol                [k] := y
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
   _HMG_aControlToolTip            [k] := ""
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := 0
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

RETURN NIL

/******
*
*       QHTM_LoadFromVal(ControlName, ParentForm, cValue)
*
*       Load web-page from variable
*
*/
Procedure QHTM_LoadFromVal(ControlName, ParentForm, cValue)
Local nHandle := GetControlHandle(ControlName, ParentForm)

If nHandle > 0
   hmg_SetWindowText(nHandle, cValue)
Endif

Return

/******
*
*       QHTM_LoadFromFile(ControlName, ParentForm, cFile)
*
*       Load web-page from file
*
*/
Procedure QHTM_LoadFromFile(ControlName, ParentForm, cFile)
Local nHandle := GetControlHandle(ControlName, ParentForm)

If nHandle > 0
   QHTM_LoadFile(nHandle, cFile)
Endif

Return

/******
*
*       QHTM_LoadFromRes(ControlName, ParentForm, cResName)
*
*       Load web-page from resource
*
*/
Procedure QHTM_LoadFromRes(ControlName, ParentForm, cResName)
Local nHandle := GetControlHandle(ControlName, ParentForm)

If nHandle > 0
   QHTM_LoadRes(nHandle, cResName)
Endif

Return

/******
*
*       QHTM_GetLink(lParam)
*
*       Receive QHTM link
*
*/
Function QHTM_GetLink(lParam)
Local cLink := QHTM_GetNotify(lParam)

QHTM_SetReturnValue(lParam, .F.)

Return cLink

/******
*
*       QHTM_ScrollPos(nHandle, nPos)
*
*       nHandle - descriptor of QHTM
*       nPos - old/new position of scrollbar
*       
*       Get/Set position of scrollbar QHTM
*
*/
Function QHTM_ScrollPos(nHandle, nPos)
Local nParamCount := PCount()

Switch nParamCount

   Case 0  // no params
     nPos := 0
     Exit

   Case 1
     If hb_IsNumeric(nHandle)
        nPos := QHTM_GetScrollPos(nHandle)
     Endif
     Exit

   Case 2
     If hb_IsNumeric(nHandle) .AND. hb_IsNumeric(nPos)
        QHTM_SetScrollPos(nHandle, nPos)
     Else
        nPos := 0
     Endif
   
End Switch

Return nPos

/******
*
*       QHTM_ScrollPercent(nHandle, nPercent)
*
*       nHandle  - descriptor of QHTM
*       nPercent - old/new position of scrollbar (in percentage)
*       
*       Get/Set position of scrollbar QHTM
*
*/
Function QHTM_ScrollPercent(nHandle, nPercent)
Local nParamCount := PCount(), ;
      nHeight                , ;
      aSize                  , ;
      nPos

If hb_IsNumeric(nHandle)

   nHeight := GetWindowHeight(nHandle)
   aSize := QHTM_GetSize(nHandle)

   // an amendment on a height of the QHTM

   If aSize[2] > nHeight
      aSize[2] -= nHeight
   Endif

Endif

Switch nParamCount

   Case 0
     nPercent := 0
     Exit

   Case 1
     nPos  := QHTM_GetScrollPos(nHandle)
     nPercent := Min(Round(((nPos / aSize[2]) * 100), 2), 100.00)
     Exit

   Case 2
     If hb_IsNumeric(nPercent)
        nPos := Round((nPercent * aSize[2] * 0.01), 0)
        QHTM_SetScrollPos(nHandle, nPos)
     Else
        nPercent := 0
     Endif

End Switch

Return nPercent

/******
*
*       QHTM_EnableUpdate(ControlName, ParentForm, lEnable)
*
*       Enable/disable redraw of control
*
*/
Procedure QHTM_EnableUpdate(ControlName, ParentForm, lEnable)

If PCount() < 2
   Return
Endif
 
hb_default(@lEnable, .T.)

hmg_SendMessage(GetControlHandle(ControlName, ParentForm), WM_SETREDRAW, iif(lEnable, 1, 0), 0)

Return
