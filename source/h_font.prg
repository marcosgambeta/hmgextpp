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

#define WM_SETFONT        0x0030

#define _FORMNAME_        "Main"

PROCEDURE _DefineFont(FontName, fName, fSize, bold, italic, underline, strikeout, nAngle, default, charset)

   LOCAL FontHandle
   LOCAL aFontList := {}
   LOCAL aFontSymb := {}
   LOCAL mVar
   LOCAL k

   IF _IsControlDefined(FontName, _FORMNAME_)
      _ReleaseFont(FontName)
   ENDIF

   hb_default(@fName, _HMG_DefaultFontName)
   hb_default(@fSize, _HMG_DefaultFontSize)
   hb_default(@bold, .F.)
   hb_default(@italic, .F.)
   hb_default(@underline, .F.)
   hb_default(@strikeout, .F.)
   hb_default(@nAngle, 0)

   IF charset == NIL
      GetFontList(NIL, NIL, NIL, NIL, NIL, NIL, @aFontList)

      GetFontList(NIL, NIL, SYMBOL_CHARSET, NIL, NIL, NIL, @aFontSymb)
      AEval(aFontSymb, {|cFont|AAdd(aFontList, cFont)})

      IF Empty(AScan(aFontList, {|cName|Upper(cName) == Upper(fName)}))
         fName := "Arial"
      ENDIF
   ENDIF

   IF hb_defaultValue(default, .F.)
      _HMG_DefaultFontName := fName
      _HMG_DefaultFontSize := fSize
   ENDIF

   mVar := "_" + _FORMNAME_ + "_" + FontName

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   PUBLIC &mVar. := k
#endif

   FontHandle := hmg_InitFont(fName, fSize, bold, italic, underline, strikeout, nAngle * 10, charset)

   _HMG_aControlType               [k] := CONTROL_TYPE_FONT
   _HMG_aControlNames              [k] := FontName
   _HMG_aControlHandles            [k] := FontHandle
   _HMG_aControlParenthandles      [k] := HMG_NULLHANDLE
   _HMG_aControlIds                [k] := _GetId()
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := 0
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := 0
   _HMG_aControlCol                [k] := 0
   _HMG_aControlWidth              [k] := hmg_GetTextWidth(NIL, "B", FontHandle)
   _HMG_aControlHeight             [k] := hmg_GetTextHeight(NIL, "B", FontHandle)
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel ], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel ], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fName
   _HMG_aControlFontSize           [k] := fSize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout, nAngle, hb_defaultValue(charset, DEFAULT_CHARSET)}
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

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

RETURN

PROCEDURE _ReleaseFont(FontName)

   LOCAL i := AScan(_HMG_aControlNames, FontName)

   IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_FONT
      _EraseFontDef(i)
   ENDIF

RETURN

PROCEDURE _EraseFontDef(i)

   LOCAL mVar

   hmg_DeleteObject(_HMG_aControlFontHandle[i])

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlDestroy, i)
   ENDIF

   mVar := "_" + _FORMNAME_ + "_" + _HMG_aControlNames[i]

#ifdef _NAMES_LIST_
   _DelNameList(mVar)
#else
   IF __mvExist(mVar)
#ifndef _PUBLIC_RELEASE_
      __mvPut(mVar, 0)
#else
      __mvXRelease(mVar)
#endif
   ENDIF
#endif

   _HMG_aControlDeleted           [i] := .T.
   _HMG_aControlType              [i] := ""
   _HMG_aControlNames             [i] := ""
   _HMG_aControlHandles           [i] := HMG_NULLHANDLE
   _HMG_aControlParentHandles     [i] := HMG_NULLHANDLE
   _HMG_aControlIds               [i] := 0
   _HMG_aControlProcedures        [i] := ""
   _HMG_aControlPageMap           [i] := {}
   _HMG_aControlValue             [i] := NIL
   _HMG_aControlInputMask         [i] := ""
   _HMG_aControllostFocusProcedure[i] := ""
   _HMG_aControlGotFocusProcedure [i] := ""
   _HMG_aControlChangeProcedure   [i] := ""
   _HMG_aControlBkColor           [i] := NIL
   _HMG_aControlFontColor         [i] := NIL
   _HMG_aControlDblClick          [i] := ""
   _HMG_aControlHeadClick         [i] := {}
   _HMG_aControlRow               [i] := 0
   _HMG_aControlCol               [i] := 0
   _HMG_aControlWidth             [i] := 0
   _HMG_aControlHeight            [i] := 0
   _HMG_aControlSpacing           [i] := 0
   _HMG_aControlContainerRow      [i] := 0
   _HMG_aControlContainerCol      [i] := 0
   _HMG_aControlPicture           [i] := ""
   _HMG_aControlContainerHandle   [i] := HMG_NULLHANDLE
   _HMG_aControlFontName          [i] := ""
   _HMG_aControlFontSize          [i] := 0
   _HMG_aControlToolTip           [i] := ""
   _HMG_aControlRangeMin          [i] := 0
   _HMG_aControlRangeMax          [i] := 0
   _HMG_aControlCaption           [i] := ""
   _HMG_aControlVisible           [i] := .F.
   _HMG_aControlHelpId            [i] := 0
   _HMG_aControlFontHandle        [i] := HMG_NULLHANDLE
   _HMG_aControlFontAttributes    [i] := {}
   _HMG_aControlBrushHandle       [i] := HMG_NULLHANDLE
   _HMG_aControlEnabled           [i] := .F.
   _HMG_aControlMiscData1         [i] := 0
   _HMG_aControlMiscData2         [i] := ""

RETURN

FUNCTION GetFontHandle(FontName)

   LOCAL i := AScan(_HMG_aControlNames, FontName)

   IF i > 0
      IF GetFontParamByRef(_HMG_aControlHandles[i])
         RETURN _HMG_aControlHandles[i]
      ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_FONT
         _EraseFontDef(i)
      ENDIF
   ENDIF

RETURN HMG_NULLHANDLE

FUNCTION GetFontParam(FontHandle)

   LOCAL aFontAttr
   LOCAL i := AScan(_HMG_aControlHandles, FontHandle)

   aFontAttr := {_HMG_DefaultFontName, _HMG_DefaultFontSize, .F., .F., .F., .F., 0, 0, 0, ""}

   IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_FONT
      aFontAttr := { ;
         _HMG_aControlFontName[i], ;
         _HMG_aControlFontSize[i], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_BOLD], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_ITALIC], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_UNDERLINE], ;
         _HMG_aControlFontAttributes[i, FONT_ATTR_STRIKEOUT], ;
         iif(Len(_HMG_aControlFontAttributes[i]) == 5, _HMG_aControlFontAttributes[i, FONT_ATTR_ANGLE], 0), ;
         _HMG_aControlWidth[i], _HMG_aControlHeight[i], _HMG_aControlNames[i] }
   ENDIF

RETURN aFontAttr

FUNCTION _GetFontAttr(ControlName, ParentForm, nType)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      SWITCH nType

      CASE FONT_ATTR_BOLD
      CASE FONT_ATTR_ITALIC
      CASE FONT_ATTR_UNDERLINE
      CASE FONT_ATTR_STRIKEOUT
      CASE FONT_ATTR_ANGLE
         RETURN _HMG_aControlFontAttributes[i][nType]

      CASE FONT_ATTR_NAME
         RETURN _HMG_aControlFontName[i]

      CASE FONT_ATTR_SIZE
         RETURN _HMG_aControlFontSize[i]

      ENDSWITCH

   ENDIF

RETURN NIL

FUNCTION _SetFontAttr(ControlName, ParentForm, Value, nType)

   LOCAL i
   LOCAL h
   LOCAL n
   LOCAL s
   LOCAL ab
   LOCAL ai
   LOCAL au
   LOCAL as
   LOCAL aa
   LOCAL t

   IF nType < FONT_ATTR_BOLD .OR. nType > FONT_ATTR_NAME
      RETURN .F.
   ENDIF

   i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN .F.
   ENDIF

   hmg_DeleteObject(_HMG_aControlFontHandle[i])

   SWITCH nType

   CASE FONT_ATTR_NAME
      _HMG_aControlFontName[i] := Value
      EXIT

   CASE FONT_ATTR_SIZE
      _HMG_aControlFontSize[i] := Value
      EXIT

   OTHERWISE
      _HMG_aControlFontAttributes[i][nType] := Value

   ENDSWITCH

   h  := _HMG_aControlHandles[i]
   n  := _HMG_aControlFontName[i]
   s  := _HMG_aControlFontSize[i]
   ab := _HMG_aControlFontAttributes[i][FONT_ATTR_BOLD]
   ai := _HMG_aControlFontAttributes[i][FONT_ATTR_ITALIC]
   au := _HMG_aControlFontAttributes[i][FONT_ATTR_UNDERLINE]
   as := _HMG_aControlFontAttributes[i][FONT_ATTR_STRIKEOUT]
   aa := iif(Len(_HMG_aControlFontAttributes[i]) == 5, _HMG_aControlFontAttributes[i][FONT_ATTR_ANGLE], 0)

   t := _HMG_aControlType[i]

   SWITCH t

   CASE CONTROL_TYPE_SPINNER
      _HMG_aControlFontHandle[i] := hmg__SetFont(h[1], n, s, ab, ai, au, as, aa)
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      _HMG_aControlFontHandle[i] := hmg__SetFont(h[1], n, s, ab, ai, au, as, aa)
      AEval(h, {|x|hmg_SendMessage(x, WM_SETFONT, _HMG_aControlFontHandle[i], 1)}, 2)
      EXIT

   OTHERWISE
      IF hmg_IsWindowHandle(h)
         _HMG_aControlFontHandle[i] := hmg__SetFont(h, n, s, ab, ai, au, as, aa)
         IF t == CONTROL_TYPE_MONTHCAL
            hmg_SetPosMonthCal(h, _HMG_aControlCol[i], _HMG_aControlRow[i])
            _HMG_aControlWidth[i] := GetWindowWidth(h)
            _HMG_aControlHeight[i] := GetWindowHeight(h)
         ENDIF
      ENDIF

   ENDSWITCH

   IF (_HMG_aControlType[i] == CONTROL_TYPE_CHECKLABEL .OR. _HMG_aControlType[i] == CONTROL_TYPE_LABEL) .AND. hb_IsLogical(_HMG_aControlInputMask[i])
      IF _HMG_aControlInputMask[i]
         _SetValue(ControlName, ParentForm, _GetValue(, , i))
      ENDIF
   ENDIF

RETURN .T.

FUNCTION GetFontParamByRef(FontHandle, FontName, FontSize, bold, italic, underline, strikeout, angle)

   LOCAL lExpr
   LOCAL i := iif((hb_IsNumeric(FontHandle) .OR. HB_ISPOINTER(FontHandle)), AScan(_HMG_aControlHandles, hmg_numbertohandle(FontHandle)), 0)

   lExpr := (i > 0 .AND. hmg_GetObjectType(_HMG_aControlHandles[i]) == OBJ_FONT)

   IF hb_PIsByRef(2)
      FontName := iif(lExpr, _HMG_aControlFontName[i], _HMG_DefaultFontName)
   ENDIF
   IF hb_PIsByRef(3)
      FontSize := iif(lExpr, _HMG_aControlFontSize[i], _HMG_DefaultFontSize)
   ENDIF
   IF hb_PIsByRef(4)
      bold := iif(lExpr, _HMG_aControlFontAttributes[i, FONT_ATTR_BOLD], .F.)
   ENDIF
   IF hb_PIsByRef(5)
      italic := iif(lExpr, _HMG_aControlFontAttributes[i, FONT_ATTR_ITALIC], .F.)
   ENDIF
   IF hb_PIsByRef(6)
      underline := iif(lExpr, _HMG_aControlFontAttributes[i, FONT_ATTR_UNDERLINE], .F.)
   ENDIF
   IF hb_PIsByRef(7)
      strikeout := iif(lExpr, _HMG_aControlFontAttributes[i, FONT_ATTR_STRIKEOUT], .F.)
   ENDIF
   IF hb_PIsByRef(8)
      angle := iif(lExpr, iif(Len(_HMG_aControlFontAttributes[i]) > 4, _HMG_aControlFontAttributes[i, FONT_ATTR_ANGLE], 0), 0)
   ENDIF

RETURN lExpr

//*********************************************
// by Dr. Claudio Soto (January 2014)
//*********************************************

FUNCTION GetFontList(hDC, cFontFamilyName, nCharSet, nPitch, nFontType, lSortCaseSensitive, aFontName)
   // return is array { { cFontName, nCharSet, nPitchAndFamily, nFontType }, ... }

   LOCAL SortCodeBlock

   IF hb_defaultValue(lSortCaseSensitive, .F.)
      SortCodeBlock := {|x, y|x[1] < y[1]}
   ELSE
      SortCodeBlock := {|x, y|Upper(x[1]) < Upper(y[1])}
   ENDIF

RETURN hmg_EnumFontsEx(hDC, cFontFamilyName, nCharSet, nPitch, nFontType, SortCodeBlock, @aFontName)
