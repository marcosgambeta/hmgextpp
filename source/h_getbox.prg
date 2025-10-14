//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// GETBOX Control Source Code
// Copyright 2006 Jacek Kubica <kubica@wssk.wroc.pl>
//
// Revision patch 2008 By Pierpaolo Martinello <pier.martinello[at]alice.it>
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

//#define __CLIPPER_COMPAT__
#include "SET_COMPILE_HMG_UNICODE.ch"

#include "minigui.ch"
#include "i_winuser.ch"
#include <hblang.ch>

#define WM_INVALID   WM_USER + 50
#define WM_CARET     WM_USER + 51

#define GBB1         2
#define GBB2         3

#define lInsert      s_Global[1]
#define lClrFocus    s_Global[2]
#define aClrFocus    s_Global[3]
#define aFntFocus    s_Global[4]
#define aOldBackClr  s_Global[5]
#define aOldFontClr  s_Global[6]

SET PROCEDURE TO tget\tget.prg
SET PROCEDURE TO tget\tgetint.prg

STATIC s_Global := { NIL, .F., { 235, 235, 145 }, NIL, NIL, NIL }

//---------------------------------------------------------------------------//
FUNCTION _DefineGetBox(ControlName, ParentFormName, x, y, w, h, Value, ;
   FontName, FontSize, aToolTip, lPassword, uLostFocus, uGotFocus, uChange, ;
   right, HelpId, readonly, bold, italic, underline, strikeout, field, backcolor, ;
   fontcolor, invisible, notabstop, nId, valid, cPicture, cmessage, cvalidmessage, ;
   when, ProcedureName, ProcedureName2, abitmap, BtnWidth, lNoMinus, noborder, bInit)
//---------------------------------------------------------------------------//

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL aControlHandle := {}
   LOCAL mVar
   LOCAL lDialogInMemory
   LOCAL FontHandle
   LOCAL nMaxLength
   LOCAL WorkArea
   LOCAL blInit
   LOCAL cBmp
   LOCAL tmp
   LOCAL lBtns := hb_IsBlock(ProcedureName)
   LOCAL lBtn2 := hb_IsBlock(ProcedureName2)
   LOCAL lModifyGotFocus := .F.
   LOCAL k
   LOCAL Style
   LOCAL aPicData
   LOCAL oGet
   LOCAL oc // := NIL
   LOCAL ow // := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   IF Empty(field) .AND. Value == NIL
      MsgMiniGUIError("GETBOX: Initial Value or Field must be specified.")
   ENDIF

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
   __defaultNIL(@uGotFocus, "")
   __defaultNIL(@uLostFocus, "")
   hb_default(@lPassword, .F.)
   hb_default(@cPicture, "")
   hb_default(@noborder, .F.)

   lInsert := IsInsertActive()
   cPicture := hb_asciiUpper(cPicture)

   DO CASE
   CASE hb_IsNumeric(Value) .AND. !( "B" $ cPicture )
      right := .T.
   CASE ValType(Value) == "D"
      nMaxLength := hb_ULen(DToC(Date()))
   CASE hb_IsLogical(Value)
      nMaxLength := 1
   CASE hb_IsChar(Value)
      nMaxLength := hb_ULen(Value)
   ENDCASE

   IF !hb_isArray(aBitmap)
      cBmp := aBitmap
      aBitmap := Array(2)
      aBitmap[1] := cBmp
   ENDIF

   IF !hb_isArray(aToolTip)
      tmp := aToolTip
      aToolTip := Array(3)
      aToolTip[1] := tmp
   ELSE
      IF Len(aToolTip) < 3
         aToolTip := ASize(aToolTip, 3)
      ENDIF
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
#ifdef _TSBROWSE_
      IF _HMG_BeginWindowMDIActive
         ParentFormHandle := GetActiveMdiHandle()
         ParentFormName := _GetWindowProperty(ParentFormHandle, "PROP_FORMNAME")
      ELSE
#endif
         ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
#ifdef _TSBROWSE_
      ENDIF
#endif
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
      MsgMiniGuiError("Control: " + ControlName + " of " + ParentFormName + " already defined.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive
      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_CHILD + ES_AUTOHSCROLL + BS_FLAT + iif(noborder, 0, WS_BORDER)

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

      IF !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF Len(_HMG_aDialogTemplate) > 0        //Dialog Template

         blInit := {|x, y, z|InitDialogTextBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "EDIT", style, 0, x, y, w, h, Value, HelpId, aTooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(ControlHandle)
         y := GetWindowRow(ControlHandle)
         w := GetWindowWidth(ControlHandle)
         h := GetWindowHeight(ControlHandle)

         hmg_SetWindowStyle(ControlHandle, Style, .T.)
      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      aControlHandle := hmg_InitGetBox(ParentFormHandle, 0, x, y, w, h, "", 0, nMaxLength, ;
         .F., .F., .F., lPassword, right, readonly, invisible, notabstop, abitmap[1], BtnWidth, lBtns, abitmap[2], lBtn2, noborder)

      ControlHandle := aControlHandle[1]

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
         hmg_SetTbBtnMargin(ControlHandle, BtnWidth, lBtns, lBtn2)
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      FOR tmp := 1 TO 3
         IF aToolTip[tmp] != NIL
            hmg_SetToolTip(aControlHandle[tmp], aToolTip[tmp], GetFormToolTipHandle(ParentFormName))
         ENDIF
      NEXT

   ENDIF

   oget := Get()
   oget:New(-1, -1, {|x|iif(x == NIL, oget:cargo, oget:cargo := x)}, "", cPicture)
   oget:cargo     := Value
   oget:preblock  := when
   oget:postblock := valid
   oget:message   := cmessage
   oget:name      := mVar
   oget:control   := ControlHandle
   oget:SetFocus()
   oget:original  := oGet:buffer

   aPicData := _GetPictureData(oGet, cPicture)

   IF cPicture == NIL .OR. !( "@K" $ cPicture )
      oget:Clear := .F.
   ENDIF

   IF !Empty(aPicData[2]) .AND. oget:type == "C"
      Value := PadR(Value, Len(aPicData[2]))
      oget:cargo := Value
   ENDIF

   oGet:UpdateBuffer()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_GETBOX
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := iif(readonly, NIL, ProcedureName)
   _HMG_aControlPageMap            [k] := Field
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := aPicData
   _HMG_aControlLostFocusProcedure [k] := uLostFocus
   _HMG_aControlGotFocusProcedure  [k] := uGotFocus
   _HMG_aControlChangeProcedure    [k] := uChange
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := iif(readonly, NIL, ProcedureName2)
   _HMG_aControlHeadClick          [k] := oGet
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := cValidMessage
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := FontName
   _HMG_aControlFontSize           [k] := FontSize
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := aToolTip
   _HMG_aControlRangeMin           [k] := aControlHandle
   _HMG_aControlRangeMax           [k] := nMaxLength
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := !invisible
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { 0, readonly, 0, ProcedureName, ProcedureName2, BtnWidth, lBtn2, lNoMinus, .T. }
   _HMG_aControlMiscData2          [k] := ""

   IF hb_IsString(cPicture) .AND. !Empty(cPicture) .AND. "@K" $ cPicture
      lModifyGotFocus := .T.
   ENDIF

   IF lModifyGotFocus .AND. Empty(uGotFocus)
      IF hb_IsChar(Value)
         _HMG_aControlGotFocusProcedure[k] := {||hmg_SendMessage(_HMG_aControlHandles[k], EM_SETSEL, 0, iif(Empty(Value), -1, Len(Trim((_HMG_aControlHeadClick[k]):Cargo))))}
      ELSEIF ValType(Value) $ "ND"
         _HMG_aControlGotFocusProcedure[k] := {||hmg_SendMessage(_HMG_aControlHandles[k], EM_SETSEL, 0, -1)}
      ENDIF
   ENDIF

   IF !lDialogInMemory
      IF !Empty(Value)
         IF oGet:type == "N" .AND. hb_UAt("B", aPicData[1]) > 0
            oGet:buffer := LTrim(oGet:buffer)
         ENDIF
         _DispGetBoxText(ControlHandle, oGet:buffer)
      ENDIF

      IF Field != NIL
         AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentFormName)], k)
      ENDIF
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, oGet, ow, oc)

RETURN oGet

//---------------------------------------------------------------------------//
STATIC PROCEDURE _GetBoxSetNextFocus(lPrevious)
//---------------------------------------------------------------------------//
   
   LOCAL NextControlHandle
   LOCAL i

   NextControlHandle := hmg_GetNextDlgTabITem(hmg_GetActiveWindow() , hmg_GetFocus() , lPrevious)
   hmg_setfocus(NextControlHandle)

   IF (i := AScan(_HMG_aControlHandles, NextControlHandle)) > 0

      IF _HMG_aControlType[i] == CONTROL_TYPE_BUTTON
         hmg_SendMessage(NextControlHandle, BM_SETSTYLE, hmg_LOWORD(BS_DEFPUSHBUTTON), 1)
      ENDIF

   ENDIF

RETURN

//---------------------------------------------------------------------------//
PROCEDURE _DataGetBoxRefresh(i)
//---------------------------------------------------------------------------//
   
   LOCAL Field := _HMG_aControlPageMap[i]

   _SetGetBoxValue(i, _HMG_aControlHandles[i], iif(Field == NIL, _HMG_aControlValue[i], &(Field)))

RETURN

//---------------------------------------------------------------------------//
PROCEDURE _DataGetBoxSave(ControlName, ParentForm)
//---------------------------------------------------------------------------//
   
   LOCAL Field
   LOCAL i
   LOCAL oGet

   i := GetControlIndex(ControlName, ParentForm)
   oGet := _HMG_aControlHeadClick[i]

   oGet:SetFocus()
   Field := _HMG_aControlPageMap[i]
   &(Field) := _GetValue(ControlName, ParentForm)

   oGet:VarPut(&(Field))
   oGet:cargo := &(Field)

   IF oGet:type == "D"
      oGet:buffer := DToC(oGet:cargo)
   ENDIF

RETURN

//---------------------------------------------------------------------------//
FUNCTION OGETEVENTS(hWnd, nMsg, wParam, lParam)
//---------------------------------------------------------------------------//
   
   LOCAL ParentHandle
   LOCAL lshift
   LOCAL lCtrl
   LOCAL nStart
   LOCAL nEnd
   LOCAL coldbuff
   LOCAL h
   LOCAL oGet
   LOCAL ipp
   LOCAL nlen
   LOCAL cText
   LOCAL cPicMask
   LOCAL cPicFunc
   LOCAL lCleanZero
   LOCAL MinDec
   LOCAL aHandle
   LOCAL HwndBtn
   LOCAL readonly
   LOCAL lAllowEdit
   LOCAL aKey
   LOCAL i := AScan(_HMG_aControlHandles, hWnd)

   STATIC lInValid := .F.

   IF i <= 0
      RETURN 0
   ENDIF

   ParentHandle := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])

   IF ParentHandle <= 0
      RETURN 0
   ENDIF

   cPicFunc   := _HMG_aControlInputMask[i, 1]
   cPicMask   := _HMG_aControlInputMask[i, 2]
   lCleanZero := _HMG_aControlInputMask[i, 3]

   oGet       := _HMG_aControlHeadClick[i]
   readonly   := _HMG_aControlMiscData1[i, 2]
   lAllowEdit := _HMG_aControlMiscData1[i, 9]

   _HMG_ThisFormName := _HMG_aFormNames[ParentHandle]
   _HMG_ThisControlName := _HMG_aControlNames[i]

   IF hb_IsBlock(oGet:preblock) .AND. nMsg == WM_SETFOCUS
      IF !Eval(oGet:preblock, oGet, .F.)
         IF oGet:VarGet() == oGet:UnTransform(oGet:original)
            lAllowEdit := .F.
            _HMG_aControlMiscData1[i, 9] := .F.
         ENDIF
      ELSE
         _HMG_aControlMiscData1[i, 9] := .T.
      ENDIF
   ENDIF

   SWITCH nMsg

   CASE WM_SETFOCUS

      nStart := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      nStart := Min(nStart, hb_ULen(Trim(oGet:buffer)))

      nEnd := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      nEnd := Min(nEnd, hb_ULen(Trim(oGet:buffer)))

      coldbuff := oGet:buffer

#ifdef __CLIPPER_COMPAT__
      IF !lAllowEdit
         Tone(400)
         hmg_PostMessage(hWnd, WM_KEYDOWN, VK_DOWN, 0)
      ENDIF
#endif
      IF lClrFocus .AND. !readonly .AND. lAllowEdit
         aOldBackClr := _HMG_aControlBkColor[i]
         IF _HMG_aControlBkColor[i] == NIL
            _HMG_aControlBkColor[i] := nRGB2Arr(waGetSysColor(COLOR_WINDOW))
         ENDIF
         IF hb_IsNumeric(_HMG_aControlBkColor[i, 1])
            _HMG_aControlBkColor[i] := iif(hb_IsBlock(aClrFocus), Eval(aClrFocus), aClrFocus)
         ELSEIF hb_IsArray(_HMG_aControlBkColor[i, 1]) .AND. Len(_HMG_aControlBkColor[i]) == 3
            _HMG_aControlBkColor[i][3] := iif(hb_IsBlock(aClrFocus), Eval(aClrFocus), aClrFocus)
         ENDIF
         aOldFontClr := _HMG_aControlFontColor[i]
         IF _HMG_aControlFontColor[i] == NIL
            _HMG_aControlFontColor[i] := nRGB2Arr(waGetSysColor(COLOR_WINDOWTEXT))
         ENDIF
         IF aFntFocus != NIL
            IF hb_IsNumeric(_HMG_aControlFontColor[i, 1])
               _HMG_aControlFontColor[i] := iif(hb_IsBlock(aFntFocus), Eval(aFntFocus), aFntFocus)
            ELSEIF hb_IsArray(_HMG_aControlFontColor[i, 1]) .AND. Len(_HMG_aControlFontColor[i]) == 3
               _HMG_aControlFontColor[i][3] := iif(hb_IsBlock(aFntFocus), Eval(aFntFocus), aFntFocus)
            ENDIF
         ENDIF
      ENDIF

      IF !( coldbuff == oGet:buffer )
         IF oGet:BadDate
            _DispGetBoxText(hWnd, BLANK_DATE)
         ELSE
            _DispGetBoxText(hWnd, oGet:buffer)
         ENDIF
      ENDIF

      hmg_InvalidateRect(hWnd, 0)
      hmg_PostMessage(hWnd, WM_CARET, 0, 0)
      _HMG_aControlMiscData1[i][1] := 1

      IF "@K" $ oGet:Picture .OR. oGet:type == "N"
         oGet:pos := 1
         nStart := oGet:pos - 1
      ELSE
         oGet:pos := nEnd + 1
      ENDIF
      oGet:changed := .F.
      oGet:UpdateBuffer()

      IF !readonly .AND. lAllowEdit
         _DispGetBoxText(hWnd, oGet:buffer)
      ELSE
         IF oGet:type == "N" .AND. hb_UAt("B", cPicFunc) > 0
            oGet:buffer := LTrim(oGet:buffer)
            _DispGetBoxText(hWnd, oGet:buffer)
         ENDIF
      ENDIF

      hmg_SendMessage(hWnd, EM_SETSEL, nStart, nStart)
      _SetGetBoxCaret(hWnd)
      hmg_SendMessage(hWnd, EM_SETSEL, nStart, nStart)

      // show message if any
      IF ParentHandle > 0
         IF _IsControlDefined("StatusBar", _HMG_aFormNames[ParentHandle])
            IF !Empty(oGet:message)
               SetProperty(_HMG_aFormNames[ParentHandle], "StatusBar", "Item", 1, oGet:message)
            ENDIF
         ENDIF
      ENDIF

      RETURN 0

   CASE WM_INVALID

      IF (_IsChildOfActiveWindow(hWnd) .OR. hmg_IsWindowHasExStyle(_HMG_aControlParenthandles[i], WS_EX_CONTROLPARENT)) .AND. !readonly .AND. lAllowEdit

         IF !lInValid
            lInValid := .T.

            IF hb_IsBlock(oGet:postblock)
               coldbuff := oGet:buffer

               h := hmg_GetFocus()
               hmg_HideCaret(hWnd)
               hmg_HideCaret(h)

#ifdef _NAMES_LIST_
               IF !Do_ControlEventProcedure(oGet:postblock, _GetNameList(oGet:name), oGet)
#else
               IF !Do_ControlEventProcedure(oGet:postblock, __mvGet(oGet:name), oGet)
#endif
                  hmg_SetFocus(hWnd)
                  IF Empty(_HMG_aControlSpacing[i])
                     oGet:changed := .T.
                  ELSE
                     oGet:changed := MsgRetryCancel(_HMG_aControlSpacing[i], _HMG_BRWLangError[11], , .F.)
                     IF !oGet:changed
                        hmg_PostMessage(hWnd, WM_KEYDOWN, VK_ESCAPE, 0)
                     ENDIF
                     hmg_SetFocus(hWnd)
                  ENDIF
               ELSE
                  oGet:changed := .T.
                  oGet:original := oGet:buffer
                  hmg_ShowCaret(h)
               ENDIF

               IF !( coldbuff == oGet:buffer )
                  _DispGetBoxText(hWnd, oGet:buffer)
               ENDIF
            ELSE
               oGet:changed := .T.
               oGet:original := oGet:buffer
               IF lClrFocus
                  _HMG_aControlBkColor[i] := aOldBackClr
                  _HMG_aControlFontColor[i] := aOldFontClr
               ENDIF
            ENDIF

            lInValid := .F.
         ENDIF

      ENDIF
      
      EXIT

   CASE WM_KILLFOCUS

      IF oGet:Changed
         oGet:assign()
         // Patch By Pier July 2008
         // Add By Pier patch for the smaller negative numbers of zero START
         IF oGet:type == "N" .AND. oGet:minus .AND. Val(hb_USubStr(oget:buffer, 1, hb_UAt(",", oget:buffer) - 1)) == 0
            IF Val(hb_USubStr(oget:buffer, hb_UAt(",", oget:buffer) + 1)) != 0
               MinDec := StrTran(oget:buffer, iif(hb_UAt("E", cPicFunc) > 0, ",", ", "), ".")
               MinDec := StrTran(MinDec,  " ", "")
               oget:VarPut(Val(MinDec) * iif(Val(MinDec) > 0, -1, 1))
            ENDIF
            oget:buffer := oget:VarGet()
         ENDIF
         // Add By Pier patch for the smaller negative numbers of zero STOP
         oGet:UpdateBuffer()
      ENDIF

      IF ValType(oGet:cargo) == "D" .AND. oGet:BadDate
         oGet:BadDate := .F.
         oGet:VarPut(BLANK_DATE)
         oGet:UpdateBuffer()
         hmg_PostMessage(hWnd, WM_INVALID, wParam, 0)
         RETURN 0
      ENDIF

      IF lCleanZero .AND. oGet:type == "N" .AND. oGet:VarGet() == 0
         oGet:buffer := Space(hb_ULen(oGet:buffer))
      ENDIF

      IF oGet:type == "N" .AND. hb_UAt("B", cPicFunc) > 0
         oGet:buffer := LTrim(oGet:buffer)
      ENDIF

      IF lClrFocus .AND. !readonly .AND. lAllowEdit
         _HMG_aControlBkColor[i] := aOldBackClr
         _HMG_aControlFontColor[i] := aOldFontClr
      ENDIF
      _DispGetBoxText(hWnd, oGet:buffer)
      hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

      // message
      IF ParentHandle > 0

         IF _IsControlDefined("StatusBar", _HMG_aFormNames[ParentHandle])

            IF hb_IsChar(_HMG_DefaultStatusBarMessage) .AND. _IsOwnerDrawStatusBarItem(_HMG_aControlContainerHandle[i], 1)
               SetProperty(_HMG_aFormNames[ParentHandle], "StatusBar", "Item", 1, _HMG_DefaultStatusBarMessage)
            ELSEIF ValType(cText := _GetDefinedStatusBarItemValue(_HMG_aControlParenthandles[i], 1)) == "C"
               SetProperty(_HMG_aFormNames[ParentHandle], "StatusBar", "Item", 1, cText)
            ELSE
               SetProperty(_HMG_aFormNames[ParentHandle], "StatusBar", "Item", 1, "")
            ENDIF

         ENDIF

      ENDIF

      // check post-validation
      IF !lInValid
         hmg_PostMessage(hWnd, WM_INVALID, wParam, 0)
      ENDIF

      RETURN 0

   CASE WM_CHAR

      nStart := hmg_LoWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0)) + 1
      nEnd   := hmg_HiWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0)) + 1
      oGet:pos := nEnd
      _HMG_aControlMiscData1[i, 3] := wParam  //JP

      DO CASE

      CASE wParam == 1  // CTRL+A SelectAll

         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, 0, -1)
         RETURN 0

      CASE wParam == 22 // CTRL+V Paste

         hmg_SendMessage(_HMG_aControlhandles[i], WM_PASTE, 0, 0)
         RETURN 0

      CASE wParam == 3  // CTRL+C Copy

         System.Clipboard := hb_USubStr(oGet:buffer, nStart, nEnd - nStart)
         RETURN 0

      CASE wParam == 24 .AND. !readonly // CTRL+X Cut

         IF !lAllowEdit .OR. oGet:type == "L"
            RETURN 0
         ENDIF

         System.Clipboard := hb_USubStr(oGet:buffer, nStart, nEnd - nStart)
         nStart := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         nEnd   := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1

         oGet:pos := nEnd

         IF nStart != nEnd

            IF nEnd > hb_ULen(oGet:buffer)
               oGet:Delete()
            ENDIF

            FOR ipp := nStart TO nEnd

               IF oGet:pos > nStart
                  oGet:BackSpace()
               ELSE
                  EXIT
               ENDIF

            NEXT

         ELSE

            IF _IsEditable(oGet:pos, i)
               oGet:Delete()
            ENDIF

         ENDIF

         oGet:Assign()
         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

         RETURN 0

      CASE wParam == 25 .AND. !readonly // CTRL+Y Clear

         IF !lAllowEdit .OR. oGet:type == "L"
            RETURN 0
         ENDIF

         oGet:DelEnd()

         oGet:Assign()
         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

         RETURN 0

      CASE wParam == 0x0D .OR. wParam == 0x09   // Return key or TAB key pressed

         IF ValType(oGet:cargo) == "D" .AND. oGet:BadDate .AND. !readonly .AND. lAllowEdit
            hmg_SendMessage(hWnd, EM_SETSEL, 0, 0)
            oGet:Pos := 1
         ELSE
            lShift := _GetKeyState(VK_SHIFT)  // Shift key pressed (or not)
            _GetBoxSetNextFocus(lShift)
         ENDIF

         RETURN 0

      CASE (wParam == VK_BACK .AND. lParam != 0 .AND. !readonly .AND. lAllowEdit .AND. oGet:type != "L")

         IF nEnd > nStart

            IF nEnd > hb_ULen(oGet:buffer) + 1
               IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos, 1) $ "(-"
                  oGet:minus := .F.
               ENDIF
               oGet:delete()
            ENDIF

            FOR ipp := nStart TO nEnd
               IF oGet:pos > nStart
                  IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos - 1, 1) $ "(-"
                     oGet:minus := .F.
                  ENDIF
                  oGet:backSpace()
               ELSE
                  EXIT
               ENDIF
            NEXT ipp

         ELSE

            IF nEnd > hb_ULen(oGet:buffer) + 1
               IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos, 1) $ "(-"
                  oGet:minus := .F.
               ENDIF
               oGet:Delete()
            ELSE
               IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos - 1, 1) $ "(-"
                  oGet:minus := .F.
               ENDIF
               oGet:BackSpace()
            ENDIF

         ENDIF

         oGet:Assign()
         _HMG_aControlValue[i] := oGet:VarGet()
         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, (oGet:pos - 1), (oGet:pos - 1))

         RETURN 0

      CASE wParam >= 32 .AND. wParam <= hb_cdpCharMax()  // regular input
         _HMG_aControlMiscData1[i, 3] := 0

         IF readonly .OR. !lAllowEdit
            RETURN 0
         ENDIF

         IF oGet:type == "L"
            nStart := 0
            nEnd := 0
            hmg_SendMessage(hWnd, EM_SETSEL, nStart, nEnd)
            oGet:Pos := 1
         ENDIF

#ifdef UNICODE
         IF wParam != 46 .AND. wParam != 44 .AND. hb_UCode(_Input(hb_UChar(wParam), i)) <= 0  // dot and coma
#else
         IF wParam != 46 .AND. wParam != 44 .AND. Asc(_Input(Chr(wParam), i)) <= 0  // dot and coma
#endif
            RETURN 0
         ENDIF

#ifdef UNICODE
         IF ("A" $ cPicFunc) .AND. !hmg_IsAlpha(hb_UChar(wParam))
#else
         IF ("A" $ cPicFunc) .AND. !hmg_IsAlpha(hb_BChar(wParam))
#endif
            RETURN 0
         ENDIF

         oGet:changed := .T.

         IF nStart != nEnd
            IF nEnd > hb_ULen(oGet:buffer) + 1
               oGet:Delete()
            ENDIF
            FOR ipp := nStart TO nEnd  // clear selection by backspacing
               IF oGet:pos > nStart
                  oGet:BackSpace()
               ELSE
                  EXIT
               ENDIF
            NEXT ipp
         ENDIF

         IF oGet:pos == 1
            oGet:home()
         ENDIF

         IF oGet:pos == _FirstEditable(i) .AND. _HMG_aControlMiscData1[i][1] == 1 .AND. ( oGet:type == "N" .OR. hb_UAt("K", cPicFunc) > 0 )
            _HMG_aControlMiscData1[i][1] := 0
            oGet:minus   := .F.
            oGet:clear   := .T.
            oGet:changed := .T.
            oGet:Assign()
            _DispGetBoxText(hWnd, oGet:buffer)
         ELSE
            _HMG_aControlMiscData1[i][1] := 1
         ENDIF

         IF oGet:type == "N" .AND. ( wParam == 46 .OR. wParam == 44 )

            IF oGet:type == "N"
               nlen := hb_ULen(oGet:buffer)
               IF (ipp := hb_UAt("." , oGet:buffer)) > 0
                  oGet:buffer := hb_UPadL(StrTran(hb_ULeft(oGet:buffer, ipp - 1) , " " , "") , ipp - 1) + ;
                     "." + hb_UPadR(StrTran(hb_USubStr(oGet:buffer, ipp + 1) , " " , "") , nlen - ipp, "0")

               ELSE
                  oGet:buffer := hb_UPadL(StrTran(oGet:buffer, " ", ""), nlen)
               ENDIF
            ENDIF

         ELSE

            IF IsInsertActive()
#ifdef UNICODE
               oGet:Insert(hb_UChar(wParam))
#else
               oGet:Insert(hb_BChar(wParam))
#endif
            ELSE
#ifdef UNICODE
               oGet:Overstrike(hb_UChar(wParam))
#else
               oGet:Overstrike(hb_BChar(wParam))
#endif
            ENDIF

            IF oGet:Rejected
#ifdef __CLIPPER_COMPAT__
               Tone(400)
#endif
            ELSE
               oGet:Assign()
               _HMG_aControlValue[i] := oGet:VarGet()
            ENDIF

         ENDIF

         IF oGet:type == "L"
            oGet:buffer := hb_asciiUpper(oGet:buffer)
            oGet:pos := 1
         ENDIF

         IF oGet:type == "N" .AND. ( wParam == 46 .OR. wParam == 44 )

            oGet:UpdateBuffer()

            IF oGet:DecPos != 0
               IF oGet:DecPos == hb_ULen(cPicMask)
                  oGet:pos := oGet:DecPos - 1   //9999.
               ELSE
                  oGet:pos := oGet:DecPos + 1   //9999.9
               ENDIF
            ELSE
               oGet:pos := oGet:nDispLen
            ENDIF

         ENDIF

         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

         RETURN 0

      ENDCASE
      
      EXIT

   CASE WM_LBUTTONDBLCLK

      IF wParam == MK_LBUTTON
         IF Len(oGet:aKeyEvent) > 0
            FOR EACH aKey IN oGet:aKeyEvent
               IF Val(aKey[1]) == nMsg
                  RETURN oGet:DoKeyEvent(nMsg)
               ENDIF
            NEXT
         ENDIF
         hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, 0, -1)
      ENDIF

      EXIT

   CASE WM_KEYDOWN

      IF wParam == 110 .OR. wParam == 190
         RETURN 0
      ENDIF

      nStart := hmg_LoWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0))
      nEnd   := hmg_HiWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0))
      oGet:pos := nEnd + 1
      _HMG_aControlMiscData1[i, 3] := wParam  //JP

      IF Len(oGet:aKeyEvent) > 0
         FOR EACH aKey IN oGet:aKeyEvent
            IF Val(aKey[1]) == wParam
               RETURN oGet:DoKeyEvent(wParam)
            ENDIF
         NEXT
      ENDIF

      lShift := _GetKeyState(VK_SHIFT)
      lCtrl  := _GetKeyState(VK_CONTROL)

      SWITCH wParam

      CASE VK_ESCAPE

         IF !readonly

            IF oGet:Type == "N" .AND. oGet:minus .AND. hb_UAt("-", oGet:original) <= 0
               oGet:buffer := oGet:original
               oGet:VarPut((oGet:unTransform()) * (-1), .T.)
               oGet:minus := .F.
               IF oGet:Changed
                  oGet:Assign()
                  oGet:UpdateBuffer()
               ENDIF
               _HMG_aControlValue[i] := oGet:VarGet()
            ELSE
               IF oGet:Changed
                  oGet:buffer := oGet:original
                  oGet:Assign()
                  oGet:UpdateBuffer()
               ENDIF
               _HMG_aControlValue[i] := oGet:VarGet()
            ENDIF

            _DispGetBoxText(hWnd, oGet:buffer)
            hmg_SendMessage(hWnd, EM_SETSEL, 0, 0)

            oGet:BadDate := .F.
            lInValid := .F.
            oGet:Pos := 1

            IF !oGet:changed
               oGet:buffer := oGet:original
               IF oGet:Type == "N" .AND. oGet:minus .AND. hb_UAt("-", oGet:original) <= 0
                  oGet:VarPut((oGet:unTransform()) * (-1), .T.)
                  oGet:minus := .F.
               ENDIF
               oGet:Assign()
               oGet:UpdateBuffer()
               _HMG_aControlValue[i] := oGet:VarGet()
               _GetBoxSetNextFocus(.F.)
            ENDIF

            oGet:Changed := .F.
            RETURN 0

         ENDIF

         EXIT

      CASE VK_INSERT

         IF lCtrl
            CopyToClipboard(hb_USubStr(oGet:buffer, nStart, nEnd - nStart)) // TODO: hmg_CopyToClipboard ?
            RETURN 0
         ELSEIF lShift
            hmg_SendMessage(hWnd, WM_PASTE, 0, 0)
            RETURN 0
         ENDIF
         lInsert := !lInsert
         _SetGetBoxCaret(hWnd)
         EXIT

      CASE VK_DOWN

         IF !lCtrl .AND. !lShift
            hmg_SendMessage(hWnd, EM_SETSEL, nEnd, nEnd)
            IF ValType(oGet:cargo) == "D" .AND. oGet:BadDate
               RETURN 0
            ELSE
               _GetBoxSetNextFocus(.F.)
               RETURN 0
            ENDIF
         ELSE
            IF lCtrl .AND. !readonly .AND. lAllowEdit
               IF oGet:type == "D" .OR. oGet:type == "N"
                  oGet:VarPut(oGet:VarGet() - 1)
                  oGet:UpdateBuffer()
                  _DispGetBoxText(hWnd, oGet:buffer)
                  oGet:changed := .T.
               ENDIF
               IF oGet:type == "L"
                  oGet:VarPut(!oGet:VarGet())
                  oGet:UpdateBuffer()
                  _DispGetBoxText(hWnd, oGet:buffer)
                  oGet:changed := .T.
               ENDIF
            ENDIF
         ENDIF

         RETURN 0

      CASE VK_UP

         IF !lCtrl .AND. !lShift
            hmg_SendMessage(hWnd, EM_SETSEL, nEnd, nEnd)
            IF ValType(oGet:cargo) == "D" .AND. oGet:BadDate
               RETURN 0
            ELSE
               _GetBoxSetNextFocus(.T.)
               RETURN 0
            ENDIF
         ELSE
            IF lCtrl .AND. !readonly .AND. lAllowEdit
               IF oGet:type == "D" .OR. oGet:type == "N"
                  oGet:VarPut(oGet:VarGet() + 1)
                  oGet:UpdateBuffer()
                  _DispGetBoxText(hWnd, oGet:buffer)
                  oGet:changed := .T.
               ENDIF
               IF oGet:type == "L"
                  oGet:VarPut(!oGet:VarGet())
                  oGet:UpdateBuffer()
                  _DispGetBoxText(hWnd, oGet:buffer)
                  oGet:changed := .T.
               ENDIF
            ENDIF
         ENDIF

         RETURN 0

      CASE VK_LEFT

         IF lShift
            IF nEnd > nStart
               nEnd --
            ENDIF
            hmg_SendMessage(hWnd, EM_SETSEL, nStart, nEnd)
         ELSE
            hmg_SendMessage(hWnd, EM_SETSEL, nEnd - 1, nEnd - 1)
            _HMG_aControlMiscData1[i][1] := 0
         ENDIF
         oGet:pos := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         EXIT

      CASE VK_RIGHT

         IF lShift
            nEnd := oGet:Pos
            hmg_SendMessage(hWnd, EM_SETSEL, nStart, nEnd)
         ELSE
            hmg_SendMessage(hWnd, EM_SETSEL, nStart + 1, nStart + 1)
            _HMG_aControlMiscData1[i][1] := 0
         ENDIF
         oGet:pos := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         EXIT

      CASE VK_HOME

         nStart := 0
         IF !lShift
            nEnd := 0
         ENDIF
         hmg_SendMessage(hWnd, EM_SETSEL, nStart, nEnd)

         RETURN 1

      CASE VK_END

         nEnd := hb_ULen(Trim(oGet:buffer))
         IF !lShift
            nStart := nEnd
         ENDIF
         hmg_SendMessage(hWnd, EM_SETSEL, nStart, nEnd)

         RETURN 1

#if 0
      CASE VK_INSERT

         lInsert := !lInsert
         _SetGetBoxCaret(hWnd)
         EXIT
#endif

      CASE VK_DELETE

         IF readonly .OR. !lAllowEdit .OR. oGet:type == "L"
            RETURN 0
         ENDIF

         nStart := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         nEnd   := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         oGet:pos := nEnd

         IF nStart != nEnd

            IF nEnd > hb_ULen(oGet:buffer)
               oGet:Delete()
            ENDIF

            FOR ipp := nStart TO nEnd

               IF oGet:pos > nStart
                  IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos, 1) $ "(-"
                     oGet:minus := .F.
                  ENDIF
                  oGet:BackSpace()
               ELSE
                  EXIT
               ENDIF

            NEXT

         ELSE

            IF _IsEditable(oGet:pos, i)
               IF oGet:type == "N" .AND. hb_USubStr(oGet:buffer, oGet:pos, 1) $ "(-"
                  oGet:minus := .F.
               ENDIF
               oGet:Delete()
            ENDIF

         ENDIF

         oGet:Assign()

         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

         RETURN 0

      ENDSWITCH

      EXIT

   CASE WM_PASTE

      IF readonly .OR. !lAllowEdit
         RETURN 0
      ENDIF

      IF (cText := System.Clipboard) != NIL

         nStart := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         nEnd   := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         nLen   := hb_ULen(oGet:buffer)

         IF nStart != nEnd
            FOR i := nStart TO nEnd  // clear selection by backspacing
               IF oGet:pos > nStart
                  oGet:BackSpace()
               ELSE
                  EXIT
               ENDIF
            NEXT
         ENDIF

         h := oGet:pos

         FOR i := 1 TO hb_ULen(cText)
#ifdef UNICODE
            wParam := hb_UCode(hb_USubStr(cText, i, 1))
#else
            wParam := Asc(hb_USubStr(cText, i, 1))
#endif
            IF oGet:type == "N" .AND. wParam == 46
               oGet:toDecPos()
            ELSE
               IF IsInsertActive()
#ifdef UNICODE
                  oGet:Insert(hb_UChar(wParam))
#else
                  oGet:Insert(hb_BChar(wParam))
#endif
               ELSE
#ifdef UNICODE
                  oGet:Overstrike(hb_UChar(wParam))
#else
                  oGet:Overstrike(hb_BChar(wParam))
#endif
               ENDIF
            ENDIF

            IF h + i > nLen
               EXIT
            ENDIF

         NEXT
         oGet:Assign()
         oGet:VarPut(oGet:unTransform())

         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

      ENDIF

      RETURN 0

   CASE WM_CUT
   CASE WM_CLEAR

      IF hmg_IsWindowEnabled(hWnd) .AND. !readonly .AND. lAllowEdit

         nStart := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         nEnd := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0)) + 1
         oGet:pos := nEnd

         IF nStart != nEnd
            IF nEnd > hb_ULen(oGet:buffer)
               oGet:delete()
            ENDIF

            FOR i := nStart TO nEnd
               IF oGet:pos > nStart
                  oGet:BackSpace()
               ELSE
                  EXIT
               ENDIF
            NEXT
         ELSE
            oGet:delete()
         ENDIF

         _DispGetBoxText(hWnd, oGet:buffer)
         hmg_SendMessage(hWnd, EM_SETSEL, oGet:pos - 1, oGet:pos - 1)

      ENDIF

      RETURN 0

   CASE WM_CARET

      _SetGetBoxCaret(hWnd)
      
      EXIT

   CASE WM_CONTEXTMENU

      ParentHandle := _HMG_aControlParentHandles[i]

      IF (i := AScan(_HMG_aControlsContextMenu, {|x|x[1] == hWnd})) > 0

         IF _HMG_aControlsContextMenu[i][4]
            hmg_setfocus(wParam)

            _HMG_xControlsContextMenuID := _HMG_aControlsContextMenu[i][3]

            hmg_TrackPopupMenu(_HMG_aControlsContextMenu[i][2] , hmg_LOWORD(lParam) , hmg_HIWORD(lParam) , ParentHandle)

            RETURN 1
         ENDIF

      ENDIF
      
      EXIT

   CASE WM_COMMAND

      IF (HwndBtn := lParam) > 0

         aHandle := _HMG_aControlRangeMin[i]

         IF hb_IsArray(aHandle) .AND. Len(aHandle) >= 1 .AND. aHandle[1] == hWnd

            SWITCH AScan(aHandle, HwndBtn)
            CASE GBB1
               _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
               EXIT
            CASE GBB2
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            ENDSWITCH

            hmg_SendMessage(HwndBtn, BM_SETSTYLE, hmg_LOWORD(BS_PUSHBUTTON), 1)
            hmg_setfocus(aHandle[1])

         ENDIF

      ENDIF

   ENDSWITCH

RETURN 0

//---------------------------------------------------------------------------//
PROCEDURE _SetGetBoxValue(nId, hWnd, Value)
//---------------------------------------------------------------------------//
   
   LOCAL aPicData
   LOCAL oGet       := _HMG_aControlHeadClick[nId]
   LOCAL cPicFunc   := _HMG_aControlInputMask[nId, 1]
   LOCAL lCleanZero := _HMG_aControlInputMask[nId, 3]

   IF ValType(Value) == ValType(oGet:VarGet())

      _HMG_ThisFormIndex   := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[nId])
      _HMG_ThisFormName    := _HMG_aFormNames[_HMG_ThisFormIndex]
      _HMG_ThisControlName := _HMG_aControlNames[nId]
      _HMG_aControlValue[nId] := Value

      oGet:VarPut(Value)
      oGet:UpdateBuffer()
      oGet:original := oGet:buffer

      IF oGet:type == "N" .AND. Value >= 0
         oGet:minus := .F.
      ENDIF

      IF oGet:type == "N" .AND. ( hb_UAt("B", cPicFunc) > 0 .OR. lCleanZero )
         oGet:buffer := LTrim(oGet:buffer)
      ENDIF

      aPicData := _GetPictureData(oGet, oGet:Picture)

      IF oGet:Picture == NIL .OR. !( "@K" $ oGet:Picture )
         oGet:clear := .F.
      ENDIF

      IF !Empty(aPicData[2]) .AND. oGet:type == "C"
         Value := PadR(Value, Len(aPicData[2]))
         oGet:cargo := Value
      ENDIF
      IF aPicData[3] .AND. oGet:type == "N" .AND. oGet:VarGet() == 0
         oGet:buffer := Space(Max(hb_ULen(oGet:buffer), Len(aPicData[2])))
      ENDIF

      _DispGetBoxText(hWnd, oGet:buffer)

   ELSE

      MsgMiniGuiError("GETBOX: Value Type Mismatch.")

   ENDIF

RETURN

//---------------------------------------------------------------------------//
FUNCTION _RangeCheck(oGet, lo, hi)
//---------------------------------------------------------------------------//
   
   LOCAL value := oGet:VarGet()

   IF !oGet:changed
      RETURN .T.
   ENDIF

RETURN (value >= lo .AND. value <= hi)

//---------------------------------------------------------------------------//
FUNCTION _SetGetBoxColorFocus(aBackColor, aFontColor)
//---------------------------------------------------------------------------//
   
   LOCAL aOldClrFocus := { aClrFocus, aFntFocus }

   lClrFocus := .T.
   
   IF aBackColor != NIL
      aClrFocus := aBackColor
   ENDIF

   IF aFontColor != NIL
      aFntFocus := aFontColor
   ENDIF

RETURN aOldClrFocus

//---------------------------------------------------------------------------//
PROCEDURE _DispGetBoxText(hWnd, cText)
//---------------------------------------------------------------------------//
   
   LOCAL ControlHandle
   LOCAL i

   IF (i := AScan(_HMG_aControlHandles, hWnd)) > 0

      ControlHandle := _HMG_aControlHandles[i]

      IF hmg_IsWindowHandle(ControlHandle)

         IF hb_bitand(hmg_GetWindowLong(ControlHandle, GWL_STYLE), ES_PASSWORD) == ES_PASSWORD
            hmg_SetWindowText(ControlHandle, Replicate("*", Len(Trim(cText))))
         ELSE
            hmg_SetWindowText(ControlHandle, cText)
         ENDIF

      ENDIF

   ENDIF

RETURN

//---------------------------------------------------------------------------//
STATIC PROCEDURE _SetGetBoxCaret(hWnd)
//---------------------------------------------------------------------------//
   hmg_HideCaret(hWnd)
   hmg_DestroyCaret()

   IF !hmg_IsWindowHasStyle(hWnd, ES_READONLY)
      hmg_CreateCaret(hWnd, 0, iif(lInsert, 2, 4), GetWindowHeight(hWnd))
      hmg_ShowCaret(hWnd)
   ENDIF

RETURN

//---------------------------------------------------------------------------//
FUNCTION _GetPictureData(oGet, cPicture)
//---------------------------------------------------------------------------//
   
   LOCAL nAt
   LOCAL nFor
   LOCAL cNum
   LOCAL cPicFunc
   LOCAL cPicMask
   LOCAL lCleanZero
   LOCAL lDecRev

   IF Left(cPicture, 1) == "@"

      nAt := hb_UAt(" ", cPicture)

      IF nAt == 0
         cPicFunc := hb_asciiUpper(cPicture)
         cPicMask := ""
      ELSE
         cPicFunc := hb_asciiUpper(hb_USubStr(cPicture, 1, nAt - 1))
         cPicMask := LTrim(hb_USubStr(cPicture, nAt + 1))
      ENDIF

      IF "D" $ cPicFunc

         cPicMask := Set(_SET_DATEFORMAT)
         FOR EACH cNum IN "yYmMdD"
            cPicMask := StrTran(cPicMask, cNum, "9")
         NEXT 

      ENDIF

      IF (nAt := hb_UAt("S", cPicFunc)) > 0

         FOR nFor := nAt + 1 TO hb_ULen(cPicFunc)
            IF !IsDigit(hb_USubStr(cPicFunc, nFor, 1))
               EXIT
            ENDIF
         NEXT

         cPicFunc := hb_USubStr(cPicFunc, 1, nAt - 1) + hb_USubStr(cPicFunc, nFor)

      ENDIF

      lCleanZero := ( "Z" $ cPicFunc )
      cPicFunc := StrTran(cPicFunc, "Z", "")

      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF

   ELSE

      cPicFunc   := ""
      cPicMask   := cPicture
      lCleanZero := .F.

   ENDIF

   IF oGet:type == "D"
      cPicMask := LTrim(cPicMask)
   ENDIF

   IF Empty(cPicMask)

      DO CASE
      CASE oGet:type == "D"

         cPicMask := Set(_SET_DATEFORMAT)
         FOR EACH cNum IN "yYmMdD"
            cPicMask := StrTran(cPicMask, cNum, "9")
         NEXT 

      CASE oGet:type == "N"

         lDecRev := "," $ Transform(1.1, "9.9")
         cNum := Str(oGet:VarGet())
         IF (nAt := hb_UAt(iif(lDecRev, ",", "."), cNum)) > 0
            cPicMask := Replicate("9", nAt - 1) + iif(lDecRev, ",", ".")
            cPicMask += Replicate("9", hb_ULen(cNum) - hb_ULen(cPicMask))
         ELSE
            cPicMask := Replicate("9", hb_ULen(cNum))
         ENDIF

      CASE oGet:type == "C" .AND. cPicFunc == "@9"

         cPicMask := Replicate("9", hb_ULen(oGet:VarGet()))
         cPicFunc := ""
      ENDCASE

   ENDIF

RETURN { cPicFunc, cPicMask, lCleanZero }

//---------------------------------------------------------------------------//
STATIC FUNCTION _FirstEditable(nId)
//---------------------------------------------------------------------------//
   
   LOCAL nFor
   LOCAL oGet := _HMG_aControlHeadClick[nId]
   LOCAL nMaxLen := hb_ULen(oGet:buffer)

   IF nMaxLen != NIL

      IF _IsEditable(1, nId)
         RETURN 1
      ENDIF

      FOR nFor := 2 TO nMaxLen
         IF _IsEditable(nFor, nId)
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   oGet:TypeOut := .T.

RETURN 0

//---------------------------------------------------------------------------//
STATIC FUNCTION _IsEditable(nPos, nId)
//---------------------------------------------------------------------------//
   
   LOCAL cChar
   LOCAL oGet
   LOCAL nMaxLen
   LOCAL cPicMask := _HMG_aControlInputMask[nId, 2]

   IF Empty(cPicMask)
      RETURN .T.
   ENDIF

   oGet := _HMG_aControlHeadClick[nId]
   nMaxLen := hb_ULen(oGet:buffer)

   IF nPos > hb_ULen(cPicMask) .AND. nPos <= nMaxLen
      RETURN .T.
   ENDIF

   cChar := hb_USubStr(cPicMask, nPos, 1)

   IF oGet:type != NIL

      SWITCH oGet:type
      CASE "C" ; RETURN (cChar $ "!ANX9#LY")
      CASE "N" ; RETURN (cChar $ "9#$*")
      CASE "D"
      CASE "T" ; RETURN (cChar == "9")
      CASE "L" ; RETURN (cChar $ "LY#")
      ENDSWITCH

   ENDIF

RETURN .F.

//---------------------------------------------------------------------------//
STATIC FUNCTION _Input(cChar, nID)
//---------------------------------------------------------------------------//
   
   LOCAL oGet     := _HMG_aControlHeadClick[nId]
   LOCAL cPicFunc := _HMG_aControlInputMask[nId, 1]
   LOCAL cPicMask := _HMG_aControlInputMask[nId, 2]
   LOCAL cLangItem_1 := hb_langMessage(HB_LANG_ITEM_BASE_TEXT + 1)
   LOCAL cLangItem_2 := hb_langMessage(HB_LANG_ITEM_BASE_TEXT + 2)
   LOCAL cPic

   SWITCH oGet:type
   CASE "N"
      DO CASE
      CASE cChar == "-" .AND. !_HMG_aControlMiscData1[nId, 8]
         oGet:minus := .T.  /* The minus symbol can be write in any place */
      CASE cChar == "."
      CASE cChar == ","
         oGet:toDecPos()
         RETURN ""
      CASE !( cChar $ "0123456789+" )
         RETURN ""
      ENDCASE
      EXIT
   CASE "D"
      IF !( cChar $ "0123456789" )
         RETURN ""
      ENDIF
      EXIT
   CASE "L"
      cPic := hb_asciiUpper(cChar)
      IF !( cPic $ "YNTF" + cLangItem_1 + cLangItem_2 )
         RETURN ""
      ENDIF
      IF cPic == cLangItem_1
         cChar := "Y"
      ELSEIF cPic == cLangItem_2
         cChar := "N"
      ENDIF
   ENDSWITCH

   IF !Empty(cPicFunc)
      IF "R" $ cPicFunc .AND. "E" $ cPicFunc
         cChar := hb_USubStr(Transform(cChar, cPicFunc), 4, 1) // Needed for @RE
      ELSE
         cChar := hb_ULeft(Transform(cChar, cPicFunc), 1) // Left needed for @D
      ENDIF
   ENDIF

   IF !Empty(cPicMask)

      cPic := hb_USubStr(cPicMask, oGet:pos, 1)
      SWITCH cPic
      CASE "A"
        IF !hmg_IsAlpha(cChar)
          cChar := ""
        ENDIF
        EXIT
      CASE "N"
        IF !hmg_IsAlpha(cChar) .AND. !hmg_IsDigit(cChar)
          cChar := ""
        ENDIF
        EXIT
      CASE "9"
        IF !hmg_IsDigit(cChar) .AND. cChar != "-"
          cChar := ""
        ENDIF
        EXIT
      CASE "#"
        IF !hmg_IsDigit(cChar) .AND. !( cChar == " " ) .AND. !( cChar $ ".+-" )
          cChar := ""
        ENDIF
        EXIT
      CASE "L"
        IF !( hb_asciiUpper(cChar) $ "YNTF" + cLangItem_1 + cLangItem_2 )
          cChar := ""
        ENDIF
        EXIT
      CASE "Y"
        IF !( hb_asciiUpper(cChar) $ "YN" )
          cChar := ""
        ENDIF
        EXIT
      CASE "$"
        EXIT
      CASE "*"
        IF oGet:type == "N"
          IF !hmg_IsDigit(cChar) .AND. cChar != "-"
            cChar := ""
          ENDIF
        ELSE
          cChar := Transform(cChar, cPic)
        ENDIF
        EXIT
      DEFAULT
        cChar := Transform(cChar, cPic)
      ENDSWITCH
   ENDIF

RETURN cChar

// (JK) HMG 1.1 Experimental Build 12
//---------------------------------------------------------------------------//
STATIC FUNCTION _GetDefinedStatusBarItemValue(ParentHandle, ItemID)
//---------------------------------------------------------------------------//
   
   LOCAL h
   LOCAL nLocID := 0
   LOCAL i

   hb_default(@ItemID, 1)

   FOR EACH h IN _HMG_aControlParentHandles

      i := hb_enumindex(h)

      IF _HMG_aControlType[i] == CONTROL_TYPE_ITEMMESSAGE .AND. h == ParentHandle

         IF ++nLocID == ItemID
            EXIT
         ENDIF

      ENDIF

   NEXT

RETURN _HMG_aControlCaption[i]

// (JK) HMG Experimental 1.1. Build 14
//---------------------------------------------------------------------------//
STATIC FUNCTION _IsChildOfActiveWindow(hWnd)
//---------------------------------------------------------------------------//
   
   LOCAL hActiveWnd := hmg_GetActiveWindow()
   LOCAL lRet := ( _GetParent(hWnd) == hActiveWnd )
   LOCAL hParent

   DO WHILE lRet

      hParent := _GetParent(hWnd)

      IF hActiveWnd != hParent

         IF hb_bitand(hmg_GetWindowLong(hParent, GWL_STYLE), WS_CHILD) > 0
            hWnd := hParent
         ELSE
            lRet := .F.
         ENDIF

      ELSE

         EXIT

      ENDIF

   ENDDO

RETURN lRet

//---------------------------------------------------------------------------//
STATIC FUNCTION _GetParent(hWnd)
//---------------------------------------------------------------------------//

   LOCAL i := AScan(_HMG_aControlHandles, hWnd)

RETURN iif(i > 0, _HMG_aControlParentHandles[i], HMG_NULLHANDLE)
