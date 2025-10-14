//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// Property Grid control source code
// (C)2007-2011 Janusz Pora <januszpora@onet.eu>
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
// Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
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
// Copyright 2001-2009 Alexander S.Kresin <alex@belacy.belgorod.su>

#include "minigui.ch"
#include "i_PropGrid.ch"
#include "i_xml.ch"

#define EM_GETSEL   176
#define EM_SETSEL   177
#define WM_CLEAR    771
#define WM_CHAR     258
//#define WM_SETFOCUS   7
#define WM_KILLFOCUS  8
#define WM_NOTIFY     78
#define WM_KEYDOWN    256
#define WM_COMMAND      0x0111
#define WM_LBUTTONDOWN    513    // 0x0201
#define WM_LBUTTONUP      514    // 0x0202
#define WM_LBUTTONDBLCLK  515    // 0x203

#define BN_CLICKED      0
#define EN_CHANGE       768
/*
 * Combo Box Notification Codes
 */
#define CBN_SELCHANGE       1
#define CBN_KILLFOCUS       4
#define CBN_EDITCHANGE      5

#define TVM_EXPAND   4354
#define TVE_COLLAPSE   1
#define TVE_EXPAND   2
#define TVE_TOGGLE   3


#define TVN_SELCHANGED TVN_SELCHANGEDA
#define TVN_SELCHANGEDA   (-402)

#define CLR_NONE                0xFFFFFFFF

#define COLOR_SCROLLBAR         0
#define COLOR_BACKGROUND        1
#define COLOR_ACTIVECAPTION     2
#define COLOR_INACTIVECAPTION   3
#define COLOR_MENU              4
#define COLOR_WINDOW            5
#define COLOR_WINDOWFRAME       6
#define COLOR_MENUTEXT          7
#define COLOR_WINDOWTEXT        8
#define COLOR_CAPTIONTEXT       9
#define COLOR_ACTIVEBORDER      10
#define COLOR_INACTIVEBORDER    11
#define COLOR_APPWORKSPACE      12
#define COLOR_HIGHLIGHT         13
#define COLOR_HIGHLIGHTTEXT     14
#define COLOR_BTNFACE           15
#define COLOR_BTNSHADOW         16
#define COLOR_GRAYTEXT          17
#define COLOR_BTNTEXT           18
#define COLOR_INACTIVECAPTIONTEXT 19
#define COLOR_BTNHIGHLIGHT      20
#define COLOR_3DDKSHADOW        21
#define COLOR_3DLIGHT           22
#define COLOR_INFOTEXT          23
#define COLOR_INFOBK            24

#define APG_TYPE     1
#define APG_NAME     2
#define APG_VALUE    3
#define APG_DATA     4
#define APG_DIS      5
#define APG_CHG      6
#define APG_DISED    7
#define APG_ID       8
#define APG_INFO     9
#define APG_VALNAME  10

//INFO aRowItem  => (Type,PropertyName,Value,Data,Disabled,changed,ItemID, ItemInfo, ValueName
//                     1        2        3     4     5       6       7       8          9
STATIC hIListSys   := HMG_NULLHANDLE
STATIC aFontName   := { "Font Name", "Font Size", "Bold", "Italic", "Underline", "Strikeout" }
STATIC aFontType   := { "enum", "numeric", "logic", "logic", "logic", "logic" }
STATIC nItemId     := 0

//----------------------------------------------------------------------------//
FUNCTION _DefinePropGrid(ControlName, ParentFormName, row, col, width, height, ;
      change, cFile, lXml, tooltip, fontname, fontsize, gotfocus, lostfocus, onclose, ;
      break, aRowItem, HelpId, bold, italic, underline, strikeout, itemexpand, ;
      backcolor, fontcolor, indent, itemheight, datawidth, ImgList, readonly, lInfo, ;
      infoHeight, changevalue, aheadname, singleexpand, ;
      lOkBtn, lApplyBtn, UserOkProc, lCancelBtn, UserCancelProc, UserHelpProc)
//----------------------------------------------------------------------------//

   LOCAL i
   LOCAL ParentFormHandle
   LOCAL aControlhandle
   LOCAL mVar
   LOCAL FontHandle
   LOCAL aFont
   LOCAL k
   LOCAL hColorIL
   LOCAL lHelpBtn := .F.

   DEFAULT lOkBtn         := .F.
   DEFAULT lApplyBtn      := .F.
   DEFAULT lCancelBtn     := .F.
   DEFAULT ImgList        := 0
   DEFAULT Width          := 240
   DEFAULT Height         := 200
   DEFAULT dataWidth      := 120
   DEFAULT indent         := 12
   DEFAULT change         := ""
   DEFAULT changevalue    := ""
   DEFAULT gotfocus       := ""
   DEFAULT lostfocus      := ""
   DEFAULT onclose        := ""
   DEFAULT cFile          := ""
   DEFAULT aRowItem       := {}
   DEFAULT lInfo          := .F.
   DEFAULT infoHeight     := 80
   DEFAULT UserOkProc     := ""
   DEFAULT UserCancelProc := ""
   DEFAULT UserHelpProc   := ""

   InitPGMessages()

   _HMG_ActivePropGridHandle := HMG_NULLHANDLE
   _HMG_ActiveCategoryHandle := HMG_NULLHANDLE

   IF dataWidth > 0.8 * width
      dataWidth := 0.8 * width
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      aFont := GetFontParam(FontHandle)
      FontName     := aFont[1]
      FontSize     := aFont[2]
      bold         := aFont[3]
      italic       := aFont[4]
      underline    := aFont[5]
      strikeout    := aFont[6]
   ENDIF

   IF _HMG_BeginWindowActive
      ParentFormName := _HMG_ActiveFormName
      IF !Empty(_HMG_ActiveFontName) .AND. FontName == NIL
         FontName := _HMG_ActiveFontName
      ENDIF
      IF !Empty(_HMG_ActiveFontSize) .AND. FontSize == NIL
         FontSize := _HMG_ActiveFontSize
      ENDIF
   ENDIF
   IF _HMG_FrameLevel > 0
      col    := col + _HMG_ActiveFrameCol[_HMG_FrameLevel]
      row    := row + _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   IF !_IsWindowDefined(ParentFormName)
      MsgMiniGuiError("Window: " + ParentFormName + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   IF aheadname != NIL .AND. hb_IsArray(aheadname)
      IF Len(aheadname) > 0 .AND. hb_IsArray(aheadname[1])
         aheadname := aheadname[1]
      ENDIF
   ENDIF
   IF !lOkBtn
      UserOkProc := ""
   ENDIF
   IF !lCancelBtn
      UserCancelProc := ""
   ENDIF
   IF hb_IsBlock(UserHelpProc)
      lHelpBtn := .T.
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   ParentFormHandle = GetFormHandle(ParentFormName)

   IF Row == NIL .OR. Col == NIL

      IF _HMG_SplitLastControl == "TOOLBAR"
         Break := .T.
      ENDIF

      i := GetFormIndex(ParentFormName)

      IF i > 0

         aControlHandle := InitPropGrid(_HMG_aFormReBarHandle[i], col, row, width, height, indent, datawidth, readonly, lInfo, infoHeight, aheadname, singleexpand)

         IF !empty(FontHandle)
            hmg__SetFontHandle(aControlHandle[1], FontHandle)
         ELSE
            IF fontname == NIL
               FontName := _HMG_DefaultFontName
            ENDIF
            IF fontsize == NIL
               FontSize := _HMG_DefaultFontSize
            ENDIF
            FontHandle := hmg__SetFont(aControlHandle[1], FontName, FontSize)
         ENDIF

         hmg_AddSplitBoxItem(aControlhandle[1], _HMG_aFormReBarHandle[i], Width, break, , , , _HMG_ActiveSplitBoxInverted)

         _HMG_SplitLastControl := "PROPGRID"

      ENDIF

   ELSE

      aControlHandle := InitPropGrid(ParentFormHandle, col, row, width, height, indent, datawidth, readonly, lInfo, infoHeight, aheadname, singleexpand, lOkBtn, lApplyBtn, lCancelBtn, lHelpBtn, _HMG_PGLangButton)

   ENDIF

   _HMG_ActivePropGridHandle := aControlHandle[1]

   IF FontHandle != HMG_NULLHANDLE
      hmg__SetFontHandle(aControlHandle[1], FontHandle)
      hmg__SetFontHandle(aControlHandle[2], FontHandle)
   ELSE
      IF fontname == NIL
         fontname := _HMG_DefaultFontName
      ENDIF
      IF fontsize == NIL
         fontsize := _HMG_DefaultFontSize
      ENDIF
      hmg__SetFont(aControlHandle[2], fontname, _HMG_DefaultFontSize, .T., italic, underline, strikeout)
      hmg__SetFont(aControlHandle[3], fontname, _HMG_DefaultFontSize, bold, italic, underline, strikeout)
      IF aControlHandle[5] != HMG_NULLHANDLE
         hmg__SetFont(aControlHandle[5], fontname, _HMG_DefaultFontSize, .T., italic, underline, strikeout)
      ENDIF
      FontHandle := hmg__SetFont(aControlHandle[1], fontname, _HMG_DefaultFontSize, bold, italic, underline, strikeout)
   ENDIF

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, aControlHandle)
   ENDIF

   IF ValType(tooltip) != "U"
      hmg_SetToolTip(aControlHandle[1], tooltip, GetFormToolTipHandle(ParentFormName))
   ENDIF

   IF hb_IsArray(backcolor)
      TreeView_SetBkColor(aControlHandle[1], backcolor)
   ENDIF

   IF hb_IsArray(fontcolor)
      TreeView_SetTextColor(aControlHandle[1], fontcolor)
   ENDIF

   indent := TreeView_GetIndent(aControlHandle[1])

   IF hb_IsNumeric(itemheight)
      TreeView_SetItemHeight(aControlHandle[1], itemheight)
   ELSE
      itemheight := TreeView_GetItemHeight(aControlHandle[1])
   ENDIF

   hColorIL := hmg_InitImageList(( itemheight - 4 ) * 1.4, itemheight - 4, .T.)

   InitPropGridImageList(aControlHandle[1], hColorIL)  // Init Color Image List

   _HMG_ActivePropGridIndex := k
#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_PROPGRID
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := aControlHandle
   _HMG_aControlParentHandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := 0
   _HMG_aControlProcedures         [k] := UserOkProc
   _HMG_aControlPageMap            [k] := aRowItem
   _HMG_aControlValue              [k] := UserCancelProc
   _HMG_aControlInputMask          [k] := itemexpand
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := onclose
   _HMG_aControlHeadClick          [k] := changevalue
   _HMG_aControlRow                [k] := Row
   _HMG_aControlCol                [k] := Col
   _HMG_aControlWidth              [k] := Width
   _HMG_aControlHeight             [k] := Height - IIF(lInfo, infoHeight, 0)
   _HMG_aControlSpacing            [k] := hColorIL
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := {}
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := itemheight
   _HMG_aControlRangeMax           [k] := datawidth
   _HMG_aControlCaption            [k] := cFile
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { indent, ImgList, lInfo, infoHeight, lXml, UserHelpProc, .F. }
   _HMG_aControlMiscData2          [k] := 0

   _InitPgArray(aRowItem, cFile, lXml, k)

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgLoadFile(ParentForm, ControlName, cFile, lXml)
//----------------------------------------------------------------------------//

   LOCAL aProp := {}
   LOCAL k
   
   k := GetControlIndex(ControlName, ParentForm)
   IF Lower(SubStr(cFile, RAt(".", cFile) + 1)) == "xml"
      lXml := .T.
   ENDIF
   IF  !Empty(cFile) .AND. File(cFile)
      IF !lXml
         aProp := _LoadProperty(cFile, k)
      ENDIF
      _InitPgArray(aProp, cFile, lXml, k)
      _HMG_aControlCaption[k] := cFile
      _HMG_aControlMiscData1[k, 5] := lXml
      _ChangeBtnState(_HMG_aControlHandles[k], .F., k)
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION _InitPgArray(aRowItem, cFile, lXml, k)
//----------------------------------------------------------------------------//

   IF  !Empty(cFile) .AND. File(cFile)
      IF lXml
         PgInitItemXml(cFile, k)
         aRowItem := {}
      ELSE
         aRowItem := _LoadProperty(cFile, k)
      ENDIF
   ENDIF
   IF Len(aRowItem) > 0
      PgInitItem(aRowItem, k)
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgBtnEvents(hwndPG, HwndBtn)
//----------------------------------------------------------------------------//
   
   LOCAL i
   LOCAL aHandle
   LOCAL nBtn
   LOCAL aRowItem
   LOCAL cFile
   LOCAL lXml
   
   i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hwndPG})
   IF i > 0 .AND. !empty(HwndBtn) // HwndBtn > 0
      aRowItem := _HMG_aControlPageMap[i]
      cFile    := _HMG_aControlCaption[i]
      lXml     := _HMG_aControlMiscData1[i, 5]
      aHandle  := _HMG_aControlHandles[i]

      nBtn := AScan(aHandle, HwndBtn)
      SWITCH nBtn
      CASE PGB_OK
      CASE PGB_APPLY
         IF _HMG_aControlMiscData1[i, 7] .OR. nBtn == PGB_APPLY
            IF hb_IsBlock(_HMG_aControlProcedures[i])
               _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            ELSE
               PgSaveFile(GetParentFormName( i ), _HMG_aControlNames[i], cFile)
            ENDIF
            _ChangeBtnState(aHandle, .F., i)
         ENDIF
         IF nBtn == PGB_OK
            DoMethod(GetParentFormName(i), "Release")
         ENDIF
         EXIT
      CASE PGB_CANCEL
         IF hb_IsBlock(_HMG_aControlValue[i])
            _DoControlEventProcedure(_HMG_aControlValue[i], i)
         ELSE
            _InitPgArray(aRowItem, cFile, lXml, i)
            _ChangeBtnState(aHandle, .F., i)
         ENDIF
         EXIT
      CASE PGB_HELP
         IF hb_IsBlock(_HMG_aControlMiscData1[i, 6])
            _DoControlEventProcedure(_HMG_aControlMiscData1[i, 6], i)
         ENDIF
      ENDSWITCH
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION _ChangeBtnState(aHandle, lChg, k)
//----------------------------------------------------------------------------//
   IF aHandle[PGB_APPLY] > 0
      IF lChg
         hmg_EnableWindow(aHandle[PGB_APPLY])
         _HMG_aControlMiscData1[k, 7] := .T.
      ELSE
         hmg_DisableWindow(aHandle[PGB_APPLY])
         _HMG_aControlMiscData1[k, 7] := .F.
      ENDIF
   ENDIF
   IF aHandle[PGB_CANCEL] > 0
      IF lChg
         hmg_EnableWindow(aHandle[PGB_CANCEL])
      ELSE
         hmg_DisableWindow(aHandle[PGB_CANCEL])
      ENDIF
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION _DefinePropertyItem(cType, cName, cValue, aData, disabled, disableedit, id, cInfo, cValName, cValNameDef)
//----------------------------------------------------------------------------//
   
   LOCAL typePg := PgIdentType(cType)
   
   DEFAULT cValue      := ""
   DEFAULT aData       := ""
   DEFAULT disabled    := .F.
   DEFAULT cInfo       := ""
   DEFAULT cValNameDef := ""
   DEFAULT cValName    := cValNameDef

   IF hb_IsNumeric(aData)
      aData := LTrim(Str(adata))
   ENDIF
   IF typePG == PG_ERROR
      //      MsgMiniGuiError("Property Item type: "+cType+" wrong defined.")
      MsgMiniGuiError(_HMG_PGLangError[1] + cType + _HMG_PGLangError[2])
   ENDIF
   IF id == NIL
      IF Len(_HMG_ActivePropGridArray) == 0
         nItemId := 100
      ENDIF
      Id := nItemId++
   ENDIF
   IF id != 0
      IF AScan(_HMG_ActivePropGridArray, {|x|x[8] == Id}) > 0
         //         MsgMiniGuiError("Property Item ID double defined.")
         MsgMiniGuiError(_HMG_PGLangError[3])
      ENDIF
   ENDIF
   PgCheckData(typePG, @cValue, @aData, 0)
   AAdd(_HMG_ActivePropGridArray, {cType, cName, cValue, aData, disabled, .F., disableedit, id, cInfo, cValName})

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgCheckData(typePG, cValue, aData, mod)
//----------------------------------------------------------------------------//

   LOCAL n
   LOCAL c
   LOCAL aCol
   LOCAL nValue
   LOCAL cData
   LOCAL aDat
   LOCAL cToken
   LOCAL cErr := ""
   LOCAL ret := .T.

   SWITCH typePG
   CASE PG_STRING
      IF !hb_isChar(cValue)
         cErr := _HMG_PGLangError[4] + " STRING " + _HMG_PGLangError[2]
         ret := .F.
      ENDIF
      EXIT
   CASE PG_INTEGER
      cErr := _HMG_PGLangError[4] + " INTEGER " + _HMG_PGLangError[2]
      IF hb_IsNumeric(cValue)
         cValue := LTrim(Str(Int(cValue)))
      ELSEIF hb_IsChar(cValue)
         IF IsDigit(cValue)
            cValue := LTrim(Str(Int(Val(cValue))))
         ELSE
            ret := .F.
         ENDIF
      ELSE
         ret := .F.
      ENDIF
      EXIT
   CASE PG_DOUBLE
      cErr := _HMG_PGLangError[4] + " DOUBLE " + _HMG_PGLangError[2]
      IF hb_IsNumeric(cValue)
         cValue := LTrim(REMRIGHT(Str(cValue, 16, 8), "0"))
      ELSEIF hb_IsChar(cValue)
         IF IsDigit(cValue) .OR. Left(cValue, 1) == "-"
            cValue := LTrim(REMRIGHT(Str(Val(CharRem(" ", cValue)), 16, 8), "0"))
         ELSE
            ret := .F.
         ENDIF
      ELSE
         ret := .F.
      ENDIF
      IF !Empty(aData) .AND. ret
         IF hb_IsNumeric(aData)
            aData := LTrim(REMRIGHT(Str(aData, 16, 8), "0"))
         ELSEIF hb_IsChar(cValue)
            ret := .T.
         ENDIF
         IF Ret
            FOR n := 1 TO Len(aData)
               c := SubStr(aData, n, 1)
               IF !( c $ "9., " )
                  cErr := _HMG_PGLangError[5] + " DOUBLE" + _HMG_PGLangError[2]
                  ret := .F.
               ENDIF
            NEXT
         ENDIF
      ENDIF
      EXIT
   CASE PG_SYSCOLOR
      IF PgIdentColor(2, cValue) == 0
         cErr := _HMG_PGLangError[4] + " SYSCOLOR " + _HMG_PGLangError[2]
         ret := .F.
      ENDIF
      EXIT
   CASE PG_COLOR
      aData := IIf(hb_IsBlock(aData), Eval(aData), aData)
      aCol := PgIdentData(aData, PG_COLOR)
      TOKENINIT(cValue, " ,()")
      n := 0
      DO WHILE !TOKENEND() .AND. n++ <= 3
         nValue := Val(TOKENNEXT(cValue))
         IF nValue != aCol[n]
            aCol[n] := nValue
         ENDIF
      ENDDO
      TOKENEXIT()
      cValue := aCol2Str(aCol)
      aData :=  aVal2Str(aCol)
      EXIT
   CASE PG_USERFUN
      IF SubStr(AllTrim(aData), 1, 1) != "{"
         aData := "{|x|" + aData + "}"
         IF !hb_isBlock(&aData)
            cErr := _HMG_PGLangError[6] + " USERFUN " + _HMG_PGLangError[2]
            ret := .F.
         ENDIF
      ENDIF
      EXIT
   CASE PG_LOGIC
      IF At(cValue, "true false") == 0
         cErr := _HMG_PGLangError[4] + " LOGIC " + _HMG_PGLangError[2]
         ret := .F.
      ENDIF
      EXIT
   CASE PG_DATE
      IF !Empty(cValue)
         IF Empty(CToD(cValue))
            cErr := _HMG_PGLangError[4] + " DATE " + _HMG_PGLangError[2]
            ret := .F.
         ENDIF
      ENDIF
      EXIT
   //CASE typePG == PG_FONT
   //CASE typePG == PG_ARRAY
   CASE PG_ENUM
      IF !( At(cValue, aData) != 0 .OR. CharOnly(cValue, aData) == cValue )
         cErr := _HMG_PGLangError[4] + " ENUM " + _HMG_PGLangError[2]
         ret := .F.
      ENDIF
      EXIT
   CASE PG_LIST
      cData := aData // aData is a string but it isn't array here
      IF At(cValue, cData) == 0
         cData := ( cData + ";" + AllTrim(cValue) )
      ENDIF
      EXIT
   CASE PG_FLAG
      cData := CharRem("[]", cValue)
      TOKENINIT(cData, ",")
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         IF At(cToken, aData) == 0
            cErr := _HMG_PGLangError[4] + " FLAG " + _HMG_PGLangError[2]
            ret := .F.
         ENDIF
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_SYSINFO
      IF !hb_isBlock(aData)
         IF At("SYSTEM", aData) == 0
            IF At("USERHOME", aData) == 0
               IF At("USERID", aData) == 0
                  IF At("USERNAME", aData) == 0
                     cErr := _HMG_PGLangError[4] + " SYSINFO " + _HMG_PGLangError[2]
                     ret := .F.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      EXIT
   //CASE typePG == PG_IMAGE
   //CASE typePG == PG_FILE
   //CASE typePG == PG_FOLDER
   CASE PG_CHECK
      aDat := PgIdentData(aData)
      cErr := _HMG_PGLangError[4] + " CHECK " + _HMG_PGLangError[2]
      IF Len(aDat) == 1
         IF At(aDat[1], "true false") == 0
            ret := .F.
         ENDIF
      ELSE
         IF AScan(aDat, cValue) == 0
            ret := .F.
         ENDIF
      ENDIF
      EXIT
   CASE PG_SIZE
      cData := CharRem("()", cValue)
      TOKENINIT(cData, ",")
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         IF !IsDigit(cToken)
            cErr := _HMG_PGLangError[4] + " SIZE " + _HMG_PGLangError[2]
            ret := .F.
         ENDIF
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_PASSWORD
      IF Empty(aData)
         aData := Chr(80) + Chr(103) + Chr(75) + Chr(101) + Chr(121)
      ENDIF
   ENDSWITCH
   IF !ret
      IF Mod == 0
         MsgMiniGuiError(cErr)
      ELSE
         MsgExclamation(cErr, _HMG_PGLangMessage[3], , .F.)
      ENDIF
   ENDIF

RETURN ret

//----------------------------------------------------------------------------//
PROCEDURE _EndCategory()
//----------------------------------------------------------------------------//
   AAdd(_HMG_ActivePropGridArray, {"category", "end", "", "", .F., .F., .F., 0, "", ""})

RETURN

//----------------------------------------------------------------------------//
PROCEDURE _EndPropGrid()
//----------------------------------------------------------------------------//
   IF Len(_HMG_ActivePropGridArray) > 0
      PgInitItem(_HMG_ActivePropGridArray, _HMG_ActivePropGridIndex)
   ENDIF
   _HMG_aControlPageMap[_HMG_ActivePropGridIndex] := _HMG_ActivePropGridArray
   _HMG_ActivePropGridHandle := HMG_NULLHANDLE
   _HMG_ActiveCategoryHandle := HMG_NULLHANDLE
   _HMG_ActivePropGridIndex  := 0
   _HMG_ActivePropGridArray  := {}

RETURN

//----------------------------------------------------------------------------//
FUNCTION _ShowInfoItem(ParentForm, ControlName)
//----------------------------------------------------------------------------//

   LOCAL k
   LOCAL aControlHandle

   k := GetControlIndex(ControlName, ParentForm)
   IF k > 0
      aControlHandle := _HMG_aControlHandles[k]
      Pg_ToggleInfo(aControlHandle[1])
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION _AddPropertyItem(ControlName, ParentForm, cCategory, cType, cName, cValue, aData, disabled, disableedit, id, Info, cValName, cValNameDef, mod)
//----------------------------------------------------------------------------//

   LOCAL aRowItem
   LOCAL hWndPG
   LOCAL nIndex
   LOCAL hItem
   LOCAL aNodeHandle := {}
   LOCAL typePg := PgIdentType(cType)

   DEFAULT cCategory   := ""
   DEFAULT cValue      := ""
   DEFAULT aData       := ""
   DEFAULT disabled    := .F.
   DEFAULT disableedit := .F.
   DEFAULT Id          := 0
   DEFAULT Info        := ""
   DEFAULT cValNameDef := ""
   DEFAULT cValName    := cValNameDef
   DEFAULT Mod         := 0

   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   nIndex := GetControlIndex(ControlName, ParentForm)
   IF Empty(cCategory)
      hItem := hmg_TreeView_GetSelection(hWndPG)
   ELSE
      hItem := PG_SearchCategory(hWndPG, cCategory)
   ENDIF

   IF id == 0 .OR. PG_SearchID(hWndPG, Id) == 0

      IF PgCheckData(typePG, @cValue, @aData, Mod)

         aRowItem := { cType, cName, cValue, aData, disabled, .F., disableedit, Id, Info, cValName  }
         IF !empty(hItem)
            IF PG_GETITEM(hWndPG, hItem, PGI_TYPE) == 1
               AAdd(aNodeHandle, hItem)
               PgAddItem(hWndPG, aRowItem, 1, aNodeHandle, nIndex, .T.)
            ELSEIF Empty(cCategory)
               hItem := hmg_TREEVIEW_GETPARENT(hWndPG, hItem)
               AAdd(aNodeHandle, hItem)
               PgAddItem(hWndPG, aRowItem, 1, aNodeHandle, nIndex, .T.)
            ELSE
               MsgInfo(_HMG_PGLangError[7], _HMG_PGLangMessage[3])
            ENDIF
         ELSE
            MsgInfo(_HMG_PGLangError[8] + cCategory + _HMG_PGLangError[9], _HMG_PGLangMessage[3])
         ENDIF
      ENDIF
   ELSE
      MsgInfo(_HMG_PGLangError[10] + AllTrim(Str(Id)) + _HMG_PGLangError[11], _HMG_PGLangMessage[3])
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION _AddPropertyCategory(ControlName, ParentForm, cCategory, cName, id, Info)
//----------------------------------------------------------------------------//
   
   LOCAL aRowItem
   LOCAL hWndPG
   LOCAL nIndex
   LOCAL hItem
   LOCAL nLev
   LOCAL aNodeHandle := {}

   DEFAULT cCategory := ""
   DEFAULT Id        := 0
   DEFAULT Info      := ""

   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   nIndex := GetControlIndex(ControlName, ParentForm)
   IF Empty(cCategory) .OR. Upper(cCategory)  == "ROOT"
      hItem := 0
      nLev  := 0
   ELSE
      hItem := PG_SearchCategory(hWndPG, cCategory)
      nLev  := 1
   ENDIF
   IF id == 0 .OR. PG_SearchID(hWndPG, Id) == 0

      aRowItem := { "category", cName, "", "", .T., .F., .F., Id, Info, "" }
      IF !empty(hItem)
         IF PG_GETITEM(hWndPG, hItem, PGI_TYPE) == PG_CATEG
            AAdd(aNodeHandle, hItem)
            PgAddItem(hWndPG, aRowItem, nLev, aNodeHandle, nIndex, .T.)
         ELSEIF Empty(cCategory)
            hItem := hmg_TREEVIEW_GETPARENT(hWndPG, hItem)
            AAdd(aNodeHandle, hItem)
            PgAddItem(hWndPG, aRowItem, nLev, aNodeHandle, nIndex, .T.)
         ELSE
            MsgInfo(_HMG_PGLangError[7], _HMG_PGLangMessage[3])
         ENDIF
      ELSE
         PgAddItem(hWndPG, aRowItem, nLev, aNodeHandle, nIndex, .F.)
      ENDIF
   ELSE
      MsgInfo(_HMG_PGLangError[10] + AllTrim(Str(Id)) + _HMG_PGLangError[11], _HMG_PGLangMessage[3])
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgInitItem(aRowItem, k)
//----------------------------------------------------------------------------//
   
   LOCAL nLev := 0
   LOCAL n
   LOCAL ControlHandle :=  _HMG_aControlHandles[k, 1]
   LOCAL aNodeHandle := {0} // TODO:

//INFO aRowItem  => (Type,PropertyName,Value,Data,Disabled,changed,DisableEdit, ItemID, ItemInfo, ValueName
//                     1        2        3     4     5       6       7           8          9        10
   hmg_TreeView_DeleteAllItems(ControlHandle)
   FOR n := 1 TO Len(aRowItem)
      nLev := PgAddItem(ControlHandle, aRowItem[n], nLev, aNodeHandle, k)
   NEXT n
   IF _HMG_aControlInputMask[k]
      ExpandPG(ControlHandle, 1)
   ENDIF
   hmg_TreeView_SelectItem(ControlHandle, PG_GetRoot(ControlHandle))
   hmg_SetFocus(ControlHandle)

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgInitItemXml(cFile, k)
//----------------------------------------------------------------------------//
   
   LOCAL nLev
   LOCAL n
   LOCAL i
   LOCAL ControlHandle := _HMG_aControlHandles[k, 1]
   LOCAL aNodeHandle := {0} // TODO:
   LOCAL aRowItem
   LOCAL oXmlDoc
   LOCAL oXmlNode
   LOCAL oXmlSubNode

   oXmlDoc := HXMLDoc():Read(cFile)
   hmg_TreeView_DeleteAllItems(ControlHandle)
   nLev := 0
//INFO aRowItem  => (Type,PropertyName,Value,Data,Disabled,changed,DisableEdit,ItemID, ItemInfo, ValueName
//                     1        2        3     4     5       6       7           8          9       10
   IF !Empty(oXmlDoc:aItems)
      FOR n := 1 TO Len(oXmlDoc:aItems[1]:aItems)
         oXmlNode := oXmlDoc:aItems[1]:aItems[n]
         IF oXmlNode:Title == "category"  .OR. oXmlNode:Title == "font"
            aRowItem := PgSetItemArray(oXmlNode)
            PgAddItem(ControlHandle, aRowItem, nLev, aNodeHandle, k)
            IF !Empty(oXmlNode:aItems)
               FOR i := 1 TO Len(oXmlNode:aItems)
                  oXmlSubNode := oXmlNode:aItems[i]
                  PgGetNextLevel(oXmlSubNode, ControlHandle, nLev, aNodeHandle, k)
               NEXT
            ENDIF
         ELSE
            aRowItem := PgSetItemArray(oXmlNode)
            PgAddItem(ControlHandle, aRowItem, nLev, aNodeHandle, k)
         ENDIF
      NEXT n
      _HMG_aControlCaption[k] := cFile
   ENDIF
   IF _HMG_aControlInputMask[k]
      ExpandPG(ControlHandle, 1)
   ENDIF
   hmg_TreeView_SelectItem(ControlHandle, PG_GetRoot(ControlHandle))
   hmg_SetFocus(ControlHandle)

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgGetNextLevel(oXmlSubNode, ControlHandle, nLev, aNodeHandle, k)
//----------------------------------------------------------------------------//
   
   LOCAL i
   LOCAL oXmlSubNode1
   LOCAL aRowItem

   nLev++
   IF oXmlSubNode:Title == "category"  .OR. oXmlSubNode:Title == "font"
      aRowItem := PgSetItemArray(oXmlSubNode)
      PgAddItem(ControlHandle, aRowItem, nLev, aNodeHandle, k)
      IF !Empty(oXmlSubNode:aItems)
         FOR i := 1 TO Len(oXmlSubNode:aItems)
            oXmlSubNode1 := oXmlSubNode:aItems[i]
            PgGetNextLevel(oXmlSubNode1, ControlHandle, nLev, aNodeHandle, k)
         NEXT
      ENDIF
   ELSE
      aRowItem := PgSetItemArray(oXmlsubNode)
      PgAddItem(ControlHandle, aRowItem, nLev, aNodeHandle, k)
   ENDIF

RETURN nlev

//----------------------------------------------------------------------------//
FUNCTION PgSetItemArray(oXmlNode)
//----------------------------------------------------------------------------//
   
   LOCAL aItem := {}

   AAdd(aItem, oXmlNode:Title)
   AAdd(aItem, PgGetAttr(oXmlNode,"Name"))
   AAdd(aItem, PgGetAttr(oXmlNode,"Value"))
   AAdd(aItem, PgGetAttr(oXmlNode,"cData"))
   AAdd(aItem, (PgGetAttr(oXmlNode,"disabled") == "true"))
   AAdd(aItem, (PgGetAttr(oXmlNode,"changed") == "true"))
   AAdd(aItem, (PgGetAttr(oXmlNode,"disableedit") == "true"))
   AAdd(aItem, Val(PgGetAttr(oXmlNode,"ItemID")))
   AAdd(aItem, PgGetAttr(oXmlNode,"Info"))
   AAdd(aItem, PgGetAttr(oXmlNode,"VarName"))

RETURN aItem

//----------------------------------------------------------------------------//
FUNCTION PgGetAttr(oXmlNode, cAttr)
//----------------------------------------------------------------------------//

   LOCAL xAttr := oXmlNode:GetAttribute(cAttr)

RETURN IIF(xAttr == NIL, "", xAttr)

//----------------------------------------------------------------------------//
FUNCTION ExpandPG(hWndPG, typ)
//----------------------------------------------------------------------------//
   
   LOCAL hItem
   
   hmg_SetFocus(hWndPG)
   hItem := PG_GetRoot(hWndPG)
   DO WHILE !empty(hItem)
      IF PG_GetItem(hWndPG, hItem, PGI_TYPE) ==  PG_CATEG
         DO CASE
         CASE typ == 0
            hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_TOGGLE, hItem)
         CASE typ == 1
            hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_EXPAND, hItem)
         CASE typ == 2
            hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_COLLAPSE, hItem)
         ENDCASE
      ENDIF
      hItem := PG_GetNextItem(hWndPG, hItem)
   ENDDO

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION ExpandCategPG(ParentForm, ControlName, cCategory, typ)
//----------------------------------------------------------------------------//
   
   LOCAL k
   LOCAL hItem
   LOCAL hWndPG

   k := GetControlIndex(ControlName, ParentForm)
   IF k > 0
      hWndPG := _HMG_aControlHandles[k, 1]
      hmg_SetFocus(hWndPG)
      hItem := PG_SearchCategory(hWndPG, cCategory)

      IF !empty(hItem)
         IF PG_GetItem(hWndPG, hItem, PGI_TYPE) ==  PG_CATEG
            DO CASE
            CASE typ == 0
               hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_TOGGLE, hItem)
            CASE typ == 1
               hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_EXPAND, hItem)
            CASE typ == 2
               hmg_SendMessage(hWndPG, TVM_EXPAND, TVE_COLLAPSE, hItem)
            ENDCASE
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgAddItem(ControlHandle, aRowIt, nLev, aNodeHandle, nIndex, lSelect)
//----------------------------------------------------------------------------//
   
   LOCAL n
   LOCAL nNodeH := 0
   LOCAL PropType
   LOCAL nNodePrevH
   LOCAL ImgId
   LOCAL ImgIdSel
   LOCAL cInfo
   LOCAL cValName
   LOCAL aData
   LOCAL nCheck
   LOCAL ItemType
   LOCAL ItemID
   
   DEFAULT lSelect := .F.
   
   PropType := Lower(aRowIt[APG_TYPE])
   ItemType   := PgIdentType(aRowIt[APG_TYPE])
   ItemId := IIF(Len(aRowIt) < APG_ID, 0, aRowIt[APG_ID])
   cInfo := IIF(Len(aRowIt) < APG_INFO, "", aRowIt[APG_INFO])
   cValName := IIF(Len(aRowIt) < APG_VALNAME, "", aRowIt[APG_VALNAME])
   DO CASE
   CASE PropType == "category"
      IF Lower(aRowIt[2]) == "end"
         nLev--
         RETURN nLev
      ELSE
         nNodePrevH := IIF(nLev > 0, aNodeHandle[nLev], 0)
         nNodeH := AddPGItem(ControlHandle, nNodePrevH, "", 0, 0, 0, aRowIt[2], aRowIt[3], aRowIt[4], aRowIt[5], aRowIt[6], aRowIt[7], ItemType, ItemID, cInfo, cValName)
         nLev++
         IF Len(aNodeHandle) >= nLev
            aNodeHandle[nLev] := nNodeH
         ELSE
            AAdd(aNodeHandle, nNodeH)
         ENDIF
         TreeView_SetBoldItem(ControlHandle, nNodeH, .T.)
      ENDIF
   CASE PropType == "font"
      IF nLev > 0
         nNodeH := AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aRowIt[2], aRowIt[3], aRowIt[4], aRowIt[5], aRowIt[6], aRowIt[7], ItemType, ItemID, cInfo, cValName)
         aData := PgIdentData(aRowIt[4], PG_FONT)
         nLev++
         IF Len(aNodeHandle) >= nLev
            aNodeHandle[nLev] := nNodeH
         ELSE
            AAdd(aNodeHandle, nNodeH)
         ENDIF
         FOR n := 1 TO Len(aData)
            AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aData[n, 2], aData[n, 3], "FONT", .F., .F., .F., PgIdentType(aData[n, 1]), 0, "", "")
         NEXT
         nLev--
      ENDIF
   CASE PropType == "flag"
      IF nLev > 0
         nNodeH := AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aRowIt[2], aRowIt[3], aRowIt[4], aRowIt[5], aRowIt[6], aRowIt[7], ItemType, ItemID, cInfo, cValName)
         aData := PgIdentData(aRowIt[4], PG_FLAG, aRowIt[3])
         nLev++
         IF Len(aNodeHandle) >= nLev
            aNodeHandle[nLev] := nNodeH
         ELSE
            AAdd(aNodeHandle, nNodeH)
         ENDIF
         FOR n := 1 TO Len(aData)
            AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aData[n, 2], aData[n, 3], "FLAG", .F., .F., .F., PgIdentType(aData[n, 1]), 0, "", "")
         NEXT
         nLev--
      ENDIF
   CASE PropType == "size"
      IF nLev > 0
         nNodeH := AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aRowIt[2], aRowIt[3], aRowIt[4], aRowIt[5], aRowIt[6], aRowIt[7], ItemType, ItemID, cInfo, cValName)
         aData := PgIdentData(aRowIt[4], PG_SIZE, aRowIt[3])
         nLev++
         IF Len(aNodeHandle) >= nLev
            aNodeHandle[nLev] := nNodeH
         ELSE
            AAdd(aNodeHandle, nNodeH)
         ENDIF
         FOR n := 1 TO Len(aData)
            AddPGItem(ControlHandle, aNodeHandle[nLev], "", 0, 0, 0, aData[n, 1], aData[n, 2], "SIZE", .F., .F., .F., PG_INTEGER, 0, "", "")
         NEXT
         nLev--
      ENDIF
   OTHERWISE
      ImgId := 0
      ImgIdSel := 0
      nCheck := 0
      IF PropType == "color"
         ImgId  := PgAddColorIL(aRowIt[3], nIndex) + 1
         ImgIdSel := ImgId
      ENDIF
      IF PropType == "syscolor"
         ImgId  := PgAddColorIL(aRowIt[3], nIndex) + 1
         ImgIdSel := ImgId
      ENDIF
      IF PropType == "sysinfo"
         aRowIt[3] := PgGetSysInfo(aRowIt)
      ENDIF
      IF PropType == "image"
         ImgId := PgLoadImag(aRowIt[3], nIndex) + 1
         ImgIdSel := ImgId
      ENDIF
      IF PropType == "check"
         aData := PgIdentData(aRowIt[APG_DATA])
         IF Len(aData) == 1
            nCheck := IIF(aData[1] == "true", 2, 1)
         ELSE
            IF (nCheck := AScan(aData, aRowIt[APG_VALUE])) == 0
               nCheck := 1
            ENDIF
         ENDIF
      ENDIF
      IF nLev > 0
         nNodeH := AddPGItem(ControlHandle, aNodeHandle[nLev], "", ImgId, ImgIdSel, nCheck, aRowIt[2], aRowIt[3], aRowIt[4], aRowIt[5], aRowIt[6], aRowIt[7], ItemType, ItemID, cInfo, cValName)
         IF nLev == 1
            TreeView_SetBoldItem(ControlHandle, nNodeH)
         ENDIF
      ENDIF
   ENDCASE
   IF lSelect
      hmg_TreeView_SelectItem(ControlHandle, nNodeH)
   ENDIF

RETURN nLev

//----------------------------------------------------------------------------//
FUNCTION PgGetSysInfo(aRowIt)
//----------------------------------------------------------------------------//

   LOCAL aDan
   LOCAL cDan := aRowIt[APG_VALUE]
   LOCAL typ

   IF hb_IsBlock(aRowIt[APG_DATA])
      cDan := Eval(aRowIt[APG_DATA])
      IF !hb_isChar(cDan)
         cDan := aRowIt[APG_VALUE]
      ENDIF
   ELSE
      typ := Lower(aRowIt[APG_DATA])
      SWITCH typ
      CASE "system"
         aDan := WindowsVersion()
         cDan := aDan[1] + "(" +  aDan[2] + "," + aDan[3] + ")"
         EXIT
      CASE "username"
         cDan := GetComputerName()
         EXIT
      CASE "userid"
         cDan :=   GetUserName()
         EXIT
      CASE "userhome"
         cDan :=  GetMyDocumentsFolder()
      ENDSWITCH
   ENDIF

RETURN cDan

//----------------------------------------------------------------------------//
FUNCTION PgAddColorIL(cColor, k)
//----------------------------------------------------------------------------//
   
   LOCAL hImageLst
   LOCAL nColor
   LOCAL hImage
   LOCAL ItHeight
   LOCAL ItWidth

   hImageLst := _HMG_aControlSpacing[k]
   ItHeight := _HMG_aControlRangeMin[k] - 4
   ItWidth := ItHeight * 1.4
   nColor := PgIdentColor(0, cColor)
   hImage := CREATECOLORBMP(_HMG_aControlParentHandles[k], nColor, ItWidth, ItHeight)

RETURN  IL_AddMaskedIndirect(hImageLst, hImage, , ItWidth, ItHeight, 1)

//----------------------------------------------------------------------------//
FUNCTION PgLoadImag(cFile, k, hItem)
//----------------------------------------------------------------------------//

   LOCAL hImageLst
   LOCAL hImage
   LOCAL ItHeight
   LOCAL ItWidth

   DEFAULT hItem := HMG_NULLHANDLE

   hImageLst := _HMG_aControlSpacing[k]
   ItHeight := _HMG_aControlRangeMin[k] - 4
   ItWidth := ItHeight * 1.4
   hImage := PG_SetPicture(_HMG_aControlHandles[k, 6], cFile, ItWidth, ItHeight)
   IF hItem == HMG_NULLHANDLE
      RETURN IL_AddMaskedIndirect(hImageLst, hImage, , ItWidth, ItHeight, 1)
   ENDIF
   ResetPropGridImageList(_HMG_aControlHandles[k, 1], hItem, hImage)

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION PgIdentData(cData, typePG, cValue, sep)
//----------------------------------------------------------------------------//

   LOCAL aData := {}
   LOCAL cToken
   LOCAL n := 1
   LOCAL pos
   LOCAL cLogic

   DEFAULT sep    := ";"
   DEFAULT typePG := PG_DEFAULT

   SWITCH typePG
   CASE PG_DEFAULT
      TOKENINIT(cData, sep)
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         AAdd(aData, cToken)
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_COLOR
      aData := { 0, 0, 0 }
      cData := REMALL(cData, "()")
      TOKENINIT(cData, sep)
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         aData[n++] := Val(cToken)
         IF n > 3
            EXIT
         ENDIF
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_FONT
      IF sep == ","
         aData := { "", "", "false", "false", "false", "false" }
      ENDIF
      n := 1
      TOKENINIT(cData, sep)
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         IF sep == ","
            IF n < 3
               aData[n] := cToken
            ELSE
               IF (pos := AScan(aFontName,cToken)) > 0
                  aData[pos] := "true"
               ENDIF
            ENDIF
         ELSE
            AAdd(aData, {aFontType[n], aFontName[n], cToken})
         ENDIF
         n++
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_FLAG
      cValue := CharRem("[]", cValue)
      TOKENINIT(cData, ";")
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         cLogic := IIF(At(cToken, cValue) != 0, "true", "false")
         AAdd(aData, {"logic", cToken, cLogic})
         IF cLogic == "false"
            aData[1, 3] := cLogic
         ENDIF
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_SIZE
      TOKENINIT(cData, ";")
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         AAdd(aData, {cToken, ""})
      ENDDO
      TOKENEXIT()
      cValue := CharRem("()", cValue)
      TOKENINIT(cValue, ",")
      n := 1
      DO WHILE !TOKENEND() .AND. n <= Len(aData)
         cToken := AllTrim(TOKENNEXT(cValue))
         aData[n, 2] := cToken
         n++
      ENDDO
      TOKENEXIT()
      EXIT
   CASE PG_FILE
   CASE PG_IMAGE
      TOKENINIT(cData, ";")
      DO WHILE !TOKENEND()
         cToken := AllTrim(TOKENNEXT(cData))
         AAdd(aData, {"File (" + cToken + ")", cToken})
      ENDDO
      TOKENEXIT()
   ENDSWITCH

RETURN aData

//----------------------------------------------------------------------------//
FUNCTION PgIdentType(cType)
//----------------------------------------------------------------------------//

   LOCAL ItemType := PG_DEFAULT

   IF hb_IsChar(cType)
      cType := AllTrim(Lower(cType))
      SWITCH cType
      CASE "category" ; ItemType := PG_CATEG    ; EXIT
      CASE "string"   ; ItemType := PG_STRING   ; EXIT
      CASE "numeric"
      CASE "integer"  ; ItemType := PG_INTEGER  ; EXIT
      CASE "double"   ; ItemType := PG_DOUBLE   ; EXIT
      CASE "logic"    ; ItemType := PG_LOGIC    ; EXIT
      CASE "font"     ; ItemType := PG_FONT     ; EXIT
      CASE "color"    ; ItemType := PG_COLOR    ; EXIT
      CASE "syscolor" ; ItemType := PG_SYSCOLOR ; EXIT
      CASE "array"    ; ItemType := PG_ARRAY    ; EXIT
      CASE "enum"     ; ItemType := PG_ENUM     ; EXIT
      CASE "date"     ; ItemType := PG_DATE     ; EXIT
      CASE "image"    ; ItemType := PG_IMAGE    ; EXIT
      CASE "sysinfo"  ; ItemType := PG_SYSINFO  ; EXIT
      CASE "flag"     ; ItemType := PG_FLAG     ; EXIT
      CASE "check"    ; ItemType := PG_CHECK    ; EXIT
      CASE "size"     ; ItemType := PG_SIZE     ; EXIT
      CASE "file"     ; ItemType := PG_FILE     ; EXIT
      CASE "folder"   ; ItemType := PG_FOLDER   ; EXIT
      CASE "list"     ; ItemType := PG_LIST     ; EXIT
      CASE "userfun"  ; ItemType := PG_USERFUN  ; EXIT
      CASE "password" ; ItemType := PG_PASSWORD ; EXIT
      OTHERWISE       ; ItemType := PG_ERROR
      ENDSWITCH
   ELSE
      SWITCH cType
      CASE PG_CATEG    ; ItemType := "category" ; EXIT
      CASE PG_STRING
      CASE PG_DEFAULT  ; ItemType := "string"   ; EXIT
      CASE PG_INTEGER  ; ItemType := "numeric"  ; EXIT
      CASE PG_DOUBLE   ; ItemType := "double"   ; EXIT
      CASE PG_LOGIC    ; ItemType := "logic"    ; EXIT
      CASE PG_FONT     ; ItemType := "font"     ; EXIT
      CASE PG_COLOR    ; ItemType := "color"    ; EXIT
      CASE PG_SYSCOLOR ; ItemType := "syscolor" ; EXIT
      CASE PG_ARRAY    ; ItemType := "array"    ; EXIT
      CASE PG_ENUM     ; ItemType := "enum"     ; EXIT
      CASE PG_DATE     ; ItemType := "date"     ; EXIT
      CASE PG_IMAGE    ; ItemType := "image"    ; EXIT
      CASE PG_SYSINFO  ; ItemType := "sysinfo"  ; EXIT
      CASE PG_FLAG     ; ItemType := "flag"     ; EXIT
      CASE PG_CHECK    ; ItemType := "check"    ; EXIT
      CASE PG_SIZE     ; ItemType := "size"     ; EXIT
      CASE PG_FILE     ; ItemType := "file"     ; EXIT
      CASE PG_FOLDER   ; ItemType := "folder"   ; EXIT
      CASE PG_LIST     ; ItemType := "list"     ; EXIT
      CASE PG_USERFUN  ; ItemType := "userfun"  ; EXIT
      CASE PG_PASSWORD ; ItemType := "password"
      ENDSWITCH
   ENDIF

RETURN ItemType

//----------------------------------------------------------------------------//
FUNCTION PgIdentColor(met, cColor)
//----------------------------------------------------------------------------//

   LOCAL nColor := CLR_NONE
   LOCAL pos
   LOCAL aCol
   LOCAL cToken
   LOCAL result
   LOCAL aSysColor := ;
      { { COLOR_3DDKSHADOW,   "ControlDark" }, ;
      { COLOR_BTNFACE,      "ButtonFace" }, ;
      { COLOR_BTNHIGHLIGHT, "ButtonHighlight" }, ;
      { COLOR_3DLIGHT,      "ControlLight" }, ;
      { COLOR_BTNSHADOW,    "ButtonShadow" }, ;
      { COLOR_ACTIVEBORDER, "ActiveBorder" }, ;
      { COLOR_ACTIVECAPTION, "ActiveCaption" }, ;
      { COLOR_APPWORKSPACE, "AppWorkspace" }, ;
      { COLOR_BACKGROUND,   "Desktop" }, ;
      { COLOR_BTNTEXT,      "ButtonText" }, ;
      { COLOR_CAPTIONTEXT,  "CaptionText" }, ;
      { COLOR_GRAYTEXT,     "GrayText" }, ;
      { COLOR_HIGHLIGHT,    "Highlight" }, ;
      { COLOR_HIGHLIGHTTEXT, "HighlightText" }, ;
      { COLOR_INACTIVEBORDER, "InactiveBorder" }, ;
      { COLOR_INACTIVECAPTION, "InactiveCaption" }, ;
      { COLOR_INACTIVECAPTIONTEXT, "InactiveCaptionText" }, ;
      { COLOR_INFOBK,       "Tooltip" }, ;
      { COLOR_INFOTEXT,     "TooltipText" }, ;
      { COLOR_MENU,         "Menu" }, ;
      { COLOR_MENUTEXT,     "MenuText" }, ;
      { COLOR_SCROLLBAR,    "Scroll" }, ;
      { COLOR_WINDOW,       "Window" }, ;
      { COLOR_WINDOWFRAME,  "WindowFrame" }, ;
      { COLOR_WINDOWTEXT,   "WindowText" } }
   LOCAL aColor := { { YELLOW,   "YELLOW" }, ;
      { PINK,     "PINK" }, ;
      { RED,      "RED" }, ;
      { FUCHSIA,  "FUCHSIA" }, ;
      { BROWN,    "BROWN" }, ;
      { ORANGE,   "ORANGE" }, ;
      { GREEN,    "GREEN" }, ;
      { PURPLE,   "PURPLE" }, ;
      { BLACK,    "BLACK" }, ;
      { WHITE,    "WHITE" }, ;
      { GRAY,     "GRAY" }, ;
      { BLUE,     "BLUE" } }

   DEFAULT met := 0

   DO CASE
   CASE met == 0
      cColor := AllTrim(cColor)
      IF (pos := AScan(aSysColor, {|x|Upper(x[2]) == Upper(cColor)})) > 0
         nColor := waGetSysColor(aSysColor[pos, 1])
      ELSEIF (pos := AScan(aColor, {|x|Upper(x[2]) == Upper(cColor)})) > 0
         nColor := RGB(aColor[pos, 1], aColor[pos, 2], aColor[pos, 3])
      ELSE
         IF SubStr(cColor, 1, 1) == "(" .AND. RAt(")", cColor) == Len(cColor)
            aCol := {}
            TOKENINIT(cColor, " ,()")
            DO WHILE !TOKENEND()
               cToken := AllTrim(TOKENNEXT(cColor))
               AAdd(aCol, Val(cToken))
            ENDDO
            TOKENEXIT()
            nColor := RGB(aCol[1], aCol[2], aCol[3])
         ENDIF
      ENDIF
      result := nColor
   CASE met == 1
      result := aSysColor
   CASE met == 2
      result := AScan(aSysColor, {|x|Upper(x[2]) == Upper(AllTrim(cColor))})
   ENDCASE

RETURN result

//----------------------------------------------------------------------------//
FUNCTION PgSaveFile(ParentForm, ControlName, cFile)
//----------------------------------------------------------------------------//

   LOCAL oXmlDoc
   LOCAL lSave := .T.
   LOCAL cExt

   cExt := Lower(SubStr(cFile, RAt(".", cFile) + 1))
   IF File(cFile)
      IF !MsgYesNo(_HMG_PGLangMessage[1] + CRLF + _HMG_PGLangButton[5] + ": " + cFile + " ?", _HMG_PGLangMessage[4])
         lSave := .F.
      ENDIF
   ELSEIF Empty(cFile)
      MsgInfo("No File to save", _HMG_BRWLangError[10])
      lSave := .F.
   ENDIF
   IF lSave
      DO CASE
      CASE cExt == "xml"
         oXmlDoc := CreatePropXml(ParentForm, ControlName)
         oXmlDoc:Save(cFile)
      CASE cExt == "txt"
         lSave := CreatePropFile(ParentForm, ControlName, cFile)
      CASE cExt == "ini"
         lSave := CreateIniFile(ParentForm, ControlName, cFile)
      CASE Empty(cExt)
         lSave := SaveMemVarible(ParentForm, ControlName)
      ENDCASE
   ENDIF

RETURN lSave

//----------------------------------------------------------------------------//
FUNCTION CreatePropXml(ParentForm, ControlName)
//----------------------------------------------------------------------------//
   
   LOCAL aLev := {}
   LOCAL aNode := {}
   LOCAL aItemRt
   LOCAL hItem
   LOCAL hParentItem
   LOCAL oXmlDoc := HXMLDoc():new(_HMG_PGEncodingXml)
   LOCAL oXmlSubNode
   LOCAL oXmlNodeAkt
   LOCAL hWndPG := GetPGControlHandle(ControlName, ParentForm)

   hItem := PG_GetRoot(hWndPG)
   hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)      // Parent Item

   oXmlNodeAkt := HXMLNode():New("ROOT")
   oXmlNodeAkt:SetAttribute("Title", "PropertyGrid")
   oXmlDoc:add(oXmlNodeAkt)
   AAdd(aNode, oXmlNodeAkt)
   AAdd(aLev, hParentItem)
   DO WHILE !empty(hItem)
      hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)   // Parent Item
      aItemRt := PG_GetItem(hWndPG, hItem, PGI_ALLDATA)
      IF aItemRt[PGI_CHG]
         Pg_ChangeItem(hWndPG, hItem, .F.)
         PG_REDRAWITEM(hWndPG, hItem)
      ELSEIF aItemRt[PGI_TYPE] == PG_PASSWORD
         aItemRt[PGI_VALUE] := ValueTran(aItemRt[PGI_VALUE], aItemRt[PGI_TYPE], aItemRt[PGI_DATA])
      ENDIF
      DO WHILE ATail(aLev) != hParentItem
         aLev := ASize(aLev, Len(aLev) - 1)
         aNode := ASize(aNode, Len(aLev))
      ENDDO
      oXmlNodeAkt := ATail(aNode)

      IF PgIdentType(aItemRt[PGI_TYPE]) == "category"
         oXmlSubNode := HXMLNode():New(PgIdentType(aItemRt[PGI_TYPE]), , AttrCreate(aItemRt))
         oXmlNodeAkt:add(oXmlSubNode)
         AAdd(aNode, oXmlSubNode)
         AAdd(aLev, hItem)
      ELSE
         oXmlSubNode := HXMLNode():New(PgIdentType(aItemRt[PGI_TYPE]), HBXML_TYPE_SINGLE, AttrCreate(aItemRt))
         oXmlNodeAkt:add(oXmlSubNode)
      ENDIF
      hItem := GetNextItemPG(hWndPG, hItem)
   ENDDO

RETURN oXmlDoc

//----------------------------------------------------------------------------//
FUNCTION GetNextItemPG(hWndPG, hItem)
//----------------------------------------------------------------------------//

   LOCAL TypePG
   LOCAL hItemParent

   TypePG := PG_GetItem(hWndPG, hItem, PGI_TYPE)
   hItemParent := hItem
   hItem := PG_GetNextItem(hWndPG, hItem)
   IF TypePG == PG_FONT .OR. TypePG == PG_FLAG .OR. TypePG == PG_SIZE
      DO WHILE hItemParent == hmg_TreeView_GetParent(hWndPG, hItem)
         hItem := PG_GetNextItem(hWndPG, hItem)
      ENDDO
   ENDIF

RETURN hItem

//----------------------------------------------------------------------------//
FUNCTION  AttrCreate(aItProperty)
//----------------------------------------------------------------------------//
//INFO aItProperty  => (Type,PropertyName,Value,Data,Disabled,changed,DisableEdit, ItemID, ItemInfo, ValueName
//                     1        2          3     4     5       6       7           8          9        10
   LOCAL aAttr := {}

   AAdd(aAttr, {"Name", AttrTran(aItProperty[PGI_NAME], "C")})
   AAdd(aAttr, {"Value", AttrTran(aItProperty[PGI_VALUE], "C")})
   AAdd(aAttr, {"cData", AttrTran(aItProperty[PGI_DATA], "C")})
   AAdd(aAttr, {"disabled", AttrTran(aItProperty[PGI_ENAB], "L")})
   AAdd(aAttr, {"changed", AttrTran(aItProperty[PGI_CHG], "L")})
   AAdd(aAttr, {"disableedit", AttrTran(aItProperty[PGI_DIED], "L")})
   AAdd(aAttr, {"ItemID", AttrTran(aItProperty[PGI_ID], "N")})
   AAdd(aAttr, {"Info", AttrTran(aItProperty[PGI_INFO], "C")})
   AAdd(aAttr, {"VarName", AttrTran(aItProperty[PGI_VAR], "C")})

RETURN aAttr

//----------------------------------------------------------------------------//
FUNCTION AttrTran(xData, type)
//----------------------------------------------------------------------------//

   LOCAL n
   LOCAL cData

   IF xData == NIL
      RETURN IIF(Type == "L", "false", "")
   ENDIF

   SWITCH Type
   CASE "C"
      IF hb_IsChar(xData)
         RETURN xData
      ENDIF
      EXIT
   CASE "N"
      IF hb_IsNumeric(xData)
         RETURN AllTrim(Str(xData))
      ENDIF
      EXIT
   CASE "A"
      IF hb_IsArray(xData)
         cData := ""
         FOR n := 1 TO Len(xData)
            IF hb_IsNumeric(xdata[n])
               cData += AllTrim(Str(xData[n])) + IIF(n < Len(xData), ";", "")
            ENDIF
            IF hb_IsChar(xdata[n])
               cData += xData[n] + IIF(n < Len(xData), ";", "")
            ENDIF
            IF hb_IsLogical(xdata[n])
               cData += IIF(xData[n], "true", "false") + IIF(n < Len(xData), ";", "")
            ENDIF
         NEXT
         RETURN cData
      ENDIF
      EXIT
   CASE "L"
      IF hb_IsLogical(xData)
         RETURN IIF(xData, "true", "false")
      ENDIF
   ENDSWITCH

RETURN ""

//----------------------------------------------------------------------------//
FUNCTION CreatePropFile(ParentForm, ControlName, cFile)
//----------------------------------------------------------------------------//

   LOCAL aLev := {}
   LOCAL aItemRt
   LOCAL hItem
   LOCAL hParentItem
   LOCAL hand
   LOCAL lret := .F.
   LOCAL hWndPG := GetPGControlHandle(ControlName, ParentForm)

   hand := FCreate(cFile, 0)
   IF FError() == 0
      lret := .T.
      hItem := PG_GetRoot(hWndPG)
      hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)      // Parent Item
      AAdd(aLev, hParentItem)
      DO WHILE !empty(hItem)
         aItemRt := PG_GetItem(hWndPG, hItem, PGI_ALLDATA)
         IF aItemRt[PGI_CHG]
            Pg_ChangeItem(hWndPG, hItem, .F.)
            PG_REDRAWITEM(hWndPG, hItem)
         ENDIF
         IF PgIdentType(aItemRt[PGI_TYPE]) == "category"
            AAdd(aLev, hItem)
            lret := ItemRt2File(hand, aItemRt)
         ELSE
            lret := ItemRt2File(hand, aItemRt)
         ENDIF
         hItem := GetNextItemPG(hWndPG, hItem)
         hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)   // Parent Item
         DO WHILE ATail(aLev) != hParentItem
            aLev := ASize(aLev, Len(aLev) - 1)
            lret := ItemRt2File(hand, { "end", "", "", "", "", "", 1, 0, "", "" })
         ENDDO
      ENDDO
      FClose(hand)
   ENDIF

RETURN lret

//----------------------------------------------------------------------------//
FUNCTION CreateIniFile(ParentForm, ControlName, cFile)
//----------------------------------------------------------------------------//
   
   LOCAL cSection := ""
   LOCAL aItemRt
   LOCAL hItem
   LOCAL lret := .F.
   LOCAL hWndPG := GetPGControlHandle(ControlName, ParentForm)

   IF _BeginIni(cFile) == 0
      lRet := .T.
      hItem := PG_GetRoot(hWndPG)
      DO WHILE !empty(hItem)
         aItemRt := PG_GetItem(hWndPG, hItem, PGI_ALLDATA)
         IF aItemRt[PGI_CHG]
            Pg_ChangeItem(hWndPG, hItem, .F.)
            PG_REDRAWITEM(hWndPG, hItem)
         ELSEIF aItemRt[PGI_TYPE] == PG_PASSWORD
            aItemRt[PGI_VALUE] := ValueTran(aItemRt[PGI_VALUE], aItemRt[PGI_TYPE], aItemRt[PGI_DATA])
         ENDIF
         IF PgIdentType(aItemRt[PGI_TYPE]) == "category"
            cSection := aItemRt[PGI_NAME]
         ELSE
            SET SECTION cSection ENTRY aItemRt[PGI_NAME] TO aItemRt[PGI_VALUE]
         ENDIF
         hItem := PG_GetNextItem(hWndPG, hItem)
      ENDDO
      _EndIni()
   ENDIF

RETURN lret

//----------------------------------------------------------------------------//
FUNCTION SaveMemVarible(ParentForm, ControlName)
//----------------------------------------------------------------------------//
   
   LOCAL aItemRt
   LOCAL hItem
   LOCAL cVar
   LOCAL lRet := .F.
   LOCAL hWndPG
   
   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   hItem := PG_GetRoot(hWndPG)
   DO WHILE !empty(hItem)
      aItemRt := PG_GetItem(hWndPG, hItem, PGI_ALLDATA)
      IF aItemRt[PGI_CHG]
         cVar := aItemRt[PGI_VAR]
         IF Type(cVar) != "U"
            IF aItemRt[PGI_TYPE] == PG_ARRAY
               &cVar := PgIdentData(aItemRt[PGI_VALUE], , , ",")
            ELSE
               &cVar := aItemRt[PGI_VALUE]
            ENDIF
            lRet := .T.
         ENDIF
         Pg_ChangeItem(hWndPG, hItem, .F.)
         PG_REDRAWITEM(hWndPG, hItem)
      ENDIF
      hItem := PG_GetNextItem(hWndPG, hItem)
   ENDDO

RETURN lRet

//----------------------------------------------------------------------------//
FUNCTION GetChangedItem(ParentForm, ControlName)
//----------------------------------------------------------------------------//
   
   LOCAL cSection := ""
   LOCAL hItem
   LOCAL hWndPG
   LOCAL aRetItem := {}

   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   hItem := PG_GetRoot(hWndPG)
   DO WHILE !empty(hItem)
      IF PG_GetItem(hWndPG, hItem, PGI_CHG)
         AAdd(aRetItem, PG_GetItem(hWndPG, hItem,PGI_ID))
      ENDIF
      hItem := PG_GetNextItem(hWndPG, hItem)
   ENDDO

RETURN aRetItem

//----------------------------------------------------------------------------//
FUNCTION ItemRt2File(hand, aItemRt)
//----------------------------------------------------------------------------//
   
   LOCAL n
   LOCAL aAttr := AttrCreate(aItemRt)
   LOCAL lin := Chr(34) + PgIdentType(aItemRt[7]) + Chr(34) + " "

   FOR n := 1 TO Len(aAttr)
      IF n == 4
         IF aAttr[n, 2] == "true"
            lin += Chr(34) + aAttr[n, 1] + Chr(34) + " "
         ELSE
            lin += Chr(34) + Chr(34) + " "
         ENDIF
      ELSEIF  n == 5
         lin += Chr(34) + Chr(34) + " "
      ELSE
         lin += Chr(34) + aAttr[n, 2] + Chr(34) + " "
      ENDIF
   NEXT
   lin += CRLF
   FWrite(hand, lin, Len(lin))

RETURN (FError() == 0)

#define NM_SETFOCUS      -7
#define NM_KILLFOCUS    (-8)
#define LVN_ITEMCHANGED (-101)
#define NM_DBLCLK       (-3)

FUNCTION OPROPGRIDEVENTS(hWnd, nMsg, wParam, lParam, hItem, hEdit)

   LOCAL i
   LOCAL ItemType
   LOCAL iCheck
   LOCAL cData
   LOCAL aData
   LOCAL cValue

   SWITCH nMsg

   CASE WM_CHAR
      IF wParam == 27
         _PGInitData(hWnd, hEdit, hItem, PG_GETITEM(hWnd, hItem, PGI_TYPE))
      ENDIF
      EXIT

   CASE WM_LBUTTONUP
      RETURN 0

   CASE WM_LBUTTONDOWN
      RETURN 0

   CASE WM_LBUTTONDBLCLK
      IF !empty(hItem)
         ItemType := PG_GetItem(hWnd, hItem, PGI_TYPE)
         IF ItemType == PG_CATEG .OR. ItemType == PG_FONT .OR. ItemType == PG_FLAG .OR. ItemType == PG_SIZE
            hmg_SendMessage(hWnd, TVM_EXPAND, TVE_TOGGLE, hItem)
            RETURN 1
         ENDIF
      ENDIF
      RETURN 0

   CASE WM_COMMAND
      IF hmg_HIWORD(wParam) == EN_CHANGE .AND. lParam == hItem
         i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hWnd})
         IF i > 0
            _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
            _DoControlEventProcedure(_HMG_aControlHeadClick[i], i)
         ENDIF
      ENDIF
      IF hmg_HIWORD(wParam) == BN_CLICKED
         IF PG_GetItem(hWnd, hItem, PGI_TYPE) == PG_CHECK
            iCheck := hmg_LOWORD(wParam)
            cData  := PG_GETITEM(hWnd, hItem, PGI_DATA)
            IF !Empty(cData)
               aData := PgIdentData(cData)
               IF Len(aData) >= 2
                  cValue := aData[iCheck]
                  PG_SETDATAITEM(hWnd, hItem, cValue, cData, .F.)
               ELSE
                  cValue := PG_GETITEM(hWnd, hItem, PGI_VALUE)
                  cData := IIF(iCheck == 2, "true", "false")
                  PG_SETDATAITEM(hWnd, hItem, cValue, cData, .T.)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF hmg_HIWORD(wParam) == CBN_KILLFOCUS
         IF PG_GetItem(hWnd, hItem, PGI_TYPE) == PG_LIST
            cValue := hmg_GetWindowText(hEdit)
            cData  := PG_GETITEM(hWnd, hItem, PGI_DATA)
            IF !Empty(cData)
               aData := PgIdentData(cData)
               IF AScan(aData, cValue) == 0
                  cData := cData + ";" + cValue
                  PG_SETDATAITEM(hWnd, hItem, cValue, cData, .T.)
               ENDIF
            ELSE
               PG_SETDATAITEM(hWnd, hItem, cValue, cValue, .T.)
            ENDIF
         ENDIF
      ENDIF
      IF hmg_HIWORD(wParam) == EN_CHANGE
         IF PG_GetItem(hWnd, hItem, PGI_TYPE) == PG_DOUBLE .OR. (PG_GetItem(hWnd, hItem, PGI_TYPE) == PG_STRING .AND. !Empty(PG_GETITEM(hWnd, hItem, PGI_DATA)))
            IF hmg_IsWindowHandle(hEdit)
               cValue := hmg_GetWindowText(hEdit)
               cData := PG_GETITEM(hWnd, hItem, PGI_DATA)
               IF !Empty(cData)
                  CharMaskEdit(hEdit, cValue, cData)
               ELSE
                  CharMaskEdit(hEdit, cValue)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE WM_NOTIFY
      i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hmg_GetHwndFrom(lParam)})
      IF i > 0
         IF hmg_GetNotifyCode(lParam) == TVN_SELCHANGED // Tree
            _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
            RETURN 0
         ENDIF

         // PropGrid Double Click .........................

         IF hmg_GetNotifyCode(lParam) == NM_DBLCLK
            _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            RETURN 0
         ENDIF

         IF hmg_GetNotifyCode(lParam) == -181
            ReDrawWindow(_hmg_acontrolhandles[i, 1])
         ENDIF
      ENDIF

   ENDSWITCH

RETURN 1

//----------------------------------------------------------------------------//
FUNCTION aCol2Str(aColor)
//----------------------------------------------------------------------------//
   
   LOCAL n
   LOCAL cColor := "("

   FOR n := 1 TO 3
      cColor += AllTrim(Str(aColor[n]) + IIF(n < 3, ",", ")"))
   NEXT

RETURN cColor

//----------------------------------------------------------------------------//
FUNCTION aFont2Str(aFont)
//----------------------------------------------------------------------------//
   
   LOCAL n
   LOCAL cValue := ""
   LOCAL sep := ""
   
   FOR n := 1 TO Len(aFont)
      IF At(aFont[n, 3], "true false") != 0
         IF aFont[n, 3]  == "true"
            cValue += "," + aFontName[n]
         ENDIF
      ELSE
         cValue += sep + aFont[n, 3]
      ENDIF
      sep := ","
   NEXT

RETURN cValue

//----------------------------------------------------------------------------//
FUNCTION aVal2Str(aData, sep)
//----------------------------------------------------------------------------//
   
   LOCAL n
   LOCAL cData := ""
   
   DEFAULT sep := ";"
   
   IF hb_IsArray(aData)
      FOR n := 1 TO Len(aData)
         IF hb_IsNumeric(adata[n])
            cData += AllTrim(Str(aData[n])) + IIF(n < Len(aData), sep, "")
         ENDIF
         IF hb_IsChar(adata[n])
            cData += aData[n] + IIF(n < Len(aData), sep, "")
         ENDIF
         IF hb_IsLogical(adata[n])
            cData += IIF(aData[n], "true", "false") + IIF(n < Len(aData), sep, "")
         ENDIF
      NEXT
   ENDIF

RETURN cData

//----------------------------------------------------------------------------//
FUNCTION SetPropGridValue(ParentForm, ControlName, nID, cValue, cData, lExp)
//----------------------------------------------------------------------------//
   
   LOCAL hItem
   LOCAL RetVal := ""
   LOCAL ItemType
   LOCAL i
   LOCAL lData := .T.
   LOCAL hWndPG
   LOCAL hEdit := 0

   DEFAULT nID  := 0
   DEFAULT lExp := .F.
   
   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hwndPG})
   IF i > 0
      hEdit := _HMG_aControlMiscData2[i]
   ENDIF
   IF cData == NIL
      lData := .F.
   ENDIF
   IF hWndPG > 0
      IF nId == 0
         hItem := hmg_TreeView_GetSelection(hWndPG)
      ELSE
         hItem := PG_SearchID(hWndPG, nID)
      ENDIF
      IF !empty(hItem) .AND. !PG_GETITEM(hWndPG, hItem, PGI_ENAB)
         ItemType := PG_GETITEM(hWndPG, hItem, PGI_TYPE)
         IF !lData
            cData := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         ENDIF
         IF PgCheckData(ItemType, @cValue, @cData, 1)
            IF hEdit > 0
               hmg_SetWindowText(hEdit, cValue)
            ENDIF
            PG_SETDATAITEM(hWndPG, hItem, cValue, cData, lData)
            IF  PG_ISVISIBLE(hWndPG, hItem)
               PG_REDRAWITEM(hWndPG, hItem)
            ELSEIF lExp
               PG_ENSUREVISIBLE(hWndPG, hItem)
            ENDIF
            hmg_PostMessage(hWndPG, WM_KEYDOWN, VK_ESCAPE, 0)
         ENDIF
      ENDIF
   ENDIF

RETURN retVal

//----------------------------------------------------------------------------//
FUNCTION EnablePropGridItem(ParentForm, ControlName, nID, lEnabled)
//----------------------------------------------------------------------------//
   
   LOCAL hItem
   LOCAL RetVal := ""
   LOCAL hWndPG
   LOCAL hItemSel
   
   DEFAULT nID := 0
   
   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hItemSel := hmg_TreeView_GetSelection(hWndPG)
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   IF hWndPG > 0
      IF nId == 0
         hItem := hmg_TreeView_GetSelection(hWndPG)
      ELSE
         hItem := PG_SearchID(hWndPG, nID)
      ENDIF
      IF !empty(hItem)
         IF (!PG_GETITEM(hWndPG, hItem, PGI_ENAB) .AND. !lEnabled) .OR. ;
               ( PG_GETITEM(hWndPG, hItem, PGI_ENAB) .AND. lEnabled )
            PG_ENABLEITEM(hWndPG, hItem, lEnabled)
            IF hItemSel == hItem
               PG_REDRAWITEM(hWndPG, hItem)
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN retVal

//----------------------------------------------------------------------------//
FUNCTION RedrawPropGridItem(ParentForm, ControlName, nID)
//----------------------------------------------------------------------------//
   
   LOCAL hItem
   LOCAL hWndPG

   DEFAULT nID := 0
   
   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   IF hWndPG > 0
      IF nId == 0
         hItem := hmg_TreeView_GetSelection(hWndPG)
      ELSE
         hItem := PG_SearchID(hWndPG, nID)
      ENDIF
      IF !empty(hItem)
         PG_REDRAWITEM(hWndPG, hItem)
      ENDIF
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION GetPropGridValue(ParentForm, ControlName, nID, lAllData, nSubItem)
//----------------------------------------------------------------------------//
   
   LOCAL hItem
   LOCAL RetVal
   LOCAL ItemType
   LOCAL aData
   LOCAL hWndPG

   DEFAULT nID := 0
   
   IF ParentForm == NIL
      IF _HMG_BeginWindowActive
         ParentForm := _HMG_ActiveFormName
      ELSE
         MsgMiniGuiError("Parent Window is not defined.")
      ENDIF
   ENDIF
   hWndPG := GetPGControlHandle(ControlName, ParentForm)
   IF nId == 0
      hItem := hmg_TreeView_GetSelection(hWndPG)
   ELSE
      hItem := PG_SearchID(hWndPG, nID)
   ENDIF
   IF !empty(hItem)
      IF lAllData
         RetVal   := PG_GETITEM(hWndPG, hItem, PGI_ALLDATA)
         IF RetVal[PGI_TYPE] == PG_PASSWORD
            RetVal[PGI_VALUE] := ValueTran(RetVal[PGI_VALUE], RetVal[PGI_TYPE], RetVal[PGI_DATA], nSubItem)
         ENDIF
      ELSE
         ItemType := PG_GETITEM(hWndPG, hItem, PGI_TYPE)
         RetVal   := PG_GETITEM(hWndPG, hItem, PGI_VALUE)
         aData    := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         RetVal   := ValueTran(RetVal, ItemType, aData, nSubItem)
      ENDIF
   ENDIF

RETURN retVal

//----------------------------------------------------------------------------//
FUNCTION GetPGControlHandle(ControlName, ParentForm)
//----------------------------------------------------------------------------//

   LOCAL aHwnd := GetControlHandle(ControlName, ParentForm)

RETURN  aHwnd[1]

//----------------------------------------------------------------------------//
FUNCTION ValueTran(cValue, ItType, cData, nSubIt)
//----------------------------------------------------------------------------//

   LOCAL xData
   LOCAL aData

   DEFAULT nSubIt := 0

   IF hb_IsChar(cValue)
      SWITCH ItType
      CASE PG_DEFAULT
      CASE PG_CATEG
      CASE PG_STRING
      CASE PG_SYSINFO
      CASE PG_IMAGE
      CASE PG_FLAG
      CASE PG_ENUM
      CASE PG_FILE
      CASE PG_FOLDER
      CASE PG_LIST
      CASE PG_USERFUN
         xData := cValue
         EXIT
      CASE PG_INTEGER
         xData := Int(Val(cValue))
         EXIT
      CASE PG_DOUBLE
         xData := Val(CharRem(" ", cValue))
         EXIT
      CASE PG_SYSCOLOR
      CASE PG_COLOR
         xData := PgIdentColor(0, cValue)
         xData := { waGetRValue(xData), waGetGValue(xData), waGetBValue(xData) }
         EXIT
      CASE PG_LOGIC
         xData := IIF(RTrim(cValue) == "true", .T., .F.)
         EXIT
      CASE PG_DATE
         xData := CToD(cValue)
         EXIT
      CASE PG_FONT
         xData := PgIdentData(cValue, PG_FONT, , ",")
         IF nSubIt > 0 .AND. nSubIt <= Len(xData)
            xdata := Val(xData[nSubIt])
         ELSE
            AEval(xData, {|x|x := IIF(x == "true", .T., .F.)}, 3)
            ASize(xData, 8)
            xData := AIns(xData, 5)
            xData[5] := { 0, 0, 0 }
            xData[8] := 0
         ENDIF
         EXIT
      CASE PG_ARRAY
         xData := PgIdentData(cValue, , , ",")
         EXIT
      CASE PG_CHECK
         aData := PgIdentData(cData)
         IF Len(aData) == 1
            xData := IIF(aData[1] == "true", .T., .F.)
         ELSE
            IF AScan(aData, cValue) == 1
               xData := .F.
            ELSE
               xData := .T.
            ENDIF
         ENDIF
         EXIT
      CASE PG_SIZE
         aData := PgIdentData(cData, PG_SIZE, cValue)
         IF nSubIt > 0 .AND. nSubIt <= Len(aData)
            xdata := Val(aData[nSubIt, 2])
         ENDIF
         EXIT
      CASE PG_PASSWORD
         xdata := CHARXOR(cValue, cData)
      ENDSWITCH
   ENDIF

RETURN xData

//----------------------------------------------------------------------------//
FUNCTION _LoadProperty(cFile, k)
//----------------------------------------------------------------------------//

   LOCAL oFile
   LOCAL cLine
   LOCAL aLine
   LOCAL cExt
   LOCAL aProperty := {}

   nItemId := 100
   cExt := Lower(SubStr(cFile, RAt(".", cFile) + 1))
   oFile := TFileRead():New(cFile)
   oFile:Open()
   IF !oFile:Error()
      DO WHILE oFile:MoreToRead()
         cLine := oFile:ReadLine()
         IF !Empty(cLine)
            IF cExt == "txt"
               aLine := FormatPropertyLine(cLine)
            ELSE
               aLine := FormatIniLine(cLine)
               IF Len(aProperty) > 0
                  IF aLine[1] == "category"
                     AAdd(aProperty, {"category", "end", "", "", .F., .F., , .F., , 0, "", ""})
                  ENDIF
               ENDIF
            ENDIF
            AAdd(aProperty, aLine)
         ENDIF
      ENDDO
      oFile:Close()
      _HMG_aControlCaption[k] := cFile
   ENDIF
   _HMG_aControlPageMap[k] := aProperty

RETURN aProperty

//----------------------------------------------------------------------------//
FUNCTION FormatPropertyLine(cString)
//----------------------------------------------------------------------------//

   LOCAL cToken
   LOCAL lToken
   LOCAL cStr := ""
   LOCAL n
   LOCAL aLine := {}
   LOCAL aRowDef := { "string", "", "", "", .F., .F., .F., 0, "", "" }

   TOKENINIT(cString, " " + Chr(9) + Chr(13))
   DO WHILE (!TOKENEND())
      cToken := TOKENNEXT(cString)
      IF RAt(Chr(34), cToken) < Len(cToken)
         lToken := .F.
         cStr += cToken + " "
      ELSE
         lToken := .T.
         cStr += cToken
      ENDIF
      IF lToken
         cStr := REMALL(cStr, Chr(34))
         AAdd(aLine, cStr)
         cStr := ""
      ENDIF
   ENDDO
   TOKENEXIT()
   IF Len(aLine) < 10
      aLine := ASize(aLine, 10)
   ENDIF
   FOR n := 1 TO 10
      IF aLine[n] == NIL
         IF n == 8
            aLine[n] :=  nItemId++
         ELSE
            aLine[n] := aRowDef[n]
         ENDIF
      ELSE
         DO CASE
         CASE n == 5 .AND. Lower(aLine[5]) == "disabled"
            aLine[5] := .T.
         CASE n == 7 .AND. Lower(aLine[7]) == "disableedit"
            aLine[7] := .T.
         CASE n == 8 .AND. hb_IsChar(aLine[8])
            aLine[8] := Val(aLine[8])
         ENDCASE
      ENDIF
   NEXT

RETURN aLine

//----------------------------------------------------------------------------//
FUNCTION FormatIniLine(cString)
//----------------------------------------------------------------------------//
   
   LOCAL cToken
   LOCAL n
   LOCAL aLine
   LOCAL aRowDef := { "string", "", "", "", .F., .F., .F., 0, "", "" }
   
   cString := LTrim(cString)
   TOKENINIT(cString, "=" + Chr(9) + Chr(13))
   IF At("[", cString) == 1
      aLine := { "category" }
   ELSE
      aLine := { "string" }
   ENDIF
   cString := CharRem("[]", cString)
   DO WHILE (!TOKENEND())
      cToken := TOKENNEXT(cString)
      AAdd(aLine, cToken)
   ENDDO
   TOKENEXIT()
   IF Len(aLine) < 10
      aLine := ASize(aLine, 10)
   ENDIF
   FOR n := 1 TO 10
      IF aLine[n] == NIL
         IF n == 8
            aLine[n] := nItemId++
         ELSE
            aLine[n] := aRowDef[n]
            IF n == 5 .AND. aLine[1] == "category"
               aLine[n] := .T.
            ENDIF
         ENDIF
      ENDIF
   NEXT

RETURN aLine

//----------------------------------------------------------------------------//
FUNCTION OPGEDITEVENTS(hWnd, nMsg, wParam, lParam, hWndPG, hItem)
//----------------------------------------------------------------------------//

   LOCAL icp
   LOCAL icpe
   LOCAL i
   LOCAL n
   LOCAL x
   LOCAL ItemType
   LOCAL nColor
   LOCAL Pos
   LOCAL hImage
   LOCAL ItHeight
   LOCAL hParentItem
   LOCAL hChildItem
   LOCAL ParentItemType
   LOCAL cParentName
   LOCAL aData
   LOCAL aDataNew
   LOCAL cData
   LOCAL cDataNew
   LOCAL cValue
   LOCAL cVal
   LOCAL cFltr
   LOCAL lAll
   LOCAL cFold
   LOCAL bData
   LOCAL lChg

   lParam := NIL //unused parameter

   IF (i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hwndPG})) == 0
      RETURN 0
   ENDIF
   IF (x := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])) > 0
      cParentName :=  _HMG_aFormNames[x]
   ENDIF
   ItHeight := _HMG_aControlRangeMin[i] - 4
   _HMG_aControlMiscData2[i] := hWnd
   SWITCH nMsg
   CASE WM_CHAR
      icp :=  hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      icpe := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      cValue := hmg_GetWindowText(hWnd)
      IF wParam == 27
         _PGInitData(hWndPG, hWnd, hItem, PG_GETITEM(hWndPG, hItem, PGI_TYPE))
         hmg_SetFocus(hWndPG)
      ELSEIF wParam >= 32
         _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
      ENDIF
      // simulate overwrite mode
      IF !IsInsertActive() .AND. wParam != 13 .AND. wParam != 8 .AND. SubStr(cValue, icp + 1, 1) != Chr(13)
         IF IsAlpha(Chr(wParam)) .OR. IsDigit(Chr(wParam))
            IF icp != icpe
               hmg_SendMessage(hWnd, WM_CLEAR, 0, 0)
               hmg_SendMessage(hWnd, EM_SETSEL, icpe, icpe)
            ELSE
               hmg_SendMessage(hWnd, EM_SETSEL, icp, icp + 1)
               hmg_SendMessage(hWnd, WM_CLEAR, 0, 0)
               hmg_SendMessage(hWnd, EM_SETSEL, icp, icp)
            ENDIF
         ELSE
            IF wParam == 1
               hmg_SendMessage(hWnd, EM_SETSEL, 0, -1)
            ENDIF
         ENDIF
      ELSE
         IF wParam == 1
            hmg_SendMessage(hWnd, EM_SETSEL, 0, -1)
         ENDIF
      ENDIF
      EXIT
   CASE WM_COMMAND
      hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)      // Parent Item
      hChildItem  := hmg_TreeView_GetChild(hWndPG, hParentItem) // First Child Item
      cValue := hmg_GetWindowText(hWnd)
      SWITCH hmg_HIWORD(wParam)
      CASE BN_CLICKED
         lChg := .F.
         SWITCH hmg_LOWORD(wParam)
         CASE PG_COLOR
            cData := PG_GETITEM(hWndPG, hItem, PGI_DATA)
            aData := PgIdentData(cData, PG_COLOR)
            aDataNew := GetColor(aData)
            IF aDataNew[1] != NIL
               cData := AttrTran(aDataNew, "A")
               cValue  := aCol2Str(aDataNew)
               hmg_SetWindowText(hWnd, cValue)
               nColor := PgIdentColor(0, cValue)
               hImage := CREATECOLORBMP(hWndPG, nColor, ItHeight * 1.4, ItHeight)
               ResetPropGridImageList(hWndPG, hItem, hImage)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_USERFUN
            cValue := hmg_GetWindowText(hWnd)
            cData := PG_GETITEM(hWndPG, hItem, PGI_DATA)
            bData := &(cData)
            cValue := Eval(bData, cValue)
            IF cValue != NIL .AND. hb_IsChar(cValue)
               hmg_SetWindowText(hWnd, cValue)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_FONT
            cData := PG_GETITEM(hWndPG, hItem, PGI_DATA)
            aData := PgIdentData(cData, PG_FONT)
            aDataNew := GetFont(aData[1, 3], Val(aData[2, 3]), ;
               iif(len(aData) >= 3, aData[3, 3] == "true", .F.), ;
               iif(len(aData) >= 4, aData[4, 3] == "true", .F.), , ;
               iif(len(aData) >= 5, aData[5, 3] == "true", .F.), ;
               iif(len(aData) >= 6, aData[6, 3] == "true", .F.))
            IF !Empty(aDataNew[1])
               ADel(aDataNew, 5)
               ASize(aDataNew, 6)
               cData := AttrTran(aDataNew, "A")
               aData := PgIdentData(cData, PG_FONT)
               cValue  := aFont2Str(aData)
               hmg_SetWindowText(hWnd, cValue)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_IMAGE
            cData := hmg_GetWindowText(hWnd)
            cFltr := PgIdentData(PG_GETITEM(hWndPG, hItem, PGI_DATA), PG_IMAGE)
            cDataNew := GetFile(cFltr, "Image File", cData, .F., .T.)
            IF !Empty(cDataNew)
               cValue := cDataNew
               hmg_SetWindowText(hWnd, cValue)
               PgLoadImag(cValue, i, hItem)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_FILE
            cData := hmg_GetWindowText(hWnd)
            cFltr := PgIdentData(PG_GETITEM(hWndPG, hItem, PGI_DATA), PG_FILE)
            cDataNew := GetFile(cFltr, "File", cData, .F., .T.)
            IF !Empty(cDataNew)
               cValue := cDataNew
               hmg_SetWindowText(hWnd, cValue)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_FOLDER
            cData := hmg_GetWindowText(hWnd)
            cFold := PG_GETITEM(hWndPG, hItem, PGI_DATA)
            cDataNew := GetFolder(cFold, cData)
            IF !Empty(cDataNew)
               cValue := cDataNew
               hmg_SetWindowText(hWnd, cValue)
               lChg := .T.
            ENDIF
            EXIT
         CASE PG_ARRAY
            cValue :=  hmg_GetWindowText(hWnd)
            cDataNew := ArrayDlg(cValue, cParentName)
            IF !Empty(cDataNew)
               hmg_SetWindowText(hWnd, cDataNew)
               lChg := .T.
            ENDIF
         ENDSWITCH
         IF lChg
            _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
         ENDIF
         EXIT
      CASE CBN_SELCHANGE
         IF PG_GETITEM(hWndPG, hItem, PGI_VALUE) != hmg_ComboGetString(hWnd, ComboGetCursel(hWnd))
            _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
            cValue := hmg_ComboGetString(hWnd, ComboGetCursel(hWnd))
            cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
            aData := PgIdentData(cData)
            IF AScan(aData, cValue) == 0
               cData := cData + ";" + cValue
            ENDIF
            hmg_SetWindowText(hWnd, cValue)
            PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .T.)
         ENDIF
      ENDSWITCH
      EXIT
   CASE WM_KILLFOCUS
      ItemType   := PG_GETITEM(hWndPG, hItem, PGI_TYPE)
      hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)      // Parent Item
      hChildItem  := hmg_TreeView_GetChild(hWndPG, hParentItem) // First Child Item
      //    _HMG_aControlMiscData2[i] := 0
      SWITCH ItemType
      CASE PG_DEFAULT
      CASE PG_CATEG
      CASE PG_LOGIC
      CASE PG_ARRAY
      CASE PG_LIST
      CASE PG_STRING
      CASE PG_INTEGER
      CASE PG_DOUBLE
      CASE PG_DATE
      CASE PG_ENUM
         cValue := hmg_GetWindowText(hWnd)
         IF ItemType == PG_DOUBLE
            cValue := CharRem(" ", cValue)
            IF (Pos := RAt(".", cValue)) > 0
               cValue := CharRem(".", Left(cValue, Pos)) + SubStr(cValue, Pos)
            ENDIF
         ENDIF
         ParentItemType := PG_GETITEM(hWndPG, hParentItem, PGI_TYPE)
         PG_SETDATAITEM(hWndPG, hItem, cValue, "", .F.)
         IF PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
            _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
         ENDIF
         SWITCH ParentItemType
         CASE PG_FONT
            cValue := ""
            cValue := PG_GETITEM(hWndPG, hChildItem, PGI_VALUE)
            cData  := cValue
            lChg   := .F.
            n := 1
            DO WHILE (hChildItem := hmg_TreeView_GetNextSibling(hWndPG, hChildItem)) > 0
               IF hmg_TreeView_GetParent(hWndPG, hChildItem) == hParentItem
                  n++
                  lChg := lChg .OR. PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
                  cVal := PG_GETITEM(hWndPG, hChildItem, PGI_VALUE)
                  cData += ";" + cVal
                  IF At(cVal, "true false") != 0
                     IF cVal  == "true"
                        cValue += "," + aFontName[n]
                     ENDIF
                  ELSE
                     cValue += "," + cVal
                  ENDIF
               ENDIF
            ENDDO
            IF !Empty(cValue) .AND. lChg
               PG_SETDATAITEM(hWndPG, hParentItem, cValue, cData, .T.)
               PG_REDRAWITEM(hWndPG, hParentItem)
               IF PG_GETITEM(hWndPG, hParentItem, PGI_CHG)
                  _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
               ENDIF
               hmg_TreeView_SelectItem(hWndPG, hItem)
               lChg   := .F.
            ENDIF
            EXIT
         CASE PG_FLAG
            cData := PG_GETITEM(hWndPG, hParentItem, PGI_DATA)
            lAll := .F.
            lChg   := .F.
            IF hItem == hChildItem
               lAll := PG_GETITEM(hWndPG, hItem, PGI_VALUE) == "true"
               cValue := "[" + PG_GETITEM(hWndPG, hItem, PGI_NAME)
            ELSE
               PG_SETDATAITEM(hWndPG, hChildItem, "false", "", .F.)
               cValue := "["
            ENDIF
            DO WHILE (hChildItem := hmg_TreeView_GetNextSibling(hWndPG, hChildItem)) > 0
               IF hmg_TreeView_GetParent(hWndPG, hChildItem) == hParentItem
                  IF lAll
                     PG_SETDATAITEM(hWndPG, hChildItem, "false", "", .F.)
                  ENDIF
                  lChg := lChg .OR. PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
                  cVal := PG_GETITEM(hWndPG, hChildItem, PGI_VALUE)
                  IF cVal  == "true"
                     cValue += IIF(Len(cValue) > 1, ",", "") + PG_GETITEM(hWndPG, hChildItem, PGI_NAME)
                  ENDIF
               ENDIF
            ENDDO
            IF lChg
               cValue += "]"
               PG_SETDATAITEM(hWndPG, hParentItem, cValue, cData, .T.)
               ReDrawWindow(hWndPG, hParentItem)
               IF PG_GETITEM(hWndPG, hParentItem, PGI_CHG)
                  _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
               ENDIF
               hmg_TreeView_SelectItem(hWndPG, hItem)
               lChg   := .F.
            ENDIF
            EXIT
         CASE PG_SIZE
            lChg   := .F.
            hParentItem := hmg_TreeView_GetParent(hWndPG, hItem)
            hChildItem  := hmg_TreeView_GetChild(hWndPG, hParentItem)
            cData  := PG_GETITEM(hWndPG, hParentItem, PGI_DATA)
            cValue := "(" + PG_GETITEM(hWndPG, hChildItem, PGI_VALUE)
            lChg := lChg .OR. PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
            DO WHILE (hChildItem := hmg_TreeView_GetNextSibling(hWndPG, hChildItem)) > 0
               IF hmg_TreeView_GetParent(hWndPG, hChildItem) == hParentItem
                  lChg := lChg .OR. PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
                  cValue += "," + PG_GETITEM(hWndPG, hChildItem, PGI_VALUE)
               ENDIF
            ENDDO
            cValue += ")"
            IF lChg
               PG_SETDATAITEM(hWndPG, hParentItem, cValue, cData, .T.)
               PG_REDRAWITEM(hWndPG, hParentItem)
               IF PG_GETITEM(hWndPG, hParentItem, PGI_CHG)
                  _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
               ENDIF
               PG_REDRAWITEM(hWndPG, hItem)
               lChg := .F.
            ENDIF
         ENDSWITCH
         EXIT
      CASE PG_FONT
         cValue := hmg_GetWindowText(hWnd)
         aData := PgIdentData(cValue, PG_FONT, , ",")
         cData := AttrTran(aData, "A")
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .T.)
         IF PG_GETITEM(hWndPG, hItem, PGI_CHG)
            aData := PgIdentData(cData, PG_FONT, , ";")
            hChildItem  := hmg_TreeView_GetChild(hWndPG, hItem)
            PG_SETDATAITEM(hWndPG, hChildItem, aData[1, 3], "FONT", .T.)
            PG_REDRAWITEM(hWndPG, hChildItem)
            IF PG_GETITEM(hWndPG, hChildItem, PGI_CHG)
               _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
            ENDIF
            DO WHILE (hChildItem := hmg_TreeView_GetNextSibling(hWndPG, hChildItem)) > 0 // TODO:
               IF hmg_TreeView_GetParent(hWndPG, hChildItem) == hItem
                  IF  ( pos := AScan(aData,{|fIt|fIt[2] == PG_GETITEM(hWndPG, hChildItem, PGI_NAME)}) ) > 0
                     PG_SETDATAITEM(hWndPG, hChildItem, aData[pos, 3], "FONT", .T.)
                     PG_REDRAWITEM(hWndPG, hChildItem)
                     hmg_TreeView_SelectItem(hWndPG, hItem)
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         EXIT
      //CASE PG_LOGIC // TODO: Duplicate case value
      //   cValue := hmg_ComboGetString(hWnd, ComboGetCursel(hWnd))
      //   PG_SETDATAITEM(hWndPG, hItem, cValue, "", .F.)
      //   EXIT
      CASE PG_IMAGE
         cValue := hmg_GetWindowText(hWnd)
         cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .T.)
         PgLoadImag(cValue, i, hItem)
         EXIT
      CASE PG_FILE
      //CASE PG_ENUM // TODO: Duplicate case value
      CASE PG_FOLDER
         cValue := hmg_GetWindowText(hWnd)
         cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .T.)
         EXIT
      CASE PG_USERFUN
         cValue := hmg_GetWindowText(hWnd)
         cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .F.)
         EXIT
      CASE PG_PASSWORD
         cValue := hmg_GetWindowText(hWnd)
         cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
         cValue := CHARXOR(cValue, cData)
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .F.)
         EXIT
      //CASE PG_LIST // TODO: Duplicate case value
      //   cValue := hmg_GetWindowText(hWnd)
      //   cData  := PG_GETITEM(hWndPG, hItem, PGI_DATA)
      //   aData := PgIdentData(cData)
      //   IF AScan(aData, cValue) == 0
      //      cData := cData + ";" + cValue
      //      PG_SETDATAITEM(hWnd, hItem, cValue, cData, .T.)
      //   ENDIF
      //   EXIT
      CASE PG_SYSCOLOR
         cValue := hmg_ComboGetString(hWnd, ComboGetCursel(hWnd))
         nColor := PgIdentColor(0, cValue)
         hImage := CREATECOLORBMP(hWndPG, nColor, ItHeight * 1.4, ItHeight)
         ResetPropGridImageList(hWndPG, hItem, hImage)
         PG_SETDATAITEM(hWndPG, hItem, cValue, "", .F.)
         EXIT
      CASE PG_COLOR
         cValue := hmg_GetWindowText(hWnd)
         aData  := PgIdentData(cValue, PG_COLOR, , ",")
         cValue := aCol2Str(aData)
         cData  := aVal2Str(aData)
         nColor := PgIdentColor(0, cValue)
         hImage := CREATECOLORBMP(hWndPG, nColor, ItHeight * 1.4, ItHeight)
         ResetPropGridImageList(hWndPG, hItem, hImage)
         PG_SETDATAITEM(hWndPG, hItem, cValue, cData, .T.)
      ENDSWITCH
      IF PG_GETITEM(hWndPG, hItem, PGI_CHG)
         _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
      ENDIF
   ENDSWITCH

RETURN 0

//----------------------------------------------------------------------------//
FUNCTION _PGInitData(hWnd, hEdit, hWndItem, ItemType)
//----------------------------------------------------------------------------//

   LOCAL i
   LOCAL n
   LOCAL aSysColor
   LOCAL nColor
   LOCAL hImage
   LOCAL ItHeight
   LOCAL aData
   LOCAL hParentItem

   i := AScan(_HMG_aControlHandles, {|x|hb_IsArray(x) .AND. x[1] == hWnd})
   IF i > 0
      ItHeight := _HMG_aControlRangeMin[i] - 4
      IF PG_GETITEM(hWnd, hWndItem, PGI_CHG)
         _ChangeBtnState(_HMG_aControlHandles[i], .T., i)
      ENDIF
      _HMG_aControlMiscData2[i] := hEdit
      SWITCH ItemType
      CASE PG_DEFAULT
      CASE PG_CATEG
      CASE PG_ARRAY
      CASE PG_INTEGER
      CASE PG_SYSINFO
      CASE PG_USERFUN
         hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         EXIT
      CASE PG_STRING
         aData := PG_GETITEM(hWnd, hWndItem, PGI_DATA)
         IF !Empty(aData)
            hmg_SetWindowText(hEdit, Transform(PG_GETITEM(hWnd, hWndItem, PGI_VALUE), aData))
         ELSE
            hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         ENDIF
         EXIT
      CASE PG_DOUBLE
         aData := PG_GETITEM(hWnd, hWndItem, PGI_DATA)
         IF !Empty(aData)
            hmg_SetWindowText(hEdit, FormatDouble(PG_GETITEM(hWnd, hWndItem, PGI_VALUE), aData))
         ELSE
            hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         ENDIF
         EXIT
      CASE PG_PASSWORD
         aData := PG_GETITEM(hWnd, hWndItem, PGI_DATA)
         hmg_SetWindowText(hEdit, CHARXOR(PG_GETITEM(hWnd, hWndItem, PGI_VALUE), aData))
         EXIT
      CASE PG_LOGIC
         ComboBoxReset(hEdit)
         hmg_ComboAddString(hEdit, "true")
         hmg_ComboAddString(hEdit, "false")
         ComboSetCurSel(hEdit, IIF(Lower(PG_GETITEM(hWnd, hWndItem, PGI_VALUE)) == "true", 1, 2))
         EXIT
      CASE PG_ENUM
      CASE PG_LIST
         hParentItem := hmg_TreeView_GetParent(hWnd, hWndItem)      // Parent Item
         hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         ComboBoxReset(hEdit)
         IF PG_GETITEM(hWnd, hParentItem, PGI_TYPE) == PG_FONT
            PG_GetFonts(hEdit)
            n := ComboFindString(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
            ComboSetCurSel(hEdit, n)
         ELSE
            aData := PgIdentData(PG_GETITEM(hWnd, hWndItem, PGI_DATA))
            FOR n := 1 TO Len(aData)
               PGCOMBOADDSTRING(hEdit, aData[n], 0)
            NEXT
            ComboSetCurSel(hEdit, AScan(aData, PG_GETITEM(hWnd, hWndItem, PGI_VALUE)))
         ENDIF
         hmg_TreeView_SelectItem(hWnd, hWndItem)
         EXIT
      CASE PG_FONT
      CASE PG_FLAG
      CASE PG_SIZE
         hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         EXIT
      CASE PG_COLOR
      CASE PG_IMAGE
      CASE PG_FILE
      CASE PG_FOLDER
         hmg_SetWindowText(hEdit, PG_GETITEM(hWnd, hWndItem, PGI_VALUE))
         EXIT
      CASE PG_SYSCOLOR
         aSysColor := PgIdentColor(1)
         ComboBoxReset(hEdit)
         IF hIListSys == HMG_NULLHANDLE
            hIListSys := hmg_InitImageList(ItHeight * 1.4, ItHeight, .F., 0)
            FOR n := 1 TO Len(aSysColor)
               nColor := waGetSysColor(aSysColor[n, 1])
               hImage := CREATECOLORBMP(hWnd, nColor, ItHeight * 1.4, ItHeight)
               IL_AddMaskedIndirect(hIListSys, hImage, , ItHeight * 1.4, ItHeight, 1)
            NEXT
         ENDIF
         FOR n := 1 TO Len(aSysColor)
            PGCOMBOADDSTRING(hEdit, aSysColor[n, 2], hIListSys)
         NEXT
         ComboSetCurSel(hEdit, AScan(aSysColor, {|colIt|colIt[2] == PG_GETITEM(hWnd, hWndItem, PGI_VALUE)}))
      ENDSWITCH
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//
FUNCTION ArrayDlg(cArr, FormName)
//----------------------------------------------------------------------------//

   LOCAL aItem
   LOCAL aItemOld
   LOCAL aPos
   LOCAL nRow
   LOCAL nCol
   LOCAL cData := ""
   LOCAL cNewArr := ""
   LOCAL lOk := .F.
   
   IF _IsWIndowDefined(FormName)
      aPos := hmg_GetCursorPos()
      nRow := DialogUnitsY(aPos[1])
      nCol := DialogUnitsX(aPos[2])

      nRow := IIF(nRow + 300 >  getdesktopheight(), nRow - 300, nRow)
      nCol := IIF(nCol + 270 > getdesktopwidth(), nCol - 270, nCol)

      aItem := PgIdentData(cArr, , , ",")
      aItemOld := AClone(aItem)
      DEFINE FONT Font_7 FONTNAME "Arial" SIZE 9

      DEFINE DIALOG Dlg_1 OF &FormName  AT nRow, nCol WIDTH 270 HEIGHT 300 FONT "Font_7" ;
         CAPTION "Array Property" MODAL ;
         DIALOGPROC  DialogFun(@lOk, @aItem, aItemOld) ON INIT SetInitItem(aItem, 0)

         @ 10, 10 TEXTBOX tbox_1 VALUE cData HEIGHT 24 WIDTH 150   ID 101
         @ 10, 170 BUTTON BtnAdd ID 110 CAPTION _HMG_aABMLangButton[14]  WIDTH 90 HEIGHT 24

         @ 40, 10 FRAME Frm   ID 100  WIDTH 250  HEIGHT 0

         @ 50, 10  LISTBOX Lst_1 ID 102 WIDTH 150 HEIGHT 170  ITEMS aItem

         @ 50, 170 BUTTON BtnRem ID 111 CAPTION _HMG_aABMLangButton[15] WIDTH 90 HEIGHT 24
         @ 80, 170 BUTTON BtnUp  ID 112 CAPTION "&Up"     WIDTH 90 HEIGHT 24
         @ 110, 170 BUTTON BtnDwn ID 113 CAPTION "&Down"   WIDTH 90 HEIGHT 24

         @ 230,  10 BUTTON Btn1 ID 105 CAPTION "&Accept" WIDTH 70 HEIGHT 24 DEFAULT
         @ 230,  90 BUTTON Btn2 ID 106 CAPTION _HMG_aABMLangButton[13] WIDTH 70 HEIGHT 24
         @ 230, 170 BUTTON Btn3 ID 107 CAPTION _HMG_aABMLangButton[1]   WIDTH 70 HEIGHT 24

      END DIALOG

      RELEASE FONT Font_7
      IF lOk
         cNewArr := aVal2Str(aItem, ",")
      ENDIF
   ENDIF

RETURN cNewArr

//----------------------------------------------------------------------------//
STATIC FUNCTION DialogFun(lOk, aItem, aItemOld)
//----------------------------------------------------------------------------//

   LOCAL ret := 0
   LOCAL cValue
   LOCAL pos
   LOCAL hListBox

   IF DLG_ID != NIL
      hListBox := hmg_Getdialogitemhandle(DLG_HWND, 102)
      SWITCH DLG_ID
      CASE 101
         IF DLG_NOT == 1024
            cValue := hmg_GetEditText(DLG_HWND, 101)
            IF !Empty(cValue)
               EnableDialogItem(DLG_HWND, 110)
            ELSE
               DisableDialogItem(DLG_HWND, 110)
            ENDIF
         ENDIF
         EXIT
      CASE 110
         IF DLG_NOT == 0
            cValue := hmg_GetEditText(DLG_HWND, 101)
            EnableDialogItem(DLG_HWND, 105)
            hmg_SetDialogItemText(DLG_HWND, 101, "")
            AAdd(aItem, cValue)
            hmg_ListboxAddString(hListBox, cValue)
            DisableDialogItem(DLG_HWND, 110)
         ENDIF
         EXIT
      CASE 111
         IF DLG_NOT == 0
            pos := ListBoxGetCurSel(hListBox)
            IF Pos > 0 .AND. pos <= Len(aItem)
               ADel(aItem, pos)
               ASize(aItem, Len(aItem) - 1)
               ListBoxReset(hListBox)
               SetInitItem(aItem, 1)
               EnableDialogItem(DLG_HWND, 105)
            ENDIF
         ENDIF
         EXIT
      CASE 112
         IF DLG_NOT == 0
            pos := ListBoxGetCurSel(hListBox)
            IF Pos > 1 .AND. pos <= Len(aItem)
               cValue := aItem[pos]
               ADel(aItem, pos)
               AIns(aItem, pos - 1)
               aItem[pos-1] := cValue
               ListBoxReset(hListBox)
               SetInitItem(aItem, 1)
               ListBoxSetCurSel(hListBox, pos - 1)
               EnableDialogItem(DLG_HWND, 105)
            ENDIF
         ENDIF
         EXIT
      CASE 113
         IF DLG_NOT == 0
            pos := ListBoxGetCurSel(hListBox)
            IF Pos > 0 .AND. pos <= Len(aItem) - 1
               cValue := aItem[pos]
               ADel(aItem, pos)
               AIns(aItem, pos + 1)
               aItem[pos+1] := cValue
               ListBoxReset(hListBox)
               SetInitItem(aItem, 1)
               ListBoxSetCurSel(hListBox, pos + 1)
               EnableDialogItem(DLG_HWND, 105)
            ENDIF
         ENDIF
         EXIT
      CASE 105
         IF DLG_NOT == 0
            ret := hmg_GetEditText(DLG_HWND, 101)
            lOk := .T.
            _ReleaseDialog()
         ENDIF
         EXIT
      CASE 106
         IF DLG_NOT == 0
            ListBoxReset(hListBox)
            aItem := AClone(aItemOld)
            SetInitItem(aItem, 1)
            hmg_SetDialogItemText(DLG_HWND, 101, "")
         ENDIF
         EXIT
      CASE 107
         IF DLG_NOT == 0
            _ReleaseDialog()
         ENDIF
      ENDSWITCH
   ENDIF

RETURN ret

//----------------------------------------------------------------------------//
STATIC FUNCTION SetInitItem(aItem, met)
//----------------------------------------------------------------------------//

   LOCAL hListBox
   LOCAL i

   IF met == 0
      DisableDialogItem(DLG_HWND, 110)
   ENDIF
   hListBox := hmg_Getdialogitemhandle(DLG_HWND, 102)
   FOR i = 1 TO Len(aItem)
      hmg_ListboxAddString(hListBox, aItem[i])
   NEXT i

RETURN NIL

//----------------------------------------------------------------------------//
STATIC FUNCTION FormatDouble(Text, InputMask)
//----------------------------------------------------------------------------//
   
   LOCAL s AS STRING
   LOCAL x
   LOCAL c

   DEFAULT InputMask := ""
   
   FOR x := 1 TO Len(Text)
      c := SubStr(Text, x, 1)
      IF c $ "0123456789,-. "
         IF c == ","
            c := "."
         ENDIF
         s += c
      ENDIF
   NEXT x
   IF !Empty(InputMask)
      s := Transform(Val(CharRem(" ", s)), InputMask)
   ENDIF

RETURN s

//----------------------------------------------------------------------------//
STATIC PROCEDURE CharMaskEdit(hWnd, cValue, Mask)
//----------------------------------------------------------------------------//

   LOCAL icp
   LOCAL x
   LOCAL CB
   LOCAL CM
   LOCAL cValueLeft
   LOCAL cValueRight
   LOCAL OldChar
   LOCAL BackcValue
   LOCAL BadEntry AS LOGICAL
   LOCAL pFlag AS LOGICAL
   LOCAL NegativeZero AS LOGICAL
   LOCAL OutBuffer AS STRING
   LOCAL pc AS NUMERIC
   LOCAL fnb AS NUMERIC
   LOCAL dc AS NUMERIC
   LOCAL ol AS NUMERIC
   LOCAL ncp
   LOCAL Output

   DEFAULT Mask := ""

   icp := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
   IF Empty(mask)
      hmg_SetWindowText(hWnd, FormatDouble(cValue))
      hmg_SendMessage(hWnd, EM_SETSEL, icp, icp)
      RETURN
   ENDIF
   IF Left(AllTrim(cValue), 1) == "-" .AND. Val(cValue) == 0
      NegativeZero := .T.
   ENDIF

   FOR x := 1 TO Len(cValue)
      CB := SubStr(cValue, x, 1)
      IF CB == "." .OR. CB == ","
         pc++
      ENDIF
   NEXT x

   IF Left(cValue, 1) == "." .OR. Left(cValue, 1) == ","
      pFlag := .T.
   ENDIF

   FOR x := 1 TO Len(cValue)
      CB := SubStr(cValue, x, 1)
      IF CB != " "
         fnb := x
         EXIT
      ENDIF
   NEXT x

   BackcValue := cValue

   OldChar := SubStr(cValue, icp + 1, 1)
   IF Len(cValue) < Len(Mask)

      cValueLeft := Left(cValue, icp)
      cValueRight := Right(cValue, Len(cValue) - icp)
      IF CharMaskTekstOK(cValueLeft + " " + cValueRight, Mask) .AND. !CharMaskTekstOK(cValueLeft + cValueRight, Mask)
         cValue := cValueLeft + " " + cValueRight
      ELSE
         cValue := cValueLeft + cValueRight
      ENDIF
   ENDIF

   IF Len(cValue) > Len(Mask)

      cValueLeft := Left(cValue, icp)
      cValueRight := Right(cValue, Len(cValue) - icp - 1)
      cValue := cValueLeft + cValueRight

   ENDIF
   FOR x := 1 TO Len(Mask)

      CB := SubStr(cValue, x, 1)
      CM := SubStr(Mask, x, 1)

      SWITCH CM
      CASE "A"
      CASE "N"
      CASE "!"
         IF IsAlpha(CB) .OR. CB == " " .OR. ( ( CM == "N" .OR. CM == "!"  ) .AND. IsDigit(CB) )
            IF CM == "!" .AND. !IsDigit(CB)
               OutBuffer += Upper(CB)
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
         IF IsDigit(CB) .OR. CB == " " .OR. ( CB == "-" .AND. x == fnb .AND. PCount() > 1 )
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
         OutBuffer += CM
      ENDSWITCH
   NEXT x
   IF !( BackcValue == OutBuffer )
      hmg_SetWindowText(hWnd, OutBuffer)
   ENDIF
   IF pc > 1
      pc := At(".", OutBuffer)
      IF NegativeZero
         Output := FormatDouble(hmg_GetWindowText(hWnd), Mask)
         Output := Right(Output, ol - 1)
         Output := "-" + Output
         hmg_SetWindowText(hWnd, Output)
         hmg_SendMessage(hWnd, EM_SETSEL, pc + dc, pc + dc)
      ELSE
         hmg_SetWindowText(hWnd, FormatDouble(hmg_GetWindowText(hWnd), Mask))
         hmg_SendMessage(hWnd, EM_SETSEL, pc + dc, pc + dc)
      ENDIF
   ELSE
      IF pFlag
         ncp := At(".", hmg_GetWindowText(hWnd))
         hmg_SendMessage(hWnd, EM_SETSEL, ncp, ncp)
      ELSE
         IF BadEntry
            icp--
         ENDIF
         hmg_SendMessage(hWnd, EM_SETSEL, icp, icp)
         FOR x := 1 TO Len(OutBuffer)
            CB := SubStr(OutBuffer, icp + x, 1)
            CM := SubStr(Mask, icp + x, 1)
            IF !IsDigit(CB) .AND. !IsAlpha(CB) .AND. ( !( CB == " " ) .OR. ( CB == " " .AND. CM == " " ) )
               hmg_SendMessage(hWnd, EM_SETSEL, icp + x, icp + x)
            ELSE
               EXIT
            ENDIF
         NEXT x
      ENDIF
   ENDIF

RETURN

//----------------------------------------------------------------------------//
STATIC FUNCTION CharMaskTekstOK(cString, cMask)
//----------------------------------------------------------------------------//
   
   LOCAL lPassed := .F.
   LOCAL CB
   LOCAL CM
   LOCAL x
   LOCAL nCount := Min(Len(cString), Len(cMask))

   IF Len(cString) == Len(cMask)
      FOR x := 1 TO nCount
         CB := SubStr(cString, x, 1)
         CM := SubStr(cMask, x, 1)
         SWITCH CM
         CASE "9"
            lPassed := ( IsDigit(CB) .OR. CB == " " )
            EXIT
         CASE " "
            lPassed := ( CB == " " )
            EXIT
         OTHERWISE
            lPassed := .T.
         ENDSWITCH
         IF !lPassed
            EXIT
         ENDIF
      NEXT
   ENDIF

RETURN lPassed
