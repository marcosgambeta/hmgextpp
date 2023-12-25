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

#define CBS_UPPERCASE         0x2000
#define CBS_LOWERCASE         0x4000

#define EM_SETCUEBANNER       0x1501
#define CB_SETCUEBANNER       0x1703

FUNCTION _DefineCombo(ControlName, ParentFormName, x, y, w, rows, value, ;
   fontname, fontsize, tooltip, changeprocedure, h, gotfocus, lostfocus, uEnter, ;
   HelpId, invisible, notabstop, sort, bold, italic, underline, strikeout, ;
   itemsource, valuesource, displaychange, ondisplaychangeprocedure, break, ;
   GripperText, ListWidth, nId, OnListDisplayProcedure, OnListCloseProcedure, ;
   backcolor, fontcolor, lUpper, lLower, cuetext, OnCancel, AutoComplete, lShowDropDown, nItemHeight, bInit)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL WorkArea
   LOCAL cField
   LOCAL ContainerHandle := 0
   LOCAL i
   LOCAL k
   LOCAL blInit
   LOCAL Style
   LOCAL lDialogInMemory
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 120)
   hb_default(@h, 150)
   __defaultNIL(@changeprocedure, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@rows, {})
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@sort, .F.)
   hb_default(@GripperText, "")
   hb_default(@ListWidth, w)
   hb_default(@AutoComplete, .F.)
   hb_default(@lShowDropDown, .F.)

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

   IF ItemSource != NIL .AND. Sort
      MsgMiniGuiError("Sort and ItemSource clauses can't be used simultaneously.")
   ENDIF

   IF ValueSource != NIL .AND. Sort
      MsgMiniGuiError("Sort and ValueSource clauses can't be used simultaneously.")
   ENDIF

   IF itemsource != NIL
      IF hb_UAt(">", ItemSource) == 0
         MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " (ItemSource): You must specify a fully qualified field name.")
      ELSE
         WorkArea := hb_ULeft(ItemSource, hb_UAt(">", ItemSource) - 2)
         cField := hb_URight(ItemSource, hb_ULen(ItemSource) - hb_UAt(">", ItemSource))
      ENDIF
   ENDIF

   hb_default(@value, 0)
   __defaultNIL(@uEnter, "")

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_CHILD + WS_VSCROLL

      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF SORT
         Style += CBS_SORT
      ENDIF

      IF !displaychange
         Style += CBS_DROPDOWNLIST
      ELSE
         Style += CBS_DROPDOWN
      ENDIF

      IF hb_IsLogical(_HMG_IsXP)
         IF _HMG_IsXP
            Style += CBS_NOINTEGRALHEIGHT
         ENDIF
      ENDIF

      IF lUpper
         Style += CBS_UPPERCASE
      ENDIF

      IF lLower
         Style += CBS_LOWERCASE
      ENDIF

      IF lDialogInMemory         //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogComboBox(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "COMBOBOX", style, 0, x, y, w, h, "", HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         IF !empty(FontHandle)
            hmg__SetFontHandle(ControlHandle, FontHandle)
         ELSE
            __defaultNIL(@FontName, _HMG_DefaultFontName)
            __defaultNIL(@FontSize, _HMG_DefaultFontSize)
            IF hmg_IsWindowHandle(ControlHandle)
               FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
            ENDIF
         ENDIF

         hmg_SetWindowStyle(ControlHandle, Style, .T.)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      IF x == NIL .OR. y == NIL

         _HMG_SplitLastControl := "COMBOBOX"

         i := GetFormIndex(ParentFormName)

         IF i > 0

            ControlHandle := InitComboBox(_HMG_aFormReBarHandle[i], 0, x, y, w, lUpper, lLower, h, invisible, notabstop, sort, displaychange, _HMG_IsXPorLater)

            IF !empty(FontHandle)
               hmg__SetFontHandle(ControlHandle, FontHandle)
            ELSE
               __defaultNIL(@FontName, _HMG_DefaultFontName)
               __defaultNIL(@FontSize, _HMG_DefaultFontSize)
               IF hmg_IsWindowHandle(ControlHandle)
                  FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
               ENDIF
            ENDIF

            hmg_AddSplitBoxItem(Controlhandle, _HMG_aFormReBarHandle[i], w, break, GripperText, w, , _HMG_ActiveSplitBoxInverted)

            Containerhandle := _HMG_aFormReBarHandle[i]

         ENDIF

      ELSE

         ControlHandle := InitComboBox(ParentFormHandle, 0, x, y, w, lUpper, lLower, h, invisible, notabstop, sort, displaychange, _HMG_IsXPorLater)

      ENDIF

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

   _HMG_aControlType               [k] := CONTROL_TYPE_COMBO
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ondisplaychangeprocedure
   _HMG_aControlPageMap            [k] := cField
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := OnListDisplayProcedure
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := changeprocedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := uEnter
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := WorkArea
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := OnListCloseProcedure
   _HMG_aControlContainerHandle    [k] := ContainerHandle
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := valuesource
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := {0, DisplayChange, ItemSource, rows, ListWidth, cuetext, AutoComplete, lShowDropDown, 0, OnCancel, nItemHeight}
   _HMG_aControlMiscData2          [k] := ""

   IF Len(_HMG_aDialogTemplate) == 0
      InitDialogComboBox(ParentFormName, ControlHandle, k)
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

FUNCTION InitDialogComboBox(ParentName, ControlHandle, k)

   LOCAL WorkArea
   LOCAL BackRec
   LOCAL rcount := 0
   LOCAL cset := 0
   LOCAL ItemHeight
   LOCAL Value
   LOCAL rows
   LOCAL DisplayChange
   LOCAL ItemSource
   LOCAL cField
   LOCAL ListWidth
   LOCAL cuetext

   WorkArea      := _HMG_aControlSpacing[k]
   cField        := _HMG_aControlPageMap[k]
   Value         := _HMG_aControlValue[k]
   rows          := _HMG_aControlMiscData1[k, 4]
   DisplayChange := _HMG_aControlMiscData1[k, 2]
   ItemSource    := _HMG_aControlMiscData1[k, 3]
   ListWidth     := _HMG_aControlMiscData1[k, 5]
   cuetext       := _HMG_aControlMiscData1[k, 6]
   ItemHeight    := _HMG_aControlMiscData1[k, 11]

   IF DisplayChange

      _HMG_aControlRangeMin[k] := hmg_FindWindowEx(ControlHandle, 0, "Edit", NIL)
      // add tooltip for editable combo window if defined //(JK) HMG Exp. Build 8
      IF _HMG_aControlToolTip[k] != NIL
         hmg_SetToolTip(_HMG_aControlRangeMin[k], _HMG_aControlToolTip[k], GetFormToolTipHandle(ParentName))
      ENDIF

      IF !Empty(cuetext) .AND. IsVistaOrLater()
         value := 0
         SendMessageWideString(_HMG_aControlRangeMin[k], EM_SETCUEBANNER, .T., cuetext)
      ENDIF

   ELSEIF !Empty(cuetext) .AND. IsVistaOrLater()

      value := 0
      SendMessageWideString(ControlHandle, CB_SETCUEBANNER, .T., cuetext)

   ENDIF

   SetDropDownWidth(ControlHandle, ListWidth)

   IF hb_IsChar(WorkArea)

      IF Select(WorkArea) != 0

         BackRec := (WorkArea)->(RecNo())

         (WorkArea)->(dbGoTop())

         DO WHILE !(WorkArea)->(EOF())
            rcount++
            IF value == (WorkArea)->(RecNo())
               cset := rcount
            ENDIF
            hmg_ComboAddString(ControlHandle, cValToChar((WorkArea)->&(cField)))
            (WorkArea)->(dbSkip())
         ENDDO

         (WorkArea)->(dbGoto(BackRec))

         ComboSetCurSel(ControlHandle, cset)

      ENDIF

   ELSE

      IF Len(rows) > 0
         AEval(rows, {|v|hmg_ComboAddString(ControlHandle, v)})
      ENDIF

      IF hb_IsNumeric(value) .AND. value != 0
         ComboSetCurSel(ControlHandle, Value)
      ENDIF

   ENDIF

   IF ItemHeight != NIL
      hmg_ComboSetItemHeight(ControlHandle, ItemHeight)
   ENDIF

   IF ItemSource != NIL
      AAdd(_HMG_aFormBrowseList[GetFormIndex(ParentName)], k)
   ENDIF
   // JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

PROCEDURE _DataComboRefresh(i)  // (JK) Modified for extend COMBO HMG 1.0 Build 8

   LOCAL BackValue
   LOCAL BackRec
   LOCAL WorkArea
   LOCAL cField
   LOCAL ControlHandle

   IF Empty(_HMG_aControlCaption[i])
      BackValue := _GetValue(, , i)
   ELSE
      cField := _HMG_aControlCaption[i]
      _HMG_aControlCaption[i] := ""
      BackValue := _GetValue(, , i)
      _HMG_aControlCaption[i] := cField
   ENDIF

   cField := _HMG_aControlPageMap[i]

   ControlHandle := _HMG_aControlHandles[i]

   WorkArea := _HMG_aControlSpacing[i]

   BackRec := (WorkArea)->(RecNo())

   (WorkArea)->(dbGoTop())

   ComboboxReset(ControlHandle)

   DO WHILE !(WorkArea)->(EOF())  // (JK) HMG 1.0 Experimental Build 8
      IF _HMG_aControlMiscData1[i][1] != 1  // standard Combo
         hmg_ComboAddString(ControlHandle, cValToChar((WorkArea)->&(cField)))
      ELSE  // extend Combo
         hmg_ComboAddDataStringEx(ControlHandle, cValToChar((WorkArea)->&(cField)))
      ENDIF
      (WorkArea)->(dbSkip())
   ENDDO

   (WorkArea)->(dbGoto(BackRec))

   IF BackValue > 0 .AND. BackValue <= (WorkArea)->(LastRec())
      _SetValue(, , BackValue, i)
   ENDIF

RETURN

#pragma BEGINDUMP

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbwinuni.hpp>

#ifndef WC_COMBOBOX
#define WC_COMBOBOX TEXT("ComboBox")
#endif

HIMAGELIST HMG_ImageListLoadFirst(const char * FileName, int cGrow, int Transparent, int * nWidth, int * nHeight);
void HMG_ImageListAdd(HIMAGELIST himl, const char * FileName, int Transparent);

/*
INITCOMBOBOX(p1, p2, nX, nY, nWidth, p6, p7, nHeight, p9, p10, p11, p12, p13) --> HWND
*/
HB_FUNC_STATIC( INITCOMBOBOX )
{
   DWORD style = WS_CHILD | WS_VSCROLL;

   if( !hb_parl(9) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(10) ) {
      style |= WS_TABSTOP;
   }

   if( hb_parl(11) ) {
      style |= CBS_SORT;
   }

   style |= hb_parl(12) ? CBS_DROPDOWN : CBS_DROPDOWNLIST;

   if( hb_parl(13) ) {
      style |= CBS_NOINTEGRALHEIGHT;
   }

   if( hb_parl(6) ) {
      style |= CBS_UPPERCASE;
   }

   if( hb_parl(7) ) {
      style |= CBS_LOWERCASE;
   }

   auto hbutton = CreateWindowEx(0,
                                 WC_COMBOBOX,
                                 TEXT(""),
                                 style,
                                 hmg_par_int(3),
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(8),
                                 hmg_par_HWND(1),
                                 hmg_par_HMENU(2),
                                 GetInstance(),
                                 nullptr);

   hmg_ret_HWND(hbutton);
}

#pragma ENDDUMP
