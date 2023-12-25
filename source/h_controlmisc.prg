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

#include "hmg.ch"

#ifndef HMG_LEGACY_OFF
#undef _BT_
#endif

#include "i_winuser.ch"

#define ESB_ENABLE_BOTH    0x0000
#define ESB_DISABLE_BOTH   0x0003

#define EM_SETCUEBANNER    0x1501
#define CB_SETCUEBANNER    0x1703

MEMVAR aResult

FUNCTION _GetValue(ControlName, ParentForm, Index)

   LOCAL retval AS NUMERIC
   LOCAL rcount AS NUMERIC
   LOCAL oGet
   LOCAL auxval
   LOCAL WorkArea
   LOCAL BackRec
   LOCAL Tmp
   LOCAL Ts
   LOCAL t
   LOCAL x
   LOCAL c
   LOCAL ix

   IF PCount() == 2

      IF Upper(ControlName) == "VSCROLLBAR"
         RETURN hmg_GetScrollPos(GetFormHandle(ParentForm), SB_VERT)
      ELSEIF Upper(ControlName) == "HSCROLLBAR"
         RETURN hmg_GetScrollPos(GetFormHandle(ParentForm), SB_HORZ)
      ENDIF
      T := GetControlType(ControlName, ParentForm)
      c := GetControlHandle(ControlName, ParentForm)
      ix := GetControlIndex(ControlName, ParentForm)

   ELSE

      T := _HMG_aControlType[Index]
      c := _HMG_aControlHandles[Index]
      ix := Index

   ENDIF

   SWITCH T

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      retval := _BrowseGetValue("", "", ix)
      EXIT
#endif

   CASE CONTROL_TYPE_PROGRESSBAR
      retval := hmg_SendMessage(c, PBM_GETPOS, 0, 0)
      EXIT

   CASE CONTROL_TYPE_IPADDRESS
      retval := hmg_GetIPAddress(c)
      EXIT

   CASE CONTROL_TYPE_MONTHCAL
      retval := hmg_GetMonthCalDate(c)
      EXIT

   CASE CONTROL_TYPE_TREE
      retval := iif(!_HMG_aControlInputMask[ix], AScan(_HMG_aControlPageMap[ix], hmg_TreeView_GetSelection(c)), hmg_TreeView_GetSelectionId(c))
      EXIT

   CASE CONTROL_TYPE_MASKEDTEXT
      IF "E" $ _HMG_aControlPageMap[ix]
         Ts := hmg_GetWindowText(c)
         IF "." $ _HMG_aControlPageMap[ix]
            DO CASE
            CASE hb_UAt(".", Ts) >  hb_UAt(",", Ts)
               retval := GetNumFromText(hmg_GetWindowText(c), ix)
            CASE hb_UAt(",", Ts) > hb_UAt(".", Ts)
               retval := GetNumFromTextSp(hmg_GetWindowText(c), ix)
            ENDCASE
         ELSE
            DO CASE
            CASE hb_UAt(".", Ts) != 0
               retval := GetNumFromTextSp(hmg_GetWindowText(c), ix)
            CASE hb_UAt(",", Ts) != 0
               retval := GetNumFromText(hmg_GetWindowText(c), ix)
            OTHERWISE
               retval := GetNumFromText(hmg_GetWindowText(c), ix)
            ENDCASE
         ENDIF
      ELSE
         retval := GetNumFromText(hmg_GetWindowText(c), ix)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_EDIT
   CASE CONTROL_TYPE_LABEL
   CASE CONTROL_TYPE_CHECKLABEL
   CASE CONTROL_TYPE_HYPERLINK
   CASE CONTROL_TYPE_RICHEDIT
      retval := hmg_GetWindowText(c)
      EXIT

   CASE CONTROL_TYPE_CHARMASKTEXT
      IF hb_IsLogical(_HMG_aControlHeadCLick[ix])
         IF _HMG_aControlHeadCLick[ix]
            retval := CToD(AllTrim(hmg_GetWindowText(c)))
         ELSE
            retval := hmg_GetWindowText(c)
         ENDIF
      ELSE
         retval := hmg_GetWindowText(c)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_NUMTEXT
      retval := Int(Val(hmg_GetWindowText(c)))
      EXIT

   CASE CONTROL_TYPE_SPINNER
      retval := GetSpinnerValue(c[2])
      EXIT

   CASE CONTROL_TYPE_CHECKBOX
      auxval := hmg_SendMessage(c, BM_GETCHECK, 0, 0)
      SWITCH auxval
      CASE BST_CHECKED       ; retval := .T. ; EXIT
      CASE BST_UNCHECKED     ; retval := .F. ; EXIT
      CASE BST_INDETERMINATE ; retval := NIL
      ENDSWITCH
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      FOR EACH x IN c
         IF !empty(x) // x > 0
            auxval := hmg_SendMessage(x, BM_GETCHECK, 0, 0)
            IF auxval == BST_CHECKED
               retval := hb_enumindex(x)
               EXIT
            ENDIF
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_COMBO
      IF hb_IsString(_HMG_aControlSpacing[ix])
         auxval := ComboGetCursel(c)
         WorkArea := _HMG_aControlSpacing[ix]
         BackRec := (WorkArea)->(RecNo())
         (WorkArea)->(dbGoTop())
         DO WHILE !(WorkArea)->(EOF())
            IF ++rcount == auxval
               IF Empty(_HMG_aControlCaption[ix])
                  RetVal := (WorkArea)->(RecNo())
               ELSE
                  Tmp := _HMG_aControlCaption[ix]
                  RetVal := &Tmp
               ENDIF
            ENDIF
            (WorkArea)->(dbSkip())
         ENDDO
         (WorkArea)->(dbGoto(BackRec))
      ELSE
         retval := ComboGetCursel(c)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_CHKLIST
      retval := ListBoxGetCursel(c)
      EXIT

   CASE CONTROL_TYPE_GRID
      retval := iif(_HMG_aControlFontColor[ix], {_HMG_aControlMiscData1[ix][1], _HMG_aControlMiscData1[ix][17]}, hmg_LISTVIEW_GETFIRSTITEM(c))
      EXIT

   CASE CONTROL_TYPE_TAB
      retval := hmg_TABCTRL_GETCURSEL(c)
      EXIT

   CASE CONTROL_TYPE_DATEPICK
      IF Empty(Ts := _SetGetDatePickerDateFormat(_HMG_aControlNames[ix], GetParentFormName(ix))) .OR. !("HH:" $ Upper(Ts)) .OR. !("mm:" $ Lower(Ts))
         retval := hmg_GetDatePickDate(c)
      ELSE
         retval := hmg_dtp_GetDateTime(c)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TIMEPICK
      retval := iif(hmg_GetDatePickHour(c) >= 0, StrZero(hmg_GetDatePickHour(c), 2) + ":" + StrZero(hmg_GetDatePickMinute(c), 2) + ":" + StrZero(hmg_GetDatePickSecond(c), 2), "")
      EXIT

   CASE CONTROL_TYPE_SLIDER
      retval := hmg_SendMessage(c, TBM_GETPOS, 0, 0)
      EXIT

   CASE CONTROL_TYPE_MULTILIST
   CASE CONTROL_TYPE_MULTICHKLIST
      retval := hmg_ListBoxGetMultiSel(c)
      EXIT

   CASE CONTROL_TYPE_MULTIGRID
      retval := hmg_ListViewGetMultiSel(c)
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      retval := hmg_IsButtonBarChecked(_HMG_aControlContainerHandle[ix], _HMG_aControlValue[ix] - 1)
      EXIT

   CASE CONTROL_TYPE_GETBOX
      oGet := _HMG_aControlHeadClick[ix]
      retval := oGet:VarGet()
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := _HMG_aControlIds[ix]
      retval := oGet:GetValue(oGet:nCell)
      EXIT
#endif

   CASE CONTROL_TYPE_HOTKEYBOX
      retval := hmg_C_GetHotKey(c)
      EXIT

   OTHERWISE
      retval := _HMG_aControlValue[ix]

   ENDSWITCH

RETURN retval

FUNCTION _SetValue(ControlName, ParentForm, Value, index)

   LOCAL nValue AS NUMERIC
   LOCAL rcount AS NUMERIC
   LOCAL TreeItemHandle
   LOCAL oGet
   LOCAL aPos
   LOCAL aTemp
   LOCAL xPreviousValue
   LOCAL backrec
   LOCAL workarea
   LOCAL t
   LOCAL h
   LOCAL c
   LOCAL x
   LOCAL ix

   ix := iif(PCount() == 3, GetControlIndex(ControlName, ParentForm), index)

   t := _HMG_aControlType[ix]
   h := _HMG_aControlParentHandles[ix]
   c := _HMG_aControlHandles[ix]

   IF hb_IsArray(Value)

      aTemp := _GetValue(NIL, NIL, ix)

      IF hb_IsArray(aTemp) .AND. T != CONTROL_TYPE_OBUTTON
         IF HMG_IsEqualArr(aTemp, Value)
            RETURN NIL
         ENDIF
      ENDIF

   ELSEIF T != CONTROL_TYPE_LABEL .AND. T != CONTROL_TYPE_CHECKLABEL .AND. T != CONTROL_TYPE_RICHEDIT .AND. T != CONTROL_TYPE_TREE

      xPreviousValue := _GetValue(NIL, NIL, ix)

      IF T == CONTROL_TYPE_GRID .AND. hb_IsArray(xPreviousValue)
         xPreviousValue := xPreviousValue[1]
      ENDIF

      IF ValType(xPreviousValue) == ValType(Value)
         IF xPreviousValue == Value
            RETURN NIL
         ENDIF
      ENDIF

   ENDIF

   SWITCH T

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      _BrowseSetValue("", "", Value, ix)
      EXIT
#endif

   CASE CONTROL_TYPE_IPADDRESS
      Value := iif(empty(Value), {}, Value)
      IF Len(Value) == 0
         ClearIpAddress(c)
      ELSE
         hmg_SetIPAddress(c, Value[1], Value[2], Value[3], Value[4])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_MONTHCAL
      Value := iif(empty(Value), BLANK_DATE, Value)
      hmg_SetMonthCalValue(c, Year(value), Month(value), Day(value))
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      IF hb_Version(HB_VERSION_BITWIDTH) >= 64
         SetDayState(_HMG_aControlNames[ix], GetParentFormName(ix))
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TREE
      IF Empty(Value)
         RETURN NIL
      ENDIF
      IF !_HMG_aControlInputMask[ix]
         IF Value > hmg_TreeView_GetCount(c)
            RETURN NIL
         ENDIF
         TreeItemHandle := _HMG_aControlPageMap[ix][Value]
      ELSE
         aPos := AScan(_HMG_aControlPicture[ix], Value)
         IF aPos == 0
            MsgMiniGuiError("Value Property: Invalid TreeItem Reference.")
         ENDIF
         TreeItemHandle := _HMG_aControlPageMap[ix][aPos]
      ENDIF
      hmg_TreeView_SelectItem(c, TreeItemHandle)
      EXIT

   CASE CONTROL_TYPE_MASKEDTEXT
      Value := iif(empty(Value), 0, Value)
      IF hmg_GetFocus() == c
         hmg_SetWindowText(_HMG_aControlhandles[ix], Transform(Value, _HMG_aControlInputMask[ix]))
      ELSE
         hmg_SetWindowText(_HMG_aControlhandles[ix], Transform(value, _HMG_aControlPageMap[ix]))
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TIMER
      x := _HMG_aControlIds[ix]
      IF _HMG_aControlEnabled[ix]
         hmg_KillTimer(_HMG_aControlParentHandles[ix], x)
      ENDIF
      FOR EACH h IN _HMG_aControlIds
         IF hb_IsNumeric(h) .AND. h == x
            IF _HMG_aControlEnabled[ix]
               hmg_InitTimer(GetFormHandle(ParentForm), h, Value)
            ENDIF
            _HMG_aControlValue[ix] := value
            EXIT
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_LABEL
   CASE CONTROL_TYPE_CHECKLABEL
   CASE CONTROL_TYPE_HYPERLINK
      IF Empty(Value)
         value := iif(T == CONTROL_TYPE_LABEL .OR. T == CONTROL_TYPE_CHECKLABEL, iif(hb_IsString(Value), Value, ""), "@")
      ENDIF
      IF hb_IsArray(Value)
         x := ""
         AEval(Value, {|v|x += cValToChar(v)})
         Value := x
      ELSEIF hb_IsBlock(Value)
         Value := cValToChar(Eval(Value))
      ELSE
         Value := cValToChar(Value)
      ENDIF
      IF _HMG_aControlSpacing[ix] == 1
         _SetControlWidth(ControlName, ParentForm, hmg_GetTextWidth(NIL, Value, _HMG_aControlFontHandle[ix]) + ;
            iif(_HMG_aControlFontAttributes[ix][1] .OR. _HMG_aControlFontAttributes[ix][2], ;
            hmg_GetTextWidth(NIL, " ", _HMG_aControlFontHandle[ix]), 0) + iif(T == CONTROL_TYPE_CHECKLABEL, _HMG_aControlRangeMin[ix] + ;
            iif(Len(Value) > 0 .AND. !_HMG_aControlRangeMax[ix], GetBorderWidth(), iif(_HMG_aControlRangeMax[ix], GetBorderWidth() / 2, 0)), 0))
         _SetControlHeight(ControlName, ParentForm, iif(T == CONTROL_TYPE_CHECKLABEL .AND. _HMG_aControlFontSize[ix] < 13, 22, _HMG_aControlFontSize[ix] + ;
            iif(_HMG_aControlFontSize[ix] < 14, 12, 16)))
      ENDIF
      hmg_SetWindowText(c, value)
      IF hb_IsLogical(_HMG_aControlInputMask[ix])
         IF _HMG_aControlInputMask[ix]
            hmg_RedrawWindowControlRect(h, _HMG_aControlRow[ix], _HMG_aControlCol[ix], _HMG_aControlRow[ix] + _HMG_aControlHeight[ix], _HMG_aControlCol[ix] + _HMG_aControlWidth[ix])
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_BTNTEXT
      Value := iif(empty(Value), "", Value)
      hmg_SetWindowText(c, RTrim(value))
      EXIT

   CASE CONTROL_TYPE_EDIT
      Value := iif(empty(Value), "", Value)
      hmg_SetWindowText(c, value)
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      EXIT

   CASE CONTROL_TYPE_RICHEDIT
      Value := iif(empty(Value), "", Value)
      IF _HMG_IsXPorLater
         IF Empty(value) .OR. !(Left(value, 7) == "{\rtf1\")
            hmg_SetWindowText(c, value)
         ELSE
            hmg_SetWindowTextW(c, value)
         ENDIF
      ELSE
         hmg_SetWindowText(c, RTrim(value))
      ENDIF
      EXIT

   CASE CONTROL_TYPE_CHARMASKTEXT
      Value := iif(empty(Value), "", Value)
      IF hb_IsLogical(_HMG_aControlHeadCLick[ix])
         IF _HMG_aControlHeadCLick[ix]
            hmg_SetWindowText(c, RTrim(DToC(hb_defaultValue(value, BLANK_DATE))))
         ELSE
            hmg_SetWindowText(c, RTrim(value))
         ENDIF
      ELSE
         hmg_SetWindowText(c, RTrim(value))
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BTNNUMTEXT
      Value := iif(empty(Value), 0, Value)
      hmg_SetWindowText(c, hb_ntos(Int(value)))
      EXIT

   CASE CONTROL_TYPE_SPINNER
      Value := iif(empty(Value), 0, Value)
      SetSpinnerValue(c[2], Value)
      EXIT

   CASE CONTROL_TYPE_CHECKBOX
      Value := iif(hb_IsLogical(Value), Value, NIL)
      DO CASE
      CASE _HMG_aControlSpacing[ix] .AND. value == NIL
         hmg_SendMessage(c, BM_SETCHECK, BST_INDETERMINATE, 0)
      CASE value
         hmg_SendMessage(c, BM_SETCHECK, BST_CHECKED, 0)
      CASE !value
         hmg_SendMessage(c, BM_SETCHECK, BST_UNCHECKED, 0)
      ENDCASE
      IF Empty(_HMG_aControlMiscData1[ix])
         _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      ENDIF
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      IF hb_IsNumeric(Value) .AND. Value <= Len(c)  // EF 93
         AEval(c, {|x|iif(x > 0, hmg_SendMessage(x, BM_SETCHECK, BST_UNCHECKED, 0), NIL)})
         _HMG_aControlValue[ix] := value
         IF value > 0
            h := c[value]
            IF h > 0
               hmg_SendMessage(h, BM_SETCHECK, BST_CHECKED, 0)
               IF !_HMG_aControlPicture[ix] .AND. IsTabStop(h)
                  SetTabStop(h, .F.)
               ENDIF
            ENDIF
         ENDIF
         IF value > 0
            hmg_setfocus(h)
         ENDIF
         _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      ENDIF
      EXIT

   CASE CONTROL_TYPE_COMBO
      Value := iif(hb_IsNumeric(Value), Value, 0)
      IF hb_IsString(_HMG_aControlSpacing[ix])
         _HMG_aControlValue[ix] := value
         WorkArea := _HMG_aControlSpacing[ix]
         BackRec := (WorkArea)->(RecNo())
         (WorkArea)->(dbGoTop())
         DO WHILE !(WorkArea)->(EOF())
            rcount++
            IF value == (WorkArea)->(RecNo())
               value := rcount
               EXIT
            ENDIF
            (WorkArea)->(dbSkip())
         ENDDO
         (WorkArea)->(dbGoto(BackRec))
      ENDIF
      ComboSetCursel(c, value)
      IF _HMG_ProgrammaticChange
         _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      ENDIF
      EXIT

   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_CHKLIST
      Value := iif(hb_IsNumeric(Value), Value, 0)
      ListBoxSetCursel(c, value)
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      EXIT

   CASE CONTROL_TYPE_GRID
      IF !_HMG_aControlFontColor[ix]
         hmg_ListView_SetCursel(c, iif(hb_IsArray(Value), value[1], value))
         hmg_ListView_EnsureVisible(c, iif(hb_IsArray(Value), value[1], value))
      ELSE
         x := (hb_IsArray(Value) .AND. (_HMG_aControlMiscData1[ix][1] != value[1] .OR. _HMG_aControlMiscData1[ix][17] != value[2]))
         _HMG_aControlMiscData1[ix][1] := iif(hb_IsArray(Value), value[1], value)
         _HMG_aControlMiscData1[ix][17] := iif(hb_IsArray(Value), value[2], 1)
         IF hb_IsArray(Value) .AND. value[1] * value[2] == 0
            _HMG_aControlMiscData1[ix][1] := 0
            _HMG_aControlMiscData1[ix][17] := 0
            hmg_RedrawWindow(c)
            _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
         ELSEIF x .OR. !hb_IsArray(Value)
            hmg_ListView_SetCursel(c, iif(hb_IsArray(Value), value[1], value))
            hmg_ListView_EnsureVisible(c, iif(hb_IsArray(Value), value[1], value))
            hmg_RedrawWindow(c)
            _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TAB
      Assign nValue := value
      IF nValue < 1
         MsgMiniGuiError("TAB: Wrong Value (only value > 0 is allowed).")
      ENDIF
      hmg_TABCTRL_SETCURSEL(c, nValue)
      IF Len(_HMG_aControlPageMap[ix]) > 0
         UpdateTab(ix)
         _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      ENDIF
      EXIT

   CASE CONTROL_TYPE_DATEPICK
      IF Empty(Value)
         hmg_SetDatePickNull(c)
      ELSE
         IF HB_ISDATETIME(value) .OR. hb_IsArray(value)
            IF hb_IsArray(value)
               IF Len(value) >= 7
                  hmg_dtp_SetDateTime(c, value[1], value[2], value[3], value[4], value[5], value[6], value[7])
               ELSE
                  hmg_dtp_SetDateTime(c, value[1], value[2], value[3], value[4], value[5], value[6])
               ENDIF
            ELSE
               hmg_dtp_SetDateTime(c, value)
            ENDIF
         ELSE
            hmg_SetDatePick(c, Year(value), Month(value), Day(value))
         ENDIF
      ENDIF
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      EXIT

   CASE CONTROL_TYPE_TIMEPICK
      IF Empty(Value)
         hmg_SetDatePickNull(c)
      ELSEIF hb_IsString(Value)
         hmg_SetTimePick(c, Val(Left(value, 2)), Val(SubStr(value, 4, 2)), Val(SubStr(value, 7, 2)))
      ENDIF
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      EXIT

   CASE CONTROL_TYPE_PROGRESSBAR
      Value := iif(empty(Value), 0, Value)
      hmg_SendMessage(c, PBM_SETPOS, value, 0)
      EXIT

   CASE CONTROL_TYPE_SLIDER
      Value := iif(empty(Value), 0, Value)
      hmg_SendMessage(c, TBM_SETPOS, 1, value)
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      EXIT

   CASE CONTROL_TYPE_MULTILIST
   CASE CONTROL_TYPE_MULTICHKLIST
      hmg_LISTBOXSETMULTISEL(c, value)
      EXIT

   CASE CONTROL_TYPE_MULTIGRID
      IF hb_IsNumeric(value)  // GF 09/02/2013
         Value := {Value}
      ENDIF
      hmg_LISTVIEWSETMULTISEL(c, value)
      IF Len(value) > 0
         hmg_ListView_EnsureVisible(c, value[1])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      hmg_CheckButtonBar(_HMG_aControlContainerHandle[ix], _HMG_aControlValue[ix] - 1, value)
      EXIT

   CASE CONTROL_TYPE_GETBOX
      _SetGetBoxValue(ix, c, Value)
      oGet := _HMG_aControlHeadClick[ix]
      IF oGet:Changed
         _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
      ENDIF
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := _HMG_aControlIds[ix]
      IF oGet:lInitGoTop
         IF hb_IsNumeric(Value) .AND. Value > 0
            oGet:GoPos(Value)
            Eval(oGet:bGoToPos, Value)
            oGet:Refresh(.T.)
         ENDIF
      ELSE
         oGet:SetValue(oGet:nCell, Value)
      ENDIF
#endif

   CASE CONTROL_TYPE_HOTKEYBOX
      hmg_SetHotKeyValue(c, value)
      EXIT

   OTHERWISE
      _HMG_aControlValue[ix] := value

   ENDSWITCH

RETURN NIL

FUNCTION _AddItem(ControlName, ParentForm, Value, Parent, aImage, Id)

   LOCAL NewHandle
   LOCAL TempHandle
   LOCAL ChildHandle
   LOCAL BackHandle
   LOCAL ParentHandle
   LOCAL TreeItemHandle
   LOCAL aPos
   LOCAL ImgDef
   LOCAL iUnSel
   LOCAL iSel
   LOCAL t
   LOCAL c
   LOCAL i
   LOCAL ix
#ifdef _TSBROWSE_
   LOCAL oGet
#endif

   ix := GetControlIndex(ControlName, ParentForm)
   T := _HMG_aControltype[ix]
   c := _HMG_aControlhandles[ix]

   __defaultNIL(@Id, 0)

   IF hb_IsNumeric(aImage)
      Id := aImage
   ENDIF

   SWITCH T

   CASE CONTROL_TYPE_TREE
      IF !_HMG_aControlInputmask[ix]
         IF Parent > hmg_TreeView_GetCount(c) .OR. Parent < 0
            MsgMiniGuiError("AddItem Method: Invalid Parent Value.")
         ENDIF
      ENDIF

      ImgDef := iif(hb_IsArray(aImage), Len(aImage), 0) //Tree+

      IF Parent != 0

         IF !_HMG_aControlInputmask[ix]
            TreeItemHandle := _HMG_aControlPageMap[ix][Parent]
         ELSE
            aPos := AScan(_HMG_aControlPicture[ix], Parent)
            IF aPos == 0
               MsgMiniGuiError("AddItem Method: Invalid Parent Value.")
            ENDIF
            TreeItemHandle := _HMG_aControlPageMap[ix][aPos]
         ENDIF

         IF ImgDef == 0
            iUnsel := 2  // Pointer to defalut Node Bitmaps, no Bitmap loaded
            iSel   := 3
         ELSE
            iUnSel := hmg_AddTreeViewBitmap(c, aImage[1], _HMG_aControlMiscData1[ix, 4]) - 1
            iSel   := iif(ImgDef == 1, iUnSel, hmg_AddTreeViewBitmap(c, aImage[2], _HMG_aControlMiscData1[ix, 4]) - 1)
            // If only one bitmap in array iSel = iUnsel, only one Bitmap loaded
         ENDIF

         NewHandle := hmg_AddTreeItem(c, TreeItemHandle, Value, iUnsel, iSel, Id)

         // Determine Position of New Item
         TempHandle := hmg_TreeView_GetChild(c, TreeItemHandle)

         i := 0

         DO WHILE .T.

            i++

            IF TempHandle == NewHandle
               EXIT
            ENDIF

            ChildHandle := hmg_TreeView_GetChild(c, TempHandle)

            IF ChildHandle == 0
               BackHandle := TempHandle
               TempHandle := hmg_TreeView_GetNextSibling(c, TempHandle)
            ELSE
               i++
               BackHandle := Childhandle
               TempHandle := hmg_TreeView_GetNextSibling(c, ChildHandle)
            ENDIF

            DO WHILE TempHandle == 0

               ParentHandle := hmg_TreeView_GetParent(c, BackHandle)

               TempHandle := hmg_TreeView_GetNextSibling(c, ParentHandle)

               IF TempHandle == 0
                  BackHandle := ParentHandle
               ENDIF

            ENDDO

         ENDDO

         // Resize Array
         ASize(_HMG_aControlPageMap[ix], hmg_TreeView_GetCount(c))
         ASize(_HMG_aControlPicture[ix], hmg_TreeView_GetCount(c))
         ASize(_HMG_aControlHeadClick[ix], hmg_TreeView_GetCount(c))

         // Insert New Element
         IF !_HMG_aControlInputmask[ix]
            AIns(_HMG_aControlPageMap[ix], Parent + i)
            AIns(_HMG_aControlPicture[ix], Parent + i)
            AIns(_HMG_aControlHeadClick[ix], Parent + i)
         ELSE
            AIns(_HMG_aControlPageMap[ix], aPos + i)
            AIns(_HMG_aControlPicture[ix], aPos + i)
            AIns(_HMG_aControlHeadClick[ix], aPos + i)
         ENDIF

         // Assign Handle
         IF !_HMG_aControlInputmask[ix]

            _HMG_aControlPageMap[ix][Parent + i] := NewHandle
            _HMG_aControlPicture[ix][Parent + i] := Id

         ELSE

            IF AScan(_HMG_aControlPicture[ix], Id) != 0
               MsgMiniGuiError("AddItem Method: Item Id " + hb_ntos(Id) + " Already In Use.")
            ENDIF

            _HMG_aControlPageMap[ix][aPos + i] := NewHandle
            _HMG_aControlPicture[ix][aPos + i] := Id

         ENDIF

      ELSE

         IF ImgDef == 0
            iUnsel := 0  // Pointer to defalut Node Bitmaps, no Bitmap loaded
            iSel   := 1
         ELSE
            iUnSel := hmg_AddTreeViewBitmap(c, aImage[1], _HMG_aControlMiscData1[ix, 4]) - 1
            iSel   := iif(ImgDef == 1, iUnSel, hmg_AddTreeViewBitmap(c, aImage[2], _HMG_aControlMiscData1[ix, 4]) - 1)
            // If only one bitmap in array iSel = iUnsel, only one Bitmap loaded
         ENDIF

         NewHandle := hmg_AddTreeItem(c, 0, Value, iUnsel, iSel, Id)

         AAdd(_HMG_aControlPageMap[ix], NewHandle)

         IF _HMG_aControlInputmask[ix]
            IF AScan(_HMG_aControlPicture[ix], Id) != 0
               MsgMiniGuiError("AddItem Method: Item Id Already In Use.")
            ENDIF
         ENDIF

         AAdd(_HMG_aControlPicture[ix], Id)
         AAdd(_HMG_aControlHeadClick[ix], NIL)

      ENDIF

      EXIT

   CASE CONTROL_TYPE_COMBO
      // (JK) HMG 1.0 Experimental Build 8
      IF _HMG_aControlMiscData1[ix][1] == 0      // standard combo
         hmg_ComboAddString(c, value)
      ELSEIF _HMG_aControlMiscData1[ix][1] == 1  // extend combo - "parent" is a picture Id. ;-)
         hmg_ComboAddStringEx(c, value, Parent)
      ENDIF
      EXIT

   // CASE "LIST" $ T
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTILIST
      IF _HMG_aControlMiscData1[ix][2] .AND. hb_IsArray(value)
         value := LB_Array2String(value)
      ENDIF
      hmg_ListBoxAddstring(c, value)
      EXIT

   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_MULTICHKLIST
      ChkListboxAddItem(c, value, 1)
      EXIT

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF !_HMG_aControlMiscData1[ix][5]
         _AddGridRow(ControlName, ParentForm, value)
         IF _HMG_aControlEnabled[ix]
            _UpdateGridColors(ix)
         ENDIF
      ENDIF
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := GetObjectByHandle(c)
      IF hb_IsObject(oGet)
         oGet:AddItem(value)
      ENDIF
#endif

   ENDSWITCH

RETURN NIL

FUNCTION _DeleteItem(ControlName, ParentForm, Value)

   LOCAL TreeItemHandle
   LOCAL BeforeCount
   LOCAL AfterCount
   LOCAL DeletedCount
   LOCAL aPos
   LOCAL t
   LOCAL c
   LOCAL i
   LOCAL ix
#ifdef _TSBROWSE_
   LOCAL oGet
#endif

   ix := GetControlIndex(ControlName, ParentForm)

   T := _HMG_aControlType[ix]
   c := _HMG_aControlHandles[ix]

   SWITCH T

   CASE CONTROL_TYPE_TREE
      BeforeCount := hmg_TreeView_GetCount(c)

      IF !_HMG_aControlInputmask[ix]

         IF Value > BeforeCount .OR. Value < 1
            MsgMiniGuiError("DeleteItem Method: Invalid Item Specified.")
         ENDIF

         TreeItemHandle := _HMG_aControlPageMap[ix][Value]
         hmg_TreeView_DeleteItem(c, TreeItemHandle)

      ELSE

         aPos := AScan(_HMG_aControlPicture[ix], Value)

         IF aPos == 0
            MsgMiniGuiError("DeleteItem Method: Invalid Item Id.")
         ENDIF

         TreeItemHandle := _HMG_aControlPageMap[ix][aPos]
         hmg_TreeView_DeleteItem(c, TreeItemHandle)

      ENDIF

      AfterCount := hmg_TreeView_GetCount(c)
      DeletedCount := BeforeCount - AfterCount

      IF !_HMG_aControlInputmask[ix]

         IF DeletedCount == 1
            ADel(_HMG_aControlPageMap[ix], Value)
            ADel(_HMG_aControlHeadClick[ix], Value)
         ELSE
            FOR i := 1 TO DeletedCount
               ADel(_HMG_aControlPageMap[ix], Value)
               ADel(_HMG_aControlHeadClick[ix], Value)
            NEXT i
         ENDIF

      ELSE

         IF DeletedCount == 1
            ADel(_HMG_aControlPageMap[ix], aPos)
            ADel(_HMG_aControlPicture[ix], aPos)
            ADel(_HMG_aControlHeadClick[ix], aPos)
         ELSE
            FOR i := 1 TO DeletedCount
               ADel(_HMG_aControlPageMap[ix], aPos)
               ADel(_HMG_aControlPicture[ix], aPos)
               ADel(_HMG_aControlHeadClick[ix], aPos)
            NEXT i
         ENDIF

      ENDIF

      ASize(_HMG_aControlPageMap[ix], AfterCount)
      ASize(_HMG_aControlPicture[ix], AfterCount)
      ASize(_HMG_aControlHeadClick[ix], AfterCount)

   // CASE "LIST" $ T
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTILIST
   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_MULTICHKLIST
      ListBoxDeleteString(c, value)
      EXIT

   CASE CONTROL_TYPE_COMBO
      ComboBoxDeleteString(c, value)
      ComboSetCursel(c, value)
      EXIT

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF !_HMG_aControlMiscData1[ix][5]
         ListViewDeleteString(c, value)
         IF _HMG_aControlFontColor[ix] .AND. T == CONTROL_TYPE_GRID
            IF _HMG_aControlMiscData1[ix][1] == value
               _HMG_aControlMiscData1[ix][1] := 0
               _HMG_aControlMiscData1[ix][17] := 0
            ELSEIF _HMG_aControlMiscData1[ix][1] > value
               _HMG_aControlMiscData1[ix][1]--
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[ix], ix, "CONTROL_ONCHANGE")
            ENDIF
            AfterCount := hmg_ListViewGetItemCount(c)
            IF value > AfterCount .AND. AfterCount > 0
               _HMG_aControlMiscData1[ix][1] := AfterCount
            ENDIF
         ENDIF
         _UpdateGridColors(ix)
      ENDIF
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := GetObjectByHandle(c)
      IF hb_IsObject(oGet)
         oGet:DeleteRow()
      ENDIF
      EXIT
#endif

   ENDSWITCH

RETURN NIL

FUNCTION _DeleteAllItems(ControlName, ParentForm)

   LOCAL t
   LOCAL c
   LOCAL i
#ifdef _TSBROWSE_
   LOCAL oGet
#endif

   i := GetControlIndex(ControlName, ParentForm)

   t := _HMG_aControlType[i]
   c := _HMG_aControlhandles[i]

   SWITCH t

   CASE CONTROL_TYPE_TREE
      hmg_TreeView_DeleteAllItems(c, _HMG_aControlPageMap[i]) // Tree+
      ASize(_HMG_aControlPageMap[i], 0)
      ASize(_HMG_aControlPicture[i], 0)
      ASize(_HMG_aControlHeadClick[i], 0)

   // CASE "LIST" $ t
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTILIST
   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_MULTICHKLIST
      ListBoxReset(c)
      EXIT

   CASE CONTROL_TYPE_COMBO
      ComboBoxReset(c)
      EXIT

   // CASE "GRID" $ t
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF !_HMG_aControlMiscData1[i][5]
         ListViewReset(c)
         IF _HMG_aControlFontColor[i] .AND. T == CONTROL_TYPE_GRID
            _HMG_aControlMiscData1[i][1] := 0
            _HMG_aControlMiscData1[i][17] := 0
         ENDIF
      ENDIF
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := GetObjectByHandle(c)
      IF hb_IsObject(oGet) .AND. oGet:lIsArr
         oGet:DeleteRow(.T.)
      ENDIF
      EXIT
#endif

   ENDSWITCH

RETURN NIL

FUNCTION GetControlIndex(ControlName, ParentForm)

   LOCAL mVar := "_" + ParentForm + "_" + ControlName

#ifdef _NAMES_LIST_
RETURN _GetNameList(mVar)
#else
RETURN __mvGetDef(mVar, 0)
#endif

FUNCTION GetControlName(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN ""
   ENDIF

RETURN _HMG_aControlNames[i]

FUNCTION GetControlHandle(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      MsgMiniGuiError("Control " + ControlName + " Of " + ParentForm + " Not defined.")
   ENDIF

RETURN _HMG_aControlHandles[i]

FUNCTION GetControlContainerHandle(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlContainerHandle[i]

FUNCTION GetControlParentHandle(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlParentHandles[i]

FUNCTION GetControlId(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlIds[i]

FUNCTION GetControlType(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN ""
   ENDIF

RETURN _HMG_aControlType[i]

// added by MAG (2023/05/18)
FUNCTION GetControlTypeAsString(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN ""
   ENDIF

RETURN controlTypeToString(_HMG_aControlType[i])

FUNCTION GetControlValue(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN NIL
   ENDIF

RETURN _HMG_aControlValue[i]

FUNCTION GetControlPageMap(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN {}
   ENDIF

RETURN _HMG_aControlPageMap[i]

FUNCTION _SetFocus(ControlName, ParentForm, Index)

   LOCAL MaskStart AS NUMERIC
   LOCAL ParentFormHandle
   LOCAL hControl
   LOCAL H
   LOCAL T
   LOCAL x
   LOCAL i

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), Index)

   H := _HMG_aControlHandles[i]
   T := _HMG_aControlType[i]

   SWITCH T

   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      hmg_setfocus(H)
      hmg_SendMessage(H, EM_SETSEL, 0, -1)
      EXIT

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      hmg_setfocus(H)
      _UpdateGridColors(i)
      EXIT

   CASE CONTROL_TYPE_CHARMASKTEXT
      hmg_setfocus(H)
      FOR x := 1 TO hb_ULen(_HMG_aControlInputMask[i])
         t := hb_USubStr(_HMG_aControlInputMask[i], x, 1) // TODO: usar outro nome de variavel
         IF hmg_IsDigit(t) .OR. hmg_IsAlpha(t) .OR. t == "!"
            MaskStart := x
            EXIT
         ENDIF
      NEXT x
      IF MaskStart > 0
         hmg_SendMessage(H, EM_SETSEL, MaskStart - 1, -1)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BUTTON
      IF _HMG_aControlEnabled[i]
         ParentFormHandle := _HMG_aControlParentHandles[i]
         FOR EACH hControl IN _HMG_aControlHandles
            x := hb_enumindex(hControl)
            IF _HMG_aControlType[x] == CONTROL_TYPE_BUTTON
               IF _HMG_aControlParentHandles[x] == ParentFormHandle
                  hmg_SendMessage(hControl, BM_SETSTYLE, hmg_LOWORD(BS_PUSHBUTTON), 1)
                  IF Empty(_HMG_aControlBrushHandle[x])
                     LOOP
                  ENDIF
                  hmg_RedrawWindow(hControl)
               ENDIF
            ENDIF
         NEXT
         hmg_setfocus(H)
         hmg_SendMessage(H, BM_SETSTYLE, hmg_LOWORD(BS_DEFPUSHBUTTON), 1)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_SPINNER
      hmg_setfocus(H[1])
      hmg_SendMessage(H[1], EM_SETSEL, 0, -1)
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      x := _GetValue(NIL, NIL, i)
      _HMG_aControlValue[i] := x
      hmg_setfocus(H[iif(x > 0, x, 1)])
      EXIT

   OTHERWISE
      hmg_setfocus(H)

   ENDSWITCH

   _HMG_SetFocusExecuted := .T.

RETURN NIL

FUNCTION _DisableControl(ControlName, ParentForm, nPosition)

   LOCAL T
   LOCAL c
   LOCAL y
   LOCAL s
   LOCAL z
   LOCAL w

   T := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)
   y := GetControlIndex(ControlName, ParentForm)

   IF T == CONTROL_TYPE_BUTTON .AND. _HMG_aControlEnabled[y]
      hmg_SendMessage(c, BM_SETSTYLE, hmg_LOWORD(BS_PUSHBUTTON), 1)
      hmg_RedrawWindow(c)
      IF !Empty(_HMG_aControlInputMask[y])
         z := _DetermineKey(_HMG_aControlInputMask[y])
         _ReleaseHotKey(ParentForm, z[2], z[1])
      ENDIF
   ENDIF

   SWITCH T

      // HMG 1.0 Experimental build 9 (JK)
   CASE CONTROL_TYPE_BUTTON
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsArray(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 0
         IF _HMG_aControlEnabled[y]
            IF !_HMG_aControlDblClick[y] .AND. _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y][2])
               hmg_ReDrawWindow(c)
            ELSE
               _DestroyBtnPicture(c, y)
               hmg__SetBtnPicture(c, _HMG_aControlPicture[y][2])
            ENDIF
            hmg_DisableWindow(c)
         ENDIF
      ENDIF
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsChar(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 0
         IF _HMG_aControlEnabled[y]
            IF !_HMG_aControlDblClick[y] .AND. _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y])
               hmg_ReDrawWindow(c)
            ELSE
               _SetBtnPictureMask(c, y)
               _DestroyBtnPicture(c, y)
            ENDIF
            hmg_DisableWindow(c)
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_CHECKBOX
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsChar(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 1
         IF _HMG_aControlEnabled[y]
            IF _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y], _HMG_aControlSpacing[y])
               hmg_ReDrawWindow(c)
            ELSE
               _SetBtnPictureMask(c, y)
               _DestroyBtnPicture(c, y)
            ENDIF
            hmg_DisableWindow(c)
         ENDIF
      ENDIF
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      hmg_DisableWindow(c)
      IF _HMG_aControlIds[y] != 0
         hmg_DisableWindow(_HMG_aControlIds[y])
      ENDIF
      hmg__EnableScrollBars(c, SB_HORZ, ESB_DISABLE_BOTH)
      EXIT
#endif

   CASE CONTROL_TYPE_GRID
      hmg_DisableWindow(c)
      hmg__EnableScrollBars(c, SB_BOTH, ESB_DISABLE_BOTH)
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      _DisableToolBarButton(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_MENU
   CASE CONTROL_TYPE_POPUP
      _DisableMenuItem(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_TIMER
      IF _HMG_aControlEnabled[y]
         w := GetControlParentHandle(ControlName, ParentForm)
         s := GetControlId(ControlName, ParentForm)
         hmg_KillTimer(w, s)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_SPINNER
      AEval(c, {|y|hmg_DisableWindow(y)})
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      IF PCount() == 3  // Position is defined, which Radiobutton to disable
         hmg_DisableWindow(c[nPosition])
         _HMG_aControlPageMap[y][nPosition] := .T.
      ELSE
         AEval(c, {|y|hmg_DisableWindow(y)})
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TAB
      IF PCount() == 3  // Position is defined, which Page to disable
         s := iif(nPosition > _GetItemCount(ControlName, ParentForm), 1, nPosition)
      ELSE
         hmg_DisableWindow(c)
         s := hmg_TabCtrl_GetCurSel(_HMG_aControlHandles[y])
      ENDIF
      FOR EACH w IN _HMG_aControlPageMap[y][s]
         IF !hb_isArray(w)
            hmg_DisableWindow(w)
         ELSE
            FOR EACH z IN w
               hmg_DisableWindow(z)
            NEXT
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_GETBOX
      FOR z := 1 TO 3
         hmg_DisableWindow(_HMG_aControlRangeMin[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      FOR z := 1 TO 3
         hmg_DisableWindow(_HMG_aControlSpacing[y][z])
      NEXT z
      EXIT

   OTHERWISE
      hmg_DisableWindow(c)

   ENDSWITCH

   _HMG_aControlEnabled[y] := .F.

RETURN NIL

FUNCTION _EnableControl(ControlName, ParentForm, nPosition)

   LOCAL t
   LOCAL c
   LOCAL y
   LOCAL s
   LOCAL z
   LOCAL w

   T := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)
   y := GetControlIndex(ControlName, ParentForm)

   IF T == CONTROL_TYPE_BUTTON .AND. !_HMG_aControlEnabled[y]
      IF !Empty(_HMG_aControlInputMask[y])
         z := _DetermineKey(_HMG_aControlInputMask[y])
         _DefineHotKey(ParentForm, z[2], z[1], _HMG_aControlProcedures[y])
      ENDIF
   ENDIF

   SWITCH T

   // HMG 1.0 Experimental build 9 (JK)
   CASE CONTROL_TYPE_BUTTON
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsArray(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 0
         IF !_HMG_aControlEnabled[y]
            IF !_HMG_aControlDblClick[y] .AND. _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y][1])
               hmg_ReDrawWindow(c)
            ELSE
               _DestroyBtnPicture(c, y)
               _HMG_aControlBrushHandle[y] := hmg__SetBtnPicture(c, _HMG_aControlPicture[y][1])
            ENDIF
            hmg_EnableWindow(c)
         ENDIF
      ENDIF
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsChar(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 0
         IF !_HMG_aControlEnabled[y]
            IF !_HMG_aControlDblClick[y] .AND. _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y])
               hmg_ReDrawWindow(c)
            ELSE
               _DestroyBtnPictureMask(c, y)
               _HMG_aControlBrushHandle[y] := hmg__SetBtnPicture(c, _HMG_aControlPicture[y])
            ENDIF
            hmg_EnableWindow(c)
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_CHECKBOX
      IF !Empty(_HMG_aControlBrushHandle[y]) .AND. hb_IsChar(_HMG_aControlPicture[y]) .AND. _HMG_aControlMiscData1[y] == 1
         IF !_HMG_aControlEnabled[y]
            IF _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[y])
               _HMG_aControlBrushHandle[y] := hmg__SetMixedBtnPicture(c, _HMG_aControlPicture[y], _HMG_aControlSpacing[y])
               hmg_ReDrawWindow(c)
            ELSE
               _DestroyBtnPictureMask(c, y)
               _HMG_aControlBrushHandle[y] := hmg__SetBtnPicture(c, _HMG_aControlPicture[y])
            ENDIF
            hmg_EnableWindow(c)
         ENDIF
      ENDIF
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      hmg_EnableWindow(c)
      IF _HMG_aControlIds[y] != 0
         hmg_EnableWindow(_HMG_aControlIds[y])
      ENDIF
      hmg__EnableScrollBars(c, SB_BOTH, ESB_ENABLE_BOTH)
      EXIT
#endif

   CASE CONTROL_TYPE_GRID
      hmg_EnableWindow(c)
      hmg__EnableScrollBars(c, SB_BOTH, ESB_ENABLE_BOTH)
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      _EnableToolBarButton(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_MENU
   CASE CONTROL_TYPE_POPUP
      _EnableMenuItem(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_TIMER
      s := GetControlId(ControlName, ParentForm)
      FOR EACH z IN _HMG_aControlIds
         IF hb_IsNumeric(z) .AND. z == s
            hmg_InitTimer(GetFormHandle(ParentForm), z, _HMG_aControlValue[hb_enumindex(z)])
            EXIT
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_SPINNER
      AEval(c, {|y|hmg_EnableWindow(y)})
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      IF PCount() == 3  // Position is defined, which Radiobutton to enable
         hmg_EnableWindow(c[nPosition])
         _HMG_aControlPageMap[y][nPosition] := .F.
      ELSE
         AEval(c, {|y|hmg_EnableWindow(y)})
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TAB
      IF PCount() == 3  // Position is defined, which Page to enable
         s := iif(nPosition > _GetItemCount(ControlName, ParentForm), 1, nPosition)
      ELSE
         hmg_EnableWindow(c)
         s := hmg_TabCtrl_GetCurSel(_HMG_aControlHandles[y])
      ENDIF
      FOR EACH w IN _HMG_aControlPageMap[y][s]
         IF !hb_isArray(w)
            hmg_EnableWindow(w)
         ELSE
            FOR EACH z IN w
               hmg_EnableWindow(z)
            NEXT
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_GETBOX
      FOR z := 1 TO 3
         hmg_EnableWindow(_HMG_aControlRangeMin[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      FOR z := 1 TO 3
         hmg_EnableWindow(_HMG_aControlSpacing[y][z])
      NEXT z
      EXIT

   OTHERWISE
      hmg_EnableWindow(c)

   ENDSWITCH

   _HMG_aControlEnabled[y] := .T.

RETURN NIL

FUNCTION _ShowControl(ControlName, ParentForm)

   LOCAL t
   LOCAL i
   LOCAL c
   LOCAL w
   LOCAL s
   LOCAL y
   LOCAL z
   LOCAL r
   LOCAL TabHide := .F.

   T := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)
   y := GetControlIndex(ControlName, ParentForm)

   IF _HMG_aControlVisible[y]
      RETURN NIL
   ENDIF

   // If the control is inside a TAB and the page is not visible,
   // the control must not be showed

   FOR i := 1 TO Len(_HMG_aControlPageMap)
      IF _HMG_aControlType[i] == CONTROL_TYPE_TAB
         s := hmg_TabCtrl_GetCurSel(_HMG_aControlHandles[i])
         FOR r := 1 TO Len(_HMG_aControlPageMap[i])
            FOR w := 1 TO Len(_HMG_aControlPageMap[i][r])
               IF t == CONTROL_TYPE_RADIOGROUP
                  IF hb_IsArray(_HMG_aControlPageMap[i][r][w])
                     IF _HMG_aControlPageMap[i][r][w][1] == _HMG_aControlHandles[y][1]
                        IF r != s
                           TabHide := .T.
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF
               ELSEIF t == CONTROL_TYPE_SPINNER
                  IF hb_IsArray(_HMG_aControlPageMap[i][r][w])
                     IF _HMG_aControlPageMap[i][r][w][1] == _HMG_aControlHandles[y][1]
                        IF r != s
                           TabHide := .T.
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF
#ifdef _DBFBROWSE_
               ELSEIF t == CONTROL_TYPE_BROWSE
                  IF hb_IsArray(_HMG_aControlPageMap[i][r][w])
                     IF _HMG_aControlPageMap[i][r][w][1] == _HMG_aControlHandles[y]
                        IF r != s
                           TabHide := .T.
                        ENDIF
                        EXIT
                     ENDIF
                  ELSEIF hb_IsNumeric(_HMG_aControlPageMap[i][r][w])
                     IF _HMG_aControlPageMap[i][r][w] == _HMG_aControlHandles[y]
                        IF r != s
                           TabHide := .T.
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF
#endif
               ELSE
                  IF hb_IsNumeric(_HMG_aControlPageMap[i][r][w])
                     IF _HMG_aControlPageMap[i][r][w] == _HMG_aControlHandles[y]
                        IF r != s
                           TabHide := .T.
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF
               ENDIF
            NEXT w
         NEXT r
      ENDIF
   NEXT i

   IF TabHide
      _HMG_aControlVisible[y] := .T.
      RETURN NIL
   ENDIF

   SWITCH T

   CASE CONTROL_TYPE_SPINNER
      AEval(c, {|y|CShowControl(y)})
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      CShowControl(c)
      IF _HMG_aControlIds[y] != 0
         CShowControl(_HMG_aControlIds[y])
      ENDIF
      IF _HMG_aControlMiscData1[y][1] != 0
         CShowControl(_HMG_aControlMiscData1[y][1])
      ENDIF
      EXIT
#endif

   CASE CONTROL_TYPE_TAB
      CShowControl(c)
      s := hmg_TabCtrl_GetCurSel(_HMG_aControlHandles[y])
      FOR EACH w IN _HMG_aControlPageMap[y][s]
         IF !hb_isArray(w)
            CShowControl(w)
         ELSE
            FOR EACH z IN w
               CShowControl(z)
            NEXT
         ENDIF
      NEXT
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      AEval(c, {|y|hmg_ShowWindow(y)})
      EXIT

#ifdef _PROPGRID_
   CASE CONTROL_TYPE_PROPGRID
      AEval(c, {|y|hmg_ShowWindow(y)})
      EXIT
#endif

   CASE CONTROL_TYPE_GETBOX
      FOR z := 1 TO 3
         CShowControl(_HMG_aControlRangeMin[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      FOR z := 1 TO 3
         CShowControl(_HMG_aControlSpacing[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_TIMER
      // Do nothing
      EXIT

   OTHERWISE
      CShowControl(c)

   ENDSWITCH

   _HMG_aControlVisible[y] := .T.

RETURN NIL

FUNCTION _HideControl(ControlName, ParentForm)

   LOCAL t
   LOCAL c
   LOCAL y
   LOCAL r
   LOCAL w
   LOCAL z

   T := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)
   y := GetControlIndex(ControlName, ParentForm)

   SWITCH T

   CASE CONTROL_TYPE_SPINNER
      AEval(c, {|y|HideWindow(y)})
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      HideWindow(c)
      IF _HMG_aControlIds[y] != 0
         HideWindow(_HMG_aControlIds[y])
      ENDIF
      IF _HMG_aControlMiscData1[y][1] != 0
         HideWindow(_HMG_aControlMiscData1[y][1])
      ENDIF
      EXIT
#endif

   CASE CONTROL_TYPE_TAB
      HideWindow(c)
      FOR EACH r IN _HMG_aControlPageMap[y]
         FOR EACH w IN r
            IF !hb_isArray(w)
               HideWindow(w)
            ELSE
               FOR EACH z IN w
                  HideWindow(z)
               NEXT
            ENDIF
         NEXT
      NEXT
      EXIT

   CASE CONTROL_TYPE_RADIOGROUP
      AEval(c, {|y|HideWindow(y)})
      EXIT

#ifdef _PROPGRID_
   CASE CONTROL_TYPE_PROPGRID
      AEval(c, {|y|HideWindow(y)})
      EXIT
#endif

   CASE CONTROL_TYPE_COMBO
      hmg_SendMessage(c, 335, 0, 0)  // close DropDown list
      HideWindow(c)
      EXIT

   CASE CONTROL_TYPE_GETBOX
      FOR z := 1 TO 3
         HideWindow(_HMG_aControlRangeMin[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      FOR z := 1 TO 3
         HideWindow(_HMG_aControlSpacing[y][z])
      NEXT z
      EXIT

   CASE CONTROL_TYPE_TIMER
      // Do nothing
      EXIT

   OTHERWISE
      HideWindow(c)

   ENDSWITCH

   _HMG_aControlVisible[y] := .F.

RETURN NIL

FUNCTION _SetItem(ControlName, ParentForm, Item, Value, index)

   LOCAL TreeHandle
   LOCAL ItemHandle
   LOCAL AEDITCONTROLS
   LOCAL AITEMS
   LOCAL ALABELS
   LOCAL ATEMP
   LOCAL CTYPE
   LOCAL CINPUTMASK
   LOCAL CFORMAT
   LOCAL AEC
   LOCAL Pos
   LOCAL ci
   LOCAL bd
   LOCAL XRES
   LOCAL t
   LOCAL c
   LOCAL i

   i := iif(PCount() == 5, index, GetControlIndex(ControlName, ParentForm))

   t := _HMG_aControlType[i]
   c := _HMG_aControlHandles[i]

   SWITCH t

   CASE CONTROL_TYPE_TREE
      IF !_HMG_aControlInputmask[i]
         IF Item > hmg_TreeView_GetCount(c) .OR. Item < 1
            MsgMiniGuiError("Item Property: Invalid Item Reference.")
         ENDIF
      ENDIF
      TreeHandle := c
      IF !_HMG_aControlInputmask[i]
         ItemHandle := _HMG_aControlPageMap[i][Item]
      ELSE
         Pos := AScan(_HMG_aControlPicture[i], Item)
         IF Pos == 0
            MsgMiniGuiError("Item Property: Invalid Item Id.")
         ENDIF
         ItemHandle := _HMG_aControlPageMap[i][Pos]
      ENDIF
      hmg_TreeView_SetItem(TreeHandle, ItemHandle, Value)
      EXIT

   CASE CONTROL_TYPE_LIST
      IF _HMG_aControlMiscData1[i][2] .AND. hb_IsArray(value)
         value := LB_Array2String(value)
      ENDIF
      ListBoxDeleteString(c, Item)
      hmg_ListBoxInsertString(c, value, Item)
      ListBoxSetCurSel(c, Item)
      EXIT

   CASE CONTROL_TYPE_CHKLIST
      Pos := iif(ChkList_GetCheckBox(c, Item), 2, 1)
      ListBoxDeleteString(c, Item)
      ChkListBoxInsertItem(c, value, Item, Pos)
      ListBoxSetCurSel(c, Item)
      EXIT

   CASE CONTROL_TYPE_MULTILIST
      aTemp := hmg_ListBoxGetMultiSel(c)
      ListBoxDeleteString(c, Item)
      hmg_ListBoxInsertString(c, value, Item)
      hmg_ListBoxSetMultiSel(c, aTemp)
      EXIT

   CASE CONTROL_TYPE_MULTICHKLIST
      Pos := iif(ChkList_GetCheckBox(c, Item), 2, 1)
      aTemp := hmg_ListBoxGetMultiSel(c)
      ListBoxDeleteString(c, Item)
      ChkListBoxInsertItem(c, value, Item, Pos)
      hmg_ListBoxSetMultiSel(c, aTemp)
      EXIT

   CASE CONTROL_TYPE_COMBO
      IF _HMG_aControlMiscData1[i][1] == 0       // standard combo
         ComboBoxDeleteString(c, Item)
         hmg_ComboInsertString(c, value, Item)
      ELSEIF _HMG_aControlMiscData1[i][1] == 1   // extend combo - value is array (1-image index, 2-string)
         ComboBoxDeleteItemEx(c, Item)
         hmg_ComboInsertStringEx(c, value[2], value[1], Item)
      ENDIF
      EXIT

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF !_HMG_aControlMiscData1[i][5]
         AEDITCONTROLS := _HMG_aControlMiscData1[i][13]
         IF !hb_isArray(AEDITCONTROLS)
#ifdef _HMG_COMPAT_
            aTemp := AClone(Value)
            AEval(aTemp, {|x, i|iif(hb_IsString(x) .OR. HB_ISNIL(x), NIL, aTemp[i] := hb_ValToStr(x))})
            hmg_ListViewSetItem(c, aTemp, Item)
#endif
         ELSE
            aTemp := Array(Len(Value))
            FOR ci := 1 TO Len(Value)
               XRES := _ParseGridControls(AEDITCONTROLS, CI, Item)
               AEC        := XRES[1]
               CTYPE      := XRES[2]
               CINPUTMASK := XRES[3]
               CFORMAT    := XRES[4]
               AITEMS     := XRES[5]
               ALABELS    := XRES[8]
               SWITCH AEC
               CASE "TEXTBOX"
                  SWITCH CTYPE
                  CASE "CHARACTER"
                     IF Empty(CINPUTMASK)
                        aTemp[ci] := VALUE[CI]
                     ELSE
                        aTemp[ci] := Transform(VALUE[CI], CINPUTMASK)
                     ENDIF
                     EXIT
                  CASE "NUMERIC"
                     IF Empty(CINPUTMASK)
                        aTemp[ci] := Str(VALUE[CI])
                     ELSE
                        IF Empty(CFORMAT)
                           aTemp[ci] := Transform(VALUE[CI], CINPUTMASK)
                        ELSE
                           aTemp[ci] := Transform(VALUE[CI], "@" + CFORMAT + " " + CINPUTMASK)
                        ENDIF
                     ENDIF
                     EXIT
                  CASE "DATE"
                     aTemp[ci] := DToC(VALUE[CI])
                  ENDSWITCH
                  EXIT
               CASE "CODEBLOCK"
                  aTemp[ci] := hb_ValToStr(VALUE[CI])
                  EXIT
               CASE "DATEPICKER"
                  bd := Set(_SET_DATEFORMAT)
                  SET CENTURY ON
                  aTemp[ci] := DToC(VALUE[CI])
                  SET(_SET_DATEFORMAT, bd)
                  EXIT
               CASE "COMBOBOX"
                  IF VALUE[CI] == 0
                     aTemp[ci] := ""
                  ELSE
                     aTemp[ci] := AITEMS[VALUE[CI]]
                  ENDIF
                  EXIT
               CASE "SPINNER"
                  aTemp[ci] := Str(VALUE[CI])
                  EXIT
               CASE "CHECKBOX"
                  IF VALUE[CI]
                     aTemp[ci] := ALABELS[1]
                  ELSE
                     aTemp[ci] := ALABELS[2]
                  ENDIF
               ENDSWITCH
            NEXT ci
            hmg_ListViewSetItem(c, aTemp, Item)
         ENDIF
         IF Len(_HMG_aControlBkColor[i]) > 0
            hmg_SetImageListViewItems(c, Item, Value[1])
         ENDIF
         _UpdateGridColors(i)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_MESSAGEBAR  // JP
      IF _IsOwnerDrawStatusBarItem(c, Item, Value, .T.)
         hmg_MoveWindow(c, 0, 0, 0, 0, .T.)
      ELSE
         hmg_SetItemBar(c, value, Item - 1)
      ENDIF
      EXIT

   ENDSWITCH

RETURN NIL

FUNCTION _GetItem(ControlName, ParentForm, Item, index)

   LOCAL TreeHandle
   LOCAL ItemHandle
   LOCAL RetVal
   LOCAL AEDITCONTROLS
   LOCAL AITEMS
   LOCAL ALABELS
   LOCAL ATEMP
   LOCAL CTYPE
   LOCAL CFORMAT
   LOCAL Pos
   LOCAL CI
   LOCAL XRES
   LOCAL AEC
   LOCAL ColumnCount
   LOCAL t
   LOCAL c
   LOCAL i
   LOCAL V
   LOCAL Z
   LOCAL X

   i := iif(PCount() == 4, index, GetControlIndex(ControlName, ParentForm))

   t := _HMG_aControlType[i]
   c := _HMG_aControlHandles[i]

   SWITCH t

   CASE CONTROL_TYPE_TREE
      IF !_HMG_aControlInputmask[i]
         IF Item > hmg_TreeView_GetCount(c) .OR. Item < 1
            MsgMiniGuiError("Item Property: Invalid Item Reference.")
         ENDIF
      ENDIF
      TreeHandle := c
      IF !_HMG_aControlInputmask[i]
         ItemHandle := _HMG_aControlPageMap[i][Item]
      ELSE
         Pos := AScan(_HMG_aControlPicture[i], Item)
         IF Pos == 0
            MsgMiniGuiError("Item Property: Invalid Item Id.")
         ENDIF
         ItemHandle := _HMG_aControlPageMap[i][Pos]
      ENDIF
      RetVal := hmg_TreeView_GetItem(TreeHandle, ItemHandle)
      EXIT

   // CASE "LIST" $ T
   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTICHKLIST
   CASE CONTROL_TYPE_MULTILIST
      RetVal := hmg_ListBoxGetString(c, Item)
      IF hb_IsArray(_HMG_aControlMiscData1[i]) .AND. _HMG_aControlMiscData1[i][2]
         RetVal := LB_String2Array(RetVal)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_COMBO
      RetVal := hmg_ComboGetString(c, Item)
      EXIT

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      ColumnCount := hmg_ListView_GetColumnCount(c)
      IF _HMG_aControlMiscData1[i][5]
         RetVal := _GetIVirtualItem(Item, i, ColumnCount)
      ELSE
         AEDITCONTROLS := _HMG_aControlMiscData1[i][13]
         IF !hb_isArray(AEDITCONTROLS)
            RetVal := hmg_ListViewGetItem(c, Item, ColumnCount)
         ELSE
            V := hmg_ListViewGetItem(c, Item, ColumnCount)
            ATEMP := Array(ColumnCount)
            FOR CI := 1 TO ColumnCount
               XRES := _ParseGridControls(AEDITCONTROLS, CI, Item)
               AEC      := XRES[1]
               CTYPE    := XRES[2]
               CFORMAT  := XRES[4]
               AITEMS   := XRES[5]
               ALABELS  := XRES[8]
               SWITCH AEC
               CASE "TEXTBOX"
                  SWITCH CTYPE
                  CASE "NUMERIC"
                     IF CFORMAT == "E"
                        ATEMP[CI] := GetNumFromCellTextSp(V[CI])
                     ELSE
                        ATEMP[CI] := GetNumFromCellText(V[CI])
                     ENDIF
                     EXIT
                  CASE "DATE"
                     ATEMP[CI] := CToD(V[CI])
                     EXIT
                  CASE "CHARACTER"
                     ATEMP[CI] := V[CI]
                  ENDSWITCH
                  EXIT
               CASE "CODEBLOCK"
                  ATEMP[CI] := V[CI]
                  EXIT
               CASE "DATEPICKER"
                  ATEMP[CI] := CToD(V[CI])
                  EXIT
               CASE "COMBOBOX"
                  Z := 0
                  FOR EACH X IN AITEMS
                     IF Upper(AllTrim(V[CI])) == Upper(AllTrim(X))
                        Z := hb_enumindex(X)
                        EXIT
                     ENDIF
                  NEXT
                  ATEMP[CI] := Z
                  EXIT
               CASE "SPINNER"
                  ATEMP[CI] := Val(V[CI])
                  EXIT
               CASE "CHECKBOX"
                  ATEMP[CI] := (Upper(AllTrim(V[CI])) == Upper(AllTrim(ALABELS[1])))
               ENDSWITCH
            NEXT CI
            RetVal := ATEMP
         ENDIF
         IF Len(_HMG_aControlBkColor[i]) > 0
            IF Len(RetVal) >= 1
               RetVal[1] := hmg_GetImageListViewItems(c, Item)
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_MESSAGEBAR  //JK
      IF _IsOwnerDrawStatusBarItem(c, Item, @i)
         RetVal := _HMG_aControlCaption[i]
      ELSE
         RetVal := hmg_GetItemBar(c, Item)
      ENDIF

   ENDSWITCH

RETURN RetVal

FUNCTION _SetControlSizePos(ControlName, ParentForm, row, col, width, height)

   LOCAL t
   LOCAL i
   LOCAL c
   LOCAL x
   LOCAL NewCol
   LOCAL NewRow
   LOCAL r
   LOCAL w
   LOCAL z
   LOCAL p
   LOCAL xx
   LOCAL sx
   LOCAL sy
#ifdef _DBFBROWSE_
   LOCAL b
   LOCAL hws
#endif
   LOCAL DelTaRow
   LOCAL DelTaCol
   LOCAL DelTaWidth
   LOCAL SpinW
   LOCAL tCol
   LOCAL tRow
   LOCAL tWidth
   LOCAL tHeight

   T := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)
   x := GetControlIndex(ControlName, ParentForm)

   sx := hmg_GetScrollPos(GetFormHandle(ParentForm), 0)
   sy := hmg_GetScrollPos(GetFormHandle(ParentForm), 1)

   DO CASE

   CASE T == CONTROL_TYPE_TAB

      DelTaRow    := Row   - _HMG_aControlRow[x]
      DelTaCol    := Col   - _HMG_aControlCol[x]
      DelTaWidth  := Width - _HMG_aControlWidth[x]

      _HMG_aControlRow   [x] := Row
      _HMG_aControlCol   [x] := Col
      _HMG_aControlWidth [x] := Width
      _HMG_aControlHeight[x] := Height

      hmg_MoveWindow(c, col - sx, Row - sy, Width, Height, .T.)

      FOR r := 1 TO Len(_HMG_aControlPageMap[x])

         FOR w := 1 TO Len(_HMG_aControlPageMap[x][r])

            IF !hb_isArray(_HMG_aControlPageMap[x][r][w])

               p := AScan(_HMG_aControlhandles, _HMG_aControlPageMap[x][r][w]) // TODO:
               IF p > 0
                  tCol    := _HMG_aControlCol   [p]
                  tRow    := _HMG_aControlRow   [p]
                  tWidth  := _HMG_aControlWidth [p]
                  tHeight := _HMG_aControlHeight[p]

                  hmg_MoveWindow(_HMG_aControlPageMap[x][r][w], tCol + DeltaCol - sx, tRow + DeltaRow - sy, tWidth, tHeight, .T.)

                  _HMG_aControlRow[p] :=  tRow + DeltaRow
                  _HMG_aControlCol[p] :=  tCol + DeltaCol

                  _HMG_aControlContainerRow[p] :=  Row
                  _HMG_aControlContainerCol[p] :=  Col
               ENDIF

            ELSE

               p := AScan(_HMG_aControlhandles, _HMG_aControlPageMap[x][r][w][1]) // TODO:
               IF p > 0 .AND. _HMG_aControlType[p] == CONTROL_TYPE_BROWSE
#ifdef _DBFBROWSE_
                  tCol    := _HMG_aControlCol   [p]
                  tRow    := _HMG_aControlRow   [p]
                  tWidth  := _HMG_aControlWidth [p]
                  tHeight := _HMG_aControlHeight[p]

                  IF _HMG_aControlIds[p] != 0

                     hmg_MoveWindow(_HMG_aControlPageMap[x][r][w][1], tCol + DeltaCol - sx, tRow + DeltaRow - sy, tWidth - GETVSCROLLBARWIDTH(), tHeight, .T.)

                     hws := 0
                     FOR b := 1 TO Len(_HMG_aControlProcedures[p])
                        hws += hmg_ListView_GetColumnWidth(_HMG_aControlHandles[p], b - 1)
                     NEXT b

                     IF hws > _HMG_aControlWidth[p] - GETVSCROLLBARWIDTH() - 4

                        hmg_MoveWindow(_HMG_aControlIds[p], tCol + DeltaCol + tWidth - GETVSCROLLBARWIDTH() - sx, tRow + DeltaRow - sy, GETVSCROLLBARWIDTH(), tHeight - GetHScrollBarHeight(), .T.)
                        hmg_MoveWindow(_HMG_aControlMiscData1[p][1], tCol + DeltaCol + tWidth - GETVSCROLLBARWIDTH() - sx, tRow + DeltaRow + tHeight - GetHScrollBarHeight() - sy, ;
                           GetWindowWidth(_HMG_aControlMiscData1[p][1]), GetWindowHeight(_HMG_aControlMiscData1[p][1]), .T.)

                     ELSE

                        hmg_MoveWindow(_HMG_aControlIds[p], tCol + DeltaCol + tWidth - GETVSCROLLBARWIDTH() - sx, tRow + DeltaRow - sy, GETVSCROLLBARWIDTH(), tHeight, .T.)
                        hmg_MoveWindow(_HMG_aControlMiscData1[p][1], tCol + DeltaCol + tWidth - GETVSCROLLBARWIDTH() - sx, tRow + DeltaRow + tHeight - GetHScrollBarHeight() - sy, 0, 0, .T.)

                     ENDIF

                     _BrowseRefresh("", "", p)
                     hmg_ReDrawWindow(_HMG_aControlIds[p])
                     hmg_ReDrawWindow(_HMG_aControlMiscData1[p][1])

                  ELSE

                     hmg_MoveWindow(_HMG_aControlPageMap[x][r][w][1], tCol + DeltaCol - sx, tRow + DeltaRow - sy, tWidth, tHeight, .T.)

                  ENDIF

                  _HMG_aControlRow[p] := tRow + DeltaRow
                  _HMG_aControlCol[p] := tCol + DeltaCol

                  _HMG_aControlContainerRow[p] := Row
                  _HMG_aControlContainerCol[p] := Col
#endif
               ELSEIF p > 0 .AND. (_HMG_aControlType[p] == CONTROL_TYPE_BTNNUMTEXT .OR. _HMG_aControlType[p] == CONTROL_TYPE_BTNTEXT .OR. (_HMG_aControlType[p] == CONTROL_TYPE_GETBOX .AND. _HMG_aControlMiscData1[p][4] != NIL))

                  tCol    := _HMG_aControlCol   [p]
                  tRow    := _HMG_aControlRow   [p]
                  tWidth  := _HMG_aControlWidth [p]
                  tHeight := _HMG_aControlHeight[p]

                  IF _HMG_aControlType[p] == CONTROL_TYPE_GETBOX
                     hmg_MoveWindow(_HMG_aControlRangeMin[p][1], tCol + DeltaCol - sx, tRow + DeltaRow - sy, tWidth + DelTaWidth, tHeight, .T.)
                     hmg_MoveBtnTextBox(_HMG_aControlRangeMin[p][1], _HMG_aControlRangeMin[p][2], _HMG_aControlRangeMin[p][3], ;
                        _HMG_aControlMiscData1[p][7], _HMG_aControlMiscData1[p][6], tWidth + DelTaWidth, tHeight)
                  ELSE

                     hmg_MoveWindow(_HMG_aControlSpacing[p][1], tCol + DeltaCol - sx, tRow + DeltaRow - sy, tWidth + DelTaWidth, tHeight, .T.)
                     hmg_MoveBtnTextBox(_HMG_aControlSpacing[p][1], _HMG_aControlSpacing[p][2], _HMG_aControlSpacing[p][3], ;
                        _HMG_aControlMiscData1[p][2], _HMG_aControlRangeMin[p], tWidth + DelTaWidth, tHeight)
                  ENDIF

                  _HMG_aControlRow[p] := _HMG_aControlRow[p] + DeltaRow
                  _HMG_aControlCol[p] := _HMG_aControlCol[p] + DeltaCol
                  _HMG_aControlWidth[p] := _HMG_aControlWidth[p] + DeltaWidth
                  _HMG_aControlContainerRow[p] := Row
                  _HMG_aControlContainerCol[p] := Col

               ELSEIF p > 0 .AND. _HMG_aControlType[p] == CONTROL_TYPE_GETBOX

                  hmg_MoveWindow(_HMG_aControlRangeMin[p][1], _HMG_aControlCol[p] + DeltaCol - sx, _HMG_aControlRow[p] + DeltaRow - sy, _HMG_aControlWidth[p], _HMG_aControlHeight[p], .T.)

                  _HMG_aControlRow[p] := _HMG_aControlRow[p] + DeltaRow
                  _HMG_aControlCol[p] := _HMG_aControlCol[p] + DeltaCol
                  _HMG_aControlContainerRow[p] := Row
                  _HMG_aControlContainerCol[p] := Col

               ELSE

                  FOR z := 1 TO Len(_HMG_aControlPageMap[x][r][w])

                     FOR xx := 1 TO Len(_HMG_aControlType)

                        IF hb_IsArray(_HMG_aControlhandles[xx])

                           IF _HMG_aControlPageMap[x][r][w] == _HMG_aControlhandles[xx]

                              IF _HMG_aControlType[xx] == CONTROL_TYPE_RADIOGROUP
                                 IF _HMG_aControlMiscData1[xx]
                                    hmg_MoveWindow(_HMG_aControlhandles[xx][z], _HMG_aControlCol[xx] + DeltaCol - sx + (_HMG_aControlWidth[xx] + _HMG_aControlSpacing[xx]) * (z - 1), _HMG_aControlRow[xx] + DeltaRow - sy, ;
                                       _HMG_aControlWidth[xx], _HMG_aControlHeight[xx], .T.)
                                 ELSE
                                    hmg_MoveWindow(_HMG_aControlhandles[xx][z], _HMG_aControlCol[xx] + DeltaCol - sx, _HMG_aControlRow[xx] + DeltaRow - sy + _HMG_aControlSpacing[xx] * (z - 1), _HMG_aControlWidth[xx], 28, .T.)
                                 ENDIF
                              ENDIF

                              IF _HMG_aControlType[xx] == CONTROL_TYPE_RADIOGROUP .AND. z == Len(_HMG_aControlPageMap[x][r][w])
                                 _HMG_aControlRow[xx] := _HMG_aControlRow[xx] + DeltaRow
                                 _HMG_aControlCol[xx] := _HMG_aControlCol[xx] + DeltaCol
                                 _HMG_aControlContainerRow[xx] := Row
                                 _HMG_aControlContainerCol[xx] := Col
                              ENDIF

                              IF _HMG_aControlType[xx] == CONTROL_TYPE_SPINNER .AND. z == 1
                                 // JD 07/22/2007
                                 SpinW := GetWindowWidth(_HMG_aControlhandles[xx][2])
                                 hmg_MoveWindow(_HMG_aControlhandles[xx][1], _HMG_aControlCol[xx] + DeltaCol - sx, _HMG_aControlRow[xx] + DeltaRow - sy, _HMG_aControlWidth[xx] - SpinW + Get3DEdgeWidth(), _HMG_aControlHeight[xx], .T.)
                              ENDIF

                              IF _HMG_aControlType[xx] == CONTROL_TYPE_SPINNER .AND. z == 2
                                 // JD 07/22/2007
                                 SpinW := GetWindowWidth(_HMG_aControlhandles[xx][2])
                                 hmg_MoveWindow(_HMG_aControlhandles[xx][2], _HMG_aControlCol[xx] + DeltaCol - sx + _HMG_aControlWidth[xx] - SpinW, _HMG_aControlRow[xx] + DeltaRow - sy, SpinW, _HMG_aControlHeight[xx], .T.)
                                 _HMG_aControlRow[xx] := _HMG_aControlRow[xx] + DeltaRow
                                 _HMG_aControlCol[xx] := _HMG_aControlCol[xx] + DeltaCol
                                 _HMG_aControlContainerRow[xx] := Row
                                 _HMG_aControlContainerCol[xx] := Col
                              ENDIF

                           ENDIF

                        ENDIF

                     NEXT xx

                  NEXT z

               ENDIF

            ENDIF

         NEXT w

      NEXT r

   CASE T == CONTROL_TYPE_SPINNER

      // JD 07/22/2007
      SpinW := GetWindowWidth(c[2])

      IF _HMG_aControlContainerRow[x] == -1

         _HMG_aControlRow   [x] := Row
         _HMG_aControlCol   [x] := Col
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         // JD 07/22/2007
         hmg_MoveWindow(c[1], Col - sx, Row - sy, Width - SpinW + Get3DEdgeWidth(), Height, .T.)
         hmg_MoveWindow(c[2], Col - sx + Width - SpinW, Row - sy, SpinW, Height, .T.)

      ELSE

         _HMG_aControlRow   [x] := Row + _HMG_aControlContainerRow[x]
         _HMG_aControlCol   [x] := Col + _HMG_aControlContainerCol[x]
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         // JD 07/22/2007
         hmg_MoveWindow(c[1], Col + _HMG_aControlContainerCol[x] - sx, Row + _HMG_aControlContainerRow[x] - sy, Width - SpinW + Get3DEdgeWidth(), Height, .T.)
         hmg_MoveWindow(c[2], Col + _HMG_aControlContainerCol[x] - sx + Width - SpinW, Row + _HMG_aControlContainerRow[x] - sy, SpinW, Height, .T.)

      ENDIF

#ifdef _DBFBROWSE_
   CASE T == CONTROL_TYPE_BROWSE

      IF _HMG_aControlContainerRow[x] == -1

         _HMG_aControlRow   [x] := Row
         _HMG_aControlCol   [x] := Col
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         IF _HMG_aControlIds[x] != 0

            hmg_MoveWindow(c, col - sx, Row - sy, Width - GETVSCROLLBARWIDTH(), Height, .T.)

            hws := 0
            FOR b := 1 TO Len(_HMG_aControlProcedures[x])
               hws += hmg_ListView_GetColumnWidth(_HMG_aControlHandles[x], b - 1)
            NEXT b

            IF hws > _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - 4

               hmg_MoveWindow(_HMG_aControlIds[x], Col + Width - sx - GETVSCROLLBARWIDTH(), Row - sy, GETVSCROLLBARWIDTH(), Height - GetHScrollBarHeight(), .T.)
               hmg_MoveWindow(_HMG_aControlMiscData1[x][1], Col + Width - sx - GETVSCROLLBARWIDTH(), Row + Height - sy - GetHScrollBarHeight(), GetWindowWidth(_HMG_aControlMiscData1[x][1]), GetWindowHeight(_HMG_aControlMiscData1[x][1]), .T.)

            ELSE

               hmg_MoveWindow(_HMG_aControlIds[x], col + Width - sx - GETVSCROLLBARWIDTH(), Row - sy, GETVSCROLLBARWIDTH(), Height, .T.)
               hmg_MoveWindow(_HMG_aControlMiscData1[x][1], col + Width - sx - GETVSCROLLBARWIDTH(), Row + Height - sy - GetHScrollBarHeight(), 0, 0, .T.)

            ENDIF

            _BrowseRefresh("", "", x)
            hmg_ReDrawWindow(_HMG_aControlIds[x])
            hmg_ReDrawWindow(_HMG_aControlMiscData1[x][1])
         ELSE
            hmg_MoveWindow(c, col - sx, Row - sy, Width, Height, .T.)
         ENDIF

      ELSE

         _HMG_aControlRow   [x] := Row + _HMG_aControlContainerRow[x]
         _HMG_aControlCol   [x] := Col + _HMG_aControlContainerCol[x]
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         IF _HMG_aControlIds[x] != 0

            hmg_MoveWindow(c, col + _HMG_aControlContainerCol[x] - sx, Row + _HMG_aControlContainerRow[x] - sy, Width - GETVSCROLLBARWIDTH(), Height, .T.)

            hws := 0
            FOR b := 1 TO Len(_HMG_aControlProcedures[x])
               hws += hmg_ListView_GetColumnWidth(_HMG_aControlHandles[x], b - 1)
            NEXT b

            IF hws > _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - 4

               hmg_MoveWindow(_HMG_aControlIds[x], col + _HMG_aControlContainerCol[x] + Width - sx - GETVSCROLLBARWIDTH(), Row + _HMG_aControlContainerRow[x] - sy, GETVSCROLLBARWIDTH(), Height - GetHScrollBarHeight(), .T.)
               hmg_MoveWindow(_HMG_aControlMiscData1[x][1], col + _HMG_aControlContainerCol[x] + Width - sx - GETVSCROLLBARWIDTH(), Row + _HMG_aControlContainerRow[x] + Height - sy - GetHScrollBarHeight(), ;
                  GetWindowWidth(_HMG_aControlMiscData1[x][1]), GetWindowHeight(_HMG_aControlMiscData1[x][1]), .T.)

            ELSE

               hmg_MoveWindow(_HMG_aControlIds[x], col + _HMG_aControlContainerCol[x] + Width - sx - GETVSCROLLBARWIDTH(), Row + _HMG_aControlContainerRow[x] - sy, GETVSCROLLBARWIDTH(), Height, .T.)
               hmg_MoveWindow(_HMG_aControlMiscData1[x][1], col + _HMG_aControlContainerCol[x] + Width - sx - GETVSCROLLBARWIDTH(), Row + _HMG_aControlContainerRow[x] + Height - sy - GetHScrollBarHeight(), 0, 0, .T.)

            ENDIF

            _BrowseRefresh("", "", x)
            hmg_ReDrawWindow(_HMG_aControlIds[x])
            hmg_ReDrawWindow(_HMG_aControlMiscData1[x][1])

         ELSE

            hmg_MoveWindow(c, col + _HMG_aControlContainerCol[x] - sx, Row + _HMG_aControlContainerRow[x] - sy, Width, Height, .T.)

         ENDIF

      ENDIF

      hmg_ReDrawWindow(c)
#endif
   CASE T == CONTROL_TYPE_RADIOGROUP

      p := Array(Len(c))
      IF _HMG_aControlContainerRow[x] == -1

         _HMG_aControlRow  [x] := Row
         _HMG_aControlCol  [x] := Col
         _HMG_aControlWidth[x] := Width

         FOR i := 1 TO Len(c)

            IF _HMG_aControlHeadClick[x]
               Width := hmg_GetTextWidth(NIL, _HMG_aControlCaption[x][i], _HMG_aControlFontHandle[x]) + 21
               p[i] := Width
               height := hmg_GetTextHeight(NIL, _HMG_aControlCaption[x][i], _HMG_aControlFontHandle[x]) + 8
            ENDIF
            IF _HMG_aControlMiscData1[x]
               IF _HMG_aControlHeadClick[x] .AND. i > 1
                  NewCol += p[i - 1] + _HMG_aControlSpacing[x]
               ELSE
                  NewCol := Col + (i - 1) * (Width + _HMG_aControlSpacing[x])
               ENDIF
               hmg_MoveWindow(c[i], NewCol - sx, Row - sy, Width, height, .T.)
            ELSE
               NewRow := Row + (i - 1) * _HMG_aControlSpacing[x]
               hmg_MoveWindow(c[i], col - sx, NewRow - sy, Width, iif(_HMG_aControlHeadClick[x], height, 28), .T.)
            ENDIF

         NEXT i

      ELSE

         _HMG_aControlRow  [x] := Row + _HMG_aControlContainerRow[x]
         _HMG_aControlCol  [x] := Col + _HMG_aControlContainerCol[x]
         _HMG_aControlWidth[x] := Width

         FOR i := 1 TO Len(c)

            IF _HMG_aControlHeadClick[x]
               Width := hmg_GetTextWidth(NIL, _HMG_aControlCaption[x][i], _HMG_aControlFontHandle[x]) + 21
               p[i] := Width
               height := hmg_GetTextHeight(NIL, _HMG_aControlCaption[x][i], _HMG_aControlFontHandle[x]) + 8
            ENDIF
            IF _HMG_aControlMiscData1[x]
               IF _HMG_aControlHeadClick[x] .AND. i > 1
                  NewCol += p[i - 1] + _HMG_aControlSpacing[x]
               ELSE
                  NewCol := Col + _HMG_aControlContainerCol[x] + (i - 1) * (Width + _HMG_aControlSpacing[x])
               ENDIF
               hmg_MoveWindow(c[i], NewCol - sx, Row + _HMG_aControlContainerRow[x] - sy, Width, height, .T.)
            ELSE
               NewRow := Row + _HMG_aControlContainerRow[x] + (i - 1) * _HMG_aControlSpacing[x]
               hmg_MoveWindow(c[i], Col + _HMG_aControlContainerCol[x] - sx, NewRow - sy, Width, iif(_HMG_aControlHeadClick[x], height, 28), .T.)
            ENDIF

         NEXT i

      ENDIF

      _RedrawControl(x)

   CASE T == CONTROL_TYPE_BTNTEXT .OR. T == CONTROL_TYPE_BTNNUMTEXT .OR. (T == CONTROL_TYPE_GETBOX .AND. _HMG_aControlMiscData1[x][4] != NIL)

      IF _HMG_aControlContainerRow[x] == -1

         _HMG_aControlRow   [x] := Row
         _HMG_aControlCol   [x] := Col
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         IF T == CONTROL_TYPE_GETBOX
            hmg_MoveWindow(_HMG_aControlRangeMin[x][1], col - sx, row - sy, width, height, .T.)
            hmg_MoveBtnTextBox(_HMG_aControlRangeMin[x][1], _HMG_aControlRangeMin[x][2], _HMG_aControlRangeMin[x][3], ;
               _HMG_aControlMiscData1[x][7], _HMG_aControlMiscData1[x][6], width, height)
         ELSE
            hmg_MoveWindow(_HMG_aControlSpacing[x][1], col - sx, row - sy, width, height, .T.)
            hmg_MoveBtnTextBox(_HMG_aControlSpacing[x][1], _HMG_aControlSpacing[x][2], _HMG_aControlSpacing[x][3], ;
               _HMG_aControlMiscData1[x][2], _HMG_aControlRangeMin[x], width, height)
         ENDIF

      ELSE

         _HMG_aControlRow   [x] := Row + _HMG_aControlContainerRow[x]
         _HMG_aControlCol   [x] := Col + _HMG_aControlContainerCol[x]
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         IF T == CONTROL_TYPE_GETBOX
            hmg_MoveWindow(_HMG_aControlRangeMin[x][1], col + _HMG_aControlContainerCol[x] - sx, row + _HMG_aControlContainerRow[x] - sy, width, height, .T.)
            hmg_MoveBtnTextBox(_HMG_aControlRangeMin[x][1], _HMG_aControlRangeMin[x][2], _HMG_aControlRangeMin[x][3], ;
               _HMG_aControlMiscData1[x][7], _HMG_aControlMiscData1[x][6], width, height)
         ELSE
            hmg_MoveWindow(_HMG_aControlSpacing[x][1], col + _HMG_aControlContainerCol[x] - sx, row + _HMG_aControlContainerRow[x] - sy, width, height, .T.)
            hmg_MoveBtnTextBox(_HMG_aControlSpacing[x][1], _HMG_aControlSpacing[x][2], _HMG_aControlSpacing[x][3], ;
               _HMG_aControlMiscData1[x][2], _HMG_aControlRangeMin[x], width, height)
         ENDIF

      ENDIF

   OTHERWISE

      IF _HMG_aControlContainerRow[x] == -1

         _HMG_aControlRow   [x] := Row
         _HMG_aControlCol   [x] := Col
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         hmg_MoveWindow(c, col - sx, row - sy, width, height, .T.)

      ELSE

         _HMG_aControlRow   [x] := Row + _HMG_aControlContainerRow[x]
         _HMG_aControlCol   [x] := Col + _HMG_aControlContainerCol[x]
         _HMG_aControlWidth [x] := Width
         _HMG_aControlHeight[x] := Height

         hmg_MoveWindow(c, col + _HMG_aControlContainerCol[x] - sx, row + _HMG_aControlContainerRow[x] - sy, width, height, .T.)

      ENDIF

      IF T == CONTROL_TYPE_LABEL .OR. T == CONTROL_TYPE_BUTTON
         _Refresh(x)
      ENDIF

   ENDCASE

RETURN NIL

FUNCTION _GetItemCount(ControlName, ParentForm)

   LOCAL nRetVal As Numeric
   LOCAL t
   LOCAL c

   t := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)

   SWITCH t

   CASE CONTROL_TYPE_TREE
      nRetVal := hmg_TreeView_GetCount(c)
      EXIT

   // CASE "LIST" $ t
   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTICHKLIST
   CASE CONTROL_TYPE_MULTILIST
      nRetVal := ListBoxGetItemCount(c)
      EXIT

   CASE CONTROL_TYPE_COMBO
      nRetVal := ComboBoxGetItemCount(c)
      EXIT

   // CASE "GRID" $ t
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      nRetVal := hmg_ListViewGetItemCount(c)
      EXIT

   CASE CONTROL_TYPE_TAB
      nRetVal := Len(_HMG_aControlPageMap[GetControlIndex(ControlName, ParentForm)])

   ENDSWITCH

RETURN nRetVal

FUNCTION _GetControlRow(ControlName, ParentForm)

   LOCAL mVar
   LOCAL i

   mVar := "_" + ParentForm + "_" + ControlName
#ifdef _NAMES_LIST_
   i := _GetNameList(mVar)
#else
   i := __mvGet(mVar)
#endif

   IF i == 0 .OR. (_HMG_aControlRow[i] == NIL .AND. _HMG_aControlCol[i] == NIL)
      RETURN 0
   ENDIF

RETURN iif(_HMG_aControlContainerRow[i] == -1, _HMG_aControlRow[i], _HMG_aControlRow[i] - _HMG_aControlContainerRow[i])

FUNCTION _GetControlCol(ControlName, ParentForm)

   LOCAL mVar
   LOCAL i

   mVar := "_" + ParentForm + "_" + ControlName
#ifdef _NAMES_LIST_
   i := _GetNameList(mVar)
#else
   i := __mvGet(mVar)
#endif

   IF i == 0 .OR. (_HMG_aControlRow[i] == NIL .AND. _HMG_aControlCol[i] == NIL)
      RETURN 0
   ENDIF

RETURN iif(_HMG_aControlContainerCol[i] == -1, _HMG_aControlCol[i], _HMG_aControlCol[i] - _HMG_aControlContainerCol[i])

FUNCTION _GetControlWidth(ControlName, ParentForm)

   LOCAL mVar
   LOCAL i

   mVar := "_" + ParentForm + "_" + ControlName

#ifdef _NAMES_LIST_
   i := _GetNameList(mVar)
#else
   i := __mvGet(mVar)
#endif
   IF i == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlWidth[i]

FUNCTION _GetControlHeight(ControlName, ParentForm)

   LOCAL mVar
   LOCAL i

   mVar := "_" + ParentForm + "_" + ControlName

#ifdef _NAMES_LIST_
   i := _GetNameList(mVar)
#else
   i := __mvGet(mVar)
#endif
   IF i == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlHeight[i]

FUNCTION _SetControlCaption(ControlName, ParentForm, Value)

   LOCAL cValue As String
   LOCAL cCaption
   LOCAL i
   LOCAL c
   LOCAL x

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      Assign cValue := Value

      IF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBUTTON

         cCaption := Upper(_HMG_aControlCaption[i])

         IF (x := hb_UAt("&", cCaption)) > 0

            c := Asc(hb_USubStr(cCaption, x + 1, 1))

            IF c >= 48 .AND. c <= 90
               _ReleaseHotKey(ParentForm, MOD_ALT, c)
            ENDIF

         ENDIF

         hmg_SetToolButtonCaption(_HMG_aControlContainerHandle[i], _HMG_aControlIds[i], cValue)

         cCaption := Upper(cValue)

         IF (x := hb_UAt("&", cValue)) > 0
            _DefineLetterOrDigitHotKey(cCaption, x, ParentForm, _HMG_aControlProcedures[i])
         ENDIF

         _HMG_aControlCaption[i] := cValue

      ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBAR

         IF _IsControlSplitBoxed(ControlName, ParentForm)
            x := GetFormIndex(ParentForm)
            hmg_SetCaptionSplitBoxItem(_HMG_aFormReBarHandle[x], _HMG_aControlMiscData1[i] - 1, cValue)
            _HMG_aControlCaption[i] := cValue
         ENDIF

      ELSE
         // This assignment should be placed before a caption setting
         _HMG_aControlCaption[i] := cValue

         hmg_SetWindowText(GetControlHandle(ControlName, ParentForm), cValue)

      ENDIF

   ENDIF

RETURN NIL

FUNCTION _SetPicture(ControlName, ParentForm, FileName)

   LOCAL cImage
   LOCAL oGet
   LOCAL w
   LOCAL h
   LOCAL t
   LOCAL i
   LOCAL c
   LOCAL cDiskFile

   c := GetControlHandle(ControlName, ParentForm)
   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)

   SWITCH t

   CASE CONTROL_TYPE_IMAGE
      IF Empty(_HMG_aControlValue[i])
         w := _HMG_aControlRangeMin[i]  // original Width
         h := _HMG_aControlRangeMax[i]  // original Height
      ELSE
         w := _HMG_aControlWidth[i]
         h := _HMG_aControlHeight[i]
      ENDIF
      hmg_DeleteObject(_hmg_aControlBrushHandle[i])
      _HMG_aControlPicture[i] := FileName
      _HMG_aControlBrushHandle[i] := hmg_C_SetPicture(c, FileName, w, h, _HMG_aControlValue[i], _HMG_aControlInputMask[i], ;
         _HMG_aControlSpacing[i], _HMG_aControlCaption[i], _HMG_aControlDblClick[i] .AND. HasAlpha(FileName), _HMG_aControlMiscData1[i])
      IF Empty(_HMG_aControlValue[i])
         _HMG_aControlWidth[i] := GetWindowWidth(c)
         _HMG_aControlHeight[i] := GetWindowHeight(c)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_GETBOX
      oGet := _HMG_aControlHeadClick[i]
      oGet:SetFocus()
      oGet:Picture := Filename
      _HMG_aControlInputMask[i] := _GetPictureData(oGet, Filename)
      _SetValue(NIL, NIL, oGet:VarGet(), i)
      EXIT

   CASE CONTROL_TYPE_ANIGIF
      _HMG_aControlPicture[i] := FileName
      IF !hb_FileExists(FileName)
         cDiskFile := TempFile(GetTempFolder(), "gif")
         IF hmg_RCDataToFile(Filename, cDiskFile, "GIF") > 0
            IF hb_FileExists(cDiskFile)
               FileName := cDiskFile
            ENDIF
         ENDIF
      ENDIF
      oGet := _HMG_aControlIds[i]
      oGet:cFileName := FileName
      oGet:Restart()
      IF hb_FileExists(cDiskFile)
         FErase(cDiskFile)
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      hmg_DeleteObject(_HMG_aControlSpacing[i][4])
      _HMG_aControlPicture[i] := FileName
      cImage := iif(hb_IsArray(Filename) .AND. Len(Filename) > 0, Filename[1], Filename)
      _HMG_aControlSpacing[i][4] := hmg__SetBtnPicture(_HMG_aControlSpacing[i][2], cImage)
      IF hb_IsArray(Filename) .AND. Len(Filename) > 1
         hmg_DeleteObject(_HMG_aControlSpacing[i][5])
         _HMG_aControlSpacing[i][5] := hmg__SetBtnPicture(_HMG_aControlSpacing[i][3], Filename[2])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      IF _HMG_aControlMiscData1[i] == 1 .AND. hb_IsNumeric(Filename)
         hmg_SetToolButtonImage(_HMG_aControlContainerHandle[i], _HMG_aControlIds[i], hb_defaultValue(Filename, 0))
      ELSE
         h := _HMG_aControlHandles[i]
         _HMG_aControlHandles[i] := hmg_ReplaceToolButtonImage(_HMG_aControlContainerHandle[i], c, Filename, Empty(Filename), _HMG_aControlIds[i])
         hmg_ReDrawWindow(_HMG_aControlContainerHandle[i])
         _HMG_aControlPicture[i] := FileName
         IF !Empty(_HMG_aControlHandles[i])
            hmg_DeleteObject(h)
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TIMER
   CASE CONTROL_TYPE_COMBO
      _HMG_aControlPicture[i] := FileName
      EXIT

   CASE CONTROL_TYPE_SPINNER
      hmg_SetSpinnerIncrement(_HMG_aControlHandles[i][2], FileName)
      _HMG_aControlPicture[i] := FileName
      EXIT

   OTHERWISE  // picture for [check]buttons

      IF _HMG_aControlEnabled[i]

         IF !Empty(_HMG_aControlBrushhandle[i])
            IF t != CONTROL_TYPE_OBUTTON .AND. _HMG_IsThemed
               hmg_ImageList_Destroy(_HMG_aControlBrushHandle[i])
            ENDIF
            IF !Empty(_HMG_aControlMiscData1[i])
               hmg_DestroyIcon(_HMG_aControlBrushHandle[i])
            ELSE
               hmg_DeleteObject(_HMG_aControlBrushHandle[i])
            ENDIF
         ENDIF

         IF hb_IsString(FileName) .OR. hb_IsArray(Filename)
            _HMG_aControlPicture[i] := FileName
            cImage := iif(hb_IsArray(Filename), Filename[1], Filename)
         ENDIF

         IF _HMG_aControlMiscData1[i] == 0  // bitmap
            IF t != CONTROL_TYPE_OBUTTON .AND. _HMG_IsThemed
               _HMG_aControlBrushHandle[i] := hmg__SetMixedBtnPicture(c, cImage)
               hmg_ReDrawWindow(c)
            ELSE
               IF t == CONTROL_TYPE_OBUTTON
                  _HMG_aControlBrushHandle[i] := hmg__SetBtnPicture(c, cImage, _HMG_aControlHeadClick[i][1], _HMG_aControlHeadClick[i][2])
               ELSE
                  _HMG_aControlBrushHandle[i] := hmg__SetBtnPicture(c, cImage, -1, -1)
               ENDIF
            ENDIF
         ELSE                                // icon
            IF t != CONTROL_TYPE_OBUTTON .AND. _HMG_IsThemed
               _HMG_aControlBrushHandle[i] := hmg__SetMixedBtnIcon(c, cImage)
               hmg_ReDrawWindow(c)
            ELSE
               IF hb_IsString(cImage)
                  _HMG_aControlBrushHandle[i] := hmg__SetBtnIcon(c, cImage)
               ELSE
                  _HMG_aControlBrushHandle[i] := Filename
                  hmg_SendMessage(c, STM_SETIMAGE, IMAGE_ICON, Filename)
                  hmg_ReDrawWindow(c)
               ENDIF
            ENDIF
         ENDIF

      ENDIF

   ENDSWITCH

RETURN NIL

STATIC FUNCTION _EnableToolbarButton(ButtonName, FormName)

   LOCAL cCaption
   LOCAL i
   LOCAL x

   IF (i := GetControlIndex(ButtonName, FormName)) > 0
      EnableToolButton(_HMG_aControlContainerHandle[i], GetControlId(ButtonName, FormName))
      IF hb_IsString(_HMG_aControlCaption[i])
         cCaption := Upper(_HMG_aControlCaption[i])
         IF (x := hb_UAt("&", cCaption)) > 0
            _DefineLetterOrDigitHotKey(cCaption, x, FormName, _HMG_aControlProcedures[i])
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

STATIC FUNCTION _DisableToolbarButton(ButtonName, FormName)

   LOCAL cCaption
   LOCAL c
   LOCAL i

   IF (i := GetControlIndex(ButtonName, FormName)) > 0
      DisableToolButton(_HMG_aControlContainerHandle[i], GetControlId(ButtonName, FormName))
      IF hb_IsString(_HMG_aControlCaption[i])
         cCaption := Upper(_HMG_aControlCaption[i])
         IF (i := hb_UAt("&", cCaption)) > 0
            c := Asc(hb_USubStr(cCaption, i + 1, 1))
            IF c >= 48 .AND. c <= 90
               _ReleaseHotKey(FormName, MOD_ALT, c)
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

STATIC FUNCTION _SetGetChkListItemState(ControlName, ParentForm, Item, lState)

   LOCAL RetVal As Logical
   LOCAL i
   LOCAL t
   LOCAL uSel

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      T := _HMG_aControlType[i]
      IF T == CONTROL_TYPE_CHKLIST .OR. T == CONTROL_TYPE_MULTICHKLIST

         IF item > 0 .AND. item <= ListBoxGetItemCount(_HMG_aControlHandles[i])

            IF hb_IsLogical(lState)
               IF T == CONTROL_TYPE_MULTICHKLIST
                  uSel := hmg_ListBoxGetMultiSel(_HMG_aControlHandles[i])
               ELSE
                  uSel := ListBoxGetCursel(_HMG_aControlHandles[i])
               ENDIF

               ChkList_SetCheckBox(_HMG_aControlHandles[i], Item, iif(lState, 2, 1))

               IF T == CONTROL_TYPE_MULTICHKLIST
                  hmg_ListBoxSetMultiSel(_HMG_aControlHandles[i], uSel)
               ELSE
                  ListBoxSetCursel(_HMG_aControlHandles[i], uSel)
               ENDIF
            ELSE
               RetVal := ChkList_GetCheckBox(_HMG_aControlHandles[i], Item)
            ENDIF

         ENDIF

      ENDIF

   ENDIF

RETURN RetVal

STATIC FUNCTION _SetGetDatePickerDateFormat(ControlName, ParentForm, cFormat)

   LOCAL ix

   IF (ix := GetControlIndex(ControlName, ParentForm)) > 0

      IF _HMG_aControlType[ix] == CONTROL_TYPE_DATEPICK .OR. _HMG_aControlType[ix] == CONTROL_TYPE_TIMEPICK

         IF hb_IsString(cFormat)

            IF hmg_SetDatePickerDateFormat(_HMG_aControlHandles[ix], cFormat)
               _HMG_aControlSpacing[ix] := cFormat
            ENDIF

         ELSE

            cFormat := _HMG_aControlSpacing[ix]

         ENDIF

      ENDIF

   ENDIF

RETURN cFormat

// (JK) HMG 1.0 Experimental Build 8e
STATIC FUNCTION _SetGetDropDownWidth(ControlName, ParentForm, nWidth)

   LOCAL h

   IF _IsComboExtend(ControlName, ParentForm)
      h := _hmg_acontrolrangemax[GetControlIndex(ControlName, ParentForm)]
   ELSE
      h := GetControlHandle(ControlName, ParentForm)
   ENDIF

   IF hb_IsNumeric(nWidth)
      IF nWidth == 0
         nWidth := GetWindowWidth(GetControlHandle(ControlName, ParentForm))
      ENDIF
      SetDropDownWidth(h, nWidth)
   ELSE
      nWidth := GetDropDownWidth(h)
   ENDIF

RETURN nWidth

// (JK) HMG 1.0 Experimental Build 8
FUNCTION _GetComboItemValue(ControlName, ParentForm, nItemIndex)

   LOCAL nItem As Numeric

   IF GetControlIndex(ControlName, ParentForm) > 0
      Assign nItem := nItemIndex

      RETURN hmg_ComboGetString(GetControlHandle(ControlName, ParentForm), nItem)
   ENDIF

RETURN NIL

FUNCTION _GetFormAction(ParentForm, cEvent)

   LOCAL bAction AS BLOCK
   LOCAL i

   IF (i := GetFormIndex(ParentForm)) > 0

      SWITCH cEvent
      CASE "ONINIT"             ; bAction := _HMG_aFormInitProcedure             [i] ; EXIT
      CASE "ONRELEASE"          ; bAction := _HMG_aFormReleaseProcedure          [i] ; EXIT
      CASE "ONINTERACTIVECLOSE" ; bAction := _HMG_aFormInteractiveCloseProcedure [i] ; EXIT
      CASE "ONGOTFOCUS"         ; bAction := _HMG_aFormGotFocusProcedure         [i] ; EXIT
      CASE "ONLOSTFOCUS"        ; bAction := _HMG_aFormLostFocusProcedure        [i] ; EXIT
      CASE "ONNOTIFYCLICK"
         IF GetWindowType(ParentForm) == "A"
            bAction := _HMG_aFormNotifyIconLeftClick[i]
         ENDIF
         EXIT
      CASE "ONMOUSECLICK"       ; bAction := _HMG_aFormClickProcedure            [i] ; EXIT
      CASE "ONMOUSEDRAG"        ; bAction := _HMG_aFormMouseDragProcedure        [i] ; EXIT
      CASE "ONMOUSEMOVE"        ; bAction := _HMG_aFormMouseMoveProcedure        [i] ; EXIT
      CASE "ONMOVE"             ; bAction := _HMG_aFormMoveProcedure             [i] ; EXIT
      CASE "ONSIZE"             ; bAction := _HMG_aFormSizeProcedure             [i] ; EXIT
      CASE "ONMAXIMIZE"         ; bAction := _HMG_aFormMaximizeProcedure         [i] ; EXIT
      CASE "ONMINIMIZE"         ; bAction := _HMG_aFormMinimizeProcedure         [i] ; EXIT
      CASE "ONPAINT"            ; bAction := _HMG_aFormPaintProcedure            [i] ; EXIT
      CASE "ONRESTORE"          ; bAction := _HMG_aFormRestoreProcedure          [i] ; EXIT
      CASE "ONDROPFILES"        ; bAction := _HMG_aFormDropProcedure             [i]
      ENDSWITCH

   ENDIF

RETURN bAction

STATIC FUNCTION _SetFormAction(ParentForm, Value, cEvent)

   LOCAL bBlock AS BLOCK
   LOCAL i

   IF (i := GetFormIndex(ParentForm)) > 0

      Assign bBlock := Value

      SWITCH cEvent
      CASE "ONINIT"             ; _HMG_aFormInitProcedure             [i] := bBlock ; EXIT
      CASE "ONRELEASE"          ; _HMG_aFormReleaseProcedure          [i] := bBlock ; EXIT
      CASE "ONINTERACTIVECLOSE" ; _HMG_aFormInteractiveCloseProcedure [i] := bBlock ; EXIT
      CASE "ONGOTFOCUS"         ; _HMG_aFormGotFocusProcedure         [i] := bBlock ; EXIT
      CASE "ONLOSTFOCUS"        ; _HMG_aFormLostFocusProcedure        [i] := bBlock ; EXIT
      CASE "ONNOTIFYCLICK"
         IF GetWindowType(ParentForm) == "A"
            _HMG_aFormNotifyIconLeftClick[i] := bBlock
         ENDIF
         EXIT
      CASE "ONMOUSECLICK"       ; _HMG_aFormClickProcedure            [i] := bBlock ; EXIT
      CASE "ONMOUSEDRAG"        ; _HMG_aFormMouseDragProcedure        [i] := bBlock ; EXIT
      CASE "ONMOUSEMOVE"        ; _HMG_aFormMouseMoveProcedure        [i] := bBlock ; EXIT
      CASE "ONMOVE"             ; _HMG_aFormMoveProcedure             [i] := bBlock ; EXIT
      CASE "ONSIZE"             ; _HMG_aFormSizeProcedure             [i] := bBlock ; EXIT
      CASE "ONMAXIMIZE"         ; _HMG_aFormMaximizeProcedure         [i] := bBlock ; EXIT
      CASE "ONMINIMIZE"         ; _HMG_aFormMinimizeProcedure         [i] := bBlock ; EXIT
      CASE "ONPAINT"            ; _HMG_aFormPaintProcedure            [i] := bBlock ; EXIT
      CASE "ONRESTORE"          ; _HMG_aFormRestoreProcedure          [i] := bBlock ; EXIT
      CASE "ONDROPFILES"        ; _HMG_aFormDropProcedure             [i] := bBlock
      ENDSWITCH

   ENDIF

RETURN NIL

FUNCTION _GetControlAction(ControlName, ParentForm, cEvent)

   LOCAL bAction As Block
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      SWITCH cEvent
      CASE "ONCHANGE"
         bAction := _HMG_aControlChangeProcedure[i]
         EXIT
      CASE "ONGOTFOCUS"
         bAction := _HMG_aControlGotFocusProcedure[i]
         EXIT
      CASE "ONLOSTFOCUS"
         bAction := _HMG_aControlLostFocusProcedure[i]
         EXIT
      CASE "ONDBLCLICK"
         SWITCH _HMG_aControlType[i]
         CASE CONTROL_TYPE_BROWSE
         CASE CONTROL_TYPE_GRID
         CASE CONTROL_TYPE_LIST
         CASE CONTROL_TYPE_LISTBOX
         CASE CONTROL_TYPE_TREE
            bAction := _HMG_aControlDblClick[i]
            EXIT
         CASE CONTROL_TYPE_LABEL
         CASE CONTROL_TYPE_IMAGE
            bAction := _HMG_aControlHeadClick[i]
         ENDSWITCH
         EXIT
      CASE "ONENTER"
         SWITCH _HMG_aControlType[i]
         CASE CONTROL_TYPE_BTNNUMTEXT
         CASE CONTROL_TYPE_BTNTEXT
         CASE CONTROL_TYPE_CHARMASKTEXT
         CASE CONTROL_TYPE_MASKEDTEXT
         CASE CONTROL_TYPE_NUMTEXT
         CASE CONTROL_TYPE_TEXT
         CASE CONTROL_TYPE_COMBO
            bAction := _HMG_aControlDblClick[i]
            EXIT
         OTHERWISE
            bAction := _HMG_aControlProcedures[i]
         ENDSWITCH
         EXIT
      OTHERWISE
         bAction := _HMG_aControlProcedures[i]
      ENDSWITCH

   ENDIF

RETURN bAction

STATIC FUNCTION _SetControlAction(ControlName, ParentForm, Value, cEvent)

   LOCAL bBlock As Block
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      Assign bBlock := Value

      SWITCH cEvent
      CASE "ONCHANGE"
         _HMG_aControlChangeProcedure[i] := bBlock
         EXIT
      CASE "ONGOTFOCUS"
         _HMG_aControlGotFocusProcedure[i] := bBlock
         EXIT
      CASE "ONLOSTFOCUS"
         _HMG_aControlLostFocusProcedure[i] := bBlock
         EXIT
      CASE "ONDBLCLICK"
         SWITCH _HMG_aControlType[i]
         CASE CONTROL_TYPE_BROWSE
         CASE CONTROL_TYPE_GRID
         CASE CONTROL_TYPE_LISTBOX
         CASE CONTROL_TYPE_TREE
            _HMG_aControlDblClick[i] := bBlock
            EXIT
         CASE CONTROL_TYPE_LABEL
         CASE CONTROL_TYPE_IMAGE
            _HMG_aControlHeadClick[i] := bBlock
         ENDSWITCH
         EXIT
      CASE "ONENTER"
         SWITCH _HMG_aControlType[i]
         CASE CONTROL_TYPE_BTNNUMTEXT
         CASE CONTROL_TYPE_BTNTEXT
         CASE CONTROL_TYPE_CHARMASKTEXT
         CASE CONTROL_TYPE_MASKEDTEXT
         CASE CONTROL_TYPE_NUMTEXT
         CASE CONTROL_TYPE_TEXT
         CASE CONTROL_TYPE_COMBO
            _HMG_aControlDblClick[i] := bBlock
            EXIT
         OTHERWISE
            _HMG_aControlProcedures[i] := bBlock
         ENDSWITCH
         EXIT
      OTHERWISE
         _HMG_aControlProcedures[i] := bBlock
      ENDSWITCH

      IF _HMG_aControlType[i] == CONTROL_TYPE_IMAGE .OR. _HMG_aControlType[i] == CONTROL_TYPE_LABEL
         hmg_ChangeStyle(_HMG_aControlHandles[i], SS_NOTIFY)
      ENDIF

   ENDIF

RETURN NIL

FUNCTION _GetToolTip(ControlName, ParentForm)

   LOCAL i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN ""
   ENDIF

RETURN _HMG_aControlToolTip[i]

FUNCTION _SetToolTip(ControlName, ParentForm, Value, Page)

   LOCAL cValue As String
   LOCAL i
   LOCAL t
   LOCAL c
   LOCAL h

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      c := GetControlHandle(ControlName, ParentForm)
      t := GetControlType(ControlName, ParentForm)

      IF t == CONTROL_TYPE_TAB

         IF hb_IsNumeric(Page)  // GF 10/12/2010
            IF Page > 0 .AND. Page <= Len(_HMG_aControlToolTip[i])
               Assign cValue := Value
               _HMG_aControlToolTip[i][Page] := cValue
            ENDIF
         ENDIF

      ELSE

         IF !hb_IsArray(Value)
            _HMG_aControlToolTip[i] := Value
         ENDIF
         IF t == CONTROL_TYPE_IMAGE .OR. t == CONTROL_TYPE_LABEL
            hmg_ChangeStyle(_HMG_aControlHandles[i], SS_NOTIFY)
         ENDIF
         h := GetFormToolTipHandle(ParentForm)
         IF t == CONTROL_TYPE_RADIOGROUP .OR. t == CONTROL_TYPE_SPINNER
            Assign cValue := Value
            AEval(c, {|x|hmg_SetToolTip(x, cValue, h)})
         ELSEIF hb_IsArray(Value)  // GF 25/07/2019
            IF hb_IsArray(_HMG_aControlSpacing[i])  // BTNTEXTBOX
               FOR c := 1 TO Len(Value)
                  IF Value[c] != NIL
                     hmg_SetToolTip(_HMG_aControlSpacing[i][c], Value[c], h)
                  ENDIF
               NEXT
            ELSEIF hb_IsArray(_HMG_aControlRangeMin[i])  // GETBOX
               FOR c := 1 TO Len(Value)
                  IF Value[c] != NIL
                     hmg_SetToolTip(_HMG_aControlRangeMin[i][c], Value[c], h)
                  ENDIF
               NEXT
            ENDIF
         ELSEIF !(t == CONTROL_TYPE_TOOLBUTTON)  // GF 15/11/2016
            Assign cValue := Value
            hmg_SetToolTip(c, cValue, h)
         ENDIF
         // HMG 1.0 Experimental Build 8
         IF t == CONTROL_TYPE_COMBO  // tooltips for editable or/and extend combo
            Assign cValue := Value
            IF !Empty(_hmg_acontrolrangemin[i])
               hmg_SetToolTip(_hmg_acontrolrangemin[i], cValue, h)
            ENDIF
            IF !Empty(_hmg_acontrolrangemax[i])
               hmg_SetToolTip(_hmg_acontrolrangemax[i], cValue, h)
            ENDIF
         ENDIF

      ENDIF

   ENDIF

RETURN NIL

FUNCTION _GetMultiToolTip(ControlName, ParentForm, Item)  // GF 10/12/2010

   LOCAL nItem As Numeric
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      Assign nItem := Item
      IF nItem <= _GetItemCount(ControlName, ParentForm)
         RETURN _HMG_aControlToolTip[i][nItem]
      ENDIF

   ENDIF

RETURN ""

FUNCTION _SetRangeMin(ControlName, ParentForm, Value)

   LOCAL i
   LOCAL h
   LOCAL t
   LOCAL m

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]
   t := GetControlType(ControlName, ParentForm)
   m := _HMG_aControlRangeMax[i]

   SWITCH t
   CASE CONTROL_TYPE_SLIDER      ; SetSliderRange(h, Value, m)         ; EXIT
   CASE CONTROL_TYPE_SPINNER     ; SetSpinnerRange(h[2], Value, m)     ; EXIT
   CASE CONTROL_TYPE_PROGRESSBAR ; SetProgressBarRange(h, Value, m)    ; EXIT
   CASE CONTROL_TYPE_DATEPICK    ; _SetDatePickerRange(h, Value, m, i)
   ENDSWITCH

   _HMG_aControlRangeMin[i] := Value

RETURN NIL

FUNCTION _SetRangeMax(ControlName, ParentForm, Value)

   LOCAL i
   LOCAL h
   LOCAL t
   LOCAL m

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]
   t := GetControlType(ControlName, ParentForm)
   m := _HMG_aControlRangeMin[i]

   SWITCH t
   CASE CONTROL_TYPE_SLIDER      ; SetSliderRange(h, m, Value)         ; EXIT
   CASE CONTROL_TYPE_SPINNER     ; SetSpinnerRange(h[2], m, Value)     ; EXIT
   CASE CONTROL_TYPE_PROGRESSBAR ; SetProgressBarRange(h, m, Value)    ; EXIT
   CASE CONTROL_TYPE_DATEPICK    ; _SetDatePickerRange(h, m, Value, i)
   ENDSWITCH

   _HMG_aControlRangeMax[i] := Value

RETURN NIL

FUNCTION _GetRangeMin(ControlName, ParentForm)

   LOCAL i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlRangeMin[i]

FUNCTION _GetRangeMax(ControlName, ParentForm)

   LOCAL i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN 0
   ENDIF

RETURN _HMG_aControlRangeMax[i]

FUNCTION _SetMultiCaption(ControlName, ParentForm, Column, Value)

   LOCAL nColumn As Numeric
   LOCAL i
   LOCAL h
   LOCAL t

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0  // JD 11/30/2006

      h := _HMG_aControlhandles[i]
      t := GetControlType(ControlName, ParentForm)

      Assign nColumn := Column
      _HMG_aControlCaption[i][nColumn] := Value

      SWITCH t

      // CASE "GRID" $ t
      CASE CONTROL_TYPE_GRID
      CASE CONTROL_TYPE_MULTIGRID
      CASE CONTROL_TYPE_PROPGRID
         hmg_SetGridColumnHeader(h, nColumn, Value, _HMG_aControlMiscData1[i, 3][nColumn])
         EXIT

      CASE CONTROL_TYPE_BROWSE
         hmg_SetGridColumnHeader(h, nColumn, Value, _HMG_aControlMiscData1[i, 16][nColumn])
         EXIT

      CASE CONTROL_TYPE_RADIOGROUP
         hmg_SetWindowText(h[nColumn], Value)
         _SetControlSizePos(ControlName, ParentForm, ;
            _GetControlRow(ControlName, ParentForm), _GetControlCol(ControlName, ParentForm), ;
            _GetControlWidth(ControlName, ParentForm), _GetControlHeight(ControlName, ParentForm))
         EXIT

      CASE CONTROL_TYPE_TAB
         hmg_SetTabCaption(h, nColumn, Value)
         UpdateTab(i)

      ENDSWITCH

   ENDIF

RETURN NIL

FUNCTION _GetMultiCaption(ControlName, ParentForm, Item)

   LOCAL nItem As Numeric
   LOCAL i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN ""
   ENDIF

   Assign nItem := Item

RETURN _HMG_aControlCaption[i][nItem]

FUNCTION _SetMultiImage(ControlName, ParentForm, Column, Value, lRightAlign)

   LOCAL i
   LOCAL h
   LOCAL t

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      h := _HMG_aControlhandles[i]
      t := GetControlType(ControlName, ParentForm)

      SWITCH t

      //CASE t $ "MULTIGRID,BROWSE"  // EF 01/09/2008
      CASE CONTROL_TYPE_MULTIGRID
      CASE CONTROL_TYPE_BROWSE
         hmg_SetGridColumnHeaderImage(h, Column, Value, hb_defaultValue(lRightAlign, .F.))
         EXIT

      CASE CONTROL_TYPE_TAB  // JD 11/30/2006
         IF Column > 0 .AND. Column <= Len(_HMG_aControlPicture[i])
            _HMG_aControlPicture[i][Column] := Value
         ENDIF
         IF !Empty(_HMG_aControlInputMask[i])
            hmg_IMAGELIST_DESTROY(_HMG_aControlInputMask[i])
         ENDIF
         _HMG_aControlInputMask[i] := hmg_AddTabBitMap(h, _HMG_aControlPicture[i], _HMG_aControlMiscData1[i, 8])
         UpdateTab(i)
         EXIT

      CASE CONTROL_TYPE_TREE  // GF 12/23/2013
         TreeItemChangeImage(ControlName, ParentForm, Column, Value)

      ENDSWITCH

   ENDIF

RETURN NIL

STATIC FUNCTION _GetMultiImage(ControlName, ParentForm, Item)  // JD 11/30/2006

   LOCAL nItem As Numeric
   LOCAL i := GetControlIndex(ControlName, ParentForm)

   IF i == 0
      RETURN ""
   ENDIF

   Assign nItem := Item

RETURN _HMG_aControlPicture[i][nItem]

FUNCTION InputWindow(cTitle, aLabels, aValues, aFormats, nRow, nCol, lCenterWindow, aButOKCancelCaptions, nLabelWidth, nControlWidth, lUseSwitcher)

   LOCAL ControlRow := 10
   LOCAL ControlCol
   LOCAL e := 0
   LOCAL LN
   LOCAL CN
   LOCAL r
   LOCAL c
   LOCAL nWidth
   LOCAL wHeight
   LOCAL diff
   LOCAL lExtendedNavigation
   LOCAL i
   LOCAL l

   lCenterWindow := (nRow == NIL .AND. nCol == NIL)
   DEFAULT nRow TO 0, nCol TO 0
   DEFAULT aButOKCancelCaptions TO {}
   DEFAULT nLabelWidth TO 90, nControlWidth TO 140
   DEFAULT lUseSwitcher TO .F.

   IF Len(aButOKCancelCaptions) == 0
      AAdd(aButOKCancelCaptions, _HMG_MESSAGE[6])
      AAdd(aButOKCancelCaptions, _HMG_MESSAGE[7])
   ENDIF

   l := Len(aLabels)
   PRIVATE aResult[l]

   lExtendedNavigation := _HMG_ExtendedNavigation
   _HMG_ExtendedNavigation := .T.

   FOR i := 1 TO l

      IF hb_IsChar(aValues[i])
         IF hb_IsNumeric(aFormats[i])
            IF aFormats[i] > 32
               e++
            ENDIF
         ENDIF
      ENDIF

      IF ValType(aValues[i]) == "M"
         e++
      ENDIF

   NEXT i

   wHeight := l * 30 + 90 + e * 60

   IF PCount() == 4
      r := 0
      c := 0
   ELSE
      r := nRow
      c := nCol
      IF r + wHeight > GetDeskTopHeight()
         diff := r + wHeight - GetDeskTopHeight()
         r -= diff
      ENDIF
   ENDIF

   nWidth := nLabelWidth + nControlWidth + 50

   ControlCol := nLabelWidth + 30

   DEFINE WINDOW _InputWindow AT r, c WIDTH nWidth HEIGHT wHeight TITLE cTitle MODAL NOSIZE

   FOR i := 1 TO l

      LN := "Label_" + hb_ntos(i)
      CN := "Control_" + hb_ntos(i)

      @ ControlRow, 10 LABEL (LN) VALUE aLabels[i] WIDTH nLabelWidth

      diff := 30

      SWITCH ValType(aValues[i])

      CASE "L"
         IF lUseSwitcher
            @ ControlRow, ControlCol SWITCHER (CN) HEIGHT 46 VALUE "" IMAGE {"MINIGUI_SWITCH_ON", "MINIGUI_SWITCH_OFF"}
            SetProperty ("_InputWindow", CN, "Checked", aValues[i])
         ELSE
            @ ControlRow, ControlCol CHECKBOX (CN) CAPTION " " VALUE aValues[i] AUTOSIZE
         ENDIF
         EXIT
      CASE "D"
         @ ControlRow, ControlCol DATEPICKER (CN) VALUE aValues[i] WIDTH nControlWidth
         EXIT
      CASE "N"
         IF hb_IsArray(aFormats[i])
            @ ControlRow, ControlCol COMBOBOX (CN) ITEMS aFormats[i] VALUE aValues[i] WIDTH nControlWidth
         ELSEIF hb_IsChar(aFormats[i])
            IF hb_UAt(".", aFormats[i]) > 0
               @ ControlRow, ControlCol TEXTBOX (CN) VALUE aValues[i] WIDTH nControlWidth NUMERIC INPUTMASK aFormats[i]
            ELSE
               @ ControlRow, ControlCol TEXTBOX (CN) VALUE aValues[i] WIDTH nControlWidth MAXLENGTH Len(aFormats[i]) NUMERIC
            ENDIF
         ENDIF
         EXIT
      CASE "C"
         IF hb_IsNumeric(aFormats[i])
            IF aFormats[i] <= 32
               @ ControlRow, ControlCol TEXTBOX (CN) VALUE aValues[i] WIDTH nControlWidth MAXLENGTH aFormats[i]
            ELSE
               @ ControlRow, ControlCol EDITBOX (CN) WIDTH nControlWidth HEIGHT 90 VALUE aValues[i] MAXLENGTH aFormats[i]
               diff := 94
            ENDIF
         ENDIF
         EXIT
      CASE "M"
         @ ControlRow, ControlCol EDITBOX (CN) WIDTH nControlWidth HEIGHT 90 VALUE aValues[i]
         diff := 94

      ENDSWITCH

      ControlRow += diff

   NEXT i

   @ ControlRow + 10, nWidth / 2 - 110 BUTTON BUTTON_1 CAPTION "&" + aButOKCancelCaptions[1] ACTION _InputWindowOk()

   @ ControlRow + 10, nWidth / 2 BUTTON BUTTON_2 CAPTION "&" + aButOKCancelCaptions[2] ACTION _InputWindowCancel()

   END WINDOW

   IF lCenterWindow
      CENTER WINDOW _InputWindow
   ENDIF

   ACTIVATE WINDOW _InputWindow

   _HMG_ExtendedNavigation := lExtendedNavigation

RETURN aResult

FUNCTION _InputWindowOk

   AEval(aResult, {|x, i|HB_SYMBOL_UNUSED(x), aResult[i] := ;
      iif(GetControlType("Control_" + hb_ntos(i), "_InputWindow") == CONTROL_TYPE_CHECKLABEL, ;
      GetProperty("_InputWindow", "Control_" + hb_ntos(i), "Checked"), ;
      _GetValue("Control_" + hb_ntos(i), "_InputWindow"))})

   RELEASE WINDOW _InputWindow

RETURN NIL

FUNCTION _InputWindowCancel

   AEval(aResult, {|x, i|HB_SYMBOL_UNUSED(x), aResult[i] := NIL})

   RELEASE WINDOW _InputWindow

RETURN NIL

FUNCTION _ReleaseControl(ControlName, ParentForm)

   LOCAL i
   LOCAL t
   LOCAL r
   LOCAL w
   LOCAL z
   LOCAL x
   LOCAL y
   LOCAL k

   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)
   k := GetFormIndex(ParentForm)

   SWITCH t

   CASE CONTROL_TYPE_ANIGIF
      _HMG_aControlIds[i]:End()
      EXIT

   CASE CONTROL_TYPE_ANIMATEBOX
      _DestroyAnimateBox(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_PLAYER
      _DestroyPlayer(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_PROGRESSWHEEL
      IF GetProperty(ParentForm, ControlName, "GradientMode") == 3
         SetProperty(ParentForm, ControlName, "GradientMode", 1)
      ENDIF
      hb_ADel(_HMG_aFormGraphTasks[GetFormIndex(ParentForm)], _HMG_aControlMiscData1[i], .T.)
      ReleaseControl(_HMG_aControlHandles[i])
      EXIT

   CASE CONTROL_TYPE_SPINNER
   CASE CONTROL_TYPE_RADIOGROUP
      AEval(_HMG_aControlHandles[i], {|y|ReleaseControl(y)})
      EXIT

   CASE CONTROL_TYPE_MESSAGEBAR
      IF _IsControlDefined("StatusBarKbd", ParentForm)
         ReleaseControl(_HMG_aControlHandles[GetControlIndex("StatusBarKbd", ParentForm)])
         _EraseControl(GetControlIndex("StatusBarKbd", ParentForm), k)
      ENDIF
      IF _IsControlDefined("StatusTimer", ParentForm)
         ReleaseControl(_HMG_aControlHandles[GetControlIndex("StatusTimer", ParentForm)])
         _EraseControl(GetControlIndex("StatusTimer", ParentForm), k)
      ENDIF
      IF _IsControlDefined("StatusKeyBrd", ParentForm)
         ReleaseControl(_HMG_aControlHandles[GetControlIndex("StatusKeyBrd", ParentForm)])
         _EraseControl(GetControlIndex("StatusKeyBrd", ParentForm), k)
      ENDIF
      IF (z := hmg_SendMessage(_HMG_aControlHandles[i], SB_GETPARTS, 0, 0)) > 0
         FOR x := 1 TO z
            hmg_SetStatusItemIcon(_HMG_aControlHandles[i], x, NIL)
         NEXT x
      ENDIF
      ReleaseControl(_HMG_aControlHandles[i])
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      ReleaseControl(_HMG_aControlHandles[i])
      IF _HMG_aControlIds[i] != 0
         ReleaseControl(_HMG_aControlIds[i])
         ReleaseControl(_HMG_aControlMiscData1[i][1])
      ENDIF
      EXIT
#endif

   CASE CONTROL_TYPE_TAB
      FOR r := 1 TO Len(_HMG_aControlPageMap[i])
         FOR w := 1 TO Len(_HMG_aControlPageMap[i][r])
            IF !hb_isArray(_HMG_aControlPageMap[i][r][w])
               ReleaseControl(_HMG_aControlPageMap[i][r][w])
               x := AScan(_HMG_aControlHandles, _HMG_aControlPageMap[i][r][w]) // TODO:
               IF x > 0
                  _EraseControl(x, k)
               ENDIF
            ELSE
               FOR z := 1 TO Len(_HMG_aControlPageMap[i][r][w])
                  ReleaseControl(_HMG_aControlPageMap[i][r][w][z])
               NEXT z
               FOR x := 1 TO Len(_HMG_aControlHandles)
                  IF hb_IsArray(_HMG_aControlHandles[x])
                     IF _HMG_aControlHandles[x][1] == _HMG_aControlPageMap[i][r][w][1]
                        _EraseControl(x, k)
                        EXIT
                     ENDIF
                  ENDIF
               NEXT x
            ENDIF
         NEXT w
      NEXT r
      ReleaseControl(_HMG_aControlHandles[i])
      EXIT

   OTHERWISE
      IF i > 0
         ReleaseControl(_HMG_aControlHandles[i])
      ENDIF

   ENDSWITCH

   // If the control is inside a TAB, PageMap must be updated

   FOR y := 1 TO Len(_HMG_aControlPageMap)
      IF _HMG_aControlType[y] == CONTROL_TYPE_TAB
         FOR r := 1 TO Len(_HMG_aControlPageMap[y])
            FOR w := 1 TO Len(_HMG_aControlPageMap[y][r])
               SWITCH t
               CASE CONTROL_TYPE_RADIOGROUP
                  IF hb_IsArray(_HMG_aControlPageMap[y][r][w])
                     IF _HMG_aControlPageMap[y][r][w][1] == _HMG_aControlHandles[i][1]
                        ADel(_HMG_aControlPageMap[y][r], w)
                        ASize(_HMG_aControlPageMap[y][r], Len(_HMG_aControlPageMap[y][r]) - 1)
                        EXIT
                     ENDIF
                  ENDIF
                  EXIT
               CASE CONTROL_TYPE_SPINNER
                  IF hb_IsArray(_HMG_aControlPageMap[y][r][w])
                     IF _HMG_aControlPageMap[y][r][w][1] == _HMG_aControlHandles[i][1]
                        ADel(_HMG_aControlPageMap[y][r], w)
                        ASize(_HMG_aControlPageMap[y][r], Len(_HMG_aControlPageMap[y][r]) - 1)
                        EXIT
                     ENDIF
                  ENDIF
                  EXIT
#ifdef _DBFBROWSE_
               CASE CONTROL_TYPE_BROWSE
                  IF hb_IsArray(_HMG_aControlPageMap[y][r][w])
                     IF _HMG_aControlPageMap[y][r][w][1] == _HMG_aControlHandles[i]
                        ADel(_HMG_aControlPageMap[y][r], w)
                        ASize(_HMG_aControlPageMap[y][r], Len(_HMG_aControlPageMap[y][r]) - 1)
                        EXIT
                     ENDIF
                  ELSEIF hb_IsNumeric(_HMG_aControlPageMap[y][r][w])
                     IF _HMG_aControlPageMap[y][r][w] == _HMG_aControlHandles[i]
                        ADel(_HMG_aControlPageMap[y][r], w)
                        ASize(_HMG_aControlPageMap[y][r], Len(_HMG_aControlPageMap[y][r]) - 1)
                        EXIT
                     ENDIF
                  ENDIF
                  EXIT
#endif
               OTHERWISE
                  IF hb_IsNumeric(_HMG_aControlPageMap[y][r][w])
                     IF _HMG_aControlPageMap[y][r][w] == _HMG_aControlHandles[i]
                        ADel(_HMG_aControlPageMap[y][r], w)
                        ASize(_HMG_aControlPageMap[y][r], Len(_HMG_aControlPageMap[y][r]) - 1)
                        EXIT
                     ENDIF
                  ENDIF
               ENDSWITCH
            NEXT w
         NEXT r
      ENDIF
   NEXT y

   _EraseControl(i, k)

RETURN NIL

FUNCTION _EraseControl(i, p)

   LOCAL hWnd
   LOCAL mVar
   LOCAL t
   LOCAL x

   x := _HMG_aControlFontHandle[i]

   IF (hb_IsNumeric(x) .OR. HB_ISPOINTER(x)) .AND. !Empty(x) .AND. !((t := AScan(_HMG_aControlHandles, hmg_numbertohandle(x))) > 0 .AND. _HMG_aControlType[t] == CONTROL_TYPE_FONT)
      hmg_DeleteObject(x)
   ENDIF

   IF (_HMG_aControlType[i] == CONTROL_TYPE_BUTTON .OR. _HMG_aControlType[i] == CONTROL_TYPE_OBUTTON) .AND. !Empty(_HMG_aControlMiscData1[i])
      hmg_DestroyIcon(_HMG_aControlBrushHandle[i])
   ELSE
      hmg_DeleteObject(_HMG_aControlBrushHandle[i])
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlDestroy, i)
   ENDIF

   t := _HMG_aControlType[i]

   SWITCH t

   CASE CONTROL_TYPE_HOTKEY
      hmg_ReleaseHotKey(_HMG_aControlParentHandles[i], _HMG_aControlIds[i])
      EXIT

   // CASE "LABEL" $ t
   CASE CONTROL_TYPE_CHECKLABEL
   CASE CONTROL_TYPE_LABEL
      IF _HMG_aControlMiscData1[i][2]
         _ReleaseControl("BlinkTimer" + hb_ntos(i), _HMG_aFormNames[p])
      ENDIF
      IF hb_IsArray(_HMG_aControlPicture[i])  // erase CheckLabel bitmap
         x := _HMG_aControlPicture[i][1]
         IF File(x) .AND. cFilePath(x) == GetTempFolder()
            FErase(x)
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BUTTON
      IF !Empty(_HMG_aControlBrushHandle[i]) .AND. _HMG_IsThemed .AND. hb_IsLogical(_HMG_aControlDblClick[i]) .AND. !_HMG_aControlDblClick[i]
         hmg_ImageList_Destroy(_HMG_aControlBrushHandle[i])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_CHECKBOX
      IF !Empty(_HMG_aControlBrushHandle[i]) .AND. _HMG_IsThemed
         hmg_ImageList_Destroy(_HMG_aControlBrushHandle[i])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_GETBOX
      AEval(_HMG_aControlRangeMin[i], {|x|hmg_DeleteObject(x)})
      EXIT

   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      AEval(_HMG_aControlSpacing[i], {|x|hmg_DeleteObject(x)})
      EXIT

   CASE CONTROL_TYPE_TAB
      IF !Empty(_HMG_aControlInputMask[i])
         hmg_IMAGELIST_DESTROY(_HMG_aControlInputMask[i])
      ENDIF
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      IF !Empty(_HMG_aControlMiscData1[i][15])
         hmg_IMAGELIST_DESTROY(_HMG_aControlMiscData1[i][15])
      ENDIF
      EXIT
#endif

   // CASE "GRID" $ t
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF !Empty(_HMG_aControlRangeMin[i])
         hmg_IMAGELIST_DESTROY(_HMG_aControlRangeMin[i])
      ENDIF
      EXIT

   CASE CONTROL_TYPE_TOOLBUTTON
      hmg_DeleteObject(_HMG_aControlHandles[i])
      EXIT

   CASE CONTROL_TYPE_PAGER
      // Remove Pager Child Controls
      hWnd := _HMG_aControlHandles[i]
      FOR x := 1 TO Len(_HMG_aControlHandles)
         IF _HMG_aControlParentHandles[x] == hWnd
            _EraseControl(x, p)
         ENDIF
      NEXT x

   ENDSWITCH

   mVar := "_" + _HMG_aFormNames[p] + "_" + _HMG_aControlNames[i]

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
   _HMG_aControlHandles           [i] := 0
   _HMG_aControlParentHandles     [i] := 0
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
   _HMG_aControlContainerHandle   [i] := 0
   _HMG_aControlFontName          [i] := ""
   _HMG_aControlFontSize          [i] := 0
   _HMG_aControlToolTip           [i] := ""
   _HMG_aControlRangeMin          [i] := 0
   _HMG_aControlRangeMax          [i] := 0
   _HMG_aControlCaption           [i] := ""
   _HMG_aControlVisible           [i] := .F.
   _HMG_aControlHelpId            [i] := 0
   _HMG_aControlFontHandle        [i] := 0
   _HMG_aControlFontAttributes    [i] := {}
   _HMG_aControlBrushHandle       [i] := 0
   _HMG_aControlEnabled           [i] := .F.
   _HMG_aControlMiscData1         [i] := 0
   _HMG_aControlMiscData2         [i] := ""
#ifdef _HMG_COMPAT_
   IF __mvExist("_HMG_SYSDATA[443][i]")
      _HMG_StopControlEventProcedure[i] := .F.
   ENDIF
#endif

RETURN NIL

FUNCTION _IsControlVisible(ControlName, FormName)

   LOCAL lVisible As Logical
   LOCAL ix

   IF (ix := GetControlIndex(ControlName, FormName)) > 0
      lVisible := _HMG_aControlVisible[ix]
   ENDIF

RETURN lVisible

FUNCTION _SetCaretPos(ControlName, FormName, Pos)

   LOCAL nPos As Numeric
   LOCAL i

   IF (i := GetControlIndex(ControlName, FormName)) > 0
      Assign nPos := Pos

      hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, nPos, nPos)
      hmg_SendMessage(_HMG_aControlhandles[i], EM_SCROLLCARET, 0, 0)
   ENDIF

RETURN NIL

FUNCTION _GetCaretPos(ControlName, FormName)

   LOCAL i

   IF (i := GetControlIndex(ControlName, FormName)) > 0
      RETURN hmg_HiWord(hmg_SendMessage(_HMG_aControlhandles[i], EM_GETSEL, 0, 0))
   ENDIF

RETURN 0

PROCEDURE SetProperty(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

   LOCAL ix
#ifdef _USERINIT_
   LOCAL cMacro
   LOCAL cProc
#endif

#ifdef _HMG_COMPAT_
   IF _RichEditBox_SetProperty(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)
      RETURN
   ENDIF
#endif

#ifdef _BT_
   IF _ProgressWheel_SetProperty(Arg1, Arg2, Arg3, Arg4)
      RETURN
   ENDIF
#endif

   SWITCH PCount()

   CASE 3 // PCount() == 3 (WINDOW)

      IF !_IsWindowDefined(Arg1)
         MsgMiniGuiError("Window: " + Arg1 + " is not defined.")
      ENDIF

      Arg2 := Upper(Arg2)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomPropertyProcedure
         IF Arg2 == cProc[1]
            cMacro := cProc[2]
            &cMacro(Arg1, Arg2, Arg3)
            IF _HMG_UserComponentProcess
               RETURN
            ENDIF
         ENDIF
      NEXT
#endif

      SWITCH Arg2
      CASE "TITLE"
         hmg_SetWindowText(GetFormHandle(Arg1), Arg3)
         EXIT
      CASE "GRIPPERTEXT"
         IF GetWindowType(Arg1) == "X"
            SetWindowGripperText(GetFormIndex(Arg1), Arg3)
         ENDIF
         EXIT
      CASE "HEIGHT"
         _SetWindowSizePos(Arg1, NIL, NIL, NIL, Arg3)
         EXIT
      CASE "WIDTH"
         _SetWindowSizePos(Arg1, NIL, NIL, Arg3, NIL)
         EXIT
      CASE "COL"
#ifdef _PANEL_
         IF GetWindowType(Arg1) == "P"
            Arg3 += GetBorderWidth()
         ENDIF
#endif
         _SetWindowSizePos(Arg1, NIL, Arg3, NIL, NIL)
         EXIT
      CASE "ROW"
#ifdef _PANEL_
         IF GetWindowType(Arg1) == "P"
            Arg3 += GetTitleHeight() + GetBorderHeight()
         ENDIF
#endif
         _SetWindowSizePos(Arg1, Arg3, NIL, NIL, NIL)
         EXIT
      CASE "NOTIFYICON"
         _SetNotifyIconName(Arg1, Arg3)
         EXIT
      CASE "NOTIFYTOOLTIP"
         _SetNotifyIconTooltip(Arg1, Arg3)
         EXIT
      CASE "TITLEBAR"
         _ChangeWindowStyle(Arg1, WS_CAPTION, Arg3)
         EXIT
      CASE "SYSMENU"
         _ChangeWindowStyle(Arg1, WS_SYSMENU, Arg3)
         EXIT
      CASE "SIZABLE"
         _ChangeWindowStyle(Arg1, WS_SIZEBOX, Arg3)
         EXIT
      CASE "MAXBUTTON"
         _ChangeWindowStyle(Arg1, WS_MAXIMIZEBOX, Arg3)
         EXIT
      CASE "MINBUTTON"
         _ChangeWindowStyle(Arg1, WS_MINIMIZEBOX, Arg3)
         EXIT
      CASE "CLOSABLE"
         IF hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_CAPTION) .AND. hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_SYSMENU)
            hmg_xDisableCloseButton(GetFormHandle(Arg1), Arg3)
         ENDIF
         EXIT
      CASE "VISIBLE"
         iif(Arg3, _ShowWindow(Arg1), _HideWindow(Arg1))
         EXIT
      CASE "ENABLED"
         iif(Arg3, hmg_EnableWindow(GetFormHandle(Arg1)), hmg_DisableWindow(GetFormHandle(Arg1)))
         EXIT
      CASE "TOPMOST"
         _ChangeWindowTopmostStyle(GetFormHandle(Arg1), Arg3)
         EXIT
      CASE "HELPBUTTON"
         _ChangeWindowHelpButtonStyle(Arg1, Arg3)
         EXIT
      CASE "BACKCOLOR"
         _SetWindowBackColor(GetFormHandle(Arg1), Arg3)
         EXIT
      CASE "CARGO"
         _WindowCargo(Arg1, Arg3)
         EXIT
      CASE "CURSOR"
         hmg_SetWindowCursor(GetFormHandle(Arg1), Arg3)
         _HMG_aFormMiscData1[GetFormIndex(Arg1)][2] := Arg3
         EXIT
      CASE "MINWIDTH" // Grigory Filatov HMG 1.4 Ext Build 43
         _SetGetMinMaxInfo(Arg1, 5, Arg3)
         EXIT
      CASE "MINHEIGHT"
         _SetGetMinMaxInfo(Arg1, 6, Arg3)
         EXIT
      CASE "MAXWIDTH"
         _SetGetMinMaxInfo(Arg1, 7, Arg3)
         EXIT
      CASE "MAXHEIGHT"
         _SetGetMinMaxInfo(Arg1, 8, Arg3)
         EXIT
      CASE "ONINIT"
      CASE "ONRELEASE"
      CASE "ONINTERACTIVECLOSE"
      CASE "ONGOTFOCUS"
      CASE "ONLOSTFOCUS"
      CASE "ONNOTIFYCLICK"
      CASE "ONMOUSECLICK"
      CASE "ONMOUSEDRAG"
      CASE "ONMOUSEMOVE"
      CASE "ONMOVE"
      CASE "ONSIZE"
      CASE "ONMAXIMIZE"
      CASE "ONMINIMIZE"
      CASE "ONPAINT"
      CASE "ONRESTORE"
      CASE "ONDROPFILES" // GF 07/10/19
         _SetFormAction(Arg1, Arg3, Arg2)
         EXIT
      CASE "ALPHABLENDTRANSPARENT"
         IF Arg3 >= 0 .AND. Arg3 <= 255
            hmg_SetLayeredWindowAttributes(GetFormHandle(Arg1), 0, Arg3, LWA_ALPHA)
         ENDIF
         EXIT
      CASE "BACKCOLORTRANSPARENT"
         IF _HMG_IsThemed .AND. IsArrayRGB(Arg3)
            hmg_SetLayeredWindowAttributes(GetFormHandle(Arg1), RGB(Arg3[1], Arg3[2], Arg3[3]), 0, LWA_COLORKEY)
         ENDIF
         EXIT
      OTHERWISE
         MsgMiniGuiError("Window: unrecognized property '" + Arg2 + "'.")
      ENDSWITCH

      EXIT

   CASE 4 // PCount() == 4 (CONTROL)

      Arg3 := Upper(Arg3)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomPropertyProcedure
         IF Arg3 == cProc[1]
            cMacro := cProc[2]
            &cMacro(Arg1, Arg2, Arg3, Arg4)
            IF _HMG_UserComponentProcess
               RETURN
            ENDIF
         ENDIF
      NEXT
#endif

      VerifyControlDefined(Arg1, Arg2)

      SWITCH Arg3
      CASE "CUEBANNER" /* P.Ch. 16.10. */
         IF IsVistaOrLater()
            SWITCH GetControlType(Arg2, Arg1)
            CASE CONTROL_TYPE_BTNNUMTEXT
            CASE CONTROL_TYPE_BTNTEXT
            CASE CONTROL_TYPE_CHARMASKTEXT
            CASE CONTROL_TYPE_MASKEDTEXT
            CASE CONTROL_TYPE_NUMTEXT
            CASE CONTROL_TYPE_TEXT
               SendMessageWideString(GetControlHandle(Arg2, Arg1), EM_SETCUEBANNER, .T., Arg4)
               EXIT
            CASE CONTROL_TYPE_SPINNER
               SendMessageWideString(GetControlHandle(Arg2, Arg1)[1], EM_SETCUEBANNER, .T., Arg4)
               EXIT
            CASE CONTROL_TYPE_COMBO
               ix := GetControlIndex(Arg2, Arg1)
               IF _HMG_aControlMiscData1[ix][2]
                  SendMessageWideString(_HMG_aControlRangeMin[ix], EM_SETCUEBANNER, .T., Arg4)
               ELSE
                  SendMessageWideString(GetControlHandle(Arg2, Arg1), CB_SETCUEBANNER, .T., Arg4)
               ENDIF
            ENDSWITCH
         ENDIF
         EXIT
      CASE "ALIGNMENT"  // GF 12/01/17
         _SetAlign(Arg2, Arg1, Upper(Arg4))
         EXIT
      CASE "CASECONVERT"  // GF 04/04/20
         _SetCase(Arg2, Arg1, Upper(Arg4))
         EXIT
      CASE "TRANSPARENT"  // GF 02/04/20
         _SetTransparent(Arg2, Arg1, Arg4)
         EXIT
      CASE "VALUE"
      CASE "GRADIENTFILL"
      CASE "INTERVAL"
         _SetValue(Arg2, Arg1, Arg4)
         EXIT
      CASE "FORMATSTRING"
         _SetGetDatePickerDateFormat(Arg2, Arg1, Arg4)
         EXIT
      CASE "CARGO"  //(GF) HMG 1.7 Exp. Build 76
         _ControlCargo(Arg2, Arg1, Arg4)
         EXIT
#ifdef _DBFBROWSE_
      CASE "ALLOWEDIT"
         _SetBrowseAllowEdit(Arg2, Arg1, Arg4)
         EXIT
      CASE "ALLOWAPPEND"
         _SetBrowseAllowAppend(Arg2, Arg1, Arg4)
         EXIT
      CASE "ALLOWDELETE"
         _SetBrowseAllowDelete(Arg2, Arg1, Arg4)
         EXIT
#endif
      CASE "PICTURE"
      CASE "PICTUREINDEX"
      CASE "ICON"
      CASE "ONCE"
      CASE "ONLISTCLOSE"
      CASE "ONCLOSEUP"
      CASE "INCREMENT"
         _SetPicture(Arg2, Arg1, Arg4)
         EXIT
      CASE "HBITMAP"
         _SetGetImageHBitmap(Arg2, Arg1, Arg4)
         EXIT
      CASE "TOOLTIP"
         _SetTooltip(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTNAME"
         _SetFontName(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTSIZE"
         _SetFontSize(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTBOLD"
         _SetFontBold(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTITALIC"
         _SetFontItalic(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTUNDERLINE"
         _SetFontUnderline(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTSTRIKEOUT"
         _SetFontStrikeout(Arg2, Arg1, Arg4)
         EXIT
      CASE "CAPTION"
         _SetControlCaption(Arg2, Arg1, Arg4)
         EXIT
      CASE "ACTION"
      CASE "ONCLICK"
      CASE "ONGOTFOCUS"
      CASE "ONLOSTFOCUS"
      CASE "ONCHANGE"
      CASE "ONDBLCLICK"
      CASE "ONDISPLAYCHANGE"
      CASE "ONENTER" // GF 10/28/10
         _SetControlAction(Arg2, Arg1, Arg4, Arg3)
         EXIT
      CASE "ONLISTDISPLAY"
      CASE "ONDROPDOWN" // GF 07/16/19
         ix := GetControlIndex(Arg2, Arg1)
         IF _HMG_aControltype[ix] == CONTROL_TYPE_COMBO
            _HMG_aControlInputMask[ix] := Arg4
         ENDIF
         EXIT
      CASE "DISPLAYVALUE"
         _SetValue(Arg2, Arg1, 0)
         hmg_SetWindowText(GetControlHandle(Arg2, Arg1), Arg4)
         EXIT
      CASE "ROW"
         _SetControlRow(Arg2, Arg1, Arg4)
         EXIT
      CASE "COL"
         _SetControlCol(Arg2, Arg1, Arg4)
         EXIT
      CASE "LISTWIDTH"
         _SetGetDropDownWidth(Arg2, Arg1, Arg4)
         EXIT
      CASE "WIDTH"
         _SetControlWidth(Arg2, Arg1, Arg4)
         EXIT
      CASE "HEIGHT"
         _SetControlHeight(Arg2, Arg1, Arg4)
         EXIT
      CASE "VISIBLE"
         iif(Arg4, _ShowControl(Arg2, Arg1), _HideControl(Arg2, Arg1))
         EXIT
      CASE "ENABLED"
         iif(Arg4, _EnableControl(Arg2, Arg1), _DisableControl(Arg2, Arg1))
         EXIT
      CASE "CHECKED"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_CHECKLABEL
            ix := GetControlHandle(Arg2, Arg1)
            IF Arg4 == NIL
               Arg4 := !GetChkLabel(ix)
            ENDIF
            SetChkLabel(ix, Arg4)
         ELSE
            iif(Arg4, _CheckMenuItem(Arg2, Arg1), _UnCheckMenuItem(Arg2, Arg1))
         ENDIF
         EXIT
      CASE "BLINK"
         IF "LABEL" $ GetControlTypeAsString(Arg2, Arg1) .AND. (_IsControlVisible(Arg2, Arg1) .OR. !Arg4)
            ix := GetControlIndex(Arg2, Arg1)
            IF _HMG_aControlMiscData1[ix][2]
               iif(Arg4, _EnableControl("BlinkTimer" + hb_ntos(ix), Arg1), _DisableControl("BlinkTimer" + hb_ntos(ix), Arg1))
               IF !_HMG_aControlMiscData1[ix][3]
                  _ShowControl(Arg2, Arg1)
               ENDIF
            ELSEIF Arg4
               _HMG_aControlMiscData1[ix][2] := Arg4
               _DefineTimer("BlinkTimer" + hb_ntos(ix), Arg1, 500, {||_HMG_aControlMiscData1[ix][3] := !_HMG_aControlMiscData1[ix][3], ;
                  iif(_HMG_aControlMiscData1[ix][3], _ShowControl(Arg2, Arg1), _HideControl(Arg2, Arg1))})
            ENDIF
         ENDIF
         EXIT
      CASE "RANGEMIN"
         _SetRangeMin(Arg2, Arg1, Arg4)
         EXIT
      CASE "RANGEMAX"
         _SetRangeMax(Arg2, Arg1, Arg4)
         EXIT
      CASE "REPEAT"
         IF Arg4
            _SetPlayerRepeatOn(Arg2, Arg1)
         ELSE
            _SetPlayerRepeatOff(Arg2, Arg1)
         ENDIF
         EXIT
      CASE "SPEED"
         _SetPlayerSpeed(Arg2, Arg1, Arg4)
         EXIT
      CASE "VOLUME"
         _SetPlayerVolume(Arg2, Arg1, Arg4)
         EXIT
      CASE "ZOOM"
         _SetPlayerZoom(Arg2, Arg1, Arg4)
         EXIT
      CASE "POSITION"
         IF Arg4 == 0
            _SetPlayerPositionHome(Arg2, Arg1)
         ELSEIF Arg4 == 1
            _SetPlayerPositionEnd(Arg2, Arg1)
         ENDIF
         EXIT
      CASE "CARETPOS"
         _SetCaretPos(Arg2, Arg1, Arg4)
         EXIT
      CASE "BACKCOLOR"
      CASE "GRADIENTOVER"
      CASE "BACKGROUNDCOLOR"
         _SetBackColor(Arg2, Arg1, Arg4)
         EXIT
      CASE "FONTCOLOR"
      CASE "FORECOLOR"
         _SetFontColor(Arg2, Arg1, Arg4)
         EXIT
      CASE "CELLNAVIGATION"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_GRID
            ix := GetControlIndex(Arg2, Arg1)
            _HMG_aControlFontColor[ix] := Arg4
            _HMG_aControlMiscData1[ix][1] := hmg_LISTVIEW_GETFOCUSEDITEM(_HMG_aControlHandles[ix])
            _RedrawControl(ix)
         ENDIF
         EXIT
      CASE "HTFORECOLOR"
         _SetGetTabHTColors(Arg2, Arg1, 6, Arg4)
         EXIT
      CASE "HTINACTIVECOLOR"
         _SetGetTabHTColors(Arg2, Arg1, 7, Arg4)
         EXIT
      CASE "ADDRESS"
         _SetAddress(Arg2, Arg1, Arg4)
         EXIT
      CASE "TABSTOP"
         SetTabStop(GetControlHandle(Arg2, Arg1), Arg4)
         EXIT
#ifdef _DBFBROWSE_
      CASE "INPUTITEMS"
         _SetBrowseInputItems(Arg2, Arg1, Arg4)
         EXIT
      CASE "DISPLAYITEMS"
         _SetBrowseDisplayItems(Arg2, Arg1, Arg4)
         EXIT
#endif
      CASE "CHECKBOXENABLED"
         IF Arg4
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_CHECKBOXES, NIL)
         ELSE
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), NIL, LVS_EX_CHECKBOXES)
         ENDIF
         _HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)][18] := Arg4
         EXIT
// TODO: implementado com IF em OTHERWISE / IMPLEMENTAR com CASE
//       CASE "DOUBLEBUFFER" $ Arg3
//
//          IF Arg4
//             hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_DOUBLEBUFFER, NIL)
//          ELSE
//             hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), NIL, LVS_EX_DOUBLEBUFFER)
//          ENDIF
      CASE "HEADERDRAGDROP"
         IF Arg4
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_HEADERDRAGDROP, NIL)
         ELSE
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), NIL, LVS_EX_HEADERDRAGDROP)
         ENDIF
         EXIT
      CASE "INFOTIP"
         IF Arg4
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_INFOTIP, NIL)
         ELSE
            hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), NIL, LVS_EX_INFOTIP)
         ENDIF
         EXIT
      CASE "READONLY"
      CASE "DISABLEEDIT"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_RADIOGROUP
            _SetRadioGroupReadOnly(Arg2, Arg1, Arg4)
         ELSE
            _SetTextEditReadOnly(Arg2, Arg1, Arg4)
         ENDIF
         EXIT
      CASE "SPACING"  // GF 04/08/19
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_RADIOGROUP
            _SetRadioGroupSpacing(Arg2, Arg1, Arg4)
         ENDIF
         EXIT
      CASE "OPTIONS"  // GF 04/10/19
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_RADIOGROUP
            _SetRadioGroupOptions(Arg2, Arg1, Arg4)
         ENDIF
         EXIT
      CASE "ITEMCOUNT"
         hmg_ListView_SetItemCount(GetControlHandle(Arg2, Arg1), Arg4)
         _HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)][6] := Arg4
         EXIT
      CASE "COLUMNWIDTHLIMITS"  // GF 16/07/18
         _SetGridColumnWidthLimits(Arg2, Arg1, Arg4)
         EXIT
      CASE "INDENT"
         TreeView_SetIndent(GetControlHandle(Arg2, Arg1), Arg4)
         EXIT
      CASE "LINECOLOR"
         TreeView_SetLineColor(GetControlHandle(Arg2, Arg1), Arg4)
         EXIT
      CASE "ITEMHEIGHT"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_COMBO
            hmg_ComboSetItemHeight(GetControlHandle(Arg2, Arg1), Arg4)
         ELSE
            TreeView_SetItemHeight(GetControlHandle(Arg2, Arg1), Arg4)
         ENDIF
         EXIT
      CASE "VALIDMESSAGE"
      CASE "EDITABLE"
         _SetGetSpacingProperty(Arg2, Arg1, Arg4)
         EXIT
      CASE "RICHVALUE" // Kevin Carmody <i@kevincarmody.com> 2007.04.23
         _SetGetRichValue(Arg2, Arg1, Arg4)
         EXIT
      CASE "AUTOFONT" // Kevin Carmody <i@kevincarmody.com> 2007.04.23
         _SetGetAutoFont(Arg2, Arg1, Arg4)
         EXIT
      CASE "FIRSTDAYOFWEEK"  // GF 22/03/22
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_MONTHCAL
            SetFirstDayOfWeek(Arg2, Arg1, Arg4)
         ENDIF
         EXIT
      OTHERWISE
         // TODO: veja nota acima
         IF "DOUBLEBUFFER" $ Arg3
            IF Arg4
               hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_DOUBLEBUFFER, NIL)
            ELSE
               hmg_ListView_ChangeExtendedStyle(GetControlHandle(Arg2, Arg1), NIL, LVS_EX_DOUBLEBUFFER)
            ENDIF
         ENDIF
         //
         IF !("GROUP" $ Arg3)
            MsgMiniGuiError("Control: unrecognized property '" + Arg3 + "'.")
         ENDIF
      ENDSWITCH
      EXIT

   CASE 5 // PCount() == 5 (CONTROL WITH ARGUMENT OR TOOLBAR BUTTON OR SPLITBOX CHILD CONTROL WITHOUT ARGUMENT)

      IF Upper(Arg2) != "SPLITBOX" .AND. GetControlType(Arg2, Arg1) != CONTROL_TYPE_TOOLBAR
         VerifyControlDefined(Arg1, Arg2)
      ENDIF

      Arg3 := Upper(Arg3)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomPropertyProcedure

         IF Arg3 == cProc[1]

            cMacro := cProc[2]
            &cMacro(Arg1, Arg2, Arg3, Arg4, Arg5)
            IF _HMG_UserComponentProcess
               RETURN
            ENDIF

         ENDIF

      NEXT
#endif
      IF Upper(Arg2) == "SPLITBOX"

         IsControlInsideSplitBox(Arg1, Arg3)
         SetProperty(Arg1, Arg3, Arg4, Arg5)

      ELSE

         SWITCH Arg3
         CASE "CAPTION"
         CASE "HEADER"
         CASE "COLUMNHEADER"
            _SetMultiCaption(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "IMAGE"
         CASE "HEADERIMAGE"
            _SetMultiImage(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "TOOLTIP"
            _SetTooltip(Arg2, Arg1, Arg5, Arg4)
            EXIT
         CASE "ITEM"
            _SetItem(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "ICON"
            _SetStatusIcon(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "WIDTH"
            _SetStatusWidth(Arg1, Arg4, Arg5)
            EXIT
         CASE "COLUMNWIDTH" //(JK) HMG 1.0 Experimental Build 6
            _SetColumnWidth(Arg2, Arg1, Arg4, Arg5)
            EXIT
#ifdef _HMG_COMPAT_
         CASE "COLUMNONHEADCLICK"
            _SetGetColumnHeadClick(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "COLUMNDISPLAYPOSITION"
            _SetColumnDisplayPosition(Arg2, Arg1, Arg4, Arg5)
            EXIT
         CASE "COLUMNCONTROL"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_CONTROL_, Arg4, Arg5)
            EXIT
         CASE "COLUMNDYNAMICFORECOLOR"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_DYNAMICFORECOLOR_, Arg4, Arg5)
            EXIT
         CASE "COLUMNDYNAMICBACKCOLOR"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_DYNAMICBACKCOLOR_, Arg4, Arg5)
            EXIT
         CASE "COLUMNJUSTIFY"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_JUSTIFY_, Arg4, Arg5)
            EXIT
         CASE "COLUMNVALID"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_VALID_, Arg4, Arg5)
            EXIT
         CASE "COLUMNWHEN"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_WHEN_, Arg4, Arg5)
            EXIT
         CASE "COLUMNVALIDMESSAGE"
            _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_VALIDMESSAGE_, Arg4, Arg5)
            EXIT
#endif
         CASE "ENABLED" // To ENABLE / DISABLE Radiobuttons and Tab pages
            iif(Arg5, _EnableControl(Arg2, Arg1, Arg4), _DisableControl(Arg2, Arg1, Arg4))
            EXIT
         CASE "RICHVALUE" // Kevin Carmody <i@kevincarmody.com> 2007.04.23
            _SetGetRichValue(Arg2, Arg1, Arg5, Arg4)
            EXIT
         CASE "CHECKBOXITEM"
            IF "GRID" $ GetControlTypeAsString(Arg2, Arg1) // Eduardo Fernandes 2009/JUN/17
               _SetGetCheckBoxItemState(Arg2, Arg1, Arg4, Arg5)
            ELSE
               _SetGetChkListItemState(Arg2, Arg1, Arg4, Arg5)
            ENDIF
            EXIT
         CASE "CARGO" // GF 16/02/2019
            IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_TREE
               TreeNodeItemCargo(Arg2, Arg1, Arg4, Arg5)
            ENDIF
            EXIT
         OTHERWISE  // If Property Not Matched Look For ToolBar Button
            IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_TOOLBAR
               IF GetControlHandle(Arg2, Arg1) != GetControlContainerHandle(Arg3, Arg1)
                  MsgMiniGuiError("Control Does Not Belong To Container.")
               ENDIF
               SetProperty(Arg1, Arg3, Arg4, Arg5)
            ELSE
               IF !("GROUP" $ Arg3)
                  MsgMiniGuiError("Control: unrecognized property '" + Arg3 + "'.")
               ENDIF
            ENDIF
         ENDSWITCH

      ENDIF
      EXIT

   CASE 6 // PCount() == 6 (TAB CHILD CONTROL OR SPLITBOX CHILD WITH ARGUMENT OR SPLITCHILD TOOLBAR BUTTON)

      IF Upper(Arg2) == "SPLITBOX"

         IF _IsControlSplitBoxed(Arg3, Arg1)
            SetProperty(Arg1, Arg3, Arg4, Arg5, Arg6)
         ELSE
            IF _IsControlDefined(Arg4, Arg1)
               IsControlInsideSplitBox(Arg1, Arg4)
               SetProperty(Arg1, Arg3, Arg4, Arg5, Arg6)
            ELSE
               MsgMiniGuiError("Control Does Not Belong To Container.")
            ENDIF
         ENDIF

      ELSE

         IF !hb_isNumeric(Arg3)
            Arg3 := Upper(Arg3)
            IF Arg3 == "CELL"
               VerifyControlDefined(Arg1, Arg2)
               IF Len(_HMG_aControlBkColor[GetControlIndex(Arg2, Arg1)]) > 0 .AND. Arg5 == 1
                  hmg_SetImageListViewItems(GetControlHandle(Arg2, Arg1), Arg4, Arg6)
               ELSE
                  _SetGridCellValue(Arg2, Arg1, Arg4, Arg5, Arg6)
               ENDIF
            ELSEIF Arg3 == "HEADERIMAGE"    // Grid & Browse
               _SetMultiImage(Arg2, Arg1, Arg4, Arg5, Arg6)
            ENDIF
         ELSE
            IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
            SetProperty(Arg1, Arg4, Arg5, Arg6)
         ENDIF

      ENDIF
      EXIT

   CASE 7 // PCount() == 7 (TAB CHILD CONTROL WITH ARGUMENT OR SPLITBOX CHILD WITH 2 ARGUMENTS)

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         SetProperty(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7)
      ELSE
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         SetProperty(Arg1, Arg4, Arg5, Arg6, Arg7)
      ENDIF
      EXIT

   CASE 8 // PCount() == 8 (TAB CHILD CONTROL WITH 2 ARGUMENTS OR SPLITBOX CHILD WITH 3 ARGUMENT)

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         SetProperty(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)
      ELSE
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         SetProperty(Arg1, Arg4, Arg5, Arg6, Arg7, Arg8)
      ENDIF

   ENDSWITCH

#ifdef _HMG_COMPAT_
   IF hb_IsChar(Arg1) .AND. hb_IsChar(Arg2) .AND. "GRID" $ GetControlTypeAsString(Arg2, Arg1) .AND. hb_IsChar(Arg3) .AND. "GROUP" $ Arg3

      SWITCH Arg3
      CASE "GROUPENABLED"
         IF _HMG_IsXPorLater .AND. _HMG_IsThemed
            hmg_ListView_EnableGroupView(GetControlHandle(Arg2, Arg1), Arg4)
         ENDIF
         EXIT
      CASE "GROUPINFO"
         ASize(Arg5, 5)
         hmg_ListView_GroupSetInfo(GetControlHandle(Arg2, Arg1), Arg4, Arg5[1], Arg5[2], Arg5[3], Arg5[4], Arg5[5])
         EXIT
      CASE "GROUPITEMID"
         hmg_ListView_GroupItemSetID(GetControlHandle(Arg2, Arg1), (Arg4 - 1), Arg5)
         EXIT
      CASE "GROUPCHECKBOXALLITEMS"
         GroupCheckBoxAllItems(Arg2, Arg1, Arg4, Arg5)
         EXIT
      OTHERWISE
         MsgMiniGuiError("Grid Group: unrecognized property '" + Arg3 + "'.")
      ENDSWITCH

   ENDIF
#endif

RETURN

FUNCTION GetProperty(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

   LOCAL RetVal
   LOCAL ix
#if defined(_BT_) .OR. defined(_HMG_COMPAT_)
   LOCAL xData
#endif
#ifdef _USERINIT_
   LOCAL cMacro
   LOCAL cProc
#endif
#ifdef _HMG_COMPAT_
   LOCAL cHeader
   LOCAL nAlignHeader
   LOCAL cFooter
   LOCAL nAlingFooter
   LOCAL nState

   IF _RichEditBox_GetProperty(@xDATA, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)
      RETURN xData
   ENDIF
#else
   HB_SYMBOL_UNUSED(Arg8)
#endif

#ifdef _BT_
   IF _ProgressWheel_GetProperty(@xDATA, Arg1, Arg2, Arg3)
      RETURN xData
   ENDIF
#endif

   SWITCH PCount()

   CASE 2 // PCount() == 2 (WINDOW)

      IF !_IsWindowDefined(Arg1)
         MsgMiniGuiError("Window: " + Arg1 + " is not defined.")
      ENDIF

      Arg2 := Upper(Arg2)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomPropertyProcedure
         IF Arg2 == cProc[1]
            cMacro := cProc[3]
            RetVal := &cMacro(Arg1, Arg2)
            IF _HMG_UserComponentProcess
               RETURN RetVal
            ENDIF
         ENDIF
      NEXT
#endif

      SWITCH Arg2
      CASE "TITLE"
         RetVal := hmg_GetWindowText(GetFormHandle(Arg1))
         EXIT
      CASE "GRIPPERTEXT"
         IF GetWindowType(Arg1) == "X"
            RetVal := _HMG_aFormMiscData1[GetFormIndex(Arg1)][5]
         ENDIF
         EXIT
      CASE "FOCUSEDCONTROL"
         RetVal := _GetFocusedControl(Arg1)
         EXIT
      CASE "NAME"
         RetVal := GetFormName(Arg1)
         EXIT
      CASE "HANDLE"
         RetVal := GetFormHandle(Arg1)
         EXIT
      CASE "INDEX"
         RetVal := GetFormIndex(Arg1)
         EXIT
      CASE "TYPE"
         RetVal := GetWindowType(Arg1)
         EXIT
      CASE "HEIGHT"
         RetVal := GetWindowHeight(GetFormHandle(Arg1))
         EXIT
      CASE "WIDTH"
         RetVal := GetWindowWidth(GetFormHandle(Arg1))
         EXIT
      CASE "COL"
         RetVal := GetWindowCol(GetFormHandle(Arg1))
#ifdef _PANEL_
         IF GetWindowType(Arg1) == "P"
            RetVal -= GetBorderWidth()
         ENDIF
#endif
         EXIT
      CASE "ROW"
         RetVal := GetWindowRow(GetFormHandle(Arg1))
#ifdef _PANEL_
         IF GetWindowType(Arg1) == "P"
            RetVal -= GetTitleHeight() + GetBorderHeight()
         ENDIF
#endif
         EXIT
      CASE "TITLEBAR"
         RetVal := hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_CAPTION)
         EXIT
      CASE "SYSMENU"
         RetVal := hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_SYSMENU)
         EXIT
      CASE "SIZABLE"
         RetVal := IsWindowSized(GetFormHandle(Arg1))
         EXIT
      CASE "MAXBUTTON"
         RetVal := hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_MAXIMIZEBOX)
         EXIT
      CASE "MINBUTTON"
         RetVal := hmg_IsWindowHasStyle(GetFormHandle(Arg1), WS_MINIMIZEBOX)
         EXIT
      CASE "CLOSABLE"
         RetVal := hmg_xGetMenuEnabledState(hmg_GetSystemMenu(GetFormHandle(Arg1)), SC_CLOSE)
         EXIT
      CASE "VISIBLE"
         RetVal := hmg_IsWindowVisible(GetFormHandle(Arg1))
         EXIT
      CASE "ENABLED"
         RetVal := hmg_IsWindowEnabled(GetFormHandle(Arg1))
         EXIT
      CASE "TOPMOST"
         RetVal := hmg_IsWindowHasExStyle(GetFormHandle(Arg1), WS_EX_TOPMOST)
         EXIT
      CASE "HELPBUTTON"
         RetVal := hmg_IsWindowHasExStyle(GetFormHandle(Arg1), WS_EX_CONTEXTHELP)
         EXIT
      CASE "NOTIFYICON"
         RetVal := _GetNotifyIconName(Arg1)
         EXIT
      CASE "NOTIFYTOOLTIP"
         RetVal := _GetNotifyIconTooltip(Arg1)
         EXIT
      CASE "BACKCOLOR"
         RetVal := _HMG_aFormBkColor[GetFormIndex(Arg1)]
         EXIT
      CASE "CARGO"
         RetVal := _WindowCargo(Arg1)
         EXIT
      CASE "CURSOR"
         RetVal := _HMG_aFormMiscData1[GetFormIndex(Arg1)][2]
         EXIT
#ifdef _OBJECT_
      CASE "OBJECT"
         RetVal := _WindowObj(Arg1)
         EXIT
#endif
      CASE "MINWIDTH" // Grigory Filatov HMG 1.4 Ext Build 43
         RetVal := _SetGetMinMaxInfo(Arg1, 5)
         EXIT
      CASE "MINHEIGHT"
         RetVal := _SetGetMinMaxInfo(Arg1, 6)
         EXIT
      CASE "MAXWIDTH"
         RetVal := _SetGetMinMaxInfo(Arg1, 7)
         EXIT
      CASE "MAXHEIGHT"
         RetVal := _SetGetMinMaxInfo(Arg1, 8)
         EXIT
      CASE "ONINIT"
      CASE "ONRELEASE"
      CASE "ONINTERACTIVECLOSE"
      CASE "ONGOTFOCUS"
      CASE "ONLOSTFOCUS"
      CASE "ONNOTIFYCLICK"
      CASE "ONMOUSECLICK"
      CASE "ONMOUSEDRAG"
      CASE "ONMOUSEMOVE"
      CASE "ONMOVE"
      CASE "ONSIZE"
      CASE "ONMAXIMIZE"
      CASE "ONMINIMIZE"
      CASE "ONPAINT"
      CASE "ONRESTORE"
      CASE "ONDROPFILES" // GF 07/10/19
         RetVal := _GetFormAction(Arg1, Arg2)
      ENDSWITCH
      
      EXIT

   CASE 3 // PCount() == 3 (CONTROL)

      Arg3 := Upper(Arg3)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomPropertyProcedure
         IF Arg3 == cProc[1]
            cMacro := cProc[3]
            RetVal := &cMacro(Arg1, Arg2, Arg3)
            IF _HMG_UserComponentProcess
               RETURN RetVal
            ENDIF
         ENDIF
      NEXT
#endif

      IF (Upper(Arg2) == "VSCROLLBAR" .OR. Upper(Arg2) == "HSCROLLBAR")
         IF !_IsWindowDefined(Arg1)
            MsgMiniGuiError("Window: " + Arg1 + " is not defined.")
         ENDIF
      ELSE
         VerifyControlDefined(Arg1, Arg2)
      ENDIF

      SWITCH Arg3
      CASE "CUEBANNER" /* P.Ch. 16.10. */
         IF IsVistaOrLater()
            IF "TEXT" $ GetControlTypeAsString(Arg2, Arg1)
               RetVal := hmg_GetCueBannerText(GetControlHandle(Arg2, Arg1))
            ELSEIF GetControlType(Arg2, Arg1) == CONTROL_TYPE_SPINNER
               RetVal := hmg_GetCueBannerText(GetControlHandle(Arg2, Arg1)[1])
            ELSEIF GetControlType(Arg2, Arg1) == CONTROL_TYPE_COMBO
               ix := GetControlIndex(Arg2, Arg1)
               IF _HMG_aControlMiscData1[ix][2]
                  RetVal := hmg_GetCueBannerText(_HMG_aControlRangeMin[ix])
               ELSE
                  RetVal := hmg_GetCueBannerText(GetControlHandle(Arg2, Arg1))
               ENDIF
            ENDIF
         ENDIF
         EXIT
      CASE "ALIGNMENT"  // GF 12/01/17
         ix := GetControlHandle(Arg2, Arg1)
         IF hmg_IsWindowHasStyle(ix, ES_CENTER)
            RetVal := "CENTER"
         ELSEIF hmg_IsWindowHasStyle(ix, ES_RIGHT)
            RetVal := "RIGHT"
         ELSEIF hmg_IsWindowHasStyle(ix, SS_CENTERIMAGE)
            RetVal := "VCENTER"
         ELSE
            RetVal := "LEFT"
         ENDIF
         EXIT
      CASE "CASECONVERT"  // GF 04/04/20
         ix := GetControlHandle(Arg2, Arg1)
         IF hmg_IsWindowHasStyle(ix, ES_UPPERCASE)
            RetVal := "UPPER"
         ELSEIF hmg_IsWindowHasStyle(ix, ES_LOWERCASE)
            RetVal := "LOWER"
         ELSE
            RetVal := "NONE"
         ENDIF
         EXIT
      CASE "TRANSPARENT"  // GF 02/04/20
         ix := GetControlHandle(Arg2, Arg1)
         IF hb_IsArray(ix)  // GF 30/06/20
            RetVal := _HMG_aControlInputMask[GetControlIndex(Arg2, Arg1)]
         ELSE
            RetVal := hmg_IsWindowHasExStyle(ix, WS_EX_TRANSPARENT)
         ENDIF
         EXIT
      CASE "VALUE"
      CASE "GRADIENTFILL"
      CASE "INTERVAL"
         RetVal := _GetValue(Arg2, Arg1)
         EXIT
      CASE "FORMATSTRING"
         RetVal := _SetGetDatePickerDateFormat(Arg2, Arg1)
         EXIT
      CASE "CARGO"  // (GF) HMG 1.7 Exp. Build 76
         RetVal := _ControlCargo(Arg2, Arg1)
         EXIT
#ifdef _TSBROWSE_
      CASE "OBJECT"
         IF _HMG_lOOPEnabled
#ifdef _OBJECT_
            RetVal := _ControlObj(Arg2, Arg1)
            IF hb_IsObject(RetVal) .AND. _HMG_aControlType[RetVal:Index] == CONTROL_TYPE_TBROWSE
               RetVal := _HMG_aControlIds[RetVal:Index]
            ENDIF
#endif
         ELSEIF (ix := GetControlIndex(Arg2, Arg1)) > 0
            IF _HMG_aControlType[ix] == CONTROL_TYPE_TBROWSE
               RetVal := _HMG_aControlIds[ix]
            ENDIF
         ENDIF
         EXIT
#endif
#ifdef _USERINIT_
      CASE "XOBJECT"
         RetVal := GetActiveXObject(Arg1, Arg2)
         EXIT
#endif
      CASE "NAME"
         RetVal := GetControlName(Arg2, Arg1)
         EXIT
      CASE "HANDLE"
         RetVal := GetControlHandle(Arg2, Arg1)
         EXIT
      CASE "INDEX"
         RetVal := GetControlIndex(Arg2, Arg1)
         EXIT
      CASE "TYPE"
         RetVal := GetUserControlType(Arg2, Arg1)
         EXIT
#ifdef _DBFBROWSE_
      CASE "ALLOWEDIT"
         RetVal := _GetBrowseAllowEdit(Arg2, Arg1)
         EXIT
      CASE "ALLOWAPPEND"
         RetVal := _GetBrowseAllowAppend(Arg2, Arg1)
         EXIT
      CASE "ALLOWDELETE"
         RetVal := _GetBrowseAllowDelete(Arg2, Arg1)
         EXIT
      CASE "INPUTITEMS"
         RetVal := _GetBrowseInputItems(Arg2, Arg1)
         EXIT
      CASE "DISPLAYITEMS"
         RetVal := _GetBrowseDisplayItems(Arg2, Arg1)
         EXIT
#endif
      CASE "PICTURE"
      CASE "ICON"
      CASE "ONCE"
      CASE "ONLISTCLOSE"
      CASE "ONCLOSEUP"
      CASE "INCREMENT"
         RetVal := _GetPicture(Arg2, Arg1)
         EXIT
      CASE "HBITMAP"
         RetVal := _SetGetImageHBitmap(Arg2, Arg1)
         EXIT
      CASE "TOOLTIP"
         RetVal := _GetTooltip(Arg2, Arg1)
         EXIT
      CASE "FONTNAME"
         RetVal := _GetFontName(Arg2, Arg1)
         EXIT
      CASE "FONTSIZE"
         RetVal := _GetFontSize(Arg2, Arg1)
         EXIT
      CASE "FONTBOLD"
         RetVal := _GetFontBold(Arg2, Arg1)
         EXIT
      CASE "FONTITALIC"
         RetVal := _GetFontItalic(Arg2, Arg1)
         EXIT
      CASE "FONTUNDERLINE"
         RetVal := _GetFontUnderline(Arg2, Arg1)
         EXIT
      CASE "FONTSTRIKEOUT"
         RetVal := _GetFontStrikeout(Arg2, Arg1)
         EXIT
      CASE "CAPTION"
      CASE "OPTIONS"  // GF 04/10/19
         RetVal := _GetCaption(Arg2, Arg1)
         EXIT
      CASE "ACTION"
      CASE "ONCLICK"
      CASE "ONGOTFOCUS"
      CASE "ONLOSTFOCUS"
      CASE "ONCHANGE"
      CASE "ONDBLCLICK"
      CASE "ONDISPLAYCHANGE"
      CASE "ONENTER" // GF 10/28/10
         RetVal := _GetControlAction(Arg2, Arg1, Arg3)
         EXIT
      CASE "ONLISTDISPLAY"
      CASE "ONDROPDOWN" // GF 07/16/19
         ix := GetControlIndex(Arg2, Arg1)
         IF _HMG_aControltype[ix] == CONTROL_TYPE_COMBO
            RetVal := _HMG_aControlInputMask[ix]
         ENDIF
         EXIT
      CASE "DISPLAYVALUE"
         //(JK) HMG 1.0 Experimental Build 14
         ix := GetControlIndex(Arg2, Arg1)
         IF _HMG_aControltype[ix] == CONTROL_TYPE_GETBOX
            RetVal := hmg_GetWindowText(GetControlHandle(Arg2, Arg1))
         ELSEIF _HMG_aControltype[ix] == CONTROL_TYPE_COMBO
            IF _HMG_aControlMiscData1[ix][1] == 1
               IF Empty(_hmg_aControlRangemin[ix])
                  RetVal := _GetComboItemValue(Arg2, Arg1, ComboGetCursel(_HMG_aControlHandles[ix]))
               ELSE
                  RetVal := hmg_GetWindowText(GetControlHandle(Arg2, Arg1))
               ENDIF
            ELSE
               IF _HMG_aControlMiscData1[ix][2] .AND. iif(Empty(_HMG_aControlCaption[ix]), _GetValue(NIL, NIL, ix) > 0, .F.) // GF 05/05/17
                  RetVal := _GetComboItemValue(Arg2, Arg1, ComboGetCursel(_HMG_aControlHandles[ix]))
               ELSE
                  RetVal := hmg_GetWindowText(iif(Empty(_hmg_aControlRangemin[ix]), GetControlHandle(Arg2, Arg1), _hmg_aControlRangemin[ix]))
               ENDIF
            ENDIF
         ENDIF
         EXIT
      CASE "ROW"
         RetVal := _GetControlRow(Arg2, Arg1)
         EXIT
      CASE "COL"
         RetVal := _GetControlCol(Arg2, Arg1)
         EXIT
      CASE "WIDTH"
         RetVal := _GetControlWidth(Arg2, Arg1)
         EXIT
      CASE "LISTWIDTH"
         RetVal := _SetGetDropDownWidth(Arg2, Arg1)
         EXIT
      CASE "HEIGHT"
         RetVal := _GetControlHeight(Arg2, Arg1)
         EXIT
      CASE "VISIBLE"
         RetVal := _IsControlVisible(Arg2, Arg1)
         EXIT
      CASE "ENABLED"
         RetVal := _IsControlEnabled(Arg2, Arg1)
         EXIT
      CASE "CHECKED"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_CHECKLABEL
            RetVal := GetChkLabel(GetControlHandle(Arg2, Arg1))
         ELSEIF GetControlType(Arg2, Arg1) == CONTROL_TYPE_DATEPICK
            RetVal := hmg_dtp_IsChecked(GetControlHandle(Arg2, Arg1))
         ELSE
            RetVal := _IsMenuItemChecked(Arg2, Arg1)
         ENDIF
         EXIT
      CASE "ITEMCOUNT"
         RetVal := _GetItemCount(Arg2, Arg1)
         EXIT
      CASE "RANGEMIN"
         RetVal := _GetRangeMin(Arg2, Arg1)
         EXIT
      CASE "RANGEMAX"
         RetVal := _GetRangeMax(Arg2, Arg1)
         EXIT
      CASE "LENGTH"
         RetVal := _GetPlayerLength(Arg2, Arg1)
         EXIT
      CASE "POSITION"
         RetVal := _GetPlayerPosition(Arg2, Arg1)
         EXIT
      CASE "VOLUME"
         RetVal := _GetPlayerVolume(Arg2, Arg1)
         EXIT
      CASE "CARETPOS"
         RetVal := _GetCaretPos(Arg2, Arg1)
         EXIT
      CASE "BACKCOLOR"
      CASE "GRADIENTOVER"
         RetVal := _GetBackColor(Arg2, Arg1)
         EXIT
      CASE "FONTCOLOR"
      CASE "FORECOLOR"
         RetVal := _GetFontColor(Arg2, Arg1)
         EXIT
      CASE "CELLNAVIGATION"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_GRID
            ix := GetControlIndex(Arg2, Arg1)
            RetVal := _HMG_aControlFontColor[ix]
         ENDIF
         EXIT
      CASE "HTFORECOLOR"
         RetVal := _SetGetTabHTColors(Arg2, Arg1, 6)
         EXIT
      CASE "HTINACTIVECOLOR"
         RetVal := _SetGetTabHTColors(Arg2, Arg1, 7)
         EXIT
      CASE "ADDRESS"
         RetVal := _GetAddress(Arg2, Arg1)
         EXIT
      CASE "TABSTOP"
         ix := GetControlHandle(Arg2, Arg1)
         RetVal := IsTabStop(iif(hb_IsArray(ix), ix[1], ix))
         EXIT
      CASE "CHECKBOXENABLED"
         RetVal := hmg_ListView_GetExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_CHECKBOXES)
         EXIT
//       CASE "DOUBLEBUFFER" $ Arg3
//
//          RetVal := hmg_ListView_GetExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_DOUBLEBUFFER)
      CASE "HEADERDRAGDROP"
         RetVal := hmg_ListView_GetExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_HEADERDRAGDROP)
         EXIT
      CASE "INFOTIP"
         RetVal := hmg_ListView_GetExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_INFOTIP)
         EXIT
#ifdef _HMG_COMPAT_
      CASE "COLUMNCOUNT"
         RetVal := hmg_ListView_GetColumnCount(GetControlHandle(Arg2, Arg1))
         EXIT
      CASE "ROWSPERPAGE"
         RetVal := hmg_ListViewGetCountPerPage(GetControlHandle(Arg2, Arg1))
         EXIT
#endif
      CASE "READONLY"
      CASE "DISABLEEDIT"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_RADIOGROUP
            RetVal := _GetRadioGroupReadOnly(Arg2, Arg1)
         ELSE
            ix := GetControlHandle(Arg2, Arg1)
            RetVal := hmg_IsWindowHasStyle(iif(hb_IsArray(ix), ix[1], ix), ES_READONLY)
         ENDIF
         EXIT
      CASE "WORKAREA"
      CASE "SPACING" // GF 04/10/19
         RetVal := _HMG_aControlSpacing[GetControlIndex(Arg2, Arg1)]
         EXIT
      CASE "HORIZONTAL"  // 26/04/2022
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_RADIOGROUP
            RetVal := _HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)]
         ELSEIF GetControlType(Arg2, Arg1) == CONTROL_TYPE_SPINNER
            ix := GetControlHandle(Arg2, Arg1)
            RetVal := hmg_IsWindowHasStyle(ix[2], UDS_HORZ)
         ENDIF
         EXIT
      CASE "WRAP"  // 26/04/2022
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_SPINNER
            ix := GetControlHandle(Arg2, Arg1)
            RetVal := hmg_IsWindowHasStyle(ix[2], UDS_WRAP)
         ENDIF
         EXIT
      CASE "COLUMNWIDTHLIMITS"  // 15/04/2022
         RetVal := _HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)][25]
         EXIT
      CASE "INDENT"
         RetVal := TreeView_GetIndent(GetControlHandle(Arg2, Arg1))
         EXIT
      CASE "LINECOLOR"
         RetVal := TreeView_GetLineColor(GetControlHandle(Arg2, Arg1))
         EXIT
      CASE "ITEMHEIGHT"
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_COMBO
            RetVal := GetWindowHeight(GetControlHandle(Arg2, Arg1)) - 6
         ELSE
            RetVal := TreeView_GetItemHeight(GetControlHandle(Arg2, Arg1))
         ENDIF
         EXIT
      CASE "VALIDMESSAGE"
      CASE "EDITABLE"
         RetVal := _SetGetSpacingProperty(Arg2, Arg1)
         EXIT
      CASE "RICHVALUE" // Kevin Carmody <i@kevincarmody.com> 2007.04.10
         RetVal := _SetGetRichValue(Arg2, Arg1)
         EXIT
      CASE "AUTOFONT"  // Kevin Carmody <i@kevincarmody.com> 2007.04.23
         RetVal := _SetGetAutoFont(Arg2, Arg1)
         EXIT
      CASE "FIRSTDAYOFWEEK"  // GF 22/03/22
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_MONTHCAL
            RetVal := GetFirstDayOfWeek(Arg2, Arg1)
         ENDIF
         EXIT
      OTHERWISE
         IF "DOUBLEBUFFER" $ Arg3
            RetVal := hmg_ListView_GetExtendedStyle(GetControlHandle(Arg2, Arg1), LVS_EX_DOUBLEBUFFER)
         ENDIF
      ENDSWITCH

      EXIT

   CASE 4 // PCount() == 4 (CONTROL WITH ARGUMENT OR TOOLBAR BUTTON OR (JK) HMG 1.0 Experimental Buid 6 GRID/BROWSE COLUMN - ColumnWidth OR SPLITBOX CHILD WITHOUT ARGUMENT)

      IF Upper(Arg2) == "SPLITBOX"

         IsControlInsideSplitBox(Arg1, Arg3)
         RetVal := GetProperty(Arg1, Arg3, Arg4)

      ELSE

         IF GetControlType(Arg2, Arg1) != CONTROL_TYPE_TOOLBAR
            VerifyControlDefined(Arg1, Arg2)
         ENDIF

         Arg3 := Upper(Arg3)

         SWITCH Arg3
         CASE "ITEM"
            RetVal := _GetItem(Arg2, Arg1, Arg4)
            EXIT
         CASE "WIDTH"  // GF 01/05/2007
            RetVal := _GetStatusItemWidth(GetFormHandle(Arg1), Arg4)
            EXIT
         CASE "CAPTION"
         CASE "HEADER"
         CASE "COLUMNHEADER"
            RetVal := _GetMultiCaption(Arg2, Arg1, Arg4)
            EXIT
         CASE "IMAGE"
         CASE "HEADERIMAGE"
            RetVal := _GetMultiImage(Arg2, Arg1, Arg4)
            EXIT
         CASE "TOOLTIP"
            RetVal := _GetMultiToolTip(Arg2, Arg1, Arg4)
            EXIT
         CASE "COLUMNWIDTH" //(JK) HMG 1.0 Experimental Build 6
            IF Empty(Arg4) .OR. Arg4 < 1
               MsgMiniGuiError("Control: " + Arg2 + " Of " + Arg1 + ". Wrong or empty index param.")
            ENDIF
            RetVal := _GetColumnWidth(Arg2, Arg1, Arg4)
            EXIT
#ifdef _HMG_COMPAT_
         CASE "COLUMNONHEADCLICK"
            RetVal := _SetGetColumnHeadClick(Arg2, Arg1, Arg4)
            EXIT
         CASE "COLUMNDISPLAYPOSITION"
            RetVal := _GetColumnDisplayPosition(Arg2, Arg1, Arg4)
            EXIT
         CASE "COLUMNCONTROL"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_CONTROL_, Arg4)
            EXIT
         CASE "COLUMNDYNAMICFORECOLOR"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_DYNAMICFORECOLOR_, Arg4)
            EXIT
         CASE "COLUMNDYNAMICBACKCOLOR"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_DYNAMICBACKCOLOR_, Arg4)
            EXIT
         CASE "COLUMNJUSTIFY"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_JUSTIFY_, Arg4)
            EXIT
         CASE "COLUMNVALID"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_VALID_, Arg4)
            EXIT
         CASE "COLUMNWHEN"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_WHEN_, Arg4)
            EXIT
         CASE "COLUMNVALIDMESSAGE"
            RetVal := _SetGetGridProperty(Arg2, Arg1, _GRID_COLUMN_VALIDMESSAGE_, Arg4)
            EXIT
#endif
         CASE "ENABLED"
            RetVal := _IsControlEnabled(Arg2, Arg1, Arg4)
            EXIT
         CASE "RICHVALUE" // Kevin Carmody <i@kevincarmody.com> 2007.04.23
            RetVal := _SetGetRichValue(Arg2, Arg1, NIL, Arg4)
            EXIT
         CASE "CHECKBOXITEM"
            IF "GRID" $ GetControlTypeAsString(Arg2, Arg1) // Eduardo Fernandes 2009/JUN/17
               RetVal := _SetGetCheckBoxItemState(Arg2, Arg1, Arg4, NIL)
            ELSE
               RetVal := _SetGetChkListItemState(Arg2, Arg1, Arg4)
            ENDIF
            EXIT
         CASE "CARGO" // GF 16/02/2019
            IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_TREE
               RetVal := TreeNodeItemCargo(Arg2, Arg1, Arg4)
            ENDIF
            EXIT
         OTHERWISE // If Property Not Matched Look For Contained Control With No Arguments (ToolBar Button)
            IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_TOOLBAR
               IF GetControlHandle(Arg2, Arg1) != GetControlContainerHandle(Arg3, Arg1)
                  MsgMiniGuiError("Control Does Not Belong To Container.")
               ENDIF
               RetVal := GetProperty(Arg1, Arg3, Arg4)
            ENDIF
         ENDSWITCH

      ENDIF

      EXIT

   CASE 5 // PCount() == 5 (TAB CHILD CONTROL (WITHOUT ARGUMENT) OR SPLITBOX CHILD WITH ARGUMENT)

      IF Upper(Arg2) == "SPLITBOX"

         IF _IsControlSplitBoxed(Arg3, Arg1)
            RetVal := GetProperty(Arg1, Arg3, Arg4, Arg5)
         ELSE
            IF _IsControlDefined(Arg4, Arg1)
               IsControlInsideSplitBox(Arg1, Arg4)
               RetVal := GetProperty(Arg1, Arg3, Arg4, Arg5)
            ELSE
               MsgMiniGuiError("Control Does Not Belong To Container.")
            ENDIF
         ENDIF

      ELSE

         IF !hb_isNumeric(Arg3)
            Arg3 := Upper(Arg3)
            IF Arg3 == "CELL"
               IF Len(_HMG_aControlBkColor[GetControlIndex(Arg2, Arg1)]) > 0 .AND. Arg5 == 1
                  RetVal := hmg_GetImageListViewItems(GetControlHandle(Arg2, Arg1), Arg4)
               ELSE
                  RetVal := _GetGridCellValue(Arg2, Arg1, Arg4, Arg5)
               ENDIF
            ENDIF
         ELSE
            IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
            RetVal := GetProperty(Arg1, Arg4, Arg5)
         ENDIF

      ENDIF
      
      EXIT

   CASE 6 // PCount() == 6 (TAB CHILD CONTROL WITH 1 ARGUMENT OR SPLITBOX CHILD WITH 2 ARGUMENT)

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         RetVal := GetProperty(Arg1, Arg3, Arg4, Arg5, Arg6)
      ELSE
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         RetVal := GetProperty(Arg1, Arg4, Arg5, Arg6)
      ENDIF

      EXIT

   CASE 7 // PCount() == 7 (TAB CHILD CONTROL WITH 2 ARGUMENT OR SPLITBOX CHILD WITH 3 ARGUMENT)

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         RetVal := GetProperty(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7)
      ELSE
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         RetVal := GetProperty(Arg1, Arg4, Arg5, Arg6, Arg7)
      ENDIF

   ENDSWITCH

#ifdef _HMG_COMPAT_
   IF hb_IsChar(Arg1) .AND. hb_IsChar(Arg2) .AND. "GRID" $ GetControlTypeAsString(Arg2, Arg1) .AND. hb_IsChar(Arg3) .AND. "GROUP" $ Arg3

      SWITCH Arg3
      CASE "GROUPENABLED"
         RetVal := hmg_ListView_IsGroupViewEnabled(GetControlHandle(Arg2, Arg1))
         EXIT
      CASE "GROUPINFO"
         cHeader := nAlignHeader := cFooter := nAlingFooter := nState := NIL
         hmg_ListView_GroupGetInfo(GetControlHandle(Arg2, Arg1), Arg4, @cHeader, @nAlignHeader, @cFooter, @nAlingFooter, @nState)
         RetVal := {cHeader, nAlignHeader, cFooter, nAlingFooter, nState}
         EXIT
      CASE "GROUPITEMID"
         RetVal := hmg_ListView_GroupItemGetID(GetControlHandle(Arg2, Arg1), (Arg4 - 1))
         EXIT
      CASE "GROUPEXIST"
         RetVal := hmg_ListView_HasGroup(GetControlHandle(Arg2, Arg1), Arg4)
         EXIT
      CASE "GROUPGETALLITEMINDEX"
         RetVal := GroupGetAllItemIndex(Arg2, Arg1, Arg4)
      ENDSWITCH

   ENDIF
#endif

RETURN RetVal

FUNCTION DoMethod(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)

   LOCAL i
#ifdef _USERINIT_
   LOCAL cMacro
   LOCAL cProc
#endif

#ifdef _HMG_COMPAT_
   IF _RichEditBox_DoMethod(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)
      RETURN NIL
   ENDIF
#endif

   SWITCH PCount()

   CASE 2 // Window

      IF hb_IsChar(Arg1)
         IF !_IsWindowDefined(Arg1)
            MsgMiniGuiError("Window: " + Arg1 + " is not defined.")
         ENDIF
      ENDIF

      Arg2 := Upper(Arg2)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomMethodProcedure
         IF Arg2 == cProc[1]
            cMacro := cProc[2]
            &cMacro(Arg1, Arg2)
            IF _HMG_UserComponentProcess
               RETURN NIL
            ENDIF
         ENDIF
      NEXT
#endif

      SWITCH Arg2
      CASE "ACTIVATE"
         IF !hb_IsArray(Arg1)
            Arg1 := {Arg1}
         ENDIF
         _ActivateWindow(Arg1)
         EXIT
      CASE "CENTER"        ; _CenterWindow(Arg1, _SetCenterWindowStyle())            ; EXIT
      CASE "REDRAW"        ; hmg_RedrawWindow(GetFormHandle(Arg1))                   ; EXIT
      CASE "RELEASE"       ; _ReleaseWindow(Arg1)                                    ; EXIT
      CASE "MAXIMIZE"      ; _MaximizeWindow(Arg1)                                   ; EXIT
      CASE "MINIMIZE"      ; _MinimizeWindow(Arg1)                                   ; EXIT
      CASE "RESTORE"       ; _RestoreWindow(Arg1)                                    ; EXIT
      CASE "SHOW"          ; _ShowWindow(Arg1)                                       ; EXIT
      CASE "HIDE"          ; _HideWindow(Arg1)                                       ; EXIT
      CASE "SETFOCUS"
         i := GetFormIndex(Arg1)
         IF i > 0 .AND. i <= Len(_HMG_aFormHandles)
            IF _HMG_aFormActive[i]
               hmg_SetFocus(_HMG_aFormHandles[i])
            ENDIF
         ENDIF
         EXIT
      CASE "ENABLEUPDATE"  ; hmg_LockWindowUpdate(0) ; hmg_RedrawWindow(GetFormHandle(Arg1)) ; EXIT
      CASE "DISABLEUPDATE" ; hmg_LockWindowUpdate(GetFormHandle(Arg1))                       ; EXIT
      OTHERWISE
         MsgMiniGuiError("Window: unrecognized method '" + Arg2 + "'.")
      ENDSWITCH

      EXIT

   CASE 3 // CONTROL

      Arg3 := Upper(Arg3)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomMethodProcedure
         IF Arg3 == cProc[1]
            cMacro := cProc[2]
            &cMacro(Arg1, Arg2, Arg3)
            IF _HMG_UserComponentProcess
               RETURN NIL
            ENDIF
         ENDIF
      NEXT
#endif
      VerifyControlDefined(Arg1, Arg2)

      SWITCH Arg3
      CASE "COLUMNSAUTOFIT"  ; _SetColumnsWidthAuto(Arg2, Arg1)                                                                        ; EXIT //(JK) HMG 1.0 Experimental Build 6
      CASE "COLUMNSAUTOFITH" ; _SetColumnsWidthAutoH(Arg2, Arg1)                                                                       ; EXIT //(JK) HMG 1.0 Experimental Build 6
      CASE "ENABLEUPDATE"    ; _EnableListViewUpdate(Arg2, Arg1, .T.)                                                                  ; EXIT
      CASE "DISABLEUPDATE"   ; _EnableListViewUpdate(Arg2, Arg1, .F.)                                                                  ; EXIT
      CASE "REFRESH"         ; _Refresh(GetControlIndex(Arg2, Arg1))                                                                   ; EXIT
      CASE "REDRAW"          ; _RedrawControl(GetControlIndex(Arg2, Arg1))                                                             ; EXIT
      CASE "SAVE"            ; _SaveData(Arg2, Arg1)                                                                                   ; EXIT
      CASE "SETFOCUS"        ; _SetFocus(Arg2, Arg1)                                                                                   ; EXIT
      CASE "DELETEALLITEMS"  ; _DeleteAllItems(Arg2, Arg1)                                                                             ; EXIT
      CASE "RELEASE"         ; _ReleaseControl(Arg2, Arg1)                                                                             ; EXIT
      CASE "SHOW"            ; _ShowControl(Arg2, Arg1)                                                                                ; EXIT
      CASE "HIDE"            ; _HideControl(Arg2, Arg1)                                                                                ; EXIT
      CASE "PLAY"            ; iif(GetControlType(Arg2, Arg1) == CONTROL_TYPE_ANIMATEBOX, _PlayAnimateBox(Arg2, Arg1), _PlayPlayer(Arg2, Arg1))   ; EXIT
      CASE "STOP"            ; iif(GetControlType(Arg2, Arg1) == CONTROL_TYPE_ANIMATEBOX, _StopAnimateBox(Arg2, Arg1), _StopPlayer(Arg2, Arg1))   ; EXIT
      CASE "CLOSE"           ; iif(GetControlType(Arg2, Arg1) == CONTROL_TYPE_ANIMATEBOX, _CloseAnimateBox(Arg2, Arg1), _ClosePlayer(Arg2, Arg1)) ; EXIT
      CASE "PLAYREVERSE"     ; _PlayPlayerReverse(Arg2, Arg1)                                                                          ; EXIT
      CASE "PAUSE"           ; _PausePlayer(Arg2, Arg1)                                                                                ; EXIT
      CASE "EJECT"           ; _EjectPlayer(Arg2, Arg1)                                                                                ; EXIT
      CASE "OPENDIALOG"      ; _OpenPlayerDialog(Arg2, Arg1)                                                                           ; EXIT
      CASE "RESUME"          ; _ResumePlayer(Arg2, Arg1)                                                                               ; EXIT
      CASE "GETARRAY"        ; RETURN _GetControlArray(Arg2, Arg1) // GF
      CASE "ACTION"
      CASE "ONCLICK"
      CASE "ONGOTFOCUS"
      CASE "ONLOSTFOCUS"
      CASE "ONCHANGE"
      CASE "ONDBLCLICK"
      CASE "ONDISPLAYCHANGE"
      CASE "ONENTER"         ; Eval(_GetControlAction(Arg2, Arg1, Arg3)) // GF 10/28/10
      ENDSWITCH

      EXIT

   CASE 4   // CONTROL WITH 1 ARGUMENT OR SPLITBOX CHILD WITHOUT ARGUMENT

      Arg3 := Upper(Arg3)

#ifdef _USERINIT_
      FOR EACH cProc IN _HMG_aCustomMethodProcedure
         IF Arg3 == cProc[1]
            cMacro := cProc[2]
            &cMacro(Arg1, Arg2, Arg3, Arg4)
            IF _HMG_UserComponentProcess
               RETURN NIL
            ENDIF
         ENDIF
      NEXT
#endif
      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         DoMethod(Arg1, Arg3, Arg4)
         RETURN NIL
      ELSE
         VerifyControlDefined(Arg1, Arg2)
      ENDIF

      //(JK) HMG 1.0 Experimental Build 6 (2 new method for GRID/BROWSE control COLUMNAUTOFIT(n),COLUMNAUTOFITH(n))

      SWITCH Arg3
      CASE "COLUMNAUTOFIT"  ; _SetColumnWidthAuto(Arg2, Arg1, Arg4)           ; EXIT
      CASE "COLUMNAUTOFITH" ; _SetColumnWidthAutoH(Arg2, Arg1, Arg4)          ; EXIT
      CASE "DELETEITEM"     ; _DeleteItem(Arg2, Arg1, Arg4)                   ; EXIT
      CASE "DELETEPAGE"     ; _DeleteTabPage(Arg2, Arg1, Arg4)                ; EXIT
      CASE "OPEN"
         iif(GetControlType(Arg2, Arg1) == CONTROL_TYPE_ANIMATEBOX, _OpenAnimateBox(Arg2, Arg1, Arg4), _OpenPlayer(Arg2, Arg1, Arg4))
         EXIT
      CASE "SEEK"           ; _SeekAnimateBox(Arg2, Arg1, Arg4)               ; EXIT
      CASE "ADDITEM"        ; _AddItem(Arg2, Arg1, Arg4)                      ; EXIT
      CASE "EXPAND"         ; _Expand(Arg2, Arg1, Arg4)                       ; EXIT
      CASE "COLLAPSE"       ; _Collapse(Arg2, Arg1, Arg4)                     ; EXIT
      CASE "DELETECOLUMN"   ; _DeleteGridColumn(Arg2, Arg1, Arg4)             ; EXIT
      CASE "DELETEIMAGE"    ; _RemoveImageFromImageList(Arg2, Arg1, Arg4 - 1) ; EXIT // JP ImageList
      CASE "SETARRAY"       ; _SetArrayToControl(Arg2, Arg1, Arg4)            ; EXIT // GF
#ifdef _BT_
      CASE "SETSHOWTEXT" // GF
         IF GetControlType(Arg2, Arg1) == CONTROL_TYPE_PROGRESSWHEEL
            PW_SetShowText(Arg2, Arg1, Arg4)
         ENDIF
         EXIT
#endif
      OTHERWISE
         IF !("GROUP" $ Arg3)
            MsgMiniGuiError("Control: unrecognized method '" + Arg3 + "'.")
         ENDIF
      ENDSWITCH

      EXIT

   CASE 5

      IF hb_IsChar(Arg3) // CONTROL WITH 2 ARGUMENTS OR SPLITBOX CHILD WITH 1 ARGUMENT

         Arg3 := Upper(Arg3)

#ifdef _USERINIT_
         FOR EACH cProc IN _HMG_aCustomMethodProcedure
            IF Arg3 == cProc[1]
               cMacro := cProc[2]
               &cMacro(Arg1, Arg2, Arg3, Arg4, Arg5)
               IF _HMG_UserComponentProcess
                  RETURN NIL
               ENDIF
            ENDIF
         NEXT
#endif
         IF Upper(Arg2) == "SPLITBOX"
            IsControlInsideSplitBox(Arg1, Arg3)
            DoMethod(Arg1, Arg3, Arg4, Arg5)
            RETURN NIL
         ELSE
            VerifyControlDefined(Arg1, Arg2)
         ENDIF

         SWITCH Arg3
         CASE "ADDITEM"      ; _AddItem(Arg2, Arg1, Arg4, Arg5)                   ; EXIT
         CASE "ADDPAGE"      ; _AddTabPage(Arg2, Arg1, Arg4, Arg5)                ; EXIT
         CASE "EXPAND"       ; _Expand(Arg2, Arg1, Arg4, Arg5)                    ; EXIT // Tree+
         CASE "COLLAPSE"     ; _Collapse(Arg2, Arg1, Arg4, Arg5)                  ; EXIT // Tree+
         CASE "ADDIMAGE"     ; _AddImageToImageList(Arg2, Arg1, Arg4, Arg5)       ; EXIT // JP ImageList
         CASE "ADDIMAGEMASK" ; _AddImageMaskedToImageList(Arg2, Arg1, Arg4, Arg5) ; EXIT // JP ImageList
         OTHERWISE
            IF !("GROUP" $ Arg3)
               MsgMiniGuiError("Control: unrecognized method '" + Arg3 + "'.")
            ENDIF
         ENDSWITCH

      ELSEIF hb_IsNumeric(Arg3) // TAB CHILD WITHOUT ARGUMENTS

         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         DoMethod(Arg1, Arg4, Arg5)

      ENDIF

      EXIT

   CASE 6

      IF hb_IsChar(Arg3) // CONTROL WITH 3 ARGUMENTS OR SPLITBOX CHILD WITH 2 ARGUMENTS

         Arg3 := Upper(Arg3)

#ifdef _USERINIT_
         FOR EACH cProc IN _HMG_aCustomMethodProcedure
            IF Arg3 == cProc[1]
               cMacro := cProc[2]
               &cMacro(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
               IF _HMG_UserComponentProcess
                  RETURN NIL
               ENDIF
            ENDIF
         NEXT
#endif
         IF Upper(Arg2) == "SPLITBOX"
            IsControlInsideSplitBox(Arg1, Arg3)
            DoMethod(Arg1, Arg3, Arg4, Arg5, Arg6)
            RETURN NIL
         ELSE
            VerifyControlDefined(Arg1, Arg2)
         ENDIF

         SWITCH Arg3
         CASE "ADDITEM" ; _AddItem(Arg2, Arg1, Arg4, Arg5, Arg6) ; EXIT
         CASE "ADDPAGE" ; _AddTabPage(Arg2, Arg1, Arg4, Arg5, Arg6)
         ENDSWITCH

      ELSEIF hb_IsNumeric(Arg3) // TAB CHILD WITH 1 ARGUMENT

         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         DoMethod(Arg1, Arg4, Arg5, Arg6)

      ENDIF

      EXIT

   CASE 7

      IF hb_IsChar(Arg3) // CONTROL WITH 4 ARGUMENTS OR SPLITBOX CHILD WITH 3 ARGUMENTS

         Arg3 := Upper(Arg3)

#ifdef _USERINIT_
         FOR EACH cProc IN _HMG_aCustomMethodProcedure
            IF Arg3 == cProc[1]
               cMacro := cProc[2]
               &cMacro(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7)
               IF _HMG_UserComponentProcess
                  RETURN NIL
               ENDIF
            ENDIF
         NEXT
#endif
         IF Upper(Arg2) == "SPLITBOX"
            IsControlInsideSplitBox(Arg1, Arg3)
            DoMethod(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7)
            RETURN NIL
         ELSE
            VerifyControlDefined(Arg1, Arg2)
         ENDIF

         SWITCH Arg3
         CASE "ADDCONTROL" ; _AddTabControl(Arg2, Arg4, Arg1, Arg5, Arg6, Arg7) ; EXIT
         CASE "ADDCOLUMN"  ; _AddGridColumn(Arg2, Arg1, Arg4, Arg5, Arg6, Arg7) ; EXIT
         CASE "ADDITEM"    ; _AddItem(Arg2, Arg1, Arg4, Arg5, Arg6, Arg7)       ; EXIT
         CASE "ADDPAGE"    ; _AddTabPage(Arg2, Arg1, Arg4, Arg5, Arg6, Arg7)    ; EXIT // JR
         OTHERWISE
            IF !("GROUP" $ Arg3)
               MsgMiniGuiError("Control: unrecognized method '" + Arg3 + "'.")
            ENDIF
         ENDSWITCH

      ELSEIF hb_IsNumeric(Arg3) // TAB CHILD WITH 2 ARGUMENTS

         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         DoMethod(Arg1, Arg4, Arg5, Arg6, Arg7)

      ENDIF

      EXIT

   CASE 8 // TAB CHILD WITH 3 ARGUMENTS OR SPLITBOX CHILD WITH 4 ARGUMENTS

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         DoMethod(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)
         RETURN NIL
      ENDIF

      IF hb_IsNumeric(Arg3)
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         DoMethod(Arg1, Arg4, Arg5, Arg6, Arg7, Arg8)
      ENDIF

      EXIT

   CASE 9 // TAB CHILD WITH 4 ARGUMENTS OR SPLITBOX CHILD WITH 5 ARGUMENTS

      IF Upper(Arg2) == "SPLITBOX"
         IsControlInsideSplitBox(Arg1, Arg3)
         DoMethod(Arg1, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)
         RETURN NIL
      ENDIF

      IF hb_IsNumeric(Arg3)
         IsControlInTabPage(Arg1, Arg2, Arg3, Arg4)
         DoMethod(Arg1, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)
      ENDIF

   ENDSWITCH

#ifdef _HMG_COMPAT_
   IF hb_IsChar(Arg1) .AND. hb_IsChar(Arg2) .AND. "GRID" $ GetControlTypeAsString(Arg2, Arg1) .AND. hb_IsChar(Arg3) .AND. "GROUP" $ Arg3
      SWITCH Arg3
      CASE "GROUPDELETEALL"      ; hmg_ListView_GroupDeleteAll(GetControlHandle(Arg2, Arg1))                                               ; EXIT
      CASE "GROUPDELETE"         ; hmg_ListView_GroupDelete(GetControlHandle(Arg2, Arg1), Arg4)                                            ; EXIT
      CASE "GROUPADD"            ; hmg_ListView_GroupAdd(GetControlHandle(Arg2, Arg1), Arg4, Arg5)                                         ; EXIT
      CASE "GROUPEXPAND"         ; hmg_ListView_GroupSetInfo(GetControlHandle(Arg2, Arg1), Arg4, NIL, NIL, NIL, NIL, GRID_GROUP_NORMAL)    ; EXIT
      CASE "GROUPCOLLAPSED"      ; hmg_ListView_GroupSetInfo(GetControlHandle(Arg2, Arg1), Arg4, NIL, NIL, NIL, NIL, GRID_GROUP_COLLAPSED) ; EXIT
      CASE "GROUPDELETEALLITEMS" ; GroupDeleteAllItems(Arg2, Arg1, Arg4)                                                               ; EXIT
      OTHERWISE
         MsgMiniGuiError("Grid Group: unrecognized method '" + Arg3 + "'.")
      ENDSWITCH
   ENDIF
#endif

RETURN NIL

STATIC FUNCTION SetFirstDayOfWeek(cControlName, cFormName, Value)

   LOCAL cDay As String
   LOCAL aWeek := Array(7, 2)
   LOCAL nPos
   LOCAL i

   Assign cDay := Value

   cDay := Upper(AllTrim(cDay))

   FOR i := 1 TO 7
      aWeek[i] := {i - 1, Upper(iif(i == 7, NToCDoW(1), NToCDoW(i + 1)))}
   NEXT

   IF (nPos := AScan(aWeek, {|x|x[2] == cDay})) == 0
      RETURN -1
   ENDIF

RETURN SetMonthCalFirstDayOfWeek(GetControlHandle(cControlName, cFormName), aWeek[nPos][1])

STATIC FUNCTION GetFirstDayOfWeek(cControlName, cFormName)

   LOCAL nDay := GetMonthCalFirstDayOfWeek(GetControlHandle(cControlName, cFormName))
   LOCAL aWeek := Array(7, 2)
   LOCAL nPos
   LOCAL i

   FOR i := 1 TO 7
      aWeek[i] := {i - 1, iif(i == 7, NToCDoW(1), NToCDoW(i + 1))}
   NEXT

   IF (nPos := AScan(aWeek, {|x|x[1] == nDay})) == 0
      RETURN ""
   ENDIF

RETURN aWeek[nPos][2]

#ifdef _HMG_COMPAT_
STATIC PROCEDURE GroupDeleteAllItems(cControlName, cParentName, nGroupID)

   LOCAL nItemCount
   LOCAL i

   IF GetProperty(cParentName, cControlName, "GroupExist", nGroupID)
      nItemCount := GetProperty(cParentName, cControlName, "ItemCount")
      FOR i := 1 TO nItemCount
         IF GetProperty(cParentName, cControlName, "GroupItemID", i) == nGroupID
            DoMethod(cParentName, cControlName, "DeleteItem", i)
         ENDIF
      NEXT i
   ENDIF

RETURN

STATIC FUNCTION GroupGetAllItemIndex(cControlName, cParentName, nGroupID)

   LOCAL aItemIndex := {}
   LOCAL nItemCount
   LOCAL i

   IF GetProperty(cParentName, cControlName, "GroupExist", nGroupID)
      nItemCount := GetProperty(cParentName, cControlName, "ItemCount")
      FOR i := 1 TO nItemCount
         IF GetProperty(cParentName, cControlName, "GroupItemID", i) == nGroupID
            AAdd(aItemIndex, i)
         ENDIF
      NEXT i
   ENDIF

RETURN aItemIndex

STATIC PROCEDURE GroupCheckBoxAllItems(cControlName, cParentName, nGroupID, lCheck)

   LOCAL aItemIndex
   LOCAL i

   IF GetProperty(cParentName, cControlName, "GroupExist", nGroupID)
      aItemIndex := GroupGetAllItemIndex(cControlName, cParentName, nGroupID)
      FOR i := 1 TO Len(aItemIndex)
         SetProperty(cParentName, cControlName, "CheckBoxItem", aItemIndex[i], lCheck)
      NEXT i
   ENDIF

RETURN
#endif

#ifdef _BT_
STATIC FUNCTION _ProgressWheel_GetProperty(xData, Arg1, Arg2, Arg3)

   LOCAL RetVal := .F.

   IF !hb_isChar(Arg1) .OR. !hb_isChar(Arg2) .OR. !hb_isChar(Arg3) .OR. !_IsControlDefined(Arg2, Arg1) .OR. (GetControlType(Arg2, Arg1) != CONTROL_TYPE_PROGRESSWHEEL)
      RETURN .F.
   ENDIF

   Arg3 := Upper(AllTrim(Arg3))

   SWITCH Arg3
   CASE "COLORDONEMIN" ; xData := PW_GetColorDoneMin(Arg2, Arg1)                      ; RetVal := .T. ; EXIT
   CASE "COLORDONEMAX" ; xData := PW_GetColorDoneMax(Arg2, Arg1)                      ; RetVal := .T. ; EXIT
   CASE "COLORREMAIN"  ; xData := PW_GetColorRemain(Arg2, Arg1)                       ; RetVal := .T. ; EXIT
   CASE "COLORINNER"   ; xData := PW_GetColorInner(Arg2, Arg1)                        ; RetVal := .T. ; EXIT
   CASE "INNERSIZE"    ; xData := _HMG_aControlSpacing[GetControlIndex(Arg2, Arg1)]   ; RetVal := .T. ; EXIT
   CASE "STARTANGLE"   ; xData := _HMG_aControlInputMask[GetControlIndex(Arg2, Arg1)] ; RetVal := .T. ; EXIT
   CASE "MIN"          ; xData := _HMG_aControlRangeMin[GetControlIndex(Arg2, Arg1)]  ; RetVal := .T. ; EXIT
   CASE "MAX"          ; xData := _HMG_aControlRangeMax[GetControlIndex(Arg2, Arg1)]  ; RetVal := .T. ; EXIT
   CASE "POSITION"     ; xData := _HMG_aControlValue[GetControlIndex(Arg2, Arg1)]     ; RetVal := .T. ; EXIT
   CASE "SHOWTEXT"     ; xData := _HMG_aControlDblClick[GetControlIndex(Arg2, Arg1)]  ; RetVal := .T. ; EXIT
   CASE "GRADIENTMODE" ; xData := _HMG_aControlPicture[GetControlIndex(Arg2, Arg1)]   ; RetVal := .T.
   ENDSWITCH

RETURN RetVal

STATIC FUNCTION _ProgressWheel_SetProperty(Arg1, Arg2, Arg3, Arg4)

   LOCAL RetVal := .F.

   IF !hb_isChar(Arg1) .OR. !hb_isChar(Arg2) .OR. !hb_isChar(Arg3) .OR. !_IsControlDefined(Arg2, Arg1) .OR. (GetControlType(Arg2, Arg1) != CONTROL_TYPE_PROGRESSWHEEL)
      RETURN .F.
   ENDIF

   Arg3 := Upper(AllTrim(Arg3))

   SWITCH Arg3
   CASE "COLORDONEMIN" ; PW_SetColorDoneMin(Arg2, Arg1, Arg4)                       ; RetVal := .T. ; EXIT
   CASE "COLORDONEMAX" ; PW_SetColorDoneMax(Arg2, Arg1, Arg4)                       ; RetVal := .T. ; EXIT
   CASE "COLORREMAIN"  ; PW_SetColorRemain(Arg2, Arg1, Arg4)                        ; RetVal := .T. ; EXIT
   CASE "COLORINNER"   ; PW_SetColorInner(Arg2, Arg1, Arg4)                         ; RetVal := .T. ; EXIT
   CASE "INNERSIZE"    ; PW_SetInnerSize(Arg2, Arg1, Arg4)                          ; RetVal := .T. ; EXIT
   CASE "STARTANGLE"   ; PW_SetStartAngle(Arg2, Arg1, Arg4)                         ; RetVal := .T. ; EXIT
   CASE "MIN"          ; PW_SetMin(Arg2, Arg1, Arg4)                                ; RetVal := .T. ; EXIT
   CASE "MAX"          ; PW_SetMax(Arg2, Arg1, Arg4)                                ; RetVal := .T. ; EXIT
   CASE "POSITION"     ; PW_SetPosition(Arg2, Arg1, Arg4)                           ; RetVal := .T. ; EXIT
   CASE "SHOWTEXT"     ; _HMG_aControlDblClick[GetControlIndex(Arg2, Arg1)] := Arg4 ; RetVal := .T. ; EXIT
   CASE "GRADIENTMODE" ; PW_SetGradientMode(Arg2, Arg1, Arg4)                       ; RetVal := .T.
   ENDSWITCH

RETURN RetVal
#endif

STATIC PROCEDURE VerifyControlDefined(cParentName, cControlName)

   IF !Empty(cControlName) .AND. !_IsControlDefined(cControlName, cParentName)
      MsgMiniGuiError("Control: " + cControlName + " Of " + cParentName + " Not defined.")
   ENDIF

RETURN

STATIC PROCEDURE IsControlInTabPage(cParentName, cTabName, nTabPage, cControlName)

   IF GetControlTabPage(cControlName, cTabName, cParentName) != nTabPage
      MsgMiniGuiError("Control Does Not Belong To Container.")
   ENDIF

RETURN

FUNCTION GetControlTabPage(cControlName, cTabName, cParentWindowName)

   LOCAL nRetVal As Numeric
   LOCAL niControl
   LOCAL niTab
   LOCAL xControlHandle
   LOCAL tabpage
   LOCAL c
   LOCAL r
   LOCAL k

   IF (niControl := GetControlIndex(cControlName, cParentWindowName)) > 0

      xControlHandle := _HMG_aControlHandles[niControl]
      niTab := GetControlIndex(cTabName, cParentWindowName)

      FOR EACH tabpage IN _HMG_aControlPageMap[niTab]

         k := hb_enumindex(tabpage)

         FOR EACH c IN tabpage

            IF hb_IsNumeric(c) .AND. hb_IsNumeric(xControlHandle)
               IF c == xControlHandle
                  nRetVal := k
                  EXIT
               ENDIF

            ELSEIF hb_IsArray(c) .AND. hb_IsArray(xControlHandle)
               FOR EACH r IN xControlHandle
                  IF AScan(c, r) != 0
                     nRetVal := k
                     EXIT
                  ENDIF
               NEXT
               IF nRetVal != 0
                  EXIT
               ENDIF

            ELSEIF hb_IsArray(c) .AND. hb_IsNumeric(xControlHandle)
               IF AScan(c, xControlHandle) != 0
                  nRetVal := k
                  EXIT
               ENDIF
            ENDIF

         NEXT

         IF nRetVal != 0
            EXIT
         ENDIF

      NEXT

   ENDIF

RETURN nRetVal

STATIC PROCEDURE IsControlInsideSplitBox(cParentName, cControlName)

   IF !_IsControlSplitBoxed(cControlName, cParentName)
      MsgMiniGuiError("Control Does Not Belong To Container.")
   ENDIF

RETURN

STATIC FUNCTION _IsControlSplitBoxed(cControlName, cWindowName)

   LOCAL lSplitBoxed As Logical
   LOCAL i

   IF (i := GetControlIndex(cControlName, cWindowName)) > 0
      IF !Empty(_HMG_SplitLastControl) .AND. _HMG_aControlRow[i] == NIL .AND. _HMG_aControlCol[i] == NIL .OR. ;
         "GRID" $ _HMG_aControlType[i] .AND. Empty(_HMG_aControlRow[i]) .AND. Empty(_HMG_aControlCol[i])
         lSplitBoxed := .T.
      ENDIF
   ENDIF

RETURN lSplitBoxed

STATIC FUNCTION _GetControlArray(cControlName, cFormName)  // GF 07/06/21

   LOCAL aItems := {}
   LOCAL nCount
   LOCAL i

   IF (nCount := _GetItemCount(cControlName, cFormName)) > 0
      FOR i := 1 TO nCount
         AAdd(aItems, GetProperty(cFormName, cControlName, "Item", i))
      NEXT i
   ENDIF

RETURN aItems

STATIC FUNCTION _SetArrayToControl(ControlName, ParentForm, aValue)  // GF 03/30/16

   LOCAL t
   LOCAL i
   LOCAL BackValue
#ifdef _TSBROWSE_
   LOCAL oGet
#endif
   IF !hb_IsArray(aValue)
      RETURN NIL
   ENDIF

   i := GetControlIndex(ControlName, ParentForm)
   T := _HMG_aControlType[i]

   SWITCH T

   // CASE "LIST" $ T
   CASE CONTROL_TYPE_CHKLIST
   CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTICHKLIST
   CASE CONTROL_TYPE_MULTILIST
      _HMG_aControlRangeMin[i] := aValue
      BackValue := _GetValue(NIL, NIL, i)
      _DeleteAllItems(ControlName, ParentForm)
      aEval(aValue, {|row|DoMethod(ParentForm, ControlName, "AddItem", row)})
      _SetValue(NIL, NIL, BackValue, i)
      EXIT

   CASE CONTROL_TYPE_COMBO
      IF !hb_isChar(_HMG_aControlSpacing[i]) .AND. _HMG_aControlMiscData1[i][1] != 1
         _HMG_aControlMiscData1[i][4] := aValue
         _Refresh(i)
      ENDIF
      EXIT

   //CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF Len(aValue) > 0 .AND. Len(aValue[1]) != Len(_HMG_aControlMiscData1[i][2])
         MsgMiniGuiError("Grid: ITEMS length mismatch.")
      ELSE
         _HMG_aControlMiscData1[i][4] := aValue
         BackValue := _GetValue(NIL, NIL, i)
         _DeleteAllItems(ControlName, ParentForm)
         aEval(aValue, {|row|DoMethod(ParentForm, ControlName, "AddItem", row)})
         _SetValue(NIL, NIL, BackValue, i)
      ENDIF
      EXIT

#ifdef _TSBROWSE_
   CASE CONTROL_TYPE_TBROWSE
      oGet := GetObjectByHandle(_HMG_aControlHandles[i])
      IF hb_IsObject(oGet)
         oGet:SetItems(aValue)
      ENDIF
#endif

   ENDSWITCH

RETURN NIL

#ifdef _HMG_COMPAT_
// *********************************************
// by Dr. Claudio Soto (January 2014)
// *********************************************

PROCEDURE FindTextDlg(OnActionCodeBlock, cFind, lNoUpDown, lNoMatchCase, lNoWholeWord, lCheckDown, lCheckMatchCase, lCheckWholeWord, cTitle)

   LOCAL cReplace := NIL

   IF !hb_isBlock(OnActionCodeBlock)
      OnActionCodeBlock := {||NIL}
   ENDIF

   IF !hb_isLogical(lCheckDown)
      lCheckDown := .T.
   ENDIF

   IF !hb_isLogical(lCheckMatchCase)
      lCheckMatchCase := .F.
   ENDIF

   IF !hb_isLogical(lCheckWholeWord)
      lCheckWholeWord := .F.
   ENDIF

   IF !FindReplaceDlgIsRelease ()
      hmg_FindReplaceDlgRelease(.T.)
   ENDIF

   _HMG_FindReplaceOnAction := OnActionCodeBlock
   hmg_FindReplaceDlg(NIL, lNoUpDown, lNoMatchCase, lNoWholeWord, lCheckDown, lCheckMatchCase, lCheckWholeWord, cFind, cReplace, .F., cTitle)

RETURN

PROCEDURE ReplaceTextDlg(OnActionCodeBlock, cFind, cReplace, lNoMatchCase, lNoWholeWord, lCheckMatchCase, lCheckWholeWord, cTitle)

   LOCAL lNoUpDown := NIL
   LOCAL lCheckDown := NIL

   IF !hb_isBlock(OnActionCodeBlock)
      OnActionCodeBlock := {||NIL}
   ENDIF

   IF !hb_isLogical(lCheckMatchCase)
      lCheckMatchCase := .F.
   ENDIF

   IF !hb_isLogical(lCheckWholeWord)
      lCheckWholeWord := .F.
   ENDIF

   IF !FindReplaceDlgIsRelease ()
      hmg_FindReplaceDlgRelease(.T.)
   ENDIF

   _HMG_FindReplaceOnAction := OnActionCodeBlock
   hmg_FindReplaceDlg(NIL, lNoUpDown, lNoMatchCase, lNoWholeWord, lCheckDown, lCheckMatchCase, lCheckWholeWord, cFind, cReplace, .T., cTitle)

RETURN

// *********************************************
// by Dr. Claudio Soto (January 2014)
// *********************************************

STATIC FUNCTION _RichEditBox_GetProperty(xData, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

   LOCAL hWndControl
   LOCAL cFontName
   LOCAL nFontSize
   LOCAL lBold
   LOCAL lItalic
   LOCAL lUnderline
   LOCAL lStrikeout
   LOCAL aTextColor
   LOCAL aBackColor
   LOCAL nScript
   LOCAL lLink
   LOCAL nAlignment
   LOCAL nNumbering
   LOCAL nNumberingStyle
   LOCAL nNumberingStart
   LOCAL ndOffset
   LOCAL ndLineSpacing
   LOCAL ndStartIndent
   LOCAL nNumerator
   LOCAL nDenominator
   LOCAL RetVal := .F.

   IF !hb_isChar(Arg1) .OR. !hb_isChar(Arg2) .OR. !hb_isChar(Arg3) .OR. ;
      !_IsControlDefined(Arg2, Arg1) .OR. (GetControlType(Arg2, Arg1) != CONTROL_TYPE_RICHEDIT) .OR. (_HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)] != 1)
      RETURN .F.
   ENDIF

   cFontName := nFontSize := lBold := lItalic := lUnderline := lStrikeout := aTextColor := aBackColor := nScript := lLink := NIL
   nAlignment := nNumbering := nNumberingStyle := nNumberingStart := ndOffset := ndLineSpacing := ndStartIndent := NIL

   hWndControl := GetControlHandle(Arg2, Arg1)
   hmg_RichEditBox_GetFont(hWndControl, @cFONTNAME, @nFONTSIZE, @lBOLD, @lItalic, @lUnderline, @lStrikeout, @aTextColor, @aBACKCOLOR, @nScript, @lLink)
   hmg_RichEditBox_GetParaFormat(hWndControl, @nALIGNMENT, @nNumbering, @nNumberingStyle, @nNumberingStart, @ndOffset, @ndLineSpacing, @ndStartIndent)

   Arg3 := Upper(AllTrim(Arg3))

   SWITCH Arg3
   CASE "FONTNAME"           ; xData := cFontName                                                                      ; RetVal := .T. ; EXIT
   CASE "FONTSIZE"           ; xData := nFontSize                                                                      ; RetVal := .T. ; EXIT
   CASE "FONTBOLD"           ; xData := lBold                                                                          ; RetVal := .T. ; EXIT
   CASE "FONTITALIC"         ; xData := lItalic                                                                        ; RetVal := .T. ; EXIT
   CASE "FONTUNDERLINE"      ; xData := lUnderline                                                                     ; RetVal := .T. ; EXIT
   CASE "FONTSTRIKEOUT"      ; xData := lStrikeout                                                                     ; RetVal := .T. ; EXIT
   CASE "FONTCOLOR"          ; xData := aTextColor                                                                     ; RetVal := .T. ; EXIT
   CASE "FONTBACKCOLOR"      ; xData := aBackColor                                                                     ; RetVal := .T. ; EXIT
   CASE "FONTSCRIPT"         ; xData := nScript                                                                        ; RetVal := .T. ; EXIT
   CASE "RTFTEXTMODE"        ; xData := hmg_RichEditBox_IsRTFTextMode(hWndControl)                                         ; RetVal := .T. ; EXIT
   CASE "AUTOURLDETECT"      ; xData := hmg_RichEditBox_GetAutoURLDetect(hWndControl)                                      ; RetVal := .T. ; EXIT
   CASE "ZOOM"
      hmg_RichEditBox_GetZoom(hWndControl, @nNumerator, @nDenominator)
      xData := (nNumerator / nDenominator) * 100
      RetVal := .T.
      EXIT
   CASE "SELECTRANGE"        ; xData := hmg_RichEditBox_GetSelRange(hWndControl)                                            ; RetVal := .T. ; EXIT
   CASE "CARETPOS"           ; xData := RichEditBox_GetCaretPos(hWndControl)                                            ; RetVal := .T. ; EXIT
   CASE "VALUE"              ; xData := hmg_RichEditBox_GetText(hWndControl, .F.)                                           ; RetVal := .T. ; EXIT
   CASE "GETSELECTTEXT"      ; xData := hmg_RichEditBox_GetText(hWndControl, .T.)                                           ; RetVal := .T. ; EXIT
   CASE "GETTEXTRANGE"       ; xData := hmg_RichEditBox_GetTextRange(hWndControl, Arg4)                                     ; RetVal := .T. ; EXIT
   CASE "GETTEXTLENGTH"      ; xData := hmg_RichEditBox_GetTextLength(hWndControl)                                          ; RetVal := .T. ; EXIT
   CASE "GETPOSCHAR"         ; xData := hmg_RichEditBox_PosFromChar(hWndControl, Arg4)                                      ; RetVal := .T. ; EXIT // return {nRowScreen, nColScreen} or {-1, -1} if character is not displayed
   CASE "PARAALIGNMENT"      ; xData := nAlignment                                                                      ; RetVal := .T. ; EXIT
   CASE "PARANUMBERING"      ; xData := nNumbering                                                                      ; RetVal := .T. ; EXIT
   CASE "PARANUMBERINGSTYLE" ; xData := nNumberingStyle                                                                 ; RetVal := .T. ; EXIT
   CASE "PARANUMBERINGSTART" ; xData := nNumberingStart                                                                 ; RetVal := .T. ; EXIT
   CASE "PARAOFFSET"         ; xData := ndOffset                                                                        ; RetVal := .T. ; EXIT // in millimeters
   CASE "PARALINESPACING"    ; xData := ndLineSpacing                                                                   ; RetVal := .T. ; EXIT
   CASE "PARAINDENT"         ; xData := ndStartIndent                                                                   ; RetVal := .T. ; EXIT // in millimeters
   CASE "CANPASTE"           ; xData := hmg_RichEditBox_CanPaste(hWndControl)                                               ; RetVal := .T. ; EXIT
   CASE "CANUNDO"            ; xData := hmg_RichEditBox_CanUnDo(hWndControl)                                                ; RetVal := .T. ; EXIT
   CASE "CANREDO"            ; xData := hmg_RichEditBox_CanReDo(hWndControl)                                                ; RetVal := .T. ; EXIT
   CASE "FINDTEXT"           ; xData := hmg_RichEditBox_FindText(hWndControl, Arg4, Arg5, Arg6, Arg7, Arg8)                 ; RetVal := .T. ; EXIT
   CASE "REPLACETEXT"        ; xData := RichEditBox_ReplaceText(hWndControl, Arg4, Arg5, Arg6, Arg7, Arg8)              ; RetVal := .T. ; EXIT
   CASE "REPLACEALLTEXT"     ; xData := RichEditBox_ReplaceAllText(hWndControl, Arg4, Arg5, Arg6, Arg7, Arg8)           ; RetVal := .T. ; EXIT
   CASE "LINK"               ; xData := lLink                                                                           ; RetVal := .T. ; EXIT
   CASE "GETCLICKLINKRANGE"  ; xData := {_HMG_CharRange_Min, _HMG_CharRange_Max}                                        ; RetVal := .T. ; EXIT // This Value is valid only into ON LINK procedure
   CASE "GETCLICKLINKTEXT"   ; xData := hmg_RichEditBox_GetTextRange(hWndControl, {_HMG_CharRange_Min, _HMG_CharRange_Max}) ; RetVal := .T. ; EXIT // This Value is valid only into ON LINK procedure
   CASE "VIEWRECT"           ; xData := hmg_RichEditBox_GetRect(hWndControl)                                                ; RetVal := .T.
   ENDSWITCH

RETURN RetVal

STATIC FUNCTION _RichEditBox_SetProperty(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)

   LOCAL hWndControl
   LOCAL cFontName
   LOCAL nFontSize
   LOCAL lBold
   LOCAL lItalic
   LOCAL lUnderline
   LOCAL lStrikeout
   LOCAL aTextColor
   LOCAL aBackColor
   LOCAL nScript
   LOCAL lLink
   LOCAL nAlignment
   LOCAL nNumbering
   LOCAL nNumberingStyle
   LOCAL nNumberingStart
   LOCAL ndOffset
   LOCAL ndLineSpacing
   LOCAL ndStartIndent
   LOCAL nNumerator
   LOCAL nDenominator
   LOCAL RetVal := .F.

   IF !hb_isChar(Arg1) .OR. !hb_isChar(Arg2) .OR. !hb_isChar(Arg3) .OR. ;
      !_IsControlDefined(Arg2, Arg1) .OR. (GetControlType(Arg2, Arg1) != CONTROL_TYPE_RICHEDIT) .OR. (_HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)] != 1)
      RETURN .F.
   ENDIF

   cFontName := nFontSize := lBold := lItalic := lUnderline := lStrikeout := aTextColor := aBackColor := nScript := lLink := NIL
   nAlignment := nNumbering := nNumberingStyle := nNumberingStart := ndOffset := ndLineSpacing := ndStartIndent := NIL

   // Parameters NOT used
   HB_SYMBOL_UNUSED(Arg6)
   HB_SYMBOL_UNUSED(Arg7)
   HB_SYMBOL_UNUSED(Arg8)

   hWndControl := GetControlHandle(Arg2, Arg1)

   Arg3 := Upper(AllTrim(Arg3))

   SWITCH Arg3
   CASE "FONTNAME"           ; cFontName := Arg4                               ; RetVal := .T. ; EXIT
   CASE "FONTSIZE"           ; nFontSize := Arg4                               ; RetVal := .T. ; EXIT
   CASE "FONTBOLD"           ; lBold := Arg4                                   ; RetVal := .T. ; EXIT
   CASE "FONTITALIC"         ; lItalic := Arg4                                 ; RetVal := .T. ; EXIT
   CASE "FONTUNDERLINE"      ; lUnderline := Arg4                              ; RetVal := .T. ; EXIT
   CASE "FONTSTRIKEOUT"      ; lStrikeout := Arg4                              ; RetVal := .T. ; EXIT
   CASE "FONTCOLOR"          ; aTextColor := Arg4                              ; RetVal := .T. ; EXIT
   CASE "FONTBACKCOLOR"      ; aBackColor := Arg4                              ; RetVal := .T. ; EXIT
   CASE "FONTSCRIPT"         ; nScript := Arg4                                 ; RetVal := .T. ; EXIT
   CASE "RTFTEXTMODE"        ; hmg_RichEditBox_SetRTFTextMode(hWndControl, Arg4)   ; RetVal := .T. ; EXIT
   CASE "AUTOURLDETECT"      ; hmg_RichEditBox_SetAutoURLDetect(hWndControl, Arg4) ; RetVal := .T. ; EXIT
   CASE "BACKGROUNDCOLOR"    ; hmg_RichEditBox_SetBkgndColor(hWndControl, Arg4)    ; RetVal := .T. ; EXIT
   CASE "ZOOM"
      nNumerator := Arg4 // in percentage
      nDenominator := 100
      hmg_RichEditBox_SetZoom(hWndControl, nNumerator, nDenominator)               ; RetVal := .T. ; EXIT
   CASE "SELECTRANGE"        ; hmg_RichEditBox_SetSelRange(hWndControl, Arg4)      ; RetVal := .T. ; EXIT
   CASE "CARETPOS"           ; RichEditBox_SetCaretPos(hWndControl, Arg4)      ; RetVal := .T. ; EXIT
   CASE "VALUE"              ; hmg_RichEditBox_SetText(hWndControl, .F., Arg4)     ; RetVal := .T. ; EXIT
   CASE "ADDTEXT"
      Arg4 := IIF(!hb_isNumeric(Arg4), -1, Arg4)
      RichEditBox_SetCaretPos(hWndControl, Arg4)
      hmg_RichEditBox_SetText(hWndControl, .T., Arg5)                              ; RetVal := .T. ; EXIT
   CASE "ADDTEXTANDSELECT"
      Arg4 := IIF(!hb_isNumeric(Arg4), -1, Arg4)
      RichEditBox_AddTextAndSelect(hWndControl, Arg4, Arg5)                    ; RetVal := .T. ; EXIT
   CASE "PARAALIGNMENT"      ; nAlignment := Arg4                              ; RetVal := .T. ; EXIT
   CASE "PARANUMBERING"      ; nNumbering := Arg4                              ; RetVal := .T. ; EXIT
   CASE "PARANUMBERINGSTYLE" ; nNumberingStyle := Arg4                         ; RetVal := .T. ; EXIT
   CASE "PARANUMBERINGSTART" ; nNumberingStart := Arg4                         ; RetVal := .T. ; EXIT
   CASE "PARAOFFSET"         ; ndOffset := Arg4 /* in millimeters */           ; RetVal := .T. ; EXIT
   CASE "PARALINESPACING"    ; ndLineSpacing := Arg4                           ; RetVal := .T. ; EXIT
   CASE "PARAINDENT"         ; ndStartIndent := Arg4 /* in millimeters */      ; RetVal := .T. ; EXIT
   CASE "LINK"               ; lLink := Arg4                                   ; RetVal := .T. ; EXIT
   CASE "VIEWRECT"           ; hmg_RichEditBox_SetRect(hWndControl, Arg4)          ; RetVal := .T.
   ENDSWITCH

   hmg_RichEditBox_SetFont(hWndControl, cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeout, aTextColor, aBackColor, nScript, lLink)
   hmg_RichEditBox_SetParaFormat(hWndControl, nAlignment, nNumbering, nNumberingStyle, nNumberingStart, ndOffset, ndLineSpacing, ndStartIndent)

RETURN RetVal

STATIC FUNCTION _RichEditBox_DoMethod(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)

   LOCAL RetVal := .F.
   LOCAL hWndControl

   IF !hb_isChar(Arg1) .OR. !hb_isChar(Arg2) .OR. !hb_isChar(Arg3) .OR. ;
      !_IsControlDefined(Arg2, Arg1) .OR. (GetControlType(Arg2, Arg1) != CONTROL_TYPE_RICHEDIT) .OR. (_HMG_aControlMiscData1[GetControlIndex(Arg2, Arg1)] != 1)
      RETURN .F.
   ENDIF

   hWndControl := GetControlHandle(Arg2, Arg1)

   Arg3 := Upper(AllTrim(Arg3))

   SWITCH Arg3
   CASE "SELECTALL"       ; RichEditBox_SelectAll(hWndControl)                                    ; RetVal := .T. ; EXIT
   CASE "UNSELECTALL"     ; RichEditBox_UnSelectAll(hWndControl)                                  ; RetVal := .T. ; EXIT
   CASE "RTFLOADFILE"
   CASE "LOADFILE"
      RichEditBox_LoadFile(hWndControl, Arg4, Arg5, Arg6) /* by default load in SF_RTF format */  ; RetVal := .T. ; EXIT
   CASE "RTFSAVEFILE"
   CASE "SAVEFILE"
      RichEditBox_SaveFile(hWndControl, Arg4, Arg5, Arg6) /* by default save in SF_RTF format */  ; RetVal := .T. ; EXIT
   CASE "SELPASTESPECIAL" ; hmg_RichEditBox_PasteSpecial(hWndControl, Arg4)                           ; RetVal := .T. ; EXIT
   CASE "SELCOPY"         ; hmg_RichEditBox_SelCopy(hWndControl)                                      ; RetVal := .T. ; EXIT
   CASE "SELPASTE"        ; hmg_RichEditBox_SelPaste(hWndControl)                                     ; RetVal := .T. ; EXIT
   CASE "SELCUT"          ; hmg_RichEditBox_SelCut(hWndControl)                                       ; RetVal := .T. ; EXIT
   CASE "SELCLEAR"        ; hmg_RichEditBox_SelClear(hWndControl)                                     ; RetVal := .T. ; EXIT
   CASE "UNDO"            ; hmg_RichEditBox_ChangeUndo(hWndControl)                                   ; RetVal := .T. ; EXIT
   CASE "REDO"            ; hmg_RichEditBox_ChangeRedo(hWndControl)                                   ; RetVal := .T. ; EXIT
   CASE "CLEARUNDOBUFFER" ; hmg_RichEditBox_ClearUndoBuffer(hWndControl)                              ; RetVal := .T. ; EXIT
   CASE "RTFPRINT"        ; RichEditBox_RTFPrint(hWndControl, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9) ; RetVal := .T.
   ENDSWITCH

RETURN RetVal

FUNCTION GetFormNameByHandle(hWnd, /*@*/cFormName)

   LOCAL nIndexForm := GetFormIndexByHandle(hWnd)

   cFormName := ""

   IF nIndexForm > 0
      cFormName := GetFormNameByIndex(nIndexForm)
   ENDIF

RETURN nIndexForm

FUNCTION GetControlNameByHandle(hWnd, /*@*/cControlName, /*@*/cFormParentName)

   LOCAL nIndexControlParent
   LOCAL ControlParentHandle
   LOCAL nIndexControl := GetControlIndexByHandle(hWnd)

   cControlName := cFormParentName := ""

   IF nIndexControl > 0

      cControlName := GetControlNameByIndex(nIndexControl)
      ControlParentHandle := GetControlParentHandleByIndex(nIndexControl)
      IF ControlParentHandle != 0
         nIndexControlParent := GetFormIndexByHandle(ControlParentHandle)
         cFormParentName := GetFormNameByIndex(nIndexControlParent)
      ENDIF

   ENDIF

RETURN nIndexControl

FUNCTION GetFormIndexByHandle(hWnd, /*@*/nFormSubIndex1, /*@*/nFormSubIndex2)

   LOCAL FormHandle
   LOCAL nIndex := 0
   LOCAL i

   FOR i = 1 TO Len(_HMG_aFormHandles)

      FormHandle := _HMG_aFormHandles[i]

      IF HMG_CompareHandle(hWnd, FormHandle, @nFormSubIndex1, @nFormSubIndex2)
         nIndex := i
         EXIT
      ENDIF

   NEXT

RETURN nIndex

FUNCTION GetControlIndexByHandle(hWnd, /*@*/nControlSubIndex1, /*@*/nControlSubIndex2)

   LOCAL ControlHandle
   LOCAL nIndex := 0
   LOCAL i

   FOR i = 1 TO Len(_HMG_aControlHandles)

      ControlHandle := _HMG_aControlHandles[i]

      IF HMG_CompareHandle(hWnd, ControlHandle, @nControlSubIndex1, @nControlSubIndex2)
         nIndex := i
         EXIT
      ENDIF

   NEXT

RETURN nIndex

STATIC FUNCTION HMG_CompareHandle(Handle1, Handle2, /*@*/nSubIndex1, /*@*/nSubIndex2)

   LOCAL i
   LOCAL k

   nSubIndex1 := nSubIndex2 := 0

   IF hb_IsNumeric(Handle1) .AND. hb_IsNumeric(Handle2)
      IF Handle1 == Handle2
         RETURN .T.
      ENDIF

   ELSEIF hb_IsArray(Handle1) .AND. hb_IsNumeric(Handle2)
      FOR i = 1 TO Len(Handle1)
         IF Handle1[i] == Handle2
            nSubIndex1 := i
            RETURN .T.
         ENDIF
      NEXT

   ELSEIF hb_IsNumeric(Handle1) .AND. hb_IsArray(Handle2)
      FOR k = 1 TO Len(Handle2)
         IF Handle1 == Handle2[k]
            nSubIndex2 := k
            RETURN .T.
         ENDIF
      NEXT

   ELSEIF hb_IsArray(Handle1) .AND. hb_IsArray(Handle2)
      FOR i = 1 TO Len(Handle1)
         FOR k = 1 TO Len(Handle2)
            IF Handle1[i] == Handle2[k]
               nSubIndex1 := i
               nSubIndex2 := k
               RETURN .T.
            ENDIF
         NEXT
      NEXT
   ENDIF

RETURN .F.

STATIC FUNCTION _SetGetColumnHeadClick(ControlName, ParentForm, nColIndex, bAction)

   LOCAL nColumnCount
   LOCAL RetVal
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. "GRID" $ _HMG_aControlType[i]

      nColumnCount := hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])

      IF Len(_HMG_aControlHeadClick[i]) < nColumnCount
         ASize(_HMG_aControlHeadClick[i], nColumnCount)
      ENDIF

      IF nColIndex > 0 .AND. nColIndex <= Len(_HMG_aControlHeadClick[i])
         IF hb_IsBlock(bAction)
            _HMG_aControlHeadClick[i][nColIndex] := bAction
         ELSE
            RetVal := _HMG_aControlHeadClick[i][nColIndex]
         ENDIF
      ELSE
         MsgMiniGuiError("Grid: Invalid Column Index.")
      ENDIF

   ENDIF

RETURN RetVal

STATIC FUNCTION _GetColumnDisplayPosition(ControlName, ParentForm, nColIndex)

   LOCAL i
   LOCAL nColumns
   LOCAL aOrder

   i := GetControlIndex(ControlName, ParentForm)
   nColumns := hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])

   aOrder := hmg_ListView_GetColumnOrderArray(_HMG_aControlHandles[i], nColumns)

RETURN AScan(aOrder, nColIndex)

STATIC FUNCTION _SetColumnDisplayPosition(ControlName, ParentForm, nColIndex, nDisplayPos)

   LOCAL i
   LOCAL nColumns
   LOCAL aOrder
   LOCAL nOld

   i := GetControlIndex(ControlName, ParentForm)
   nColumns := hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])

   aOrder := hmg_ListView_GetColumnOrderArray(_HMG_aControlHandles[i], nColumns)

   IF (nOld := AScan(aOrder, nColIndex)) > 0 .AND. nDisplayPos != nOld

      ADel(aOrder, nOld)
      AIns(aOrder, nDisplayPos, nColIndex, .F.)

      hmg_ListView_SetColumnOrderArray(_HMG_aControlHandles[i], nColumns, aOrder)

   ENDIF

RETURN nOld

STATIC FUNCTION _SetGetGridProperty(ControlName, ParentForm, nControl, nColIndex, Value)

   LOCAL z As Numeric
   LOCAL i := GetControlIndex(ControlName, ParentForm)
   LOCAL nColumnCount
   LOCAL nRow
   LOCAL xCellValue
   LOCAL RetVal := .T.

   IF i > 0 .AND. "GRID" $ _HMG_aControlType[i]

      Assign z := nColIndex

      IF z > 0 .AND. z <= (nColumnCount := hmg_ListView_GetColumnCount(_HMG_aControlHandles[i]))

         IF PCount() > 4

            IF !hb_isArray(_HMG_aControlMiscData1[i][nControl])
               _HMG_aControlMiscData1[i][nControl] := {}
            ENDIF
            IF Len(_HMG_aControlMiscData1[i][nControl]) < nColumnCount
               ASize(_HMG_aControlMiscData1[i][nControl], nColumnCount)
            ENDIF

            _HMG_aControlMiscData1[i][nControl][z] := Value

            IF nControl == _GRID_COLUMN_CONTROL_
               FOR nRow := 1 TO hmg_ListViewGetItemCount(_HMG_aControlHandles[i])
                  xCellValue := _GetGridCellValue(ControlName, ParentForm, nRow, nColIndex)
                  _SetGridCellValue(ControlName, ParentForm, nRow, nColIndex, xCellValue)
               NEXT
            ELSEIF nControl == _GRID_COLUMN_JUSTIFY_
               hmg_SetGridColumnJustify(_HMG_aControlHandles[i], z, Value)
            ENDIF

            DoMethod(ParentForm, ControlName, "Refresh")

         ELSE

            IF hb_IsArray(_HMG_aControlMiscData1[i][nControl])
               RetVal := _HMG_aControlMiscData1[i][nControl][z]
            ENDIF

         ENDIF

      ENDIF

   ENDIF

RETURN RetVal

#endif

PROCEDURE _Refresh(i)

   LOCAL Field
   LOCAL rows
   LOCAL BackValue
   LOCAL T

   T := _HMG_aControlType[i]

   SWITCH T

   // CASE "TEXT" $ T
   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_TEXT
      _DataTextBoxRefresh(i)
      EXIT

   CASE CONTROL_TYPE_COMBO
      IF hb_IsChar(_HMG_aControlSpacing[i])
         _DataComboRefresh(i)
      ELSEIF _HMG_aControlMiscData1[i][1] != 1  // GF 03/30/16
         t := _HMG_aControlHandles[i]
         rows := _HMG_aControlMiscData1[i][4]
         BackValue := _GetValue(NIL, NIL, i)
         ComboboxReset(t)
         AEval(rows, {|v|hmg_ComboAddString(t, v)})
         IF BackValue > 0 .AND. BackValue <= Len(rows)
            _SetValue(NIL, NIL, BackValue, i)
         ENDIF
      ENDIF
      EXIT

   // CASE "PICK" $ T
   CASE CONTROL_TYPE_DATEPICK
   CASE CONTROL_TYPE_TIMEPICK
   CASE CONTROL_TYPE_CHECKBOX
   CASE CONTROL_TYPE_RICHEDIT
   CASE CONTROL_TYPE_CHECKLABEL
      Field := _HMG_aControlPageMap[i]
      IF Field != NIL
         IF T == CONTROL_TYPE_CHECKLABEL
            SetProperty(GetParentFormName(i), _HMG_aControlNames[i], "Checked", &Field)
         ELSE
            _SetValue(NIL, NIL, &Field, i)
         ENDIF
      ELSE
         MsgMiniGuiError("Control: " + _HMG_aControlNames[i] + " Of " + GetParentFormName(i) + " : Refresh method can be used only if FIELD clause is set.")
      ENDIF
      EXIT

   CASE CONTROL_TYPE_EDIT
      _DataEditBoxRefresh(i)
      EXIT

   CASE CONTROL_TYPE_GETBOX
      _DataGetBoxRefresh(i)
      EXIT

#ifdef _DBFBROWSE_
   CASE CONTROL_TYPE_BROWSE
      _BrowseRefresh("", "", i)
      EXIT
#endif

   // CASE "GRID" $ T
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
      IF _HMG_aControlMiscData1[i][5]
         hmg_ListView_SetItemCount(_HMG_aControlHandles[i], hmg_ListViewGetItemCount(_HMG_aControlHandles[i]))
      ENDIF
      _UpdateGridColors(i)
      EXIT

   OTHERWISE
      hmg_RedrawWindowControlRect(_HMG_aControlParentHandles[i], _HMG_aControlRow[i], _HMG_aControlCol[i], _HMG_aControlRow[i] + _HMG_aControlHeight[i], _HMG_aControlCol[i] + _HMG_aControlWidth[i])

   ENDSWITCH

RETURN

PROCEDURE _SaveData(ControlName, ParentForm)

   LOCAL Field
   LOCAL i
   LOCAL T

   i := GetControlIndex(ControlName, ParentForm)
   T := _HMG_aControlType[i]

   SWITCH T

   // CASE "TEXT" $ T
   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_TEXT
      _DataTextBoxSave(ControlName, ParentForm)
      EXIT

   // CASE "PICK" $ T
   CASE CONTROL_TYPE_DATEPICK
   CASE CONTROL_TYPE_TIMEPICK
   CASE CONTROL_TYPE_CHECKBOX
   CASE CONTROL_TYPE_EDIT
   CASE CONTROL_TYPE_CHECKLABEL
      Field := _HMG_aControlPageMap[i]
      IF _IsFieldExists(Field)
         IF T == CONTROL_TYPE_CHECKLABEL
            REPLACE &Field WITH GetProperty(ParentForm, ControlName, "Checked")
         ELSE
            REPLACE &Field WITH _GetValue(ControlName, ParentForm)
         ENDIF
      ENDIF
      EXIT

   CASE CONTROL_TYPE_RICHEDIT  // JP Exp. build 8
      _DataBaseRichEditBoxSave(ControlName, ParentForm)
      EXIT

   CASE CONTROL_TYPE_GETBOX
      _DataGetBoxSave(ControlName, ParentForm)

   ENDSWITCH

RETURN

#define HWND_TOPMOST    (-1)
#define HWND_NOTOPMOST  (-2)

STATIC PROCEDURE _ChangeWindowTopmostStyle(FormHandle, Value)

   LOCAL lTopmost As Logical

   Assign lTopmost := Value

   hmg_SetWindowPos(FormHandle, iif(lTopmost, HWND_TOPMOST, HWND_NOTOPMOST), 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE)

RETURN

STATIC PROCEDURE _ChangeWindowHelpButtonStyle(FormName, Value)

   LOCAL lHelpButton As Logical
   LOCAL h := GetFormHandle(FormName)

   Assign lHelpButton := Value

   IF lHelpButton
      hmg_ChangeStyle(h, WS_EX_CONTEXTHELP, NIL, .T.)
   ELSE
      hmg_ChangeStyle(h, NIL, WS_EX_CONTEXTHELP, .T.)
   ENDIF

   _ChangeWindowStyle(FormName, WS_MAXIMIZEBOX, !lHelpButton)
   _ChangeWindowStyle(FormName, WS_MINIMIZEBOX, !lHelpButton)

RETURN

STATIC PROCEDURE _ChangeWindowStyle(FormName, Style, Value)

   LOCAL lSwitch As Logical
   LOCAL h := GetFormHandle(FormName)

   Assign lSwitch := Value

   hmg_SetWindowStyle(h, Style, lSwitch)

   h := GetWindowHeight(h)  // store the current height of the window

   // Refresh Title
   _SetWindowSizePos(FormName, NIL, NIL, NIL, 0)
   _SetWindowSizePos(FormName, NIL, NIL, NIL, h)

RETURN

FUNCTION _SetWindowBackColor(FormHandle, aColor)

   LOCAL hBrush
   LOCAL i

   IF (i := AScan(_HMG_aFormHandles, FormHandle)) > 0

      IF hmg_GetObjectType(_HMG_aFormBrushHandle[i]) == OBJ_BRUSH
         hmg_DeleteObject(_HMG_aFormBrushHandle[i])
      ENDIF

      hBrush := hmg_PaintBkGnd(FormHandle, aColor)

      SetWindowBackground(FormHandle, hBrush)

      _HMG_aFormBkColor[i] := aColor
      _HMG_aFormBrushHandle[i] := hBrush

      hmg_RedrawWindow(FormHandle)

   ENDIF

RETURN NIL

FUNCTION _GetFocusedControl(cFormName)

   LOCAL cRetVal As String
   LOCAL hFormHandle := GetFormHandle(cFormName)
   LOCAL hControl
   LOCAL hCtrl
   LOCAL hFocusedControlHandle
   LOCAL i

   IF (hFocusedControlHandle := hmg_GetFocus()) != 0

      FOR EACH hControl IN _HMG_aControlHandles

         i := hb_enumindex(hControl)

         IF _HMG_aControlParentHandles[i] == hFormHandle

            IF hb_IsNumeric(hControl)

               IF hControl == hFocusedControlHandle .OR. ;
                  (!Empty(_HMG_aControlType[i]) .AND. (hb_IsNumeric(_HMG_aControlRangeMin[i]) .AND. _HMG_aControlRangeMin[i] == hFocusedControlHandle) .OR. ;
                  (hb_IsNumeric(_HMG_aControlRangeMax[i]) .AND. _HMG_aControlRangeMax[i] == hFocusedControlHandle)) .OR. ;
                  (hb_IsArray(_HMG_aControlSpacing[i]) .AND. hb_IsNumeric(_HMG_aControlSpacing[i][1]) .AND. AScan(_HMG_aControlSpacing[i], hFocusedControlHandle) > 0) .OR. ;
                  (hb_IsArray(_HMG_aControlRangeMin[i]) .AND. hb_IsNumeric(_HMG_aControlRangeMin[i][1]) .AND. AScan(_HMG_aControlRangeMin[i], hFocusedControlHandle) > 0)

                  cRetVal := _HMG_aControlNames[i]

               ENDIF

            ELSEIF hb_IsArray(hControl)

               FOR EACH hCtrl IN hControl

                  IF hCtrl == hFocusedControlHandle
                     cRetVal := _HMG_aControlNames[i]
                     EXIT
                  ENDIF

               NEXT

            ENDIF

         ENDIF

         IF !Empty(cRetVal)
            EXIT
         ENDIF

      NEXT

   ENDIF

RETURN cRetVal

STATIC FUNCTION _SetFontColor(ControlName, ParentForm, Value)

   LOCAL default := hmg_GetSysColor(COLOR_WINDOWTEXT)
   LOCAL i
   LOCAL t
   LOCAL c

   IF value == NIL
      RETURN NIL
   ENDIF

   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)

   SWITCH t

   // CASE "GRID" $ t
   CASE CONTROL_TYPE_GRID
   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_PROPGRID
   CASE CONTROL_TYPE_BROWSE
      hmg_ListView_SetTextColor(c, value[1], value[2], value[3])
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_RICHEDIT
      hmg_SetFontRTF(c, -1, _HMG_aControlFontName[i], _HMG_aControlFontSize[i], _HMG_aControlFontAttributes[i][1], _HMG_aControlFontAttributes[i][2], ;
         RGB(Value[1], Value[2], Value[3]), _HMG_aControlFontAttributes[i][3], _HMG_aControlFontAttributes[i][4])
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_DATEPICK
      _HMG_aControlFontColor[i] := Value
      SetDatePickFontColor(c, value[1], value[2], value[3])
      EXIT

   CASE CONTROL_TYPE_MONTHCAL
      _HMG_aControlFontColor[i] := Value
      IF _HMG_IsThemed .AND. IsArrayRGB(Value)
         hmg_SetWindowTheme(c, "", "")
         hmg_SetPosMonthCal(c, _HMG_aControlCol[i], _HMG_aControlRow[i])
         _HMG_aControlWidth[i] := GetWindowWidth(c)
         _HMG_aControlHeight[i] := GetWindowHeight(c)
      ENDIF
      SetMonthCalFontColor(c, value[1], value[2], value[3])
      EXIT

   CASE CONTROL_TYPE_PROGRESSBAR
      _HMG_aControlFontColor[i] := Value
      IF _HMG_IsThemed .AND. IsArrayRGB(Value)
         hmg_SetWindowTheme(c, "", "")
      ENDIF
      SetProgressBarBarColor(c, value[1], value[2], value[3])
      EXIT

   CASE CONTROL_TYPE_TREE
      _HMG_aControlFontColor[i] := Value
      TreeView_SetTextColor(c, value)
      EXIT

   // CASE "TEXT" $ t
   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_EDIT
      _HMG_aControlFontColor[i] := iif(hb_IsArray(Value), Value, nRGB2Arr(default))
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_CHECKLABEL
      _HMG_aControlFontColor[i] := Value
      hmg_RedrawWindowControlRect(_HMG_aControlParentHandles[i], _HMG_aControlRow[i], _HMG_aControlCol[i], _HMG_aControlRow[i] + _HMG_aControlHeight[i], _HMG_aControlCol[i] + _HMG_aControlWidth[i])
      EXIT

   OTHERWISE
      _HMG_aControlFontColor[i] := Value
      _RedrawControl(i)

   ENDSWITCH

RETURN NIL

STATIC FUNCTION _SetBackColor(ControlName, ParentForm, Value)

   LOCAL f := hmg_GetSysColor(COLOR_3DFACE)
   LOCAL d := hmg_GetSysColor(COLOR_WINDOW)
   LOCAL i
   LOCAL t
   LOCAL c

   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)
   c := GetControlHandle(ControlName, ParentForm)

   IF Value == NIL
      RETURN NIL
   ENDIF

   SWITCH t

   CASE CONTROL_TYPE_SLIDER
      _HMG_aControlBkColor[i] := Value
      hmg_RedrawWindow(c)
      f := hmg_GetFocus()
      hmg_setfocus(c)
      hmg_setfocus(f)
      EXIT

   CASE CONTROL_TYPE_MULTIGRID
   CASE CONTROL_TYPE_BROWSE
      hmg_ListView_SetBkColor(c, Value[1], Value[2], Value[3])
      hmg_ListView_SetTextBkColor(c, Value[1], Value[2], Value[3])
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_RICHEDIT
      hmg_SendMessage(c, EM_SETBKGNDCOLOR, 0, RGB(Value[1], Value[2], Value[3]))
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_DATEPICK
      _HMG_aControlBkColor[i] := Value
      SetDatePickBkColor(c, Value[1], Value[2], Value[3])
      EXIT

   CASE CONTROL_TYPE_MONTHCAL
      _HMG_aControlBkColor[i] := Value
      IF _HMG_IsThemed .AND. IsArrayRGB(Value)
         hmg_SetWindowTheme(c, "", "")
         hmg_SetPosMonthCal(c, _HMG_aControlCol[i], _HMG_aControlRow[i])
         _HMG_aControlWidth[i] := GetWindowWidth(c)
         _HMG_aControlHeight[i] := GetWindowHeight(c)
      ENDIF
      SetMonthCalMonthBkColor(c, Value[1], Value[2], Value[3])
      EXIT

   CASE CONTROL_TYPE_PROGRESSBAR
      _HMG_aControlBkColor[i] := Value
      IF _HMG_IsThemed .AND. IsArrayRGB(Value)
         hmg_SetWindowTheme(c, "", "")
      ENDIF
      SetProgressBarBkColor(c, Value[1], Value[2], Value[3])
      EXIT

   CASE CONTROL_TYPE_TREE
      _HMG_aControlBkColor[i] := Value
      TreeView_SetBkColor(c, Value)
      EXIT

   CASE CONTROL_TYPE_TAB
      IF IsArrayRGB(Value) .AND. Value[1] != -1
         IF !hb_IsArray(_HMG_aControlBkColor[i])
            hmg_ChangeStyle(c, TCS_OWNERDRAWFIXED)
         ENDIF
         _HMG_aControlBkColor[i] := Value
         hmg_DeleteObject(_HMG_aControlBrushHandle[i])
         _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(Value[1], Value[2], Value[3])
         hmg_SetWindowBrush(c, _HMG_aControlBrushHandle[i])
      ENDIF
      EXIT

   // CASE "TEXT" $ t
   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_EDIT
   CASE CONTROL_TYPE_GETBOX
      _HMG_aControlBkColor[i] := iif(hb_IsArray(Value), Value, {nRGB2Arr(d), nRGB2Arr(f), nRGB2Arr(d)})
      hmg_RedrawWindow(c)
      EXIT

   CASE CONTROL_TYPE_IMAGE
      _HMG_aControlSpacing[i] := iif(hb_IsArray(Value), RGB(Value[1], Value[2], Value[3]), Value)
      _SetPicture(ControlName, ParentForm, _HMG_aControlPicture[i])
      EXIT

   OTHERWISE
      _HMG_aControlBkColor[i] := Value
      _RedrawControl(i)

   ENDSWITCH

RETURN NIL

STATIC FUNCTION _GetFontColor(ControlName, ParentForm)

   LOCAL RetVal
   LOCAL nTmp
   LOCAL i
   LOCAL t

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      t := GetControlType(ControlName, ParentForm)

      IF t == CONTROL_TYPE_GRID .OR. t == CONTROL_TYPE_MULTIGRID .OR. t == CONTROL_TYPE_PROPGRID .OR. t == CONTROL_TYPE_BROWSE
         nTmp := hmg_ListView_GetTextColor(_HMG_aControlHandles[i])
         RetVal := nRGB2Arr(nTmp)
      ELSE
         RetVal := _HMG_aControlFontColor[i]
      ENDIF

   ENDIF

RETURN RetVal

FUNCTION _GetBackColor(ControlName, ParentForm)

   LOCAL RetVal
   LOCAL nTmp
   LOCAL i
   LOCAL t

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      t := GetControlType(ControlName, ParentForm)

      IF t == CONTROL_TYPE_MULTIGRID .OR. t == CONTROL_TYPE_BROWSE
         nTmp := hmg_ListView_GetBkColor(_HMG_aControlHandles[i])
         RetVal := nRGB2Arr(nTmp)
      ELSE
         RetVal := _HMG_aControlBkColor[i]
      ENDIF

   ENDIF

RETURN RetVal

STATIC PROCEDURE _SetGridColumnWidthLimits(ControlName, ParentForm, aLimits)

   LOCAL lError := .F.
   LOCAL i
   LOCAL z
   LOCAL w

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0
      IF hb_IsArray(aLimits)
         IF Len(aLimits) == hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])
            FOR z := 1 TO Len(aLimits)
               IF !hb_isArray(aLimits[z]) .OR. !hb_isNumeric(aLimits[z][1]) .OR. !hb_isNumeric(aLimits[z][2])
                  lError := .T.
                  EXIT
               ENDIF
            NEXT z
         ELSE
            lError := .T.
         ENDIF
      ELSE
         lError := .T.
      ENDIF
   ELSE
      lError := .T.
   ENDIF

   IF !lError
      _HMG_aControlMiscData1[i][25] := aLimits
      // Force Column Width modification
      FOR z := 1 TO Len(aLimits)
         w := _HMG_aControlMiscData1[i][2][z]
         IF w < aLimits[z, 1]
            _SetColumnWidth(ControlName, Parentform, z, aLimits[z, 1])
         ENDIF
         IF w > aLimits[z, 2]
            _SetColumnWidth(ControlName, Parentform, z, aLimits[z, 2])
         ENDIF
      NEXT z
   ENDIF

RETURN

STATIC PROCEDURE _SetRadioGroupOptions(ControlName, ParentForm, aOptions)

   LOCAL i
   LOCAL nOptions

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. hb_IsArray(aOptions)
      nOptions := Len(_HMG_aControlCaption[i])
      IF Len(aOptions) >= nOptions
         FOR i := 1 to nOptions
            SetProperty(ParentForm, ControlName, "Caption", i, aOptions[i])
         NEXT i
      ENDIF
   ENDIF

RETURN

STATIC PROCEDURE _SetRadioGroupSpacing(ControlName, ParentForm, nSpacing)

   LOCAL i
   LOCAL r
   LOCAL c

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      _HMG_aControlSpacing[i] := nSpacing
      r := _HMG_aControlRow[i] - iif(_HMG_aControlContainerRow[i] == -1, 0, _HMG_aControlContainerRow[i])
      c := _HMG_aControlCol[i] - iif(_HMG_aControlContainerCol[i] == -1, 0, _HMG_aControlContainerCol[i])

      _SetControlSizePos(ControlName, ParentForm, r, c, _HMG_aControlWidth[i], _HMG_aControlHeight[i])
      _RedrawControl(i)

   ENDIF

RETURN

STATIC PROCEDURE _SetRadioGroupReadOnly(ControlName, ParentForm, aReadOnly)

   LOCAL aHandles
   LOCAL aOptions
   LOCAL lError := .F.
   LOCAL i
   LOCAL z

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0
      aHandles := _HMG_aControlHandles[i]
      aOptions := _HMG_aControlCaption[i]
      IF hb_IsArray(aReadOnly)
         IF Len(aReadOnly) == Len(aOptions)
            FOR z := 1 TO Len(aReadOnly)
               IF hb_IsLogical(aReadOnly[z])
                  IF aReadOnly[z]
                     hmg_DisableWindow(aHandles[z])
                  ELSE
                     hmg_EnableWindow(aHandles[z])
                  ENDIF
               ELSE
                  lError := .T.
                  EXIT
               ENDIF
            NEXT z
         ELSE
            lError := .T.
         ENDIF
      ELSE
         lError := .T.
      ENDIF
   ELSE
      lError := .T.
   ENDIF

   IF !lError

      _HMG_aControlPageMap[i] := aReadOnly

      IF (z := _GetValue(NIL, NIL, i)) > 0 .AND. aReadOnly[z]
         _SetValue(NIL, NIL, AScan(aReadOnly, .F.), i)
      ENDIF

   ENDIF

RETURN

STATIC PROCEDURE _SetTextEditReadOnly(ControlName, ParentForm, Value)

   LOCAL lValue As Logical
   LOCAL i
   LOCAL t

   Assign lValue := Value

   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)

   SWITCH t

   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_SPINNER
      _HMG_aControlMiscData1[i, 2] := lValue
      EXIT

   CASE CONTROL_TYPE_GETBOX
      _HMG_aControlMiscData1[i, 2] := lValue
      _HMG_aControlProcedures[i] := iif(lValue, NIL, _HMG_aControlMiscData1[i, 4])
      _HMG_aControlDblClick[i] := iif(lValue, NIL, _HMG_aControlMiscData1[i, 5])
      EXIT

   CASE CONTROL_TYPE_EDIT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_BTNNUMTEXT
      _HMG_aControlMiscData1[i, 3] := lValue

   ENDSWITCH

   IF _HMG_aControlEnabled[i]

      SWITCH t
      CASE CONTROL_TYPE_SPINNER
         hmg_SendMessage(_HMG_aControlHandles[i][1], EM_SETREADONLY, iif(lValue, 1, 0), 0)
         EXIT
      CASE CONTROL_TYPE_COMBO
         IF _HMG_aControlMiscData1[i][2]
            hmg_SendMessage(_HMG_aControlRangeMin[i], EM_SETREADONLY, iif(lValue, 1, 0), 0)
         ENDIF
         EXIT
      CASE CONTROL_TYPE_BTNNUMTEXT
      CASE CONTROL_TYPE_BTNTEXT
      CASE CONTROL_TYPE_CHARMASKTEXT
      CASE CONTROL_TYPE_MASKEDTEXT
      CASE CONTROL_TYPE_NUMTEXT
      CASE CONTROL_TYPE_EDIT
      CASE CONTROL_TYPE_GETBOX
         hmg_SendMessage(_HMG_aControlHandles[i], EM_SETREADONLY, iif(lValue, 1, 0), 0)
      ENDSWITCH

   ENDIF

RETURN

PROCEDURE _SetStatusIcon(ControlName, ParentForm, Item, Icon)

   LOCAL nItem As Numeric
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0
      Assign nItem := Item
      hmg_SetStatusItemIcon(_HMG_aControlHandles[i], nItem, Icon)
   ENDIF

RETURN

PROCEDURE _SetStatusWidth(ParentForm, Item, Size)

   LOCAL nItem As Numeric
   LOCAL nSize As Numeric
   LOCAL FormHandle
   LOCAL StatusHandle
   LOCAL aWidths

   IF (StatusHandle := GetControlHandle("StatusBar", ParentForm)) > 0
      Assign nItem := Item
      Assign nSize := Size

      FormHandle := GetFormHandle(ParentForm)
      aWidths := _GetStatusItemWidth(FormHandle)
      _SetStatusItemWidth(nItem, nSize, FormHandle)

      aWidths[nItem] := nSize
      hmg_SetStatusBarSize(StatusHandle, aWidths)
      hmg_RefreshItemBar(StatusHandle, _GetStatusItemWidth(FormHandle, 1))
   ENDIF

RETURN

FUNCTION _GetControlFree()

   LOCAL k := AScan(_HMG_aControlDeleted, .T.)

#ifdef _HMG_COMPAT_
   IF k == 0 .OR. (__mvExist("_HMG_SYSDATA[443][k]") .AND. _HMG_StopControlEventProcedure[k])
#else
   IF k == 0
#endif
      k := Len(_HMG_aControlNames) + 1

      AAdd(_HMG_aControlType              , NIL)
      AAdd(_HMG_aControlNames             , NIL)
      AAdd(_HMG_aControlHandles           , NIL)
      AAdd(_HMG_aControlParenthandles     , NIL)
      AAdd(_HMG_aControlIds               , NIL)
      AAdd(_HMG_aControlProcedures        , NIL)
      AAdd(_HMG_aControlPageMap           , NIL)
      AAdd(_HMG_aControlValue             , NIL)
      AAdd(_HMG_aControlInputMask         , NIL)
      AAdd(_HMG_aControllostFocusProcedure, NIL)
      AAdd(_HMG_aControlGotFocusProcedure , NIL)
      AAdd(_HMG_aControlChangeProcedure   , NIL)
      AAdd(_HMG_aControlDeleted           , NIL)
      AAdd(_HMG_aControlBkColor           , NIL)
      AAdd(_HMG_aControlFontColor         , NIL)
      AAdd(_HMG_aControlDblClick          , NIL)
      AAdd(_HMG_aControlHeadClick         , NIL)
      AAdd(_HMG_aControlRow               , NIL)
      AAdd(_HMG_aControlCol               , NIL)
      AAdd(_HMG_aControlWidth             , NIL)
      AAdd(_HMG_aControlHeight            , NIL)
      AAdd(_HMG_aControlSpacing           , NIL)
      AAdd(_HMG_aControlContainerRow      , NIL)
      AAdd(_HMG_aControlContainerCol      , NIL)
      AAdd(_HMG_aControlPicture           , NIL)
      AAdd(_HMG_aControlContainerHandle   , NIL)
      AAdd(_HMG_aControlFontName          , NIL)
      AAdd(_HMG_aControlFontSize          , NIL)
      AAdd(_HMG_aControlFontAttributes    , NIL)
      AAdd(_HMG_aControlToolTip           , NIL)
      AAdd(_HMG_aControlRangeMin          , NIL)
      AAdd(_HMG_aControlRangeMax          , NIL)
      AAdd(_HMG_aControlCaption           , NIL)
      AAdd(_HMG_aControlVisible           , NIL)
      AAdd(_HMG_aControlHelpId            , NIL)
      AAdd(_HMG_aControlFontHandle        , NIL)
      AAdd(_HMG_aControlBrushHandle       , NIL)
      AAdd(_HMG_aControlEnabled           , NIL)
      AAdd(_HMG_aControlMiscData1         , NIL)
      AAdd(_HMG_aControlMiscData2         , NIL)
#ifdef _HMG_COMPAT_
      AAdd(_HMG_StopControlEventProcedure, .F.)
#endif
   ENDIF

RETURN k

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.1 Experimental Build 10b
// enable/disable screen update for List controls
FUNCTION _EnableListViewUpdate(ControlName, ParentForm, lEnable)

   LOCAL i
   LOCAL t

   i := GetControlIndex(ControlName, ParentForm)
   t := _HMG_aControlType[i]

   IF t == CONTROL_TYPE_GRID .OR. ;
      t == CONTROL_TYPE_MULTIGRID .OR. ;
      t == CONTROL_TYPE_PROPGRID .OR. ;
      t == CONTROL_TYPE_COMBO .OR. ;
      t == CONTROL_TYPE_BROWSE .OR. ;
      t == CONTROL_TYPE_TBROWSE .OR. ;
      t == CONTROL_TYPE_TREE
      hmg_SendMessage(_HMG_aControlHandles[i], WM_SETREDRAW, iif(lEnable, 1, 0), 0)
      _HMG_aControlEnabled[i] := lEnable
   ELSE
      MsgMiniGuiError("Method " + iif(lEnable, "En", "Dis") + "ableUpdate is not available for control " + ControlName)
   ENDIF

RETURN NIL

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.1 Experimental Build 12a
// Extend disable/enable control without change any control properties
FUNCTION _ExtDisableControl(ControlName, ParentForm)

   LOCAL hWnd := GetControlHandle(ControlName, ParentForm)
   LOCAL icp
   LOCAL icpe
#ifdef _DBFBROWSE_
   LOCAL idx
#endif
   IF hmg_IsWindowEnabled(hWnd)

      icp  := hmg_HiWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      icpe := hmg_LoWord(hmg_SendMessage(hWnd, EM_GETSEL, 0, 0))
      hmg_ChangeStyle(hWnd, WS_DISABLED, NIL, .F.)
      IF icp != icpe
         hmg_SendMessage(hWnd, EM_SETSEL, icpe, icpe)
      ENDIF
      hmg_HideCaret(hWnd)
#ifdef _DBFBROWSE_
      IF GetControlType(ControlName, ParentForm) == CONTROL_TYPE_BROWSE
         idx := GetControlIndex(ControlName, ParentForm)
         IF _HMG_aControlIds[idx] != 0
            hmg_ChangeStyle(_HMG_aControlIds[idx], WS_DISABLED, NIL, .F.)
         ENDIF
      ENDIF
#endif
   ENDIF

RETURN NIL

FUNCTION _ExtEnableControl(ControlName, ParentForm)

   LOCAL hWnd := GetControlHandle(ControlName, ParentForm)
#ifdef _DBFBROWSE_
   LOCAL idx
#endif
   IF !hmg_IsWindowEnabled(hWnd)

      hmg_ChangeStyle(hWnd, NIL, WS_DISABLED, .F.)
      hmg_ShowCaret(hWnd)
#ifdef _DBFBROWSE_
      IF GetControlType(ControlName, ParentForm) == CONTROL_TYPE_BROWSE
         idx := GetControlIndex(ControlName, ParentForm)
         IF _HMG_aControlIds[idx] != 0
            hmg_ChangeStyle(_HMG_aControlIds[idx], NIL, WS_DISABLED, .F.)
         ENDIF
      ENDIF
#endif
   ENDIF

RETURN NIL

// Kevin Carmody <i@kevincarmody.com> 2007.04.23
// Set/Get RTF value of rich edit box.
STATIC FUNCTION _SetGetRichValue(ControlName, ParentForm, cValue, nType)

   LOCAL RetVal As String
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_RICHEDIT

      IF hb_IsChar(cValue)
         _DataRichEditBoxSetValue(ControlName, ParentForm, cValue, nType)
      ELSE
         RetVal := _DataRichEditBoxGetValue(ControlName, ParentForm, nType)
      ENDIF

   ENDIF

RETURN RetVal

// Kevin Carmody <i@kevincarmody.com> 2007.04.23
// Set/Get AutoFont mode of rich edit box.
STATIC FUNCTION _SetGetAutoFont(ControlName, ParentForm, lAuto)

   LOCAL RetVal
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_RICHEDIT

      IF hb_IsLogical(lAuto)
         hmg_SetAutoFontRTF(GetControlHandle(ControlName, ParentForm), lAuto)
      ELSE
         RetVal := hmg_GetAutoFontRTF(GetControlHandle(ControlName, ParentForm))
      ENDIF

   ENDIF

RETURN RetVal

// Eduardo Fernandes 2009/JUN/17
// Set/Get Grid Checkboxes item state.
STATIC FUNCTION _SetGetCheckboxItemState(ControlName, ParentForm, Item, lState)

   LOCAL RetVal As Logical
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. ;
      (_HMG_aControlType[i] == CONTROL_TYPE_GRID .OR. _HMG_aControlType[i] == CONTROL_TYPE_MULTIGRID .OR. _HMG_aControlType[i] == CONTROL_TYPE_PROPGRID)

      IF _HMG_aControlMiscData1[i][18]  // if checkboxes mode was activated

         IF hb_IsLogical(lState)
            hmg_ListView_SetCheckState(_HMG_aControlHandles[i], Item, lState)
         ELSE
            RetVal := hmg_ListView_GetCheckState(_HMG_aControlHandles[i], Item)
         ENDIF

      ENDIF

   ENDIF

RETURN RetVal

FUNCTION _GetId(nMax)

   LOCAL nRetVal

   hb_default(@nMax, 65536)

   REPEAT

      nRetVal := Random(nMax)

   UNTIL (AScan(_HMG_aControlIds, nRetVal) != 0)

RETURN nRetVal

FUNCTION IsArrayRGB(aColor)

   IF hb_IsArray(aColor) .AND. Len(aColor) == 3
      RETURN (aColor[1] != NIL .AND. aColor[2] != NIL .AND. aColor[3] != NIL)
   ENDIF

RETURN .F.

FUNCTION HMG_IsEqualArr(aData1, aData2)

   LOCAL x
   LOCAL lEqual := .T.

   IF Len(aData1) == Len(aData2)
      FOR x := 1 TO Len(aData1)
         IF ValType(aData1[x]) == ValType(aData2[x])
            IF hb_IsArray(aData1[x])
               lEqual := HMG_IsEqualArr(aData1[x], aData2[x])
            ELSE
               IF aData1[x] != aData2[x]
                  lEqual := .F.
                  EXIT
               ENDIF
            ENDIF
         ELSE
            lEqual := .F.
            EXIT
         ENDIF
      NEXT x
   ELSE
      lEqual := .F.
   ENDIF

RETURN lEqual

//(JK) HMG 1.0 Experimental Build 8
FUNCTION _IsComboExtend(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      IF GetControlType(ControlName, ParentForm) == CONTROL_TYPE_COMBO .AND. _HMG_aControlMiscData1[i][1] == 1
         RETURN .T.
      ENDIF

   ENDIF

RETURN .F.

PROCEDURE _DefineLetterOrDigitHotKey(Caption, i, FormName, bAction)

   LOCAL c := Asc(hb_USubStr(Caption, i + 1, 1))

   IF c >= 48 .AND. c <= 90
      _DefineHotKey(FormName, MOD_ALT, c, bAction)
   ENDIF

RETURN

STATIC FUNCTION _SetGetMinMaxInfo(cWindowName, nIndex, nValue)

   LOCAL RetVal
   LOCAL i

   IF (i := GetFormIndex(cWindowName)) > 0

      IF PCount() == 2
         RetVal := _HMG_aFormMinMaxInfo[i][nIndex]
      ELSE
         _HMG_aFormMinMaxInfo[i][nIndex] := nValue
      ENDIF

   ENDIF

RETURN RetVal

STATIC FUNCTION _SetGetImageHBitmap(ControlName, ParentForm, hBitmap)

   LOCAL hWnd
   LOCAL RetVal
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_IMAGE

      IF PCount() == 2

         RetVal := _HMG_aControlBrushHandle[i]

      ELSE

         IF hmg_GetObjectType(_HMG_aControlBrushHandle[i]) == OBJ_BITMAP
            hmg_DeleteObject(_HMG_aControlBrushHandle[i])
         ENDIF

         _HMG_aControlBrushHandle[i] := hBitmap
         hWnd := GetControlHandle(ControlName, ParentForm)

         hmg_SendMessage(hWnd, STM_SETIMAGE, IMAGE_BITMAP, hBitmap)

         IF Empty(_HMG_aControlValue[i])
            _HMG_aControlWidth[i] := GetWindowWidth(hWnd)
            _HMG_aControlHeight[i] := GetWindowHeight(hWnd)
         ENDIF

      ENDIF

   ENDIF

RETURN RetVal

FUNCTION _GetPicture(ControlName, ParentForm)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN ""
   ENDIF

RETURN iif(hb_IsArray(_HMG_aControlPicture[i]), _HMG_aControlPicture[i][1], _HMG_aControlPicture[i])

FUNCTION _GetCaption(ControlName, ParentForm)

   LOCAL cRetVal As String
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      IF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBAR .OR. ;
         _HMG_aControlType[i] == CONTROL_TYPE_TOOLBUTTON .OR. ;
         _HMG_aControlType[i] == CONTROL_TYPE_MENU .OR. ;
         _HMG_aControlType[i] == CONTROL_TYPE_RADIOGROUP
         cRetVal := _HMG_aControlCaption[i]
      ELSE
         cRetVal := hmg_GetWindowText(_HMG_aControlHandles[i])
      ENDIF

   ENDIF

RETURN cRetVal

FUNCTION _IsFieldExists(Field)

   LOCAL WorkArea := Alias()
   LOCAL x := hb_UAt(">", Field)

   IF x > 0
      WorkArea := hb_ULeft(Field, x - 2)
      Field := hb_URight(Field, hb_ULen(Field) - x)
   ENDIF

RETURN ((WorkArea)->(FieldPos(Field)) != 0)

FUNCTION _IsControlEnabled(ControlName, ParentForm, Position)

   LOCAL RetVal
   LOCAL i
   LOCAL t
   LOCAL w

   i := GetControlIndex(ControlName, ParentForm)
   t := GetControlType(ControlName, ParentForm)

   IF t == CONTROL_TYPE_MENU
      RetVal := _IsMenuItemEnabled(ControlName, ParentForm)

   ELSEIF t == CONTROL_TYPE_RADIOGROUP
      RetVal := hmg_IsWindowEnabled(_HMG_aControlHandles[i][hb_defaultValue(Position, 1)])

   ELSEIF t == CONTROL_TYPE_TAB .AND. hb_IsNumeric(Position)
      FOR EACH w IN _HMG_aControlPageMap[i][Position]

         IF !hb_isArray(w)
            RetVal := hmg_IsWindowEnabled(w)
         ELSE
            FOR EACH t IN w
               RetVal := hmg_IsWindowEnabled(t)
               IF RetVal
                  EXIT
               ENDIF
            NEXT
         ENDIF

         IF RetVal
            EXIT
         ENDIF

      NEXT

   ELSE
      RetVal := _HMG_aControlEnabled[i]

   ENDIF

RETURN RetVal

FUNCTION _IsControlDefined(ControlName, FormName)

   LOCAL mVar
   LOCAL i

   mVar := "_" + _NoQuote(FormName) + "_" + _NoQuote(ControlName)

#ifdef _NAMES_LIST_
   i := _GetNameList(mVar)
#else
   i := __mvGetDef(mVar, 0)
#endif
   IF i == 0
      RETURN .F.
   ENDIF

RETURN !_HMG_aControlDeleted[i]

FUNCTION _NoQuote(cStr)

RETURN CharRem(Chr(34) + Chr(39), cStr)

FUNCTION HMG_GetForms(cTyp, lObj)

   LOCAL i
   LOCAL lTyp
   LOCAL lHand
   LOCAL aNames := {}
#ifdef _OBJECT_
   LOCAL o
#endif

   cTyp := iif(hb_IsChar(cTyp), Upper(cTyp), "")
   lHand := iif(hb_IsLogical(lObj), !lObj, .F.)
   lObj := _HMG_lOOPEnabled .AND. !Empty(lObj)

   FOR i := 1 TO Len(_HMG_aFormNames)

      IF _HMG_aFormDeleted[i]
         LOOP
      ENDIF

      lTyp := iif(Empty(cTyp), .T., _HMG_aFormType[i] $ cTyp)
      IF lTyp

         IF lHand
            AAdd(aNames, _HMG_aFormHandles[i])
#ifdef _OBJECT_
         ELSEIF lObj
            o := do_obj(_HMG_aFormHandles[i])
            IF hb_IsObject(o)
               AAdd(aNames, o)
            ENDIF
#endif
         ELSE
            AAdd(aNames, _HMG_aFormNames[i])
         ENDIF

      ENDIF

   NEXT

RETURN aNames

FUNCTION HMG_GetFormControls(cFormName, cUserType)

   LOCAL aRetVal := {}
   LOCAL hWnd
   LOCAL h
   LOCAL i
   LOCAL x

   DEFAULT cUserType := "ALL"

   cUserType := Upper(cUserType)

   hWnd := GetFormHandle(cFormName)

   FOR EACH h IN _HMG_aControlParentHandles

      i := hb_enumindex(h)

      IF h == hWnd .AND. iif(cUserType == "ALL", .T., GetUserControlType(_HMG_aControlNames[i], GetParentFormName(i)) == cUserType)

         IF hb_IsNumeric(_HMG_aControlHandles[i])

            IF !Empty(_HMG_aControlNames[i])

               IF AScan(aRetVal, _HMG_aControlNames[i], NIL, NIL, .T.) == 0
                  AAdd(aRetVal, _HMG_aControlNames[i])
               ENDIF

            ENDIF

         ELSEIF hb_IsArray(_HMG_aControlHandles[i])

            FOR x := 1 TO Len(_HMG_aControlHandles[i])

               IF !Empty(_HMG_aControlNames[i])

                  IF AScan(aRetVal, _HMG_aControlNames[i], NIL, NIL, .T.) == 0
                     AAdd(aRetVal, _HMG_aControlNames[i])
                  ENDIF

               ENDIF

            NEXT x

         ENDIF

      ENDIF

   NEXT

RETURN ASort(aRetVal)

STATIC FUNCTION GetUserControlType(ControlName, ParentForm)

   LOCAL cRetName
   LOCAL i
   LOCAL t

   IF (i := GetControlIndex(ControlName, ParentForm)) == 0
      RETURN ""
   ENDIF

   t := _HMG_aControlType[i]
   cRetName := controlTypeToString(t)

   SWITCH t

   CASE CONTROL_TYPE_CHECKBOX
      IF hb_IsArray(_HMG_aControlPageMap[i])
         cRetName := "CHECKBUTTON"
      ENDIF
      EXIT

   CASE CONTROL_TYPE_COMBO
      IF _HMG_aControlMiscData1[i][1] == 0      // standard combo
         cRetName += "BOX"
      ELSEIF _HMG_aControlMiscData1[i][1] == 1  // extend combo
         cRetName += "BOXEX"
      ENDIF
      EXIT

   CASE CONTROL_TYPE_BTNNUMTEXT
   CASE CONTROL_TYPE_BTNTEXT
   CASE CONTROL_TYPE_CHARMASKTEXT
   CASE CONTROL_TYPE_MASKEDTEXT
   CASE CONTROL_TYPE_NUMTEXT
   CASE CONTROL_TYPE_TEXT
   CASE CONTROL_TYPE_EDIT
   CASE CONTROL_TYPE_RICHEDIT
      cRetName += "BOX"
      IF cRetName == "RICHEDITBOX" .AND. _HMG_aControlMiscData1[i] == 1
         cRetName += "EX"
      ENDIF
      EXIT

   CASE CONTROL_TYPE_CHKLIST
   //CASE CONTROL_TYPE_IMAGELIST
   CASE CONTROL_TYPE_LIST
   CASE CONTROL_TYPE_MULTICHKLIST
   CASE CONTROL_TYPE_MULTILIST
      cRetName += "BOX"
      EXIT

   CASE CONTROL_TYPE_DATEPICK
   CASE CONTROL_TYPE_TIMEPICK
      cRetName += "ER"
      EXIT

   CASE CONTROL_TYPE_MONTHCAL
      cRetName += "ENDAR"
      EXIT

   CASE CONTROL_TYPE_MESSAGEBAR
      cRetName := "STATUSBAR"
      EXIT

   CASE CONTROL_TYPE_ITEMMESSAGE
      cRetName := "STATUSITEM"

   ENDSWITCH

RETURN cRetName

// added by MAG (2023/05/16)
STATIC FUNCTION controlTypeToString(type)

   SWITCH type
   CASE CONTROL_TYPE_ACTIVEX       ; RETURN "ACTIVEX"
   CASE CONTROL_TYPE_ANIGIF        ; RETURN "ANIGIF"
   CASE CONTROL_TYPE_ANIMATEBOX    ; RETURN "ANIMATEBOX"
   CASE CONTROL_TYPE_ANIMATERES    ; RETURN "ANIMATERES"
   CASE CONTROL_TYPE_BROWSE        ; RETURN "BROWSE"
   CASE CONTROL_TYPE_BTNNUMTEXT    ; RETURN "BTNNUMTEXT"
   CASE CONTROL_TYPE_BTNTEXT       ; RETURN "BTNTEXT"
   CASE CONTROL_TYPE_BUTTON        ; RETURN "BUTTON"
   CASE CONTROL_TYPE_CHARMASKTEXT  ; RETURN "CHARMASKTEXT"
   CASE CONTROL_TYPE_CHECKBOX      ; RETURN "CHECKBOX"
   CASE CONTROL_TYPE_CHECKLABEL    ; RETURN "CHECKLABEL"
   CASE CONTROL_TYPE_CHKLIST       ; RETURN "CHKLIST"
   CASE CONTROL_TYPE_CLBUTTON      ; RETURN "CLBUTTON"
   CASE CONTROL_TYPE_COMBO         ; RETURN "COMBO"
   CASE CONTROL_TYPE_DATEPICK      ; RETURN "DATEPICK"
   CASE CONTROL_TYPE_EDIT          ; RETURN "EDIT"
   CASE CONTROL_TYPE_FONT          ; RETURN "FONT"
   CASE CONTROL_TYPE_FRAME         ; RETURN "FRAME"
   CASE CONTROL_TYPE_GETBOX        ; RETURN "GETBOX"
   CASE CONTROL_TYPE_GRID          ; RETURN "GRID"
   CASE CONTROL_TYPE_HOTKEY        ; RETURN "HOTKEY"
   CASE CONTROL_TYPE_HOTKEYBOX     ; RETURN "HOTKEYBOX"
   CASE CONTROL_TYPE_HYPERLINK     ; RETURN "HYPERLINK"
   CASE CONTROL_TYPE_IMAGE         ; RETURN "IMAGE"
   CASE CONTROL_TYPE_IMAGELIST     ; RETURN "IMAGELIST"
   CASE CONTROL_TYPE_IPADDRESS     ; RETURN "IPADDRESS"
   CASE CONTROL_TYPE_LABEL         ; RETURN "LABEL"
   CASE CONTROL_TYPE_LIST          ; RETURN "LIST"
   CASE CONTROL_TYPE_LISTBOX       ; RETURN "LISTBOX"
   CASE CONTROL_TYPE_MASKEDTEXT    ; RETURN "MASKEDTEXT"
   CASE CONTROL_TYPE_MENU          ; RETURN "MENU"
   CASE CONTROL_TYPE_MESSAGEBAR    ; RETURN "MESSAGEBAR"
   CASE CONTROL_TYPE_MONTHCAL      ; RETURN "MONTHCAL"
   CASE CONTROL_TYPE_MULTICHKLIST  ; RETURN "MULTICHKLIST"
   CASE CONTROL_TYPE_MULTIGRID     ; RETURN "MULTIGRID"
   CASE CONTROL_TYPE_MULTILIST     ; RETURN "MULTILIST"
   CASE CONTROL_TYPE_NUMTEXT       ; RETURN "NUMTEXT"
   CASE CONTROL_TYPE_OBUTTON       ; RETURN "OBUTTON"
   CASE CONTROL_TYPE_PAGER         ; RETURN "PAGER"
   CASE CONTROL_TYPE_PLAYER        ; RETURN "PLAYER"
   CASE CONTROL_TYPE_POPUP         ; RETURN "POPUP"
   CASE CONTROL_TYPE_PROGRESSBAR   ; RETURN "PROGRESSBAR"
   CASE CONTROL_TYPE_PROGRESSWHEEL ; RETURN "PROGRESSWHEEL"
   CASE CONTROL_TYPE_PROPGRID      ; RETURN "PROPGRID"
   CASE CONTROL_TYPE_RADIOGROUP    ; RETURN "RADIOGROUP"
   CASE CONTROL_TYPE_RICHEDIT      ; RETURN "RICHEDIT"
   CASE CONTROL_TYPE_SLIDER        ; RETURN "SLIDER"
   CASE CONTROL_TYPE_SPBUTTON      ; RETURN "SPBUTTON"
   CASE CONTROL_TYPE_SPINNER       ; RETURN "SPINNER"
   CASE CONTROL_TYPE_TAB           ; RETURN "TAB"
   CASE CONTROL_TYPE_TBROWSE       ; RETURN "TBROWSE"
   CASE CONTROL_TYPE_TEXT          ; RETURN "TEXT"
   CASE CONTROL_TYPE_TIMEPICK      ; RETURN "TIMEPICK"
   CASE CONTROL_TYPE_TIMER         ; RETURN "TIMER"
   CASE CONTROL_TYPE_TOOLBAR       ; RETURN "TOOLBAR"
   CASE CONTROL_TYPE_TOOLBUTTON    ; RETURN "TOOLBUTTON"
   CASE CONTROL_TYPE_TREE          ; RETURN "TREE"
   CASE CONTROL_TYPE_ITEMMESSAGE   ; RETURN "ITEMMESSAGE"
   CASE CONTROL_TYPE_RATING        ; RETURN "RATING"
   CASE CONTROL_TYPE_WEBCAM        ; RETURN "WEBCAM"
   ENDSWITCH

RETURN ""

FUNCTION _SetAlign(ControlName, ParentForm, cAlign)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      SWITCH cAlign
      CASE "LEFT"
         hmg_ChangeStyle(_HMG_aControlHandles[i], NIL, ES_CENTER + ES_RIGHT)
         EXIT
      CASE "RIGHT"
         hmg_ChangeStyle(_HMG_aControlHandles[i], ES_RIGHT, ES_CENTER + ES_RIGHT)
         EXIT
      CASE "CENTER"
         hmg_ChangeStyle(_HMG_aControlHandles[i], ES_CENTER, ES_CENTER + ES_RIGHT)
         EXIT
      CASE "VCENTER"
         hmg_ChangeStyle(_HMG_aControlHandles[i], SS_CENTERIMAGE)
         EXIT
      CASE "NOVCENTER"
         IF hmg_IsWindowHasStyle(_HMG_aControlHandles[i], SS_CENTERIMAGE)
            hmg_ChangeStyle(_HMG_aControlHandles[i], NIL, SS_CENTERIMAGE)
         ENDIF
      ENDSWITCH

      _Refresh(i)

   ENDIF

RETURN NIL

STATIC FUNCTION _SetCase(ControlName, ParentForm, cCase)

   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      SWITCH cCase
      CASE "NONE"
         hmg_ChangeStyle(_HMG_aControlHandles[i], NIL, ES_UPPERCASE + ES_LOWERCASE)
         EXIT
      CASE "UPPER"
         hmg_ChangeStyle(_HMG_aControlHandles[i], ES_UPPERCASE, ES_LOWERCASE)
         EXIT
      CASE "LOWER"
         hmg_ChangeStyle(_HMG_aControlHandles[i], ES_LOWERCASE, ES_UPPERCASE)
      ENDSWITCH

      _Refresh(i)

   ENDIF

RETURN NIL

STATIC FUNCTION _SetTransparent(ControlName, ParentForm, lTransparent)

   LOCAL h
   LOCAL cType
   LOCAL ix
   LOCAL i

   IF (i := GetControlIndex(ControlName, ParentForm)) > 0

      ix := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])

      IF ix > 0 .AND. _HMG_aFormBkColor[ix][1] != -1

         h := _HMG_aControlHandles[i]
         cType := _HMG_aControlType[i]

         SWITCH cType

         CASE CONTROL_TYPE_LABEL
         CASE CONTROL_TYPE_HYPERLINK
            IF lTransparent
               hmg_ChangeStyle(h, WS_EX_TRANSPARENT, NIL, .T.)
            ELSE
               hmg_ChangeStyle(h, NIL, WS_EX_TRANSPARENT, .T.)
            ENDIF
            EXIT

         CASE CONTROL_TYPE_CHECKBOX
            IF lTransparent
               hmg_ChangeStyle(h, WS_EX_TRANSPARENT, NIL, .T.)
               _HMG_aControlBkColor[i] := _HMG_aFormBkColor[ix]
            ELSE
               hmg_ChangeStyle(h, NIL, WS_EX_TRANSPARENT, .T.)
               _HMG_aControlBkColor[i] := nRGB2Arr(hmg_GetSysColor(COLOR_BTNFACE))
            ENDIF
            EXIT

         CASE CONTROL_TYPE_RADIOGROUP
            IF lTransparent
               _HMG_aControlBkColor[i] := NIL
            ELSE
               _HMG_aControlBkColor[i] := nRGB2Arr(hmg_GetSysColor(COLOR_3DFACE))
            ENDIF

         ENDSWITCH

         _HMG_aControlInputMask[i] := iif(cType != CONTROL_TYPE_CHECKBOX, lTransparent, .F.)

         _RedrawControl(i)

      ENDIF

   ENDIF

RETURN NIL

STATIC FUNCTION _RedrawControl(i)

   LOCAL ControlHandle

   IF i > 0

      ControlHandle := _HMG_aControlHandles[i]

      IF hb_IsArray(ControlHandle)
         AEval(ControlHandle, {|x|hmg_RedrawWindow(x, .T.)})
      ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_OBUTTON
         hmg_InvalidateRect(ControlHandle, 0)
      ELSE
         hmg_RedrawWindow(ControlHandle)
      ENDIF

   ENDIF

RETURN NIL

STATIC FUNCTION _SetGetTabHTColors(ControlName, ParentForm, nId, Value)

   LOCAL i := GetControlIndex(ControlName, ParentForm)
   LOCAL RetVal := .T.

   IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_TAB .AND. _HMG_aControlMiscData1[i][5]  // HotTrack

      IF PCount() > 3
         _HMG_aControlMiscData1[i][nId] := Value
      ELSE
         RetVal := _HMG_aControlMiscData1[i][nId]
      ENDIF

   ENDIF

RETURN RetVal

STATIC FUNCTION _SetGetSpacingProperty(ControlName, ParentForm, Value)

   LOCAL i := GetControlIndex(ControlName, ParentForm)
   LOCAL CurrValue

   IF i > 0 .AND. (_HMG_aControlType[i] == CONTROL_TYPE_GETBOX .OR. ;
                   _HMG_aControlType[i] == CONTROL_TYPE_GRID .OR. ;
                   _HMG_aControlType[i] == CONTROL_TYPE_MULTIGRID .OR. ;
                   _HMG_aControlType[i] == CONTROL_TYPE_PROPGRID)

      IF PCount() > 2
         _HMG_aControlSpacing[i] := Value
      ELSE
         CurrValue := _HMG_aControlSpacing[i]
      ENDIF

   ENDIF

RETURN CurrValue

STATIC FUNCTION SetWindowGripperText(i, cValue)

   hmg_SetCaptionSplitBoxItem(_HMG_aFormNotifyMenuHandle[i], _HMG_aFormMiscData1[i][4] - 1, cValue)
   _HMG_aFormMiscData1[i][5] := cValue

RETURN NIL

FUNCTION _SetType(cType)   // (c) 1996-1997, Bryan Duchesne

   LOCAL cFormat := Set(4)
   LOCAL xRetVal

   SWITCH Upper(Left(cType, 1))

   CASE "S"
      xRetVal := ""
      EXIT
   CASE "N"
      xRetVal := 0
      EXIT
   CASE "D"
      cFormat := Upper(cFormat)
      cFormat := StrTran(cFormat, "Y", " ")
      cFormat := StrTran(cFormat, "M", " ")
      cFormat := StrTran(cFormat, "D", " ")
      xRetVal := CToD(cFormat)
      EXIT
   CASE "B"
      xRetVal := {||NIL}
      EXIT
   CASE "A"
      xRetVal := {}
      EXIT
   CASE "L"
      xRetVal := .F.

   ENDSWITCH

RETURN xRetVal

FUNCTION _IsTyped(a, b)    // (c) 1996-1997, Bryan Duchesne

   IF a != NIL
      IF !(ValType(a) == ValType(b))
         MsgMiniGuiError("Strongly Typed Variable Assignment: Data Type Mismatch.")
      ENDIF
   ENDIF

RETURN b

FUNCTION cValToChar(uVal)

   SWITCH ValType(uVal)
   CASE "C"
   CASE "M" ; RETURN uVal
   CASE "D" ; RETURN DToC(uVal)
   CASE "T" ; RETURN iif(Year(uVal) == 0, hb_TToC(uVal, "", Set(_SET_TIMEFORMAT)), hb_TToC(uVal))
   CASE "L" ; RETURN iif(uVal, "T", "F")
   CASE "N" ; RETURN hb_ntos(uVal)
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ ... }"
   CASE "O" ; RETURN uVal:ClassName()
   CASE "H" ; RETURN "{=>}"
   CASE "P" ; RETURN "0x" + hb_NumToHex(uVal)
   ENDSWITCH

RETURN ""

FUNCTION cNumToChar(nVal)

   LOCAL cVal := ""
   LOCAL i
   LOCAL c

   IF nVal < 0
      cVal := "-"
      nVal := -nVal
   ENDIF

   i := Int(nVal)
   cVal += hb_ntos(i)

   IF (nVal -= i) > 0.0
      c := RemRight(SubStr(Str(nVal, 15, 13), 3), "0")
      IF Len(c) > 0
         cVal += "." + c
      ENDIF
   ENDIF

RETURN cVal
