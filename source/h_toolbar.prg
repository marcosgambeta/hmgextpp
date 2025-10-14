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

//---------------------------------------------------------------------------//
FUNCTION _DefineToolBar(ControlName, ParentForm, x, y, caption, ProcedureName, w, h, fontname, fontsize, tooltip, flat, bottom, righttext, break, bold, italic, underline, strikeout, border, mixedbuttons, rows, tbsize, imagelst, hotimagelst, wrap, custom)
//---------------------------------------------------------------------------//

   LOCAL FontHandle
   LOCAL ControlHandle
   LOCAL cParentForm
   LOCAL mVar
   LOCAL id
   LOCAL k

   IF !_IsWindowDefined(ParentForm)
      MsgMiniGuiError("Window: " + ParentForm + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentForm)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + " Already defined.")
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   _HMG_ActiveToolBarBreak := break

   mVar := "_" + ParentForm + "_" + ControlName

   cParentForm := ParentForm

   ParentForm := iif(_HMG_BeginPagerActive, _HMG_ActivePagerForm, GetFormHandle(ParentForm))

   Id := _GetId()

   IF _HMG_ActiveSplitBox
      _HMG_SplitLastControl := "TOOLBAR"
   ENDIF

   _HMG_ActiveToolBarCaption := Caption

   IF _HMG_ActiveToolBarExtend
      ControlHandle := hmg_InitToolBarEx(ParentForm, Caption, id, 0, 0, w, h, "", 0, flat, bottom, righttext, _HMG_ActiveSplitBox, border, mixedbuttons, wrap, custom)
   ELSE
      ControlHandle := hmg_InitToolBar(ParentForm, Caption, id, 0, 0, w, h, "" , 0, flat, bottom, righttext, _HMG_ActiveSplitBox, border, wrap, custom)
   ENDIF

   IF !empty(FontHandle)
      hmg__SetFontHandle(ControlHandle, FontHandle)
   ELSE
      __defaultNIL(@FontName, _HMG_DefaultFontName)
      __defaultNIL(@FontSize, _HMG_DefaultFontSize)
      IF hmg_IsWindowHandle(ControlHandle)
         FontHandle := hmg__SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
      ENDIF
   ENDIF

   IF tooltip != NIL
      hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(cParentForm))
   ENDIF

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_TOOLBAR
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentForm
   _HMG_aControlIds                [k] := id
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := { imagelst, hotimagelst }
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := NIL
   _HMG_aControlFontColor          [k] := NIL
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := rows
   _HMG_aControlContainerRow       [k] := -1
   _HMG_aControlContainerCol       [k] := -1
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := { .F. , .F. , .F. , .F. }
   _HMG_aControlToolTip            [k] := ""
   _HMG_aControlRangeMin           [k] := tbsize
   _HMG_aControlRangeMax           [k] := iif(wrap, 1, 0)
   _HMG_aControlCaption            [k] := Caption
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

RETURN ControlHandle

//---------------------------------------------------------------------------//
FUNCTION _EndToolBar()
//---------------------------------------------------------------------------//
   
   LOCAL ParentForm
   LOCAL h
   LOCAL aSize
   LOCAL nRow
   LOCAL nBand
   LOCAL ix
   LOCAL i

   ParentForm := iif(_HMG_BeginWindowActive, _HMG_ActiveFormName, _HMG_ActiveToolBarFormName)
   h := GetControlHandle(_HMG_ActiveToolBarName, ParentForm)

   IF _HMG_BeginPagerActive
#ifdef _PAGER_
      _AddChildToPager(_HMG_ActiveToolBarName, ParentForm)
#endif
   ELSE
      IF _HMG_ActiveSplitBox
         _AddToolBarToSplitBox(_HMG_ActiveToolBarName, _HMG_ActiveToolBarBreak, _HMG_ActiveToolBarCaption, ParentForm)
         ix     := GetControlIndex(_HMG_ActiveToolBarName, ParentForm)
         i      := GetFormIndex(_HMG_ActiveSplitBoxParentFormName)
         nBand  := GetBandCount(_HMG_aFormReBarHandle[i])
         _HMG_aControlMiscData1[ix] := nBand
         nRow   := _HMG_aControlSpacing[ix]
         IF nRow > 1
            aSize := hmg_SetRowsButton(h, nRow, .T.)
            hmg_ResizeSplitBoxItem(_HMG_aFormReBarHandle[i], nBand - 1, aSize[1], aSize[2], aSize[1])
         ENDIF
      ELSE
         hmg_MaxTextBtnToolBar(h, _GetControlWidth(_HMG_ActiveToolBarName, ParentForm), _GetControlHeight(_HMG_ActiveToolBarName, ParentForm))
      ENDIF
   ENDIF

   _HMG_ActiveToolBarBreak    := .F.
   _HMG_ActiveToolBarExtend   := .F.

   _HMG_ActiveToolBarName     := ""
   _HMG_ActiveToolBarFormName := ""

   _HMG_ToolBarActive         := .F.

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _DefineToolButton(ControlName, ParentControl, x, y, Caption, ProcedureName, w, h, image, tooltip, gotfocus, lostfocus, flat, separator, autosize, check, group, dropdown, WholeDropDown, adjust, imageindex, notrans)
//---------------------------------------------------------------------------//
   
   LOCAL ParentForm
   LOCAL hParentForm
   LOCAL cParentForm
   LOCAL mVar
   LOCAL ControlHandle
   LOCAL aImage
   LOCAL imagelst
   LOCAL hotimagelst
   LOCAL id
   LOCAL npos
   LOCAL nToolBarIndex
   LOCAL i
   LOCAL k

   HB_SYMBOL_UNUSED(Flat)

   IF ProcedureName == NIL .AND. Dropdown
      MsgMiniGuiError("ToolBar DropDown buttons must have an associated action (Use WholeDropDown style for no action).")
   ENDIF

   IF ProcedureName != NIL .AND. WholeDropDown
      MsgMiniGuiError("ToolBar Action and WholeDropDown clauses can't be used simultaneously.")
   ENDIF

   ParentForm := iif(_HMG_BeginWindowActive, _HMG_ActiveFormName, _HMG_ActiveToolBarFormName)

   cParentForm := ParentForm
   hParentForm := GetFormHandle(ParentForm)

   mVar := "_" + ParentForm + "_" + ControlName

   ParentForm := GetControlHandle(ParentControl, ParentForm)

   id := _GetId()

   hb_default(@Caption, "")
   hb_default(@image, "")

   hb_default(@w, _GetControlWidth(_HMG_ActiveToolBarName, cParentForm))
   hb_default(@h, _GetControlHeight(_HMG_ActiveToolBarName, cParentForm))

   __defaultNIL(@gotfocus, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@tooltip, "")
   hb_default(@notrans, .F.)

   IF imageindex != NIL
      aImage      := GetControlValue(_HMG_ActiveToolBarName, cParentForm)
      imagelst    := aImage[1]
      hotimagelst := aImage[2]
   ELSE
      imageindex := -1
   ENDIF

   IF _HMG_ActiveToolBarExtend
      ControlHandle := hmg_InitToolButtonEx(ParentForm, Caption, id, 0, 0, w, h, image, notrans, separator, autosize, check, group, dropdown, WholeDropDown, adjust, imageindex, imagelst, hotimagelst)
   ELSE
      ControlHandle := hmg_InitToolButton(ParentForm, Caption, id, 0, 0, w, h, image, notrans, separator, autosize, check, group, dropdown, WholeDropDown, adjust)
   ENDIF

   nPos := GetButtonBarCount(ParentForm) - iif(separator, 1, 0)

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := CONTROL_TYPE_TOOLBUTTON
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := hParentForm
   _HMG_aControlIds                [k] := id
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := nPos
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := ""
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
   _HMG_aControlContainerRow       [k] := -1
   _HMG_aControlContainerCol       [k] := -1
   _HMG_aControlPicture            [k] := image
   _HMG_aControlContainerHandle    [k] := ParentForm
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := { .F. , .F. , .F. , .F. }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := 0
   _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := iif(_HMG_ActiveToolBarExtend, 1, 0)
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   Caption := Upper(Caption)

   IF (i := hb_UAt("&" , Caption)) > 0

      IF WholeDropDown
         nToolBarIndex := AScan(_HMG_aControlHandles, ParentForm)
         ProcedureName := {||_DropDownShortcut(Id, hParentForm, nToolBarIndex, nPos)}
      ENDIF

      _DefineLetterOrDigitHotKey(Caption, i, cParentForm, ProcedureName)

   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
STATIC FUNCTION _AddToolBarToSplitBox(ControlName, break, Caption, ParentForm)
//---------------------------------------------------------------------------//
   
   LOCAL MinWidth
   LOCAL MinHeight
   LOCAL i
   LOCAL c
   LOCAL w
   LOCAL ix

   i := GetFormIndex(_HMG_ActiveSplitBoxParentFormName)
   c := GetControlHandle(ControlName, _HMG_ActiveSplitBoxParentFormName)

   IF !_HMG_ActiveToolBarExtend
      hmg_MaxTextBtnToolBar(c, _GetControlWidth(ControlName, ParentForm), _GetControlHeight(ControlName, ParentForm))
   ENDIF

   w := hmg_GetSizeToolBar(c)

   MinWidth  := hmg_HiWord(w)
   MinHeight := hmg_LoWord(w)

   w := GetWindowWidth(c)

   ix := GetControlIndex(ControlName, ParentForm)
   /* WRAP style handling */
   IF (_HMG_aControlRangeMax[ix] == 1) .AND. ;
      ( hb_bitand(hmg_GetWindowLong(_HMG_aFormReBarHandle[i], GWL_STYLE), CCS_VERT) == CCS_VERT )
      MinWidth  := _HMG_aControlWidth[ix]
      MinHeight := hmg_HiWord(w)
   ENDIF
   IF i > 0
      hmg_AddSplitBoxItem(c, _HMG_aFormReBarHandle[i], w, break, Caption, MinWidth, MinHeight, _HMG_ActiveSplitBoxInverted, _HMG_aControlRangeMin[ix])
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _BeginToolBar(name, parent, row, col, w, h, caption, ProcedureName, fontname, fontsize, tooltip, flat, bottom, righttext, break, bold, italic, underline, strikeout, border, wrap, custom)
//---------------------------------------------------------------------------//

   _HMG_ToolBarActive       := .T.
   _HMG_ActiveToolBarExtend := .F.

   IF _HMG_SplitChildActive
      MsgMiniGuiError("ToolBars Can't Be Defined Inside SplitChild Windows.")
   ENDIF

   IF _HMG_BeginWindowActive
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF

   IF parent == NIL
      parent := _HMG_ActiveFormName
   ENDIF

   _HMG_ActiveToolBarFormName := parent

   hb_default(@caption, "")
   hb_default(@w, 0)
   hb_default(@h, 0)
   hb_default(@wrap, .F.)

   IF break == NIL
      break := !_HMG_ActiveSplitBox
   ENDIF

   _HMG_ActiveToolBarName := name

   _DefineToolBar(name, parent, col, row, caption, ProcedureName, w, h, fontname, fontsize, tooltip, flat, bottom, righttext, break, bold, italic, underline, strikeout, border, .F., 0, 0, , , wrap, custom)

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _BeginToolBarEx(name, parent, row, col, w, h, caption, ProcedureName, fontname, fontsize, tooltip, flat, bottom, righttext, break, bold, italic, underline, strikeout, border, mixedbuttons, rows, tbsize, imagelst, hotimagelst, wrap, custom)
//---------------------------------------------------------------------------//

   _HMG_ToolBarActive       := .T.
   _HMG_ActiveToolBarExtend := .T.

   IF _HMG_SplitChildActive
      MsgMiniGuiError("ToolBars Can't Be Defined Inside SplitChild Windows.")
   ENDIF

   IF _HMG_BeginWindowActive
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF

   IF parent == NIL
      parent := _HMG_ActiveFormName
   ENDIF

   _HMG_ActiveToolBarFormName := parent

   hb_default(@caption, "")
   hb_default(@w, 0)
   hb_default(@h, 0)
   hb_default(@rows, 1)
   hb_default(@tbsize, 0)
   hb_default(@wrap, .F.)

   IF imagelst != NIL
      IF hb_IsChar(imagelst)
         imagelst := GetControlHandle(imagelst, parent)
      ENDIF
   ENDIF

   IF hotimagelst != NIL
      IF hb_IsChar(hotimagelst)
         hotimagelst := GetControlHandle(hotimagelst, parent)
      ENDIF
   ENDIF

   IF break == NIL
      break := !_HMG_ActiveSplitBox
   ENDIF

   _HMG_ActiveToolBarName := name

   _DefineToolBar(name, parent, col, row, caption, ProcedureName, w, h, fontname, fontsize, tooltip, flat, bottom, righttext, break, bold, italic, underline, strikeout, border, mixedbuttons, rows, tbsize, imagelst, hotimagelst, wrap, custom)

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION _CreatePopUpChevron(hWnd, wParam, lParam)
//---------------------------------------------------------------------------//
   
   LOCAL hMenu
   LOCAL hImage
   LOCAL TbHwnd
   LOCAL aChevronInfo
   LOCAL aBtnInfo
   LOCAL aPos
   LOCAL cMenu
   LOCAL image
   LOCAL lEnable
   LOCAL i
   LOCAL k
   LOCAL n

   IF (i := AScan(_HMG_aFormhandles, hWnd)) > 0
      aChevronInfo := hmg_CreatePopUpChevron(_HMG_aFormReBarHandle[i], lParam)

      TbHwnd := aChevronInfo[5]
      hMenu := hmg_CreatePopupMenu()

      FOR n := aChevronInfo[6] TO aChevronInfo[7] - 1

         aBtnInfo := hmg_GetButtonBar(TbHwnd, n)
         lEnable  := hmg_IsButtonEnabled(TbHwnd, n)
         hImage := hmg_GetImageList(tbhwnd, aBtnInfo[1])

         IF (k := AScan(_HMG_aControlIds, aBtnInfo[2])) > 0 .AND. !aBtnInfo[3]

            IF !Empty(_HMG_aControlToolTip[k])
               cMenu := _HMG_aControlToolTip[k]
            ELSEIF !Empty(_HMG_aControlCaption[k])
               cMenu := _HMG_aControlCaption[k]
            ELSE
               cMenu := "Button " + hb_ntos(n)
            ENDIF

            hmg_AppendMenuString(hMenu, aBtnInfo[2], cMenu)

            image := _HMG_aControlPicture  [k]
            IF Len(image) != 0
               hmg_MenuItem_SetBitMaps(hMenu, aBtnInfo[2], image, NIL)
            ELSE
               hmg_SetChevronImage(hMenu, aBtnInfo[2], hImage)
            ENDIF

            IF !lEnable
               hmg_xDisableMenuItem(hMenu, aBtnInfo[2])
            ENDIF

         ENDIF

      NEXT

      aPos := { 0, 0, 0, 0 }
      hmg_GetWindowRect(_HMG_aFormReBarHandle[i], aPos)

      hmg_TrackPopupMenu(hMenu, aPos[1] + aChevronInfo[1], aPos[2] + aChevronInfo[4] + 3, hWnd)
   ENDIF

   hmg_DefWindowProc(hWnd, RBN_CHEVRONPUSHED, wParam, lParam)
   hmg_DestroyMenu(hMenu)

RETURN NIL

//---------------------------------------------------------------------------//
STATIC PROCEDURE _DropDownShortcut(nToolButtonId, nParentWindowHandle, i, nButtonPos)
//---------------------------------------------------------------------------//
   
   LOCAL aPos
   LOCAL aSize
   LOCAL x

   IF (x := AScan(_HMG_aControlIds, nToolButtonId)) > 0 .AND. _HMG_aControlType[x] == CONTROL_TYPE_TOOLBUTTON
      aPos := { 0, 0, 0, 0 }
      hmg_GetWindowRect(_HMG_aControlHandles[i] , aPos)

      hmg_SendMessage(_HMG_aControlHandles[i], TB_SETHOTITEM, nButtonPos - 1, 0)

      aSize := hmg_GetButtonBarRect(_HMG_aControlHandles[i] , nButtonPos - 1)

      hmg_TrackPopupMenu(_HMG_aControlRangeMax[x] , aPos[1] + hmg_LoWord(aSize) , aPos[2] + hmg_HiWord(aSize) + ;
         iif(_HMG_ActiveSplitBoxInverted, 0, ( aPos[4] - aPos[2] - hmg_HiWord(aSize) ) / 2), nParentWindowHandle)

      hmg_SendMessage(_HMG_aControlHandles[i], TB_SETHOTITEM, -1, 0)
   ENDIF

RETURN
