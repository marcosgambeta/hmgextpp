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

STATIC _HMG_xPopupMenuFont
STATIC _HMG_xContextPopupMenuFont

/*
_DefineMainMenu(Parent)
*/
PROCEDURE _DefineMainMenu(Parent)

   IF Parent == NIL
      Parent := _HMG_ActiveFormName
   ENDIF

   _HMG_xMenuType := "MAIN"
   _HMG_xMainMenuParentName := Parent
   _HMG_xMainMenuParentHandle := GetFormHandle(Parent)
   _HMG_xMainMenuHandle := hmg_CreateMenu()
   _HMG_xMenuPopupLevel := 0

RETURN

/*
_DefineMenuPopup(Caption, Name, Image, Font)
*/
PROCEDURE _DefineMenuPopup(Caption, Name, Image, Font)

   LOCAL mVar
   LOCAL k
   LOCAL FormName

   IF hb_IsChar(Font)
      Font := GetFontHandle(Font)
   ENDIF

   IF _HMG_xMenuType $ "MAIN,CONTEXT,OWNCONTEXT,NOTIFY,DROPDOWN"

      mVar := Left(_HMG_xMenuType, 1)

      SWITCH mVar

      CASE "M"

         IF _HMG_xMenuPopupLevel == 0
            IF Font != NIL .AND. _HMG_xPopupMenuFont == NIL
               _HMG_xPopupMenuFont := Font
            ENDIF
         ENDIF

         _HMG_xMenuPopupLevel++

         _HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel] := hmg_CreatePopupMenu(1)
         _HMG_xMenuPopupCaption[_HMG_xMenuPopupLevel] := Caption

         IF _HMG_xMenuPopupLevel > 1
            hmg_AppendMenuPopup(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel - 1], _HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel], _HMG_xMenuPopupCaption[_HMG_xMenuPopupLevel], 2, Font)
         ENDIF
         EXIT

      CASE "C"
      CASE "O"
      CASE "N"
      CASE "D"

         SWITCH mVar

         CASE "C"
            k := 3
            EXIT

         CASE "N"
            k := 4
            EXIT

         CASE "O"
         CASE "D"
            k := 5

         ENDSWITCH

         IF _HMG_xContextPopupLevel == 0
            IF Font != NIL .AND. _HMG_xContextPopupMenuFont == NIL
               _HMG_xContextPopupMenuFont := Font
            ENDIF
         ENDIF

         _HMG_xContextPopupLevel++

         _HMG_xContextPopupHandle[_HMG_xContextPopupLevel] := hmg_CreatePopupMenu(k)
         _HMG_xContextPopupCaption[_HMG_xContextPopupLevel] := Caption

         IF _HMG_xContextPopupLevel > 1
            hmg_AppendMenuPopup(_HMG_xContextPopupHandle[_HMG_xContextPopupLevel - 1], _HMG_xContextPopupHandle[_HMG_xContextPopupLevel], _HMG_xContextPopupCaption[_HMG_xContextPopupLevel], k, Font)
         ENDIF

      ENDSWITCH

      FormName := iif(_HMG_xMenuType == "MAIN", _HMG_xMainMenuParentName, _HMG_xContextMenuParentName)
      k := _GetControlFree()

      IF name == NIL
#ifndef _EMPTY_MENU_
         Name := "DummyPopupName" + hb_ntos(k)
#else
         Name := ""
#endif
      ENDIF

      IF !Empty(name)
         mVar := "_" + FormName + "_" + Name
#ifdef _NAMES_LIST_
         _SetNameList(mVar, k)
#else
         PUBLIC &mVar. := k
#endif
      ENDIF

      _HMG_aControlType               [k] := CONTROL_TYPE_POPUP
      _HMG_aControlNames              [k] := Name
      _HMG_aControlHandles            [k] := HMG_NULLHANDLE
      _HMG_aControlIds                [k] := iif(_HMG_xMenuType == "MAIN", _HMG_xMenuPopupLevel, _HMG_xContextPopupLevel)
      _HMG_aControlProcedures         [k] := NIL
      _HMG_aControlValue              [k] := NIL
      _HMG_aControlInputMask          [k] := FormName
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
      _HMG_aControlWidth              [k] := 0
      _HMG_aControlHeight             [k] := 0
      _HMG_aControlContainerRow       [k] := -1
      _HMG_aControlContainerCol       [k] := -1
      _HMG_aControlPicture            [k] := Image
      _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
      _HMG_aControlFontName           [k] := ""
      _HMG_aControlFontSize           [k] := 0
      _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
      _HMG_aControlToolTip            [k] := ""
      _HMG_aControlRangeMin           [k] := 0
      _HMG_aControlRangeMax           [k] := 0
      _HMG_aControlCaption            [k] := Caption
      _HMG_aControlVisible            [k] := .T.
      _HMG_aControlHelpId             [k] := 0
      _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
      _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
      _HMG_aControlEnabled            [k] := .T.
      _HMG_aControlMiscData1          [k] := 0
      _HMG_aControlMiscData2          [k] := ""

      IF _HMG_xMenuType == "MAIN"
         _HMG_aControlHandles[k]       := _HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel]
         _HMG_aControlParentHandles[k] := _HMG_xMainMenuParentHandle
         _HMG_aControlPageMap[k]       := _HMG_xMainMenuHandle
         _HMG_aControlSpacing[k]       := _HMG_xMenuPopupHandle[_HMG_xMenuPopupLevel]
      ELSE
         _HMG_aControlHandles[k]       := _HMG_xContextPopupHandle[_HMG_xContextPopupLevel]
         _HMG_aControlParentHandles[k] := _HMG_xContextMenuParentHandle
         _HMG_aControlPageMap[k]       := _HMG_xContextMenuHandle
         _HMG_aControlSpacing[k]       := _HMG_xContextPopupHandle[_HMG_xContextPopupLevel]
      ENDIF

      IF _HMG_lOOPEnabled
         Eval(_HMG_bOnControlInit, k, mVar)
      ENDIF

   ELSE

      MsgMiniGuiError("Menu type incorrect.")

   ENDIF

RETURN

/*
_EndMenuPopup()
*/
PROCEDURE _EndMenuPopup()

   LOCAL k

   IF _HMG_xMenuType == "MAIN"
      _HMG_xMenuPopupLevel--
      IF _HMG_xMenuPopupLevel == 0
         hmg_AppendMenuPopup(_HMG_xMainMenuHandle, _HMG_xMenuPopupHandle[1], _HMG_xMenuPopupCaption[1], 1, _HMG_xPopupMenuFont)
      ENDIF
   ELSEIF _HMG_xMenuType $ "CONTEXT,OWNCONTEXT,NOTIFY,DROPDOWN"
      _HMG_xContextPopupLevel--
      IF _HMG_xContextPopupLevel == 0
         SWITCH Left(_HMG_xMenuType, 1)
         CASE "C"
            k := 3
            EXIT
         CASE "N"
            k := 4
            EXIT
         CASE "O"
         CASE "D"
            k := 5
         ENDSWITCH
         hmg_AppendMenuPopup(_HMG_xContextMenuHandle, _HMG_xContextPopupHandle[1], _HMG_xContextPopupCaption[1], k, _HMG_xContextPopupMenuFont)
      ENDIF
   ELSE
      MsgMiniGuiError("Menu type incorrect.")
   ENDIF

RETURN

/*
_DefineMenuItem(caption, action, name, Image, checked, disabled, cMessage, font, check_image, lBreakMenu, lSeparator, icon, default)
*/
PROCEDURE _DefineMenuItem(caption, action, name, Image, checked, disabled, cMessage, font, check_image, lBreakMenu, lSeparator, icon, default)

   LOCAL ControlHandle
   LOCAL ContextMenuHandle
   LOCAL hBitmap := 0
   LOCAL mVar
   LOCAL k
   LOCAL id
   LOCAL nBreakCode := 6

   IF hb_IsChar(font)
      font := GetFontHandle(font)
   ENDIF

   hb_default(@checked, .F.)
   hb_default(@disabled, .F.)
   hb_default(@lBreakMenu, .F.)
   hb_default(@default, .F.)

   IF lBreakMenu
      nBreakCode := 1
      IF hb_defaultValue(lSeparator, .F.)
         nBreakCode := 2
      ENDIF
   ENDIF

   id := _GetId()

   IF _HMG_xMenuType == "MAIN"

      IF !(caption == "-")
         ControlHandle := _HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel]
         hmg_AppendMenuString(ControlHandle, id, caption, nBreakCode)
      ENDIF

      IF image != NIL
         hBitmap := hmg_MenuItem_SetBitMaps(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ], id, image, "")
      ELSEIF icon != NIL
         hBitmap := hmg_MenuItem_SetIcon(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ], id, icon)
      ENDIF

      IF check_image != NIL
         hmg_MenuItem_SetCheckMarks(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ], id, check_image, "")
      ENDIF

      IF font != NIL
         hmg_MenuItem_SetFont(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ], id, font)
      ENDIF

      k := _GetControlFree()

      IF name == NIL
#ifndef _EMPTY_MENU_
         Name := "DummyMenuName" + hb_ntos(k)
#else
         Name := ""
#endif
      ENDIF

      IF !Empty(name)
         mVar := "_" + _HMG_xMainMenuParentName + "_" + Name
#ifdef _NAMES_LIST_
         _SetNameList(mVar, k)
#else
         PUBLIC &mVar. := k
#endif
      ENDIF

      _HMG_aControlType               [k] := CONTROL_TYPE_MENU
      _HMG_aControlNames              [k] := Name
      _HMG_aControlHandles            [k] := ControlHandle
      _HMG_aControlParentHandles      [k] := _HMG_xMainMenuParentHandle
      _HMG_aControlIds                [k] := id
      _HMG_aControlProcedures         [k] := action
      _HMG_aControlPageMap            [k] := _HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ]
      _HMG_aControlValue              [k] := cMessage
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
      _HMG_aControlWidth              [k] := 0
      _HMG_aControlHeight             [k] := 0
      _HMG_aControlSpacing            [k] := 0
      _HMG_aControlContainerRow       [k] := -1
      _HMG_aControlContainerCol       [k] := -1
      _HMG_aControlPicture            [k] := ""
      _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
      _HMG_aControlFontName           [k] := ""
      _HMG_aControlFontSize           [k] := 0
      _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
      _HMG_aControlToolTip            [k] := ""
      _HMG_aControlRangeMin           [k] := 0
      _HMG_aControlRangeMax           [k] := 0
      _HMG_aControlCaption            [k] := Caption
      _HMG_aControlVisible            [k] := .T.
      _HMG_aControlHelpId             [k] := 0
      _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
      _HMG_aControlBrushHandle        [k] := hBitmap
      _HMG_aControlEnabled            [k] := .T.
      _HMG_aControlMiscData1          [k] := 0
      _HMG_aControlMiscData2          [k] := ""

      IF _HMG_lOOPEnabled
         Eval(_HMG_bOnControlInit, k, mVar)
      ENDIF

      IF checked
         hmg_xCheckMenuItem(ControlHandle, id)
      ENDIF

      IF disabled
         hmg_xDisableMenuItem(ControlHandle, id)
         _HMG_aControlEnabled[k] := .F.
      ENDIF

      IF default
         hmg_SetMenuDefaultItem(ControlHandle, id)
      ENDIF

   ELSE

      IF !(caption == "-")
         IF _HMG_xContextPopupLevel > 0
            ContextMenuHandle := _HMG_xContextPopupHandle[_HMG_xContextPopupLevel]
            hmg_AppendMenuString(ContextMenuHandle, id, caption, iif(lBreakMenu, nBreakCode, 7))
         ELSE
            ContextMenuHandle := _HMG_xContextMenuHandle
            hmg_AppendMenuString(ContextMenuHandle, id, caption, iif(lBreakMenu, nBreakCode, 8))
         ENDIF
         ControlHandle := ContextMenuHandle
      ENDIF

      IF image != NIL
         hBitmap := hmg_MenuItem_SetBitMaps(ContextMenuHandle, id, image, "")
      ENDIF

      IF check_image != NIL
         hmg_MenuItem_SetCheckMarks(ContextMenuHandle, id, check_image, "")
      ENDIF

      IF font != NIL
         hmg_MenuItem_SetFont(ContextMenuHandle, id, font)
      ENDIF

      k := _GetControlFree()

      IF name == NIL
#ifndef _EMPTY_MENU_
         Name := "DummyMenuName" + hb_ntos(k)
#else
         Name := ""
#endif
      ENDIF

      IF !Empty(name)
         mVar := "_" + _HMG_xContextMenuParentName + "_" + Name
#ifdef _NAMES_LIST_
         _SetNameList(mVar, k)
#else
         PUBLIC &mVar. := k
#endif
      ENDIF

      _HMG_aControlType               [k] := CONTROL_TYPE_MENU
      _HMG_aControlNames              [k] := Name
      _HMG_aControlHandles            [k] := ControlHandle
      _HMG_aControlParentHandles      [k] := _HMG_xContextMenuParentHandle
      _HMG_aControlIds                [k] := id
      _HMG_aControlProcedures         [k] := action
      _HMG_aControlPageMap            [k] := _HMG_xContextMenuHandle
      _HMG_aControlValue              [k] := cMessage
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
      _HMG_aControlWidth              [k] := 0
      _HMG_aControlHeight             [k] := 0
      _HMG_aControlSpacing            [k] := 0
      _HMG_aControlContainerRow       [k] := -1
      _HMG_aControlContainerCol       [k] := -1
      _HMG_aControlPicture            [k] := ""
      _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
      _HMG_aControlFontName           [k] := ""
      _HMG_aControlFontSize           [k] := 0
      _HMG_aControlFontAttributes     [k] := {.F., .F., .F., .F.}
      _HMG_aControlToolTip            [k] := ""
      _HMG_aControlRangeMin           [k] := 0
      _HMG_aControlRangeMax           [k] := 0
      _HMG_aControlCaption            [k] := Caption
      _HMG_aControlVisible            [k] := .T.
      _HMG_aControlHelpId             [k] := 0
      _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
      _HMG_aControlBrushHandle        [k] := hBitmap
      _HMG_aControlEnabled            [k] := .T.
      _HMG_aControlMiscData1          [k] := 1
      _HMG_aControlMiscData2          [k] := ""

      IF _HMG_lOOPEnabled
         Eval(_HMG_bOnControlInit, k, mVar)
      ENDIF

      IF checked
         hmg_xCheckMenuItem(ContextMenuHandle, id)
      ENDIF

      IF disabled
         hmg_xDisableMenuItem(ContextMenuHandle, id)
         _HMG_aControlEnabled[k] := .F.
      ENDIF

      IF default
         hmg_SetMenuDefaultItem(ContextMenuHandle, id)
      ENDIF

   ENDIF

RETURN

/*
_DefineSeparator()
*/
PROCEDURE _DefineSeparator()

   IF _HMG_xMenuType == "MAIN"
      hmg_AppendMenuSeparator(_HMG_xMenuPopuphandle[_HMG_xMenuPopupLevel ])
   ELSE
      IF _HMG_xContextPopupLevel > 0
         hmg_AppendMenuSeparator(_HMG_xContextPopupHandle[_HMG_xContextPopupLevel])
      ELSE
         hmg_AppendMenuSeparator(_HMG_xContextMenuHandle)
      ENDIF
   ENDIF

   IF IsExtendedMenuStyleActive() // GF 30/08/10
      _DefineMenuItem("-")
   ENDIF

RETURN

/*
_EndMenu()
*/
PROCEDURE _EndMenu()

   LOCAL image
   LOCAL i
   LOCAL h

   SWITCH Left(_HMG_xMenuType, 1)

   CASE "M" // MAIN
      hmg_SetMenu(_HMG_xMainMenuParentHandle, _HMG_xMainMenuHandle)
      EXIT

   CASE "C" // CONTEXT
      i := GetFormIndex(_HMG_xContextMenuParentName)
      _HMG_aFormContextMenuHandle[i] := _HMG_xContextMenuHandle
      EXIT

   CASE "N" // NOTIFY
      i := GetFormIndex(_HMG_xContextMenuParentName)
      _HMG_aFormNotifyMenuHandle[i] := _HMG_xContextMenuHandle
      EXIT

   CASE "D" // DROPDOWN
      _HMG_aControlRangeMax[_HMG_xContextMenuButtonIndex] := _HMG_xContextMenuHandle
      EXIT

   CASE "O" // OWNCONTEXT
      IF hb_IsArray(_HMG_xContextMenuButtonIndex)

         FOR i := 1 TO Len(_HMG_xContextMenuButtonIndex)

            h := _HMG_aControlHandles[_HMG_xContextMenuButtonIndex[i]]

            IF hb_IsArray(h)
               AEval(h, {|x|AAdd(_HMG_aControlsContextMenu, {x, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex[i], .T.})})
            ELSE
               AAdd(_HMG_aControlsContextMenu, {h, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex[i], .T.})
               IF _HMG_aControlType[_HMG_xContextMenuButtonIndex[i]] == CONTROL_TYPE_IMAGE .OR. ;
                  _HMG_aControlType[_HMG_xContextMenuButtonIndex[i]] == CONTROL_TYPE_LABEL
                  hmg_ChangeStyle(h, SS_NOTIFY)
               ENDIF
            ENDIF

         NEXT

      ELSE

         h := _HMG_aControlHandles[_HMG_xContextMenuButtonIndex]

         IF hb_IsArray(h)
            AEval(h, {|x|AAdd(_HMG_aControlsContextMenu, {x, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex, .T.})})
         ELSE
            AAdd(_HMG_aControlsContextMenu, {h, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex, .T.})
         ENDIF

      ENDIF

   ENDSWITCH

   FOR EACH h IN _HMG_aControlHandles
      i := hb_enumindex(h)
      IF _HMG_aControlType[i] == CONTROL_TYPE_POPUP
         image := _HMG_aControlPicture[i]
         IF image != NIL
            _HMG_aControlBrushHandle[i] := hmg_MenuItem_SetBitMaps(_HMG_aControlPageMap[i], _HMG_aControlSpacing[i], image, "")
         ENDIF
      ENDIF
   NEXT

RETURN

/*
_GetMenuIds(ItemName, FormName) --> array
*/
STATIC FUNCTION _GetMenuIds(ItemName, FormName)

   LOCAL h
   LOCAL id
   LOCAL x

   IF (x := GetControlIndex(ItemName, FormName)) > 0
      h := _HMG_aControlPageMap[x]
      IF _HMG_aControlType[x] == CONTROL_TYPE_MENU
         id := _HMG_aControlIds[x]
      ELSEIF _HMG_aControlType[x] == CONTROL_TYPE_POPUP
         id := _HMG_aControlSpacing[x]
      ENDIF
   ENDIF

RETURN {h, id}

/*
_DefaultMenuItem(ItemName, FormName) --> NIL
*/
FUNCTION _DefaultMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   hmg_SetMenuDefaultItem(a[1], a[2])

RETURN NIL

/*
_DisableMenuItem(ItemName, FormName) --> NIL
*/
FUNCTION _DisableMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   hmg_xDisableMenuItem(a[1], a[2])

RETURN NIL

/*
_EnableMenuItem(ItemName, FormName) --> NIL
*/
FUNCTION _EnableMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   hmg_xEnableMenuItem(a[1], a[2])

RETURN NIL

/*
_CheckMenuItem(ItemName, FormName) --> NIL
*/
FUNCTION _CheckMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   hmg_xCheckMenuItem(a[1], a[2])

RETURN NIL

/*
_UncheckMenuItem(ItemName, FormName) --> NIL
*/
FUNCTION _UncheckMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   hmg_xUncheckMenuItem(a[1], a[2])

RETURN NIL

/*
_IsMenuItemChecked(ItemName, FormName) -->
*/
FUNCTION _IsMenuItemChecked(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_xGetMenuCheckState(a[1], a[2])

/*
_IsMenuItemEnabled(ItemName, FormName) -->
*/
FUNCTION _IsMenuItemEnabled(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_xGetMenuEnabledState(a[1], a[2])

/*
_DefineContextMenu(Parent)
*/
PROCEDURE _DefineContextMenu(Parent)

   _HMG_xContextMenuHandle := HMG_NULLHANDLE
   _HMG_xContextMenuParentHandle := HMG_NULLHANDLE
   _HMG_xContextPopupLevel := 0
   _HMG_xContextMenuParentName := ""
   _HMG_xMenuType := "CONTEXT"
   _HMG_xMenuPopupLevel := 0

   IF Parent == NIL
      Parent := _HMG_ActiveFormName
   ENDIF

   _HMG_xContextMenuParentHandle := GetFormHandle(Parent)
   _HMG_xContextMenuParentName := Parent
   _HMG_xContextMenuHandle := hmg_CreatePopupMenu(3)

RETURN

/*
_ShowContextMenu(Parent, nRow, nCol)
*/
PROCEDURE _ShowContextMenu(Parent, nRow, nCol)

   LOCAL xContextMenuParentHandle
   LOCAL aPos

   hb_default(@Parent, "")

   IF !_IsWindowDefined(Parent)
      xContextMenuParentHandle := _HMG_xContextMenuParentHandle
   ELSE
      xContextMenuParentHandle := GetFormHandle(Parent)
   ENDIF

   IF empty(xContextMenuParentHandle)
      MsgMiniGuiError("Context Menu is not defined.")
   ENDIF

   IF hb_defaultValue(nRow, 0) == 0 .AND. hb_defaultValue(nCol, 0) == 0
      aPos := hmg_GetCursorPos()
      nRow := aPos[1]
      nCol := aPos[2]
   ENDIF

   hmg_TrackPopupMenu(_HMG_xContextMenuHandle, nCol, nRow, xContextMenuParentHandle)
   hmg_DoEvents()

RETURN

/*
_DefineNotifyMenu(Parent)
*/
PROCEDURE _DefineNotifyMenu(Parent)

   _HMG_xContextMenuHandle := HMG_NULLHANDLE
   _HMG_xContextMenuParentHandle := HMG_NULLHANDLE
   _HMG_xContextPopupLevel := 0
   _HMG_xContextMenuParentName := ""
   _HMG_xMenuType := "NOTIFY"
   _HMG_xMenuPopupLevel := 0

   IF Parent == NIL
      Parent := _HMG_ActiveFormName
   ENDIF

   _HMG_xContextMenuParentHandle := GetFormHandle(Parent)
   _HMG_xContextMenuParentName := Parent
   _HMG_xContextMenuHandle := hmg_CreatePopupMenu(4)

RETURN

/*
_DefineDropDownMenu(Button, Parent)
*/
PROCEDURE _DefineDropDownMenu(Button, Parent)

   _HMG_xContextMenuHandle := HMG_NULLHANDLE
   _HMG_xContextMenuParentHandle := HMG_NULLHANDLE
   _HMG_xContextPopupLevel := 0
   _HMG_xContextMenuParentName := ""
   _HMG_xMenuType := "DROPDOWN"
   _HMG_xMenuPopupLevel := 0

   IF Parent == NIL
      Parent := _HMG_ActiveFormName
   ENDIF

   _HMG_xContextMenuButtonIndex := GetControlIndex(Button, Parent)
   _HMG_xContextMenuParentHandle := GetFormHandle(Parent)
   _HMG_xContextMenuParentName := Parent
   _HMG_xContextMenuHandle := hmg_CreatePopupMenu(5)

RETURN

// Added HMG Ex v.1.3 build 38

/*
_DefineControlContextMenu(Control, Parent)
*/
PROCEDURE _DefineControlContextMenu(Control, Parent)

   _HMG_xContextMenuHandle := HMG_NULLHANDLE
   _HMG_xContextMenuParentHandle := HMG_NULLHANDLE
   _HMG_xContextPopupLevel := 0
   _HMG_xContextMenuParentName := ""
   _HMG_xMenuType := "OWNCONTEXT"
   _HMG_xMenuPopupLevel := 0

   IF Parent == NIL
      Parent := _HMG_ActiveFormName
   ENDIF

   IF hb_IsArray(Control)
      _HMG_xContextMenuButtonIndex := {}
      AEval(Control, {|x|AAdd(_HMG_xContextMenuButtonIndex, GetControlIndex(x, Parent))})
   ELSE
      _HMG_xContextMenuButtonIndex := GetControlIndex(Control, Parent)
   ENDIF

   _HMG_xContextMenuParentHandle := GetFormHandle(Parent)
   _HMG_xContextMenuParentName := Parent
   _HMG_xContextMenuHandle := hmg_CreatePopupMenu(5)

RETURN

/*
_ShowControlContextMenu(Control, Parent, lShow)
*/
PROCEDURE _ShowControlContextMenu(Control, Parent, lShow)

   LOCAL h := GetControlHandle(Control, Parent)
   LOCAL i
   LOCAL j

   IF hb_IsArray(h)
      FOR j := 1 TO Len(h)
         FOR i := 1 TO Len(_HMG_aControlsContextMenu)
            IF _HMG_aControlsContextMenu[i, 1] == h[j]
               _HMG_aControlsContextMenu[i, 4] := lShow
            ENDIF
         NEXT
      NEXT
   ELSE
      FOR i := 1 TO Len(_HMG_aControlsContextMenu)
         IF _HMG_aControlsContextMenu[i, 1] == h
            _HMG_aControlsContextMenu[i, 4] := lShow
         ENDIF
      NEXT
   ENDIF

RETURN

// Added HMG Ex v.1.3 build 35

/*
_GetMenuItemCaption(ItemName, FormName) -->
*/
FUNCTION _GetMenuItemCaption(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_xGetMenuCaption(a[1], a[2])

/*
_SetMenuItemCaption(ItemName, FormName, Caption) -->
*/
FUNCTION _SetMenuItemCaption(ItemName, FormName, Caption)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_xSetMenuCaption(a[1], a[2], Caption)

/*
_SetMenuItemBitmap(ItemName, FormName, Bitmap)
*/
PROCEDURE _SetMenuItemBitmap(ItemName, FormName, Bitmap)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   _HMG_aControlBrushHandle[GetControlIndex(ItemName, FormName)] := hmg_MenuItem_SetBitMaps(a[1], a[2], Bitmap, "")

RETURN

/*
_SetMenuItemIcon(ItemName, FormName, Icon)
*/
PROCEDURE _SetMenuItemIcon(ItemName, FormName, Icon)

   LOCAL a := _GetMenuIds(ItemName, FormName)

   _HMG_aControlBrushHandle[GetControlIndex(ItemName, FormName)] := hmg_MenuItem_SetIcon(a[1], a[2], Icon)

RETURN

/*
_SetMenuItemFont(ItemName, FormName, Font) -->
*/
FUNCTION _SetMenuItemFont(ItemName, FormName, Font)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_MenuItem_SetFont(a[1], a[2], iif(hb_IsString(Font), GetFontHandle(Font), Font))

/*
_InsertMenuItem(ItemName, FormName, caption, action, name, Image) --> NIL
*/
FUNCTION _InsertMenuItem(ItemName, FormName, caption, action, name, Image)

   LOCAL a := _GetMenuIds(ItemName, FormName)
   LOCAL Controlhandle := a[1]
   LOCAL hBitmap := 0
   LOCAL mVar
   LOCAL Id

   Id := _GetId()

   IF name != NIL
      mVar := "_" + _HMG_xMainMenuParentName + "_" + Name
#ifdef _NAMES_LIST_
      _SetNameList(mVar, Len(_HMG_aControlNames) + 1)
#else
      PUBLIC &mVar. := Len(_HMG_aControlNames) + 1
#endif
   ELSE
      mVar := "_MenuDummyVar"
#ifdef _NAMES_LIST_
      _SetNameList(mVar, 0)
#else
      PUBLIC &mVar. := 0
#endif
   ENDIF

   hmg_InsertMenuItem(Controlhandle, a[2], Id, caption)

   IF image != NIL
      hBitmap := hmg_MenuItem_SetBitMaps(Controlhandle, Id, image, "")
   ENDIF

   AAdd(_HMG_aControlType              , CONTROL_TYPE_MENU         )
   AAdd(_HMG_aControlNames             , Name                      )
   AAdd(_HMG_aControlHandles           , Controlhandle             )
   AAdd(_HMG_aControlParentHandles     , _HMG_xMainMenuParentHandle)
   AAdd(_HMG_aControlIds               , id                        )
   AAdd(_HMG_aControlProcedures        , action                    )
   AAdd(_HMG_aControlPageMap           , a[1]                      )
   AAdd(_HMG_aControlValue             , NIL                       )
   AAdd(_HMG_aControlInputMask         , ""                        )
   AAdd(_HMG_aControllostFocusProcedure, ""                        )
   AAdd(_HMG_aControlGotFocusProcedure , ""                        )
   AAdd(_HMG_aControlChangeProcedure   , ""                        )
   AAdd(_HMG_aControlDeleted           , .F.                       )
   AAdd(_HMG_aControlBkColor           , NIL                       )
   AAdd(_HMG_aControlFontColor         , NIL                       )
   AAdd(_HMG_aControlDblClick          , ""                        )
   AAdd(_HMG_aControlHeadClick         , {}                        )
   AAdd(_HMG_aControlRow               , 0                         )
   AAdd(_HMG_aControlCol               , 0                         )
   AAdd(_HMG_aControlWidth             , 0                         )
   AAdd(_HMG_aControlHeight            , 0                         )
   AAdd(_HMG_aControlSpacing           , 0                         )
   AAdd(_HMG_aControlContainerRow      , -1                        )
   AAdd(_HMG_aControlContainerCol      , -1                        )
   AAdd(_HMG_aControlPicture           , ""                        )
   AAdd(_HMG_aControlContainerHandle   , HMG_NULLHANDLE               )
   AAdd(_HMG_aControlFontName          , ""                        )
   AAdd(_HMG_aControlFontSize          , 0                         )
   AAdd(_HMG_aControlFontAttributes    , {.F., .F., .F., .F.}      )
   AAdd(_HMG_aControlToolTip           , ""                        )
   AAdd(_HMG_aControlRangeMin          , 0                         )
   AAdd(_HMG_aControlRangeMax          , 0                         )
   AAdd(_HMG_aControlCaption           , Caption                   )
   AAdd(_HMG_aControlVisible           , .T.                       )
   AAdd(_HMG_aControlHelpId            , 0                         )
   AAdd(_HMG_aControlFontHandle        , HMG_NULLHANDLE               )
   AAdd(_HMG_aControlBrushHandle       , hBitmap                   )
   AAdd(_HMG_aControlEnabled           , .T.                       )
   AAdd(_HMG_aControlMiscData1         , 0                         )
   AAdd(_HMG_aControlMiscData2         , ""                        )

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, Len(_HMG_aControlNames), mVar)
   ENDIF

RETURN NIL

/*
_ModifyMenuItem(ItemName, FormName, Caption, action, name, Image) --> NIL
*/
FUNCTION _ModifyMenuItem(ItemName, FormName, Caption, action, name, Image)

   LOCAL a := _GetMenuIds(ItemName, FormName)
   LOCAL x := GetControlIndex(ItemName, FormName)
   LOCAL mVar
   LOCAL Id

   Id := _HMG_aControlIds[x]

   IF name != NIL
      mVar := "_" + _HMG_xMainMenuParentName + "_" + Name
#ifdef _NAMES_LIST_
      _SetNameList(mVar, x)
#else
      PUBLIC &mVar. := x
#endif
   ELSE
      mVar := "_MenuDummyVar"
#ifdef _NAMES_LIST_
      _SetNameList(mVar, 0)
#else
      PUBLIC &mVar. := 0
#endif
   ENDIF

   hmg_ModifyMenuItem(a[1], a[2], Id, Caption)

   IF image != NIL
      hmg_DeleteObject(_HMG_aControlBrushHandle[x])
      _HMG_aControlBrushHandle[x] := hmg_MenuItem_SetBitMaps(a[1], Id, image, "")
   ENDIF

   _HMG_aControlNames[x] := Name
   _HMG_aControlProcedures[x] := action
   _HMG_aControlCaption[x] := Caption

RETURN NIL

/*
_RemoveMenuItem(ItemName, FormName) -->
*/
FUNCTION _RemoveMenuItem(ItemName, FormName)

   LOCAL a := _GetMenuIds(ItemName, FormName)

RETURN hmg_RemoveMenuItem(a[1], a[2])

/*
HMG_SetMenuTheme(nType, cFormName, aUserDefined) --> nType
*/
FUNCTION HMG_SetMenuTheme(nType, cFormName, aUserDefined)

   LOCAL aColors := hmg_GetMenuColors()

   hb_default(@nType, MNUCLR_THEME_DEFAULT)

   IF PCount() < 2 .AND. Len(_HMG_aFormHandles) > 0
      cFormName := ThisWindow.Name
   ENDIF

   IF PCount() > 2 .AND. !hb_IsArray(aUserDefined)
      aUserDefined := Array(24)
   ENDIF

   SWITCH nType

   CASE MNUCLR_THEME_DEFAULT

      aColors[MNUCLR_MENUBARBACKGROUND1] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARBACKGROUND2] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUBARSELECTEDTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUBARGRAYEDTEXT] := RGB(192, 192, 192)
      aColors[MNUCLR_MENUBARSELECTEDITEM1] := RGB(255, 252, 248)
      aColors[MNUCLR_MENUBARSELECTEDITEM2] := RGB(136, 133, 116)

      aColors[MNUCLR_MENUITEMTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUITEMSELECTEDTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUITEMGRAYEDTEXT] := RGB(192, 192, 192)
      aColors[MNUCLR_MENUITEMBACKGROUND1] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUITEMBACKGROUND2] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND1] := RGB(182, 189, 210)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND2] := RGB(182, 189, 210)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND1] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND2] := RGB(255, 255, 255)

      aColors[MNUCLR_IMAGEBACKGROUND1] := RGB(246, 245, 244)
      aColors[MNUCLR_IMAGEBACKGROUND2] := RGB(207, 210, 200)

      aColors[MNUCLR_SEPARATOR1] := RGB(168, 169, 163)
      aColors[MNUCLR_SEPARATOR2] := RGB(255, 255, 255)

      aColors[MNUCLR_SELECTEDITEMBORDER1] := RGB(10, 36, 106)
      aColors[MNUCLR_SELECTEDITEMBORDER2] := RGB(10, 36, 106)
      aColors[MNUCLR_SELECTEDITEMBORDER3] := RGB(10, 36, 106)
      aColors[MNUCLR_SELECTEDITEMBORDER4] := RGB(10, 36, 106)

      SET MENUCURSOR FULL
      SET MENUSEPARATOR SINGLE RIGHTALIGN
      SET MENUITEM BORDER 3DSTYLE

      EXIT

   CASE MNUCLR_THEME_XP

      aColors[MNUCLR_MENUBARBACKGROUND1] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARBACKGROUND2] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARTEXT] := waGetSysColor(COLOR_MENUTEXT)
      aColors[MNUCLR_MENUBARSELECTEDTEXT] := waGetSysColor(COLOR_HIGHLIGHTTEXT)
      aColors[MNUCLR_MENUBARGRAYEDTEXT] := waGetSysColor(COLOR_GRAYTEXT)
      aColors[MNUCLR_MENUBARSELECTEDITEM1] := waGetSysColor(COLOR_HIGHLIGHT)
      aColors[MNUCLR_MENUBARSELECTEDITEM2] := waGetSysColor(COLOR_HIGHLIGHT)

      aColors[MNUCLR_MENUITEMTEXT] := waGetSysColor(COLOR_MENUTEXT)
      aColors[MNUCLR_MENUITEMSELECTEDTEXT] := waGetSysColor(COLOR_HIGHLIGHTTEXT)
      aColors[MNUCLR_MENUITEMGRAYEDTEXT] := waGetSysColor(COLOR_GRAYTEXT)
      aColors[MNUCLR_MENUITEMBACKGROUND1] := IIF(_HMG_IsXP, waGetSysColor(COLOR_MENU), RGB(255, 255, 255))
      aColors[MNUCLR_MENUITEMBACKGROUND2] := IIF(_HMG_IsXP, waGetSysColor(COLOR_MENU), RGB(255, 255, 255))
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND1] := waGetSysColor(COLOR_HIGHLIGHT)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND2] := waGetSysColor(COLOR_HIGHLIGHT)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND1] := IIF(_HMG_IsXP, waGetSysColor(COLOR_MENU), RGB(255, 255, 255))
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND2] := IIF(_HMG_IsXP, waGetSysColor(COLOR_MENU), RGB(255, 255, 255))

      aColors[MNUCLR_IMAGEBACKGROUND1] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_IMAGEBACKGROUND2] := waGetSysColor(COLOR_3DFACE)

      aColors[MNUCLR_SEPARATOR1] := waGetSysColor(COLOR_GRAYTEXT)
      aColors[MNUCLR_SEPARATOR2] := waGetSysColor(COLOR_HIGHLIGHTTEXT)

      aColors[MNUCLR_SELECTEDITEMBORDER1] := waGetSysColor(COLOR_HIGHLIGHT)
      aColors[MNUCLR_SELECTEDITEMBORDER2] := waGetSysColor(COLOR_HIGHLIGHT)
      aColors[MNUCLR_SELECTEDITEMBORDER3] := waGetSysColor(COLOR_GRAYTEXT)
      aColors[MNUCLR_SELECTEDITEMBORDER4] := waGetSysColor(COLOR_HIGHLIGHTTEXT)

      SET MENUCURSOR FULL
      SET MENUSEPARATOR DOUBLE RIGHTALIGN
      SET MENUITEM BORDER FLAT

      EXIT

   CASE MNUCLR_THEME_2000

      aColors[MNUCLR_MENUBARBACKGROUND1] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARBACKGROUND2] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUBARSELECTEDTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUBARGRAYEDTEXT] := RGB(128, 128, 128)
      aColors[MNUCLR_MENUBARSELECTEDITEM1] := waGetSysColor(COLOR_3DFACE)
      aColors[MNUCLR_MENUBARSELECTEDITEM2] := waGetSysColor(COLOR_3DFACE)

      aColors[MNUCLR_MENUITEMTEXT] := RGB(0, 0, 0)
      aColors[MNUCLR_MENUITEMSELECTEDTEXT] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUITEMGRAYEDTEXT] := RGB(128, 128, 128)
      aColors[MNUCLR_MENUITEMBACKGROUND1] := RGB(212, 208, 200)
      aColors[MNUCLR_MENUITEMBACKGROUND2] := RGB(212, 208, 200)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND1] := RGB(10, 36, 106)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND2] := RGB(10, 36, 106)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND1] := RGB(212, 208, 200)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND2] := RGB(212, 208, 200)

      aColors[MNUCLR_IMAGEBACKGROUND1] := RGB(212, 208, 200)
      aColors[MNUCLR_IMAGEBACKGROUND2] := RGB(212, 208, 200)

      aColors[MNUCLR_SEPARATOR1] := RGB(128, 128, 128)
      aColors[MNUCLR_SEPARATOR2] := RGB(255, 255, 255)

      aColors[MNUCLR_SELECTEDITEMBORDER1] := RGB(10, 36, 106)
      aColors[MNUCLR_SELECTEDITEMBORDER2] := RGB(128, 128, 128)
      aColors[MNUCLR_SELECTEDITEMBORDER3] := RGB(10, 36, 106)
      aColors[MNUCLR_SELECTEDITEMBORDER4] := RGB(255, 255, 255)

      SET MENUCURSOR SHORT
      SET MENUSEPARATOR DOUBLE LEFTALIGN
      SET MENUITEM BORDER 3D

      EXIT

   CASE MNUCLR_THEME_DARK

      aColors[MNUCLR_MENUBARBACKGROUND1] := RGB(43, 43, 43)
      aColors[MNUCLR_MENUBARBACKGROUND2] := RGB(43, 43, 43)
      aColors[MNUCLR_MENUBARTEXT] := RGB(237, 237, 237)
      aColors[MNUCLR_MENUBARSELECTEDTEXT] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUBARGRAYEDTEXT] := RGB(128, 128, 128)
      aColors[MNUCLR_MENUBARSELECTEDITEM1] := RGB(65, 65, 65)
      aColors[MNUCLR_MENUBARSELECTEDITEM2] := RGB(65, 65, 65)

      aColors[MNUCLR_MENUITEMTEXT] := RGB(237, 237, 237)
      aColors[MNUCLR_MENUITEMSELECTEDTEXT] := RGB(255, 255, 255)
      aColors[MNUCLR_MENUITEMGRAYEDTEXT] := RGB(128, 128, 128)
      aColors[MNUCLR_MENUITEMBACKGROUND1] := RGB(43, 43, 43)
      aColors[MNUCLR_MENUITEMBACKGROUND2] := RGB(43, 43, 43)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND1] := RGB(65, 65, 65)
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND2] := RGB(65, 65, 65)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND1] := RGB(43, 43, 43)
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND2] := RGB(43, 43, 43)

      aColors[MNUCLR_IMAGEBACKGROUND1] := RGB(43, 43, 43)
      aColors[MNUCLR_IMAGEBACKGROUND2] := RGB(43, 43, 43)

      aColors[MNUCLR_SEPARATOR1] := RGB(128, 128, 128)
      aColors[MNUCLR_SEPARATOR2] := RGB(128, 128, 128)

      aColors[MNUCLR_SELECTEDITEMBORDER1] := RGB(75, 75, 75)
      aColors[MNUCLR_SELECTEDITEMBORDER2] := RGB(128, 128, 128)
      aColors[MNUCLR_SELECTEDITEMBORDER3] := RGB(75, 75, 75)
      aColors[MNUCLR_SELECTEDITEMBORDER4] := RGB(237, 237, 237)

      SET MENUCURSOR FULL
      SET MENUSEPARATOR SINGLE LEFTALIGN
      SET MENUITEM BORDER FLAT

      EXIT

   DEFAULT /* MNUCLR_THEME_USER_DEFINED */

      aColors[MNUCLR_MENUBARBACKGROUND1] := aUserDefined[1]
      aColors[MNUCLR_MENUBARBACKGROUND2] := aUserDefined[2]
      aColors[MNUCLR_MENUBARTEXT] := aUserDefined[3]
      aColors[MNUCLR_MENUBARSELECTEDTEXT] := aUserDefined[4]
      aColors[MNUCLR_MENUBARGRAYEDTEXT] := aUserDefined[5]
      aColors[MNUCLR_MENUBARSELECTEDITEM1] := aUserDefined[6]
      aColors[MNUCLR_MENUBARSELECTEDITEM2] := aUserDefined[7]

      aColors[MNUCLR_MENUITEMTEXT] := aUserDefined[8]
      aColors[MNUCLR_MENUITEMSELECTEDTEXT] := aUserDefined[9]
      aColors[MNUCLR_MENUITEMGRAYEDTEXT] := aUserDefined[10]
      aColors[MNUCLR_MENUITEMBACKGROUND1] := aUserDefined[11]
      aColors[MNUCLR_MENUITEMBACKGROUND2] := aUserDefined[12]
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND1] := aUserDefined[13]
      aColors[MNUCLR_MENUITEMSELECTEDBACKGROUND2] := aUserDefined[14]
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND1] := aUserDefined[15]
      aColors[MNUCLR_MENUITEMGRAYEDBACKGROUND2] := aUserDefined[16]

      aColors[MNUCLR_IMAGEBACKGROUND1] := aUserDefined[17]
      aColors[MNUCLR_IMAGEBACKGROUND2] := aUserDefined[18]

      aColors[MNUCLR_SEPARATOR1] := aUserDefined[19]
      aColors[MNUCLR_SEPARATOR2] := aUserDefined[20]

      aColors[MNUCLR_SELECTEDITEMBORDER1] := aUserDefined[21]
      aColors[MNUCLR_SELECTEDITEMBORDER2] := aUserDefined[22]
      aColors[MNUCLR_SELECTEDITEMBORDER3] := aUserDefined[23]
      aColors[MNUCLR_SELECTEDITEMBORDER4] := aUserDefined[24]

      SET MENUCURSOR FULL
      SET MENUSEPARATOR DOUBLE RIGHTALIGN
      SET MENUITEM BORDER FLAT

   ENDSWITCH

   hmg_SetMenuColors(aColors)

   IF hb_IsString(cFormName)
      SetProperty(cFormName, "BackColor", aColors[MNUCLR_MENUBARBACKGROUND1])
      hmg__ColorMenu(GetFormHandle(cFormName), nRGB2Arr(aColors[MNUCLR_MENUBARBACKGROUND2]))
   ENDIF

RETURN nType
