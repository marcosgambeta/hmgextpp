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

#include "SET_COMPILE_HMG_UNICODE.ch"

#include "i_winuser.ch"
#include "minigui.ch"
#include "error.ch"

#define lOpaque                      s_Global[1]
#define IsInitMenuPopup              s_Global[2]
#define lEnterSizeMove               s_Global[3]
#define lCellGridRowChanged          s_Global[4]
#define IsXPThemed                   s_Global[5]
#define nOldPage                     s_Global[6]
#define SpaceKeyIsPressedInGrid      s_Global[7]
#define nLastRadioGroupFocusedIndex  s_Global[8]

#ifdef _TSBROWSE_
   MEMVAR _TSB_aControlhWnd
   MEMVAR _TSB_aControlObjects
#endif

/*
Events(hWnd, nMsg, wParam, lParam) --> numeric
*/
FUNCTION Events(hWnd, nMsg, wParam, lParam)

   LOCAL NextControlHandle
   LOCAL oGet
   LOCAL aPos
   LOCAL aCellData
   LOCAL NewPos
   LOCAL NewHPos
   LOCAL NewVPos
   LOCAL maskstart
   LOCAL ControlCount
   LOCAL mVar
   LOCAL TmpStr
   LOCAL Tmp
   LOCAL nGridRowValue
   LOCAL i
   LOCAL z
   LOCAL x
   LOCAL lvc
   LOCAL k
   LOCAL ts
   LOCAL r
   LOCAL a
#ifdef _DBFBROWSE_
   LOCAL BackRec
   LOCAL BackArea
   LOCAL BrowseArea
   LOCAL RecordCount
   LOCAL SkipCount
   LOCAL aTemp
   LOCAL aTemp2
   LOCAL DeltaSelect
   LOCAL dBc
   LOCAL dFc
   LOCAL xs
   LOCAL xd
   LOCAL nr
   LOCAL hws
   LOCAL hwm
#endif
   LOCAL backcolor
   LOCAL fontcolor
   LOCAL titlebkclr
   LOCAL titlefrclr
   LOCAL trlfontclr
   LOCAL nDestinationColumn
   LOCAL nFrozenColumnCount
   LOCAL aOriginalColumnWidths
#ifdef _USERINIT_
   LOCAL cProc
#endif
   LOCAL cParent
   LOCAL hEdit
   LOCAL nStart
   LOCAL nEnd
   LOCAL TxtLen

   STATIC s_Global := {.F., .F., .F., .F., NIL, NIL, 0, 0}

#ifdef _TSBROWSE_
   oGet := GetObjectByHandle(hWnd)
   IF hb_IsObject(oGet)

      r := oGet:HandleEvent(nMsg, wParam, lParam)

      IF hb_IsNumeric(r)
         IF r != 0
            RETURN r
         ENDIF
      ENDIF

   ENDIF
#endif
#ifdef _USERINIT_
   FOR EACH cProc IN _HMG_aCustomEventProcedure

      r := &cProc(hWnd, nMsg, wParam, lParam)

      IF r != NIL
         RETURN r
      ENDIF

   NEXT
#endif

   SWITCH nMsg
   //**************************************************************************
   CASE WM_MEASUREITEM
   //**************************************************************************

      SWITCH GETMISCTLTYPE(lParam)

      CASE ODT_MENU
         hmg__OnMeasureMenuItem(hWnd, nMsg, wParam, lParam)
         EXIT

      CASE ODT_LISTBOX
         _OnMeasureListBoxItem(lParam)

      END SWITCH
      EXIT
   //**************************************************************************
   CASE WM_DRAWITEM
   //**************************************************************************

      i := AScan(_HMG_aFormHandles, hWnd)

      ts := (i > 0 .AND. _IsControlDefined("StatusBar", _HMG_aFormNames[i]))

      SWITCH hmg_GETOWNBTNCTLTYPE(lParam)

      CASE ODT_MENU
         IF ts .AND. hmg_GetDrawItemHandle(lParam) == GetControlHandle("StatusBar", _HMG_aFormNames[i])
            _OnDrawStatusItem(hWnd, lParam)
            RETURN 0
         ELSE
            hmg__OnDrawMenuItem(lParam)
            RETURN 0
         ENDIF

      CASE ODT_LISTBOX
         _OnDrawListBoxItem(lParam)
         RETURN 0

      CASE ODT_BUTTON
         RETURN OwnButtonPaint(lParam)

      CASE ODT_TAB
         RETURN OwnTabPaint(lParam)

      END SWITCH

      IF ts
         _OnDrawStatusItem(hWnd, lParam)
      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_CTLCOLORSTATIC
   //**************************************************************************

      i := AScan(_HMG_aControlHandles, hmg_numbertohandle(lParam))

      IF i > 0

         TmpStr := _HMG_aControlType[i]

         IF TmpStr == CONTROL_TYPE_GETBOX .OR. ;
            TmpStr == CONTROL_TYPE_LABEL .OR. ;
            TmpStr == CONTROL_TYPE_HYPERLINK .OR. ;
            TmpStr == CONTROL_TYPE_CHECKBOX .OR. ;
            TmpStr == CONTROL_TYPE_FRAME .OR. ;
            TmpStr == CONTROL_TYPE_SLIDER .OR. ;
            TmpStr == CONTROL_TYPE_NUMTEXT .OR. ;
            TmpStr == CONTROL_TYPE_MASKEDTEXT .OR. ;
            TmpStr == CONTROL_TYPE_CHARMASKTEXT .OR. ;
            TmpStr == CONTROL_TYPE_BTNTEXT .OR. ;
            TmpStr == CONTROL_TYPE_BTNNUMTEXT .OR. ;
            TmpStr == CONTROL_TYPE_EDIT .OR. ;
            TmpStr == CONTROL_TYPE_ANIMATEBOX .OR. ;
            TmpStr == CONTROL_TYPE_CHECKLABEL

            IF TmpStr == CONTROL_TYPE_GETBOX .OR. ;
               TmpStr == CONTROL_TYPE_NUMTEXT .OR. ;
               TmpStr == CONTROL_TYPE_MASKEDTEXT .OR. ;
               TmpStr == CONTROL_TYPE_CHARMASKTEXT .OR. ;
               TmpStr == CONTROL_TYPE_BTNTEXT .OR. ;
               TmpStr == CONTROL_TYPE_BTNNUMTEXT .OR. ;
               TmpStr == CONTROL_TYPE_EDIT

               IF hmg_IsWindowEnabled(_HMG_aControlHandles[i])

                  IF _HMG_aControlFontColor[i] != NIL

                     IF hb_IsArray(_HMG_aControlFontColor[i, 2]) .AND. Len(_HMG_aControlFontColor[i, 2]) == 3
                        hmg_SetTextColor(wParam, _HMG_aControlFontColor[i, 2, 1], _HMG_aControlFontColor[i, 2, 2], _HMG_aControlFontColor[i, 2, 3])
                     ELSEIF hb_IsNumeric(_HMG_aControlFontColor[i, 2]) .AND. Len(_HMG_aControlFontColor[i]) == 3
                        hmg_SetTextColor(wParam, _HMG_aControlFontColor[i, 1], _HMG_aControlFontColor[i, 2], _HMG_aControlFontColor[i, 3])
                     ENDIF

                  ENDIF

                  IF _HMG_aControlBkColor[i] != NIL

                     IF hb_IsArray(_HMG_aControlBkColor[i, 2]) .AND. Len(_HMG_aControlBkColor[i, 2]) == 3

                        hmg_SetBkColor(wParam, _HMG_aControlBkColor[i, 2, 1], _HMG_aControlBkColor[i, 2, 2], _HMG_aControlBkColor[i, 2, 3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i, 2, 1], _HMG_aControlBkColor[i, 2, 2], _HMG_aControlBkColor[i, 2, 3])
                        RETURN _HMG_aControlBrushHandle[i]

                     ELSE

                        IF hb_IsNumeric(_HMG_aControlBkColor[i, 2]) .AND. Len(_HMG_aControlBkColor[i]) == 3

                           hmg_SetBkColor(wParam, _HMG_aControlBkColor[i, 1], _HMG_aControlBkColor[i, 2], _HMG_aControlBkColor[i, 3])
                           hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                           _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i, 1], _HMG_aControlBkColor[i, 2], _HMG_aControlBkColor[i, 3])
                           RETURN _HMG_aControlBrushHandle[i]

                        ENDIF

                     ENDIF

                  ELSE

                     a := nRGB2Arr(hmg_GetSysColor(COLOR_3DFACE))
                     hmg_SetBkColor(wParam, a[1], a[2], a[3])
                     hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                     _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                     RETURN _HMG_aControlBrushHandle[i]

                  ENDIF

               ENDIF

            ELSE

               IF IsXPThemed == NIL
                  IsXPThemed := _HMG_IsThemed
               ENDIF

               Tmp := _HMG_aControlContainerRow[i] != -1 .AND. _HMG_aControlContainerCol[i] != -1 .AND. _HMG_aControlBkColor[i] == NIL

               IF IsXPThemed .AND. TmpStr == CONTROL_TYPE_SLIDER .AND. Tmp

                  IF (a := _GetBackColor(_HMG_aControlFontHandle[i], _HMG_aControlMiscData1[i])) != NIL
                     _HMG_aControlBkColor[i] := a
                  ELSE
                     IF !_HMG_aControlDblClick[i] .AND. !lOpaque
                        r := GetControlIndex(_HMG_aControlFontHandle[i], _HMG_aControlMiscData1[i])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[r])
                        z := GetControlHandle(_HMG_aControlFontHandle[i], _HMG_aControlMiscData1[i])
                        _HMG_aControlBrushHandle[r] := hmg_GetTabBrush(z)
                        RETURN hmg_GetTabbedControlBrush(wParam, lParam, z, _HMG_aControlBrushHandle[r])
                     ENDIF
                  ENDIF

               ENDIF

               IF IsXPThemed .AND. TmpStr == CONTROL_TYPE_FRAME .AND. Tmp

                  IF (a := _GetBackColor(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])) != NIL
                     IF hb_IsLogical(_HMG_aControlInputMask[i]) .AND. _HMG_aControlInputMask[i]
                        hmg_SetBkColor(wParam, a[1], a[2], a[3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                        RETURN _HMG_aControlBrushHandle[i]
                     ENDIF
                  ELSE
                     IF !_HMG_aControlDblClick[i]
                        r := GetControlIndex(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[r])
                        z := GetControlHandle(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                        _HMG_aControlBrushHandle[r] := hmg_GetTabBrush(z)
                        RETURN hmg_GetTabbedControlBrush(wParam, lParam, z, _HMG_aControlBrushHandle[r])
                     ELSE
                        lOpaque := .T.
                     ENDIF
                  ENDIF

               ENDIF

               IF IsXPThemed .AND. TmpStr == CONTROL_TYPE_CHECKBOX .AND. Tmp

                  lvc := (hb_IsLogical(_HMG_aControlInputMask[i]) .AND. !_HMG_aControlInputMask[i])

                  IF (a := _GetBackColor(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])) != NIL
                     IF lvc
                        hmg_SetBkColor(wParam, a[1], a[2], a[3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                        RETURN _HMG_aControlBrushHandle[i]
                     ENDIF
                  ELSE
#if defined(_PANEL_) .AND. defined(_HMG_COMPAT_)
                     IF GetFormNameByHandle(_HMG_aControlParentHandles[i], @z) > 0 .AND. GetWindowType(z) == "P"
                     ELSEIF !_HMG_aControlDblClick[i] .AND. (!lOpaque .OR. !lvc)
#else
                     IF !_HMG_aControlDblClick[i] .AND. (!lOpaque .OR. !lvc)
#endif
                        r := GetControlIndex(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[r])
                        z := GetControlHandle(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                        _HMG_aControlBrushHandle[r] := hmg_GetTabBrush(z)
                        RETURN hmg_GetTabbedControlBrush(wParam, lParam, z, _HMG_aControlBrushHandle[r])
                     ENDIF
                  ENDIF

               ENDIF

               IF _HMG_aControlFontColor[i] != NIL
                  hmg_SetTextColor(wParam, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])
               ENDIF

               IF hb_IsLogical(_HMG_aControlInputMask[i])
                  IF _HMG_aControlInputMask[i] .AND. ;
                     (_HMG_aFormBkColor[x := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])][1] != -1 .OR. ;
                     Len(HMG_GetFormControls(_HMG_aFormNames[x], "IMAGE")) > 0)
                     hmg_SetBkMode(wParam, TRANSPARENT)
                     RETURN hmg_GetStockObject(NULL_BRUSH)
                  ENDIF
               ENDIF

               IF IsXPThemed .AND. TmpStr == CONTROL_TYPE_LABEL .AND. Tmp

                  IF (a := _GetBackColor(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])) == NIL
                     IF _HMG_aControlDblClick[i]
                        a := nRGB2Arr(hmg_GetSysColor(COLOR_BTNFACE))
#if defined(_PANEL_) .AND. defined(_HMG_COMPAT_)
                     ELSEIF GetFormNameByHandle(_HMG_aControlParentHandles[i], @z) > 0 .AND. GetWindowType(z) == "P"
                        a := NIL
#endif
                     ELSEIF (r := GetControlIndex(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])) > 0
                        hmg_DeleteObject(_HMG_aControlBrushHandle[r])
                        z := GetControlHandle(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                        _HMG_aControlBrushHandle[r] := hmg_GetTabBrush(z)
                        RETURN hmg_GetTabbedControlBrush(wParam, lParam, z, _HMG_aControlBrushHandle[r])
                     ELSE
                        a := _HMG_ActiveTabColor[1]
                     ENDIF
                  ENDIF

                  IF hb_IsArray(a)
                     hmg_SetBkColor(wParam, a[1], a[2], a[3])
                     hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                     _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                     RETURN _HMG_aControlBrushHandle[i]
                  ENDIF

               ENDIF

               IF _HMG_aControlBkColor[i] != NIL

                  hmg_SetBkColor(wParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                  hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                  _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                  RETURN _HMG_aControlBrushHandle[i]

               ELSE

                  a := nRGB2Arr(hmg_GetSysColor(COLOR_3DFACE))
                  hmg_SetBkColor(wParam, a[1], a[2], a[3])
                  hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                  _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                  RETURN _HMG_aControlBrushHandle[i]

               ENDIF

            ENDIF

         ENDIF

      ELSE

         IF IsXPThemed == NIL
            IsXPThemed := _HMG_IsThemed
         ENDIF

         ControlCount := Len(_HMG_aControlHandles)

         FOR i := 1 TO ControlCount

            Tmp := _HMG_aControlHandles[i]
            IF hb_IsArray(Tmp)

               IF _HMG_aControlType[i] == CONTROL_TYPE_RADIOGROUP

                  FOR x := 1 TO Len(Tmp)

                     IF Tmp[x] == hmg_numbertohandle(lParam)

                        IF _HMG_aControlFontColor[i] != NIL
                           hmg_SetTextColor(wParam, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])
                        ENDIF

                        lvc := (hb_IsLogical(_HMG_aControlInputMask[i]) .AND. _HMG_aControlInputMask[i])

                        IF IsXPThemed .AND. _HMG_aControlContainerRow[i] != -1 .AND. _HMG_aControlContainerCol[i] != -1

                           IF (a := _GetBackColor(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])) != NIL

                              IF lvc
                                 _HMG_aControlBkColor[i] := a
                              ENDIF

                           ELSE

#if defined(_PANEL_) .AND. defined(_HMG_COMPAT_)
                              IF GetFormNameByHandle(_HMG_aControlParentHandles[i], @z) > 0 .AND. GetWindowType(z) == "P"
                              ELSEIF !_HMG_aControlDblClick[i] .AND. (!lOpaque .OR. lvc) .AND. _HMG_aControlBkColor[i] == NIL
#else
                              IF !_HMG_aControlDblClick[i] .AND. (!lOpaque .OR. lvc) .AND. _HMG_aControlBkColor[i] == NIL
#endif
                                 r := GetControlIndex(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                                 hmg_DeleteObject(_HMG_aControlBrushHandle[r])
                                 z := GetControlHandle(_HMG_aControlRangeMin[i], _HMG_aControlRangeMax[i])
                                 _HMG_aControlBrushHandle[r] := hmg_GetTabBrush(z)
                                 RETURN hmg_GetTabbedControlBrush(wParam, lParam, z, _HMG_aControlBrushHandle[r])
                              ENDIF

                           ENDIF

                        ENDIF

                        IF hb_IsLogical(_HMG_aControlInputMask[i])

                           IF _HMG_aControlInputMask[i] .AND. _HMG_aControlBkColor[i] == NIL

                              IF IsXPThemed .AND. (a := _HMG_aFormBkColor[AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])])[1] != -1
                                 _HMG_aControlBkColor[i] := a
                              ELSE
                                 hmg_SetBkMode(wParam, TRANSPARENT)
                                 RETURN hmg_GetStockObject(NULL_BRUSH)
                              ENDIF

                           ENDIF

                        ENDIF

                        IF _HMG_aControlBkColor[i] != NIL

                           hmg_SetBkColor(wParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                           IF x == 1
                              hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                              _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                           ENDIF
                           RETURN _HMG_aControlBrushHandle[i]

                        ELSE

                           IF x == 1
                              hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                              _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(hmg_GetRed(hmg_GetSysColor(COLOR_3DFACE)), hmg_GetGreen(hmg_GetSysColor(COLOR_3DFACE)), hmg_GetBlue(hmg_GetSysColor(COLOR_3DFACE)))
                           ENDIF
                           hmg_SetBkColor(wParam, hmg_GetRed(hmg_GetSysColor(COLOR_3DFACE)), hmg_GetGreen(hmg_GetSysColor(COLOR_3DFACE)), hmg_GetBlue(hmg_GetSysColor(COLOR_3DFACE)))
                           RETURN _HMG_aControlBrushHandle[i]

                        ENDIF

                     ENDIF

                  NEXT x

               ENDIF

            ENDIF

         NEXT i

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_CTLCOLOREDIT
   CASE WM_CTLCOLORLISTBOX
   //**************************************************************************

      i := AScan(_HMG_aControlHandles, hmg_numbertohandle(lParam))

      IF i > 0

         TmpStr := _HMG_aControlType[i]

         IF TmpStr == CONTROL_TYPE_GETBOX .OR. ;
            TmpStr == CONTROL_TYPE_NUMTEXT .OR. ;
            TmpStr == CONTROL_TYPE_MASKEDTEXT .OR. ;
            TmpStr == CONTROL_TYPE_CHARMASKTEXT .OR. ;
            TmpStr == CONTROL_TYPE_EDIT .OR. ;
            TmpStr == CONTROL_TYPE_BTNTEXT .OR. ;
            TmpStr == CONTROL_TYPE_BTNNUMTEXT .OR. ;
            TmpStr == CONTROL_TYPE_MULTILIST .OR. ;
            TmpStr == CONTROL_TYPE_COMBO

            IF _HMG_aControlFontColor[i] != NIL

               IF hb_IsNumeric(_HMG_aControlFontColor[i, 1])

                  hmg_SetTextColor(wParam, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])

               ELSEIF hb_IsArray(_HMG_aControlFontColor[i, 1])

                  IF Len(_HMG_aControlFontColor[i]) > 2 .AND. hmg_GetFocus() == _HMG_aControlHandles[i]
                     hmg_SetTextColor(wParam, _HMG_aControlFontColor[i, 3, 1], _HMG_aControlFontColor[i, 3, 2], _HMG_aControlFontColor[i, 3, 3])
                  ELSE
                     hmg_SetTextColor(wParam, _HMG_aControlFontColor[i, 1, 1], _HMG_aControlFontColor[i, 1, 2], _HMG_aControlFontColor[i, 1, 3])
                  ENDIF

               ENDIF

               IF _HMG_aControlBkColor[i] == NIL

                  a := nRGB2Arr(hmg_GetSysColor(COLOR_WINDOW))
                  hmg_SetBkColor(wParam, a[1], a[2], a[3])
                  hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                  _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                  RETURN _HMG_aControlBrushHandle[i]

               ENDIF

            ENDIF

            IF _HMG_aControlBkColor[i] != NIL

               IF hb_IsNumeric(_HMG_aControlBkColor[i, 1])

                  hmg_SetBkColor(wParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                  hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                  _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                  RETURN _HMG_aControlBrushHandle[i]

               ELSE

                  IF hb_IsArray(_HMG_aControlBkColor[i, 1])

                     IF Len(_HMG_aControlBkColor[i]) == 3 .AND. hmg_GetFocus() == _HMG_aControlHandles[i]

                        hmg_SetBkColor(wParam, _HMG_aControlBkColor[i, 3, 1], _HMG_aControlBkColor[i, 3, 2], _HMG_aControlBkColor[i, 3, 3])
                        hmg_DeleteObject (_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i, 3, 1], _HMG_aControlBkColor[i, 3, 2], _HMG_aControlBkColor[i, 3, 3])
                        RETURN _HMG_aControlBrushHandle[i]

                     ELSE

                        hmg_SetBkColor(wParam, _HMG_aControlBkColor[i, 1, 1], _HMG_aControlBkColor[i, 1, 2], _HMG_aControlBkColor[i, 1, 3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i, 1, 1], _HMG_aControlBkColor[i, 1, 2], _HMG_aControlBkColor[i, 1, 3])
                        RETURN _HMG_aControlBrushHandle[i]

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ELSE

         ControlCount := Len(_HMG_aControlHandles)

         FOR i := 1 TO ControlCount

            IF hb_IsArray(_HMG_aControlHandles[i])

               IF _HMG_aControlType[i] == CONTROL_TYPE_SPINNER

                  IF _HMG_aControlHandles[i][1] == lParam // TODO: compare pointer with numeric

                     IF _HMG_aControlFontColor[i] != NIL

                        hmg_SetTextColor(wParam, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])
                        IF _HMG_aControlBkColor[i] == NIL
                           a := nRGB2Arr(hmg_GetSysColor(COLOR_WINDOW))
                           hmg_SetBkColor(wParam, a[1], a[2], a[3])
                           hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                           _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                           RETURN _HMG_aControlBrushHandle[i]
                        ENDIF

                     ENDIF

                     IF _HMG_aControlBkColor[i] != NIL

                        hmg_SetBkColor(wParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                        RETURN _HMG_aControlBrushHandle[i]

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

            IF _HMG_aControlType[i] == CONTROL_TYPE_COMBO

               IF _HMG_aControlMiscData1[i][2] .AND. (hmg_GetFocus() == _HMG_aControlRangeMin[i] .OR. _HMG_aControlRangeMin[i] == lParam) .OR. ;
                  hmg_GetFocus() == _HMG_aControlHandles[i] .AND. (_HMG_aControlHandles[i] == lParam .OR. !_HMG_aControlMiscData1[i][2]) // TODO: compare pointer with numeric

                  IF _HMG_aControlFontColor[i] != NIL

                     hmg_SetTextColor(wParam, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])
                     IF _HMG_aControlBkColor[i] == NIL
                        a := nRGB2Arr(hmg_GetSysColor(COLOR_WINDOW))
                        hmg_SetBkColor(wParam, a[1], a[2], a[3])
                        hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                        _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(a[1], a[2], a[3])
                        RETURN _HMG_aControlBrushHandle[i]
                     ENDIF

                  ENDIF

                  IF _HMG_aControlBkColor[i] != NIL

                     IF _HMG_aControlRangeMin[i] == lParam
                        hmg_SetBkColor(lParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                     ENDIF
                     hmg_SetBkColor(wParam, _HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                     hmg_DeleteObject(_HMG_aControlBrushHandle[i])
                     _HMG_aControlBrushHandle[i] := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
                     RETURN _HMG_aControlBrushHandle[i]

                  ENDIF

               ENDIF

            ENDIF

         NEXT i

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MENUSELECT
   //**************************************************************************

      IF hb_bitand(hmg_HiWord(wParam), MF_HILITE) != 0

         i := AScan(_HMG_aControlIds, hmg_LoWord(wParam))  // hmg_LoWord(wParam) => menu id

         IF i > 0

            IF (x := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])) > 0

               IF _IsControlDefined("StatusBar", _HMG_aFormNames[x])

                  IF _HMG_aControlValue[i] != NIL

                     SetProperty(_HMG_aFormNames[x], "StatusBar", "Item", 1, _HMG_aControlValue[i])

                  ELSEIF hb_IsString(_HMG_DefaultStatusBarMessage)

                     SetProperty(_HMG_aFormNames[x], "StatusBar", "Item", 1, _HMG_DefaultStatusBarMessage)

                  ENDIF

               ENDIF

            ENDIF

         ELSEIF (x := AScan(_HMG_aFormHandles, hWnd)) > 0

            IF _IsControlDefined("StatusBar", _HMG_aFormNames[x])

               IF hb_IsString(_HMG_DefaultStatusBarMessage)
                  SetProperty(_HMG_aFormNames[x], "StatusBar", "Item", 1, _HMG_DefaultStatusBarMessage)
               ENDIF

            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MENUCHAR
   //**************************************************************************

      IF IsExtendedMenuStyleActive()

         x := Chr(hmg_LoWord(wParam))  // hmg_LoWord(wParam) => pressed char code

         IF (i := AScan(GetMenuItems((hmg_HiWord(wParam) == MF_POPUP), lParam), {|r|"&" + Upper(x) $ Upper(r)})) > 0
            RETURN MAKELRESULT(i - 1, MNC_EXECUTE)
         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_INITMENUPOPUP
   //**************************************************************************
      IsInitMenuPopup := .T.
      EXIT
   //**************************************************************************
   CASE WM_UNINITMENUPOPUP
   //**************************************************************************
      IsInitMenuPopup := .F.
      EXIT
   //**************************************************************************
   CASE WM_HOTKEY
   //**************************************************************************

      // Process HotKeys

      i := AScan(_HMG_aControlIds, wParam)

      IF i > 0

         IF _HMG_aControlType[i] == CONTROL_TYPE_HOTKEY

            IF _HMG_aControlValue[i] == VK_ESCAPE .AND. IsInitMenuPopup
               hmg__CloseMenu()
               RETURN 0
            ENDIF

            //JP MDI HotKey
            IF _HMG_BeginWindowMDIActive

               IF _HMG_aControlParentHandles[i] == GetActiveMdiHandle() .OR. _HMG_InplaceParentHandle != 0

                  IF _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
                     RETURN 0
                  ENDIF

               ELSEIF _HMG_aControlParentHandles[i] == hmg_GetActiveWindow() .OR. _HMG_GlobalHotkeys

                  IF _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
                     RETURN 0
                  ENDIF

               ENDIF
            //End JP
            ELSE

               IF _HMG_aControlParentHandles[i] == hmg_GetForegroundWindow() .OR. _HMG_GlobalHotkeys
                  IF _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
                     RETURN 0
                  ENDIF
               ENDIF

            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MOUSEWHEEL
   //**************************************************************************

      hwnd := 0

      IF !_HMG_AutoAdjust

         i := AScan(_HMG_aFormHandles, hmg_GetFocus())

         IF i > 0

            IF _HMG_aFormVirtualHeight[i] > 0
               hwnd := _HMG_aFormHandles[i]
            ENDIF

         ELSE

            IF (i := AScan(_HMG_aControlHandles, hmg_GetFocus())) > 0

               IF (x := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])) > 0

                  IF _HMG_aFormVirtualHeight[x] > 0
                     hwnd := _HMG_aFormHandles[x]
                  ENDIF

               ENDIF

            ELSE

               ControlCount := Len(_HMG_aControlHandles)

               FOR i := 1 TO ControlCount

                  IF _HMG_aControlType[i] == CONTROL_TYPE_RADIOGROUP

                     IF AScan(_HMG_aControlHandles[i], hmg_GetFocus()) > 0

                        IF (z := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])) > 0

                           IF _HMG_aFormVirtualHeight[z] > 0
                              hwnd := _HMG_aFormHandles[z]
                              EXIT
                           ENDIF

                        ENDIF

                     ENDIF

                  ENDIF

               NEXT i

            ENDIF

         ENDIF

      ENDIF

      IF hwnd != 0

         IF hmg_HiWord(wParam) == WHEEL_DELTA

            IF hmg_GetScrollPos(hwnd, SB_VERT) < 25
               hmg_SendMessage(hwnd, WM_VSCROLL, SB_TOP, 0)
            ELSE
               hmg_SendMessage(hwnd, WM_VSCROLL, SB_PAGEUP, 0)
            ENDIF

         ELSE

            IF hmg_GetScrollPos(hwnd, SB_VERT) >= hmg_GetScrollRangeMax(hwnd, SB_VERT) - 10
               hmg_SendMessage(hwnd, WM_VSCROLL, SB_BOTTOM, 0)
            ELSE
               hmg_SendMessage(hwnd, WM_VSCROLL, SB_PAGEDOWN, 0)
            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_CREATE
   //**************************************************************************

      IF _HMG_BeginWindowMDIActive .AND. _HMG_MainClientMDIHandle == 0
         _HMG_MainClientMDIHandle := InitMDIClientWindow(hWnd)
      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_ACTIVATE
   //**************************************************************************

      i := AScan(_HMG_aFormhandles, hWnd)

      IF i > 0

         IF hmg_LoWord(wParam) == 0

            IF !_HMG_GlobalHotkeys

               FOR EACH r IN _HMG_aControlType

                  IF r == CONTROL_TYPE_HOTKEY
                     x := hb_enumindex(r)
                     hmg_ReleaseHotKey(_HMG_aControlParentHandles[x], _HMG_aControlIds[x])
                  ENDIF

               NEXT

            ENDIF

            _HMG_aFormFocusedControl[i] := hmg_GetFocus()

            _DoWindowEventProcedure(_HMG_aFormLostFocusProcedure[i], i, "WINDOW_LOSTFOCUS")

         ELSE

            hmg_UpdateWindow(hWnd)

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_SETFOCUS
   //**************************************************************************

      i := AScan(_HMG_aFormHandles, hWnd)

      IF i > 0

         IF _HMG_aFormActive[i] .AND. _HMG_aFormType[i] != "X"
            _HMG_UserWindowHandle := hWnd
         ENDIF

         ControlCount := Len(_HMG_aControlHandles)

         FOR x := 1 TO ControlCount
            //JP MDI HotKey
            IF _HMG_aControlType[x] == CONTROL_TYPE_HOTKEY

               IF !_HMG_BeginWindowMDIActive
                  hmg_ReleaseHotKey(_HMG_aControlParentHandles[x], _HMG_aControlIds[x])
               ELSEIF _HMG_aControlParentHandles[x] != GetFormHandle(_HMG_MainClientMDIName) .OR. _HMG_InplaceParentHandle != 0
                  hmg_ReleaseHotKey(_HMG_aControlParentHandles[x], _HMG_aControlIds[x])
               ENDIF

            ENDIF
            //End
         NEXT x

         FOR x := 1 TO ControlCount

            IF _HMG_aControlType[x] == CONTROL_TYPE_HOTKEY

               IF _HMG_aControlParentHandles[x] == hWnd
                  hmg_InitHotKey(hWnd, _HMG_aControlPageMap[x], _HMG_aControlValue[x], _HMG_aControlIds[x])
               ENDIF

            ENDIF

         NEXT x

         _DoWindowEventProcedure(_HMG_aFormGotFocusProcedure[i], i, "WINDOW_GOTFOCUS")

         IF _HMG_IsModalActive .AND. Empty(_HMG_InplaceParentHandle) .AND. ;
            (_HMG_aFormVirtualWidth[i] == 0 .OR. _HMG_aFormVirtualHeight[i] == 0) .AND. ;
            _HMG_SplitLastControl != "TOOLBAR" // TODO:

            IF _HMG_aFormType[i] != "M"

               IF iswinnt()

                  IF !_OnFlashExit()
                     hmg_BringWindowToTop(_HMG_ActiveModalHandle)
                     // Form's caption blinking if a top window is not Modal window
                     hmg_FlashWindowEx(_HMG_ActiveModalHandle, 1, 5, 60)
                  ENDIF

               ELSE

                  IF !_OnFlashExit()
                     hmg_BringWindowToTop(_HMG_ActiveModalHandle)
                  ENDIF

               ENDIF

            ENDIF

         ENDIF

         IF !empty(_HMG_aFormFocusedControl[i])
            hmg_setfocus(_HMG_aFormFocusedControl[i])
         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_HELP
   //**************************************************************************

      i := AScan(_HMG_aControlHandles, hmg_GetHelpData(lParam)) // TODO:

      IF i > 0

         IF Right(AllTrim(Upper(_HMG_ActiveHelpFile)), 4) == ".CHM"

            _HMG_nTopic := _HMG_aControlHelpId[i]

            _Execute(hWnd, "open", "hh.exe", ;
               iif(hb_IsChar(_HMG_nTopic), Chr(34) + _HMG_ActiveHelpFile + "::/" + AllTrim(_HMG_nTopic) + Chr(34), ;
               iif(hb_IsNumeric(_HMG_nTopic) .AND. _HMG_nTopic > 0, "-mapid " + hb_ntos(_HMG_nTopic) + " " + ;
               _HMG_ActiveHelpFile, Chr(34) + _HMG_ActiveHelpFile + Chr(34))), , SW_SHOW)

         ELSE

            hmg_WinHelp(hWnd, _HMG_ActiveHelpFile, 2, _HMG_aControlHelpId[i])

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_GETMINMAXINFO
   //**************************************************************************

      RETURN _OnGetMinMaxInfo(hWnd, lParam)
   //**************************************************************************
   CASE WM_VSCROLL
   //**************************************************************************

      i := AScan(_HMG_aFormHandles, hWnd)

      IF i > 0

         // Vertical ScrollBar Processing

         IF _HMG_aFormVirtualHeight[i] > 0 .AND. lParam == 0

            IF _HMG_aFormRebarhandle[i] > 0
               MsgMiniGuiError("SplitBox's Parent Window cannot be a 'Virtual Dimensioned' window (use 'Virtual Dimensioned' SplitChild instead).")
            ENDIF

            z := iif(_HMG_aScrollStep[1] > 0, _HMG_aScrollStep[1], hmg_GetScrollRangeMax(hwnd, SB_VERT) / _HMG_aScrollStep[2])

            IF hmg_LoWord(wParam) == SB_LINEDOWN

               NewPos := hmg_GetScrollPos(hwnd, SB_VERT) + z
               IF NewPos >= hmg_GetScrollRangeMax(hwnd, SB_VERT) - 10
                  NewPos := hmg_GetScrollRangeMax(hwnd, SB_VERT)
               ENDIF
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_LINEUP

               NewPos := hmg_GetScrollPos(hwnd, SB_VERT) - z
               IF NewPos < 10
                  NewPos := 0
               ENDIF
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_TOP

               NewPos := 0
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_BOTTOM

               NewPos := hmg_GetScrollRangeMax(hwnd, SB_VERT)
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_PAGEUP

               NewPos := hmg_GetScrollPos(hwnd, SB_VERT) - _HMG_aScrollStep[2]
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_PAGEDOWN

               NewPos := hmg_GetScrollPos(hwnd, SB_VERT) + _HMG_aScrollStep[2]
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ELSEIF hmg_LoWord(wParam) == SB_THUMBPOSITION

               NewPos := hmg_HiWord(wParam)
               hmg_SetScrollPos(hwnd, SB_VERT, NewPos, .T.)

            ENDIF

            IF _HMG_aFormVirtualWidth[i] > 0
               NewHPos := hmg_GetScrollPos(hwnd, SB_HORZ)
            ELSE
               NewHPos := 0
            ENDIF

            // Control Repositioning

            IF hmg_LoWord(wParam) == SB_THUMBPOSITION .OR. hmg_LoWord(wParam) == SB_LINEDOWN .OR. hmg_LoWord(wParam) == SB_LINEUP .OR. hmg_LoWord(wParam) == SB_PAGEUP .OR. hmg_LoWord(wParam) == SB_PAGEDOWN .OR. hmg_LoWord(wParam) == SB_BOTTOM .OR. hmg_LoWord(wParam) == SB_TOP .AND. !_HMG_AutoAdjust

               FOR x := 1 TO Len(_HMG_aControlHandles)

                  IF _HMG_aControlParentHandles[x] == hwnd

                     IF _HMG_aControlType[x] == CONTROL_TYPE_SPINNER

                        hmg_MoveWindow(_HMG_aControlhandles[x][1], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewPos, _HMG_aControlWidth[x] - GetWindowWidth(_HMG_aControlhandles[x][2]) + 1, _HMG_aControlHeight[x], .T.)
                        hmg_MoveWindow(_HMG_aControlhandles[x][2], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GetWindowWidth(_HMG_aControlhandles[x][2]) - NewHPos, _HMG_aControlRow[x] - NewPos, GetWindowWidth(_HMG_aControlhandles[x][2]), _HMG_aControlHeight[x], .T.)
#ifdef _DBFBROWSE_
                     ELSEIF _HMG_aControlType[x] == CONTROL_TYPE_BROWSE

                        hmg_MoveWindow(_HMG_aControlhandles[x], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewPos, _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH(), _HMG_aControlHeight[x], .T.)
                        hmg_MoveWindow(_HMG_aControlIds[x], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - NewHPos, _HMG_aControlRow[x] - NewPos, GETVSCROLLBARWIDTH(), GetWIndowHeight(_HMG_aControlIds[x]), .T.)

                        hmg_MoveWindow(_HMG_aControlMiscData1[x][1], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - NewHPos, _HMG_aControlRow[x] + _HMG_aControlHeight[x] - GetHScrollBarHeight() - NewPos, ;
                           GetWindowWidth(_HMG_aControlMiscData1[x][1]), GetWindowHeight(_HMG_aControlMiscData1[x][1]), .T.)

                        hmg_ReDrawWindow(_HMG_aControlhandles[x])
#endif
                     ELSEIF _HMG_aControlType[x] == CONTROL_TYPE_RADIOGROUP

                        FOR z := 1 TO Len(_HMG_aControlhandles[x])

                           IF !_HMG_aControlMiscData1[x]
                              hmg_MoveWindow(_HMG_aControlhandles[x][z], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewPos + ((z - 1) * _HMG_aControlSpacing[x]), _HMG_aControlWidth[x], _HMG_aControlHeight[x] / Len(_HMG_aControlhandles[x]), .T.)
                           ELSE  // horizontal
                              hmg_MoveWindow(_HMG_aControlhandles[x][z], _HMG_aControlCol[x] - NewHPos + (z - 1) * (_HMG_aControlWidth[x] + _HMG_aControlSpacing[x]), _HMG_aControlRow[x] - NewPos, _HMG_aControlWidth[x], _HMG_aControlHeight[x], .T.)
                           ENDIF

                        NEXT z

                     ELSEIF _HMG_aControlType[x] == CONTROL_TYPE_TOOLBAR

                        MsgMiniGuiError("ToolBar's Parent Window cannot be a 'Virtual Dimensioned' window (use 'Virtual Dimensioned' SplitChild instead).")

                     ELSE

                        hmg_MoveWindow(_HMG_aControlhandles[x], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewPos, _HMG_aControlWidth[x], _HMG_aControlHeight[x], .T.)

                     ENDIF

                  ENDIF

               NEXT x

               hmg_ReDrawWindow(hwnd)

            ENDIF

         ENDIF

         SWITCH hmg_LoWord(wParam)

         CASE SB_LINEDOWN
            _DoWindowEventProcedure(_HMG_aFormScrollDown[i], i)
            EXIT

         CASE SB_LINEUP
            _DoWindowEventProcedure(_HMG_aFormScrollUp[i], i)
            EXIT

         CASE SB_THUMBPOSITION
         CASE SB_PAGEUP
         CASE SB_PAGEDOWN
         CASE SB_TOP
         CASE SB_BOTTOM
            _DoWindowEventProcedure(_HMG_aFormVScrollBox[i], i)

         END SWITCH

      ENDIF

#ifdef _DBFBROWSE_
      i := AScan(_HMG_aControlIds, lParam)

      IF i > 0

         IF _HMG_aControlType[i] == CONTROL_TYPE_BROWSE

            IF hmg_LoWord(wParam) == SB_LINEDOWN
               hmg_setfocus(_HMG_aControlHandles[i])
               InsertDown()
            ENDIF

            IF hmg_LoWord(wParam) == SB_LINEUP
               hmg_setfocus(_HMG_aControlHandles[i])
               InsertUp()
            ENDIF

            IF hmg_LoWord(wParam) == SB_PAGEUP
               hmg_setfocus(_HMG_aControlHandles[i])
               InsertPrior()
            ENDIF

            IF hmg_LoWord(wParam) == SB_PAGEDOWN
               hmg_setfocus(_HMG_aControlHandles[i])
               InsertNext()
            ENDIF

            IF hmg_LoWord(wParam) == SB_THUMBPOSITION

               BackArea := Alias()
               BrowseArea := _HMG_aControlSpacing[i]

               IF Select(BrowseArea) != 0

                  dbSelectArea(BrowseArea)
                  BackRec := RecNo()

                  IF ordKeyCount() > 0
                     RecordCount := ordKeyCount()
                  ELSE
                     RecordCount := RecCount()
                  ENDIF

                  SkipCount := Int(hmg_HIWORD(wParam) * RecordCount / hmg_GetScrollRangeMax(_HMG_aControlIds[i], 2))

                  IF SkipCount > (RecordCount / 2)
                     dbGoBottom()
                     dbSkip(-(RecordCount - SkipCount))
                  ELSE
                     dbGoTop()
                     IF SkipCount > 1
                        dbSkip(SkipCount - RecCount() / 100)
                     ENDIF
                  ENDIF

                  IF EOF()
                     dbSkip(-1)
                  ENDIF

                  nr := RecNo()

                  hmg_SetScrollPos(_HMG_aControlIds[i], 2, hmg_HIWORD(wParam), .T.)

                  dbGoTo(BackRec)

                  IF Select(BackArea) != 0
                     dbSelectArea(BackArea)
                  ELSE
                     dbSelectArea(0)
                  ENDIF

                  _BrowseSetValue("", "", nr, i)

               ENDIF

            ENDIF

         ENDIF

      ENDIF
#endif
      _ProcessSliderEvents(lParam, wParam)
      EXIT
   //**************************************************************************
   CASE WM_TASKBAR
   //**************************************************************************

      IF wParam == ID_TASKBAR .AND. lParam != WM_MOUSEMOVE

         SWITCH lParam

         CASE WM_LBUTTONDOWN

            IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0
               _DoWindowEventProcedure(_HMG_aFormNotifyIconLeftClick[i], i, "TASKBAR_CLICK")
            ENDIF
            EXIT
         CASE TTM_DELTOOLA

            IF IsToolTipBalloonActive .AND. hWnd == _HMG_MainHandle
               _DoWindowEventProcedure(_HMG_NotifyBalloonClick, AScan(_HMG_aFormHandles, hWnd), "TOOLTIP_CLICK")
            ENDIF
            EXIT
         CASE WM_LBUTTONDBLCLK

            IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0
               _DoWindowEventProcedure(_HMG_aFormNotifyIconDblClick[i], i, "TASKBAR_DBLCLICK")
            ENDIF
            EXIT
         CASE WM_RBUTTONDOWN

            IF _HMG_ShowContextMenus

               aPos := hmg_GetCursorPos()

               IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0

                  IF _HMG_aFormNotifyMenuHandle[i] != 0
                     hmg_TrackPopupMenu(_HMG_aFormNotifyMenuHandle[i], aPos[2], aPos[1], hWnd, .T.)
                  ENDIF

               ENDIF

            ENDIF

         END SWITCH

      ENDIF
      EXIT

#ifdef _OBJECT_
   //**************************************************************************
   CASE WM_WND_LAUNCH
   //**************************************************************************
      IF _HMG_lOOPEnabled
         Eval(_HMG_bOnWndLaunch, hWnd, nMsg, wParam, lParam)
      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_CTL_LAUNCH
   //**************************************************************************
      IF _HMG_lOOPEnabled
         Eval(_HMG_bOnCtlLaunch, hWnd, nMsg, wParam, lParam)
      ENDIF
      EXIT
#endif
   //**************************************************************************
   CASE WM_NEXTDLGCTL
   //**************************************************************************

      IF hmg_LoWord(lParam) != 0
         NextControlHandle := hmg_numbertohandle(wParam)
      ELSE
         NextControlHandle := hmg_GetNextDlgTabITem(hmg_GetActiveWindow(), hmg_GetFocus(), (wParam != 0))
      ENDIF

      hmg_setfocus(NextControlHandle)

      i := AScan(_HMG_aControlHandles, NextControlHandle)

      IF i > 0

         IF _HMG_aControlType[i] == CONTROL_TYPE_BUTTON

            hmg_SendMessage(NextControlHandle, BM_SETSTYLE, hmg_LOWORD(BS_DEFPUSHBUTTON), 1)

         ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_EDIT .OR. _HMG_aControlType[i] == CONTROL_TYPE_TEXT

            hmg_SendMessage(_HMG_aControlHandles[i], EM_SETSEL, 0, -1)

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_HSCROLL
   //**************************************************************************

      i := AScan(_HMG_aFormHandles, hWnd)

      IF i > 0

         // Horizontal ScrollBar Processing

         IF _HMG_aFormVirtualWidth[i] > 0 .AND. lParam == 0

            IF _HMG_aFormRebarhandle[i] > 0
               MsgMiniGuiError("SplitBox's Parent Window cannot be a 'Virtual Dimensioned' window (use 'Virtual Dimensioned' SplitChild instead).")
            ENDIF

            z := iif(_HMG_aScrollStep[1] > 0, _HMG_aScrollStep[1], hmg_GetScrollRangeMax(hwnd, SB_HORZ) / _HMG_aScrollStep[2])

            SWITCH hmg_LoWord(wParam)

            CASE SB_LINERIGHT
               NewHPos := hmg_GetScrollPos(hwnd, SB_HORZ) + z
               IF NewHPos >= hmg_GetScrollRangeMax(hwnd, SB_HORZ) - 10
                  NewHPos := hmg_GetScrollRangeMax(hwnd, SB_HORZ)
               ENDIF
               hmg_SetScrollPos(hwnd, SB_HORZ, NewHPos, .T.)
               EXIT

            CASE SB_LINELEFT
               NewHPos := hmg_GetScrollPos(hwnd, SB_HORZ) - z
               IF NewHPos < 10
                  NewHPos := 0
               ENDIF
               hmg_SetScrollPos(hwnd, SB_HORZ, NewHPos, .T.)
               EXIT

            CASE SB_PAGELEFT
               NewHPos := hmg_GetScrollPos(hwnd, SB_HORZ) - _HMG_aScrollStep[2]
               hmg_SetScrollPos(hwnd, SB_HORZ, NewHPos, .T.)
               EXIT

            CASE SB_PAGERIGHT
               NewHPos := hmg_GetScrollPos(hwnd, SB_HORZ) + _HMG_aScrollStep[2]
               hmg_SetScrollPos(hwnd, SB_HORZ, NewHPos, .T.)
               EXIT

            CASE SB_THUMBPOSITION
               NewHPos := hmg_HIWORD(wParam)
               hmg_SetScrollPos(hwnd, SB_HORZ, NewHPos, .T.)

            ENDSWITCH

            IF _HMG_aFormVirtualHeight[i] > 0
               NewVPos := hmg_GetScrollPos(hwnd, SB_VERT)
            ELSE
               NewVPos := 0
            ENDIF

            // Control Repositioning

            IF hmg_LoWord(wParam) == SB_THUMBPOSITION .OR. hmg_LoWord(wParam) == SB_LINELEFT .OR. hmg_LoWord(wParam) == SB_LINERIGHT .OR. hmg_LoWord(wParam) == SB_PAGELEFT .OR. hmg_LoWord(wParam) == SB_PAGERIGHT .AND. !_HMG_AutoAdjust

               FOR x := 1 TO Len(_HMG_aControlhandles)

                  IF _HMG_aControlParentHandles[x] == hwnd

                     SWITCH _HMG_aControlType[x]

                     CASE CONTROL_TYPE_SPINNER
                        hmg_MoveWindow(_HMG_aControlhandles[x][1], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewVPos, _HMG_aControlWidth[x] - GetWindowWidth(_HMG_aControlhandles[x][2]) + 1, _HMG_aControlHeight[x], .T.)
                        hmg_MoveWindow(_HMG_aControlhandles[x][2], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GetWindowWidth(_HMG_aControlhandles[x][2]) - NewHPos, _HMG_aControlRow[x] - NewVPos, ;
                           GetWindowWidth(_HMG_aControlhandles[x][2]), _HMG_aControlHeight[x], .T.)
                        EXIT

#ifdef _DBFBROWSE_
                     CASE CONTROL_TYPE_BROWSE
                        hmg_MoveWindow(_HMG_aControlhandles[x], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewVPos, _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH(), _HMG_aControlHeight[x], .T.)
                        hmg_MoveWindow(_HMG_aControlIds[x], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - NewHPos, _HMG_aControlRow[x] - NewVPos, ;
                           GetWindowWidth(_HMG_aControlIds[x]), GetWindowHeight(_HMG_aControlIds[x]), .T.)
                        hmg_MoveWindow(_HMG_aControlMiscData1[x][1], _HMG_aControlCol[x] + _HMG_aControlWidth[x] - GETVSCROLLBARWIDTH() - NewHPos, _HMG_aControlRow[x] + _HMG_aControlHeight[x] - GethScrollBarHeight() - NewVPos, ;
                           GetWindowWidth(_HMG_aControlMiscData1[x][1]), GetWindowHeight(_HMG_aControlMiscData1[x][1]), .T.)
                        hmg_ReDrawWindow(_HMG_aControlhandles[x])
                        EXIT
#endif

                     CASE CONTROL_TYPE_RADIOGROUP
                        FOR z := 1 TO Len(_HMG_aControlhandles[x])
                           IF !_HMG_aControlMiscData1[x]
                              hmg_MoveWindow(_HMG_aControlhandles[x][z], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewVPos + ((z - 1) * _HMG_aControlSpacing[x]), ;
                                 _HMG_aControlWidth[x], _HMG_aControlHeight[x] / Len(_HMG_aControlhandles[x]), .T.)
                           ELSE  // horizontal
                              hmg_MoveWindow(_HMG_aControlhandles[x][z], _HMG_aControlCol[x] - NewHPos + (z - 1) * (_HMG_aControlWidth[x] + _HMG_aControlSpacing[x]), _HMG_aControlRow[x] - NewVPos, ;
                                 _HMG_aControlWidth[x], _HMG_aControlHeight[x], .T.)
                           ENDIF
                        NEXT z
                        EXIT

                     CASE CONTROL_TYPE_TOOLBAR
                        MsgMiniGuiError("ToolBar's Parent Window cannot be a 'Virtual Dimensioned' window (use 'Virtual Dimensioned' SplitChild instead).")
                        EXIT

                     OTHERWISE
                        hmg_MoveWindow(_HMG_aControlhandles[x], _HMG_aControlCol[x] - NewHPos, _HMG_aControlRow[x] - NewVPos, _HMG_aControlWidth[x], _HMG_aControlHeight[x], .T.)

                     ENDSWITCH

                  ENDIF
               NEXT x

               hmg_RedrawWindow(hwnd)

            ENDIF

         ENDIF

         SWITCH hmg_LoWord(wParam)

         CASE SB_LINERIGHT
            _DoWindowEventProcedure(_HMG_aFormScrollRight[i], i)
            EXIT

         CASE SB_LINELEFT
            _DoWindowEventProcedure(_HMG_aFormScrollLeft[i], i)
            EXIT

         CASE SB_THUMBPOSITION
         CASE SB_PAGELEFT
         CASE SB_PAGERIGHT
            _DoWindowEventProcedure(_HMG_aFormHScrollBox[i], i)

         END SWITCH

      ENDIF

      _ProcessSliderEvents(lParam, wParam)
      EXIT
   //**************************************************************************
   CASE WM_PAINT
   //**************************************************************************

      FOR EACH r IN _HMG_aFormHandles

         z := hb_enumindex(r)

         IF !_HMG_aFormDeleted[z] .AND. _HMG_aFormType[z] == "X"

            a := _HMG_aFormGraphTasks[z]
            IF hb_IsArray(a) .AND. Len(a) > 0
               AEval(a, {|x|Eval(x)})
            ENDIF

         ENDIF

      NEXT

      i := AScan(_HMG_aFormHandles, hWnd)

      IF i > 0

         _DoWindowEventProcedure(_HMG_aFormPaintProcedure[i], i)

         FOR x := 1 TO Len(_HMG_aFormGraphTasks[i])

            Eval(_HMG_aFormGraphTasks[i][x])

         NEXT x

         k := 0
         FOR EACH r IN _HMG_aControlHandles

            z := hb_enumindex(r)

            IF _HMG_aControlParentHandles[z] == hWnd

               IF _HMG_aControlType[z] == CONTROL_TYPE_TOOLBAR .AND. hb_bitand(GetWindowLong(r, GWL_STYLE), CCS_BOTTOM) == CCS_BOTTOM
                  k := r
                  EXIT
               ENDIF

            ENDIF

         NEXT

         IF k > 0 .AND. _IsControlDefined("StatusBar", _HMG_aFormNames[i]) .AND. _HMG_SplitLastControl != "TOOLBAR"
            aPos := {0, 0, 0, 0}
            hmg_GetClientRect(_HMG_aFormHandles[i], /*@*/ aPos)
            hmg_SetWindowPos(k, 0, 0, aPos[4] - hmg_LoWord(hmg_GetSizeToolBar(k)) - GetBorderHeight() - GetProperty(_HMG_aFormNames[i], "StatusBar", "Height"), 0, 0, SWP_NOSIZE + SWP_NOZORDER)
         ENDIF

         RETURN 0

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MBUTTONDOWN
   CASE WM_RBUTTONDOWN
   //**************************************************************************

      _HMG_MouseRow := hmg_HIWORD(lParam)
      _HMG_MouseCol := hmg_LOWORD(lParam)
      _HMG_MouseState := iif(nMsg == WM_RBUTTONDOWN, 2, 3)

      IF !_HMG_ShowContextMenus

         i := AScan(_HMG_aFormHandles, hWnd)

         IF i > 0

            _MouseCoordCorr(hWnd, i)

            _DoWindowEventProcedure(_HMG_aFormClickProcedure[i], i)

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_LBUTTONDOWN
   //**************************************************************************

      _HMG_MouseRow := hmg_HIWORD(lParam)
      _HMG_MouseCol := hmg_LOWORD(lParam)
      _HMG_MouseState := 1

      i := AScan(_HMG_aFormhandles, hWnd)

      IF i > 0

         _MouseCoordCorr(hWnd, i)

         _DoWindowEventProcedure(_HMG_aFormClickProcedure[i], i)

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MBUTTONUP
   CASE WM_RBUTTONUP
   CASE WM_LBUTTONUP
   //**************************************************************************

      _HMG_MouseState := 0
      EXIT
   //**************************************************************************
   CASE WM_MOUSEMOVE
   //**************************************************************************

      _HMG_MouseRow := hmg_HIWORD(lParam)
      _HMG_MouseCol := hmg_LOWORD(lParam)

      i := AScan(_HMG_aFormhandles, hWnd)

      IF i > 0

         _MouseCoordCorr(hWnd, i)

         IF wParam == MK_LBUTTON
            _DoWindowEventProcedure(_HMG_aFormMouseDragProcedure[i], i)
         ELSE
            _DoWindowEventProcedure(_HMG_aFormMouseMoveProcedure[i], i)
         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_DROPFILES
   //**************************************************************************

      i := AScan(_HMG_aFormhandles, hWnd)

      IF i > 0

         hmg_SetForegroundWindow(hWnd)

         Eval(_HMG_aFormDropProcedure[i], hmg_DragQueryFiles(wParam))

         hmg_DragFinish(wParam)

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_CONTEXTMENU
   //**************************************************************************

      _HMG_MouseRow := hmg_HIWORD(lParam)
      _HMG_MouseCol := hmg_LOWORD(lParam)

      IF (i := AScan(_HMG_aControlHandles, hmg_numbertohandle(wParam))) > 0 .AND. (_HMG_aControlType[i] == CONTROL_TYPE_IMAGE .OR. _HMG_aControlType[i] == CONTROL_TYPE_LABEL) .AND. hb_IsBlock(_HMG_aControlChangeProcedure[i])

         _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)

      ELSEIF (i := AScan(_HMG_aControlsContextMenu, {|x|x[1] == wParam})) > 0 .AND. _HMG_aControlsContextMenu[i, 4]

         hmg_setfocus(wParam)
         _HMG_xControlsContextMenuID := _HMG_aControlsContextMenu[i, 3]

         // RichEditBox Processing .........................
         IF _HMG_aControlType[_HMG_xControlsContextMenuID] = CONTROL_TYPE_RICHEDIT .AND. _HMG_aControlMiscData1[_HMG_xControlsContextMenuID] == 1

            cParent := GetParentFormName(_HMG_xControlsContextMenuID)
            hEdit := _HMG_aControlHandles[_HMG_xControlsContextMenuID]

            // Current selection range:
            nStart := hmg_LoWord(hmg_SendMessage(hEdit, EM_GETSEL, 0, 0))
            nEnd := hmg_HiWord(hmg_SendMessage(hEdit, EM_GETSEL, 0, 0))

            // Undo:
            SetProperty(cParent, "mnuEditUndo", "Enabled", hmg_RichEditBox_CanUndo(hEdit))

            // Cut, Copy & Delete: enable if a selection, disable if no selection
            SetProperty(cParent, "mnuEditCut", "Enabled", (nStart < nEnd))
            SetProperty(cParent, "mnuEditCopy", "Enabled", (nStart < nEnd))
            SetProperty(cParent, "mnuEditDelete", "Enabled", (nStart < nEnd))

            // Paste:
            SetProperty(cParent, "mnuEditPaste", "Enabled", hmg_RichEditBox_CanPaste(hEdit))

            // Select All: disable if everything's already selected, enable otherwise.
            TxtLen := hmg_SendMessage(hEdit, WM_GETTEXTLENGTH, 0, 0)
            SetProperty(cParent, "mnuEditSelAll", "Enabled", !(nStart == 0 .AND. nEnd == TxtLen))

         ENDIF

         hmg_TrackPopupMenu(_HMG_aControlsContextMenu[i, 2], _HMG_MouseCol, _HMG_MouseRow, hWnd)

      ELSEIF _HMG_ShowContextMenus

         hmg_setfocus(wParam)

         IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0

            IF _HMG_aFormContextMenuHandle[i] != 0
               aPos := hmg_GetCursorPos()
               _HMG_MouseRow := aPos[1]
               _HMG_MouseCol := aPos[2]
               _MouseCoordCorr(hWnd, i)
               hmg_TrackPopupMenu(_HMG_aFormContextMenuHandle[i], _HMG_MouseCol, _HMG_MouseRow, hWnd)
            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_TIMER
   //**************************************************************************

      i := AScan(_HMG_aControlIds, wParam)

      IF i > 0

         IF _HMG_aControlPicture[i]  // Once
            _DisableControl(_HMG_aControlNames[i], GetParentFormName(i))
         ENDIF

         IF _HMG_aControlVisible[i]  // with using THIS environment

            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)

         ELSEIF hb_IsBlock(_HMG_aControlProcedures[i])

            IF !_HMG_BeginWindowActive .OR. _HMG_MainClientMDIHandle != 0
               Eval(_HMG_aControlProcedures[i])
            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_MOVE
   //**************************************************************************

      _HMG_MouseRow := hmg_HIWORD(lParam)
      _HMG_MouseCol := hmg_LOWORD(lParam)

      i := AScan(_HMG_aFormhandles, hWnd)

      IF i > 0

         IF _HMG_MainActive
            _DoWindowEventProcedure(_HMG_aFormMoveProcedure[i], i)
         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_ENTERSIZEMOVE
   //**************************************************************************

      IF !_HMG_AutoAdjust
         lEnterSizeMove := NIL
         _HMG_MouseState := 1
      ENDIF

      IF _HMG_MainClientMDIHandle != 0
         EXIT
      ENDIF
   //**************************************************************************
   CASE WM_SIZE
   //**************************************************************************

      IF HB_ISNIL(lEnterSizeMove) .OR. !lEnterSizeMove .OR. !iswinnt()

         hb_default(@lEnterSizeMove, .T.)

         ControlCount := Len(_HMG_aControlHandles)

         i := AScan(_HMG_aFormHandles, hWnd)

         IF i > 0

            r := 0
            IF (k := _HMG_aFormReBarHandle[i]) > 0

               SizeRebar(k)
               r := RebarHeight(k)
               hmg_RedrawWindow(k)

            ENDIF

            FOR x := 1 TO ControlCount

               IF _HMG_aControlParentHandles[x] == hWnd

                  IF _HMG_aControlType[x] == CONTROL_TYPE_MESSAGEBAR

                     hmg_MoveWindow(_HMG_aControlHandles[x], 0, 0, 0, 0, .T.)
                     hmg_RefreshItemBar(_HMG_aControlHandles[x], _GetStatusItemWidth(hWnd, 1))

                     IF (k := GetControlIndex("ProgressMessage", GetParentFormName(x))) != 0
                        hmg_RefreshProgressItem(_HMG_aControlMiscData1[k, 1], _HMG_aControlHandles[k], _HMG_aControlMiscData1[k, 2])
                     ENDIF
                     EXIT

                  ENDIF

               ENDIF

            NEXT x

            IF _HMG_MainClientMDIHandle != 0

               IF wParam != SIZE_MINIMIZED

                  SizeClientWindow(hWnd, _HMG_ActiveStatusHandle, _HMG_MainClientMDIHandle, r)

               ENDIF

            ENDIF

            IF _HMG_MainActive .OR. !_HMG_MainWindowFirst

               IF wParam == SIZE_MAXIMIZED

                  _DoWindowEventProcedure(_HMG_aFormMaximizeProcedure[i], i)

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust(hWnd)
                  ENDIF

               ELSEIF wParam == SIZE_MINIMIZED

                  _DoWindowEventProcedure(_HMG_aFormMinimizeProcedure[i], i)

               ELSEIF wParam == SIZE_RESTORED .AND. !IsWindowSized(hWnd)

                  _DoWindowEventProcedure(_HMG_aFormRestoreProcedure[i], i)

               ELSE

                  IF _HMG_aFormType[i] == "M" .AND. hmg_IsMenu(hmg_GetMenu(hWnd)) .AND. !_IsWindowActive(_HMG_aFormNames[i])
                  ELSE
                     _DoWindowEventProcedure(_HMG_aFormSizeProcedure[i], i)
                  ENDIF

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust(hWnd)
                  ENDIF

               ENDIF

            ENDIF

         ENDIF

         FOR i := 1 TO ControlCount

            IF _HMG_aControlParentHandles[i] == hWnd

               IF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBAR
                  hmg_SendMessage(_HMG_aControlHandles[i], TB_AUTOSIZE, 0, 0)
               ENDIF

            ENDIF

         NEXT i

         IF _HMG_MainClientMDIHandle != 0
            RETURN 1
         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_EXITSIZEMOVE
   //**************************************************************************

      lEnterSizeMove := .F.
      IF !_HMG_AutoAdjust
         _HMG_MouseState := 0
         hmg_SendMessage(hWnd, WM_SIZE, 0, 0)
      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_COMMAND
   //**************************************************************************

      //................................................
      // Search Control From Received Id LoWord (wParam)
      //................................................

      i := AScan(_HMG_aControlIds, hmg_LoWord(wParam))

      IF i > 0

         // Process Menus .......................................

         IF hmg_HiWord(wParam) == 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_MENU
            IF _HMG_aControlMiscData1[i] == 1
               hmg__CloseMenu()
            ENDIF
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            RETURN 0
         ENDIF

         // Process ToolBar Buttons ............................

         IF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBUTTON
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            RETURN 0
         ENDIF

      ENDIF

      //..............................................
      // Search Control From Received Handle (lParam)
      //..............................................

#ifdef _TSBROWSE_
      oGet := GetObjectByHandle(lParam)
      IF hb_IsObject(oGet)

         r := oGet:HandleEvent(nMsg, wParam, lParam)

         IF hb_IsNumeric(r)
            IF r != 0
               RETURN r
            ENDIF
         ENDIF

      ENDIF
#endif

      i := AScan(_HMG_aControlHandles, hmg_numbertohandle(lParam))

      // If Handle Not Found, Look For Spinner

      IF i == 0
         FOR EACH r IN _HMG_aControlHandles
            IF hb_IsArray(r)
               x := hb_enumindex(r)
               IF r[1] == hmg_numbertohandle(lParam) .AND. _HMG_aControlType[x] == CONTROL_TYPE_SPINNER
                  i := x
                  EXIT
               ENDIF
            ENDIF
         NEXT
      ENDIF

      //................................
      // Process Command (Handle based)
      //................................

      IF i > 0

         // Button Click ...................................

         IF hmg_HiWord(wParam) == BN_CLICKED .AND. (_HMG_aControlType[i] == CONTROL_TYPE_BUTTON .OR. _HMG_aControlType[i] == CONTROL_TYPE_OBUTTON)

            IF _HMG_aControlType[i] == CONTROL_TYPE_BUTTON

               _SetFocus(, , i)

            ELSEIF _HMG_aControlRangeMax[i][1] > 0  // GF 08/27/2010

               a := hmg_GetCursorPos()
               IF (x := GetWindowCol(lParam)) + GetWindowWidth(lParam) < a[2] .OR. ;
                  (r := GetWindowRow(lParam)) + GetWindowHeight(lParam) < a[1] .OR. x > a[2] .OR. r > a[1]
                  RETURN 0
               ENDIF

            ENDIF

            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            RETURN 0

         ENDIF

         // CheckBox Click .................................

         IF hmg_HiWord(wParam) == BN_CLICKED .AND. _HMG_aControlType[i] == CONTROL_TYPE_CHECKBOX
            _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
            RETURN 0
         ENDIF

         // Label / HyperLink / Image Click ................

         IF hmg_HiWord(wParam) == STN_CLICKED .AND. ( ;
            (_HMG_aControlType[i] == CONTROL_TYPE_LABEL .OR. _HMG_aControlType[i] == CONTROL_TYPE_CHECKLABEL) .OR. ;
            (_HMG_aControlType[i] == CONTROL_TYPE_HYPERLINK .OR. _HMG_aControlType[i] == CONTROL_TYPE_IMAGE))
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            RETURN 0
         ENDIF

         // Label and Image Double Click ...................

         IF hmg_HiWord(wParam) == STN_DBLCLK .AND. (_HMG_aControlType[i] == CONTROL_TYPE_LABEL .OR. _HMG_aControlType[i] == CONTROL_TYPE_IMAGE)
            _DoControlEventProcedure(_HMG_aControlHeadClick[i], i)
            RETURN 0
         ENDIF

         // Process Richedit Area Change ...................

         IF hmg_HiWord(wParam) == EN_VSCROLL .AND. (_HMG_aControlType[i] == CONTROL_TYPE_RICHEDIT)
            IF _HMG_aControlMiscData1[i] == 0
               _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            ELSE
               _DoControlEventProcedure(_HMG_aControlRangeMax[i], i) // RE
            ENDIF
            RETURN 0
         ENDIF

         // TextBox and GetBox Change ......................

         IF hmg_HiWord(wParam) == EN_CHANGE

            IF _HMG_DateTextBoxActive
               _HMG_DateTextBoxActive := .F.
            ELSE

               IF Len(_HMG_aControlInputMask[i]) > 0

                  IF _HMG_aControlType[i] == CONTROL_TYPE_GETBOX

                     IF hmg_GetFocus(i) == _HMG_aControlHandles[i]
                        oGet := _HMG_aControlHeadClick[i]
                        IF oGet:Changed
                           _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
                        ENDIF
                     ENDIF

                  ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT

                     IF _HMG_aControlSpacing[i]
                        ProcessCharmask(i, .T.)
                     ENDIF

                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")

                  ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_CHARMASKTEXT

                     _HMG_DateTextBoxActive := .T.
                     ProcessCharMask(i)

                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")

                  ENDIF

               ELSE

                  IF _HMG_aControlType[i] == CONTROL_TYPE_BTNNUMTEXT .OR. _HMG_aControlType[i] == CONTROL_TYPE_NUMTEXT
                     ProcessNumText(i)
                  ENDIF

                  IF _HMG_aControlType[i] != CONTROL_TYPE_GETBOX
                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
                  ENDIF

               ENDIF

            ENDIF

            RETURN 0

         ENDIF

         // TextBox LostFocus ..............................

         IF hmg_HiWord(wParam) == EN_KILLFOCUS

            IF _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT

               _HMG_DateTextBoxActive := .T.
               _HMG_aControlSpacing[i] := .F.

               IF "E" $ _HMG_aControlPageMap[i]

                  Ts := hmg_GetWindowText(_HMG_aControlHandles[i])

                  IF "." $ _HMG_aControlPageMap[i]

                     DO CASE

                     CASE hb_UAt(".", Ts) > hb_UAt(",", Ts)
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlHandles[i]), i), _HMG_aControlPageMap[i]))

                     CASE hb_UAt(",", Ts) > hb_UAt(".", Ts)
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromTextSp(hmg_GetWindowText(_HMG_aControlHandles[i]), i), _HMG_aControlPageMap[i]))

                     ENDCASE

                  ELSE

                     DO CASE

                     CASE hb_UAt(".", Ts) != 0
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromTextSp(hmg_GetWindowText(_HMG_aControlHandles[i]), i), _HMG_aControlPageMap[i]))

                     CASE hb_UAt(",", Ts) != 0
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlHandles[i]), i), _HMG_aControlPageMap[i]))

                     OTHERWISE
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlHandles[i]), i), _HMG_aControlPageMap[i]))

                     ENDCASE

                  ENDIF

               ELSE

                  hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlPageMap[i]))

               ENDIF

            ENDIF

            IF _HMG_aControlType[i] == CONTROL_TYPE_CHARMASKTEXT

               IF hb_IsLogical(_HMG_aControlHeadCLick[i])

                  IF _HMG_aControlHeadCLick[i]
                     _HMG_DateTextBoxActive := .T.
                     hmg_SetWindowText(_HMG_aControlHandles[i], DToC(CToD(hmg_GetWindowText(_HMG_aControlHandles[i]))))
                  ENDIF

               ENDIF

            ENDIF

            IF !_HMG_InteractiveCloseStarted

               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)

               // Spinner Checking .........................

               IF _HMG_aControlType[i] == CONTROL_TYPE_SPINNER

                  Ts := hmg_GetWindowText(_HMG_aControlHandles[i][1])

                  IF !_HMG_aControlMiscData1[i][2] .AND. !Empty(Ts)

                     z := Val(Ts)
                     IF z < _HMG_aControlRangeMin[i]
                        SetSpinnerValue(_HMG_aControlHandles[i][2], _HMG_aControlRangeMin[i])

                     ELSEIF z > _HMG_aControlRangeMax[i]
                        SetSpinnerValue(_HMG_aControlHandles[i][2], _HMG_aControlRangeMax[i])

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

            RETURN 0

         ENDIF

         // TextBox GotFocus ...............................

         IF hmg_HiWord(wParam) == EN_SETFOCUS

            VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])

            IF _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT

               _HMG_DateTextBoxActive := .T.

               IF "E" $ _HMG_aControlPageMap[i]

                  Ts := hmg_GetWindowText(_HMG_aControlHandles[i])

                  IF "." $ _HMG_aControlPageMap[i]

                     DO CASE

                     CASE hb_UAt(".", Ts) >  hb_UAt(",", Ts)
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i]))

                     CASE hb_UAt(",", Ts) > hb_UAt(".", Ts)
                        TmpStr := Transform(GetNumFromTextSP(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i])
                        IF Val(TmpStr) == 0
                           TmpStr := StrTran(TmpStr, "0.", " .")
                        ENDIF
                        hmg_SetWindowText(_HMG_aControlhandles[i], TmpStr)

                     ENDCASE

                  ELSE

                     DO CASE

                     CASE hb_UAt(".", Ts) !=  0
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromTextSP(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i]))

                     CASE hb_UAt(",", Ts) != 0
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i]))

                     OTHERWISE
                        hmg_SetWindowText(_HMG_aControlhandles[i], Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i]))

                     ENDCASE

                  ENDIF

               ELSE

                  TmpStr := Transform(GetNumFromText(hmg_GetWindowText(_HMG_aControlhandles[i]), i), _HMG_aControlInputMask[i])

                  IF Val(TmpStr) == 0
                     TmpStr := StrTran(TmpStr, "0.", " .")
                  ENDIF

                  hmg_SetWindowText(_HMG_aControlhandles[i], TmpStr)

               ENDIF

               hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, 0, -1)

               _HMG_aControlSpacing[i] := .T.

            ENDIF

            IF _HMG_aControlType[i] == CONTROL_TYPE_CHARMASKTEXT

               MaskStart := 1

               FOR x := 1 TO hb_ULen(_HMG_aControlInputMask[i])
                  z := hb_USubStr(_HMG_aControlInputMask[i], x, 1)
                  IF hmg_IsDigit(z) .OR. hmg_IsAlpha(z) .OR. z == "!"
                     MaskStart := x
                     EXIT
                  ENDIF
               NEXT x

               hmg_SendMessage(_HMG_aControlhandles[i], EM_SETSEL, MaskStart - 1, -1)

            ENDIF

            _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)

            RETURN 0

         ENDIF

         // ListBox Processing .............................

         IF _HMG_aControlType[i] == CONTROL_TYPE_CHKLIST .OR. ;
            _HMG_aControlType[i] == CONTROL_TYPE_IMAGELIST .OR. ;
            _HMG_aControlType[i] == CONTROL_TYPE_LIST .OR. ;
            _HMG_aControlType[i] == CONTROL_TYPE_MULTICHKLIST .OR. ;
            _HMG_aControlType[i] == CONTROL_TYPE_MULTILIST

            // ListBox OnChange ............................

            IF hmg_HiWord(wParam) == LBN_SELCHANGE
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ENDIF

            // ListBox LostFocus ...........................

            IF hmg_HiWord(wParam) == LBN_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // ListBox GotFocus ............................

            IF hmg_HiWord(wParam) == LBN_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // ListBox Double Click ........................

            IF hmg_HiWord(wParam) == LBN_DBLCLK
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
               RETURN 0
            ENDIF

         ENDIF

         // ComboBox Processing ............................

         IF _HMG_aControlType[i] == CONTROL_TYPE_COMBO

            // ComboBox Change .............................

            IF hmg_HiWord(wParam) == CBN_SELCHANGE
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ENDIF

            // ComboBox OnCancel ...........................

            IF hmg_HiWord(wParam) == CBN_SELENDCANCEL .AND. _HMG_aControlMiscData1[i][1] != 1
               _DoControlEventProcedure(_HMG_aControlMiscData1[i][10], i)
               RETURN 0
            ENDIF

            // ComboBox DropDownList visible ...............

            IF hmg_HiWord(wParam) == CBN_DROPDOWN
               _DoControlEventProcedure(_HMG_aControlInputMask[i], i)
               RETURN 0
            ENDIF

            // ComboBox DropDownList closed ................

            IF hmg_HiWord(wParam) == CBN_CLOSEUP
               _DoControlEventProcedure(_HMG_aControlPicture[i], i)
               RETURN 0
            ENDIF

            // ComboBox LostFocus ..........................

            IF hmg_HiWord(wParam) == CBN_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // ComboBox GotFocus ...........................

            IF hmg_HiWord(wParam) == CBN_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Process Combo Display Area Change ...........

            IF hmg_HiWord(wParam) == CBN_EDITCHANGE
               _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
               IF _HMG_aControlMiscData1[i][1] == 0 .AND. _HMG_aControlMiscData1[i][2] .AND. _HMG_aControlMiscData1[i][7]
                  DoComboAutoComplete(i)
               ENDIF
               RETURN 0
            ENDIF

         ENDIF

         // Button LostFocus ...............................

         IF hmg_HiWord(wParam) == BN_KILLFOCUS .AND. _HMG_aControlType[i] != CONTROL_TYPE_COMBO
            _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
            RETURN 0
         ENDIF

         // Button GotFocus ................................

         IF hmg_HiWord(wParam) == BN_SETFOCUS
            VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
            _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
            RETURN 0
         ENDIF

      ENDIF

      // RadioGrop Processing ..............................

      i := AScan(_HMG_aControlHandles, {|x|iif(hb_isArray(x), AScan(x, hmg_numbertohandle(lParam), , , .T.) != 0, .F.)})

      IF i > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_RADIOGROUP

         // RadioGrop OnChange .............................

         IF hmg_HiWord(wParam) == BN_CLICKED

            IF _HMG_aControlValue[i] != (z := _GetValue(, , i)) .OR. !_HMG_ProgrammaticChange
               _HMG_aControlValue[i] := z
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
            ENDIF

            IF !_HMG_aControlPicture[i] // No TabStop
               IF IsTabStop(lParam)
                  SetTabStop(lParam, .F.)
               ENDIF
            ENDIF
            RETURN 0

         // RadioGrop LostFocus ............................

         ELSEIF hmg_HiWord(wParam) == BN_KILLFOCUS .AND. _HMG_ProceedEachRadioButtonEvent

            _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
            RETURN 0

         // RadioGrop GotFocus .............................

         ELSEIF hmg_HiWord(wParam) == BN_SETFOCUS

            VirtualChildControlFocusProcess(lParam, _HMG_aControlParentHandles[i])

            IF _HMG_ProceedEachRadioButtonEvent

               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)

            ELSEIF nLastRadioGroupFocusedIndex != i // Other radiogroup get focus

               IF nLastRadioGroupFocusedIndex != 0  // call last radiogroup onLostFocus action
                  _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[nLastRadioGroupFocusedIndex], nLastRadioGroupFocusedIndex)
               ENDIF

               nLastRadioGroupFocusedIndex := i     // store current radiogroup
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)

            ENDIF

            IF !_HMG_aControlPicture[i] // No TabStop
               IF IsTabStop(lParam)
                  SetTabStop(lParam, .F.)
               ENDIF
            ENDIF
            RETURN 0

         ENDIF

      ENDIF

      IF !_HMG_ProceedEachRadioButtonEvent .AND. nLastRadioGroupFocusedIndex > 0 .AND. GetEscapeState() >= 0

         // Other control type than radiogroup -> call last radiogroup onLostFocus action
         _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[nLastRadioGroupFocusedIndex], nLastRadioGroupFocusedIndex)
         nLastRadioGroupFocusedIndex := 0

      ENDIF

      //...................
      // Process Enter Key
      //...................

#ifdef _TSBROWSE_
      oGet := GetObjectByHandle(hmg_GetFocus())
      IF hb_IsObject(oGet)
         r := oGet:HandleEvent(nMsg, wParam, lParam)
         IF hb_IsNumeric(r)
            IF r != 0
               RETURN r
            ENDIF
         ENDIF
      ENDIF
#endif

      i := AScan(_HMG_aControlHandles, hmg_GetFocus())

      IF i > 0

         // CheckBox or CheckButton Enter ..................

         IF _HMG_aControlType[i] == CONTROL_TYPE_CHECKBOX .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1
            _HMG_SetFocusExecuted := .F.
            IF Empty(_HMG_aControlMiscData1[i])
               _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            ENDIF
            IF !_HMG_SetFocusExecuted
               IF _HMG_ExtendedNavigation
                  _SetNextFocus()
               ENDIF
            ELSE
               _HMG_SetFocusExecuted := .F.
            ENDIF
            RETURN 0
         ENDIF

         // ButtonEx Enter .................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_OBUTTON .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            RETURN 0
         ENDIF

         // DatePicker or TimePicker Enter .................

         IF (_HMG_aControlType[i] == CONTROL_TYPE_DATEPICK .OR. _HMG_aControlType[i] == CONTROL_TYPE_TIMEPICK) .AND. (hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1)
            _HMG_SetFocusExecuted := .F.
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
            IF !_HMG_SetFocusExecuted
               IF _HMG_ExtendedNavigation
                  _SetNextFocus()
               ENDIF
            ELSE
               _HMG_SetFocusExecuted := .F.
            ENDIF
            RETURN 0
         ENDIF

#ifdef _DBFBROWSE_
         // Browse Escape ..................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_BROWSE .AND. lParam == 0 .AND. wParam == 2
            RETURN 1
         ENDIF

         // Browse Enter ...................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_BROWSE .AND. lParam == 0 .AND. wParam == 1
            IF _hmg_acontrolmiscdata1[i][6]
               IF _HMG_aControlFontColor[i]
                  ProcessInPlaceKbdEdit(i)
               ELSE
                  _BrowseEdit(_hmg_acontrolhandles[i], _HMG_acontrolmiscdata1[i][4], _HMG_acontrolmiscdata1[i][5], _HMG_acontrolmiscdata1[i][3], _HMG_aControlInputMask[i], .F., _HMG_aControlFontColor[i])
               ENDIF
            ELSE
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            ENDIF
            RETURN 1 // JP12
         ENDIF
#endif
         // Grid Enter .....................................

         IF (_HMG_aControlType[i] == CONTROL_TYPE_GRID .OR. _HMG_aControlType[i] == CONTROL_TYPE_MULTIGRID) .AND. lParam == 0 .AND. wParam == 1

            IF _hmg_acontrolspacing[i]

               IF _HMG_aControlMiscData1[i][20] .OR. _HMG_aControlFontColor[i]
                  IF !_HMG_aControlFontColor[i]
                     _GridInplaceKBDEdit(i)
                  ELSE
                     _GridInplaceKBDEdit_2(i)
                  ENDIF
               ELSE
                  _EditItem(_hmg_acontrolhandles[i])
               ENDIF

            ELSE
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            ENDIF
            RETURN 0

         ENDIF

         // ComboBox Enter .................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_COMBO .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1

            _HMG_SetFocusExecuted := .F.
            _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            IF !_HMG_SetFocusExecuted
               IF _HMG_ExtendedNavigation
                  _SetNextFocus()
               ENDIF
            ELSE
               _HMG_SetFocusExecuted := .F.
            ENDIF
            RETURN 0

         ENDIF

         // ListBox Enter ..................................

         IF (_HMG_aControlType[i] == CONTROL_TYPE_LIST .OR. _HMG_aControlType[i] == CONTROL_TYPE_MULTILIST) .AND. (hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1)
            _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            RETURN 0
         ENDIF

         // TextBox Enter ..................................

         IF (_HMG_aControlType[i] == CONTROL_TYPE_BTNNUMTEXT .OR. ;
             _HMG_aControlType[i] == CONTROL_TYPE_BTNTEXT .OR. ;
             _HMG_aControlType[i] == CONTROL_TYPE_CHARMASKTEXT .OR. ;
             _HMG_aControlType[i] == CONTROL_TYPE_MASKEDTEXT .OR. ;
             _HMG_aControlType[i] == CONTROL_TYPE_NUMTEXT .OR. ;
             _HMG_aControlType[i] == CONTROL_TYPE_TEXT) .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1

            IF _HMG_aControlType[i] == CONTROL_TYPE_BTNTEXT .OR. _HMG_aControlType[i] == CONTROL_TYPE_BTNNUMTEXT
               IF _HMG_aControlMiscData1[i][4]
                  _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
                  RETURN 0
               ENDIF
            ENDIF
            _HMG_SetFocusExecuted := .F.
            _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            IF !_HMG_SetFocusExecuted
               IF _HMG_ExtendedNavigation
                  _SetNextFocus()
               ENDIF
            ELSE
               _HMG_SetFocusExecuted := .F.
            ENDIF
            RETURN 0

         ENDIF

         // Tree Enter .....................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_TREE .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1
            _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
            RETURN 0
         ENDIF

      ELSE

         // ComboBox (DisplayEdit) .........................

         ControlCount := Len(_HMG_aControlHandles)

         FOR i := 1 TO ControlCount

            IF _HMG_aControlType[i] == CONTROL_TYPE_COMBO .AND. hmg_HiWord(wParam) == 0 .AND. hmg_LoWord(wParam) == 1

               IF _hmg_acontrolrangemin[i] == hmg_GetFocus() .OR. _hmg_acontrolrangemax[i] == hmg_GetFocus()

                  _HMG_SetFocusExecuted := .F.
                  _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
                  IF !_HMG_SetFocusExecuted
                     IF _HMG_ExtendedNavigation
                        _SetNextFocus()
                     ENDIF
                  ELSE
                     _HMG_SetFocusExecuted := .F.
                  ENDIF
                  EXIT

               ENDIF

            ENDIF

         NEXT i

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_NOTIFY
   //**************************************************************************

      IF !_HMG_ProceedEachRadioButtonEvent .AND. nLastRadioGroupFocusedIndex > 0 .AND. hmg_GetNotifyCode(lParam) == NM_SETFOCUS

         // Other control type than radiogroup -> call last radiogroup onLostFocus action
         _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[nLastRadioGroupFocusedIndex], nLastRadioGroupFocusedIndex)
         nLastRadioGroupFocusedIndex := 0

      ENDIF

      // Process Grid Drag Item ............................

      IF _GetFocusedControlType(hWnd) == CONTROL_TYPE_GRID .OR. ;
         _GetFocusedControlType(hWnd) == CONTROL_TYPE_MULTIGRID .OR. ;
         _GetFocusedControlType(hWnd) == CONTROL_TYPE_PROPGRID // TODO: reescrever para não chamar a função repetidamente

         x := hmg_GetFocus()

         i := AScan(_HMG_aControlHandles, x)

#ifdef UNICODE
         IF i > 0 .AND. !hb_IsBlock(_HMG_aControlProcedures[i])
#else
         IF i > 0
#endif
            IF hmg_GetHwndFrom(lParam) == hmg_ListView_GetHeader(x)

               // https://thomasfreudenberg.com/archive/2004/03/14/hdn-track-and-hds-fulldrag
               IF hmg_GetNotifyCode(lParam) == HDN_ITEMCHANGING

                  k := hmg_GetHeaderListViewItemCX(lParam)

                  IF k >= 0

                     z := hmg_GetHeaderListViewItem(lParam) + 1

                     aCellData := _HMG_aControlMiscData1[i][25]

                     IF hb_IsArray(aCellData) .AND. z <= Len(aCellData) .AND. hb_IsArray(aCellData[z])

                        NewHPos := aCellData[z][1]
                        NewVPos := aCellData[z][2]

                        IF k < NewHPos
                           hmg_ListView_SetColumnWidth(x, z - 1, NewHPos)
                           RETURN 1
                        ELSEIF k > NewVPos
                           hmg_ListView_SetColumnWidth(x, z - 1, NewVPos)
                           RETURN 1
                        ENDIF

                     ENDIF

                  ENDIF

                  RETURN 0

               ELSEIF hmg_GetNotifyCode(lParam) == HDN_ITEMCHANGED

                  _DoControlEventProcedure(_HMG_aControlMiscData1[i][26], i)
                  RETURN 0

               ENDIF

            ENDIF

         ENDIF

      ENDIF

      // Process ToolBar ToolTip ...........................

      IF hmg_GetNotifyCode(lParam) == TTN_NEEDTEXT .AND. IsToolTipActive

         x := AScan(_HMG_aControlIds, hmg_GetNotifyId(lParam)) // for tooltip TOOLBUTTON

         IF x > 0 .AND. _HMG_aControlType[x] == CONTROL_TYPE_TOOLBUTTON

            IF hb_IsString(_HMG_aControlToolTip[x])
               hmg_SetButtonTip(lParam, _HMG_aControlToolTip[x])
            ENDIF

            k := hmg_GetHwndFrom(lParam)  // control handle

#ifdef _HMG_COMPAT_
            IF GetFormNameByHandle(_HMG_aControlParentHandles[x], @z) > 0 .AND. hb_IsArray(a := _WindowCargo(z)) .AND. Len(a) == 2 .AND. hb_IsString(a[2]) .AND. !Empty(a[2])

               hmg_SendMessageString(k, TTM_SETTITLE, a[1], a[2])
            ENDIF
#endif
            IF IsToolTipBalloonActive
               IF !(hb_bitand(GetWindowLong(k, GWL_STYLE), TTS_BALLOON) == TTS_BALLOON)
                  SetWindowStyle(k, TTS_BALLOON, .T.)
               ENDIF
            ENDIF

         ELSE  // JR

            a := hmg_GetMessagePos()
            k := hmg_WindowFromPoint({hmg_LoWord(a), hmg_HiWord(a)})  // control handle
            x := AScan(_HMG_aControlHandles, k)

            IF x > 0 .AND. _HMG_aControlType[x] == CONTROL_TYPE_TAB

               IF hb_IsArray(_HMG_aControlTooltip[x])
                  i := hmg_GetNotifyId(lParam)  // page number
                  hmg_SetButtonTip(lParam, _HMG_aControlTooltip[x, i + 1])
               ELSE
                  hmg_SetButtonTip(lParam, _HMG_aControlTooltip[x])
               ENDIF

            ENDIF

         ENDIF

      ENDIF

      IF hmg_GetNotifyCode(lParam) == RBN_CHEVRONPUSHED  // Notify for chevron button
         _CreatePopUpChevron(hWnd, wParam, lParam)
      ENDIF

#ifdef _TSBROWSE_
      // Process TSBrowse ..................................

      oGet := GetObjectByHandle(hmg_GetHwndFrom(lParam))
      IF hb_IsObject(oGet)

         r := oGet:HandleEvent(nMsg, wParam, lParam)

         IF hb_IsNumeric(r)
            IF r != 0
               RETURN r
            ENDIF
         ENDIF

      ENDIF
#endif

      i := AScan(_HMG_aControlHandles, hmg_GetHwndFrom(lParam))

      IF i > 0

#ifdef _PAGER_
         // Process Pager ..................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_PAGER

            IF hmg_GetNotifyCode(lParam) == PGN_CALCSIZE

               IF _HMG_aControlMiscData1[i]
                  hmg_PagerCalcSize(lParam, _HMG_aControlHeight[i])
               ELSE
                  hmg_PagerCalcSize(lParam, _HMG_aControlWidth[i])
               ENDIF

            ENDIF

            IF hmg_GetNotifyCode(lParam) == PGN_SCROLL
               hmg_PagerScroll(lParam, _HMG_aControlSpacing[i])
            ENDIF

         ENDIF
#endif

#ifdef _DBFBROWSE_
         // Process Browse .................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_BROWSE

            // Browse Refresh On Column Size ...............

            IF hmg_GetNotifyCode(lParam) == NM_CUSTOMDRAW

               hws := 0
               hwm := .F.
               FOR EACH r IN _HMG_aControlProcedures[i]

                  x := hb_enumindex(r)
                  hws += (lvc := hmg_ListView_GetColumnWidth(_HMG_aControlHandles[i], x - 1))
                  IF _HMG_aControlProcedures[i][x] != lvc
                     hwm := .T.
                     _HMG_aControlProcedures[i][x] := lvc
                     _BrowseRefresh("", "", i)
                  ENDIF

               NEXT

               // Browse ReDraw Vertical ScrollBar If Needed

               IF _HMG_aControlIds[i] != 0 .AND. hwm

                  IF hws > _HMG_aControlWidth[i] - GETVSCROLLBARWIDTH() - 4

                     hmg_MoveWindow(_HMG_aControlIds[i], _HMG_aControlCol[i] + _HMG_aControlWidth[i] - GETVSCROLLBARWIDTH(), _HMG_aControlRow[i], GETVSCROLLBARWIDTH(), _HMG_aControlHeight[i] - GETHSCROLLBARHEIGHT(), .T.)
                     hmg_MoveWindow(_HMG_aControlMiscData1[i][1], _HMG_aControlCol[i] + _HMG_aControlWidth[i] - GETVSCROLLBARWIDTH(), _HMG_aControlRow[i] + _HMG_aControlHeight[i] - GETHSCROLLBARHEIGHT(), GETVSCROLLBARWIDTH(), GETHSCROLLBARHEIGHT(), .T.)

                  ELSE

                     hmg_MoveWindow(_HMG_aControlIds[i], _HMG_aControlCol[i] + _HMG_aControlWidth[i] - GETVSCROLLBARWIDTH(), _HMG_aControlRow[i], GETVSCROLLBARWIDTH(), _HMG_aControlHeight[i], .T.)
                     hmg_MoveWindow(_HMG_aControlMiscData1[i][1], _HMG_aControlCol[i] + _HMG_aControlWidth[i] - GETVSCROLLBARWIDTH(), _HMG_aControlRow[i] + _HMG_aControlHeight[i] - GETHSCROLLBARHEIGHT(), 0, 0, .T.)

                  ENDIF

               ENDIF

            ENDIF

            dBc := _HMG_aControlMiscData1[i][10]
            dFc := _HMG_aControlMiscData1[i][9]

            IF hmg_GetNotifyCode(lParam) == NM_CUSTOMDRAW .AND. (hb_IsArray(dBc) .OR. hb_IsArray(dFc))

               IF (r := hmg_GetDs(lParam)) != -1

                  RETURN r

               ELSE

                  a := hmg_GetRc(lParam)

                  IF a[1] >= 1 .AND. a[1] <= Len(_HMG_aControlRangeMax[i]) .AND. ;  // MaxBrowseRows
                     a[2] >= 1 .AND. a[2] <= Len(_HMG_aControlRangeMin[i])          // MaxBrowseCols

                     aTemp  := _HMG_aControlMiscData1[i][18]
                     aTemp2 := _HMG_aControlMiscData1[i][17]

                     IF hb_IsArray(aTemp) .AND. !hb_isArray(aTemp2)
                        IF Len(aTemp) >= a[1]

                           IF aTemp[a[1]][a[2]] != -1
                              RETURN hmg_SetBcFc(lParam, aTemp[a[1]][a[2]], RGB(0, 0, 0))
                           ELSE
                              RETURN hmg_SETBRCCD(lParam)
                           ENDIF

                        ENDIF

                     ELSEIF !hb_isArray(aTemp) .AND. hb_IsArray(aTemp2)
                        IF Len(aTemp2) >= a[1]

                           IF aTemp2[a[1]][a[2]] != -1
                              RETURN hmg_SetBcFc(lParam, RGB(255, 255, 255), aTemp2[a[1]][a[2]])
                           ELSE
                              RETURN hmg_SETBRCCD(lParam)
                           ENDIF

                        ENDIF

                     ELSEIF hb_IsArray(aTemp) .AND. hb_IsArray(aTemp2)
                        IF Len(aTemp) >= a[1] .AND. Len(aTemp2) >= a[1]

                           IF aTemp[a[1]][a[2]] != -1
                              RETURN hmg_SetBcFc(lParam, aTemp[a[1]][a[2]], aTemp2[a[1]][a[2]])
                           ELSE
                              RETURN hmg_SETBRCCD(lParam)
                           ENDIF

                        ENDIF

                     ENDIF

                  ELSE

                     RETURN hmg_SETBRCCD(lParam)

                  ENDIF

               ENDIF

            ENDIF

            // Browse Click ................................

            IF hmg_GetNotifyCode(lParam) == NM_CLICK .OR. hmg_GetNotifyCode(lParam) == LVN_BEGINDRAG .OR. hmg_GetNotifyCode(lParam) == NM_RCLICK

               IF (r := hmg_LISTVIEW_GETFIRSTITEM(_HMG_aControlHandles[i])) > 0

                  DeltaSelect := r - AScan(_HMG_aControlRangeMax[i], _HMG_aControlValue[i])
                  nr := _HMG_aControlValue[i]
                  _HMG_aControlValue[i] := _HMG_aControlRangeMax[i][r]

                  _BrowseVscrollFastUpdate(i, DeltaSelect)
                  IF nr != _HMG_aControlValue[i] .OR. _HMG_BrowseUpdateStatus
                     _BrowseOnChange(i)
                  ENDIF

               ENDIF

               RETURN 0

            ENDIF

            // Browse Key Handling .........................

            IF hmg_GetNotifyCode(lParam) == LVN_KEYDOWN

               DO CASE

               CASE hmg_GetGridvKey(lParam) == 65 .AND. (GetAltState() == -127 .OR. GetAltState() == -128) .AND. hmg_GetKeyState(VK_CONTROL) >= 0 .AND. hmg_GetKeyState(VK_SHIFT) >= 0 // ALT + A

                  IF _HMG_acontrolmiscdata1[i][2]
                     _BrowseEdit(_hmg_acontrolhandles[i], _HMG_acontrolmiscdata1[i][4], _HMG_acontrolmiscdata1[i][5], ;
                        _HMG_acontrolmiscdata1[i][3], _HMG_aControlInputMask[i], .T., _HMG_aControlFontColor[i], _HMG_acontrolmiscdata1[i][13])
                  ENDIF

               CASE hmg_GetGridvKey(lParam) == 46 // DEL

                  IF _HMG_aControlMiscData1[i][12]
                     IF MsgYesNo(_HMG_BRWLangMessage[1], _HMG_BRWLangMessage[2])
                        _BrowseDelete("", "", i)
                     ENDIF
                  ENDIF

               CASE hmg_GetGridvKey(lParam) == 36 // HOME

                  _BrowseHome("", "", i)
                  RETURN 1

               CASE hmg_GetGridvKey(lParam) == 35 // END

                  _BrowseEnd("", "", i)
                  RETURN 1

               CASE hmg_GetGridvKey(lParam) == 33 // PGUP

                  _BrowsePrior("", "", i)
                  RETURN 1

               CASE hmg_GetGridvKey(lParam) == 34 // PGDN

                  _BrowseNext("", "", i)
                  RETURN 1

               CASE hmg_GetGridvKey(lParam) == 38 // UP

                  _BrowseUp("", "", i)
                  RETURN 1

               CASE hmg_GetGridvKey(lParam) == 40 // DOWN

                  _BrowseDown("", "", i)
                  RETURN 1

               ENDCASE

               RETURN 0

            ENDIF

            // Browse Double Click .........................

            IF hmg_GetNotifyCode(lParam) == NM_DBLCLK

               _PushEventInfo()
               _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
               _HMG_ThisType := "C"
               _HMG_ThisIndex := i
               _HMG_ThisFormName :=  _HMG_aFormNames[_HMG_ThisFormIndex]
               _HMG_ThisControlName := _HMG_aControlNames[_HMG_THISIndex ]
               r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))
               IF r[2] == 1
                  hmg_ListView_Scroll(_HMG_aControlHandles[i], -10000, 0)
                  r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))
               ELSE
                  r := hmg_LISTVIEW_GETSUBITEMRECT(_HMG_aControlHandles[i], r[1] - 1, r[2] - 1)
                  //                CellCol            CellWidth
                  xs := (_HMG_aControlCol[i] + r[2] + r[3]) - (_HMG_aControlCol[i] + _HMG_aControlWidth[i])
                  xd := 20
                  IF xs > - xd
                     hmg_ListView_Scroll(_HMG_aControlHandles[i], xs + xd, 0)
                  ELSE
                     IF r[2] < 0
                        hmg_ListView_Scroll(_HMG_aControlHandles[i], r[2], 0)
                     ENDIF
                  ENDIF
                  r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))
               ENDIF

               _HMG_ThisItemRowIndex := r[1]
               _HMG_ThisItemColIndex := r[2]
               IF r[2] == 1
                  r := hmg_LISTVIEW_GETITEMRECT(_HMG_aControlHandles[i], r[1] - 1)
               ELSE
                  r := hmg_LISTVIEW_GETSUBITEMRECT(_HMG_aControlHandles[i], r[1] - 1, r[2] - 1)
               ENDIF
               _HMG_ThisItemCellRow := _HMG_aControlRow[i] + r[1]
               _HMG_ThisItemCellCol := _HMG_aControlCol[i] + r[2]
               _HMG_ThisItemCellWidth  := r[3]
               _HMG_ThisItemCellHeight := r[4]

               IF _hmg_acontrolmiscdata1[i][6]
                  _BrowseEdit(_hmg_acontrolhandles[i], _HMG_acontrolmiscdata1[i][4], _HMG_acontrolmiscdata1[i][5], ;
                     _HMG_acontrolmiscdata1[i][3], _HMG_aControlInputMask[i], .F., _HMG_aControlFontColor[i], _HMG_acontrolmiscdata1[i][13])
               ELSE
                  IF hb_IsBlock(_HMG_aControlDblClick[i])
                     Eval(_HMG_aControlDblClick[i])
                  ENDIF
               ENDIF

               _PopEventInfo()
               _HMG_ThisItemRowIndex := 0
               _HMG_ThisItemColIndex := 0
               _HMG_ThisItemCellRow := 0
               _HMG_ThisItemCellCol := 0
               _HMG_ThisItemCellWidth := 0
               _HMG_ThisItemCellHeight := 0

            ENDIF

            // Browse LostFocus ............................

            IF hmg_GetNotifyCode(lParam) == NM_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Browse GotFocus .............................

            IF hmg_GetNotifyCode(lParam) == NM_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Browse Header Click .........................

            IF hmg_GetNotifyCode(lParam) == LVN_COLUMNCLICK
               IF hb_IsArray(_HMG_aControlHeadClick[i])
                  lvc := hmg_GetGridColumn(lParam) + 1
                  IF Len(_HMG_aControlHeadClick[i]) >= lvc
                     _DoControlEventProcedure(_HMG_aControlHeadClick[i][lvc], i, , lvc)
                  ENDIF
               ENDIF
               RETURN 0
            ENDIF

         ENDIF
#endif
         // ToolBar DropDown Button Click ..................

         IF hmg_GetNotifyCode(lParam) == TBN_DROPDOWN

            hmg_DefWindowProc(hWnd, TBN_DROPDOWN, wParam, lParam)

            x := AScan(_HMG_aControlIds, hmg_GetButtonPos(lParam))

            IF x > 0 .AND. _HMG_aControlType[x] == CONTROL_TYPE_TOOLBUTTON

               aPos := {0, 0, 0, 0}
               hmg_GetWindowRect(_HMG_aControlHandles[i], aPos)

               r := hmg_GetButtonBarRect(_HMG_aControlHandles[i], _HMG_aControlValue[x] - 1)

               k := AScan(_HMG_aControlHandles, _HMG_aControlParentHandles[i])

               IF _HMG_ActiveSplitBoxInverted .OR. ;  // a tool button into an inverted splitbox
                  (k > 0 .AND. _HMG_aControlType[k] == CONTROL_TYPE_PAGER .AND. _HMG_aControlMiscData1[k])  // a tool button into a vertical pagerbox
                  hmg_TrackPopupMenu(_HMG_aControlRangeMax[x], aPos[1] + hmg_LoWord(r), aPos[2] + hmg_HiWord(r), hWnd)
               ELSE
                  hmg_TrackPopupMenu(_HMG_aControlRangeMax[x], aPos[1] + hmg_LoWord(r), aPos[2] + hmg_HiWord(r) + (aPos[4] - aPos[2] - hmg_HiWord(r)) / 2, hWnd)
               ENDIF

            ENDIF

            RETURN 0

         ENDIF

         // RichEdit Selection Change ......................

         IF _HMG_aControlType[i] == CONTROL_TYPE_RICHEDIT

            IF hmg_GetNotifyCode(lParam) == EN_MSGFILTER // for typing text

               IF hb_IsBlock(_HMG_aControlChangeProcedure[i])
                  _HMG_ThisType := "C"
                  _HMG_ThisIndex := i
                  Eval(_HMG_aControlChangeProcedure[i])
                  _HMG_ThisIndex := 0
                  _HMG_ThisType := ""
               ENDIF

            ENDIF

            IF hmg_GetNotifyCode(lParam) == EN_DRAGDROPDONE // for change text by drag

               IF hb_IsBlock(_HMG_aControlChangeProcedure[i])
                  _HMG_ThisType := "C"
                  _HMG_ThisIndex := i
                  Eval(_HMG_aControlChangeProcedure[i])
                  _HMG_ThisIndex := 0
                  _HMG_ThisType := ""
               ENDIF

            ENDIF

            IF hmg_GetNotifyCode(lParam) == EN_SELCHANGE // for change text

               IF hb_IsBlock(_HMG_aControlDblClick[i])
                  _HMG_ThisType := "C"
                  _HMG_ThisIndex := i
                  Eval(_HMG_aControlDblClick[i])
                  _HMG_ThisIndex := 0
                  _HMG_ThisType := ""
               ENDIF

            ENDIF

         ENDIF

         // MonthCalendar Processing .......................

         IF _HMG_aControlType[i] == CONTROL_TYPE_MONTHCAL

            Tmp := (hb_Version(HB_VERSION_BITWIDTH) >= 64)

            // MonthCalendar Selection Change ..............

            IF hmg_GetNotifyCode(lParam) == MCN_SELECT

               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")

               IF Tmp
                  SetDayState(_HMG_aControlNames[i], GetParentFormName(i))
               ENDIF
               RETURN 0

            ELSEIF hmg_GetNotifyCode(lParam) == MCN_SELCHANGE

               _DoControlEventProcedure(_HMG_aControlDblClick[i], i, "CONTROL_ONCHANGE")

               IF Tmp
                  SetDayState(_HMG_aControlNames[i], GetParentFormName(i))
               ENDIF
               RETURN 0

            // MonthCalendar BoldDay Handling ..............

            ELSEIF hmg_GetNotifyCode(lParam) == MCN_GETDAYSTATE

               RetDayState(i, lParam)
               RETURN 0

            ENDIF


         ENDIF

         // Grid Processing ................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_MULTIGRID

            IF _HMG_aControlFontColor[i]

               // Grid Key Handling ........................

               IF hmg_GetNotifyCode(lParam) == LVN_KEYDOWN

                  SWITCH hmg_GetGridvKey(lParam)

                  CASE 37 // LEFT
                     IF _HMG_aControlMiscData1[i][17] > 1
                        _HMG_aControlMiscData1[i][17]--

                        nFrozenColumnCount := _HMG_aControlMiscData1[i][19]
                        IF nFrozenColumnCount > 0
                           nDestinationColumn := _HMG_aControlMiscData1[i][17]
                           IF nDestinationColumn >= nFrozenColumnCount + 1
                              aOriginalColumnWidths := _HMG_aControlMiscData1[i][2]
                              // Set Destination Column Width To Original
                              hmg_LISTVIEW_SETCOLUMNWIDTH(_HMG_aControlHandles[i], nDestinationColumn - 1, aOriginalColumnWidths[nDestinationColumn])
                           ENDIF
                        ENDIF

                        _GRID_KBDSCROLL(i)

                        hmg_LISTVIEW_REDRAWITEMS(_HMG_aControlHandles[i], _HMG_aControlMiscData1[i][1] - 1, _HMG_aControlMiscData1[i][1] - 1)
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 39 // RIGHT
                     IF _HMG_aControlMiscData1[i][17] < Len(_HMG_aControlCaption[i])
                        _HMG_aControlMiscData1[i][17]++

                        nFrozenColumnCount := _HMG_aControlMiscData1[i][19]
                        IF nFrozenColumnCount > 0
                           nDestinationColumn := _HMG_aControlMiscData1[i][17]
                           FOR k := nDestinationColumn TO Len(_HMG_aControlCaption[i]) - 1
                              IF hmg_LISTVIEW_GETCOLUMNWIDTH(_HMG_aControlHandles[i], k - 1) == 0
                                 _HMG_aControlMiscData1[i][17]++
                              ENDIF
                           NEXT k

                           IF nDestinationColumn > nFrozenColumnCount + 1
                              // Set Current Column Width To 0
                              hmg_LISTVIEW_SETCOLUMNWIDTH(_HMG_aControlHandles[i], nDestinationColumn - 2, 0)
                           ENDIF
                        ENDIF

                        _GRID_KBDSCROLL(i)

                        hmg_LISTVIEW_REDRAWITEMS(_HMG_aControlHandles[i], _HMG_aControlMiscData1[i][1] - 1, _HMG_aControlMiscData1[i][1] - 1)
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 38 // UP
                     IF _HMG_aControlMiscData1[i][17] == 0
                        _HMG_aControlMiscData1[i][17] := 1
                     ENDIF
                     IF _HMG_aControlMiscData1[i][1] > 1
                        _HMG_aControlMiscData1[i][1]--
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 40 // DOWN
                     IF _HMG_aControlMiscData1[i][17] == 0
                        _HMG_aControlMiscData1[i][17] := 1
                     ENDIF
                     IF _HMG_aControlMiscData1[i][1] < hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETITEMCOUNT, 0, 0)
                        _HMG_aControlMiscData1[i][1]++
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 33 // PGUP
                     nGridRowValue := _HMG_aControlMiscData1[i][1]

                     IF _HMG_aControlMiscData1[i][1] == hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETTOPINDEX, 0, 0) + 1
                        _HMG_aControlMiscData1[i][1] -= hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETCOUNTPERPAGE, 0, 0) - 1
                     ELSE
                        _HMG_aControlMiscData1[i][1] := hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETTOPINDEX, 0, 0) + 1
                     ENDIF

                     IF _HMG_aControlMiscData1[i][1] < 1
                        _HMG_aControlMiscData1[i][1] := 1
                     ENDIF

                     IF nGridRowValue != _HMG_aControlMiscData1[i][1]
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 34 // PGDOWN
                     nGridRowValue := _HMG_aControlMiscData1[i][1]

                     IF _HMG_aControlMiscData1[i][1] == hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETTOPINDEX, 0, 0) + hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETCOUNTPERPAGE, 0, 0)
                        _HMG_aControlMiscData1[i][1] += hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETCOUNTPERPAGE, 0, 0) - 1
                     ELSE
                        _HMG_aControlMiscData1[i][1] := hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETTOPINDEX, 0, 0) + hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETCOUNTPERPAGE, 0, 0)
                     ENDIF

                     IF _HMG_aControlMiscData1[i][1] > hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETITEMCOUNT, 0, 0)
                        _HMG_aControlMiscData1[i][1] := hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETITEMCOUNT, 0, 0)
                     ENDIF

                     IF nGridRowValue != _HMG_aControlMiscData1[i][1]
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 35 // END
                     nGridRowValue := _HMG_aControlMiscData1[i][1]

                     _HMG_aControlMiscData1[i][1] := hmg_SendMessage(_HMG_aControlHandles[i], LVM_GETITEMCOUNT, 0, 0)

                     IF nGridRowValue != _HMG_aControlMiscData1[i][1]
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  CASE 36 // HOME
                     nGridRowValue := _HMG_aControlMiscData1[i][1]

                     _HMG_aControlMiscData1[i][1] := 1

                     IF nGridRowValue != _HMG_aControlMiscData1[i][1]
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF
                     EXIT
                  DEFAULT
                     RETURN 1

                  END SWITCH

               ENDIF

            ENDIF

            IF hmg_GetNotifyCode(lParam) == NM_CUSTOMDRAW

               IF (r := iif(_HMG_aControlFontColor[i], hmg_GetDs(lParam, _HMG_aControlHandles[i], _HMG_aControlMiscData1[i][1] - 1), hmg_GetDs(lParam))) != -1

                  RETURN r

               ELSE

                  a := hmg_GetRc(lParam)
                  IF _HMG_aControlFontColor[i]

                     IF a[1] == _HMG_aControlMiscData1[i][1] .AND. a[2] == _HMG_aControlMiscData1[i][17]

                        RETURN hmg_SetBcFc(lParam, RGB(_HMG_GridSelectedCellBackColor[1], _HMG_GridSelectedCellBackColor[2], _HMG_GridSelectedCellBackColor[3] ), RGB(_HMG_GridSelectedCellForeColor[1], _HMG_GridSelectedCellForeColor[2], _HMG_GridSelectedCellForeColor[3]))

                     ELSEIF a[1] == _HMG_aControlMiscData1[i][1] .AND. a[2] != _HMG_aControlMiscData1[i][17]

                        RETURN hmg_SetBcFc(lParam, RGB(_HMG_GridSelectedRowBackColor[1], _HMG_GridSelectedRowBackColor[2], _HMG_GridSelectedRowBackColor[3] ), RGB(_HMG_GridSelectedRowForeColor[1], _HMG_GridSelectedRowForeColor[2], _HMG_GridSelectedRowForeColor[3]))

                     ELSE

                        RETURN _DoGridCustomDraw(i, a, lParam)

                     ENDIF

                  ELSE

                     RETURN _DoGridCustomDraw(i, a, lParam)

                  ENDIF

               ENDIF

            ENDIF

            IF hmg_GetNotifyCode(lParam) == -181
               hmg_ReDrawWindow(_hmg_acontrolhandles[i])
            ENDIF

            // Grid OnQueryData ............................

            IF hmg_GetNotifyCode(lParam) == LVN_GETDISPINFO

               IF hb_IsBlock(_HMG_aControlProcedures[i])

                  _PushEventInfo()
                  _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
                  _HMG_ThisType  := "C"
                  _HMG_ThisIndex := i
                  _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
                  _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

                  aPos := hmg_GETGRIDDISPINFOINDEX(lParam)
                  _HMG_ThisQueryRowIndex := aPos[1]
                  _HMG_ThisQueryColIndex := aPos[2]

                  Eval(_HMG_aControlProcedures[i])

                  IF Len(_HMG_aControlBkColor[i]) > 0 .AND. _HMG_ThisQueryColIndex == 1
                     hmg_SetGridQueryImage(lParam, _HMG_ThisQueryData)
                  ELSE
                     hmg_SetGridQueryData(lParam, hb_ValToStr(_HMG_ThisQueryData))
                  ENDIF

                  _HMG_ThisQueryRowIndex := 0
                  _HMG_ThisQueryColIndex := 0
                  _HMG_ThisQueryData := ""
                  _PopEventInfo()

                  RETURN 0

               ENDIF

            ENDIF

            // Grid LostFocus ..............................

            IF hmg_GetNotifyCode(lParam) == NM_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Grid GotFocus ...............................

            IF hmg_GetNotifyCode(lParam) == NM_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Grid Change .................................

            IF hmg_GetNotifyCode(lParam) == LVN_KEYDOWN .AND. hmg_GetGridvKey(lParam) == 32
               SpaceKeyIsPressedInGrid := _HMG_aControlHandles[i]
            ENDIF

            IF hmg_GetNotifyCode(lParam) == LVN_ITEMCHANGED

               // Grid Checkboxes Click ....................
               #define LVIS_UNCHECKED 0x1000
               #define LVIS_CHECKED   0x2000
               IF hmg_GetGridNewState(lParam) == LVIS_UNCHECKED .OR. hmg_GetGridNewState(lParam) == LVIS_CHECKED

                  aCellData := _GetGridCellData(i)
                  r := aCellData[1]

                  IF r > 0 .AND. r <= hmg_ListViewGetItemCount(_HMG_aControlHandles[i]) .OR. SpaceKeyIsPressedInGrid == _HMG_aControlHandles[i]
                     _DoControlEventProcedure(_HMG_aControlMiscData1[i][23], i, "CONTROL_ONCHANGE", SpaceKeyIsPressedInGrid, r)
                     SpaceKeyIsPressedInGrid := 0
                     RETURN 0
                  ENDIF

               ENDIF

               IF _HMG_aControlMiscData1[i, 10]

                  IF hmg_GetGridNewState(lParam) != 0
                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
                     RETURN 0
                  ENDIF

               ELSEIF hmg_GetGridOldState(lParam) == 0 .AND. hmg_GetGridNewState(lParam) != 0

                  IF _HMG_aControlFontColor[i]
                     lCellGridRowChanged := .T.
                     _HMG_aControlMiscData1[i][1] := hmg_LISTVIEW_GETFIRSTITEM(_HMG_aControlHandles[i])
                  ELSE
                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
                  ENDIF

                  RETURN 0

               ENDIF

            ENDIF

            // Grid Header Click ...........................

            IF hmg_GetNotifyCode(lParam) == LVN_COLUMNCLICK

               IF hb_IsArray(_HMG_aControlHeadClick[i])
                  lvc := hmg_GetGridColumn(lParam) + 1
                  IF Len(_HMG_aControlHeadClick[i]) >= lvc
                     _DoControlEventProcedure(_HMG_aControlHeadClick[i][lvc], i, , lvc)
                     RETURN 0
                  ENDIF
               ENDIF

            ENDIF

            // Grid Click ..................................

            IF hmg_GetNotifyCode(lParam) == NM_CLICK

               IF _HMG_aControlFontColor[i]

                  aCellData := _GetGridCellData(i)

                  IF (NewPos := aCellData[2]) > 0

                     x := _HMG_aControlMiscData1[i][17]
                     _HMG_aControlMiscData1[i][17] := NewPos

                     IF (nGridRowValue := _HMG_aControlMiscData1[i][1]) == 0
                        _HMG_aControlMiscData1[i][1] := aCellData[1]
                     ENDIF

                     IF lCellGridRowChanged .OR. x != NewPos
                        lCellGridRowChanged := .F.
                        hmg_LISTVIEW_REDRAWITEMS(_HMG_aControlHandles[i], nGridRowValue - 1, nGridRowValue - 1)
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
                     ENDIF

                  ENDIF

               ENDIF

               RETURN 0

            ENDIF

            // Grid Double Click ...........................

            IF hmg_GetNotifyCode(lParam) == NM_DBLCLK

               IF _HMG_aControlSpacing[i]

                  IF _HMG_aControlMiscData1[i][20] .OR. _HMG_aControlFontColor[i]

                     _PushEventInfo()
                     _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
                     _HMG_ThisType := "C"
                     _HMG_ThisIndex := i
                     _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
                     _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

                     aCellData := _GetGridCellData(i)

                     _HMG_ThisItemRowIndex   := aCellData[1]
                     _HMG_ThisItemColIndex   := aCellData[2]
                     _HMG_ThisItemCellRow    := aCellData[3]
                     _HMG_ThisItemCellCol    := aCellData[4]
                     _HMG_ThisItemCellWidth  := aCellData[5]
                     _HMG_ThisItemCellHeight := aCellData[6]

                     _GridInplaceEdit(i)

                     _PopEventInfo()
                     _HMG_ThisItemRowIndex   := 0
                     _HMG_ThisItemColIndex   := 0
                     _HMG_ThisItemCellRow    := 0
                     _HMG_ThisItemCellCol    := 0
                     _HMG_ThisItemCellWidth  := 0
                     _HMG_ThisItemCellHeight := 0

                  ELSE

                     _EditItem(_hmg_acontrolhandles[i])

                  ENDIF

               ELSE

                  IF hb_IsBlock(_HMG_aControlDblClick[i])

                     _PushEventInfo()
                     _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
                     _HMG_ThisType := "C"
                     _HMG_ThisIndex := i
                     _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
                     _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

                     aCellData := _GetGridCellData(i)

                     _HMG_ThisItemRowIndex   := aCellData[1]
                     _HMG_ThisItemColIndex   := aCellData[2]
                     _HMG_ThisItemCellRow    := aCellData[3]
                     _HMG_ThisItemCellCol    := aCellData[4]
                     _HMG_ThisItemCellWidth  := aCellData[5]
                     _HMG_ThisItemCellHeight := aCellData[6]

                     Eval(_HMG_aControlDblClick[i])

                     _PopEventInfo()
                     _HMG_ThisItemRowIndex   := 0
                     _HMG_ThisItemColIndex   := 0
                     _HMG_ThisItemCellRow    := 0
                     _HMG_ThisItemCellCol    := 0
                     _HMG_ThisItemCellWidth  := 0
                     _HMG_ThisItemCellHeight := 0

                  ENDIF

               ENDIF

               RETURN 0

            ENDIF

            // Grid Right Click ............................

            IF hmg_GetNotifyCode(lParam) == NM_RCLICK

               IF _DoControlEventProcedure(_HMG_aControlMiscData1[i][29], i)
                  RETURN 1
               ENDIF

            ENDIF

         ENDIF

         // DatePicker Process .............................

         IF _HMG_aControlType[i] == CONTROL_TYPE_DATEPICK .OR. _HMG_aControlType[i] == CONTROL_TYPE_TIMEPICK

            // DatePicker MonthCal colors ..................

            IF _HMG_aControlType[i] == CONTROL_TYPE_DATEPICK .AND. hmg_GetNotifyCode(lParam) == DTN_DROPDOWN
               BackColor  := _HMG_aControlMiscData1[i][1]
               FontColor  := _HMG_aControlMiscData1[i][2]
               TitleBkClr := _HMG_aControlMiscData1[i][3]
               TitleFrClr := _HMG_aControlMiscData1[i][4]
               TrlFontClr := _HMG_aControlMiscData1[i][5]
               IF _HMG_IsThemed .AND. (IsArrayRGB(backcolor) .OR. IsArrayRGB(fontcolor) .OR. IsArrayRGB(TitleBkClr) .OR. IsArrayRGB(TitleFrClr))
                  hWnd := hmg_SendMessage(_HMG_aControlHandles[i], DTM_GETMONTHCAL, 0, 0)
                  SetWindowTheme(hWnd, "", "")
                  IF IsArrayRGB(BackColor)
                     SetMonthCalMonthBkColor(hWnd, BackColor[1], BackColor[2], BackColor[3])
                  ENDIF
                  IF IsArrayRGB(FontColor)
                     SetMonthCalFontColor(hWnd, FontColor[1], FontColor[2], FontColor[3])
                  ENDIF
                  IF IsArrayRGB(TitleBkClr)
                     SetMonthCalTitleBkColor(hWnd, TitleBkClr[1], TitleBkClr[2], TitleBkClr[3])
                  ENDIF
                  IF IsArrayRGB(TitleFrClr)
                     SetMonthCalTitleFontColor(hWnd, TitleFrClr[1], TitleFrClr[2], TitleFrClr[3])
                  ENDIF
                  IF IsArrayRGB(TrlFontClr)
                     SetMonthCalTrlFontColor(hWnd, TrlFontClr[1], TrlFontClr[2], TrlFontClr[3])
                  ENDIF
                  // get the month calendar coordinate
                  aPos := {0, 0, 0, 0}
                  hmg_GetWindowRect(hWnd, aPos)
                  // set the ideal size of the month calendar control
                  hmg_SetPosMonthCal(hmg_GetParent(hWnd), aPos[1], aPos[2], .T.)
                  hmg_setfocus(hWnd)
               ENDIF
               RETURN 0
            ENDIF

            // DatePicker/TimePicker Change ................

            IF _HMG_aControlType[i] == CONTROL_TYPE_DATEPICK .AND. hmg_GetNotifyCode(lParam) == DTN_DATETIMECHANGE .AND. ;
               hmg_SendMessage(_HMG_aControlHandles[i], DTM_GETMONTHCAL, 0, 0) == 0 .OR. hmg_GetNotifyCode(lParam) == DTN_CLOSEUP
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ELSEIF _HMG_aControlType[i] == CONTROL_TYPE_TIMEPICK .AND. hmg_GetNotifyCode(lParam) == DTN_DATETIMECHANGE
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ENDIF

            // DatePicker/TimePicker LostFocus .............

            IF hmg_GetNotifyCode(lParam) == NM_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // DatePicker/TimePicker GotFocus ..............

            IF hmg_GetNotifyCode(lParam) == NM_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

         ENDIF

         // RichEditBox Processing .........................

         IF _HMG_aControlType[i] = CONTROL_TYPE_RICHEDIT .AND. _HMG_aControlMiscData1[i] == 1  // by Dr. Claudio Soto, January 2014

            // RichEditBox Selelection Change ..............

            IF hmg_GetNotifyCode(lParam) = EN_SELCHANGE
               _DoControlEventProcedure(_HMG_aControlSpacing[i], i)
               RETURN 0
            ENDIF

            IF hmg_GetNotifyCode(lParam) = EN_LINK

               IF hmg_GetNotifyLink(lParam, NIL, NIL, @_HMG_CharRange_Min, @_HMG_CharRange_Max) == WM_LBUTTONDOWN
                  _DoControlEventProcedure(_HMG_aControlRangeMin[i], i)
                  _HMG_CharRange_Min := 0
                  _HMG_CharRange_Max := 0
                  RETURN 0
               ENDIF

            ENDIF

         ENDIF

         // Tab Processing .................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_TAB

            // Tab Change ..................................

            IF hmg_GetNotifyCode(lParam) == TCN_SELCHANGING
               nOldPage := _GetValue(, , i)
               RETURN 0
            ELSEIF hmg_GetNotifyCode(lParam) == TCN_SELCHANGE
               IF Len(_HMG_aControlPageMap[i]) > 0
                  IF !_HMG_ProgrammaticChange
                     UpdateTab(i)
                     _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
                  ELSEIF !hb_IsBlock(_HMG_aControlChangeProcedure[i]) .OR. ;
                        _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE", nOldPage)
                     UpdateTab(i)
                  ELSE
                     hmg_TabCtrl_SetCurSel(_HMG_aControlHandles[i], nOldPage)
                  ENDIF
               ENDIF
               RETURN 0
            ENDIF

         ENDIF

         // Tree Processing ................................

         IF _HMG_aControlType[i] == CONTROL_TYPE_TREE

            // Tree LostFocus ..............................

            IF hmg_GetNotifyCode(lParam) == NM_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Tree GotFocus ...............................

            IF hmg_GetNotifyCode(lParam) == NM_SETFOCUS
               VirtualChildControlFocusProcess(_HMG_aControlHandles[i], _HMG_aControlParentHandles[i])
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // Tree Change .................................

            IF hmg_GetNotifyCode(lParam) == TVN_SELCHANGED
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ENDIF

            // Tree Double Click ...........................

            IF hmg_GetNotifyCode(lParam) == NM_DBLCLK
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
               RETURN 0
            ENDIF

         ENDIF

#ifdef _TSBROWSE_
         IF _HMG_aControlType[i] == CONTROL_TYPE_TOOLBAR

            IF hmg_GetNotifyCode(lParam) == TBN_QUERYINSERT .OR. hmg_GetNotifyCode(lParam) == TBN_QUERYDELETE
               RETURN 1
            ENDIF
            IF hmg_GetNotifyCode(lParam) == TBN_DELETINGBUTTON
                RETURN 1
            ENDIF
            IF hmg_GetNotifyCode(lParam) == TBN_INITCUSTOMIZE
               RETURN TBNRF_HIDEHELP
            ENDIF

            RETURN iif(hmg_ToolBarExCustFunc(hWnd, nMsg, wParam, lParam), 1, 0)

         ENDIF
#endif

#ifdef _PROPGRID_
         // PropGrid Processing ............................

         IF _HMG_aControlType[i] == CONTROL_TYPE_PROPGRID

            // PropGrid LostFocus ..........................

            IF hmg_GetNotifyCode(lParam) == NM_KILLFOCUS
               _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // PropGrid GotFocus ...........................

            IF hmg_GetNotifyCode(lParam) == NM_SETFOCUS
               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)
               RETURN 0
            ENDIF

            // PropGrid Double Click .......................

            IF hmg_GetNotifyCode(lParam) == NM_DBLCLK
               _DoControlEventProcedure(_HMG_aControlDblClick[i], i)
               RETURN 0
            ENDIF

            // PropGrid Change .............................

            IF hmg_GetNotifyCode(lParam) == TVN_SELCHANGED
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i, "CONTROL_ONCHANGE")
               RETURN 0
            ENDIF

            IF hmg_GetNotifyCode(lParam) == -181
               hmg_ReDrawWindow(_hmg_acontrolhandles[i])
            ENDIF

         ENDIF
#endif
         // StatusBar Processing ...........................

         IF _HMG_aControlType[i] == CONTROL_TYPE_MESSAGEBAR

            IF hmg_GetNotifyCode(lParam) == NM_CLICK  // StatusBar Click

               hmg_DefWindowProc(hWnd, NM_CLICK, wParam, lParam)

               x := hmg_GetItemPos(lParam) + 1

               FOR EACH r IN _HMG_aControlHandles

                  i := hb_enumindex(r)

                  IF _HMG_aControlType[i] == CONTROL_TYPE_ITEMMESSAGE .AND. _HMG_aControlParentHandles[i] == hWnd

                     IF r == x

                        IF _DoControlEventProcedure(_HMG_aControlProcedures[i], i)
                           RETURN 0
                        ENDIF

                     ENDIF

                  ENDIF

               NEXT

            ENDIF

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_NCACTIVATE
   //**************************************************************************

      IF _HMG_IsXPorLater .AND. wParam == 0 .AND. lParam == 0

         IF _HMG_InplaceParentHandle != 0
            InsertReturn()
         ENDIF

      ENDIF
      EXIT

#ifdef _HMG_COMPAT_
   //**************************************************************************
   CASE WM_WINDOWPOSCHANGING
   //**************************************************************************

      _HMG_DialogBoxProcedure()
      EXIT
#endif
   //**************************************************************************
   CASE WM_QUERYENDSESSION
   //**************************************************************************

      __Quit()
      EXIT
   //**************************************************************************
   CASE WM_CLOSE
   //**************************************************************************

#ifdef _TSBROWSE_
      oGet := GetObjectByHandle(hWnd)
      IF GetEscapeState() < 0 .AND. ((_GetFocusedControlType(hWnd) == CONTROL_TYPE_EDIT .OR. _GetFocusedControlType(hWnd) == CONTROL_TYPE_RICHEDIT) .OR. ;
         iif(hb_IsObject(oGet), "Edit" $ oGet:cChildControl, .F. ) )
#else
      IF GetEscapeState() < 0 .AND. (_GetFocusedControlType(hWnd) == CONTROL_TYPE_EDIT .OR. _GetFocusedControlType(hWnd) == CONTROL_TYPE_RICHEDIT)
#endif
         RETURN 1
      ENDIF

      IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0

         // Process Interactive Close Event / Setting
         IF hb_IsBlock(_HMG_aFormInteractiveCloseProcedure[i])

            r := _DoWindowEventProcedure(_HMG_aFormInteractiveCloseProcedure[i], i, "WINDOW_ONINTERACTIVECLOSE")
            IF hb_IsLogical(r) .AND. !r
               RETURN 1
            ENDIF

         ENDIF

         IF lParam != 1

            SWITCH _HMG_InteractiveClose

            CASE 0
               MsgStop(_HMG_MESSAGE[3])
               RETURN 1

            CASE 2
               IF !MsgYesNo(_HMG_MESSAGE[1], _HMG_MESSAGE[2])
                  RETURN 1
               ENDIF
               EXIT

            CASE 3
               IF _HMG_aFormType[i] == "A"
                  IF !MsgYesNo(_HMG_MESSAGE[1], _HMG_MESSAGE[2])
                     RETURN 1
                  ENDIF
               ENDIF

            END SWITCH

         ENDIF

         // Process AutoRelease Property
         IF !_HMG_aFormAutoRelease[i]

            _HideWindow(_HMG_aFormNames[i])
            RETURN 1

         ENDIF

         // If Not AutoRelease Then Destroy Window
         IF _HMG_aFormType[i] == "A"

            ReleaseAllWindows()

         ELSE

            IF hb_IsBlock(_HMG_aFormReleaseProcedure[i])
               _HMG_InteractiveCloseStarted := .T.
               _DoWindowEventProcedure(_HMG_aFormReleaseProcedure[i], i, "WINDOW_RELEASE")
            ENDIF

            IF _HMG_lOOPEnabled
               Eval(_HMG_bOnFormDestroy, i)
            ENDIF

            _hmg_OnHideFocusManagement(i)

            hmg_DestroyWindow(hWnd)

         ENDIF

      ENDIF
      EXIT
   //**************************************************************************
   CASE WM_DESTROY
   //**************************************************************************

      i := AScan(_HMG_aFormHandles, hWnd)

      IF i > 0

         // Remove All HMG_* Properties

         IF Len(EnumProps(hWnd)) > 0
            EnumPropsEx(hWnd, {|hWnd, cPropName, hHandle|HB_SYMBOL_UNUSED(hHandle), iif(hb_LeftEqI(cPropName, "HMG_"), RemoveProp(hWnd, cPropName), NIL), .T.})
         ENDIF

         // Remove Child Controls

         FOR EACH r IN _HMG_aControlParentHandles

            IF r == hWnd

               x := hb_enumindex(r)

               IF _HMG_aControlType[x] == CONTROL_TYPE_MESSAGEBAR
                  _ReleaseControl("StatusBar", GetParentFormName(x))
               ELSE
                  _EraseControl(x, i)
               ENDIF

            ENDIF

         NEXT

         // Delete Brush

         hmg_DeleteObject(_HMG_aFormBrushHandle[i])

         // Delete ToolTip

         ReleaseControl(_HMG_aFormToolTipHandle[i])

         // Update/Release Form Index Variable

         mVar := "_" + _HMG_aFormNames[i]
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

         // If Window was Multi-activated, determine If it is a Last one.
         // If Yes, then post Quit message to finish the Message Loop.

         // Quit Message, will be posted always for single activated windows.

         IF _HMG_aFormActivateId[i] > 0

            TmpStr := "_HMG_ACTIVATE_" + hb_ntos(_HMG_aFormActivateId[i])

#ifdef _NAMES_LIST_
            Tmp := _GetNameList(TmpStr)
#else
            Tmp := __mvGetDef(TmpStr)
#endif
            IF hb_IsNumeric(Tmp)

#ifdef _NAMES_LIST_
               _SetNameList(TmpStr, --Tmp)
#else
               __mvPut(TmpStr, --Tmp)
#endif
               IF Tmp == 0
                  hmg_PostQuitMessage(0)
#ifdef _NAMES_LIST_
                  _DelNameList(TmpStr)
#else
                  __mvXRelease(TmpStr)
#endif
               ENDIF

            ENDIF

         ELSE

            hmg_PostQuitMessage(0)

         ENDIF

         _HMG_aFormDeleted                  [i] := .T.
         _HMG_aFormhandles                  [i] := HMG_NULLHANDLE
         _HMG_aFormNames                    [i] := ""
         _HMG_aFormActive                   [i] := .F.
         _HMG_aFormType                     [i] := ""
         _HMG_aFormParenthandle             [i] := HMG_NULLHANDLE
         _HMG_aFormInitProcedure            [i] := ""
         _HMG_aFormReleaseProcedure         [i] := ""
         _HMG_aFormToolTipHandle            [i] := HMG_NULLHANDLE
         _HMG_aFormContextMenuHandle        [i] := HMG_NULLHANDLE
         _HMG_aFormMouseDragProcedure       [i] := ""
         _HMG_aFormSizeProcedure            [i] := ""
         _HMG_aFormClickProcedure           [i] := ""
         _HMG_aFormMouseMoveProcedure       [i] := ""
         _HMG_aFormMoveProcedure            [i] := ""
         _HMG_aFormDropProcedure            [i] := ""
         _HMG_aFormBkColor                  [i] := NIL
         _HMG_aFormPaintProcedure           [i] := ""
         _HMG_aFormNoShow                   [i] := .F.
         _HMG_aFormNotifyIconName           [i] := ""
         _HMG_aFormNotifyIconToolTip        [i] := ""
         _HMG_aFormNotifyIconLeftClick      [i] := ""
         _HMG_aFormNotifyIconDblClick       [i] := ""
         _HMG_aFormReBarHandle              [i] := HMG_NULLHANDLE
         _HMG_aFormNotifyMenuHandle         [i] := HMG_NULLHANDLE
         _HMG_aFormBrowseList               [i] := {}
         _HMG_aFormSplitChildList           [i] := {}
         _HMG_aFormVirtualHeight            [i] := 0
         _HMG_aFormGotFocusProcedure        [i] := ""
         _HMG_aFormLostFocusProcedure       [i] := ""
         _HMG_aFormVirtualWidth             [i] := 0
         _HMG_aFormFocused                  [i] := .F.
         _HMG_aFormScrollUp                 [i] := ""
         _HMG_aFormScrollDown               [i] := ""
         _HMG_aFormScrollLeft               [i] := ""
         _HMG_aFormScrollRight              [i] := ""
         _HMG_aFormHScrollBox               [i] := ""
         _HMG_aFormVScrollBox               [i] := ""
         _HMG_aFormBrushHandle              [i] := HMG_NULLHANDLE
         _HMG_aFormFocusedControl           [i] := HMG_NULLHANDLE
         _HMG_aFormGraphTasks               [i] := {}
         _HMG_aFormMaximizeProcedure        [i] := NIL
         _HMG_aFormMinimizeProcedure        [i] := NIL
         _HMG_aFormRestoreProcedure         [i] := NIL
         _HMG_aFormAutoRelease              [i] := .F.
         _HMG_aFormInteractiveCloseProcedure[i] := ""
         _HMG_aFormMinMaxInfo               [i] := {}
         _HMG_aFormActivateId               [i] := 0
         _HMG_aFormMiscData1                [i] := {}
         _HMG_aFormMiscData2                [i] := ""

         _HMG_InteractiveCloseStarted := .F.

      ENDIF
      EXIT
   //**************************************************************************
   DEFAULT
   //**************************************************************************

     IF nMsg == _HMG_MsgIDFindDlg   // FindReplace Dialog Notification ( by Dr. Claudio Soto, January 2014 )

         _HMG_FindReplaceOptions := hmg_FindReplaceDlgGetOptions(lParam)

         Eval(_HMG_FindReplaceOnAction)

         IF _HMG_FindReplaceOptions[1] == 0   // User CANCEL or CLOSE Dialog
            hmg_FindReplaceDlgRelease(.T.)      // Destroy Dialog Window and Set NULL Dialog Handle
         ENDIF

         AFill(_HMG_FindReplaceOptions, NIL)

      ELSEIF nMsg == _HMG_ListBoxDragNotification

         SWITCH hmg_GET_DRAG_LIST_NOTIFICATION_CODE(lParam)

         CASE DL_BEGINDRAG
            // Original Item
            _HMG_ListBoxDragItem := hmg_GET_DRAG_LIST_DRAGITEM(lParam)
            RETURN 1

         CASE DL_DRAGGING
            // Current Item
            _HMG_ListBoxDragListId := hmg_GET_DRAG_LIST_DRAGITEM(lParam)

            IF _HMG_ListBoxDragListId > _HMG_ListBoxDragItem
               hmg_DRAG_LIST_DRAWINSERT(hWnd, lParam, _HMG_ListBoxDragListId + 1)
            ELSE
               hmg_DRAG_LIST_DRAWINSERT(hWnd, lParam, _HMG_ListBoxDragListId)
            ENDIF

            IF _HMG_ListBoxDragListId != -1

               IF _HMG_ListBoxDragListId > _HMG_ListBoxDragItem
                  hmg_SetResCursor(hmg_LoadCursor(hmg_GetInstance(), "MINIGUI_DRAGDOWN"))
               ELSE
                  hmg_SetResCursor(hmg_LoadCursor(hmg_GetInstance(), "MINIGUI_DRAGUP"))
               ENDIF

               RETURN 0

            ENDIF

            RETURN DL_STOPCURSOR

         CASE DL_CANCELDRAG
            _HMG_ListBoxDragItem := -1
            EXIT

         CASE DL_DROPPED
            _HMG_ListBoxDragListId := hmg_GET_DRAG_LIST_DRAGITEM(lParam)

            IF _HMG_ListBoxDragListId != -1
               hmg_DRAG_LIST_MOVE_ITEMS(lParam, _HMG_ListBoxDragItem, _HMG_ListBoxDragListId)
            ENDIF

            hmg_DRAG_LIST_DRAWINSERT(hWnd, lParam, -1)
            _HMG_ListBoxDragItem := -1

         END SWITCH

      ENDIF

   END SWITCH

RETURN 0

/*
*/
PROCEDURE _SetNextFocus(lSkip)

   LOCAL NextControlHandle
   LOCAL hWnd
   LOCAL lMdiChildActive := (_HMG_BeginWindowMDIActive .AND. !_HMG_IsModalActive)
   LOCAL lShift
   LOCAL i

   hWnd := iif(lMdiChildActive, GetActiveMdiHandle(), hmg_GetActiveWindow())

   NextControlHandle := hmg_GetNextDlgTabITem(hWnd, hmg_GetFocus(), hb_defaultValue(lSkip, .F.))

   lShift := _GetKeyState(VK_SHIFT)  // Is Shift key pressed ?

   IF (i := AScan(_HMG_aControlHandles, NextControlHandle)) > 0

      IF _HMG_aControlType[i] == CONTROL_TYPE_BUTTON .OR. lMdiChildActive

         hmg_SetFocus(NextControlHandle)

         IF lMdiChildActive .AND. (i := AScan(_HMG_aFormHandles, hWnd)) > 0
            _HMG_aFormFocusedControl[i] := NextControlHandle
         ELSE
            hmg_SendMessage(NextControlHandle, BM_SETSTYLE, hmg_LOWORD(BS_DEFPUSHBUTTON), 1)
         ENDIF

      ELSE

         iif(lshift, hmg_InsertShiftTab(), InsertTab())

      ENDIF

   ELSEIF !lMdiChildActive

      iif(lshift, hmg_InsertShiftTab(), InsertTab())

   ENDIF

RETURN

/*
*/
PROCEDURE _PushEventInfo()

   AAdd(_HMG_aEventInfo, {_HMG_ThisFormIndex, _HMG_ThisEventType, _HMG_ThisType, _HMG_ThisIndex, _HMG_ThisFormName, _HMG_ThisControlName})

RETURN

/*
*/
PROCEDURE _PopEventInfo(n)

   LOCAL l

   IF (l := Len(_HMG_aEventInfo)) > 0

      DEFAULT n := 0

      IF n > 0 .AND. n <= l; l := n
      ENDIF

      _HMG_ThisFormIndex   := _HMG_aEventInfo[l][1]
      _HMG_ThisEventType   := _HMG_aEventInfo[l][2]
      _HMG_ThisType        := _HMG_aEventInfo[l][3]
      _HMG_ThisIndex       := _HMG_aEventInfo[l][4]
      _HMG_ThisFormName    := _HMG_aEventInfo[l][5]
      _HMG_ThisControlName := _HMG_aEventInfo[l][6]

      IF n == 0
         ASize(_HMG_aEventInfo, l - 1)
      ENDIF

   ELSE

      _HMG_ThisIndex := 0
      _HMG_ThisFormIndex := 0
      _HMG_ThisType := ""
      _HMG_ThisEventType := ""
      _HMG_ThisFormName :=  ""
      _HMG_ThisControlName := ""

   ENDIF

RETURN

#ifdef _TSBROWSE_
/*
*/
FUNCTION GetObjectByHandle(hWnd)

   LOCAL oWnd
   LOCAL nPos

   IF Type("_TSB_aControlhWnd") == "A" .AND. Len(_TSB_aControlhWnd) > 0

      IF (nPos := AScan(_TSB_aControlhWnd, hWnd)) > 0
         oWnd := _TSB_aControlObjects[nPos]
      ENDIF

   ENDIF

RETURN oWnd
#endif

#ifdef _USERINIT_
/*
*/
PROCEDURE InstallEventHandler(cProcedure)

   AAdd(_HMG_aCustomEventProcedure, AllTrim(Upper(NoBrackets(cProcedure))))

RETURN

/*
*/
PROCEDURE InstallPropertyHandler(cPropertyName, cSetProcedure, cGetProcedure)

   AAdd(_HMG_aCustomPropertyProcedure, {AllTrim(Upper(cPropertyName)), AllTrim(Upper(cSetProcedure)), AllTrim(Upper(cGetProcedure))})

RETURN

/*
*/
PROCEDURE InstallMethodHandler(cEventName, cMethodProcedure)

   AAdd(_HMG_aCustomMethodProcedure, {AllTrim(Upper(cEventName)), AllTrim(Upper(cMethodProcedure))})

RETURN

/*
*/
STATIC FUNCTION NoBrackets(cStr)

   LOCAL nPos

   IF (nPos := hb_UAt("(", cStr)) > 0
      RETURN hb_ULeft(cStr, nPos - 1)
   ENDIF

RETURN cStr
#endif

/*
*/
STATIC FUNCTION _DoGridCustomDraw(i, a, lParam)

   LOCAL aTemp
   LOCAL aTemp2

   IF a[1] >= 1 .AND. a[1] <= hmg_ListViewGetItemCount(_HMG_aControlHandles[i]) .AND. ; // MaxGridRows
      a[2] >= 1 .AND. a[2] <= hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])      // MaxGridCols

      aTemp  := _HMG_aControlMiscData1[i, 22]
      aTemp2 := _HMG_aControlMiscData1[i, 21]

      IF hb_IsArray(aTemp) .AND. !hb_isArray(aTemp2)

         IF Len(aTemp) >= a[1]
            IF aTemp[a[1]][a[2]] != -1
               RETURN hmg_SetBcFc(lParam, aTemp[a[1]][a[2]], RGB(0, 0, 0))
            ELSE
               RETURN hmg_SETBRCCD(lParam)
            ENDIF
         ENDIF

      ELSEIF !hb_isArray(aTemp) .AND. hb_IsArray(aTemp2)

         IF Len(aTemp2) >= a[1]
            IF aTemp2[a[1]][a[2]] != -1
               RETURN hmg_SetBcFc(lParam, RGB(255, 255, 255), aTemp2[a[1]][a[2]])
            ELSE
               RETURN hmg_SETBRCCD(lParam)
            ENDIF
         ENDIF

      ELSEIF hb_IsArray(aTemp) .AND. hb_IsArray(aTemp2)

         IF Len(aTemp) >= a[1] .AND. Len(aTemp2) >= a[1]
            IF aTemp[a[1]][a[2]] != -1
               RETURN hmg_SetBcFc(lParam, aTemp[a[1]][a[2]], aTemp2[a[1]][a[2]])
            ELSE
               RETURN hmg_SETBRCCD(lParam)
            ENDIF
         ENDIF

      ENDIF

   ELSE

      RETURN hmg_SETBRCCD(lParam)

   ENDIF

RETURN 0

/*
*/
STATIC FUNCTION _GetGridCellData(i)

   LOCAL ThisItemRowIndex
   LOCAL ThisItemColIndex
   LOCAL ThisItemCellRow
   LOCAL ThisItemCellCol
   LOCAL ThisItemCellWidth
   LOCAL ThisItemCellHeight
   LOCAL r
   LOCAL xs
   LOCAL xd
   LOCAL aCellData

   r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))

   IF r[2] == 1

      hmg_ListView_Scroll(_HMG_aControlHandles[i], -10000, 0)
      r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))

   ELSEIF r[1] > 0 .AND. r[2] > 0

      r := hmg_LISTVIEW_GETSUBITEMRECT(_HMG_aControlHandles[i], r[1] - 1, r[2] - 1)

      //          CellCol                      CellWidth
      xs := ((_HMG_aControlCol[i] + r[2]) + (r[3])) - (_HMG_aControlCol[i] + _HMG_aControlWidth[i])

      IF hmg_ListViewGetItemCount(_HMG_aControlHandles[i]) > hmg_ListViewGetCountPerPage(_HMG_aControlHandles[i])
         xd := 20
      ELSE
         xd := 0
      ENDIF

      IF xs > -xd
         hmg_ListView_Scroll(_HMG_aControlHandles[i], xs + xd, 0)
      ELSE
         IF r[2] < 0
            hmg_ListView_Scroll(_HMG_aControlHandles[i], r[2], 0)
         ENDIF
      ENDIF

      r := hmg_ListView_HitTest(_HMG_aControlHandles[i], GetCursorRow() - GetWindowRow(_HMG_aControlHandles[i]), GetCursorCol() - GetWindowCol(_HMG_aControlHandles[i]))

   ELSE

      r := AFill(Array(4), 0)

   ENDIF

   ThisItemRowIndex := r[1]
   ThisItemColIndex := r[2]

   IF r[2] == 1

      r := hmg_ListView_GetItemRect(_HMG_aControlHandles[i], r[1] - 1)

   ELSEIF r[1] > 0 .AND. r[2] > 0

      r := hmg_ListView_GetSubItemRect(_HMG_aControlHandles[i], r[1] - 1, r[2] - 1)

   ENDIF

   ThisItemCellRow := _HMG_aControlRow[i] + r[1]
   ThisItemCellCol := _HMG_aControlCol[i] + r[2]
   ThisItemCellWidth := r[3]
   ThisItemCellHeight := r[4]

   aCellData := {ThisItemRowIndex, ThisItemColIndex, ThisItemCellRow, ThisItemCellCol, ThisItemCellWidth, ThisItemCellHeight}

RETURN aCellData

/*
*/
STATIC FUNCTION _GetFocusedControlType(nFormHandle)

   LOCAL nHandle := hmg_GetFocus()
   LOCAL cType As String
   LOCAL hControl
   LOCAL i

   FOR EACH hControl IN _HMG_aControlHandles

      i := hb_enumindex(hControl)

      IF _HMG_aControlParentHandles[i] == nFormHandle

         IF hb_IsNumeric(hControl) .AND. hControl == nHandle // TODO:
            cType := _HMG_aControlType[i]
            EXIT
         ENDIF

      ENDIF

   NEXT

RETURN cType

/*
*/
STATIC FUNCTION GetMenuItems(lMenuItem, hMenu)

   LOCAL i
   LOCAL h
   LOCAL aMenuItems := {}
   LOCAL cMenuType := iif(lMenuItem, CONTROL_TYPE_MENU, CONTROL_TYPE_POPUP)

   FOR EACH h IN _HMG_aControlHandles

      i := hb_enumindex(h)

      IF _HMG_aControlType[i] == cMenuType .AND. _HMG_aControlPageMap[i] == hMenu .AND. iif(lMenuItem, .T., (_HMG_aControlIds[i] == 1))
         AAdd(aMenuItems, _HMG_aControlCaption[i])
      ENDIF

   NEXT

RETURN aMenuItems

#define DT_VCENTER       0x00000004
#define DT_SINGLELINE    0x00000020

/*
*/
STATIC PROCEDURE _OnDrawStatusItem(hWnd, lParam)

   LOCAL hDC
   LOCAL aRect
   LOCAL nItem
   LOCAL hBrush
   LOCAL oldBkMode
   LOCAL i
   LOCAL h
   LOCAL nIndex := 0

   hDC := hmg_GETOWNBTNDC(lParam)
   aRect := hmg_GETOWNBTNRECT(lParam)
   nItem := hmg_GETOWNBTNITEMID(lParam)

   FOR EACH h IN _HMG_aControlParentHandles

      i := hb_enumindex(h)

      IF _HMG_aControlType[i] == CONTROL_TYPE_ITEMMESSAGE .AND. h == hWnd

         IF nIndex++ == nItem
            oldBkMode := hmg_SetBkMode(hDC, TRANSPARENT)

            IF _HMG_aControlBkColor[i] != NIL
               hBrush := hmg_CreateSolidBrush(_HMG_aControlBkColor[i][1], _HMG_aControlBkColor[i][2], _HMG_aControlBkColor[i][3])
               hmg_FillRect(hDC, aRect[1], aRect[2], aRect[3], aRect[4], hBrush)
               hmg_DeleteObject(hBrush)
            ENDIF

            IF _HMG_aControlFontColor[i] != NIL
               hmg_SetTextColor(hDC, _HMG_aControlFontColor[i][1], _HMG_aControlFontColor[i][2], _HMG_aControlFontColor[i][3])
            ENDIF

            hmg_DrawText(hDC, _HMG_aControlCaption[i], aRect[1] + 1, aRect[2], aRect[3] - 1, aRect[4], hb_BitOr(_HMG_aControlSpacing[i], DT_SINGLELINE, DT_VCENTER))

            hmg_SetBkMode(hDC, oldBkMode)
            EXIT
         ENDIF

      ENDIF

   NEXT

RETURN

/*
*/
STATIC FUNCTION _OnGetMinMaxInfo(hWnd, pMinMaxInfo)

   LOCAL i
   LOCAL nRet := 0

   IF (i := AScan(_HMG_aFormHandles, hWnd)) > 0

      IF Len(_HMG_aFormMinMaxInfo[i]) > 0
         nRet := hmg_SetMinMaxInfo(pMinMaxInfo, _HMG_aFormMinMaxInfo[i])
      ENDIF

   ENDIF

RETURN nRet

/*
*/
STATIC FUNCTION _OnFlashExit()

   LOCAL x
   LOCAL lExit := .F.
   LOCAL nFormCount := Len(_HMG_aFormNames)

   FOR x := 1 TO nFormCount

      IF _HMG_aFormHandles[x] == _HMG_ActiveModalHandle .AND. _HMG_aFormFocused[x]

         hmg_PostMessage(_HMG_ActiveModalHandle, WM_CLOSE, 0, 0)
         lExit := .T.
         EXIT

      ENDIF

   NEXT x

RETURN lExit

/*
*/
STATIC PROCEDURE _MouseCoordCorr(hWnd, i)

   IF _hmg_aformvirtualheight[i] > 0
      _HMG_MouseRow += hmg_GetScrollPos(hWnd, SB_VERT)
   ENDIF

   IF _hmg_aformvirtualwidth[i] > 0
      _HMG_MouseCol += hmg_GetScrollPos(hWnd, SB_HORZ)
   ENDIF

RETURN

/*
*/
STATIC PROCEDURE _ProcessSliderEvents(lParam, wParam)

   LOCAL i

   IF _SetGetGlobal("_HMG_OnChangeEvent") == NIL
      STATIC _HMG_OnChangeEvent AS GLOBAL VALUE .F., _HMG_OnScrollEvent AS GLOBAL VALUE .F.
   ENDIF

   IF (i := AScan(_HMG_aControlHandles, hmg_numbertohandle(lParam))) > 0

      IF _HMG_aControlType[i] == CONTROL_TYPE_SLIDER

         SWITCH hmg_LoWord(wParam)

         CASE TB_ENDTRACK
            IF _SetGetGlobal("_HMG_OnScrollEvent")
               ASSIGN GLOBAL _HMG_OnScrollEvent := .F.
            ELSE
               ASSIGN GLOBAL _HMG_OnChangeEvent := .T.
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
            ENDIF
            EXIT

         CASE TB_THUMBPOSITION
            IF _SetGetGlobal("_HMG_OnChangeEvent")
               ASSIGN GLOBAL _HMG_OnChangeEvent := .F.
            ELSE
               ASSIGN GLOBAL _HMG_OnScrollEvent := .T.
               _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)
            ENDIF
            EXIT

         CASE TB_THUMBTRACK
         CASE TB_LINEUP
         CASE TB_LINEDOWN
         CASE TB_PAGEUP
         CASE TB_PAGEDOWN
            _DoControlEventProcedure(_HMG_aControlProcedures[i], i)

         END SWITCH

      ENDIF

   ENDIF

RETURN

/*
*/
STATIC PROCEDURE RetDayState(i, lParam)

   LOCAL aData
   LOCAL aDays
   LOCAL aBoldDays
   LOCAL dStart
   LOCAL dEoM
   LOCAL dDay
   LOCAL nCount
   LOCAL iNextD
   LOCAL nMonth
   LOCAL nLen

   aBoldDays := _HMG_aControlPageMap[i]

   aData := hmg_GetDayStateData(lParam)
   nCount := aData[1]

   IF nCount < 1 .OR. Empty(Len(aBoldDays))
      RETURN
   ENDIF

   aDays := Array(nCount * 32)
   AFill(aDays, 0)

   dStart := aData[2]
   iNextD := AScan(aBoldDays, {|d|d >= dStart})

   IF iNextD > 0

      dEoM := EoM(dStart)
      nMonth := 0
      dDay := aBoldDays[iNextD]
      nLen := Len(aBoldDays)

      DO WHILE nMonth < nCount

         IF dDay <= dEoM
            aDays[nMonth * 32 + Day(dDay)] := 1
            iNextD++
            IF iNextD > nLen
               EXIT
            ENDIF
            dDay := aBoldDays[iNextD]
         ELSE
            nMonth++
            dEoM := EoM(dEoM + 1)
         ENDIF

      ENDDO

   ENDIF

   hmg_C_RETDAYSTATE(lParam, nCount, aDays)

RETURN

/*
*/
STATIC PROCEDURE DoComboAutoComplete(i)

   LOCAL hWnd := _HMG_aControlHandles[i]
   LOCAL cValue := hmg_GetWindowText(_HMG_aControlRangeMin[i])
   LOCAL nStart := Len(cValue)
   LOCAL lShowDropDown := _HMG_aControlMiscData1[i][8]
   LOCAL nPos := _HMG_aControlMiscData1[i][9]

   IF nPos > 0

      IF nStart == nPos

         IF nStart > 1

            cValue := SubStr(cValue, 1, nStart - 1)

         ELSE

            IF hmg_ComboFindString(hWnd, cValue) > 0

               hmg_ComboSelectString(hWnd, cValue)
               hmg_ComboEditSetSel(hWnd, 0, -1)
               _HMG_aControlMiscData1[i][9] := 0

               RETURN

            ENDIF

         ENDIF

      ENDIF

   ENDIF

   _HMG_aControlMiscData1[i][9] := nStart

   IF hWnd > 0

      IF lShowDropDown
         hmg_ComboShowDropDown(hWnd)
      ENDIF

      IF hmg_ComboFindString(hWnd, cValue) > 0

         hmg_ComboSelectString(hWnd, cValue)
         hmg_ComboEditSetSel(hWnd, nStart, -1)

      ENDIF

   ENDIF

RETURN

/*
*/
PROCEDURE _AutoAdjust(hWnd, aInitSize)

   LOCAL ParentForm
   LOCAL ControlCount
   LOCAL ControlName
   LOCAL ControlType
   LOCAL nDivw
   LOCAL nDivh
   LOCAL nDivw2
   LOCAL nDivh2
   LOCAL lAutoZooming := (_HMG_AutoZooming == .T.)
   LOCAL aControlExcept := {CONTROL_TYPE_IMAGE}
   LOCAL lInvisible := .T.
   LOCAL nWidth
   LOCAL nHeight
   LOCAL i
   LOCAL k
#ifdef _TSBROWSE_
   LOCAL oBrw
#endif

   nWidth := iif(GetDesktopWidth() < GetWindowWidth(hWnd), GetDesktopWidth(), GetWindowWidth(hWnd))
   nHeight := iif(GetDesktopHeight() < GetWindowHeight(hWnd), GetDesktopHeight(), GetWindowHeight(hWnd))

   IF hmg_IsWindowVisible(hWnd) .AND. !IsAppXPThemed()
      HideWindow(hWnd)
   ELSE
      lInvisible := !hmg_LockWindowUpdate(hWnd)
   ENDIF

   i := AScan(_HMG_aFormHandles, hWnd)
   ParentForm := _HMG_aFormNames[i]

   IF hb_IsArray(aInitSize)
      _HMG_aFormVirtualWidth[i] := aInitSize[1]
      _HMG_aFormVirtualHeight[i] := aInitSize[2]
   ENDIF

   IF _HMG_aFormVirtualWidth[i] > 0 .AND. _HMG_aFormVirtualHeight[i] > 0
      nDivw := nWidth / _HMG_aFormVirtualWidth[i]
      nDivh := nHeight / _HMG_aFormVirtualHeight[i]
   ELSE
      nDivw := 1
      nDivh := 1
   ENDIF

   IF lAutoZooming
      nDivw2 := nDivw
      nDivh2 := nDivh
   ELSEIF _HMG_AutoAdjustException
      AAdd(aControlExcept, CONTROL_TYPE_BUTTON)
      AAdd(aControlExcept, CONTROL_TYPE_OBUTTON)
      AAdd(aControlExcept, CONTROL_TYPE_CHECKBOX)
   ENDIF

   ControlCount := Len(_HMG_aControlHandles)

   FOR k := 1 TO ControlCount

      IF _HMG_aControlParentHandles[k] == hWnd

         ControlName := _HMG_aControlNames[k]
         ControlType := _HMG_aControlType[k]

         IF !Empty(ControlName) .AND. !(ControlType == CONTROL_TYPE_MENU .OR. ;
                                        ControlType == CONTROL_TYPE_HOTKEY .OR. ;
                                        ControlType == CONTROL_TYPE_TOOLBAR .OR. ;
                                        ControlType == CONTROL_TYPE_MESSAGEBAR .OR. ;
                                        ControlType == CONTROL_TYPE_ITEMMESSAGE .OR. ;
                                        ControlType == CONTROL_TYPE_TIMER) .AND. ;
            Empty(GetControlContainerHandle(ControlName, ParentForm))

            IF ControlType == CONTROL_TYPE_RADIOGROUP
               _HMG_aControlSpacing[k] := _HMG_aControlSpacing[k] * iif(_HMG_aControlMiscData1[k], nDivw, nDivh)
            ENDIF

            IF !lAutoZooming
               DO CASE
               CASE AScan(aControlExcept, ControlType) > 0 .OR. (ControlType == CONTROL_TYPE_DATEPICK .OR. ControlType == CONTROL_TYPE_TIMEPICK)
                  nDivw2 := 1
                  nDivh2 := 1
               CASE ControlType == CONTROL_TYPE_BTNNUMTEXT .OR. ;
                    ControlType == CONTROL_TYPE_BTNTEXT .OR. ;
                    ControlType == CONTROL_TYPE_CHARMASKTEXT .OR. ;
                    ControlType == CONTROL_TYPE_MASKEDTEXT .OR. ;
                    ControlType == CONTROL_TYPE_NUMTEXT .OR. ;
                    ControlType == CONTROL_TYPE_TEXT .OR. ;
                    ControlType == CONTROL_TYPE_CHECKLABEL .OR. ;
                    ControlType == CONTROL_TYPE_LABEL .OR. ;
                    ControlType == CONTROL_TYPE_GETBOX .OR. ;
                    ControlType == CONTROL_TYPE_SPINNER .OR. ;
                    ControlType == CONTROL_TYPE_HYPERLINK .OR. ;
                    ControlType == CONTROL_TYPE_PROGRESSBAR .OR. ;
                    ControlType == CONTROL_TYPE_COMBO .OR. ;
                    ControlType == CONTROL_TYPE_HOTKEYBOX
                  nDivw2 := nDivw
                  nDivh2 := 1
               OTHERWISE
                  nDivw2 := nDivw
                  nDivh2 := nDivh
               ENDCASE
            ENDIF

            _SetControlSizePos(ControlName, ParentForm, ;
               _GetControlRow(ControlName, ParentForm) * nDivh, _GetControlCol(ControlName, ParentForm) * nDivw, ;
               _GetControlWidth(ControlName, ParentForm) * nDivw2, _GetControlHeight(ControlName, ParentForm) * nDivh2)

#ifdef _TSBROWSE_
            IF ControlType == CONTROL_TYPE_TBROWSE
               oBrw := _HMG_aControlIds[k]
               IF oBrw:lIsDbf
                  oBrw:UpStable()
               ELSE
                  oBrw:Refresh(.T.)
                  oBrw:DrawSelect()
               ENDIF
            ELSEIF lAutoZooming .AND. ControlType != CONTROL_TYPE_SLIDER
#else
               IF lAutoZooming .AND. ControlType != CONTROL_TYPE_SLIDER
#endif
                  _SetFontSize(ControlName, ParentForm, _HMG_aControlFontSize[k] * nDivh)
               ENDIF

            ENDIF

         ENDIF

   NEXT k

   _HMG_aFormVirtualWidth[i] := nWidth
   _HMG_aFormVirtualHeight[i] := nHeight

   IF lInvisible
      hmg_ShowWindow(hWnd)
   ELSE
      hmg_LockWindowUpdate(0)
      hmg_RedrawWindow(hWnd)
   ENDIF

RETURN
