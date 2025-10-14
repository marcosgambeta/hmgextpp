//
// MINIGUI - Harbour Win32 GUI library source code
//
// BUTTONEX Control Source Code
// Copyright 2005 Jacek Kubica <kubica@wssk.wroc.pl>
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

#include "i_winuser.ch"
#include "minigui.ch"

#define BP_PUSHBUTTON 1

#define PBS_NORMAL 1
#define PBS_HOT 2
#define PBS_PRESSED 3
#define PBS_DISABLED 4
#define PBS_DEFAULTED 5

#define DT_TOP 0
#define DT_LEFT 0
#define DT_CENTER 1
#define DT_RIGHT 2
#define DT_VCENTER 4
#define DT_BOTTOM 8
#define DT_SINGLELINE 32

/* Aspects for owner buttons */
#define OBT_HORIZONTAL    0
#define OBT_VERTICAL      1
#define OBT_LEFTTEXT      2
#define OBT_UPTEXT        4
#define OBT_HOTLIGHT      8
#define OBT_FLAT          16
#define OBT_NOTRANSPARENT 32
#define OBT_NOXPSTYLE     64
#define OBT_ADJUST       128

STATIC lXPThemeActive := .F.

FUNCTION _DefineOwnerButton(ControlName, ParentForm, x, y, Caption, ;
   ProcedureName, w, h, image, tooltip, gotfocus, lostfocus, flat, notrans, HelpId, ;
   invisible, notabstop, default, icon, fontname, fontsize, bold, italic, underline, ;
   strikeout, lvertical, lefttext, uptext, aRGB_bk, aRGB_font, lnohotlight, lnoxpstyle, ;
   ladjust, handcursor, imagewidth, imageheight, aGradInfo, lhorizontal, bInit, cKey)

   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL aRet
   LOCAL aBmp
   LOCAL cParentForm
   LOCAL cPicture
   LOCAL mVar
   LOCAL k
   LOCAL oc
   LOCAL ow

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 100)
   hb_default(@h, 28)
   hb_default(@Caption, "")
   hb_default(@flat, .F.)
   hb_default(@notrans, .F.)
   hb_default(@invisible, .F.)
   hb_default(@notabstop, .F.)
   hb_default(@lvertical, .F.)
   hb_default(@lefttext, .F.)
   hb_default(@uptext, .F.)
   hb_default(@lnohotlight, .F.)
   hb_default(@lnoxpstyle, .F.)
   hb_default(@ladjust, .F.)
   hb_default(@imagewidth, -1)
   hb_default(@imageheight, -1)

   IF ladjust  // ignore CAPTION clause when ADJUST is defined
      Caption := ""
   ENDIF

   IF _HMG_ToolBarActive
      RETURN NIL
   ENDIF

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentForm := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentForm := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   IF !_IsWindowDefined(ParentForm)
      MsgMiniGuiError("Window: " + iif(ParentForm == NIL, "Parent", ParentForm) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentForm)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + " Already defined.")
   ENDIF

   IF !Empty(image) .AND. !Empty(icon)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + ". Either bitmap or icon must be specified.")
   ENDIF

   cPicture := iif(empty(icon), image, icon)
   IF hb_IsArray(cPicture)
      image := cPicture[1]
   ENDIF

   IF aRGB_bk != NIL .AND. !IsArrayRGB(aRGB_bk)
      aRGB_bk := nRGB2Arr(aRGB_bk)
   ENDIF

   IF aRGB_font != NIL .AND. !IsArrayRGB(aRGB_font)
      aRGB_font := nRGB2Arr(aRGB_font)
   ENDIF

   mVar := "_" + ParentForm + "_" + ControlName

   cParentForm := ParentForm

   ParentForm := GetFormHandle(ParentForm)

   aRet := hmg_InitOwnerButton(ParentForm, Caption, 0, x, y, w, h, image, flat, notrans, invisible, notabstop, default, icon, imagewidth, imageheight)

   ControlHandle := aRet[1]

   IF !Empty(image) .AND. Empty(aRet[2])
      aRet[2] := iif(_HMG_IsThemed .AND. hb_UAt(".", image) == 0 .AND. imagewidth < 0 .AND. imageheight < 0, ;
         hmg_C_GetResPicture(image), hmg__SetBtnPicture(ControlHandle, image, imagewidth, imageheight))
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

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
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

   _HMG_aControlType               [k] := CONTROL_TYPE_OBUTTON
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentForm
   _HMG_aControlIds                [k] := 0
   _HMG_aControlProcedures         [k] := ProcedureName
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := aGradInfo
   _HMG_aControlInputMask          [k] := hb_defaultValue(handcursor, .F.)
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := ""
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := aRGB_bk
   _HMG_aControlFontColor          [k] := aRGB_font
   _HMG_aControlDblClick           [k] := default
   _HMG_aControlHeadClick          [k] := {imagewidth, imageheight}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := iif(lvertical, 1, 0) + iif(lefttext, OBT_LEFTTEXT, 0) + iif(uptext, OBT_UPTEXT, 0) + iif(!lnohotlight, OBT_HOTLIGHT, 0) + iif(flat, OBT_FLAT, 0) + iif(notrans, OBT_NOTRANSPARENT, 0) + iif(lnoxpstyle, OBT_NOXPSTYLE, 0) + iif(ladjust, OBT_ADJUST, 0)
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := cPicture
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := hb_defaultValue(lhorizontal, .F.)
   _HMG_aControlRangeMax           [k] := {iif(lnohotlight, 2, 0), .F.}  // used for mouse hot tracking !!
   _HMG_aControlCaption            [k] := Caption
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := aRet[2]  // handle to an Image (Icon or Bitmap)
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := iif(empty(icon), 0, 1)  // 0 - bitmap  1 - icon
   _HMG_aControlMiscData2          [k] := ""

   IF !Empty(_HMG_aControlBrushHandle[k]) .AND. imagewidth < 0 .AND. imageheight < 0

      SWITCH _HMG_aControlMiscData1[k]
      CASE 0
         aBmp := hmg_GetBitmapSize(_HMG_aControlBrushHandle[k])
         EXIT
      CASE 1
         aBmp := hmg_GetIconSize(_HMG_aControlBrushHandle[k])
      ENDSWITCH

      _HMG_aControlHeadClick[k] := {aBmp[1], aBmp[2]}

   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentForm)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

   _SetHotKeyByName(cParentForm, ckey, ProcedureName)

RETURN NIL

FUNCTION OBTNEVENTS(hWnd, nMsg, wParam, lParam)
   
   LOCAL i

   IF (i := AScan(_HMG_aControlHandles, hWnd)) > 0 .AND. _HMG_aControlType[i] == CONTROL_TYPE_OBUTTON

      wParam := _HMG_aControlRangeMax[i][1]
      lParam := _HMG_aControlRangeMax[i][2]

      SWITCH nMsg

      CASE WM_MOUSEMOVE

         IF _HMG_aControlInputMask[i]
            RC_CURSOR("MINIGUI_FINGER")
         ENDIF

         IF !lParam

            _HMG_aControlRangeMax[i][2] := .T.

            IF hb_bitand(_HMG_aControlSpacing[i], OBT_HOTLIGHT) == OBT_HOTLIGHT .OR. _HMG_IsThemed

               _DoControlEventProcedure(_HMG_aControlGotFocusProcedure[i], i)

               IF wParam == 0
                  hmg_InvalidateRect(hWnd, 0)
                  _HMG_aControlRangeMax[i][1] := 1
               ENDIF

            ENDIF

         ENDIF
         EXIT

      CASE WM_MOUSELEAVE

         IF lParam

            _HMG_aControlRangeMax[i][2] := .F.

            _DoControlEventProcedure(_HMG_aControlLostFocusProcedure[i], i)

            IF hb_bitand(_HMG_aControlSpacing[i], OBT_HOTLIGHT) == OBT_HOTLIGHT .OR. lXPThemeActive

               IF wParam == 1
                  _HMG_aControlRangeMax[i][1] := 0
                  hmg_InvalidateRect(hWnd, 0)
               ENDIF

            ENDIF

         ENDIF

      ENDSWITCH

   ENDIF

RETURN 0

FUNCTION OwnButtonPaint(pdis)

   LOCAL hDC
   LOCAL itemState
   LOCAL itemAction
   LOCAL i
   LOCAL rgbTrans
   LOCAL hWnd
   LOCAL lFlat
   LOCAL lNotrans
   LOCAL oldBkMode
   LOCAL oldTextColor
   LOCAL hOldFont
   LOCAL hBrush
   LOCAL nFreeSpace
   LOCAL x1
   LOCAL y1
   LOCAL x2
   LOCAL y2
   LOCAL xp1
   LOCAL yp1
   LOCAL xp2
   LOCAL yp2
   LOCAL aBmp := {}
   LOCAL aMetr
   LOCAL aBtnRc
   LOCAL aBtnClipRc
   LOCAL aDarkColor
   LOCAL lDisabled
   LOCAL lSelected
   LOCAL lFocus
   LOCAL lDrawEntire
   LOCAL loFocus
   LOCAL loSelect
   LOCAL hTheme
   LOCAL nStyle
   LOCAL lnoxpstyle
   LOCAL lnoadjust
   LOCAL lXPThemeActive := .F.
   LOCAL pozYpic
   LOCAL pozYtext := 0
   LOCAL xPoz
   LOCAL nCRLF
   LOCAL lGradient
   LOCAL aGradient
   LOCAL lvertical

   hDC := hmg_GETOWNBTNDC(pdis)

   IF Empty(hDC) .OR. hmg_GETOWNBTNCTLTYPE(pdis) != ODT_BUTTON
      RETURN 1
   ENDIF

   itemAction := hmg_GETOWNBTNITEMACTION(pdis)
   lDrawEntire := (hb_bitand(itemAction, ODA_DRAWENTIRE) == ODA_DRAWENTIRE)
   loFocus := (hb_bitand(itemAction, ODA_FOCUS) == ODA_FOCUS)
   loSelect := (hb_bitand(itemAction, ODA_SELECT) == ODA_SELECT)

   IF !lDrawEntire .AND. !loFocus .AND. !loSelect
      RETURN 1
   ENDIF

   hWnd := hmg_GETOWNBTNHANDLE(pdis)
   aBtnRc := hmg_GETOWNBTNRECT(pdis)
   itemState := hmg_GETOWNBTNSTATE(pdis)

   i := AScan(_HMG_aControlHandles, hWnd)

   IF (i <= 0 .OR. _HMG_aControlType[i] != CONTROL_TYPE_OBUTTON)
      RETURN 1
   ENDIF

   nCRLF := hb_tokenCount(_HMG_aControlCaption[i], CRLF)

   lDisabled := hb_bitand(itemState, ODS_DISABLED) == ODS_DISABLED
   lSelected := hb_bitand(itemState, ODS_SELECTED) == ODS_SELECTED
   lFocus := hb_bitand(itemState, ODS_FOCUS) == ODS_FOCUS
   lFlat := hb_bitand(_HMG_aControlSpacing[i], OBT_FLAT) == OBT_FLAT
   lNotrans := hb_bitand(_HMG_aControlSpacing[i], OBT_NOTRANSPARENT) == OBT_NOTRANSPARENT
   lnoxpstyle := hb_bitand(_HMG_aControlSpacing[i], OBT_NOXPSTYLE) == OBT_NOXPSTYLE
   lnoadjust := !(hb_bitand(_HMG_aControlSpacing[i], OBT_ADJUST) == OBT_ADJUST)

   IF !lNotrans

      rgbTrans := NIL

   ELSE

      IF IsArrayRGB(_HMG_aControlBkColor[i]) .AND. !lXPThemeActive
         rgbTrans := RGB(_HMG_aControlBkColor[i, 1], _HMG_aControlBkColor[i, 2], _HMG_aControlBkColor[i, 3])
      ELSE
         rgbTrans := waGetSysColor(COLOR_BTNFACE)
      ENDIF

   ENDIF

   aGradient := _HMG_aControlValue[i]
   lGradient := (hb_IsArray(aGradient) .AND. !Empty(_HMG_aControlBkColor[i]))
   lvertical := !_HMG_aControlRangeMin[i]

   IF !lnoxpstyle .AND. _HMG_IsThemed .AND. !lGradient

      lXPThemeActive := .T.
      nStyle := PBS_NORMAL

      IF hb_bitand(itemState, ODS_HOTLIGHT) == ODS_HOTLIGHT .OR. _HMG_aControlRangeMax[i][1] == 1
         nStyle := PBS_HOT
      ELSEIF lFocus
         nStyle := PBS_DEFAULTED
      ENDIF

      IF lDisabled
         nStyle := PBS_DISABLED
      ENDIF

      IF lSelected
         nStyle := PBS_PRESSED
      ENDIF

      aBtnClipRc := AClone(aBtnRc)
      aBtnClipRc[3] += 1
      aBtnClipRc[4] += 1

      hTheme := hmg_OpenThemeData(hWnd, ToUnicode("BUTTON"))
      hmg_DrawThemeBackground(hTheme, hDC, BP_PUSHBUTTON, nStyle, aBtnRc, aBtnClipRc)
      hmg_CloseThemeData(hTheme)

   ELSE

      hmg_DrawButton(hDC, iif(lFocus .OR. _HMG_aControlRangeMax[i][1] == 1, 1, 0), ;
         DFCS_BUTTONPUSH + iif(lSelected, DFCS_PUSHED, 0) + iif(lDisabled, DFCS_INACTIVE, 0) + iif(lflat, DFCS_FLAT, 0), ;
         pdis, iif(hb_bitand(_HMG_aControlSpacing[i], OBT_HOTLIGHT) == OBT_HOTLIGHT, _HMG_aControlRangeMax[i][1], 2), iif(lflat, 1, 0))

   ENDIF

   hOldFont := hmg_SelectObject(hDC, _HMG_aControlFontHandle[i])
   aMetr := hmg_GetTextMetric(hDC)
   oldBkMode := hmg_SetBkMode(hDC, TRANSPARENT)
   oldTextColor := hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_BTNTEXT)), waGetGValue(waGetSysColor(COLOR_BTNTEXT)), waGetBValue(waGetSysColor(COLOR_BTNTEXT)))

   IF !lDisabled

      IF Empty(_HMG_aControlFontColor[i]) .OR. !IsArrayRGB(_HMG_aControlFontColor[i])
         hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_BTNTEXT)), waGetGValue(waGetSysColor(COLOR_BTNTEXT)), waGetBValue(waGetSysColor(COLOR_BTNTEXT)))
      ELSE
         hmg_SetTextColor(hDC, _HMG_aControlFontColor[i, 1], _HMG_aControlFontColor[i, 2], _HMG_aControlFontColor[i, 3])
      ENDIF

      IF !Empty(_HMG_aControlBkColor[i]) .AND. !lXPThemeActive

         xp1 := aBtnRc[1]
         xp2 := aBtnRc[2]
         yp1 := aBtnRc[3]
         yp2 := aBtnRc[4]

         // paint button background

         IF lGradient

            IF Len(aGradient[1]) == 2
               ReplaceGradInfo(aGradient, 1, 1)
               ReplaceGradInfo(aGradient, 1, 2)
            ENDIF

            IF lSelected

               IF Len(aGradient[1]) == 3
                  aDarkColor := _HMG_aControlBkColor[i]
                  IF IsArrayRGB(_HMG_aControlBkColor[i]) .AND. hb_IsNumeric(_HMG_aControlBkColor[i, 1])
                     _HMG_aControlBkColor[i] := {{1, _HMG_aControlBkColor[i], Darker(_HMG_aControlBkColor[i], 82)}}
                  ENDIF
                  _GradientFill(hDC, xp2 + 2, xp1 + 2, yp2 - 2, yp1 - 2, _HMG_aControlBkColor[i], lvertical)
                  _HMG_aControlBkColor[i] := aDarkColor
               ELSE
                  hBrush := hmg_CreateButtonBrush(hDC, yp1 - 2, yp2 - 2, aGradient[1][2], aGradient[1][1])
                  hmg_FillRect(hDC, xp1 + 2, xp2 + 2, yp1 - 2, yp2 - 2, hBrush)
                  hmg_DeleteObject(hBrush)
               ENDIF

            ELSEIF !(_HMG_aControlRangeMax[i][1] == 1)

               IF Len(aGradient[1]) == 3
                  _GradientFill(hDC, xp2 + 1, xp1 + 1, yp2 - 1, yp1 - 1, aGradient, lvertical)
               ELSE
                  hBrush := hmg_CreateButtonBrush(hDC, yp1 - 1, yp2 - 1, aGradient[1][1], aGradient[1][2])
                  hmg_FillRect(hDC, xp1 + 1, xp2 + 1, yp1 - 1, yp2 - 1, hBrush)
                  hmg_DeleteObject(hBrush)
               ENDIF

            ELSE

               IF Len(aGradient[1]) == 3
                  _GradientFill(hDC, xp2 + 1, xp1 + 1, yp2 - 1, yp1 - 1, ;
                     iif(Len(aGradient) == 1, InvertGradInfo(aGradient), ;
                     iif(hb_IsArray(_HMG_aControlBkColor[i, 1]), _HMG_aControlBkColor[i], ModifGradInfo(aGradient))), lvertical)
               ELSE
                  hBrush := hmg_CreateButtonBrush(hDC, yp1 - 1, yp2 - 1, aGradient[1][2], aGradient[1][1])
                  hmg_FillRect(hDC, xp1 + 1, xp2 + 1, yp1 - 1, yp2 - 1, hBrush)
                  hmg_DeleteObject(hBrush)
               ENDIF

            ENDIF

         ELSE

            IF lSelected
               aDarkColor := Darker({_HMG_aControlBkColor[i, 1], _HMG_aControlBkColor[i, 2], _HMG_aControlBkColor[i, 3]}, 97)
            ELSE
               aDarkColor := {_HMG_aControlBkColor[i, 1], _HMG_aControlBkColor[i, 2], _HMG_aControlBkColor[i, 3]}
            ENDIF

            hBrush := hmg_CreateSolidBrush(aDarkColor[1], aDarkColor[2], aDarkColor[3])

            IF lflat

               IF !lfocus .AND. !lSelected .AND. !(_HMG_aControlRangeMax[i][1] == 1)
                  hmg_FillRect(hDC, xp1 + 1, xp2 + 1, yp1 - 1, yp2 - 1, hBrush)
               ELSE
                  hmg_FillRect(hDC, xp1 + 2, xp2 + 2, yp1 - 2, yp2 - 2, hBrush)
               ENDIF

            ELSE

               hmg_FillRect(hDC, xp1 + 2, xp2 + 2, yp1 - 3, yp2 - 3, hBrush)

            ENDIF

            hmg_DeleteObject(hBrush)

         ENDIF

      ENDIF

   ENDIF

   IF !Empty(_HMG_aControlBrushHandle[i])

      SWITCH _HMG_aControlMiscData1[i]
      CASE 0
         aBmp := hmg_GetBitmapSize(_HMG_aControlBrushHandle[i])
         EXIT
      CASE 1
         aBmp := hmg_GetIconSize(_HMG_aControlBrushHandle[i])
      ENDSWITCH

   ENDIF

   IF hb_bitand(_HMG_aControlSpacing[i], OBT_VERTICAL) == OBT_VERTICAL  // vertical text/picture aspect

      y2 := aMetr[1] * nCRLF
      x2 := aBtnRc[3] - 2

      xp2 := iif(!Empty(aBmp), aBmp[1], 0) // picture width
      yp2 := iif(!Empty(aBmp), aBmp[2], 0) // picture height
      xp1 := Round((aBtnRc[3] / 2) - (xp2 / 2), 0)

      IF Empty(nCRLF)
         nFreeSpace := Round((aBtnRc[4] - 4 - (aMetr[4] + yp2)) / 3, 0)
         nCRLF := 1
      ELSE
         nFreeSpace := Round((aBtnRc[4] - 4 - (y2 + yp2)) / 3, 0)
      ENDIF

      IF !Empty(_HMG_aControlCaption[i])  // button has caption

         IF !Empty(_HMG_aControlBrushHandle[i])
            IF !(hb_bitand(_HMG_aControlSpacing[i], OBT_UPTEXT) == OBT_UPTEXT)  // upper text aspect not set
               pozYpic := Max(aBtnRc[2] + nFreeSpace, 5)
               pozYtext := aBtnRc[2] + iif(!Empty(aBmp), nFreeSpace, 0) + yp2 + iif(!Empty(aBmp), nFreeSpace, 0)
            ELSE
               pozYtext := Max(aBtnRc[2] + nFreeSpace, 5)
               aBtnRc[4] := nFreeSpace + ((aMetr[1]) * nCRLF) + nFreeSpace
               pozYpic := aBtnRc[4]
            ENDIF
         ELSE
            pozYpic := 0
            pozYtext := Round((aBtnRc[4] - y2) / 2, 0)
         ENDIF

      ELSE  // button without caption

         IF lnoadjust
            pozYpic := Round(((aBtnRc[4] / 2) - (yp2 / 2)), 0)
            pozYtext := 0
         ELSE  // strech image
            pozYpic := 1
         ENDIF

      ENDIF

      IF !lDisabled

         IF lSelected  // vertical selected

            IF !lXPThemeActive
               xp1 ++
               xPoz := 2
               pozYtext ++
               pozYpic ++
            ELSE
               xPoz := 0
            ENDIF

            IF lnoadjust
               hmg_DrawGlyph(hDC, xp1, pozYpic, xp2, yp2, _HMG_aControlBrushHandle[i], rgbTrans, .F., .F.)
               hmg_DrawText(hDC, _HMG_aControlCaption[i], xPoz, pozYtext - 1, x2, aBtnRc[4], DT_CENTER)
            ELSE
               hmg_DrawGlyph(hDC, aBtnRc[1] + 4, aBtnRc[2] + 4, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], rgbTrans, .F., .T.)
            ENDIF

         ELSE  // vertical non selected

            IF lnoadjust
               hmg_DrawGlyph(hDC, xp1, pozYpic, xp2, yp2, _HMG_aControlBrushHandle[i], rgbTrans, .F., .F.)
               hmg_DrawText(hDC, _HMG_aControlCaption[i], 0, pozYtext - 1, x2, aBtnRc[4], DT_CENTER)
            ELSE
               hmg_DrawGlyph(hDC, aBtnRc[1] + 3, aBtnRc[2] + 3, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], rgbTrans, .F., .T.)
            ENDIF

         ENDIF

      ELSE  // vertical disabled

         IF lnoadjust
            hmg_DrawGlyph(hDC, xp1, pozYpic, xp2, yp2, _HMG_aControlBrushHandle[i], , .T., .F.)
            hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_3DHILIGHT)), waGetGValue(waGetSysColor(COLOR_3DHILIGHT)), waGetBValue(waGetSysColor(COLOR_3DHILIGHT)))
            hmg_DrawText(hDC, _HMG_aControlCaption[i], 2, pozYtext + 1, x2, aBtnRc[4] + 1, DT_CENTER)
            hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_3DSHADOW)), waGetGValue(waGetSysColor(COLOR_3DSHADOW)), waGetBValue(waGetSysColor(COLOR_3DSHADOW)))
            hmg_DrawText(hDC, _HMG_aControlCaption[i], 0, pozYtext, x2, aBtnRc[4], DT_CENTER)
         ELSE
            hmg_DrawGlyph(hDC, aBtnRc[1] + 4, aBtnRc[2] + 4, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], , .T., .T.)
         ENDIF

      ENDIF

   ELSE

      y1 := Round(aBtnRc[4] / 2, 0) - (aMetr[1] - 10)
      y2 := y1 + aMetr[1]
      x2 := aBtnRc[3] - 2

      IF !Empty(_HMG_aControlBrushHandle[i]) // horizontal

         xp2 := iif(!Empty(aBmp), aBmp[1], 0) // picture width
         yp2 := iif(!Empty(aBmp), aBmp[2], 0) // picture height
         yp1 := Round(aBtnRc[4] / 2 - yp2 / 2, 0)

         IF !Empty(_HMG_aControlCaption[i])

            lDrawEntire := (aBtnRc[3] > 109) .AND. (aBtnRc[4] - yp2 > 16)
            nStyle := xp2 / 2 - iif(xp2 > 24, 8, 0)

            IF !(hb_bitand(_HMG_aControlSpacing[i], OBT_LEFTTEXT) == OBT_LEFTTEXT)

               xp1 := 5 + iif(lDrawEntire, nStyle, 0)
               x1 := aBtnRc[1] + xp1 + xp2

            ELSE

               xp1 := aBtnRc[3] - xp2 - 5 - iif(lDrawEntire, nStyle, 0)
               x1 := 3
               x2 := aBtnRc[3] - xp2 - iif(lDrawEntire, xp2 / 2 + 5, 0)

            ENDIF

         ELSE

            xp1 := Round(aBtnRc[3] / 2 - xp2 / 2, 0)
            x1 := aBtnRc[1]

         ENDIF

      ELSE

         xp1 := 2
         xp2 := 0
         yp1 := 0
         yp2 := 0

         x1 := aBtnRc[1] + xp1

      ENDIF

      IF lnoadjust
         y1 := Max(((aBtnRc[4] / 2) - (nCRLF * aMetr[1]) / 2) - 1, 1)
         y2 := (aMetr[1] + aMetr[7]) * nCRLF
      ENDIF

      IF !lDisabled

         IF lSelected

            IF !lXPThemeActive
               x1 += 2
               xp1 ++
               yp1 ++
            ENDIF

            IF lnoadjust
               hmg_DrawGlyph(hDC, xp1, yp1, xp2, yp2, _HMG_aControlBrushHandle[i], rgbTrans, .F., .F.)
               hmg_DrawText(hDC, _HMG_aControlCaption[i], x1, y1 + 1, x2, y1 + y2, DT_CENTER)
            ELSE
               hmg_DrawGlyph(hDC, aBtnRc[1] + 4, aBtnRc[2] + 4, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], rgbTrans, .F., .T.)
            ENDIF

         ELSE

            IF lnoadjust
               hmg_DrawGlyph(hDC, xp1, yp1, xp2, yp2, _HMG_aControlBrushHandle[i], rgbTrans, .F., .F.)
               hmg_DrawText(hDC, _HMG_aControlCaption[i], x1, y1, x2, y1 + y2, DT_CENTER)
            ELSE
               hmg_DrawGlyph(hDC, aBtnRc[1] + 3, aBtnRc[2] + 3, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], rgbTrans, .F., .T.)
            ENDIF

         ENDIF

      ELSE  // disabled horizontal

         IF lnoadjust
            hmg_DrawGlyph(hDC, xp1, yp1, xp2, yp2, _HMG_aControlBrushHandle[i], , .T., .F.)
            hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_3DHILIGHT)), waGetGValue(waGetSysColor(COLOR_3DHILIGHT)), waGetBValue(waGetSysColor(COLOR_3DHILIGHT)))
            hmg_DrawText(hDC, _HMG_aControlCaption[i], x1 + 1, y1 + 1, x2 + 1, y1 + y2 + 1, DT_CENTER)
            hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_3DSHADOW)), waGetGValue(waGetSysColor(COLOR_3DSHADOW)), waGetBValue(waGetSysColor(COLOR_3DSHADOW)))
            hmg_DrawText(hDC, _HMG_aControlCaption[i], x1, y1, x2, y1 + y2, DT_CENTER)
         ELSE
            hmg_DrawGlyph(hDC, aBtnRc[1] + 3, aBtnRc[2] + 3, aBtnRc[3] - 6, aBtnRc[4] - 6, _HMG_aControlBrushHandle[i], , .T., .T.)
         ENDIF

      ENDIF

   ENDIF

   IF (lSelected .OR. lFocus) .AND. !lDisabled .AND. !lXPThemeActive
      hmg_SetTextColor(hDC, waGetRValue(waGetSysColor(COLOR_BTNTEXT)), waGetGValue(waGetSysColor(COLOR_BTNTEXT)), waGetBValue(waGetSysColor(COLOR_BTNTEXT)))
      hmg_DrawFocusRect(pdis)
   ENDIF

   hmg_SelectObject(hDC, hOldFont)
   hmg_SetBkMode(hDC, oldBkMode)
   hmg_SetTextColor(hDC, oldTextColor)

RETURN 1

STATIC FUNCTION ToUnicode(cString)
   
   LOCAL cTemp As String
   LOCAL i

   FOR i := 1 TO Len(cString)
      cTemp += SubStr(cString, i, 1) + Chr(0)
   NEXT
   cTemp += Chr(0)

RETURN cTemp

FUNCTION _SetBtnPictureMask(hWnd, i /*ControlIndex*/)
   
   LOCAL hDC := hmg_GetDC(hWnd)
   LOCAL aBtnRc := Array(4)
   LOCAL aBMP
   LOCAL x
   LOCAL y

   IF Empty(hDC) .OR. Empty(_HMG_aControlBrushHandle[i])
      RETURN NIL
   ENDIF

   aBtnRc[1] := _HMG_aControlRow[i]
   aBtnRc[2] := _HMG_aControlCol[i]
   aBtnRc[3] := _HMG_aControlWidth[i]
   aBtnRc[4] := _HMG_aControlHeight[i]

   aBmp := hmg_GetBitmapSize(_HMG_aControlBrushHandle[i])
   x := aBtnRc[3] / 2 - aBmp[1] / 2
   y := aBtnRc[4] / 2 - aBmp[2] / 2

   hmg_DrawGlyphMask(hDC, x, y, aBmp[1], aBmp[2], _HMG_aControlBrushHandle[i], , .T., .F., hWnd)

   hmg_ReleaseDC(hWnd, hDC)

RETURN NIL

FUNCTION _DestroyBtnPictureMask(hWnd, ControlIndex)
   
   LOCAL MaskHwnd := hmg__GetBtnPictureHandle(hWnd)

   IF !Empty(MaskHwnd) .AND. MaskHwnd != _HMG_aControlBrushHandle[ControlIndex]
      hmg_DeleteObject(MaskHwnd)
   ENDIF

RETURN NIL

FUNCTION _DestroyBtnPicture(hWnd, ControlIndex)
   
   LOCAL BtnPicHwnd := hmg__GetBtnPictureHandle(hWnd)

   IF !Empty(BtnPicHwnd) .AND. BtnPicHwnd == _HMG_aControlBrushHandle[ControlIndex]
      hmg_DeleteObject(BtnPicHwnd)
   ENDIF

RETURN NIL

FUNCTION Darker(aColor, Percent)
   
   LOCAL aDark := Array(3)

   Percent := Percent / 100

   aDark[1] := Round(aColor[1] * Percent, 0)
   aDark[2] := Round(aColor[2] * Percent, 0)
   aDark[3] := Round(aColor[3] * Percent, 0)

RETURN aDark

FUNCTION Lighter(aColor, Percent)
   
   LOCAL aLight := Array(3)
   LOCAL Light

   Percent := Percent / 100
   Light := Round(255 - Percent * 255, 0)

   aLight[1] := Round(aColor[1] * Percent, 0) + Light
   aLight[2] := Round(aColor[2] * Percent, 0) + Light
   aLight[3] := Round(aColor[3] * Percent, 0) + Light

RETURN aLight

STATIC FUNCTION InvertGradInfo(aGradInfo)
   
   LOCAL aGradInvert := {}

   IF !Empty(aGradInfo) .AND. hb_IsArray(aGradInfo)

      AEval(aGradInfo, {|x|AAdd(aGradInvert, {x[1], x[3], x[2]})})

   ENDIF

RETURN aGradInvert

STATIC FUNCTION ModifGradInfo(aGradInfo)
   
   LOCAL aReturn := {}
   LOCAL nClr

   IF !Empty(aGradInfo) .AND. hb_IsArray(aGradInfo)

      FOR nClr := 1 TO Len(aGradInfo)
         ReplaceGradInfo(aGradInfo, nClr, 2)
         ReplaceGradInfo(aGradInfo, nClr, 3)
      NEXT

      AEval(aGradInfo, {|x|AAdd(aReturn, {x[1], x[3], x[2]})})

   ENDIF

RETURN aReturn

//---------------------------------------------------------------------------//
STATIC PROCEDURE ReplaceGradInfo(aGradInfo, nClr, nItem)
//---------------------------------------------------------------------------//
   
   LOCAL aColor

   aColor := aGradInfo[nClr][nItem]
   IF IsArrayRGB(aColor)
      aGradInfo[nClr][nItem] := RGB(aColor[1], aColor[2], aColor[3])
   ENDIF

RETURN

STATIC FUNCTION _GradientFill(hDC, nTop, nLeft, nBottom, nRight, aGradInfo, lVertical)

   LOCAL nClr
   LOCAL nClrs
   LOCAL nSize
   LOCAL nSlice

   IF !Empty(aGradInfo) .AND. hb_IsArray(aGradInfo)

      nClrs := Len(aGradInfo)

      IF lVertical

         nSize := nBottom - nTop + 1

         FOR nClr := 1 TO nClrs

            nSlice = iif(nClr == nClrs, nBottom, Min(nBottom, nTop + nSize * aGradInfo[nClr][1] - 1))

            ReplaceGradInfo(aGradInfo, nClr, 2)
            ReplaceGradInfo(aGradInfo, nClr, 3)

            hmg_FillGradient(hDC, nTop, nLeft, nSlice, nRight, .T., aGradInfo[nClr][2], aGradInfo[nClr][3])

            nTop := nSlice - 1

            IF nTop > nBottom
               EXIT
            ENDIF

         NEXT

      ELSE

         nSize := nRight - nLeft + 1

         FOR nClr := 1 TO nClrs

            nSlice := iif(nClr == nClrs, nRight, Min(nRight, nLeft + nSize * aGradInfo[nClr][1] - 1))

            ReplaceGradInfo(aGradInfo, nClr, 2)
            ReplaceGradInfo(aGradInfo, nClr, 3)

            hmg_FillGradient(hDC, nTop, nLeft, nBottom, nSlice, .F., aGradInfo[nClr][2], aGradInfo[nClr][3])

            nLeft := nSlice - 1

            IF nLeft > nRight
               EXIT
            ENDIF

         NEXT

      ENDIF

   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <shellapi.h>
#include <commctrl.h>
#include <math.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbwinuni.h>

LRESULT CALLBACK OwnButtonProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

/*
HMG_INITOWNERBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> array
*/
HB_FUNC_STATIC(HMG_INITOWNERBUTTON)
{
   HWND  himage;
   HICON hIcon;

   void * WindowName;
   void * ImageName;
   void * IconName;

   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);
   LPCTSTR lpImageName  = HB_PARSTR(8, &ImageName, nullptr);
   LPCTSTR lpIconName   = HB_PARSTR(14, &IconName, nullptr);

   DWORD style = BS_NOTIFY | WS_CHILD | BS_OWNERDRAW | (HB_ISNIL(14) ? BS_BITMAP : BS_ICON) | (hb_parl(13) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON);

   if (hb_parl(9))
   {
      style |= BS_FLAT;
   }

   if (!hb_parl(11))
   {
      style |= WS_VISIBLE;
   }

   if (!hb_parl(12))
   {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(0,
                                 WC_BUTTON,
                                 lpWindowName,
                                 style,
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(6),
                                 hmg_par_int(7),
                                 hmg_par_HWND(1),
                                 hmg_par_HMENU(3),
                                 GetInstance(),
                                 nullptr);

   SetProp(hbutton, TEXT("oldbtnproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hbutton, GWLP_WNDPROC)));
   SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnButtonProc));

   int ImgStyle = hb_parl(10) ? 0 : LR_LOADTRANSPARENT;

   if (HB_ISNIL(14))
   {
      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(15, 0), 0), HB_MAX(hb_parnidef(16, 0), 0), LR_LOADMAP3DCOLORS | ImgStyle));

      if (himage == nullptr)
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(15, 0), 0), HB_MAX(hb_parnidef(16, 0), 0), LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | ImgStyle));
      }

      hb_reta(2);
      hmg_storvhandle(hbutton, -1, 1);
      hmg_storvhandle(himage, -1, 2);
   }
   else
   {
      hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

      if (hIcon == nullptr)
      {
         hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
      }

      if (hIcon == nullptr)
      {
         hIcon = ExtractIcon(GetInstance(), lpIconName, 0);
      }

      hb_reta(2);
      hmg_storvhandle(hbutton, -1, 1);
      hmg_storvhandle(hIcon, -1, 2);
   }

   hb_strfree(WindowName);
   hb_strfree(ImageName);
   hb_strfree(IconName);
}

LRESULT CALLBACK OwnButtonProc(HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   auto OldWndProc = reinterpret_cast<WNDPROC>(GetProp(hButton, TEXT("oldbtnproc")));

   switch( Msg ) {
      case WM_LBUTTONDBLCLK:
         SendMessage(hButton, WM_LBUTTONDOWN, wParam, lParam);
         break;

      case WM_MOUSEMOVE:
      {
         TRACKMOUSEEVENT tme;
         tme.cbSize      = sizeof(TRACKMOUSEEVENT);
         tme.dwFlags     = TME_LEAVE;
         tme.hwndTrack   = hButton;
         tme.dwHoverTime = 0;
         _TrackMouseEvent(&tme);

         if (!pSymbol)
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OBTNEVENTS"));
         }

         if (pSymbol)
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHWND(hButton);
            hmg_vmPushUINT(Msg);
            hmg_vmPushWPARAM(wParam);
            hmg_vmPushLPARAM(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hButton, Msg, wParam, lParam);
      }
      case WM_MOUSELEAVE:
      {
         if (!pSymbol)
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OBTNEVENTS"));
         }

         if (pSymbol)
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHWND(hButton);
            hmg_vmPushUINT(Msg);
            hmg_vmPushWPARAM(wParam);
            hmg_vmPushLPARAM(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hButton, Msg, wParam, lParam);
      }
   }

   return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
}

#pragma ENDDUMP
