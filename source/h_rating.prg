/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2014-2022 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

*-----------------------------------------------------------------------------*
FUNCTION _DefineRating ( ControlName, ParentForm, x, y, w, h, nValue, aImages, nCnt, ;
      nSpace, tooltip, onchangeprocedure, border, resource, readonly, invisible, vertical )
*-----------------------------------------------------------------------------*
   
   LOCAL cParentForm
   LOCAL mVar
   LOCAL ControlHandle
   LOCAL k

   DEFAULT h TO 20
   DEFAULT w TO 100
   DEFAULT nValue TO 0
   DEFAULT aImages := { "empty.png", "full.png" }
   DEFAULT nCnt TO 5
   DEFAULT border TO .F.
   DEFAULT readonly TO .F.
   DEFAULT invisible TO .F.
   DEFAULT vertical TO .F.

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentForm := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   IF !_IsWindowDefined(ParentForm)
      MsgMiniGuiError("Window: " + ParentForm + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentForm)
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentForm + " Already defined.")
   ENDIF

   IF resource
      aImages [1] := "_empty"
      aImages [2] := "_full"
   ENDIF

   mVar := "_" + ParentForm + "_" + ControlName
   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   Public &mVar. := k
#endif
   cParentForm := ParentForm

   ParentForm := GetFormHandle(ParentForm)

   ControlHandle := _InitRating ( cParentForm, ControlName, x, y, w, h, nValue, aImages, nCnt, nSpace, tooltip, onchangeprocedure, border, readonly, invisible, vertical )

   IF _HMG_BeginTabActive
      AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
   ENDIF

   _HMG_aControlType               [k] := CONTROL_TYPE_RATING
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentForm
   _HMG_aControlIds                [k] := nCnt
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := nValue
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := ""
   _HMG_aControlGotFocusProcedure  [k] := ""
   _HMG_aControlChangeProcedure    [k] := onchangeprocedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := {}
   _HMG_aControlFontColor          [k] := {}
   _HMG_aControlDblClick           [k] := ""
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := ""
   _HMG_aControlFontSize           [k] := 0
   _HMG_aControlFontAttributes     [k] := { .F., .F., .F., .F. }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := 0
   _HMG_aControlFontHandle         [k] := HMG_NULLHANDLE
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := 0
   _HMG_aControlMiscData2          [k] := ""

RETURN NIL

*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseRating ( cWindow, cControl )
*-----------------------------------------------------------------------------*

   LOCAL i
   LOCAL img_name

   IF _IsControlDefined(cControl, cWindow)

      FOR i := 1 TO GetControlId(cControl, cWindow)
         img_name := cWindow + "_" + cControl + "_" + hb_ntos(i)
         DoMethod(cWindow, img_name, "Release")
      NEXT

      EraseWindow(cWindow)

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _InitRating ( ParentForm, ControlName, x, y, w, h, nValue, aImages, nCnt, ;
      nSpace, tooltip, onchangeprocedure, border, readonly, invisible, vertical )
*-----------------------------------------------------------------------------*
   
   LOCAL i
   LOCAL img_name
   LOCAL col := x
   LOCAL row := y

   DEFAULT nSpace := 0

   FOR i := 1 TO nCnt
      img_name := ParentForm + "_" + ControlName + "_" + hb_ntos(i)
      DEFINE IMAGE &img_name
         PARENT &ParentForm
         ROW y
         COL col
         WIDTH w
         HEIGHT h
         PICTURE aImages [1]
         TOOLTIP tooltip
         ONMOUSEHOVER iif(readonly, NIL, OnHoverRate(ParentForm, ControlName))
         ONMOUSELEAVE iif(readonly, NIL, OnLeaveRate(ParentForm, ControlName, onchangeprocedure))
         ONCLICK iif(readonly, NIL, ( SetProperty( ParentForm, ControlName, "Value", ;
            Val(SubStr(This.NAME, RAt("_", This.Name) + 1)) ), ;
            OnSelectRate(ParentForm, ControlName, onchangeprocedure) ))
         INVISIBLE invisible
      END IMAGE
      _HMG_aControlIds[ GetControlIndex(img_name, ParentForm) ] := nCnt
      _HMG_aControlMiscData2[ GetControlIndex(img_name, ParentForm) ] := aImages
      _HMG_aControlChangeProcedure[ GetControlIndex(img_name, ParentForm) ] := onchangeprocedure
      IF vertical
         y += h + nSpace
      ELSE
         col += w + nSpace
      ENDIF
      IF nValue > 0
         _HMG_aControlValue[ GetControlIndex(img_name, ParentForm) ] := nValue
      ENDIF
   NEXT

   IF border
      IF vertical
         DRAW RECTANGLE ;
            IN WINDOW Win_1 ;
            AT row - 1, x - 1 ;
            TO y - nSpace + 1, col + w + 1 ;
            PENCOLOR { 192, 192, 192 }
      ELSE
         DRAW RECTANGLE ;
            IN WINDOW Win_1 ;
            AT row - 1, x - 1 ;
            TO row + h + 1, col - nSpace + 1 ;
            PENCOLOR { 192, 192, 192 }
      ENDIF
   ENDIF

   IF nValue > 0
      OnLeaveRate(ParentForm, ControlName, onchangeprocedure)
   ENDIF

RETURN _GetId()


STATIC FUNCTION OnHoverRate(cWindow, cControl)

   LOCAL i
   LOCAL img_name
   LOCAL select := Val(SubStr(This.Name, RAt("_", This.Name) + 1 ))

   ClearRating( cWindow, cControl )
   FOR i := 1 TO select
      img_name := cWindow + "_" + cControl + "_" + hb_ntos(i)
      SetProperty( cWindow, img_name, "Picture", GetProperty( cWindow, img_name, "Cargo" ) [2] )
   NEXT

RETURN NIL


STATIC FUNCTION OnLeaveRate(cWindow, cControl, onchange)

   LOCAL pressed := GetProperty( cWindow, cControl, "Value" )

   IF pressed == 0
      ClearRating( cWindow, cControl )
      IF hb_IsBlock(onchange)
         Eval(onchange, pressed)
      ENDIF
   ELSE
      OnSelectRate(cWindow, cControl, onchange)
   ENDIF

RETURN NIL


STATIC FUNCTION OnSelectRate(cWindow, cControl, onchange)

   LOCAL i
   LOCAL img_name
   LOCAL pressed := GetProperty( cWindow, cControl, "Value" )

   IF pressed > 0
      ClearRating( cWindow, cControl )
      FOR i := 1 TO pressed
         img_name := cWindow + "_" + cControl + "_" + hb_ntos(i)
         SetProperty( cWindow, img_name, "Picture", GetProperty( cWindow, img_name, "Cargo" ) [2] )
      NEXT
      IF hb_IsBlock(onchange)
         Eval(onchange, pressed)
      ENDIF
   ENDIF

RETURN NIL


FUNCTION ClearRating( cWindow, cControl )

   LOCAL i
   LOCAL img_name
   LOCAL nCount := GetControlId(cControl, cWindow)

   FOR i := 1 TO nCount
      img_name := cWindow + "_" + cControl + "_" + hb_ntos(i)
      SetProperty( cWindow, img_name, "Picture", GetProperty( cWindow, img_name, "Cargo" ) [1] )
   NEXT

RETURN NIL


FUNCTION RefreshRating( ParentForm, ControlName )

   LOCAL onchangeprocedure := _GetControlAction( ControlName, ParentForm, "ONCHANGE" )

RETURN OnLeaveRate(ParentForm, ControlName, onchangeprocedure)
