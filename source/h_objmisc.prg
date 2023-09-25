/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2017-2022 Aleksandr Belov, Sergej Kiselev <bilance@bilance.lv>
 */

#include "minigui.ch"

*-----------------------------------------------------------------------------*
FUNCTION _WindowCargo(FormName, xValue)
*-----------------------------------------------------------------------------*

#ifdef _OBJECT_
   LOCAL o := iif(hb_IsObject(FormName), FormName, _WindowObj(FormName))
   LOCAL i := iif(hb_IsObject(o), o:Index, GetFormIndex(FormName))
#else
   LOCAL i := GetFormIndex(FormName)
#endif

   IF i > 0
      IF PCount() > 1;        _HMG_aFormMiscData2[i] := xValue
      ELSE           ; RETURN _HMG_aFormMiscData2[i]
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _ControlCargo(ControlName, FormName, xValue)
*-----------------------------------------------------------------------------*

#ifdef _OBJECT_
   LOCAL o := iif(hb_IsObject(ControlName), ControlName, _ControlObj(ControlName, FormName))
   LOCAL i := iif(hb_IsObject(o), o:Index, GetControlIndex(ControlName, FormName))
#else
   LOCAL i := GetControlIndex(ControlName, FormName)
#endif

   IF i > 0
      IF PCount() > 2;        _HMG_aControlMiscData2[i] := xValue
      ELSE           ; RETURN _HMG_aControlMiscData2[i]
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Do_ControlEventProcedure(bBlock, i, p1, p2, p3, p4)
*-----------------------------------------------------------------------------*
   
   LOCAL RetVal

   IF hb_IsBlock(bBlock) .AND. i > 0

      _PushEventInfo()

      _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
      _HMG_ThisType := "C"
      _HMG_ThisIndex := i
      _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
      _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

      RetVal := Eval(bBlock, p1, p2, p3, p4)

      _PopEventInfo()

   ENDIF

RETURN RetVal

*-----------------------------------------------------------------------------*
FUNCTION Do_WindowEventProcedure(bBlock, i, p1, p2, p3, p4)
*-----------------------------------------------------------------------------*
   
   LOCAL RetVal

   IF hb_IsBlock(bBlock) .AND. i > 0

      _PushEventInfo()

      _HMG_ThisFormIndex := i
      _HMG_ThisEventType := ""
      _HMG_ThisType := "W"
      _HMG_ThisIndex := i
      _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
      _HMG_ThisControlName := ""

      RetVal := Eval(bBlock, p1, p2, p3, p4)

      _PopEventInfo()

   ENDIF

RETURN RetVal

#ifdef _OBJECT_

*-----------------------------------------------------------------------------*
FUNCTION _WindowObj(FormName)
*-----------------------------------------------------------------------------*
   
   LOCAL h := iif(hb_IsNumeric(FormName), FormName, GetFormHandle(FormName))

RETURN hmg_GetWindowObject(h)

*-----------------------------------------------------------------------------*
FUNCTION _ControlObj(ControlName, FormName)
*-----------------------------------------------------------------------------*
   
   LOCAL h := iif(hb_IsNumeric(ControlName), ControlName, GetControlHandle(ControlName, FormName))

   IF hb_IsArray(h)
      h := h[1]
   ENDIF

RETURN hmg_GetWindowObject(h)

*-----------------------------------------------------------------------------*
FUNCTION _wPost(nEvent, nIndex, xParam)
*-----------------------------------------------------------------------------*
   
   LOCAL oWnd

   IF hb_IsObject(nIndex)
      IF nIndex:ClassName == "TSBROWSE"
         oWnd   := _WindowObj(nIndex:cParentWnd)
         IF !hb_IsObject(oWnd)
            RETURN NIL
         ENDIF
         nIndex := oWnd:GetObj(nIndex:cControlName):Index
      ELSE
         oWnd   := nIndex
         nIndex := NIL
      ENDIF
   ELSEIF hb_IsChar(nIndex)
      oWnd   := _WindowObj(nIndex)
      nIndex := NIL
   ELSE
      oWnd := _WindowObj(_HMG_THISFORMNAME)
   ENDIF

   oWnd:PostMsg(nEvent, nIndex, xParam)

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _wSend(nEvent, nIndex, xParam)
*-----------------------------------------------------------------------------*
   
   LOCAL oWnd

   IF hb_IsObject(nIndex)
      IF nIndex:ClassName == "TSBROWSE"
         oWnd   := _WindowObj(nIndex:cParentWnd)
         IF !hb_IsObject(oWnd)
            RETURN NIL
         ENDIF
         nIndex := oWnd:GetObj(nIndex:cControlName):Index
      ELSE
         oWnd   := nIndex
         nIndex := NIL
      ENDIF
   ELSEIF hb_IsChar(nIndex)
      oWnd   := _WindowObj(nIndex)
      nIndex := NIL
   ELSE
      oWnd := _WindowObj(_HMG_THISFORMNAME)
   ENDIF

   oWnd:SendMsg(nEvent, nIndex, xParam)

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Do_Obj(nHandle, bBlock, p1, p2, p3)
*-----------------------------------------------------------------------------*
   
   LOCAL o

   IF hmg_IsWindowObject(nHandle)
      o := hmg_GetWindowObject(nHandle)
      IF hb_IsBlock(bBlock)
         IF o:IsWindow 
            RETURN Do_WindowEventProcedure(bBlock, o:Index, o, p1, p2, p3)
         ELSE 
            RETURN Do_ControlEventProcedure(bBlock, o:Index, o, p1, p2, p3)
         ENDIF 
      ENDIF
   ENDIF

RETURN o

*-----------------------------------------------------------------------------*
FUNC Do_OnWndInit(i, cVar)
*-----------------------------------------------------------------------------*
   
   LOCAL nIndex  := i
   LOCAL cName   := _HMG_aFormNames[i]
   LOCAL nHandle := _HMG_aFormHandles[i]
   LOCAL nParent := _HMG_aFormParentHandle[i]
   LOCAL cType   := _HMG_aFormType[i]

RETURN oWndData(nIndex, cName, nHandle, nParent, cType, cVar)

*-----------------------------------------------------------------------------*
FUNC Do_OnWndRelease(i)
*-----------------------------------------------------------------------------*
   
   LOCAL o
   LOCAL hWnd := _HMG_aFormHandles[i]

   IF hmg_IsWindowObject(hWnd)
      o := hmg_GetWindowObject(hWnd)
      IF __objHasMethod(o, "Del")
         o:Del()
      ENDIF
      IF __objHasMethod(o, "Destroy")
         o:Destroy()
      ENDIF
      RETURN .T.
   ENDIF

RETURN .F.

*-----------------------------------------------------------------------------*
FUNC Do_OnCtlInit(i, cVar)
*-----------------------------------------------------------------------------*
   
   LOCAL nCtlIndex := i
   LOCAL cCtlName  := _HMG_aControlNames[i]
   LOCAL nHandle   := iif(hb_IsArray(_HMG_aControlHandles[i]), _HMG_aControlHandles[i][1], _HMG_aControlHandles[i])
   LOCAL nParent   := _HMG_aControlParentHandles[i]
   LOCAL cFormName := GetParentFormName(i)
   LOCAL cCtlType  := iif(Empty(cFormName), _HMG_aControlType[i], GetProperty(cFormName, cCtlName, "Type"))

RETURN oCnlData(nCtlIndex, cCtlName, nHandle, nParent, cCtlType, cVar)

*-----------------------------------------------------------------------------*
FUNC Do_OnCtlRelease(i)
*-----------------------------------------------------------------------------*
   
   LOCAL o
   LOCAL hWnd := _HMG_aControlHandles[i]

   IF hmg_IsWindowObject(hWnd)
      o := hmg_GetWindowObject(hWnd)
      IF __objHasMethod(o, "Del")
         o:Del()
      ENDIF
      IF __objHasMethod(o, "Destroy")
         o:Destroy()
      ENDIF
      RETURN .T.
   ENDIF

RETURN .F.

*-----------------------------------------------------------------------------*
FUNC Do_OnWndLaunch(hWnd, nMsg, wParam, lParam)
*-----------------------------------------------------------------------------*
   IF hmg_IsWindowObject(hWnd)
      hmg_GetWindowObject(hWnd):DoEvent(wParam, lParam)
   ENDIF

   HB_SYMBOL_UNUSED(nMsg)

RETURN NIL

*-----------------------------------------------------------------------------*
FUNC Do_OnCtlLaunch(hWnd, nMsg, wParam, lParam)
*-----------------------------------------------------------------------------*
   HB_SYMBOL_UNUSED(nMsg)

   IF !Empty(lParam)
      hWnd := lParam
   ENDIF

   IF hmg_IsWindowObject(hWnd)
      hmg_GetWindowObject(hWnd):DoEvent(wParam, lParam)
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <hbapiitm.hpp>
#include <commctrl.h>

HB_FUNC( HMG_SETWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) ) {
      pObject = ( PHB_ITEM ) hb_param(2, Harbour::Item::OBJECT);

      if( pObject && HB_IS_OBJECT(pObject) ) {
         pObject = hb_itemNew(pObject);

         hb_gcLock(pObject);    // Ref++

         SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LPARAM>(pObject));

         hb_retl(true);
      } else {
         hb_retl(false);
      }
   } else {
      hb_retl(false);
   }
}

HB_FUNC( HMG_DELWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) ) {
      pObject = ( PHB_ITEM ) GetWindowLongPtr(hWnd, GWLP_USERDATA);

      SetWindowLongPtr(hWnd, GWLP_USERDATA, 0);

      if( pObject && HB_IS_OBJECT(pObject) ) {
         hb_gcUnlock(pObject);     // Ref --
         hb_itemRelease(pObject);
      }
   }
}

HB_FUNC( HMG_GETWINDOWOBJECT )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) ) {
      hb_itemReturn((PHB_ITEM) GetWindowLongPtr(hWnd, GWLP_USERDATA));
   } else {
      hb_ret();
   }
}

HB_FUNC( HMG_ISWINDOWOBJECT )
{
   PHB_ITEM pObject;

   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) ) {
      pObject = ( PHB_ITEM ) GetWindowLongPtr(hWnd, GWLP_USERDATA);

      hb_retl(pObject && HB_IS_OBJECT(pObject));
   } else {
      hb_retl(false);
   }
}

#pragma ENDDUMP

#endif
