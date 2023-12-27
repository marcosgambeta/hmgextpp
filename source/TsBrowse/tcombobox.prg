#include "minigui.ch"
#include <hbclass.ch>
#include "TSBrowse.ch"


#define CBS_NOINTEGRALHEIGHT  0x0400

#define COMBO_BASE       320

#define CB_SETEDITSEL    ( COMBO_BASE +  2 )
#define CB_SHOWDROPDOWN  ( COMBO_BASE + 15 )
#define CB_ERR           -1
#define CBN_CLOSEUP      8


* ============================================================================
* CLASS TComboBox  Driver for ComboBox  TSBrowse 7.0
* ============================================================================

CLASS TComboBox FROM TControl

   CLASSDATA lRegistered AS LOGICAL
   
   DATA Atx
   DATA lAppend
   DATA nAt
   DATA aItems   AS ARRAY                     // Combo array
   DATA bCloseUp                              // Block to be evaluated on Close Combo

   METHOD New(nRow, nCol, bSetGet, aGetData, nWidth, nHeight, oWnd, bChanged,;
             nClrFore, nClrBack, hFont, cMsg, cControl, cWnd)
   METHOD Default()
   METHOD GetDlgCode(nLastKey, nFlags)
   METHOD HandleEvent(nMsg, nWParam, nLParam)
   METHOD KeyDown(nKey, nFlags)
   METHOD KeyChar(nKey, nFlags)
   METHOD LButtonDown(nRow, nCol)
   METHOD LostFocus()

ENDCLASS

* ============================================================================
* METHOD TComboBox:New() Version 7.0
* ============================================================================

METHOD TComboBox:New(nRow, nCol, bSetGet, aGetData, nWidth, nHeight, oWnd, bChanged,;
           nClrFore, nClrBack, hFont, cMsg, cControl, cWnd)

   LOCAL invisible     := .F.
   LOCAL sort          := .F.
   LOCAL displaychange := .F.
   LOCAL notabstop     := .F.
   LOCAL ParentHandle

   DEFAULT nClrFore  := waGetSysColor(COLOR_WINDOWTEXT),;
           nClrBack  := waGetSysColor(COLOR_WINDOW),;
           nHeight   := 12

   ::nTop         := nRow
   ::nLeft        := nCol
   ::nBottom      := ::nTop + nHeight - 1
   ::nRight       := ::nLeft + nWidth - 1
   
   if oWnd == NIL
       oWnd       := Self
       oWnd:hWnd  := GetFormHandle(cWnd)                  //JP
   endif
   
   ::oWnd         := oWnd
   ParentHandle   := oWnd:hWnd
   
   if _HMG_BeginWindowMDIActive
        ParentHandle :=  GetActiveMdiHandle()
        cWnd         := _GetWindowProperty(ParentHandle, "PROP_FORMNAME")
   endif
   
   ::nId          := ::GetNewId()
   ::cControlName := cControl
   ::cParentWnd   := cWnd
   ::nStyle       := nOR(WS_CHILD, WS_VISIBLE, WS_TABSTOP, WS_VSCROLL, WS_BORDER, CBS_DROPDOWN, CBS_NOINTEGRALHEIGHT)

   ::bSetGet      := bSetGet
   ::aItems       := aGetData
   ::lCaptured    := .F.
   ::hFont        := hFont
   ::cMsg         := cMsg
   ::bChange      := bChanged
   ::lFocused     := .F.
   ::lAppend      := .F.
   ::nLastKey     := 0
   ::Atx          := 0

   ::SetColor(nClrFore, nClrBack)

   if !Empty(ParentHandle)

      ::hWnd := InitComboBox(ParentHandle, 0, nCol, nRow, nWidth, "", 0, nHeight, invisible, notabstop, sort, displaychange, _HMG_IsXP)

      ::AddVars(::hWnd)
      ::Default()

      if hmg_GetObjectType(hFont) == OBJ_FONT
         hmg__SetFontHandle(::hWnd, hFont)
         ::hFont := hFont
      endif

      oWnd:AddControl(::hWnd)

   endif

return Self

* ============================================================================
* METHOD TComboBox:Default() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:Default()

   LOCAL i

   For i = 1 To Len(::aItems)
      hmg_ComboAddString(::hWnd, ::aItems[i])
      If i == Eval(::bSetGet)
         ComboSetCurSel(::hWnd, i)
      Endif
   Next

Return NIL

* ============================================================================
* METHOD TComboBox:GetDlgCode() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:GetDlgCode(nLastKey, nFlags)

   HB_SYMBOL_UNUSED(nFlags)
   ::nLastKey := nLastKey

Return DLGC_WANTALLKEYS

* ============================================================================
* METHOD TComboBox:HandleEvent() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:HandleEvent(nMsg, nWParam, nLParam)

   If hmg_HiWord(nWParam) == CBN_CLOSEUP
      if ::bCloseUp != NIL
         IIf(hb_IsBlock(::bCloseUp), Eval(::bCloseUp, Self), ::bCloseUp(Self))
         Return 0
      endif
   Endif

Return ::Super:HandleEvent(nMsg, nWParam, nLParam)

* ============================================================================
* METHOD TComboBox:KeyDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:KeyDown(nKey, nFlags)

   LOCAL nAt := ::SendMsg(CB_GETCURSEL)

   If nAt != CB_ERR
      ::nAt := nAt + 1
   Endif

   ::nLastKey := nKey
   If nKey == VK_TAB .OR. nKey == VK_RETURN .OR. nKey == VK_ESCAPE
      ::bLostFocus := NIL
      Eval(::bKeyDown, nKey, nFlags, .T.)
      Return 0
   Endif

Return ::Super:KeyDown(nKey, nFlags)

* ============================================================================
* METHOD TComboBox:LostFocus() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:LostFocus()

   LOCAL nAt

   DEFAULT ::lAppend := .F.

   If ::nLastKey == NIL .AND. ::lAppend
      ::SetFocus()
      ::nLastKey := 0
      Return 0
   EndIf

   nAt := ::SendMsg(CB_GETCURSEL)

   If nAt != CB_ERR
      ::nAt = nAt + 1
      If hb_IsNumeric(Eval(::bSetGet))
         Eval(::bSetGet, nAt + 1)
      Else
         Eval(::bSetGet, ::aItems[nAt + 1])
      Endif
   Else
      Eval(::bSetGet, hmg_GetWindowText(::hWnd))
   Endif

   ::lFocused := .F.

   If ::bLostFocus != NIL
      Eval(::bLostFocus, ::nLastKey)
   EndIf

Return 0

* ============================================================================
* METHOD TComboBox:KeyChar() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:KeyChar(nKey, nFlags)

   SWITCH nKey

   CASE VK_TAB
      Return 0            // We don't want API default behavior

   CASE VK_ESCAPE
      Return 0

   CASE VK_RETURN
      Return 0

   OTHERWISE
      Return ::Super:Keychar(nKey, nFlags)

   ENDSWITCH

Return 0

* ============================================================================
* METHOD TComboBox:LButtonDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TComboBox:LButtonDown(nRow, nCol)

   LOCAL nShow := 1
   
   HB_SYMBOL_UNUSED(nRow)
   HB_SYMBOL_UNUSED(nCol)

   If ::nLastKey != NIL .AND. ::nLastKey == 9999
      nShow := 0
      ::nLastKey := 0
   Else
      ::nLastKey := 9999
   EndIf

   ::PostMsg(CB_SHOWDROPDOWN, nShow, 0)

Return 0
