#include "minigui.ch"
#include <hbclass.ch>
#include "TSBrowse.ch"

#define DTS_UPDOWN          0x0001 // use UPDOWN instead of MONTHCAL
#define DTS_SHOWNONE        0x0002 // allow a NONE selection
#define NM_KILLFOCUS   (-8)

* ============================================================================
* CLASS TDatePicker  Driver for DatePicker  TSBrowse 7.0
* ============================================================================

CLASS TDatePicker FROM TControl

   CLASSDATA lRegistered AS LOGICAL
   DATA Atx, lAppend

   METHOD New(nRow, nCol, bSetGet, oWnd, nWidth, nHeight, cPict, bValid,;
      nClrFore, nClrBack, hFont, cControl, oCursor, cWnd, cMsg,;
      lUpdate, bWhen, lCenter, lRight, bChanged,;
      lNoBorder, nHelpId, shownone, updown)

   METHOD Default()
   METHOD HandleEvent(nMsg, nWParam, nLParam)
   METHOD KeyChar(nKey, nFlags)
   METHOD KeyDown(nKey, nFlags)
   METHOD LostFocus()
   METHOD lValid()
   METHOD VarGet()

ENDCLASS

* ============================================================================
* METHOD TDatePicker:New() Version 7.0
* ============================================================================

METHOD TDatePicker:New(nRow, nCol, bSetGet, oWnd, nWidth, nHeight, cPict, bValid,;
           nClrFore, nClrBack, hFont, cControl, oCursor, cWnd, cMsg,;
           lUpdate, bWhen, lCenter, lRight, bChanged,;
           lNoBorder, nHelpId, shownone, updown)

   Local invisible   := .F.
   Local rightalign  := .F.
   Local notabstop   := .F.

   DEFAULT nClrFore  := waGetSysColor(COLOR_WINDOWTEXT),;
           nClrBack  := waGetSysColor(COLOR_WINDOW),;
           nHeight   := 12 ,;
           lUpdate   := .F.,;
           lNoBorder := .F.,;
           shownone  := .F.,;
           updown    := .F.

   HB_SYMBOL_UNUSED(cPict)
   HB_SYMBOL_UNUSED(lCenter)
   HB_SYMBOL_UNUSED(lRight)

   ::nTop         := nRow
   ::nLeft        := nCol
   ::nBottom      := ::nTop + nHeight - 1
   ::nRight       := ::nLeft + nWidth - 1
   if oWnd == NIL
       oWnd := Self
       oWnd:hWnd  := GetFormHandle(cWnd)           //JP
   endif
   ::oWnd         := oWnd

   ::nId          := ::GetNewId()

   ::cControlName := cControl
   ::cParentWnd   := cWnd
   ::nStyle       := nOR(WS_CHILD, WS_VISIBLE, WS_TABSTOP, ;
                         WS_VSCROLL, WS_BORDER, ;
                         iif(updown, DTS_UPDOWN, 0), ;
                         iif(shownone, DTS_SHOWNONE, 0))

   ::bSetGet      := bSetGet
   ::bValid       := bValid
   ::lCaptured    := .F.
   ::hFont        := hFont
   ::oCursor      := oCursor
   ::cMsg         := cMsg
   ::lUpdate      := lUpdate
   ::bWhen        := bWhen
   ::bChange      := bChanged
   ::lFocused     := .F.
   ::nHelpId      := nHelpId
   ::cCaption     := "DateTime"
   ::nLastKey     := 0
   ::Atx          := 0

   ::SetColor(nClrFore, nClrBack)

   if oWnd == NIL
       oWnd := GetFormHandle(cWnd)                 //JP
   endif

   if !Empty(::oWnd:hWnd)

      ::hWnd := hmg_InitDatePick(::oWnd:hWnd, 0, nCol, nRow, nWidth, nHeight, "", 0, shownone, updown, rightalign, invisible, notabstop)

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
* METHOD TDatePicker:Default()
* ============================================================================

METHOD TDatePicker:Default()

   Local Value

   Value := Eval(::bSetGet)
   If Empty(Value)
      hmg_SetDatePickNull(::hWnd)
   Else
      hmg_SetDatePick(::hWnd, Year(value), Month(value), Day(value))
   EndIf

Return NIL

* ============================================================================
* METHOD TDatePicker:HandleEvent()
* ============================================================================

METHOD TDatePicker:HandleEvent(nMsg, nWParam, nLParam)

   If nMsg == WM_NOTIFY
      If hmg_HiWord(nWParam) == NM_KILLFOCUS
         ::LostFocus()
      EndIf
   EndIf

Return ::Super:HandleEvent(nMsg, nWParam, nLParam)

* ============================================================================
* METHOD TDatePicker:KeyChar() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TDatePicker:KeyChar(nKey, nFlags)

   If _GetKeyState(VK_CONTROL)
      nKey := IIf(Upper(Chr(nKey)) == "W" .OR. nKey == VK_RETURN, VK_TAB, nKey)
   EndIf

   If nKey == VK_TAB .OR. nKey == VK_ESCAPE
      Return 0
   Endif

RETURN ::Super:KeyChar(nKey, nFlags)

* ============================================================================
* METHOD TDatePicker:KeyDown()
* ============================================================================

METHOD TDatePicker:KeyDown(nKey, nFlags)

   ::nLastKey := nKey

   If nKey == VK_TAB .OR. nKey == VK_RETURN .OR. nKey == VK_ESCAPE
      ::bLostFocus := NIL
      Eval(::bKeyDown, nKey, nFlags, .T.)
   Endif

RETURN 0

* ============================================================================
* METHOD TDatePicker:lValid() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TDatePicker:lValid()

   Local lRet := .T.

   If hb_IsBlock(::bValid)
      lRet := Eval(::bValid, ::GetText())
   EndIf

Return lRet

* ============================================================================
* METHOD TDatePicker:VarGet() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TDatePicker:VarGet()

RETURN hb_Date(hmg_GetDatePickYear(::hWnd), hmg_GetDatePickMonth(::hWnd), hmg_GetDatePickDay(::hWnd))

* ============================================================================
* METHOD TDatePicker:LostFocus() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TDatePicker:LostFocus()

   Default ::lAppend := .F.

   If ::nLastKey == NIL .AND. ::lAppend
      ::SetFocus()
      ::nLastKey := 0
      Return 0
   EndIf

   ::lFocused := .F.

   If ::bLostFocus != NIL
      Eval(::bLostFocus, ::nLastKey)
   EndIf

Return 0
