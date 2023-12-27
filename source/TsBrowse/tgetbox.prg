#include "minigui.ch"
#include <hbclass.ch>
#include "TSBrowse.ch"

* ============================================================================
* CLASS TGetBox  Driver for GetBox  TSBrowse 7.0
* ============================================================================

CLASS TGetBox FROM TControl

   DATA Atx, lAppend, oGet

   METHOD New(nRow, nCol, bSetGet, oWnd, nWidth, nHeight, cPict, bValid,;
              nClrFore, nClrBack, hFont, cControl, cWnd, cMsg,;
              lUpdate, bWhen, lCenter, lRight, bChanged,;
              lNoBorder, nHelpId, lSpinner, bUp, bDown, bMin, bMax, lNoMinus)
   METHOD HandleEvent(nMsg, nWParam, nLParam)
   Method KeyChar(nKey, nFlags)
   Method KeyDown(nKey, nFlags)
   Method LostFocus(hCtlFocus)
   Method lValid()
   METHOD VarGet()

   ACCESS Index  INLINE ::Atx
   ACCESS Handle INLINE ::hWnd

ENDCLASS

* ============================================================================
* METHOD TGetBox:New() Version 7.0
* ============================================================================

METHOD TGetBox:New(nRow, nCol, bSetGet, oWnd, nWidth, nHeight, cPict, bValid,;
           nClrFore, nClrBack, hFont, cControl, cWnd, cMsg,;
           lUpdate, bWhen, lCenter, lRight, bChanged,;
           lNoBorder, nHelpId, lSpinner, bUp, bDown, bMin, bMax, lNoMinus)

   LOCAL cText          := Space(50), uValue, ix
   LOCAL Fontname       := _HMG_DefaultFontName
   LOCAL FontSize       := _HMG_DefaultFontSize
   LOCAL ParentFormName
   LOCAL invisible      := .F.
   LOCAL uLostFocus, uGotFocus, uChange := "", Right := .F.,;
         bold           := .F., italic := .F., underline := .F., strikeout := .F., field,;
         notabstop      := .F., nId, cvalidmessage := "", tooltip := ""
   LOCAL aFontColor
   LOCAL aBackColor
   LOCAL ReadOnly       := .F., lPassword := .F.

   DEFAULT nClrFore  := waGetSysColor(COLOR_WINDOWTEXT), ;
           nClrBack  := waGetSysColor(COLOR_WINDOW), ;
           lUpdate   := .F., ;
           lCenter   := .F., ;
           lRight    := .F., ;
           lSpinner  := .F., ;
           lNoBorder := .F., ;
           bSetGet   := bSETGET(cText)

   HB_SYMBOL_UNUSED(bUp)
   HB_SYMBOL_UNUSED(bDown)
   HB_SYMBOL_UNUSED(bMin)
   HB_SYMBOL_UNUSED(bMax)

   ::nTop          := nRow
   ::nLeft         := nCol
   ::nBottom       := ::nTop + nHeight - 1
   ::nRight        := ::nLeft + nWidth - 1
   
   if oWnd == NIL
      oWnd         := Self
      oWnd:hWnd    := GetFormHandle(cWnd)
   endif
   
   ::oWnd          := oWnd
   ::nId           := ::GetNewId()
   ::cControlName  := cControl
   ::cParentWnd    := cWnd
   ::bSetGet       := bSetGet
   ::bValid        := bValid
   ::lCaptured     := .F.
   ::hFont         := hFont
   ::cMsg          := cMsg
   ::lUpdate       := lUpdate
   ::bWhen         := bWhen
   ::bChange       := bChanged
   ::lFocused      := .F.
   ::nHelpId       := nHelpId

   ::SetColor(nClrFore, nClrBack)

   nId             := ::nId
   ParentFormName  := oWnd:cParentWnd

   uValue          := Eval(bSetGet)
   aFontColor      := { hmg_GetRed(nClrFore), hmg_GetGreen(nClrFore), hmg_GetBlue(nClrFore) }
   aBackColor      := { hmg_GetRed(nClrBack), hmg_GetGreen(nClrBack), hmg_GetBlue(nClrBack) }
   uLostFocus      := ::LostFocus()
   uGotFocus       := ::GotFocus()

   if hb_IsBlock(cPict) 
      cPict := Eval(cPict)
   endif

   if !Empty(::oWnd:hWnd)

      ::oGet := _DefineGetBox(cControl, ParentFormName, nCol, nRow, nWidth, nHeight, uValue, ;
         FontName, FontSize, ToolTip, lPassword, uLostFocus, uGotFocus, uChange, right, ;
         nHelpId, readonly, bold, italic, underline, strikeout, field, aBackColor, aFontColor, ;
         invisible, notabstop, nId, bvalid, cPict, cMsg, cvalidmessage, bWhen ,,,,, lNoMinus)

      ix     := GetControlIndex(cControl, ParentFormName)
      ::Atx  := ix
      ::hWnd :=_HMG_aControlHandles[ix]

      ::AddVars(::hWnd)

      if hmg_GetObjectType(hFont) == OBJ_FONT
         hmg__SetFontHandle(::hWnd, hFont)
         ::hFont := hFont
      endif

      oWnd:AddControl(::hWnd)

   endif

return Self

* ============================================================================
* METHOD TGetBox:HandleEvent() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:HandleEvent(nMsg, nWParam, nLParam)

Return ::Super:HandleEvent(nMsg, nWParam, nLParam)

* ============================================================================
* METHOD TGetBox:KeyChar() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:KeyChar(nKey, nFlags)

   If _GetKeyState(VK_CONTROL)
      nKey := IIf(Upper(Chr(nKey)) == "W" .OR. nKey == VK_RETURN, VK_TAB, nKey)
   EndIf

   If nKey == VK_TAB .OR. nKey == VK_ESCAPE
      Return 0
   Endif

RETURN ::Super:KeyChar(nKey, nFlags)

* ============================================================================
* METHOD TGetBox:KeyDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:KeyDown(nKey, nFlags)

   ::nLastKey := nKey

   If nKey == VK_TAB .OR. nKey == VK_RETURN .OR. nKey == VK_ESCAPE
      ::bLostFocus := NIL
      Eval(::bKeyDown, nKey, nFlags, .T.)
   Endif

RETURN 0

* ============================================================================
* METHOD TGetBox:lValid() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:lValid()

   Local lRet := .T.

   If hb_IsBlock(::bValid)
      lRet := Eval(::bValid, ::GetText())
   EndIf

Return lRet

* ============================================================================
* METHOD TGetBox:LostFocus() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:LostFocus(hCtlFocus)

   ::lFocused := .F.

   If ::bLostFocus != NIL
      Eval(::bLostFocus, ::nLastKey, hCtlFocus)
   EndIf

Return 0

* ============================================================================
* METHOD TGetBox:VarGet() Version 7.0 Jul/15/2004
* ============================================================================

METHOD TGetBox:VarGet()

RETURN _GetValue(::cControlName, ::oWnd:cParentWnd)
