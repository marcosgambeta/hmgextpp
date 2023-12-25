/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 */

#if (__HARBOUR__ - 0 > 0x030000)

#include <hbclass.ch>
#include "TaskDlgs.ch"
#include "i_var.ch"

////////////////////////////////////////////////////////////////////////////////
CREATE CLASS TSimpleTaskDialog FUNCTION SimpleTaskDialog
////////////////////////////////////////////////////////////////////////////////

   EXPORTED:
   VAR    Cargo
   VAR    lError                       READONLY   INIT .T.
   VAR    nButtonResult                READONLY   INIT NIL
   VAR    nResult                      READONLY   INIT E_FAIL

   METHOD New(cTitle, cInstruction, cContent, nCommonButtons, nMainIcon)
   METHOD Execute()

   METHOD Title(cTitle)              SETGET
   METHOD Instruction(cInstruction)  SETGET
   METHOD Content(cContent)          SETGET
   METHOD CommonButtons(nCBs)        SETGET
   METHOD MainIcon(nIcon)            SETGET

   PROTECTED:
   VAR    cTitle                       INIT       NIL
   VAR    cInstruction                 INIT       NIL
   VAR    cContent                     INIT       NIL
   VAR    nCommonButtons               INIT       TDCBF_OK_BUTTON
   VAR    nMainIcon                    INIT       TD_NO_ICON

ENDCLASS
////////////////////////////////////////////////////////////////////////////////

METHOD TSimpleTaskDialog:New(cTitle, cInstruction, cContent, nCommonButtons, nMainIcon)

   ::cTitle       := iif(hb_IsNumeric(cTitle), cTitle, iif(!hb_IsString(cTitle), NIL, iif(HB_ISNULL(cTitle), NIL, cTitle)))
   ::cInstruction := iif(hb_IsNumeric(cInstruction), cInstruction, iif(!hb_IsString(cInstruction), NIL, iif(HB_ISNULL(cInstruction), NIL, cInstruction)))
   ::cContent     := iif(hb_IsNumeric(cContent), cContent, iif(!hb_IsString(cContent), NIL, iif(HB_ISNULL(cContent), NIL, cContent)))

   IF hb_IsNumeric(nCommonButtons)
      ::nCommonButtons := nCommonButtons
   ENDIF

   IF hb_IsNumeric(nMainIcon)
      ::nMainIcon := nMainIcon
   ENDIF

RETURN Self

METHOD TSimpleTaskDialog:Execute()

   LOCAL nResult
   LOCAL nButton // := NIL

   ::lError        := .T.
   ::nButtonResult := NIL
   ::nResult       := E_FAIL

   IF os_IsWinVista_Or_Later()
      nResult := hmg_win_TaskDialog0( ,, ::cTitle, ::cInstruction, ::cContent, ::nCommonButtons, ::nMainIcon, @nButton )
   ELSE
      nResult := E_NOTIMPL // Not implemented yet
   ENDIF

   ::lError        := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nResult       := nResult

RETURN !::lError

METHOD TSimpleTaskDialog:Title(cTitle)

   LOCAL cOldVal := ::cTitle

   IF hb_IsString(cTitle) .OR. hb_IsNumeric(cTitle)
      ::cTitle := iif(hb_IsString(cTitle) .AND. HB_ISNULL(cTitle), NIL, cTitle)
   ENDIF

RETURN cOldVal

METHOD TSimpleTaskDialog:Instruction(cInstruction)

   LOCAL cOldVal := ::cInstruction

   IF hb_IsString(cInstruction) .OR. hb_IsNumeric(cInstruction)
      ::cInstruction := iif(hb_IsString(cInstruction) .AND. HB_ISNULL(cInstruction), NIL, cInstruction)
   ENDIF

RETURN cOldVal

METHOD TSimpleTaskDialog:Content(cContent)

   LOCAL cOldVal := ::cContent

   IF hb_IsString(cContent) .OR. hb_IsNumeric(cContent)
      ::cContent := iif(hb_IsString(cContent) .AND. HB_ISNULL(cContent), NIL, cContent)
   ENDIF

RETURN cOldVal

METHOD TSimpleTaskDialog:CommonButtons(nCBs)

   LOCAL nOldVal := ::nCommonButtons

   IF hb_IsNumeric(nCBs)
      ::nCommonButtons := nCBs
   ENDIF

RETURN nOldVal

METHOD TSimpleTaskDialog:MainIcon(nIcon)

   LOCAL nOldVal := ::nMainIcon

   IF hb_IsNumeric(nIcon)
      ::nMainIcon := nIcon
   ENDIF

RETURN nOldVal

////////////////////////////////////////////////////////////////////////////////
CREATE CLASS TTaskDialog FUNCTION TaskDialog
////////////////////////////////////////////////////////////////////////////////

   EXPORTED:
   VAR    Cargo
   VAR    lActive               READONLY   INIT .F.
   VAR    lError                READONLY   INIT .T.
   VAR    nButtonResult         READONLY   INIT NIL
   VAR    nRadioButtonResult    READONLY   INIT NIL
   VAR    nResult               READONLY   INIT E_FAIL
   VAR    lVerifyResult         READONLY   INIT .F.

   METHOD New(cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon)
   METHOD Execute() INLINE ::ShowDialog()
   METHOD ShowDialog()
   METHOD DialogHandle()
   METHOD Showing(lState)
   METHOD OnCreated(hWnd, nNotify, nWParam, nLParam)
   METHOD OnDestroyed(hWnd, nNotify, nWParam, nLParam)
   METHOD Listener(hWnd, nNotify, nWParam, nLParam)
   METHOD CommonButtons(nCBs)                SETGET
   METHOD WindowTitle(cTitle)                SETGET
   METHOD Title(cTitle)                      SETGET
   METHOD MainIcon(nIcon)                    SETGET
   METHOD MainInstruction(cInstruction)      SETGET
   METHOD Instruction(cInstruction)          SETGET
   METHOD Content(cContent)                  SETGET
   METHOD CustomButtons(aCustButton)         SETGET
   METHOD DefaultButton(nDefaultButton)      SETGET
   METHOD CustomRadioButtons(aCustButton)    SETGET
   METHOD DefaultRadioButton(nDefaultButton) SETGET
   METHOD VerificationText(cText)            SETGET
   METHOD ExpandedInfo(cText)                SETGET
   METHOD ExpandedControlText(cText)         SETGET
   METHOD ExpandedCtrlText(cText)            SETGET
   METHOD CollapsedControlText(cText)        SETGET
   METHOD CollapsedCtrlText(cText)           SETGET
   METHOD FooterIcon(nIcon)                  SETGET
   METHOD Footer(cFooter)                    SETGET
   METHOD Width(nWidth)                      SETGET
   METHOD Parent(cFormName)                  SETGET
   METHOD ParentHandle(nHandle)              SETGET
   METHOD CallBackBlock(bCode)               SETGET
   METHOD Flags(nFlags)                      SETGET
   METHOD AllowDialogCancellation(lNewVal)   SETGET
   METHOD CanBeMinimized(lNewVal)            SETGET
   METHOD EnableHyperlinks(lNewVal)          SETGET
   METHOD ExpandedByDefault(lNewVal)         SETGET
   METHOD ExpandFooterArea(lNewVal)          SETGET
   METHOD NoDefaultRadioButton(lNewVal)      SETGET
   METHOD PositionRelativeToWindow(lNewVal)  SETGET
   METHOD RightToLeftLayout(lNewVal)         SETGET
   METHOD VerificationEnabled(lNewVal)       SETGET
   METHOD timeoutMS(nMS)                     SETGET
   METHOD TimedOut(lOut)                     SETGET
   // NOTE: Next method returns valid (non NIL) result if a the dialog has been shown
   // The ID of the clicked button
   METHOD SelectedButton()      INLINE ::nButtonResult
   // The ID of the selected radio button
   METHOD SelectedRadioButton() INLINE ::nRadioButtonResult
   // The state of the verification checkbox (read only)
   METHOD VerificationChecked() INLINE ::lVerifyResult    /*TODO*/

   PROTECTED:
   VAR aConfig                  INIT Array(TDC_CONFIG)
   VAR HWND            READONLY INIT NIL
   VAR lTimeOut        READONLY INIT .F.
   VAR nTimeOutMS      READONLY INIT 0

ENDCLASS
////////////////////////////////////////////////////////////////////////////////

METHOD TTaskDialog:New(cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon)

   ::aConfig[TDC_WINDOWTITLE]     := iif(hb_IsNumeric(cTitle), cTitle, iif(!hb_IsString(cTitle), NIL, iif(HB_ISNULL(cTitle), NIL, cTitle)))
   ::aConfig[TDC_MAININSTRUCTION] := iif(hb_IsNumeric(cInstruction), cInstruction, iif(!hb_IsString(cInstruction), NIL, iif(HB_ISNULL(cInstruction), NIL, cInstruction)))
   ::aConfig[TDC_CONTENT] := iif(hb_IsNumeric(cContent), cContent, iif(!hb_IsString(cContent), NIL, iif(HB_ISNULL(cContent), NIL, cContent)))
   ::aConfig[TDC_FOOTER]  := iif(hb_IsNumeric(cFooter), cFooter, iif(!hb_IsString(cFooter), NIL, iif(HB_ISNULL(cFooter), NIL, cFooter)))

   IF hb_IsNumeric(nCommonButtons)
      ::aConfig[TDC_COMMON_BUTTON_FLAGS] := nCommonButtons
   ENDIF

   IF hb_IsNumeric(nMainIcon)
      ::aConfig[TDC_MAINICON] := nMainIcon
   ENDIF

RETURN Self

/*
   Shows the dialog.

   NOTE: Returns true if everything worked right. Returns false if creation of dialog failed.
   Requires Windows Vista or newer.
 */
METHOD TTaskDialog:ShowDialog()

   LOCAL nResult
   LOCAL nButton      := NIL
   LOCAL nRadioButton := NIL
   LOCAL lVerificationFlagChecked := .F.

   IF !::lActive
      ::lError             := .T.
      ::nButtonResult      := NIL
      ::nRadioButtonResult := NIL
      ::nResult            := E_FAIL
      ::TimedOut           := .F.

      IF ::timeoutMS() > 0 .OR. __objHasMethod(Self, "ONTIMER")
         ::Flags := hb_bitOr(::Flags, TDF_CALLBACK_TIMER)
      ENDIF

      IF ::timeoutMS() > 0
         ::AllowDialogCancellation := .T.
      ENDIF

      IF os_IsWinVista_Or_Later()
         ::aConfig[23] := self
         nResult := hmg_win_TaskDialogIndirect0(::aConfig, @nButton, @nRadioButton, @lVerificationFlagChecked)
      ELSE
         nResult := E_NOTIMPL // Not implemented yet
      ENDIF

      ::lError             := !( nResult == NOERROR )
      ::nButtonResult      := nButton
      ::nRadioButtonResult := nRadioButton
      ::lVerifyResult      := lVerificationFlagChecked
      ::nResult            := nResult
   ENDIF

RETURN !::lError

/*
   The handle of the dialog.

   NOTE: This is only valid (and non NIL) while dialog is visible (read only).
 */
METHOD TTaskDialog:DialogHandle()
RETURN ::HWND

/*
   Whether dialog is currently showing (read/write).
 */
METHOD TTaskDialog:Showing(lState)

   hb_default(@lState, .F.)

   IF lState .AND. !::lActive
      ::ShowDialog()
   ENDIF

RETURN ::lActive

/*
   Indicates that the Task Dialog has been created.
*/
METHOD TTaskDialog:OnCreated(hWnd, nNotify, nWParam, nLParam)

   HB_SYMBOL_UNUSED(nWParam)
   HB_SYMBOL_UNUSED(nLParam)

   IF nNotify == TDN_CREATED
      ::lActive := .T.
      ::HWND := hWnd
   ENDIF

RETURN .F.

/*
   Indicates that the Task Dialog has been destroyed.
*/
METHOD TTaskDialog:OnDestroyed(hWnd, nNotify, nWParam, nLParam)

   HB_SYMBOL_UNUSED(hWnd)
   HB_SYMBOL_UNUSED(nWParam)
   HB_SYMBOL_UNUSED(nLParam)

   IF nNotify == TDN_DESTROYED
      ::lActive := .F.
      ::HWND := NIL
   ENDIF

RETURN .F.

/*
   The default Events Listener.
*/
METHOD TTaskDialog:Listener(hWnd, nNotify, nWParam, nLParam)

   HB_SYMBOL_UNUSED(hWnd)

   IF HB_ISEVALITEM(::aConfig[TDC_CALLBACK])
      RETURN ::aConfig[TDC_CALLBACK]:Eval(self, nNotify, nWParam, nLParam)
   ENDIF

RETURN .T.

/*
   Specifies the push buttons displayed in the task dialog (read/write).

   NOTE:  If  no  common  buttons  are  specified  and  no  custom buttons are
   specified through buttons array, the task dialog will contain the OK button
   by default.
 */
METHOD TTaskDialog:CommonButtons(nCBs)

   LOCAL nOldCBS := ::aConfig[TDC_COMMON_BUTTON_FLAGS]

   IF !::lActive
      IF hb_IsNumeric(nCBs)
         ::aConfig[TDC_COMMON_BUTTON_FLAGS] := nCBs
      ENDIF
   ENDIF

RETURN nOldCBS

/*
   The string to be used for the task dialog title (read/write, LIVE).
 */
METHOD TTaskDialog:WindowTitle(cTitle)

   LOCAL cOldVal := ::aConfig[TDC_WINDOWTITLE]

   IF hb_IsString(cTitle) .OR. hb_IsNumeric(cTitle)
      ::aConfig[TDC_WINDOWTITLE] := iif(hb_IsString(cTitle) .AND. HB_ISNULL(cTitle), NIL, cTitle)
      IF ::lActive
         hmg__SetWindowTitle(::HWND, ::aConfig[TDC_WINDOWTITLE])
      ENDIF
   ENDIF

RETURN cOldVal

METHOD TTaskDialog:Title(cTitle)
RETURN ::WindowTitle(cTitle)

/*
   TODO
*/
METHOD TTaskDialog:MainIcon(nIcon)

   IF hb_IsNumeric(nIcon)
      ::aConfig[TDC_MAINICON] := nIcon
      IF ::lActive
         hmg__UpdateMainIcon(::HWND, ::aConfig[TDC_MAINICON])
      ENDIF
   ENDIF

RETURN ::aConfig[TDC_MAINICON]

/* MainInstruction

   The string to be used for the main instruction (read/write, LIVE).
 */
METHOD TTaskDialog:MainInstruction(cInstruction)

   LOCAL cOldVal := ::aConfig[TDC_MAININSTRUCTION]

   IF hb_IsString(cInstruction) .OR. hb_IsNumeric(cInstruction)
      ::aConfig[TDC_MAININSTRUCTION] := iif(hb_IsString(cInstruction) .AND. HB_ISNULL(cInstruction), NIL, cInstruction)
      IF ::lActive
         hmg__SetMainInstruction(::HWND, ::aConfig[TDC_MAININSTRUCTION])
      ENDIF
   ENDIF

RETURN cOldVal

METHOD TTaskDialog:Instruction(cInstruction)
RETURN ::MainInstruction(cInstruction)

/*
   The string to be used for the dialog's primary content (read/write, LIVE).
 */
METHOD TTaskDialog:Content(cContent)

   LOCAL cOldVal := ::aConfig[TDC_CONTENT]

   IF hb_IsString(cContent) .OR. hb_IsNumeric(cContent)
      ::aConfig[TDC_CONTENT] := iif(hb_IsString(cContent) .AND. HB_ISNULL(cContent), NIL, cContent)
      IF ::lActive
         hmg__SetContent(::HWND, ::aConfig[TDC_CONTENT])
      ENDIF
   ENDIF

RETURN cOldVal

/*
   TODO
*/
METHOD TTaskDialog:CustomButtons(aCustButton)

   LOCAL aOldVal := ::aConfig[TDC_TASKDIALOG_BUTTON]

   IF !::lActive
      IF hb_IsArray(aCustButton) .AND. Len(aCustButton) > 0
         ::aConfig[TDC_BUTTON] := Len(aCustButton)
         ::aConfig[TDC_TASKDIALOG_BUTTON] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*
   The default button for the task dialog (read/write).

   Note:  This may be any of the values specified in ID of one of the buttons,
   or   one  of  the  IDs  corresponding  to  the  buttons  specified  in  the
   CommonButtons property.
*/
METHOD TTaskDialog:DefaultButton(nDefaultButton)

   LOCAL nOldVal := ::aConfig[TDC_DEFAULTBUTTON]

   IF !::lActive
      IF hb_IsNumeric(nDefaultButton)
         ::aConfig[TDC_DEFAULTBUTTON] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*
   TODO
*/
METHOD TTaskDialog:CustomRadioButtons(aCustButton)

   LOCAL aOldVal := ::aConfig[TDC_TASKDIALOG_RADIOBUTTON]

   IF !::lActive
      IF hb_IsArray(aCustButton) .AND. Len(aCustButton) > 0
         ::aConfig[TDC_RADIOBUTTON] := Len(aCustButton)
         ::aConfig[TDC_TASKDIALOG_RADIOBUTTON] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*
   The button ID of the radio button that is selected by default (read/write).

   NOTE: If this value does not correspond to a button ID, the first button in the array is selected by default.
 */
METHOD TTaskDialog:DefaultRadioButton(nDefaultButton)

   LOCAL nOldVal := ::aConfig[TDC_DEFAULTRADIOBUTTON]

   IF !::lActive
      IF hb_IsNumeric(nDefaultButton)
         ::aConfig[TDC_DEFAULTRADIOBUTTON] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*
   The string to be used to label the verification checkbox (read/write).
*/
METHOD TTaskDialog:VerificationText(cText)

   LOCAL cOldVal := ::aConfig[TDC_VERIFICATIONTEXT]

   IF !::lActive
      IF hb_IsString(cText) .OR. hb_IsNumeric(cText)
         ::aConfig[TDC_VERIFICATIONTEXT] := cText
      ENDIF
   ENDIF

RETURN cOldVal

/* ExpandedInformation
   The string to be used for displaying additional information (read/write,
   LIVE).

   NOTE:  The additional information is displayed either immediately below the
   content  or below the footer text depending on whether the ExpandFooterArea
   flag  is  true.  If the EnableHyperlinks flag is true, then this string may
   contain   hyperlinks  in  the  form:

   <A  HREF="executablestring">Hyperlink Text</A>.

   WARNING:  Enabling  hyperlinks when using content from an unsafe source may
   cause security vulnerabilities.
 */
METHOD TTaskDialog:ExpandedInfo(cText)

   LOCAL cOldVal := ::aConfig[TDC_EXPANDEDINFORMATION]

   IF hb_IsString(cText) .OR. hb_IsNumeric(cText)
      ::aConfig[TDC_EXPANDEDINFORMATION] := cText
      IF ::lActive
         hmg__SetExpandedInformation(::HWND, ::aConfig[TDC_EXPANDEDINFORMATION])
      ENDIF
   ENDIF

RETURN cOldVal

/* ExpandedControlText
   The  string  to  be  used to label the button for collapsing the expandable
   information (read/write).

   NOTE: This member is ignored when the ExpandedInformation member is empty.
   If this member is empty and the CollapsedControlText is specified, then the
   CollapsedControlText value will be used for this member as well.
 */
METHOD TTaskDialog:ExpandedControlText(cText)

   LOCAL cOldVal := ::aConfig[TDC_EXPANDEDCONTROLTEXT]

   IF !::lActive
      IF hb_IsString(cText) .OR. hb_IsNumeric(cText)
         ::aConfig[TDC_EXPANDEDCONTROLTEXT] := cText
      ENDIF
   ENDIF

RETURN cOldVal

METHOD TTaskDialog:ExpandedCtrlText(cText)
RETURN ::ExpandedControlText(cText)

/* CollapsedControlText
   The  string  to  be  used  to label the button for expanding the expandable
   information (read/write).

   NOTE: This member  is ignored when the ExpandedInformation member is empty.
   If this member is empty and the CollapsedControlText is specified, then the
   CollapsedControlText value will be used for this member as well.
 */
METHOD TTaskDialog:CollapsedControlText(cText)

   LOCAL cOldVal := ::aConfig[TDC_COLLAPSEDCONTROLTEXT]

   IF !::lActive
      IF hb_IsString(cText) .OR. hb_IsNumeric(cText)
         ::aConfig[TDC_COLLAPSEDCONTROLTEXT] := cText
      ENDIF
   ENDIF

RETURN cOldVal

METHOD TTaskDialog:CollapsedCtrlText(cText)
RETURN ::CollapsedControlText(cText)

/*
   TODO
*/
METHOD TTaskDialog:FooterIcon(nIcon)

   LOCAL nOldVal := ::aConfig[TDC_FOOTERICON]

   IF hb_IsNumeric(nIcon)
      ::aConfig[TDC_FOOTERICON] := nIcon
      IF ::lActive
         hmg__UpdateFooterIcon(::HWND, ::aConfig[TDC_FOOTERICON])
      ENDIF
   ENDIF

RETURN nOldVal

/*
   The string to be used in the footer area of the task dialog (read/write).

   NOTE: If EnableHyperlinks is true, this can show clickable links.
 */
METHOD TTaskDialog:Footer(cFooter)

   LOCAL cOldVal := ::aConfig[TDC_FOOTER]

   IF hb_IsString(cFooter) .OR. hb_IsNumeric(cFooter)
      ::aConfig[TDC_FOOTER] := cFooter
      IF ::lActive
         hmg__SetFooter(::HWND, ::aConfig[TDC_FOOTER])
      ENDIF
   ENDIF

RETURN cOldVal

/*
   The width of the task dialog's client area, in dialog units (read/write).

   NOTE: If 0, the task dialog manager will calculate the ideal width.
 */
METHOD TTaskDialog:Width(nWidth)

   LOCAL nOldVal := ::aConfig[TDC_WIDTH]

   IF !::lActive .AND. hb_IsNumeric(nWidth)
      ::aConfig[TDC_WIDTH] := nWidth
   ENDIF

RETURN nOldVal

/*
   Parent window handle (read/write).
 */
METHOD TTaskDialog:ParentHandle(nHandle)

   LOCAL nOldVal := ::aConfig[TDC_HWND]

   IF !::lActive .AND. hb_IsNumeric(nHandle) .AND. hmg_IsWindowHandle(nHandle)
      ::aConfig[TDC_HWND] := nHandle
   ENDIF

RETURN nOldVal

/*
   Parent window name (read/write).
 */
METHOD TTaskDialog:Parent(cFormName)
RETURN _HMG_aFormNames[AScan(_HMG_aFormHandles, ::ParentHandle(GetFormHandle(cFormName)))]

/*
   NOTE: Method CallBackBlock will be deleted in future (not near)
*/
METHOD TTaskDialog:CallBackBlock(bCode)

   IF !::lActive
      IF HB_ISEVALITEM(bCode)
         ::aConfig[TDC_CALLBACK] := bCode
      ENDIF
   ENDIF

RETURN ::aConfig[TDC_CALLBACK]

////////////////////////////////////////////////////////////////////////////////
/*
   The flags (read/write).

   NOTE:  Maybe You should not need to set flags as we have properties for all
   relevant flags.
 */
METHOD TTaskDialog:Flags(nFlags)

   LOCAL nOldVal := ::aConfig[TDC_TASKDIALOG_FLAGS]
   
   IF !::lActive
      IF hb_IsNumeric(nFlags)
         ::aConfig[TDC_TASKDIALOG_FLAGS] := nFlags
      ENDIF
   ENDIF

RETURN nOldVal

/*
   Whether to allow cancel (read/write).

   NOTE: Indicates that the dialog  should be  able to be closed using Alt-F4,
   Escape,  and  the  title  bar's  close  button  even if no cancel button is
   specified  in  either the CommonButtons or Buttons members.
 */
METHOD TTaskDialog:AllowDialogCancellation(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_ALLOW_DIALOG_CANCELLATION))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Indicates that the task dialog can be minimized (read/write).
 */
METHOD TTaskDialog:CanBeMinimized(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_CAN_BE_MINIMIZED) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_CAN_BE_MINIMIZED)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_CAN_BE_MINIMIZED))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Whether to enable hyperlinks (read/write).

   NOTE:  Enables  hyperlink  processing  for  the  strings  specified  in the
   Content,  ExpandedInformation  and  Footer  members.  When  enabled,  these
   members may point to strings that contain hyperlinks in the following form:

   <A HREF="executablestring">Hyperlink Text</A>

   NOTE:  Task  Dialogs  will  not  actually execute any hyperlinks. Hyperlink
   execution _must be handled_ in the OnHyperlinkClicked event.

   WARNING:  Enabling  hyperlinks when using content from an unsafe source may
   cause security vulnerabilities.
 */
METHOD TTaskDialog:EnableHyperlinks(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_ENABLE_HYPERLINKS) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_ENABLE_HYPERLINKS)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_ENABLE_HYPERLINKS))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Indicates  that  the  string specified by the ExpandedInformation member is
   displayed when the dialog is initially displayed (read/write).

   NOTE: This flag is ignored if the ExpandedInformation member is empty.
 */
METHOD TTaskDialog:ExpandedByDefault(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_EXPANDED_BY_DEFAULT) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_EXPANDED_BY_DEFAULT)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_EXPANDED_BY_DEFAULT))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Whether expand footer area is displayed at the bottom (read/write).

   NOTE: Indicates that the string specified by the ExpandedInformation member
   is  displayed  at  the  bottom  of  the  dialog's  footer  area  instead of
   immediately  after  the  dialog's  content.  This  flag  is  ignored if the
   ExpandedInformation member is empty.
 */
METHOD TTaskDialog:ExpandFooterArea(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_EXPAND_FOOTER_AREA) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_EXPAND_FOOTER_AREA)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_EXPAND_FOOTER_AREA))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Indicates that no default item will be selected (read/write)
 */
METHOD TTaskDialog:NoDefaultRadioButton(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_NO_DEFAULT_RADIO_BUTTON))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Indicates  that  the  task  dialog is positioned (centered) relative to the
   window specified by parent.

   NOTE:  If  the flag is not supplied (or no parent member is specified), the
   task dialog is positioned (centered) relative to the monitor.
 */
METHOD TTaskDialog:PositionRelativeToWindow(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_POSITION_RELATIVE_TO_WINDOW))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   Indicates that text is displayed reading right to left (read/write).
 */
METHOD TTaskDialog:RightToLeftLayout(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_RTL_LAYOUT) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_RTL_LAYOUT)
      ELSEIF lOldVal .AND. (!NewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_RTL_LAYOUT))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

/*
   The enable state of the verification checkbox.

   NOTE: Can be true to enable the checkbox or false to disable.
 */
METHOD TTaskDialog:VerificationEnabled(lNewVal)

   LOCAL nCurFlags := ::Flags()
   LOCAL lOldVal
   LOCAL nNewFlags

   hb_default(@nCurFlags, 0)
   lOldVal := ( hb_bitAnd(nCurFlags, TDF_VERIFICATION_FLAG_CHECKED) != 0 )

   IF !::lActive .AND. hb_IsLogical(lNewVal)
      IF (!lOldVal) .AND. lNewVal
         nNewFlags := hb_bitOr(nCurFlags, TDF_VERIFICATION_FLAG_CHECKED)
      ELSEIF lOldVal .AND. (!lNewVal)
         nNewFlags := hb_bitAnd(nCurFlags, hb_bitNot(TDF_VERIFICATION_FLAG_CHECKED))
      ENDIF
      ::Flags(nNewFlags)
   ENDIF

RETURN lOldVal

////////////////////////////////////////////////////////////////////////////////
/*
   The timeout for the dialog (read/write).

   NOTE: In Milliseconds. The dialog closes after given time.
 */
METHOD TTaskDialog:timeoutMS(nMS)

   LOCAL nOldVal := ::nTimeOutMS

   IF !::lActive .AND. hb_IsNumeric(nMS)
      ::nTimeOutMS := nMS
   ENDIF

RETURN nOldVal

/*
   Whether we got a timeout (read/write, read only in future, maybe)
 */
METHOD TTaskDialog:TimedOut(lOut)

   IF ::lActive .AND. hb_IsLogical(lOut)
      ::lTimeOut := lOut
   ENDIF

RETURN ::lTimeOut

#endif
