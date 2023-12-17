#include "minigui.ch"
#include <hbclass.ch>
#include "TSBrowse.ch"

#define GWL_WNDPROC        (-4)

#define LTGRAY_BRUSH       1
#define GRAY_BRUSH         2

#define GW_HWNDNEXT        2
#define GW_CHILD           5
#define CW_USEDEFAULT      32768

#define SC_NEXT            61504
#define SC_KEYMENU         61696   // 0xF100

#define DM_GETDEFID        WM_USER

#define CBN_SELCHANGE      1
#define CBN_CLOSEUP        8
#define CBN_KILLFOCUS      4
#define NM_KILLFOCUS       (-8)

MEMVAR _TSB_aControlhWnd
MEMVAR _TSB_aControlObjects
MEMVAR _TSB_aClientMDIhWnd

CLASS TControl

   DATA bSetGet
   DATA bChange
   DATA cCaption
   DATA nLastRow
   DATA nLastCol
   DATA nAlign AS NUMERIC
   DATA nStatusItem INIT 1

   DATA bLClicked                                  // TControl
   DATA bLDblClick                                 // TControl
   DATA bRClicked                                  // TControl
   DATA bWhen                                      // TWindow
   DATA cMsg                                       // TWindows

   DATA bMoved
   DATA bLButtonUp
   DATA bKeyDown
   DATA bPainted
   DATA bMButtonDown
   DATA bMButtonUp
   DATA bRButtonUp
   DATA bResized
   DATA bValid
   DATA bKeyChar
   DATA bMMoved
   DATA bGotFocus
   DATA bLostFocus
   DATA bDropFiles
   DATA bDdeInit
   DATA bDdeExecute

   DATA lFocused      AS LOGICAL                   // TWindow
   DATA lValidating   AS LOGICAL                   // TWindow
   DATA lCaptured     AS LOGICAL                   // TControl
   DATA lUpdate       AS LOGICAL                   // TControl
   DATA lDesign       AS LOGICAL                   // TControl
   DATA lVisible      AS LOGICAL                   // TControl
   DATA lMouseDown    AS LOGICAL                   // TControl
   DATA lKeepDefaultStatus AS LOGICAL INIT .F.     // TControl

   DATA nTop                                       // TWindow
   DATA nLeft                                      // TWindow
   DATA nBottom                                    // TWindow
   DATA nRight                                     // TWindow
   DATA nStyle                                     // TWindow
   DATA nId                                        // TWindow
   DATA nClrText                                   // TWindow
   DATA nClrPane                                   // TWindow
   DATA nPaintCount                                // TWindow
   DATA nLastKey                                   // TWindow
   DATA nHelpId                                    // TWindow
   DATA nChrHeight                                 // TWindow

   DATA oWnd          AS OBJECT                    // TWindow
   DATA oCursor                                    // TWindow
   DATA hCursor                                    // JP
   DATA oFont                                      // TWindow
   DATA hFont                                      // New
   DATA hBrush                                     // New
   DATA hWnd
   DATA hCtlFocus                           // TWindow
   DATA cControlName                               // new
   DATA cParentWnd                                 // new
   DATA hDc                                        // TWindow
   DATA cPS                                        // TWindow
   DATA oVScroll                                   // TWindow
   DATA oHScroll                                   // TWindow

   DATA hWndParent                                 // New
   DATA aControls INIT {}                        // New
   DATA oWndlAppendMode INIT .F.                   // New

   DATA oBrw
   DATA oCol
   DATA nCol

   CLASSDATA aProperties INIT {"cTitle", "cVarName", "nClrText", "nClrPane", "nAlign", "nTop", "nLeft", "nWidth", "nHeight", "Cargo"}

   METHOD AddControl(hControl) INLINE IIf(::aControls == NIL, ::aControls := {}, NIL), AAdd(::aControls, hControl), ::lValidating := .F.
   METHOD AddVars(hControl)
   METHOD Change() VIRTUAL
   METHOD Click() INLINE ::oWnd:AEvalWhen()
   METHOD Init(hDlg)
   METHOD Colors(hDC)
   METHOD CoorsUpdate()                //TWindow
   METHOD Create(cClsName)           //TWindow
   METHOD Default()
   METHOD DelVars(hControl)
   METHOD Display()              VIRTUAL
   METHOD DrawItem(nPStruct)   VIRTUAL
   METHOD Save()                 VIRTUAL
   METHOD End()
   METHOD EraseBkGnd(hDC)
   METHOD FillMeasure()          VIRTUAL
   METHOD ForWhen()
   METHOD GetDlgCode(nLastKey)
   METHOD GetCliRect()                 //TWindow
   METHOD GetRect()
   METHOD GetNewId() INLINE IIf(::nId == NIL, ::nId := 100, NIL), ++::nId
   METHOD GotFocus(hCtlLost)
   METHOD GoNextCtrl(hCtrl)
   METHOD GoPrevCtrl(hCtrl)
   METHOD LostFocus(hWndGetFocus)
   METHOD nWidth() INLINE GetWindowWidth(::hWnd)
   METHOD nHeight() INLINE GetWindowHeight(::hWnd)
   METHOD HandleEvent(nMsg, nWParam, nLParam)
   METHOD KeyChar(nKey, nFlags)
   METHOD KeyDown(nKey, nFlags)
   METHOD KeyUp(nKey, nFlags) VIRTUAL
   METHOD KillFocus(hCtlFocus)
   METHOD VarPut(uVal) INLINE IIf(hb_IsBlock(::bSetGet), Eval(::bSetGet, uVal), NIL)
   METHOD VarGet() INLINE IIf(hb_IsBlock(::bSetGet), Eval(::bSetGet), NIL)
   METHOD LButtonDown(nRow, nCol, nKeyFlags)
   METHOD LButtonUp(nRow, nCol, nKeyFlags)
   METHOD MouseMove(nRow, nCol, nKeyFlags)
   METHOD Paint() VIRTUAL
   METHOD SuperKeyDown(nKey, nFlags, xObj)
   MESSAGE BeginPaint METHOD _BeginPaint()
   METHOD EndPaint() INLINE ::nPaintCount--, hmg_EndPaint(::hWnd, ::cPS), ::cPS := NIL, ::hDC := NIL
   METHOD Register(nClsStyle)            //TWindow
   MESSAGE SetFocus METHOD __SetFocus()   //TWindow
   METHOD RButtonUp(nRow, nCol, nKeyFlags)    //TWindow
   METHOD Capture() INLINE SetCapture(::hWnd) //TWindow
   METHOD GetDC() INLINE IIf(::hDC == NIL, ::hDC := GetDC(::hWnd), NIL), IIf(::nPaintCount == NIL, ::nPaintCount := 1, ::nPaintCount++), ::hDC
   METHOD ReleaseDC() INLINE ::nPaintCount--, IIf(::nPaintCount == 0, IIf(ReleaseDC(::hWnd, ::hDC), ::hDC := NIL, NIL), NIL)
   METHOD PostMsg(nMsg, nWParam, nLParam) INLINE PostMessage(::hWnd, nMsg, nWParam, nLParam)
   METHOD lValid() INLINE IIf(::bValid != NIL, Eval(::bValid), .T.)
   METHOD SetMsg(cText, lDefault)
   METHOD lWhen() INLINE IIf(::bWhen != NIL, Eval(::bWhen), .T.)
   METHOD SetColor(nClrFore, nClrBack, hBrush)
   METHOD EndCtrl() BLOCK {|Self, lEnd|IIf(lEnd := ::lValid(), ::PostMsg(WM_CLOSE), NIL), lEnd} // It has to be Block
   METHOD Hide() INLINE ShowWindow(::hWnd, SW_HIDE)
   METHOD Show() INLINE ShowWindow(::hWnd, SW_SHOWNA)
   METHOD SendMsg(nMsg, nWParam, nLParam) INLINE SendMessage(::hWnd, nMsg, nWParam, nLParam)
   METHOD Move(nTop, nLeft, nWidth, nHeight, lRepaint)
   METHOD ReSize(nSizeType, nWidth, nHeight)
   METHOD Command(nWParam, nLParam)
   METHOD Notify(nWParam, nLParam)
   METHOD Refresh(lErase) INLINE hmg_InvalidateRect(::hWnd, IIf(lErase == NIL .OR. !lErase, 0, 1))
   METHOD nGetChrHeight() INLINE ::hDC := GetDC(::hWnd), ::nChrHeight := _GetTextHeight(::hWnd, ::hDC) // Temp
   METHOD GetText() INLINE GetWindowText(::hWnd)   //TWindow
   METHOD VScroll(nWParam, nLParam)                //TWindow

ENDCLASS

METHOD TControl:Init(hDlg)

   LOCAL oRect

   DEFAULT ::lActive := .T., ::lCaptured := .F.

   IF (::hWnd := hmg_GetDialogItemHandle(hDlg, ::nId)) != 0    //JP
      oRect     := ::GetRect()

      ::nTop    := iif(::nTop    == NIL, oRect:nTop,   ::nTop    )
      ::nLeft   := iif(::nLeft   == NIL, oRect:nLeft,  ::nLeft   )
      ::nBottom := iif(::nBottom == NIL, oRect:nBottom,::nBottom )
      ::nRight  := iif(::nRight  == NIL, oRect:nRight, ::nRight  )

      ::Move (::nTop,::nLeft,::nRight - ::nLeft, ::nBottom - ::nTop)

      IIf(::lActive, ::Enable(), ::Disable())

      ::Link()

      IF ::oFont != NIL
         ::SetFont(::oFont)
      ELSE
         ::GetFont()
      ENDIF

   ELSE
        MsgInfo("No Valid Control ID", "Error")
   ENDIF

RETURN NIL

METHOD TControl:AddVars(hControl)

   AAdd(_TSB_aControlhWnd, hControl)
   AAdd(_TSB_aControlObjects, Self)
   AAdd(_TSB_aClientMDIhWnd, iif(_HMG_BeginWindowMDIActive, GetActiveMdiHandle(), 0))

RETURN NIL

METHOD TControl:DelVars(hControl)

   LOCAL nAt := iif(!Empty(_TSB_aControlhWnd), AScan(_TSB_aControlhWnd, {|hCtrl|hCtrl == Self:hWnd}), 0)

   HB_SYMBOL_UNUSED(hControl)

   IF nAt != 0
      ADel(_TSB_aControlhWnd, nAt)
      ASize(_TSB_aControlhWnd, Len(_TSB_aControlhWnd) - 1)
      ADel(_TSB_aControlObjects, nAt)
      ASize(_TSB_aControlObjects, Len(_TSB_aControlObjects) - 1)
      ADel(_TSB_aClientMDIhWnd, nAt)
      ASize(_TSB_aClientMDIhWnd, Len(_TSB_aClientMDIhWnd) - 1)
   ENDIF

RETURN NIL

METHOD TControl:_BeginPaint()

   LOCAL cPS

   IF ::nPaintCount == NIL
      ::nPaintCount := 1
   ELSE
      ::nPaintCount++
   ENDIF

   ::hDC = hmg_BeginPaint(::hWnd, @cPS)
   ::cPS = cPS

RETURN NIL

METHOD TControl:Colors(hDC)

   DEFAULT ::nClrText := GetTextColor(hDC)
   DEFAULT ::nClrPane := GetBkColor(hDC)
   DEFAULT ::hBrush   := CreateSolidBrush(GetRed(::nClrPane), GetGreen(::nClrPane), GetBlue(::nClrPane))

   SetTextColor(hDC, ::nClrText)
   SetBkColor(hDC, ::nClrPane)

RETURN ::hBrush

METHOD TControl:CoorsUpdate()

   LOCAL aRect := {0,0,0,0}

   GetWindowRect(::hWnd, aRect)
/*
   ::nTop    = aRect[2]
   ::nLeft   = aRect[1]
   ::nBottom = aRect[4]
   ::nRight  = aRect[3]
*/
RETURN NIL

METHOD TControl:Create(cClsName)

   LOCAL xStyle := 0

   DEFAULT cClsName   := ::ClassName()
   DEFAULT ::cCaption := ""
   DEFAULT ::nStyle   := WS_OVERLAPPEDWINDOW
   DEFAULT ::nTop     := 0
   DEFAULT ::nLeft    := 0
   DEFAULT ::nBottom  := 10
   DEFAULT ::nRight   := 10
   DEFAULT ::nId      := 0

   IF ::hWnd != NIL
      ::nStyle := nOr(::nStyle, WS_CHILD)
   ENDIF

   IF ::hBrush == NIL
      ::hBrush := CreateSolidBrush(GetRed(::nClrPane), GetGreen(::nClrPane), GetBlue(::nClrPane))
   ENDIF

   IF GetClassInfo(hmg_GetInstance(), cClsName) == NIL
      IF _HMG_MainClientMDIHandle != 0
         ::lRegistered := Register_Class(cClsName, ::hBrush, _HMG_MainClientMDIHandle)
      ELSE
         ::lRegistered := Register_Class(cClsName, ::hBrush)
      ENDIF
   ELSE
      ::lRegistered := .T.
   ENDIF

   IF ::nBottom != CW_USEDEFAULT
     ::hWnd := _CreateWindowEx(xStyle, cClsName, ::cCaption, ::nStyle, ::nLeft, ::nTop, ::nRight - ::nLeft + 1, ::nBottom - ::nTop + 1, ::hWndParent, 0, hmg_GetInstance(), ::nId)
   ELSE
     ::hWnd := _CreateWindowEx(xStyle, cClsName, ::cCaption, ::nStyle, ::nLeft, ::nTop, ::nRight, ::nBottom, ::hWndParent, 0, hmg_GetInstance(), ::nId)
   ENDIF

   IF ::hWnd == 0
      MsgAlert("Window Create Error!", "Alert")
   ELSE
      ::AddVars(::hWnd)
   ENDIF

RETURN NIL

METHOD TControl:Default()

   ::lCaptured := .F.

RETURN NIL

METHOD TControl:End()

   LOCAL ix
   LOCAL nAt := IIf(!Empty(::oWnd:aControls), AScan(::oWnd:aControls, {|hCtrl|hCtrl == Self:hWnd}), 0)

   IF nAt != 0
      ADel(::oWnd:aControls, nAt)
      ASize(::oWnd:aControls, Len(::oWnd:aControls) - 1)
   ENDIF

   ::DelVars(Self:hWnd)

   IF "TGETBOX" $ Upper(Self:ClassName())
      ix := GetControlIndex(::cControlName, ::oWnd:cParentWnd)
      IF ix > 0
         ReleaseControl(_HMG_aControlHandles[ix])
         _HMG_aControlDeleted[ix] := .T.
      ENDIF
   ENDIF
   IF "TBTNBOX" $ Upper(Self:ClassName())
      IF ::hWndChild != NIL
         PostMessage(::hWndChild, WM_CLOSE)
      ENDIF
      ::PostMsg(WM_CLOSE)
      RETURN .T.
   ENDIF

RETURN ::EndCtrl()

METHOD TControl:EraseBkGnd(hDC)

   LOCAL aRect

   IF IsIconic(::hWnd)
      IF ::hWnd != NIL
         aRect := ::GetCliRect(::hWnd)
         FillRect(hDC, aRect[1], aRect[2], aRect[3], aRect[4], ::hBrush )
         RETURN 1
      ENDIF
      RETURN 0
   ENDIF

   IF ::hBrush != NIL .AND. !Empty(::hBrush)   //JP
        aRect := ::GetCliRect(::hWnd)
        FillRect(hDC, aRect[1], aRect[2], aRect[3], aRect[4], ::hBrush)
      RETURN 1
   ENDIF

RETURN 0   //NIL JP

METHOD TControl:ForWhen()

   ::oWnd:AEvalWhen()

   ::lCaptured := .F.

   // keyboard navigation
   IF ::oWnd:nLastKey == VK_UP .OR. ::oWnd:nLastKey == VK_DOWN .OR. ::oWnd:nLastKey == VK_RETURN .OR. ::oWnd:nLastKey == VK_TAB
      IF _GetKeyState(VK_SHIFT)
         ::GoPrevCtrl(::hWnd)
      ELSE
         ::GoNextCtrl(::hWnd)
      ENDIF
   ELSE
      IF Empty(GetFocus())
         hmg_SetFocus(::hWnd)
      ENDIF
   ENDIF

   ::oWnd:nLastKey := 0

RETURN NIL

METHOD TControl:GetCliRect()

   LOCAL aRect := _GetClientRect(::hWnd)

RETURN aRect

METHOD TControl:GetDlgCode(nLastKey)

   IF !::oWnd:lValidating
      IF nLastKey == VK_RETURN .OR. nLastKey == VK_TAB
         ::oWnd:nLastKey := nLastKey

      // don't do a else here with :nLastKey = 0
      // or WHEN does not work properly, as we pass here twice before
      // evaluating the WHEN
      ENDIF
   ENDIF

RETURN DLGC_WANTALLKEYS // It is the only way to have 100% control using Folders

METHOD TControl:GetRect()

   LOCAL aRect := {0,0,0,0}

   GetWindowRect(::hWnd, aRect)

RETURN aRect

METHOD TControl:GotFocus(hCtlLost)

   HB_SYMBOL_UNUSED(hCtlLost)

   ::lFocused := .T.
   ::SetMsg(::cMsg)

   IF ::bGotFocus != NIL
      RETURN Eval(::bGotFocus)
   ENDIF

RETURN NIL

METHOD TControl:GoNextCtrl(hCtrl)

   LOCAL  hCtlNext

   hCtlNext    := GetNextDlgTabITem(GetActiveWindow(), GetFocus(), .F.)

   ::hCtlFocus := hCtlNext

   IF hCtlNext != hCtrl
      hmg_SetFocus(hCtlNext)
   ENDIF

RETURN NIL

METHOD TControl:GoPrevCtrl(hCtrl)

   LOCAL hCtlPrev

   hCtlPrev := GetNextDlgTabItem(GetActiveWindow(), GetFocus(), .T.)

   ::hCtlFocus := hCtlPrev

   IF hCtlPrev != hCtrl
      hmg_SetFocus(hCtlPrev)
   ENDIF

RETURN NIL

METHOD TControl:KeyChar(nKey, nFlags)

   LOCAL bKeyAction := SetKey(nKey)

   DO CASE
   CASE nKey == VK_TAB .AND. _GetKeyState(VK_SHIFT)
      ::GoPrevCtrl(::hWnd)
      RETURN 0    // We don't want API default behavior
   CASE nKey == VK_TAB
      ::GoNextCtrl(::hWnd)
      RETURN 0    // We don't want API default behavior
   ENDCASE

   IF bKeyAction != NIL     // Clipper SET KEYs !!!
      RETURN Eval(bKeyAction, ProcName(4), ProcLine(4))
   ENDIF

   IF ::bKeyChar != NIL
      RETURN Eval(::bKeyChar, nKey, nFlags)
   ENDIF

RETURN 0

METHOD TControl:KeyDown(nKey, nFlags)

   LOCAL bKeyAction := SetKey(nKey)

   IF nKey == VK_TAB .AND. ::hWnd != NIL
      ::GoNextCtrl(::hWnd)
      RETURN 0
   ENDIF

   IF bKeyAction != NIL     // Clipper SET KEYs !!!
      Eval(bKeyAction, ProcName(4), ProcLine(4))
      RETURN 0
   ENDIF

   IF nKey == VK_F1
      // JP ::HelpTopic()
      RETURN 0
   ENDIF

   IF ::bKeyDown != NIL
      RETURN Eval(::bKeyDown, nKey, nFlags)
   ENDIF

RETURN 0

METHOD TControl:KillFocus(hCtlFocus)

   HB_SYMBOL_UNUSED(hCtlFocus)

RETURN ::LostFocus()

METHOD TControl:LButtonDown(nRow, nCol, nKeyFlags)

   ::lMouseDown := .T.
   ::nLastRow   := nRow
   ::nLastCol   := nCol

   IF ::bLClicked != NIL
      RETURN Eval(::bLClicked, nRow, nCol, nKeyFlags)
   ENDIF

RETURN NIL

METHOD TControl:LButtonUp(nRow, nCol, nKeyFlags)

   IF ::bLButtonUp != NIL
      RETURN Eval(::bLButtonUp, nRow, nCol, nKeyFlags)
   ENDIF

RETURN NIL

METHOD TControl:LostFocus(hWndGetFocus)

   ::lFocused := .F.
   ::SetMsg()
   IF !Empty(::bLostFocus)
      RETURN Eval(::bLostFocus, hWndGetFocus)
   ENDIF

RETURN NIL

METHOD TControl:MouseMove(nRow, nCol, nKeyFlags)

   IF ::oCursor != NIL
      hmg_SetResCursor(::oCursor:hCursor)
   ELSE
      CursorArrow()
   ENDIF

   IF ::lFocused
      ::SetMsg(::cMsg, ::lKeepDefaultStatus)
   ENDIF

   IF ::bMMoved != NIL
      RETURN Eval(::bMMoved, nRow, nCol, nKeyFlags)
   ENDIF

RETURN 0

METHOD TControl:Move(nTop, nLeft, nWidth, nHeight, lRepaint)

   MoveWindow(::hWnd, nTop, nLeft, nWidth, nHeight, lRepaint)

   ::CoorsUpdate()

RETURN NIL

METHOD TControl:RButtonUp(nRow, nCol, nKeyFlags)

   IF ::bRButtonUp != NIL
      Eval(::bRButtonUp, nRow, nCol, nKeyFlags)
   ENDIF

RETURN NIL

METHOD TControl:Register(nClsStyle)

   LOCAL hUser
   LOCAL ClassName

   DEFAULT ::lRegistered := .F.

   IF ::lRegistered
      RETURN NIL
   ENDIF

   hUser := hmg_GetInstance()

   ClassName := ::cControlName

   DEFAULT nClsStyle  := nOr(CS_VREDRAW, CS_HREDRAW)
   DEFAULT ::nClrPane := GetSysColor(COLOR_WINDOW)
   DEFAULT ::hBrush   := CreateSolidBrush(GetRed(::nClrPane), GetGreen(::nClrPane), GetBlue(::nClrPane))

   nClsStyle := nOr(nClsStyle, CS_GLOBALCLASS, CS_DBLCLKS)

   IF GetClassInfo(hUser, ClassName) == NIL
      ::lRegistered := Register_Class(ClassName, nClsStyle, "", , hUser, 0, ::hBrush)
   ELSE
      ::lRegistered := .T.
   ENDIF

RETURN ::hBrush

METHOD TControl:ReSize(nSizeType, nWidth, nHeight)

   ::CoorsUpdate()
   IF ::bResized != NIL
      Eval(::bResized, nSizeType, nWidth, nHeight)
   ENDIF

RETURN NIL

METHOD TControl:SetMsg(cText, lDefault)

   LOCAL cOldText
   LOCAL cParentWnd

   IF ::nStatusItem < 1
      RETURN NIL
   ENDIF

   DEFAULT lDefault := .F.
   DEFAULT cText    := ""

   cParentWnd := iif(_HMG_MainClientMDIHandle == 0, ::cParentWnd, _HMG_MainClientMDIName)

   IF _IsWindowActive(cParentWnd)
      IF _IsControlDefined("StatusBar", cParentWnd)
         IF !lDefault
            cOldText := GetItemBar(_HMG_ActiveStatusHandle, ::nStatusItem)
            IF !(AllTrim(cOldText) == AllTrim(cText))
               SetProperty(cParentWnd, "StatusBar", "Item", ::nStatusItem, cText)
            ENDIF
         ELSEIF hb_IsChar(_HMG_DefaultStatusBarMessage)
            SetProperty(cParentWnd, "StatusBar", "Item", ::nStatusItem, _HMG_DefaultStatusBarMessage)
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

METHOD TControl:SetColor(nClrFore, nClrBack, hBrush)

   ::nClrText = nClrFore
   ::nClrPane = nClrBack

   IF ::hBrush != NIL
      hmg_DeleteObject(::hBrush)  // Alen Uzelac 13.09.2012
   ENDIF

   IF hBrush != NIL
      ::hBrush := hBrush
   ELSE
      ::hBrush := CreateSolidBrush(GetRed(nClrBack), GetGreen(nClrBack), GetBlue(nClrBack))
   ENDIF

RETURN NIL

//==========From TWindow ===============================

METHOD TControl:SuperKeyDown(nKey, nFlags, xObj)

   LOCAL bKeyAction := SetKey(nKey)

   IF bKeyAction != NIL     // Clipper SET KEYs !!!
      Eval(bKeyAction, ProcName(4), ProcLine(4))
      RETURN 0
   ENDIF

   IF nKey == VK_F1
      // ::HelpTopic()
      RETURN 0
   ENDIF

   IF ::bKeyDown != NIL
      RETURN Eval(::bKeyDown, nKey, nFlags, xObj)
   ENDIF

RETURN NIL

METHOD TControl:__SetFocus()

   IF ::lWhen()
      hmg_SetFocus(::hWnd)
      ::oWnd:hCtlFocus := ::hWnd
   ENDIF

RETURN NIL

METHOD TControl:VScroll(nWParam, nLParam)

   LOCAL nScrHandle := HiWord(nLParam)

   IF nScrHandle == 0                   // Window ScrollBar
      IF ::oVScroll != NIL
         SWITCH nWParam
         CASE SB_LINEUP        ; ::oVScroll:GoUp()                      ; EXIT
         CASE SB_LINEDOWN      ; ::oVScroll:GoDown()                    ; EXIT
         CASE SB_PAGEUP        ; ::oVScroll:PageUp()                    ; EXIT
         CASE SB_PAGEDOWN      ; ::oVScroll:PageDown()                  ; EXIT
         CASE SB_THUMBPOSITION ; ::oVScroll:ThumbPos(LoWord(nLParam))   ; EXIT
         CASE SB_THUMBTRACK    ; ::oVScroll:ThumbTrack(LoWord(nLParam)) ; EXIT
         CASE SB_ENDSCROLL     ; RETURN 0
         ENDSWITCH
      ENDIF
   ELSE                                 // Control ScrollBar
      SWITCH nWParam
      CASE SB_LINEUP        ; SendMessage(nScrHandle, FM_SCROLLUP)                  ; EXIT
      CASE SB_LINEDOWN      ; SendMessage(nScrHandle, FM_SCROLLDOWN)                ; EXIT
      CASE SB_PAGEUP        ; SendMessage(nScrHandle, FM_SCROLLPGUP)                ; EXIT
      CASE SB_PAGEDOWN      ; SendMessage(nScrHandle, FM_SCROLLPGDN)                ; EXIT
      CASE SB_THUMBPOSITION ; SendMessage(nScrHandle, FM_THUMBPOS, LoWord(nLParam)) ; EXIT
      CASE SB_THUMBTRACK    ; SendMessage(nScrHandle, FM_THUMBTRACK, LoWord(nLParam))
      ENDSWITCH
   ENDIF

RETURN 0

METHOD TControl:HandleEvent(nMsg, nWParam, nLParam)

   SWITCH nMsg

   CASE WM_CLOSE
      RETURN 0

   CASE WM_COMMAND
      RETURN ::Command(nWParam, nLParam)

   CASE WM_NOTIFY
      RETURN ::Notify(nWParam, nLParam)

   CASE WM_PAINT
      ::BeginPaint()
      ::Paint()
      ::EndPaint()
      SysRefresh()
      EXIT

   CASE WM_DESTROY
      RETURN ::Destroy()

   CASE WM_DRAWITEM
      RETURN ::DrawItem(nWParam, nLParam)

   CASE WM_ERASEBKGND
      RETURN ::EraseBkGnd(nWParam)

   CASE WM_HSCROLL
      RETURN ::HScroll(nWParam, nLParam)

   CASE WM_KEYDOWN
      RETURN ::KeyDown(nWParam, nLParam)

   CASE WM_CHAR
      RETURN ::KeyChar(nWParam, nLParam)

   CASE WM_GETDLGCODE
      RETURN ::GetDlgCode(nWParam)

   CASE WM_KILLFOCUS
      RETURN ::LostFocus(nWParam) // LostFocus(), not KillFocus()!!!

   CASE WM_LBUTTONDOWN
      RETURN ::LButtonDown(HiWord(nLParam), LoWord(nLParam), nWParam)

   CASE WM_LBUTTONUP
      RETURN ::LButtonUp(HiWord(nLParam), LoWord(nLParam), nWParam)

   CASE WM_MOUSEMOVE
      RETURN ::MouseMove(HiWord(nLParam), LoWord(nLParam), nWParam)

   CASE WM_RBUTTONDOWN
      RETURN ::RButtonDown(HiWord(nLParam), LoWord(nLParam), nWParam)

   CASE WM_RBUTTONUP
      RETURN ::RButtonUp(HiWord(nLParam), LoWord(nLParam), nWParam)

   CASE WM_SETFOCUS
      RETURN ::GotFocus(nWParam)

   CASE WM_VSCROLL
      RETURN ::VScroll(nWParam, nLParam)

   CASE WM_SIZE
      RETURN ::ReSize(nWParam, LoWord(nLParam), HiWord(nLParam))

   CASE WM_TIMER
      RETURN ::Timer(nWParam, nLParam)

   CASE WM_ASYNCSELECT
      RETURN ::AsyncSelect(nWParam, nLParam)

   ENDSWITCH

RETURN 0

METHOD TControl:Command(nWParam, nLParam)

   LOCAL nNotifyCode
   LOCAL hWndCtl

   nNotifyCode := HiWord(nWParam)
//   nID         := LoWord(nWParam)
   hWndCtl     := nLParam

   DO CASE

   CASE hWndCtl == 0

      // TGet Enter ......................................
      IF HiWord(nWParam) == 0 .AND. LoWord(nWParam) == 1
         ::KeyDown(VK_RETURN, 0)
      ENDIF
      // TGet Escape .....................................
      IF HiWord(nwParam) == 0 .AND. LoWord(nwParam) == 2
         ::KeyDown(VK_ESCAPE, 0)
      ENDIF

   CASE hWndCtl != 0

      SWITCH nNotifyCode
      CASE CBN_KILLFOCUS ; ::LostFocus() ; EXIT
      CASE NM_KILLFOCUS  ; ::LostFocus() ; EXIT
      CASE EN_KILLFOCUS  ; ::LostFocus()
//    CASE EN_UPDATE     ; ::KeyDown(VK_RETURN, 0)
      ENDSWITCH

   ENDCASE

RETURN NIL

METHOD TControl:Notify(nWParam, nLParam)

   HB_SYMBOL_UNUSED(nWParam)

//   nNotifyCode := GetNotifyCode(nLParam)
//   hWndCtl     := GetHwndFrom(nLParam)

   IF GetNotifyCode(nLParam) == NM_KILLFOCUS
      ::LostFocus()
   ENDIF

RETURN NIL
