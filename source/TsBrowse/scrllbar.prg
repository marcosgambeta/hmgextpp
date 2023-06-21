*==============================================================================
* SCRLLBAR.PRG Version 5.0 27/Feb/2002
* Is an adaptation of FiveTech TScrollBar Class to be used with TSBrowse
*==============================================================================

#include "minigui.ch"
#include <hbclass.ch>
#include "TSBrowse.ch"

#define SB_HORZ         0
#define SB_VERT         1
#define SB_CTL          2

#define COLOR_SCROLLBAR 0

CLASS TSBScrlBar FROM TControl

   DATA lVertical
   DATA lReDraw
   DATA lIsChild
   DATA nMin
   DATA nMax
   DATA nPgStep
   DATA bGoUp
   DATA bGoDown
   DATA bGoTop
   DATA bGoBottom
   DATA bPageUp
   DATA bPageDown
   DATA bPos
   DATA bTrack
   DATA l32Bit
   DATA lShowDisabled
   DATA hWnd
   DATA oWnd
   DATA lUpdate AS LOGICAL                   // TControl
   DATA bWhen                                // TWindow
   DATA bValid                               // TWindow

   CLASSDATA aProperties INIT {"cVarName", "nMin", "nMax", "nPgStep", "nTop", "nLeft", "Cargo"}

   METHOD New(nRow, nCol, nMin, nMax, nPgStep, lVertical, oWnd, nWidth, nHeight, bUpAct, bDownAct, bPgUp, bPgDown, bPos, lPixel, nClrText, nClrBack, cMsg, lUpdate, bWhen, bValid, lDesign) CONSTRUCTOR
   METHOD WinNew(nMin, nMax, nPgStep, lVertical, oWnd, bUpAction, bDownAction, bPgUp, bPgDown, bPos, nClrText, nClrBack, lUpdate, bWhen, bValid) CONSTRUCTOR
   METHOD GetPos() INLINE GetScrollPos(IIf(::lIsChild, ::oWnd:hWnd, ::hWnd), IIf(::lIsChild, IIf(::lVertical, SB_VERT, SB_HORZ), SB_CTL))
   METHOD GetRange() INLINE GetScrlRange(IIf(::lIsChild, ::oWnd:hWnd, ::hWnd), IIf(::lIsChild, IIf(::lVertical, SB_VERT, SB_HORZ), SB_CTL))
   METHOD HandleEvent(nMsg, nWParam, nLParam)

//   METHOD Initiate( hDlg ) INLINE  ::Super:Initiate( hDlg ), Tone(500), ;
//                               ::SetRange(::nMin, ::nMax),;
//                               ::SetPos(::nMin)

   // These two have to be BLOCK

   METHOD GoUp()     BLOCK {|Self, nPos|nPos := ::GetPos(), iif(nPos > ::GetRange()[1], ::SetPos(--nPos), NIL), IIf(::bGoUp != NIL, Eval(::bGoUp), NIL)}
   METHOD GoDown()   BLOCK {|Self, nPos|nPos := ::GetPos(), iif(nPos < ::nMax, ::SetPos(++nPos), NIL), IIf(::bGoDown != NIL, Eval(::bGoDown), NIL)}
   METHOD GoTop()    INLINE ::SetPos(::nMin), IIf(::bGoTop != NIL, Eval(::bGoTop), NIL)
   METHOD GoBottom() INLINE ::SetPos(::nMax), IIf(::bGoBottom != NIL, Eval(::bGoBottom), NIL)

   METHOD PageUp()   INLINE IIf(::bPageUp != NIL, Eval(::bPageUp), NIL), ::SetPos(::GetPos() - ::nPgStep)
   METHOD PageDown() INLINE IIf(::bPageDown != NIL, Eval(::bPageDown), NIL), ::SetPos(::GetPos() + ::nPgStep)

   METHOD SetMode(l32Bit)

   METHOD SetPage(nSize, lReDraw)

   METHOD SetPos( nPos ) INLINE SetScrollPos( IIf(::lIsChild, ::oWnd:hWnd, ;
                                  ::hWnd), IIf(::lIsChild, IIf(::lVertical, ;
                                  SB_VERT, SB_HORZ), SB_CTL), ;
                                  nPos, ::lReDraw, ::lShowDisabled, ;
                                  ::l32Bit )

   METHOD SetRange(nMin, nMax) INLINE ::nMin := nMin, ::nMax := nMax, ;
           SetScrollRange(iif(::lIsChild, ::oWnd:hWnd, ::hWnd), ;
               IIf(::lIsChild, IIf(::lVertical, SB_VERT, SB_HORZ), SB_CTL), ;
                   nMin, nMax, ::lReDraw, ::lShowDisabled, ::l32Bit)   // JP 74

   METHOD ThumbPos( nPos ) INLINE  IIf(::bPos != NIL, Eval(::bPos, nPos), NIL)

   METHOD MouseMove(nRow, nCol, nKeyFlags)

   METHOD ThumbTrack( nPos ) INLINE IIf(::bTrack != NIL, Eval(::bTrack, nPos), ::ThumbPos(nPos))

ENDCLASS

* ============================================================================
* METHOD TSBScrlBar:New() Version 5.0 27/Feb/2002
* ============================================================================

METHOD New( nRow, nCol, nMin, nMax, nPgStep, lVertical, oWnd, nWidth, nHeight,;
            bUpAct, bDownAct, bPgUp, bPgDown, bPos, lPixel, nClrText,;
            nClrBack, cMsg, lUpdate, bWhen, bValid, lDesign ) CLASS TSBScrlBar

   Default nRow := 0, nCol := 0,;
           nMin := 0, nMax := 0, nPgStep := 1,;
           lVertical := .T., nWidth := IIf(lVertical, 16, 100),;
           nHeight   := IIf(lVertical, 100, 17),;
           lPixel    := .F.,;
           nClrText  := GetSysColor( COLOR_WINDOW ),;
           nClrBack  := GetSysColor( COLOR_SCROLLBAR ),;
           lUpdate   := .F., lDesign := .F.

   ::cCaption   := ""
   ::nTop       := nRow * IIf(lPixel, 1, SCRL_CHARPIX_H) //14
   ::nLeft      := nCol * IIf(lPixel, 1, SCRL_CHARPIX_W)   // 8
   ::nBottom    := ::nTop + nHeight - 1
   ::nRight     := ::nLeft + nWidth - 1
   ::nMin       := nMin
   ::nMax       := nMax
   ::nPgStep    := nPgStep
   ::lVertical  := lVertical
   ::lReDraw    := .T.
   ::nStyle     := nOr( WS_CHILD, WS_VISIBLE, WS_TABSTOP,;
                       iif(lVertical, SBS_VERT, SBS_HORZ),;
                       iif(lDesign, WS_CLIPSIBLINGS, 0) )
   ::bGoUp      := bUpAct
   ::bGoDown    := bDownAct
   ::bPageUp    := bPgUp
   ::bPageDown  := bPgDown
   ::bPos       := bPos
   ::oWnd       := oWnd
   ::lIsChild   := .F.
   ::lDrag      := lDesign
   ::lCaptured  := .F.
   ::cMsg       := cMsg
   ::lUpdate    := lUpdate
   ::bWhen      := bWhen
   ::bValid     := bValid

   if !Empty(::oWnd:hWnd)
      ::Create("SCROLLBAR")
      ::SetRange(::nMin, ::nMax)
      ::SetPos(::nMin)
   EndIf

   if lDesign
      ::CheckDots()
   EndIf

Return Self

* ============================================================================
* METHOD TSBScrlBar:WinNew() Version 5.0 27/Feb/2002
* Constructor for non-true ScrollBar Controls
* ( when using WS_VSCROLL, WS_HSCROLL styles in a Window )
* They are NOT controls but we consider them as real Objects!
* ============================================================================

METHOD WinNew( nMin, nMax, nPgStep, lVertical, oWnd, bUpAction,;
               bDownAction, bPgUp, bPgDown, bPos, nClrText, nClrBack,;
               lUpdate, bWhen, bValid ) CLASS TSBScrlBar

   Default nMin := 1, nMax := 2, nPgStep := 1, lVertical := .T.,;
           nClrText  := GetSysColor( COLOR_WINDOW ),;
           nClrBack  := GetSysColor( COLOR_SCROLLBAR ),;
           lUpdate   := .F.

   ::oWnd          := oWnd
   ::lVertical     := lVertical
   ::lReDraw       := .T.
   ::lIsChild      := .T.
   ::nMin          := nMin
   ::nMax          := nMax
   ::nPgStep       := nPgStep
   ::bGoUp         := bUpAction
   ::bGoDown       := bDownAction
   ::bPageUp       := bPgUp
   ::bPageDown     := bPgDown
   ::bPos          := bPos
   ::lUpdate       := lUpdate
   ::bWhen         := bWhen
   ::bValid        := bValid
   ::hWnd          := 0
   ::lShowDisabled := .F.
   ::l32Bit        := ::SetMode()
   
   ::SetPage(::nPgStep, .T.)
   ::SetRange(nMin, nMax)
   ::SetPos( nMin )

Return Self

* ============================================================================
* METHOD TSBScrlBar:HandleEvent() Version 5.0 27/Feb/2002
* ============================================================================

METHOD HandleEvent(nMsg, nWParam, nLParam) CLASS TSBScrlBar

   SWITCH nMsg

   CASE FM_SCROLLUP
      ::GoUp()
      Return 0

   CASE FM_SCROLLDOWN
      ::GoDown()
      Return 0

   CASE FM_SCROLLPGUP
      ::PageUp()
      Return 0

   CASE FM_SCROLLPGDN
      ::PageDown()
      Return 0

   CASE FM_THUMBPOS
      ::ThumbPos( nWParam )
      Return 0

   CASE FM_THUMBTRACK
      ::ThumbTrack( nWParam )
      Return 0

   ENDSWITCH

Return ::Super:HandleEvent(nMsg, nWParam, nLParam)

* ============================================================================
* METHOD TSBScrlBar:MouseMove() Version 5.0 27/Feb/2002
* ============================================================================

METHOD MouseMove(nRow, nCol, nKeyFlags) CLASS TSBScrlBar

   Local nResult := ::Super:MouseMove(nRow, nCol, nKeyFlags)

Return IIf(::lDrag, nResult, NIL)    // We want standard behavior !!!

* ============================================================================
* METHOD TSBScrlBar:SetPage() Version 5.0 27/Feb/2002
* ============================================================================

METHOD SetPage(nSize, lReDraw) CLASS TSBScrlBar

   Local nFlags

   Default lRedraw := .F.

   if !Empty(::hWnd)
      nFlags = SB_CTL
   Else
      if ::lVertical
         nFlags = SB_VERT
      Else
         nFlags = SB_HORZ
      EndIf
   EndIf

   ::nPgStep := nSize

   SetScrollInfo(IIf(!Empty(::hWnd), ::oWnd:hWnd, ::hWnd), nFlags, nSize, lReDraw, ::lShowDisabled)

Return NIL

* ============================================================================
* METHOD TSBScrlBar:SetMode() Version 5.0 27/Feb/2002
* ============================================================================

METHOD SetMode(l32Bit) CLASS TSBScrlBar

   LOCAL lWin32 := .T. //IsWin95()  // W2K also returns TRUE. NT 4 doesn't support SetScrollInfo

   Default l32Bit := .T.

Return iif(l32Bit .AND. !lWin32, .F., l32Bit)
