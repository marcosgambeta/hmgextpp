//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
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

#include "hmg.ch"
#include "i_winuser.ch"

STATIC _lTabKeyPressed := .F.
STATIC _lShiftTabKeyPressed := .F.

/*
_DefineGrid(...) --> NIL
*/
FUNCTION _DefineGrid(ControlName, ParentFormName, ;
      x, y, w, h, ;
      aHeaders, aWidths, ;
      aRows, value, ;
      fontname, fontsize, tooltip, ;
      change, dblclick, aHeadClick, ;
      gotfocus, lostfocus, ;
      nogrid, aImage, aJust, ;
      break, HelpId, ;
      bold, italic, underline, strikeout, ;
      ownerdata, ondispinfo, itemcount, editable, ;
      dynamicforecolor, dynamicbackcolor, ;
      multiselect, ;
      editcontrols, ;
      backcolor, fontcolor, ;
      nId, ;
      columnvalid, columnwhen, validmessages, ;
      showheaders, aImageHeader, ;
      NoTabStop, ;
      celled, ;
      lCheckboxes, lockcolumns, ;
      OnCheckBoxClicked, doublebuffer, nosortheaders, ;
      columnsort, aWidthLimits, ondragitems, ;
      bInit, autosizeH, autosizeW, ;
      rclick, OnInplaceEditEvent)

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL nHeaderImageListHandle := 0
   LOCAL blInit
   LOCAL mVar
   LOCAL Style
   LOCAL i
   LOCAL k
   LOCAL inplace
   LOCAL lsort
   LOCAL lDialogInMemory
   LOCAL oc // := NIL
   LOCAL ow // := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default(@w, 240)
   hb_default(@h, 120)
   hb_default(@showheaders, .T.)
   hb_default(@nosortheaders, .F.)
   __defaultNIL(@aImage, {})
   __defaultNIL(@aImageHeader, {})
   __defaultNIL(@aHeadClick, {})
   __defaultNIL(@change, "")
   __defaultNIL(@dblclick, "")
   __defaultNIL(@rclick, "")
   hb_default(@notabstop, .F.)
   hb_default(@lockcolumns, 0)
   hb_default(@doublebuffer, .F.)
   hb_default(@lcheckboxes, .F.)
   hb_default(@autosizeW, .F.)

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError(_HMG_BRWLangError[1] + iif(ParentFormName == NIL, "Parent", ParentFormName) + _HMG_BRWLangError[2], .F.)
   ENDIF
   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError(_HMG_BRWLangError[4] + ControlName + _HMG_BRWLangError[5] + ParentFormName + _HMG_BRWLangError[6], .F.)
   ENDIF
   IF Len(aHeadClick) > 0
      FOR EACH mVar IN aHeadClick
         IF !(mVar == NIL) .AND. !hb_IsBlock(mVar)
            MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + ": ON HEADCLICK event must be defined as array of the codeblocks.")
         ENDIF
      NEXT
   ENDIF
   /* Dr. Claudio Soto, April 2016 */
   #define DEFAULT_COLUMNHEADER  "Column "
   #define DEFAULT_COLUMNWIDTH   150
   IF hb_IsArray(aRows) .AND. Len(aRows) > 0
      IF aHeaders == NIL .AND. aWidths == NIL
         aHeaders := Array(Len(aRows[1]))
         aWidths  := Array(Len(aRows[1]))
         AEval(aHeaders, {|xValue, nIndex| xValue := NIL, aHeaders[nIndex] := DEFAULT_COLUMNHEADER + hb_ntos(nIndex)})
         AFill(aWidths,  DEFAULT_COLUMNWIDTH)
      ELSEIF hb_IsArray(aHeaders) .AND. aWidths == NIL
         aWidths  := Array(Len(aHeaders))
         AFill(aWidths,  DEFAULT_COLUMNWIDTH)
         IF Len(aImage) > 0
            aWidths[1] := 0
         ENDIF
      ELSEIF aHeaders == NIL .AND. hb_IsArray(aWidths)
         aHeaders := Array(Len(aWidths))
         AEval(aHeaders, {|xValue, nIndex|xValue := NIL, aHeaders[nIndex] := DEFAULT_COLUMNHEADER + hb_ntos(nIndex)})
      ENDIF
   ELSE
      IF aHeaders == NIL .AND. aWidths == NIL
         aHeaders := {""}
         aWidths  := {0}
      ELSEIF hb_IsArray(aHeaders) .AND. aWidths == NIL
         aWidths  := Array(Len(aHeaders))
         AFill(aWidths,  DEFAULT_COLUMNWIDTH)
         IF Len(aImage) > 0
            aWidths[1] := 0
         ENDIF
      ELSEIF aHeaders == NIL .AND. hb_IsArray(aWidths)
         aHeaders := Array(Len(aWidths))
         AEval(aHeaders, {|xValue, nIndex|xValue := NIL, aHeaders[nIndex] := DEFAULT_COLUMNHEADER + hb_ntos(nIndex)})
      ENDIF
   ENDIF
   /* end code borrowed */
   __defaultNIL(@aRows, {})

   IF !showheaders
      aHeaders := AFill(Array(Len(aWidths)), "")
   ENDIF
   IF value == NIL .AND. !MultiSelect
      value := 0
   ENDIF
   /* code borrowed from ooHG project */
   IF !hb_IsArray(aJust)
      aJust := AFill(Array(Len(aHeaders)), 0)
   ELSE
      ASize(aJust, Len(aHeaders))
      AEval(aJust, {|x, i|aJust[i] := iif(hb_IsNumeric(x), x, 0)})
   ENDIF
   /* end code borrowed */
   inplace := hb_IsArray(editcontrols)
   lsort := (hb_IsArray(columnsort) .AND. !nosortheaders .AND. !ownerdata)

   IF !hb_IsArray(aWidthLimits)
      aWidthLimits := Array(Len(aWidths))
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle
      Style := WS_BORDER + LVS_SHOWSELALWAYS + WS_CHILD + WS_VISIBLE + LVS_REPORT

      IF !NoTabStop
         Style += WS_TABSTOP
      ENDIF

      IF !multiselect
         Style += LVS_SINGLESEL
      ENDIF

      IF ownerdata
         Style += LVS_OWNERDATA
      ENDIF

      IF !showheaders
         Style += LVS_NOCOLUMNHEADER
      ELSEIF nosortheaders
         Style += LVS_NOSORTHEADER
      ENDIF

      IF Len(_HMG_aDialogTemplate) > 0     //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z|InitDialogGrid(x, y, z)}
         AAdd(_HMG_aDialogItems, {nId, k, "SysListView32", style, 0, x, y, w, h, "", HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, , _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := hmg_GetDialogItemHandle(ParentFormHandle, nId)

         hmg_SetWindowStyle(ControlHandle, Style, .T.)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         IF ownerdata
            hmg_LISTVIEW_SETITEMCOUNT(ControlHandle, itemcount)
         ENDIF

         hmg_SendMessage(ControlHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, iif(nogrid, 0, 1) + LVS_EX_FULLROWSELECT + LVS_EX_INFOTIP + LVS_EX_HEADERDRAGDROP + iif(lCheckboxes, LVS_EX_CHECKBOXES, 0))

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      IF x == NIL .OR. y == NIL

         IF _HMG_SplitLastControl == "TOOLBAR"
            Break := .T.
         ENDIF

         _HMG_SplitLastControl := "GRID"

         i := GetFormIndex(ParentFormName)

         IF i > 0

            x := y := 0
            ControlHandle := hmg_InitListView(_HMG_aFormReBarHandle[i], 0, x, y, w, h, ownerdata, itemcount, multiselect, showheaders, nosortheaders, NoTabStop)

            hmg_AddSplitBoxItem(Controlhandle, _HMG_aFormReBarHandle[i], w, break, , , , _HMG_ActiveSplitBoxInverted)

         ENDIF

      ELSE

         ControlHandle := hmg_InitListView(ParentFormHandle, 0, x, y, w, h, ownerdata, itemcount, multiselect, showheaders, nosortheaders, NoTabStop)

      ENDIF

   ENDIF

   IF !lDialogInMemory
      IF lsort
         aImageHeader := {"MINIGUI_GRID_ASC", "MINIGUI_GRID_DSC"}
         aHeadClick := Array(Len(aHeaders))
         AEval(aHeadClick, {|x, i|aHeadClick[i] := {|n|HMG_SortColumn(n)}, HB_SYMBOL_UNUSED(x)})
      ENDIF

      IF Len(aImageHeader) > 0
         nHeaderImageListHandle := hmg_AddListViewBitmapHeader(ControlHandle, aImageHeader)
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
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

      IF tooltip != NIL
         hmg_SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := iif(multiselect, CONTROL_TYPE_MULTIGRID, CONTROL_TYPE_GRID)
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ondispinfo
   _HMG_aControlPageMap            [k] := aHeaders
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := NIL
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := aImage
   _HMG_aControlFontColor          [k] := celled
   _HMG_aControlDblClick           [k] := dblclick
   _HMG_aControlHeadClick          [k] := aHeadClick
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := Editable
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := aImageHeader
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := nHeaderImageListHandle
   _HMG_aControlRangeMax           [k] := iif(lsort, Array(Len(aHeaders)), 0)
   _HMG_aControlCaption            [k] := aHeaders
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := {0, aWidths, aJust, aRows, ;
                                           ownerdata, itemcount, nogrid, ;
                                           backcolor, fontcolor, ;
                                           multiselect, ;
                                           dynamicforecolor, dynamicbackcolor, ;
                                           editcontrols, ;
                                           columnvalid, columnwhen, validmessages, ;
                                           1, lcheckboxes, lockcolumns, ;
                                           inplace, NIL, NIL, ;
                                           OnCheckBoxClicked, ;
                                           doublebuffer, aWidthLimits, ondragitems, ;
                                           autosizeH, autosizeW, rclick, OnInplaceEditEvent ;
                                         }
   _HMG_aControlMiscData2          [k] := ""

   IF Len(_HMG_aDialogTemplate) == 0   //Dialog Template
      IF lsort
         AFill(_HMG_aControlRangeMax[k], 1)
         IF Len(columnsort) > 0
            FOR i := 1 TO Min(Len(columnsort), Len(_HMG_aControlRangeMax[k]))
               IF hb_IsNumeric(columnsort[i])
                  _HMG_aControlRangeMax[k][i] := columnsort[i]
               ENDIF
            NEXT i
         ENDIF
      ENDIF

      InitDialogGrid(ParentFormName, ControlHandle, k)
   ENDIF

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure(bInit, k, ow, oc)

RETURN NIL

/*
InitDialogGrid(ParentName, ControlHandle, k) --> NIL
*/
FUNCTION InitDialogGrid(ParentName, ControlHandle, k)

   LOCAL ControlName
   LOCAL Value
   LOCAL aWidths
   LOCAL aJust
   LOCAL nogrid
   LOCAL lcheckboxes
   LOCAL lockcolumns
   LOCAL doublebuffer
   LOCAL ownerdata
   LOCAL itemcount
   LOCAL fontcolor
   LOCAL backcolor
   LOCAL aRows
   LOCAL multiselect
   LOCAL row
   LOCAL wBitmap
   LOCAL z
   LOCAL autosizeH
   LOCAL autosizeW

   ControlName  := _HMG_aControlNames[k]
   Value        := _HMG_aControlValue[k]
   aWidths      := _HMG_aControlMiscData1[k, 2]
   aJust        := _HMG_aControlMiscData1[k, 3]
   aRows        := _HMG_aControlMiscData1[k, 4]
   ownerdata    := _HMG_aControlMiscData1[k, 5]
   itemcount    := _HMG_aControlMiscData1[k, 6]
   nogrid       := _HMG_aControlMiscData1[k, 7]
   backcolor    := _HMG_aControlMiscData1[k, 8]
   fontcolor    := _HMG_aControlMiscData1[k, 9]
   multiselect  := _HMG_aControlMiscData1[k, 10]
   lcheckboxes  := _HMG_aControlMiscData1[k, 18]
   lockcolumns  := _HMG_aControlMiscData1[k, 19]
   doublebuffer := _HMG_aControlMiscData1[k, 24]
   autosizeH    := _HMG_aControlMiscData1[k, 27]
   autosizeW    := _HMG_aControlMiscData1[k, 28]

   IF ownerdata
      hmg_LISTVIEW_SETITEMCOUNT(ControlHandle, itemcount)
   ENDIF

   hmg_SendMessage(ControlHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, iif(nogrid, 0, LVS_EX_GRIDLINES) + iif(doublebuffer, LVS_EX_DOUBLEBUFFER, 0) + ;
      LVS_EX_FULLROWSELECT + LVS_EX_INFOTIP + iif(lockcolumns > 0, 0, LVS_EX_HEADERDRAGDROP) + iif(lcheckboxes, LVS_EX_CHECKBOXES, 0))

   IF IsArrayRGB(backcolor)
      hmg_ListView_SetBkColor(ControlHandle, backcolor[1], backcolor[2], backcolor[3])
      hmg_ListView_SetTextBkColor(ControlHandle, backcolor[1], backcolor[2], backcolor[3])
   ENDIF

   IF IsArrayRGB(fontcolor)
      hmg_ListView_SetTextColor(ControlHandle, fontcolor[1], fontcolor[2], fontcolor[3])
   ENDIF

   wBitmap := iif(Len(_HMG_aControlBkColor[k]) > 0, hmg_AddListViewBitmap(ControlHandle, _HMG_aControlBkColor[k]), 0) // Add Bitmap Column
   aWidths[1] := Max(aWidths[1], wBitmap + GetBorderWidth() / 2)  // Set Column 1 width to Bitmap width

   hmg_InitListViewColumns(ControlHandle, _HMG_aControlCaption[k], aWidths, aJust)

   AEval(aRows, {|r|_AddGridRow(ControlName, ParentName, r)})

   IF hb_IsNumeric(autosizeH) .OR. autosizeW

      IF ListView_CalculateSize(ControlHandle, autosizeH, @z, @row) > 49

         IF hb_IsNumeric(autosizeH)
            SetProperty(ParentName, ControlName, "Height", row)
         ENDIF
         IF autosizeW
            SetProperty(ParentName, ControlName, "Width", z)
         ENDIF

      ENDIF

   ENDIF

   IF Len(_HMG_aControlPicture[k]) == Len(_HMG_aControlPageMap[k])

      FOR z := 1 TO Len(_HMG_aControlPageMap[k])
         hmg_SetGridColumnHeaderImage(_HMG_aControlHandles[k], z, z, (aJust[z] == 1))
      NEXT z

   ENDIF

   IF multiselect

      IF hb_IsArray(Value)
         hmg_ListViewSetMultiSel(ControlHandle, Value)
      ENDIF

   ELSE

      row := iif(hb_IsArray(value), value[1], value)
      IF row != 0
         _SetValue(, , Value, k)
      ENDIF

   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

   _UpdateGridColors(k)

RETURN NIL

/*
ListView_CalculateSize(hLV, nNumberOfRows, nWidth, nHeight) --> nHeight
*/
FUNCTION ListView_CalculateSize(hLV, nNumberOfRows, /*@*/nWidth, /*@*/nHeight)

   LOCAL nResult

   DEFAULT nNumberOfRows := -1
   // Collect and/or adjust the number of rows
   IF nNumberOfRows < 0
      nNumberOfRows := hmg_SendMessage(hLV, LVM_GETITEMCOUNT, 0, 0)
   ENDIF

   IF nNumberOfRows > 0  // Not zero
       nNumberOfRows -= 1
   ENDIF
   // Calculate size
   nResult := hmg_SendMessage(hLV, LVM_APPROXIMATEVIEWRECT, nNumberOfRows, hmg_MAKELONG(-1, -1))
   // Extract, adjust, and return values
   nWidth  := hmg_LOWORD(nResult) + 4
   nHeight := hmg_HIWORD(nResult) + 4

RETURN nHeight

/*
_AddGridRow(ControlName, ParentForm, aRow) --> NIL
*/
FUNCTION _AddGridRow(ControlName, ParentForm, aRow)

   LOCAL aGridRow := AClone(aRow)
   LOCAL i
   LOCAL h
   LOCAL iIm := 0

   i := GetControlIndex(ControlName, ParentForm)

   IF Len(_HMG_aControlPageMap[i]) != Len(aRow)
      MsgMiniGuiError("Grid.AddItem: Item size mismatch.")
   ENDIF

   IF Len(_HMG_aControlBkColor[i]) > 0
      iIm := aGridRow[1]
      aGridRow[1] := NIL
   ENDIF

#ifdef _HMG_COMPAT_
   IF !hb_IsArray(_HMG_aControlMiscData1[i][13])
      AEval(aGridRow, {|x, i|iif(hb_IsString(x) .OR. HB_ISNIL(x), , aGridRow[i] := hb_ValToStr(x))})
   ENDIF
#endif

   h := _HMG_aControlHandles[i]
   hmg_AddListViewItems(h, aGridRow, iIm)

   IF hb_IsArray(_HMG_aControlMiscData1[i][13])
      _SetItem(ControlName, ParentForm, hmg_ListViewGetItemCount(h), aGridRow)
      IF Len(_HMG_aControlBkColor[i]) > 0
         hmg_SetImageListViewItems(h, hmg_ListViewGetItemCount(h), iIm)
      ENDIF
   ENDIF

RETURN NIL

/*
HMG_SortColumn(nColumnNo)
*/
PROCEDURE HMG_SortColumn(nColumnNo)

   LOCAL cFormName := ThisWindow.Name
   LOCAL cControlName := This.Name
   LOCAL ix := GetControlIndex(cControlName, cFormName)
   LOCAL i
   LOCAL nOrder
   LOCAL lAscend
   LOCAL nCount
   LOCAL aImages
   LOCAL aItems := {}

   IF ix > 0 .AND. (nOrder := _HMG_aControlRangeMax[ix][nColumnNo]) > 0

      _EnableListViewUpdate(cControlName, cFormName, .F.)

      aImages := Array(Len(_HMG_aControlRangeMax[ix]))
      nCount := hmg_ListViewGetItemCount(_HMG_aControlHandles[ix])

      FOR i := 1 TO nCount
         AAdd(aItems, GetProperty(cFormName, cControlName, "Item", i))
      NEXT i

      lAscend := (nOrder < 2)
      IF lAscend
         ASort(aItems, , , {|x, y|x[nColumnNo] < y[nColumnNo]})
      ELSE
         ASort(aItems, , , {|x, y|x[nColumnNo] > y[nColumnNo]})
      ENDIF

      DoMethod(cFormName, cControlName, "DeleteAllItems")

      AEval(aItems, {|x|DoMethod(cFormName, cControlName, "AddItem", x)})

      AEval(aImages, {|x, i|aImages[i] := HDR_IMAGE_NONE, HB_SYMBOL_UNUSED(x)})

      aImages[nColumnNo] := iif(lAscend, HDR_IMAGE_ASCENDING, HDR_IMAGE_DESCENDING)

      AEval(aImages, {|n, i|_SetMultiImage(cControlName, cFormName, i, n, (_HMG_aControlMiscData1[ix][3][i] == 1))})

      _HMG_aControlRangeMax[ix][nColumnNo] := iif(lAscend, ++nOrder, 1)

      _UpdateGridColors(ix)

      _EnableListViewUpdate(cControlName, cFormName, .T.)

   ENDIF

RETURN

/*
_EditItem(GridHandle) --> NIL
*/
FUNCTION _EditItem(GridHandle)

   LOCAL a
   LOCAL l
   LOCAL g
   LOCAL IRow
   LOCAL actpos := {0, 0, 0, 0}
   LOCAL h := GridHandle
   LOCAL GRow
   LOCAL GCol
   LOCAL GWidth
   LOCAL Col
   LOCAL i
   LOCAL ControlName
   LOCAL j
   LOCAL FormName
   LOCAL item

   _HMG_ActiveFormNameBak := _HMG_ActiveFormName

   i := AScan(_HMG_aControlHandles, GridHandle) // TODO:

   a := _HMG_aControlPageMap[i]

   ControlName := _HMG_aControlNames[i]

   j := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])

   FormName := _HMG_aFormNames[j]

   item := _GetValue(ControlName, FormName)

   l := Len(a)

   g := _GetItem(ControlName, FormName, Item)

   IRow := hmg_ListViewGetItemRow(h, Item)

   hmg_GetWindowRect(h, actpos)

   GRow   := actpos[2] + IRow / 2
   GCol   := actpos[1]
   GWidth := actpos[3] - actpos[1]

   Col := GCol + ((GWidth - 260) / 2)

   DEFINE WINDOW _EditItem AT GRow, Col WIDTH 260 HEIGHT l * 30 + 70 + GetTitleHeight() ;
      TITLE _HMG_MESSAGE[5] MODAL NOSIZE

      FOR i := 1 TO l
         @ i * 30 - 17, 10 LABEL("Label_" + hb_ntos(i)) OF _EditItem VALUE AllTrim(a[i]) + ":"
         @ i * 30 - 20, 120 TEXTBOX("Text_" + hb_ntos(i)) OF _EditItem VALUE g[i]
      NEXT i

      @ l * 30 + 20, 20 BUTTON BUTTON_1 CAPTION _HMG_MESSAGE[6] ACTION {||_EditItemOk(ControlName, FormName, Item, l)}

      @ l * 30 + 20, 130 BUTTON BUTTON_2 CAPTION _HMG_MESSAGE[7] ACTION _EditItem.Release

   END WINDOW

   ACTIVATE WINDOW _EditItem

   _HMG_ActiveFormName := _HMG_ActiveFormNameBak

   _SetFocus(ControlName, FormName)

RETURN NIL

/*
_EditItemOk(ControlName, FormName, Item, l) --> NIL
*/
STATIC FUNCTION _EditItemOk(ControlName, FormName, Item, l)

   LOCAL a := Array(l)

   AEval(a, {|x, i|HB_SYMBOL_UNUSED(x), a[i] := GetProperty("_EditItem", "Text_" + hb_ntos(i), "Value")})

   _SetItem(ControlName, FormName, Item, a)

   _EditItem.Release

RETURN NIL

/*
_AddGridColumn(cControlName, cParentForm, nColIndex, cCaption, nWidth, nJustify)
*/
PROCEDURE _AddGridColumn(cControlName, cParentForm, nColIndex, cCaption, nWidth, nJustify)

   LOCAL atemp As Array
   LOCAL atemp2 As Array
   LOCAL i
   LOCAL x

   // Get Control Index
   i := GetControlIndex(cControlName, cParentForm)
   // Set Default Values
   hb_default(@nColIndex, Len(_HMG_aControlPageMap[i]) + 1)
   __defaultNIL(@cCaption, "")
   hb_default(@nWidth, 120)
   hb_default(@nJustify, 0)
   // Update Grid Object
   FOR x := 1 TO nColIndex - 1
      AAdd(atemp, _HMG_aControlPageMap[i][x])
      AAdd(atemp2, _HMG_aControlMiscData1[i][3][x])
   NEXT x

   AAdd(atemp, cCaption)
   AAdd(atemp2, nJustify)

   FOR x := nColIndex + 1 TO Len(_HMG_aControlPageMap[i]) + 1
      AAdd(atemp, _HMG_aControlPageMap[i][x-1])
      AAdd(atemp2, _HMG_aControlMiscData1[i][3][x-1])
   NEXT x

   _HMG_aControlPageMap[i] := atemp
   _HMG_aControlCaption[i] := atemp
   _HMG_aControlMiscData1[i][3] := atemp2
   ASize(_HMG_aControlMiscData1[i][25], Len(atemp))

   // Call C-Level Routine
   hmg_ListView_AddColumn(_HMG_aControlHandles[i], nColIndex, nWidth, cCaption, nJustify)

RETURN

/*
_DeleteGridColumn(cControlName, cParentForm, nColIndex)
*/
PROCEDURE _DeleteGridColumn(cControlName, cParentForm, nColIndex)

   LOCAL atemp As Array
   LOCAL atemp2 As Array
   LOCAL i
   LOCAL x

   // Get Control Index
   i := GetControlIndex(cControlName, cParentForm)
   // Update Grid Object
   FOR x := 1 TO nColIndex - 1
      AAdd(atemp, _HMG_aControlPageMap[i][x])
      AAdd(atemp2, _HMG_aControlMiscData1[i][3][x])
   NEXT x

   FOR x := nColIndex + 1 TO Len(_HMG_aControlPageMap[i])
      AAdd(atemp, _HMG_aControlPageMap[i][x])
      AAdd(atemp2, _HMG_aControlMiscData1[i][3][x])
   NEXT x

   _HMG_aControlPageMap[i] := atemp
   _HMG_aControlCaption[i] := atemp
   _HMG_aControlMiscData1[i][3] := atemp2
   hb_ADel(_HMG_aControlMiscData1[i][25], nColIndex, .T.)

   // Call C-Level Routine
   hmg_ListView_DeleteColumn(_HMG_aControlHandles[i], nColIndex)

RETURN

/*
_UpdateGridColors(i)
*/
PROCEDURE _UpdateGridColors(i)

   LOCAL dBc := _HMG_aControlMiscData1[i, 12]
   LOCAL dFc := _HMG_aControlMiscData1[i, 11]
   LOCAL processdbc := hb_IsArray(dbc)
   LOCAL processdfc := hb_IsArray(dfc)
   LOCAL h
   LOCAL Cols
   LOCAL Rows

   IF !processdbc .AND. !processdfc
      RETURN
   ENDIF

   h := _HMG_aControlHandles[i]
   Rows := hmg_ListViewGetItemCount(h)
   Cols := hmg_ListView_GetColumnCount(h)

   IF processdbc
      ProcessDynamicArray(i, Rows, Cols, dBc, 22)
   ENDIF
   IF processdfc
      ProcessDynamicArray(i, Rows, Cols, dFc, 21)
   ENDIF

   hmg_ReDrawWindow(h)

RETURN

/*
ProcessDynamicArray(i, Rows, Cols, Arr, item)
*/
STATIC PROCEDURE ProcessDynamicArray(i, Rows, Cols, Arr, item)

   LOCAL aValues
   LOCAL aTemp
   LOCAL r
   LOCAL c

   aTemp := Array(Rows, Cols)

   FOR r := 1 TO Rows

      IF _HMG_aControlMiscData1[i][5]

         aValues := _GetIVirtualItem(r, i, Cols)

      ELSE

         aValues := _GetItem(, , r, i)

      ENDIF

      FOR c := 1 TO Cols

         _HMG_ThisItemRowIndex := r
         _HMG_ThisItemColIndex := c

         _HMG_ThisItemCellValue := aValues[c]

         aTemp[r][c] := iif(hb_IsBlock(Arr[c]), _tEval(Arr[c], aValues, r), -1)

      NEXT c

   NEXT r

   _HMG_aControlMiscData1[i][item] := aTemp

RETURN

/*
_tEval(bBlock, Par1, Par2) --> numeric
*/
FUNCTION _tEval(bBlock, Par1, Par2)

   LOCAL tEval

   tEval := Eval(bBlock, Par1, Par2)

   IF IsArrayRGB(tEval)
      tEval := RGB(tEval[1], tEval[2], tEval[3])
   ENDIF

RETURN iif(hb_IsNumeric(tEval), tEval, 0)

/*
_GridInplaceEdit(idx) --> .T.|.F.
*/
FUNCTION _GridInplaceEdit(idx)

   LOCAL FormName := "_hmg_grid_inplaceedit"
   LOCAL ControlName
   LOCAL aEditControls
   LOCAL AEC := "TEXTBOX"
   LOCAL AITEMS
   LOCAL ARANGE
   LOCAL DTYPE
   LOCAL ALABELS := {".T.", ".F."}
   LOCAL CTYPE
   LOCAL CINPUTMASK
   LOCAL CFORMAT
   LOCAL XRES
   LOCAL CWH
   LOCAL WHEN
   LOCAL abKeyBlocks := {}
   LOCAL aTemp
   LOCAL DH := 0
   LOCAL DR := 0
   LOCAL r
   LOCAL c
   LOCAL v
   LOCAL h
   LOCAL ri
   LOCAL ci
   LOCAL Save
   LOCAL bChange
   LOCAL bBlock

   IF _HMG_ThisEventType == "GRID_WHEN"
      MsgMiniGuiError("GRID: Editing within WHEN event procedure is not allowed.")
   ELSEIF _HMG_ThisEventType == "GRID_VALID"
      MsgMiniGuiError("GRID: Editing within VALID event procedure is not allowed.")
   ENDIF

   IF _HMG_aControlFontColor[idx]
      IF This.CellRowIndex != _HMG_aControlMiscData1[idx][1]
         RETURN .F.
      ENDIF
   ELSE
      IF This.CellRowIndex != hmg_LISTVIEW_GETFIRSTITEM(_hmg_acontrolhandles[idx])
         RETURN .F.
      ENDIF
   ENDIF

   ri := This.CellRowIndex
   ci := This.CellColIndex

   IF ri == 0 .OR. ci == 0
      RETURN .F.
   ENDIF

   IF ci == 1 .AND. Len(_HMG_aControlBkColor[idx]) > 0
      HMG_PressKey(VK_RIGHT, VK_RETURN)
      _HMG_IPE_CANCELLED := .F.
      RETURN .F.
   ENDIF

   _HMG_GridInplaceEdit_GridIndex := idx

   IF !_HMG_aControlMiscData1[idx][5]
      aTemp := This.Item(ri)
      v := aTemp[ci]
   ELSE
      _HMG_ThisQueryRowIndex := ri
      _HMG_ThisQueryColIndex := ci
      Eval(_HMG_aControlProcedures[idx])
      v := _HMG_ThisQueryData
   ENDIF

   Save := v

   CWH := _HMG_aControlMiscData1[idx][15]

   IF hb_IsArray(CWH) .AND. Len(CWH) >= ci

      IF hb_IsBlock(CWH[ci])

         _HMG_ThisItemCellValue := v

         _HMG_ThisEventType := "GRID_WHEN"

         WHEN := Eval(CWH[ci])

         _HMG_ThisEventType := ""

         IF hb_IsLogical(WHEN) .AND. !WHEN
            _HMG_IPE_CANCELLED := .F.
            RETURN .F.
         ENDIF

      ENDIF

   ENDIF

   h := _HMG_aControlHandles[idx]

   r := This.CellRow + GetWindowRow(h) - This.Row - 1

   IF _HMG_aControlContainerRow[idx] != -1
      r -= _HMG_aControlContainerRow[idx]
   ENDIF

   c := This.CellCol + GetWindowCol(h) - This.Col + 2

   IF _HMG_aControlContainerCol[idx] != -1
      c -= _HMG_aControlContainerCol[idx]
   ENDIF

   aEditControls := _HMG_aControlMiscData1[idx][13]

   XRES := _ParseGridControls(aEditControls, ci, ri)

   AEC        := XRES[1]
   CTYPE      := XRES[2]
   CINPUTMASK := XRES[3]
   CFORMAT    := XRES[4]
   AITEMS     := XRES[5]
   ARANGE     := XRES[6]
   DTYPE      := XRES[7]
   ALABELS    := XRES[8]
   bChange    := XRES[9]
   bBlock     := XRES[10]

   IF hb_IsBlock(bBlock)
      aTemp[ci] := (v := Eval(bBlock, v))
      IF !_HMG_aControlMiscData1[idx][5]
         _SetItem(, , ri, aTemp, idx)
      ENDIF
      IF Save != hb_ValToStr(v)
         Eval(bChange, v)
      ENDIF
      RETURN .T.
   ENDIF

   IF AEC == "COMBOBOX"
      DH := 1
   ELSEIF AEC == "CHECKBOX"
      DR := 3
      DH := -7
   ENDIF

   _HMG_InplaceParentHandle := hmg_GetActiveWindow()

   DEFINE WINDOW _hmg_grid_inplaceedit ;
      AT r + DR, c ;
      WIDTH This.CellWidth ;
      HEIGHT This.CellHeight + 6 + DH ;
      TITLE "" MODAL ;
      NOSIZE ;
      NOCAPTION

   ON KEY ESCAPE    ACTION (_HMG_IPE_CANCELLED := .T., ThisWindow.Release)

   ON KEY RETURN    ACTION iif(_IsWindowActive(FormName), (_HMG_IPE_CANCELLED := .F., _GridInplaceEditOK(IDX, CI, RI, AEC)), NIL)

   ON KEY TAB       ACTION (_lTabKeyPressed := .T., InsertReturn())

   ON KEY SHIFT+TAB ACTION iif(_HMG_GridNavigationMode, , (_lShiftTabKeyPressed := .T., ThisWindow.Release, InsertReturn()))

   ControlName := Left(AEC, 1)

   IF AEC == "TEXTBOX"

      DEFINE TEXTBOX T
        ROW      0
        COL      0
        WIDTH    This.CellWidth
        HEIGHT   This.CellHeight + 6
        IF CTYPE == "NUMERIC"
           NUMERIC .T.
        ELSEIF CTYPE == "DATE"
           DATE .T.
        ENDIF
        VALUE    v
        FONTNAME _hmg_aControlFontName[idx]
        FONTSIZE _hmg_aControlFontSize[idx]
        IF !Empty(CINPUTMASK)
           INPUTMASK CINPUTMASK
        ENDIF
        IF !Empty(CFORMAT)
           FORMAT CFORMAT
        ENDIF
        ONCHANGE (v := This.Value, _HMG_GridInplaceEdit_StageEvent := 2, _HMG_OnInplaceEditEvent(idx))
      END TEXTBOX

      IF Empty(CINPUTMASK) .AND. Empty(CFORMAT)
         _SetFocus(ControlName, FormName)
      ENDIF

   ELSEIF AEC == "DATEPICKER"

      DEFINE DATEPICKER D
        ROW      0
        COL      0
        WIDTH    This.CellWidth
        HEIGHT   This.CellHeight + 6
        VALUE    v
        SHOWNONE .T.
        FONTNAME _hmg_aControlFontName[idx]
        FONTSIZE _hmg_aControlFontSize[idx]
        IF DTYPE == "DROPDOWN"
           UPDOWN .F.
        ELSEIF DTYPE == "UPDOWN"
           UPDOWN .T.
        ENDIF
        ONCHANGE (v := This.Value, _HMG_GridInplaceEdit_StageEvent := 2, _HMG_OnInplaceEditEvent(idx))
      END DATEPICKER

   ELSEIF AEC == "COMBOBOX"

      DEFINE COMBOBOX C
        ROW      0
        COL      0
        WIDTH    This.CellWidth
        ITEMS    AITEMS
        VALUE    v
        FONTNAME _hmg_aControlFontName[idx]
        FONTSIZE _hmg_aControlFontSize[idx]
        ON LISTDISPLAY (AAdd(abKeyBlocks, _GetHotKeyBlock(FormName, 0, 27)), ;
           _ReleaseHotKey(FormName, 0, 27), ;
           AAdd(abKeyBlocks, _GetHotKeyBlock(FormName, 0, 13)), ;
           _ReleaseHotKey (FormName, 0, 13))
        ON LISTCLOSE (_DefineHotKey(FormName, 0, 27, abKeyBlocks[1]), ;
           _DefineHotKey(FormName, 0, 13, abKeyBlocks[2]), abKeyBlocks := {})
        ONCHANGE (v := This.Value, _HMG_GridInplaceEdit_StageEvent := 2, _HMG_OnInplaceEditEvent(idx))
      END COMBOBOX

   ELSEIF AEC == "SPINNER"

      DEFINE SPINNER S
        ROW       0
        COL       0
        WIDTH     This.CellWidth
        HEIGHT    This.CellHeight + 6
        VALUE     v
        RANGEMIN  ARANGE[1]
        RANGEMAX  ARANGE[2]
        INCREMENT ARANGE[3]
        FONTNAME _hmg_aControlFontName[idx]
        FONTSIZE _hmg_aControlFontSize[idx]
        ONCHANGE (v := This.Value, _HMG_GridInplaceEdit_StageEvent := 2, _HMG_OnInplaceEditEvent(idx))
      END SPINNER

      _SetFocus(ControlName, FormName)

   ELSEIF AEC == "CHECKBOX"

      DEFINE CHECKBOX C
        ROW       0
        COL       0
        WIDTH     This.CellWidth
        HEIGHT    This.CellHeight + 6 + DH
        VALUE     v
        FONTNAME _hmg_aControlFontName[idx]
        FONTSIZE _hmg_aControlFontSize[idx]
        CAPTION   ALABELS[iif(V, 1, 2)]
        BACKCOLOR WHITE
        ON CHANGE (v := This.Value, This.Caption := ALABELS[iif(V, 1, 2)], _HMG_GridInplaceEdit_StageEvent := 2, _HMG_OnInplaceEditEvent(idx))
      END CHECKBOX

   ENDIF

   END WINDOW

   _HMG_GridInplaceEdit_ControlHandle := GetControlHandle(ControlName, FormName)

   IF hb_IsArray(_HMG_GridInplaceEdit_ControlHandle)
      _HMG_GridInplaceEdit_ControlHandle := _HMG_GridInplaceEdit_ControlHandle[1]
   ENDIF

   _HMG_GridInplaceEdit_StageEvent := 1   // PreEvent
   _HMG_OnInplaceEditEvent(idx)

   ACTIVATE WINDOW _hmg_grid_inplaceedit

   _HMG_GridInplaceEdit_StageEvent := 3   // PostEvent
   _HMG_OnInplaceEditEvent(idx)

   _HMG_GridInplaceEdit_StageEvent    := 0
   _HMG_GridInplaceEdit_ControlHandle := 0
   _HMG_GridInplaceEdit_GridIndex     := 0

   _HMG_InplaceParentHandle := 0

   hmg_SetFocus(_HMG_aControlHandles[idx])

   IF Save != V
      Eval(bChange, v)
   ENDIF

RETURN .T.

/*
_HMG_OnInplaceEditEvent(nIndex) -->
*/
FUNCTION _HMG_OnInplaceEditEvent(nIndex)

   LOCAL Ret

   hb_default(@nIndex, _HMG_GridInplaceEdit_GridIndex)

   IF _HMG_GridInplaceEdit_ControlHandle != 0 .AND. hb_IsBlock(_HMG_aControlMiscData1[nIndex][30])
      Ret := Eval(_HMG_aControlMiscData1[nIndex][30])
   ENDIF

RETURN Ret

/*
_ParseGridControls(aEditControls, ci, ri) --> array
*/
FUNCTION _ParseGridControls(aEditControls, ci, ri)

   LOCAL AEC := "TEXTBOX"
   LOCAL AITEMS := {}
   LOCAL ARANGE := {}
   LOCAL DTYPE := "D"
   LOCAL ALABELS := {".T.", ".F."}
   LOCAL CTYPE := "CHARACTER"
   LOCAL CINPUTMASK := ""
   LOCAL CFORMAT := ""
   LOCAL aEdit
   LOCAL bChange := {||NIL}
   LOCAL i
   LOCAL bBlock

   IF hb_IsArray(aEditControls)

      IF Len(aEditControls) >= ci

         IF hb_IsArray(aEditControls[ci])

            IF Len(aEditControls[ci]) >= 1

               AEC := Upper(aEditControls[ci][1])

               // check for a new type control defined as {'DYNAMIC', {|r,c| CodeBlock_Return_Control_Array}}
               IF Len(aEditControls[ci]) >= 2 .AND. AEC == "DYNAMIC"

                  aEdit := Eval(aEditControls[ci][2], ri, ci)

                  // A more generic function to simulate ONCHANGE event
                  bChange := iif(Len(aEditControls[ci]) > 2 .AND. hb_IsBlock(aEditControls[ci][3]), aEditControls[ci][3], {||NIL})

                  IF hb_IsArray(aEdit) .AND. Len(aEdit) >= 1 .AND. iif(Len(aEdit) > 1, !hb_IsBlock(aEdit[2]), .T.)
                     AEC := aEdit[1]    // get normal type for this cell
                  ELSE
                     IF hb_IsArray(aEdit) .AND. Len(aEdit) >= 2 .AND. hb_IsBlock(aEdit[2])
                        AEC := "CODEBLOCK"
                        bBlock := aEdit[2]
                     ELSE
                        AEC := "TEXTBOX" // default
                        aEdit := {}      // set as array
                     ENDIF
                  ENDIF

               ELSE

                  aEdit := aEditControls[ci]

               ENDIF

               IF Len(aEdit) >= 2 .AND. AEC == "TEXTBOX"

                  IF hb_IsString(AEDIT[2])
                     CTYPE := Upper(AEDIT[2])
                  ENDIF

                  IF Len(AEDIT) >= 3
                     IF hb_IsString(AEDIT[3])
                        CINPUTMASK := AEDIT[3]
                     ENDIF
                  ENDIF

                  IF Len(AEDIT) >= 4
                     IF hb_IsString(AEDIT[4])
                        CFORMAT := AEDIT[4]
                     ENDIF
                  ENDIF

               ENDIF

               IF Len(aEdit) >= 2 .AND. AEC == "COMBOBOX"
                  IF hb_IsArray(AEDIT[2])
                     AITEMS := AEDIT[2]
                  ENDIF
               ENDIF

               IF Len(aEdit) >= 3 .AND. AEC == "SPINNER"

                  IF Len(aEdit) > 3 .AND. hb_IsNumeric(AEDIT[2]) .AND. hb_IsNumeric(AEDIT[3]) .AND. hb_IsNumeric(AEDIT[4])
                     ARANGE := {AEDIT[2], AEDIT[3], AEDIT[4]}
                  ELSEIF hb_IsNumeric(AEDIT[2]) .AND. hb_IsNumeric(AEDIT[3])
                     ARANGE := {AEDIT[2], AEDIT[3], 1}
                  ENDIF

               ENDIF

               IF Len(aEdit) >= 2 .AND. AEC == "DATEPICKER"
                  IF hb_IsString(AEDIT[2])
                     DTYPE := Upper(AEDIT[2])
                  ENDIF
               ENDIF

               IF Len(aEdit) >= 3 .AND. AEC == "CHECKBOX"

                  IF hb_IsString(AEDIT[2]) .AND. hb_IsString(AEDIT[3])
                     ALABELS := {AEDIT[2], AEDIT[3]}
                  ENDIF

               ENDIF

               // An individual function overlapping a generic
               FOR i := 3 TO Len(aEdit)
                  IF hb_IsBlock(aEdit[i])
                     bChange := aEdit[i]
                  ENDIF
               NEXT

            ENDIF

         ENDIF

      ENDIF

   ENDIF

RETURN {AEC, CTYPE, CINPUTMASK, CFORMAT, AITEMS, ARANGE, DTYPE, ALABELS, bChange, bBlock}

/*
_GridInplaceEditOK(idx, ci, ri, aec)
*/
STATIC PROCEDURE _GridInplaceEditOK(idx, ci, ri, aec)

   LOCAL CVA
   LOCAL VALID
   LOCAL aValidMessages
   LOCAL aTemp
   LOCAL Cols
   LOCAL z

   CVA := _HMG_aControlMiscData1[idx][14]

   IF hb_IsArray(CVA) .AND. Len(CVA) >= ci

      IF hb_IsBlock(CVA[ci])

         _HMG_ThisItemCellValue := GetProperty("_hmg_grid_inplaceedit", Left(AEC, 1), "value")

         _HMG_ThisFormName := _HMG_aFormNames[Ascan(_HMG_aFormHandles, _HMG_aControlParentHandles[idx])]
         _HMG_ThisControlName := _HMG_aControlNames[idx]
         _HMG_ThisEventType := "GRID_VALID"

         VALID := Eval(CVA[ci])

         _HMG_ThisEventType := ""
         _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
         _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

         IF hb_IsLogical(VALID) .AND. !VALID

            aValidMessages := _HMG_aControlMiscData1[idx][16]

            IF hb_IsArray(aValidMessages)

               IF hb_IsString(aValidMessages[ci])

                  MsgAlert(aValidMessages[ci], _HMG_BRWLangError[10])

               ELSEIF hb_IsBlock(aValidMessages[ci])

                  Eval(aValidMessages[ci], _HMG_ThisItemCellValue)

               ELSE

                  MsgAlert(_HMG_BRWLangError[11], _HMG_BRWLangError[10])

               ENDIF

            ELSE

               MsgAlert(_HMG_BRWLangError[11], _HMG_BRWLangError[10])

            ENDIF

            RETURN

         ENDIF

         hmg_ReDrawWindow(_HMG_aControlHandles[idx])

      ENDIF

   ENDIF

   IF !_HMG_aControlMiscData1[idx][5]

      aTemp := _GetItem(, , ri, idx)

   ELSE

      Cols := hmg_ListView_GetColumnCount(_HMG_aControlHandles[idx])

      aTemp := AFill(Array(Cols), "")

      FOR z := 1 TO Cols

         _HMG_ThisQueryRowIndex  := ri
         _HMG_ThisQueryColIndex  := z

         Eval(_HMG_aControlProcedures[idx])

         aTemp[z] := _HMG_ThisQueryData

      NEXT

   ENDIF

   aTemp[ci] := GetProperty("_hmg_grid_inplaceedit", Left(AEC, 1), "value")

   IF !_HMG_aControlMiscData1[idx][5]
      _SetItem(, , ri, aTemp, idx)
   ENDIF

   _hmg_grid_inplaceedit.Release

RETURN

/*
_SetGridCellEditValue(arg)
*/
PROCEDURE _SetGridCellEditValue(arg)

   LOCAL FormName := "_hmg_grid_inplaceedit"
   LOCAL ControlName

   SWITCH ValType(arg)

   CASE "C"
      ControlName := iif(_IsControlDefined("t", FormName) .AND. !(ValType(_GetValue("t", FormName)) == "N"), "t", NIL)
      EXIT

   CASE "D"
      ControlName := iif(_IsControlDefined("d", FormName), "d", iif(_IsControlDefined("t", FormName), "t", NIL))
      EXIT

   CASE "N"
      ControlName := iif(_IsControlDefined("c", FormName), "c", ;
         iif(_IsControlDefined("s", FormName), "s", ;
         iif(_IsControlDefined("t", FormName), "t", NIL)))
      EXIT

   CASE "L"
      ControlName := iif(_IsControlDefined("c", FormName), "c", NIL)

   END SWITCH

   IF hb_IsString(ControlName)
      SetProperty(FormName, ControlName, "value", arg)
   ELSE
      MsgMiniGuiError("CellValue replace: type mismatch.")
   ENDIF

RETURN

/*
_GridInplaceKbdEdit(i)
*/
PROCEDURE _GridInplaceKbdEdit(i)

   LOCAL h := _HMG_aControlHandles[i]
   LOCAL IPE_MAXCOL := hmg_ListView_GetColumnCount(h)
   LOCAL TmpRow
   LOCAL XS
   LOCAL XD
   LOCAL R

   DO WHILE .T.

      TmpRow := hmg_LISTVIEW_GETFIRSTITEM(h)

      IF TmpRow != _HMG_IPE_ROW
         _HMG_IPE_ROW := TmpRow
         _HMG_IPE_COL := iif(Len(_HMG_aControlBkColor[i]) > 0, 2, 1)
      ENDIF

      _HMG_ThisItemRowIndex := _HMG_IPE_ROW
      _HMG_ThisItemColIndex := _HMG_IPE_COL

      IF _HMG_IPE_COL == 1
         r := hmg_LISTVIEW_GETITEMRECT(h, _HMG_IPE_ROW - 1)
      ELSE
         r := hmg_LISTVIEW_GETSUBITEMRECT(h, _HMG_IPE_ROW - 1, _HMG_IPE_COL - 1)
      ENDIF

      xs := (_HMG_aControlCol[i] + r[2] + r[3]) - (_HMG_aControlCol[i] + _HMG_aControlWidth[i])

      xd := 20

      IF xs > - xd
         hmg_ListView_Scroll(h, xs + xd, 0)
      ELSE
         IF r[2] < 0
            hmg_ListView_Scroll(h, r[2], 0)
         ENDIF
      ENDIF

      IF _HMG_IPE_COL == 1
         r := hmg_LISTVIEW_GETITEMRECT(h, _HMG_IPE_ROW - 1)
      ELSE
         r := hmg_LISTVIEW_GETSUBITEMRECT(h, _HMG_IPE_ROW - 1, _HMG_IPE_COL - 1)
      ENDIF

      _HMG_ThisItemCellRow := iif(hb_IsNumeric(_HMG_aControlRow[i]), _HMG_aControlRow[i], 0) + r[1]
      _HMG_ThisItemCellCol := iif(hb_IsNumeric(_HMG_aControlCol[i]), _HMG_aControlCol[i], 0) + r[2]
      _HMG_ThisItemCellWidth := r[3]
      _HMG_ThisItemCellHeight := r[4]

      _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
      _HMG_ThisType := "C"
      _HMG_ThisIndex := i

      _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
      _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

      _GridInplaceEdit(i)

      _HMG_ThisIndex := 0
      _HMG_ThisType := ""

      _HMG_ThisItemRowIndex := 0
      _HMG_ThisItemColIndex := 0
      _HMG_ThisItemCellRow := 0
      _HMG_ThisItemCellCol := 0
      _HMG_ThisItemCellWidth := 0
      _HMG_ThisItemCellHeight := 0

      _HMG_ThisFormIndex := 0
      _HMG_ThisEventType := ""
      _HMG_ThisFormName := ""
      _HMG_ThisControlName := ""

      IF _HMG_IPE_CANCELLED

         IF _HMG_IPE_COL == IPE_MAXCOL
            _HMG_IPE_COL := 1
            hmg_ListView_Scroll(h, -10000, 0)
         ENDIF
         EXIT

      ELSE

         _HMG_IPE_COL++

         IF _HMG_IPE_COL > IPE_MAXCOL
            _HMG_IPE_COL := 1
            hmg_ListView_Scroll(h, -10000, 0)
            EXIT
         ENDIF

      ENDIF

   ENDDO

RETURN

/*
GetNumFromCellText(Text) --> numeric
*/
FUNCTION GetNumFromCellText(Text)

   LOCAL s As String
   LOCAL x
   LOCAL c

   FOR x := 1 TO hb_ULen(Text)

      c := hb_USubStr(Text, x, 1)

      IF c = "0" .OR. c = "1" .OR. c = "2" .OR. c = "3" .OR. c = "4" .OR. c = "5" .OR. c = "6" .OR. c = "7" .OR. c = "8" .OR. c = "9" .OR. c = "." .OR. c = "-"
         s += c
      ENDIF

   NEXT x

   IF hb_ULeft(AllTrim(Text), 1) == "(" .OR. hb_URight(AllTrim(Text), 2) == "DB"
      s := "-" + s
   ENDIF

RETURN Val(s)

/*
GETNumFromCellTextSP(Text) --> numeric
*/
FUNCTION GETNumFromCellTextSP(Text)

   LOCAL s As String
   LOCAL x
   LOCAL c

   FOR x := 1 TO hb_ULen(Text)

      c := hb_USubStr(Text, x, 1)

      IF c = "0" .OR. c = "1" .OR. c = "2" .OR. c = "3" .OR. c = "4" .OR. c = "5" .OR. c = "6" .OR. c = "7" .OR. c = "8" .OR. c = "9" .OR. c = "," .OR. c = "-" .OR. c = "."

         IF c == "."
            c := ""
         ENDIF

         IF c == ","
            c := "."
         ENDIF

         s += c

      ENDIF

   NEXT x

   IF hb_ULeft(AllTrim(Text), 1) == "(" .OR. hb_URight(AllTrim(Text), 2) == "DB"
      s := "-" + s
   ENDIF

RETURN Val(s)

/*
_GetGridCellValue(ControlName, ParentForm, Row, Col) --> value
*/
FUNCTION _GetGridCellValue(ControlName, ParentForm, Row, Col)

   LOCAL A := _GetItem(ControlName, ParentForm, Row)

RETURN iif(Col > 0 .AND. Col <= Len(A), A[COL], NIL)

/*
_SetGridCellValue(ControlName, ParentForm, Row, Col, CellValue)
*/
PROCEDURE _SetGridCellValue(ControlName, ParentForm, Row, Col, CellValue)

   LOCAL A := _GetItem(ControlName, ParentForm, Row)

   IF Col > 0 .AND. Col <= Len(A)

      A[Col] := CellValue

      _SetItem(ControlName, ParentForm, Row, A)

   ENDIF

RETURN

// (JK) HMG 1.0 Experimental Build 6
/*
   _GetColumnWidth(ControlName, ParentForm, nColumnNo)->nColumnWidth

   _SetColumnWidth(ControlName, ParentForm, nColumnNo, nWidth)->lSuccess

   _SetColumnWidthAuto(ControlName, ParentForm, nColumnNo)->lSuccess
   - Autofit column width to largest grid item width in column nColumnNo

   _SetColumnWidthAutoH(ControlName, ParentForm, nColumnNo)->lSuccess
   - Autofit column width to largest grid header width in column nColumnNo

   _SetColumnsWidthAuto(ControlName, ParentForm)
   - Autofit all columns width to largest grid item width in column

   _SetColumnsWidthAutoH(ControlName, ParentForm)
   - Autofit all columns width to largest grid header width in column
*/

/*
_GetColumnWidth(ControlName, ParentForm, nColumnNo) --> nWidth
*/
FUNCTION _GetColumnWidth(ControlName, ParentForm, nColumnNo)

   LOCAL z As Numeric
   LOCAL nWidth := -1
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   Assign z := nColumnNo

   IF z > 0 .AND. z <= hmg_ListView_GetColumnCount(h)
      nWidth := hmg_ListView_GetColumnWidth(h, z - 1)
   ENDIF

RETURN nWidth

/*
_SetColumnWidth(ControlName, ParentForm, nColumnNo, nWidth) --> .T.|.F.
*/
FUNCTION _SetColumnWidth(ControlName, ParentForm, nColumnNo, nWidth)

   LOCAL z As Numeric
   LOCAL lSuccess := .F.
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   Assign z := nColumnNo

   IF z > 0 .AND. z <= hmg_ListView_GetColumnCount(h)
      lSuccess := hmg_ListView_SetColumnWidth(h, z - 1, nWidth)
   ENDIF

RETURN lSuccess

/*
_SetColumnWidthAuto(ControlName, ParentForm, nColumnNo) --> .T.|.F.
*/
FUNCTION _SetColumnWidthAuto(ControlName, ParentForm, nColumnNo)

   LOCAL z As Numeric
   LOCAL lSuccess := .F.
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   Assign z := nColumnNo

   IF z > 0 .AND. z <= hmg_ListView_GetColumnCount(h)
      lSuccess := ListView_SetColumnWidthAuto(h, z - 1)
   ENDIF

RETURN lSuccess

/*
_SetColumnWidthAutoH(ControlName, ParentForm, nColumnNo) --> .T.|.F.
*/
FUNCTION _SetColumnWidthAutoH(ControlName, ParentForm, nColumnNo)

   LOCAL z As Numeric
   LOCAL lSuccess := .F.
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   Assign z := nColumnNo

   IF z > 0 .AND. z <= hmg_ListView_GetColumnCount(h)
      lSuccess := ListView_SetColumnWidthAutoH(h, z - 1)
   ENDIF

RETURN lSuccess

/*
_SetColumnsWidthAuto(ControlName, ParentForm) --> .T.|.F.
*/
FUNCTION _SetColumnsWidthAuto(ControlName, ParentForm)

   LOCAL ColumnCount
   LOCAL lSuccess := .F.
   LOCAL z
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   IF (ColumnCount := hmg_ListView_GetColumnCount(h)) > 0
      FOR z := 1 TO ColumnCount
         lSuccess := ListView_SetColumnWidthAuto(h, z - 1)
      NEXT z
   ENDIF

RETURN lSuccess

/*
_SetColumnsWidthAutoH(ControlName, ParentForm) --> .T.|.F.
*/
FUNCTION _SetColumnsWidthAutoH(ControlName, ParentForm)

   LOCAL ColumnCount
   LOCAL lSuccess := .F.
   LOCAL z
   LOCAL i
   LOCAL h

   i := GetControlIndex(ControlName, ParentForm)
   h := _HMG_aControlHandles[i]

   IF (ColumnCount := hmg_ListView_GetColumnCount(h)) > 0
      FOR z := 1 TO ColumnCount
         lSuccess := ListView_SetColumnWidthAutoH(h, z - 1)
      NEXT z
   ENDIF

RETURN lSuccess

//(JP) HMG 1.4 Extended Build 40
/*
_GridPgDn(ControlName, ParentForm, z)
*/
PROCEDURE _GridPgDn(ControlName, ParentForm, z)

   LOCAL i
   LOCAL PageLength
   LOCAL _DeltaScroll
   LOCAL s
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)
   PageLength   := hmg_LISTVIEWGETCOUNTPERPAGE(GridHandle)
   IF s + PageLength <=  hmg_LISTVIEWGETITEMCOUNT(GridHandle)
      hmg_ListView_SetCursel(GridHandle, s + PageLength)
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * PageLength)
   ELSE
      hmg_ListView_SetCursel(GridHandle, hmg_LISTVIEWGETITEMCOUNT(GridHandle))
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (hmg_LISTVIEWGETITEMCOUNT(GridHandle) - s))
   ENDIF

RETURN

/*
_GridPgUp(ControlName, ParentForm, z)
*/
PROCEDURE _GridPgUp(ControlName, ParentForm, z)

   LOCAL i
   LOCAL PageLength
   LOCAL s
   LOCAL _DeltaScroll
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)
   PageLength   := hmg_LISTVIEWGETCOUNTPERPAGE(GridHandle)

   IF s - PageLength >= 1
      hmg_ListView_SetCursel(GridHandle, s - PageLength)
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * PageLength * (-1))
   ELSE
      hmg_ListView_SetCursel(GridHandle, 1)
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * s * (-1))
   ENDIF

RETURN

/*
_GridHome(ControlName, ParentForm, z)
*/
PROCEDURE _GridHome(ControlName, ParentForm, z)

   LOCAL i
   LOCAL s
   LOCAL _DeltaScroll
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)

   hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (-1) * s)

   hmg_ListView_SetCursel(GridHandle, 1)

RETURN

/*
_GridEnd(ControlName, ParentForm, z)
*/
PROCEDURE _GridEnd(ControlName, ParentForm, z)

   LOCAL i
   LOCAL s
   LOCAL _DeltaScroll
   LOCAL _BottomPos
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   _BottomPos   := hmg_LISTVIEWGETITEMCOUNT(GridHandle)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)

   hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (_BottomPos - s))

   hmg_ListView_SetCursel(GridHandle, _BottomPos)

RETURN

/*
_GridPrior(ControlName, ParentForm, z)
*/
PROCEDURE _GridPrior(ControlName, ParentForm, z)

   LOCAL i
   LOCAL s
   LOCAL TopInx
   LOCAL _DeltaScroll
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   TopInx       := hmg_LISTVIEW_GETTOPINDEX(GridHandle)

   s := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)
   hmg_ListView_SetCursel(GridHandle, s - 1)

   IF s <= TopInx + 1
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (-1))
   ENDIF

RETURN

/*
_GridNext(ControlName, ParentForm, z)
*/
PROCEDURE _GridNext(ControlName, ParentForm, z)

   LOCAL i
   LOCAL PageLength
   LOCAL s
   LOCAL TopInx
   LOCAL _DeltaScroll
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)
   TopInx       := hmg_LISTVIEW_GETTOPINDEX(GridHandle)
   PageLength   := hmg_LISTVIEWGETCOUNTPERPAGE(GridHandle)
   hmg_ListView_SetCursel(GridHandle, s + 1)

   IF s - TopInx == PageLength
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4])
   ENDIF

RETURN

/*
_GridScrollToPos(ControlName, ParentForm, z)
*/
PROCEDURE _GridScrollToPos(ControlName, ParentForm, z)

   LOCAL i
   LOCAL s
   LOCAL TopInx
   LOCAL PageLength
   LOCAL _DeltaScroll
   LOCAL GridHandle

   i := iif(PCount() == 2, GetControlIndex(ControlName, ParentForm), z)

   GridHandle   := _HMG_aControlHandles[i]
   _DeltaScroll := hmg_ListView_GetSubItemRect(GridHandle, 0, 0)
   s            := hmg_LISTVIEW_GETFIRSTITEM(GridHandle)
   TopInx       := hmg_LISTVIEW_GETTOPINDEX(GridHandle)
   PageLength   := hmg_LISTVIEWGETCOUNTPERPAGE(GridHandle)
   IF topInx < s
      IF s - TopInx > PageLength
         hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (s - TopInx - PageLength))
      ENDIF
   ELSE
      hmg_ListView_Scroll(GridHandle, 0, _DeltaScroll[4] * (-1) * (s - TopInx))
   ENDIF

RETURN

/*
_GetIVirtualItem(nRow, i, nCols) --> array
*/
FUNCTION _GetIVirtualItem(nRow, i, nCols)

   LOCAL aTemp := Array(nCols)
   LOCAL j

   FOR j := 1 TO nCols

      _HMG_ThisQueryRowIndex := nRow

      _HMG_ThisQueryColIndex := j

      IF hb_IsBlock(_HMG_aControlProcedures[i])

         Eval(_HMG_aControlProcedures[i])   // OnQueryData Event

      ENDIF

      aTemp[j] := _HMG_ThisQueryData

   NEXT j

RETURN aTemp

/*
_GRIDINPLACEKBDEDIT_2(i)
*/
PROCEDURE _GRIDINPLACEKBDEDIT_2(i)

   LOCAL IPE_MAXCOL := hmg_ListView_GetColumnCount(_HMG_aControlHandles[i])
   LOCAL IPE_MAXROW := hmg_ListViewGetItemCount(_HMG_aControlHandles[i])
   LOCAL ownerdata := _HMG_aControlMiscData1[i][5]
   LOCAL aColumnWhen := _HMG_aControlMiscData1[i][15]
   LOCAL aTemp
   LOCAL nStart
   LOCAL nEnd
   LOCAL lResult
   LOCAL r
   LOCAL j

   _GRID_KBDSCROLL(i)

   IF _HMG_ThisItemColIndex == 1
      r := hmg_LISTVIEW_GETITEMRECT(_HMG_aControlHandles[i], _HMG_ThisItemRowIndex - 1)
   ELSE
      r := hmg_LISTVIEW_GETSUBITEMRECT(_HMG_aControlHandles[i], _HMG_ThisItemRowIndex - 1, _HMG_ThisItemColIndex - 1)
   ENDIF

   _HMG_ThisItemCellRow := iif(hb_IsNumeric(_HMG_aControlRow[i]), _HMG_aControlRow[i], 0) + r[1]
   _HMG_ThisItemCellCol := iif(hb_IsNumeric(_HMG_aControlCol[i]), _HMG_aControlCol[i], 0) + r[2]
   _HMG_ThisItemCellWidth := r[3]
   _HMG_ThisItemCellHeight := r[4]

   _HMG_ThisFormIndex := AScan(_HMG_aFormHandles, _HMG_aControlParentHandles[i])
   _HMG_ThisType := "C"
   _HMG_ThisIndex := i

   _HMG_ThisFormName := _HMG_aFormNames[_HMG_ThisFormIndex]
   _HMG_ThisControlName := _HMG_aControlNames[_HMG_ThisIndex]

   r := _GridInplaceEdit(i)

   IF !_HMG_IPE_CANCELLED

      IF r .AND. _HMG_aControlMiscData1[i][19] == 0

         IF _HMG_aControlMiscData1[i][17] < IPE_MAXCOL

            IF _lShiftTabKeyPressed
               _lShiftTabKeyPressed := .F.
               IF _HMG_aControlMiscData1[i][17] > 1
                  _HMG_aControlMiscData1[i][17]--
                  InsertReturn()
               ELSE
                  _HMG_aControlMiscData1[i][17] := IPE_MAXCOL
               ENDIF
            ELSE
               IF _lTabKeyPressed
                  _lTabKeyPressed := .F.
                  _HMG_aControlMiscData1[i][17]++
                  InsertReturn()
               ELSE
                  IF _HMG_GridNavigationMode
                     IF This.CellRowIndex < IPE_MAXROW
                        InsertDown()
                        InsertReturn()
                     ENDIF
                  ELSE
                     _HMG_aControlMiscData1[i][17]++
                  ENDIF
               ENDIF
            ENDIF

            IF hb_IsArray(aColumnWhen)

               IF !ownerdata
                  aTemp := This.Item(This.CellRowIndex)
               ELSE
                  _HMG_ThisQueryRowIndex := This.CellRowIndex
               ENDIF

               nStart := _HMG_aControlMiscData1[i][17]
               nEnd := Len(aColumnWhen)

               FOR j := nStart TO nEnd
                  IF hb_IsBlock(aColumnWhen[j])
                     r := Min(IPE_MAXCOL, j)
                     IF !ownerdata
                        _HMG_ThisItemCellValue := aTemp[r]
                     ELSE
                        _HMG_ThisQueryColIndex := r
                        Eval(_HMG_aControlProcedures[i])
                        _HMG_ThisItemCellValue := _HMG_ThisQueryData
                     ENDIF
                     _HMG_ThisEventType := "GRID_WHEN"
                     lResult := Eval(aColumnWhen[j])
                     _HMG_ThisEventType := ""
                     IF !lResult
                        _HMG_aControlMiscData1[i][17]++
                     ELSE
                        EXIT
                     ENDIF
                  ELSE
                     EXIT
                  ENDIF
               NEXT j

               IF !_HMG_GridNavigationMode
                  IF _HMG_aControlMiscData1[i][17] > nEnd
                     _HMG_aControlMiscData1[i][17] := nStart - 1
                  ENDIF
               ENDIF

            ENDIF

         ELSEIF _HMG_aControlMiscData1[i][17] == IPE_MAXCOL

            IF _lShiftTabKeyPressed
               _HMG_aControlMiscData1[i][17]--
               _lShiftTabKeyPressed := .F.
               InsertReturn()
            ELSE
               IF _lTabKeyPressed
                  _lTabKeyPressed := .F.
                  _HMG_aControlMiscData1[i][17] := 1
               ELSE
                  IF _HMG_GridNavigationMode
                     IF This.CellRowIndex < IPE_MAXROW
                        InsertDown()
                        InsertReturn()
                     ENDIF
                  ELSE
                     _HMG_aControlMiscData1[i][17] := 1
                  ENDIF
               ENDIF
            ENDIF

         ENDIF

      ENDIF

      hmg_LISTVIEW_REDRAWITEMS(_HMG_aControlHandles[i], _HMG_aControlMiscData1[i][1] - 1, _HMG_aControlMiscData1[i][1] - 1)
      _DoControlEventProcedure(_HMG_aControlChangeProcedure[i], i)

   ENDIF

   _HMG_ThisIndex := 0
   _HMG_ThisType := ""

   _HMG_ThisItemRowIndex := 0
   _HMG_ThisItemColIndex := 0
   _HMG_ThisItemCellRow := 0
   _HMG_ThisItemCellCol := 0
   _HMG_ThisItemCellWidth := 0
   _HMG_ThisItemCellHeight := 0

   _HMG_ThisFormIndex := 0
   _HMG_ThisEventType := ""
   _HMG_ThisFormName := ""
   _HMG_ThisControlName := ""

RETURN

/*
_GRID_KBDSCROLL(i)
*/
PROCEDURE _GRID_KBDSCROLL(i)

   LOCAL r
   LOCAL xs
   LOCAL xd

   _HMG_ThisItemRowIndex := _HMG_aControlMiscData1[i][1]
   _HMG_ThisItemColIndex := _HMG_aControlMiscData1[i][17]

   IF _HMG_ThisItemColIndex == 1
      r := hmg_LISTVIEW_GETITEMRECT(_HMG_aControlHandles[i], _HMG_ThisItemRowIndex - 1)
   ELSE
      r := hmg_LISTVIEW_GETSUBITEMRECT(_HMG_aControlHandles[i], _HMG_ThisItemRowIndex - 1, _HMG_ThisItemColIndex - 1)
   ENDIF

   xs := (_HMG_aControlCol[i] + r[2] + r[3]) - (_HMG_aControlCol[i] + _HMG_aControlWidth[i])

   xd := 20

   IF xs > -xd
      hmg_ListView_Scroll(_HMG_aControlHandles[i], xs + xd, 0)
   ELSE
      IF r[2] < 0
         hmg_ListView_Scroll(_HMG_aControlHandles[i], r[2], 0)
      ENDIF
   ENDIF

RETURN
