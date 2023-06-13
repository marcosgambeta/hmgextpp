/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software; see the file COPYING. If not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 * visit the web site http://www.gnu.org/).
 *
 * As a special exception, you have permission for additional uses of the text
 * contained in this release of Harbour Minigui.
 *
 * The exception is that, if you link the Harbour Minigui library with other
 * files to produce an executable, this does not by itself cause the resulting
 * executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of linking the
 * Harbour-Minigui library code into it.
 *
 * Parts of this project are based upon:
 *
 * "Harbour GUI framework for Win32"
 * Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - https://harbour.github.io/
 *
 * "Harbour Project"
 * Copyright 1999-2022, https://harbour.github.io/
 *
 * "WHAT32"
 * Copyright 2002 AJ Wos <andrwos@aust1.net>
 *
 * "HWGUI"
 * Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
 */

#include "minigui.ch"
#include "i_winuser.ch"

STATIC a_Node_Item_Cargo := {}

*-----------------------------------------------------------------------------*
FUNCTION _DefineTree ( ControlName, ParentFormName, row, col, width, height, ;
      change, tooltip, fontname, fontsize, gotfocus, lostfocus, dblclick, break, ;
      value, HelpId, aImgNode, aImgItem, noBot, bold, italic, underline, strikeout, ;
      itemids, backcolor, fontcolor, linecolor, indent, itemheight, nId, bInit, NoTrans )
*-----------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL Controlhandle
   LOCAL FontHandle
   LOCAL ImgDefNode
   LOCAL ImgDefItem
   LOCAL aBitmaps := Array(4)
   LOCAL mVar
   LOCAL k
   LOCAL Mask
   LOCAL Style
   LOCAL blInit
   LOCAL i
   LOCAL oc // := NIL
   LOCAL ow // := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   _HMG_ActiveTreeHandle := 0
   _HMG_NodeIndex := 1
   _HMG_NodeHandle[1] := 0
   _HMG_aTreeMap := {}
   _HMG_aTreeIdMap := {}
   _HMG_ActiveTreeItemIds := itemids

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      col := col + _HMG_ActiveFrameCol[_HMG_FrameLevel]
      row := row + _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF
   STATIC _HMG_lDialogInMemory AS GLOBAL VALUE _HMG_DialogInMemory

   IF !_IsWindowDefined(ParentFormName) .AND. !_HMG_DialogInMemory
      MsgMiniGuiError("Window: " + iif(ParentFormName == NIL, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName) .AND. !_HMG_DialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   _HMG_ActiveTreeValue := iif(hb_IsNumeric(Value), Value, 0)

   hb_default(@Width, 120)
   hb_default(@Height, 120)
   __defaultNIL(@change, "")
   __defaultNIL(@gotfocus, "")
   __defaultNIL(@lostfocus, "")
   __defaultNIL(@dblclick, "")
   hb_default(@NoTrans, .F.)

   a_Node_Item_Cargo := {}
   
   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      IF hb_IsLogical(nobot)
         mask := iif(nobot, 0, TVS_LINESATROOT)
      ENDIF
      Style := WS_BORDER + WS_VISIBLE + WS_TABSTOP + WS_CHILD + TVS_HASLINES + TVS_HASBUTTONS + mask + TVS_SHOWSELALWAYS

      IF Len(_HMG_aDialogTemplate) > 0        // Dialog Template

         // {{"ID",k/hwnd,class,Style,ExStyle,col,row,width,height,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {| x, y, z| InitDialogTree(x, y, z) }
         _HMG_aDialogTreeItem := {}
         AAdd(_HMG_aDialogItems, {nId, k, "SysTreeView32", style, 0, col, row, width, height, "", HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := GetDialogItemHandle(ParentFormHandle, nId)

         SetWindowStyle(ControlHandle, Style, .T.)

         col := GetWindowCol(Controlhandle)
         row := GetWindowRow(Controlhandle)
         Width := GetWindowWidth(Controlhandle)
         Height := GetWindowHeight(Controlhandle)

         ImgDefNode := iif(hb_IsArray(aImgNode), Len(aImgNode), 0)  // Tree+
         ImgDefItem := iif(hb_IsArray(aImgItem), Len(aImgItem), 0)  // Tree+

         IF ImgDefNode > 0

            aBitmaps[1] := aImgNode[1]              // Node default
            aBitmaps[2] := aImgNode[ImgDefNode]

            IF ImgDefItem > 0

               aBitmaps[3] := aImgItem[1]          // Item default
               aBitmaps[4] := aImgItem[ImgDefItem]

            ELSE

               aBitmaps[3] := aImgNode[1]           // Copy Node def if no Item def
               aBitmaps[4] := aImgNode[ImgDefNode]

            ENDIF

            InitTreeViewBitmap(ControlHandle, aBitmaps, NoTrans) // Init Bitmap List

         ENDIF

         _HMG_ActiveTreeHandle := ControlHandle

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      IF Row == NIL .OR. Col == NIL

         IF _HMG_SplitLastControl == "TOOLBAR"
            Break := .T.
         ENDIF

         i := GetFormIndex ( ParentFormName )

         IF i > 0

            ControlHandle := InitTree ( _HMG_aFormReBarHandle[i], col, row, width, height, 0, "", 0, iif(noBot, 1, 0) )

            AddSplitBoxItem(Controlhandle, _HMG_aFormReBarHandle[i], Width, break, , , , _HMG_ActiveSplitBoxInverted)

            _HMG_SplitLastControl := "TREE"

         ENDIF

      ELSE

         ControlHandle := InitTree ( ParentFormHandle, col, row, width, height, 0, "", 0, iif(noBot, 1, 0) )

      ENDIF

      ImgDefNode := iif(hb_IsArray(aImgNode), Len(aImgNode), 0)  // Tree+
      ImgDefItem := iif(hb_IsArray(aImgItem), Len(aImgItem), 0)  // Tree+

      IF ImgDefNode > 0

         aBitmaps[1] := aImgNode[1]          // Node default
         aBitmaps[2] := aImgNode[ImgDefNode]

         IF ImgDefItem > 0

            aBitmaps[3] := aImgItem[1]       // Item default
            aBitmaps[4] := aImgItem[ImgDefItem]

         ELSE

            aBitmaps[3] := aImgNode[1]       // Copy Node def if no Item def
            aBitmaps[4] := aImgNode[ImgDefNode]

         ENDIF

         InitTreeViewBitmap(ControlHandle, aBitmaps, NoTrans)    // Init Bitmap List

      ENDIF

      _HMG_ActiveTreeHandle := ControlHandle

   ENDIF

   IF !_HMG_DialogInMemory

      IF !empty(FontHandle)
         _SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(ControlHandle)
            FontHandle := _SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      IF tooltip != NIL
         SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

      IF hb_IsArray(backcolor)
         TreeView_SetBkColor(ControlHandle, backcolor)
      ENDIF

      IF hb_IsArray(fontcolor)
         TreeView_SetTextColor(ControlHandle, fontcolor)
      ENDIF

      IF hb_IsArray(linecolor)
         TreeView_SetLineColor(ControlHandle, linecolor)
      ENDIF

      IF hb_IsNumeric(indent)
         TreeView_SetIndent(ControlHandle, indent)
      ENDIF

      IF hb_IsNumeric(itemheight)
         TreeView_SetItemHeight(ControlHandle, itemheight)
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_ActiveTreeIndex := k

   _HMG_aControlType               [k] := CONTROL_TYPE_TREE
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParentHandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Nil
   _HMG_aControlInputMask          [k] := itemids
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := change
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := dblclick
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := Row
   _HMG_aControlCol                [k] := Col
   _HMG_aControlWidth              [k] := Width
   _HMG_aControlHeight             [k] := Height
   _HMG_aControlSpacing            [k] := 0
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := {}
   _HMG_aControlContainerHandle    [k] := HMG_NULLHANDLE
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := 0
   _HMG_aControlRangeMax           [k] := 0
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := .T.
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := HMG_NULLHANDLE
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := { 0, aImgNode, aImgItem, NoTrans }
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
#ifdef _OBJECT_
      ow := _WindowObj(ParentFormHandle)
      oc := _ControlObj(ControlHandle)
#endif
   ENDIF

   Do_ControlEventProcedure ( bInit, k, ow, oc )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogTree(ParentName, ControlHandle, k)
*-----------------------------------------------------------------------------*
   
   LOCAL aBitmaps := Array(4)
   LOCAL aImgNode
   LOCAL aImgItem
   LOCAL aImage
   LOCAL ImgDefNode
   LOCAL ImgDefItem
   LOCAL NoTrans
   LOCAL NodeHandle
   LOCAL Handle
   LOCAL TEXT
   LOCAL ImgDef
   LOCAL Id
   LOCAL iUnsel
   LOCAL iSel
   LOCAL NodeIndex
   LOCAL n
   LOCAL Cargo

   HB_SYMBOL_UNUSED(ParentName)

   aImgNode := _HMG_aControlMiscData1[k, 2]
   aImgItem := _HMG_aControlMiscData1[k, 3]
   NoTrans  := _HMG_aControlMiscData1[k, 4]

   ImgDefNode := iif(hb_IsArray(aImgNode), Len(aImgNode), 0)  // Tree+
   ImgDefItem := iif(hb_IsArray(aImgItem), Len(aImgItem), 0)  // Tree+

   IF ImgDefNode > 0
      aBitmaps[1] := aImgNode[1]              // Node default
      aBitmaps[2] := aImgNode[ImgDefNode]
      IF ImgDefItem > 0
         aBitmaps[3] := aImgItem[1]           // Item default
         aBitmaps[4] := aImgItem[ImgDefItem]
      ELSE
         aBitmaps[3] := aImgNode[1]           // Copy Node def if no Item def
         aBitmaps[4] := aImgNode[ImgDefNode]
      ENDIF
      InitTreeViewBitmap(ControlHandle, aBitmaps, NoTrans) // Init Bitmap List
   ENDIF

   _HMG_ActiveTreeHandle := ControlHandle

   FOR n := 1 TO Len(_HMG_aDialogTreeItem)
      aImage     := _HMG_aDialogTreeItem[n, 2]
      text       := _HMG_aDialogTreeItem[n, 1]
      id         := _HMG_aDialogTreeItem[n, 3]
      NodeIndex  := _HMG_aDialogTreeItem[n, 4]
      Cargo      := _HMG_aDialogTreeItem[n, 6]
      ImgDef     := iif(hb_IsArray(aImage), Len(aImage), 0)  // Tree+
      NodeHandle := _HMG_NodeHandle[NodeIndex]

      AAdd(a_Node_Item_Cargo, Cargo)

      IF ImgDef == 0
         IF _HMG_aDialogTreeItem[n, 5] == "NODE"
            iUnsel := 0   // Pointer to defalut Node Bitmaps, no Bitmap loaded
            iSel   := 1
         ELSE
            iUnsel := 2   // Pointer to defalut Item Bitmaps, no Bitmap loaded
            iSel   := 3
         ENDIF
      ELSE
         iUnSel := AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[1], NoTrans ) - 1
         iSel := iif(ImgDef == 1, iUnSel, AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[2], NoTrans ) - 1)
         // If only one bitmap in array iSel = iUnsel, only one Bitmap loaded
      ENDIF
      IF _HMG_aDialogTreeItem[n, 5] == "NODE"
         _HMG_NodeHandle[NodeIndex] := AddTreeItem ( _HMG_ActiveTreeHandle, _HMG_NodeHandle[NodeIndex - 1], text, iUnsel, iSel, Id, .T. )
         AAdd(_HMG_aTreeMap, _HMG_NodeHandle[NodeIndex])
         AAdd(_HMG_aTreeIdMap, Id)
      ELSE
         handle := AddTreeItem ( _HMG_ActiveTreeHandle, NodeHandle, text, iUnSel, iSel, Id, .F. )
         AAdd(_HMG_aTreeMap, Handle)
         AAdd(_HMG_aTreeIdMap, Id)
      ENDIF
   NEXT

   _HMG_aControlPageMap  [_HMG_ActiveTreeIndex] := _HMG_aTreeMap
   _HMG_aControlPicture  [_HMG_ActiveTreeIndex] := _HMG_aTreeIdMap
   _HMG_aControlHeadClick[_HMG_ActiveTreeIndex] := AClone ( a_Node_Item_Cargo )

   IF _HMG_ActiveTreeValue > 0

      IF !_HMG_ActiveTreeItemIds
         TreeView_SelectItem ( _HMG_ActiveTreeHandle, _HMG_aTreeMap[_HMG_ActiveTreeValue] )
      ELSE
         TreeView_SelectItem ( _HMG_ActiveTreeHandle, _HMG_aTreeMap[AScan(_HMG_aTreeIdMap, _HMG_ActiveTreeValue)] )
      ENDIF
   ENDIF
   // JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]   // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _DefineTreeNode ( text, aImage, Id, Cargo )
*-----------------------------------------------------------------------------*
   
   LOCAL ImgDef
   LOCAL k
   LOCAL iUnSel
   LOCAL iSel

   hb_default(@Id, 0)

   IF _SetGetGlobal( "_HMG_lDialogInMemory" )

      _HMG_NodeIndex++
      AAdd(_HMG_aDialogTreeItem, {text, aImage, Id, _HMG_NodeIndex, "NODE", Cargo})

   ELSE

      ImgDef := iif(hb_IsArray(aImage), Len(aImage), 0)  // Tree+

      IF ImgDef == 0
         iUnsel := 0   // Pointer to defalut Node Bitmaps, no Bitmap loaded
         iSel   := 1
      ELSE
         k := _HMG_ActiveTreeIndex
         iUnSel := AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[1], _HMG_aControlMiscData1[k, 4] ) - 1
         iSel := iif(ImgDef == 1, iUnSel, AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[2], _HMG_aControlMiscData1[k, 4] ) - 1)
         // If only one bitmap in array iSel = iUnsel, only one Bitmap loaded
      ENDIF

      _HMG_NodeIndex++
      _HMG_NodeHandle[_HMG_NodeIndex] := AddTreeItem ( _HMG_ActiveTreeHandle, _HMG_NodeHandle[_HMG_NodeIndex - 1], text, iUnsel, iSel, Id, .T. )
      AAdd(_HMG_aTreeMap, _HMG_NodeHandle[_HMG_NodeIndex])
      AAdd(_HMG_aTreeIdMap, Id)
      AAdd(a_Node_Item_Cargo, Cargo)

   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _EndTreeNode()
*-----------------------------------------------------------------------------*

   _HMG_NodeIndex--

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _DefineTreeItem ( text, aImage, Id, Cargo )
*-----------------------------------------------------------------------------*
   
   LOCAL handle
   LOCAL ImgDef
   LOCAL k
   LOCAL iUnSel
   LOCAL iSel

   hb_default(@Id, 0)

   IF _SetGetGlobal( "_HMG_lDialogInMemory" )

      AAdd(_HMG_aDialogTreeItem, {text, aImage, Id, _HMG_NodeIndex, "ITEM", Cargo})

   ELSE

      ImgDef := iif(hb_IsArray(aImage), Len(aImage), 0)  // Tree+

      IF ImgDef == 0
         iUnsel := 2   // Pointer to defalut Item Bitmaps, no Bitmap loaded
         iSel   := 3
      ELSE
         k := _HMG_ActiveTreeIndex
         iUnSel := AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[1], _HMG_aControlMiscData1[k, 4] ) - 1
         iSel := iif(ImgDef == 1, iUnSel, AddTreeViewBitmap( _HMG_ActiveTreeHandle, aImage[2], _HMG_aControlMiscData1[k, 4] ) - 1)
         // If only one bitmap in array iSel = iUnsel, only one Bitmap loaded
      ENDIF

      handle := AddTreeItem ( _HMG_ActiveTreeHandle, _HMG_NodeHandle[_HMG_NodeIndex], text, iUnSel, iSel, Id, .F. )
      AAdd(_HMG_aTreeMap, Handle)
      AAdd(_HMG_aTreeIdMap, Id)
      AAdd(a_Node_Item_Cargo, Cargo)

   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _EndTree()
*-----------------------------------------------------------------------------*

   IF !_SetGetGlobal( "_HMG_lDialogInMemory" )

      _HMG_aControlPageMap  [_HMG_ActiveTreeIndex] := _HMG_aTreeMap
      _HMG_aControlPicture  [_HMG_ActiveTreeIndex] := _HMG_aTreeIdMap
      _HMG_aControlHeadClick[_HMG_ActiveTreeIndex] := AClone(a_Node_Item_Cargo)

      IF _HMG_ActiveTreeValue > 0

         IF !_HMG_ActiveTreeItemIds
            TreeView_SelectItem ( _HMG_ActiveTreeHandle, _HMG_aTreeMap[_HMG_ActiveTreeValue] )
         ELSE
            TreeView_SelectItem ( _HMG_ActiveTreeHandle, _HMG_aTreeMap[AScan(_HMG_aTreeIdMap, _HMG_ActiveTreeValue)] )
         ENDIF

      ENDIF

   ENDIF
      
   a_Node_Item_Cargo := {}

RETURN NIL

*-----------------------------------------------------------------------------*
PROCEDURE _Collapse ( ControlName, ParentForm, nItem, lRecurse )   // Dr. Claudio Soto (November 2013)
*-----------------------------------------------------------------------------*
   
   LOCAL i
   LOCAL ItemHandle

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0
      ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)
      IF ItemHandle != 0
         TreeView_ExpandChildrenRecursive ( _HMG_aControlHandles[i], ItemHandle, TVE_COLLAPSE, hb_defaultValue(lRecurse, .F.) )
      ENDIF
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _Expand(ControlName, ParentForm, nItem, lRecurse)   // Dr. Claudio Soto (November 2013)
*-----------------------------------------------------------------------------*
   
   LOCAL i
   LOCAL ItemHandle

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0
      ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)
      IF ItemHandle != 0
         TreeView_ExpandChildrenRecursive ( _HMG_aControlHandles[i], ItemHandle, TVE_EXPAND, hb_defaultValue(lRecurse, .F.) )
      ENDIF
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION TreeItemGetHandle(ControlName, ParentForm, Item)
*-----------------------------------------------------------------------------*
   
   LOCAL ItemHandle := 0
   LOCAL Pos
   LOCAL i

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0

      IF !_HMG_aControlInputMask[i]
         IF Item <= Len(_HMG_aControlPageMap[i])
            ItemHandle := _HMG_aControlPageMap[i][Item]
         ENDIF
      ELSE
         Pos := AScan(_HMG_aControlPicture[i], Item)
         IF Pos > 0
            ItemHandle := _HMG_aControlPageMap[i][Pos]
         ENDIF
      ENDIF

   ENDIF

RETURN ItemHandle

*-----------------------------------------------------------------------------*
PROCEDURE TreeItemChangeImage ( ControlName, ParentForm, nItem, aImage )
*-----------------------------------------------------------------------------*
   
   LOCAL TreeHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)
   LOCAL ImgDef
   LOCAL k
   LOCAL iUnSel
   LOCAL iSel

   IF ItemHandle > 0 .AND. hb_IsArray(aImage) .AND. ( ImgDef := Len(aImage) ) > 0
      k := GetControlIndex(ControlName, ParentForm)
      iUnSel := AddTreeViewBitmap( TreeHandle, aImage[1], _HMG_aControlMiscData1[k, 4] ) - 1
      iSel := iif(ImgDef == 1, iUnSel, AddTreeViewBitmap( TreeHandle, aImage[2], _HMG_aControlMiscData1[k, 4] ) - 1)

      TREEITEM_SETIMAGEINDEX ( TreeHandle, ItemHandle, iUnSel, iSel )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION TreeItemGetRootValue ( ControlName, ParentForm )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeView_GetRoot ( nControlHandle )
   LOCAL nPos
   LOCAL nID
   LOCAL i

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0 .AND. ItemHandle != 0
      IF !_HMG_aControlInputMask[i]
         nPos := AScan(_HMG_aControlPageMap[i], ItemHandle)
         RETURN nPos
      ELSE
         nID := TREEITEM_GETID(nControlHandle, ItemHandle)
         RETURN nID
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION TreeItemGetParentValue ( ControlName , ParentForm , nItem )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeView_GetParent(nControlHandle, TreeItemGetHandle(ControlName, ParentForm, nItem))
   LOCAL nPos
   LOCAL nID
   LOCAL i

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0 .AND. ItemHandle != 0
      IF !_HMG_aControlInputMask[i]
         nPos := AScan(_HMG_aControlPageMap[i], ItemHandle)
         RETURN nPos
      ELSE
         nID := TREEITEM_GETID(nControlHandle, ItemHandle)
         RETURN nID
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION TreeItemGetFirstItemValue ( ControlName, ParentForm )
*-----------------------------------------------------------------------------*
   
   LOCAL nIndex := GetControlIndex(ControlName, ParentForm)
   LOCAL nID
   LOCAL nPos := 1

   IF GetProperty ( ParentForm, ControlName, "ItemCount" ) > 0
      IF !_HMG_aControlInputMask[nIndex]
         RETURN nPos
      ELSE
         nID := _HMG_aControlPicture[nIndex][nPos]
         RETURN nID
      ENDIF
   ENDIF

RETURN NIL

#define TREESORTNODE_MIX    2
*-----------------------------------------------------------------------------*
PROCEDURE TreeItemSort ( cTreeName, cFormName, nItem, lRecurse, lCaseSensitive, lAscendingOrder, nNodePosition )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle
   LOCAL nItemHandle

   nControlHandle := GetControlHandle(cTreeName, cFormName)

   IF nItem == NIL
      nItemHandle := TreeView_GetRoot ( nControlHandle )
   ELSE
      nItemHandle := TreeItemGetHandle(cTreeName, cFormName, nItem)
   ENDIF

   hb_default(@lRecurse, .T.)
   hb_default(@lCaseSensitive, .F.)
   hb_default(@lAscendingOrder, .T.)
   hb_default(@nNodePosition, TREESORTNODE_MIX)

   TreeView_SortChildrenRecursiveCB(nControlHandle, nItemHandle, lRecurse, lCaseSensitive, lAscendingOrder, nNodePosition)

RETURN

*-----------------------------------------------------------------------------*
FUNCTION TreeItemIsTrueNode ( ControlName , ParentForm , nItem )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)

RETURN !Empty(TreeView_GetChild(nControlHandle, ItemHandle))

*-----------------------------------------------------------------------------*
FUNCTION TreeItemSetNodeFlag ( ControlName , ParentForm , nItem , lNodeFlag )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)

RETURN TREEITEM_SETNODEFLAG ( nControlHandle, ItemHandle, lNodeFlag )

*-----------------------------------------------------------------------------*
FUNCTION TreeItemGetNodeFlag ( ControlName , ParentForm , nItem )
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle := TreeItemGetHandle(ControlName, ParentForm, nItem)

RETURN TREEITEM_GETNODEFLAG ( nControlHandle, ItemHandle )

#define TVIS_EXPANDED   32
*-----------------------------------------------------------------------------*
FUNCTION TreeItemIsExpand(ControlName, ParentForm, nItem)
*-----------------------------------------------------------------------------*
   
   LOCAL nControlHandle := GetControlHandle(ControlName, ParentForm)
   LOCAL ItemHandle     := TreeItemGetHandle(ControlName, ParentForm, nItem)

RETURN ( hb_bitand(TreeView_GetItemState(nControlHandle, ItemHandle, TVIS_EXPANDED), TVIS_EXPANDED) == TVIS_EXPANDED )

*-----------------------------------------------------------------------------*
FUNCTION TreeNodeItemCargo( ControlName, ParentForm, Item, Value )
*-----------------------------------------------------------------------------*
   
   LOCAL i
   LOCAL xData
   LOCAL Pos

   IF ( i := GetControlIndex(ControlName, ParentForm) ) > 0

      IF !_HMG_aControlInputMask[i]

         IF Item > 0 .AND. Item <= Len(_HMG_aControlHeadClick[i])
            xData := _HMG_aControlHeadClick[i][Item]
            IF PCount() > 3
               _HMG_aControlHeadClick[i][Item] := Value
            ENDIF
         ENDIF

      ELSE

         Pos := AScan(_HMG_aControlPicture[i], Item)
         IF Pos > 0 .AND. Pos <= Len(_HMG_aControlHeadClick[i])
            xData := _HMG_aControlHeadClick[i][Pos]
            IF PCount() > 3
               _HMG_aControlHeadClick[i][Pos] := Value
            ENDIF
         ENDIF

      ENDIF

   ENDIF
   
RETURN xData
