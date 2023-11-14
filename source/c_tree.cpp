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

#define _WIN32_IE  0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbwinuni.hpp>

HIMAGELIST HMG_ImageListLoadFirst(const char * FileName, int cGrow, int Transparent, int * nWidth, int * nHeight);
void HMG_ImageListAdd(HIMAGELIST himl, char * FileName, int Transparent);

/*
INITTREE(HWND, p2, p3, p4, p5, p6, p7, p8, p9) --> HWND
*/
HB_FUNC( INITTREE )
{
   INITCOMMONCONTROLSEX icex{};
   icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
   icex.dwICC  = ICC_TREEVIEW_CLASSES;
   InitCommonControlsEx(&icex);

   UINT mask;
   if( hb_parni(9) != 0 ) { //Tree+
      mask = 0x0000;
   } else {
      mask = TVS_LINESATROOT;
   }

   HWND hWndTV = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      WC_TREEVIEW,
      TEXT(""),
      WS_VISIBLE | WS_TABSTOP | WS_CHILD | TVS_HASLINES | TVS_HASBUTTONS | mask | TVS_SHOWSELALWAYS,
      hb_parni(2),
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hmg_par_HWND(1),
      hmg_par_HMENU(6),
      GetInstance(),
      nullptr);

   hmg_ret_HWND(hWndTV);
}

/*
INITTREEVIEWBITMAP(HWND, p2, p3) --> numeric
*/
HB_FUNC( INITTREEVIEWBITMAP ) //Tree+
{
   int nCount = hb_parinfa(2, 0);
   char * FileName;
   HIMAGELIST himl = nullptr;
   int ic = 0;

   if( nCount > 0 ) {
      int Transparent = hb_parl(3) ? 0 : 1;
      auto hArray = hb_param(2, Harbour::Item::ARRAY);

      for( int s = 1; s <= nCount; s++ ) {
         FileName = const_cast<char*>(hb_arrayGetCPtr(hArray, s)); // TODO: unicode

         if( himl == nullptr ) {
            himl = HMG_ImageListLoadFirst(FileName, nCount, Transparent, nullptr, nullptr);
         } else {
            HMG_ImageListAdd(himl, FileName, Transparent);
         }
      }

      if( himl != nullptr ) {
         SendMessage(hmg_par_HWND(1), TVM_SETIMAGELIST, TVSIL_NORMAL, reinterpret_cast<LPARAM>(himl));
      }

      ic = ImageList_GetImageCount(himl);
   }

   hb_retni(ic);
}

/*
ADDTREEVIEWBITMAP(HWND, p2, p3) --> numeric
*/
HB_FUNC( ADDTREEVIEWBITMAP )  // Tree+
{
   int Transparent = hb_parl(3) ? 0 : 1;
   int ic = 0;
   auto hbutton = hmg_par_HWND(1);
   HIMAGELIST himl = TreeView_GetImageList(hbutton, TVSIL_NORMAL);
   if( himl != nullptr ) {
      HMG_ImageListAdd(himl, const_cast<char*>(hb_parc(2)), Transparent);
      SendMessage(hbutton, TVM_SETIMAGELIST, TVSIL_NORMAL, reinterpret_cast<LPARAM>(himl));
      ic = ImageList_GetImageCount(himl);
   }
   hb_retni(ic);
}

#define MAX_ITEM_TEXT  256

struct HMG_StructTreeItemLPARAM
{
   HTREEITEM ItemHandle;
   LONG      nID;
   BOOL      IsNodeFlag;
} ;

void AddTreeItemLPARAM(HWND hWndTV, HTREEITEM ItemHandle, LONG nID, BOOL IsNodeFlag)
{
   if( ( hWndTV != nullptr ) && ( ItemHandle != nullptr ) ) {
      auto TreeItemLPARAM = static_cast<HMG_StructTreeItemLPARAM*>(hb_xgrab(sizeof(HMG_StructTreeItemLPARAM)));
      TreeItemLPARAM->ItemHandle = ItemHandle;
      TreeItemLPARAM->nID        = nID;
      TreeItemLPARAM->IsNodeFlag = IsNodeFlag;
      TV_ITEM TV_Item;
      TV_Item.mask   = TVIF_PARAM;
      TV_Item.hItem  = ( HTREEITEM ) ItemHandle;
      TV_Item.lParam = ( LPARAM ) TreeItemLPARAM;
      TreeView_SetItem(hWndTV, &TV_Item);
   }
}

/*
ADDTREEITEM(HWND, HTREEITEM, cText, p4, p5, p6, p7) --> HWND
*/
HB_FUNC( ADDTREEITEM )
{
   HTREEITEM hPrev = hmg_par_HTREEITEM(2);
   LONG nID        = hmg_par_LONG(6);
   BOOL IsNodeFlag = hmg_par_BOOL(7);

   TV_ITEM tvi;
   tvi.mask = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_PARAM;
   void * str;
   tvi.pszText        = const_cast<TCHAR*>(HB_PARSTR(3, &str, nullptr));
   tvi.cchTextMax     = 1024;
   tvi.iImage         = hb_parni(4);
   tvi.iSelectedImage = hb_parni(5);
   tvi.lParam         = nID;

   TV_INSERTSTRUCT is;
   is.item = tvi;
   if( hPrev == nullptr ) {
      is.hInsertAfter = hPrev;
      is.hParent      = nullptr;
   } else {
      is.hInsertAfter = TVI_LAST;
      is.hParent      = hPrev;
   }

   auto hWndTV = hmg_par_HWND(1);
   HTREEITEM hRet = TreeView_InsertItem(hWndTV, &is);
   AddTreeItemLPARAM(hWndTV, hRet, nID, IsNodeFlag);
   hmg_ret_HTREEITEM(hRet);
   hb_strfree(str);
}

/*
TREEVIEW_GETSELECTION(HWND) --> HTREEITEM|NIL
*/
HB_FUNC( TREEVIEW_GETSELECTION )
{
   HTREEITEM ItemHandle = TreeView_GetSelection(hmg_par_HWND(1));
   if( ItemHandle != nullptr ) {
      hmg_ret_HTREEITEM(ItemHandle);
   }
}

/*
TREEVIEW_SELECTITEM(HWND, HTREEITEM) --> NIL
*/
HB_FUNC( TREEVIEW_SELECTITEM )
{
   TreeView_SelectItem(hmg_par_HWND(1), hmg_par_HTREEITEM(2));
}

void TreeView_FreeMemoryLPARAMRecursive(HWND hWndTV, HTREEITEM ItemHandle)
{
   TV_ITEM   TreeItem;
   TreeItem.mask   = TVIF_PARAM;
   TreeItem.hItem  = ItemHandle;
   TreeItem.lParam = ( LPARAM ) nullptr;
   TreeView_GetItem(hWndTV, &TreeItem);

   HMG_StructTreeItemLPARAM * TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   if( TreeItemLPARAM != nullptr ) {
      hb_xfree(TreeItemLPARAM);
      TreeItem.lParam = ( LPARAM ) nullptr;      // for security set lParam = nullptr
      TreeView_SetItem(hWndTV, &TreeItem);
   }

   HTREEITEM ChildItem = TreeView_GetChild(hWndTV, ItemHandle);
   HTREEITEM NextItem;
   while( ChildItem != nullptr ) {
      TreeView_FreeMemoryLPARAMRecursive(hWndTV, ChildItem);
      NextItem  = TreeView_GetNextSibling(hWndTV, ChildItem);
      ChildItem = NextItem;
   }
}

/*
TREEVIEW_DELETEITEM(HWND, HTREEITEM) --> NIL
*/
HB_FUNC( TREEVIEW_DELETEITEM )
{
   auto TreeHandle = hmg_par_HWND(1);
   HTREEITEM ItemHandle = hmg_par_HTREEITEM(2);
   TreeView_FreeMemoryLPARAMRecursive(TreeHandle, ItemHandle);
   TreeView_DeleteItem(TreeHandle, ItemHandle);
}

/*
TREEVIEW_DELETEALLITEMS(HWND, p2) -->
*/
HB_FUNC( TREEVIEW_DELETEALLITEMS )
{
   int nCount = hb_parinfa(2, 0);
   TV_ITEM TreeItem;
   auto TreeHandle = hmg_par_HWND(1);
   HMG_StructTreeItemLPARAM * TreeItemLPARAM;
   for( int i = 1; i <= nCount; i++ ) {
      TreeItem.mask   = TVIF_PARAM;
      TreeItem.hItem  = ( HTREEITEM ) HB_PARVNL(2, i);
      TreeItem.lParam = 0;
      TreeView_GetItem(TreeHandle, &TreeItem);
      TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      if( TreeItemLPARAM != nullptr ) {
         hb_xfree(TreeItemLPARAM);
      }
   }
   TreeView_DeleteAllItems(TreeHandle);
}

/*
TREEVIEW_GETCOUNT(HWND) --> numeric
*/
HB_FUNC( TREEVIEW_GETCOUNT )
{
   hb_retni(TreeView_GetCount(hmg_par_HWND(1)));
}

/*
TREEVIEW_GETPREVSIBLING(HWND, HTREEITEM) --> HTREEITEM
*/
HB_FUNC( TREEVIEW_GETPREVSIBLING )
{
   hmg_ret_HTREEITEM(TreeView_GetPrevSibling(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
TREEVIEW_GETITEM(HWND, HTREEITEM) --> string
*/
HB_FUNC( TREEVIEW_GETITEM )
{
   TCHAR ItemText[MAX_ITEM_TEXT];
   TV_ITEM TreeItem{};
   TreeItem.mask       = TVIF_TEXT;
   TreeItem.hItem      = hmg_par_HTREEITEM(2);
   TreeItem.pszText    = ItemText;
   TreeItem.cchTextMax = sizeof(ItemText) / sizeof(TCHAR);
   TreeView_GetItem(hmg_par_HWND(1), &TreeItem);
   HB_RETSTR(ItemText);
}

/*
TREEVIEW_SETITEM(HWND, HTREEITEM) --> NIL
*/
HB_FUNC( TREEVIEW_SETITEM )
{
   TCHAR ItemText[MAX_ITEM_TEXT];
   TV_ITEM TreeItem{};
   void * str;
   lstrcpy(ItemText, HB_PARSTR(3, &str, nullptr));
   TreeItem.mask       = TVIF_TEXT;
   TreeItem.hItem      = hmg_par_HTREEITEM(2);
   TreeItem.pszText    = ItemText;
   TreeItem.cchTextMax = sizeof(ItemText) / sizeof(TCHAR);
   TreeView_SetItem(hmg_par_HWND(1), &TreeItem);
   hb_strfree(str);
}

/*
TREEITEM_SETIMAGEINDEX(HWND, HTREEITEM, np3, np4) --> NIL
*/
HB_FUNC( TREEITEM_SETIMAGEINDEX )
{
   TV_ITEM TreeItem;
   TreeItem.mask           = TVIF_IMAGE | TVIF_SELECTEDIMAGE;
   TreeItem.hItem          = hmg_par_HTREEITEM(2);
   TreeItem.iImage         = hb_parni(3);
   TreeItem.iSelectedImage = hb_parni(4);
   TreeView_SetItem(hmg_par_HWND(1), &TreeItem);
}

/*
TREEVIEW_GETSELECTIONID(HWND) --> numeric
*/
HB_FUNC( TREEVIEW_GETSELECTIONID )
{
   auto TreeHandle = hmg_par_HWND(1);
   HTREEITEM ItemHandle = TreeView_GetSelection(TreeHandle);
   if( ItemHandle != nullptr ) {
      TV_ITEM TreeItem;
      TreeItem.mask   = TVIF_PARAM;
      TreeItem.hItem  = ItemHandle;
      TreeItem.lParam = 0;
      TreeView_GetItem(TreeHandle, &TreeItem);
      HMG_StructTreeItemLPARAM * TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      hb_retnl( TreeItemLPARAM->nID );
   }
}

/*
TREEVIEW_GETNEXTSIBLING(HWND, HTREEITEM) --> HTREEITEM
*/
HB_FUNC( TREEVIEW_GETNEXTSIBLING )
{
   hmg_ret_HTREEITEM(TreeView_GetNextSibling(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
TREEVIEW_GETCHILD(HWND, HTREEITEM) --> HTREEITEM
*/
HB_FUNC( TREEVIEW_GETCHILD )
{
   hmg_ret_HTREEITEM(TreeView_GetChild(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
TREEVIEW_GETPARENT(HWND, HTREEITEM) --> HTREEITEM
*/
HB_FUNC( TREEVIEW_GETPARENT )
{
   hmg_ret_HTREEITEM(TreeView_GetParent(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

//**************************************************
//    by  Dr. Claudio Soto  (November 2013)
//**************************************************

/*
TREEVIEW_GETITEMSTATE(HWND, HTREEITEM, StateMask) --> nState
*/
HB_FUNC( TREEVIEW_GETITEMSTATE )
{
   hb_retni(TreeView_GetItemState(hmg_par_HWND(1), hmg_par_HTREEITEM(2), hmg_par_UINT(3)));
}

BOOL TreeView_IsNode(HWND hWndTV, HTREEITEM ItemHandle)
{
   return (TreeView_GetChild(hWndTV, ItemHandle) != nullptr) ? TRUE : FALSE;
}

//--------------------------------------------------------------------------------------------------------
//   TreeView_ExpandChildrenRecursive ( hWndTV, ItemHandle, nExpand, fRecurse )
//--------------------------------------------------------------------------------------------------------

void TreeView_ExpandChildrenRecursive(HWND hWndTV, HTREEITEM ItemHandle, UINT nExpand)
{
   if( TreeView_IsNode(hWndTV, ItemHandle) ) {
      TreeView_Expand(hWndTV, ItemHandle, nExpand);
      HTREEITEM ChildItem = TreeView_GetChild(hWndTV, ItemHandle);
      HTREEITEM NextItem;
      while( ChildItem != nullptr ) {
         TreeView_ExpandChildrenRecursive(hWndTV, ChildItem, nExpand);
         NextItem  = TreeView_GetNextSibling(hWndTV, ChildItem);
         ChildItem = NextItem;
      }
   }
}

/*
TREEVIEW_EXPANDCHILDRENRECURSIVE(HWND, HTREEITEM, nExpand, lRecurse) --> NIL
*/
HB_FUNC( TREEVIEW_EXPANDCHILDRENRECURSIVE )
{
   auto hWndTV = hmg_par_HWND(1);
   HTREEITEM ItemHandle = hmg_par_HTREEITEM(2);
   UINT      nExpand    = hmg_par_UINT(3);
   BOOL      fRecurse   = hmg_par_BOOL(4);
   HWND      hWndParent = GetParent(hWndTV);
   BOOL      lEnabled   = IsWindowEnabled(hWndParent);

   if( fRecurse == FALSE ) {
      TreeView_Expand(hWndTV, ItemHandle, nExpand);
   } else {
      EnableWindow(hWndParent, FALSE);
      TreeView_ExpandChildrenRecursive(hWndTV, ItemHandle, nExpand);
      if( lEnabled == TRUE ) {
         EnableWindow(hWndParent, TRUE);
      }
   }
}

//---------------------------------------------------------------------------------------------------------------------
// TreeView_SortChildrenRecursiveCB(hWndTV, ItemHandle, fRecurse, lCaseSensitive, lAscendingOrder, nNodePosition)
//---------------------------------------------------------------------------------------------------------------------

#define SORTTREENODE_FIRST  0
#define SORTTREENODE_LAST   1
#define SORTTREENODE_MIX    2

struct HMG_StructTreeViewCompareInfo
{
   HWND hWndTV;
   BOOL CaseSensitive;
   BOOL AscendingOrder;
   int  NodePosition;
};

int CALLBACK TreeViewCompareFunc( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort )
{
   HMG_StructTreeItemLPARAM * TreeItemLPARAM1 = ( HMG_StructTreeItemLPARAM * ) lParam1;
   HMG_StructTreeItemLPARAM * TreeItemLPARAM2 = ( HMG_StructTreeItemLPARAM * ) lParam2;

   HMG_StructTreeViewCompareInfo * TreeViewCompareInfo = ( HMG_StructTreeViewCompareInfo * ) lParamSort;

   HWND hWndTV = TreeViewCompareInfo->hWndTV;

   HTREEITEM ItemHandle1 = ( HTREEITEM ) TreeItemLPARAM1->ItemHandle;
   HTREEITEM ItemHandle2 = ( HTREEITEM ) TreeItemLPARAM2->ItemHandle;

   BOOL IsTreeNode1;
   BOOL IsTreeNode2;
   int  CmpValue;

   TCHAR   ItemText1[MAX_ITEM_TEXT];
   TV_ITEM TV_Item1;
   TCHAR   ItemText2[MAX_ITEM_TEXT];
   TV_ITEM TV_Item2;

   TV_Item1.mask       = TVIF_TEXT;
   TV_Item1.pszText    = ItemText1;
   TV_Item1.cchTextMax = sizeof(ItemText1) / sizeof(TCHAR);
   TV_Item1.hItem      = ( HTREEITEM ) ItemHandle1;
   TreeView_GetItem(hWndTV, &TV_Item1);

   TV_Item2.mask       = TVIF_TEXT;
   TV_Item2.pszText    = ItemText2;
   TV_Item2.cchTextMax = sizeof(ItemText2) / sizeof(TCHAR);
   TV_Item2.hItem      = ( HTREEITEM ) ItemHandle2;
   TreeView_GetItem(hWndTV, &TV_Item2);

   IsTreeNode1 = ( TreeItemLPARAM1->IsNodeFlag == TRUE || TreeView_GetChild(hWndTV, ItemHandle1) != nullptr ) ? TRUE : FALSE;
   IsTreeNode2 = ( TreeItemLPARAM2->IsNodeFlag == TRUE || TreeView_GetChild(hWndTV, ItemHandle2) != nullptr ) ? TRUE : FALSE;

   if( TreeViewCompareInfo->CaseSensitive == FALSE ) {
      CmpValue = lstrcmpi( ItemText1, ItemText2 );
   } else {
      CmpValue = lstrcmp(ItemText1, ItemText2);
   }

   if( TreeViewCompareInfo->AscendingOrder == FALSE ) {
      CmpValue = CmpValue * ( -1 );
   }

   if( TreeViewCompareInfo->NodePosition == SORTTREENODE_FIRST ) {
      if( IsTreeNode1 == TRUE && IsTreeNode2 == FALSE ) {
         return -1;
      }
      if( IsTreeNode1 == FALSE && IsTreeNode2 == TRUE ) {
         return +1;
      }
   }

   if( TreeViewCompareInfo->NodePosition == SORTTREENODE_LAST ) {
      if( IsTreeNode1 == TRUE && IsTreeNode2 == FALSE ) {
         return +1;
      }
      if( IsTreeNode1 == FALSE && IsTreeNode2 == TRUE ) {
         return -1;
      }
   }

   return CmpValue;
}

void TreeView_SortChildrenRecursiveCB(HWND hWndTV, TVSORTCB TVSortCB)
{
   if( TreeView_IsNode(hWndTV, TVSortCB.hParent) ) {
      TreeView_SortChildrenCB(hWndTV, &TVSortCB, 0);
      HTREEITEM ChildItem = TreeView_GetChild(hWndTV, TVSortCB.hParent);
      HTREEITEM NextItem;
      while( ChildItem != nullptr ) {
         TVSortCB.hParent = ( HTREEITEM ) ChildItem;
         TreeView_SortChildrenRecursiveCB(hWndTV, TVSortCB);
         NextItem  = TreeView_GetNextSibling(hWndTV, ChildItem);
         ChildItem = NextItem;
      }
   }
}

/*
TREEVIEW_SORTCHILDRENRECURSIVECB() -->
*/
HB_FUNC( TREEVIEW_SORTCHILDRENRECURSIVECB )
{
   auto hWndTV = hmg_par_HWND(1);
   HTREEITEM ItemHandle      = hmg_par_HTREEITEM(2);
   BOOL      fRecurse        = hmg_par_BOOL(3);
   BOOL      lCaseSensitive  = hmg_par_BOOL(4);
   BOOL      lAscendingOrder = hmg_par_BOOL(5);
   auto nNodePosition = hmg_par_INT(6);
   HWND      hWndParent      = GetParent(hWndTV);
   BOOL      lEnabled        = IsWindowEnabled(hWndParent);

   TVSORTCB TVSortCB;
   HMG_StructTreeViewCompareInfo TreeViewCompareInfo;

   TreeViewCompareInfo.hWndTV         = hWndTV;
   TreeViewCompareInfo.CaseSensitive  = lCaseSensitive;
   TreeViewCompareInfo.AscendingOrder = lAscendingOrder;
   TreeViewCompareInfo.NodePosition   = nNodePosition;

   TVSortCB.hParent     = ( HTREEITEM ) ItemHandle;
   TVSortCB.lpfnCompare = ( PFNTVCOMPARE ) TreeViewCompareFunc;
   TVSortCB.lParam      = reinterpret_cast<LPARAM>(&TreeViewCompareInfo);

   if( fRecurse == FALSE ) {
      TreeView_SortChildrenCB(hWndTV, &TVSortCB, 0);
   } else {
      EnableWindow(hWndParent, FALSE);

      TreeView_SortChildrenRecursiveCB(hWndTV, TVSortCB);

      if( lEnabled == TRUE ) {
         EnableWindow(hWndParent, TRUE);
      }
   }
}

/*
TREEVIEW_GETROOT(HWND) --> HTREEITEM
*/
HB_FUNC( TREEVIEW_GETROOT )
{
   hmg_ret_HTREEITEM(TreeView_GetRoot(hmg_par_HWND(1)));
}

/*
TREEITEM_GETID(HWND, HTREEITEM) --> nID
*/
HB_FUNC( TREEITEM_GETID )
{
   TV_ITEM TreeItem;
   TreeItem.mask   = TVIF_PARAM;
   TreeItem.hItem  = hmg_par_HTREEITEM(2);
   TreeItem.lParam = 0;
   if( TreeView_GetItem(hmg_par_HWND(1), &TreeItem) == TRUE ) {
      HMG_StructTreeItemLPARAM * TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      hb_retnl( TreeItemLPARAM->nID );
   }
}

/*
TREEITEM_SETNODEFLAG(HWND, HTREEITEM, lIsNodeFlag) --> NIL
*/
HB_FUNC( TREEITEM_SETNODEFLAG )
{
   auto hWndTV = hmg_par_HWND(1);
   TV_ITEM TreeItem;
   TreeItem.mask   = TVIF_PARAM;
   TreeItem.hItem  = hmg_par_HTREEITEM(2);
   TreeItem.lParam = 0;
   TreeView_GetItem(hWndTV, &TreeItem);
   HMG_StructTreeItemLPARAM * TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   TreeItemLPARAM->IsNodeFlag = hmg_par_BOOL(3);
   TreeItem.lParam = ( LPARAM ) TreeItemLPARAM;
   TreeView_SetItem(hWndTV, &TreeItem);
}

/*
TREEITEM_GETNODEFLAG() -->
*/
HB_FUNC( TREEITEM_GETNODEFLAG )
{
   TV_ITEM TreeItem;
   TreeItem.mask   = TVIF_PARAM;
   TreeItem.hItem  = hmg_par_HTREEITEM(2);
   TreeItem.lParam = 0;
   TreeView_GetItem(hmg_par_HWND(1), &TreeItem);
   HMG_StructTreeItemLPARAM * TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   hb_retl(TreeItemLPARAM->IsNodeFlag);
}
