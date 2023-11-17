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
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapicdp.hpp>

extern BOOL _isValidCtrlClass(HWND, LPCTSTR);

HIMAGELIST HMG_ImageListLoadFirst(const char * FileName, int cGrow, int Transparent, int * nWidth, int * nHeight);
void HMG_ImageListAdd(HIMAGELIST himl, char * FileName, int Transparent);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR  WideToAnsi(LPWSTR);
#endif

#if ((defined(__BORLANDC__) && __BORLANDC__ < 1410))

typedef struct tagLVITEMA2
{
   UINT   mask;
   int    iItem;
   int    iSubItem;
   UINT   state;
   UINT   stateMask;
   LPSTR  pszText;
   int    cchTextMax;
   int    iImage;
   LPARAM lParam;
   int    iIndent;
#if (NTDDI_VERSION >= NTDDI_WINXP)
   int   iGroupId;
   UINT  cColumns; // tile view columns
   PUINT puColumns;
#endif
#if (NTDDI_VERSION >= NTDDI_VISTA) // Will be unused downlevel, but sizeof(LVITEMA) must be equal to sizeof(LVITEMW)
   int * piColFmt;
   int   iGroup;                     // readonly. only valid for owner data.
#endif
} LVITEMA2, * LPLVITEMA2;

typedef struct tagLVITEMW2
{
   UINT   mask;
   int    iItem;
   int    iSubItem;
   UINT   state;
   UINT   stateMask;
   LPWSTR pszText;
   int    cchTextMax;
   int    iImage;
   LPARAM lParam;
   int    iIndent;
#if (NTDDI_VERSION >= NTDDI_WINXP)
   int   iGroupId;
   UINT  cColumns; // tile view columns
   PUINT puColumns;
#endif
#if (NTDDI_VERSION >= NTDDI_VISTA)
   int * piColFmt;
   int   iGroup; // readonly. only valid for owner data.
#endif
} LVITEMW2, * LPLVITEMW2;

#ifdef UNICODE
#define _LVITEM                 LVITEMW2
#define _LPLVITEM               LPLVITEMW2
#else
#define _LVITEM                 LVITEMA2
#define _LPLVITEM               LPLVITEMA2
#endif

#define LVGF_NONE               0x00000000
#define LVGF_HEADER             0x00000001
#define LVGF_FOOTER             0x00000002
#define LVGF_STATE              0x00000004
#define LVGF_ALIGN              0x00000008
#define LVGF_GROUPID            0x00000010

#define LVGS_NORMAL             0x00000000
#define LVGS_COLLAPSED          0x00000001
#define LVGS_HIDDEN             0x00000002
#define LVGS_NOHEADER           0x00000004
#define LVGS_COLLAPSIBLE        0x00000008
#define LVGS_FOCUSED            0x00000010
#define LVGS_SELECTED           0x00000020
#define LVGS_SUBSETED           0x00000040
#define LVGS_SUBSETLINKFOCUSED  0x00000080

#define LVGA_HEADER_LEFT        0x00000001
#define LVGA_HEADER_CENTER      0x00000002
#define LVGA_HEADER_RIGHT       0x00000004 // Don't forget to validate exclusivity
#define LVGA_FOOTER_LEFT        0x00000008
#define LVGA_FOOTER_CENTER      0x00000010
#define LVGA_FOOTER_RIGHT       0x00000020 // Don't forget to validate exclusivity

#define LVIF_GROUPID            0x100
#define LVIF_COLUMNS            0x200

typedef struct tagLVGROUP
{
   UINT   cbSize;
   UINT   mask;
   LPWSTR pszHeader;
   int    cchHeader;
   LPWSTR pszFooter;
   int    cchFooter;
   int    iGroupId;
   UINT   stateMask;
   UINT   state;
   UINT   uAlign;
} LVGROUP, * PLVGROUP;

#define LVM_ENABLEGROUPVIEW     (LVM_FIRST + 157)
#define ListView_EnableGroupView(hwnd, fEnable) \
   SNDMSG((hwnd), LVM_ENABLEGROUPVIEW, static_cast<WPARAM>(fEnable), 0)

#define LVM_REMOVEALLGROUPS     (LVM_FIRST + 160)
#define ListView_RemoveAllGroups(hwnd) \
   SNDMSG((hwnd), LVM_REMOVEALLGROUPS, 0, 0)

#define LVM_HASGROUP            (LVM_FIRST + 161)
#define ListView_HasGroup(hwnd, dwGroupId) \
   SNDMSG((hwnd), LVM_HASGROUP, dwGroupId, 0)

#define LVM_ISGROUPVIEWENABLED  (LVM_FIRST + 175)
#define ListView_IsGroupViewEnabled(hwnd) static_cast<BOOL>(SNDMSG((hwnd), LVM_ISGROUPVIEWENABLED, 0, 0))

#define LVM_INSERTGROUP         (LVM_FIRST + 145)
#define ListView_InsertGroup(hwnd, index, pgrp)      SNDMSG((hwnd), LVM_INSERTGROUP, static_cast<WPARAM>(index), static_cast<LPARAM>(pgrp))
#define LVM_SETGROUPINFO        (LVM_FIRST + 147)
#define ListView_SetGroupInfo(hwnd, iGroupId, pgrp)  SNDMSG((hwnd), LVM_SETGROUPINFO, static_cast<WPARAM>(iGroupId), static_cast<LPARAM>(pgrp))
#define LVM_GETGROUPINFO        (LVM_FIRST + 149)
#define ListView_GetGroupInfo(hwnd, iGroupId, pgrp)  SNDMSG((hwnd), LVM_GETGROUPINFO, static_cast<WPARAM>(iGroupId), static_cast<LPARAM>(pgrp))
#define LVM_REMOVEGROUP         (LVM_FIRST + 150)
#define ListView_RemoveGroup(hwnd, iGroupId)         SNDMSG((hwnd), LVM_REMOVEGROUP, static_cast<WPARAM>(iGroupId), 0)
#define LVM_MOVEGROUP           (LVM_FIRST + 151)
#define ListView_MoveGroup(hwnd, iGroupId, toIndex)  SNDMSG((hwnd), LVM_MOVEGROUP, static_cast<WPARAM>(iGroupId), static_cast<LPARAM>(toIndex))
#define LVM_GETGROUPCOUNT       (LVM_FIRST + 152)
#define ListView_GetGroupCount(hwnd)                 SNDMSG((hwnd), LVM_GETGROUPCOUNT, 0, 0)

#endif

/*
INITLISTVIEW() -->
*/
HB_FUNC( INITLISTVIEW )
{
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC  = ICC_LISTVIEW_CLASSES;
   InitCommonControlsEx(&i);

   DWORD style = LVS_SHOWSELALWAYS | WS_CHILD | WS_VISIBLE | LVS_REPORT;

   if( !hb_parl(9) ) {
      style |= LVS_SINGLESEL;
   }

   if( !hb_parl(12) ) {
      style |= WS_TABSTOP;
   }

   if( !hb_parl(10) ) {
      style |= LVS_NOCOLUMNHEADER;
   }
   else if( hb_parl(11) ) {
      style |= LVS_NOSORTHEADER;
   }

   if( hb_parl(7) ) {
      style |= LVS_OWNERDATA;
   }

   auto hbutton = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      WC_LISTVIEW,
      "",
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(6),
      hmg_par_HWND(1),
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr);

   if( hb_parl(7) ) {
      ListView_SetItemCount(hbutton, hb_parni(8));
   }

   hmg_ret_HWND(hbutton);
}

/*
LISTVIEW_SETITEMCOUNT() -->
*/
HB_FUNC( LISTVIEW_SETITEMCOUNT )
{
   ListView_SetItemCount(hmg_par_HWND(1), hb_parni(2));
}

/*
ADDLISTVIEWBITMAP() -->
*/
HB_FUNC( ADDLISTVIEWBITMAP )       // Grid+
{
   int cx = 0;

   auto nCount = static_cast<int>(hb_parinfa(2, 0));

   if( nCount > 0 ) {
      auto hArray = hb_param(2, Harbour::Item::ARRAY);
      HIMAGELIST himl = nullptr;
      char * FileName;
      for( int s = 1; s <= nCount; s++ ) {
         FileName = const_cast<char*>(hb_arrayGetCPtr(hArray, s));
         if( himl == nullptr ) {
            himl = HMG_ImageListLoadFirst(FileName, nCount, 1, &cx, nullptr);
         } else {
            HMG_ImageListAdd(himl, FileName, 1);
         }
      }
      if( himl != nullptr ) {
         SendMessage(hmg_par_HWND(1), LVM_SETIMAGELIST, LVSIL_SMALL, reinterpret_cast<LPARAM>(himl));
      }
   }

   hb_retni(cx);
}

/*
ADDLISTVIEWBITMAPHEADER() -->
*/
HB_FUNC( ADDLISTVIEWBITMAPHEADER )  // Grid+
{
   HWND hheader = ListView_GetHeader(hmg_par_HWND(1));
   HIMAGELIST himl = nullptr;

   if( hheader ) {
      auto nCount = static_cast<int>(hb_parinfa(2, 0));
      if( nCount > 0 ) {
         auto hArray = hb_param(2, Harbour::Item::ARRAY);
         char * FileName;
         for( int s = 1; s <= nCount; s++ ) {
            FileName = const_cast<char*>(hb_arrayGetCPtr(hArray, s));
            if( himl == nullptr ) {
               himl = HMG_ImageListLoadFirst(FileName, nCount, 1, nullptr, nullptr);
            } else {
               HMG_ImageListAdd(himl, FileName, 1);
            }
         }
         if( himl != nullptr ) {
            SendMessage(hheader, HDM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(himl));
            RegisterResource(himl, "IMAGELIST");
         }
      }
   }

   hmg_ret_HIMAGELIST(himl);
}

/*
LISTVIEW_GETFOCUSEDITEM() -->
*/
HB_FUNC( LISTVIEW_GETFOCUSEDITEM )
{
   hb_retni(ListView_GetNextItem(hmg_par_HWND(1), -1, LVNI_ALL | LVNI_FOCUSED) + 1);
}

/*
LISTVIEW_GETFIRSTITEM() -->
*/
HB_FUNC( LISTVIEW_GETFIRSTITEM )
{
   hb_retni(ListView_GetNextItem(hmg_par_HWND(1), -1, LVNI_ALL | LVNI_SELECTED) + 1);
}

/* code INITLISTVIEWCOLUMNS function was borrowed from ooHG */

/*
INITLISTVIEWCOLUMNS() -->
*/
HB_FUNC( INITLISTVIEWCOLUMNS )
{
#ifndef UNICODE
   LPSTR lpText;
#else
   LPWSTR lpText;
#endif
   int iLen   = static_cast<int>(hb_parinfa(2, 0)) - 1;
   auto hArray = hb_param(2, Harbour::Item::ARRAY);
   auto wArray = hb_param(3, Harbour::Item::ARRAY);
   auto jArray = hb_param(4, Harbour::Item::ARRAY);

   LV_COLUMN COL;
   COL.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

   int s;
   int iColumn = 0;
   auto hc = hmg_par_HWND(1);

   for( s = 0; s <= iLen; s++ ) {
      COL.fmt = hb_arrayGetNI(jArray, s + 1);
      COL.cx  = hb_arrayGetNI(wArray, s + 1);
#ifndef UNICODE
      lpText = const_cast<char*>(hb_arrayGetCPtr(hArray, s + 1));
#else
      lpText = AnsiToWide(static_cast<char*>(hb_arrayGetCPtr(hArray, s + 1)));
#endif
      COL.pszText  = lpText;
      COL.iSubItem = iColumn;
      ListView_InsertColumn(hc, iColumn, &COL);
      if( iColumn == 0 && COL.fmt != LVCFMT_LEFT ) {
         iColumn++;
         COL.iSubItem = iColumn;
         ListView_InsertColumn(hc, iColumn, &COL);
      }

      iColumn++;

#ifdef UNICODE
      hb_xfree(lpText);
#endif
   }

   if( iColumn != s ) {
      ListView_DeleteColumn(hc, 0);
   }
}

/*
ADDLISTVIEWITEMS() -->
*/
HB_FUNC( ADDLISTVIEWITEMS )
{
#ifndef UNICODE
   LPSTR lpText;
#else
   LPWSTR lpText;
#endif

   auto h = hmg_par_HWND(1);
   int l = static_cast<int>(hb_parinfa(2, 0)) - 1;
   auto hArray = hb_param(2, Harbour::Item::ARRAY);
   int c = ListView_GetItemCount(h);

   char * caption = const_cast<char*>(hb_arrayGetCPtr(hArray, 1));
#ifndef UNICODE
   lpText = caption;
#else
   lpText = AnsiToWide(static_cast<char*>(caption));
#endif

   LV_ITEM LI{};
   LI.mask      = LVIF_TEXT | LVIF_IMAGE;
   //LI.state     = 0;
   //LI.stateMask = 0;
   LI.iImage    = hb_parni(3);
   //LI.iSubItem  = 0;
   LI.iItem     = c;
   LI.pszText   = lpText;
   ListView_InsertItem(h, &LI);

   for( int s = 1; s <= l; s = s + 1 ) {
      caption = const_cast<char*>(hb_arrayGetCPtr(hArray, s + 1));
#ifndef UNICODE
      lpText = caption;
#else
      lpText = AnsiToWide(static_cast<char*>(caption));
#endif
      ListView_SetItemText(h, c, s, lpText);
   }

#ifdef UNICODE
   hb_xfree(lpText);
#endif
}

/*
LISTVIEW_SETCURSEL() -->
*/
HB_FUNC( LISTVIEW_SETCURSEL )
{
   ListView_SetItemState(hmg_par_HWND(1), hmg_par_WPARAM(2) - 1, LVIS_FOCUSED | LVIS_SELECTED, LVIS_FOCUSED | LVIS_SELECTED);
}

/*
LISTVIEWGETMULTISEL() -->
*/
HB_FUNC( LISTVIEWGETMULTISEL )
{
   auto hwnd = hmg_par_HWND(1);
   int n = SendMessage(hwnd, LVM_GETSELECTEDCOUNT, 0, 0);
   hb_reta(n);

   int i = -1;
   int j = 0;

   while( true ) {
      i = ListView_GetNextItem(hwnd, i, LVNI_ALL | LVNI_SELECTED);
      if( i == -1 ) {
         break;
      } else {
         j++;
      }
      HB_STORNI(i + 1, -1, j);
   }
}

/*
LISTVIEWSETMULTISEL() -->
*/
HB_FUNC( LISTVIEWSETMULTISEL )
{
   // CLEAR CURRENT SELECTIONS

   int i = -1;
   auto hwnd = hmg_par_HWND(1);

   while( true ) {
      i = ListView_GetNextItem(hwnd, i, LVNI_ALL | LVNI_SELECTED);
      if( i == -1 ) {
         break;
      } else {
         ListView_SetItemState(hwnd, i, 0, LVIS_FOCUSED | LVIS_SELECTED);
      }
   }

   // SET NEW SELECTIONS

   auto wArray = hb_param(2, Harbour::Item::ARRAY);
   int l = static_cast<int>(hb_parinfa(2, 0)) - 1;
   for( i = 0; i <= l; i++ ) {
      ListView_SetItemState(hwnd, hb_arrayGetNI(wArray, i + 1) - 1, LVIS_FOCUSED | LVIS_SELECTED, LVIS_FOCUSED | LVIS_SELECTED);
   }
}

/*
LISTVIEWSETITEM() -->
*/
HB_FUNC( LISTVIEWSETITEM )
{
#ifndef UNICODE
   LPSTR lpText;
#else
   LPWSTR lpText;
#endif
   int  l = static_cast<int>(hb_parinfa(2, 0)) - 1;
   auto hArray = hb_param(2, Harbour::Item::ARRAY);
   char * caption;
   auto h = hmg_par_HWND(1);
   int  c = hb_parni(3) - 1;

   for( int s = 0; s <= l; s = s + 1 ) {
      caption = const_cast<char*>(hb_arrayGetCPtr(hArray, s + 1));
#ifndef UNICODE
      lpText = caption;
#else
      lpText = AnsiToWide(caption);
#endif
      ListView_SetItemText(h, c, s, lpText);

#ifdef UNICODE
      hb_xfree(lpText);
#endif
   }
}

static TCHAR * GetLVItemText(HWND hListView, int i, int iSubItem_)
{
#ifndef UNICODE
   auto lpText = reinterpret_cast<LPSTR>(hb_xgrab(1));
   lpText[0] = '\0'; // '\0';
#else
   LPWSTR lpText = '\0';
#endif

   LV_ITEM lvi;
   lvi.iSubItem = iSubItem_;

   int nLen = 64;
   int nRes;

   do {
      nLen          *= 2;
      lpText         = static_cast<TCHAR*>(hb_xrealloc(lpText, sizeof(TCHAR) * nLen));
      lvi.cchTextMax = nLen;
      lvi.pszText    = lpText;
      nRes           = SendMessage(hListView, LVM_GETITEMTEXT, i, reinterpret_cast<LPARAM>(&lvi));
   } while( nRes >= nLen - 1 );

   return static_cast<TCHAR*>(lpText);
}

/*
LISTVIEWGETITEM() -->
*/
HB_FUNC( LISTVIEWGETITEM )
{
#ifdef UNICODE
   LPSTR pStr;
#endif

   auto l = hb_parni(3);
   hb_reta(l);
   auto h = hmg_par_HWND(1);
   int c = hb_parni(2) - 1;
   TCHAR * pszRet;

   for( int s = 0; s <= l - 1; s++ ) {
      pszRet = GetLVItemText(h, c, s);
#ifndef UNICODE
      HB_STORC(pszRet, -1, s + 1);
#else
      pStr = WideToAnsi(pszRet);
      HB_STORC(pStr, -1, s + 1);
      hb_xfree(pStr);
#endif
      hb_xfree(pszRet);
   }
}

/*
LISTVIEWGETITEMROW() -->
*/
HB_FUNC( LISTVIEWGETITEMROW )
{
   POINT point;
   ListView_GetItemPosition(hmg_par_HWND(1), hb_parni(2), &point);
   hb_retnl(point.y);
}

/*
LISTVIEWGETITEMCOUNT() -->
*/
HB_FUNC( LISTVIEWGETITEMCOUNT )
{
   hb_retnl(ListView_GetItemCount(hmg_par_HWND(1)));
}

/*
SETGRIDCOLUMNJUSTIFY() -->
*/
HB_FUNC( SETGRIDCOLUMNJUSTIFY )
{
   LV_COLUMN COL;
   COL.mask = LVCF_FMT;
   COL.fmt  = hb_parni(3);
   ListView_SetColumn(hmg_par_HWND(1), hb_parni(2) - 1, &COL);
}

/*
SETGRIDCOLUMNHEADER() -->
*/
HB_FUNC( SETGRIDCOLUMNHEADER )
{
#ifndef UNICODE
   LPSTR lpText = const_cast<char*>(hb_parc(3));
#else
   LPWSTR lpText = AnsiToWide(static_cast<char*>(hb_parc(3)));
#endif

   LV_COLUMN COL;
   COL.mask    = LVCF_FMT | LVCF_TEXT;
   COL.pszText = lpText;
   COL.fmt     = hb_parni(4);
   ListView_SetColumn(hmg_par_HWND(1), hb_parni(2) - 1, &COL);

#ifdef UNICODE
   hb_xfree(lpText);
#endif
}

/*
SETGRIDCOLUMNHEADERIMAGE() -->
*/
HB_FUNC( SETGRIDCOLUMNHEADERIMAGE )
{
   int fmt = LVCFMT_IMAGE | LVCFMT_COL_HAS_IMAGES;

   if( hb_parl(4) ) {
      fmt = fmt | LVCFMT_BITMAP_ON_RIGHT | LVCFMT_RIGHT;
   } else {
      fmt = fmt | LVCFMT_LEFT;
   }

   LV_COLUMN COL;
   COL.mask   = LVCF_FMT | LVCF_IMAGE;
   COL.fmt    = fmt;
   COL.iImage = hb_parni(3) - 1;

   ListView_SetColumn(hmg_par_HWND(1), hb_parni(2) - 1, &COL);
}

/*
LISTVIEWGETCOUNTPERPAGE() -->
*/
HB_FUNC( LISTVIEWGETCOUNTPERPAGE )
{
   hb_retnl(ListView_GetCountPerPage(hmg_par_HWND(1)));
}

/*
LISTVIEW_ENSUREVISIBLE() -->
*/
HB_FUNC( LISTVIEW_ENSUREVISIBLE )
{
   ListView_EnsureVisible(hmg_par_HWND(1), hb_parni(2) - 1, 1);
}

/*
SETIMAGELISTVIEWITEMS() -->
*/
HB_FUNC( SETIMAGELISTVIEWITEMS )
{
   LV_ITEM LI{};
   LI.mask      = LVIF_IMAGE;
   //LI.state     = 0;
   //LI.stateMask = 0;
   LI.iImage    = hb_parni(3);
   //LI.iSubItem  = 0;
   LI.iItem     = hb_parni(2) - 1;
   ListView_SetItem(hmg_par_HWND(1), &LI);
}

/*
GETIMAGELISTVIEWITEMS() -->
*/
HB_FUNC( GETIMAGELISTVIEWITEMS )
{
   LV_ITEM LI{};
   LI.mask      = LVIF_IMAGE;
   //LI.state     = 0;
   //LI.stateMask = 0;
   //LI.iSubItem  = 0;
   LI.iItem     = hb_parni(2) - 1;
   ListView_GetItem(hmg_par_HWND(1), &LI);
   hb_retni(LI.iImage);
}

/*
LISTVIEW_GETTOPINDEX() -->
*/
HB_FUNC( LISTVIEW_GETTOPINDEX )
{
   hb_retnl(ListView_GetTopIndex(hmg_par_HWND(1)));
}

/*
LISTVIEW_REDRAWITEMS() -->
*/
HB_FUNC( LISTVIEW_REDRAWITEMS )
{
   hb_retnl(ListView_RedrawItems(hmg_par_HWND(1), hb_parni(2), hb_parni(3)));
}

/*
LISTVIEW_HITTEST() -->
*/
HB_FUNC( LISTVIEW_HITTEST )
{
   POINT point;
   point.y = hb_parni(2);
   point.x = hb_parni(3);

   LVHITTESTINFO lvhti;
   lvhti.pt = point;

   if( hb_parni(4) ) { // checkbox area.
      ListView_HitTest(hmg_par_HWND(1), &lvhti);
      if( lvhti.flags & LVHT_ONITEMSTATEICON ) {
         hb_retl(true);
      } else {
         hb_retl(false);
      }
   } else { // item area.
      ListView_SubItemHitTest(hmg_par_HWND(1), &lvhti);
      if( lvhti.flags & LVHT_ONITEM ) {
         hb_reta(2);
         HB_STORNI(lvhti.iItem + 1, -1, 1);
         HB_STORNI(lvhti.iSubItem + 1, -1, 2);
      } else {
         hb_reta(2);
         HB_STORNI(0, -1, 1);
         HB_STORNI(0, -1, 2);
      }
   }
}

/*
LISTVIEW_GETSUBITEMRECT() -->
*/
HB_FUNC( LISTVIEW_GETSUBITEMRECT )
{
   auto pRect = static_cast<RECT*>(hb_xgrab(sizeof(RECT)));

   ListView_GetSubItemRect(hmg_par_HWND(1), hb_parni(2), hb_parni(3), LVIR_BOUNDS, pRect);

   hb_reta(4);
   HB_STORNI(pRect->top, -1, 1);
   HB_STORNI(pRect->left, -1, 2);
   HB_STORNI(pRect->right - pRect->left, -1, 3);
   HB_STORNI(pRect->bottom - pRect->top, -1, 4);

   hb_xfree(static_cast<void*>(pRect));
}

/*
LISTVIEW_GETITEMRECT() -->
*/
HB_FUNC( LISTVIEW_GETITEMRECT )
{
   auto pRect = static_cast<RECT*>(hb_xgrab(sizeof(RECT)));

   ListView_GetItemRect(hmg_par_HWND(1), hb_parni(2), pRect, LVIR_LABEL);

   hb_reta(4);
   HB_STORNI(pRect->top, -1, 1);
   HB_STORNI(pRect->left, -1, 2);
   HB_STORNI(pRect->right - pRect->left, -1, 3);
   HB_STORNI(pRect->bottom - pRect->top, -1, 4);

   hb_xfree(static_cast<void*>(pRect));
}

/*
LISTVIEW_UPDATE() -->
*/
HB_FUNC( LISTVIEW_UPDATE )
{
   ListView_Update(hmg_par_HWND(1), hb_parni(2) - 1);
}

/*
LISTVIEW_SCROLL() -->
*/
HB_FUNC( LISTVIEW_SCROLL )
{
   ListView_Scroll(hmg_par_HWND(1), hb_parni(2), hb_parni(3));
}

/*
LISTVIEW_SETBKCOLOR() -->
*/
HB_FUNC( LISTVIEW_SETBKCOLOR )
{
   ListView_SetBkColor(hmg_par_HWND(1), static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4))));
}

/*
LISTVIEW_SETTEXTBKCOLOR() -->
*/
HB_FUNC( LISTVIEW_SETTEXTBKCOLOR )
{
   ListView_SetTextBkColor(hmg_par_HWND(1), static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4))));
}

/*
LISTVIEW_SETTEXTCOLOR() -->
*/
HB_FUNC( LISTVIEW_SETTEXTCOLOR )
{
   ListView_SetTextColor(hmg_par_HWND(1), static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4))));
}

/*
LISTVIEW_GETTEXTCOLOR() -->
*/
HB_FUNC( LISTVIEW_GETTEXTCOLOR )
{
   hb_retnl( ListView_GetTextColor(hmg_par_HWND(1)) );
}

/*
LISTVIEW_GETBKCOLOR() -->
*/
HB_FUNC( LISTVIEW_GETBKCOLOR )
{
   hb_retnl( ListView_GetBkColor(hmg_par_HWND(1)) );
}

/*
LISTVIEW_GETHEADER() -->
*/
HB_FUNC( LISTVIEW_GETHEADER )
{
   auto hGrid = hmg_par_HWND(1);

   hmg_ret_HANDLE(ListView_GetHeader(hGrid));
}

/*
GETHEADERLISTVIEWITEM() -->
*/
HB_FUNC( GETHEADERLISTVIEWITEM )
{
   auto lpnmheader = reinterpret_cast<LPNMHEADER>(HB_PARNL(1));

   hb_retni(lpnmheader->iItem);
}

/*
GETHEADERLISTVIEWITEMCX() -->
*/
HB_FUNC( GETHEADERLISTVIEWITEMCX )
{
   auto lpnmheader = reinterpret_cast<LPNMHEADER>(HB_PARNL(1));

   if( lpnmheader->pitem->mask == HDI_WIDTH ) {
      hb_retni(lpnmheader->pitem->cxy);
   } else {
      hb_retni(-1);
   }
}

/*
LISTVIEW_ADDCOLUMN() -->
*/
HB_FUNC( LISTVIEW_ADDCOLUMN )
{
#ifndef UNICODE
   LPSTR lpText;
#else
   LPWSTR lpText;
#endif
   auto hwnd = hmg_par_HWND(1);
   int       iColumn = hb_parni(2) - 1;
   auto pValue = hb_itemNew(nullptr);

   hb_itemCopy(pValue, hb_param(4, Harbour::Item::STRING));

   LV_COLUMN COL;
   COL.mask = LVCF_WIDTH | LVCF_TEXT | LVCF_FMT | LVCF_SUBITEM;
   COL.cx   = hb_parni(3);
   #ifndef UNICODE
   lpText = const_cast<char*>(hb_itemGetCPtr(pValue));
   #else
   lpText = AnsiToWide(static_cast<char*>(hb_itemGetCPtr(pValue)));
   #endif
   COL.pszText  = lpText;
   COL.iSubItem = iColumn;
   COL.fmt      = hb_parni(5);

   ListView_InsertColumn(hwnd, iColumn, &COL);

#ifdef UNICODE
   hb_xfree(lpText);
#endif
   if( iColumn == 0 && COL.fmt != LVCFMT_LEFT ) {
      COL.iSubItem = 1;
      ListView_InsertColumn(hwnd, 1, &COL);
      ListView_DeleteColumn(hwnd, 0);
   }

   SendMessage(hwnd, LVM_DELETEALLITEMS, 0, 0);

   RedrawWindow(hwnd, nullptr, nullptr, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW);
}

/*
LISTVIEW_DELETECOLUMN() -->
*/
HB_FUNC( LISTVIEW_DELETECOLUMN )
{
   auto hwnd = hmg_par_HWND(1);
   ListView_DeleteColumn(hwnd, hb_parni(2) - 1);
   SendMessage(hwnd, LVM_DELETEALLITEMS, 0, 0);
   RedrawWindow(hwnd, nullptr, nullptr, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW);
}

/*
LISTVIEW_GETCOLUMNWIDTH() -->
*/
HB_FUNC( LISTVIEW_GETCOLUMNWIDTH )
{
   hb_retni(ListView_GetColumnWidth(hmg_par_HWND(1), hb_parni(2)));
}

/*
LISTVIEW_SETCOLUMNWIDTH() -->
*/
HB_FUNC( LISTVIEW_SETCOLUMNWIDTH )  // (JK) HMG Experimental Build 6
{
   hb_retl(ListView_SetColumnWidth(hmg_par_HWND(1), hb_parni(2), hb_parni(3)));
}

/*
LISTVIEW_GETCHECKSTATE() -->
*/
HB_FUNC( LISTVIEW_GETCHECKSTATE )
{
   auto hwndLV = hmg_par_HWND(1);

   if( _isValidCtrlClass(hwndLV, WC_LISTVIEW) ) {
      hb_retl(ListView_GetCheckState(hwndLV, hb_parni(2) - 1));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
   }
}

/*
LISTVIEW_SETCHECKSTATE() -->
*/
HB_FUNC( LISTVIEW_SETCHECKSTATE )
{
   auto hwndLV = hmg_par_HWND(1);

   if( _isValidCtrlClass(hwndLV, WC_LISTVIEW) ) {
      ListView_SetCheckState(hwndLV, hb_parni(2) - 1, hb_parl(3));
      hb_retl(true);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
   }
}

/*
LISTVIEW_GETCOLUMNCOUNT() -->
*/
HB_FUNC( LISTVIEW_GETCOLUMNCOUNT )  // Dr. Claudio Soto 2016/APR/07
{
   auto hwndLV = hmg_par_HWND(1);

   if( _isValidCtrlClass(hwndLV, WC_LISTVIEW) ) {
      hb_retni(Header_GetItemCount(ListView_GetHeader(hwndLV)));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
   }
}

/*
LISTVIEW_GETCOLUMNORDERARRAY() -->
*/
HB_FUNC( LISTVIEW_GETCOLUMNORDERARRAY )
{
   auto iCols = hb_parni(2);

   if( iCols ) {
      auto iArray = static_cast<int*>(hb_xgrab(iCols * sizeof(int)));
      auto pArray = hb_itemArrayNew(static_cast<HB_SIZE>(iCols));

      ListView_GetColumnOrderArray(hmg_par_HWND(1), iCols, static_cast<int*>(iArray));

      for( int i = 0; i < iCols; i++ ) {
         hb_arraySetNI(pArray, static_cast<HB_SIZE>(i) + 1, iArray[i] + 1);
      }

      hb_xfree(iArray);

      hb_itemReturnRelease(pArray);
   } else {
      hb_reta(0);
   }
}

/*
LISTVIEW_SETCOLUMNORDERARRAY() -->
*/
HB_FUNC( LISTVIEW_SETCOLUMNORDERARRAY )
{
   auto pOrder = hb_param(3, Harbour::Item::ARRAY);

   if( pOrder != nullptr ) {
      auto iColumn = hb_parni(2);

      if( iColumn ) {
         auto iArray = static_cast<int*>(hb_xgrab(iColumn * sizeof(int)));

         for( int i = 0; i < iColumn; i++ ) {
            iArray[i] = HB_PARNI(3, i + 1) - 1;
         }

         ListView_SetColumnOrderArray(hmg_par_HWND(1), iColumn, static_cast<int*>(iArray));

         hb_xfree(iArray);
      }
   }
}

//       ListView_ChangeExtendedStyle(hWnd, [nAddStyle], [nRemoveStyle])

/*
LISTVIEW_CHANGEEXTENDEDSTYLE() -->
*/
HB_FUNC( LISTVIEW_CHANGEEXTENDEDSTYLE )  // Dr. Claudio Soto
{
   auto hWnd = hmg_par_HWND(1);
   auto Add = hmg_par_DWORD(2);
   auto Remove = hmg_par_DWORD(3);

   DWORD OldStyle = ListView_GetExtendedListViewStyle(hWnd);
   DWORD NewStyle = (OldStyle | Add) & (~Remove);
   DWORD style = ListView_SetExtendedListViewStyle(hWnd, NewStyle);

   hb_retnl(style);
}

//       ListView_GetExtendedStyle(hWnd, [nExStyle])

/*
LISTVIEW_GETEXTENDEDSTYLE() -->
*/
HB_FUNC( LISTVIEW_GETEXTENDEDSTYLE )  // Dr. Claudio Soto
{
   auto ExStyle = hmg_par_DWORD(2);
   DWORD OldStyle = ListView_GetExtendedListViewStyle(hmg_par_HWND(1));

   if( HB_ISNUM(2) ) {
      hb_retl(static_cast<BOOL>((OldStyle & ExStyle) == ExStyle));
   } else {
      hb_retnl(OldStyle);
   }
}

#if ((defined(__BORLANDC__) && __BORLANDC__ < 1410))
#define HDF_SORTDOWN  0x0200
#define HDF_SORTUP    0x0400
#endif

//       ListView_SetSortHeader (nHWndLV, nColumn [, nType
//                                /*0==none, positive==UP arrow or negative==DOWN arrow*/]) -> nType (previous setting)

/*
LISTVIEW_SETSORTHEADER() -->
*/
HB_FUNC( LISTVIEW_SETSORTHEADER )
{
   auto hWndHD = reinterpret_cast<HWND>(SendMessage(hmg_par_HWND(1), LVM_GETHEADER, 0, 0));
   INT nItem  = hb_parni(2) - 1;
   INT nType;
   HDITEM hdItem;

   if( hb_parl(4) ) {
      hdItem.mask = HDI_FORMAT;

      SendMessage(hWndHD, HDM_GETITEM, nItem, reinterpret_cast<LPARAM>(&hdItem));

      if( hdItem.fmt & HDF_SORTUP ) {
         hb_retni(1);
      } else if( hdItem.fmt & HDF_SORTDOWN ) {
         hb_retni(-1);
      } else {
         hb_retni(0);
      }

      if( ( hb_pcount() > 2 ) && HB_ISNUM(3) ) {
         nType = hb_parni(3);

         if( nType == 0 ) {
            hdItem.fmt &= ~(HDF_SORTDOWN | HDF_SORTUP);
         } else if( nType > 0 ) {
            hdItem.fmt = (hdItem.fmt & ~HDF_SORTDOWN) | HDF_SORTUP;
         } else {
            hdItem.fmt = (hdItem.fmt & ~HDF_SORTUP) | HDF_SORTDOWN;
         }

         SendMessage(hWndHD, HDM_SETITEM, nItem, reinterpret_cast<LPARAM>(&hdItem));
      }
   } else {
      hdItem.mask = HDI_BITMAP | HDI_FORMAT;

      SendMessage(hWndHD, HDM_GETITEM, nItem, reinterpret_cast<LPARAM>(&hdItem));

      nType = hb_parni(3);

      if( nType == 0 ) {
         hdItem.mask = HDI_FORMAT;
         hdItem.fmt &= ~(HDF_BITMAP | HDF_BITMAP_ON_RIGHT);
      } else {
         if( nType > 0 ) {
            hdItem.hbm = static_cast<HBITMAP>(LoadImage(GetInstance(), "MINIGUI_GRID_ASC", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS));
         } else {
            hdItem.hbm = static_cast<HBITMAP>(LoadImage(GetInstance(), "MINIGUI_GRID_DSC", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS));
         }

         hdItem.fmt |= HDF_BITMAP;
         if( hdItem.fmt & HDF_RIGHT ) {
            hdItem.fmt &= ~HDF_BITMAP_ON_RIGHT;
         } else {
            hdItem.fmt |= HDF_BITMAP_ON_RIGHT;
         }
      }

      SendMessage(hWndHD, HDM_SETITEM, nItem, reinterpret_cast<LPARAM>(&hdItem));
   }
}

#define MAX_GROUP_BUFFER  2048

//        ListView_GroupItemSetID(hWnd, nRow, nGroupID)

/*
LISTVIEW_GROUPITEMSETID() -->
*/
HB_FUNC( LISTVIEW_GROUPITEMSETID )
{
   auto hWnd = hmg_par_HWND(1);
   auto nRow = hmg_par_INT(2);
   auto GroupID = hmg_par_INT(3);

#if ((defined(__BORLANDC__) && __BORLANDC__ < 1410))
   _LVITEM LVI{};
#else
   LVITEM LVI{};
#endif
   LVI.mask     = LVIF_GROUPID;
   LVI.iItem    = nRow;
   //LVI.iSubItem = 0;
   LVI.iGroupId = GroupID;

   hb_retl(static_cast<BOOL>(ListView_SetItem(hWnd, &LVI)));
}

//        ListView_GroupItemGetID(hWnd, nRow)

/*
LISTVIEW_GROUPITEMGETID() -->
*/
HB_FUNC( LISTVIEW_GROUPITEMGETID )
{
   auto hWnd = hmg_par_HWND(1);
   auto nRow = hmg_par_INT(2);

#if ((defined(__BORLANDC__) && __BORLANDC__ < 1410))
   _LVITEM LVI{};
#else
   LVITEM LVI{};
#endif
   LVI.mask     = LVIF_GROUPID;
   LVI.iItem    = nRow;
   //LVI.iSubItem = 0;
   ListView_GetItem(hWnd, &LVI);

   hb_retni(LVI.iGroupId);
}

//        ListView_IsGroupViewEnabled(hWnd)

/*
LISTVIEW_ISGROUPVIEWENABLED() -->
*/
HB_FUNC( LISTVIEW_ISGROUPVIEWENABLED )
{
   hb_retl(static_cast<BOOL>(ListView_IsGroupViewEnabled(hmg_par_HWND(1))));
}

//        ListView_EnableGroupView(hWnd, lEnable)

/*
LISTVIEW_ENABLEGROUPVIEW() -->
*/
HB_FUNC( LISTVIEW_ENABLEGROUPVIEW )
{
   ListView_EnableGroupView(hmg_par_HWND(1), hmg_par_BOOL(2));
}

//        ListView_GroupDeleteAll(hWnd)

/*
LISTVIEW_GROUPDELETEALL() -->
*/
HB_FUNC( LISTVIEW_GROUPDELETEALL )
{
   ListView_RemoveAllGroups(hmg_par_HWND(1));
}

//        ListView_GroupDelete(hWnd, nGroupID)

/*
LISTVIEW_GROUPDELETE() -->
*/
HB_FUNC( LISTVIEW_GROUPDELETE )
{
   hb_retni(ListView_RemoveGroup(hmg_par_HWND(1), hmg_par_INT(2)));
}

//        ListView_GroupAdd(hWnd, nGroupID, [nIndex])

/*
LISTVIEW_GROUPADD() -->
*/
HB_FUNC( LISTVIEW_GROUPADD )
{
   LVGROUP LVG;
   LVG.cbSize    = sizeof(LVGROUP);
   LVG.stateMask = LVM_SETGROUPINFO;
   LVG.mask      = LVGF_GROUPID | LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.iGroupId  = hmg_par_INT(2);
   LVG.pszHeader = nullptr; //L""; // TODO: check
   LVG.pszFooter = nullptr; //L""; // TODO: check
   LVG.uAlign    = LVGA_HEADER_LEFT | LVGA_FOOTER_LEFT;
   LVG.state     = LVGS_NORMAL;
   hb_retni(ListView_InsertGroup(hmg_par_HWND(1), HB_ISNUM(3) ? hb_parni(3) : -1, &LVG));
}

//        ListView_GroupSetInfo(hWnd, nGroupID, cHeader, nAlignHeader, cFooter, nAlingFooter, nState)

/*
LISTVIEW_GROUPSETINFO() -->
*/
HB_FUNC( LISTVIEW_GROUPSETINFO )
{
   auto hWnd = hmg_par_HWND(1);
   auto GroupID = hmg_par_INT(2);
   auto cHeader = static_cast<HB_WCHAR*>((hb_parclen(3) == 0) ? nullptr : hb_mbtowc(hb_parc(3)));
   auto nAlignHeader = hmg_par_UINT(4);
   HB_WCHAR * cFooter      = (hb_parclen(5) == 0) ? nullptr : hb_mbtowc( hb_parc(5));
   auto nAlignFooter = hmg_par_UINT(6);
   auto nState = hmg_par_UINT(7);

   HB_WCHAR cHeaderBuffer[MAX_GROUP_BUFFER];
   HB_WCHAR cFooterBuffer[MAX_GROUP_BUFFER];

   LVGROUP LVG;
   LVG.cbSize    = sizeof(LVGROUP);
   LVG.stateMask = LVM_GETGROUPINFO;
   LVG.mask      = LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.pszHeader = cHeaderBuffer;
   LVG.cchHeader = sizeof(cHeaderBuffer) / sizeof(WCHAR);
   LVG.pszFooter = cFooterBuffer;
   LVG.cchFooter = sizeof(cFooterBuffer) / sizeof(WCHAR);

   if( ListView_GetGroupInfo(hWnd, GroupID, &LVG) != -1 ) {
      UINT nAlign = 0;
      LVG.stateMask = LVM_SETGROUPINFO;
      LVG.pszHeader = (cHeader != nullptr) ? cHeader : cHeaderBuffer;
      LVG.pszFooter = (cFooter != nullptr) ? cFooter : cFooterBuffer;
      nAlign        = nAlign | ((nAlignHeader != 0) ?  nAlignHeader       : (LVG.uAlign & 0x07));
      nAlign        = nAlign | ((nAlignFooter != 0) ? (nAlignFooter << 3) : (LVG.uAlign & 0x38));
      LVG.uAlign    = nAlign;
      LVG.state     = ((nState != 0) ? (nState >> 1) : LVG.state);
      hb_retni(ListView_SetGroupInfo(hWnd, GroupID, &LVG));
   } else {
      hb_retni(-1);
   }
}

//        ListView_GroupGetInfo(hWnd, nGroupID, @cHeader, @nAlignHeader, @cFooter, @nAlingFooter, @nState)

/*
LISTVIEW_GROUPGETINFO() -->
*/
HB_FUNC( LISTVIEW_GROUPGETINFO )
{
   auto hWnd = hmg_par_HWND(1);
   auto GroupID = hmg_par_INT(2);

   INT      nRet;
   HB_WCHAR cHeaderBuffer[MAX_GROUP_BUFFER];
   HB_WCHAR cFooterBuffer[MAX_GROUP_BUFFER];

   LVGROUP LVG;
   LVG.cbSize    = sizeof(LVGROUP);
   LVG.stateMask = LVM_GETGROUPINFO;
   LVG.mask      = LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.pszHeader = cHeaderBuffer;
   LVG.cchHeader = sizeof(cHeaderBuffer) / sizeof(WCHAR);
   LVG.pszFooter = cFooterBuffer;
   LVG.cchFooter = sizeof(cFooterBuffer) / sizeof(WCHAR);

   if( (nRet = ListView_GetGroupInfo(hWnd, GroupID, &LVG)) != -1 ) {
      HB_STORC(hb_wctomb(cHeaderBuffer), 3);
      hb_storni((LVG.uAlign & 0x07), 4);
      HB_STORC(hb_wctomb(cFooterBuffer), 5);
      hb_storni(((LVG.uAlign & 0x38) >> 3), 6);
      hb_storni(((LVG.state != 0) ? (LVG.state << 1) : 1), 7);
   }

   hb_retni(nRet);
}

//        ListView_HasGroup(hWnd, nGroupID)

/*
LISTVIEW_HASGROUP() -->
*/
HB_FUNC( LISTVIEW_HASGROUP )
{
   hb_retl(static_cast<BOOL>(ListView_HasGroup(hmg_par_HWND(1), hmg_par_INT(2))));
}
