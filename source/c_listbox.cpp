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

#include "mgdefs.hpp"
#include <commctrl.h>
#include <windowsx.h>
#include <hbwinuni.hpp>

#ifndef WC_LISTBOX
#define WC_LISTBOX  "ListBox"
#endif

#define TOTAL_TABS  10

#ifdef UNICODE
LPSTR  WideToAnsi(LPWSTR);
#endif

HB_FUNC( INITLISTBOX )
{
   HWND hwnd;
   HWND hbutton;
   int style = WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT;

   hwnd = hmg_par_HWND(1);

   if( !hb_parl(9) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(10) ) {
      style |= WS_TABSTOP;
   }

   if( hb_parl(11) ) {
      style |= LBS_SORT;
   }

   if( hb_parl(13) ) {
      style |= LBS_USETABSTOPS;
   }

   if( hb_parl(14) ) {
      style |= LBS_MULTICOLUMN | WS_HSCROLL;
   }

   hbutton = CreateWindowEx
             (
      WS_EX_CLIENTEDGE,
      WC_LISTBOX,
      TEXT(""),
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(6),
      hwnd,
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr
             );

   if( hb_parl(12) ) {
      MakeDragList(hbutton);
   }

   if( hb_parl(14) ) {
      SendMessage(hbutton, LB_SETCOLUMNWIDTH, hb_parni(5) - 20, 0);
   }

   hmg_ret_HWND(hbutton);
}

HB_FUNC( LISTBOXADDSTRING )
{
   void * str;
   LPCTSTR lpString = HB_PARSTR(2, &str, nullptr);
   SendMessage(hmg_par_HWND(1), LB_ADDSTRING, 0, reinterpret_cast<LPARAM>(lpString));
   hb_strfree(str);
}

HB_FUNC( LISTBOXINSERTSTRING )
{
   void * str;
   LPCTSTR lpString = HB_PARSTR(2, &str, nullptr);
   SendMessage(hmg_par_HWND(1), LB_INSERTSTRING, hmg_par_WPARAM(3) - 1, reinterpret_cast<LPARAM>(lpString));
   hb_strfree(str);
}

/* Modified by P.Ch. 16.10. */
HB_FUNC( LISTBOXGETSTRING )
{
#ifdef UNICODE
   LPSTR lpString;
#endif
   int     iLen = SendMessage(hmg_par_HWND(1), LB_GETTEXTLEN, hmg_par_WPARAM(2) - 1, 0);
   TCHAR * cString;

   if( iLen > 0 && ( cString = ( TCHAR * ) hb_xgrab((iLen + 1) * sizeof(TCHAR)) ) != nullptr ) {
      SendMessage(hmg_par_HWND(1), LB_GETTEXT, hmg_par_WPARAM(2) - 1, reinterpret_cast<LPARAM>(cString));
   #ifdef UNICODE
      lpString = WideToAnsi(cString);
      hb_retc( lpString );
      hb_xfree(lpString);
   #else
      hb_retclen_buffer(cString, iLen);
   #endif
   } else {
      hb_retc_null();
   }
}

HB_FUNC( INITMULTILISTBOX )
{
   HWND hwnd;
   HWND hbutton;
   int style = LBS_EXTENDEDSEL | WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_MULTIPLESEL | LBS_NOINTEGRALHEIGHT;

   hwnd = hmg_par_HWND(1);

   if( !hb_parl(9) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(10) ) {
      style |= WS_TABSTOP;
   }

   if( hb_parl(11) ) {
      style |= LBS_SORT;
   }

   if( hb_parl(13) ) {
      style |= LBS_USETABSTOPS;
   }

   if( hb_parl(14) ) {
      style |= LBS_MULTICOLUMN;
   }

   hbutton = CreateWindowEx
             (
      WS_EX_CLIENTEDGE,
      WC_LISTBOX,
      TEXT(""),
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(6),
      hwnd,
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr
             );

   if( hb_parl(12) ) {
      MakeDragList(hbutton);
   }

   hmg_ret_HWND(hbutton);
}

HB_FUNC( LISTBOXGETMULTISEL )
{
   HWND hwnd = hmg_par_HWND(1);
   int  buffer[32768];
   int  n;

   n = SendMessage(hwnd, LB_GETSELCOUNT, 0, 0);

   SendMessage(hwnd, LB_GETSELITEMS, n, reinterpret_cast<LPARAM>(buffer));

   hb_reta(n);

   for( INT i = 0; i < n; i++ ) {
      HB_STORNI( buffer[i] + 1, -1, i + 1 );
   }
}

HB_FUNC( LISTBOXSETMULTISEL )
{
   PHB_ITEM wArray;

   HWND hwnd = hmg_par_HWND(1);

   int n, l;

   wArray = hb_param(2, Harbour::Item::ARRAY);

   l = ( int ) hb_parinfa(2, 0) - 1;

   n = SendMessage(hwnd, LB_GETCOUNT, 0, 0);

   // CLEAR CURRENT SELECTIONS
   for( int i = 0; i < n; i++ ) {
      SendMessage(hwnd, LB_SETSEL, 0, i);
   }

   // SET NEW SELECTIONS
   for( int i = 0; i <= l; i++ ) {
      SendMessage(hwnd, LB_SETSEL, 1, hb_arrayGetNI( wArray, i + 1 ) - 1);
   }
}

HB_FUNC( LISTBOXSETMULTITAB )
{
   PHB_ITEM wArray;
   int      nTabStops[TOTAL_TABS];
   int      l;
   DWORD    dwDlgBase = GetDialogBaseUnits();
   int      baseunitX = LOWORD(dwDlgBase);

   HWND hwnd = hmg_par_HWND(1);

   wArray = hb_param(2, Harbour::Item::ARRAY);

   l = ( int ) hb_parinfa(2, 0) - 1;

   for( int i = 0; i <= l; i++ ) {
      nTabStops[i] = MulDiv(hb_arrayGetNI(wArray, i + 1), 4, baseunitX);
   }

   SendMessage(hwnd, LB_SETTABSTOPS, l, reinterpret_cast<LPARAM>(&nTabStops));
}

HB_FUNC( _GETDDLMESSAGE )
{
   UINT g_dDLMessage;

   g_dDLMessage = RegisterWindowMessage(DRAGLISTMSGSTRING);

   hb_retnl( g_dDLMessage );
}

HB_FUNC( GET_DRAG_LIST_NOTIFICATION_CODE )
{
   LPARAM lParam        = HB_PARNL(1);
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   hb_retni( lpdli->uNotification );
}

HB_FUNC( GET_DRAG_LIST_DRAGITEM )
{
   int    nDragItem;
   LPARAM lParam        = HB_PARNL(1);
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   nDragItem = LBItemFromPt(lpdli->hWnd, lpdli->ptCursor, TRUE);

   hb_retni( nDragItem );
}

HB_FUNC( DRAG_LIST_DRAWINSERT )
{
   HWND   hwnd          = hmg_par_HWND(1);
   LPARAM lParam        = HB_PARNL(2);
   int    nItem         = hb_parni(3);
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;
   int nItemCount;

   nItemCount = SendMessage(lpdli->hWnd, LB_GETCOUNT, 0, 0);

   if( nItem < nItemCount ) {
      DrawInsert(hwnd, lpdli->hWnd, nItem);
   } else {
      DrawInsert(hwnd, lpdli->hWnd, -1);
   }
}

HB_FUNC( DRAG_LIST_MOVE_ITEMS )
{
   LPARAM lParam        = HB_PARNL(1);
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   char string[1024];
   int  result;

   result = ListBox_GetText(lpdli->hWnd, hb_parni(2), string);
   if( result != LB_ERR ) {
      result = ListBox_DeleteString(lpdli->hWnd, hb_parni(2));
   }
   if( result != LB_ERR ) {
      result = ListBox_InsertString(lpdli->hWnd, hb_parni(3), string);
   }
   if( result != LB_ERR ) {
      result = ListBox_SetCurSel( lpdli->hWnd, hb_parni(3) );
   }

   hb_retl(result != LB_ERR ? TRUE : FALSE);
}
