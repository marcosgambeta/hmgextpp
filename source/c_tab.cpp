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

extern BOOL Array2Point(PHB_ITEM aPoint, POINT * pt);

HIMAGELIST HMG_ImageListLoadFirst(const char * FileName, int cGrow, int Transparent, int * nWidth, int * nHeight);
void HMG_ImageListAdd(HIMAGELIST himl, char * FileName, int Transparent);

HB_FUNC( INITTABCONTROL )
{
   DWORD style = WS_CHILD | WS_VISIBLE | TCS_TOOLTIPS;

   if( hb_parl(11) ) {
      style |= TCS_BUTTONS;
   }

   if( hb_parl(12) ) {
      style |= TCS_FLATBUTTONS;
   }

   if( hb_parl(13) ) {
      style |= TCS_HOTTRACK;
   }

   if( hb_parl(14) ) {
      style |= TCS_VERTICAL;
   }

   if( hb_parl(15) ) {
      style |= TCS_BOTTOM;
   }

   if( hb_parl(16) ) {
      style |= TCS_MULTILINE;
   }

   if( hb_parl(17) ) {
      style |= TCS_OWNERDRAWFIXED;
   }

   if( !hb_parl(18) ) {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(
      0,
      WC_TABCONTROL,
      nullptr,
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(6),
      hmg_par_HWND(1),
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr);

   int l = hb_parinfa(7, 0) - 1;
   auto hArray = hb_param(7, Harbour::Item::ARRAY);

   TC_ITEM tie{};
   tie.mask   = TCIF_TEXT;
   tie.iImage = -1;

   for( int i = l; i >= 0; i = i - 1 ) {
      void * str;
      tie.pszText = const_cast<TCHAR*>(HB_ARRAYGETSTR(hArray, i + 1, &str, nullptr));
      TabCtrl_InsertItem(hbutton, 0, &tie);
      hb_strfree(str);
   }

   TabCtrl_SetCurSel(hbutton, hb_parni(8) - 1);
   hmg_ret_HWND(hbutton);
}

HB_FUNC( TABCTRL_SETCURSEL )
{
   TabCtrl_SetCurSel(hmg_par_HWND(1), hb_parni(2) - 1);
}

HB_FUNC( TABCTRL_GETCURSEL )
{
   hb_retni(TabCtrl_GetCurSel(hmg_par_HWND(1)) + 1);
}

HB_FUNC( TABCTRL_INSERTITEM )
{
   void * str;
   TC_ITEM tie{};
   tie.mask    = TCIF_TEXT;
   tie.iImage  = -1;
   tie.pszText = const_cast<TCHAR*>(HB_PARSTR(3, &str, nullptr));
   TabCtrl_InsertItem(hmg_par_HWND(1), hb_parni(2), &tie);
   hb_strfree(str);
}

HB_FUNC( TABCTRL_DELETEITEM )
{
   TabCtrl_DeleteItem(hmg_par_HWND(1), hb_parni(2));
}

HB_FUNC( SETTABCAPTION )
{
   void * str;
   TC_ITEM tie{};
   tie.mask = TCIF_TEXT;
   tie.pszText = const_cast<TCHAR*>(HB_PARSTR(3, &str, nullptr));
   TabCtrl_SetItem(hmg_par_HWND(1), hb_parni(2) - 1, &tie);
   hb_strfree(str);
}

HB_FUNC( ADDTABBITMAP )
{
   auto hbutton = hmg_par_HWND(1);
   HIMAGELIST himl = nullptr;
   int nCount = hb_parinfa(2, 0);

   if( nCount > 0 ) {
      int Transparent = hb_parl(3) ? 0 : 1;
      auto hArray = hb_param(2, Harbour::Item::ARRAY);

      TCHAR * FileName;

      for( int i = 1; i <= nCount; i++ ) {
         FileName = const_cast<TCHAR*>(hb_arrayGetCPtr(hArray, i));

         if( himl == nullptr ) {
            himl = HMG_ImageListLoadFirst(FileName, nCount, Transparent, nullptr, nullptr);
         } else {
            HMG_ImageListAdd(himl, FileName, Transparent);
         }
      }

      if( himl != nullptr ) {
         SendMessage(hbutton, TCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(himl));
      }

      TC_ITEM tie{};

      for( int i = 0; i < nCount; i++ ) {
         tie.mask = TCIF_IMAGE;
         tie.iImage = i;
         TabCtrl_SetItem(hbutton, i, &tie);
      }
   }

   RegisterResource(himl, "IMAGELIST");
   hmg_ret_HIMAGELIST(himl);
}

HB_FUNC( WINDOWFROMPOINT )
{
   POINT Point;
   Array2Point(hb_param(1, Harbour::Item::ARRAY), &Point);
   hmg_ret_HWND(WindowFromPoint(Point));
}

HB_FUNC( GETMESSAGEPOS )
{
   hb_retnl(GetMessagePos());
}
