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
 *
 * Parts of this code is contributed and used here under permission of his
 * author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "mgdefs.hpp"
#include <shellapi.h>
#include <hbwinuni.hpp>

/*
COPYICON(HICON) --> HICON
*/
HB_FUNC( COPYICON )
{
   HICON hIcon = CopyIcon(hmg_par_HICON(1));
   RegisterResource(hIcon, "ICON");
   hmg_ret_HICON(hIcon);
}

/*
DESTROYICON(HICON) --> .T.|.F.
*/
HB_FUNC( DESTROYICON )
{
   HICON hIcon = hmg_par_HICON(1);
   DelResource(hIcon);
   hb_retl(DestroyIcon(hIcon));
}

/*
DUPLICATEICON(HICON) --> HICON
*/
HB_FUNC( DUPLICATEICON )
{
   HICON hIcon = DuplicateIcon(nullptr, hmg_par_HICON(1));
   RegisterResource(hIcon, "ICON");
   hmg_ret_HICON(hIcon);
}

/*
LOADICON(HINSTANCE, nIcon|cIcon) --> HICON
*/
HB_FUNC( LOADICON )
{
   HINSTANCE hinstance = HB_ISNIL(1) ? nullptr : hmg_par_HINSTANCE(1);
   void * str;
   HICON hIcon = LoadIcon(hinstance, HB_ISCHAR(2) ? HB_PARSTR(2, &str, nullptr) : MAKEINTRESOURCE(hb_parni(2)));
   hb_strfree(str);
   RegisterResource(hIcon, "ICON");
   hmg_ret_HICON(hIcon);
}

/*
EXTRACTICON(cExeFileName, nIconIndex) --> HICON
*/
HB_FUNC( EXTRACTICON )
{
   void * str;
   HICON hIcon = ExtractIcon(GetInstance(), HB_PARSTR(1, &str, nullptr), hmg_par_UINT(2));
   hb_strfree(str);
   RegisterResource(hIcon, "ICON");
   hmg_ret_HICON(hIcon);
}

/*
EXTRACTICONEX(cFileName, nIconIndex) --> {HIconLarge, HIconSmall}
*/
HB_FUNC( EXTRACTICONEX )
{
   void * str;
   int nIconIndex = hb_parni(2);
   if( nIconIndex == -1 ) {
      hb_retni(ExtractIconEx(HB_PARSTR(1, &str, nullptr), -1, nullptr, nullptr, 0));
   } else {
      HICON hIconLarge, hIconSmall;
      UINT nIconCount = ExtractIconEx(HB_PARSTR(1, &str, nullptr), nIconIndex, &hIconLarge, &hIconSmall, 1);
      if( nIconCount > 0 ) {
         hb_reta(2);
         hmg_storvhandle(hIconLarge, -1, 1);
         hmg_storvhandle(hIconSmall, -1, 2);
      }
   }
   hb_strfree(str);
}

/*
LOADICONBYNAME(cResource|cFile, cxDesired, cyDesired, HINSTANCE) --> HICON
*/
HB_FUNC( LOADICONBYNAME )
{
   HICON hIcon = nullptr;

   if( hb_parclen(1) > 0 ) {
      void * str;
      LPCTSTR pszResOrFile = HB_PARSTR(1, &str, nullptr);
      int cxDesired = hb_parni(2);
      int cyDesired = hb_parni(3);
      HINSTANCE hInstance = HB_PARNL(4) ? hmg_par_HINSTANCE(4) : GetResources();
      hIcon = static_cast<HICON>(LoadImage(hInstance, pszResOrFile, IMAGE_ICON, cxDesired, cyDesired, LR_DEFAULTCOLOR));
      if( hIcon == nullptr ) {
         hIcon = static_cast<HICON>(LoadImage(0, pszResOrFile, IMAGE_ICON, cxDesired, cyDesired, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
      }
      if( hIcon != nullptr ) {
         RegisterResource(hIcon, "ICON");
      }
      hb_strfree(str);
   }

   hmg_ret_HICON(hIcon);
}

/*
DRAWICONEX(HWND, np2, np3, HICON, np5, np6, np7, lp8) --> .T.|.F.|NIL
*/
HB_FUNC( DRAWICONEX )
{
   auto hwnd = hmg_par_HWND(1);

   if( IsWindow(hwnd) ) {
      HICON hIcon = hmg_par_HICON(4);
      HDC hdc = GetDC(hwnd);
      HBRUSH hbrFlickerFreeDraw = CreateSolidBrush(hb_parni(7));
      hb_retl(DrawIconEx(hdc, hb_parni(2), hb_parni(3), hIcon, hb_parni(5), hb_parni(6), 0, hbrFlickerFreeDraw, DI_NORMAL));
      DeleteObject(hbrFlickerFreeDraw);
      if( hb_parldef(8, true) ) {
         DelResource(hIcon);
         DestroyIcon(hIcon);
      }
      ReleaseDC(hwnd, hdc);
   }
}
