/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * ANIMATERES Control Source Code
 * Copyright 2011 Grigory Filatov <gfilatov@gmail.com>
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

// #include "minigui.ch"

// #ifdef _USERINIT_ /* TODO: */

// *------------------------------------------------------------------------------*
// * Low Level C Routines
// *------------------------------------------------------------------------------*

// #pragma BEGINDUMP

#include <mgdefs.h>
#include <mmsystem.h>
#include <commctrl.h>

#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
#endif

/*
INITANIMATERES() --> HWND
*/
HB_FUNC( INITANIMATERES )
{
#ifndef UNICODE
   LPCSTR lpszDllName = hb_parc(7);
#else
   LPWSTR lpszDllName = AnsiToWide(static_cast<char*>(hb_parc(7)));
#endif

   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC  = ICC_ANIMATE_CLASS;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD | WS_VISIBLE | ACS_TRANSPARENT | ACS_CENTER | ACS_AUTOPLAY;

   if( !hb_parl(9) )
   {
      style |= WS_VISIBLE;
   }

   HINSTANCE avi = LoadLibrary(lpszDllName);
   HWND AnimationCtrl = CreateWindowEx(0, ANIMATE_CLASS, nullptr, style, 
      hmg_par_int(3), hmg_par_int(4), hmg_par_int(5), hmg_par_int(6), 
      hmg_par_HWND(1), hmg_par_HMENU(2), avi, nullptr);
   Animate_OpenEx(AnimationCtrl, avi, MAKEINTRESOURCE(hb_parni(8)));
   HB_STORNL(reinterpret_cast<LONG_PTR>(avi), 2);
   hmg_ret_HWND(AnimationCtrl);
}

/*
UNLOADANIMATELIB(HINSTANCE) --> NIL
*/
HB_FUNC( UNLOADANIMATELIB )
{
   HINSTANCE hLib = hmg_par_HINSTANCE(1);

   if( hLib != nullptr )
   {
      FreeLibrary(hLib);
   }
}

// #pragma ENDDUMP

// #endif
