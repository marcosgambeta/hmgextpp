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

/* undocumented Windows API */
int WINAPI MessageBoxTimeout(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds);

extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);

// JK HMG 1.2 Experimental Build 16g
// MessageBoxIndirect([hWnd], [cText], [cCaption], [nStyle], [xIcon], [hInst], [nHelpId], [nProc], [nLang])
// Contributed by Andy Wos <andywos@unwired.com.au>

HB_FUNC( MESSAGEBOXINDIRECT )
{
   MSGBOXPARAMS mbp;

   mbp.cbSize    = sizeof(MSGBOXPARAMS);
   mbp.hwndOwner = HB_ISNUM(1) ? hmg_par_HWND(1) : GetActiveWindow();
   mbp.hInstance = HB_ISNUM(6) ? hmg_par_HINSTANCE(6) : GetInstance();
#ifndef UNICODE
   mbp.lpszText    = HB_ISCHAR(2) ? hb_parc(2) : (HB_ISNUM(2) ? MAKEINTRESOURCE(hb_parni(2)) : nullptr);
   mbp.lpszCaption = HB_ISCHAR(3) ? hb_parc(3) : (HB_ISNUM(3) ? MAKEINTRESOURCE(hb_parni(3)) : "");
   mbp.lpszIcon    = HB_ISCHAR(5) ? hb_parc(5) : (HB_ISNUM(5) ? MAKEINTRESOURCE(hb_parni(5)) : nullptr);
#else
   mbp.lpszText    = static_cast<LPCWSTR>(HB_ISCHAR(2) ? hb_osStrU16Encode(hb_parc(2)) : (HB_ISNUM(2) ? MAKEINTRESOURCE(hb_parni(2)) : nullptr));
   mbp.lpszCaption = static_cast<LPCWSTR>(HB_ISCHAR(3) ? hb_osStrU16Encode(hb_parc(3)) : (HB_ISNUM(3) ? MAKEINTRESOURCE(hb_parni(3)) : TEXT("")));
   mbp.lpszIcon    = static_cast<LPCWSTR>(HB_ISCHAR(5) ? hb_osStrU16Encode(hb_parc(5)) : (HB_ISNUM(5) ? MAKEINTRESOURCE(hb_parni(5)) : nullptr));
#endif
   mbp.dwStyle            = static_cast<DWORD>(hb_parni(4));
   mbp.dwContextHelpId    = HB_ISNUM(7) ? static_cast<DWORD>(hb_parni(7)) : 0;
   mbp.lpfnMsgBoxCallback = nullptr; /* Modified by P.Ch. 16.10. */
   mbp.dwLanguageId       = HB_ISNUM(9) ? static_cast<DWORD>(hb_parni(9)) : MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL);

   hb_retni(MessageBoxIndirect(&mbp));
}

// MessageBoxTimeout (Text, Caption, nTypeButton, nMilliseconds) ---> Return iRetButton
HB_FUNC( MESSAGEBOXTIMEOUT )
{
   HWND hWnd = GetActiveWindow();

#ifndef UNICODE
   const char * lpText    = hb_parc(1);
   const char * lpCaption = hb_parc(2);
#else
   TCHAR * lpText    = hb_osStrU16Encode(hb_parc(1));
   TCHAR * lpCaption = hb_osStrU16Encode(hb_parc(2));
#endif
   auto uType = static_cast<UINT>( hb_parnldef(3, MB_OK));
   WORD  wLanguageId    = MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL);
   DWORD dwMilliseconds = HB_ISNUM(4) ? hmg_par_DWORD(4) : static_cast<DWORD>(0xFFFFFFFF);

   hb_retni( MessageBoxTimeout(hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds) );
}

int WINAPI MessageBoxTimeout(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds)
{
   using PMessageBoxTimeout = int (WINAPI *)(HWND, LPCTSTR, LPCTSTR, UINT, WORD, DWORD);
   static PMessageBoxTimeout pMessageBoxTimeout = nullptr;

   if( pMessageBoxTimeout == nullptr ) {
      HMODULE hLib = LoadLibrary(TEXT("User32.dll"));

   #ifdef UNICODE
      pMessageBoxTimeout = static_cast<PMessageBoxTimeout>(wapi_GetProcAddress(hLib, "MessageBoxTimeoutW"));
   #else
      pMessageBoxTimeout = reinterpret_cast<PMessageBoxTimeout>(wapi_GetProcAddress(hLib, "MessageBoxTimeoutA"));
   #endif
   }

   return pMessageBoxTimeout == nullptr ? 0 : pMessageBoxTimeout(hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds);
}
