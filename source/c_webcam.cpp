/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * WEBCAM Control Source Code
 * Copyright 2012 Grigory Filatov <gfilatov@gmail.com>
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

// #ifdef _USERINIT_

#include "mgdefs.hpp"
#include <vfw.h>

#if defined(__BORLANDC__)
#pragma warn -use /* unused var */
#pragma warn -eff /* no effect */
#endif

#ifdef UNICODE
   LPWSTR AnsiToWide(LPCSTR);
#endif

/*
CAP_CREATECAPTUREWINDOW(cWindowName, nStyle, nX, nY, nWidth, nHeight, nWndParent, nID) --> handle
*/
HB_FUNC( CAP_CREATECAPTUREWINDOW )
{
#ifndef UNICODE
   LPCSTR lpszWindowName = hb_parc(1);
#else
   LPWSTR lpszWindowName = AnsiToWide(( char * ) hb_parc(1));
#endif

   hmg_ret_HWND(capCreateCaptureWindow(lpszWindowName, hmg_par_DWORD(2), hmg_par_int(3), hmg_par_int(4), hmg_par_int(5), hmg_par_int(6), hmg_par_HWND(7), hmg_par_int(8)));
}

/*
CAP_DRIVERCONNECT(nWnd, nIndex) --> .T.|.F.
*/
HB_FUNC( CAP_DRIVERCONNECT )
{
   hb_retl(capDriverConnect(hmg_par_HWND(1), hmg_par_WPARAM(2)));
}

/*
CAP_DRIVERDISCONNECT(nWnd) --> .T.|.F.
*/
HB_FUNC( CAP_DRIVERDISCONNECT )
{
   hb_retl(capDriverDisconnect(hmg_par_HWND(1)));
}

/*
CAP_SETVIDEOFORMAT(nWnd, nWidth, nHeight) --> .T.|.F.
*/
HB_FUNC( CAP_SETVIDEOFORMAT )
{
   BITMAPINFO binf;
   HWND hCapWnd = hmg_par_HWND(1);
   capGetVideoFormat(hCapWnd, &binf, sizeof(BITMAPINFO));
   binf.bmiHeader.biWidth        = hb_parni(2);
   binf.bmiHeader.biHeight       = hb_parni(3);
   binf.bmiHeader.biPlanes       = 1;
   binf.bmiHeader.biBitCount     = 24;
   binf.bmiHeader.biCompression  = BI_RGB;
   binf.bmiHeader.biSizeImage    = 0;
   binf.bmiHeader.biClrUsed      = 0;
   binf.bmiHeader.biClrImportant = 0;
   hb_retl(capSetVideoFormat(hCapWnd, &binf, sizeof(BITMAPINFO)));
}

/*
CAP_PREVIEWRATE(nWnd, nRate) --> .T.|.F.
*/
HB_FUNC( CAP_PREVIEWRATE )
{
   hb_retl(capPreviewRate(hmg_par_HWND(1), hmg_par_WPARAM(2)));
}

/*
CAP_PREVIEWSCALE(nWnd, lPreviewScale) --> .T.|.F.
*/
HB_FUNC( CAP_PREVIEWSCALE )
{
   hb_retl(capPreviewScale(hmg_par_HWND(1), hmg_par_BOOL(2)));
}

/*
CAP_PREVIEW(nWnd, lPreviewMode) --> .T.|.F.
*/
HB_FUNC( CAP_PREVIEW )
{
   hb_retl(capPreview(hmg_par_HWND(1), hmg_par_BOOL(2)));
}

/*
CAP_EDITCOPY(nWnd) --> .T.|.F.
*/
HB_FUNC( CAP_EDITCOPY )
{
   hb_retl(capEditCopy(hmg_par_HWND(1)));
}

// #endif
