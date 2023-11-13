/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * GETBOX Control Source Code
 * Copyright 2006 Jacek Kubica <kubica@wssk.wroc.pl>
 * http://www.wssk.wroc.pl/~kubica
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
#if defined(_MSC_VER)
#pragma warning (disable:4201)
#endif
#include "richedit.h"

#include <hbapiitm.hpp>
#include <hbstack.hpp>
#include <hbvm.hpp>

#ifndef WC_EDIT
#define WC_EDIT    "Edit"
#define WC_BUTTON  "Button"
#endif

#define GBB1       2
#define GBB2       3

LRESULT CALLBACK  OwnGetProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

HB_FUNC( INITGETBOX )
{
   HWND hedit;                   // Handle of the child window/control.
   int  style;                   // GETBOX window base style.
   int  ibtnStyle1, ibtnStyle2;  // BUTTON window base style.
   HWND himage, himage2;
   HWND hBtn1, hBtn2;
   BOOL fBtns, fBtn2;
   int  BtnWidth  = 0;
   int  BtnWidth2 = 0;

   fBtns = hb_parl(20);
   fBtn2 = hb_parl(22);

   // Get the handle of the parent window/form.

   auto hwnd = hmg_par_HWND(1); // Handle of the parent window/form.
   if( fBtns ) {
      BtnWidth  = (HB_ISNIL(19) ? 0 : hmg_par_int(19));
      BtnWidth  = (BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE));
      BtnWidth2 = (fBtn2 ? BtnWidth : 0);
   }
   style = WS_CHILD | ES_AUTOHSCROLL | WS_CLIPCHILDREN;

   if( hb_parl(12) ) { // if <lNumeric> is TRUE, then ES_NUMBER style is added.
      style |= ES_NUMBER;
   }

   // Set to a numeric TEXTBOX, so don't worry about other "textual" styles.

   else {
      if( hb_parl(10) ) { // if <lUpper> is TRUE, then ES_UPPERCASE style is added.
         style |= ES_UPPERCASE;
      }

      if( hb_parl(11) ) { // if <lLower> is TRUE, then ES_LOWERCASE style is added.
         style |= ES_LOWERCASE;
      }
   }

   if( hb_parl(13) ) { // if <lPassword> is TRUE, then ES_PASSWORD style is added.
      style |= ES_PASSWORD;
   }

   if( hb_parl(14) ) {
      style |= ES_RIGHT;
   }

   if( hb_parl(15) ) {
      style |= ES_READONLY;
   }

   if( !hb_parl(16) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(17) ) {
      style |= WS_TABSTOP;
   }

   // Creates the child control.

   hedit = CreateWindowEx(hb_parl(23) ? 0 : WS_EX_CLIENTEDGE,
                          WC_EDIT,
                          TEXT(""),
                          style,
                          hb_parni(3),
                          hb_parni(4),
                          hb_parni(5),
                          hb_parni(6),
                          hwnd,
                          static_cast<HMENU>(nullptr),
                          GetInstance(),
                          nullptr);

   SetProp(hedit, TEXT("OldWndProc"), reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnGetProc)));

   SendMessage(hedit, EM_LIMITTEXT, hmg_par_WPARAM(9), 0);

   if( hb_parc(18) != nullptr ) {
#ifndef UNICODE
      LPCSTR lpImageName = hb_parc(18);
#else
      LPWSTR lpImageName = AnsiToWide(static_cast<char*>(hb_parc(18)));
#endif
      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage == nullptr ) {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage != nullptr ) {
         BITMAP bm;
         GetObject(himage, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > hb_parni(6) - 5 ) {
            DeleteObject(himage);
            himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4,
               hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            if( himage == nullptr ) {
               himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, BtnWidth - 4,
                  hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }
#ifdef UNICODE
      hb_xfree(lpImageName);
#endif
   } else {
      himage = nullptr;
   }

   if( hb_parc(21) != nullptr ) {
#ifndef UNICODE
      LPCSTR lpImageName2 = hb_parc(21);
#else
      LPWSTR lpImageName2 = AnsiToWide(static_cast<char*>(hb_parc(21)));
#endif
      himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage2 == nullptr ) {
         himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage2 != nullptr ) {
         BITMAP bm;
         GetObject(himage2, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > hb_parni(6) - 5 ) {
            DeleteObject(himage2);
            himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4,
               hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

            if( himage2 == nullptr ) {
               himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4,
                  hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }
#ifdef UNICODE
      hb_xfree(lpImageName2);
#endif
   } else {
      himage2 = nullptr;
   }

   ibtnStyle1 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE;

   if( himage != nullptr ) {
      ibtnStyle1 |= BS_BITMAP;
   }

   ibtnStyle2 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE;

   if( himage2 != nullptr ) {
      ibtnStyle2 |= BS_BITMAP;
   }

   if( fBtns ) {
      hBtn1 = CreateWindow(WC_BUTTON,
                           TEXT("..."),
                           ibtnStyle1,
                           hb_parni(5) - BtnWidth - 3,
                           -1,
                           BtnWidth,
                           hb_parni(6) - 2,
                           hedit,
                           reinterpret_cast<HMENU>(GBB1),
                           GetInstance(),
                           nullptr);
   } else {
      hBtn1 = 0;
   }

   if( fBtn2 ) {
      hBtn2 = CreateWindow(WC_BUTTON,
                           TEXT("..."),
                           ibtnStyle2,
                           hb_parni(5) - BtnWidth - BtnWidth2 - 3,
                           -1,
                           BtnWidth,
                           hb_parni(6) - 2,
                           hedit,
                           reinterpret_cast<HMENU>(GBB2),
                           GetInstance(),
                           nullptr);
   } else {
      hBtn2 = 0;
   }

   if( himage != nullptr ) {
      SendMessage(hBtn1, BM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(himage));
   }

   if( himage2 != nullptr ) {
      SendMessage(hBtn2, BM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(himage2));
   }

   SendMessage(hedit, EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG(0, BtnWidth + BtnWidth2 + 2));

   hb_reta(5);
   hmg_storvhandle(hedit, -1, 1);
   hmg_storvhandle(hBtn1, -1, 2);
   hmg_storvhandle(hBtn2, -1, 3);
   hmg_storvhandle(himage, -1, 4);
   hmg_storvhandle(himage2, -1, 5);
}

HB_FUNC( CHECKBIT )
{
   hb_retl(hb_parnl(1) & (1 << (hb_parni(2) - 1)));
}

HB_FUNC( GETTEXTHEIGHT )               // returns the height of a string in pixels
{
   HDC   hDC        = hmg_par_HDC(1);
   HWND  hWnd       = nullptr;
   BOOL  bDestroyDC = FALSE;
   HFONT hFont      = hmg_par_HFONT(3);
   HFONT hOldFont   = nullptr;
   SIZE  sz;

#ifndef UNICODE
   LPCSTR lpString = hb_parc(2);
#else
   LPCWSTR lpString = AnsiToWide(static_cast<char*>(hb_parc(2)));
#endif

   if( !hDC ) {
      bDestroyDC = TRUE;
      hWnd       = GetActiveWindow();
      hDC        = GetDC(hWnd);
   }

   if( hFont ) {
      hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));
   }

   GetTextExtentPoint32(hDC, lpString, lstrlen(lpString), &sz);

   if( hFont ) {
      SelectObject(hDC, hOldFont);
   }

   if( bDestroyDC ) {
      ReleaseDC(hWnd, hDC);
   }

   hb_retni(sz.cy);

#ifdef UNICODE
   hb_xfree(static_cast<TCHAR*>(lpString));
#endif
}

LRESULT CALLBACK OwnGetProc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;
   WNDPROC OldWndProc;

   OldWndProc = reinterpret_cast<WNDPROC>(reinterpret_cast<LONG_PTR>(GetProp(hwnd, TEXT("OldWndProc"))));
   switch( Msg ) {
      case WM_NCDESTROY: {
         return CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
      }
      case WM_GETDLGCODE: {
         return DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS + DLGC_HASSETSEL;
      }
      case EM_DISPLAYBAND: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hwnd, Msg, wParam, lParam);
      }
      case WM_CHAR: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hwnd, Msg, wParam, lParam);
      }
      case EM_CANPASTE: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
      }
      case WM_PASTE: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hwnd, Msg, wParam, lParam);
      }
      case WM_CONTEXTMENU:
      case WM_KILLFOCUS:
      case WM_SETFOCUS: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
      }
      case WM_LBUTTONDBLCLK:
      case WM_KEYDOWN:
      case WM_KEYUP: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hwnd, Msg, wParam, lParam);
      }
      case WM_CUT: {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hwnd, Msg, wParam, lParam);
      }
      case WM_COMMAND: {
         if( lParam != 0 && HIWORD(wParam) == BN_CLICKED ) {
            if( pSymbol == nullptr ) {
               pSymbol = hb_dynsymSymbol(hb_dynsymGet("OGETEVENTS"));
            }

            if( pSymbol != nullptr ) {
               hb_vmPushSymbol(pSymbol);
               hb_vmPushNil();
               hmg_vmPushHandle(hwnd);
               hb_vmPushLong(Msg);
               hb_vmPushNumInt(wParam);
               hb_vmPushNumInt(lParam);
               hb_vmDo(4);
            }

            long int r = hb_parnl(-1);

            return r ? TRUE : CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
         }
      }   
   }

   return CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
}
