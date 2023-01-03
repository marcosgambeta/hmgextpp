/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * BTNTEXTBOX control source code
 * (C)2006-2011 Janusz Pora <januszpora@onet.eu>
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

#include "mgdefs.h"
#include <hbapiitm.h>
#include <hbstack.h>
#include <hbvm.h>
#include <hbwinuni.h>

#ifndef WC_EDIT
#define WC_EDIT    TEXT("Edit")
#define WC_BUTTON  TEXT("Button")
#endif

#define TBB1       2
#define TBB2       3

LRESULT CALLBACK OwnBtnTextProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

HINSTANCE GetInstance(void);
HINSTANCE GetResources(void);

/*
INITBTNTEXTBOX(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) --> array
*/
HB_FUNC( INITBTNTEXTBOX )
{
   HWND himage, himage2;
   BOOL fBtn2 = hb_parl(20);
   int  BtnWidth2;
   int  BtnWidth = HB_ISNIL(18) ? 0 : hb_parni(18);

   // Get the handle of the parent window/form.
   HWND hwnd = hmg_par_HWND(1);

   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   DWORD style = WS_CHILD | ES_AUTOHSCROLL | WS_CLIPCHILDREN; // TEXTBOX window base style

   if( hb_parl(12) )  // if <lNumeric> is TRUE, then ES_NUMBER style is added.
   {
      style |= ES_NUMBER;
   }
   else
   {
      if( hb_parl(10) ) // if <lUpper> is TRUE, then ES_UPPERCASE style is added.
      {
         style |= ES_UPPERCASE;
      }

      if( hb_parl(11) ) // if <lLower> is TRUE, then ES_LOWERCASE style is added.
      {
         style |= ES_LOWERCASE;
      }
   }

   if( hb_parl(13) )  // if <lPassword> is TRUE, then ES_PASSWORD style is added.
   {
      style |= ES_PASSWORD;
   }

   if( hb_parl(14) )
   {
      style |= ES_RIGHT;
   }

   if( !hb_parl(15) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(16) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(21) )
   {
      style |= ES_READONLY;
   }

   // Creates the child Frame control.
   HWND hedit = CreateWindowEx(WS_EX_CLIENTEDGE, WC_EDIT, TEXT(""), style,
      hmg_par_int(3), hmg_par_int(4), hmg_par_int(5), hmg_par_int(6),
      hwnd, nullptr, GetInstance(), nullptr);

   SetProp(hedit, TEXT("OldWndProc"), reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnBtnTextProc));

   SendMessage(hedit, EM_LIMITTEXT, hmg_par_WPARAM(9), 0);

   if( hb_parc(17) != nullptr )
   {
      void * ImageName;
      LPCTSTR lpImageName = HB_PARSTR(17, &ImageName, nullptr);

      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage == nullptr )
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage != nullptr )
      {
         BITMAP bm;
         GetObject(himage, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > hb_parni(6) - 5 )
         {
            DeleteObject(himage);
            himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4, hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            if( himage == nullptr )
            {
               himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, BtnWidth - 4, hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName);
   }
   else
   {
      himage = nullptr;
   }

   if( hb_parc(19) != nullptr )
   {
      void * ImageName2;
      LPCTSTR lpImageName2 = HB_PARSTR(19, &ImageName2, nullptr);

      himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage2 == nullptr )
      {
         himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage2 != nullptr )
      {
         BITMAP bm;
         GetObject(himage2, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > hb_parni(6) - 5 )
         {
            DeleteObject(himage2);
            himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, hb_parni(6) - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

            if( himage2 == nullptr )
            {
               himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, hb_parni(6) - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName2);
   }
   else
   {
      himage2 = nullptr;
   }

   DWORD ibtnStyle1 = BS_NOTIFY | WS_CHILD | WS_VISIBLE | (hb_parl(22) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON);

   if( himage != nullptr )
   {
      ibtnStyle1 |= BS_BITMAP;
   }

   DWORD ibtnStyle2 = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE; // BUTTON window base style.

   if( himage2 != nullptr )
   {
      ibtnStyle2 |= BS_BITMAP;
   }

   HWND hBtn1 = CreateWindowEx(0, WC_BUTTON, TEXT("..."), ibtnStyle1,
      hmg_par_int(5) - BtnWidth - 3, -1, BtnWidth, hmg_par_int(6) - 2,
      hedit, reinterpret_cast<HMENU>(TBB1), GetInstance(), nullptr);

   HWND hBtn2 = fBtn2 ? CreateWindowEx(0, WC_BUTTON, TEXT("..."), ibtnStyle2,
      hmg_par_int(5) - BtnWidth - BtnWidth2 - 3, -1, BtnWidth, hmg_par_int(6) - 2,
      hedit, reinterpret_cast<HMENU>(TBB2), GetInstance(), nullptr) : nullptr;

   if( himage != nullptr )
   {
      SendMessage(hBtn1, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));
   }

   if( himage2 != nullptr )
   {
      SendMessage(hBtn2, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage2));
   }

   hb_reta(5);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hedit), -1, 1);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn1), -1, 2);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn2), -1, 3);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 4);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage2), -1, 5);
}

/*
REDEFBTNTEXTBOX(p1, p2, p3, p4, p5, p6, p7) --> array
*/
HB_FUNC( REDEFBTNTEXTBOX )
{
   HWND himage, himage2;
   int  width, height, BtnWidth2;
   int  BtnWidth = HB_ISNIL(3) ? 0 : hb_parni(3);

   HWND hedit = hmg_par_HWND(1);
   BOOL fBtn2 = hb_parl(5);
   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth2 = fBtn2 ?  BtnWidth : 0;
   width     = hb_parni(6);
   height    = hb_parni(7);

   SetProp(hedit, TEXT("OldWndProc"), reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnBtnTextProc));

   if( hb_parc(2) != nullptr )
   {
      void * ImageName;
      LPCTSTR lpImageName = HB_PARSTR(2, &ImageName, nullptr);

      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage == nullptr )
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage != nullptr )
      {
         BITMAP bm;
         GetObject(himage, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth - 4 || bm.bmHeight > height - 5 )
         {
            DeleteObject(himage);
            himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, BtnWidth - 4, height - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            if( himage == nullptr )
            {
               himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, BtnWidth - 4, height - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName);
   }
   else
   {
      himage = nullptr;
   }

   if( hb_parc(4) != nullptr )
   {
      void * ImageName2;
      LPCTSTR lpImageName2 = HB_PARSTR(4, &ImageName2, nullptr);

      himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( himage2 == nullptr )
      {
         himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

      if( himage2 != nullptr )
      {
         BITMAP bm;
         GetObject(himage2, sizeof(BITMAP), &bm);
         if( bm.bmWidth > BtnWidth2 - 4 || bm.bmHeight > height - 5 )
         {
            DeleteObject(himage2);
            himage2 = static_cast<HWND>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, height - 6, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

            if( himage2 == nullptr )
            {
               himage2 = static_cast<HWND>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, BtnWidth2 - 4, height - 6, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
            }
         }
      }

      hb_strfree(ImageName2);
   }
   else
   {
      himage2 = nullptr;
   }

   HWND hBtn1 = CreateWindowEx(0, WC_BUTTON, TEXT("..."), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE | BS_BITMAP,
      width - BtnWidth - 4, -1, BtnWidth, height - 2,
      hedit, reinterpret_cast<HMENU>(TBB1), GetInstance(), nullptr);

   HWND hBtn2 = fBtn2 ? CreateWindowEx(0, WC_BUTTON, TEXT("..."), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE | BS_BITMAP,
      width - BtnWidth - BtnWidth2 - 4, -1, BtnWidth, height - 2,
      hedit, reinterpret_cast<HMENU>(TBB2), GetInstance(), nullptr) : nullptr;

   if( himage != nullptr )
   {
      SendMessage(hBtn1, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));
   }

   if( himage2 != nullptr )
   {
      SendMessage(hBtn2, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage2));
   }

   SendMessage(hedit, EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG(0, BtnWidth + BtnWidth2 + 2));

   hb_reta(5);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hedit), -1, 1);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn1), -1, 2);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(hBtn2), -1, 3);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 4);
   HB_STORVNL(reinterpret_cast<LONG_PTR>(himage2), -1, 5);
}

/*
SETTBBTNMARGIN(hedit, nBtnWidth, lBtns, lBtn2) --> NIL
*/
HB_FUNC( SETTBBTNMARGIN )
{
   int  BtnWidth = hb_parni(2);
   BOOL fBtns    = hb_parl(3);
   BOOL fBtn2    = hb_parl(4);
   int  BtnWidth2;

   BtnWidth  = BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1;
   BtnWidth  = fBtns ? BtnWidth : 0;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   SendMessage(hmg_par_HWND(1), EM_SETMARGINS, EC_LEFTMARGIN | EC_RIGHTMARGIN, MAKELONG(0, BtnWidth + BtnWidth2 + 2));
}

LRESULT CALLBACK OwnBtnTextProc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;
   long int r;

   WNDPROC OldWndProc = reinterpret_cast<WNDPROC>(GetProp(hwnd, TEXT("OldWndProc")));

   switch( Msg )
   {
      case WM_CONTEXTMENU:
      case WM_COMMAND:

         if( lParam != 0 && (HIWORD(wParam) == BN_CLICKED || Msg == WM_CONTEXTMENU) )
         {
            if( !pSymbol )
            {
               pSymbol = hb_dynsymSymbol(hb_dynsymGet("TBBTNEVENTS"));
            }

            if( pSymbol )
            {
               hb_vmPushSymbol(pSymbol);
               hb_vmPushNil();
               hb_vmPushNumInt(reinterpret_cast<LONG_PTR>(hwnd));
               hb_vmPushNumInt(lParam);
               hb_vmPushLong(Msg);
               hb_vmDo(3);
            }

            r = hb_parnl(-1);

            return r != 0 ? r : CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
         }
   }

   return CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
}
