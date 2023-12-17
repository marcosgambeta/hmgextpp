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
 * TOOLBAREX and TOOLBUTTONEX controls source code
 * (C)2005 Janusz Pora <januszpora@onet.eu>
 */

#define _WIN32_IE     0x0501
#define _WIN32_WINNT  0x0502

#include "mgdefs.hpp"
#include <hbwinuni.hpp>

#if defined(_MSC_VER)
#pragma warning (disable:4996)
#endif
#include <commctrl.h>

#define NUM_TOOLBAR_BUTTONS  10

LRESULT APIENTRY ToolBarExFunc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

static LPTBBUTTON lpSaveButtons;
static int nResetCount;
static int buttonCount;
static int isInSizeMsg = 0;

/*
INITTOOLBAR(HWND, p2, HMENU, p4, p5, np6, np7, p8, p9, lp10, lp11, lp12, lp13, lp14, lp15, lp16) --> HWND
*/
HB_FUNC( INITTOOLBAR )
{
   DWORD style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;
   DWORD ExStyle = 0;
   int TbExStyle = TBSTYLE_EX_DRAWDDARROWS;

   if( hb_parl(14) ) {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   if( hb_parl(10) ) {
      style |= TBSTYLE_FLAT;
   }

   if( hb_parl(11) ) {
      style |= CCS_BOTTOM;
   }

   if( hb_parl(12) ) {
      style |= TBSTYLE_LIST;
   }

   if( hb_parl(13) ) {
      style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl(15) ) {
      style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl(16) ) {
      style |= CCS_ADJUSTABLE;
   }

   auto hwndTB = CreateWindowEx(ExStyle, TOOLBARCLASSNAME, nullptr, style, 0, 0, 0, 0, hmg_par_HWND(1), hmg_par_HMENU(3), GetInstance(), nullptr);

   if( hb_parni(6) && hb_parni(7) ) {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(6), hb_parni(7));
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(6), hb_parni(7)));
   }

   SendMessage(hwndTB, TB_SETEXTENDEDSTYLE, 0, TbExStyle);
   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HWND(hwndTB);
}

/*
INITTOOLBUTTON(HWND, cText, np3, p4, p5, np6, np7, cp8, p9, lp10, lp11, lp12, lp13, lp14, lp15, lp16) --> HWND
*/
HB_FUNC( INITTOOLBUTTON )
{
   auto hwndTB = hmg_par_HWND(1);
   HWND himage = nullptr;
   int Transparent = hb_parl(9) ? 0 : 1;

   if( hb_parclen(8) > 0 ) {
      DWORD tSize = SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      int px    = LOWORD(tSize);
      int py    = HIWORD(tSize);
      auto ix = 0;
      auto iy = 0;
      if( hb_parl(16) ) {
         ix = hb_parni(6) - px;
         iy = hb_parni(7) - py;
      }
      himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(8), hb_parl(16) ? ix : -1, hb_parl(16) ? iy : -1, hwndTB, 1, Transparent, -1, hb_parl(16) ? 1 : 0, false, 255));
   }

   TBBUTTON    tbb[NUM_TOOLBAR_BUTTONS];
   memset(tbb, 0, sizeof tbb);

   // Add the bitmap containing button images to the toolbar.

   auto nBtn = 0;
   TBADDBITMAP tbab;
   tbab.hInst = nullptr;
   tbab.nID   = reinterpret_cast<UINT_PTR>(himage);
   int nPoz       = SendMessage(hwndTB, TB_ADDBITMAP, 1, reinterpret_cast<LPARAM>(&tbab));

   // Add the strings

   if( hb_parclen(2) > 0 ) {
      void * str;
      LPCTSTR lpText = HB_PARSTR(2, &str, nullptr);
      int index = SendMessage(hwndTB, TB_ADDSTRING, 0, reinterpret_cast<LPARAM>(lpText));
      tbb[nBtn].iString = index;
      hb_strfree(str);
   }

   int style = TBSTYLE_BUTTON;

   if( hb_parl(11) ) {
      style |= TBSTYLE_AUTOSIZE;
   }

   if( hb_parl(12) ) {
      style |= BTNS_CHECK;
   }

   if( hb_parl(13) ) {
      style |= BTNS_GROUP;
   }

   if( hb_parl(14) ) {
      style |= BTNS_DROPDOWN;
   }

   if( hb_parl(15) ) {
      style |= BTNS_WHOLEDROPDOWN;
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   // Button New

   tbb[nBtn].iBitmap   = nPoz;
   tbb[nBtn].idCommand = hb_parni(3);
   tbb[nBtn].fsState   = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle   = style;
   nBtn++;

   if( hb_parl(10) ) {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage(hwndTB, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);
   SendMessage(hwndTB, TB_ADDBUTTONS, nBtn, reinterpret_cast<LPARAM>(&tbb));
   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HWND(himage);
}

LONG WidestBtn(LPCTSTR pszStr, HWND hwnd)
{
#ifndef UNICODE
   LPCSTR lpString = pszStr;
#else
   LPCWSTR lpString = AnsiToWide(const_cast<char*>(pszStr));
#endif

   LOGFONT lf;
   SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(LOGFONT), &lf, 0);

   auto hdc = GetDC(hwnd);
   auto hFont = CreateFontIndirect(&lf);
   SelectObject(hdc, hFont);

   SIZE sz;
   GetTextExtentPoint32(hdc, lpString, lstrlen(lpString), &sz);

   ReleaseDC(hwnd, hdc);
   DeleteObject(hFont);

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpString);
#endif

   return MAKELONG(sz.cx, sz.cy);
}

/*
INITTOOLBAREX(HWND, p2, HMENU, p4, p5, np6, np7, p8, p9, lp10, lp11, lp12, lp13, lp14, lp15, lp16, lp17) --> HWND
*/
HB_FUNC( INITTOOLBAREX )
{
   INITCOMMONCONTROLSEX icex{};
   icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
   icex.dwICC  = ICC_BAR_CLASSES;
   InitCommonControlsEx(&icex);

   DWORD style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;
   auto ExStyle = 0;
   int TbExStyle = TBSTYLE_EX_DRAWDDARROWS;

   if( hb_parl(14) ) {
      ExStyle |= WS_EX_CLIENTEDGE;
   } else {
      TbExStyle |= TBSTYLE_EX_HIDECLIPPEDBUTTONS;
   }

   if( hb_parl(10) ) {
      style |= TBSTYLE_FLAT;
   }

   if( hb_parl(11) ) {
      style |= CCS_BOTTOM;
   }

   if( hb_parl(12) ) {
      style |= TBSTYLE_LIST;
   }

   if( hb_parl(13) ) {
      style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl(15) ) {
      TbExStyle |= TBSTYLE_EX_MIXEDBUTTONS;
   }

   if( hb_parl(16) ) {
      style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl(17) ) {
      style |= CCS_ADJUSTABLE;
   }

   auto hwndTB = CreateWindowEx(ExStyle, TOOLBARCLASSNAME, nullptr, style, 0, 0, 0, 0, hmg_par_HWND(1), hmg_par_HMENU(3), GetInstance(), nullptr);

   if( hb_parni(6) && hb_parni(7) ) {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(6), hb_parni(7));
      DWORD nPadd = SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(6) - LOWORD(nPadd), hb_parni(7) - HIWORD(nPadd)));
   }

   SendMessage(hwndTB, TB_SETBUTTONWIDTH, 0, MAKELONG(hb_parni(6), hb_parni(6)));
   SendMessage(hwndTB, TB_SETEXTENDEDSTYLE, 0, TbExStyle);
   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HWND(hwndTB);
}

/*
INITTOOLBUTTONEX(HWND, cText, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) --> HWND
*/
HB_FUNC( INITTOOLBUTTONEX )
{
   TBBUTTON      lpBtn;
   DWORD         tSize;
   TCHAR         cBuff[255] = { 0 };
   int           index;
   int           nPoz;
   int           Transparent = hb_parl(9) ? 0 : 1;
   UINT          fuLoad = (Transparent == 0) ? LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS : LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT;

   TBBUTTON tbb[NUM_TOOLBAR_BUTTONS];
   memset(tbb, 0, sizeof tbb);

   auto hwndTB = hmg_par_HWND(1);
   auto nBtn    = 0;
   auto tmax    = 0;
   auto ix      = 0;
   auto iy      = 0;
   int xBtn    = SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   DWORD TbStyle = SendMessage(hwndTB, TB_GETSTYLE, 0, 0);
   int style   = TBSTYLE_BUTTON;

   OSVERSIONINFO osvi;
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);

   // Add the strings

   if( hb_parclen(2) ) {
      void * str;
      LPCTSTR lpText = HB_PARSTR(2, &str, nullptr);
      index = SendMessage(hwndTB, TB_ADDSTRING, 0, reinterpret_cast<LPARAM>(lpText));
      tbb[nBtn].iString = index;
      style |= BTNS_SHOWTEXT;
      hb_strfree(str);
      tSize = WidestBtn(static_cast<LPCTSTR>(hb_parc(2)), hwndTB);
      tmax  = HIWORD(tSize);
      for( auto i = 0; i < xBtn; i++ ) {
         SendMessage(hwndTB, TB_GETBUTTON, i, reinterpret_cast<LPARAM>(&lpBtn));
         SendMessage(hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, reinterpret_cast<LPARAM>(cBuff));
         tSize = WidestBtn(cBuff, hwndTB);
         if( tmax < HIWORD(tSize) ) {
            tmax = HIWORD(tSize);
         }
      }
   }

   tSize = SendMessage(hwndTB, TB_GETPADDING, 0, 0);
   int px    = LOWORD(tSize);
   int py    = HIWORD(tSize);

   if( hb_parl(16) ) {
      ix = hb_parni(6) - px;
      iy = hb_parni(7) - py;
   }

   HWND himage = nullptr;

   if( HB_ISCHAR(8) ) {
      void * str;
      LPCTSTR lpImageName = HB_PARSTR(8, &str, nullptr);
      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, ix, iy, fuLoad));
      if( himage == nullptr ) {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, ix, iy, LR_LOADFROMFILE | fuLoad));
      }
      if( himage == nullptr ) {
         himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(8), hb_parl(16) ? ix : -1, hb_parl(16) ? iy : -1, hwndTB, 1, Transparent, -1, hb_parl(16) ? 1 : 0, false, 255));
      }
      hb_strfree(str);
   }

   if( himage != nullptr ) {
      tSize = SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      px    = LOWORD(tSize);
      py    = HIWORD(tSize);
      BITMAP bm;
      if( GetObject(himage, sizeof(BITMAP), &bm) != 0 ) {
         ix = bm.bmWidth;
         iy = bm.bmHeight;
         if( TbStyle & TBSTYLE_LIST ) {
            tmax = 0;
         }

         if( (ix + px) > hb_parni(6) ) {
            ix = hb_parni(6) - px;
         } else {
            px = hb_parni(6) - ix;
         }

         if( (iy + tmax + py) > hb_parni(7) ) {
            iy = hb_parni(7) - tmax - py;
         } else {
            py = hb_parni(7) - tmax - iy;
         }

         if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 ) {
            if( !(TbStyle & TBSTYLE_LIST) ) {
               SendMessage(hwndTB, TB_SETPADDING, 0, MAKELPARAM(px, py));
            }
         } else if( !(style & BTNS_SHOWTEXT) ) {
            SendMessage(hwndTB, TB_SETPADDING, 0, MAKELPARAM(px, py));
         }

         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(ix, iy));
      }
   }

   // Add the bitmap containing button images to the toolbar.

   if( hb_parl(11) ) {
      style |= TBSTYLE_AUTOSIZE;
   }

   TBADDBITMAP   tbab;

   nBtn = 0;
   if( hb_parni(17) > -1 ) {
      if( xBtn == 0 ) {
         if( hb_parni(18) > IDB_HIST_LARGE_COLOR ) {
            SendMessage(hwndTB, TB_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(hmg_par_HIMAGELIST(18))); // TODO: revisar
            if( hb_parni(19) ) {
               SendMessage(hwndTB, TB_SETHOTIMAGELIST, 0, reinterpret_cast<LPARAM>(hmg_par_HIMAGELIST(19))); // TODO: revisar
            }

            tbab.nID = hb_parni(18);
         } else {
            tbab.hInst = HINST_COMMCTRL;
            tbab.nID   = hb_parni(18);
            SendMessage(hwndTB, TB_ADDBITMAP, 1, reinterpret_cast<LPARAM>(&tbab));
         }
      }

      nPoz = hb_parni(17);
   } else {
      tbab.hInst = nullptr;
      tbab.nID   = reinterpret_cast<UINT_PTR>(himage);
      nPoz       = SendMessage(hwndTB, TB_ADDBITMAP, 1, reinterpret_cast<LPARAM>(&tbab));
   }

   if( hb_parl(12) ) {
      style |= BTNS_CHECK;
   }

   if( hb_parl(13) ) {
      style |= BTNS_GROUP;
   }

   if( hb_parl(14) ) {
      style |= BTNS_DROPDOWN;
   }

   if( hb_parl(15) ) {
      style |= BTNS_WHOLEDROPDOWN;
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   // Button New

   tbb[nBtn].iBitmap   = nPoz;
   tbb[nBtn].idCommand = hb_parni(3);
   tbb[nBtn].fsState   = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle   = style;
   nBtn++;

   if( hb_parl(10) ) {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage(hwndTB, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);
   SendMessage(hwndTB, TB_ADDBUTTONS, nBtn, reinterpret_cast<LPARAM>(&tbb));
   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HWND(himage);
}

/*
GETSIZETOOLBAR(HWND) --> numeric
*/
HB_FUNC( GETSIZETOOLBAR )
{
   auto hwndTB = hmg_par_HWND(1);
   SIZE lpSize;
   SendMessage(hwndTB, TB_GETMAXSIZE, 0, reinterpret_cast<LPARAM>(&lpSize));

   OSVERSIONINFO osvi;
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);

   int nBtn = SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   TBBUTTON lpBtn;
   for( auto i = 0; i < nBtn; i++ ) {
      SendMessage(hwndTB, TB_GETBUTTON, i, reinterpret_cast<LPARAM>(&lpBtn));
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 ) {
         if( lpBtn.fsStyle & TBSTYLE_SEP ) {
            lpSize.cx = lpSize.cx + 3;
         }
      }
      if( lpBtn.fsStyle & BTNS_DROPDOWN ) {
         lpSize.cx = lpSize.cx + 16;
      }
   }

   hb_retnl(MAKELONG(lpSize.cy, lpSize.cx));
}

/*
MAXTEXTBTNTOOLBAR() -->
*/
HB_FUNC( MAXTEXTBTNTOOLBAR )
{
   TCHAR cString[255] = { 0 };
   auto hwndTB = hmg_par_HWND(1);
   int nBtn   = SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   TBBUTTON lpBtn;
   DWORD    tSize;
   auto ty = 0;
   auto tmax = 0;

   for( auto i = 0; i < nBtn; i++ ) {
      SendMessage(hwndTB, TB_GETBUTTON, i, reinterpret_cast<LPARAM>(&lpBtn));
      SendMessage(hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, reinterpret_cast<LPARAM>(cString));
      tSize = WidestBtn(cString, hwndTB);
      ty    = HIWORD(tSize);
      if( tmax < LOWORD(tSize) ) {
         tmax = LOWORD(tSize);
      }
   }

   if( tmax == 0 ) {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3));
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(2), hb_parni(3)));
   } else {
      DWORD style = SendMessage(hwndTB, TB_GETSTYLE, 0, 0);
      if( style & TBSTYLE_LIST ) {
         SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3) + 2);
         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(3), hb_parni(3)));
      } else {
         SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3) - ty + 2);
         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(3) - ty, hb_parni(3) - ty));
      }

      SendMessage(hwndTB, TB_SETBUTTONWIDTH, 0, MAKELONG(hb_parni(2), hb_parni(2) + 2));
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0); //JP62
}

/*
ISBUTTONBARCHECKED(HWND, WPARAM) --> .T.|.F.
*/
HB_FUNC( ISBUTTONBARCHECKED )
{
   TBBUTTON lpBtn;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&lpBtn));
   hb_retl(SendMessage(hmg_par_HWND(1), TB_ISBUTTONCHECKED, lpBtn.idCommand, 0));
}

/*
CHECKBUTTONBAR(HWND, WPARAM, lp3) --> NIL
*/
HB_FUNC( CHECKBUTTONBAR )
{
   TBBUTTON lpBtn;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&lpBtn));
   SendMessage(hmg_par_HWND(1), TB_CHECKBUTTON, lpBtn.idCommand, hb_parl(3));
}

/*
ISBUTTONENABLED(HWND, WPARAM) --> .T.|.F.
*/
HB_FUNC( ISBUTTONENABLED )
{
   TBBUTTON lpBtn;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&lpBtn));
   hb_retl(SendMessage(hmg_par_HWND(1), TB_ISBUTTONENABLED, lpBtn.idCommand, 0));
}

/*
GETBUTTONBARRECT(HWND, WPARAM) --> numeric
*/
HB_FUNC( GETBUTTONBARRECT )
{
   RECT rc;
   SendMessage(hmg_par_HWND(1), TB_GETITEMRECT, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rc));
   hb_retnl(MAKELONG(rc.left, rc.bottom));
}

/*
GETBUTTONPOS(p1) --> numeric
*/
HB_FUNC( GETBUTTONPOS )
{
   hb_retnl(((reinterpret_cast<NMTOOLBAR FAR*>(HB_PARNL(1)))->iItem));
}

/*
SETBUTTONTIP(p1, cText) --> NIL
*/
HB_FUNC( SETBUTTONTIP )
{
   //void * str;
   auto lpttt = reinterpret_cast<LPTOOLTIPTEXT>(HB_PARNL(1));
   lpttt->hinst = GetModuleHandle(nullptr);
   lpttt->lpszText = const_cast<TCHAR*>(hb_parc(2)); //HB_PARSTR(2, &str, nullptr); // TODO: revisar (memory leak ?)
   //hb_strfree(str);
}

/*
SETTOOLBUTTONCAPTION(HWND, WPARAM, cText) --> NIL
*/
HB_FUNC( SETTOOLBUTTONCAPTION )
{
   void * str;
   TBBUTTONINFO tbinfo;
   tbinfo.cbSize = sizeof(tbinfo);
   tbinfo.dwMask = TBIF_TEXT;
   tbinfo.pszText = const_cast<TCHAR*>(HB_PARSTR(3, &str, nullptr));
   SendMessage(hmg_par_HWND(1), TB_SETBUTTONINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&tbinfo));
   hb_strfree(str);
}

/*
SETTOOLBUTTONIMAGE(HWND, WPARAM, np3) --> NIL
*/
HB_FUNC( SETTOOLBUTTONIMAGE )
{
   TBBUTTONINFO tbinfo;
   tbinfo.cbSize = sizeof(tbinfo);
   tbinfo.dwMask = TBIF_IMAGE;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTONINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&tbinfo));
   tbinfo.iImage = hb_parni(3);
   SendMessage(hmg_par_HWND(1), TB_SETBUTTONINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&tbinfo));
}

/*
REPLACETOOLBUTTONIMAGE(HWND, hBITMAP, cp3, lp4, np5) --> HBITMAP
*/
HB_FUNC( REPLACETOOLBUTTONIMAGE )
{
   auto hwndTB = hmg_par_HWND(1);
   auto hBitmapOld = hmg_par_HBITMAP(2);
   int     iImageIdx  = hb_parl(4) ? I_IMAGECALLBACK : I_IMAGENONE;
   int     nButtonID  = hmg_par_INT(5);

   auto hBitmapNew = HMG_LoadPicture(hb_parc(3), -1, -1, hwndTB, 1, 1, -1, 0, false, 255);

   if( (hBitmapOld != nullptr) && (hBitmapNew != nullptr) ) {
      TBREPLACEBITMAP tbrb;
      tbrb.hInstOld = nullptr;
      tbrb.nIDOld   = reinterpret_cast<UINT_PTR>(hBitmapOld);
      tbrb.hInstNew = nullptr;
      tbrb.nIDNew   = reinterpret_cast<UINT_PTR>(hBitmapNew);
      tbrb.nButtons = 1;
      SendMessage(hwndTB, TB_REPLACEBITMAP, 0, reinterpret_cast<LPARAM>(&tbrb));
   } else {
      TBBUTTONINFO tbinfo;
      int          iBitMapIndex;

      if( hBitmapNew != nullptr ) {
         TBADDBITMAP tbab;
         tbab.hInst   = nullptr;
         tbab.nID     = reinterpret_cast<UINT_PTR>(hBitmapNew);
         iBitMapIndex = SendMessage(hwndTB, TB_ADDBITMAP, 1, reinterpret_cast<LPARAM>(&tbab));
      } else {
         iBitMapIndex = iImageIdx;
      }

      tbinfo.cbSize = sizeof(tbinfo);
      tbinfo.dwMask = TBIF_IMAGE;
      SendMessage(hwndTB, TB_GETBUTTONINFO, nButtonID, reinterpret_cast<LPARAM>(&tbinfo));

      tbinfo.iImage = iBitMapIndex;
      SendMessage(hwndTB, TB_SETBUTTONINFO, nButtonID, reinterpret_cast<LPARAM>(&tbinfo));
   }

   hmg_ret_HBITMAP(hBitmapNew);
}

/*
SETROWSBUTTON(HWND, np2, lp3) --> array
*/
HB_FUNC( SETROWSBUTTON )
{
   RECT rc;
   SendMessage(hmg_par_HWND(1), TB_SETROWS, MAKEWPARAM(hb_parni(2), hb_parl(3)), reinterpret_cast<LPARAM>(&rc));
   hb_reta(2);
   HB_STORVNL(rc.right - rc.left, -1, 1);
   HB_STORVNL(rc.bottom - rc.top, -1, 2);
}

/*
RESIZESPLITBOXITEM(HWND, WPARAM, np3, np4, np5) --> NIL
*/
HB_FUNC( RESIZESPLITBOXITEM )
{
   REBARBANDINFO rbBand;
   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_CHILDSIZE | RBBIM_IDEALSIZE;
   SendMessage(hmg_par_HWND(1), RB_GETBANDINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rbBand));
   rbBand.fStyle     = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxMinChild = hb_parni(3);
   rbBand.cyMinChild = hb_parni(4);
   rbBand.cxIdeal    = hb_parni(5);
   rbBand.cx         = hb_parni(5);
   SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rbBand));
}

/*
SETCHEVRONSTYLESPLITBOXITEM(HWND, WPARAM, np3) --> .T.|.F.
*/
HB_FUNC( SETCHEVRONSTYLESPLITBOXITEM )
{
   REBARBANDINFO rbBand;
   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_STYLE | RBBIM_IDEALSIZE;
   SendMessage(hmg_par_HWND(1), RB_GETBANDINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rbBand));
   rbBand.fStyle  = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxIdeal = hb_parni(3) + 50;
   hb_retl(SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rbBand)));
}

/*
SETCAPTIONSPLITBOXITEM(HWND, WPARAM, cText) --> NIL
*/
HB_FUNC( SETCAPTIONSPLITBOXITEM )
{
   void * str;
   REBARBANDINFO rbBand;
   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_TEXT;
   rbBand.lpText = const_cast<TCHAR*>(HB_PARSTR(3, &str, nullptr));
   SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&rbBand));
   hb_strfree(str);
}

int TestHidenBtn(HWND tbHwnd, RECT rcRb, INT dv, INT nBtn)
{
   RECT rcDst, rcBt;
   auto nBtnV = 0;

   for( auto i = 0; i < nBtn; i++ ) {
      SendMessage(tbHwnd, TB_GETITEMRECT, i, reinterpret_cast<LPARAM>(&rcBt));

      rcBt.left   += dv;
      rcBt.top    += rcRb.top;
      rcBt.right  += dv;
      rcBt.bottom += rcRb.top;

      IntersectRect(&rcDst, &rcRb, &rcBt);
      if( EqualRect(&rcDst, &rcBt) ) {
         nBtnV++;
      }
   }

   return nBtnV;
}

/*
CREATEPOPUPCHEVRON(HWND, np2) --> array
*/
HB_FUNC( CREATEPOPUPCHEVRON )
{
   auto hwnd = hmg_par_HWND(1);

   RECT rcRR;
   GetWindowRect(hwnd, &rcRR);

   auto lpRB = reinterpret_cast<LPNMREBARCHEVRON>(HB_PARNL(2));
   int uBand = lpRB->uBand;
   RECT rcCvr = lpRB->rc;

   RECT rcRb;
   SendMessage(hwnd, RB_GETRECT, uBand, reinterpret_cast<LPARAM>(&rcRb));

   rcRb.right -= ((rcCvr.right - rcCvr.left));

   REBARBANDINFO    rbbi;
   rbbi.cbSize = sizeof(REBARBANDINFO);
   rbbi.fMask  = RBBIM_SIZE | RBBIM_CHILD | RBBIM_CHILDSIZE;
   SendMessage(hwnd, RB_GETBANDINFO, uBand, reinterpret_cast<LPARAM>(&rbbi));

   HWND tbHwnd = rbbi.hwndChild;
   RECT rcTB;
   GetWindowRect(tbHwnd, &rcTB);
   int dv   = rcTB.left - rcRR.left + 1;
   int nBtn = SendMessage(tbHwnd, TB_BUTTONCOUNT, 0, 0);

   int tx = TestHidenBtn(tbHwnd, rcRb, dv, nBtn);

   hb_reta(7);
   HB_STORVNL(rcCvr.left, -1, 1);
   HB_STORVNL(rcCvr.top, -1, 2);
   HB_STORVNL(rcCvr.right, -1, 3);
   HB_STORVNL(rcCvr.bottom, -1, 4);
   hmg_storvhandle(tbHwnd, -1, 5);
   HB_STORNI(tx, -1, 6);
   HB_STORNI(nBtn, -1, 7);
}

/*
GETBUTTONBAR(HWND, WPARAM) --> array
*/
HB_FUNC( GETBUTTONBAR )
{
   TBBUTTON lpBtn;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), reinterpret_cast<LPARAM>(&lpBtn));

   bool lSep    = (lpBtn.fsStyle & TBSTYLE_SEP) ? true : false;
   bool lEnable = (lpBtn.fsState & TBSTATE_ENABLED) ? true : false;

   hb_reta(4);
   HB_STORNI(lpBtn.iBitmap, -1, 1);
   HB_STORNI(lpBtn.idCommand, -1, 2);
   HB_STORL(lSep, -1, 3);
   HB_STORL(lEnable, -1, 4);
}

/*
GETIMAGELIST(HWND, np2) --> HBITMAP
*/
HB_FUNC( GETIMAGELIST )
{
   auto himl = reinterpret_cast<HIMAGELIST>(SendMessage(hmg_par_HWND(1), TB_GETIMAGELIST, 0, 0));
   IMAGEINFO  ImageInfo;
   ImageList_GetImageInfo(himl, hmg_par_INT(2), &ImageInfo);
   HBITMAP himage = ImageInfo.hbmImage;
   hmg_ret_HBITMAP(himage);
}

/*
SETCHEVRONIMAGE(HMENU, np2, HBITMAP) --> NIL
*/
HB_FUNC( SETCHEVRONIMAGE )
{
   SetMenuItemBitmaps(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND, hmg_par_HBITMAP(3), hmg_par_HBITMAP(3));
}

/*
DESTROYMENU(HMENU) --> NIL
*/
HB_FUNC( DESTROYMENU )
{
   DestroyMenu(hmg_par_HMENU(1));
}

/*
ADJUSTFLOATTOOLBAR() -->
*/
HB_FUNC( ADJUSTFLOATTOOLBAR )
{
   auto hwndTB = hmg_par_HWND(3);
   RECT  rc;
   SendMessage(hwndTB, TB_GETITEMRECT, 0, reinterpret_cast<LPARAM>(&rc));
   int nbuttons = SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   int height = rc.bottom + GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME) + 2 * GetSystemMetrics(SM_CYDLGFRAME);
   int width  = nbuttons * rc.right;
   width += 2 * GetSystemMetrics(SM_CXDLGFRAME);
   POINT pt;
   pt.x   = pt.y = 50;
   MapWindowPoints(hmg_par_HWND(1), HWND_DESKTOP, static_cast<LPPOINT>(&pt), 1);
   MoveWindow(hmg_par_HWND(2), pt.x, pt.y, width, height, TRUE);
}

int ResizeToolbar( HWND hwndTB, int widthTb ) // TODO: revisar (porque passar parâmetros se os valores passados não são utilizados ?)
{
   hwndTB  = hmg_par_HWND(1);
   widthTb = hb_parni(2);

   RECT rc;
   SendMessage(hwndTB, TB_GETITEMRECT, 0, reinterpret_cast<LPARAM>(&rc));
   int bwidth = rc.right;
   if( widthTb < bwidth ) {
      return 0;
   }

   GetWindowRect(hwndTB, &rc);
   int heightTB = rc.bottom - rc.top;

   int nButtons = SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   int n;

   if( bwidth > 0 ) {
      n = widthTb / bwidth;
   } else {
      return 0;
   }

   int nrow;

   if( nButtons % n == 0 ) {
      nrow = nButtons / n;
   } else {
      nrow = nButtons / n + 1;
   }

   RECT  rcb{};
   SendMessage(hwndTB, TB_SETROWS, MAKEWPARAM(nrow, TRUE), reinterpret_cast<LPARAM>(&rcb));
   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   HWND hwndParent = GetParent(hwndTB);
   DWORD style = GetWindowLong(hwndParent, GWL_STYLE);
   AdjustWindowRect(&rcb, style, 0);
   MapWindowPoints(hwndParent, HWND_DESKTOP, reinterpret_cast<LPPOINT>(&rcb), 2);

   int nBtnRow = nButtons / nrow;
   if( nrow > 1 ) {
      nBtnRow += nButtons & 1;
   }

   int width  = nBtnRow * bwidth;
   width += 2 * GetSystemMetrics(SM_CXDLGFRAME);
   width += 2 * GetSystemMetrics(SM_CXBORDER);
   int height = rcb.bottom - rcb.top;
   if( !(width == widthTb) || !(height == heightTB) ) {
      MoveWindow(hwndParent, rcb.left, rcb.top, width, height, TRUE);
   }

   return 1;
}

/*
RESIZEFLOATTOOLBAR(HWND, nWidth) --> .T.|.F.
*/
HB_FUNC( RESIZEFLOATTOOLBAR )
{
   if( isInSizeMsg ) {
      hb_retl(false);
   }

   isInSizeMsg = 1;

   auto hwndTB = hmg_par_HWND(1);
   auto widthTb = hb_parni(2);

   if( hwndTB ) {
      ResizeToolbar(hwndTB, widthTb);
   }

   isInSizeMsg = 0;

   hb_retl(true);
}

/*
TOOLBAREXCUSTFUNC(p1, p2, p3, p4) --> .T.|.F.
*/
HB_FUNC( TOOLBAREXCUSTFUNC )
{
   LPARAM lParam = HB_PARNL(4);
   auto lpTB = reinterpret_cast<LPTBNOTIFY>(lParam);

   auto Msg = hmg_par_UINT(2);

   switch( Msg ) {
      case WM_NOTIFY: {
         switch( (reinterpret_cast<LPNMHDR>(lParam))->code) {
            case TBN_BEGINADJUST: { // Start customizing the toolbar.
               nResetCount = SendMessage(lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0);
               buttonCount = nResetCount;
               lpSaveButtons = static_cast<LPTBBUTTON>(GlobalAlloc(GPTR, sizeof(TBBUTTON) * nResetCount));
               for( auto i = 0; i < nResetCount; i++ ) {
                  SendMessage(lpTB->hdr.hwndFrom, TB_GETBUTTON, i, reinterpret_cast<LPARAM>(lpSaveButtons + i));
               }
               hb_retl(true);
               break;
            }
            case TBN_GETBUTTONINFO: {
               auto lpTbNotify = reinterpret_cast<LPTBNOTIFY>(lParam);
               if( lpTbNotify->iItem >= buttonCount || lpTbNotify->iItem < 0 ) {
                  hb_retl(false);
               } else {
                  TBBUTTON lpBtn;
                  SendMessage(lpTB->hdr.hwndFrom, TB_GETBUTTON, lpTbNotify->iItem, reinterpret_cast<LPARAM>(&lpBtn));
                  lpTbNotify->tbButton = lpSaveButtons[lpTbNotify->iItem];
                  hb_retl(true);
               }
               break;
            }
            case TBN_RESET: {
               int nCount = SendMessage(lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0);
               for( int i = nCount - 1; i >= 0; i-- ) {
                  SendMessage(lpTB->hdr.hwndFrom, TB_DELETEBUTTON, i, 0);
               }
               SendMessage(lpTB->hdr.hwndFrom, TB_ADDBUTTONS, nResetCount, reinterpret_cast<LPARAM>(lpSaveButtons));
               hb_retl(true);
               break;
            }
            case TBN_ENDADJUST: {
               GlobalFree(lpSaveButtons);
               hb_retl(true);
               break;
            }
            default: {
               hb_retl(false);
               break;
            }
         }
      }
   }
}
