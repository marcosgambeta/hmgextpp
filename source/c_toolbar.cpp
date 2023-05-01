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

#if defined( _MSC_VER )
#pragma warning ( disable:4996 )
#endif
#include <commctrl.h>

#define NUM_TOOLBAR_BUTTONS  10

extern HBITMAP HMG_LoadPicture(const char * FileName, int New_Width, int New_Height, HWND hWnd, int ScaleStretch, int Transparent, long BackgroundColor, int AdjustImage,
                               HB_BOOL bAlphaFormat, int iAlpfaConstant);

LRESULT APIENTRY ToolBarExFunc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam );

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

static LPTBBUTTON lpSaveButtons;
static int        nResetCount, buttonCount;
static int        isInSizeMsg = 0;

HB_FUNC( INITTOOLBAR )
{
   HWND hwnd;
   HWND hwndTB;
   int style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;

   int ExStyle = 0;
   int TbExStyle = TBSTYLE_EX_DRAWDDARROWS;

   hwnd = hmg_par_HWND(1);

   if( hb_parl(14) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   if( hb_parl(10) )
   {
      style |= TBSTYLE_FLAT;
   }

   if( hb_parl(11) )
   {
      style |= CCS_BOTTOM;
   }

   if( hb_parl(12) )
   {
      style |= TBSTYLE_LIST;
   }

   if( hb_parl(13) )
   {
      style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl(15) )
   {
      style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl(16) )
   {
      style |= CCS_ADJUSTABLE;
   }

   hwndTB = CreateWindowEx(ExStyle, TOOLBARCLASSNAME, nullptr, style, 0, 0, 0, 0, hwnd, hmg_par_HMENU(3), GetInstance(), nullptr);

   if( hb_parni(6) && hb_parni(7) )
   {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(6), hb_parni(7));
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(6), hb_parni(7)));
   }

   SendMessage(hwndTB, TB_SETEXTENDEDSTYLE, 0, TbExStyle);

   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HANDLE(hwndTB);
}

HB_FUNC( INITTOOLBUTTON )
{
   HWND        hwndTB = hmg_par_HWND(1);
   HWND        himage = nullptr;
   TBADDBITMAP tbab;
   TBBUTTON    tbb[NUM_TOOLBAR_BUTTONS];
   DWORD       tSize;
   int         index;
   int         nPoz;
   int         nBtn;
   int         style = TBSTYLE_BUTTON;
   int         Transparent = hb_parl(9) ? 0 : 1;

#ifndef UNICODE
   LPCSTR lpText;
#else
   LPWSTR lpText;
#endif

   if( hb_parclen(8) > 0 )
   {
      int px;
      int py;
      int ix = 0;
      int iy = 0;

      tSize = ( DWORD ) SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      px    = LOWORD(tSize);
      py    = HIWORD(tSize);

      if( hb_parl(16) )
      {
         ix = hb_parni(6) - px;
         iy = hb_parni(7) - py;
      }

      himage = ( HWND ) HMG_LoadPicture(hb_parc(8), hb_parl(16) ? ix : -1, hb_parl(16) ? iy : -1, hwndTB, 1, Transparent, -1, hb_parl(16) ? 1 : 0, HB_FALSE, 255);
   }

   memset(tbb, 0, sizeof tbb);

   // Add the bitmap containing button images to the toolbar.

   if( hb_parl(11) )
   {
      style |= TBSTYLE_AUTOSIZE;
   }

   nBtn       = 0;
   tbab.hInst = nullptr;
   tbab.nID   = ( UINT_PTR ) himage;
   nPoz       = ( int ) SendMessage(hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) &tbab);

   // Add the strings

   if( hb_parclen(2) > 0 )
   {
#ifndef UNICODE
      lpText = hb_parc(2);
#else
      lpText = AnsiToWide(( char * ) hb_parc(2));
#endif
      index = ( int ) SendMessage(hwndTB, TB_ADDSTRING, 0, ( LPARAM ) lpText);
      tbb[nBtn].iString = index;
#ifdef UNICODE
      hb_xfree(lpText);
#endif
   }

   if( hb_parl(12) )
   {
      style |= BTNS_CHECK;
   }

   if( hb_parl(13) )
   {
      style |= BTNS_GROUP;
   }

   if( hb_parl(14) )
   {
      style |= BTNS_DROPDOWN;
   }

   if( hb_parl(15) )
   {
      style |= BTNS_WHOLEDROPDOWN;
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   // Button New

   tbb[nBtn].iBitmap   = nPoz;
   tbb[nBtn].idCommand = hb_parni(3);
   tbb[nBtn].fsState   = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle   = ( BYTE ) style;
   nBtn++;

   if( hb_parl(10) )
   {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage(hwndTB, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);

   SendMessage(hwndTB, TB_ADDBUTTONS, nBtn, ( LPARAM ) &tbb);

   ShowWindow(hwndTB, SW_SHOW);

   hmg_ret_HANDLE(himage);
}

LONG WidestBtn(LPCTSTR pszStr, HWND hwnd)
{
   SIZE    sz;
   LOGFONT lf;
   HFONT   hFont;
   HDC     hdc;

#ifndef UNICODE
   LPCSTR lpString = pszStr;
#else
   LPCWSTR lpString = AnsiToWide(( char * ) pszStr);
#endif

   SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(LOGFONT), &lf, 0);

   hdc   = GetDC(hwnd);
   hFont = CreateFontIndirect(&lf);
   SelectObject(hdc, hFont);

   GetTextExtentPoint32(hdc, lpString, ( int ) lstrlen(lpString), &sz);

   ReleaseDC(hwnd, hdc);
   DeleteObject(hFont);

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpString);
#endif

   return MAKELONG(sz.cx, sz.cy);
}

HB_FUNC( INITTOOLBAREX )
{
   HWND hwnd;
   HWND hwndTB;
   int style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;

   int   ExStyle   = 0;
   int   TbExStyle = TBSTYLE_EX_DRAWDDARROWS;
   DWORD nPadd;

   INITCOMMONCONTROLSEX icex;

   icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
   icex.dwICC  = ICC_BAR_CLASSES;
   InitCommonControlsEx(&icex);

   hwnd = hmg_par_HWND(1);

   if( hb_parl(14) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }
   else
   {
      TbExStyle |= TBSTYLE_EX_HIDECLIPPEDBUTTONS;
   }

   if( hb_parl(10) )
   {
      style |= TBSTYLE_FLAT;
   }

   if( hb_parl(11) )
   {
      style |= CCS_BOTTOM;
   }

   if( hb_parl(12) )
   {
      style |= TBSTYLE_LIST;
   }

   if( hb_parl(13) )
   {
      style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl(15) )
   {
      TbExStyle |= TBSTYLE_EX_MIXEDBUTTONS;
   }

   if( hb_parl(16) )
   {
      style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl(17) )
   {
      style |= CCS_ADJUSTABLE;
   }

   hwndTB = CreateWindowEx(ExStyle, TOOLBARCLASSNAME, nullptr, style, 0, 0, 0, 0, hwnd, hmg_par_HMENU(3), GetInstance(), nullptr);

   if( hb_parni(6) && hb_parni(7) )
   {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(6), hb_parni(7));
      nPadd = ( DWORD ) SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(6) - LOWORD(nPadd), hb_parni(7) - HIWORD(nPadd)));
   }

   SendMessage(hwndTB, TB_SETBUTTONWIDTH, 0, MAKELONG(hb_parni(6), hb_parni(6)));
   SendMessage(hwndTB, TB_SETEXTENDEDSTYLE, 0, TbExStyle);

   ShowWindow(hwndTB, SW_SHOW);
   hmg_ret_HANDLE(hwndTB);
}

HB_FUNC( INITTOOLBUTTONEX )
{
   HWND          hwndTB;
   HWND          himage = nullptr;
   BITMAP        bm;
   TBADDBITMAP   tbab;
   TBBUTTON      lpBtn;
   TBBUTTON      tbb[NUM_TOOLBAR_BUTTONS];
   DWORD         tSize;
   TCHAR         cBuff[255] = { 0 };
   int           index;
   int           nPoz, xBtn;
   int           nBtn, tmax;
   int           style;
   DWORD         TbStyle;
   int           ix;
   int           iy;
   int           px;
   int           py;
   int           Transparent = hb_parl(9) ? 0 : 1;
   UINT          fuLoad = ( Transparent == 0 ) ? LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS : LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT;
   OSVERSIONINFO osvi;

#ifndef UNICODE
   LPCSTR lpImageName, lpText;
#else
   LPWSTR lpImageName, lpText;
#endif

   memset(tbb, 0, sizeof tbb);

   hwndTB  = hmg_par_HWND(1);
   nBtn    = 0;
   tmax    = 0;
   ix      = 0;
   iy      = 0;
   xBtn    = ( int ) SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   TbStyle = ( DWORD ) SendMessage(hwndTB, TB_GETSTYLE, 0, 0);
   style   = TBSTYLE_BUTTON;

   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);

   // Add the strings

   if( hb_parclen(2) )
   {
#ifndef UNICODE
      lpText = hb_parc(2);
#else
      lpText = AnsiToWide(( char * ) hb_parc(2));
#endif
      index = ( int ) SendMessage(hwndTB, TB_ADDSTRING, 0, ( LPARAM ) lpText);
      tbb[nBtn].iString = index;
      style |= BTNS_SHOWTEXT;
#ifdef UNICODE
      hb_xfree(lpText);
#endif
      tSize = WidestBtn(( LPCTSTR ) hb_parc(2), hwndTB);
      tmax  = HIWORD(tSize);
      for( int i = 0; i < xBtn; i++ )
      {
         SendMessage(hwndTB, TB_GETBUTTON, i, ( LPARAM ) &lpBtn);
         SendMessage(hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, ( LPARAM ) ( LPCTSTR ) cBuff);
         tSize = WidestBtn(cBuff, hwndTB);
         if( tmax < HIWORD(tSize) )
         {
            tmax = HIWORD(tSize);
         }
      }
   }

   tSize = ( DWORD ) SendMessage(hwndTB, TB_GETPADDING, 0, 0);
   px    = LOWORD(tSize);
   py    = HIWORD(tSize);

   if( hb_parl(16) )
   {
      ix = hb_parni(6) - px;
      iy = hb_parni(7) - py;
   }

   if( HB_ISCHAR(8) )
   {
#ifndef UNICODE
      lpImageName = hb_parc(8);
#else
      lpImageName = AnsiToWide(( char * ) hb_parc(8));
#endif
      himage = ( HWND ) LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, ix, iy, fuLoad);
      if( himage == nullptr )
      {
         himage = ( HWND ) LoadImage(nullptr, lpImageName, IMAGE_BITMAP, ix, iy, LR_LOADFROMFILE | fuLoad);
      }
      if( himage == nullptr )
      {
         himage = ( HWND ) HMG_LoadPicture(hb_parc(8), hb_parl(16) ? ix : -1, hb_parl(16) ? iy : -1, hwndTB, 1, Transparent, -1, hb_parl(16) ? 1 : 0, HB_FALSE, 255);
      }

#ifdef UNICODE
      hb_xfree(lpImageName);
#endif
   }

   if( himage != nullptr )
   {
      tSize = ( DWORD ) SendMessage(hwndTB, TB_GETPADDING, 0, 0);
      px    = LOWORD(tSize);
      py    = HIWORD(tSize);
      if( GetObject(himage, sizeof(BITMAP), &bm) != 0 )
      {
         ix = bm.bmWidth;
         iy = bm.bmHeight;
         if( TbStyle & TBSTYLE_LIST )
         {
            tmax = 0;
         }

         if( ( ix + px ) > hb_parni(6) )
         {
            ix = hb_parni(6) - px;
         }
         else
         {
            px = hb_parni(6) - ix;
         }

         if( ( iy + tmax + py ) > hb_parni(7) )
         {
            iy = hb_parni(7) - tmax - py;
         }
         else
         {
            py = hb_parni(7) - tmax - iy;
         }

         if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 )
         {
            if( !( TbStyle & TBSTYLE_LIST ) )
            {
               SendMessage(hwndTB, TB_SETPADDING, 0, MAKELPARAM(px, py));
            }
         }
         else if( !( style & BTNS_SHOWTEXT ) )
         {
            SendMessage(hwndTB, TB_SETPADDING, 0, MAKELPARAM(px, py));
         }

         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(ix, iy));
      }
   }

   // Add the bitmap containing button images to the toolbar.

   if( hb_parl(11) )
   {
      style |= TBSTYLE_AUTOSIZE;
   }

   nBtn = 0;
   if( hb_parni(17) > -1 )
   {
      if( xBtn == 0 )
      {
         if( hb_parni(18) > IDB_HIST_LARGE_COLOR )
         {
            SendMessage(hwndTB, TB_SETIMAGELIST, 0, ( LPARAM ) hmg_par_HIMAGELIST(18));
            if( hb_parni(19) )
            {
               SendMessage(hwndTB, TB_SETHOTIMAGELIST, 0, ( LPARAM ) hmg_par_HIMAGELIST(19));
            }

            tbab.nID = hb_parni(18);
         }
         else
         {
            tbab.hInst = HINST_COMMCTRL;
            tbab.nID   = hb_parni(18);
            SendMessage(hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) &tbab);
         }
      }

      nPoz = hb_parni(17);
   }
   else
   {
      tbab.hInst = nullptr;
      tbab.nID   = ( UINT_PTR ) reinterpret_cast<HBITMAP>(himage);
      nPoz       = ( int ) SendMessage(hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) &tbab);
   }

   if( hb_parl(12) )
   {
      style |= BTNS_CHECK;
   }

   if( hb_parl(13) )
   {
      style |= BTNS_GROUP;
   }

   if( hb_parl(14) )
   {
      style |= BTNS_DROPDOWN;
   }

   if( hb_parl(15) )
   {
      style |= BTNS_WHOLEDROPDOWN;
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   // Button New

   tbb[nBtn].iBitmap   = nPoz;
   tbb[nBtn].idCommand = hb_parni(3);
   tbb[nBtn].fsState   = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle   = ( BYTE ) style;
   nBtn++;

   if( hb_parl(10) )
   {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage(hwndTB, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);

   SendMessage(hwndTB, TB_ADDBUTTONS, nBtn, ( LPARAM ) &tbb);

   ShowWindow(hwndTB, SW_SHOW);

   hmg_ret_HANDLE(himage);
}

HB_FUNC( GETSIZETOOLBAR )
{
   SIZE          lpSize;
   TBBUTTON      lpBtn;
   int           nBtn;
   OSVERSIONINFO osvi;
   HWND          hwndTB;

   hwndTB = hmg_par_HWND(1);

   SendMessage(hwndTB, TB_GETMAXSIZE, 0, ( LPARAM ) &lpSize);

   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);
   nBtn = ( int ) SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   for( int i = 0; i < nBtn; i++ )
   {
      SendMessage(hwndTB, TB_GETBUTTON, i, ( LPARAM ) &lpBtn);
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 )
      {
         if( lpBtn.fsStyle & TBSTYLE_SEP )
         {
            lpSize.cx = lpSize.cx + 3;
         }
      }

      if( lpBtn.fsStyle & BTNS_DROPDOWN )
      {
         lpSize.cx = lpSize.cx + 16;
      }
   }

   hb_retnl( MAKELONG(lpSize.cy, lpSize.cx) );
}

HB_FUNC( MAXTEXTBTNTOOLBAR )
{
   TCHAR cString[255] = { 0 };
   HWND  hwndTB;

   int      nBtn;
   int      tmax = 0;
   int      ty   = 0;
   DWORD    tSize;
   DWORD    style;
   TBBUTTON lpBtn;

   hwndTB = hmg_par_HWND(1);
   nBtn   = ( int ) SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);
   for( int i = 0; i < nBtn; i++ )
   {
      SendMessage(hwndTB, TB_GETBUTTON, i, ( LPARAM ) &lpBtn);
      SendMessage(hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, ( LPARAM ) ( LPCTSTR ) cString);

      tSize = WidestBtn(cString, hwndTB);
      ty    = HIWORD(tSize);

      if( tmax < LOWORD(tSize) )
      {
         tmax = LOWORD(tSize);
      }
   }

   if( tmax == 0 )
   {
      SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3));
      SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(2), hb_parni(3)));
   }
   else
   {
      style = ( DWORD ) SendMessage(hwndTB, TB_GETSTYLE, 0, 0);
      if( style & TBSTYLE_LIST )
      {
         SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3) + 2);
         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(3), hb_parni(3)));
      }
      else
      {
         SendMessage(hwndTB, TB_SETBUTTONSIZE, hb_parni(2), hb_parni(3) - ty + 2);
         SendMessage(hwndTB, TB_SETBITMAPSIZE, 0, MAKELONG(hb_parni(3) - ty, hb_parni(3) - ty));
      }

      SendMessage(hwndTB, TB_SETBUTTONWIDTH, 0, MAKELONG(hb_parni(2), hb_parni(2) + 2));
   }

   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0); //JP62
}

HB_FUNC( ISBUTTONBARCHECKED )
{
   TBBUTTON lpBtn;

   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), ( LPARAM ) &lpBtn);
   hb_retl(( int ) SendMessage(hmg_par_HWND(1), TB_ISBUTTONCHECKED, lpBtn.idCommand, 0));
}

HB_FUNC( CHECKBUTTONBAR )
{
   TBBUTTON lpBtn;

   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), ( LPARAM ) &lpBtn);
   SendMessage(hmg_par_HWND(1), TB_CHECKBUTTON, lpBtn.idCommand, hb_parl(3));
}

HB_FUNC( ISBUTTONENABLED )
{
   TBBUTTON lpBtn;

   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), ( LPARAM ) &lpBtn);
   hb_retl(( int ) SendMessage(hmg_par_HWND(1), TB_ISBUTTONENABLED, lpBtn.idCommand, 0));
}

HB_FUNC( GETBUTTONBARRECT )
{
   RECT rc;

   SendMessage(hmg_par_HWND(1), TB_GETITEMRECT, hmg_par_WPARAM(2), ( LPARAM ) &rc);
   hb_retnl( MAKELONG(rc.left, rc.bottom) );
}

HB_FUNC( GETBUTTONPOS )
{
   hb_retnl( ( ( ( NMTOOLBAR FAR * ) HB_PARNL(1) )->iItem ) );
}

HB_FUNC( SETBUTTONTIP )
{
#ifndef UNICODE
   LPSTR lpText = ( LPSTR ) hb_parc(2);
#else
   LPWSTR lpText = AnsiToWide(( char * ) hb_parc(2));
#endif
   LPTOOLTIPTEXT lpttt;

   lpttt           = ( LPTOOLTIPTEXT ) HB_PARNL(1);
   lpttt->hinst    = GetModuleHandle(nullptr);
   lpttt->lpszText = lpText;
}

HB_FUNC( SETTOOLBUTTONCAPTION )
{
#ifndef UNICODE
   LPSTR lpText = ( LPSTR ) hb_parc(3);
#else
   LPWSTR lpText = AnsiToWide(( char * ) hb_parc(3));
#endif
   TBBUTTONINFO tbinfo;

   tbinfo.cbSize  = sizeof(tbinfo);
   tbinfo.dwMask  = TBIF_TEXT;
   tbinfo.pszText = lpText;

   SendMessage(hmg_par_HWND(1), TB_SETBUTTONINFO, hmg_par_WPARAM(2), ( LPARAM ) &tbinfo);

#ifdef UNICODE
   hb_xfree(lpText);
#endif
}

HB_FUNC( SETTOOLBUTTONIMAGE )
{
   TBBUTTONINFO tbinfo;

   tbinfo.cbSize = sizeof(tbinfo);
   tbinfo.dwMask = TBIF_IMAGE;
   SendMessage(hmg_par_HWND(1), TB_GETBUTTONINFO, hmg_par_WPARAM(2), ( LPARAM ) &tbinfo);

   tbinfo.iImage = hb_parni(3);
   SendMessage(hmg_par_HWND(1), TB_SETBUTTONINFO, hmg_par_WPARAM(2), ( LPARAM ) &tbinfo);
}

HB_FUNC( REPLACETOOLBUTTONIMAGE )
{
   HWND    hwndTB     = hmg_par_HWND(1);
   HBITMAP hBitmapOld = hmg_par_HBITMAP(2);
   int     iImageIdx  = hb_parl(4) ? I_IMAGECALLBACK : I_IMAGENONE;
   int     nButtonID  = hmg_par_INT(5);
   HBITMAP hBitmapNew;

   hBitmapNew = static_cast<HBITMAP>(HMG_LoadPicture(hb_parc(3), -1, -1, hwndTB, 1, 1, -1, 0, HB_FALSE, 255));

   if( ( hBitmapOld != nullptr ) && ( hBitmapNew != nullptr ) )
   {
      TBREPLACEBITMAP tbrb;
      tbrb.hInstOld = nullptr;
      tbrb.nIDOld   = ( UINT_PTR ) hBitmapOld;
      tbrb.hInstNew = nullptr;
      tbrb.nIDNew   = ( UINT_PTR ) hBitmapNew;
      tbrb.nButtons = 1;
      SendMessage(hwndTB, TB_REPLACEBITMAP, 0, ( LPARAM ) &tbrb);
   }
   else
   {
      TBBUTTONINFO tbinfo;
      int          iBitMapIndex;

      if( hBitmapNew != nullptr )
      {
         TBADDBITMAP tbab;
         tbab.hInst   = nullptr;
         tbab.nID     = ( UINT_PTR ) hBitmapNew;
         iBitMapIndex = ( int ) SendMessage(hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) &tbab);
      }
      else
      {
         iBitMapIndex = iImageIdx;
      }

      tbinfo.cbSize = sizeof(tbinfo);
      tbinfo.dwMask = TBIF_IMAGE;
      SendMessage(hwndTB, TB_GETBUTTONINFO, nButtonID, ( LPARAM ) &tbinfo);

      tbinfo.iImage = iBitMapIndex;
      SendMessage(hwndTB, TB_SETBUTTONINFO, nButtonID, ( LPARAM ) &tbinfo);
   }

   hmg_ret_HANDLE(hBitmapNew);
}

HB_FUNC( SETROWSBUTTON )
{
   RECT rc;

   SendMessage(hmg_par_HWND(1), ( UINT ) TB_SETROWS, MAKEWPARAM(hb_parni(2), hb_parl(3)), ( LPARAM ) &rc);

   hb_reta(2);
   HB_STORVNL( rc.right - rc.left, -1, 1 );
   HB_STORVNL( rc.bottom - rc.top, -1, 2 );
}

HB_FUNC( RESIZESPLITBOXITEM )
{
   REBARBANDINFO rbBand;

   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_CHILDSIZE | RBBIM_IDEALSIZE;

   SendMessage(hmg_par_HWND(1), RB_GETBANDINFO, hmg_par_WPARAM(2), ( LPARAM ) &rbBand);

   rbBand.fStyle     = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxMinChild = hb_parni(3);
   rbBand.cyMinChild = hb_parni(4);
   rbBand.cxIdeal    = hb_parni(5);
   rbBand.cx         = hb_parni(5);

   SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), ( LPARAM ) &rbBand);
}

HB_FUNC( SETCHEVRONSTYLESPLITBOXITEM )
{
   REBARBANDINFO rbBand;

   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_STYLE | RBBIM_IDEALSIZE;

   SendMessage(hmg_par_HWND(1), RB_GETBANDINFO, hmg_par_WPARAM(2), ( LPARAM ) &rbBand);

   rbBand.fStyle  = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxIdeal = hb_parni(3) + 50;

   hb_retl(( int ) SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), ( LPARAM ) &rbBand));
}

HB_FUNC( SETCAPTIONSPLITBOXITEM )
{
#ifndef UNICODE
   LPSTR lpText = ( LPSTR ) hb_parc(3);
#else
   LPWSTR lpText = AnsiToWide(( char * ) hb_parc(3));
#endif
   REBARBANDINFO rbBand;

   rbBand.cbSize = sizeof(REBARBANDINFO);
   rbBand.fMask  = RBBIM_TEXT;
   rbBand.lpText = lpText;

   SendMessage(hmg_par_HWND(1), RB_SETBANDINFO, hmg_par_WPARAM(2), ( LPARAM ) &rbBand);

#ifdef UNICODE
   hb_xfree(lpText);
#endif
}

int TestHidenBtn(HWND tbHwnd, RECT rcRb, INT dv, INT nBtn)
{
   RECT rcDst, rcBt;
   int  nBtnV = 0;

   for( int i = 0; i < nBtn; i++ )
   {
      SendMessage(( HWND ) tbHwnd, TB_GETITEMRECT, ( UINT ) i, ( LPARAM ) &rcBt);

      rcBt.left   += dv;
      rcBt.top    += rcRb.top;
      rcBt.right  += dv;
      rcBt.bottom += rcRb.top;

      IntersectRect(&rcDst, &rcRb, &rcBt);
      if( EqualRect(&rcDst, &rcBt) )
      {
         nBtnV++;
      }
   }

   return nBtnV;
}

HB_FUNC( CREATEPOPUPCHEVRON )
{
   RECT rcRb;
   RECT rcTB;
   RECT rcRR;
   RECT rcCvr;
   int  uBand;
   int  tx;
   int  dv;
   int  nBtn;
   LPNMREBARCHEVRON lpRB;
   REBARBANDINFO    rbbi;
   HWND tbHwnd;

   GetWindowRect(hmg_par_HWND(1), &rcRR);

   lpRB  = ( LPNMREBARCHEVRON ) HB_PARNL(2);
   uBand = lpRB->uBand;
   rcCvr = lpRB->rc;

   SendMessage(hmg_par_HWND(1), ( UINT ) RB_GETRECT, uBand, ( LPARAM ) &rcRb);

   rcRb.right -= ( ( rcCvr.right - rcCvr.left ) );
   rbbi.cbSize = sizeof(REBARBANDINFO);
   rbbi.fMask  = RBBIM_SIZE | RBBIM_CHILD | RBBIM_CHILDSIZE;

   SendMessage(hmg_par_HWND(1), RB_GETBANDINFO, uBand, ( LPARAM ) ( LPREBARBANDINFO ) &rbbi);

   tbHwnd = ( HWND ) rbbi.hwndChild;
   GetWindowRect(( HWND ) tbHwnd, &rcTB);
   dv   = rcTB.left - rcRR.left + 1;
   nBtn = ( INT ) SendMessage(( HWND ) tbHwnd, TB_BUTTONCOUNT, 0, 0);

   tx = TestHidenBtn(( HWND ) tbHwnd, rcRb, dv, nBtn);

   hb_reta(7);
   HB_STORVNL( rcCvr.left, -1, 1 );
   HB_STORVNL( rcCvr.top, -1, 2 );
   HB_STORVNL( rcCvr.right, -1, 3 );
   HB_STORVNL( rcCvr.bottom, -1, 4 );
   HB_STORVNL( ( LONG_PTR ) tbHwnd, -1, 5 );
   HB_STORNI( tx, -1, 6 );
   HB_STORNI( nBtn, -1, 7 );
}

HB_FUNC( GETBUTTONBAR )
{
   TBBUTTON lpBtn;
   BOOL     lSep;
   BOOL     lEnable;

   SendMessage(hmg_par_HWND(1), TB_GETBUTTON, hmg_par_WPARAM(2), ( LPARAM ) &lpBtn);

   lSep    = ( lpBtn.fsStyle & TBSTYLE_SEP ) ? TRUE : FALSE;
   lEnable = ( lpBtn.fsState & TBSTATE_ENABLED ) ? TRUE : FALSE;

   hb_reta(4);
   HB_STORNI( lpBtn.iBitmap, -1, 1 );
   HB_STORNI( lpBtn.idCommand, -1, 2 );
   HB_STORL( lSep, -1, 3 );
   HB_STORL( lEnable, -1, 4 );
}

HB_FUNC( GETIMAGELIST )
{
   HIMAGELIST himl;
   HBITMAP    himage;
   IMAGEINFO  ImageInfo;

   himl = ( HIMAGELIST ) SendMessage(hmg_par_HWND(1), TB_GETIMAGELIST, 0, 0);
   ImageList_GetImageInfo(himl, hmg_par_INT(2), &ImageInfo);

   himage = ImageInfo.hbmImage;

   hmg_ret_HANDLE(himage);
}

HB_FUNC( SETCHEVRONIMAGE )
{
   SetMenuItemBitmaps(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND, hmg_par_HBITMAP(3), hmg_par_HBITMAP(3));
}

HB_FUNC( DESTROYMENU )
{
   DestroyMenu(hmg_par_HMENU(1));
}

HB_FUNC( ADJUSTFLOATTOOLBAR )
{
   HWND  hwndTB;
   RECT  rc;
   int   nbuttons, height, width;
   POINT pt;

   hwndTB = hmg_par_HWND(3);

   SendMessage(hwndTB, TB_GETITEMRECT, 0, ( LPARAM ) &rc);
   nbuttons = ( int ) SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);

   height = rc.bottom + GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME) + 2 * GetSystemMetrics(SM_CYDLGFRAME);
   width  = ( nbuttons ) * rc.right;
   width += 2 * GetSystemMetrics(SM_CXDLGFRAME);
   pt.x   = pt.y = 50;

   MapWindowPoints(hmg_par_HWND(1), HWND_DESKTOP, ( LPPOINT ) &pt, 1);
   MoveWindow(hmg_par_HWND(2), pt.x, pt.y, width, height, TRUE);
}

int ResizeToolbar( HWND hwndTB, int widthTb )
{
   RECT  rcb, rc;
   int   n, width, height, nrow;
   HWND  hwndParent;
   DWORD style;
   int   nButtons, bwidth, nBtnRow;
   int   heightTB;

   hwndTB  = hmg_par_HWND(1);
   widthTb = hb_parni(2);

   SendMessage(hwndTB, TB_GETITEMRECT, 0, ( LPARAM ) &rc);
   bwidth = rc.right;
   if( widthTb < bwidth )
   {
      return 0;
   }

   GetWindowRect(hwndTB, &rc);
   heightTB = rc.bottom - rc.top;

   nButtons = ( int ) SendMessage(hwndTB, TB_BUTTONCOUNT, 0, 0);

   memset(&rcb, 0, sizeof(RECT));
   if( bwidth > 0 )
   {
      n = widthTb / ( bwidth );
   }
   else
   {
      return 0;
   }

   if( nButtons % n == 0 )
   {
      nrow = nButtons / n;
   }
   else
   {
      nrow = nButtons / n + 1;
   }

   SendMessage(hwndTB, TB_SETROWS, MAKEWPARAM(nrow, TRUE), ( LPARAM ) &rcb);
   SendMessage(hwndTB, TB_AUTOSIZE, 0, 0);

   hwndParent = GetParent(hwndTB);
   style = GetWindowLong(hwndParent, GWL_STYLE);
   AdjustWindowRect(&rcb, style, 0);
   MapWindowPoints(hwndParent, HWND_DESKTOP, ( LPPOINT ) &rcb, 2);

   nBtnRow = nButtons / ( nrow );
   if( nrow > 1 )
   {
      nBtnRow += nButtons & 1;
   }

   width  = nBtnRow * bwidth;
   width += 2 * GetSystemMetrics(SM_CXDLGFRAME);
   width += 2 * GetSystemMetrics(SM_CXBORDER);
   height = rcb.bottom - rcb.top;
   if( !( width == widthTb ) || !( height == heightTB ) )
   {
      MoveWindow(hwndParent, rcb.left, rcb.top, width, height, TRUE);
   }

   return 1;
}

HB_FUNC( RESIZEFLOATTOOLBAR )
{
   HWND hwndTB  = hmg_par_HWND(1);
   int  widthTb = hb_parni(2);

   if( isInSizeMsg )
   {
      hb_retl(FALSE);
   }

   isInSizeMsg = 1;

   if( hwndTB )
   {
      ResizeToolbar( hwndTB, widthTb );
   }

   isInSizeMsg = 0;

   hb_retl(TRUE);
}

HB_FUNC( TOOLBAREXCUSTFUNC )
{
   TBBUTTON   lpBtn;
   UINT       Msg    = hmg_par_UINT(2);
   LPARAM     lParam = ( LPARAM ) HB_PARNL(4);
   LPTBNOTIFY lpTB   = ( LPTBNOTIFY ) lParam;

   switch( Msg )
   {
      case WM_NOTIFY:
         switch( ( ( LPNMHDR ) lParam )->code )
         {
            case TBN_BEGINADJUST: // Start customizing the toolbar.

               nResetCount = ( int ) SendMessage(lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0);
               buttonCount = nResetCount;

               lpSaveButtons = ( LPTBBUTTON ) GlobalAlloc(GPTR, sizeof(TBBUTTON) * nResetCount);
               for( int i = 0; i < nResetCount; i++ )
               {
                  SendMessage(lpTB->hdr.hwndFrom, TB_GETBUTTON, i, ( LPARAM ) (lpSaveButtons + i));
               }

               hb_retl(TRUE);
               break;

            case TBN_GETBUTTONINFO:
            {
               LPTBNOTIFY lpTbNotify = ( LPTBNOTIFY ) lParam;

               if( lpTbNotify->iItem >= buttonCount || lpTbNotify->iItem < 0 )
               {
                  hb_retl(FALSE);
               }
               else
               {
                  SendMessage(lpTB->hdr.hwndFrom, TB_GETBUTTON, lpTbNotify->iItem, ( LPARAM ) &lpBtn);
                  lpTbNotify->tbButton = lpSaveButtons[lpTbNotify->iItem];

                  hb_retl(TRUE);
               }
            }
            break;

            case TBN_RESET:
            {
               int nCount;

               nCount = ( int ) SendMessage(lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0);
               for( int i = nCount - 1; i >= 0; i-- )
               {
                  SendMessage(lpTB->hdr.hwndFrom, TB_DELETEBUTTON, i, 0);
               }

               SendMessage(lpTB->hdr.hwndFrom, TB_ADDBUTTONS,
                            nResetCount, ( LPARAM ) lpSaveButtons);

               hb_retl(TRUE);
            }
            break;

            case TBN_ENDADJUST:

               GlobalFree(( HGLOBAL ) lpSaveButtons);
               hb_retl(TRUE);
               break;

            default:
               hb_retl(FALSE);
               break;
         }
   }
}
