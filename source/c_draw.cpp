/*
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 http://harbourminigui.googlepages.com/

 This program is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 2 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with
 this software; see the file COPYING. If not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 visit the web site http://www.gnu.org/).

 As a special exception, you have permission for additional uses of the text
 contained in this release of Harbour Minigui.

 The exception is that, if you link the Harbour Minigui library with other
 files to produce an executable, this does not by itself cause the resulting
 executable to be covered by the GNU General Public License.
 Your use of that executable is in no way restricted on account of linking the
 Harbour-Minigui library code into it.

 Parts of this project are based upon:

 "Harbour GUI framework for Win32"
 Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 Copyright 2001 Antonio Linares <alinares@fivetech.com>
 www - https://harbour.github.io/

 "Harbour Project"
 Copyright 1999-2022, https://harbour.github.io/

 "WHAT32"
 Copyright 2002 AJ Wos <andrwos@aust1.net>

 "HWGUI"
 Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 Parts  of  this  code  is contributed and used here under permission of his
 author: Copyright 2017 (C) P.Chornyj <myorg63@mail.ru>
*/

#include "mgdefs.hpp"
#include <hbapiitm.h>

#if defined(__BORLANDC__)
WINGDIAPI BOOL WINAPI GdiFlush(void);
#endif

extern HB_EXPORT BOOL Array2ColorRef(PHB_ITEM aCRef, COLORREF * cr);
extern HB_EXPORT BOOL Array2Rect(PHB_ITEM aRect, RECT * rc);
extern HB_EXPORT PHB_ITEM Rect2Array(RECT * rc);

HB_FUNC( BEGINPAINT )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      PAINTSTRUCT ps;
      hmg_ret_HANDLE(BeginPaint(hWnd, &ps));
      hb_storclen(reinterpret_cast<const char*>(&ps), sizeof(PAINTSTRUCT), 2);
   }
   else
   {
      hmg_ret_HANDLE(nullptr);
   }
}

HB_FUNC( ENDPAINT )
{
   HWND hWnd = hmg_par_HWND(1);
   PAINTSTRUCT * pps = reinterpret_cast<PAINTSTRUCT*>(const_cast<char*>(hb_parc(2)));

   if( IsWindow(hWnd) && pps )
   {
      hb_retl(EndPaint(hWnd, pps));
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( DRAWFOCUSRECT )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      InflateRect(&pps->rcItem, -3, -3);
      DrawFocusRect(pps->hDC, &pps->rcItem);
      InflateRect(&pps->rcItem, +3, +3);
   }
}

HB_FUNC( DRAWSTATE )
{
   HWND hWnd = hmg_par_HWND(1);
   HDC hDC;
   BOOL bDC = FALSE;

   if( IsWindow(hWnd) )
   {
      hDC = GetDC(hWnd);
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_HDC(1);
   }

   if( GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC )
   {
      HBRUSH hBrush = nullptr;
      COLORREF crBrush;
      LPARAM lpData;
      WPARAM wData = static_cast<WPARAM>(hb_parclen(4));
      HB_ISIZ fuFlags = hb_parns(10);

      if( Array2ColorRef(hb_param(2, Harbour::Item::ANY), &crBrush) )
      {
         hBrush = CreateSolidBrush(crBrush);
      }

      if( wData > 0 )
      {
         lpData = reinterpret_cast<LPARAM>(hb_parc(4));
      }
      else
      {
         lpData = static_cast<LPARAM>(static_cast<LONG_PTR>(HB_PARNL(4)));
      }

      hb_retl(DrawState(hDC, hBrush, nullptr, lpData, wData, hmg_par_int(6), hmg_par_int(7), hmg_par_int(8), hmg_par_int(9), static_cast<UINT>(fuFlags)) ? true : false);

      if( bDC )
      {
         ReleaseDC(hWnd, hDC);
      }

      if( hb_parl(11) )
      {
         if( GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_BITMAP )
         {
            DeleteObject(reinterpret_cast<HBITMAP>(lpData));
         }
         else
         {
            DestroyIcon(reinterpret_cast<HICON>(lpData));
         }
      }
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( GETUPDATERECT )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      if( HB_ISNIL(2) )
      {
         hb_retl(GetUpdateRect(hWnd, nullptr, hmg_par_BOOL(3)) ? true : false);
      }
      else
      {
         RECT rc;
         hb_retl(GetUpdateRect(hWnd, &rc, hmg_par_BOOL(3)) ? true : false);
         hb_itemParamStoreRelease(2, Rect2Array(&rc));
      }
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( GDIFLUSH )
{
   hb_retl(GdiFlush() ? true : false);
}

HB_FUNC( GRAYSTRING )
{
   int nCount = hb_parni(5);
   int nLen = static_cast<int>(hb_parclen(4));

   if( nCount > 0 )
   {
      nCount = HB_MIN(nCount, nLen);
   }
   else
   {
      nCount = nLen;
   }

   if( nLen > 0 )
   {
      HWND hWnd = hmg_par_HWND(1);
      HDC hDC;
      BOOL bDC = FALSE;

      if( IsWindow(hWnd) )
      {
         hDC = GetDC(hWnd);
         bDC = TRUE;
      }
      else
      {
         hDC = hmg_par_HDC(1);
      }

      if( GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC )
      {
         HBRUSH hBrush = nullptr;
         COLORREF crBrush;
         const char * lpData = hb_parc(4);

         if( Array2ColorRef(hb_param(2, Harbour::Item::ANY), &crBrush) )
         {
            hBrush = CreateSolidBrush(crBrush);
         }

         hb_retl(GrayString(hDC, hBrush, nullptr, reinterpret_cast<LPARAM>(lpData), nCount, hmg_par_int(6), hmg_par_int(7), hmg_par_int(8), hmg_par_int(9)) ? true : false);

         if( bDC )
         {
            ReleaseDC(hWnd, hDC);
         }
      }
      else
      {
         hb_retl(false);
      }
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( INVALIDATERECT )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      BOOL bRect = FALSE;
      RECT rc;

      if( (hb_pcount() > 2) && (!HB_ISNIL(3)) )
      {
         bRect = Array2Rect(hb_param(3, Harbour::Item::ANY), &rc);

         if( !bRect )
         {
            rc.left   = hmg_par_LONG(3);
            rc.top    = hmg_par_LONG(4);
            rc.right  = hmg_par_LONG(5);
            rc.bottom = hmg_par_LONG(6);

            bRect = TRUE;
         }
      }

      hb_retl(InvalidateRect(hWnd, bRect ? &rc : nullptr, hb_parni(2) /* erase-background flag */) ? true : false);
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( REDRAWWINDOW )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      UINT uiFlags = RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW;

      if( hb_parl(2) == true )
      {
         uiFlags |= RDW_INTERNALPAINT;
      }

      hb_retl(RedrawWindow(hWnd, nullptr, nullptr, uiFlags) ? true : false);
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( C_SETBACKCOLOR )
{
   HWND hWnd = hmg_par_HWND(1);
   HDC hDC;
   BOOL bDC = FALSE;

   if( IsWindow(hWnd) )
   {
      hDC = GetDC(hWnd);
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_HDC(1);
   }

   if( GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC )
   {
      COLORREF cr;

      if( !Array2ColorRef(hb_param(2, Harbour::Item::ANY), &cr) )
      {
         cr = static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4)));
      }

      hb_retns(static_cast<HB_ISIZ>(SetBkColor(hDC, cr)));

      if( bDC )
      {
         ReleaseDC(hWnd, hDC);
      }
   }
   else
   {
      hb_retns(static_cast<HB_ISIZ>(CLR_INVALID));
   }
}

HB_FUNC( SETBKMODE )
{
   HWND hWnd = hmg_par_HWND(1);
   HDC hDC;
   BOOL bDC = FALSE;

   if( IsWindow(hWnd) )
   {
      hDC = GetDC(hWnd);
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_HDC(1);
   }

   if( GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC )
   {
      hb_retni(SetBkMode(hDC, hb_parnidef(2, OPAQUE)));

      if( bDC )
      {
         ReleaseDC(hWnd, hDC);
      }
   }
   else
   {
      hb_retni(0);
   }
}

HB_FUNC( UPDATEWINDOW )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      hb_retl(UpdateWindow(hWnd) ? true : false);
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( VALIDATERECT )
{
   HWND hWnd = hmg_par_HWND(1);

   if( IsWindow(hWnd) )
   {
      BOOL bRect = FALSE;
      RECT rc;

      if( (hb_pcount() > 1) && (!HB_ISNIL(2)) )
      {
         bRect = Array2Rect(hb_param(2, Harbour::Item::ANY), &rc);

         if( !bRect )
         {
            rc.left   = hmg_par_LONG(2);
            rc.top    = hmg_par_LONG(3);
            rc.right  = hmg_par_LONG(4);
            rc.bottom = hmg_par_LONG(5);

            bRect = TRUE;
         }
      }

      hb_retl(ValidateRect(hWnd, bRect ? &rc : nullptr));
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( WINDOWFROMDC )
{
   HB_RETNL(reinterpret_cast<LONG_PTR>(WindowFromDC(hmg_par_HDC(1))));
}
