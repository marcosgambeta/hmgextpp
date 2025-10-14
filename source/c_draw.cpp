//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

// $BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this software; see the file COPYING. If not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
// visit the web site http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of Harbour Minigui.
//
// The exception is that, if you link the Harbour Minigui library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// Harbour-Minigui library code into it.
// $END_LICENSE$

// Parts of this project are based upon:
//
// "Harbour GUI framework for Win32"
// Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2022, https://harbour.github.io/
//
// "WHAT32"
// Copyright 2002 AJ Wos <andrwos@aust1.net>
//
// "HWGUI"
// Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

// Parts  of  this  code  is contributed and used here under permission of his
// author: Copyright 2017 (C) P.Chornyj <myorg63@mail.ru>

#include "mgdefs.hpp"
#include <hbapiitm.h>

#if 0
#if defined(__BORLANDC__)
WINGDIAPI BOOL WINAPI GdiFlush(void);
#endif
#endif

bool hmg_ArrayToColorRef(PHB_ITEM aCRef, COLORREF *cr);
bool hmg_ArrayToRect(PHB_ITEM aRect, RECT *rc);
PHB_ITEM hmg_RectToArray(RECT *rc);

// HMG_BEGINPAINT(HWND, cp2) --> HANDLE
HB_FUNC(HMG_BEGINPAINT)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    PAINTSTRUCT ps;
    hmg_ret_HDC(BeginPaint(hWnd, &ps));
    hb_storclen(reinterpret_cast<const char *>(&ps), sizeof(PAINTSTRUCT), 2);
  } else {
    hmg_ret_HDC(nullptr);
  }
}

// HMG_ENDPAINT(HWND, cp2) --> .T.|.F.
HB_FUNC(HMG_ENDPAINT)
{
  auto hWnd = hmg_par_HWND(1);
  auto pps = reinterpret_cast<PAINTSTRUCT *>(const_cast<char *>(hb_parc(2)));

  if (IsWindow(hWnd) && pps) {
    hb_retl(EndPaint(hWnd, pps));
  } else {
    hb_retl(false);
  }
}

// HMG_DRAWFOCUSRECT(p1) --> NIL
HB_FUNC(HMG_DRAWFOCUSRECT)
{
  auto pps = reinterpret_cast<DRAWITEMSTRUCT *>(HB_PARNL(1));

  if (pps) {
    InflateRect(&pps->rcItem, -3, -3);
    DrawFocusRect(pps->hDC, &pps->rcItem);
    InflateRect(&pps->rcItem, +3, +3);
  }
}

// HMG_DRAWSTATE(HWND|HDC, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) --> .T.|.F.
HB_FUNC(HMG_DRAWSTATE)
{
  auto hWnd = hmg_par_HWND(1);
  HDC hDC;
  auto bDC = false;

  if (IsWindow(hWnd)) {
    hDC = GetDC(hWnd);
    bDC = true;
  } else {
    hDC = hmg_par_HDC(1);
  }

  if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC) {
    HBRUSH hBrush = nullptr;
    COLORREF crBrush;
    LPARAM lpData;
    auto wData = static_cast<WPARAM>(hb_parclen(4));
    HB_ISIZ fuFlags = hb_parns(10);

    if (hmg_ArrayToColorRef(hb_param(2, Harbour::Item::ANY), &crBrush)) {
      hBrush = CreateSolidBrush(crBrush);
    }

    if (wData > 0) {
      lpData = reinterpret_cast<LPARAM>(hb_parc(4));
    } else {
      lpData = static_cast<LPARAM>(static_cast<LONG_PTR>(HB_PARNL(4)));
    }

    hb_retl(DrawState(hDC, hBrush, nullptr, lpData, wData, hmg_par_int(6), hmg_par_int(7), hmg_par_int(8),
                      hmg_par_int(9), static_cast<UINT>(fuFlags))
                ? true
                : false);

    if (bDC) {
      ReleaseDC(hWnd, hDC);
    }

    if (hb_parl(11)) {
      if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_BITMAP) {
        DeleteObject(reinterpret_cast<HBITMAP>(lpData));
      } else {
        DestroyIcon(reinterpret_cast<HICON>(lpData));
      }
    }
  } else {
    hb_retl(false);
  }
}

// HMG_GETUPDATERECT(HWND, p2, p3) --> .T.|.F.
HB_FUNC(HMG_GETUPDATERECT)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    if (HB_ISNIL(2)) {
      hb_retl(GetUpdateRect(hWnd, nullptr, hmg_par_BOOL(3)) ? true : false);
    } else {
      RECT rc;
      hb_retl(GetUpdateRect(hWnd, &rc, hmg_par_BOOL(3)) ? true : false);
      hb_itemParamStoreRelease(2, hmg_RectToArray(&rc));
    }
  } else {
    hb_retl(false);
  }
}

// HMG_GDIFLUSH() --> .T.|.F.
HB_FUNC(HMG_GDIFLUSH)
{
  hb_retl(GdiFlush() ? true : false);
}

// HMG_GRAYSTRING(HWND|HDC, p2, p3, p4, p5, p6, p7, p8, p9) --> .T.|.F.
HB_FUNC(HMG_GRAYSTRING)
{
  auto nCount = hb_parni(5);
  auto nLen = static_cast<int>(hb_parclen(4));

  if (nCount > 0) {
    nCount = HB_MIN(nCount, nLen);
  } else {
    nCount = nLen;
  }

  if (nLen > 0) {
    auto hWnd = hmg_par_HWND(1);
    HDC hDC;
    auto bDC = false;

    if (IsWindow(hWnd)) {
      hDC = GetDC(hWnd);
      bDC = true;
    } else {
      hDC = hmg_par_HDC(1);
    }

    if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC) {
      HBRUSH hBrush = nullptr;
      COLORREF crBrush;
      auto lpData = hb_parc(4);

      if (hmg_ArrayToColorRef(hb_param(2, Harbour::Item::ANY), &crBrush)) {
        hBrush = CreateSolidBrush(crBrush);
      }

      hb_retl(GrayString(hDC, hBrush, nullptr, reinterpret_cast<LPARAM>(lpData), nCount, hmg_par_int(6), hmg_par_int(7),
                         hmg_par_int(8), hmg_par_int(9))
                  ? true
                  : false);

      if (bDC) {
        ReleaseDC(hWnd, hDC);
      }
    } else {
      hb_retl(false);
    }
  } else {
    hb_retl(false);
  }
}

// HMG_INVALIDATERECT(HWND, p2, p3, p4, p5, p6) --> .T.|.F.
HB_FUNC(HMG_INVALIDATERECT)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    auto bRect = false;
    RECT rc;

    if ((hb_pcount() > 2) && (!HB_ISNIL(3))) {
      bRect = hmg_ArrayToRect(hb_param(3, Harbour::Item::ANY), &rc);

      if (!bRect) {
        rc.left = hmg_par_LONG(3);
        rc.top = hmg_par_LONG(4);
        rc.right = hmg_par_LONG(5);
        rc.bottom = hmg_par_LONG(6);

        bRect = true;
      }
    }

    hb_retl(InvalidateRect(hWnd, bRect ? &rc : nullptr, hb_parni(2) /* erase-background flag */) ? true : false);
  } else {
    hb_retl(false);
  }
}

// HMG_REDRAWWINDOW(HWND, p2) --> .T.|.F.
HB_FUNC(HMG_REDRAWWINDOW)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    UINT uiFlags = RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW;

    if (static_cast<bool>(hb_parl(2)) == true) {
      uiFlags |= RDW_INTERNALPAINT;
    }

    hb_retl(RedrawWindow(hWnd, nullptr, nullptr, uiFlags) ? true : false);
  } else {
    hb_retl(false);
  }
}

// HMG_C_SETBACKCOLOR(HWND|HDC, p2, p3, p4) --> ns
HB_FUNC(HMG_C_SETBACKCOLOR)
{
  auto hWnd = hmg_par_HWND(1);
  HDC hDC;
  auto bDC = false;

  if (IsWindow(hWnd)) {
    hDC = GetDC(hWnd);
    bDC = true;
  } else {
    hDC = hmg_par_HDC(1);
  }

  if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC) {
    COLORREF cr;

    if (!hmg_ArrayToColorRef(hb_param(2, Harbour::Item::ANY), &cr)) {
      cr = static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4)));
    }

    hb_retns(static_cast<HB_ISIZ>(SetBkColor(hDC, cr)));

    if (bDC) {
      ReleaseDC(hWnd, hDC);
    }
  } else {
    hb_retns(static_cast<HB_ISIZ>(CLR_INVALID));
  }
}

// HMG_SETBKMODE(HWND|HDC, p2) --> numeric
HB_FUNC(HMG_SETBKMODE)
{
  auto hWnd = hmg_par_HWND(1);
  HDC hDC;
  auto bDC = false;

  if (IsWindow(hWnd)) {
    hDC = GetDC(hWnd);
    bDC = true;
  } else {
    hDC = hmg_par_HDC(1);
  }

  if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC) {
    hb_retni(SetBkMode(hDC, hb_parnidef(2, OPAQUE)));

    if (bDC) {
      ReleaseDC(hWnd, hDC);
    }
  } else {
    hb_retni(0);
  }
}

// HMG_UPDATEWINDOW(HWND) --> .T.|.F.
HB_FUNC(HMG_UPDATEWINDOW)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    hb_retl(UpdateWindow(hWnd) ? true : false);
  } else {
    hb_retl(false);
  }
}

// HMG_VALIDATERECT(HWND, p2, p3, p4, p5) --> .T.|.F.
HB_FUNC(HMG_VALIDATERECT)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    auto bRect = false;
    RECT rc;

    if ((hb_pcount() > 1) && (!HB_ISNIL(2))) {
      bRect = hmg_ArrayToRect(hb_param(2, Harbour::Item::ANY), &rc);

      if (!bRect) {
        rc.left = hmg_par_LONG(2);
        rc.top = hmg_par_LONG(3);
        rc.right = hmg_par_LONG(4);
        rc.bottom = hmg_par_LONG(5);

        bRect = true;
      }
    }

    hb_retl(ValidateRect(hWnd, bRect ? &rc : nullptr));
  } else {
    hb_retl(false);
  }
}

// HMG_WINDOWFROMDC(HDC) --> numeric
HB_FUNC(HMG_WINDOWFROMDC)
{
  HB_RETNL(reinterpret_cast<LONG_PTR>(WindowFromDC(hmg_par_HDC(1))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(BEGINPAINT, HMG_BEGINPAINT)
HB_FUNC_TRANSLATE(ENDPAINT, HMG_ENDPAINT)
HB_FUNC_TRANSLATE(DRAWFOCUSRECT, HMG_DRAWFOCUSRECT)
HB_FUNC_TRANSLATE(DRAWSTATE, HMG_DRAWSTATE)
HB_FUNC_TRANSLATE(GETUPDATERECT, HMG_GETUPDATERECT)
HB_FUNC_TRANSLATE(GDIFLUSH, HMG_GDIFLUSH)
HB_FUNC_TRANSLATE(GRAYSTRING, HMG_GRAYSTRING)
HB_FUNC_TRANSLATE(INVALIDATERECT, HMG_INVALIDATERECT)
HB_FUNC_TRANSLATE(REDRAWWINDOW, HMG_REDRAWWINDOW)
HB_FUNC_TRANSLATE(C_SETBACKCOLOR, HMG_C_SETBACKCOLOR)
HB_FUNC_TRANSLATE(SETBKMODE, HMG_SETBKMODE)
HB_FUNC_TRANSLATE(UPDATEWINDOW, HMG_UPDATEWINDOW)
HB_FUNC_TRANSLATE(VALIDATERECT, HMG_VALIDATERECT)
HB_FUNC_TRANSLATE(WINDOWFROMDC, HMG_WINDOWFROMDC)
#endif
