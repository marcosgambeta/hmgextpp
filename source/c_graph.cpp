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

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbwinuni.hpp>

bool hmg_ArrayToColorRef(PHB_ITEM aCRef, COLORREF *cr);
extern HFONT PrepareFont(const TCHAR *FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline,
                         DWORD StrikeOut, DWORD Angle, DWORD charset);

// HMG_TEXTDRAW(HWND|HDC, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) --> .T.|.F.
HB_FUNC(HMG_TEXTDRAW)
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
    void *str1;
    void *str2;

    int bold = hb_parl(11) ? FW_BOLD : FW_NORMAL;
    auto italic = static_cast<DWORD>(hb_parl(12));
    auto underline = static_cast<DWORD>(hb_parl(13));
    auto strikeout = static_cast<DWORD>(hb_parl(14));
    DWORD angle = hb_parnl(16);
    LPCTSTR lpString = HB_PARSTR(4, &str1, nullptr);
    int iBkMode;
    COLORREF crBkColor = CLR_INVALID;
    COLORREF crFgColor = CLR_INVALID;
    RECT rect;

    auto font = PrepareFont(HB_PARSTR(9, &str2, nullptr), hb_parni(10), bold, italic, underline, strikeout, angle,
                            DEFAULT_CHARSET);

    auto hgdiobj = SelectObject(hDC, font);

    if (hb_parl(15)) {
      iBkMode = SetBkMode(hDC, TRANSPARENT);
    } else {
      iBkMode = SetBkMode(hDC, OPAQUE);

      if (hmg_ArrayToColorRef(hb_param(8, Harbour::Item::ANY), &crBkColor)) {
        crBkColor = SetBkColor(hDC, crBkColor);
      }
    }

    if (hmg_ArrayToColorRef(hb_param(7, Harbour::Item::ANY), &crFgColor)) {
      SetTextColor(hDC, crFgColor);
    }

    SetRect(&rect, hb_parni(3), hb_parni(2), hb_parni(6), hb_parni(5));

    hb_retl(ExtTextOut(hDC, hb_parni(3), hb_parni(2), ETO_OPAQUE, &rect, lpString, lstrlen(lpString), nullptr) ? true
                                                                                                               : false);

    SelectObject(hDC, hgdiobj);

    if (0 != iBkMode) {
      SetBkMode(hDC, iBkMode);
    }

    if (CLR_INVALID != crBkColor) {
      SetBkColor(hDC, crBkColor);
    }

    if (CLR_INVALID != crFgColor) {
      SetTextColor(hDC, crFgColor);
    }

    DeleteObject(font);

    if (bDC) {
      ReleaseDC(hWnd, hDC);
    }

    hb_strfree(str1);
    hb_strfree(str2);
  } else {
    hb_retl(false);
  }
}

// HMG_LINEDRAW(HWND, p2, p3, p4, p5, p6, p7) --> NIL
HB_FUNC(HMG_LINEDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(7), static_cast<COLORREF>(RGB(HB_PARNI(6, 1), HB_PARNI(6, 2), HB_PARNI(6, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);
  MoveToEx(hdc1, hmg_par_int(3), hmg_par_int(2), nullptr);
  LineTo(hdc1, hmg_par_int(5), hmg_par_int(4));
  SelectObject(hdc1, hgdiobj1);
  DeleteObject(hpen);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_RECTDRAW(HWND, p2, p3, p4, p5, p6, p7, p8, p9) --> NIL
HB_FUNC(HMG_RECTDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(7), static_cast<COLORREF>(RGB(HB_PARNI(6, 1), HB_PARNI(6, 2), HB_PARNI(6, 3))));

  auto hgdiobj1 = SelectObject(hdc1, hpen);

  HBRUSH hbrush;
  HGDIOBJ hgdiobj2;
  LOGBRUSH br;

  if (hb_parl(9)) {
    hbrush = CreateSolidBrush(static_cast<COLORREF>(RGB(HB_PARNI(8, 1), HB_PARNI(8, 2), HB_PARNI(8, 3))));
    hgdiobj2 = SelectObject(hdc1, hbrush);
  } else {
    br.lbStyle = BS_HOLLOW;
    hbrush = CreateBrushIndirect(&br);
    hgdiobj2 = SelectObject(hdc1, hbrush);
  }

  Rectangle(hdc1, hmg_par_int(3), hmg_par_int(2), hmg_par_int(5), hmg_par_int(4));
  SelectObject(hdc1, hgdiobj1);
  SelectObject(hdc1, hgdiobj2);
  DeleteObject(hpen);
  DeleteObject(hbrush);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_ROUNDRECTDRAW() --> NIL
HB_FUNC(HMG_ROUNDRECTDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(9), static_cast<COLORREF>(RGB(HB_PARNI(8, 1), HB_PARNI(8, 2), HB_PARNI(8, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);

  HBRUSH hbrush;
  HGDIOBJ hgdiobj2;
  LOGBRUSH br;

  if (hb_parl(11)) {
    hbrush = CreateSolidBrush(static_cast<COLORREF>(RGB(HB_PARNI(10, 1), HB_PARNI(10, 2), HB_PARNI(10, 3))));
    hgdiobj2 = SelectObject(hdc1, hbrush);
  } else {
    br.lbStyle = BS_HOLLOW;
    hbrush = CreateBrushIndirect(&br);
    hgdiobj2 = SelectObject(hdc1, hbrush);
  }

  RoundRect(hdc1, hmg_par_int(3), hmg_par_int(2), hmg_par_int(5), hmg_par_int(4), hmg_par_int(6), hmg_par_int(7));
  SelectObject(hdc1, hgdiobj1);
  SelectObject(hdc1, hgdiobj2);
  DeleteObject(hpen);
  DeleteObject(hbrush);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_ELLIPSEDRAW(HWND, p2, p3, p4, p5, p6, p7, p8) --> NIL
HB_FUNC(HMG_ELLIPSEDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(7), static_cast<COLORREF>(RGB(HB_PARNI(6, 1), HB_PARNI(6, 2), HB_PARNI(6, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);

  HBRUSH hbrush;
  HGDIOBJ hgdiobj2;
  LOGBRUSH br;

  if (hb_parl(9)) {
    hbrush = CreateSolidBrush(static_cast<COLORREF>(RGB(HB_PARNI(8, 1), HB_PARNI(8, 2), HB_PARNI(8, 3))));
    hgdiobj2 = SelectObject(hdc1, hbrush);
  } else {
    br.lbStyle = BS_HOLLOW;
    hbrush = CreateBrushIndirect(&br);
    hgdiobj2 = SelectObject(hdc1, hbrush);
  }

  Ellipse(hdc1, hmg_par_int(3), hmg_par_int(2), hmg_par_int(5), hmg_par_int(4));
  SelectObject(hdc1, hgdiobj1);
  SelectObject(hdc1, hgdiobj2);
  DeleteObject(hpen);
  DeleteObject(hbrush);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_ARCDRAW(HWND, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) --> NIL
HB_FUNC(HMG_ARCDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen = CreatePen(PS_SOLID, hmg_par_int(11),
                        static_cast<COLORREF>(RGB(HB_PARNI(10, 1), HB_PARNI(10, 2), HB_PARNI(10, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);
  Arc(hdc1, hmg_par_int(3), hmg_par_int(2), hmg_par_int(5), hmg_par_int(4), hmg_par_int(7), hmg_par_int(6),
      hmg_par_int(9), hmg_par_int(8));
  SelectObject(hdc1, hgdiobj1);
  DeleteObject(hpen);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_PIEDRAW(HWND, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) --> NIL
HB_FUNC(HMG_PIEDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen = CreatePen(PS_SOLID, hmg_par_int(11),
                        static_cast<COLORREF>(RGB(HB_PARNI(10, 1), HB_PARNI(10, 2), HB_PARNI(10, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);

  HBRUSH hbrush;
  HGDIOBJ hgdiobj2;
  LOGBRUSH br;

  if (hb_parl(13)) {
    hbrush = CreateSolidBrush(static_cast<COLORREF>(RGB(HB_PARNI(12, 1), HB_PARNI(12, 2), HB_PARNI(12, 3))));
    hgdiobj2 = SelectObject(hdc1, hbrush);
  } else {
    br.lbStyle = BS_HOLLOW;
    hbrush = CreateBrushIndirect(&br);
    hgdiobj2 = SelectObject(hdc1, hbrush);
  }

  Pie(hdc1, hmg_par_int(3), hmg_par_int(2), hmg_par_int(5), hmg_par_int(4), hmg_par_int(7), hmg_par_int(6),
      hmg_par_int(9), hmg_par_int(8));
  SelectObject(hdc1, hgdiobj1);
  SelectObject(hdc1, hgdiobj2);
  DeleteObject(hpen);
  DeleteObject(hbrush);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_POLYGONDRAW(HWND, p2, p3, p4, p5, p6, p7) --> NIL
HB_FUNC(HMG_POLYGONDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(5), static_cast<COLORREF>(RGB(HB_PARNI(4, 1), HB_PARNI(4, 2), HB_PARNI(4, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);

  HBRUSH hbrush;
  HGDIOBJ hgdiobj2;
  LOGBRUSH br;

  if (hb_parl(7)) {
    hbrush = CreateSolidBrush(static_cast<COLORREF>(RGB(HB_PARNI(6, 1), HB_PARNI(6, 2), HB_PARNI(6, 3))));
    hgdiobj2 = SelectObject(hdc1, hbrush);
  } else {
    br.lbStyle = BS_HOLLOW;
    hbrush = CreateBrushIndirect(&br);
    hgdiobj2 = SelectObject(hdc1, hbrush);
  }

  POINT apoints[1024];
  auto number = static_cast<int>(hb_parinfa(2, 0));

  for (auto i = 0; i <= number - 1; i++) {
    apoints[i].x = HB_PARNI(2, i + 1);
    apoints[i].y = HB_PARNI(3, i + 1);
  }

  Polygon(hdc1, apoints, number);
  SelectObject(hdc1, hgdiobj1);
  SelectObject(hdc1, hgdiobj2);
  DeleteObject(hpen);
  DeleteObject(hbrush);
  ReleaseDC(hWnd1, hdc1);
}

// HMG_POLYBEZIERDRAW(HWND, p2, p3, p4) --> NIL
HB_FUNC(HMG_POLYBEZIERDRAW)
{
  auto hWnd1 = hmg_par_HWND(1);
  auto hdc1 = GetDC(hWnd1);
  auto hpen =
      CreatePen(PS_SOLID, hmg_par_int(5), static_cast<COLORREF>(RGB(HB_PARNI(4, 1), HB_PARNI(4, 2), HB_PARNI(4, 3))));
  auto hgdiobj1 = SelectObject(hdc1, hpen);

  POINT apoints[1024];
  auto number = static_cast<DWORD>(hb_parinfa(2, 0));

  for (DWORD i = 0; i <= number - 1; i++) {
    apoints[i].x = HB_PARNI(2, i + 1);
    apoints[i].y = HB_PARNI(3, i + 1);
  }

  PolyBezier(hdc1, apoints, number);
  SelectObject(hdc1, hgdiobj1);
  DeleteObject(hpen);
  ReleaseDC(hWnd1, hdc1);
}

void WndDrawBox(HDC hDC, RECT *rct, HPEN hPUpLeft, HPEN hPBotRit)
{
  auto hOldPen = static_cast<HPEN>(SelectObject(hDC, hPUpLeft));

  POINT pt;

  MoveToEx(hDC, rct->left, rct->bottom, &pt);

  LineTo(hDC, rct->left, rct->top);
  LineTo(hDC, rct->right, rct->top);
  SelectObject(hDC, hPBotRit);

  MoveToEx(hDC, rct->left, rct->bottom, &pt);

  LineTo(hDC, rct->right, rct->bottom);
  LineTo(hDC, rct->right, rct->top - 1);

  SelectObject(hDC, hOldPen);
}

void WindowBoxIn(HDC hDC, RECT *pRect)
{
  auto hWhite = CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNHIGHLIGHT));
  auto hGray = CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));

  WndDrawBox(hDC, pRect, hGray, hWhite);

  DeleteObject(hGray);
  DeleteObject(hWhite);
}

void WindowRaised(HDC hDC, RECT *pRect)
{
  auto hGray = CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
  auto hWhite = CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNHIGHLIGHT));

  WndDrawBox(hDC, pRect, hWhite, hGray);

  DeleteObject(hGray);
  DeleteObject(hWhite);
}

// HMG_WNDBOXIN(HDC, p2, p3, p4, p5) --> NIL
HB_FUNC(HMG_WNDBOXIN)
{
  RECT rct;

  rct.top = hb_parni(2);
  rct.left = hb_parni(3);
  rct.bottom = hb_parni(4);
  rct.right = hb_parni(5);

  WindowBoxIn(hmg_par_HDC(1), &rct);
}

// HMG_WNDBOXRAISED(HDC, p2, p3, p4, p5) --> NIL
HB_FUNC(HMG_WNDBOXRAISED)
{
  RECT rct;

  rct.top = hb_parni(2);
  rct.left = hb_parni(3);
  rct.bottom = hb_parni(4);
  rct.right = hb_parni(5);

  WindowRaised(hmg_par_HDC(1), &rct);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(TEXTDRAW, HMG_TEXTDRAW)
HB_FUNC_TRANSLATE(LINEDRAW, HMG_LINEDRAW)
HB_FUNC_TRANSLATE(RECTDRAW, HMG_RECTDRAW)
HB_FUNC_TRANSLATE(ROUNDRECTDRAW, HMG_ROUNDRECTDRAW)
HB_FUNC_TRANSLATE(ELLIPSEDRAW, HMG_ELLIPSEDRAW)
HB_FUNC_TRANSLATE(ARCDRAW, HMG_ARCDRAW)
HB_FUNC_TRANSLATE(PIEDRAW, HMG_PIEDRAW)
HB_FUNC_TRANSLATE(POLYGONDRAW, HMG_POLYGONDRAW)
HB_FUNC_TRANSLATE(POLYBEZIERDRAW, HMG_POLYBEZIERDRAW)
HB_FUNC_TRANSLATE(WNDBOXIN, HMG_WNDBOXIN)
HB_FUNC_TRANSLATE(WNDBOXRAISED, HMG_WNDBOXRAISED)
#endif
