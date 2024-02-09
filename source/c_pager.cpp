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

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"
#include <shlobj.h>
#include <commctrl.h>
#include <hbwinuni.hpp>

#if defined(__MINGW32__) && defined(__MINGW32_VERSION)
#define Pager_ForwardMouse(hwnd, bForward)                                                                             \
  static_cast<void>(SendMessage((hwnd), PGM_FORWARDMOUSE, static_cast<WPARAM>(bForward), 0))
#define Pager_SetBorder(hwnd, iBorder) SendMessage((hwnd), PGM_SETBORDER, 0, static_cast<LPARAM>(iBorder))
#define Pager_GetBorder(hwnd) SendMessage((hwnd), PGM_GETBORDER, 0, 0)
#define Pager_SetPos(hwnd, iPos) SendMessage((hwnd), PGM_SETPOS, 0, static_cast<LPARAM>(iPos))
#define Pager_GetPos(hwnd) SendMessage((hwnd), PGM_GETPOS, 0, 0)
#define Pager_SetButtonSize(hwnd, iSize) SendMessage((hwnd), PGM_SETBUTTONSIZE, 0, static_cast<LPARAM>(iSize))
#define Pager_GetButtonSize(hwnd) SendMessage((hwnd), PGM_GETBUTTONSIZE, 0, 0)
#endif

HB_FUNC(HMG_GETHANDLEREBAR) // GetHandleRebar(hPager)
{
  hmg_ret_HWND(reinterpret_cast<HWND>(GetWindowLongPtr(hmg_par_HWND(1), GWLP_USERDATA)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETHANDLEREBAR, HMG_GETHANDLEREBAR)
#endif

HB_FUNC(HMG_ADDTOPAGER) // AdToPager(hwndPG , hToolBar)
{
  auto hPager = hmg_par_HWND(1);
  SendMessage(hPager, PGM_SETCHILD, 0, reinterpret_cast<LPARAM>(hmg_par_HWND(2)));
  SendMessage(hPager, PGM_RECALCSIZE, 0, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ADDTOPAGER, HMG_ADDTOPAGER)
#endif

HB_FUNC(HMG_SETBKCOLORPAGER) // SetBkColorPager(hwndPG , COLOR[])
{
  SendMessage(hmg_par_HWND(1), PGM_SETBKCOLOR, 0, RGB(hb_parni(2), hb_parni(3), hb_parni(4)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETBKCOLORPAGER, HMG_SETBKCOLORPAGER)
#endif

HB_FUNC(HMG_PAGERCALCSIZE) // PagerCalcSize(lParam , nWidth)
{
  auto lpCalcSize = reinterpret_cast<LPNMPGCALCSIZE>(HB_PARNL(1));

  if (lpCalcSize->dwFlag == PGF_CALCWIDTH)
  {
    lpCalcSize->iWidth = hmg_par_INT(2);
  }

  if (lpCalcSize->dwFlag == PGF_CALCHEIGHT)
  {
    lpCalcSize->iHeight = hmg_par_INT(2);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERCALCSIZE, HMG_PAGERCALCSIZE)
#endif

HB_FUNC(HMG_PAGERSCROLL) // PagerScroll(lParam , nScroll)
{
  auto lpScroll = reinterpret_cast<LPNMPGSCROLL>(HB_PARNL(1));
  lpScroll->iScroll = hb_parnl(2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERSCROLL, HMG_PAGERSCROLL)
#endif

HB_FUNC(HMG_INITPAGER) // InitPager(ParentForm, hRebar, nWidth, nHeight, vertical, autoscroll)
{
  INITCOMMONCONTROLSEX i;
  i.dwSize = sizeof(INITCOMMONCONTROLSEX);
  i.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES | ICC_PAGESCROLLER_CLASS;
  InitCommonControlsEx(&i);

  auto hRebar = hmg_par_HWND(1);
  int nWidth = hmg_par_INT(2);
  int nHeight = hmg_par_INT(3);

  DWORD style = WS_CHILD | WS_VISIBLE;

  if (hb_parl(4))
  {
    style |= PGS_VERT;
  }
  else
  {
    style |= PGS_HORZ;
  }

  if (hb_parl(5))
  {
    style |= PGS_AUTOSCROLL;
  }

  REBARBANDINFO rbBand{};
  rbBand.cbSize = sizeof(REBARBANDINFO);
  rbBand.fMask = RBBIM_TEXT | RBBIM_STYLE | RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_SIZE | RBBS_BREAK | RBBIM_COLORS;
  rbBand.fStyle = RBBS_CHILDEDGE;
  // rbBand.cxMinChild = 0;
  // rbBand.cyMinChild = 0;

  auto hPager = CreateWindowEx(0, WC_PAGESCROLLER, nullptr, style, 0, 0, 0, 0, hRebar, nullptr, GetInstance(), nullptr);

  void *str;

  if (hb_parclen(6) > 0)
  {
    rbBand.lpText = const_cast<TCHAR *>(HB_PARSTR(6, &str, nullptr));
  }

  rbBand.hwndChild = hPager;

  if (hb_parl(4))
  {
    rbBand.cyMinChild = nWidth ? nWidth : 0;
    rbBand.cxMinChild = 0;
    rbBand.cx = nHeight;
  }
  else
  {
    rbBand.cxMinChild = 0;
    rbBand.cyMinChild = nHeight ? nHeight : 0;
    rbBand.cx = nWidth;
  }

  SendMessage(hRebar, RB_INSERTBAND, -1, reinterpret_cast<LPARAM>(&rbBand));
  SetWindowLongPtr(hPager, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(hRebar));
  hmg_ret_HWND(hPager);
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITPAGER, HMG_INITPAGER)
#endif

HB_FUNC(HMG_PAGERFORWARDMOUSE)
{
  Pager_ForwardMouse(hmg_par_HWND(1), hmg_par_BOOL(2));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERFORWARDMOUSE, HMG_PAGERFORWARDMOUSE)
#endif

HB_FUNC(HMG_PAGERGETBUTTONSIZE)
{
  hb_retni(Pager_GetButtonSize(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERGETBUTTONSIZE, HMG_PAGERGETBUTTONSIZE)
#endif

HB_FUNC(HMG_PAGERSETBUTTONSIZE)
{
  Pager_SetButtonSize(hmg_par_HWND(1), hmg_par_INT(2));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERSETBUTTONSIZE, HMG_PAGERSETBUTTONSIZE)
#endif

HB_FUNC(HMG_PAGERGETBORDER)
{
  hb_retni(Pager_GetBorder(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERGETBORDER, HMG_PAGERGETBORDER)
#endif

HB_FUNC(HMG_PAGERSETBORDER)
{
  hb_retni(Pager_SetBorder(hmg_par_HWND(1), hmg_par_INT(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERSETBORDER, HMG_PAGERSETBORDER)
#endif

HB_FUNC(HMG_PAGERGETPOS)
{
  hb_retni(Pager_GetPos(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERGETPOS, HMG_PAGERGETPOS)
#endif

HB_FUNC(HMG_PAGERSETPOS)
{
  hb_retni(Pager_SetPos(hmg_par_HWND(1), hmg_par_INT(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(PAGERSETPOS, HMG_PAGERSETPOS)
#endif
