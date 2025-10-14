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

// Parts of this code is contributed and used here under permission of his
// author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>

#include "mgdefs.hpp"
#include <hbapierr.hpp>
#include <hbapiitm.hpp>

bool hmg_ArrayToPoint(PHB_ITEM aPoint, POINT *pt);
HB_EXPORT PHB_ITEM Rect2Hash(RECT *rc);
BOOL CALLBACK _MonitorEnumProc0(HMONITOR hMonitor, HDC hdcMonitor, LPRECT lprcMonitor, LPARAM dwData);
// BOOL CALLBACK _MonitorEnumProc1(HMONITOR hMonitor, HDC hdcMonitor, LPRECT lprcMonitor, LPARAM
// dwData);
static void ClipOrCenterRectToMonitor(LPRECT prc, HMONITOR hMonitor, UINT flags);

HB_FUNC(HMG_COUNTMONITORS)
{
  hb_retni(GetSystemMetrics(SM_CMONITORS));
}

HB_FUNC(HMG_ISSAMEDISPLAYFORMAT)
{
  hb_retl(GetSystemMetrics(SM_SAMEDISPLAYFORMAT) ? true : false);
}

// The  EnumDisplayMonitors  function  enumerates  display monitors
// (including invisible pseudo-monitors associated with the mirroring drivers)

// BOOL EnumDisplayMonitors(HDC hdc, LPCRECT lprcClip, MONITORENUMPROC lpfnEnum, LPARAM dwData)

HB_FUNC(HMG_ENUMDISPLAYMONITORS)
{
  auto pMonitorEnum = hb_itemArrayNew(0);
  EnumDisplayMonitors(nullptr, nullptr, _MonitorEnumProc0, reinterpret_cast<LPARAM>(pMonitorEnum));
  hb_itemReturnRelease(pMonitorEnum);
}

BOOL CALLBACK _MonitorEnumProc0(HMONITOR hMonitor, HDC hdcMonitor, LPRECT lprcMonitor, LPARAM dwData)
{
  HB_SYMBOL_UNUSED(hdcMonitor);
  auto pMonitor = hb_itemArrayNew(2);
  PHB_ITEM pRect = Rect2Hash(lprcMonitor);
  hb_arraySetNInt(pMonitor, 1, reinterpret_cast<LONG_PTR>(hMonitor));
  hb_itemArrayPut(pMonitor, 2, pRect);
  hb_arrayAddForward(reinterpret_cast<PHB_ITEM>(dwData), pMonitor);
  hb_itemRelease(pMonitor);
  hb_itemRelease(pRect);
  return TRUE;
}

//        BOOL GetMonitorInfo(HMONITOR hMonitor, LPMONITORINFO lpmi)
HB_FUNC(HMG_GETMONITORINFO)
{
  MONITORINFO mi;
  mi.cbSize = sizeof(MONITORINFO);

  if (GetMonitorInfo(reinterpret_cast<HMONITOR>(HB_PARNL(1)), &mi)) {
    auto pMonInfo = hb_itemArrayNew(3);
    PHB_ITEM pMonitor = Rect2Hash(&mi.rcMonitor);
    PHB_ITEM pWork = Rect2Hash(&mi.rcWork);
    hb_itemArrayPut(pMonInfo, 1, pMonitor);
    hb_itemArrayPut(pMonInfo, 2, pWork);
    hb_arraySetNInt(pMonInfo, 3, static_cast<LONG_PTR>(mi.dwFlags));
    hb_itemReturnRelease(pMonInfo);
    hb_itemRelease(pMonitor);
    hb_itemRelease(pWork);
  } else {
    hb_ret();
  }
}

// HMONITOR MonitorFromPoint(POINT pt, DWORD dwFlags)
HB_FUNC(HMG_MONITORFROMPOINT)
{
  POINT pt;

  if (HB_ISARRAY(1)) {
    if (!hmg_ArrayToPoint(hb_param(1, Harbour::Item::ARRAY), &pt)) {
      hb_errRT_BASE_SubstR(EG_ARG, 5000, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    } else {
      hmg_ret_HMONITOR(MonitorFromPoint(pt, hb_parnldef(2, MONITOR_DEFAULTTONULL)));
    }
  } else if (HB_ISNUM(1) && HB_ISNUM(2)) {
    pt.x = hb_parnl(1);
    pt.y = hb_parnl(2);
    hmg_ret_HMONITOR(MonitorFromPoint(pt, hb_parnldef(3, MONITOR_DEFAULTTONULL)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 5000, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// HMONITOR MonitorFromWindow(HWND  hwnd, DWORD dwFlags)
HB_FUNC(HMG_MONITORFROMWINDOW)
{
  auto hwnd = hmg_par_HWND(1);

  if (IsWindow(hwnd)) {
    hmg_ret_HMONITOR(MonitorFromWindow(hwnd, hb_parnldef(2, MONITOR_DEFAULTTONULL)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// Based on
// https://msdn.microsoft.com/ru-ru/library/windows/desktop/dd162826(v=vs.85).aspx
//
// The  most common problem apps have when running on a multimonitor system is
// that  they "clip" or "pin" windows based on the SM_CXSCREEN and SM_CYSCREEN
// system  metrics.  Because of app compatibility reasons these system metrics
// return the size of the primary monitor.
//
// This shows how you use the multi-monitor functions to do the same thing.

#define MONITOR_CENTER 0x0001   // center rect to monitor
#define MONITOR_CLIP 0x0000     // clip rect to monitor
#define MONITOR_WORKAREA 0x0002 // use monitor work area
#define MONITOR_AREA 0x0000     // use monitor entire area

HB_FUNC(HMG_WINDOWTOMONITOR)
{
  auto hwnd = hmg_par_HWND(1);

  if (IsWindow(hwnd)) {
    HMONITOR hMonitor = HB_ISNUM(2) ? reinterpret_cast<HMONITOR>(HB_PARNL(2)) : nullptr;
    UINT flags = 0 | (static_cast<UINT>(hb_parnldef(3, (MONITOR_CENTER | MONITOR_WORKAREA))));
    RECT rc;
    GetWindowRect(hwnd, &rc);
    ClipOrCenterRectToMonitor(&rc, hMonitor, flags);
    SetWindowPos(hwnd, nullptr, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

static void ClipOrCenterRectToMonitor(LPRECT prc, HMONITOR hMonitor, UINT flags)
{
  int w = prc->right - prc->left;
  int h = prc->bottom - prc->top;

  // get the nearest monitor to the passed rect.
  if (hMonitor == nullptr) {
    hMonitor = MonitorFromRect(prc, MONITOR_DEFAULTTONEAREST);
  }

  // get the work area or entire monitor rect.
  MONITORINFO mi;
  mi.cbSize = sizeof(mi);
  GetMonitorInfo(hMonitor, &mi);

  RECT rc = (flags & MONITOR_WORKAREA) ? mi.rcWork : mi.rcMonitor;

  // center or clip the passed rect to the monitor rect
  if (flags & MONITOR_CENTER) {
    prc->left = rc.left + (rc.right - rc.left - w) / 2;
    prc->top = rc.top + (rc.bottom - rc.top - h) / 2;
    prc->right = prc->left + w;
    prc->bottom = prc->top + h;
  } else {
    prc->left = HB_MAX(rc.left, HB_MIN(rc.right - w, prc->left));
    prc->top = HB_MAX(rc.top, HB_MIN(rc.bottom - h, prc->top));
    prc->right = prc->left + w;
    prc->bottom = prc->top + h;
  }
}

HB_EXPORT PHB_ITEM Rect2Hash(RECT *rc)
{
  PHB_ITEM phRect = hb_hashNew(nullptr);
  auto pKey = hb_itemPutCConst(nullptr, "left");
  auto pValue = hb_itemPutNL(nullptr, rc->left);

  hb_hashAddNew(phRect, pKey, pValue);

  hb_itemPutCConst(pKey, "top");
  hb_itemPutNL(pValue, rc->top);
  hb_hashAddNew(phRect, pKey, pValue);

  hb_itemPutCConst(pKey, "right");
  hb_itemPutNL(pValue, rc->right);
  hb_hashAddNew(phRect, pKey, pValue);

  hb_itemPutCConst(pKey, "bottom");
  hb_itemPutNL(pValue, rc->bottom);
  hb_hashAddNew(phRect, pKey, pValue);

  hb_itemRelease(pKey);
  hb_itemRelease(pValue);

  return phRect;
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(COUNTMONITORS, HMG_COUNTMONITORS)
HB_FUNC_TRANSLATE(ISSAMEDISPLAYFORMAT, HMG_ISSAMEDISPLAYFORMAT)
HB_FUNC_TRANSLATE(ENUMDISPLAYMONITORS, HMG_ENUMDISPLAYMONITORS)
HB_FUNC_TRANSLATE(GETMONITORINFO, HMG_GETMONITORINFO)
HB_FUNC_TRANSLATE(MONITORFROMPOINT, HMG_MONITORFROMPOINT)
HB_FUNC_TRANSLATE(MONITORFROMWINDOW, HMG_MONITORFROMWINDOW)
HB_FUNC_TRANSLATE(WINDOWTOMONITOR, HMG_WINDOWTOMONITOR)
#endif
