//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

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

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"
#if defined(__MINGW32__) && (_WIN32_WINNT < 0x0500)
#define _WIN32_WINNT 0x0500
#endif

#include <commctrl.h>
#if defined(_MSC_VER)
#pragma warning(disable : 4201)
#endif
#include <richedit.h>
#include <shellapi.h>

#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

#define WM_TASKBAR WM_USER + 1043

// extern functions
#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif
HINSTANCE GetResources(void);
extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);
extern void hmg_ErrorExit(LPCTSTR lpMessage, DWORD dwError, BOOL bExit);

// local  function
HRGN BitmapToRegion(HBITMAP hBmp, COLORREF cTransparentColor, COLORREF cTolerance);

// global variables
HWND g_hWndMain = nullptr;
HACCEL g_hAccel = nullptr;
// static variables
static HWND hDlgModeless = nullptr;

BOOL SetAcceleratorTable(HWND hWnd, HACCEL hHaccel)
{
  g_hWndMain = hWnd;
  g_hAccel = hHaccel;

  return TRUE;
}

HB_FUNC(HMG_DOMESSAGELOOP)
{
  MSG Msg;
  int status;

  while ((status = GetMessage(&Msg, nullptr, 0, 0)) != 0)
  {
    if (status == -1)
    { // Exception
      // handle the error and possibly exit
      if (hb_parldef(1, true))
      {
        hmg_ErrorExit(TEXT("DOMESSAGELOOP"), 0, TRUE);
      }
    }
    else
    {
      hDlgModeless = GetActiveWindow();

      if (hDlgModeless == nullptr ||
          (!IsDialogMessage(hDlgModeless, &Msg) && !TranslateAccelerator(g_hWndMain, g_hAccel, &Msg)))
      {
        TranslateMessage(&Msg);
        DispatchMessage(&Msg);
      }
    }
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(DOMESSAGELOOP, HMG_DOMESSAGELOOP)
#endif

/*
 * DoEvents is a statement that yields execution of the current
 * thread so that the operating system can process other events.
 * This function cleans out the message loop and executes any other pending
 * business.
 */
HB_FUNC(HMG_DOEVENTS)
{
  MSG Msg;

  while (PeekMessage(static_cast<LPMSG>(&Msg), 0, 0, 0, PM_REMOVE))
  {
    hDlgModeless = GetActiveWindow();

    if (hDlgModeless == nullptr || !IsDialogMessage(hDlgModeless, &Msg))
    {
      TranslateMessage(&Msg);
      DispatchMessage(&Msg);
    }
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(DOEVENTS, HMG_DOEVENTS)
#endif

HB_FUNC(HMG_EXITPROCESS)
{
  ExitProcess(HB_ISNUM(1) ? hb_parni(1) : 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(EXITPROCESS, HMG_EXITPROCESS)
#endif

HB_FUNC(HMG_SHOWWINDOW)
{
  ShowWindow(hmg_par_HWND(1), HB_ISNUM(2) ? hb_parni(2) : SW_SHOW);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SHOWWINDOW, HMG_SHOWWINDOW)
#endif

HB_FUNC(HMG_GETACTIVEWINDOW)
{
  hmg_ret_HWND(GetActiveWindow());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETACTIVEWINDOW, HMG_GETACTIVEWINDOW)
#endif

HB_FUNC(HMG_SETACTIVEWINDOW)
{
  SetActiveWindow(hmg_par_HWND(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETACTIVEWINDOW, HMG_SETACTIVEWINDOW)
#endif

HB_FUNC(HMG_POSTQUITMESSAGE)
{
  PostQuitMessage(hb_parni(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(POSTQUITMESSAGE, HMG_POSTQUITMESSAGE)
#endif

HB_FUNC(HMG_DESTROYWINDOW)
{
  DestroyWindow(hmg_par_HWND(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(DESTROYWINDOW, HMG_DESTROYWINDOW)
#endif

HB_FUNC(HMG_ISWINDOWVISIBLE)
{
  hb_retl(IsWindowVisible(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ISWINDOWVISIBLE, HMG_ISWINDOWVISIBLE)
#endif

HB_FUNC(HMG_ISWINDOWENABLED)
{
  hb_retl(IsWindowEnabled(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ISWINDOWENABLED, HMG_ISWINDOWENABLED)
#endif

HB_FUNC(HMG_ENABLEWINDOW)
{
  EnableWindow(hmg_par_HWND(1), TRUE);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ENABLEWINDOW, HMG_ENABLEWINDOW)
#endif

HB_FUNC(HMG_DISABLEWINDOW)
{
  EnableWindow(hmg_par_HWND(1), FALSE);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(DISABLEWINDOW, HMG_DISABLEWINDOW)
#endif

HB_FUNC(HMG_SETFOREGROUNDWINDOW)
{
  SetForegroundWindow(hmg_par_HWND(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETFOREGROUNDWINDOW, HMG_SETFOREGROUNDWINDOW)
#endif

HB_FUNC(HMG_BRINGWINDOWTOTOP)
{
  BringWindowToTop(hmg_par_HWND(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(BRINGWINDOWTOTOP, HMG_BRINGWINDOWTOTOP)
#endif

HB_FUNC(HMG_GETFOREGROUNDWINDOW)
{
  hmg_ret_HWND(GetForegroundWindow());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETFOREGROUNDWINDOW, HMG_GETFOREGROUNDWINDOW)
#endif

HB_FUNC(HMG_SETWINDOWTEXT)
{
  void *str;
  SetWindowText(hmg_par_HWND(1), HB_PARSTR(2, &str, nullptr));
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETWINDOWTEXT, HMG_SETWINDOWTEXT)
#endif

HB_FUNC(HMG_SETWINDOWTEXTW)
{
  SetWindowTextW(hmg_par_HWND(1), reinterpret_cast<LPCWSTR>(hb_parc(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETWINDOWTEXTW, HMG_SETWINDOWTEXTW)
#endif

HB_FUNC(HMG_SETWINDOWPOS)
{
  hb_retl(static_cast<BOOL>(
      SetWindowPos(hmg_par_HWND(1), hmg_par_HWND(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETWINDOWPOS, HMG_SETWINDOWPOS)
#endif

HB_FUNC(HMG_ANIMATEWINDOW)
{
  hb_retl(static_cast<BOOL>(AnimateWindow(hmg_par_HWND(1), hmg_par_DWORD(2), hmg_par_DWORD(3))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ANIMATEWINDOW, HMG_ANIMATEWINDOW)
#endif

HB_FUNC(HMG_FLASHWINDOWEX)
{
  FLASHWINFO FlashWinInfo;

  FlashWinInfo.cbSize = sizeof(FLASHWINFO);
  FlashWinInfo.hwnd = hmg_par_HWND(1);
  FlashWinInfo.dwFlags = hmg_par_DWORD(2);
  FlashWinInfo.uCount = hmg_par_UINT(3);
  FlashWinInfo.dwTimeout = hmg_par_DWORD(4);

  hb_retl(static_cast<BOOL>(FlashWindowEx(&FlashWinInfo)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FLASHWINDOWEX, HMG_FLASHWINDOWEX)
#endif

HB_FUNC(HMG_SETLAYEREDWINDOWATTRIBUTES)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd))
  {
    HMODULE hDll = GetModuleHandle(TEXT("user32.dll"));

    hb_retl(false);

    if (hDll != nullptr)
    {
      using SetLayeredWindowAttributes_ptr = BOOL(__stdcall *)(HWND, COLORREF, BYTE, DWORD);

      auto fn_SetLayeredWindowAttributes =
          reinterpret_cast<SetLayeredWindowAttributes_ptr>(wapi_GetProcAddress(hDll, "SetLayeredWindowAttributes"));

      if (fn_SetLayeredWindowAttributes != nullptr)
      {
        auto crKey = hmg_par_COLORREF(2);
        auto bAlpha = hmg_par_BYTE(3);
        auto dwFlags = hmg_par_DWORD(4);

        if (!(GetWindowLongPtr(hWnd, GWL_EXSTYLE) & WS_EX_LAYERED))
        {
          SetWindowLongPtr(hWnd, GWL_EXSTYLE, GetWindowLongPtr(hWnd, GWL_EXSTYLE) | WS_EX_LAYERED);
        }

        hb_retl(fn_SetLayeredWindowAttributes(hWnd, crKey, bAlpha, dwFlags) ? true : false);
      }
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETLAYEREDWINDOWATTRIBUTES, HMG_SETLAYEREDWINDOWATTRIBUTES)
#endif

static BOOL CenterIntoParent(HWND hwnd)
{
  // make the window relative to its parent
  auto hwndParent = GetParent(hwnd);

  RECT rect;
  GetWindowRect(hwnd, &rect);
  RECT rectP;
  ;
  GetWindowRect(hwndParent, &rectP);

  int width = rect.right - rect.left;
  int height = rect.bottom - rect.top;

  int x = ((rectP.right - rectP.left) - width) / 2 + rectP.left;
  int y = ((rectP.bottom - rectP.top) - height) / 2 + rectP.top;

  int screenwidth = GetSystemMetrics(SM_CXSCREEN);
  int screenheight = GetSystemMetrics(SM_CYSCREEN);

  // make sure that the child window never moves outside of the screen
  if (x < 0)
  {
    x = 0;
  }
  if (y < 0)
  {
    y = 0;
  }
  if (x + width > screenwidth)
  {
    x = screenwidth - width;
  }
  if (y + height > screenheight)
  {
    y = screenheight - height;
  }

  MoveWindow(hwnd, x, y, width, height, FALSE);

  return TRUE;
}

HB_FUNC(HMG_C_CENTER)
{
  auto hwnd = hmg_par_HWND(1);

  if (hb_parl(2))
  {
    CenterIntoParent(hwnd);
  }
  else
  {
    RECT rect;
    GetWindowRect(hwnd, &rect);
    int w = rect.right - rect.left;
    int h = rect.bottom - rect.top;
    int x = GetSystemMetrics(SM_CXSCREEN);
    SystemParametersInfo(SPI_GETWORKAREA, 1, &rect, 0);
    int y = rect.bottom - rect.top;
    SetWindowPos(hwnd, HWND_TOP, (x - w) / 2, (y - h) / 2, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(C_CENTER, HMG_C_CENTER)
#endif

HB_FUNC(HMG_GETWINDOWTEXT)
{
#ifdef UNICODE
  LPSTR pStr;
#endif
  auto hWnd = hmg_par_HWND(1);
  int iLen = GetWindowTextLength(hWnd);
  auto szText = static_cast<TCHAR *>(hb_xgrab((iLen + 1) * sizeof(TCHAR)));
#ifndef UNICODE
  iLen = GetWindowText(hWnd, szText, iLen + 1);
  hb_retclen(szText, iLen);
#else
  GetWindowText(hWnd, szText, iLen + 1);
  pStr = WideToAnsi(szText);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
  hb_xfree(szText);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETWINDOWTEXT, HMG_GETWINDOWTEXT)
#endif

HB_FUNC(HMG_SENDMESSAGE)
{
  auto hwnd = hmg_par_HWND(1);

  if (IsWindow(hwnd))
  {
    HB_RETNL(static_cast<LONG_PTR>(SendMessage(hwnd, hmg_par_UINT(2), hb_parnl(3), hmg_par_LPARAM(4))));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SENDMESSAGE, HMG_SENDMESSAGE)
#endif

HB_FUNC(HMG_SENDMESSAGESTRING)
{
  HB_RETNL(static_cast<LONG_PTR>(
      SendMessage(hmg_par_HWND(1), hmg_par_UINT(2), hb_parnl(3), reinterpret_cast<LPARAM>(hb_parc(4)))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SENDMESSAGESTRING, HMG_SENDMESSAGESTRING)
#endif

HB_FUNC(HMG_GETNOTIFYCODE)
{
  LPARAM lParam = HB_PARNL(1);
  auto nmhdr = reinterpret_cast<NMHDR *>(lParam);
  hb_retni(nmhdr->code);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETNOTIFYCODE, HMG_GETNOTIFYCODE)
#endif

HB_FUNC(HMG_GETNOTIFYLINK)
{
  LPARAM lParam = HB_PARNL(1);
  auto pENLink = reinterpret_cast<ENLINK *>(lParam);
  hb_retnl(pENLink->msg);
  HB_STORNL(static_cast<LONG_PTR>(pENLink->wParam), 2);
  HB_STORNL(static_cast<LONG_PTR>(pENLink->lParam), 3);
  hb_stornl(pENLink->chrg.cpMin, 4);
  hb_stornl(pENLink->chrg.cpMax, 5);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETNOTIFYLINK, HMG_GETNOTIFYLINK)
#endif

// JP 107a
HB_FUNC(HMG_GETNOTIFYID)
{
  LPARAM lParam = HB_PARNL(1);
  auto nmhdr = reinterpret_cast<NMHDR *>(lParam);
  HB_RETNL(static_cast<LONG_PTR>(nmhdr->idFrom)); // TODO: hmg_ret_HANDLE ?
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETNOTIFYID, HMG_GETNOTIFYID)
#endif

HB_FUNC(HMG_GETHWNDFROM)
{
  LPARAM lParam = HB_PARNL(1);
  auto nmhdr = reinterpret_cast<NMHDR *>(lParam);
  hmg_ret_HWND(nmhdr->hwndFrom);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETHWNDFROM, HMG_GETHWNDFROM)
#endif

HB_FUNC(HMG_GETDRAWITEMHANDLE)
{
  hmg_ret_HWND((reinterpret_cast<DRAWITEMSTRUCT FAR *>(HB_PARNL(1)))->hwndItem);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETDRAWITEMHANDLE, HMG_GETDRAWITEMHANDLE)
#endif

HB_FUNC(HMG_GETFOCUS)
{
  hmg_ret_HWND(GetFocus());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETFOCUS, HMG_GETFOCUS)
#endif

HB_FUNC(HMG_GETGRIDCOLUMN)
{
  hb_retnl(static_cast<LPARAM>((reinterpret_cast<NM_LISTVIEW *>(HB_PARNL(1)))->iSubItem));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGRIDCOLUMN, HMG_GETGRIDCOLUMN)
#endif

HB_FUNC(HMG_GETGRIDVKEY)
{
  hb_retnl(static_cast<LPARAM>((reinterpret_cast<LV_KEYDOWN *>(HB_PARNL(1)))->wVKey));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGRIDVKEY, HMG_GETGRIDVKEY)
#endif

HB_FUNC(HMG_MOVEWINDOW)
{
  hb_retl(MoveWindow(hmg_par_HWND(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5),
                     (HB_ISNIL(6) ? TRUE : hb_parl(6))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(MOVEWINDOW, HMG_MOVEWINDOW)
#endif

HB_FUNC(HMG_GETSYSTEMMETRICS) // TODO: deprecated (using waGetSystemMetrics from WinApi library)
{
  hb_retni(GetSystemMetrics(hb_parni(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETSYSTEMMETRICS, WAGETSYSTEMMETRICS)
#endif

HB_FUNC(HMG_GETWINDOWRECT)
{
  RECT rect;
  GetWindowRect(hmg_par_HWND(1), &rect);

  if (HB_ISNUM(2))
  {
    switch (hb_parni(2))
    {
    case 1:
      hb_retni(rect.top);
      break;
    case 2:
      hb_retni(rect.left);
      break;
    case 3:
      hb_retni(rect.right - rect.left);
      break;
    case 4:
      hb_retni(rect.bottom - rect.top);
    }
  }
  else if (HB_ISARRAY(2))
  {
    HB_STORVNL(rect.left, 2, 1);
    HB_STORVNL(rect.top, 2, 2);
    HB_STORVNL(rect.right, 2, 3);
    HB_STORVNL(rect.bottom, 2, 4);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETWINDOWRECT, HMG_GETWINDOWRECT)
#endif

HB_FUNC(HMG_GETCLIENTRECT)
{
  RECT rect;
  hb_retl(GetClientRect(hmg_par_HWND(1), &rect));
  HB_STORVNL(rect.left, 2, 1);
  HB_STORVNL(rect.top, 2, 2);
  HB_STORVNL(rect.right, 2, 3);
  HB_STORVNL(rect.bottom, 2, 4);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETCLIENTRECT, HMG_GETCLIENTRECT)
#endif

HB_FUNC(HMG_GETDESKTOPAREA)
{
  RECT rect;
  SystemParametersInfo(SPI_GETWORKAREA, 1, &rect, 0);
  hb_reta(4);
  HB_STORNI(rect.left, -1, 1);
  HB_STORNI(rect.top, -1, 2);
  HB_STORNI(rect.right, -1, 3);
  HB_STORNI(rect.bottom, -1, 4);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETDESKTOPAREA, HMG_GETDESKTOPAREA)
#endif

HB_FUNC(HMG_GETTASKBARHEIGHT)
{
  RECT rect;
  GetWindowRect(FindWindow(TEXT("Shell_TrayWnd"), nullptr), &rect);
  hb_retni(rect.bottom - rect.top);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETTASKBARHEIGHT, HMG_GETTASKBARHEIGHT)
#endif

static BOOL ShowNotifyIcon(HWND hWnd, BOOL bAdd, HICON hIcon, const TCHAR *szText)
{
  NOTIFYICONDATA nid{};
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hIcon = hIcon;
  nid.hWnd = hWnd;
  nid.uID = 0;
  nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
  nid.uCallbackMessage = WM_TASKBAR;
  lstrcpy(nid.szTip, szText);
  return Shell_NotifyIcon(bAdd ? NIM_ADD : NIM_DELETE, &nid);
}

HB_FUNC(HMG_SHOWNOTIFYICON)
{
  void *str;
  hb_retl(static_cast<BOOL>(
      ShowNotifyIcon(hmg_par_HWND(1), hmg_par_BOOL(2), hmg_par_HICON(3), HB_PARSTR(4, &str, nullptr))));
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SHOWNOTIFYICON, HMG_SHOWNOTIFYICON)
#endif

HB_FUNC(HMG_GETCURSORPOS)
{
  POINT pt;
  GetCursorPos(&pt);
  if (hb_pcount() == 1)
  {
    ScreenToClient(hmg_par_HWND(1), &pt);
  }

  hb_reta(2);
  if (hb_pcount() == 0)
  {
    HB_STORNI(pt.y, -1, 1);
    HB_STORNI(pt.x, -1, 2);
  }
  else
  {
    HB_STORNI(pt.x, -1, 1);
    HB_STORNI(pt.y, -1, 2);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETCURSORPOS, HMG_GETCURSORPOS)
#endif

HB_FUNC(HMG_SCREENTOCLIENT)
{
  auto x = hmg_par_LONG(2);
  auto y = hmg_par_LONG(3);

  POINT pt;
  pt.x = x;
  pt.y = y;
  ScreenToClient(hmg_par_HWND(1), &pt);

  hb_reta(2);
  HB_STORNI(pt.x, -1, 1);
  HB_STORNI(pt.y, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SCREENTOCLIENT, HMG_SCREENTOCLIENT)
#endif

HB_FUNC(HMG_CLIENTTOSCREEN)
{
  auto x = hmg_par_LONG(2);
  auto y = hmg_par_LONG(3);

  POINT pt;
  pt.x = x;
  pt.y = y;
  hb_retl(ClientToScreen(hmg_par_HWND(1), &pt));

  if (HB_ISBYREF(2))
  {
    hb_storni(pt.x, 2);
  }
  if (HB_ISBYREF(3))
  {
    hb_storni(pt.y, 3);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CLIENTTOSCREEN, HMG_CLIENTTOSCREEN)
#endif

HB_FUNC(HMG_LOADTRAYICON)
{
  HINSTANCE hInstance = hmg_par_HINSTANCE(1); // handle to application instance
  void *str = nullptr;
  LPCTSTR lpIconName =
      HB_ISCHAR(2) ? HB_PARSTR(2, &str, nullptr) : MAKEINTRESOURCE(hb_parni(2)); // name string or resource identifier
  int cxDesired = HB_ISNUM(3) ? hb_parni(3) : GetSystemMetrics(SM_CXSMICON);
  int cyDesired = HB_ISNUM(4) ? hb_parni(4) : GetSystemMetrics(SM_CYSMICON);
  auto hIcon = static_cast<HICON>(LoadImage(hInstance, lpIconName, IMAGE_ICON, cxDesired, cyDesired, LR_DEFAULTCOLOR));
  if (hIcon == nullptr)
  {
    hIcon = static_cast<HICON>(
        LoadImage(hInstance, lpIconName, IMAGE_ICON, cxDesired, cyDesired, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
  }
  RegisterResource(hIcon, "ICON");
  hmg_ret_HICON(hIcon);
  if (HB_ISCHAR(2))
  {
    hb_strfree(str);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(LOADTRAYICON, HMG_LOADTRAYICON)
#endif

static BOOL ChangeNotifyIcon(HWND hWnd, HICON hIcon, const TCHAR *szText)
{
  NOTIFYICONDATA nid{};
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hIcon = hIcon;
  nid.hWnd = hWnd;
  nid.uID = 0;
  nid.uFlags = NIF_ICON | NIF_TIP;
  lstrcpy(nid.szTip, szText);
  return Shell_NotifyIcon(NIM_MODIFY, &nid);
}

HB_FUNC(HMG_CHANGENOTIFYICON)
{
  void *str;
  hb_retl(static_cast<BOOL>(ChangeNotifyIcon(hmg_par_HWND(1), hmg_par_HICON(2), HB_PARSTR(3, &str, nullptr))));
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CHANGENOTIFYICON, HMG_CHANGENOTIFYICON)
#endif

HB_FUNC(HMG_GETITEMPOS)
{
  HB_RETNL(static_cast<LONG_PTR>((reinterpret_cast<NMMOUSE FAR *>(HB_PARNL(1)))->dwItemSpec)); // TODO: hmg_ret_HANDLE ?
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETITEMPOS, HMG_GETITEMPOS)
#endif

HB_FUNC(HMG_SETSCROLLRANGE)
{
  hb_retl(SetScrollRange(hmg_par_HWND(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parl(5)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETSCROLLRANGE, HMG_SETSCROLLRANGE)
#endif

HB_FUNC(HMG_GETSCROLLPOS)
{
  hb_retni(GetScrollPos(hmg_par_HWND(1), hb_parni(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETSCROLLPOS, HMG_GETSCROLLPOS)
#endif

HB_FUNC(HMG_GETWINDOWSTATE)
{
  WINDOWPLACEMENT wp;
  wp.length = sizeof(WINDOWPLACEMENT);
  GetWindowPlacement(hmg_par_HWND(1), &wp);
  hb_retni(wp.showCmd);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETWINDOWSTATE, HMG_GETWINDOWSTATE)
#endif

HB_FUNC(HMG_GETPARENT)
{
  hmg_ret_HWND(GetParent(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETPARENT, HMG_GETPARENT)
#endif

HB_FUNC(HMG_GETDESKTOPWINDOW)
{
  hmg_ret_HWND(GetDesktopWindow());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETDESKTOPWINDOW, HMG_GETDESKTOPWINDOW)
#endif

static BOOL CALLBACK EnumWindowsProc(HWND hWnd, LPARAM pArray)
{
  auto pHWnd = hb_itemPutNInt(nullptr, reinterpret_cast<LONG_PTR>(hWnd));
  hb_arrayAddForward(reinterpret_cast<PHB_ITEM>(pArray), pHWnd);
  hb_itemRelease(pHWnd);
  return TRUE;
}

HB_FUNC(HMG_ENUMWINDOWS)
{
  auto pArray = hb_itemArrayNew(0);
  EnumWindows(static_cast<WNDENUMPROC>(EnumWindowsProc), reinterpret_cast<LPARAM>(pArray));
  hb_itemReturnRelease(pArray);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ENUMWINDOWS, HMG_ENUMWINDOWS)
#endif

static BOOL CALLBACK EnumChildProc(HWND hWnd, LPARAM lParam)
{
  auto pCodeBlock = reinterpret_cast<PHB_ITEM>(lParam);
  auto pHWnd = hb_itemPutNInt(nullptr, reinterpret_cast<LONG_PTR>(hWnd));

  if (pCodeBlock)
  {
    hb_evalBlock1(pCodeBlock, pHWnd);
  }

  hb_itemRelease(pHWnd);

  return hmg_par_BOOL(-1);
}

HB_FUNC(HMG_C_ENUMCHILDWINDOWS)
{
  auto hWnd = hmg_par_HWND(1);
  auto pCodeBlock = hb_param(2, Harbour::Item::BLOCK);

  if (IsWindow(hWnd) && pCodeBlock)
  {
    hb_retl(EnumChildWindows(hWnd, EnumChildProc, reinterpret_cast<LPARAM>(pCodeBlock)) ? true : false);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(C_ENUMCHILDWINDOWS, HMG_C_ENUMCHILDWINDOWS)
#endif

HB_FUNC(HMG_REDRAWWINDOWCONTROLRECT)
{
  RECT r;
  r.top = hb_parni(2);
  r.left = hb_parni(3);
  r.bottom = hb_parni(4);
  r.right = hb_parni(5);
  RedrawWindow(hmg_par_HWND(1), &r, nullptr,
               RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(REDRAWWINDOWCONTROLRECT, HMG_REDRAWWINDOWCONTROLRECT)
#endif

HB_FUNC(HMG_ADDSPLITBOXITEM)
{
  REBARBANDINFO rbBand;
  RECT rc;
  int style = RBBS_CHILDEDGE | RBBS_GRIPPERALWAYS | RBBS_USECHEVRON;

#ifndef UNICODE
  LPSTR lpText = const_cast<LPSTR>(hb_parc(5));
#else
  LPWSTR lpText = AnsiToWide(const_cast<char *>(hb_parc(5)));
#endif

  if (hb_parl(4))
  {
    style |= RBBS_BREAK;
  }

  GetWindowRect(hmg_par_HWND(1), &rc);

  rbBand.cbSize = sizeof(REBARBANDINFO);
  rbBand.fMask = RBBIM_TEXT | RBBIM_STYLE | RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_SIZE;
  rbBand.fStyle = style;
  rbBand.hbmBack = 0;

  rbBand.lpText = lpText;
  rbBand.hwndChild = hmg_par_HWND(1);

  if (hb_parni(9))
  {
    rbBand.fMask = rbBand.fMask | RBBIM_IDEALSIZE;
  }

  if (!hb_parl(8))
  {
    // Not Horizontal
    rbBand.cxMinChild = hb_parni(6) ? hb_parni(6) : 0;
    rbBand.cyMinChild = hb_parni(7) ? hb_parni(7) : rc.bottom - rc.top;
    rbBand.cx = hb_parni(3);
    if (hb_parni(9))
    {
      rbBand.cxIdeal = hb_parni(6) ? hb_parni(6) : 0;
      rbBand.cxMinChild = hb_parni(9);
    }
    else
    {
      rbBand.cxMinChild = hb_parni(6) ? hb_parni(6) : 0;
    }
  }
  else
  {
    // Horizontal
    if (hb_parni(6) == 0 && hb_parni(7) == 0)
    {
      // Not ToolBar
      rbBand.cxMinChild = 0;
      rbBand.cyMinChild = rc.right - rc.left;
      rbBand.cx = rc.bottom - rc.top;
    }
    else
    {
      // ToolBar
      rbBand.cyMinChild = hb_parni(6) ? hb_parni(6) : 0;
      rbBand.cx = hb_parni(7) ? hb_parni(7) : rc.bottom - rc.top;
      if (hb_parni(9))
      {
        rbBand.cxIdeal = hb_parni(7) ? hb_parni(7) : rc.bottom - rc.top;
        rbBand.cxMinChild = hb_parni(9);
      }
      else
      {
        rbBand.cxMinChild = hb_parni(7) ? hb_parni(7) : rc.bottom - rc.top;
      }
    }
  }

  SendMessage(hmg_par_HWND(2), RB_INSERTBAND, -1, reinterpret_cast<LPARAM>(&rbBand));

#ifdef UNICODE
  hb_xfree(lpText);
#endif
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ADDSPLITBOXITEM, HMG_ADDSPLITBOXITEM)
#endif

HB_FUNC(HMG_C_SETWINDOWRGN)
{
  HRGN hRgn = nullptr;
  HBITMAP hbmp;

  if (hb_parni(6) == 0)
  {
    SetWindowRgn(GetActiveWindow(), nullptr, TRUE);
  }
  else
  {
    switch (hb_parni(6))
    {
    case 1:
      hRgn = CreateRectRgn(hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5));
      break;
    case 2:
      hRgn = CreateEllipticRgn(hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5));
      break;
    case 3:
      hRgn = CreateRoundRectRgn(0, 0, hb_parni(4), hb_parni(5), hb_parni(2), hb_parni(3));
      break;
    case 4:
      hbmp = static_cast<HBITMAP>(
          LoadImage(GetResources(), const_cast<TCHAR *>(hb_parc(2)), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));
      if (hbmp == nullptr)
      {
        hbmp = static_cast<HBITMAP>(LoadImage(nullptr, const_cast<TCHAR *>(hb_parc(2)), IMAGE_BITMAP, 0, 0,
                                              LR_LOADFROMFILE | LR_CREATEDIBSECTION));
      }

      hRgn = BitmapToRegion(hbmp, static_cast<COLORREF>(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3))), 0x101010);
      DeleteObject(hbmp);
      break;
    default:
      break;
    }

    SetWindowRgn(hmg_par_HWND(1), hRgn, TRUE);

    RegisterResource(hRgn, "REGION");
    hmg_ret_HRGN(hRgn);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(C_SETWINDOWRGN, HMG_C_SETWINDOWRGN)
#endif

HB_FUNC(HMG_C_SETPOLYWINDOWRGN)
{
  POINT lppt[512];
  int fnPolyFillMode;
  auto cPoints = static_cast<int>(hb_parinfa(2, 0));

  if (hb_parni(4) == 1)
  {
    fnPolyFillMode = WINDING;
  }
  else
  {
    fnPolyFillMode = ALTERNATE;
  }

  for (auto i = 0; i <= cPoints - 1; i++)
  {
    lppt[i].x = HB_PARNI(2, i + 1);
    lppt[i].y = HB_PARNI(3, i + 1);
  }

  auto hRgn = CreatePolygonRgn(lppt, cPoints, fnPolyFillMode);

  SetWindowRgn(GetActiveWindow(), hRgn, TRUE);

  RegisterResource(hRgn, "REGION");
  hmg_ret_HRGN(hRgn);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(C_SETPOLYWINDOWRGN, HMG_C_SETPOLYWINDOWRGN)
#endif

HB_FUNC(HMG_GETHELPDATA)
{
  hmg_ret_HANDLE((reinterpret_cast<HELPINFO FAR *>(HB_PARNL(1)))->hItemHandle);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETHELPDATA, HMG_GETHELPDATA)
#endif

HB_FUNC(HMG_GETMSKTEXTMESSAGE)
{
  HB_RETNL(static_cast<LONG_PTR>((reinterpret_cast<MSGFILTER FAR *>(HB_PARNL(1)))->msg)); // TODO: hmg_ret_HANDLE ?
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETMSKTEXTMESSAGE, HMG_GETMSKTEXTMESSAGE)
#endif

HB_FUNC(HMG_GETMSKTEXTWPARAM)
{
  HB_RETNL(static_cast<LONG_PTR>((reinterpret_cast<MSGFILTER FAR *>(HB_PARNL(1)))->wParam)); // TODO: hmg_ret_HANDLE ?
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETMSKTEXTWPARAM, HMG_GETMSKTEXTWPARAM)
#endif

HB_FUNC(HMG_GETMSKTEXTLPARAM)
{
  HB_RETNL(static_cast<LONG_PTR>((reinterpret_cast<MSGFILTER FAR *>(HB_PARNL(1)))->lParam)); // TODO: hmg_ret_HANDLE ?
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETMSKTEXTLPARAM, HMG_GETMSKTEXTLPARAM)
#endif

HB_FUNC(HMG_GETWINDOW)
{
  hmg_ret_HWND(GetWindow(hmg_par_HWND(1), hb_parni(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETWINDOW, HMG_GETWINDOW)
#endif

HB_FUNC(HMG_GETGRIDOLDSTATE)
{
  LPARAM lParam = HB_PARNL(1);
  auto NMLV = reinterpret_cast<NM_LISTVIEW *>(lParam);
  hb_retni(NMLV->uOldState);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGRIDOLDSTATE, HMG_GETGRIDOLDSTATE)
#endif

HB_FUNC(HMG_GETGRIDNEWSTATE)
{
  LPARAM lParam = HB_PARNL(1);
  auto NMLV = reinterpret_cast<NM_LISTVIEW *>(lParam);
  hb_retni(NMLV->uNewState);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGRIDNEWSTATE, HMG_GETGRIDNEWSTATE)
#endif

HB_FUNC(HMG_GETGRIDDISPINFOINDEX)
{
  LPARAM lParam = HB_PARNL(1);
  auto pDispInfo = reinterpret_cast<LV_DISPINFO *>(lParam);

  int iItem = pDispInfo->item.iItem;
  int iSubItem = pDispInfo->item.iSubItem;

  hb_reta(2);
  HB_STORNI(iItem + 1, -1, 1);
  HB_STORNI(iSubItem + 1, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGRIDDISPINFOINDEX, HMG_GETGRIDDISPINFOINDEX)
#endif

HB_FUNC(HMG_SETGRIDQUERYDATA)
{
  LPARAM lParam = HB_PARNL(1);
  auto pDispInfo = reinterpret_cast<LV_DISPINFO *>(lParam);

  // Copy the text to the LV_ITEM structure
  // Maximum number of characters is in pDispInfo->Item.cchTextMax
#ifdef UNICODE
  LPWSTR lpText = AnsiToWide(const_cast<char *>(hb_parc(2)));
  lstrcpyn(pDispInfo->item.pszText, lpText, pDispInfo->item.cchTextMax);
  hb_xfree(lpText);
#else
  lstrcpyn(pDispInfo->item.pszText, const_cast<char *>(hb_parc(2)), pDispInfo->item.cchTextMax);
#endif
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETGRIDQUERYDATA, HMG_SETGRIDQUERYDATA)
#endif

HB_FUNC(HMG_SETGRIDQUERYIMAGE)
{
  LPARAM lParam = HB_PARNL(1);
  auto pDispInfo = reinterpret_cast<LV_DISPINFO *>(lParam);
  pDispInfo->item.iImage = hb_parni(2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETGRIDQUERYIMAGE, HMG_SETGRIDQUERYIMAGE)
#endif

HB_FUNC(HMG_FINDWINDOWEX)
{
  void *str1 = nullptr;
  void *str2 = nullptr;
  hmg_ret_HWND(
      FindWindowEx(hmg_par_HWND(1), hmg_par_HWND(2), HB_PARSTR(3, &str1, nullptr), HB_PARSTR(4, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDWINDOWEX, HMG_FINDWINDOWEX)
#endif

HB_FUNC(HMG_GETDS)
{
  LPARAM lParam = HB_PARNL(1);
  auto lplvcd = reinterpret_cast<LPNMLVCUSTOMDRAW>(lParam);

  if (lplvcd->nmcd.dwDrawStage == CDDS_PREPAINT)
  {
    hb_retni(CDRF_NOTIFYITEMDRAW);
  }
  else if (lplvcd->nmcd.dwDrawStage == CDDS_ITEMPREPAINT)
  {
    if (hb_pcount() > 1)
    {
      if (ListView_GetNextItem(hmg_par_HWND(2), -1, LVNI_ALL | LVNI_SELECTED) == hb_parni(3))
      {
        ListView_SetItemState(hmg_par_HWND(2), hb_parni(3), 0, LVIS_SELECTED);
      }
    }
    hb_retni(CDRF_NOTIFYSUBITEMDRAW);
  }
  else if (lplvcd->nmcd.dwDrawStage == (CDDS_SUBITEM | CDDS_ITEMPREPAINT))
  {
    hb_retni(-1);
  }
  else
  {
    hb_retni(CDRF_DODEFAULT);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETDS, HMG_GETDS)
#endif

HB_FUNC(HMG_GETRC) // Get ListView CustomDraw Row and Column
{
  LPARAM lParam = HB_PARNL(1);
  auto lplvcd = reinterpret_cast<LPNMLVCUSTOMDRAW>(lParam);
  hb_reta(2);
  HB_STORVNL(lplvcd->nmcd.dwItemSpec + 1, -1, 1);
  HB_STORNI(lplvcd->iSubItem + 1, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETRC, HMG_GETRC)
#endif

HB_FUNC(HMG_SETBCFC) // Set Dynamic BackColor and ForeColor
{
  LPARAM lParam = HB_PARNL(1);
  auto lplvcd = reinterpret_cast<LPNMLVCUSTOMDRAW>(lParam);
  lplvcd->clrTextBk = hb_parni(2);
  lplvcd->clrText = hb_parni(3);
  hb_retni(CDRF_NEWFONT);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETBCFC, HMG_SETBCFC)
#endif

HB_FUNC(HMG_SETBRCCD) // Set Default BackColor and ForeColor
{
  LPARAM lParam = HB_PARNL(1);
  auto lplvcd = reinterpret_cast<LPNMLVCUSTOMDRAW>(lParam);
  lplvcd->clrText = RGB(0, 0, 0);
  lplvcd->clrTextBk = RGB(255, 255, 255);
  hb_retni(CDRF_NEWFONT);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETBRCCD, HMG_SETBRCCD)
#endif

HB_FUNC(HMG_GETTABBEDCONTROLBRUSH)
{
  auto hDC = hmg_par_HDC(1);
  SetBkMode(hDC, TRANSPARENT);
  RECT rc;
  GetWindowRect(hmg_par_HWND(2), &rc);
  MapWindowPoints(nullptr, hmg_par_HWND(3), reinterpret_cast<LPPOINT>(&rc), 2);
  SetBrushOrgEx(hDC, -rc.left, -rc.top, nullptr);
  auto hBrush = hmg_par_HBRUSH(4);
  hmg_ret_HBRUSH(hBrush);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETTABBEDCONTROLBRUSH, HMG_GETTABBEDCONTROLBRUSH)
#endif

HB_FUNC(HMG_GETTABBRUSH)
{
  auto hWnd = hmg_par_HWND(1);
  RECT rc;
  GetWindowRect(hWnd, &rc);
  auto hDC = GetDC(hWnd);
  auto hDCMem = CreateCompatibleDC(hDC);
  auto hBmp = CreateCompatibleBitmap(hDC, rc.right - rc.left, rc.bottom - rc.top);
  auto hOldBmp = static_cast<HBITMAP>(SelectObject(hDCMem, hBmp));
  SendMessage(hWnd, WM_PRINTCLIENT, reinterpret_cast<WPARAM>(hDCMem), PRF_ERASEBKGND | PRF_CLIENT | PRF_NONCLIENT);
  auto hBrush = CreatePatternBrush(hBmp);
  hmg_ret_HBRUSH(hBrush);
  SelectObject(hDCMem, hOldBmp);
  DeleteObject(hBmp);
  DeleteDC(hDCMem);
  ReleaseDC(hWnd, hDC);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETTABBRUSH, HMG_GETTABBRUSH)
#endif

HB_FUNC(HMG_INITMINMAXINFO) // (hWnd) --> aMinMaxInfo
{
  long x, y, mx, my;

  if (GetWindowLong(hmg_par_HWND(1), GWL_STYLE) & WS_SIZEBOX)
  {
    x = -GetSystemMetrics(SM_CXFRAME);
    y = -GetSystemMetrics(SM_CYFRAME);
  }
  else
  {
    x = -GetSystemMetrics(SM_CXBORDER);
    y = -GetSystemMetrics(SM_CYBORDER);
  }

  mx = GetSystemMetrics(SM_CXSCREEN) - 2 * x;
  my = GetSystemMetrics(SM_CYSCREEN) - 2 * y;

  hb_reta(8);
  HB_STORVNL(mx, -1, 1);
  HB_STORVNL(my, -1, 2);
  HB_STORVNL(x, -1, 3);
  HB_STORVNL(y, -1, 4);
  HB_STORVNL(0, -1, 5);
  HB_STORVNL(0, -1, 6);
  HB_STORVNL(mx, -1, 7);
  HB_STORVNL(my, -1, 8);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITMINMAXINFO, HMG_INITMINMAXINFO)
#endif

HB_FUNC(HMG_SETMINMAXINFO) // (pMinMaxInfo, aMinMaxInfo) --> 0
{
  auto pMinMaxInfo = reinterpret_cast<MINMAXINFO *>(HB_PARNL(1));
  pMinMaxInfo->ptMaxSize.x = HB_PARNI(2, 1);
  pMinMaxInfo->ptMaxSize.y = HB_PARNI(2, 2);
  pMinMaxInfo->ptMaxPosition.x = HB_PARNI(2, 3);
  pMinMaxInfo->ptMaxPosition.y = HB_PARNI(2, 4);
  pMinMaxInfo->ptMinTrackSize.x = HB_PARNI(2, 5);
  pMinMaxInfo->ptMinTrackSize.y = HB_PARNI(2, 6);
  pMinMaxInfo->ptMaxTrackSize.x = HB_PARNI(2, 7);
  pMinMaxInfo->ptMaxTrackSize.y = HB_PARNI(2, 8);
  hb_retni(0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETMINMAXINFO, HMG_SETMINMAXINFO)
#endif

HB_FUNC(HMG_LOCKWINDOWUPDATE)
{
  hb_retl(LockWindowUpdate(hmg_par_HWND(1)) ? true : false);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(LOCKWINDOWUPDATE, HMG_LOCKWINDOWUPDATE)
#endif

HB_FUNC(HMG_ISWINDOWHANDLE)
{
  hb_retl(IsWindow(hmg_par_HWND(1)) ? true : false);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ISWINDOWHANDLE, HMG_ISWINDOWHANDLE)
#endif

HB_FUNC(HMG_ISICONIC)
{
  hb_retl(IsIconic(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ISICONIC, HMG_ISICONIC)
#endif

HB_FUNC(HMG_ISZOOMED)
{
  hb_retl(IsZoomed(hmg_par_HWND(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(ISZOOMED, HMG_ISZOOMED)
#endif

HB_FUNC(HMG_GETWINDOWBRUSH)
{
  HB_RETNL(static_cast<LONG_PTR>(GetClassLongPtr(hmg_par_HWND(1), GCLP_HBRBACKGROUND)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETWINDOWBRUSH, HMG_GETWINDOWBRUSH)
#endif

HB_FUNC(HMG_SETWINDOWBRUSH)
{
  HB_RETNL(
      static_cast<LONG_PTR>(SetClassLongPtr(hmg_par_HWND(1), GCLP_HBRBACKGROUND, static_cast<LONG_PTR>(HB_PARNL(2)))));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETWINDOWBRUSH, HMG_SETWINDOWBRUSH)
#endif

HB_FUNC(HMG_CREATEHATCHBRUSH)
{
  hmg_ret_HBRUSH(CreateHatchBrush(hb_parni(1), hmg_par_COLORREF(2)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CREATEHATCHBRUSH, HMG_CREATEHATCHBRUSH)
#endif

/* Modified by P.Ch. 16.10. */
HB_FUNC(HMG_CREATEPATTERNBRUSH)
{
  void *str = nullptr;
  LPCTSTR lpImageName =
      HB_ISCHAR(1) ? HB_PARSTR(1, &str, nullptr) : (HB_ISNUM(1) ? MAKEINTRESOURCE(hb_parni(1)) : nullptr);
  auto hImage = static_cast<HBITMAP>(
      LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
  if (hImage == nullptr && HB_ISCHAR(1))
  {
    hImage = static_cast<HBITMAP>(
        LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
  }
  if (hImage == nullptr)
  {
    hImage = HMG_LoadImage(hb_parc(1), nullptr);
  }
  hmg_ret_HBRUSH((hImage != nullptr) ? CreatePatternBrush(hImage) : nullptr);
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CREATEPATTERNBRUSH, HMG_CREATEPATTERNBRUSH)
#endif

/*
   BitmapToRegion: Create a region from the "non-transparent" pixels of a bitmap
   Author        : Jean-Edouard Lachand-Robert
   (http://www.geocities.com/Paris/LeftBank/1160/resume.htm), June 1998.

   hBmp :              Source bitmap
   cTransparentColor : Color base for the "transparent" pixels
                       (default is black)
   cTolerance :        Color tolerance for the "transparent" pixels.

   A pixel is assumed to be transparent if the value of each of its 3
   components (blue, green and red) is
   greater or equal to the corresponding value in cTransparentColor and is
   lower or equal to the corresponding value in cTransparentColor + cTolerance.
 */
#define ALLOC_UNIT 100

HRGN BitmapToRegion(HBITMAP hBmp, COLORREF cTransparentColor, COLORREF cTolerance)
{
  HRGN hRgn = nullptr;
  VOID *pbits32;
  DWORD maxRects = ALLOC_UNIT;

  if (hBmp != nullptr)
  {
    // Create a memory DC inside which we will scan the bitmap content
    auto hMemDC = CreateCompatibleDC(nullptr);
    if (hMemDC != nullptr)
    {
      // Get bitmap size
      BITMAP bm;
      GetObject(hBmp, sizeof(bm), &bm);

      // Create a 32 bits depth bitmap and select it into the memory DC
      BITMAPINFOHEADER RGB32BITSBITMAPINFO{};
      RGB32BITSBITMAPINFO.biSize = sizeof(BITMAPINFOHEADER);
      RGB32BITSBITMAPINFO.biWidth = bm.bmWidth;
      RGB32BITSBITMAPINFO.biHeight = bm.bmHeight;
      RGB32BITSBITMAPINFO.biPlanes = 1;
      RGB32BITSBITMAPINFO.biBitCount = 32;
      RGB32BITSBITMAPINFO.biCompression = BI_RGB;
      // RGB32BITSBITMAPINFO.biSizeImage     = 0;
      // RGB32BITSBITMAPINFO.biXPelsPerMeter = 0;
      // RGB32BITSBITMAPINFO.biYPelsPerMeter = 0;
      // RGB32BITSBITMAPINFO.biClrUsed       = 0;
      // RGB32BITSBITMAPINFO.biClrImportant  = 0;

      HBITMAP hbm32 = CreateDIBSection(hMemDC, reinterpret_cast<BITMAPINFO *>(&RGB32BITSBITMAPINFO), DIB_RGB_COLORS,
                                       &pbits32, nullptr, 0);
      if (hbm32 != nullptr)
      {
        auto holdBmp = static_cast<HBITMAP>(SelectObject(hMemDC, hbm32));

        // Create a DC just to copy the bitmap into the memory DC
        auto hDC = CreateCompatibleDC(hMemDC);
        if (hDC != nullptr)
        {
          // Get how many bytes per row we have for the bitmap bits (rounded up to 32 bits)
          BYTE *p32;
          HRGN h;

          BITMAP bm32;
          GetObject(hbm32, sizeof(bm32), &bm32);
          while (bm32.bmWidthBytes % 4)
          {
            bm32.bmWidthBytes++;
          }

          // Copy the bitmap into the memory DC
          holdBmp = static_cast<HBITMAP>(SelectObject(hDC, hBmp));
          BitBlt(hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hDC, 0, 0, SRCCOPY);

          // For better performances, we will use the  ExtCreateRegion() function to create the
          // region. This function take a RGNDATA structure on  entry. We will add rectangles by
          // amount of ALLOC_UNIT number in this structure.
          HANDLE hData = GlobalAlloc(GMEM_MOVEABLE, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects));

          auto pData = static_cast<RGNDATA *>(GlobalLock(hData));
          pData->rdh.dwSize = sizeof(RGNDATAHEADER);
          pData->rdh.iType = RDH_RECTANGLES;
          pData->rdh.nCount = pData->rdh.nRgnSize = 0;
          SetRect(&pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0);

          // Keep on hand highest and lowest values for the  "transparent" pixels
          BYTE lr = GetRValue(cTransparentColor);
          BYTE lg = GetGValue(cTransparentColor);
          BYTE lb = GetBValue(cTransparentColor);
          auto hr = static_cast<BYTE>(HB_MIN(0xff, lr + GetRValue(cTolerance)));
          auto hg = static_cast<BYTE>(HB_MIN(0xff, lg + GetGValue(cTolerance)));
          auto hb = static_cast<BYTE>(HB_MIN(0xff, lb + GetBValue(cTolerance)));

          // Scan each bitmap row from bottom to top (the bitmap is  inverted vertically)
          p32 = static_cast<BYTE *>(bm32.bmBits) + (bm32.bmHeight - 1) * bm32.bmWidthBytes;
          for (auto y = 0; y < bm.bmHeight; y++)
          { // Scan each bitmap pixel from left to right
            for (auto x = 0; x < bm.bmWidth; x++)
            { // Search for a continuous range of "non transparent pixels"
              int x0 = x;
              auto p = reinterpret_cast<LONG *>(p32) + x;
              while (x < bm.bmWidth)
              {
                BYTE b = GetRValue(*p);
                if (b >= lr && b <= hr)
                {
                  b = GetGValue(*p);
                  if (b >= lg && b <= hg)
                  {
                    b = GetBValue(*p);
                    if (b >= lb && b <= hb)
                    {
                      break; // This pixel is "transparent"
                    }
                  }
                }

                p++;
                x++;
              }

              if (x > x0)
              { // Add the pixels (x0, y) to (x, y+1) as a new rectangle in the region
                RECT *pr;
                if (pData->rdh.nCount >= maxRects)
                {
                  GlobalUnlock(hData);
                  maxRects += ALLOC_UNIT;
                  hData = GlobalReAlloc(hData, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects), GMEM_MOVEABLE);
                  pData = static_cast<RGNDATA *>(GlobalLock(hData));
                }

                pr = reinterpret_cast<RECT *>(&pData->Buffer);
                SetRect(&pr[pData->rdh.nCount], x0, y, x, y + 1);
                if (x0 < pData->rdh.rcBound.left)
                {
                  pData->rdh.rcBound.left = x0;
                }

                if (y < pData->rdh.rcBound.top)
                {
                  pData->rdh.rcBound.top = y;
                }

                if (x > pData->rdh.rcBound.right)
                {
                  pData->rdh.rcBound.right = x;
                }

                if (y + 1 > pData->rdh.rcBound.bottom)
                {
                  pData->rdh.rcBound.bottom = y + 1;
                }

                pData->rdh.nCount++;

                // On Windows98, ExtCreateRegion() may fail if  the number of rectangles is too
                // large (ie: > 4000).
                // Therefore, we have to create the region by multiple steps.
                if (pData->rdh.nCount == 2000)
                {
                  h = ExtCreateRegion(nullptr, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects), pData);
                  if (hRgn != nullptr)
                  {
                    CombineRgn(hRgn, hRgn, h, RGN_OR);
                    DeleteObject(h);
                  }
                  else
                  {
                    hRgn = h;
                  }

                  pData->rdh.nCount = 0;
                  SetRect(&pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
                }
              }
            }

            // Go to next row (remember, the bitmap is inverted vertically)
            p32 -= bm32.bmWidthBytes;
          }

          // Create or extend the region with the remaining  rectangles
          h = ExtCreateRegion(nullptr, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects), pData);
          if (hRgn != nullptr)
          {
            CombineRgn(hRgn, hRgn, h, RGN_OR);
            DeleteObject(h);
          }
          else
          {
            hRgn = h;
          }

          // Clean up
          GlobalFree(hData);
          SelectObject(hDC, holdBmp);
          DeleteDC(hDC);
        }

        DeleteObject(SelectObject(hMemDC, holdBmp));
      }

      DeleteDC(hMemDC);
    }
  }

  return hRgn;
}
