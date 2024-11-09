//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// MDI window source code
// (C)2005 Janusz Pora <januszpora@onet.eu>
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

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

LRESULT CALLBACK MdiWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK MdiChildWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

static HWND hwndMDIClient;

/*
HMG_REGISTERMDIWINDOW() -->
*/
HB_FUNC(HMG_REGISTERMDIWINDOW)
{
  void *str1 = nullptr;
  LPCTSTR lpIconName = HB_ISCHAR(1) ? HB_PARSTR(1, &str1, nullptr) : nullptr;
  void *str2;
  LPCTSTR lpClassName = HB_PARSTR(2, &str2, nullptr);

  WNDCLASS WndClass{};
  WndClass.style = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
  WndClass.lpfnWndProc = MdiWndProc;
  WndClass.cbClsExtra = 0;
  WndClass.cbWndExtra = 0;
  WndClass.hInstance = GetInstance();
  WndClass.hIcon = LoadIcon(GetResources(), lpIconName);
  if (WndClass.hIcon == nullptr)
  {
    WndClass.hIcon =
        static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
  }

  if (WndClass.hIcon == nullptr)
  {
    WndClass.hIcon = LoadIcon(nullptr, IDI_APPLICATION);
  }

  WndClass.hCursor = LoadCursor(nullptr, IDC_ARROW);
  HBRUSH hbrush = nullptr;
  if (HB_PARNI(3, 1) == -1)
  {
    WndClass.hbrBackground = reinterpret_cast<HBRUSH>(COLOR_WINDOW + 1);
  }
  else
  {
    hbrush = CreateSolidBrush(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3)));
    WndClass.hbrBackground = hbrush;
  }

  WndClass.lpszMenuName = nullptr;
  WndClass.lpszClassName = lpClassName;

  if (!RegisterClass(&WndClass))
  {
    MessageBox(0, TEXT("Window MDI Registration Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    ExitProcess(0);
  }

  WndClass.style = 0;
  WndClass.lpfnWndProc = static_cast<WNDPROC>(MdiChildWndProc);
  WndClass.cbClsExtra = 0;
  WndClass.cbWndExtra = 20;
  WndClass.hInstance = GetInstance();

  // Owner of this class

  WndClass.hIcon = LoadIcon(GetResources(), lpIconName);
  if (WndClass.hIcon == nullptr)
  {
    WndClass.hIcon =
        static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
  }

  if (WndClass.hIcon == nullptr)
  {
    WndClass.hIcon = LoadIcon(nullptr, IDI_APPLICATION);
  }

  WndClass.hCursor = LoadCursor(nullptr, IDC_ARROW);

  if (HB_PARNI(3, 1) == -1)
  {
    WndClass.hbrBackground = reinterpret_cast<HBRUSH>(COLOR_WINDOW + 1);
  }
  else
  {
    WndClass.hbrBackground = hbrush;
  }

  WndClass.lpszMenuName = nullptr;
  WndClass.lpszClassName = TEXT("MdiChildWndClass");
  if (!RegisterClass((LPWNDCLASS)&WndClass))
  {
    MessageBox(0, TEXT("Window MdiChild Registration Failed!"), TEXT("Error!"),
               MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    ExitProcess(0);
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hmg_ret_HBRUSH(hbrush);
}

LRESULT CALLBACK MdiWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  if (!pSymbol)
  {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("EVENTS"));
  }

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHandle(hWnd);
    hmg_vmPushUINT(message);
    hb_vmPushNumInt(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
  }

  long int r = hb_parnl(-1);

  if (r != 0)
  {
    return r;
  }
  else
  {
    return DefFrameProc(hWnd, hwndMDIClient, message, wParam, lParam);
  }
}

LRESULT CALLBACK MdiChildWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  if (!pSymbol)
  {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("MDIEVENTS"));
  }

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHandle(hWnd);
    hmg_vmPushUINT(message);
    hb_vmPushNumInt(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
  }

  long int r = hb_parnl(-1);

  if (r == 0)
  {
    return DefMDIChildProc(hWnd, message, wParam, lParam);
  }
  else
  {
    return r;
  }
}

HB_FUNC(HMG_INITMDIWINDOW)
{
  void *str1;
  LPCTSTR lpWindowName = HB_PARSTR(1, &str1, nullptr);
  void *str2;
  LPCTSTR lpClassName = HB_PARSTR(12, &str2, nullptr);

  DWORD ExStyle;
  DWORD style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_BORDER | WS_SYSMENU | WS_THICKFRAME;

  if (hb_parl(16))
  {
    ExStyle = WS_EX_CONTEXTHELP;
  }
  else
  {
    ExStyle = 0;
    if (!hb_parl(6))
    {
      style |= WS_MINIMIZEBOX;
    }
    if (!hb_parl(7))
    {
      style |= WS_MAXIMIZEBOX;
    }
  }

  if (!hb_parl(8))
  {
    style |= WS_SIZEBOX;
  }

  if (!hb_parl(9))
  {
    style |= WS_SYSMENU;
  }

  if (!hb_parl(10))
  {
    style |= WS_CAPTION;
  }

  if (hb_parl(11))
  {
    ExStyle |= WS_EX_TOPMOST;
  }

  if (hb_parl(14))
  {
    style |= WS_VSCROLL;
  }

  if (hb_parl(15))
  {
    style |= WS_HSCROLL;
  }

  auto hwnd = CreateWindowEx(ExStyle, lpClassName, lpWindowName, style, hb_parni(2), hb_parni(3), hb_parni(4),
                             hb_parni(5), hmg_par_HWND(13), nullptr, GetInstance(), nullptr);

  hb_strfree(str1);
  hb_strfree(str2);

  if (hwnd == nullptr)
  {
    MessageBox(0, TEXT("MDI Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
  }

  hmg_ret_HWND(hwnd);
}

/*
HMG_INITMDICLIENTWINDOW() -->
*/
HB_FUNC(HMG_INITMDICLIENTWINDOW)
{
  auto hwndparent = hmg_par_HWND(1);
  int icount = GetMenuItemCount(GetMenu(hwndparent));

  // Find window menu where children will be listed

  CLIENTCREATESTRUCT ccs{};
  ccs.hWindowMenu = GetSubMenu(GetMenu(hwndparent), icount - 2);
  ccs.idFirstChild = 0;

  // Create the MDI client filling the client area

  auto hwndMDIClient =
      CreateWindowEx(0, TEXT("mdiclient"), nullptr, WS_CHILD | WS_CLIPCHILDREN | WS_VSCROLL | WS_HSCROLL | WS_VISIBLE,
                     0, 0, 0, 0, hwndparent, (HMENU)0xCAC, GetInstance(), reinterpret_cast<LPSTR>(&ccs));

  ShowWindow(hwndMDIClient, SW_SHOW);
  hmg_ret_HWND(hwndMDIClient);
}

/*
HMG_INITMDICHILDWINDOW() -->
*/
HB_FUNC(HMG_INITMDICHILDWINDOW)
{
  TCHAR rgch[150];
  static int cUntitled;

  if (hb_parl(9))
  {
    rgch[0] = 0;
  }
  else
  {
    if (hb_parc(2) == nullptr)
    {
      wsprintf(rgch, TEXT("Untitled%d"), cUntitled++);
    }
    else
    {
      void *str;
      LPCTSTR lpTitle = HB_PARSTR(2, &str, nullptr);
      HB_STRNCPY(rgch, lpTitle, 149);
      rgch[149] = 0;
      hb_strfree(str);
    }
  }

  DWORD style = 0;

  if (hb_parl(10))
  {
    style |= WS_VSCROLL;
  }

  if (hb_parl(11))
  {
    style |= WS_HSCROLL;
  }

  // Create the MDI child window

  MDICREATESTRUCT mcs;
  mcs.szClass = TEXT("MdiChildWndClass"); // window class name
  mcs.szTitle = rgch;                     // window title
  mcs.hOwner = GetInstance();             // owner
  mcs.x = hb_parni(3);                    // x position
  mcs.y = hb_parni(4);                    // y position
  mcs.cx = hb_parni(5);                   // width
  mcs.cy = hb_parni(6);                   // height
  mcs.style = style;                      // window style
  mcs.lParam = 0;                         // lparam

  auto hwndChild =
      reinterpret_cast<HWND>(SendMessage(hmg_par_HWND(1), WM_MDICREATE, 0, reinterpret_cast<LPARAM>(&mcs)));

  if (hwndChild != nullptr)
  {
    style = GetWindowLong(hwndChild, GWL_STYLE);

    if (hb_parl(7))
    {
      style &= ~WS_MINIMIZEBOX;
    }

    if (hb_parl(8))
    {
      style &= ~WS_MAXIMIZEBOX;
    }

    if (hb_parl(9))
    {
      style &= ~WS_CAPTION;
    }

    SetWindowLongPtr(hwndChild, GWL_STYLE, style);
    ShowWindow(hwndChild, SW_SHOW);
  }

  hmg_ret_HWND(hwndChild);
}

/*
HMG_ARRANGEICONICWINDOWS(HWND) --> numeric
*/
HB_FUNC(HMG_ARRANGEICONICWINDOWS)
{
  hb_retni(ArrangeIconicWindows(hmg_par_HWND(1)));
}

/*
HMG_DEFMDICHILDPROC(HWND, np2, np3, np4) --> numeric
*/
HB_FUNC(HMG_DEFMDICHILDPROC)
{
  hb_retnl(DefMDIChildProc(hmg_par_HWND(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

/*
HMG_DEFFRAMEPROC(HWND, HWND, np3, np4, np5) --> numeric
*/
HB_FUNC(HMG_DEFFRAMEPROC)
{
  hb_retnl(DefFrameProc(hmg_par_HWND(1), hmg_par_HWND(2), hb_parnl(3), hb_parnl(4), hb_parnl(5)));
}

/*
HMG_SIZECLIENTWINDOW(HWND, HWND, HWND, np4) --> NIL
*/
HB_FUNC(HMG_SIZECLIENTWINDOW)
{
  RECT rcClient;
  GetClientRect(hmg_par_HWND(1), &rcClient);
  if (HB_PARNL(2))
  {
    RECT rc;
    GetWindowRect(hmg_par_HWND(2), &rc);
    ScreenToClient(hmg_par_HWND(1), (LPPOINT)&rc.left);
    rcClient.bottom = rc.top;
  }
  rcClient.top = hb_parnl(4);
  MoveWindow(hmg_par_HWND(3), rcClient.left, rcClient.top, rcClient.right - rcClient.left,
             rcClient.bottom - rcClient.top, TRUE);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(REGISTERMDIWINDOW, HMG_REGISTERMDIWINDOW)
HB_FUNC_TRANSLATE(INITMDIWINDOW, HMG_INITMDIWINDOW)
HB_FUNC_TRANSLATE(INITMDICLIENTWINDOW, HMG_INITMDICLIENTWINDOW)
HB_FUNC_TRANSLATE(INITMDICHILDWINDOW, HMG_INITMDICHILDWINDOW)
HB_FUNC_TRANSLATE(ARRANGEICONICWINDOWS, HMG_ARRANGEICONICWINDOWS)
HB_FUNC_TRANSLATE(DEFMDICHILDPROC, HMG_DEFMDICHILDPROC)
HB_FUNC_TRANSLATE(DEFFRAMEPROC, HMG_DEFFRAMEPROC)
HB_FUNC_TRANSLATE(SIZECLIENTWINDOW, HMG_SIZECLIENTWINDOW)
#endif
