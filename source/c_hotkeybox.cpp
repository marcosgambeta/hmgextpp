//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// HOTKEYBOX Control Source Code
// Copyright 2006 Grigory Filatov <gfilatov@gmail.com>
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

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif

void InterpretHotKey(UINT setting, TCHAR *szKeyName)
{
#ifndef UNICODE
  LPSTR lpString;
#else
  LPWSTR lpString;
#endif

  UINT uCode = (setting & 0x0000FF00) >> 8;
  UINT uVKey = setting & 255;
  *szKeyName = 0;

  BOOL Ctrl = uCode & HOTKEYF_CONTROL;
  BOOL Alt = uCode & HOTKEYF_ALT;
  BOOL Shift = uCode & HOTKEYF_SHIFT;

  lstrcat(szKeyName, Ctrl ? TEXT("Ctrl + ") : TEXT(""));
  lstrcat(szKeyName, Shift ? TEXT("Shift + ") : TEXT(""));
  lstrcat(szKeyName, Alt ? TEXT("Alt + ") : TEXT(""));

#ifndef UNICODE
  lpString = szKeyName;
#else
  lpString = AnsiToWide(static_cast<char *>(szKeyName));
#endif
  UINT WorkKey = MapVirtualKey(uVKey, 0);

  if (uCode & 0x00000008)
  { // extended key
    WorkKey = 0x03000000 | (WorkKey << 16);
  }
  else
  {
    WorkKey = 0x02000000 | (WorkKey << 16);
  }

  GetKeyNameText(WorkKey, lpString + lstrlen(lpString), 100);

#ifdef UNICODE
  hb_xfree(static_cast<TCHAR *>(lpString));
#endif
}

HB_FUNC(HMG_C_GETHOTKEYNAME)
{
#ifdef UNICODE
  LPSTR pStr;
#endif

  auto hWnd = hmg_par_HWND(1);
  auto wHotKey = static_cast<WORD>(SendMessage(hWnd, HKM_GETHOTKEY, 0, 0));
  TCHAR szKeyName[100];
  InterpretHotKey(wHotKey, szKeyName);

#ifndef UNICODE
  hb_retclen(szKeyName, 100);
#else
  pStr = WideToAnsi(szKeyName);
  hb_retclen(pStr, 100);
  hb_xfree(pStr);
#endif
}

HB_FUNC(HMG_INITHOTKEYBOX)
{
  DWORD style = WS_CHILD;

  if (!hb_parl(8))
  {
    style |= WS_VISIBLE;
  }

  if (!hb_parl(9))
  {
    style |= WS_TABSTOP;
  }

  auto hwndHotKey = CreateWindowEx(0, HOTKEY_CLASS, TEXT(""), style, hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5),
                                   hmg_par_HWND(1), nullptr, GetInstance(), nullptr);

  hmg_ret_HWND(hwndHotKey);
}

HB_FUNC(HMG_SETHOTKEYVALUE)
{
  auto hWnd = hmg_par_HWND(1);

  auto wHotKey = static_cast<WORD>(hb_parnl(2));

  if (wHotKey != 0)
  {
    SendMessage(hWnd, HKM_SETHOTKEY, wHotKey, 0);
  }

  SendMessage(hWnd, HKM_SETRULES, HKCOMB_NONE | HKCOMB_S,
              /* invalid key combinations */ MAKELPARAM(HOTKEYF_ALT, 0)); // add ALT to invalid entries
}

HB_FUNC(HMG_C_GETHOTKEYVALUE)
{
  auto wHotKey = static_cast<WORD>(SendMessage(hmg_par_HWND(1), HKM_GETHOTKEY, 0, 0));
  UINT uVirtualKeyCode = LOBYTE(LOWORD(wHotKey));
  UINT uModifiers = HIBYTE(LOWORD(wHotKey));
  UINT iModifierKeys = ((uModifiers & HOTKEYF_CONTROL) ? MOD_CONTROL : 0) | ((uModifiers & HOTKEYF_ALT) ? MOD_ALT : 0) |
                       ((uModifiers & HOTKEYF_SHIFT) ? MOD_SHIFT : 0);
  hb_reta(2);
  HB_STORVNL(static_cast<UINT>(uVirtualKeyCode), -1, 1);
  HB_STORNI(static_cast<UINT>(iModifierKeys), -1, 2);
}

HB_FUNC(HMG_C_GETHOTKEY)
{
  hb_retnl(static_cast<WORD>(SendMessage(hmg_par_HWND(1), HKM_GETHOTKEY, 0, 0)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(C_GETHOTKEYNAME, HMG_C_GETHOTKEYNAME)
HB_FUNC_TRANSLATE(INITHOTKEYBOX, HMG_INITHOTKEYBOX)
HB_FUNC_TRANSLATE(SETHOTKEYVALUE, HMG_SETHOTKEYVALUE)
HB_FUNC_TRANSLATE(C_GETHOTKEYVALUE, HMG_C_GETHOTKEYVALUE)
HB_FUNC_TRANSLATE(C_GETHOTKEY, HMG_C_GETHOTKEY)
#endif
