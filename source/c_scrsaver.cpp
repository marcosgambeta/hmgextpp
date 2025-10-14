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

#include "mgdefs.hpp"

#if defined(_MSC_VER)
#pragma warning(disable : 4996)
#endif
#include <commctrl.h>

extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);

using VERIFYSCREENSAVEPWD = BOOL(WINAPI *)(HWND hwnd);
using PWDCHANGEPASSWORD = VOID(WINAPI *)(LPCSTR lpcRegkeyname, HWND hwnd, UINT uiReserved1, UINT uiReserved2);

HB_FUNC(HMG_VERIFYPASSWORD)
{
  // Under NT, we return TRUE immediately. This lets the saver quit,
  // and the system manages passwords. Under '95, we call VerifyScreenSavePwd.
  // This checks the appropriate registry key and, if necessary,
  // pops up a verify dialog.

  HINSTANCE hpwdcpl;
  VERIFYSCREENSAVEPWD VerifyScreenSavePwd;
  BOOL bres;
  OSVERSIONINFO osvi;

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&osvi);

  if (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT) {
    hb_retl(true);
  }

  hpwdcpl = LoadLibrary(TEXT("PASSWORD.CPL"));

  if (hpwdcpl == nullptr) {
    hb_retl(false);
  }

  VerifyScreenSavePwd = (VERIFYSCREENSAVEPWD)wapi_GetProcAddress(hpwdcpl, "VerifyScreenSavePwd");
  if (VerifyScreenSavePwd == nullptr) {
    FreeLibrary(hpwdcpl);
    hb_retl(false);
  }

  auto hwnd = hmg_par_HWND(1);

  bres = VerifyScreenSavePwd(hwnd);
  FreeLibrary(hpwdcpl);

  hb_retl(bres);
}

HB_FUNC(HMG_CHANGEPASSWORD)
{
  // This only ever gets called under '95, when started with the /a option.

  HINSTANCE hmpr = LoadLibrary(TEXT("MPR.DLL"));
  PWDCHANGEPASSWORD PwdChangePassword;

  if (hmpr == nullptr) {
    hb_retl(false);
  }

  PwdChangePassword = (PWDCHANGEPASSWORD)wapi_GetProcAddress(hmpr, "PwdChangePasswordA");

  if (PwdChangePassword == nullptr) {
    FreeLibrary(hmpr);
    hb_retl(false);
  }

  auto hwnd = hmg_par_HWND(1);
  PwdChangePassword("SCRSAVE", hwnd, 0, 0);
  FreeLibrary(hmpr);

  hb_retl(true);
}

// Moved from c_controlmisc.c

HB_FUNC(HMG_SETCURSORPOS)
{
  SetCursorPos(hb_parni(1), hb_parni(2));
}

HB_FUNC(HMG_SHOWCURSOR)
{
  hb_retni(ShowCursor(hb_parl(1)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(VERIFYPASSWORD, HMG_VERIFYPASSWORD)
HB_FUNC_TRANSLATE(CHANGEPASSWORD, HMG_CHANGEPASSWORD)
HB_FUNC_TRANSLATE(SETCURSORPOS, HMG_SETCURSORPOS)
HB_FUNC_TRANSLATE(SHOWCURSOR, HMG_SHOWCURSOR)
#endif
