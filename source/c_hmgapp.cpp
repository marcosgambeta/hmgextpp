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

#define WINVER 0x0410

#include "mgdefs.hpp"
#include "shlwapi.h"
#include <hbinit.hpp>
#include <hbvm.hpp>
#define _HMG_STUB_
#include "hbgdiplus.h"
#undef _HMG_STUB_

#define PACKVERSION(major, minor) MAKELONG(minor, major)

extern void hmg_ErrorExit(LPCTSTR lpMessage, DWORD dwError, BOOL bExit);
extern GpStatus GdiplusInit(void);
HMODULE hmg_LoadLibrarySystem(LPCTSTR pFileName);
// auxiliary functions
TCHAR *hmg_tstrdup(const TCHAR *pszText);
TCHAR *hmg_tstrncat(TCHAR *pDest, const TCHAR *pSource, HB_SIZE nLen);
HB_SIZE hmg_tstrlen(const TCHAR *pText);
static DWORD DllGetVersion(LPCTSTR lpszDllName);
static TCHAR *hmg_FileNameAtSystemDir(const TCHAR *pFileName);
using _DLLGETVERSIONPROC = HRESULT(CALLBACK *)(DLLVERSIONINFO2 *);

static HINSTANCE g_hInstance = nullptr;
static DWORD g_dwComCtl32Ver = 0;

static void hmg_init(void *cargo)
{
  LPCTSTR lpszDllName = TEXT("ComCtl32.dll");

  HB_SYMBOL_UNUSED(cargo);

  if (S_FALSE == CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE | COINIT_SPEED_OVER_MEMORY)) {
    hmg_ErrorExit(TEXT("hmg_init(void)"), S_FALSE, TRUE);
  }

  g_dwComCtl32Ver = DllGetVersion(lpszDllName);

  GetInstance();

  if (Ok != GdiplusInit()) {
    hmg_ErrorExit(TEXT("GdiplusInit(void)"), 0, TRUE);
  }
}

HB_CALL_ON_STARTUP_BEGIN(_hmg_init_)
hb_vmAtInit(hmg_init, nullptr);
HB_CALL_ON_STARTUP_END(_hmg_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup _hmg_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY HB_DATASEG_FUNC(_hmg_init_)
#include <hbiniseg.hpp>
#endif

HINSTANCE GetInstance(void)
{
  if (!g_hInstance) {
    g_hInstance = GetModuleHandle(0);
  }

  return g_hInstance;
}

static DWORD DllGetVersion(LPCTSTR lpszDllName)
{
  DWORD dwVersion = 0;

  HINSTANCE hinstDll = hmg_LoadLibrarySystem(lpszDllName);

  if (hinstDll) {
    auto pDllGetVersion = reinterpret_cast<_DLLGETVERSIONPROC>(wapi_GetProcAddress(hinstDll, "DllGetVersion"));

    if (pDllGetVersion) {
      DLLVERSIONINFO2 dvi{};
      dvi.info1.cbSize = sizeof(dvi);
      HRESULT hr = (*pDllGetVersion)(&dvi);
      if (S_OK == hr) {
        dwVersion = PACKVERSION(dvi.info1.dwMajorVersion, dvi.info1.dwMinorVersion);
      }
    }
    FreeLibrary(hinstDll);
  }

  return dwVersion;
}

HB_FUNC(HMG_GETINSTANCE)
{
  hmg_ret_HANDLE(g_hInstance);
}

HB_FUNC(HMG_GETCOMCTL32DLLVER)
{
  hb_retnint(g_dwComCtl32Ver);
}

HB_FUNC(HMG_OLEDATARELEASE)
{
  CoUninitialize();
}

// borrowed from hbwapi.lib [vszakats]
#ifndef LOAD_LIBRARY_SEARCH_SYSTEM32
#define LOAD_LIBRARY_SEARCH_SYSTEM32 0x00000800
#endif

static bool win_has_search_system32(void)
{
  HMODULE hKernel32 = GetModuleHandle(TEXT("kernel32.dll"));

  if (hKernel32) {
    return GetProcAddress(hKernel32, "AddDllDirectory") != nullptr; // Detect KB2533623
  }

  return false;
}

HMODULE hmg_LoadLibrarySystem(LPCTSTR pFileName)
{
  TCHAR *pLibPath = hmg_FileNameAtSystemDir(pFileName);
  HMODULE h = LoadLibraryEx(pLibPath, nullptr,
                            win_has_search_system32() ? LOAD_LIBRARY_SEARCH_SYSTEM32 : LOAD_WITH_ALTERED_SEARCH_PATH);
  hb_xfree(pLibPath);
  return h;
}

static TCHAR *hmg_FileNameAtSystemDir(const TCHAR *pFileName)
{
  UINT nLen = GetSystemDirectory(nullptr, 0);

  if (nLen) {
    if (pFileName) {
      nLen += static_cast<UINT>(hmg_tstrlen(pFileName)) + 1;
    }

    auto buffer = static_cast<LPTSTR>(hb_xgrab(nLen * sizeof(TCHAR)));

    GetSystemDirectory(buffer, nLen);

    if (pFileName) {
      hmg_tstrncat(buffer, TEXT("\\"), nLen - 1);
      hmg_tstrncat(buffer, pFileName, nLen - 1);
    }

    return buffer;
  } else {
    return hmg_tstrdup(pFileName);
  }
}

TCHAR *hmg_tstrdup(const TCHAR *pszText)
{
  HB_SIZE nLen = (hmg_tstrlen(pszText) + 1) * sizeof(TCHAR);
  auto pszDup = static_cast<TCHAR *>(hb_xgrab(nLen));
  memcpy(pszDup, pszText, nLen);
  return pszDup;
}

TCHAR *hmg_tstrncat(TCHAR *pDest, const TCHAR *pSource, HB_SIZE nLen)
{
  TCHAR *pBuf = pDest;

  pDest[nLen] = '\0';

  while (nLen && *pDest) {
    pDest++;
    nLen--;
  }

  while (nLen && (*pDest++ = *pSource++) != '\0') {
    nLen--;
  }

  return pBuf;
}

HB_SIZE hmg_tstrlen(const TCHAR *pText)
{
  HB_SIZE nLen = 0;

  while (pText[nLen] != '\0') {
    ++nLen;
  }

  return nLen;
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETINSTANCE, HMG_GETINSTANCE)
HB_FUNC_TRANSLATE(GETCOMCTL32DLLVER, HMG_GETCOMCTL32DLLVER)
HB_FUNC_TRANSLATE(OLEDATARELEASE, HMG_OLEDATARELEASE)
#endif
