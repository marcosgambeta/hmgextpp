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
 *
 * Parts of this code is contributed and used here under permission of his
 * author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "mgdefs.hpp"

#include <hbapifs.hpp>

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif
static HINSTANCE hResources = 0;
static HINSTANCE HMG_DllStore[256];

static HINSTANCE HMG_LoadDll(char *DllName)
{
  static int DllCnt;

#ifndef UNICODE
  LPCSTR lpLibFileName = DllName;
#else
  LPCWSTR lpLibFileName = AnsiToWide(DllName);
#endif

  DllCnt = (DllCnt + 1) & 255;
  FreeLibrary(HMG_DllStore[DllCnt]);

  return HMG_DllStore[DllCnt] = LoadLibraryEx(lpLibFileName, nullptr, 0);
}

static void HMG_UnloadDll(void)
{
  for (auto i = 255; i >= 0; i--)
  {
    FreeLibrary(HMG_DllStore[i]);
  }
}

HINSTANCE GetResources(void)
{
  return (hResources) ? (hResources) : (GetInstance());
}

HB_FUNC(HMG_GETRESOURCES)
{
  hmg_ret_HANDLE(GetResources());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETRESOURCES, HMG_GETRESOURCES)
#endif

HB_FUNC(HMG_SETRESOURCES)
{
  if (HB_ISCHAR(1))
  {
    hResources = HMG_LoadDll(const_cast<char *>(hb_parc(1)));
  }
  else if (HB_ISNUM(1))
  {
    hResources = hmg_par_HINSTANCE(1);
  }

  hmg_ret_HANDLE(hResources);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETRESOURCES, HMG_SETRESOURCES)
#endif

HB_FUNC(HMG_FREERESOURCES)
{
  HMG_UnloadDll();

  if (hResources)
  {
    hResources = 0;
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FREERESOURCES, HMG_FREERESOURCES)
#endif

HB_FUNC(HMG_RCDATATOFILE)
{
  HMODULE hModule = (HMODULE)(0 != HB_PARNL(4) ? hmg_par_HINSTANCE(4) : GetResources());

  /* lpType is RT_RCDATA by default */
#ifndef UNICODE
  LPCSTR lpName = hb_parc(1);
  LPCSTR lpType = (hb_parclen(3) > 0) ? static_cast<LPCSTR>(hb_parc(3)) : MAKEINTRESOURCE(hb_parnidef(3, 10));
#else
  LPCWSTR lpName = AnsiToWide(static_cast<char *>(hb_parc(1)));
  LPCWSTR lpType = HB_ISCHAR(3) ? AnsiToWide(static_cast<char *>(hb_parc(3)))
                                : static_cast<LPCWSTR>(MAKEINTRESOURCE(hb_parnidef(3, 10)));
#endif
  HRSRC hResInfo;
  HGLOBAL hResData = nullptr;
  HB_SIZE dwResult = 0;

  if (HB_ISCHAR(1))
  {
    hResInfo = FindResource(hModule, lpName, lpType);
  }
  else
  {
    hResInfo = FindResource(hModule, MAKEINTRESOURCE(hb_parni(1)), lpType);
  }

  if (hResInfo != nullptr)
  {
    hResData = LoadResource(hModule, hResInfo);

    if (hResData == nullptr)
    {
      dwResult = (HB_SIZE)-2; // can't load
    }
  }
  else
  {
    dwResult = (HB_SIZE)-1; // can't find
  }

  if (0 == dwResult)
  {
    LPVOID lpData = LockResource(hResData);

    if (lpData != nullptr)
    {
      DWORD dwSize = SizeofResource(hModule, hResInfo);
      PHB_FILE pFile;

      pFile = hb_fileExtOpen(hb_parcx(2), nullptr, FO_CREAT | FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE, nullptr, nullptr);

      if (pFile != nullptr)
      {
        dwResult = hb_fileWrite(pFile, (const void *)lpData, (HB_SIZE)dwSize, -1);

        if (dwResult != dwSize)
        {
          dwResult = (HB_SIZE)-5; // can't write
        }

        hb_fileClose(pFile);
      }
      else
      {
        dwResult = (HB_SIZE)-4; // can't open
      }
    }
    else
    {
      dwResult = (HB_SIZE)-3; // can't lock
    }

    FreeResource(hResData);
  }

  hb_retnl(dwResult);

#ifdef UNICODE
  hb_xfree((TCHAR *)lpName);
  if (HB_ISCHAR(3))
  {
    hb_xfree((TCHAR *)lpType);
  }
#endif
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RCDATATOFILE, HMG_RCDATATOFILE)
#endif
