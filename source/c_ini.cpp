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
#include <hbwinuni.hpp>

#ifdef UNICODE
LPSTR WideToAnsi(LPWSTR);
#endif

// HMG_GETPRIVATEPROFILESTRING(cSection, cEntry, cDefault, cFileName) --> string
HB_FUNC(HMG_GETPRIVATEPROFILESTRING)
{
  void *str1 = nullptr;
  LPCTSTR lpSection = HB_ISCHAR(1) ? HB_PARSTR(1, &str1, nullptr) : nullptr;
  void *str2 = nullptr;
  LPCTSTR lpEntry = HB_ISCHAR(2) ? HB_PARSTR(2, &str2, nullptr) : nullptr;
  void *str3;
  LPCTSTR lpDefault = HB_PARSTR(3, &str3, nullptr);
  void *str4;
  LPCTSTR lpFileName = HB_PARSTR(4, &str4, nullptr);

#ifdef UNICODE
  LPSTR pStr;
#endif

  DWORD nSize = 256;
  TCHAR *bBuffer;
  DWORD dwLen;

  do
  {
    nSize *= 2;
    bBuffer = static_cast<TCHAR *>(hb_xgrab(sizeof(TCHAR) * nSize));
    dwLen = GetPrivateProfileString(lpSection, lpEntry, lpDefault, bBuffer, nSize, lpFileName);
  } while (dwLen >= nSize - 1);

  if (dwLen) {
#ifndef UNICODE
    hb_retclen(static_cast<TCHAR *>(bBuffer), dwLen);
#else
    pStr = WideToAnsi(bBuffer);
    hb_retc(pStr);
    hb_xfree(pStr);
#endif
  }
  else
  {
    HB_RETSTR(lpDefault);
  }

  hb_xfree(bBuffer);
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

// HMG_WRITEPRIVATEPROFILESTRING(cSection, cEntry|NIL, cData|NIL, cFileName) --> .T.|.F.
HB_FUNC(HMG_WRITEPRIVATEPROFILESTRING)
{
  void *str1;
  void *str2 = nullptr;
  void *str3 = nullptr;
  void *str4;
  hb_retl(WritePrivateProfileString(HB_PARSTR(1, &str1, nullptr), HB_ISCHAR(2) ? HB_PARSTR(2, &str2, nullptr) : nullptr,
                                    HB_ISCHAR(3) ? HB_PARSTR(3, &str3, nullptr) : nullptr,
                                    HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

// HMG_DELINIENTRY(cSection, cEntry, cFileName) --> .T.|.F.
HB_FUNC(HMG_DELINIENTRY)
{
  void *str1;
  void *str2;
  void *str3;
  hb_retl(WritePrivateProfileString(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), nullptr,
                                    HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

// HMG_DELINISECTION(cSection, cFileName) --> .T.|.F.
HB_FUNC(HMG_DELINISECTION)
{
  void *str1;
  void *str2;
  hb_retl(WritePrivateProfileString(HB_PARSTR(1, &str1, nullptr), nullptr, TEXT(""), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

static TCHAR *FindFirstSubString(TCHAR *Strings)
{
  TCHAR *p = Strings;

  if (*p == 0) {
    p = nullptr;
  }

  return p;
}

static TCHAR *FindNextSubString(TCHAR *Strings)
{
  TCHAR *p = Strings;

  p = p + lstrlen(Strings) + 1;

  if (*p == 0) {
    p = nullptr;
  }

  return p;
}

static INT FindLenSubString(TCHAR *Strings)
{
  INT i = 0;
  TCHAR *p = Strings;

  if ((p = FindFirstSubString(p)) != nullptr) {
    for (i = 1; (p = FindNextSubString(p)) != nullptr; i++)
    {
    }
  }

  return i;
}

// (JK) HMG 1.0 Experimental build 6

// HMG__GETPRIVATEPROFILESECTIONNAMES(cFileName) --> array
HB_FUNC(HMG__GETPRIVATEPROFILESECTIONNAMES)
{
  TCHAR bBuffer[32767];
  INT nLen;
#ifdef UNICODE
  LPSTR pStr;
#endif

  ZeroMemory(bBuffer, sizeof(bBuffer));
  void *str;
  GetPrivateProfileSectionNames(bBuffer, sizeof(bBuffer) / sizeof(TCHAR), HB_PARSTR(1, &str, nullptr));
  hb_strfree(str);

  auto p = static_cast<TCHAR *>(bBuffer);
  nLen = FindLenSubString(p);
  hb_reta(nLen);
  if (nLen > 0) {
#ifndef UNICODE
    HB_STORC((p = FindFirstSubString(p)), -1, 1);
    for (auto i = 2; (p = FindNextSubString(p)) != nullptr; i++)
    {
      HB_STORC(p, -1, i);
    }
#else
    p = FindFirstSubString(p);
    pStr = WideToAnsi(p);
    HB_STORC(pStr, -1, 1);
    for (auto i = 2; (p = FindNextSubString(p)) != nullptr; i++)
    {
      pStr = WideToAnsi(p);
      HB_STORC(pStr, -1, i);
    }
    hb_xfree(pStr);
#endif
  }
}

// Used to retrieve all key/value pairs of a given section.

// HMG__GETPRIVATEPROFILESECTION(cSectionName, cFileName) --> array
HB_FUNC(HMG__GETPRIVATEPROFILESECTION)
{
#ifdef UNICODE
  LPSTR pStr;
#endif

  TCHAR bBuffer[32767];
  ZeroMemory(bBuffer, sizeof(bBuffer));
  void *str1;
  void *str2;
  GetPrivateProfileSection(HB_PARSTR(1, &str1, nullptr), bBuffer, sizeof(bBuffer) / sizeof(TCHAR),
                           HB_PARSTR(2, &str2, nullptr));
  hb_strfree(str1);
  hb_strfree(str2);
  auto p = static_cast<TCHAR *>(bBuffer);
  INT nLen = FindLenSubString(p);
  hb_reta(nLen);
  if (nLen > 0) {
#ifndef UNICODE
    HB_STORC((p = FindFirstSubString(p)), -1, 1);
    for (auto i = 2; (p = FindNextSubString(p)) != nullptr; i++)
    {
      HB_STORC(p, -1, i);
    }
#else
    p = FindFirstSubString(p);
    pStr = WideToAnsi(p);
    HB_STORC(pStr, -1, 1);
    for (auto i = 2; (p = FindNextSubString(p)) != nullptr; i++)
    {
      pStr = WideToAnsi(p);
      HB_STORC(pStr, -1, i);
    }
    hb_xfree(pStr);
#endif
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETPRIVATEPROFILESTRING, HMG_GETPRIVATEPROFILESTRING)
HB_FUNC_TRANSLATE(WRITEPRIVATEPROFILESTRING, HMG_WRITEPRIVATEPROFILESTRING)
HB_FUNC_TRANSLATE(DELINIENTRY, HMG_DELINIENTRY)
HB_FUNC_TRANSLATE(DELINISECTION, HMG_DELINISECTION)
HB_FUNC_TRANSLATE(_GETPRIVATEPROFILESECTIONNAMES, HMG__GETPRIVATEPROFILESECTIONNAMES)
HB_FUNC_TRANSLATE(_GETPRIVATEPROFILESECTION, HMG__GETPRIVATEPROFILESECTION)
#endif
