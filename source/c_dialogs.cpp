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
#define _WIN32_WINNT 0x0400

#include "mgdefs.hpp"
#include <commdlg.h>
#include <shlobj.h>
#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

/*
HMG_CHOOSEFONT(cFaceName, nHeight, nWeight, lItalic, nRGB, lUnderline, lStrikeOut, nCharSet, nFlags)
--> array[8]
*/
HB_FUNC(HMG_CHOOSEFONT)
{
  auto hwnd = GetActiveWindow();
  auto hdc = GetDC(hwnd);

  LOGFONT lf{};
  void *str;
  lstrcpy(lf.lfFaceName, HB_PARSTR(1, &str, nullptr));
  hb_strfree(str);
  lf.lfHeight = -MulDiv(hb_parnl(2), GetDeviceCaps(hdc, LOGPIXELSY), 72);
  lf.lfWeight = hb_parl(3) ? 700 : 400;
  lf.lfItalic = hb_parl(4) ? TRUE : FALSE;
  lf.lfUnderline = hb_parl(6) ? TRUE : FALSE;
  lf.lfStrikeOut = hb_parl(7) ? TRUE : FALSE;
  lf.lfCharSet = HB_ISNIL(8) ? DEFAULT_CHARSET : hb_parni(8);

  CHOOSEFONT cf{};
  cf.lStructSize = sizeof(CHOOSEFONT);
  cf.hwndOwner = hwnd;
  cf.hDC = nullptr;
  cf.lpLogFont = &lf;
  cf.Flags = HB_ISNUM(9) ? hb_parni(9) : CF_SCREENFONTS | CF_EFFECTS | CF_INITTOLOGFONTSTRUCT;
  cf.rgbColors = hb_parnl(5);
  cf.lCustData = 0L;
  cf.lpfnHook = nullptr;
  cf.hInstance = nullptr;
  cf.nFontType = SCREEN_FONTTYPE;
  cf.nSizeMin = 0;
  cf.nSizeMax = 0;

  if (!ChooseFont(&cf))
  {
    hb_reta(8);
    HB_STORC("", -1, 1);
    HB_STORVNL(0, -1, 2);
    HB_STORL(0, -1, 3);
    HB_STORL(0, -1, 4);
    HB_STORVNL(0, -1, 5);
    HB_STORL(0, -1, 6);
    HB_STORL(0, -1, 7);
    HB_STORNI(0, -1, 8);
    ReleaseDC(hwnd, hdc);
    return;
  }

  long PointSize = -MulDiv(lf.lfHeight, 72, GetDeviceCaps(hdc, LOGPIXELSY));
  int bold = (lf.lfWeight == 700) ? 1 : 0;

  auto pArray = hb_itemArrayNew(8);
  HB_ARRAYSETSTR(pArray, 1, lf.lfFaceName);
  hb_arraySetNL(pArray, 2, PointSize);
  hb_arraySetL(pArray, 3, bold);
  hb_arraySetL(pArray, 4, lf.lfItalic);
  hb_arraySetNL(pArray, 5, cf.rgbColors);
  hb_arraySetL(pArray, 6, lf.lfUnderline);
  hb_arraySetL(pArray, 7, lf.lfStrikeOut);
  hb_arraySetNI(pArray, 8, lf.lfCharSet);
  hb_itemReturnRelease(pArray);

  ReleaseDC(hwnd, hdc);
}

/*
HMG_C_GETFILE(cFilter, cTitle, cInitialDir, lp4, lp5, np6) --> string
*/
HB_FUNC(HMG_C_GETFILE)
{
  TCHAR buffer[32768];
  TCHAR cFullName[256][1024];
  TCHAR cCurDir[512];
  TCHAR cFileName[512];
  auto iFilterIndex = 1;
  auto iPosition = 0;
  auto iNumSelected = 0;

  DWORD flags = OFN_FILEMUSTEXIST;

#ifdef UNICODE
  LPWSTR pW1, pW2;
  LPSTR pStr;
  auto j = 0;
  auto cont = 0;
  auto p = static_cast<char *>(hb_parc(1));
  TCHAR Filter[4096];
  memset(static_cast<void *>(&Filter), 0, sizeof(Filter));

  while (*p != '\0')
  {
    cont += strlen(p) + 1;
    if (cont < 4096)
    {
      lstrcpy(&Filter[j], AnsiToWide(p));
      j += lstrlen(AnsiToWide(p)) + 1;
      p += strlen(p) + 1;
    }
    else
    {
      break;
    }
  }
#endif

  buffer[0] = 0;

  if (hb_parl(4))
  {
    flags = flags | OFN_ALLOWMULTISELECT | OFN_EXPLORER;
  }

  if (hb_parl(5))
  {
    flags = flags | OFN_NOCHANGEDIR;
  }

  if (hb_parni(6))
  {
    iFilterIndex = hb_parni(6);
  }

  OPENFILENAME ofn{};
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner = GetActiveWindow();
  void *str1;
  ofn.lpstrFilter = HB_PARSTR(1, &str1, nullptr);
  ofn.lpstrFile = buffer;
  void *str2;
  ofn.lpstrInitialDir = HB_PARSTR(3, &str2, nullptr);
  void *str3;
  ofn.lpstrTitle = HB_PARSTR(2, &str3, nullptr);
  ofn.nFilterIndex = iFilterIndex;
  ofn.nMaxFile = sizeof(buffer) / sizeof(TCHAR);
  ofn.nMaxFileTitle = 512;
  ofn.Flags = flags;

  if (GetOpenFileName(&ofn))
  {
    if (ofn.nFileExtension != 0)
    {
      HB_RETSTR(ofn.lpstrFile);
    }
    else
    {
      wsprintf(cCurDir, "%s", &buffer[iPosition]);
      iPosition = iPosition + lstrlen(cCurDir) + 1;

      do
      {
        iNumSelected++;
        wsprintf(cFileName, "%s", &buffer[iPosition]);
        iPosition = iPosition + lstrlen(cFileName) + 1;
        wsprintf(cFullName[iNumSelected], "%s\\%s", cCurDir, cFileName);
      } while ((lstrlen(cFileName) != 0) && (iNumSelected <= 255));

      if (iNumSelected > 1)
      {
        hb_reta(iNumSelected - 1);

        for (auto n = 1; n < iNumSelected; n++)
        {
#ifndef UNICODE
          HB_STORC(cFullName[n], -1, n);
#else
          pStr = hb_osStrU16Decode(cFullName[n]);
          HB_STORC(pStr, -1, n);
          hb_xfree(pStr);
#endif
        }
      }
      else
      {
        HB_RETSTR(&buffer[0]);
      }
    }
  }
  else
  {
    HB_RETSTR("");
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);

#ifdef UNICODE
  hb_xfree(pW1);
  hb_xfree(pW2);
#endif
}

/*
HMG_C_PUTFILE() -->
*/
HB_FUNC(HMG_C_PUTFILE) // JK JP
{
  TCHAR buffer[512];
  TCHAR cExt[4];
  auto iFilterIndex = 1;

#ifdef UNICODE
  LPWSTR pW, pW1, pW2;
  LPSTR pStr;
  auto j = 0;
  auto cont = 0;
  auto p = static_cast<char *>(hb_parc(1));
  TCHAR Filter[4096];
  memset(static_cast<void *>(&Filter), 0, sizeof(Filter));

  while (*p != '\0')
  {
    cont += strlen(p) + 1;
    if (cont < 4096)
    {
      lstrcpy(&Filter[j], AnsiToWide(p));
      j += lstrlen(AnsiToWide(p)) + 1;
      p += strlen(p) + 1;
    }
    else
    {
      break;
    }
  }
#endif

  DWORD flags = OFN_FILEMUSTEXIST | OFN_EXPLORER;

  if (hb_parl(4))
  {
    flags |= OFN_NOCHANGEDIR;
  }

  if (hb_parl(7))
  { // p.d. 12/05/2016
    flags |= OFN_OVERWRITEPROMPT;
  }

#ifndef UNICODE
  if (hb_parclen(5) > 0)
  {
    strcpy(buffer, hb_parc(5));
  }
  else
  {
    strcpy(buffer, "");
  }
#else
  pW = AnsiToWide(hb_parc(5));
  lstrcpy(buffer, pW);
  hb_xfree(pW);
#endif

  lstrcpy(cExt, "");

  if (hb_parni(6))
  {
    iFilterIndex = hb_parni(6);
  }

  OPENFILENAME ofn{};
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner = GetActiveWindow();
  void *str1;
  ofn.lpstrFilter = HB_PARSTR(1, &str1, nullptr);
  ofn.lpstrFile = buffer;
  void *str2;
  ofn.lpstrInitialDir = HB_PARSTR(3, &str2, nullptr);
  void *str3;
  ofn.lpstrTitle = HB_PARSTR(2, &str3, nullptr);
  ofn.nFilterIndex = iFilterIndex;
  ofn.nMaxFile = sizeof(buffer) / sizeof(TCHAR);
  ofn.Flags = flags;
  ofn.lpstrDefExt = cExt;

  if (GetSaveFileName(&ofn))
  {
    if (ofn.nFileExtension == 0)
    {
      ofn.lpstrFile = lstrcat(ofn.lpstrFile, ".");
      ofn.lpstrFile = lstrcat(ofn.lpstrFile, ofn.lpstrDefExt);
    }
    if (HB_ISBYREF(6))
    {
      hb_storni(ofn.nFilterIndex, 6);
    }

    HB_RETSTR(ofn.lpstrFile);
  }
  else
  {
    HB_RETSTR("");
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);

#ifdef UNICODE
  hb_xfree(pW1);
  hb_xfree(pW2);
#endif
}

static TCHAR s_szWinName[MAX_PATH + 1];

// JK HMG 1.0 Experimental Build 8
// --- callback function for C_BROWSEFORFOLDER(). Contributed By Andy Wos.

int CALLBACK BrowseCallbackProc(HWND hWnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
  TCHAR szPath[MAX_PATH];

  switch (uMsg)
  {
  case BFFM_INITIALIZED:
    if (lpData)
    {
      SendMessage(hWnd, BFFM_SETSELECTION, TRUE, lpData);
      SetWindowText(hWnd, static_cast<LPCTSTR>(s_szWinName));
    }
    break;
  case BFFM_VALIDATEFAILED:
    MessageBeep(MB_ICONHAND);
    return 1;
  case BFFM_SELCHANGED:
    if (lpData)
    {
      SHGetPathFromIDList(reinterpret_cast<LPITEMIDLIST>(lParam), szPath);
      SendMessage(hWnd, BFFM_SETSTATUSTEXT, 0, reinterpret_cast<LPARAM>(szPath));
    }
  }

  return 0;
}

/*
HMG_C_BROWSEFORFOLDER(HWND, cTitle, nFlags, np4, cp5) --> string
*/
HB_FUNC(HMG_C_BROWSEFORFOLDER) // Syntax:
                               // C_BROWSEFORFOLDER([<hWnd>],[<cTitle>],[<nFlags>],[<nFolderType>],[<cInitPath>])
{
  HWND hWnd = HB_ISNIL(1) ? GetActiveWindow() : hmg_par_HWND(1);

  if (HB_ISCHAR(5))
  {
    GetWindowText(hWnd, static_cast<LPTSTR>(s_szWinName), MAX_PATH);
  }

  LPITEMIDLIST pidlBrowse;
  SHGetSpecialFolderLocation(hWnd, HB_ISNIL(4) ? CSIDL_DRIVES : hb_parni(4), &pidlBrowse);

  TCHAR lpBuffer[MAX_PATH];

  BROWSEINFO BrowseInfo{};
  BrowseInfo.hwndOwner = hWnd;
  BrowseInfo.pidlRoot = pidlBrowse;
  BrowseInfo.pszDisplayName = lpBuffer;
  void *str1 = nullptr;
  BrowseInfo.lpszTitle = HB_ISNIL(2) ? "Select a Folder" : HB_PARSTR(2, &str1, nullptr);
  BrowseInfo.ulFlags = hb_parni(3) | (HB_ISCHAR(5) ? BIF_STATUSTEXT | BIF_RETURNONLYFSDIRS : 0);
  BrowseInfo.lpfn = BrowseCallbackProc;
  void *str2 = nullptr;
  BrowseInfo.lParam = HB_ISCHAR(5) ? reinterpret_cast<LPARAM>(HB_PARSTR(5, &str2, nullptr)) : 0;
  BrowseInfo.iImage = 0;

  pidlBrowse = SHBrowseForFolder(&BrowseInfo);

  if (pidlBrowse)
  {
    SHGetPathFromIDList(pidlBrowse, lpBuffer);
    HB_RETSTR(lpBuffer);
  }
  else
  {
    hb_retc("");
  }

  CoTaskMemFree(pidlBrowse);

  hb_strfree(str1);
  hb_strfree(str2);
}

/*
HMG_CHOOSECOLOR(HWND, nColor, ap3, np4) --> nColor|-1
*/
HB_FUNC(HMG_CHOOSECOLOR)
{
  COLORREF crCustClr[16];
  for (auto i = 0; i < 16; i++)
  {
    crCustClr[i] = (HB_ISARRAY(3) ? static_cast<COLORREF>(HB_PARVNL(3, i + 1)) : GetSysColor(COLOR_BTNFACE));
  }

  CHOOSECOLOR cc{};
  cc.lStructSize = sizeof(CHOOSECOLOR);
  cc.hwndOwner = HB_ISNIL(1) ? GetActiveWindow() : hmg_par_HWND(1);
  cc.rgbResult = static_cast<COLORREF>(HB_ISNIL(2) ? 0 : hb_parnl(2));
  cc.lpCustColors = crCustClr;
  cc.Flags = static_cast<WORD>(HB_ISNIL(4) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hb_parnl(4));

  if (!ChooseColor(&cc))
  {
    hb_retnl(-1);
  }
  else
  {
    hb_retnl(cc.rgbResult);
  }
}

/*
HMG_UNITSTOPIXELSX(nUnitsX) --> cx
*/
HB_FUNC(HMG_UNITSTOPIXELSX)
{
  hb_retni(MulDiv(hb_parni(1), LOWORD(GetDialogBaseUnits()), 4));
}

/*
HMG_UNITSTOPIXELSY(nUnitsY) --> cy
*/
HB_FUNC(HMG_UNITSTOPIXELSY)
{
  hb_retni(MulDiv(hb_parni(1), HIWORD(GetDialogBaseUnits()), 8));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CHOOSEFONT, HMG_CHOOSEFONT)
HB_FUNC_TRANSLATE(C_GETFILE, HMG_C_GETFILE)
HB_FUNC_TRANSLATE(C_PUTFILE, HMG_C_PUTFILE)
HB_FUNC_TRANSLATE(C_BROWSEFORFOLDER, HMG_C_BROWSEFORFOLDER)
HB_FUNC_TRANSLATE(CHOOSECOLOR, HMG_CHOOSECOLOR)
HB_FUNC_TRANSLATE(UNITSTOPIXELSX, HMG_UNITSTOPIXELSX)
HB_FUNC_TRANSLATE(UNITSTOPIXELSY, HMG_UNITSTOPIXELSY)
#endif
