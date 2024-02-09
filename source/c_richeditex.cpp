/*
 * HMG - Harbour Windows GUI library source code
 *
 * Copyright 2002-2017 Roberto Lopez <mail.box.hmg@gmail.com>
 * http://sites.google.com/site/hmgweb/
 *
 * Head of HMG project:
 *
 * 2002-2012 Roberto Lopez <mail.box.hmg@gmail.com>
 * http://sites.google.com/site/hmgweb/
 *
 * 2012-2017 Dr. Claudio Soto <srvet@adinet.com.uy>
 * http://srvet.blogspot.com
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
 * contained in this release of HMG.
 *
 * The exception is that, if you link the HMG library with other
 * files to produce an executable, this does not by itself cause the resulting
 * executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of linking the
 * HMG library code into it.
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
 * Copyright 2001-2008 Alexander S.Kresin <alex@kresin.ru>
 */

#include "mgdefs.hpp"
#include <commdlg.h>
#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4201) /* warning C4201: nonstandard extension used: nameless struct/union */
#endif
#include <richedit.h>
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
#include <shlwapi.h>

#define HB_PARNL3 hb_parvnl

#ifndef CP_UNICODE
#define CP_UNICODE 1200 // The text is UTF-16 (the WCHAR data type)
#endif

#ifdef UNICODE
LPSTR WideToAnsi(LPWSTR);
#endif

static HINSTANCE hRELib = nullptr;

HB_FUNC(HMG_INITRICHEDITBOXEX)
{
  auto hWnd = hmg_par_HWND(1);
  auto hMenu = hmg_par_HMENU(2);
  HWND hWndControl = nullptr;

  int style = ES_MULTILINE | ES_WANTRETURN | WS_CHILD | ES_NOHIDESEL;

  if (hb_parl(10))
  {
    style |= ES_READONLY;
  }

  if (!hb_parl(11))
  {
    style |= WS_VISIBLE;
  }

  if (!hb_parl(12))
  {
    style |= WS_TABSTOP;
  }

  if (!hb_parl(13))
  {
    style |= WS_HSCROLL;
  }

  style |= !hb_parl(14) ? WS_VSCROLL : ES_AUTOVSCROLL;

  if (!hRELib)
  {
    hRELib = LoadLibrary(TEXT("RichEd20.dll"));
  }

  if (hRELib)
  {
    hWndControl = CreateWindowEx(WS_EX_CLIENTEDGE, static_cast<LPCTSTR>(RICHEDIT_CLASS), TEXT(""), style, hb_parni(3),
                                 hb_parni(4), hb_parni(5), hb_parni(6), hWnd, hMenu, GetInstance(), nullptr);

    SendMessage(hWndControl, EM_LIMITTEXT, hmg_par_WPARAM(9), 0);
    SendMessage(hWndControl, EM_SETEVENTMASK, 0,
                ENM_CHANGE | ENM_SELCHANGE | ENM_PROTECTED | ENM_SCROLL | ENM_LINK | ENM_KEYEVENTS | ENM_REQUESTRESIZE |
                    ENM_MOUSEEVENTS);

    SendMessage(hWndControl, EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY);

    RegisterClipboardFormat(CF_RTF);
    RegisterClipboardFormat(CF_RETEXTOBJ);
  }

  hmg_ret_HWND(hWndControl);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITRICHEDITBOXEX, HMG_INITRICHEDITBOXEX)
#endif

HB_FUNC(HMG_UNLOADRICHEDITEXLIB)
{
  if (hRELib)
  {
    FreeLibrary(hRELib);
    hRELib = nullptr;
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(UNLOADRICHEDITEXLIB, HMG_UNLOADRICHEDITEXLIB)
#endif

DWORD CALLBACK EditStreamCallbackRead(DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb)
{
  auto hFile = reinterpret_cast<HANDLE>(dwCookie);

  if (ReadFile(hFile, static_cast<LPVOID>(lpBuff), static_cast<DWORD>(cb), reinterpret_cast<LPDWORD>(pcb), nullptr))
  {
    return 0;
  }
  else
  {
    return static_cast<DWORD>(-1);
  }
}

//        RichEditBox_StreamIn(hWndControl, cFileName, lSelection, nDataFormat)
HB_FUNC(HMG_RICHEDITBOX_STREAMIN)
{
  auto nDataFormat = hmg_par_LONG(4);
  LONG Format;
  switch (nDataFormat)
  {
  case 1:
    Format = SF_TEXT;
    break; // ANSI and UTF-8 with BOM
  case 2:
    Format = (CP_UTF8 << 16) | SF_USECODEPAGE | SF_TEXT;
    break; // ANSI and UTF-8 without BOM
  case 3:
    Format = SF_TEXT | SF_UNICODE;
    break; // UTF-16 LE
  case 4:
    Format = SF_RTF;
    break;
  case 5:
    Format = (CP_UTF8 << 16) | SF_USECODEPAGE | SF_RTF;
    break;
  default:
    Format = SF_RTF;
  }

  if (hmg_par_BOOL(3))
  {
    Format |= SFF_SELECTION;
  }

  void *str;
  HANDLE hFile = CreateFile(HB_PARSTR(2, &str, nullptr), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                            FILE_FLAG_SEQUENTIAL_SCAN, nullptr);
  hb_strfree(str);

  if (hFile == INVALID_HANDLE_VALUE)
  {
    hb_retl(false);
    return;
  }

  EDITSTREAM es;
  es.pfnCallback = EditStreamCallbackRead;
  es.dwCookie = reinterpret_cast<DWORD_PTR>(hFile);
  es.dwError = 0;
  SendMessage(hmg_par_HWND(1), EM_STREAMIN, Format, reinterpret_cast<LPARAM>(&es));
  CloseHandle(hFile);
  hb_retl(es.dwError ? false : true);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_STREAMIN, HMG_RICHEDITBOX_STREAMIN)
#endif

DWORD CALLBACK EditStreamCallbackWrite(DWORD_PTR dwCookie, LPBYTE lpBuff, LONG cb, LONG *pcb)
{
  auto hFile = reinterpret_cast<HANDLE>(dwCookie);

  if (WriteFile(hFile, static_cast<LPVOID>(lpBuff), static_cast<DWORD>(cb), reinterpret_cast<LPDWORD>(pcb), nullptr))
  {
    return 0;
  }
  else
  {
    return static_cast<DWORD>(-1);
  }
}

//        RichEditBox_StreamOut(hWndControl, cFileName, lSelection, nDataFormat)
HB_FUNC(HMG_RICHEDITBOX_STREAMOUT)
{
  auto nDataFormat = hmg_par_LONG(4);
  LONG Format;
  switch (nDataFormat)
  {
  case 1:
    Format = SF_TEXT;
    break; // ANSI and UTF-8 with BOM
  case 2:
    Format = (CP_UTF8 << 16) | SF_USECODEPAGE | SF_TEXT;
    break; // ANSI and UTF-8 without BOM
  case 3:
    Format = SF_TEXT | SF_UNICODE;
    break; // UTF-16 LE
  case 4:
    Format = SF_RTF;
    break;
  case 5:
    Format = (CP_UTF8 << 16) | SF_USECODEPAGE | SF_RTF;
    break;
  default:
    Format = SF_RTF;
  }

  if (hmg_par_BOOL(3))
  {
    Format = Format | SFF_SELECTION;
  }

  void *str;
  HANDLE hFile = CreateFile(HB_PARSTR(2, &str, nullptr), GENERIC_WRITE, FILE_SHARE_WRITE, nullptr, CREATE_ALWAYS,
                            FILE_ATTRIBUTE_NORMAL, nullptr);
  hb_strfree(str);

  if (hFile == INVALID_HANDLE_VALUE)
  {
    hb_retl(false);
    return;
  }

  EDITSTREAM es;
  es.pfnCallback = EditStreamCallbackWrite;
  es.dwCookie = reinterpret_cast<DWORD_PTR>(hFile);
  es.dwError = 0;
  SendMessage(hmg_par_HWND(1), EM_STREAMOUT, Format, reinterpret_cast<LPARAM>(&es));
  CloseHandle(hFile);
  hb_retl(es.dwError ? false : true);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_STREAMOUT, HMG_RICHEDITBOX_STREAMOUT)
#endif

//        RichEditBox_RTFLoadResourceFile(hWndControl, cFileName, lSelect)
HB_FUNC(HMG_RICHEDITBOX_RTFLOADRESOURCEFILE)
{
  void *str;
  HRSRC hResourceData = FindResource(nullptr, HB_PARSTR(2, &str, nullptr), TEXT("RTF"));
  hb_strfree(str);

  TCHAR *lpGlobalResource = nullptr;

  if (hResourceData != nullptr)
  {
    HGLOBAL hGlobalResource = LoadResource(nullptr, hResourceData);
    if (hGlobalResource != nullptr)
    {
      lpGlobalResource = reinterpret_cast<TCHAR *>(LockResource(hGlobalResource));
      if (lpGlobalResource != nullptr)
      {
        SETTEXTEX ST;
        ST.flags = hmg_par_BOOL(3) ? ST_SELECTION : ST_DEFAULT;
#ifdef UNICODE
        ST.codepage = CP_UNICODE;
#else
        ST.codepage = CP_ACP;
#endif
        SendMessage(hmg_par_HWND(1), EM_SETTEXTEX, (WPARAM)&ST, reinterpret_cast<LPARAM>(lpGlobalResource));
      }
      FreeResource(hGlobalResource);
    }
  }

  hb_retl((lpGlobalResource == nullptr) ? false : true);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_RTFLOADRESOURCEFILE, HMG_RICHEDITBOX_RTFLOADRESOURCEFILE)
#endif

//        RichEditBox_SetRTFTextMode(hWndControl, lRTF)
HB_FUNC(HMG_RICHEDITBOX_SETRTFTEXTMODE)
{
  BOOL lRTF = HB_ISLOG(2) ? hmg_par_BOOL(2) : TRUE;
  SendMessage(hmg_par_HWND(1), EM_SETTEXTMODE,
              (lRTF ? TM_RICHTEXT : TM_PLAINTEXT) | TM_MULTILEVELUNDO | TM_MULTICODEPAGE, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETRTFTEXTMODE, HMG_RICHEDITBOX_SETRTFTEXTMODE)
#endif

//        RichEditBox_IsRTFTextMode(hWndControl) --> return lRTF
HB_FUNC(HMG_RICHEDITBOX_ISRTFTEXTMODE)
{
  hb_retl(SendMessage(hmg_par_HWND(1), EM_GETTEXTMODE, 0, 0) & TM_RICHTEXT);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_ISRTFTEXTMODE, HMG_RICHEDITBOX_ISRTFTEXTMODE)
#endif

//        RichEditBox_SetAutoURLDetect(hWndControl, lLink)
HB_FUNC(HMG_RICHEDITBOX_SETAUTOURLDETECT)
{
  SendMessage(hmg_par_HWND(1), EM_AUTOURLDETECT, HB_ISLOG(2) ? hmg_par_BOOL(2) : TRUE, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETAUTOURLDETECT, HMG_RICHEDITBOX_SETAUTOURLDETECT)
#endif

//        RichEditBox_GetAutoURLDetect(hWndControl) --> return lLink
HB_FUNC(HMG_RICHEDITBOX_GETAUTOURLDETECT)
{
  hb_retl(SendMessage(hmg_par_HWND(1), EM_GETAUTOURLDETECT, 0, 0));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETAUTOURLDETECT, HMG_RICHEDITBOX_GETAUTOURLDETECT)
#endif

//        RichEditBox_SetBkgndColor(hWndControl, [aBkgndColor])
HB_FUNC(HMG_RICHEDITBOX_SETBKGNDCOLOR)
{
  auto hWndControl = hmg_par_HWND(1);

  if (HB_ISARRAY(2))
  {
    SendMessage(hWndControl, EM_SETBKGNDCOLOR, 0, RGB(HB_PARNI(2, 1), HB_PARNI(2, 2), HB_PARNI(2, 3)));
  }
  else
  {
    SendMessage(hWndControl, EM_SETBKGNDCOLOR, 1, 0); // Set to the window background system color
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETBKGNDCOLOR, HMG_RICHEDITBOX_SETBKGNDCOLOR)
#endif

//        RichEditBox_SetZoom(hWndControl, nNumerator, nDenominator)
HB_FUNC(HMG_RICHEDITBOX_SETZOOM) // ZoomRatio = nNumerator / nDenominator
{
  SendMessage(hmg_par_HWND(1), EM_SETZOOM, hmg_par_WPARAM(2),
              hb_parni(3)); //    1/64 < ZoomRatio < 64
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETZOOM, HMG_RICHEDITBOX_SETZOOM)
#endif

//        RichEditBox_GetZoom(hWndControl, @nNumerator, @nDenominator)
HB_FUNC(HMG_RICHEDITBOX_GETZOOM)
{
  int nNumerator, nDenominator;
  SendMessage(hmg_par_HWND(1), EM_GETZOOM, (WPARAM)&nNumerator, reinterpret_cast<LPARAM>(&nDenominator));
  if (HB_ISBYREF(2))
  {
    hb_storni(nNumerator, 2);
  }
  if (HB_ISBYREF(3))
  {
    hb_storni(nDenominator, 3);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETZOOM, HMG_RICHEDITBOX_GETZOOM)
#endif

//        RichEditBox_SetFont(hWndControl, cFontName, nFontSize, lBold, lItalic, lUnderline,
//        lStrikeout, aTextColor, aBackColor, nScript, lLink)
HB_FUNC(HMG_RICHEDITBOX_SETFONT)
{
  CHARFORMAT2 CharFormat2{};
  CharFormat2.cbSize = sizeof(CHARFORMAT2);

  DWORD Mask = 0;

  if (HB_ISCHAR(2))
  {
    Mask |= CFM_FACE;
    void *str;
    lstrcpy(CharFormat2.szFaceName, HB_PARSTR(2, &str, nullptr));
    hb_strfree(str);
  }

  if (HB_ISNUM(3) && hb_parnl(3))
  {
    Mask |= CFM_SIZE;
    CharFormat2.yHeight =
        hb_parnl(3) * 20 / 1; // yHeight (character height) is in twips (1/1440 of an inch or 1/20 of a printer point)
  }

  DWORD Effects = 0;

  if (HB_ISLOG(4))
  {
    Mask |= CFM_BOLD;
    if (hb_parl(4))
    {
      Effects |= CFE_BOLD;
    }
  }

  if (HB_ISLOG(5))
  {
    Mask |= CFM_ITALIC;
    if (hb_parl(5))
    {
      Effects |= CFE_ITALIC;
    }
  }

  if (HB_ISLOG(6))
  {
    Mask |= CFM_UNDERLINE;
    if (hb_parl(6))
    {
      Effects |= CFE_UNDERLINE;
    }
  }

  if (HB_ISLOG(7))
  {
    Mask |= CFM_STRIKEOUT;
    if (hb_parl(7))
    {
      Effects |= CFE_STRIKEOUT;
    }
  }

  if (HB_ISARRAY(8))
  {
    Mask |= CFM_COLOR;
    CharFormat2.crTextColor = RGB(HB_PARNI(8, 1), HB_PARNI(8, 2), HB_PARNI(8, 3));
  }
  else if (HB_ISNUM(8) && hb_parnl(8) == -1)
  {
    Mask |= CFM_COLOR;
    Effects |= CFE_AUTOCOLOR; // equivalent to GetSysColor(COLOR_WINDOWTEXT)
  }

  if (HB_ISARRAY(9))
  {
    Mask |= CFM_BACKCOLOR;
    CharFormat2.crBackColor = RGB(HB_PARNI(9, 1), HB_PARNI(9, 2), HB_PARNI(9, 3));
  }
  else if (HB_ISNUM(9) && hb_parnl(9) == -1)
  {
    Mask |= CFM_BACKCOLOR;
    Effects |= CFE_AUTOBACKCOLOR; // equivalent to GetSysColor(COLOR_WINDOW)
  }

  if (HB_ISNUM(10))
  {
    Mask |= CFM_SUBSCRIPT | CFM_SUPERSCRIPT; // The CFE_SUPERSCRIPT and CFE_SUBSCRIPT values are mutually exclusive
    if (hb_parnl(10) == 1)
    {
      Effects |= CFE_SUBSCRIPT;
    }
    if (hb_parnl(10) == 2)
    {
      Effects |= CFE_SUPERSCRIPT;
    }
  }

  if (HB_ISLOG(11))
  {
    Mask |= CFM_LINK;
    if (hb_parl(11))
    {
      Effects |= CFE_LINK;
    }
  }

  CharFormat2.dwMask = Mask;
  CharFormat2.dwEffects = Effects;

  hb_retl(SendMessage(hmg_par_HWND(1), EM_SETCHARFORMAT, SCF_SELECTION, reinterpret_cast<LPARAM>(&CharFormat2))
              ? true
              : false);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETFONT, HMG_RICHEDITBOX_SETFONT)
#endif

//        RichEditBox_GetFont(hWndControl, @cFontName, @nFontSize, @lBold, @lItalic, @lUnderline,
//        @lStrikeout, @aTextColor, @aBackColor, @nScript, @lLink)
HB_FUNC(HMG_RICHEDITBOX_GETFONT)
{
#ifdef UNICODE
  LPSTR pStr;
#endif

  CHARFORMAT2 CharFormat2{};
  CharFormat2.cbSize = sizeof(CHARFORMAT2);
  DWORD Mask = CFM_FACE | CFM_SIZE | CFM_BOLD | CFM_ITALIC | CFM_UNDERLINE | CFM_STRIKEOUT | CFM_COLOR | CFM_BACKCOLOR |
               CFM_SUBSCRIPT | CFM_SUPERSCRIPT | CFM_LINK;
  CharFormat2.dwMask = Mask;
  SendMessage(hmg_par_HWND(1), EM_GETCHARFORMAT, SCF_SELECTION, reinterpret_cast<LPARAM>(&CharFormat2));
  DWORD Effects = CharFormat2.dwEffects;

  if (HB_ISBYREF(2))
  {
#ifndef UNICODE
    hb_storc(CharFormat2.szFaceName, 2);
#else
    pStr = WideToAnsi(CharFormat2.szFaceName);
    hb_storc(pStr, 2);
    hb_xfree(pStr);
#endif
  }

  if (HB_ISBYREF(3))
  {
    hb_stornl((CharFormat2.yHeight * 1 / 20),
              3); // yHeight (character height) is in twips (1/1440 of an inch or 1/20 of a printer point)
  }

  if (HB_ISBYREF(4))
  {
    hb_storl((Effects & CFE_BOLD), 4);
  }

  if (HB_ISBYREF(5))
  {
    hb_storl((Effects & CFE_ITALIC), 5);
  }

  if (HB_ISBYREF(6))
  {
    hb_storl((Effects & CFE_UNDERLINE), 6);
  }

  if (HB_ISBYREF(7))
  {
    hb_storl((Effects & CFE_STRIKEOUT), 7);
  }

  if (HB_ISBYREF(8))
  {
    auto pArray = hb_param(8, Harbour::Item::ANY);
    hb_arrayNew(pArray, 3);
    hb_arraySetNL(pArray, 1, GetRValue(CharFormat2.crTextColor));
    hb_arraySetNL(pArray, 2, GetGValue(CharFormat2.crTextColor));
    hb_arraySetNL(pArray, 3, GetBValue(CharFormat2.crTextColor));
  }

  if (HB_ISBYREF(9))
  {
    auto pArray = hb_param(9, Harbour::Item::ANY);
    hb_arrayNew(pArray, 3);
    hb_arraySetNL(pArray, 1, GetRValue(CharFormat2.crBackColor));
    hb_arraySetNL(pArray, 2, GetGValue(CharFormat2.crBackColor));
    hb_arraySetNL(pArray, 3, GetBValue(CharFormat2.crBackColor));
  }

  if (HB_ISBYREF(10))
  {
    if (Effects & CFE_SUPERSCRIPT)
    {
      hb_stornl(2, 10);
    }
    else if (Effects & CFE_SUBSCRIPT)
    {
      hb_stornl(1, 10);
    }
    else
    {
      hb_stornl(0, 10);
    }
  }

  if (HB_ISBYREF(11))
  {
    hb_storl((Effects & CFE_LINK), 11);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETFONT, HMG_RICHEDITBOX_GETFONT)
#endif

//        RichEditBox_SetSelRange(hWndControl, {nMin, nMax})
HB_FUNC(HMG_RICHEDITBOX_SETSELRANGE)
{
  CHARRANGE CharRange;
  CharRange.cpMin = HB_PARVNL(2, 1);
  CharRange.cpMax = HB_PARVNL(2, 2);
  SendMessage(hmg_par_HWND(1), EM_EXSETSEL, 0, reinterpret_cast<LPARAM>(&CharRange));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETSELRANGE, HMG_RICHEDITBOX_SETSELRANGE)
#endif

//        RichEditBox_GetSelRange(hWndControl) --> return {nMin, nMax}
HB_FUNC(HMG_RICHEDITBOX_GETSELRANGE)
{
  CHARRANGE CharRange;
  SendMessage(hmg_par_HWND(1), EM_EXGETSEL, 0, reinterpret_cast<LPARAM>(&CharRange));
  hb_reta(2);
  HB_STORVNL(CharRange.cpMin, -1, 1);
  HB_STORVNL(CharRange.cpMax, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETSELRANGE, HMG_RICHEDITBOX_GETSELRANGE)
#endif

//        RichEditBox_ReplaceSel(hWndControl, cText)   ==   RichEditBox_SetText(hWndControl, .T.,
//        cText)
HB_FUNC(HMG_RICHEDITBOX_REPLACESEL)
{
  void *str;
  LPCTSTR cBuffer = HB_PARSTR(2, &str, nullptr);
  SendMessage(hmg_par_HWND(1), EM_REPLACESEL, TRUE, reinterpret_cast<LPARAM>(cBuffer));
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_REPLACESEL, HMG_RICHEDITBOX_REPLACESEL)
#endif

//        RichEditBox_SetText(hWndControl, lSelect, cText)
HB_FUNC(HMG_RICHEDITBOX_SETTEXT)
{
  void *str;
  LPCTSTR cBuffer = HB_PARSTR(3, &str, nullptr);
  SETTEXTEX ST;
  ST.flags = hmg_par_BOOL(2) ? ST_SELECTION : ST_DEFAULT;
#ifdef UNICODE
  ST.codepage = CP_UNICODE;
#else
  ST.codepage = CP_ACP;
#endif
  SendMessage(hmg_par_HWND(1), EM_SETTEXTEX, (WPARAM)&ST, reinterpret_cast<LPARAM>(cBuffer));
  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETTEXT, HMG_RICHEDITBOX_SETTEXT)
#endif

//        RichEditBox_GetText(hWndControl, lSelect)
HB_FUNC(HMG_RICHEDITBOX_GETTEXT)
{
  TCHAR cBuffer[4096];
  GETTEXTEX GT;
  GT.cb = sizeof(cBuffer);
  GT.flags = hmg_par_BOOL(2) ? GT_SELECTION : GT_DEFAULT;
#ifdef UNICODE
  GT.codepage = CP_UNICODE;
#else
  GT.codepage = CP_ACP;
#endif
  GT.lpDefaultChar = nullptr;
  GT.lpUsedDefChar = nullptr;
  SendMessage(hmg_par_HWND(1), EM_GETTEXTEX, (WPARAM)&GT, reinterpret_cast<LPARAM>(&cBuffer));
  HB_RETSTR(cBuffer);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETTEXT, HMG_RICHEDITBOX_GETTEXT)
#endif

//        RichEditBox_GetTextLength(hWndControl)
HB_FUNC(HMG_RICHEDITBOX_GETTEXTLENGTH)
{
  GETTEXTLENGTHEX GTL;
  GTL.flags = GTL_NUMCHARS;
#ifdef UNICODE
  GTL.codepage = CP_UNICODE;
#else
  GTL.codepage = CP_ACP;
#endif
  LONG nLength = SendMessage(hmg_par_HWND(1), EM_GETTEXTLENGTHEX, (WPARAM)&GTL, 0);
  hb_retnl(nLength);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETTEXTLENGTH, HMG_RICHEDITBOX_GETTEXTLENGTH)
#endif

//        RichEditBox_GetTextRange(hWndControl,  {nMin, nMax})
HB_FUNC(HMG_RICHEDITBOX_GETTEXTRANGE)
{
  TCHAR cBuffer[4096];
  TEXTRANGE TextRange;
  TextRange.lpstrText = cBuffer;
  TextRange.chrg.cpMin = HB_PARNL3(2, 1);
  TextRange.chrg.cpMax = HB_PARNL3(2, 2);
  SendMessage(hmg_par_HWND(1), EM_GETTEXTRANGE, 0, reinterpret_cast<LPARAM>(&TextRange));
  HB_RETSTR(TextRange.lpstrText);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETTEXTRANGE, HMG_RICHEDITBOX_GETTEXTRANGE)
#endif

//        RichEditBox_FindText(hWndControl, cFind, lDown, lMatchCase, lWholeWord, lSelectFindText)
HB_FUNC(HMG_RICHEDITBOX_FINDTEXT)
{
  auto hWndControl = hmg_par_HWND(1);
  BOOL Down = static_cast<BOOL>(HB_ISNIL(3) ? TRUE : hb_parl(3));
  BOOL MatchCase = static_cast<BOOL>(HB_ISNIL(4) ? FALSE : hb_parl(4));
  BOOL WholeWord = static_cast<BOOL>(HB_ISNIL(5) ? FALSE : hb_parl(5));
  BOOL SelectFindText = static_cast<BOOL>(HB_ISNIL(6) ? TRUE : hb_parl(6));

  auto Options = 0;

  if (Down)
  {
    Options |= FR_DOWN;
  }

  if (MatchCase)
  {
    Options |= FR_MATCHCASE;
  }

  if (WholeWord)
  {
    Options |= FR_WHOLEWORD;
  }

  CHARRANGE CharRange;
  SendMessage(hWndControl, EM_EXGETSEL, 0, reinterpret_cast<LPARAM>(&CharRange));

  if (Down)
  {
    CharRange.cpMin = CharRange.cpMax;
    CharRange.cpMax = -1;
  }
  else
  {
    CharRange.cpMin = CharRange.cpMin;
    CharRange.cpMax = 0;
  }

#ifdef UNICODE
  FINDTEXTEXW FindText;
#else
  FINDTEXTEX FindText;
#endif

  FindText.chrg = CharRange;
  void *str;
  FindText.lpstrText = const_cast<TCHAR *>(HB_PARSTR(2, &str, nullptr));

#ifdef UNICODE
  SendMessage(hWndControl, EM_FINDTEXTEXW, Options, reinterpret_cast<LPARAM>(&FindText));
#else
  SendMessage(hWndControl, EM_FINDTEXTEX, Options, reinterpret_cast<LPARAM>(&FindText));
#endif

  if (SelectFindText == FALSE)
  {
    FindText.chrgText.cpMin = FindText.chrgText.cpMax;
  }

  SendMessage(hWndControl, EM_EXSETSEL, 0, reinterpret_cast<LPARAM>(&FindText.chrgText));

  hb_reta(2);
  HB_STORVNL(FindText.chrgText.cpMin, -1, 1);
  HB_STORVNL(FindText.chrgText.cpMax, -1, 2);

  hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_FINDTEXT, HMG_RICHEDITBOX_FINDTEXT)
#endif

//        RichEditBox_SetParaFormat(hWndControl, nAlignment, nNumbering, nNumberingStyle,
//        nNumberingStart, ndOffset, ndLineSpacing, ndStartIndent)
HB_FUNC(HMG_RICHEDITBOX_SETPARAFORMAT)
{
  WORD Alignment = (HB_ISNIL(2) ? 0 : hb_parni(2));
  WORD Numbering = (HB_ISNIL(3) ? 0 : hb_parni(3));
  WORD NumberingStyle = (HB_ISNIL(4) ? 0 : hb_parni(4));
  WORD NumberingStart = (HB_ISNIL(5) ? 0 : hb_parni(5));
  double Offset = HB_ISNIL(6) ? 0.0 : hb_parnd(6);
  double LineSpacing = HB_ISNIL(7) ? 0.0 : hb_parnd(7);
  double StartIndent = HB_ISNIL(8) ? 0.0 : hb_parnd(8);

  DWORD Mask = 0;

  PARAFORMAT2 ParaFormat2{};
  ParaFormat2.cbSize = sizeof(PARAFORMAT2);

  if (Alignment > 0)
  {
    Mask |= PFM_ALIGNMENT;
    switch (Alignment)
    {
    case 1:
      ParaFormat2.wAlignment = PFA_LEFT;
      break;
    case 2:
      ParaFormat2.wAlignment = PFA_RIGHT;
      break;
    case 3:
      ParaFormat2.wAlignment = PFA_CENTER;
      break;
    case 4:
      ParaFormat2.wAlignment = PFA_JUSTIFY;
      break;

    default:
      ParaFormat2.wAlignment = PFA_LEFT;
      break;
    }
  }

  if (Numbering > 0)
  {
    Mask |= PFM_NUMBERING;
    switch (Numbering)
    {
    case 1:
      ParaFormat2.wNumbering = 0;
      break; // No paragraph numbering or bullets
    case 2:
      ParaFormat2.wNumbering = PFN_BULLET;
      break; // Insert a bullet at the beginning of each selected paragraph
    case 3:
      ParaFormat2.wNumbering = PFN_ARABIC;
      break; // Use Arabic numbers          ( 0,  1,   2, ... )
    case 4:
      ParaFormat2.wNumbering = PFN_LCLETTER;
      break; // Use lowercase letters       ( a,  b,   c, ... )
    case 5:
      ParaFormat2.wNumbering = PFN_LCROMAN;
      break; // Use lowercase Roman letters ( i, ii, iii, ... )
    case 6:
      ParaFormat2.wNumbering = PFN_UCLETTER;
      break; // Use uppercase letters       ( A,  B,   C, ... )
    case 7:
      ParaFormat2.wNumbering = PFN_UCROMAN;
      break; // Use uppercase Roman letters ( I, II, III, ... )
    case 8:
      ParaFormat2.wNumbering = 7;
      break; // Uses a sequence of characters beginning with the Unicode character specified
    // by the nNumberingStart
    default:
      ParaFormat2.wNumbering = 0;
      break;
    }
  }

  if (NumberingStyle > 0)
  {
    Mask |= PFM_NUMBERINGSTYLE;
    switch (NumberingStyle)
    {
    case 1:
      ParaFormat2.wNumberingStyle = PFNS_PAREN;
      break; // Follows the number with a right parenthesis.
    case 2:
      ParaFormat2.wNumberingStyle = PFNS_PARENS;
      break; // Encloses the number in parentheses
    case 3:
      ParaFormat2.wNumberingStyle = PFNS_PERIOD;
      break; // Follows the number with a period
    case 4:
      ParaFormat2.wNumberingStyle = PFNS_PLAIN;
      break; // Displays only the number
    case 5:
      ParaFormat2.wNumberingStyle = PFNS_NONUMBER;
      break; // Continues a numbered list without applying the next number or bullet
    case 6:
      ParaFormat2.wNumberingStyle = PFNS_NEWNUMBER;
      break; // Starts a new number with nNumberingStart

    default:
      ParaFormat2.wNumberingStyle = 0;
      break;
    }
  }

  if (HB_ISNUM(5))
  {
    Mask |= PFM_NUMBERINGSTART;
    ParaFormat2.wNumberingStart = NumberingStart;
  }

  if (HB_ISNUM(6))
  {
    Mask |= PFM_OFFSET;
    ParaFormat2.dxOffset = static_cast<LONG>(
        static_cast<double>(Offset * 1440.0 / 25.4)); // Offset is in millimeters ( 1 inch = 25.4 mm = 1440 twips )
  }

  if (LineSpacing > 0.0)
  {
    Mask |= PFM_LINESPACING;
    ParaFormat2.bLineSpacingRule = 5; // Spacing from one line to the next, 20 twips = single-spaced text
    ParaFormat2.dyLineSpacing = static_cast<LONG>(static_cast<double>(LineSpacing * 20.0 / 1.0));
  }

  if (HB_ISNUM(8))
  {
    Mask |= PFM_STARTINDENT;
    ParaFormat2.dxStartIndent = static_cast<LONG>(static_cast<double>(
        StartIndent * 1440.0 / 25.4)); // StartIndent is in millimeters ( 1 inch = 25.4 mm = 1440 twips )
  }

  ParaFormat2.dwMask = Mask;
  SendMessage(hmg_par_HWND(1), EM_SETPARAFORMAT, 0, reinterpret_cast<LPARAM>(&ParaFormat2));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETPARAFORMAT, HMG_RICHEDITBOX_SETPARAFORMAT)
#endif

//        RichEditBox_GetParaFormat(hWndControl, @nAlignment, @nNumbering, @nNumberingStyle,
//        @nNumberingStart, @Offset, @ndLineSpacing, @ndStartIndent)
HB_FUNC(HMG_RICHEDITBOX_GETPARAFORMAT)
{
  WORD Alignment = 0;
  WORD Numbering = 0;
  WORD NumberingStyle;
  WORD NumberingStart;
  double Offset;
  double LineSpacing = 0.0;
  double StartIndent;

  PARAFORMAT2 ParaFormat2{};
  ParaFormat2.cbSize = sizeof(PARAFORMAT2);
  ParaFormat2.dwMask = PFM_ALIGNMENT | PFM_NUMBERING | PFM_NUMBERINGSTYLE | PFM_NUMBERINGSTART | PFM_LINESPACING |
                       PFM_STARTINDENT | PFM_OFFSET;

  SendMessage(hmg_par_HWND(1), EM_GETPARAFORMAT, 0, reinterpret_cast<LPARAM>(&ParaFormat2));

  if (HB_ISBYREF(2))
  {
    switch (ParaFormat2.wAlignment)
    {
    case PFA_LEFT:
      Alignment = 1;
      break;
    case PFA_RIGHT:
      Alignment = 2;
      break;
    case PFA_CENTER:
      Alignment = 3;
      break;
    case PFA_JUSTIFY:
      Alignment = 4;
    }
    hb_stornl(Alignment, 2);
  }

  if (HB_ISBYREF(3))
  {
    switch (ParaFormat2.wNumbering)
    {
    case 0:
      Numbering = 1;
      break;
    case PFN_BULLET:
      Numbering = 2;
      break;
    case PFN_ARABIC:
      Numbering = 3;
      break;
    case PFN_LCLETTER:
      Numbering = 4;
      break;
    case PFN_LCROMAN:
      Numbering = 5;
      break;
    case PFN_UCLETTER:
      Numbering = 6;
      break;
    case PFN_UCROMAN:
      Numbering = 7;
      break;
    case 7:
      Numbering = 8;
    }
    hb_stornl(Numbering, 3);
  }

  if (HB_ISBYREF(4))
  {
    switch (ParaFormat2.wNumberingStyle)
    {
    case PFNS_PAREN:
      NumberingStyle = 1;
      break;
    case PFNS_PARENS:
      NumberingStyle = 2;
      break;
    case PFNS_PERIOD:
      NumberingStyle = 3;
      break;
    case PFNS_PLAIN:
      NumberingStyle = 4;
      break;
    case PFNS_NONUMBER:
      NumberingStyle = 5;
      break;
    case PFNS_NEWNUMBER:
      NumberingStyle = 6;
      break;
    default:
      NumberingStyle = 0;
    }
    hb_stornl(NumberingStyle, 4);
  }

  if (HB_ISBYREF(5))
  {
    NumberingStart = ParaFormat2.wNumberingStart;
    hb_stornl(NumberingStart, 5);
  }

  if (HB_ISBYREF(6))
  {
    Offset = static_cast<double>(ParaFormat2.dxOffset);
    hb_stornd(static_cast<double>(Offset * 25.4 / 1440.0), 6);
  }

  if (HB_ISBYREF(7))
  {
    switch (ParaFormat2.bLineSpacingRule)
    {
    case 0:
      LineSpacing = 1.0;
      break;
    case 1:
      LineSpacing = 1.5;
      break;
    case 2:
      LineSpacing = 2.0;
      break;
    case 3:
      LineSpacing = (static_cast<double>(ParaFormat2.dyLineSpacing)) * -1.0; // if < 0 is in twips
      break;
    case 4:
      LineSpacing = (static_cast<double>(ParaFormat2.dyLineSpacing)) * -1.0; // if < 0 is in twips
      break;
    case 5:
      LineSpacing = (static_cast<double>(ParaFormat2.dyLineSpacing)) * 1.0 / 20.0;
    }
    hb_stornd(static_cast<double>(LineSpacing), 7);
  }

  if (HB_ISBYREF(8))
  {
    StartIndent = static_cast<double>(ParaFormat2.dxStartIndent);
    hb_stornd(static_cast<double>(StartIndent * 25.4 / 1440.0), 8);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETPARAFORMAT, HMG_RICHEDITBOX_GETPARAFORMAT)
#endif

HB_FUNC(HMG_RICHEDITBOX_SELCOPY)
{
  SendMessage(hmg_par_HWND(1), WM_COPY, 0,
              0); // copy the current selection to the clipboard in CF_TEXT format
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SELCOPY, HMG_RICHEDITBOX_SELCOPY)
#endif

HB_FUNC(HMG_RICHEDITBOX_SELPASTE)
{
  SendMessage(hmg_par_HWND(1), WM_PASTE, 0,
              0); // copy the current content of the clipboard at the current caret position,
} // data is inserted only if the clipboard contains data in CF_TEXT format

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SELPASTE, HMG_RICHEDITBOX_SELPASTE)
#endif

HB_FUNC(HMG_RICHEDITBOX_SELCUT)
{
  SendMessage(hmg_par_HWND(1), WM_CUT, 0,
              0); // delete (cut) the current selection and place the deleted content on the clipboard
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SELCUT, HMG_RICHEDITBOX_SELCUT)
#endif

HB_FUNC(HMG_RICHEDITBOX_SELCLEAR)
{
  SendMessage(hmg_par_HWND(1), WM_CLEAR, 0, 0); // delete (cut) the current selection
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SELCLEAR, HMG_RICHEDITBOX_SELCLEAR)
#endif

HB_FUNC(HMG_RICHEDITBOX_CHANGEUNDO)
{
  SendMessage(hmg_par_HWND(1), EM_UNDO, 0, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CHANGEUNDO, HMG_RICHEDITBOX_CHANGEUNDO)
#endif

HB_FUNC(HMG_RICHEDITBOX_CHANGEREDO)
{
  SendMessage(hmg_par_HWND(1), EM_REDO, 0, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CHANGEREDO, HMG_RICHEDITBOX_CHANGEREDO)
#endif

HB_FUNC(HMG_RICHEDITBOX_CLEARUNDOBUFFER)
{
  SendMessage(hmg_par_HWND(1), EM_EMPTYUNDOBUFFER, 0, 0);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CLEARUNDOBUFFER, HMG_RICHEDITBOX_CLEARUNDOBUFFER)
#endif

HB_FUNC(HMG_RICHEDITBOX_CANPASTE)
{
  hb_retl(SendMessage(hmg_par_HWND(1), EM_CANPASTE, 0, 0));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CANPASTE, HMG_RICHEDITBOX_CANPASTE)
#endif

HB_FUNC(HMG_RICHEDITBOX_CANUNDO)
{
  hb_retl(SendMessage(hmg_par_HWND(1), EM_CANUNDO, 0, 0));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CANUNDO, HMG_RICHEDITBOX_CANUNDO)
#endif

HB_FUNC(HMG_RICHEDITBOX_CANREDO)
{
  hb_retl(SendMessage(hmg_par_HWND(1), EM_CANREDO, 0, 0));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_CANREDO, HMG_RICHEDITBOX_CANREDO)
#endif

//        RichEditBox_GetRect(hWndControl) --> { nLeft, nTop, nRight, nBottom }
HB_FUNC(HMG_RICHEDITBOX_GETRECT)
{
  RECT rc;
  SendMessage(hmg_par_HWND(1), EM_GETRECT, 0, reinterpret_cast<LPARAM>(&rc));
  hb_reta(4);
  HB_STORNI(rc.left, -1, 1);
  HB_STORNI(rc.top, -1, 2);
  HB_STORNI(rc.right, -1, 3);
  HB_STORNI(rc.bottom, -1, 4);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_GETRECT, HMG_RICHEDITBOX_GETRECT)
#endif

//        RichEditBox_SetRect(hWndControl, {nLeft, nTop, nRight, nBottom})
HB_FUNC(HMG_RICHEDITBOX_SETRECT)
{
  RECT rc;
  rc.left = HB_PARNI(2, 1);
  rc.top = HB_PARNI(2, 2);
  rc.right = HB_PARNI(2, 3);
  rc.bottom = HB_PARNI(2, 4);
  SendMessage(hmg_par_HWND(1), EM_SETRECT, 1, reinterpret_cast<LPARAM>(&rc));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETRECT, HMG_RICHEDITBOX_SETRECT)
#endif

//        RichEditBox_PastEspecial(hWndControl, ClipboardFormat)
HB_FUNC(HMG_RICHEDITBOX_PASTESPECIAL) // Paste a specific clipboard format in a rich edit control
{
  auto hWndControl = hmg_par_HWND(1);

  if (HB_ISCHAR(2))
  {
    CHAR *ClipboardFormat = const_cast<CHAR *>(hb_parc(2));
    SendMessage(hWndControl, EM_PASTESPECIAL, (WPARAM)ClipboardFormat, reinterpret_cast<LPARAM>(nullptr));
  }
  else
  {
    WPARAM ClipboardFormat = (WPARAM)hb_parnl(2);
    SendMessage(hWndControl, EM_PASTESPECIAL, ClipboardFormat, reinterpret_cast<LPARAM>(nullptr));
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_PASTESPECIAL, HMG_RICHEDITBOX_PASTESPECIAL)
#endif

//        RichEditBox_SetMargins(hWndControl, LeftMargin, RightMargin)
HB_FUNC(HMG_RICHEDITBOX_SETMARGINS)
{
  SendMessage(hmg_par_HWND(1), EM_SETMARGINS, EC_USEFONTINFO, MAKELPARAM(hmg_par_WORD(2), hmg_par_WORD(3)));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_SETMARGINS, HMG_RICHEDITBOX_SETMARGINS)
#endif

//        RichEditBox_FormatRange(hWndControl, hDCPrinter, nLeft, nTop, nRight, nBottom, {cpMin,
//        cpMax})
HB_FUNC(HMG_RICHEDITBOX_FORMATRANGE)
{
  RECT rc;
  rc.left = hb_parni(3);   // in twips
  rc.top = hb_parni(4);    // in twips
  rc.right = hb_parni(5);  // in twips
  rc.bottom = hb_parni(6); // in twips

  auto hDCPrinter = hmg_par_HDC(2);

  FORMATRANGE FormatRange;
  FormatRange.hdc = hDCPrinter;
  FormatRange.hdcTarget = hDCPrinter;
  FormatRange.rc = rc;
  FormatRange.rcPage = rc;
  FormatRange.chrg.cpMin = HB_PARNL3(7, 1);
  FormatRange.chrg.cpMax = HB_PARNL3(7, 2);

  auto hWndControl = hmg_par_HWND(1);
  LONG cpMin = SendMessage(hWndControl, EM_FORMATRANGE, TRUE, reinterpret_cast<LPARAM>(&FormatRange));
  SendMessage(hWndControl, EM_FORMATRANGE, FALSE, reinterpret_cast<LPARAM>(nullptr));
  hb_retnl(cpMin);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_FORMATRANGE, HMG_RICHEDITBOX_FORMATRANGE)
#endif

//        RichEditBox_PosFromChar(hWndControl, nPosChar)   return --> { nRowScreen, nColScreen } or
//        { -1, -1 } if character is not displayed
HB_FUNC(HMG_RICHEDITBOX_POSFROMCHAR)
{
  auto hWndControl = hmg_par_HWND(1);
  POINTL PointL;
  auto nPosChar = hmg_par_LONG(2);

  SendMessage(hWndControl, EM_POSFROMCHAR, (WPARAM)&PointL,
              nPosChar); // Retrieves the client area coordinates of
                         // a specified character in an edit control
  POINT Point;

  // A returned coordinate can be a negative value if the specified character is not displayed in
  // the edit control's client area
  if (PointL.y < 0 || PointL.x < 0)
  {
    Point.y = -1;
    Point.x = -1;
  }
  else
  {
    Point.x = PointL.x;
    Point.y = PointL.y;
    ClientToScreen(hWndControl, &Point);
  }

  hb_reta(2);
  HB_STORNI(Point.y, -1, 1);
  HB_STORNI(Point.x, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(RICHEDITBOX_POSFROMCHAR, HMG_RICHEDITBOX_POSFROMCHAR)
#endif

//********************************************************************
// by Dr. Claudio Soto ( January 2014 )
//********************************************************************

static TCHAR cFindWhat[1024];
static TCHAR cReplaceWith[1024];
static FINDREPLACE FindReplace;
static HWND hDlgFindReplace = nullptr;

HB_FUNC(HMG_REGISTERFINDMSGSTRING)
{
  UINT MessageID = RegisterWindowMessage(FINDMSGSTRING);

  hb_retnl(MessageID);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(REGISTERFINDMSGSTRING, HMG_REGISTERFINDMSGSTRING)
#endif

HB_FUNC(HMG_FINDREPLACEDLG)
{
  HWND hWnd = HB_ISNIL(1) ? GetActiveWindow() : hmg_par_HWND(1);
  BOOL NoUpDown = static_cast<BOOL>(HB_ISNIL(2) ? FALSE : hb_parl(2));
  BOOL NoMatchCase = static_cast<BOOL>(HB_ISNIL(3) ? FALSE : hb_parl(3));
  BOOL NoWholeWord = static_cast<BOOL>(HB_ISNIL(4) ? FALSE : hb_parl(4));
  BOOL CheckDown = static_cast<BOOL>(HB_ISNIL(5) ? TRUE : hb_parl(5));
  BOOL CheckMatchCase = static_cast<BOOL>(HB_ISNIL(6) ? FALSE : hb_parl(6));
  BOOL CheckWholeWord = static_cast<BOOL>(HB_ISNIL(7) ? FALSE : hb_parl(7));
  auto lReplace = hmg_par_BOOL(10);

  void *str1;
  void *str2;
  void *str3;
  LPCTSTR FindWhat = HB_PARSTR(8, &str1, nullptr);
  LPCTSTR ReplaceWith = HB_PARSTR(9, &str2, nullptr);
  LPCTSTR cTitle = HB_PARSTR(11, &str3, nullptr);

  if (hDlgFindReplace == nullptr)
  {
    ZeroMemory(&FindReplace, sizeof(FindReplace));

    lstrcpy(cFindWhat, FindWhat);
    lstrcpy(cReplaceWith, ReplaceWith);

    FindReplace.lStructSize = sizeof(FindReplace);
    FindReplace.Flags = (NoUpDown ? FR_HIDEUPDOWN : 0) | (NoMatchCase ? FR_HIDEMATCHCASE : 0) |
                        (NoWholeWord ? FR_HIDEWHOLEWORD : 0) | (CheckDown ? FR_DOWN : 0) |
                        (CheckMatchCase ? FR_MATCHCASE : 0) | (CheckWholeWord ? FR_WHOLEWORD : 0);
    FindReplace.hwndOwner = hWnd;
    FindReplace.lpstrFindWhat = cFindWhat;
    FindReplace.wFindWhatLen = sizeof(cFindWhat) / sizeof(TCHAR);
    FindReplace.lpstrReplaceWith = cReplaceWith;
    FindReplace.wReplaceWithLen = sizeof(cReplaceWith) / sizeof(TCHAR);

    if (lReplace)
    {
      hDlgFindReplace = ReplaceText(&FindReplace);
    }
    else
    {
      hDlgFindReplace = FindText(&FindReplace);
    }

    if (HB_ISCHAR(11))
    {
      SetWindowText(hDlgFindReplace, cTitle);
    }

    ShowWindow(hDlgFindReplace, SW_SHOW);
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLG, HMG_FINDREPLACEDLG)
#endif

HB_FUNC(HMG_FINDREPLACEDLGSETTITLE)
{
  if (hDlgFindReplace != nullptr)
  {
    void *str;
    SetWindowText(hDlgFindReplace, HB_PARSTR(1, &str, nullptr));
    hb_strfree(str);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGSETTITLE, HMG_FINDREPLACEDLGSETTITLE)
#endif

HB_FUNC(HMG_FINDREPLACEDLGGETTITLE)
{
  TCHAR cTitle[256];

  if (hDlgFindReplace != nullptr)
  {
    GetWindowText(hDlgFindReplace, cTitle, sizeof(cTitle) / sizeof(TCHAR));
    HB_RETSTR(cTitle);
  }
  else
  {
    hb_retc("");
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGGETTITLE, HMG_FINDREPLACEDLGGETTITLE)
#endif

HB_FUNC(HMG_FINDREPLACEDLGSHOW)
{
  BOOL lShow = HB_ISNIL(1) ? TRUE : hb_parl(1);

  if (hDlgFindReplace != nullptr)
  {
    ShowWindow(hDlgFindReplace, lShow ? SW_SHOW : SW_HIDE);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGSHOW, HMG_FINDREPLACEDLGSHOW)
#endif

HB_FUNC(HMG_FINDREPLACEDLGGETHANDLE)
{
  hmg_ret_HWND(hDlgFindReplace);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGGETHANDLE, HMG_FINDREPLACEDLGGETHANDLE)
#endif

HB_FUNC(HMG_FINDREPLACEDLGISRELEASE)
{
  hb_retl((hDlgFindReplace == nullptr));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGISRELEASE, HMG_FINDREPLACEDLGISRELEASE)
#endif

HB_FUNC(HMG_FINDREPLACEDLGRELEASE)
{
  BOOL lDestroy = HB_ISNIL(1) ? TRUE : hb_parl(1);

  if (hDlgFindReplace != nullptr && lDestroy)
  {
    DestroyWindow(hDlgFindReplace);
  }
  hDlgFindReplace = nullptr;
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGRELEASE, HMG_FINDREPLACEDLGRELEASE)
#endif

HB_FUNC(HMG_FINDREPLACEDLGGETOPTIONS)
{
  LPARAM lParam = HB_PARNL(1);
  FINDREPLACE *FR = (FINDREPLACE *)lParam;
  LONG nRet = -1;

  if (FR->Flags & FR_DIALOGTERM)
  {
    nRet = 0;
  }

  if (FR->Flags & FR_FINDNEXT)
  {
    nRet = 1;
  }

  if (FR->Flags & FR_REPLACE)
  {
    nRet = 2;
  }

  if (FR->Flags & FR_REPLACEALL)
  {
    nRet = 3;
  }

  auto pArray = hb_itemArrayNew(6);
  hb_arraySetNL(pArray, 1, nRet);
  HB_ARRAYSETSTR(pArray, 2, FR->lpstrFindWhat);
  HB_ARRAYSETSTR(pArray, 3, FR->lpstrReplaceWith);
  hb_arraySetL(pArray, 4, (FR->Flags & FR_DOWN));
  hb_arraySetL(pArray, 5, (FR->Flags & FR_MATCHCASE));
  hb_arraySetL(pArray, 6, (FR->Flags & FR_WHOLEWORD));
  hb_itemReturnRelease(pArray);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(FINDREPLACEDLGGETOPTIONS, HMG_FINDREPLACEDLGGETOPTIONS)
#endif
