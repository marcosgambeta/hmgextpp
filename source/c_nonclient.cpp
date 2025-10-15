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

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif

// Grigory Filatov <gfilatov@gmail.com> HMG 18.05

HB_FUNC(HMG_GETNONCLIENT)
{
  NONCLIENTMETRICS ncm{};
  ncm.cbSize = sizeof(NONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);

  hb_reta(7);
  HB_STORVNL(ncm.iBorderWidth, -1, 1);
  HB_STORVNL(ncm.iScrollWidth, -1, 2);
  HB_STORVNL(ncm.iScrollHeight, -1, 3);
  HB_STORVNL(ncm.iCaptionWidth, -1, 4);
  HB_STORVNL(ncm.iCaptionHeight, -1, 5);
  HB_STORVNL(ncm.iMenuWidth, -1, 6);
  HB_STORVNL(ncm.iMenuHeight, -1, 7);
}

HB_FUNC(HMG_GETNONCLIENTFONT)
{
#ifdef UNICODE
  LPSTR pStr;
#endif
  NONCLIENTMETRICS ncm{};
  ncm.cbSize = sizeof(NONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);

  hb_reta(4);

  switch (hb_parni(1)) {
  case 1: {
#ifndef UNICODE
    HB_STORC(ncm.lfCaptionFont.lfFaceName, -1, 1);
#else
    pStr = WideToAnsi(ncm.lfCaptionFont.lfFaceName);
    HB_STORC(pStr, -1, 1);
    hb_xfree(pStr);
#endif
    HB_STORNI(static_cast<int>(-0.75) * ncm.lfCaptionFont.lfHeight, -1, 2);
    HB_STORL(ncm.lfCaptionFont.lfWeight == 700 ? TRUE : FALSE, -1, 3);
    HB_STORNI(ncm.lfCaptionFont.lfCharSet, -1, 4);
    break;
  }
  case 2: {
#ifndef UNICODE
    HB_STORC(ncm.lfMenuFont.lfFaceName, -1, 1);
#else
    pStr = WideToAnsi(ncm.lfMenuFont.lfFaceName);
    HB_STORC(pStr, -1, 1);
    hb_xfree(pStr);
#endif
    HB_STORNI(static_cast<int>(-0.75) * ncm.lfMenuFont.lfHeight, -1, 2);
    HB_STORL(ncm.lfMenuFont.lfWeight == 700 ? TRUE : FALSE, -1, 3);
    HB_STORNI(ncm.lfMenuFont.lfCharSet, -1, 4);
    break;
  }
  case 3: {
#ifndef UNICODE
    HB_STORC(ncm.lfStatusFont.lfFaceName, -1, 1);
#else
    pStr = WideToAnsi(ncm.lfStatusFont.lfFaceName);
    HB_STORC(pStr, -1, 1);
    hb_xfree(pStr);
#endif
    HB_STORNI(static_cast<int>(-0.75) * ncm.lfStatusFont.lfHeight, -1, 2);
    HB_STORL(ncm.lfStatusFont.lfWeight == 700 ? TRUE : FALSE, -1, 3);
    HB_STORNI(ncm.lfStatusFont.lfCharSet, -1, 4);
    break;
  }
  case 4: {
#ifndef UNICODE
    HB_STORC(ncm.lfMessageFont.lfFaceName, -1, 1);
#else
    pStr = WideToAnsi(ncm.lfMessageFont.lfFaceName);
    HB_STORC(pStr, -1, 1);
    hb_xfree(pStr);
#endif
    HB_STORNI(static_cast<int>(-0.75) * ncm.lfMessageFont.lfHeight, -1, 2);
    HB_STORL(ncm.lfMessageFont.lfWeight == 700 ? TRUE : FALSE, -1, 3);
    HB_STORNI(ncm.lfMessageFont.lfCharSet, -1, 4);
    break; // TODO: unnecessary break
  }
  }
}

HB_FUNC(HMG_SETNONCLIENT)
{
  NONCLIENTMETRICS ncm{};
  ncm.cbSize = sizeof(NONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);

  switch (hb_parni(1)) {
  case 1: {
    ncm.iBorderWidth = HB_MIN(50, HB_MAX(1, hb_parni(2)));
    break;
  }
  case 2: {
    ncm.iScrollWidth = HB_MIN(100, HB_MAX(8, hb_parni(2)));
    ncm.iScrollHeight = HB_MIN(100, HB_MAX(8, hb_parni(2)));
    break;
  }
  case 3: {
    ncm.iCaptionWidth = HB_MIN(100, HB_MAX(17, hb_parni(2)));
    break;
  }
  case 4: {
    ncm.iCaptionHeight = HB_MIN(100, HB_MAX(17, hb_parni(2)));
    break;
  }
  case 5: {
    ncm.iMenuWidth = HB_MIN(100, HB_MAX(17, hb_parni(2)));
    ncm.iMenuHeight = HB_MIN(100, HB_MAX(17, hb_parni(2)));
    break; // TODO: unnecessary break
  }
  }

  SystemParametersInfo(SPI_SETNONCLIENTMETRICS, sizeof(ncm), &ncm, 0);
}

HB_FUNC(HMG_SETNONCLIENTFONT)
{
#ifdef UNICODE
  LPWSTR pWStr;
#endif
  auto hDC = GetDC(HWND_DESKTOP);

  NONCLIENTMETRICS ncm{};
  ncm.cbSize = sizeof(NONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);

  LOGFONT lf{};
#ifdef UNICODE
  pWStr = AnsiToWide(hb_parc(2));
  lstrcpy(lf.lfFaceName, pWStr);
  hb_xfree(pWStr);
#else
  lstrcpy(lf.lfFaceName, hb_parc(2));
#endif
  lf.lfHeight = -MulDiv(hb_parni(3), GetDeviceCaps(hDC, LOGPIXELSY), 72);
  lf.lfWeight = (HB_ISLOG(4) && hb_parl(4)) ? 700 : 400;
  lf.lfCharSet = static_cast<BYTE>(HB_ISNIL(5) ? 0 : hb_parni(5));

  switch (hb_parni(1)) {
  case 1: {
    ncm.lfCaptionFont = lf;
    break;
  }
  case 2: {
    ncm.lfMenuFont = lf;
    break;
  }
  case 3: {
    ncm.lfStatusFont = lf;
    break;
  }
  case 4: {
    ncm.lfMessageFont = lf;
    break; // TODO: unnecessary break
  }
  }

  SystemParametersInfo(SPI_SETNONCLIENTMETRICS, sizeof(ncm), &ncm, 0);

  ReleaseDC(HWND_DESKTOP, hDC);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETNONCLIENT, HMG_GETNONCLIENT)
HB_FUNC_TRANSLATE(GETNONCLIENTFONT, HMG_GETNONCLIENTFONT)
HB_FUNC_TRANSLATE(SETNONCLIENT, HMG_SETNONCLIENT)
HB_FUNC_TRANSLATE(SETNONCLIENTFONT, HMG_SETNONCLIENTFONT)
#endif
