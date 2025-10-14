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
#include <commctrl.h>

HB_FUNC(HMG_INITIPADDRESS)
{
  INITCOMMONCONTROLSEX icex;
  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC = ICC_INTERNET_CLASSES;
  InitCommonControlsEx(&icex);

  DWORD style = WS_CHILD;

  if (!hb_parl(7)) {
    style |= WS_VISIBLE;
  }

  if (!hb_parl(8)) {
    style |= WS_TABSTOP;
  }

  auto hIpAddress = CreateWindowEx(WS_EX_CLIENTEDGE, WC_IPADDRESS, TEXT(""), style, hb_parni(3), hb_parni(4),
                                   hb_parni(5), hb_parni(6), hmg_par_HWND(1), hmg_par_HMENU(2), GetInstance(), nullptr);

  hmg_ret_HWND(hIpAddress);
}

HB_FUNC(HMG_SETIPADDRESS)
{
  SendMessage(hmg_par_HWND(1), IPM_SETADDRESS, 0,
              MAKEIPADDRESS(hmg_par_BYTE(2), hmg_par_BYTE(3), hmg_par_BYTE(4), hmg_par_BYTE(5)));
}

HB_FUNC(HMG_GETIPADDRESS)
{
  DWORD pdwAddr;
  SendMessage(hmg_par_HWND(1), IPM_GETADDRESS, 0, reinterpret_cast<LPARAM>(&pdwAddr));
  hb_reta(4);
  HB_STORNI(FIRST_IPADDRESS(pdwAddr), -1, 1);
  HB_STORNI(SECOND_IPADDRESS(pdwAddr), -1, 2);
  HB_STORNI(THIRD_IPADDRESS(pdwAddr), -1, 3);
  HB_STORNI(FOURTH_IPADDRESS(pdwAddr), -1, 4);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITIPADDRESS, HMG_INITIPADDRESS)
HB_FUNC_TRANSLATE(SETIPADDRESS, HMG_SETIPADDRESS)
HB_FUNC_TRANSLATE(GETIPADDRESS, HMG_GETIPADDRESS)
#endif
