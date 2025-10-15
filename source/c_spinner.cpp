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
#include <hbvm.hpp>

#ifndef WC_EDIT
#define WC_EDIT "Edit"
#endif

#if (defined(__BORLANDC__) && _WIN32_WINNT >= 0x501)
#define ICC_STANDARD_CLASSES 0x00004000
#endif

LRESULT CALLBACK OwnSpinProc(HWND hedit, UINT Msg, WPARAM wParam, LPARAM lParam);

HB_FUNC(HMG_INITSPINNER)
{
  INITCOMMONCONTROLSEX i;
  i.dwSize = sizeof(INITCOMMONCONTROLSEX);
  i.dwICC = ICC_STANDARD_CLASSES;
  InitCommonControlsEx(&i);
  i.dwICC = ICC_UPDOWN_CLASS; // P.Ch. 10.16.
  InitCommonControlsEx(&i);

  DWORD style1 = ES_NUMBER | WS_CHILD | ES_AUTOHSCROLL;
  DWORD style2 = WS_CHILD | WS_BORDER | UDS_ARROWKEYS | UDS_ALIGNRIGHT | UDS_SETBUDDYINT | UDS_NOTHOUSANDS;

  if (!hb_parl(11)) {
    style1 |= WS_VISIBLE;
    style2 |= WS_VISIBLE;
  }

  if (!hb_parl(12)) {
    style1 |= WS_TABSTOP;
  }

  if (hb_parl(13)) {
    style2 |= UDS_WRAP;
  }

  if (hb_parl(14)) {
    style1 |= ES_READONLY;
  }

  if (hb_parl(15)) {
    style2 |= UDS_HORZ | UDS_ALIGNRIGHT; // P.Ch. 10.16.
  }

  auto hwnd = hmg_par_HWND(1);

  auto hedit = CreateWindowEx(WS_EX_CLIENTEDGE, WC_EDIT, TEXT(""), style1, hb_parni(3), hb_parni(4), hb_parni(5),
                              hb_parni(10), hwnd, hmg_par_HMENU(2), GetInstance(), nullptr);

  auto hupdown = CreateWindowEx(WS_EX_CLIENTEDGE, UPDOWN_CLASS, TEXT(""), style2, hb_parni(3) + hb_parni(5),
                                hb_parni(4), 15, hb_parni(10), hwnd, nullptr, GetInstance(), nullptr);

  SendMessage(hupdown, UDM_SETBUDDY, reinterpret_cast<WPARAM>(hedit), reinterpret_cast<LPARAM>(nullptr));
  SendMessage(hupdown, UDM_SETRANGE32, hmg_par_WPARAM(8), hb_parni(9));

  // 2006.08.13 JD
  SetProp(hedit, TEXT("oldspinproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hedit, GWLP_WNDPROC)));
  SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnSpinProc)));

  hb_reta(2);
  hmg_storvhandle(hedit, -1, 1);
  hmg_storvhandle(hupdown, -1, 2);
}

HB_FUNC(HMG_SETSPINNERINCREMENT)
{
  UDACCEL inc;
  inc.nSec = 0;
  inc.nInc = hb_parni(2);
  SendMessage(hmg_par_HWND(1), UDM_SETACCEL, 1, reinterpret_cast<LPARAM>(&inc));
}

// 2006.08.13 JD
LRESULT CALLBACK OwnSpinProc(HWND hedit, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  auto OldWndProc = reinterpret_cast<WNDPROC>(reinterpret_cast<LONG_PTR>(GetProp(hedit, TEXT("oldspinproc"))));

  switch (Msg) {
  case WM_DESTROY: {
    SetWindowLongPtr(hedit, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OldWndProc)));
    RemoveProp(hedit, TEXT("oldspinproc"));
    break;
  }
  case WM_CONTEXTMENU:
  case WM_GETDLGCODE: {
    if (!pSymbol) {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("OSPINEVENTS"));
    }
    if (pSymbol) {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hmg_vmPushHWND(hedit);
      hmg_vmPushUINT(Msg);
      hmg_vmPushWPARAM(wParam);
      hmg_vmPushLPARAM(lParam);
      hb_vmDo(4);
    }
    long int r = hb_parnl(-1); // P.Ch. 10.16.
    return (r != 0) ? r : CallWindowProc(OldWndProc, hedit, Msg, wParam, lParam);
  }
  }

  return CallWindowProc(OldWndProc, hedit, Msg, wParam, lParam);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITSPINNER, HMG_INITSPINNER)
HB_FUNC_TRANSLATE(SETSPINNERINCREMENT, HMG_SETSPINNERINCREMENT)
#endif
