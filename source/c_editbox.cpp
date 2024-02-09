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
 */

#include "mgdefs.hpp"
#include <commctrl.h>

#ifndef WC_EDIT
#define WC_EDIT "Edit"
#endif

extern LRESULT CALLBACK OwnEditProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

/*
HMG_INITEDITBOX(HWND, HMENU, nX, nY, nWidth, nHeight, p7, p8, p9, lReadOnly, lInvisible, lNoTabStop,
lNoVScroll, lNoHScroll) --> HWND
*/
HB_FUNC(HMG_INITEDITBOX)
{
  DWORD style = ES_MULTILINE | ES_WANTRETURN | WS_CHILD;

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
    style |= WS_VSCROLL;
  }
  else
  {
    style |= ES_AUTOVSCROLL;
  }

  if (!hb_parl(14))
  {
    style |= WS_HSCROLL;
  }

  auto hbutton =
      CreateWindowEx(WS_EX_CLIENTEDGE, WC_EDIT, TEXT(""), style, hmg_par_int(3), hmg_par_int(4), hmg_par_int(5),
                     hmg_par_int(6), hmg_par_HWND(1), hmg_par_HMENU(2), GetInstance(), nullptr);

  SendMessage(hbutton, EM_LIMITTEXT, hmg_par_WPARAM(9), 0);
  SetProp(hbutton, TEXT("oldeditproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hbutton, GWLP_WNDPROC)));
  SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnEditProc)));

  hmg_ret_HWND(hbutton);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITEDITBOX, HMG_INITEDITBOX)
#endif
