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
#include <hbapierr.hpp>
#include <hbwinuni.hpp>

#ifndef WC_BUTTON
#define WC_BUTTON  TEXT("Button")
#endif

/* Modified by P.Ch. 16.12. */

/*
INITFRAME(HWND, HMENU, nX, nY, nWidth, nHeight, cp7, p8, p9, lp10) --> HANDLE
*/
HB_FUNC( INITFRAME )
{
   HWND hwnd = hmg_par_HWND(1);
   HWND hbutton = nullptr;

   if( IsWindow(hwnd) ) {
      HMENU hmenu = hmg_par_HMENU(2);
      DWORD dwExStyle = hmg_par_BOOL(10) ? 0 : WS_EX_TRANSPARENT; /* opaque | transparent */
      void * str;

      hbutton = CreateWindowEx(dwExStyle,
                               WC_BUTTON,
                               HB_PARSTR(7, &str, nullptr),
                               WS_CHILD | WS_VISIBLE | BS_GROUPBOX | BS_NOTIFY,
                               hmg_par_int(3),
                               hmg_par_int(4),
                               hmg_par_int(5),
                               hmg_par_int(6),
                               hwnd,
                               (IsMenu(hmenu) ? hmenu : nullptr),
                               GetInstance(),
                               nullptr);

      hb_strfree(str);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }

   hmg_ret_HWND(hbutton);
}
