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

#define _WIN32_IE 0x0501

#include "mgdefs.h"
#include <commctrl.h>

HINSTANCE GetInstance(void);

/*
INITSLIDER(p1, p2, nX, nY, nWidth, nHeight, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) --> HWND
*/
HB_FUNC( INITSLIDER )
{
   int iSelMin = 0;
   int iSelMax = 0;

   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_BAR_CLASSES;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD;

   if( hb_parl(9) )
   {
      style |= TBS_VERT;
   }

   style |= hb_parl(10) ? TBS_NOTICKS : TBS_AUTOTICKS;

   if( hb_parl(11) )
   {
      style |= TBS_BOTH;
   }

   if( hb_parl(12) )
   {
      style |= TBS_TOP;
   }

   if( hb_parl(13) )
   {
      style |= TBS_LEFT;
   }

   if( !hb_parl(14) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(15) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(16) )  /* P.Ch. 16.10 */
   {
      style |= TBS_ENABLESELRANGE;
      iSelMin = HB_MIN(hb_parnidef(17, 0), hb_parnidef(18, 0));
      iSelMax = HB_MAX(hb_parnidef(17, 0), hb_parnidef(18, 0));
   }

   HWND hTrackBar = CreateWindowEx(0,
                                   TRACKBAR_CLASS,
                                   nullptr,
                                   style,
                                   hmg_par_int(3),
                                   hmg_par_int(4),
                                   hmg_par_int(5),
                                   hmg_par_int(6),
                                   hmg_par_HWND(1),
                                   hmg_par_HMENU(2),
                                   GetInstance(),
                                   nullptr);

   SendMessage(hTrackBar, TBM_SETRANGE, static_cast<WPARAM>(TRUE), MAKELONG(hb_parni(7), hb_parni(8)));

   if( hb_parl(16) && (iSelMin != iSelMax) )
   {
      SendMessage(hTrackBar, TBM_SETSEL, static_cast<WPARAM>(TRUE), MAKELONG(iSelMin, iSelMax));  /* P.Ch. 16.10 */
   }

   hmg_ret_HANDLE(hTrackBar);
}
