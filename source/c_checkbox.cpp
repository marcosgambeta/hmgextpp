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

#include "mgdefs.h"
#include <commctrl.h>
#include <hbwinuni.h>

#ifndef WC_BUTTON
#define WC_BUTTON         "Button"
#endif

#ifndef BCM_FIRST
#define BCM_FIRST         0x1600
#define BCM_SETIMAGELIST  (BCM_FIRST + 0x0002)
#endif

HBITMAP HMG_LoadPicture(const char * FileName, int New_Width, int New_Height, HWND hWnd, int ScaleStretch, int Transparent, long BackgroundColor, int AdjustImage, HB_BOOL bAlphaFormat, int iAlpfaConstant);
HIMAGELIST HMG_SetButtonImageList(HWND hButton, const char * FileName, int Transparent, UINT uAlign);

HINSTANCE GetInstance(void);
HINSTANCE GetResources(void);

#if (defined(__BORLANDC__) && __BORLANDC__ < 1410) || (defined(__MINGW32__) && defined(__MINGW32_VERSION))
struct BUTTON_IMAGELIST
{
   HIMAGELIST himl;
   RECT       margin;
   UINT       uAlign;
};
using PBUTTON_IMAGELIST = BUTTON_IMAGELIST *;
#endif

/*
INITCHECKBOX(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11, par12, par13) --> HWND
*/
HB_FUNC( INITCHECKBOX )
{
   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   DWORD style = BS_NOTIFY | WS_CHILD;

   if( !hb_parl(10) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(11) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(12) )
   {
      style |= BS_LEFTTEXT;
   }

   if( hb_parl(6) )
   {
      style |= BS_MULTILINE;
   }

   style |= hb_parl(7) ? BS_AUTO3STATE : BS_AUTOCHECKBOX;

   DWORD ExStyle = 0;

   if( hb_parl(13) )
   {
      ExStyle |= WS_EX_TRANSPARENT;
   }

   hmg_ret_HWND(CreateWindowEx(ExStyle, WC_BUTTON, lpWindowName, style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(8), hmg_par_int(9),
      hmg_par_HWND(1), hmg_par_HMENU(3), GetInstance(), nullptr));

   hb_strfree(WindowName);
}

/*
INITCHECKBUTTON(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11) --> HWND
*/
HB_FUNC( INITCHECKBUTTON )
{
   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   DWORD style = BS_NOTIFY | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl(10) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(11) )
   {
      style |= WS_TABSTOP;
   }

   hmg_ret_HWND(CreateWindowEx(0, WC_BUTTON, lpWindowName, style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(8), hmg_par_int(9),
      hmg_par_HWND(1), hmg_par_HMENU(3), GetInstance(), nullptr));

   hb_strfree(WindowName);
}

/*
INITIMAGECHECKBUTTON(par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11, par12, par13) --> HWND
*/
HB_FUNC( INITIMAGECHECKBUTTON )
{
   HWND       himage;
   HIMAGELIST himl;

   void * WindowName;
   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);

   int Transparent = hb_parl(7) ? 0 : 1;

   HWND hwnd = hmg_par_HWND(1);

   DWORD style = BS_NOTIFY | BS_BITMAP | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl(11) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(12) )
   {
      style |= WS_TABSTOP;
   }

   HWND hbutton = CreateWindowEx(0, WC_BUTTON, lpWindowName, style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(9), hmg_par_int(10),
      hwnd, hmg_par_HMENU(3), GetInstance(), nullptr);

   if( !hb_parl(13) )
   {
      himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(8), -1, -1, hwnd, 0, Transparent, -1, 0, HB_FALSE, 255));

      SendMessage(hbutton, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));

      hb_reta(2);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 2);
   }
   else
   {
      himl = HMG_SetButtonImageList(hbutton, hb_parc(8), Transparent, BUTTON_IMAGELIST_ALIGN_CENTER);

      hb_reta(2);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(himl), -1, 2);
   }

   hb_strfree(WindowName);
}
