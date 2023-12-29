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
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbwinuni.hpp>
#include <hbstack.hpp>

HFONT PrepareFont(const TCHAR * FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline, DWORD StrikeOut, DWORD Angle, DWORD charset)
{
   auto hDC = GetDC(HWND_DESKTOP);
   FontSize = -MulDiv(FontSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
   ReleaseDC(HWND_DESKTOP, hDC);
   return CreateFont(FontSize, 0, Angle, 0, Weight, Italic, Underline, StrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName);
}

/*
HMG_INITFONT(cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeOut, nAngle, nCharSet) --> HANDLE
*/
HB_FUNC( HMG_INITFONT )
{
   int   bold      = hb_parl(3) ? FW_BOLD : FW_NORMAL;
   auto italic    = static_cast<DWORD>(hb_parl(4));
   auto underline = static_cast<DWORD>(hb_parl(5));
   auto strikeout = static_cast<DWORD>(hb_parl(6));
   DWORD angle     = hb_parnl(7);
   DWORD charset   = hb_parnldef(8, DEFAULT_CHARSET);
   void * str;
   auto hFont = PrepareFont(HB_PARSTR(1, &str, nullptr), hb_parni(2), bold, italic, underline, strikeout, angle, charset);
   hb_strfree(str);
   RegisterResource(hFont, "FONT");
   hmg_ret_HFONT(hFont);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( INITFONT, HMG_INITFONT )
#endif

/*
HMG__SETFONT(HWND, cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeOut, nAngle, nCharSet) --> HANDLE
*/
HB_FUNC( HMG__SETFONT )
{
   auto hwnd = hmg_par_HWND(1);

   if( IsWindow(hwnd) ) {
      int   bold      = hb_parl(4) ? FW_BOLD : FW_NORMAL;
      auto italic    = static_cast<DWORD>(hb_parl(5));
      auto underline = static_cast<DWORD>(hb_parl(6));
      auto strikeout = static_cast<DWORD>(hb_parl(7));
      DWORD angle     = hb_parnl(8);
      DWORD charset   = hb_parnldef(9, DEFAULT_CHARSET);
      void * str;
      auto hFont = PrepareFont(HB_PARSTR(2, &str, nullptr), hb_parni(3), bold, italic, underline, strikeout, angle, charset);
      hb_strfree(str);
      SendMessage(hwnd, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), 1);
      RegisterResource(hFont, "FONT");
      hmg_ret_HFONT(hFont);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( _SETFONT, HMG__SETFONT )
#endif

/*
HMG__SETFONTHANDLE(HWND, HFONT) --> NIL
*/
HB_FUNC( HMG__SETFONTHANDLE )
{
   auto hwnd = hmg_par_HWND(1);

   if( IsWindow(hwnd) ) {
      if( GetObjectType(hmg_par_HGDIOBJ(2)) == OBJ_FONT ) {
         SendMessage(hwnd, WM_SETFONT, reinterpret_cast<WPARAM>(hmg_par_HFONT(2)), 1);
      } else {
         hb_errRT_BASE_SubstR(EG_ARG, 5050 + OBJ_FONT, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( _SETFONTHANDLE, HMG__SETFONTHANDLE )
#endif

/*
HMG_GETSYSTEMFONT() --> array ([1]=name [2]=height)
*/
HB_FUNC( HMG_GETSYSTEMFONT )
{
   NONCLIENTMETRICS ncm;
   ncm.cbSize = sizeof(ncm);
   SystemParametersInfo(SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0);
   LOGFONT lfDlgFont = ncm.lfMessageFont;
   hb_reta(2);
   //HB_STORC(lfDlgFont.lfFaceName, -1, 1);
   HB_ARRAYSETSTR(hb_stackReturnItem(), 1, lfDlgFont.lfFaceName);
   HB_STORNI(lfDlgFont.lfHeight + 21, -1, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( GETSYSTEMFONT, HMG_GETSYSTEMFONT )
#endif

/*
   Added by P.Ch. for 16.12.
   Parts of this code based on an original work by Dr. Claudio Soto (January 2014)

   EnumFontsEx ( [hDC], [cFontFamilyName], [nCharSet], [nPitch], [nFontType], [SortCodeBlock], [@aFontName] )
             --> return array { { cFontName, nCharSet, nPitchAndFamily, nFontType }, ... }
 */

int CALLBACK EnumFontFamExProc(ENUMLOGFONTEX * lpelfe, NEWTEXTMETRICEX * lpntme, DWORD FontType, LPARAM lParam);

/*
HMG_ENUMFONTSEX(HDC, cp2, nCharSet, nPitchAndFamily, p5, bp6, ap7) --> array
*/
HB_FUNC( HMG_ENUMFONTSEX )
{
   HDC      hdc;
   auto pArray = hb_itemArrayNew(0);
   auto bReleaseDC = false;

   if( GetObjectType(hmg_par_HGDIOBJ(1)) == OBJ_DC ) {
      hdc = hmg_par_HDC(1);
   } else {
      hdc = GetDC(nullptr);
      bReleaseDC = true;
   }

   LOGFONT lf{};

   if( hb_parclen(2) > 0 ) {
      HB_STRNCPY(lf.lfFaceName, static_cast<LPCTSTR>(hb_parc(2)), HB_MIN(LF_FACESIZE - 1, hb_parclen(2)));
   } else {
      lf.lfFaceName[0] = '\0';
   }

   lf.lfCharSet        = static_cast<BYTE>(HB_ISNUM(3) ? (hb_parni(3) == DEFAULT_CHARSET ? GetTextCharset(hdc) : hb_parni(3)) : -1);
   lf.lfPitchAndFamily = static_cast<BYTE>(HB_ISNUM(4) ? (hb_parni(4) == DEFAULT_PITCH ? -1 : (hb_parni(4) | FF_DONTCARE)) : -1);
   /* TODO - nFontType */

   EnumFontFamiliesEx(hdc, &lf, reinterpret_cast<FONTENUMPROC>(EnumFontFamExProc), reinterpret_cast<LPARAM>(pArray), 0);

   if( bReleaseDC ) {
      ReleaseDC(nullptr, hdc);
   }

   if( HB_ISBLOCK(6) ) {
      hb_arraySort(pArray, nullptr, nullptr, hb_param(6, Harbour::Item::BLOCK));
   }

   if( HB_ISBYREF(7) ) {
      auto aFontName = hb_param(7, Harbour::Item::ANY);
      int nLen = hb_arrayLen(pArray);

      hb_arrayNew(aFontName, nLen);

      for( auto i = 1; i <= nLen; i++ ) {
         hb_arraySetC(aFontName, i, hb_arrayGetC(hb_arrayGetItemPtr(pArray, i), 1));
      }
   }

   hb_itemReturnRelease(pArray);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( ENUMFONTSEX, HMG_ENUMFONTSEX )
#endif

int CALLBACK EnumFontFamExProc(ENUMLOGFONTEX * lpelfe, NEWTEXTMETRICEX * lpntme, DWORD FontType, LPARAM lParam)
{
   HB_SYMBOL_UNUSED(lpntme);

   if( lpelfe->elfLogFont.lfFaceName[0] != '@' ) {
      auto pSubArray = hb_itemArrayNew(4);
      HB_ARRAYSETSTR(pSubArray, 1, lpelfe->elfLogFont.lfFaceName);
      hb_arraySetNL(pSubArray, 2, lpelfe->elfLogFont.lfCharSet);
      hb_arraySetNI(pSubArray, 3, lpelfe->elfLogFont.lfPitchAndFamily & FIXED_PITCH);
      hb_arraySetNI(pSubArray, 4, FontType & TRUETYPE_FONTTYPE);
      hb_arrayAddForward(reinterpret_cast<PHB_ITEM>(lParam), pSubArray);
      hb_itemRelease(pSubArray);
   }

   return 1;
}
