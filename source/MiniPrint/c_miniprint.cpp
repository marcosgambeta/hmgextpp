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
 * Copyright 2001 Alexander S.Kresin <alex@belacy.ru>
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - https://harbour.github.io/
 *
 * "Harbour Project"
 * Copyright 1999-2022, https://harbour.github.io/
 *
 * Parts of this module are based upon:
 *
 * "HBPRINT"
 * Copyright 2002 Richard Rylko <rrylko@poczta.onet.pl>
 * http://rrylko.republika.pl
 *
 * "HBPRINTER"
 * Copyright 2002 Richard Rylko <rrylko@poczta.onet.pl>
 * http://rrylko.republika.pl
 */

///////////////////////////////////////////////////////////////////////////////
// LOW LEVEL C PRINT ROUTINES
///////////////////////////////////////////////////////////////////////////////

#ifndef CINTERFACE
#define CINTERFACE
#endif

#define WINVER  0x0410

#define NO_LEAN_AND_MEAN

#include "mgdefs.hpp"
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>
#include <olectl.h>

#ifndef WC_STATIC
#define WC_STATIC  "Static"
#endif

static DWORD charset = DEFAULT_CHARSET;

#ifdef UNICODE
  LPWSTR AnsiToWide(LPCSTR);
  LPSTR  WideToAnsi(LPWSTR);
#endif

/*
_HMG_SETCHARSET(np1) --> NIL
*/
HB_FUNC( _HMG_SETCHARSET )
{
   charset = hmg_par_DWORD(1);
}

/*
_HMG_PRINTER_ABORTDOC(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_ABORTDOC )
{
   AbortDoc(hmg_par_HDC(1));
}

/*
_HMG_PRINTER_STARTDOC(HDC, cText) --> numeric
*/
HB_FUNC( _HMG_PRINTER_STARTDOC )
{
   auto hdcPrint = hmg_par_HDC(1);

   if( hdcPrint != nullptr ) {
      DOCINFO docInfo{};
      docInfo.cbSize      = sizeof(docInfo);
      void * str;
      docInfo.lpszDocName = HB_PARSTR(2, &str, nullptr);
      hb_retni(StartDoc(hdcPrint, &docInfo));
      hb_strfree(str);
   }
}

/*
_HMG_PRINTER_STARTPAGE(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_STARTPAGE )
{
   auto hdcPrint = hmg_par_HDC(1);

   if( hdcPrint != nullptr ) {
      StartPage(hdcPrint);
   }
}

/*
_HMG_PRINTER_C_PRINT() -->
*/
HB_FUNC( _HMG_PRINTER_C_PRINT )
{
   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: Angle Flag
   // 18: Angle

   auto hdcPrint = hmg_par_HDC(1);

   if( hdcPrint != nullptr ) {

      // Bold
      int fnWeight = hb_parl(10) ? FW_BOLD : FW_NORMAL;

      // Italic
      DWORD fdwItalic = hb_parl(11) ? TRUE : FALSE;

      // UnderLine
      DWORD fdwUnderline = hb_parl(12) ? TRUE : FALSE;

      // StrikeOut
      DWORD fdwStrikeOut = hb_parl(13) ? TRUE : FALSE;

      // Color
      int r;
      int g;
      int b;
      if( hb_parl(14) ) {
         r = hb_parni(6);
         g = hb_parni(7);
         b = hb_parni(8);
      } else {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname
      TCHAR FontName[32];
      if( hb_parl(15) ) {
         void * str;
         lstrcpy(FontName, HB_PARSTR(4, &str, nullptr));
         hb_strfree(str);
      } else {
         lstrcpy(FontName, "Arial");
      }

      // FontSize
      int FontSize = hb_parl(16) ? hb_parni(5) : 10;

      // Angle
      int FontAngle = hb_parl(17) ? hb_parni(18) : 0;

      int FontHeight = -MulDiv(FontSize, GetDeviceCaps(hdcPrint, LOGPIXELSY), 72);

      auto hfont = CreateFont(FontHeight, 0, FontAngle, FontAngle, fnWeight, fdwItalic, fdwUnderline, fdwStrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName);

      HGDIOBJ hgdiobj = SelectObject(hdcPrint, hfont);

      SetTextColor(hdcPrint, RGB(r, g, b));
      SetBkMode(hdcPrint, TRANSPARENT);

      auto x = hb_parni(3);
      auto y = hb_parni(2);

      #ifdef UNICODE
      LPWSTR pText;
      #endif

      #ifndef UNICODE
      TextOut(hdcPrint,
              ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
              ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY),
              hb_parc(9),
              strlen(hb_parc(9)));
      #else
      pText = AnsiToWide(hb_parc(9));
      TextOut(hdcPrint,
              ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
              ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY),
              pText,
              lstrlen(pText));
      hb_xfree(pText);
      #endif

      SelectObject(hdcPrint, hgdiobj);
      DeleteObject(hfont);
   }
}

/*
_HMG_PRINTER_C_MULTILINE_PRINT() -->
*/
HB_FUNC( _HMG_PRINTER_C_MULTILINE_PRINT )
{
   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: ToRow
   // 18: ToCol
   // 19: Alignment

   UINT uFormat = 0;

   HGDIOBJ hgdiobj;

   TCHAR FontName[32];
   int   FontSize;

#ifdef UNICODE
   LPWSTR pFontName, pText;
#endif

   DWORD fdwItalic;
   DWORD fdwUnderline;
   DWORD fdwStrikeOut;

   RECT rect;

   int fnWeight;
   int r;
   int g;
   int b;

   auto x   = hb_parni(3);
   auto y   = hb_parni(2);
   auto toy = hb_parni(17);
   auto tox = hb_parni(18);

   HFONT hfont;

   auto hdcPrint = hmg_par_HDC(1);

   int FontHeight;

   if( hdcPrint != 0 ) {

      // Bold

      if( hb_parl(10) ) {
         fnWeight = FW_BOLD;
      } else {
         fnWeight = FW_NORMAL;
      }

      // Italic

      if( hb_parl(11) ) {
         fdwItalic = TRUE;
      } else {
         fdwItalic = FALSE;
      }

      // UnderLine

      if( hb_parl(12) ) {
         fdwUnderline = TRUE;
      } else {
         fdwUnderline = FALSE;
      }

      // StrikeOut

      if( hb_parl(13) ) {
         fdwStrikeOut = TRUE;
      } else {
         fdwStrikeOut = FALSE;
      }

      // Color

      if( hb_parl(14) ) {
         r = hb_parni(6);
         g = hb_parni(7);
         b = hb_parni(8);
      } else {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname

      if( hb_parl(15) ) {
         void * str;
         lstrcpy(FontName, HB_PARSTR(4, &str, nullptr));
         hb_strfree(str);
      } else {
         lstrcpy(FontName, "Arial");
      }

      // FontSize

      if( hb_parl(16) ) {
         FontSize = hb_parni(5);
      } else {
         FontSize = 10;
      }

      FontHeight = -MulDiv(FontSize, GetDeviceCaps(hdcPrint, LOGPIXELSY), 72);

      hfont = CreateFont(FontHeight, 0, 0, 0, fnWeight, fdwItalic, fdwUnderline, fdwStrikeOut, charset, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName);

      if( hb_parni(19) == 0 ) {
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_LEFT;
      } else if( hb_parni(19) == 2 ) {
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_RIGHT;
      } else if( hb_parni(19) == 6 ) {
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_CENTER;
      }

      hgdiobj = SelectObject(hdcPrint, hfont);

      SetTextColor(hdcPrint, RGB(r, g, b));
      SetBkMode(hdcPrint, TRANSPARENT);

      rect.left   = ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX);
      rect.top    = ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY);
      rect.right  = ( tox * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX);
      rect.bottom = ( toy * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY);

#ifndef UNICODE
      DrawText(hdcPrint, hb_parc(9), strlen(hb_parc(9)), &rect, uFormat);
#else
      pText = AnsiToWide(hb_parc(9));
      DrawText(hdcPrint, pText, lstrlen(pText), &rect, uFormat);
      hb_xfree(pText);
#endif

      SelectObject(hdcPrint, hgdiobj);

      DeleteObject(hfont);

   }

}

/*
_HMG_PRINTER_ENDPAGE(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_ENDPAGE )
{
   auto hdcPrint = hmg_par_HDC(1);
   if( hdcPrint != nullptr ) {
      EndPage(hdcPrint);
   }
}

/*
_HMG_PRINTER_ENDDOC(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_ENDDOC )
{
   auto hdcPrint = hmg_par_HDC(1);
   if( hdcPrint != nullptr ) {
      EndDoc(hdcPrint);
   }
}

/*
_HMG_PRINTER_DELETEDC(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_DELETEDC )
{
   DeleteDC(hmg_par_HDC(1));
}

/*
_HMG_PRINTER_PRINTDIALOG() -->
*/
HB_FUNC( _HMG_PRINTER_PRINTDIALOG )
{
   PRINTDLG pd;
   pd.lStructSize         = sizeof(PRINTDLG);
   pd.hDevMode            = nullptr;
   pd.hDevNames           = nullptr;
   pd.Flags               = PD_RETURNDC | PD_PRINTSETUP;
   pd.hwndOwner           = nullptr;
   pd.hDC                 = nullptr;
   pd.nFromPage           = 1;
   pd.nToPage             = 0xFFFF;
   pd.nMinPage            = 1;
   pd.nMaxPage            = 0xFFFF;
   pd.nCopies             = 1;
   pd.hInstance           = nullptr;
   pd.lCustData           = 0L;
   pd.lpfnPrintHook       = nullptr;
   pd.lpfnSetupHook       = nullptr;
   pd.lpPrintTemplateName = nullptr;
   pd.lpSetupTemplateName = nullptr;
   pd.hPrintTemplate      = nullptr;
   pd.hSetupTemplate      = nullptr;

   if( PrintDlg(&pd) ) {
      #ifdef UNICODE
      LPSTR pStr;
      #endif
      LPDEVMODE pDevMode = ( LPDEVMODE ) GlobalLock(pd.hDevMode);
      hb_reta(4);
      hmg_storvhandle(pd.hDC, -1, 1);
      #ifndef UNICODE
      HB_STORC(( const char * ) pDevMode->dmDeviceName, -1, 2);
      #else
      pStr = WideToAnsi(pDevMode->dmDeviceName);
      HB_STORC(pStr, -1, 2);
      hb_xfree(pStr);
      #endif
      HB_STORNI( pDevMode->dmCopies > 1 ? pDevMode->dmCopies : pd.nCopies, -1, 3 );
      HB_STORNI( pDevMode->dmCollate, -1, 4 );
      GlobalUnlock(pd.hDevMode);
   } else {
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      #ifndef UNICODE
      HB_STORC("", -1, 2);
      #else
      pStr = WideToAnsi("");
      HB_STORC(pStr, -1, 2);
      hb_xfree(pStr);
      #endif
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }
}

/*
APRINTERS() -->
*/
HB_FUNC( APRINTERS )
{
   #ifdef UNICODE
   LPSTR pStr;
   #endif

   PRINTER_INFO_4 * pInfo4 = nullptr;
   PRINTER_INFO_5 * pInfo  = nullptr;

   OSVERSIONINFO osvi;
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);

   DWORD dwSize     = 0;
   DWORD dwPrinters = 0;
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
      EnumPrinters(PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, nullptr, 4, nullptr, 0, &dwSize, &dwPrinters);
   } else {
      EnumPrinters(PRINTER_ENUM_LOCAL, nullptr, 5, nullptr, 0, &dwSize, &dwPrinters);
   }

   HGLOBAL pBuffer = static_cast<char*>(GlobalAlloc(GPTR, dwSize));

   if( pBuffer == nullptr ) {
      hb_reta(0);
      GlobalFree(pBuffer);
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
      EnumPrinters(PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, nullptr, 4, static_cast<LPBYTE>(pBuffer), dwSize, &dwSize, &dwPrinters);
   } else {
      EnumPrinters(PRINTER_ENUM_LOCAL, nullptr, 5, static_cast<LPBYTE>(pBuffer), dwSize, &dwSize, &dwPrinters);
   }

   if( dwPrinters == 0 ) {
      hb_reta(0);
      GlobalFree(pBuffer);
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
      pInfo4 = ( PRINTER_INFO_4 * ) pBuffer;
   } else {
      pInfo = ( PRINTER_INFO_5 * ) pBuffer;
   }

   hb_reta(dwPrinters);

   DWORD i;
   HGLOBAL cBuffer;

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
      for( i = 0; i < dwPrinters; i++, pInfo4++ ) {
         cBuffer = GlobalAlloc(GPTR, 256);
         lstrcat(reinterpret_cast<LPSTR>(cBuffer), pInfo4->pPrinterName);
#ifndef UNICODE
         HB_STORC(( const char * ) cBuffer, -1, i + 1);
#else
         pStr = WideToAnsi(cBuffer);
         HB_STORC(pStr, -1, i + 1);
         hb_xfree(pStr);
#endif
         GlobalFree(cBuffer);
      }
   } else {
      for( i = 0; i < dwPrinters; i++, pInfo++ ) {
         cBuffer = GlobalAlloc(GPTR, 256);
         lstrcat(reinterpret_cast<LPSTR>(cBuffer), pInfo->pPrinterName);
#ifndef UNICODE
         HB_STORC(( const char * ) cBuffer, -1, i + 1);
#else
         pStr = WideToAnsi(cBuffer);
         HB_STORC(pStr, -1, i + 1);
         hb_xfree(pStr);
#endif
         GlobalFree(cBuffer);
      }
   }

   GlobalFree(pBuffer);
}

/*
_HMG_PRINTER_C_RECTANGLE() -->
*/
HB_FUNC( _HMG_PRINTER_C_RECTANGLE )
{
   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   auto x = hb_parni(3);
   auto y = hb_parni(2);

   auto tox = hb_parni(5);
   auto toy = hb_parni(4);

   int width;

   auto hdcPrint = hmg_par_HDC(1);
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = nullptr;
   HPEN    hpen   = nullptr;
   RECT    rect;

   if( hdcPrint != 0 ) {
      // Width
      if( hb_parl(10) ) {
         width = hb_parni(6);
      } else {
         width = 1 * 10000 / 254;
      }

      // Color
      if( hb_parl(11) ) {
         r = hb_parni(7);
         g = hb_parni(8);
         b = hb_parni(9);
      } else {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled
      if( hb_parl(12) ) {
         hbrush  = CreateSolidBrush(static_cast<COLORREF>(RGB(r, g, b)));
         hgdiobj = SelectObject(hdcPrint, hbrush);
      } else {
         hpen    = CreatePen(PS_SOLID, ( width * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ), static_cast<COLORREF>(RGB(r, g, b)));
         hgdiobj = SelectObject(hdcPrint, hpen);
      }

      // Border  ( contributed by Alen Uzelac 08.06.2011 )

      rect.left   = ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX);
      rect.top    = ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY);
      rect.right  = ( tox * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX);
      rect.bottom = ( toy * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY);

      if( hb_parl(12) && hb_parl(13) ) {
         FillRect(hdcPrint, &rect, static_cast<HBRUSH>(hbrush));
      } else {
         Rectangle(hdcPrint, rect.left, rect.top, rect.right, rect.bottom);
      }

      SelectObject(hdcPrint, static_cast<HGDIOBJ>(hgdiobj));

      if( hb_parl(12) ) {
         DeleteObject(hbrush);
      } else {
         DeleteObject(hpen);
      }
   }
}

/*
_HMG_PRINTER_C_ROUNDRECTANGLE() -->
*/
HB_FUNC( _HMG_PRINTER_C_ROUNDRECTANGLE )
{
   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   auto x = hb_parni(3);
   auto y = hb_parni(2);

   auto tox = hb_parni(5);
   auto toy = hb_parni(4);

   int width;

   int w, h, p;

   auto hdcPrint = hmg_par_HDC(1);
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = nullptr;
   HPEN    hpen   = nullptr;

   if( hdcPrint != 0 ) {
      // Width
      if( hb_parl(10) ) {
         width = hb_parni(6);
      } else {
         width = 1 * 10000 / 254;
      }

      // Color
      if( hb_parl(11) ) {
         r = hb_parni(7);
         g = hb_parni(8);
         b = hb_parni(9);
      } else {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled
      if( hb_parl(12) ) {
         hbrush  = CreateSolidBrush(static_cast<COLORREF>(RGB(r, g, b)));
         hgdiobj = SelectObject(hdcPrint, hbrush);
      } else {
         hpen    = CreatePen(PS_SOLID, ( width * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ), static_cast<COLORREF>(RGB(r, g, b)));
         hgdiobj = SelectObject(hdcPrint, hpen);
      }

      w = ( tox * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 );
      h = ( toy * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 );
      p = ( w + h ) / 2;
      p = p / 10;

      RoundRect(hdcPrint,
                 ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
                 ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY),
                 ( tox * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
                 ( toy * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY),
                 p,
                 p
                );

      SelectObject(hdcPrint, static_cast<HGDIOBJ>(hgdiobj));

      if( hb_parl(12) ) {
         DeleteObject(hbrush);
      } else {
         DeleteObject(hpen);
      }
   }
}

/*
_HMG_PRINTER_C_LINE() -->
*/
HB_FUNC( _HMG_PRINTER_C_LINE )
{
   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: nStyle

   int r;
   int g;
   int b;

   auto x = hb_parni(3);
   auto y = hb_parni(2);

   auto tox = hb_parni(5);
   auto toy = hb_parni(4);

   int width;
   int Style;

   auto hdcPrint = hmg_par_HDC(1);
   HGDIOBJ hgdiobj;
   HPEN    hpen;

   if( hdcPrint != 0 ) {

      // Width

      if( hb_parl(10) ) {
         width = hb_parni(6);
      } else {
         width = 1 * 10000 / 254;
      }

      // Color

      if( hb_parl(11) ) {
         r = hb_parni(7);
         g = hb_parni(8);
         b = hb_parni(9);
      } else {
         r = 0;
         g = 0;
         b = 0;
      }

      switch( hb_parni(12) ) {
         case 1:
            Style = PS_DOT;
            break;
         case 2:
            Style = PS_DASH;
            break;
         case 3:
            Style = PS_DASHDOT;
            break;
         case 4:
            Style = PS_DASHDOTDOT;
            break;
         default:
            Style = PS_SOLID;
      }

      hpen = CreatePen(Style, ( width * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ), static_cast<COLORREF>(RGB(r, g, b)));

      hgdiobj = SelectObject(hdcPrint, hpen);

      MoveToEx(hdcPrint,
               ( x * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
               ( y * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY),
               nullptr
               );

      LineTo(hdcPrint,
             ( tox * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX),
             ( toy * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY)
             );

      SelectObject(hdcPrint, static_cast<HGDIOBJ>(hgdiobj));

      DeleteObject(hpen);
   }
}

/*
_HMG_PRINTER_SETPRINTERPROPERTIES() -->
*/
HB_FUNC( _HMG_PRINTER_SETPRINTERPROPERTIES )
{
   HANDLE hPrinter = nullptr;
   DWORD  dwNeeded = 0;
   PRINTER_INFO_2 * pi2;
   DEVMODE *        pDevMode = nullptr;
   LONG lFlag;

   HDC hdcPrint;

#ifdef UNICODE
   LPWSTR pPrinterName, pDeviceName, pwszDevice;
   LPSTR  pStr;
#endif

   int fields = 0;

   void * str;
   BOOL bFlag = OpenPrinter(const_cast<TCHAR*>(HB_PARSTR(1, &str, nullptr)), &hPrinter, nullptr );
   hb_strfree(str);

   if( !bFlag || ( hPrinter == nullptr ) ) {
#ifdef _ERRORMSG_
      MessageBox(0, "Printer Configuration Failed! (001)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   SetLastError(0);

   bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );

   if( ( !bFlag ) && ( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) ) ) {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox(0, "Printer Configuration Failed! (002)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   pi2 = ( PRINTER_INFO_2 * ) GlobalAlloc(GPTR, dwNeeded);

   if( pi2 == nullptr ) {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox(0, "Printer Configuration Failed! (003)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   bFlag = GetPrinter( hPrinter, 2, reinterpret_cast<LPBYTE>(pi2), dwNeeded, &dwNeeded );

   if( !bFlag ) {
      GlobalFree(pi2);
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox(0, "Printer Configuration Failed! (004)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   if( pi2->pDevMode == nullptr ) {
#ifndef UNICODE
      dwNeeded = DocumentProperties(nullptr, hPrinter, const_cast<LPSTR>(hb_parc(1)), nullptr, nullptr, 0);
#else
      pDeviceName = AnsiToWide(hb_parc(1));
      dwNeeded    = DocumentProperties(nullptr, hPrinter, pDeviceName, nullptr, nullptr, 0);
      hb_xfree(pDeviceName);
#endif
      if( dwNeeded > 0 ) {
         pDevMode = ( DEVMODE * ) GlobalAlloc(GPTR, dwNeeded);
      } else {
         GlobalFree(pi2);
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed! (005)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      if( pDevMode == nullptr ) {
         GlobalFree(pi2);
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed! (006)", "Error! (006)", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

#ifndef UNICODE
      lFlag = DocumentProperties(nullptr, hPrinter, const_cast<LPSTR>(hb_parc(1)), pDevMode, nullptr, DM_OUT_BUFFER);
#else
      pDeviceName = AnsiToWide(hb_parc(1));
      lFlag       = DocumentProperties(nullptr, hPrinter, pDeviceName, pDevMode, nullptr, DM_OUT_BUFFER);
      hb_xfree(pDeviceName);
#endif
      if( lFlag != IDOK || pDevMode == nullptr ) {
         GlobalFree(pDevMode);
         GlobalFree(pi2);
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed! (007)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode = pDevMode;
   }

   ///////////////////////////////////////////////////////////////////////
   // Specify Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni(2) != -999 ) {
      fields = fields | DM_ORIENTATION;
   }

   // PaperSize
   if( hb_parni(3) != -999 ) {
      fields = fields | DM_PAPERSIZE;
   }

   // PaperLength
   if( hb_parni(4) != -999 ) {
      fields = fields | DM_PAPERLENGTH;
   }

   // PaperWidth
   if( hb_parni(5) != -999 ) {
      fields = fields | DM_PAPERWIDTH;
   }

   // Copies
   if( hb_parni(6) != -999 ) {
      fields = fields | DM_COPIES;
   }

   // Default Source
   if( hb_parni(7) != -999 ) {
      fields = fields | DM_DEFAULTSOURCE;
   }

   // Print Quality
   if( hb_parni(8) != -999 ) {
      fields = fields | DM_PRINTQUALITY;
   }

   // Print Color
   if( hb_parni(9) != -999 ) {
      fields = fields | DM_COLOR;
   }

   // Print Duplex Mode
   if( hb_parni(10) != -999 ) {
      fields = fields | DM_DUPLEX;
   }

   // Print Collate
   if( hb_parni(11) != -999 ) {
      fields = fields | DM_COLLATE;
   }

   pi2->pDevMode->dmFields = fields;

   ///////////////////////////////////////////////////////////////////////
   // Load Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni(2) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_ORIENTATION ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: ORIENTATION Property Not Supported By Selected Printer", "Error!",
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmOrientation = static_cast<short>(hb_parni(2));
   }

   // PaperSize
   if( hb_parni(3) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_PAPERSIZE ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: PAPERSIZE Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperSize = static_cast<short>(hb_parni(3));
   }

   // PaperLength
   if( hb_parni(4) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_PAPERLENGTH ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: PAPERLENGTH Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperLength = static_cast<short>(hb_parni(4) * 10);
   }

   // PaperWidth
   if( hb_parni(5) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_PAPERWIDTH ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: PAPERWIDTH Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperWidth = static_cast<short>(hb_parni(5) * 10);
   }

   // Copies
   if( hb_parni(6) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_COPIES ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: COPIES Property Not Supported By Selected Printer", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCopies = static_cast<short>(hb_parni(6));
   }

   // Default Source
   if( hb_parni(7) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_DEFAULTSOURCE ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: DEFAULTSOURCE Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDefaultSource = static_cast<short>(hb_parni(7));
   }

   // Print Quality
   if( hb_parni(8) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_PRINTQUALITY ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: QUALITY Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPrintQuality = static_cast<short>(hb_parni(8));
   }

   // Print Color
   if( hb_parni(9) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_COLOR ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: COLOR Property Not Supported By Selected Printer", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmColor = static_cast<short>(hb_parni(9));
   }

   // Print Duplex
   if( hb_parni(10) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_DUPLEX ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: DUPLEX Property Not Supported By Selected Printer", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDuplex = static_cast<short>(hb_parni(10));
   }

   // Print Collate
   if( hb_parni(11) != -999 ) {
      if( !( pi2->pDevMode->dmFields & DM_COLLATE ) ) {
#ifdef _ERRORMSG_
         MessageBox(0, "Printer Configuration Failed: COLLATE Property Not Supported By Selected Printer", "Error!",
                    MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
         hb_reta(4);
         HB_STORVNL( 0, -1, 1 );
         HB_STORC("", -1, 2);
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCollate = static_cast<short>(hb_parni(11));
   }

   //////////////////////////////////////////////////////////////////////

   pi2->pSecurityDescriptor = nullptr;

#ifndef UNICODE
   lFlag = DocumentProperties(nullptr, hPrinter, const_cast<LPSTR>(hb_parc(1)), pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER);
#else
   pDeviceName = AnsiToWide(hb_parc(1));
   lFlag       = DocumentProperties(nullptr, hPrinter, pDeviceName, pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER);
   hb_xfree(pDeviceName);
#endif
   if( lFlag != IDOK ) {
      GlobalFree(pi2);
      ClosePrinter( hPrinter );
      if( pDevMode ) {
         GlobalFree(pDevMode);
      }
#ifdef _ERRORMSG_
      MessageBox(0, "Printer Configuration Failed! (008)", "Error!", MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
#endif
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

#ifdef UNICODE
   pwszDevice = AnsiToWide(const_cast<char*>(hb_parc(1)));
   hdcPrint   = CreateDC(nullptr, pwszDevice, nullptr, pi2->pDevMode);
#else
   hdcPrint = CreateDC(nullptr, hb_parc(1), nullptr, pi2->pDevMode);
#endif

   if( hdcPrint != nullptr ) {
      hb_reta(4);
      hmg_storvhandle(hdcPrint, -1, 1);
#ifndef UNICODE
      HB_STORC(hb_parc(1), -1, 2);
#else
      pStr = WideToAnsi(pwszDevice);
      HB_STORC(hb_parc(1), -1, 2);
      hb_xfree(pStr);
#endif
      HB_STORNI( pi2->pDevMode->dmCopies, -1, 3 );
      HB_STORNI( pi2->pDevMode->dmCollate, -1, 4 );
   } else {
      hb_reta(4);
      HB_STORVNL( 0, -1, 1 );
      HB_STORC("", -1, 2);
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }

#ifdef UNICODE
   hb_xfree(pwszDevice);
#endif

   if( pi2 ) {
      GlobalFree(pi2);
   }

   if( hPrinter ) {
      ClosePrinter( hPrinter );
   }

   if( pDevMode ) {
      GlobalFree(pDevMode);
   }

}

#if !( ( defined(__MINGW32__) ) )

/*
GETDEFAULTPRINTER() -->
*/
HB_FUNC( GETDEFAULTPRINTER )
{
   LPPRINTER_INFO_5 PrinterInfo;
   DWORD Needed, Returned;
   DWORD BufferSize = 254;

   TCHAR DefaultPrinter[254];

#ifdef UNICODE
   LPSTR pStr;
#endif

   OSVERSIONINFO osvi;
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
   GetVersionEx(&osvi);

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS ) {
      EnumPrinters(PRINTER_ENUM_DEFAULT, nullptr, 5, nullptr, 0, &Needed, &Returned);
      PrinterInfo = ( LPPRINTER_INFO_5 ) LocalAlloc(LPTR, Needed);
      EnumPrinters(PRINTER_ENUM_DEFAULT, nullptr, 5, static_cast<LPBYTE>(PrinterInfo), Needed, &Needed, &Returned);
      lstrcpy(DefaultPrinter, PrinterInfo->pPrinterName);
      LocalFree(PrinterInfo);
   } else if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
      GetProfileString("windows", "device", "", DefaultPrinter, BufferSize);
      _tcstok(DefaultPrinter, ",");
   }

#ifndef UNICODE
   hb_retc(DefaultPrinter);
#else
   pStr = WideToAnsi(DefaultPrinter);
   hb_retc(pStr);
   hb_xfree(pStr);
#endif
}

#endif

/*
_HMG_PRINTER_STARTPAGE_PREVIEW(HDC, filename) --> numeric
*/
HB_FUNC( _HMG_PRINTER_STARTPAGE_PREVIEW )
{
   RECT emfrect;
   SetRect(&emfrect, 0, 0, GetDeviceCaps(hmg_par_HDC(1), HORZSIZE) * 100, GetDeviceCaps(hmg_par_HDC(1), VERTSIZE) * 100);
   void * str;
   HDC tmpDC = CreateEnhMetaFile(hmg_par_HDC(1), HB_PARSTR(2, &str, nullptr), &emfrect, "");
   hb_strfree(str);
   HB_RETNL(reinterpret_cast<LONG_PTR>(tmpDC));
}

/*
_HMG_PRINTER_ENDPAGE_PREVIEW(HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_ENDPAGE_PREVIEW )
{
   DeleteEnhMetaFile(CloseEnhMetaFile(hmg_par_HDC(1)));
}

/*
_HMG_PRINTER_SHOWPAGE(filename, HWND, HDC) --> NIL
*/
HB_FUNC( _HMG_PRINTER_SHOWPAGE )
{
   auto hWnd = hmg_par_HWND(2);
   auto hDCPrinter = hmg_par_HDC(3);
   PAINTSTRUCT ps;
   auto hDC = BeginPaint(hWnd, &ps);

   void * str;
   HENHMETAFILE hemf = GetEnhMetaFile(HB_PARSTR(1, &str, nullptr));
   hb_strfree(str);

   RECT        rct;
   GetClientRect(hWnd, &rct);

   int ClientWidth  = rct.right - rct.left;
   int ClientHeight = rct.bottom - rct.top;

   int zw = hb_parni(5) * GetDeviceCaps(hDCPrinter, HORZSIZE) / 750;
   int zh = hb_parni(5) * GetDeviceCaps(hDCPrinter, VERTSIZE) / 750;

   int xOffset = ( ClientWidth - ( GetDeviceCaps(hDCPrinter, HORZSIZE) * hb_parni(4) / 10000 ) ) / 2;
   int yOffset = ( ClientHeight - ( GetDeviceCaps(hDCPrinter, VERTSIZE) * hb_parni(4) / 10000 ) ) / 2;

   SetRect(&rct,
           xOffset + hb_parni(6) - zw,
           yOffset + hb_parni(7) - zh,
           xOffset + ( GetDeviceCaps(hDCPrinter, HORZSIZE) * hb_parni(4) / 10000 ) + hb_parni(6) + zw,
           yOffset + ( GetDeviceCaps(hDCPrinter, VERTSIZE) * hb_parni(4) / 10000 ) + hb_parni(7) + zh
           );

   FillRect(hDC, &rct, reinterpret_cast<HBRUSH>(RGB(255, 255, 255)));

   PlayEnhMetaFile(hDC, hemf, &rct);

   // Remove prints outside printable area

   RECT        aux;

   // Right
   aux.top    = 0;
   aux.left   = rct.right;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect(hDC, &aux, static_cast<HBRUSH>(GetStockObject(GRAY_BRUSH)));

   // Bottom
   aux.top    = rct.bottom;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect(hDC, &aux, static_cast<HBRUSH>(GetStockObject(GRAY_BRUSH)));

   // Top
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = yOffset + hb_parni(7) - zh;
   FillRect(hDC, &aux, static_cast<HBRUSH>(GetStockObject(GRAY_BRUSH)));

   // Left
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = xOffset + hb_parni(6) - zw;
   aux.bottom = ClientHeight;
   FillRect(hDC, &aux, static_cast<HBRUSH>(GetStockObject(GRAY_BRUSH)));

   // Clean up

   DeleteEnhMetaFile(hemf);

   EndPaint(hWnd, &ps);
}

/*
_HMG_PRINTER_GETPAGEWIDTH(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPAGEWIDTH )
{
   hb_retni( GetDeviceCaps(hmg_par_HDC(1), HORZSIZE) );
}

/*
_HMG_PRINTER_GETPAGEHEIGHT(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPAGEHEIGHT )
{
   hb_retni( GetDeviceCaps(hmg_par_HDC(1), VERTSIZE) );
}

/*
_HMG_PRINTER_PRINTPAGE(HDC, filename) --> NIL
*/
HB_FUNC( _HMG_PRINTER_PRINTPAGE )
{
   void * str;
   HENHMETAFILE hemf = GetEnhMetaFile(HB_PARSTR(2, &str, nullptr));
   hb_strfree(str);
   RECT rect;
   SetRect(&rect, 0, 0, GetDeviceCaps(hmg_par_HDC(1), HORZRES), GetDeviceCaps(hmg_par_HDC(1), VERTRES));
   StartPage(hmg_par_HDC(1));
   PlayEnhMetaFile(hmg_par_HDC(1), ( HENHMETAFILE ) hemf, &rect);
   EndPage(hmg_par_HDC(1));
   DeleteEnhMetaFile(hemf);
}

/*
_HMG_PRINTER_PREVIEW_ENABLESCROLLBARS(HWND) --> NIL
*/
HB_FUNC( _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_HWND(1), SB_BOTH, ESB_ENABLE_BOTH  );
}

/*
_HMG_PRINTER_PREVIEW_DISABLESCROLLBARS(HWND) --> NIL
*/
HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_HWND(1), SB_BOTH, ESB_DISABLE_BOTH );
}

/*
_HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR(HWND) --> NIL
*/
HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR )
{
   EnableScrollBar( hmg_par_HWND(1), SB_HORZ, ESB_DISABLE_BOTH );
}

/*
_HMG_PRINTER_GETPRINTERWIDTH(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTERWIDTH )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), HORZSIZE) );
}

/*
_HMG_PRINTER_GETPRINTERHEIGHT(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTERHEIGHT )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), VERTSIZE) );
}

/*
_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), PHYSICALOFFSETX) );
}

/*
_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), LOGPIXELSX) );
}

/*
_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), PHYSICALOFFSETY) );
}

/*
_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY )
{
   hb_retnl( GetDeviceCaps(hmg_par_HDC(1), LOGPIXELSY) );
}

/*
_HMG_PRINTER_C_IMAGE() -->
*/
HB_FUNC( _HMG_PRINTER_C_IMAGE )
{
   // 1: hDC
   // 2: Image File
   // 3: Row
   // 4: Col
   // 5: Height
   // 6: Width
   // 7: Stretch
   // 8: Transparent

   auto hdcPrint = hmg_par_HDC(1);

#ifndef UNICODE
   LPSTR FileName = const_cast<LPSTR>(hb_parc(2));
#else
   LPWSTR FileName = AnsiToWide(const_cast<char*>(hb_parc(2)));
#endif
   BOOL    bBmpImage = TRUE;
   HBITMAP hBitmap;
   HRGN    hRgn;
   HDC     memDC;
   INT     nWidth, nHeight;
   POINT   Point;
   BITMAP  Bmp;
   auto r = hb_parni(3); // Row
   auto c = hb_parni(4); // Col
   auto odr = hb_parni(5); // Height
   auto odc = hb_parni(6); // Width
   int     dr;
   int     dc;

   if( hdcPrint != nullptr ) {
      c  = ( c * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETX);
      r  = ( r * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 ) - GetDeviceCaps(hdcPrint, PHYSICALOFFSETY);
      dc = ( odc * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 );
      dr = ( odr * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 );

      hBitmap = static_cast<HBITMAP>(LoadImage(GetInstance(), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));

      if( hBitmap == nullptr ) {
         hBitmap = static_cast<HBITMAP>(LoadImage(nullptr, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION));
      }

      if( hBitmap == nullptr ) {
         bBmpImage = FALSE;
         hBitmap   = HMG_LoadImage(hb_parc(2), nullptr);
      }
      if( hBitmap == nullptr ) {
         return;
      }

      GetObject(hBitmap, sizeof(BITMAP), &Bmp);
      nWidth  = Bmp.bmWidth;
      nHeight = Bmp.bmHeight;

      if( !hb_parl(7) ) { // Scale
         if( odr * nHeight / nWidth <= odr ) {
            dr = odc * GetDeviceCaps(hdcPrint, LOGPIXELSY) / 1000 * nHeight / nWidth;
         } else {
            dc = odr * GetDeviceCaps(hdcPrint, LOGPIXELSX) / 1000 * nWidth / nHeight;
         }
      }

      GetViewportOrgEx(hdcPrint, &Point);

      hRgn = CreateRectRgn(c + Point.x,
                           r + Point.y,
                           c + dc + Point.x - 1,
                           r + dr + Point.y - 1);

      SelectClipRgn(hdcPrint, hRgn);

      if( !bBmpImage ) {
         if( hb_parl(7) ) {            // Stretch
            SetStretchBltMode(hdcPrint, COLORONCOLOR);
         } else {
            GetBrushOrgEx(hdcPrint, &Point);
            SetStretchBltMode(hdcPrint, HALFTONE);
            SetBrushOrgEx(hdcPrint, Point.x, Point.y, nullptr);
         }
      }

      memDC = CreateCompatibleDC(hdcPrint);
      SelectObject(memDC, hBitmap);

      if( hb_parl(8) && !bBmpImage ) { // Transparent
         TransparentBlt(hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, GetPixel(memDC, 0, 0));
      } else {
         StretchBlt(hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY);
      }

      SelectClipRgn(hdcPrint, nullptr);

      DeleteObject(hBitmap);
      DeleteDC(memDC);
   }
}

//  GetJobInfo ( cPrinterName, nJobID ) --> { nJobID, cPrinterName, cMachineName, cUserName, cDocument, cDataType, cStatus, nStatus
//                                            nPriorityLevel, nPositionPrintQueue, nTotalPages, nPagesPrinted, cLocalDate, cLocalTime }

/*
_HMG_PRINTGETJOBINFO() -->
*/
HB_FUNC( _HMG_PRINTGETJOBINFO )
{
#ifndef UNICODE
   LPSTR cPrinterName = const_cast<LPSTR>(hb_parc(1));
#else
   LPWSTR cPrinterName = AnsiToWide(const_cast<char*>(hb_parc(1)));
   LPSTR  pStr;
#endif
   auto nJobID = static_cast<DWORD>(hb_parni(2));
   HANDLE     hPrinter = nullptr;
   TCHAR      cDateTime[256];
   SYSTEMTIME LocalSystemTime;

   if( OpenPrinter( cPrinterName, &hPrinter, nullptr ) ) {
      DWORD        nBytesNeeded = 0;
      DWORD        nBytesUsed   = 0;
      JOB_INFO_1 * Job_Info_1;

      GetJob(hPrinter, nJobID, 1, nullptr, 0, &nBytesNeeded);

      if( nBytesNeeded > 0 ) {
         Job_Info_1 = ( JOB_INFO_1 * ) hb_xgrab(nBytesNeeded);
         ZeroMemory(Job_Info_1, nBytesNeeded);

         if( GetJob(hPrinter, nJobID, 1, reinterpret_cast<LPBYTE>(Job_Info_1), nBytesNeeded, &nBytesUsed) ) {
            hb_reta(14);
            HB_STORNI( Job_Info_1->JobId, -1, 1 );
#ifndef UNICODE
            HB_STORC(Job_Info_1->pPrinterName, -1, 2);
            HB_STORC(Job_Info_1->pMachineName, -1, 3);
            HB_STORC(Job_Info_1->pUserName, -1, 4);
            HB_STORC(Job_Info_1->pDocument, -1, 5);
            HB_STORC(Job_Info_1->pDatatype, -1, 6);
            HB_STORC(Job_Info_1->pStatus, -1, 7);
#else
            pStr = WideToAnsi(Job_Info_1->pPrinterName);
            HB_STORC(pStr, -1, 2);
            hb_xfree(pStr);
            pStr = WideToAnsi(Job_Info_1->pMachineName);
            HB_STORC(pStr, -1, 3);
            hb_xfree(pStr);
            pStr = WideToAnsi(Job_Info_1->pUserName);
            HB_STORC(pStr, -1, 4);
            hb_xfree(pStr);
            pStr = WideToAnsi(Job_Info_1->pDocument);
            HB_STORC(pStr, -1, 5);
            hb_xfree(pStr);
            pStr = WideToAnsi(Job_Info_1->pDatatype);
            HB_STORC(pStr, -1, 6);
            hb_xfree(pStr);
            pStr = WideToAnsi(Job_Info_1->pStatus);
            HB_STORC(pStr, -1, 7);
            hb_xfree(pStr);
#endif
            HB_STORNI( Job_Info_1->Status, -1, 8 );
            HB_STORNI( Job_Info_1->Priority, -1, 9 );
            HB_STORNI( Job_Info_1->Position, -1, 10 );
            HB_STORNI( Job_Info_1->TotalPages, -1, 11 );
            HB_STORNI( Job_Info_1->PagesPrinted, -1, 12 );

            SystemTimeToTzSpecificLocalTime(nullptr, &Job_Info_1->Submitted, &LocalSystemTime);

            wsprintf( cDateTime, "%02d/%02d/%02d", LocalSystemTime.wYear, LocalSystemTime.wMonth, LocalSystemTime.wDay );
#ifndef UNICODE
            HB_STORC(cDateTime, -1, 13);
#else
            pStr = WideToAnsi(cDateTime);
            HB_STORC(pStr, -1, 13);
            hb_xfree(pStr);
#endif

            wsprintf( cDateTime, "%02d:%02d:%02d", LocalSystemTime.wHour, LocalSystemTime.wMinute, LocalSystemTime.wSecond );
#ifndef UNICODE
            HB_STORC(cDateTime, -1, 14);
#else
            pStr = WideToAnsi(cDateTime);
            HB_STORC(pStr, -1, 14);
            hb_xfree(pStr);
#endif
         } else {
            hb_reta(0);
         }

         if( Job_Info_1 ) {
            hb_xfree(static_cast<void*>(Job_Info_1));
         }
      } else {
         hb_reta(0);
      }

      ClosePrinter( hPrinter );
   } else {
      hb_reta(0);
   }
}

/*
_HMG_PRINTERGETSTATUS() -->
*/
HB_FUNC( _HMG_PRINTERGETSTATUS )
{
#ifndef UNICODE
   LPSTR cPrinterName = const_cast<LPSTR>(hb_parc(1));
#else
   LPWSTR cPrinterName = AnsiToWide(const_cast<char*>(hb_parc(1)));
#endif
   HANDLE hPrinter     = nullptr;
   DWORD  nBytesNeeded = 0;
   DWORD  nBytesUsed   = 0;
   PRINTER_INFO_6 * Printer_Info_6;

   if( OpenPrinter( cPrinterName, &hPrinter, nullptr ) ) {
      GetPrinter( hPrinter, 6, nullptr, 0, &nBytesNeeded );
      if( nBytesNeeded > 0 ) {
         Printer_Info_6 = ( PRINTER_INFO_6 * ) hb_xgrab(nBytesNeeded);
         ZeroMemory(Printer_Info_6, nBytesNeeded);

         if( GetPrinter( hPrinter, 6, reinterpret_cast<LPBYTE>(Printer_Info_6), nBytesNeeded, &nBytesUsed ) ) {
            hb_retnl( Printer_Info_6->dwStatus );
         } else {
            hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );
         }

         if( Printer_Info_6 ) {
            hb_xfree(static_cast<void*>(Printer_Info_6));
         }
      } else {
         hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );
      }

      ClosePrinter( hPrinter );
   } else {
      hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );
   }
}

/*
GETTEXTALIGN(HDC) --> numeric
*/
HB_FUNC( GETTEXTALIGN )
{
   hb_retni( GetTextAlign(hmg_par_HDC(1)) );
}

/*
SETTEXTALIGN(HDC, UINT) --> numeric
*/
HB_FUNC( SETTEXTALIGN )
{
   hb_retni( SetTextAlign(hmg_par_HDC(1), hmg_par_UINT(2)) );
}

static HBITMAP loademffile(const TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground);

/*
INITEMFFILE(HWND, p2, p3, p4, p5, p6) --> HANDLE
*/
HB_FUNC( INITEMFFILE )
{
   DWORD Style = WS_CHILD | SS_BITMAP;

   if( !hb_parl(5) ) {
      Style |= WS_VISIBLE;
   }

   if( hb_parl(6) ) {
      Style |= SS_NOTIFY;
   }

   auto hWnd = CreateWindowEx(0,
                              WC_STATIC,
                              nullptr,
                              Style,
                              hmg_par_int(3),
                              hmg_par_int(4),
                              0,
                              0,
                              hmg_par_HWND(1),
                              hmg_par_HMENU(2),
                              GetInstance(),
                              nullptr);

   HB_RETNL(reinterpret_cast<LONG_PTR>(hWnd));
}

/*
C_SETEMFFILE(p1, p2, p3, p4, p5, p6) --> HANDLE
*/
HB_FUNC( C_SETEMFFILE )
{
   if( hb_parclen(2) == 0 ) {
      HB_RETNL(reinterpret_cast<LONG_PTR>(nullptr));
   }

   void * str;
   HBITMAP hBitmap = loademffile(HB_PARSTR(2, &str, nullptr), hb_parni(3), hb_parni(4), hmg_par_HWND(1), hb_parni(5), hb_parni(6));
   hb_strfree(str);

   if( hBitmap != nullptr ) {
      SendMessage(hmg_par_HWND(1), STM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(hBitmap));
   }

   HB_RETNL(reinterpret_cast<LONG_PTR>(hBitmap));
}

static BOOL read_image(const TCHAR * filename, DWORD * nFileSize, HGLOBAL * hMem)
{
   // open the file
   HANDLE hFile = CreateFile(filename, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
   if( hFile == INVALID_HANDLE_VALUE ) {
      return FALSE;
   }
   // we will read the whole file in global memory, find the size first
   DWORD dwFileSize = GetFileSize(hFile, nullptr);
   // allocate memory to read the whole file
   if( dwFileSize == INVALID_FILE_SIZE || ( *hMem = GlobalAlloc(GHND, dwFileSize) ) == nullptr ) {
      CloseHandle(hFile);
      return FALSE;
   }

   *nFileSize = dwFileSize;

   // lock memory for image
   LPVOID lpDest = GlobalLock(*hMem);

   if( lpDest == nullptr ) {
      GlobalFree(*hMem);
      CloseHandle(hFile);
      return FALSE;
   }

   // read file and store in global memory
   DWORD  dwBytesRead = 0;
   BOOL bRead = ReadFile(hFile, lpDest, dwFileSize, &dwBytesRead, nullptr);

   GlobalUnlock(*hMem);
   CloseHandle(hFile);

   if( !bRead ) {
      GlobalFree(*hMem);
      return FALSE;
   }

   return TRUE;
}

static void calc_rect(HWND handle, int width, int height, int scalestrech, LONG lWidth, LONG lHeight, RECT * rect, RECT * rect2)
{
   if( width == 0 && height == 0 ) {
      GetClientRect(handle, rect);
   } else {
      SetRect(rect, 0, 0, width, height);
   }

   SetRect(rect2, 0, 0, rect->right, rect->bottom);

   if( scalestrech == 0 ) {
      if( static_cast<int>(lWidth) * rect->bottom / lHeight <= rect->right ) {
         rect->right = static_cast<int>(lWidth) * rect->bottom / lHeight;
      } else {
         rect->bottom = static_cast<int>(lHeight) * rect->right / lWidth;
      }
   }

   rect->left = ( width - rect->right ) / 2;
   rect->top  = ( height - rect->bottom ) / 2;
}

static HBITMAP loademffile(const TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground)
{
   IStream *  iStream;
   IPicture * iPicture = nullptr;
   HGLOBAL    hMem     = nullptr;
   HRESULT    hr;
   DWORD      nFileSize = 0;
   RECT       rect, rect2;
   LONG       lWidth, lHeight;
   auto imgDC = GetDC(handle);

   if( read_image(filename, &nFileSize, &hMem ) == FALSE) {
      ReleaseDC(handle, imgDC);
      return nullptr;
   }
   // don't delete memory on object's release
   hr = CreateStreamOnHGlobal( hMem, FALSE, &iStream );
   if( hr != S_OK || iStream == nullptr ) {
      GlobalFree(hMem);
      ReleaseDC(handle, imgDC);
      return nullptr;
   }
   // Load from stream
#if defined(__cplusplus)
   hr = OleLoadPicture(iStream, nFileSize, ( nFileSize == 0 ), IID_IPicture, ( LPVOID * ) &iPicture);
#else
   hr = OleLoadPicture(iStream, nFileSize, ( nFileSize == 0 ), &IID_IPicture, ( LPVOID * ) &iPicture);
   iStream->lpVtbl->Release(iStream);
#endif
   if( hr != S_OK || iPicture == nullptr ) {
      GlobalFree(hMem);
      ReleaseDC(handle, imgDC);
      return nullptr;
   }

   iPicture->lpVtbl->get_Width(iPicture, &lWidth);
   iPicture->lpVtbl->get_Height(iPicture, &lHeight);

   calc_rect(handle, width, height, scalestrech, lWidth, lHeight, &rect, &rect2);

   auto tmpDC  = CreateCompatibleDC(imgDC);
   auto bitmap = CreateCompatibleBitmap(imgDC, width, height);
   SelectObject(tmpDC, bitmap);

   if( whitebackground == 1 ) {
      FillRect(tmpDC, &rect2, static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)));
   } else {
      FillRect(tmpDC, &rect2, static_cast<HBRUSH>(GetSysColorBrush(COLOR_BTNFACE)));
   }

   // Render to device context
   iPicture->lpVtbl->Render( iPicture, tmpDC, rect.left, rect.top, rect.right, rect.bottom, 0, lHeight, lWidth, -lHeight, nullptr );
   iPicture->lpVtbl->Release(iPicture);
   GlobalFree(hMem);

   DeleteDC(tmpDC);
   ReleaseDC(handle, imgDC);

   return bitmap;
}
