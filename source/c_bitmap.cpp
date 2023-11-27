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
 *
 * Parts of this code are contributed for MiniGUI Project
 * used here under permission of authors:
 *
 * Copyright 2005 (C) Andy Wos <andywos@unwired.com.au>
 * + DrawGlyph()
 * Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 * + GetBitmapSize(),GetIconSize(),DrawGlyphMask()
 * Copyright 2009 (C) Andi Jahja <harbour@cbn.net.id>
 * + GetImageSize()
 */

#include "mgdefs.hpp"
#include <hbapiitm.hpp>
#include <hbapifs.hpp>
#include <hbwinuni.hpp>

HANDLE DibFromBitmap(HBITMAP, HPALETTE);
WORD GetDIBColors(LPSTR);

/*
HMG_SAVEWINDOWBYHANDLE(HWND, fileName, top, left, bottom, right) --> NIL
*/
HB_FUNC( HMG_SAVEWINDOWBYHANDLE )
{
   auto hWnd = hmg_par_HWND(1);
   auto hDC = GetDC(hWnd);
   HPALETTE hPal = nullptr;
   void * FileName;
   auto top = hb_parni(3);
   auto left = hb_parni(4);
   auto bottom = hb_parni(5);
   auto right = hb_parni(6);

   RECT rc;
   if( top != -1 && left != -1 && bottom != -1 && right != -1 ) {
      rc.top    = top;
      rc.left   = left;
      rc.bottom = bottom;
      rc.right  = right;
   } else {
      GetClientRect(hWnd, &rc);
   }

   auto hMemDC = CreateCompatibleDC(hDC);
   auto hBitmap = CreateCompatibleBitmap(hDC, rc.right - rc.left, rc.bottom - rc.top);
   auto hOldBmp = static_cast<HBITMAP>(SelectObject(hMemDC, hBitmap));
   BitBlt(hMemDC, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, rc.top, rc.left, SRCCOPY);
   SelectObject(hMemDC, hOldBmp);
   HANDLE hDIB = DibFromBitmap(hBitmap, hPal);

   HANDLE filehandle = CreateFile(HB_PARSTR(2, &FileName, nullptr), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, nullptr);

   auto lpBI = static_cast<LPBITMAPINFOHEADER>(GlobalLock(hDIB));
   if( lpBI && lpBI->biSize == sizeof(BITMAPINFOHEADER) ) {
      BITMAPFILEHEADER bmfHdr;
      bmfHdr.bfType = static_cast<WORD>('M' << 8) | 'B';

      DWORD dwDIBSize = *reinterpret_cast<LPDWORD>(lpBI) + (GetDIBColors(reinterpret_cast<LPSTR>(lpBI)) * sizeof(RGBTRIPLE));

      DWORD dwBmBitsSize = ((((lpBI->biWidth) * (static_cast<DWORD>(lpBI->biBitCount))) + 31) / 32 * 4) * lpBI->biHeight;
      dwDIBSize += dwBmBitsSize;
      lpBI->biSizeImage = dwBmBitsSize;

      bmfHdr.bfSize = dwDIBSize + sizeof(BITMAPFILEHEADER);
      bmfHdr.bfReserved1 = 0;
      bmfHdr.bfReserved2 = 0;

      bmfHdr.bfOffBits = static_cast<DWORD>(sizeof(BITMAPFILEHEADER)) + lpBI->biSize + (GetDIBColors(reinterpret_cast<LPSTR>(lpBI)) * sizeof(RGBTRIPLE));

      DWORD dwWritten;
      WriteFile(filehandle, reinterpret_cast<LPSTR>(&bmfHdr), sizeof(BITMAPFILEHEADER), &dwWritten, nullptr);
      WriteFile(filehandle, reinterpret_cast<LPSTR>(lpBI), dwDIBSize, &dwWritten, nullptr);
   }

   hb_strfree(FileName);

   GlobalUnlock(hDIB);
   CloseHandle(filehandle);

   DeleteObject(hBitmap);
   DeleteDC(hMemDC);
   GlobalFree(hDIB);
   ReleaseDC(hWnd, hDC);
}

#if 1
HB_FUNC_TRANSLATE( SAVEWINDOWBYHANDLE, HMG_SAVEWINDOWBYHANDLE )
#endif

/*
HMG_WNDCOPY(HWND, lp2, ) --> NIL
*/
HB_FUNC( HMG_WNDCOPY )
{
   auto hWnd = hmg_par_HWND(1);
   auto hDC = GetDC(hWnd);
   HPALETTE hPal = nullptr;
   bool bRect = hb_parl(2);
   void * FileName;

   RECT rc;
   if( bRect ) {
      GetWindowRect(hWnd, &rc);
   } else {
      GetClientRect(hWnd, &rc);
   }

   auto hMemDC = CreateCompatibleDC(hDC);
   auto hBitmap = CreateCompatibleBitmap(hDC, rc.right - rc.left, rc.bottom - rc.top);
   auto hOldBmp = static_cast<HBITMAP>(SelectObject(hMemDC, hBitmap));
   BitBlt(hMemDC, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, 0, 0, SRCCOPY);
   SelectObject(hMemDC, hOldBmp);
   HANDLE hDIB = DibFromBitmap(hBitmap, hPal);

   HANDLE filehandle = CreateFile(HB_PARSTR(3, &FileName, nullptr), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, nullptr);

   auto lpBI = static_cast<LPBITMAPINFOHEADER>(GlobalLock(hDIB));
   if( lpBI && lpBI->biSize == sizeof(BITMAPINFOHEADER) ) {
      BITMAPFILEHEADER bmfHdr;
      bmfHdr.bfType = (static_cast<WORD>('M' << 8) | 'B');

      DWORD dwDIBSize = *reinterpret_cast<LPDWORD>(lpBI) + (GetDIBColors(reinterpret_cast<LPSTR>(lpBI)) * sizeof(RGBTRIPLE));

      DWORD dwBmBitsSize = ((((lpBI->biWidth) * (static_cast<DWORD>(lpBI->biBitCount))) + 31) / 32 * 4) * lpBI->biHeight;
      dwDIBSize += dwBmBitsSize;
      lpBI->biSizeImage = dwBmBitsSize;

      bmfHdr.bfSize = dwDIBSize + sizeof(BITMAPFILEHEADER);
      bmfHdr.bfReserved1 = 0;
      bmfHdr.bfReserved2 = 0;

      bmfHdr.bfOffBits = static_cast<DWORD>(sizeof(BITMAPFILEHEADER)) + lpBI->biSize + (GetDIBColors(reinterpret_cast<LPSTR>(lpBI)) * sizeof(RGBTRIPLE));

      DWORD dwWritten;
      WriteFile(filehandle, reinterpret_cast<LPSTR>(&bmfHdr), sizeof(BITMAPFILEHEADER), &dwWritten, nullptr);
      WriteFile(filehandle, reinterpret_cast<LPSTR>(lpBI), dwDIBSize, &dwWritten, nullptr);
   }

   hb_strfree(FileName);

   GlobalUnlock(hDIB);
   CloseHandle(filehandle);

   DeleteDC(hMemDC);
   GlobalFree(hDIB);
   ReleaseDC(hWnd, hDC);
}

#if 1
HB_FUNC_TRANSLATE( WNDCOPY, HMG_WNDCOPY )
#endif

WORD DibNumColors(VOID FAR * pv)
{
   int bits;

   auto lpbi = static_cast<LPBITMAPINFOHEADER>(pv);
   auto lpbc = static_cast<LPBITMAPCOREHEADER>(pv);

   // With the BITMAPINFO format headers, the size of the palette
   // is in biClrUsed, whereas in the BITMAPCORE - style headers, it
   // is dependent on the bits per pixel ( = 2 raised to the power of
   // bits/pixel).

   if( lpbi->biSize != sizeof(BITMAPCOREHEADER) ) {
      if( lpbi->biClrUsed != 0 ) {
         return static_cast<WORD>(lpbi->biClrUsed);
      }
      bits = lpbi->biBitCount;
   } else {
      bits = lpbc->bcBitCount;
   }

   switch( bits ) {
      case 1:
         return 2;
      case 4:
         return 16;
      case 8:
         return 256;
      default:
         // A 24 bitcount DIB has no color table
         return 0;
   }
}

static WORD PaletteSize(VOID FAR * pv)
{
   auto lpbi = static_cast<LPBITMAPINFOHEADER>(pv);
   WORD NumColors = DibNumColors(lpbi);
   return (lpbi->biSize == sizeof(BITMAPCOREHEADER)) ? static_cast<WORD>(NumColors * sizeof(RGBTRIPLE)) : static_cast<WORD>(NumColors * sizeof(RGBQUAD));
}

#define WIDTHBYTES(i)  ((i + 31) / 32 * 4)

HANDLE DibFromBitmap(HBITMAP hbm, HPALETTE hpal)
{
   if( !hbm ) {
      return nullptr;
   }

   if( hpal == nullptr ) {
      hpal = static_cast<HPALETTE>(GetStockObject(DEFAULT_PALETTE));
   }

   BITMAP bm;
   GetObject(hbm, sizeof(bm), reinterpret_cast<LPSTR>(&bm));

   auto biBits = static_cast<WORD>(bm.bmPlanes * bm.bmBitsPixel);

   BITMAPINFOHEADER bi; // TODO: initialize with {}
   bi.biSize          = sizeof(BITMAPINFOHEADER);
   bi.biWidth         = bm.bmWidth;
   bi.biHeight        = bm.bmHeight;
   bi.biPlanes        = 1;
   bi.biBitCount      = biBits;
   bi.biCompression   = BI_RGB;
   bi.biSizeImage     = 0;
   bi.biXPelsPerMeter = 0;
   bi.biYPelsPerMeter = 0;
   bi.biClrUsed       = 0;
   bi.biClrImportant  = 0;

   DWORD dwLen = bi.biSize + PaletteSize(&bi);

   auto hdc = GetDC(nullptr);
   hpal = SelectPalette(hdc, hpal, FALSE);
   RealizePalette(hdc);

   HANDLE hdib = GlobalAlloc(GHND, dwLen);

   if( !hdib ) {
      SelectPalette(hdc, hpal, FALSE);
      ReleaseDC(nullptr, hdc);
      return nullptr;
   }

   BITMAPINFOHEADER FAR * lpbi = static_cast<LPBITMAPINFOHEADER>(GlobalLock(hdib));

   memcpy(reinterpret_cast<char*>(lpbi), reinterpret_cast<char*>(&bi), sizeof(bi));

   // call GetDIBits with a nullptr lpBits param, so it will calculate the
   // biSizeImage field for us

   GetDIBits(hdc, hbm, 0L, static_cast<DWORD>(bi.biHeight), nullptr, reinterpret_cast<LPBITMAPINFO>(lpbi), static_cast<DWORD>(DIB_RGB_COLORS));

   memcpy(reinterpret_cast<char*>(&bi), reinterpret_cast<char*>(lpbi), sizeof(bi));
   GlobalUnlock(hdib);

   // If the driver did not fill in the biSizeImage field, make one up
   if( bi.biSizeImage == 0 ) {
      bi.biSizeImage = WIDTHBYTES(static_cast<DWORD>(bm.bmWidth) * biBits) * bm.bmHeight;
   }

   // realloc the buffer big enough to hold all the bits
   dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;

   HANDLE h = GlobalReAlloc(hdib, dwLen, 0);
   if( h ) {
      hdib = h;
   } else {
      GlobalFree(hdib);
      SelectPalette(hdc, hpal, FALSE);
      ReleaseDC(nullptr, hdc);
      return nullptr;
   }

   // call GetDIBits with a NON-nullptr lpBits param, and actualy get the
   // bits this time

   lpbi = static_cast<LPBITMAPINFOHEADER>(GlobalLock(hdib));

   if( GetDIBits(hdc, hbm, 0L, static_cast<DWORD>(bi.biHeight), reinterpret_cast<LPBYTE>(lpbi) + static_cast<WORD>(lpbi->biSize) + PaletteSize(lpbi), reinterpret_cast<LPBITMAPINFO>(lpbi), static_cast<DWORD>(static_cast<unsigned int>(DIB_RGB_COLORS))) == 0 ) {
      GlobalUnlock(hdib);
      SelectPalette(hdc, hpal, FALSE);
      ReleaseDC(nullptr, hdc);
      return nullptr;
   }

   GlobalUnlock(hdib);
   SelectPalette(hdc, hpal, FALSE);
   ReleaseDC(nullptr, hdc);
   return hdib;
}

WORD GetDIBColors(LPSTR lpDIB)
{
   return reinterpret_cast<LPBITMAPCOREHEADER>(lpDIB)->bcBitCount;
}

/*
HMG_C_HASALPHA(HBITMAP) --> .T.|.F.
*/
HB_FUNC( HMG_C_HASALPHA ) // hBitmap --> lYesNo
{
   auto hDC = GetDC(GetDesktopWindow());

   if( GetDeviceCaps(hDC, BITSPIXEL) < 32 ) {
      ReleaseDC(GetDesktopWindow(), hDC);
      hb_retl(false);
      return;
   }

   ReleaseDC(GetDesktopWindow(), hDC);

   HANDLE hDib = DibFromBitmap(hmg_par_HBITMAP(1), nullptr);

   bool bAlphaChannel = false;

   if( hDib ) {
      auto lpbmi = static_cast<LPBITMAPINFO>(GlobalLock(hDib));
      unsigned char * uc = reinterpret_cast<LPBYTE>(lpbmi) + static_cast<WORD>(lpbmi->bmiHeader.biSize) + PaletteSize(lpbmi);

      for( unsigned long ul = 0; ul < lpbmi->bmiHeader.biSizeImage && !bAlphaChannel; ul += 4 ) {
         if( uc[ul + 3] != 0 ) {
            bAlphaChannel = true;
         }
      }

      GlobalUnlock(hDib);
      GlobalFree(hDib);
   }

   hb_retl(bAlphaChannel);
}

#if 1
HB_FUNC_TRANSLATE( C_HASALPHA, HMG_C_HASALPHA ) // hBitmap --> lYesNo
#endif

HBITMAP Icon2Bmp(HICON hIcon)
{
   auto hDC = GetDC(nullptr);
   auto hMemDC = CreateCompatibleDC(hDC);

   ICONINFO icon;
   GetIconInfo(hIcon, &icon);
   BITMAP bitmap;
   GetObject(icon.hbmColor, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));
   auto hBmp = CreateCompatibleBitmap(hDC, bitmap.bmWidth, bitmap.bmHeight);
   auto hOldBmp = static_cast<HBITMAP>(SelectObject(hMemDC, hBmp));

   PatBlt(hMemDC, 0, 0, bitmap.bmWidth, bitmap.bmHeight, WHITENESS);
   DrawIconEx(hMemDC, 0, 0, hIcon, bitmap.bmWidth, bitmap.bmHeight, 0, nullptr, DI_NORMAL);
   SelectObject(hMemDC, hOldBmp);
   DeleteDC(hMemDC);
   DeleteObject(icon.hbmMask);
   DeleteObject(icon.hbmColor);

   ReleaseDC(nullptr, hDC);

   return hBmp;
}

// Function IconMask2Bmp converts icon mask to bitmap

HBITMAP IconMask2Bmp(HICON hIcon)
{
   auto hDC = GetDC(nullptr);
   auto hMemDC = CreateCompatibleDC(hDC);

   ICONINFO icon;
   GetIconInfo(hIcon, &icon);
   BITMAP bitmap;
   GetObject(icon.hbmColor, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));
   auto hBmp = CreateCompatibleBitmap(hDC, bitmap.bmWidth, bitmap.bmHeight);
   auto hOldBmp = static_cast<HBITMAP>(SelectObject(hMemDC, hBmp));

   PatBlt(hMemDC, 0, 0, bitmap.bmWidth, bitmap.bmHeight, WHITENESS);
   DrawIconEx(hMemDC, 0, 0, hIcon, bitmap.bmWidth, bitmap.bmHeight, 0, nullptr, DI_MASK);
   SelectObject(hMemDC, hOldBmp);
   DeleteDC(hMemDC);
   DeleteObject(icon.hbmMask);
   DeleteObject(icon.hbmColor);
   ReleaseDC(0, hDC);

   return hBmp;
}

// DrawGlyph(HDC hDC, int x, int y, int dx, int dy, HBITMAP hBmp, COLORREF rgbTransparent, BOOL disabled, BOOL stretched)
// (c) Andy Wos <andywos@unwired.com.au>

/*
HMG_DRAWGLYPH(HDC, x, y, dx, dy, HBITMAP, rgbTransparent, disabled, stretched) --> NIL
*/
HB_FUNC( HMG_DRAWGLYPH )
{
   auto hDC = hmg_par_HDC(1);
   auto x = hb_parni(2);
   auto y = hb_parni(3);
   auto dx = hb_parni(4);
   auto dy = hb_parni(5);
   auto hBmp = hmg_par_HBITMAP(6);
   COLORREF rgbTransparent = RGB(255, 255, 255);
   bool     disabled       = hb_parl(8);
   bool     stretched      = HB_ISNIL(9) ? false : hb_parl(9);
   bool     bHasBkColor    = !HB_ISNIL(7);
   HBITMAP  hBmpDefault;
   HBITMAP  hBmpStretch    = nullptr;
   HBITMAP  hBmpIcon       = nullptr;

   if( bHasBkColor ) {
      rgbTransparent = hmg_par_COLORREF(7);
   }

   BITMAP bitmap;

   // is it a bitmap?
   if( static_cast<UINT>(GetObject(hBmp, sizeof(BITMAP), static_cast<LPVOID>(&bitmap))) != sizeof(BITMAP) ) {
      ICONINFO icon;

      // is it an icon?
      if( !GetIconInfo(reinterpret_cast<HICON>(hBmp), &icon) ) {
         return;
      }

      DeleteObject(icon.hbmMask);
      DeleteObject(icon.hbmColor);

      if( !icon.fIcon ) {
         return;
      }

      if( !disabled && !stretched ) {
         // just simply draw it - nothing to do
         // (API is faster and the transparent colour is more accurate)
         DrawIconEx(hDC, x, y, reinterpret_cast<HICON>(hBmp), dx, dy, 0, nullptr, DI_NORMAL);
         return;
      } else {
         if( !stretched ) {
            // convert icon to bitmap mask.
            hBmp = IconMask2Bmp(reinterpret_cast<HICON>(hBmp));
         } else {
            // convert icon to bitmap.
            hBmp = Icon2Bmp(reinterpret_cast<HICON>(hBmp));
         }

         hBmpIcon = hBmp;

         // ignore colour given by the user, if any
         rgbTransparent = RGB(255, 255, 255);
         bHasBkColor = true;
         GetObject(hBmp, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));
      }
   }

   auto hDCMem = CreateCompatibleDC(hDC);

   if( stretched ) {
      dx = (dx > 0 ? dx : bitmap.bmWidth);
      dy = (dy > 0 ? dy : bitmap.bmHeight);
      hBmpStretch = CreateCompatibleBitmap(hDC, dx, dy);
      SelectObject(hDCMem, hBmpStretch);
      auto hDCStretch = CreateCompatibleDC(hDC);
      hBmpDefault = static_cast<HBITMAP>(SelectObject(hDCStretch, hBmp));
      StretchBlt(hDCMem, 0, 0, dx, dy, hDCStretch, 0, 0, bitmap.bmWidth, bitmap.bmHeight, SRCCOPY);
      SelectObject(hDCStretch, hBmpDefault);
      DeleteDC(hDCStretch);
   } else {
      dx = (dx > 0 ? HB_MIN(dx, bitmap.bmWidth) : bitmap.bmWidth);
      dy = (dy > 0 ? HB_MIN(dy, bitmap.bmHeight) : bitmap.bmHeight);
      hBmpDefault = static_cast<HBITMAP>(SelectObject(hDCMem, hBmp));
   }

   // prime the "no blink" device context
   auto hDCNoBlink = CreateCompatibleDC(hDC);
   auto hBmpNoBlink = CreateCompatibleBitmap(hDC, dx, dy);
   auto hBmpNoBlinkOld = static_cast<HBITMAP>(SelectObject(hDCNoBlink, hBmpNoBlink));
   BitBlt(hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY);
   SetBkColor(hDCNoBlink, RGB(255, 255, 255)); // White
   SetTextColor(hDCNoBlink, RGB(0, 0, 0)); // Black

   // was background colour given?
   // no? get the color automatically
   if( !bHasBkColor ) {
      rgbTransparent = GetPixel(hDCMem, 0, 0);
   }

   // build mask based on transparent color.
   auto hDCMask = CreateCompatibleDC(hDCNoBlink);
   auto hBmpTransMask = CreateBitmap(dx, dy, 1, 1, nullptr);
   SelectObject(hDCMask, hBmpTransMask);
   SetBkColor(hDCMem, rgbTransparent);
   BitBlt(hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY);

   if( disabled ) {
      auto hBr = CreateSolidBrush(GetSysColor(COLOR_BTNHIGHLIGHT));
      auto hOld = static_cast<HBRUSH>(SelectObject(hDCNoBlink, hBr));
      BitBlt(hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490);
      SelectObject(hDCNoBlink, hOld);
      DeleteObject(hBr);
      hBr = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW));
      hOld = static_cast<HBRUSH>(SelectObject(hDCNoBlink, hBr));
      BitBlt(hDCNoBlink, 0, 0, dx - 0, dy - 0, hDCMask, 0, 0, 12060490);
      SelectObject(hDCNoBlink, hOld);
      DeleteObject(hBr);
   } else {
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT);
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMask, 0, 0, SRCAND);
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT);
   }

   BitBlt(hDC, x, y, dx, dy, hDCNoBlink, 0, 0, SRCCOPY);

   // clean up
   SelectObject(hDCMem, hBmpDefault);
   SelectObject(hDCMask, hBmpDefault);
   SelectObject(hDCNoBlink, hBmpNoBlinkOld);
   DeleteDC(hDCMem);
   DeleteDC(hDCMask);
   DeleteDC(hDCNoBlink);
   DeleteObject(hBmpTransMask);
   DeleteObject(hBmpNoBlink);
   if( stretched ) {
      DeleteObject(hBmpStretch);
   }

   if( hBmpIcon ) {
      DeleteObject(hBmpIcon);
   }
}

#if 1
HB_FUNC_TRANSLATE( DRAWGLYPH, HMG_DRAWGLYPH )
#endif

// Function DRAWGLYPHMASK create and draw bimap mask - first pixel is treated as transparent color
// Based upon function DrawGlyph by Andy Wos <andywos@unwired.com.au>

/*
HMG_DRAWGLYPHMASK(HDC, 2, 3, dx, dy, HBITMAP, 7, 8, 9, HWND) --> NIL
*/
HB_FUNC( HMG_DRAWGLYPHMASK )
{
   auto hBmp = hmg_par_HBITMAP(6);
   BITMAP bitmap;
   GetObject(hBmp, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));

   auto hDC = hmg_par_HDC(1);
   SetBkColor(hDC, RGB(255, 255, 255)); // White
   SetTextColor(hDC, RGB(0, 0, 0)); // Black
   auto hDCMem = CreateCompatibleDC(hDC);

   auto dx = hb_parni(4);
   auto dy = hb_parni(5);

   dx = (dx > 0 ? HB_MIN(dx, bitmap.bmWidth) : bitmap.bmWidth);
   dy = (dy > 0 ? HB_MIN(dy, bitmap.bmHeight) : bitmap.bmHeight);
   auto hBmpDefault = static_cast<HBITMAP>(SelectObject(hDCMem, hBmp));
   COLORREF rgbTransparent = GetPixel(hDCMem, 0, 0);

   // build mask based on transparent color
   auto hDCMask = CreateCompatibleDC(hDC);
   auto hBmpTransMask = CreateBitmap(dx, dy, 1, 1, nullptr);

   SelectObject(hDCMask, hBmpTransMask);
   SetBkColor(hDCMem, rgbTransparent);
   BitBlt(hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY);

   auto hwnd = hmg_par_HWND(10);

   // handle to bitmaped button mask
   if( hwnd != nullptr ) {
      SendMessage(hwnd, BM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(hBmpTransMask));
   }

   SelectObject(hDCMem, hBmpDefault);
   SelectObject(hDCMask, hBmpDefault);
   DeleteDC(hDCMem);
   DeleteDC(hDCMask);
}

#if 1
HB_FUNC_TRANSLATE( DRAWGLYPHMASK, HMG_DRAWGLYPHMASK )
#endif

// Harbour MiniGUI 1.3 Extended (Build 33)
// Author P.Chornyj
//
// Function LoadBitmap()
// ---------------------
// Syntax
//   LoadBitmap(cBitmap) --> nHandle
//
// Arguments
//   <cBitmap> is the name of resource
//
// Returns
//   If the function succeeds,
//   the return value is the handle to the specified bitmap.
//   If the function fails, the return value is 0.

/*
HMG_LOADBITMAP(fileName) --> HBITMAP
*/
HB_FUNC( HMG_LOADBITMAP )
{
   void * ImageName;
   LPCTSTR lpImageName = HB_PARSTR(1, &ImageName, nullptr);

   auto hBitmap = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR));

   if( hBitmap == nullptr ) {
      hBitmap = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
   }

   RegisterResource(hBitmap, "BMP");
   hmg_ret_HBITMAP(hBitmap);

   hb_strfree(ImageName);
}

#if 1
HB_FUNC_TRANSLATE( LOADBITMAP, HMG_LOADBITMAP )
#endif

// DrawGlyph for C-level

// Harbour MiniGUI 1.3 Extended (Build 34)

VOID DrawGlyph(HDC hDC, int x, int y, int dx, int dy, HBITMAP hBmp, COLORREF rgbTransparent, BOOL disabled, BOOL stretched)
{
   HBITMAP hBmpDefault;
   HBITMAP hBmpStretch = nullptr;
   HBITMAP hBmpIcon = nullptr;
   bool bHasBkColor = !HB_ISNIL(7);

   BITMAP bitmap;

   // is it a bitmap?
   if( static_cast<UINT>(GetObject(hBmp, sizeof(BITMAP), static_cast<LPVOID>(&bitmap))) != sizeof(BITMAP) ) {
      ICONINFO icon;

      // is it an icon?
      if( !GetIconInfo(reinterpret_cast<HICON>(hBmp), &icon) ) {
         return;
      }

      DeleteObject(icon.hbmMask);
      DeleteObject(icon.hbmColor);

      if( !icon.fIcon ) {
         return;
      }

      if( !disabled && !stretched ) {
         DrawIconEx(hDC, x, y, reinterpret_cast<HICON>(hBmp), dx, dy, 0, nullptr, DI_NORMAL);
         return;
      } else {
         if( !stretched ) {
            // convert icon to bitmap mask.
            hBmp = IconMask2Bmp(reinterpret_cast<HICON>(hBmp));
         } else {
            // convert icon to bitmap.
            hBmp = Icon2Bmp(reinterpret_cast<HICON>(hBmp));
         }

         hBmpIcon = hBmp;

         // ignore colour given by the user, if any
         rgbTransparent = RGB(255, 255, 255);
         bHasBkColor = true;
         GetObject(hBmp, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));
      }
   }

   auto hDCMem = CreateCompatibleDC(hDC);

   if( stretched ) {
      dx = (dx > 0 ? dx : bitmap.bmWidth);
      dy = (dy > 0 ? dy : bitmap.bmHeight);

      hBmpStretch = CreateCompatibleBitmap(hDC, dx, dy);
      SelectObject(hDCMem, hBmpStretch);
      auto hDCStretch = CreateCompatibleDC(hDC);
      hBmpDefault = static_cast<HBITMAP>(SelectObject(hDCStretch, hBmp));

      StretchBlt(hDCMem, 0, 0, dx, dy, hDCStretch, 0, 0, bitmap.bmWidth, bitmap.bmHeight, SRCCOPY);

      SelectObject(hDCStretch, hBmpDefault);
      DeleteDC(hDCStretch);
   } else {
      dx = (dx > 0 ? HB_MIN(dx, bitmap.bmWidth) : bitmap.bmWidth);
      dy = (dy > 0 ? HB_MIN(dy, bitmap.bmHeight) : bitmap.bmHeight);
      hBmpDefault = static_cast<HBITMAP>(SelectObject(hDCMem, hBmp));
   }

   // prime the "no blink" device context
   auto hDCNoBlink = CreateCompatibleDC(hDC);
   auto hBmpNoBlink = CreateCompatibleBitmap(hDC, dx, dy);
   auto hBmpNoBlinkOld = static_cast<HBITMAP>(SelectObject(hDCNoBlink, hBmpNoBlink));
   BitBlt(hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY);
   SetBkColor(hDCNoBlink, RGB(255, 255, 255));
   SetTextColor(hDCNoBlink, RGB(0, 0, 0));

   // was background colour given?
   // no? get the color automatically
   if( !bHasBkColor ) {
      rgbTransparent = GetPixel(hDCMem, 0, 0);
   }

   // build mask based on transparent color.
   auto hDCMask = CreateCompatibleDC(hDCNoBlink);
   auto hBmpTransMask = CreateBitmap(dx, dy, 1, 1, nullptr);
   SelectObject(hDCMask, hBmpTransMask);
   SetBkColor(hDCMem, rgbTransparent);
   BitBlt(hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY);

   if( disabled ) {
      auto hBr = CreateSolidBrush(GetSysColor(COLOR_BTNHIGHLIGHT));
      auto hOld = static_cast<HBRUSH>(SelectObject(hDCNoBlink, hBr));
      BitBlt(hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490);
      SelectObject(hDCNoBlink, hOld);
      DeleteObject(hBr);

      hBr = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW));
      hOld = static_cast<HBRUSH>(SelectObject(hDCNoBlink, hBr));
      BitBlt(hDCNoBlink, 0, 0, dx - 0, dy - 0, hDCMask, 0, 0, 12060490);
      SelectObject(hDCNoBlink, hOld);
      DeleteObject(hBr);
   } else {
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT);
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMask, 0, 0, SRCAND);
      BitBlt(hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT);
   }

   BitBlt(hDC, x, y, dx, dy, hDCNoBlink, 0, 0, SRCCOPY);

   // clean up
   SelectObject(hDCMem, hBmpDefault);
   SelectObject(hDCMask, hBmpDefault);
   SelectObject(hDCNoBlink, hBmpNoBlinkOld);

   DeleteDC(hDCMem);
   DeleteDC(hDCMask);
   DeleteDC(hDCNoBlink);

   DeleteObject(hBmpTransMask);
   DeleteObject(hBmpNoBlink);

   if( stretched ) {
      DeleteObject(hBmpStretch);
   }

   if( hBmpIcon ) {
      DeleteObject(hBmpIcon);
   }
}

// Function GetImageSize()
// Author: Andi Jahja <harbour@cbn.net.id>

BOOL GetImageSize(const char * fn, int * x, int * y)
{
   unsigned char buf[24];

   FILE * f = hb_fopen(fn, "rb");

   if( !f ) {
      return FALSE;
   }

   fseek(f, 0, SEEK_END);

   long len = ftell( f );

   fseek(f, 0, SEEK_SET);

   if( len < 24 ) {
      fclose(f);
      return FALSE;
   }

   // Strategy:
   // reading GIF dimensions requires the first 10 bytes of the file
   // reading PNG dimensions requires the first 24 bytes of the file
   // reading JPEG dimensions requires scanning through jpeg chunks
   // In all formats, the file is at least 24 bytes big, so we'll read
   // that always
   fread(buf, 1, 24, f);

   // For JPEGs, we need to read the first 12 bytes of each chunk.
   // We'll read those 12 bytes at buf+2...buf+14, i.e. overwriting
   // the existing buf.
   if( buf[0] == 0xFF && buf[1] == 0xD8 && buf[2] == 0xFF && buf[3] == 0xE0 && buf[6] == 'J' && buf[7] == 'F' && buf[8] == 'I' && buf[9] == 'F' ) {
      long pos = 2;
      while( buf[2] == 0xFF ) {
         if( buf[3] == 0xC0 || buf[3] == 0xC1 || buf[3] == 0xC2 || buf[3] == 0xC3 || buf[3] == 0xC9 || buf[3] == 0xCA || buf[3] == 0xCB ) {
            break;
         }
         pos += 2 + (buf[4] << 8) + buf[5];
         if( pos + 12 > len ) {
            break;
         }
         fseek(f, pos, SEEK_SET);
         fread(buf + 2, 1, 12, f);
      }
   }

   fclose(f);

   // JPEG: (first two bytes of buf are first two bytes of the jpeg
   // file; rest of buf is the DCT frame
   if( buf[0] == 0xFF && buf[1] == 0xD8 && buf[2] == 0xFF ) {
      *y = (buf[7] << 8) + buf[8];
      *x = (buf[9] << 8) + buf[10];
      return TRUE;
   }

   // GIF: first three bytes say "GIF", next three give version
   // number. Then dimensions
   if( buf[0] == 'G' && buf[1] == 'I' && buf[2] == 'F' ) {
      *x = buf[6] + (buf[7] << 8);
      *y = buf[8] + (buf[9] << 8);
      return TRUE;
   }

   // PNG: the first frame is by definition an IHDR frame, which gives
   // dimensions
   if( buf[0] == 0x89 && buf[1] == 'P' && buf[2] == 'N' && buf[3] == 'G' &&
       buf[4] == 0x0D && buf[5] == 0x0A && buf[6] == 0x1A && buf[7] == 0x0A &&
       buf[12] == 'I' && buf[13] == 'H' && buf[14] == 'D' && buf[15] == 'R' ) {
      *x = (buf[16] << 24 ) + (buf[17] << 16) + (buf[18] << 8) + (buf[19] << 0);
      *y = (buf[20] << 24 ) + (buf[21] << 16) + (buf[22] << 8) + (buf[23] << 0);
      return TRUE;
   }

   return FALSE;
}

// Syntax: hb_GetImageSize(cPicFile)
// Parameter: cPicFile = graphic file (JPG, GIF, PNG)
// Return: 2 dim array -> array[1] = width, array[2] = height

/*
HMG_HB_GETIMAGESIZE(file) --> array
*/
HB_FUNC( HMG_HB_GETIMAGESIZE )
{
   int x = 0;
   int y = 0;
   void * str;
   GetImageSize(HB_PARSTR(1, &str, nullptr), &x, &y);
   hb_strfree(str);
   hb_reta(2);
   HB_STORNI(x, -1, 1);
   HB_STORNI(y, -1, 2);
}

#if 1
HB_FUNC_TRANSLATE( HB_GETIMAGESIZE, HMG_HB_GETIMAGESIZE )
#endif

// Harbour MiniGUI 1.3 Extended (Build 33)
// Author P.Chornyj
//
// Function BitmapSize()
// ---------------------
// Syntax
//   BitmapSize(xBitmap) --> aTarget
//
// Arguments
//   <xBitmap> is the NAME of the bitmap file or resource
//   or
//   <xBitmap> is the handle to OBJ_BITMAP
//
// Returns
//   BitmapSize() returns an array has the following structure:
//   ----------------------------------------------------------
//   Position     Metasymbol     i_bitmap.ch
//   ----------------------------------------------------------
//   1            nWidth         BM_WIDTH
//   2            nHeight        BM_HEIGHT
//   3            nBitsPerPixel  BM_BITSPIXEL
//   ----------------------------------------------------------
//   If file or resource are not found or corrupt, or is not OBJ_BITMAP,
//   BitmapSize returns an array {0, 0, 4} for compatibility

static void _arraySet(PHB_ITEM pArray, int Width, int Height, int BitsPixel)
{
   HB_arraySetNL(pArray, 1, Width);
   HB_arraySetNL(pArray, 2, Height);
   HB_arraySetNL(pArray, 3, BitsPixel);
}

/*
HMG_GETBITMAPSIZE(image) --> array
*/
HB_FUNC( HMG_GETBITMAPSIZE )
{
   auto pResult = hb_itemArrayNew(3);
   HBITMAP hBitmap = nullptr;
   bool bDelete = true;

   if( hb_parclen(1) > 0 ) {
      void * ImageName;
      LPCTSTR lpImageName = HB_PARSTR(1, &ImageName, nullptr);
      hBitmap = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));
      if( hBitmap == nullptr ) {
         hBitmap = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION));
      }
      hb_strfree(ImageName);
   } else {
      if( GetObjectType(hmg_par_HGDIOBJ(1)) == OBJ_BITMAP ) {
         hBitmap = hmg_par_HBITMAP(1);
         bDelete = false;
      }
   }

   _arraySet(pResult, 0, 0, 4);

   if( hBitmap != nullptr ) {
      BITMAP bm;

      if( GetObject(hBitmap, sizeof(BITMAP), &bm) ) {
         _arraySet(pResult, bm.bmWidth, bm.bmHeight, bm.bmBitsPixel);
      }

      if( bDelete ) {
         DeleteObject(hBitmap);
      }
   }

   hb_itemReturnRelease(pResult);
}

#if 1
HB_FUNC_TRANSLATE( GETBITMAPSIZE, HMG_GETBITMAPSIZE )
#endif

/*
HMG_GETICONSIZE(HICON) --> array
*/
HB_FUNC( HMG_GETICONSIZE )
{
   auto pResult = hb_itemArrayNew(3);
   auto hIcon = hmg_par_HICON(1);

   _arraySet(pResult, 0, 0, 4);

   if( hIcon ) {
      ICONINFO sIconInfo;

      if( GetIconInfo(hIcon, &sIconInfo) ) {
         BITMAP bm;

         if( GetObject(sIconInfo.hbmColor, sizeof(BITMAP), &bm) ) {
            _arraySet(pResult, bm.bmWidth, bm.bmHeight, bm.bmBitsPixel);
         }

         DeleteObject(sIconInfo.hbmMask);
         DeleteObject(sIconInfo.hbmColor);
      }
   }

   hb_itemReturnRelease(pResult);
}

#if 1
HB_FUNC_TRANSLATE( GETICONSIZE, HMG_GETICONSIZE )
#endif

/*
HMG_GETPIXELCOLOR(HDC, x, y) --> .T.|.F.
*/
HB_FUNC( HMG_GETPIXELCOLOR )
{
   COLORREF pixel = GetPixel(hmg_par_HDC(1), hb_parni(2), hb_parni(3));

   bool result = (pixel != CLR_INVALID ? true : false);

   if( result ) {
      COLORREF C1 = static_cast<USHORT>(GetRValue(pixel));
      COLORREF C2 = static_cast<USHORT>(GetGValue(pixel));
      COLORREF C3 = static_cast<USHORT>(GetBValue(pixel));
      HB_STORNI(C1, 4, 1);
      HB_STORNI(C2, 4, 2);
      HB_STORNI(C3, 4, 3);
   }

   hb_retl(result);
}

#if 1
HB_FUNC_TRANSLATE( GETPIXELCOLOR, HMG_GETPIXELCOLOR )
#endif
