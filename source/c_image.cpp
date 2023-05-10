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
 * Parts of this code is contributed and used here under permission of his
 * author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#ifndef CINTERFACE
#define CINTERFACE
#endif

#include "mgdefs.hpp"
#include <commctrl.h>

#if defined( _MSC_VER )
# pragma warning(push)
# pragma warning(disable:4201)  /* warning C4201: nonstandard extension used: nameless struct/union */
#endif
#include <olectl.h>
#if defined( _MSC_VER )
# pragma warning(pop)
#endif

#include "hbgdiplus.h"
#include <hbapiitm.hpp>
#include <hbvm.hpp>

#ifndef WC_STATIC
#define WC_STATIC TEXT("Static")
#endif

#define HB_GPLUS_MSG_ERROR( text ) \
   do \
   { \
      MessageBox(nullptr, \
                 text, \
                 TEXT("GPlus error"), \
                 MB_OK | MB_ICONERROR); \
   } while( 0 )

#define LOGHIMETRIC_TO_PIXEL(hm, ppli)  MulDiv((hm), (ppli), 2540) // ppli = Point per Logic Inch
#define PIXEL_TO_LOGHIMETRIC( px, ppli )  MulDiv((px), 2540, (ppli)) // ppli = Point per Logic Inch

LRESULT APIENTRY ImageSubClassFunc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam );
HB_EXPORT IStream * HMG_CreateMemStreamFromResource(HINSTANCE instance, const char * res_type, const char * res_name);
HB_EXPORT IStream * HMG_CreateMemStream(const BYTE * pInit, UINT cbInitSize);
HB_EXPORT HBITMAP   HMG_GdiCreateHBITMAP(HDC hDC_mem, int width, int height, WORD iBitCount);
HB_EXPORT HBITMAP   HMG_OleLoadPicturePath(const char * pszURLorPath);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

static WNDPROC s_Image_WNDPROC;
static char * MimeTypeOld;

HB_EXPORT IStream * HMG_CreateMemStreamFromResource(HINSTANCE hinstance, const char * res_name, const char * res_type)
{
   HRSRC     resource;
   DWORD     res_size;
   HGLOBAL   res_global;
   void *    res_data;
   wchar_t * res_nameW;
   wchar_t * res_typeW;
   IStream * stream;

   if( res_name == nullptr || res_type == nullptr )
   {
      return nullptr;
   }

   res_nameW = hb_mbtowc( res_name );
   res_typeW = hb_mbtowc( res_type );

   resource = FindResourceW(hinstance, res_nameW, res_typeW);

   hb_xfree(res_nameW);
   hb_xfree(res_typeW);

   if( resource == nullptr )
   {
      return nullptr;
   }

   res_size   = SizeofResource(hinstance, resource);
   res_global = LoadResource(hinstance, resource);

   if( res_global == nullptr )
   {
      return nullptr;
   }

   res_data = LockResource(res_global);

   if( res_data == nullptr )
   {
      return nullptr;
   }

   stream = HMG_CreateMemStream(( const BYTE * ) res_data, ( UINT ) res_size);

   return stream;
}

HB_EXPORT IStream * HMG_CreateMemStream(const BYTE * pInit, UINT cbInitSize)
{
   HMODULE   hShlDll = LoadLibrary(TEXT("shlwapi.dll"));
   IStream * stream  = nullptr;

   if( hShlDll != nullptr )
   {
      typedef IStream * ( __stdcall * SHCreateMemStreamPtr )( const BYTE * pInit, UINT cbInitSize );

      SHCreateMemStreamPtr f_SHCreateMemStream = ( SHCreateMemStreamPtr ) wapi_GetProcAddress(hShlDll, ( LPCSTR ) 12);

      if( f_SHCreateMemStream != nullptr )
      {
         stream = f_SHCreateMemStream(pInit, cbInitSize);
      }
      
      FreeLibrary(hShlDll);
   }
   return stream;
}

HB_EXPORT HBITMAP HMG_GdiCreateHBITMAP(HDC hDC_mem, int width, int height, WORD iBitCount)
{
   LPBYTE     pBits;
   HBITMAP    hBitmap;
   BITMAPINFO BI;

   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = width;
   BI.bmiHeader.biHeight        = height;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = iBitCount;
   BI.bmiHeader.biCompression   = BI_RGB;    // TODO
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   hBitmap = CreateDIBSection(hDC_mem, ( BITMAPINFO * ) &BI, DIB_RGB_COLORS, ( VOID ** ) &pBits, nullptr, 0);

   return hBitmap;
}

static HBITMAP HMG_GdipLoadBitmap(const char * res_name, const char * res_type)
{
   HBITMAP    hBitmap  = nullptr;
   GpStatus   status   = GenericError;
   GpBitmap * gpBitmap = nullptr;
   wchar_t *  res_nameW;

   if( res_name == nullptr )
   {
      return hBitmap;  // nullptr
   }

   res_nameW = hb_mbtowc( res_name );

   if( fn_GdipCreateBitmapFromResource != nullptr )
   {
      status = fn_GdipCreateBitmapFromResource(GetResources(), res_nameW, &gpBitmap);
   }

   if( status != Ok && res_type != nullptr )
   {
      IStream * stream;

      stream = HMG_CreateMemStreamFromResource(GetResources(), res_name, res_type);

      if( stream != nullptr )
      {
         if( fn_GdipCreateBitmapFromStream != nullptr )
         {
            status = fn_GdipCreateBitmapFromStream(stream, &gpBitmap);
         }

         stream->lpVtbl->Release(stream);
      }
   }

   if( status != Ok && res_type == nullptr && fn_GdipCreateBitmapFromFile != nullptr )
   {
      status = fn_GdipCreateBitmapFromFile(res_nameW, &gpBitmap);
   }

   if( Ok == status )
   {
      ARGB BkColor = 0xFF000000UL;  // TODO

      if( fn_GdipCreateHBITMAPFromBitmap != nullptr )
      {
         fn_GdipCreateHBITMAPFromBitmap(gpBitmap, &hBitmap, BkColor);
      }

      if( fn_GdipDisposeImage != nullptr )
      {
         fn_GdipDisposeImage(gpBitmap);
      }
   }

   hb_xfree(res_nameW);

   return hBitmap;
}

LRESULT APIENTRY ImageSubClassFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static BOOL bMouseTracking = FALSE;

   if( Msg == WM_MOUSEMOVE || Msg == WM_MOUSELEAVE )
   {
      long r = 0;
      static PHB_SYMB pSymbol = nullptr;

      if( Msg == WM_MOUSEMOVE )
      {
         if( bMouseTracking == FALSE )
         {
            TRACKMOUSEEVENT tme;

            tme.cbSize      = sizeof(TRACKMOUSEEVENT);
            tme.dwFlags     = TME_LEAVE;
            tme.hwndTrack   = hWnd;
            tme.dwHoverTime = HOVER_DEFAULT;

            bMouseTracking = _TrackMouseEvent(&tme);
         }
      }
      else
      {
         bMouseTracking = FALSE;
      }

      if( !pSymbol )
      {
         pSymbol = hb_dynsymSymbol(hb_dynsymGet("OLABELEVENTS"));
      }

      if( pSymbol && hb_vmRequestReenter() )
      {
         hb_vmPushSymbol(pSymbol);
         hb_vmPushNil();
         hb_vmPushNumInt(( LONG_PTR ) hWnd);
         hb_vmPushLong(Msg);
         hb_vmPushNumInt(wParam);
         hb_vmPushNumInt(lParam);
         hb_vmDo(4);

         r = hb_parnl( -1 );

         hb_vmRequestRestore();
      }
      return ( r != 0 ) ? r : CallWindowProc(s_Image_WNDPROC, hWnd, 0, 0, 0);
   }
   bMouseTracking = FALSE;

   return CallWindowProc(s_Image_WNDPROC, hWnd, Msg, wParam, lParam);
}

HB_FUNC( INITIMAGE )
{
   HWND hWnd;
   HWND hWndParent = hmg_par_HWND(1);
   int  style = WS_CHILD | SS_BITMAP;

   if( !hb_parl(5) )
   {
      style |= WS_VISIBLE;
   }

   if( hb_parl(6) || hb_parl(7) )
   {
      style |= SS_NOTIFY;
   }

   hWnd = CreateWindowEx(0, WC_STATIC, nullptr, style, hb_parni(3), hb_parni(4), 0, 0, hWndParent, hmg_par_HMENU(2), GetResources(), nullptr);

   if( hb_parl(7) )
   {
      s_Image_WNDPROC = ( WNDPROC ) SetWindowLongPtr(hWnd, GWLP_WNDPROC, ( LONG_PTR ) ImageSubClassFunc);
   }

   hmg_ret_HWND(hWnd);
}

HB_FUNC( C_SETPICTURE )
{
   HWND    hWnd    = hmg_par_HWND(1);
   HBITMAP hBitmap = nullptr;

   if( IsWindow(hWnd) && ( hb_parclen(2) > 0 ) )
   {
      hBitmap = HMG_LoadPicture(hb_parc(2), hb_parni(3), hb_parni(4), hWnd, hb_parni(5), hb_parni(6), hb_parnl(7), hb_parni(8), hb_parldef(9, false), hb_parnidef(10, 255));

      if( hBitmap != nullptr )
      {
         HBITMAP hOldBitmap = reinterpret_cast<HBITMAP>(SendMessage(hWnd, STM_SETIMAGE, IMAGE_BITMAP, ( LPARAM ) hBitmap));
         RegisterResource(hBitmap, "BMP");

         if( hOldBitmap != nullptr )
         {
            DeleteObject(hOldBitmap);
         }
      }
   }

   hmg_ret_HBITMAP(hBitmap);
}

HB_FUNC( LOADIMAGE )
{
   HWND    hWnd    = HB_ISNIL(2) ? GetActiveWindow() : hmg_par_HWND(2);
   HBITMAP hBitmap = nullptr;

   if( hb_parclen(1) > 0 )
   {
      hBitmap = HMG_LoadPicture(hb_parc(1), hb_parnidef(3, -1), hb_parnidef(4, -1), hWnd, hb_parnidef(5, 1), hb_parnidef(6, 1), hb_parnldef(7, -1), hb_parnidef(8, 0), hb_parldef(9, false), hb_parnidef(10, 255));

      if( hBitmap != nullptr )
      {
         RegisterResource(hBitmap, "BMP");
      }
   }

   hmg_ret_HBITMAP(hBitmap);
}

HB_FUNC( C_GETRESPICTURE )
{
   HBITMAP hBitmap = HMG_LoadImage(hb_parc(1), hb_parc(2));

   if( hBitmap != nullptr )
   {
      RegisterResource(hBitmap, "BMP");
   }

   hmg_ret_HBITMAP(hBitmap);
}

//****************************************************************************************************************
// HMG_LoadImage (const char *FileName) -> hBitmap (Load: JPG, GIF, ICO, TIF, PNG, WMF)
//****************************************************************************************************************
HBITMAP HMG_LoadImage(const char * pszImageName, const char * pszTypeOfRes)
{
   HB_SYMBOL_UNUSED( pszTypeOfRes );

   // Find PNG Image in resourses
   HBITMAP hBitmap = HMG_GdipLoadBitmap(pszImageName, "PNG");

   // If fail: find JPG Image in resourses
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, "JPG");
   }

   // If fail: find GIF Image in resourses
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, "GIF");
   }

   // If fail: find ICON Image in resourses
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, "ICO");
   }

   // If fail: find TIF Image in resourses
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, "TIF");
   }

   // If fail: find WMF Image in resourses
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, "WMF");
   }

   // If fail: PNG, JPG, GIF, WMF and TIF Image on a disk
   if( hBitmap == nullptr )
   {
      hBitmap = HMG_GdipLoadBitmap(pszImageName, nullptr);
   }

   return hBitmap;
}

//****************************************************************************************************************
// HMG_LoadPicture (Name, width, height, ...) -> hBitmap (Load: BMP, GIF, JPG, TIF, WMF, EMF, PNG)
//****************************************************************************************************************
HBITMAP HMG_LoadPicture(const char * pszName, int width, int height, HWND hWnd, int ScaleStretch, int Transparent, long BackgroundColor, int AdjustImage, bool bAlphaFormat, int iAlphaConstant)
{
   UINT    fuLoad = ( Transparent == 0 ) ? LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS : LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT;
   HBITMAP old_hBitmap, new_hBitmap, hBitmap_old, hBitmap_new = nullptr;
   RECT    rect, rect2;
   BITMAP  bm;
   LONG    bmWidth, bmHeight;
   HDC     hDC, memDC1, memDC2;

   if( pszName == nullptr )
   {
      return nullptr;
   }

   if( bAlphaFormat == false ) // Firstly find BMP image in resourses (.EXE file)
   {
#ifndef UNICODE
      LPCSTR lpImageName = pszName;
#else
      LPWSTR lpImageName = AnsiToWide(( char * ) pszName);
#endif
      hBitmap_new = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, fuLoad));
      // If fail: find BMP in disk
      if( hBitmap_new == nullptr )
      {
         hBitmap_new = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | fuLoad));
      }
#ifdef UNICODE
      hb_xfree(lpImageName);
#endif
   }
   // Secondly find BMP (bitmap), ICO (icon), JPEG, GIF, WMF (metafile) file on disk or URL
   if( hBitmap_new == nullptr && hb_strnicmp("http", pszName, 4) == 0 )
   {
      hBitmap_new = HMG_OleLoadPicturePath(pszName);
   }
   // If fail: find JPG, GIF, WMF, TIF and PNG images using GDI+
   if( hBitmap_new == nullptr )
   {
      hBitmap_new = HMG_LoadImage(pszName, nullptr);
   }
   // If fail: return
   if( hBitmap_new == nullptr )
   {
      return nullptr;
   }

   GetObject(hBitmap_new, sizeof(BITMAP), &bm);
   bmWidth  = bm.bmWidth;
   bmHeight = bm.bmHeight;

   if( width < 0 )  // load image with original Width
   {
      width = bmWidth;
   }

   if( height < 0 ) // load image with original Height
   {
      height = bmHeight;
   }

   if( width == 0 || height == 0 )
   {
      GetClientRect(hWnd, &rect);
   }
   else
   {
      SetRect(&rect, 0, 0, width, height);
   }

   SetRect(&rect2, 0, 0, rect.right, rect.bottom);

   hDC    = GetDC(hWnd);
   memDC1 = CreateCompatibleDC(hDC);
   memDC2 = CreateCompatibleDC(hDC);

   if( ScaleStretch == 0 )
   {
      if( ( int ) bmWidth * rect.bottom / bmHeight <= rect.right )
      {
         rect.right = ( int ) bmWidth * rect.bottom / bmHeight;
      }
      else
      {
         rect.bottom = ( int ) bmHeight * rect.right / bmWidth;
      }

      if( AdjustImage == 1 )
      {
         width  = ( long ) rect.right;
         height = ( long ) rect.bottom;
      }
      else // Center Image
      {
         rect.left = ( int ) ( width - rect.right ) / 2;
         rect.top  = ( int ) ( height - rect.bottom ) / 2;
      }
   }

   hBitmap_old = static_cast<HBITMAP>(SelectObject(memDC1, hBitmap_new));
   new_hBitmap = CreateCompatibleBitmap(hDC, width, height);
   old_hBitmap = static_cast<HBITMAP>(SelectObject(memDC2, new_hBitmap));

   if( BackgroundColor == -1 )
   {
      FillRect(memDC2, &rect2, reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1));
   }
   else
   {
      HBRUSH hBrush = CreateSolidBrush(BackgroundColor);

      FillRect(memDC2, &rect2, hBrush);
      DeleteObject(hBrush);
   }

   if( ScaleStretch == 1 )
   {
      SetStretchBltMode(memDC2, COLORONCOLOR);
   }
   else
   {
      POINT Point;

      GetBrushOrgEx(memDC2, &Point);
      SetStretchBltMode(memDC2, HALFTONE);
      SetBrushOrgEx(memDC2, Point.x, Point.y, nullptr);
   }

   if( Transparent == 1 && bAlphaFormat == false )
   {
      TransparentBlt(memDC2, rect.left, rect.top, rect.right, rect.bottom, memDC1, 0, 0, bmWidth, bmHeight, GetPixel(memDC1, 0, 0));
   }
   else if( Transparent == 1 || bAlphaFormat == true )
   {
      // TransparentBlt is supported for source bitmaps of 4 bits per pixel and 8 bits per pixel.
      // Use AlphaBlend to specify 32 bits-per-pixel bitmaps with transparency.
      BLENDFUNCTION ftn;

      if( bAlphaFormat )
      {
         ftn.AlphaFormat = AC_SRC_ALPHA;
      }

      ftn.BlendOp    = AC_SRC_OVER;
      ftn.BlendFlags = 0;
      ftn.SourceConstantAlpha = ( BYTE ) iAlphaConstant;

      AlphaBlend(memDC2, rect.left, rect.top, rect.right, rect.bottom, memDC1, 0, 0, bmWidth, bmHeight, ftn);
   }
   else
   {
      StretchBlt(memDC2, rect.left, rect.top, rect.right, rect.bottom, memDC1, 0, 0, bmWidth, bmHeight, SRCCOPY);
   }

   // clean up
   SelectObject(memDC2, old_hBitmap);
   SelectObject(memDC1, hBitmap_old);
   DeleteDC(memDC1);
   DeleteDC(memDC2);
   ReleaseDC(hWnd, hDC);

   DeleteObject(hBitmap_new);

   return new_hBitmap;
}

//*************************************************************************************************
// HMG_OleLoadPicturePath(pszURLorPath) -> hBitmap
// (stream must be in BMP (bitmap), JPEG, WMF (metafile), ICO (icon), or GIF format)
//*************************************************************************************************
HB_EXPORT HBITMAP HMG_OleLoadPicturePath(const char * pszURLorPath)
{
   IPicture * iPicture = nullptr;
   HRESULT    hres     = E_FAIL;
   HBITMAP    hBitmap  = nullptr;
   HDC        memDC;
   LONG       hmWidth, hmHeight; // HiMetric

   if( pszURLorPath != nullptr )
   {
      LPOLESTR lpURLorPath = ( LPOLESTR ) ( LPCTSTR ) hb_mbtowc( pszURLorPath );

      hres = OleLoadPicturePath(lpURLorPath, nullptr, 0, 0, IID_IPicture, ( LPVOID * ) &iPicture);
      hb_xfree(lpURLorPath);
   }

   if( S_OK != hres )
   {
      return hBitmap;  // nullptr
   }

   iPicture->lpVtbl->get_Width(iPicture, &hmWidth);
   iPicture->lpVtbl->get_Height(iPicture, &hmHeight);

   if( ( memDC = CreateCompatibleDC(nullptr) ) != nullptr )
   {
      POINT Point;
      INT   pxWidth, pxHeight; // Pixel

      GetBrushOrgEx(memDC, &Point);
      SetStretchBltMode(memDC, HALFTONE);
      SetBrushOrgEx(memDC, Point.x, Point.y, nullptr);

      // Convert HiMetric to Pixel
      pxWidth  = LOGHIMETRIC_TO_PIXEL(hmWidth, GetDeviceCaps(memDC, LOGPIXELSX));
      pxHeight = LOGHIMETRIC_TO_PIXEL(hmHeight, GetDeviceCaps(memDC, LOGPIXELSY));

      hBitmap = HMG_GdiCreateHBITMAP(memDC, pxWidth, pxHeight, 32);
      SelectObject(memDC, hBitmap);

      iPicture->lpVtbl->Render( iPicture, memDC, 0, 0, pxWidth, pxHeight, 0, hmHeight, hmWidth, -hmHeight, nullptr );
      iPicture->lpVtbl->Release(iPicture);

      DeleteDC(memDC);
   }
   else
   {
      iPicture->lpVtbl->Release(iPicture);
   }

   return hBitmap;
}

/*
 * Get encoders
 */
HB_FUNC( GPLUSGETENCODERSNUM )
{
   UINT num  = 0;  // number of image encoders
   UINT size = 0;  // size of the image encoder array in bytes

   fn_GdipGetImageEncodersSize(&num, &size);

   hb_retni( num );
}

HB_FUNC( GPLUSGETENCODERSSIZE )
{
   UINT num  = 0;
   UINT size = 0;

   fn_GdipGetImageEncodersSize(&num, &size);

   hb_retni( size );
}

HB_FUNC( GPLUSGETENCODERSMIMETYPE )
{
   UINT num  = 0;
   UINT size = 0;
   ImageCodecInfo * pImageCodecInfo;
   PHB_ITEM         pResult = hb_itemArrayNew(0);
   PHB_ITEM         pItem;
   char * RecvMimeType;

   fn_GdipGetImageEncodersSize(&num, &size);

   if( size == 0 )
   {
      hb_itemReturnRelease(pResult);
      return;
   }

   pImageCodecInfo = ( ImageCodecInfo * ) hb_xalloc(size);

   if( pImageCodecInfo == nullptr )
   {
      hb_itemReturnRelease(pResult);
      return;
   }

   RecvMimeType = reinterpret_cast<char*>(LocalAlloc(LPTR, size));

   if( RecvMimeType == nullptr )
   {
      hb_xfree(pImageCodecInfo);
      hb_itemReturnRelease(pResult);
      return;
   }

   fn_GdipGetImageEncoders(num, size, pImageCodecInfo);

   pItem = hb_itemNew(nullptr);

   for( UINT i = 0; i < num; ++i )
   {
      WideCharToMultiByte(CP_ACP, 0, pImageCodecInfo[i].MimeType, -1, RecvMimeType, size, nullptr, nullptr);

      pItem = hb_itemPutC( nullptr, RecvMimeType );

      hb_arrayAdd( pResult, pItem );
   }

   // free resource
   LocalFree(RecvMimeType);
   hb_xfree(pImageCodecInfo);

   hb_itemRelease(pItem);

   // return a result array
   hb_itemReturnRelease(pResult);
}

static BOOL GetEnCodecClsid( const char * MimeType, CLSID * Clsid )
{
   UINT num  = 0;
   UINT size = 0;
   ImageCodecInfo * pImageCodecInfo;
   UINT   CodecIndex;
   char * RecvMimeType;
   BOOL   bFounded = FALSE;

   hb_xmemset(Clsid, 0, sizeof(CLSID));

   if( ( MimeType == nullptr ) || ( Clsid == nullptr ) || ( g_GpModule == nullptr ) )
   {
      return FALSE;
   }

   if( fn_GdipGetImageEncodersSize(&num, &size) )
   {
      return FALSE;
   }

   if( ( pImageCodecInfo = reinterpret_cast<ImageCodecInfo*>(hb_xalloc(size)) ) == nullptr )
   {
      return FALSE;
   }

   hb_xmemset(pImageCodecInfo, 0, sizeof(ImageCodecInfo));

   if( fn_GdipGetImageEncoders(num, size, pImageCodecInfo) || (pImageCodecInfo == nullptr) )
   {
      hb_xfree(pImageCodecInfo);

      return FALSE;
   }

   if( ( RecvMimeType = reinterpret_cast<char*>(LocalAlloc(LPTR, size)) ) == nullptr )
   {
      hb_xfree(pImageCodecInfo);

      return FALSE;
   }

   for( CodecIndex = 0; CodecIndex < num; ++CodecIndex )
   {
      WideCharToMultiByte(CP_ACP, 0, pImageCodecInfo[CodecIndex].MimeType, -1, RecvMimeType, size, nullptr, nullptr);

      if( strcmp(MimeType, RecvMimeType) == 0 )
      {
         bFounded = TRUE;
         break;
      }
   }

   if( bFounded )
   {
      CopyMemory(Clsid, &pImageCodecInfo[CodecIndex].Clsid, sizeof(CLSID));
   }

   hb_xfree(pImageCodecInfo);
   LocalFree(RecvMimeType);

   return bFounded ? TRUE : FALSE;
}

BOOL SaveHBitmapToFile(void * HBitmap, const char * FileName, unsigned int Width, unsigned int Height, const char * MimeType, ULONG JpgQuality)
{
   GpBitmap *        GBitmap;
   GpBitmap *        GBitmapThumbnail;
   LPWSTR            WFileName;
   static CLSID      Clsid;
   EncoderParameters EncoderParameters;

   if( ( HBitmap == nullptr ) || ( FileName == nullptr ) || ( MimeType == nullptr ) || ( g_GpModule == nullptr ) )
   {
      HB_GPLUS_MSG_ERROR( TEXT("Wrong Param") );
      return FALSE;
   }

   if( MimeTypeOld == nullptr )
   {
      if( !GetEnCodecClsid( MimeType, &Clsid ) )
      {
         HB_GPLUS_MSG_ERROR( TEXT("Wrong MimeType") );
         return FALSE;
      }

      MimeTypeOld = reinterpret_cast<char*>(LocalAlloc(LPTR, strlen(MimeType) + 1));

      if( MimeTypeOld == nullptr )
      {
         HB_GPLUS_MSG_ERROR( TEXT("LocalAlloc Error") );
         return FALSE;
      }

      strcpy(MimeTypeOld, MimeType);
   }
   else
   {
      if( strcmp(( const char * ) MimeTypeOld, MimeType) != 0 )
      {
         LocalFree(MimeTypeOld);

         if( !GetEnCodecClsid( MimeType, &Clsid ) )
         {
            HB_GPLUS_MSG_ERROR( TEXT("Wrong MimeType") );
            return FALSE;
         }

         MimeTypeOld = reinterpret_cast<char*>(LocalAlloc(LPTR, strlen(MimeType) + 1));

         if( MimeTypeOld == nullptr )
         {
            HB_GPLUS_MSG_ERROR( TEXT("LocalAlloc Error") );
            return FALSE;
         }
         strcpy(MimeTypeOld, MimeType);
      }
   }

   ZeroMemory(&EncoderParameters, sizeof(EncoderParameters));
   EncoderParameters.Count = 1;
   EncoderParameters.Parameter[0].Guid.Data1      = 0x1d5be4b5;
   EncoderParameters.Parameter[0].Guid.Data2      = 0xfa4a;
   EncoderParameters.Parameter[0].Guid.Data3      = 0x452d;
   EncoderParameters.Parameter[0].Guid.Data4[0] = 0x9c;
   EncoderParameters.Parameter[0].Guid.Data4[1] = 0xdd;
   EncoderParameters.Parameter[0].Guid.Data4[2] = 0x5d;
   EncoderParameters.Parameter[0].Guid.Data4[3] = 0xb3;
   EncoderParameters.Parameter[0].Guid.Data4[4] = 0x51;
   EncoderParameters.Parameter[0].Guid.Data4[5] = 0x05;
   EncoderParameters.Parameter[0].Guid.Data4[6] = 0xe7;
   EncoderParameters.Parameter[0].Guid.Data4[7] = 0xeb;
   EncoderParameters.Parameter[0].NumberOfValues  = 1;
   EncoderParameters.Parameter[0].Type  = 4;
   EncoderParameters.Parameter[0].Value = ( void * ) &JpgQuality;

   GBitmap = 0;

   if( fn_GdipCreateBitmapFromHBITMAP(reinterpret_cast<HBITMAP>(HBitmap), nullptr, &GBitmap) )
   {
      HB_GPLUS_MSG_ERROR( TEXT("CreateBitmap Operation Error") );
      return FALSE;
   }

   WFileName = reinterpret_cast<LPWSTR>(LocalAlloc(LPTR, (strlen(FileName) * sizeof(WCHAR)) + 1));

   if( WFileName == nullptr )
   {
      HB_GPLUS_MSG_ERROR( TEXT("WFile LocalAlloc Error") );
      return FALSE;
   }

   MultiByteToWideChar( CP_ACP, 0, FileName, -1, WFileName, ( int ) ( strlen(FileName) * sizeof(WCHAR) ) - 1 );

   if( ( Width > 0 ) && ( Height > 0 ) )
   {
      GBitmapThumbnail = nullptr;

      if( Ok != fn_GdipGetImageThumbnail( GBitmap, Width, Height, &GBitmapThumbnail, nullptr, nullptr ) )
      {
         fn_GdipDisposeImage(GBitmap);
         LocalFree(WFileName);
         HB_GPLUS_MSG_ERROR( TEXT("Thumbnail Operation Error") );
         return FALSE;
      }

      fn_GdipDisposeImage(GBitmap);
      GBitmap = GBitmapThumbnail;
   }

   if( Ok != fn_GdipSaveImageToFile(GBitmap, WFileName, &Clsid, &EncoderParameters) )
   {
      fn_GdipDisposeImage(GBitmap);
      LocalFree(WFileName);
      HB_GPLUS_MSG_ERROR( TEXT("Save Operation Error") );
      return FALSE;
   }

   fn_GdipDisposeImage(GBitmap);
   LocalFree(WFileName);

   return TRUE;
}

HB_FUNC( C_SAVEHBITMAPTOFILE )
{
   HBITMAP hbmp = hmg_par_HBITMAP(1);

   hb_retl(SaveHBitmapToFile(( void * ) hbmp, hb_parc(2),
                               hmg_par_UINT(3), hmg_par_UINT(4), hb_parc(5), ( ULONG ) hb_parnl(6)));
}

//*************************************************************************************************
//        ICONS (.ICO type 1) are structured like this:
//
//        ICONHEADER                                        (just 1)
//        ICONDIR                                                [1...n]  (an array, 1 for each image)
//        [BITMAPINFOHEADER+COLOR_BITS+MASK_BITS]                [1...n]         (1 after the other, for each image)
//
//        CURSORS (.ICO type 2) are identical in structure, but use
//        two monochrome bitmaps (real XOR and AND masks, this time).
//*************************************************************************************************
typedef struct
{
   WORD idReserved;                                     // must be 0
   WORD idType;                                         // 1 = ICON, 2 = CURSOR
   WORD idCount;                                        // number of images (and ICONDIRs)

} ICONHEADER;

//*************************************************************************************************
//        An array of ICONDIRs immediately follow the ICONHEADER
//*************************************************************************************************
typedef struct
{
   BYTE  bWidth;
   BYTE  bHeight;
   BYTE  bColorCount;
   BYTE  bReserved;
   WORD  wPlanes;                                       // for cursors, this field = wXHotSpot
   WORD  wBitCount;                                     // for cursors, this field = wYHotSpot
   DWORD dwBytesInRes;
   DWORD dwImageOffset;                                 // file-offset to the start of ICONIMAGE

} ICONDIR;

//*************************************************************************************************
//        After the ICONDIRs follow the ICONIMAGE structures -
//        consisting of a BITMAPINFOHEADER, (optional) RGBQUAD array, then
//        the color and mask bitmap bits (all packed together).
//*************************************************************************************************
typedef struct
{
   BITMAPINFOHEADER biHeader;                           // header for color bitmap (no mask header)

} ICONIMAGE;

//*************************************************************************************************
//        Write the ICO header to disk
//*************************************************************************************************
static UINT WriteIconHeader(HANDLE hFile, int nImages)
{
   ICONHEADER iconheader;
   UINT       nWritten;

   // Setup the icon header
   iconheader.idReserved = 0;                           // Must be 0
   iconheader.idType     = 1;                           // Type 1 = ICON  (type 2 = CURSOR)
   iconheader.idCount    = ( WORD ) nImages;            // number of ICONDIRs

   // Write the header to disk
   WriteFile(hFile, ( LPVOID ) &iconheader, sizeof(iconheader), ( LPDWORD ) &nWritten, nullptr);

   // following ICONHEADER is a series of ICONDIR structures (idCount of them, in fact)
   return nWritten;
}

//*************************************************************************************************
//        Return the number of BYTES the bitmap will take ON DISK
//*************************************************************************************************
static UINT NumBitmapBytes(BITMAP * pBitmap)
{
   int nWidthBytes = pBitmap->bmWidthBytes;

   // bitmap scanlines MUST be a multiple of 4 bytes when stored
   // inside a bitmap resource, so round up if necessary
   if( nWidthBytes & 3 )
   {
      nWidthBytes = ( nWidthBytes + 4 ) & ~3;
   }

   return nWidthBytes * pBitmap->bmHeight;
}

//*************************************************************************************************
//        Return number of bytes written
//*************************************************************************************************
static UINT WriteIconImageHeader(HANDLE hFile, BITMAP * pbmpColor, BITMAP * pbmpMask)
{
   BITMAPINFOHEADER biHeader;
   UINT nWritten;
   UINT nImageBytes;

   // calculate how much space the COLOR and MASK bitmaps take
   nImageBytes = NumBitmapBytes(pbmpColor) + NumBitmapBytes(pbmpMask);

   // write the ICONIMAGE to disk (first the BITMAPINFOHEADER)
   ZeroMemory(&biHeader, sizeof(biHeader));

   // Fill in only those fields that are necessary
   biHeader.biSize      = sizeof(biHeader);
   biHeader.biWidth     = pbmpColor->bmWidth;
   biHeader.biHeight    = pbmpColor->bmHeight * 2;                                      // height of color+mono
   biHeader.biPlanes    = pbmpColor->bmPlanes;
   biHeader.biBitCount  = pbmpColor->bmBitsPixel;
   biHeader.biSizeImage = nImageBytes;

   // write the BITMAPINFOHEADER
   WriteFile(hFile, ( LPVOID ) &biHeader, sizeof(biHeader), ( LPDWORD ) &nWritten, nullptr);

   return nWritten;
}

//*************************************************************************************************
//        Wrapper around GetIconInfo and GetObject(BITMAP)
//*************************************************************************************************
static BOOL GetIconBitmapInfo(HICON hIcon, ICONINFO * pIconInfo, BITMAP * pbmpColor, BITMAP * pbmpMask)
{
   if( !GetIconInfo(hIcon, pIconInfo) )
   {
      return FALSE;
   }

   if( !GetObject(pIconInfo->hbmColor, sizeof(BITMAP), pbmpColor) )
   {
      return FALSE;
   }

   if( !GetObject(pIconInfo->hbmMask, sizeof(BITMAP), pbmpMask) )
   {
      return FALSE;
   }

   return TRUE;
}

//*************************************************************************************************
//        Write one icon directory entry - specify the index of the image
//*************************************************************************************************
static UINT WriteIconDirectoryEntry(HANDLE hFile, HICON hIcon, UINT nImageOffset)
{
   ICONINFO iconInfo;
   ICONDIR  iconDir;

   BITMAP bmpColor;
   BITMAP bmpMask;

   UINT nWritten;
   UINT nColorCount;
   UINT nImageBytes;

   GetIconBitmapInfo(hIcon, &iconInfo, &bmpColor, &bmpMask);

   nImageBytes = NumBitmapBytes(&bmpColor) + NumBitmapBytes(&bmpMask);

   if( bmpColor.bmBitsPixel >= 8 )
   {
      nColorCount = 0;
   }
   else
   {
      nColorCount = 1 << ( bmpColor.bmBitsPixel * bmpColor.bmPlanes );
   }

   // Create the ICONDIR structure
   iconDir.bWidth        = ( BYTE ) bmpColor.bmWidth;
   iconDir.bHeight       = ( BYTE ) bmpColor.bmHeight;
   iconDir.bColorCount   = ( BYTE ) nColorCount;
   iconDir.bReserved     = 0;
   iconDir.wPlanes       = bmpColor.bmPlanes;
   iconDir.wBitCount     = bmpColor.bmBitsPixel;
   iconDir.dwBytesInRes  = sizeof(BITMAPINFOHEADER) + nImageBytes;
   iconDir.dwImageOffset = nImageOffset;

   // Write to disk
   WriteFile(hFile, ( LPVOID ) &iconDir, sizeof(iconDir), ( LPDWORD ) &nWritten, nullptr);

   // Free resources
   DeleteObject(iconInfo.hbmColor);
   DeleteObject(iconInfo.hbmMask);

   return nWritten;
}

static UINT WriteIconData(HANDLE hFile, HBITMAP hBitmap)
{
   BITMAP bmp;
   BYTE * pIconData;

   UINT nBitmapBytes;
   UINT nWritten;

   GetObject(hBitmap, sizeof(BITMAP), &bmp);

   nBitmapBytes = NumBitmapBytes(&bmp);

   pIconData = ( BYTE * ) malloc(nBitmapBytes);

   GetBitmapBits(hBitmap, nBitmapBytes, pIconData);

   // bitmaps are stored inverted (vertically) when on disk..
   // so write out each line in turn, starting at the bottom + working
   // towards the top of the bitmap. Also, the bitmaps are stored in packed
   // in memory - scanlines are NOT 32bit aligned, just 1-after-the-other
   for( int i = bmp.bmHeight - 1; i >= 0; i-- )
   {
      // Write the bitmap scanline
      WriteFile(
         hFile,
         pIconData + ( i * bmp.bmWidthBytes ),                          // calculate offset to the line
         bmp.bmWidthBytes,                                              // 1 line of BYTES
         ( LPDWORD ) &nWritten,
         nullptr );

      // extend to a 32bit boundary (in the file) if necessary
      if( bmp.bmWidthBytes & 3 )
      {
         DWORD padding = 0;
         WriteFile(hFile, ( LPVOID ) &padding, 4 - bmp.bmWidthBytes, ( LPDWORD ) &nWritten, nullptr);
      }
   }

   free(pIconData);

   return nBitmapBytes;
}

//*************************************************************************************************
//        Create a .ICO file, using the specified array of HICON images
//*************************************************************************************************
BOOL SaveIconToFile(TCHAR * szIconFile, HICON hIcon[], int nNumIcons)
{
   HANDLE hFile;
   int *  pImageOffset;

   if( hIcon == 0 || nNumIcons < 1 )
   {
      return FALSE;
   }

   // Save icon to disk:
   hFile = CreateFile(szIconFile, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);

   if( hFile == INVALID_HANDLE_VALUE )
   {
      return FALSE;
   }

   //
   //        Write the iconheader first of all
   //
   WriteIconHeader(hFile, nNumIcons);

   //
   //        Leave space for the IconDir entries
   //
   SetFilePointer( hFile, sizeof(ICONDIR) * nNumIcons, 0, FILE_CURRENT );

   pImageOffset = ( int * ) malloc(nNumIcons * sizeof(int));

   //
   //        Now write the actual icon images
   //
   for( int i = 0; i < nNumIcons; i++ )
   {
      ICONINFO iconInfo;
      BITMAP   bmpColor, bmpMask;

      GetIconBitmapInfo(hIcon[i], &iconInfo, &bmpColor, &bmpMask);

      // record the file-offset of the icon image for when we write the icon directories
      pImageOffset[i] = SetFilePointer( hFile, 0, 0, FILE_CURRENT );

      // bitmapinfoheader + colortable
      WriteIconImageHeader(hFile, &bmpColor, &bmpMask);

      // color and mask bitmaps
      WriteIconData(hFile, iconInfo.hbmColor);
      WriteIconData(hFile, iconInfo.hbmMask);

      DeleteObject(iconInfo.hbmColor);
      DeleteObject(iconInfo.hbmMask);
   }

   //
   //        Lastly, skip back and write the icon directories.
   //
   SetFilePointer( hFile, sizeof(ICONHEADER), 0, FILE_BEGIN );

   for( int i = 0; i < nNumIcons; i++ )
   {
      WriteIconDirectoryEntry(hFile, hIcon[i], pImageOffset[i]);
   }

   free(pImageOffset);

   // finished
   CloseHandle(hFile);

   return TRUE;
}

//*************************************************************************************************
//        Save the icon resources to disk
//*************************************************************************************************
HB_FUNC( C_SAVEHICONTOFILE )
{
#ifndef UNICODE
   TCHAR * szIconFile = ( TCHAR * ) hb_parc(1);
#else
   TCHAR * szIconFile = ( TCHAR * ) AnsiToWide(( char * ) hb_parc(1));
#endif
   HICON    hIcon[9];
   PHB_ITEM pArray = hb_param(2, Harbour::Item::ARRAY);
   int      nLen;

   if( pArray && ( ( nLen = ( int ) hb_arrayLen(pArray) ) > 0 ) )
   {
      for( int i = 0; i < nLen; i++ )
      {
         hIcon[i] = reinterpret_cast<HICON>(hb_arrayGetNL( pArray, i + 1 ));
      }

      if( SaveIconToFile(szIconFile, hIcon, hb_parnidef(3, nLen)) )
      {
         hb_retl(true);

         // clean up
         for( int i = 0; i < nLen; i++ )
         {
            DestroyIcon(hIcon[i]);
         }
      }
      else
      {
         hb_retl(false);
      }
   }
   else
   {
      hb_retl(false);
   }

#ifdef UNICODE
   hb_xfree(szIconFile);
#endif
}

BOOL bmp_SaveFile(HBITMAP hBitmap, TCHAR * FileName)
{
   HGLOBAL hBits;
   LPBYTE  lp_hBits;
   HANDLE  hFile;
   HDC     memDC;
   BITMAPFILEHEADER BIFH;
   BITMAPINFO       Bitmap_Info;
   BITMAP bm;
   DWORD  nBytes_Bits, nBytes_Written;
   BOOL   ret;

   memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);
   GetObject(hBitmap, sizeof(BITMAP), ( LPBYTE ) &bm);

   bm.bmBitsPixel  = 24;
   bm.bmWidthBytes = ( bm.bmWidth * bm.bmBitsPixel + 31 ) / 32 * 4;
   nBytes_Bits     = ( DWORD ) ( bm.bmWidthBytes * labs(bm.bmHeight) );

   BIFH.bfType      = ( 'M' << 8 ) + 'B';
   BIFH.bfSize      = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + nBytes_Bits;
   BIFH.bfReserved1 = 0;
   BIFH.bfReserved2 = 0;
   BIFH.bfOffBits   = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

   Bitmap_Info.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   Bitmap_Info.bmiHeader.biWidth         = bm.bmWidth;
   Bitmap_Info.bmiHeader.biHeight        = bm.bmHeight;
   Bitmap_Info.bmiHeader.biPlanes        = 1;
   Bitmap_Info.bmiHeader.biBitCount      = 24;
   Bitmap_Info.bmiHeader.biCompression   = BI_RGB;
   Bitmap_Info.bmiHeader.biSizeImage     = 0;
   Bitmap_Info.bmiHeader.biXPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biYPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biClrUsed       = 0;
   Bitmap_Info.bmiHeader.biClrImportant  = 0;

   hBits = GlobalAlloc(GHND, ( DWORD ) nBytes_Bits);
   if( hBits == nullptr )
   {
      return FALSE;
   }

   lp_hBits = ( LPBYTE ) GlobalLock(hBits);

   GetDIBits(memDC, hBitmap, 0, Bitmap_Info.bmiHeader.biHeight, ( LPVOID ) lp_hBits, &Bitmap_Info, DIB_RGB_COLORS);

   hFile = CreateFile(FileName, GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, nullptr);

   if( hFile != INVALID_HANDLE_VALUE )
   {
      WriteFile(hFile, ( LPBYTE ) &BIFH, sizeof(BITMAPFILEHEADER), &nBytes_Written, nullptr);
      WriteFile(hFile, ( LPBYTE ) &Bitmap_Info.bmiHeader, sizeof(BITMAPINFOHEADER), &nBytes_Written, nullptr);
      WriteFile(hFile, ( LPBYTE ) lp_hBits, nBytes_Bits, &nBytes_Written, nullptr);
      CloseHandle(hFile);
      ret = TRUE;
   }
   else
   {
      ret = FALSE;
   }

   GlobalUnlock(hBits);
   GlobalFree(hBits);

   DeleteDC(memDC);
   return ret;
}

HIMAGELIST HMG_ImageListLoadFirst(const char * FileName, int cGrow, int Transparent, int * nWidth, int * nHeight)
{
   HIMAGELIST hImageList;
   HBITMAP    hBitmap;
   BITMAP     Bmp;
   TCHAR      TempPathFileName[MAX_PATH];

   hBitmap = HMG_LoadPicture(FileName, -1, -1, nullptr, 0, 0, -1, 0, false, 255);
   if( hBitmap == nullptr )
   {
      return nullptr;
   }

   GetObject(hBitmap, sizeof(BITMAP), &Bmp);

   if( nWidth != nullptr )
   {
      *nWidth = Bmp.bmWidth;
   }

   if( nHeight != nullptr )
   {
      *nHeight = Bmp.bmHeight;
   }

   GetTempPath(MAX_PATH, TempPathFileName);
   lstrcat(TempPathFileName, TEXT("_MG_temp.BMP"));
   bmp_SaveFile(hBitmap, TempPathFileName);
   DeleteObject(hBitmap);

   if( Transparent == 1 )
   {
      hImageList = ImageList_LoadImage(GetResources(), TempPathFileName, Bmp.bmWidth, cGrow, CLR_DEFAULT, IMAGE_BITMAP, LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT);
   }
   else
   {
      hImageList = ImageList_LoadImage(GetResources(), TempPathFileName, Bmp.bmWidth, cGrow, CLR_NONE, IMAGE_BITMAP, LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS);
   }

   DeleteFile(TempPathFileName);

   return hImageList;
}

void HMG_ImageListAdd(HIMAGELIST hImageList, const char * FileName, int Transparent)
{
   HBITMAP hBitmap;

   if( hImageList == nullptr )
   {
      return;
   }

   hBitmap = HMG_LoadPicture(FileName, -1, -1, nullptr, 0, 0, -1, 0, false, 255);
   if( hBitmap == nullptr )
   {
      return;
   }

   if( Transparent == 1 )
   {
      ImageList_AddMasked(hImageList, hBitmap, CLR_DEFAULT);
   }
   else
   {
      ImageList_AddMasked(hImageList, hBitmap, CLR_NONE);
   }

   DeleteObject(hBitmap);
}

void HMG_ImageListAdd(HIMAGELIST hImageList, char * FileName, int Transparent) // TODO: remover quando não for mais necessária
{
   HBITMAP hBitmap;

   if( hImageList == nullptr )
   {
      return;
   }

   hBitmap = HMG_LoadPicture(FileName, -1, -1, nullptr, 0, 0, -1, 0, false, 255);
   if( hBitmap == nullptr )
   {
      return;
   }

   if( Transparent == 1 )
   {
      ImageList_AddMasked(hImageList, hBitmap, CLR_DEFAULT);
   }
   else
   {
      ImageList_AddMasked(hImageList, hBitmap, CLR_NONE);
   }

   DeleteObject(hBitmap);
}
