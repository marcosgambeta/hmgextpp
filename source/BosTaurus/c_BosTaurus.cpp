/*
 * BOS TAURUS - Graphic Library for HMG
 *
 * Copyright 2012-2016 by Dr. Claudio Soto (from Uruguay).
 * mail: <srvet@adinet.com.uy>
 * blog: http://srvet.blogspot.com
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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301, USA
 * (or visit their web site at http://www.gnu.org/).
 *
 * As a special exception, you have permission for additional uses of the text
 * contained in this release of BOS TAURUS.
 *
 * The exception is that, if you link the BOS TAURUS library with other
 * files to produce an executable, this does not by itself cause the resulting
 * executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of linking the
 * BOS TAURUS library code into it.
 */

// *******************************************************************************
// * ARCHIVO:  c_BosTaurus.c
// * LENGUAJE: HMG
// * FECHA:    Setiembre 2012
// * AUTOR:    Dr. CLAUDIO SOTO
// * PAIS:     URUGUAY
// * E-MAIL:   srvet@adinet.com.uy
// * BLOG:     http://srvet.blogspot.com
// *******************************************************************************

// *******************************************************************************
// * FUNCTIONS in C
// *******************************************************************************

#define WINVER 0x0501  // minimum requirements: Windows XP

#include "mgdefs.hpp"
#include <commctrl.h>
#include <olectl.h>
#include <time.h>
#include <math.h>

#define HB_STORVNI  hb_storvni

#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#include <math.h>
#endif

extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);

// ::::::::::::::::::::::::::::::::::::
// :::   INTERNAL Functions in C    :::
// ::::::::::::::::::::::::::::::::::::

//*************************************************************************************************
//* bt_MsgDebugInfo ("Info: Text=%s  Num1=%d  Num2=%d", String, Num1, Num2);
//*************************************************************************************************
/*
void bt_MsgDebugInfo (char *Format, ...)
{
   char Buffer [1024];
   va_list Args;
   va_start (Args, Format);
   vsprintf (Buffer, Format, Args);
   MessageBox (nullptr, Buffer, "BT - DEBUG INFO", MB_OK);
}
*/

//*************************************************************************************************
//* bt_bmp_adjust_rect (&Width1, &Height1, &Width2, &Height2, Mode_Stretch)
//*************************************************************************************************

// Mode_Stretch
#define BT_SCALE   0
#define BT_STRETCH 1
#define BT_COPY    3

static void bt_bmp_adjust_rect(int * Width1, int * Height1, int * Width2, int * Height2, int Mode_Stretch)
{
   switch( Mode_Stretch ) {
      case BT_SCALE:
         if( static_cast<int>(*Width2 * *Height1 / *Height2) <= *Width1 ) {
            *Width1 = static_cast<int>(*Width2 * *Height1 / *Height2);
         } else {
            *Height1 = static_cast<int>(*Height2 * *Width1 / *Width2);
         }
         break;

      case BT_STRETCH:
         break;

      case BT_COPY:
         *Width1  = *Width2 = HB_MIN(*Width1, *Width2);
         *Height1 = *Height2 = HB_MIN(*Height1, *Height2);
   }
}

//*************************************************************************************************
//* bt_bmp_is_24bpp (hBitmap) ---> Return TRUE or FALSE
//*************************************************************************************************
/*
BOOL bt_bmp_is_24bpp (HBITMAP hBitmap)
{
   BITMAP bm;
   GetObject (hBitmap, sizeof(BITMAP), (LPBYTE)&bm);
   if( bm.bmBitsPixel == 24 ) {
      return TRUE;
   } else {
      return FALSE;
   }
}
*/

//*************************************************************************************************
//* bt_bmp_create_24bpp (int Width, int Height) ---> Return hBITMAP
//*************************************************************************************************

static HBITMAP bt_bmp_create_24bpp(int Width, int Height)
{
   HDC hDC_mem = CreateCompatibleDC(nullptr);

   BITMAPINFO Bitmap_Info;
   Bitmap_Info.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   Bitmap_Info.bmiHeader.biWidth         = Width;
   Bitmap_Info.bmiHeader.biHeight        = -Height;
   Bitmap_Info.bmiHeader.biPlanes        = 1;
   Bitmap_Info.bmiHeader.biBitCount      = 24;
   Bitmap_Info.bmiHeader.biCompression   = BI_RGB;
   Bitmap_Info.bmiHeader.biSizeImage     = 0;
   Bitmap_Info.bmiHeader.biXPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biYPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biClrUsed       = 0;
   Bitmap_Info.bmiHeader.biClrImportant  = 0;

   LPBYTE Bitmap_mem_pBits;

   HBITMAP hBitmap_mem = CreateDIBSection(hDC_mem, ( BITMAPINFO * ) &Bitmap_Info, DIB_RGB_COLORS, ( VOID ** ) &Bitmap_mem_pBits, nullptr, 0);

   DeleteDC(hDC_mem);

   return hBitmap_mem;
}

//*************************************************************************************************
//* bt_bmp_convert_to_24bpp (hBitmap, IsDelete_hBitmap_Original) ---> Return New_hBitmap
//*************************************************************************************************

// IsDelete_hBitmap_Original
#define BMP_DELETE_ORIGINAL_HBITMAP      TRUE
#define BMP_NOT_DELETE_ORIGINAL_HBITMAP  FALSE

#if 0 // not used
static HBITMAP bt_bmp_convert_to_24bpp(HBITMAP hBitmap_Original, BOOL IsDelete_hBitmap_Original)
{
   BITMAP bm;
   GetObject(hBitmap_Original, sizeof(BITMAP), static_cast<LPBYTE>(&bm));
   HBITMAP hBitmap_New = bt_bmp_create_24bpp(bm.bmWidth, bm.bmHeight);

   HDC memDC1 = CreateCompatibleDC(nullptr);
   SelectObject(memDC1, hBitmap_Original);

   HDC memDC2 = CreateCompatibleDC(nullptr);
   SelectObject(memDC2, hBitmap_New);

   StretchBlt(memDC2, 0, 0, bm.bmWidth, bm.bmHeight, memDC1, 0, 0, bm.bmWidth, bm.bmHeight, SRCCOPY);

   DeleteDC(memDC1);
   DeleteDC(memDC2);

   if( IsDelete_hBitmap_Original ) {
      DeleteObject(hBitmap_Original);
   }

   return hBitmap_New;
}
#endif

//*************************************************************************************************
// bt_LoadFileFromResources (FileName, TypeResource) ---> Return hGlobalAlloc
//*************************************************************************************************
static HGLOBAL bt_LoadFileFromResources(const TCHAR * FileName, const TCHAR * TypeResource)
{
   HRSRC hResourceData = FindResource(nullptr, FileName, TypeResource);
   if( hResourceData == nullptr ) {
      return nullptr;
   }

   HGLOBAL hGlobalResource = LoadResource(nullptr, hResourceData);
   if( hGlobalResource == nullptr ) {
      return nullptr;
   }

   LPVOID lpGlobalResource = LockResource(hGlobalResource);
   if( lpGlobalResource == nullptr ) {
      return nullptr;
   }

   DWORD nFileSize = SizeofResource(nullptr, hResourceData);

   HGLOBAL hGlobalAlloc = GlobalAlloc(GHND, nFileSize);
   if( hGlobalAlloc == nullptr ) {
      FreeResource(hGlobalResource);
      return nullptr;
   }

   LPVOID lpGlobalAlloc = GlobalLock(hGlobalAlloc);
   memcpy(lpGlobalAlloc, lpGlobalResource, nFileSize);
   GlobalUnlock(hGlobalAlloc);

   FreeResource(hGlobalResource);

   return hGlobalAlloc;
}

//*************************************************************************************************
//  bt_LoadFileFromDisk (FileName) ---> Return hGlobalAlloc
//*************************************************************************************************
static HGLOBAL bt_LoadFileFromDisk(const TCHAR * FileName)
{
   HANDLE hFile = CreateFile(FileName, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
   if( hFile == INVALID_HANDLE_VALUE ) {
      return nullptr;
   }

   DWORD nFileSize = GetFileSize(hFile, nullptr);
   if( nFileSize == INVALID_FILE_SIZE ) {
      CloseHandle(hFile);
      return nullptr;
   }

   HGLOBAL hGlobalAlloc = GlobalAlloc(GHND, nFileSize);
   if( hGlobalAlloc == nullptr ) {
      CloseHandle(hFile);
      return nullptr;
   }

   LPVOID lpGlobalAlloc = GlobalLock(hGlobalAlloc);
   DWORD nReadByte;
   ReadFile(hFile, lpGlobalAlloc, nFileSize, &nReadByte, nullptr);
   GlobalUnlock(hGlobalAlloc);

   CloseHandle(hFile);

   return hGlobalAlloc;
}

bool _bt_OleInitialize_Flag_ = false;

//*************************************************************************************************
//  bt_LoadOLEPicture (FileName, TypePicture) ---> Return hBitmap  (Load GIF and JPG image)
//*************************************************************************************************
static HBITMAP bt_LoadOLEPicture(const TCHAR * FileName, const TCHAR * TypePictureResource)
{
   IStream * iStream;
   HGLOBAL   hGlobalAlloc;
   LONG      hmWidth, hmHeight;

   if( TypePictureResource != nullptr ) {
      hGlobalAlloc = bt_LoadFileFromResources(FileName, TypePictureResource);
   } else {
      hGlobalAlloc = bt_LoadFileFromDisk(FileName);
   }

   if( hGlobalAlloc == nullptr ) {
      return nullptr;
   }

   if( _bt_OleInitialize_Flag_ == false ) {
      _bt_OleInitialize_Flag_ = true;
      OleInitialize(nullptr);
   }

   IPicture * iPicture = nullptr;
   CreateStreamOnHGlobal(hGlobalAlloc, TRUE, &iStream);
   OleLoadPicture(iStream, 0, TRUE, IID_IPicture, ( LPVOID * ) &iPicture);
   if( iPicture == nullptr ) {
      GlobalFree(hGlobalAlloc);
      return nullptr;
   }

   iPicture->lpVtbl->get_Width(iPicture, &hmWidth);
   iPicture->lpVtbl->get_Height(iPicture, &hmHeight);

   HDC memDC = CreateCompatibleDC(nullptr);
   //SetStretchBltMode (memDC, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(memDC, &Point);
   SetStretchBltMode(memDC, HALFTONE);
   SetBrushOrgEx(memDC, Point.x, Point.y, nullptr);

   // Convert HiMetric to Pixel
   #define HIMETRIC_PER_INCH  2540                                                            // Number of HIMETRIC units per INCH
   #define bt_LOGHIMETRIC_TO_PIXEL(hm, ppli)  MulDiv((hm), (ppli), HIMETRIC_PER_INCH) // ppli = Point per Logic Inch
   #define bt_PIXEL_TO_LOGHIMETRIC(px, ppli)  MulDiv((px), HIMETRIC_PER_INCH, (ppli)) // ppli = Point per Logic Inch

   INT pxWidth  = bt_LOGHIMETRIC_TO_PIXEL(hmWidth, GetDeviceCaps(memDC, LOGPIXELSX));
   INT pxHeight = bt_LOGHIMETRIC_TO_PIXEL(hmHeight, GetDeviceCaps(memDC, LOGPIXELSY));

   HBITMAP hBitmap = bt_bmp_create_24bpp(pxWidth, pxHeight);
   SelectObject(memDC, hBitmap);

   iPicture->lpVtbl->Render(iPicture, memDC, 0, 0, pxWidth, pxHeight, 0, hmHeight, hmWidth, -hmHeight, nullptr);
   iPicture->lpVtbl->Release(iPicture);
   iStream->lpVtbl->Release(iStream);

   DeleteDC(memDC);
   GlobalFree(hGlobalAlloc);

   return hBitmap;
}

//*************************************************************************************************
//  GDI Plus: Functions and Definitions
//*************************************************************************************************

// Begin GDIPLUS Definitions
enum GpStatus
{
   Ok                        = 0,
   GenericError              = 1,
   InvalidParameter          = 2,
   OutOfMemory               = 3,
   ObjectBusy                = 4,
   InsufficientBuffer        = 5,
   NotImplemented            = 6,
   Win32Error                = 7,
   WrongState                = 8,
   Aborted                   = 9,
   FileNotFound              = 10,
   ValueOverflow             = 11,
   AccessDenied              = 12,
   UnknownImageFormat        = 13,
   FontFamilyNotFound        = 14,
   FontStyleNotFound         = 15,
   NotTrueTypeFont           = 16,
   UnsupportedGdiplusVersion = 17,
   GdiplusNotInitialized     = 18,
   PropertyNotFound          = 19,
   PropertyNotSupported      = 20,
   ProfileNotFound           = 21
};

#define WINGDIPAPI  __stdcall
#define GDIPCONST   const
using GpBitmap = void;
using GpGraphics = void;
using GpImage = void;
using DebugEventProc = void *;

struct GdiplusStartupInput
{
   UINT32 GdiplusVersion;
   DebugEventProc DebugEventCallback;
   BOOL SuppressBackgroundThread;
   BOOL SuppressExternalCodecs;
};

#ifdef _MSC_VER
   using NotificationHookProc = GpStatus (WINGDIPAPI *)(ULONG_PTR * token);
   using NotificationUnhookProc = VOID (WINGDIPAPI *)(ULONG_PTR token);
#else
   using NotificationHookProc = GpStatus WINGDIPAPI (*)(ULONG_PTR * token);
   using NotificationUnhookProc = VOID WINGDIPAPI (*)(ULONG_PTR token);
#endif

struct GdiplusStartupOutput
{
   NotificationHookProc   NotificationHook;
   NotificationUnhookProc NotificationUnhook;
};

struct ImageCodecInfo
{
   CLSID   Clsid;
   GUID    FormatID;
   WCHAR * CodecName;
   WCHAR * DllName;
   WCHAR * FormatDescription;
   WCHAR * FilenameExtension;
   WCHAR * MimeType;
   DWORD   Flags;
   DWORD   Version;
   DWORD   SigCount;
   DWORD   SigSize;
   BYTE *  SigPattern;
   BYTE *  SigMask;
};

struct EncoderParameter
{
   GUID   Guid;
   ULONG  NumberOfValues;
   ULONG  Type;
   VOID * Value;
};

struct EncoderParameters
{
   UINT Count;
   EncoderParameter Parameter[1];
};

using ARGB = ULONG;
using Func_GdiPlusStartup = GpStatus (WINGDIPAPI *)(ULONG_PTR *, GDIPCONST GdiplusStartupInput *, GdiplusStartupOutput *);
using Func_GdiPlusShutdown = VOID (WINGDIPAPI *)(ULONG_PTR);
using Func_GdipCreateBitmapFromStream = GpStatus (WINGDIPAPI *)(IStream *, GpBitmap **);
using Func_GdipCreateHBITMAPFromBitmap = GpStatus (WINGDIPAPI *)(GpBitmap *, HBITMAP *, ARGB);
using Func_GdipCreateFromHDC = GpStatus (WINGDIPAPI *)(HDC, GpGraphics **);
using Func_GdipDrawImageI = GpStatus (WINGDIPAPI *)(GpGraphics *, GpImage *, INT, INT);

using Func_GdipGetImageEncodersSize = GpStatus (WINGDIPAPI *)(UINT *, UINT *);
using Func_GdipGetImageEncoders = GpStatus (WINGDIPAPI *)(UINT, UINT, ImageCodecInfo *);
using Func_GdipSaveImageToFile = GpStatus (WINGDIPAPI *)(GpImage *, GDIPCONST WCHAR *, GDIPCONST CLSID *, GDIPCONST EncoderParameters *);
using Func_GdipLoadImageFromStream = GpStatus (WINGDIPAPI *)(IStream *, GpImage **);

// End GDIPLUS Definitions

// GDI Plus Functions
Func_GdiPlusStartup  GdiPlusStartup;
Func_GdiPlusShutdown GdiPlusShutdown;
Func_GdipCreateBitmapFromStream  GdipCreateBitmapFromStream;
Func_GdipCreateHBITMAPFromBitmap GdipCreateHBITMAPFromBitmap;

Func_GdipGetImageEncodersSize GdipGetImageEncodersSize;
Func_GdipGetImageEncoders     GdipGetImageEncoders;
Func_GdipLoadImageFromStream  GdipLoadImageFromStream;
Func_GdipSaveImageToFile      GdipSaveImageToFile;

// Global Variables
HMODULE GdiPlusHandle = nullptr; // VOID *    GdiPlusHandle = nullptr;
ULONG_PTR GdiPlusToken;
GdiplusStartupInput GDIPlusStartupInput;

//BOOL bt_Load_GDIplus();
//BOOL bt_Release_GDIplus();

//  Load Library GDI Plus
static BOOL bt_Load_GDIplus()
{
   GdiPlusHandle = LoadLibrary(TEXT("GdiPlus.dll"));
   if( GdiPlusHandle == nullptr ) {
      return FALSE;
   }

   GdiPlusStartup  = ( Func_GdiPlusStartup ) wapi_GetProcAddress(GdiPlusHandle, "GdiplusStartup");
   GdiPlusShutdown = ( Func_GdiPlusShutdown ) wapi_GetProcAddress(GdiPlusHandle, "GdiplusShutdown");
   GdipCreateBitmapFromStream  = ( Func_GdipCreateBitmapFromStream ) wapi_GetProcAddress(GdiPlusHandle, "GdipCreateBitmapFromStream");
   GdipCreateHBITMAPFromBitmap = ( Func_GdipCreateHBITMAPFromBitmap ) wapi_GetProcAddress(GdiPlusHandle, "GdipCreateHBITMAPFromBitmap");

   GdipGetImageEncodersSize = ( Func_GdipGetImageEncodersSize ) wapi_GetProcAddress(GdiPlusHandle, "GdipGetImageEncodersSize");
   GdipGetImageEncoders     = ( Func_GdipGetImageEncoders ) wapi_GetProcAddress(GdiPlusHandle, "GdipGetImageEncoders");
   GdipLoadImageFromStream  = ( Func_GdipLoadImageFromStream ) wapi_GetProcAddress(GdiPlusHandle, "GdipLoadImageFromStream");
   GdipSaveImageToFile      = ( Func_GdipSaveImageToFile ) wapi_GetProcAddress(GdiPlusHandle, "GdipSaveImageToFile");

   if( GdiPlusStartup == nullptr ||
       GdiPlusShutdown == nullptr ||
       GdipCreateBitmapFromStream == nullptr ||
       GdipCreateHBITMAPFromBitmap == nullptr ||
       GdipGetImageEncodersSize == nullptr ||
       GdipGetImageEncoders == nullptr ||
       GdipLoadImageFromStream == nullptr ||
       GdipSaveImageToFile == nullptr ) {
      FreeLibrary(GdiPlusHandle);
      GdiPlusHandle = nullptr;
      return FALSE;
   }

   GDIPlusStartupInput.GdiplusVersion           = 1;
   GDIPlusStartupInput.DebugEventCallback       = nullptr;
   GDIPlusStartupInput.SuppressBackgroundThread = FALSE;
   GDIPlusStartupInput.SuppressExternalCodecs   = FALSE;

   if( GdiPlusStartup(&GdiPlusToken, &GDIPlusStartupInput, nullptr) ) {
      FreeLibrary(GdiPlusHandle);
      GdiPlusHandle = nullptr;
      return FALSE;
   }
   return TRUE;
}

// Release Library GDI Plus
static BOOL bt_Release_GDIplus()
{
   if( GdiPlusHandle == nullptr ) {
      return FALSE;
   }

   GdiPlusShutdown(GdiPlusToken);
   FreeLibrary(GdiPlusHandle);
   GdiPlusHandle = nullptr;
   return TRUE;
}

//*************************************************************************************************************
//  bt_LoadGDIPlusPicture (FileName, TypePicture) ---> Return hBitmap  (Load BMP, GIF, JPG, TIF and PNG image)
//*************************************************************************************************************

static HBITMAP bt_LoadGDIPlusPicture(const TCHAR * FileName, const TCHAR * TypePictureResource)
{
   HGLOBAL hGlobalAlloc;

   if( bt_Load_GDIplus() == FALSE ) {
      return nullptr;
   }

   if( TypePictureResource != nullptr ) {
      hGlobalAlloc = bt_LoadFileFromResources(FileName, TypePictureResource);
   } else {
      hGlobalAlloc = bt_LoadFileFromDisk(FileName);
   }

   if( hGlobalAlloc == nullptr ) {
      return nullptr;
   }

   IStream * iStream = nullptr;
   HBITMAP hBitmap = nullptr;
   if( CreateStreamOnHGlobal(hGlobalAlloc, FALSE, &iStream) == S_OK )
   {
      ARGB BkColor = 0xFF000000UL;
      GpBitmap * pGpBitmap;
      GdipCreateBitmapFromStream(iStream, &pGpBitmap);
      GdipCreateHBITMAPFromBitmap(pGpBitmap, &hBitmap, BkColor);
      iStream->lpVtbl->Release(iStream);
   }

   bt_Release_GDIplus();
   GlobalFree(hGlobalAlloc);
   return hBitmap;
}

//***********************************************************************************************************************
// bt_SaveGDIPlusPicture (hBitmap, FileName, TypePicture) // Return TRUE/FALSE  (Save BMP, GIF, JPG, TIF and PNG image)
//***********************************************************************************************************************

// Internal Function: bt_Bitmap_To_Stream () ---> Return hGlobalAlloc
static HGLOBAL bt_Bitmap_To_Stream(HBITMAP hBitmap)
{
   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);
   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   bm.bmBitsPixel    = 24;
   bm.bmWidthBytes   = (bm.bmWidth * bm.bmBitsPixel + 31) / 32 * 4;
   DWORD nBytes_Bits = static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight));

   BITMAPFILEHEADER BIFH;
   BIFH.bfType      = ('M' << 8) + 'B';
   BIFH.bfSize      = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + nBytes_Bits;
   BIFH.bfReserved1 = 0;
   BIFH.bfReserved2 = 0;
   BIFH.bfOffBits   = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

   BITMAPINFO Bitmap_Info;
   Bitmap_Info.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   Bitmap_Info.bmiHeader.biWidth         = bm.bmWidth;
   Bitmap_Info.bmiHeader.biHeight        = bm.bmHeight;
   Bitmap_Info.bmiHeader.biPlanes        = 1;
   Bitmap_Info.bmiHeader.biBitCount      = 24;
   Bitmap_Info.bmiHeader.biCompression   = BI_RGB;
   Bitmap_Info.bmiHeader.biSizeImage     = 0;       //nBytes_Bits;
   Bitmap_Info.bmiHeader.biXPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biYPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biClrUsed       = 0;
   Bitmap_Info.bmiHeader.biClrImportant  = 0;

   HGLOBAL hGlobalAlloc = GlobalAlloc(GHND, static_cast<DWORD>(BIFH.bfSize));
   if( hGlobalAlloc == nullptr ) {
      return nullptr;
   }

   LPBYTE lp_hGlobalAlloc = static_cast<LPBYTE>(GlobalLock(hGlobalAlloc));

   memcpy(lp_hGlobalAlloc, &BIFH, sizeof(BITMAPFILEHEADER));
   memcpy((lp_hGlobalAlloc + sizeof(BITMAPFILEHEADER)), &Bitmap_Info, sizeof(BITMAPINFO));
   GetDIBits(memDC, hBitmap, 0, Bitmap_Info.bmiHeader.biHeight, static_cast<LPVOID>(lp_hGlobalAlloc + BIFH.bfOffBits), &Bitmap_Info, DIB_RGB_COLORS);

   GlobalUnlock(hGlobalAlloc);
   DeleteDC(memDC);

   return hGlobalAlloc;
}

// Internal Function: bt_GetEncoderCLSID () ---> Return TRUE/FALSE
static BOOL bt_GetEncoderCLSID(WCHAR * format, CLSID * pClsid)
{
   UINT num  = 0;          // number of image encoders
   UINT size = 0;          // size of the image encoder array in bytes
   GdipGetImageEncodersSize(&num, &size);
   if( size == 0 ) {
      return FALSE;
   }

   ImageCodecInfo * pImageCodecInfo = ( ImageCodecInfo * ) malloc(size);
   if( pImageCodecInfo == nullptr ) {
      return FALSE;
   }

   GdipGetImageEncoders(num, size, pImageCodecInfo);

   for( UINT i = 0; i < num; ++i ) {
      if( wcscmp(pImageCodecInfo[i].MimeType, format) == 0 ) {
         *pClsid = pImageCodecInfo[i].Clsid;
         free(pImageCodecInfo);
         return TRUE;
      }
   }
   free(pImageCodecInfo);
   return FALSE;
}

// bt_SaveGDIPlusPicture (hBitmap, FileName, TypePicture) ---> Return TRUE/FALSE  (Save BMP, GIF, JPG, TIF and PNG image)
#define BT_FILEFORMAT_BMP  0
#define BT_FILEFORMAT_JPG  1
#define BT_FILEFORMAT_GIF  2
#define BT_FILEFORMAT_TIF  3
#define BT_FILEFORMAT_PNG  4

static BOOL bt_SaveGDIPlusPicture(HBITMAP hBitmap, const TCHAR * FileName, INT TypePicture)
{
   HGLOBAL   hGlobalAlloc;
   IStream * iStream;
   WCHAR     format[21];

   switch( TypePicture ) {
      case BT_FILEFORMAT_BMP:
         wcscpy(format, L"image/bmp");
         break;
      case BT_FILEFORMAT_JPG:
         wcscpy(format, L"image/jpeg");
         break;
      case BT_FILEFORMAT_GIF:
         wcscpy(format, L"image/gif");
         break;
      case BT_FILEFORMAT_TIF:
         wcscpy(format, L"image/tiff");
         break;
      case BT_FILEFORMAT_PNG:
         wcscpy(format, L"image/png");
         break;
      default:
         return FALSE;
   }

   if( bt_Load_GDIplus() == FALSE ) {
      return FALSE;
   }

   CLSID encoderClsid;
   BOOL result = bt_GetEncoderCLSID(( WCHAR * ) format, &encoderClsid);

   if( result == TRUE ) {
      hGlobalAlloc = bt_Bitmap_To_Stream(hBitmap);
      iStream      = nullptr;
      if( CreateStreamOnHGlobal(hGlobalAlloc, FALSE, &iStream) == S_OK ) {
         WCHAR wFileName[MAX_PATH];
         #ifdef UNICODE
         lstrcpy(wFileName, FileName);
         #else
         MultiByteToWideChar(CP_ACP, 0, FileName, -1, wFileName, MAX_PATH);
         #endif

         GpImage * image;
         INT ret1 = GdipLoadImageFromStream(iStream, &image);
         INT ret2 = GdipSaveImageToFile(image, wFileName, &encoderClsid, nullptr);  // Save the image

         iStream->lpVtbl->Release(iStream);
         bt_Release_GDIplus();

         GlobalFree(hGlobalAlloc);   // ADD, September 2016

         return (ret1 == 0 && ret2 == 0) ? TRUE : FALSE;
      }
   }
   bt_Release_GDIplus();
   return FALSE; // The File encoder is not installed
}

// ::::::::::::::::::::::::::::::::
// :::       DC Functions       :::
// ::::::::::::::::::::::::::::::::

//*****************************************************************************************
//* BT_STRUCT (Type, hWnd, hBitmap, hDC, PaintStruct)
//*****************************************************************************************

struct BT_STRUCT
{
   INT         Type;
   HWND        hWnd;
   HDC         hDC;
   PAINTSTRUCT PaintStruct;
};

//****************************************************************************************************
//* BT_DC_CREATE (Type, [hWnd | hBitmap]) ---> Return array = {Type, hWnd, hBitmap, hDC, PaintStruct}
//****************************************************************************************************

// Type
#define BT_HDC_DESKTOP            1
#define BT_HDC_WINDOW             2
#define BT_HDC_ALLCLIENTAREA      3
#define BT_HDC_INVALIDCLIENTAREA  4
#define BT_HDC_BITMAP             5

/*
BT_DC_CREATE(type, HWND) --> NIL|array
*/
HB_FUNC( BT_DC_CREATE )
{
   HBITMAP hBitmap;

   BT_STRUCT BT;
   ZeroMemory(&BT, sizeof(BT_STRUCT));
   BT.Type = hmg_par_INT(1);

   switch( BT.Type ) {
      case BT_HDC_DESKTOP:
         // BT.hDC  = CreateDC("DISPLAY", nullptr, nullptr, nullptr);
         // BT.hDC  = GetDC(nullptr);
         BT.hWnd = GetDesktopWindow();
         BT.hDC  = GetDC(BT.hWnd);
         break;

      case BT_HDC_WINDOW:
         BT.hWnd = hmg_par_HWND(2);
         BT.hDC  = GetWindowDC(BT.hWnd);
         break;

      case BT_HDC_ALLCLIENTAREA:
         BT.hWnd = hmg_par_HWND(2);
         BT.hDC  = GetDC(BT.hWnd);
         break;

      case BT_HDC_INVALIDCLIENTAREA:
         BT.hWnd = hmg_par_HWND(2);
         BT.hDC  = BeginPaint(BT.hWnd, &BT.PaintStruct);
         break;

      case BT_HDC_BITMAP:
         hBitmap = hmg_par_HBITMAP(2);
         BT.hDC  = CreateCompatibleDC(nullptr);
         SelectObject(BT.hDC, hBitmap);
         break;

      default:
         hb_ret();     // Return NIL
         return;
   }

   hb_reta(50);                                                         // Return array = {Type, hWnd, hBitmap, hDC, PaintStruct ...}

   HB_STORVNI( BT.Type, -1, 1);                                 // Type
   hmg_storvhandle(BT.hWnd, -1, 2);                             // hWnd
   hmg_storvhandle(BT.hDC, -1, 3);                              // hDC
   // PAINTSTRUCT
   hmg_storvhandle(BT.PaintStruct.hdc, -1, 4);                  // HDC  hdc;
   HB_STORVNI( BT.PaintStruct.fErase, -1, 5);                   // BOOL fErase;
   HB_STORVNL( BT.PaintStruct.rcPaint.left, -1, 6);             // RECT rcPaint.left;
   HB_STORVNL( BT.PaintStruct.rcPaint.top, -1, 7);              // RECT rcPaint.top;
   HB_STORVNL( BT.PaintStruct.rcPaint.right, -1, 8);            // RECT rcPaint.right;
   HB_STORVNL( BT.PaintStruct.rcPaint.bottom, -1, 9);           // RECT rcPaint.bottom;
   HB_STORVNI( BT.PaintStruct.fRestore, -1, 10);                // BOOL fRestore;
   HB_STORVNI( BT.PaintStruct.fIncUpdate, -1, 11);              // BOOL fIncUpdate;
   for( INT i = 0; i < 32; i++ ) {
      HB_STORVNI(BT.PaintStruct.rgbReserved[i], -1, 12 + i);  // BYTE rgbReserved[32];
   }

//   GdiSetBatchLimit (100);
}

//****************************************************************************************************
//* BT_DC_DELETE ({Type, hWnd, hBitmap, hDC, PaintStruct})
//****************************************************************************************************

/*
BT_DC_DELETE() --> .T.|.F.
*/
HB_FUNC( BT_DC_DELETE )
{
//   GdiSetBatchLimit (0);
   BT_STRUCT BT;
   BT.Type = hb_parvni(1, 1);
   BT.hWnd = reinterpret_cast<HWND>(HB_PARVNL(1, 2));
   BT.hDC  = reinterpret_cast<HDC>(HB_PARVNL(1, 3));
   // PAINTSTRUCT
   BT.PaintStruct.hdc            = reinterpret_cast<HDC>(HB_PARVNL(1, 4));             // HDC  hdc;
   BT.PaintStruct.fErase         = ( BOOL ) hb_parvni(1, 5);            // BOOL fErase;
   BT.PaintStruct.rcPaint.left   = HB_PARVNL(1, 6);            // RECT rcPaint.left;
   BT.PaintStruct.rcPaint.top    = HB_PARVNL(1, 7);            // RECT rcPaint.top;
   BT.PaintStruct.rcPaint.right  = HB_PARVNL(1, 8);            // RECT rcPaint.right;
   BT.PaintStruct.rcPaint.bottom = HB_PARVNL(1, 9);            // RECT rcPaint.bottom;
   BT.PaintStruct.fRestore       = ( BOOL ) hb_parvni(1, 10);           // BOOL fRestore;
   BT.PaintStruct.fIncUpdate     = ( BOOL ) hb_parvni(1, 11);           // BOOL fIncUpdate;
   for( INT i = 0; i < 32; i++ ) {
      BT.PaintStruct.rgbReserved[i] = static_cast<BYTE>(hb_parvni(1, 12 + i));  // BYTE rgbReserved[32];
   }

   switch( BT.Type ) {
      case BT_HDC_DESKTOP:
         // DeleteDC(BT.hDC);
         ReleaseDC(BT.hWnd, BT.hDC);
         break;

      case BT_HDC_WINDOW:
         ReleaseDC(BT.hWnd, BT.hDC);
         break;

      case BT_HDC_ALLCLIENTAREA:
         ReleaseDC(BT.hWnd, BT.hDC);
         break;

      case BT_HDC_INVALIDCLIENTAREA:
         EndPaint(BT.hWnd, &BT.PaintStruct);
         break;

      case BT_HDC_BITMAP:
         DeleteDC(BT.hDC);
         break;

      default:
         hb_retl(false);
         return;
   }
   hb_retl(true);
}

// ::::::::::::::::::::::::::::::::
// :::     SCREEN Functions     :::
// ::::::::::::::::::::::::::::::::

//**************************************************************************
//* BT_SCR_GETDESKTOPHANDLE ()
//**************************************************************************

/*
BT_SCR_GETDESKTOPHANDLE() --> HWND
*/
HB_FUNC( BT_SCR_GETDESKTOPHANDLE )
{
   HB_RETNL(reinterpret_cast<LONG_PTR>(GetDesktopWindow()));
}

//**************************************************************************
//* BT_SCR_GETINFO (hWnd, Mode, info)
//**************************************************************************

// Mode
#define BT_SCR_DESKTOP      0
#define BT_SCR_WINDOW       1
#define BT_SCR_CLIENTAREA   2

// Info
#define BT_SCR_INFO_WIDTH   0
#define BT_SCR_INFO_HEIGHT  1

/*
BT_SCR_GETINFO() --> numeric
*/
HB_FUNC( BT_SCR_GETINFO )
{
   HDC hDC = nullptr;
   RECT rect;

   auto hWnd = hmg_par_HWND(1);
   INT Mode = hmg_par_INT(2);
   INT info = hmg_par_INT(3);

   switch( Mode ) {
      case BT_SCR_DESKTOP:
         break;
      case BT_SCR_WINDOW:
         break;
      case BT_SCR_CLIENTAREA:
         hDC = GetDC(hWnd);
         break;
   }

   switch( Mode ) {
      case BT_SCR_DESKTOP:
         rect.right  = GetSystemMetrics(SM_CXSCREEN);
         rect.bottom = GetSystemMetrics(SM_CYSCREEN);
         break;
      case BT_SCR_WINDOW:
         GetWindowRect(hWnd, &rect);
         rect.right  = rect.right - rect.left;
         rect.bottom = rect.bottom - rect.top;
         break;
      case BT_SCR_CLIENTAREA:
         GetClientRect(hWnd, &rect);
         ReleaseDC(hWnd, hDC);
         break;
      default:
         rect.right  = 0;
         rect.bottom = 0;
         break;
   }

   hb_retnl(info == BT_SCR_INFO_WIDTH ? rect.right : rect.bottom);
}

//*************************************************************************************************
// BT_SCR_INVALIDATERECT(hWnd , [{x_left, y_top, x_right, y_bottom}] , lEraseBackground)
//*************************************************************************************************

/*
BT_SCR_INVALIDATERECT(HWND, aRect, pl3) --> .T.|.F.
*/
HB_FUNC( BT_SCR_INVALIDATERECT )
{
   if( !HB_ISARRAY(2) ) {
      hb_retl(InvalidateRect(hmg_par_HWND(1), nullptr, hb_parl(3)));  // Invalidate all client area
      return;
   }

   auto pArrayRect = hb_param(2, Harbour::Item::ARRAY);

   if( hb_arrayLen(pArrayRect) == 4 ) {
      RECT rect;
      rect.left   = hb_arrayGetNL(pArrayRect, 1);
      rect.top    = hb_arrayGetNL(pArrayRect, 2);
      rect.right  = hb_arrayGetNL(pArrayRect, 3);
      rect.bottom = hb_arrayGetNL(pArrayRect, 4);
      hb_retl(InvalidateRect(hmg_par_HWND(1), &rect, hb_parl(3))); // Invalidate specific rectangle of client area
   } else {
      hb_retl(false);
   }
}

// ::::::::::::::::::::::::::::::::
// :::    DRAW hDC Functions    :::
// ::::::::::::::::::::::::::::::::

//***********************************************************************************************************************
//* BT_DRAWEDGE (hDC, nRow, nCol, nWidth, nHeight, nEdge, nGrfFlags)
//***********************************************************************************************************************

/*
BT_DRAWEDGE(HDC, nTop, nLeft, nRight, nBottom, nEdge, nGrfFlags) --> NIL
*/
HB_FUNC( BT_DRAWEDGE )
{
   HDC hDC = hmg_par_HDC(1);

   RECT Rect;
   Rect.top    = hb_parni(2);
   Rect.left   = hb_parni(3);
   Rect.right  = hb_parni(4);
   Rect.bottom = hb_parni(5);

   INT Edge = hb_parni(6);
   INT GrfFlags = hb_parni(7);

   DrawEdge(hDC, &Rect, Edge, GrfFlags);
}

//***********************************************************************************************************************
//* BT_DRAW_HDC_POLY(hDC, aPointX, aPointY, ColorLine, nWidthLine, ColorFill, nPOLY)
//***********************************************************************************************************************

// nPOLY
#define BT_DRAW_POLYLINE    0
#define BT_DRAW_POLYGON     1
#define BT_DRAW_POLYBEZIER  2

/*
BT_DRAW_HDC_POLY(HDC, countx, county, colorLine, widthLine, colorFill, nPOLY) --> .T.|.F.
*/
HB_FUNC( BT_DRAW_HDC_POLY )
{
   #ifndef __MINGW_H
   POINT aPoint[2048];
   #endif

   HDC hDC            = hmg_par_HDC(1);
   INT nCountX        = static_cast<INT>(hb_parinfa(2, 0));
   INT nCountY        = static_cast<INT>(hb_parinfa(3, 0));
   COLORREF ColorLine = hmg_par_COLORREF(4);
   INT nWidthLine     = hmg_par_INT(5);
   COLORREF ColorFill = hmg_par_COLORREF(6);
   INT nPOLY          = hmg_par_INT(7);

   INT nLen = HB_MIN(nCountX, nCountY);

   if( nLen > 0 ) {
      #ifdef __MINGW_H
      POINT aPoint[nLen];
      #endif
      for( INT i = 0; i < nLen; i++ ) {
         aPoint[i].x = hb_parvni(2, i + 1);
         aPoint[i].y = hb_parvni(3, i + 1);
      }

      HPEN hPen       = CreatePen(PS_SOLID, nWidthLine, ColorLine);
      HPEN OldPen     = static_cast<HPEN>(SelectObject(hDC, hPen));
      HBRUSH hBrush   = CreateSolidBrush(ColorFill);
      HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(hDC, hBrush));

      switch( nPOLY ) {
         case BT_DRAW_POLYLINE:
            Polyline(hDC, aPoint, nLen);
            break;
         case BT_DRAW_POLYGON:
            Polygon(hDC, aPoint, nLen);
            break;
         case BT_DRAW_POLYBEZIER:
            PolyBezier(hDC, aPoint, nLen);
            break;
      }

      SelectObject(hDC, OldBrush);
      DeleteObject(hBrush);
      SelectObject(hDC, OldPen);
      DeleteObject(hPen);

      hb_retl(true);
   } else {
      hb_retl(false);
   }
}

//******************************************************************************************************************************
//* BT_DRAW_HDC_ARCX (hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc, ColorLine, nWidthLine, ColorFill, nArcType )
//******************************************************************************************************************************

// nArcType
#define BT_DRAW_ARC    0
#define BT_DRAW_CHORD  1
#define BT_DRAW_PIE    2

/*
BT_DRAW_HDC_ARCX(HDC, x1, y1, x2, y3, XStartArc, YStartArc, xEndArc, YEndArc, colorLine, widthLine, colorFill, arcType) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_ARCX )
{
   HDC hDC = hmg_par_HDC(1);
   INT x1  = hmg_par_INT(2);
   INT y1  = hmg_par_INT(3);
   INT x2  = hmg_par_INT(4);
   INT y2  = hmg_par_INT(5);

   INT XStartArc = hmg_par_INT(6);
   INT YStartArc = hmg_par_INT(7);
   INT XEndArc   = hmg_par_INT(8);
   INT YEndArc   = hmg_par_INT(9);

   COLORREF ColorLine = hmg_par_COLORREF(10);
   INT nWidthLine     = hmg_par_INT(11);
   COLORREF ColorFill = hmg_par_COLORREF(12);

   INT nArcType = hmg_par_INT(13);

   HPEN hPen       = CreatePen(PS_SOLID, nWidthLine, ColorLine);
   HPEN OldPen     = static_cast<HPEN>(SelectObject(hDC, hPen));
   HBRUSH hBrush   = CreateSolidBrush(ColorFill);
   HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(hDC, hBrush));

   switch( nArcType ) {
      case BT_DRAW_ARC:
         Arc(hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc);
         break;
      case BT_DRAW_CHORD:
         Chord(hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc);
         break;
      case BT_DRAW_PIE:
         Pie(hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc);
         break;
   }

   SelectObject(hDC, OldBrush);
   DeleteObject(hBrush);
   SelectObject(hDC, OldPen);
   DeleteObject(hPen);
}

//**************************************************************************************************************************
//* BT_DRAW_HDC_FILLEDOBJECT (hDC, x1, y1, Width1, Height1, ColorFill, ColorLine, nWidthLine, Type, RoundWidth, RoundHeight)
//***************************************************************************************************************************

// Type
#define BT_FILLRECTANGLE  1
#define BT_FILLELLIPSE    2
#define BT_FILLROUNDRECT  3  // RoundWidth , RoundHeight
#define BT_FILLFLOOD      4

/*
BT_DRAW_HDC_FILLEDOBJECT(HDC, x, y, width, height, colorFill, colorLine, widthLine, type, roundWidth, roundHeight) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_FILLEDOBJECT )
{
   HDC hDC            = hmg_par_HDC(1);
   INT x1             = hmg_par_INT(2);
   INT y1             = hmg_par_INT(3);
   INT Width1         = hmg_par_INT(4);
   INT Height1        = hmg_par_INT(5);
   COLORREF ColorFill = hmg_par_COLORREF(6);
   COLORREF ColorLine = hmg_par_COLORREF(7);
   INT nWidthLine     = hmg_par_INT(8);
   INT Type           = hmg_par_INT(9);
   INT RoundWidth     = hmg_par_INT(10);
   INT RoundHeight    = hmg_par_INT(11);

   HPEN hPen       = CreatePen(PS_SOLID, nWidthLine, ColorLine);
   HPEN OldPen     = static_cast<HPEN>(SelectObject(hDC, hPen));
   HBRUSH hBrush   = CreateSolidBrush(ColorFill);
   HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(hDC, hBrush));

   switch( Type ) {
      case BT_FILLRECTANGLE:
         Rectangle(hDC, x1, y1, x1 + Width1, y1 + Height1);
         break;
      case BT_FILLELLIPSE:
         Ellipse(hDC, x1, y1, x1 + Width1, y1 + Height1);
         break;
      case BT_FILLROUNDRECT:
         RoundRect(hDC, x1, y1, x1 + Width1, y1 + Height1, RoundWidth, RoundHeight);
         break;
      case BT_FILLFLOOD:
         ExtFloodFill(hDC, x1, y1, GetPixel(hDC, x1, y1), FLOODFILLSURFACE);
   }

   SelectObject(hDC, OldBrush);
   DeleteObject(hBrush);
   SelectObject(hDC, OldPen);
   DeleteObject(hPen);
}

//*****************************************************************************************************************************
//* BT_DRAW_HDC_BITMAP (hDC1, x1, y1, Width1, Height1, hBitmap, x2, y2, Width2, Height2, Mode_Stretch, Action, Color_Transp)
//*****************************************************************************************************************************

// Action
#define BT_BITMAP_OPAQUE       0
#define BT_BITMAP_TRANSPARENT  1

/*
BT_DRAW_HDC_BITMAP(HDC, x1, y1, width1, height1, HBITMAP, x2, y2, width2, height2, modeStretch, action, colorTransp) --> .T.|.F.
*/
HB_FUNC( BT_DRAW_HDC_BITMAP )
{
   HDC hDC     = hmg_par_HDC(1);
   INT x1      = hmg_par_INT(2);
   INT y1      = hmg_par_INT(3);
   INT Width1  = hmg_par_INT(4);
   INT Height1 = hmg_par_INT(5);

   HBITMAP hBitmap = hmg_par_HBITMAP(6);
   INT x2          = hmg_par_INT(7);
   INT y2          = hmg_par_INT(8);
   INT Width2      = hmg_par_INT(9);
   INT Height2     = hmg_par_INT(10);

   INT Mode_Stretch = hmg_par_INT(11);
   INT Action       = hmg_par_INT(12);

   COLORREF color_transp = hmg_par_COLORREF(13);

   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (hDC, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(hDC, &Point);
   SetStretchBltMode(hDC, HALFTONE);
   SetBrushOrgEx(hDC, Point.x, Point.y, nullptr);

   switch( Action ) {
      case BT_BITMAP_OPAQUE:
         StretchBlt(hDC, x1, y1, Width1, Height1, memDC, x2, y2, Width2, Height2, SRCCOPY);
         break;
      case BT_BITMAP_TRANSPARENT:
         TransparentBlt(hDC, x1, y1, Width1, Height1, memDC, x2, y2, Width2, Height2, color_transp);
         break;
      default:
         hb_retl(false);
         return;
   }

   DeleteDC(memDC);
   hb_retl(true);
}

//**********************************************************************************************************************
//* BT_DRAW_HDC_BITMAPALPHABLEND (hDC, x1, y1, Width1, Height1, hBitmap, x2, y2, Width2, Height2, Alpha, Mode_Stretch)
//**********************************************************************************************************************

// Alpha = 0 to 255
#define BT_ALPHABLEND_TRANSPARENT  0
#define BT_ALPHABLEND_OPAQUE       255

/*
BT_DRAW_HDC_BITMAPALPHABLEND(HDC, x1, y1, width1, height1, HBITMAP, x2, y2, width2, height2, alpha, modeStretch) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_BITMAPALPHABLEND )
{
   HDC hDC     = hmg_par_HDC(1);
   INT x1      = hmg_par_INT(2);
   INT y1      = hmg_par_INT(3);
   INT Width1  = hmg_par_INT(4);
   INT Height1 = hmg_par_INT(5);

   HBITMAP hBitmap = hmg_par_HBITMAP(6);
   INT x2          = hmg_par_INT(7);
   INT y2          = hmg_par_INT(8);
   INT Width2      = hmg_par_INT(9);
   INT Height2     = hmg_par_INT(10);

   BYTE Alpha        = hmg_par_BYTE(11);
   INT Mode_Stretch  = hmg_par_INT(12);

   BLENDFUNCTION blend;
   blend.BlendOp             = AC_SRC_OVER;
   blend.BlendFlags          = 0;
   blend.AlphaFormat         = 0;
   blend.SourceConstantAlpha = Alpha;

   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (hDC, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(hDC, &Point);
   SetStretchBltMode(hDC, HALFTONE);
   SetBrushOrgEx(hDC, Point.x, Point.y, nullptr);

   AlphaBlend(hDC, x1, y1, Width1, Height1, memDC, x2, y2, Width2, Height2, blend);

   DeleteDC(memDC);
}

//****************************************************************************************************
// BT_DRAW_HDC_GRADIENTFILL (hDC, x1, y1, Width1, Height1, Color_RGB_O, Color_RGB_D, Mode)
//****************************************************************************************************

// Mode
#define BT_GRADIENTFILL_HORIZONTAL  0
#define BT_GRADIENTFILL_VERTICAL    1

/*
BT_DRAW_HDC_GRADIENTFILL(HDC, x1, y1, x2, y2, colorRGBO, colorRGBD, mode) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_GRADIENTFILL )
{
   HDC hDC = hmg_par_HDC(1);

   COLORREF Color_RGB_O = hmg_par_COLORREF(6);
   COLORREF Color_RGB_D = hmg_par_COLORREF(7);
   ULONG Mode = static_cast<ULONG>(hb_parnl(8));

   TRIVERTEX Vert[2];

   Vert[0].x     = hb_parnl(2);
   Vert[0].y     = hb_parnl(3);
   Vert[0].Red   = static_cast<COLOR16>(GetRValue(Color_RGB_O) << 8);
   Vert[0].Green = static_cast<COLOR16>(GetGValue(Color_RGB_O) << 8);
   Vert[0].Blue  = static_cast<COLOR16>(GetBValue(Color_RGB_O) << 8);
   Vert[0].Alpha = 0x0000;

   Vert[1].x     = hb_parnl(2) + hb_parnl(4);
   Vert[1].y     = hb_parnl(3) + hb_parnl(5);
   Vert[1].Red   = static_cast<COLOR16>(GetRValue(Color_RGB_D) << 8);
   Vert[1].Green = static_cast<COLOR16>(GetGValue(Color_RGB_D) << 8);
   Vert[1].Blue  = static_cast<COLOR16>(GetBValue(Color_RGB_D) << 8);
   Vert[1].Alpha = 0x0000;

   GRADIENT_RECT gRect;
   gRect.UpperLeft  = 0;
   gRect.LowerRight = 1;
   GradientFill(hDC, Vert, 2, &gRect, 1, Mode);
}

//*******************************************************************************************************
//* BT_DRAW_HDC_TEXTOUT (hDC, x, y, Text, FontName, FontSize, Text_Color, Back_color, Type, Align, Action)
//*******************************************************************************************************

// Type
#define BT_TEXT_OPAQUE       0
#define BT_TEXT_TRANSPARENT  1

#define BT_TEXT_BOLD         2
#define BT_TEXT_ITALIC       4
#define BT_TEXT_UNDERLINE    8
#define BT_TEXT_STRIKEOUT    16

// Align
#define BT_TEXT_LEFT         0
#define BT_TEXT_CENTER       6
#define BT_TEXT_RIGHT        2

#define BT_TEXT_TOP          0
#define BT_TEXT_BASELINE     24
#define BT_TEXT_BOTTOM       8

/*
BT_DRAW_HDC_TEXTOUT(HDC, x, y, text, fontname, fontsize, textColor, backColor, type, align, orientation) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_TEXTOUT )
{
   HDC hDC         = hmg_par_HDC(1);
   INT x           = hmg_par_INT(2);
   INT y           = hmg_par_INT(3);
#ifndef UNICODE
   TCHAR * lpText      = ( TCHAR * ) hb_parc(4);
   TCHAR * FontName    = ( TCHAR * ) hb_parc(5);
#else
   TCHAR * lpText      = ( TCHAR * ) hb_osStrU16Encode(hb_parc(4));
   TCHAR * FontName    = ( TCHAR * ) hb_osStrU16Encode(hb_parc(5));
#endif
   INT FontSize        = hmg_par_INT(6);
   COLORREF Text_Color = hmg_par_COLORREF(7);
   COLORREF Back_Color = hmg_par_COLORREF(8);
   INT Type            = hmg_par_INT(9);
   INT Align           = hmg_par_INT(10);
   INT Orientation     = hmg_par_INT(11);

   if( (Orientation < -360) || (Orientation > 360) ) {
      Orientation = 0;
   }

   Orientation = Orientation * 10;

   if( (Type & BT_TEXT_TRANSPARENT) == BT_TEXT_TRANSPARENT ) {
      SetBkMode(hDC, TRANSPARENT);
   } else {
      SetBkMode(hDC, OPAQUE);
      SetBkColor(hDC, Back_Color);
   }

   INT Bold      = FW_NORMAL;
   INT Italic    = 0;
   INT Underline = 0;
   INT StrikeOut = 0;

   if( (Type & BT_TEXT_BOLD) == BT_TEXT_BOLD ) {
      Bold = FW_BOLD;
   }

   if( (Type & BT_TEXT_ITALIC) == BT_TEXT_ITALIC ) {
      Italic = 1;
   }

   if( (Type & BT_TEXT_UNDERLINE) == BT_TEXT_UNDERLINE ) {
      Underline = 1;
   }

   if( (Type & BT_TEXT_STRIKEOUT) == BT_TEXT_STRIKEOUT ) {
      StrikeOut = 1;
   }

   SetGraphicsMode(hDC, GM_ADVANCED);

   FontSize = FontSize * GetDeviceCaps(hDC, LOGPIXELSY) / 72;

   // CreateFont (Height, Width, Escapement, Orientation, Weight, Italic, Underline, StrikeOut,
   //             CharSet, OutputPrecision, ClipPrecision, Quality, PitchAndFamily, Face);
   HFONT hFont = CreateFont(0 - FontSize, 0, Orientation, Orientation, Bold, Italic, Underline, StrikeOut,
                      DEFAULT_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, FontName);

   HFONT hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));

   SetTextAlign(hDC, Align);
   SetTextColor(hDC, Text_Color);

   TextOut(hDC, x, y, lpText, lstrlen(lpText));

/*
   When GetTextExtentPoint32() returns the text extent, it assumes that the text is HORIZONTAL,
   that is, that the ESCAPEMENT is always 0. This is true for both the horizontal and
   vertical measurements of the text. Even if you use a font that specifies a nonzero
   escapement, this function doesn't use the angle while it computes the text extent.
   The app must convert it explicitly.

   SIZE SizeText;
   GetTextExtentPoint32 (hDC, Text, lstrlen(Text), &SizeText);
   hb_reta(2);
   hb_storvnl (SizeText.cx, -1, 1);
   hb_storvnl (SizeText.cy, -1, 2);
 */
   SelectObject(hDC, hOldFont);
   DeleteObject(hFont);
}

//****************************************************************************************************************
//* BT_DRAW_HDC_DRAWTEXT (hDC, x, y, w, h, Text, FontName, FontSize, Text_Color, Back_color, Type, Align, Action)
//****************************************************************************************************************

/*
BT_DRAW_HDC_DRAWTEXT(HDC, x, y, w, h, text, fontName, fontSize, textColor, backColor, type, align, orientation) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_DRAWTEXT )
{
   HDC hDC         = hmg_par_HDC(1);
   INT x           = hmg_par_INT(2);
   INT y           = hmg_par_INT(3);
   INT w           = hmg_par_INT(4);
   INT h           = hmg_par_INT(5);
#ifndef UNICODE
   TCHAR * lpText      = ( TCHAR * ) hb_parc(6);
   TCHAR * FontName    = ( TCHAR * ) hb_parc(7);
#else
   TCHAR * lpText      = ( TCHAR * ) hb_osStrU16Encode(hb_parc(6));
   TCHAR * FontName    = ( TCHAR * ) hb_osStrU16Encode(hb_parc(7));
#endif
   INT FontSize        = hmg_par_INT(8);
   COLORREF Text_Color = hmg_par_COLORREF(9);
   COLORREF Back_Color = hmg_par_COLORREF(10);
   INT Type            = hmg_par_INT(11);
   INT Align           = hmg_par_INT(12);
   double Orientation  = static_cast<double>(hb_parnd(13));

   if( (Orientation < static_cast<double>(-360.0)) || (Orientation > static_cast<double>(360.0)) ) {
      Orientation = static_cast<double>(0.0);
   }

   Orientation = Orientation * static_cast<double>(10.0);   // Angle in tenths of degrees

   if( (Type & BT_TEXT_TRANSPARENT) == BT_TEXT_TRANSPARENT ) {
      SetBkMode(hDC, TRANSPARENT);
   } else {
      SetBkMode(hDC, OPAQUE);
      SetBkColor(hDC, Back_Color);
   }

   INT Bold      = FW_NORMAL;
   INT Italic    = 0;
   INT Underline = 0;
   INT StrikeOut = 0;

   if( (Type & BT_TEXT_BOLD) == BT_TEXT_BOLD ) {
      Bold = FW_BOLD;
   }

   if( (Type & BT_TEXT_ITALIC) == BT_TEXT_ITALIC ) {
      Italic = 1;
   }

   if( (Type & BT_TEXT_UNDERLINE) == BT_TEXT_UNDERLINE ) {
      Underline = 1;
   }

   if( (Type & BT_TEXT_STRIKEOUT) == BT_TEXT_STRIKEOUT ) {
      StrikeOut = 1;
   }

   SetGraphicsMode(hDC, GM_ADVANCED);

   FontSize = FontSize * GetDeviceCaps(hDC, LOGPIXELSY) / 72;

   // CreateFont (Height, Width, Escapement, Orientation, Weight, Italic, Underline, StrikeOut,
   //             CharSet, OutputPrecision, ClipPrecision, Quality, PitchAndFamily, Face);
   HFONT hFont = CreateFont(0 - FontSize, 0, static_cast<int>(Orientation), static_cast<int>(Orientation), Bold, Italic, Underline, StrikeOut,
                      DEFAULT_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, FontName);

   HFONT hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));

   SetTextColor(hDC, Text_Color);

   RECT rect;
   SetRect(&rect, x, y, x + w, y + h);

   DrawText(hDC, lpText, -1, &rect, /*DT_EXTERNALLEADING |*/ DT_NOPREFIX | Align);

   SelectObject(hDC, hOldFont);
   DeleteObject(hFont);
}

//*******************************************************************************************************
//* BT_DRAW_HDC_TEXTSIZE (hDC, Text, FontName, FontSize, Type)
//*******************************************************************************************************

/*
   // Type
   #define BT_TEXT_BOLD        2
   #define BT_TEXT_ITALIC      4
   #define BT_TEXT_UNDERLINE   8
   #define BT_TEXT_STRIKEOUT   16
 */

/*
BT_DRAW_HDC_TEXTSIZE(HDC, text, fontName, fontSize, type) --> array
*/
HB_FUNC( BT_DRAW_HDC_TEXTSIZE )
{
   HDC hDC          = hmg_par_HDC(1);
#ifndef UNICODE
   TCHAR * lpText   = ( TCHAR * ) hb_parc(2);
   TCHAR * FontName = ( TCHAR * ) hb_parc(3);
#else
   TCHAR * lpText   = ( TCHAR * ) hb_osStrU16Encode(hb_parc(2));
   TCHAR * FontName = ( TCHAR * ) hb_osStrU16Encode(hb_parc(3));
#endif
   INT FontSize     = hmg_par_INT(4);
   INT Type         = hmg_par_INT(5);

   INT Bold      = FW_NORMAL;
   INT Italic    = 0;
   INT Underline = 0;
   INT StrikeOut = 0;

   if( (Type & BT_TEXT_BOLD) == BT_TEXT_BOLD ) {
      Bold = FW_BOLD;
   }

   if( (Type & BT_TEXT_ITALIC) == BT_TEXT_ITALIC ) {
      Italic = 1;
   }

   if( (Type & BT_TEXT_UNDERLINE) == BT_TEXT_UNDERLINE ) {
      Underline = 1;
   }

   if( (Type & BT_TEXT_STRIKEOUT) == BT_TEXT_STRIKEOUT ) {
      StrikeOut = 1;
   }

   SetGraphicsMode(hDC, GM_ADVANCED);

   FontSize = FontSize * GetDeviceCaps(hDC, LOGPIXELSY) / 72;

   // CreateFont (Height, Width, Escapement, Orientation, Weight, Italic, Underline, StrikeOut,
   //             CharSet, OutputPrecision, ClipPrecision, Quality, PitchAndFamily, Face);
   HFONT hFont = CreateFont(0 - FontSize, 0, 0, 0, Bold, Italic, Underline, StrikeOut,
                      DEFAULT_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, FontName);

   HFONT hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));

   SIZE SizeText;
   GetTextExtentPoint32(hDC, lpText, lstrlen(lpText), &SizeText);
   hb_reta(6);
   HB_STORVNL(SizeText.cx, -1, 1);
   HB_STORVNL(SizeText.cy, -1, 2);

   UINT iFirstChar = static_cast<UINT>(lpText[0]);
   UINT iLastChar  = static_cast<UINT>(lpText[0]);
   ABCFLOAT ABCfloat;
   GetCharABCWidthsFloat(hDC, iFirstChar, iLastChar, &ABCfloat);
   hb_storvnd(static_cast<double>(static_cast<FLOAT>(ABCfloat.abcfA + ABCfloat.abcfB + ABCfloat.abcfC)), -1, 3);
   hb_storvnd(static_cast<double>(static_cast<FLOAT>(ABCfloat.abcfA)), -1, 4);
   hb_storvnd(static_cast<double>(static_cast<FLOAT>(ABCfloat.abcfB)), -1, 5);
   hb_storvnd(static_cast<double>(static_cast<FLOAT>(ABCfloat.abcfC)), -1, 6);

   SelectObject(hDC, hOldFont);
   DeleteObject(hFont);
}

//*****************************************************************************************************************************
//* BT_DRAW_HDC_PIXEL (hDC, x, y, Action, Color)
//*****************************************************************************************************************************

// Action
#define BT_HDC_GETPIXEL  0
#define BT_HDC_SETPIXEL  1

/*
BT_DRAW_HDC_PIXEL(HDC, x, y, action, color) --> array
*/
HB_FUNC( BT_DRAW_HDC_PIXEL )
{
   HDC hDC        = hmg_par_HDC(1);
   INT x          = hmg_par_INT(2);
   INT y          = hmg_par_INT(3);
   INT Action     = hmg_par_INT(4);
   COLORREF Color = hmg_par_COLORREF(5);

   switch( Action ) {
      case BT_HDC_GETPIXEL:
         Color = GetPixel(hDC, x, y);
         break;
      case BT_HDC_SETPIXEL:
         Color = SetPixel(hDC, x, y, Color);
   }

   hb_reta(3);
   HB_STORVNI(GetRValue(Color), -1, 1);
   HB_STORVNI(GetGValue(Color), -1, 2);
   HB_STORVNI(GetBValue(Color), -1, 3);
}

//*****************************************************************************************************************************
//* BT_DRAW_HDC_TO_HDC(hDC1, x1, y1, Width1, Height1, hDC2, x2, y2, Width2, Height2, Mode_Stretch, Action, Color_Transp)
//*****************************************************************************************************************************

// Action
#define BT_HDC_OPAQUE       0
#define BT_HDC_TRANSPARENT  1

/*
BT_DRAW_HDC_TO_HDC(HDC1, x1, y1, width1, height1, HDC2, x2, y2, width2, height2, modeStretch, action, colorTransp) --> .T.|.F.
*/
HB_FUNC( BT_DRAW_HDC_TO_HDC )
{
   HDC hDC1    = hmg_par_HDC(1);
   INT x1      = hmg_par_INT(2);
   INT y1      = hmg_par_INT(3);
   INT Width1  = hmg_par_INT(4);
   INT Height1 = hmg_par_INT(5);

   HDC hDC2    = hmg_par_HDC(6);
   INT x2      = hmg_par_INT(7);
   INT y2      = hmg_par_INT(8);
   INT Width2  = hmg_par_INT(9);
   INT Height2 = hmg_par_INT(10);

   INT Mode_Stretch      = hmg_par_INT(11);
   INT Action            = hmg_par_INT(12);
   COLORREF color_transp = hmg_par_COLORREF(13);

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (hDC1, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(hDC1, &Point);
   SetStretchBltMode(hDC1, HALFTONE);
   SetBrushOrgEx(hDC1, Point.x, Point.y, nullptr);

   switch( Action ) {
      case BT_HDC_OPAQUE:
         StretchBlt(hDC1, x1, y1, Width1, Height1, hDC2, x2, y2, Width2, Height2, SRCCOPY);
         break;
      case BT_HDC_TRANSPARENT:
         TransparentBlt(hDC1, x1, y1, Width1, Height1, hDC2, x2, y2, Width2, Height2, color_transp);
         break;
      default:
         hb_retl(false);
         return;
   }

   hb_retl(true);
}

//**********************************************************************************************************************
//* BT_DRAW_HDC_TO_HDC_ALPHABLEND (hDC1, x1, y1, Width1, Height1, hDC2, x2, y2, Width2, Height2, Alpha, Mode_Stretch)
//**********************************************************************************************************************

// Alpha = 0 to 255
#define BT_ALPHABLEND_TRANSPARENT  0
#define BT_ALPHABLEND_OPAQUE       255

/*
BT_DRAW_HDC_TO_HDC_ALPHABLEND(HDC1, x1, y1, width1, height1, HDC2, x2, y2, width2, height2, alpha, modeStretch) --> NIL
*/
HB_FUNC( BT_DRAW_HDC_TO_HDC_ALPHABLEND )
{
   HDC hDC1    = hmg_par_HDC(1);
   INT x1      = hmg_par_INT(2);
   INT y1      = hmg_par_INT(3);
   INT Width1  = hmg_par_INT(4);
   INT Height1 = hmg_par_INT(5);

   HDC hDC2    = hmg_par_HDC(6);
   INT x2      = hmg_par_INT(7);
   INT y2      = hmg_par_INT(8);
   INT Width2  = hmg_par_INT(9);
   INT Height2 = hmg_par_INT(10);

   BYTE Alpha   = hmg_par_BYTE(11);
   INT Mode_Stretch = hmg_par_INT(12);

   BLENDFUNCTION blend;
   blend.BlendOp             = AC_SRC_OVER;
   blend.BlendFlags          = 0;
   blend.AlphaFormat         = 0;
   blend.SourceConstantAlpha = Alpha;

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (hDC1, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(hDC1, &Point);
   SetStretchBltMode(hDC1, HALFTONE);
   SetBrushOrgEx(hDC1, Point.x, Point.y, nullptr);

   AlphaBlend(hDC1, x1, y1, Width1, Height1, hDC2, x2, y2, Width2, Height2, blend);
}

// ::::::::::::::::::::::::::::::::
// :::     BITMAP Functions     :::
// ::::::::::::::::::::::::::::::::

//**************************************************************************************************
//* BT_BMP_CREATE (Width, Height, Color_Fill_Bk) ---> Return hBITMAP
//**************************************************************************************************

/*
BT_BMP_CREATE(width, height, backColor) --> HBITMAP
*/
HB_FUNC( BT_BMP_CREATE )
{
   INT Width              = hmg_par_INT(1);
   INT Height             = hmg_par_INT(2);
   COLORREF Color_Fill_Bk = hmg_par_COLORREF(3);

   HBITMAP hBitmap_New = bt_bmp_create_24bpp(Width, Height);

   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap_New);

   BITMAP bm;
   GetObject(hBitmap_New, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));
   RECT Rect;
   SetRect(&Rect, 0, 0, bm.bmWidth, bm.bmHeight);

   HBRUSH hBrush = CreateSolidBrush(Color_Fill_Bk);
   HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(memDC, hBrush));
   FillRect(memDC, &Rect, hBrush);

   SelectObject(memDC, OldBrush);
   DeleteDC(memDC);
   DeleteObject(hBrush);

   hmg_ret_HBITMAP(hBitmap_New);
}

//*************************************************************************************************
//* BT_BMP_RELEASE (hBitmap) ---> Return Success (TRUE or FALSE)
//*************************************************************************************************

/*
BT_BMP_RELEASE(HBITMAP) --> .T.|.F.
*/
HB_FUNC( BT_BMP_RELEASE )
{
   hb_retl(DeleteObject(hmg_par_HBITMAP(1)));
}

//*************************************************************************************************
//* BT_BMP_LOADFILE (cFileBMP) ---> Return hBITMAP
//*************************************************************************************************

/*
BT_BMP_LOADFILE(cFileName) --> HBITMAP
*/
HB_FUNC( BT_BMP_LOADFILE )
{
#ifndef UNICODE
   TCHAR * FileName = ( TCHAR * ) hb_parc(1);
#else
   TCHAR * FileName = ( TCHAR * ) hb_osStrU16Encode(hb_parc(1));
#endif

   // First find BMP image in resourses (.EXE file)
   HBITMAP hBitmap = static_cast<HBITMAP>(LoadImage(GetModuleHandle(nullptr), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));

   // If fail: find BMP in disk
   if( hBitmap == nullptr ) {
      hBitmap = static_cast<HBITMAP>(LoadImage(nullptr, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION));
   }

   // If fail: find JPG Image in resourses
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadOLEPicture(FileName, TEXT("JPG"));
   }

   // If fail: find GIF Image in resourses
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadOLEPicture(FileName, TEXT("GIF"));
   }

   // If fail: find PNG Image in resourses
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadGDIPlusPicture(FileName, TEXT("PNG"));
   }

   // If fail: find TIF Image in resourses
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadGDIPlusPicture(FileName, TEXT("TIF"));
   }

   // If fail: find JPG and GIF Image in disk
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadOLEPicture(FileName, nullptr);
   }

   // If fail: find PNG and TIF Image in disk
   if( hBitmap == nullptr ) {
      hBitmap = bt_LoadGDIPlusPicture(FileName, nullptr);
   }

   // If fail load: return zero
   if( hBitmap == nullptr ) {
      hmg_ret_HBITMAP(nullptr);
      return;
   }

//   if (bt_bmp_is_24bpp (hBitmap) == FALSE)
//       hBitmapFile = bt_bmp_convert_to_24bpp (hBitmap, BMP_DELETE_ORIGINAL_HBITMAP);   // Convert Bitmap in 24bpp

   hmg_ret_HBITMAP(hBitmap);
}

//*********************************************************************************************************************************
//* BT_BitmapLoadEMF(cFileName, [aRGBBackgroundColor], [nNewWidth], [nNewHeight], [ModeStretch])  ---> Return hBITMAP
//*********************************************************************************************************************************

/*
BT_BITMAPLOADEMF(cFileName, backgroundColor, width, height, modeStretch) --> HBITMAP
*/
HB_FUNC( BT_BITMAPLOADEMF )
{
#ifndef UNICODE
   TCHAR * FileName = ( TCHAR * ) hb_parc(1);
#else
   TCHAR * FileName = ( TCHAR * ) hb_osStrU16Encode(hb_parc(1));
#endif
   COLORREF BackgroundColor = static_cast<COLORREF>(RGB(HB_PARVNL(2, 1), HB_PARVNL(2, 2), HB_PARVNL(2, 3)));
   INT      ModeStretch     = HB_ISNUM(5) ? hb_parnl(5) : BT_SCALE;

   HENHMETAFILE  hEMF = nullptr;
   HGLOBAL       hGlobalResource;
   LPVOID        lpGlobalResource;
   DWORD         nFileSize;

   // Load MetaFile from Resource
   HRSRC hResourceData = FindResource(nullptr, FileName, TEXT("EMF"));
   if( hResourceData ) {
      hGlobalResource = LoadResource(nullptr, hResourceData);
      if( hGlobalResource ) {
         lpGlobalResource = LockResource(hGlobalResource);
         nFileSize        = SizeofResource(nullptr, hResourceData);
         hEMF = SetEnhMetaFileBits(nFileSize, reinterpret_cast<const BYTE*>(lpGlobalResource));
      }
   }

   // If fail load MetaFile from Disk
   if( hEMF == nullptr ) {
      hEMF = GetEnhMetaFile(FileName);
   }

   // If fail load from Resource and Disk return nullptr
   if( hEMF == nullptr ) {
      hmg_ret_HBITMAP(nullptr);
      return;
   }

   // Get the header of MetaFile
   ENHMETAHEADER emh;
   ZeroMemory(&emh, sizeof(ENHMETAHEADER));
   emh.nSize = sizeof(ENHMETAHEADER);
   if( GetEnhMetaFileHeader(hEMF, sizeof(ENHMETAHEADER), &emh) == 0 ) {
      DeleteEnhMetaFile(hEMF);
      hmg_ret_HBITMAP(nullptr);
      return;
   }

   INT nWidth  = HB_ISNUM(3) ? hb_parnl(3) : emh.rclBounds.right;  // The dimensions: in device units
   INT nHeight = HB_ISNUM(4) ? hb_parnl(4) : emh.rclBounds.bottom; // The dimensions: in device units

   if( ModeStretch == BT_SCALE ) {
      bt_bmp_adjust_rect(&nWidth, &nHeight, ( int * ) &emh.rclBounds.right, ( int * ) &emh.rclBounds.bottom, BT_SCALE);
   }

   // Create Bitmap
   HDC memDC   = CreateCompatibleDC(nullptr);
   HBITMAP hBitmap = bt_bmp_create_24bpp(nWidth, nHeight);
   SelectObject(memDC, hBitmap);

   // Paint the background of the Bitmap
   HBRUSH hBrush   = CreateSolidBrush(BackgroundColor);
   HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(memDC, hBrush));
   RECT Rect;
   Rect.left   = 0;
   Rect.top    = 0;
   Rect.right  = nWidth;
   Rect.bottom = nHeight;
   FillRect(memDC, &Rect, hBrush);

   POINT Point;
   GetBrushOrgEx(memDC, &Point);
   SetStretchBltMode(memDC, HALFTONE);
   SetBrushOrgEx(memDC, Point.x, Point.y, nullptr);

   // Play MetaFile into Bitmap
   PlayEnhMetaFile(memDC, hEMF, &Rect);

   // Release handles
   SelectObject(memDC, OldBrush);
   DeleteEnhMetaFile(hEMF);
   DeleteDC(memDC);
   DeleteObject(hBrush);

   hmg_ret_HBITMAP(hBitmap);
}

//*************************************************************************************************
//* BT_BMP_SAVEFILE (hBitmap, cFileName, nTypePicture) ---> Return Success (TRUE or FALSE)
//*************************************************************************************************

// nTypePicture
#define BT_FILEFORMAT_BMP  0
#define BT_FILEFORMAT_JPG  1
#define BT_FILEFORMAT_GIF  2
#define BT_FILEFORMAT_TIF  3
#define BT_FILEFORMAT_PNG  4

static BOOL bt_bmp_SaveFile(HBITMAP hBitmap, const TCHAR * FileName, INT nTypePicture)
{
   if( nTypePicture != 0 ) {
      return bt_SaveGDIPlusPicture(hBitmap, FileName, nTypePicture);
   }

   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);
   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   bm.bmBitsPixel  = 24;
   bm.bmWidthBytes = (bm.bmWidth * bm.bmBitsPixel + 31) / 32 * 4;
   DWORD nBytes_Bits = static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight));

   BITMAPFILEHEADER BIFH;
   BIFH.bfType      = ('M' << 8) + 'B';
   BIFH.bfSize      = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + nBytes_Bits;
   BIFH.bfReserved1 = 0;
   BIFH.bfReserved2 = 0;
   BIFH.bfOffBits   = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

   BITMAPINFO Bitmap_Info;
   Bitmap_Info.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   Bitmap_Info.bmiHeader.biWidth         = bm.bmWidth;
   Bitmap_Info.bmiHeader.biHeight        = bm.bmHeight;
   Bitmap_Info.bmiHeader.biPlanes        = 1;
   Bitmap_Info.bmiHeader.biBitCount      = 24;
   Bitmap_Info.bmiHeader.biCompression   = BI_RGB;
   Bitmap_Info.bmiHeader.biSizeImage     = 0;       //nBytes_Bits;
   Bitmap_Info.bmiHeader.biXPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biYPelsPerMeter = 0;
   Bitmap_Info.bmiHeader.biClrUsed       = 0;
   Bitmap_Info.bmiHeader.biClrImportant  = 0;

   HGLOBAL hBits = GlobalAlloc(GHND, static_cast<DWORD>(nBytes_Bits));
   if( hBits == nullptr ) {
      return FALSE;
   }

   LPBYTE lp_hBits = static_cast<LPBYTE>(GlobalLock(hBits));

   GetDIBits(memDC, hBitmap, 0, Bitmap_Info.bmiHeader.biHeight, static_cast<LPVOID>(lp_hBits), &Bitmap_Info, DIB_RGB_COLORS);

   HANDLE hFile = CreateFile(FileName, GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, nullptr);

   BOOL ret = FALSE;

   if( hFile != INVALID_HANDLE_VALUE ) {
      DWORD nBytes_Written;
      WriteFile(hFile, reinterpret_cast<LPBYTE>(&BIFH), sizeof(BITMAPFILEHEADER), &nBytes_Written, nullptr);
      WriteFile(hFile, reinterpret_cast<LPBYTE>(&Bitmap_Info.bmiHeader), sizeof(BITMAPINFOHEADER), &nBytes_Written, nullptr);
      WriteFile(hFile, static_cast<LPBYTE>(lp_hBits), nBytes_Bits, &nBytes_Written, nullptr);
      CloseHandle(hFile);
      ret = TRUE;
   }

   GlobalUnlock(hBits);
   GlobalFree(hBits);

   DeleteDC(memDC);
   return ret;
}

/*
BT_BMP_SAVEFILE(HBITMAP, FileName, nTypePicture) --> .T.|.F.
*/
HB_FUNC( BT_BMP_SAVEFILE )
{
#ifndef UNICODE
   TCHAR *  FileName    = ( TCHAR * ) hb_parc(2);
#else
   TCHAR *  FileName    = ( TCHAR * ) hb_osStrU16Encode(hb_parc(2));
#endif

   hb_retl(bt_bmp_SaveFile(hmg_par_HBITMAP(1), FileName, hb_parnl(3)));
}

//**************************************************************************************************
//* BT_BMP_GETINFO (hBitmap, Info, x, y) ---> Return BT_BITMAP_INFO_xxx
//**************************************************************************************************

// Info
#define BT_BITMAP_INFO_WIDTH          0
#define BT_BITMAP_INFO_HEIGHT         1
#define BT_BITMAP_INFO_BITSPIXEL      2
#define BT_BITMAP_INFO_GETCOLORPIXEL  3

/*
BT_BMP_GETINFO(HBITMAP, info) --> numeric
*/
HB_FUNC( BT_BMP_GETINFO )
{
   HBITMAP hBitmap = hmg_par_HBITMAP(1);
   INT Info = hb_parnl(2);

   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   /*
      BITMAP:
       LONG bmType;
       LONG bmWidth;
       LONG bmHeight;
       LONG bmWidthBytes;
       WORD bmPlanes;
       WORD bmBitsPixel;
       LPVOID bmBits;
    */

   switch( Info ) {
      case BT_BITMAP_INFO_WIDTH:
         hb_retnl(bm.bmWidth);
         break;
      case BT_BITMAP_INFO_HEIGHT:
         hb_retnl(bm.bmHeight);
         break;
      case BT_BITMAP_INFO_BITSPIXEL:
         hb_retnl(bm.bmBitsPixel);
         break;
      case BT_BITMAP_INFO_GETCOLORPIXEL:
      {
         INT x = hmg_par_INT(3);
         INT y = hmg_par_INT(4);
         HDC memDC = CreateCompatibleDC(nullptr);
         SelectObject(memDC, hBitmap);
         COLORREF color = GetPixel(memDC, x, y);
         DeleteDC(memDC);
         hb_retnl(color);
         break;
      }
      default:
         hb_retnl(0);
   }
}

//*************************************************************************************************
//* BT_BMP_CLONE (hBitmap, x1, y1, Width1, Height1) ---> Return new_hBITMAP
//*************************************************************************************************

/*
BT_BMP_CLONE(HBITMAP, x, y, width, height) --> HBITMAP
*/
HB_FUNC( BT_BMP_CLONE )
{
   HBITMAP hBitmap = hmg_par_HBITMAP(1);
   INT x1          = hmg_par_INT(2);
   INT y1          = hmg_par_INT(3);
   INT Width1      = hmg_par_INT(4);
   INT Height1     = hmg_par_INT(5);

   HDC memDC1 = CreateCompatibleDC(nullptr);
   SelectObject(memDC1, hBitmap);

   HDC memDC2 = CreateCompatibleDC(nullptr);
   HBITMAP hBitmap_New = bt_bmp_create_24bpp(Width1, Height1);
   SelectObject(memDC2, hBitmap_New);

   BitBlt(memDC2, 0, 0, Width1, Height1, memDC1, x1, y1, SRCCOPY);
   DeleteDC(memDC1);
   DeleteDC(memDC2);

   hmg_ret_HBITMAP(hBitmap_New);
}

//************************************************************************************************************
//* BT_BMP_COPYANDRESIZE (hBitmap, New_Width, New_Height, Mode_Stretch, nAlgorithm) ---> Return new_hBITMAP
//************************************************************************************************************

struct bt_BMPIMAGE
{
   HGLOBAL hGlobal;
   HBITMAP hBitmap;
   LONG    Width;
   LONG    Height;
   LONG    WidthBytes;
   INT     nChannels;
   LPBYTE  lp_Bits;
};

// nAction
#define BT_BMP_GETBITS  0
#define BT_BMP_SETBITS  1

static BOOL bt_BMP_BITS(bt_BMPIMAGE * Image, INT nAction)
{
   if( (nAction != BT_BMP_GETBITS) && (nAction != BT_BMP_SETBITS) ) {
      return FALSE;
   }

   BITMAP bm;
   GetObject(Image->hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   BITMAPINFO BI;
   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = bm.bmWidth;
   BI.bmiHeader.biHeight        = -bm.bmHeight;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = 24;
   BI.bmiHeader.biCompression   = BI_RGB;
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   bm.bmWidthBytes = (bm.bmWidth * BI.bmiHeader.biBitCount + 31) / 32 * 4;

   if( nAction == BT_BMP_GETBITS ) {
      Image->WidthBytes = bm.bmWidthBytes;
      Image->Height     = bm.bmHeight;
      Image->Width      = bm.bmWidth;
      Image->nChannels  = 3;    //3 bytes per pixel
      Image->hGlobal    = GlobalAlloc(GHND, static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight)));
   }

   if( Image->hGlobal == nullptr ) {
      return FALSE;
   }

   LPBYTE lp_Bits = static_cast<LPBYTE>(GlobalLock(Image->hGlobal));
   HDC memDC = CreateCompatibleDC(nullptr);

   if( nAction == BT_BMP_GETBITS ) {
      GetDIBits(memDC, Image->hBitmap, 0, bm.bmHeight, static_cast<LPVOID>(lp_Bits), &BI, DIB_RGB_COLORS);
   } else {
      SetDIBits(memDC, Image->hBitmap, 0, bm.bmHeight, static_cast<LPVOID>(lp_Bits), &BI, DIB_RGB_COLORS);
   }

   DeleteDC(memDC);
   GlobalUnlock(Image->hGlobal);
   return TRUE;
}

static int bt_BMP_GETBYTE(bt_BMPIMAGE Image, int x, int y, int channel)
{
   if( x >= 0 && x < Image.Width && y >= 0 && y < Image.Height ) {
      return static_cast<int>(Image.lp_Bits[(y * Image.WidthBytes) + (x * Image.nChannels + channel)]);
   } else {
      return 0;
   }
}

static int bt_BMP_SETBYTE(bt_BMPIMAGE Image, int x, int y, int channel, BYTE value)
{
   if( x >= 0 && x < Image.Width && y >= 0 && y < Image.Height ) {
      return static_cast<int>(Image.lp_Bits[(y * Image.WidthBytes) + (x * Image.nChannels + channel)] = value);
   } else {
      return -1;
   }
}

static HBITMAP bt_BiLinearInterpolation(HBITMAP hBitmap, int newWidth, int newHeight)
{
   bt_BMPIMAGE Image1;
   Image1.hBitmap = hBitmap;
   if( bt_BMP_BITS(&Image1, BT_BMP_GETBITS) == FALSE ) {
      return nullptr;
   }

   bt_BMPIMAGE Image2;
   Image2.hBitmap = bt_bmp_create_24bpp(newWidth, newHeight);
   if( bt_BMP_BITS(&Image2, BT_BMP_GETBITS) == FALSE ) {
      GlobalFree(Image1.hGlobal);
      if( Image2.hBitmap != nullptr ) {
         DeleteObject(Image2.hBitmap);
      }
      return nullptr;
   }

   Image1.lp_Bits = static_cast<LPBYTE>(GlobalLock(Image1.hGlobal));
   Image2.lp_Bits = static_cast<LPBYTE>(GlobalLock(Image2.hGlobal));

   double y_ratio = static_cast<double>(Image1.Height) / static_cast<double>(Image2.Height);
   double x_ratio = static_cast<double>(Image1.Width) / static_cast<double>(Image2.Width);

   int x;
   int y;
   double x_diff;
   double y_diff;
   double a;
   double b;
   double c;
   double d;
   double Color;

   for( int Row = 0; Row < Image2.Height; Row++ ) {
      for( int Col = 0; Col < Image2.Width; Col++ ) {
         x = static_cast<int>(x_ratio * Col);
         y = static_cast<int>(y_ratio * Row);

         x_diff = static_cast<double>((x_ratio * Col) - x);
         y_diff = static_cast<double>((y_ratio * Row) - y);

         for( int Channel = 0; Channel < 3; Channel++ ) { // color channel C = R,G,B
            a = static_cast<double>(bt_BMP_GETBYTE(Image1, (x + 0), (y + 0), Channel));
            b = static_cast<double>(bt_BMP_GETBYTE(Image1, (x + 1), (y + 0), Channel));
            c = static_cast<double>(bt_BMP_GETBYTE(Image1, (x + 0), (y + 1), Channel));
            d = static_cast<double>(bt_BMP_GETBYTE(Image1, (x + 1), (y + 1), Channel));

            // Color = A(1-w)(1-h) + B(w)(1-h) + C(h)(1-w) + D(wh)
            Color = a * (1.00 - x_diff) * (1.00 - y_diff) + b * (x_diff) * (1.00 - y_diff) + c * (y_diff) * (1.00 - x_diff) + d * (x_diff * y_diff);

            bt_BMP_SETBYTE(Image2, Col, Row, Channel, static_cast<BYTE>(Color));
         }
      }
   }
   GlobalUnlock(Image1.hGlobal);
   GlobalUnlock(Image2.hGlobal);

   bt_BMP_BITS(&Image2, BT_BMP_SETBITS);

   GlobalFree(Image1.hGlobal);
   GlobalFree(Image2.hGlobal);

   return Image2.hBitmap;
}

// nAlgorithm
#define BT_RESIZE_COLORONCOLOR  0
#define BT_RESIZE_HALFTONE      1
#define BT_RESIZE_BILINEAR      2

/*
BT_BMP_COPYANDRESIZE(HBITMAP, newWidth, newHeight, modeStretch, algorithm) --> HBITMAP
*/
HB_FUNC( BT_BMP_COPYANDRESIZE )
{
   HBITMAP hBitmap1    = hmg_par_HBITMAP(1);
   INT New_Width       = hmg_par_INT(2);
   INT New_Height      = hmg_par_INT(3);
   INT Mode_Stretch    = hmg_par_INT(4);
   INT nAlgorithm      = hmg_par_INT(5);
   HBITMAP hBitmap_New = nullptr;

   HDC memDC1 = CreateCompatibleDC(nullptr);
   SelectObject(memDC1, hBitmap1);
   BITMAP bm;
   GetObject(hBitmap1, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   INT Width1  = bm.bmWidth;
   INT Height1 = bm.bmHeight;
   bt_bmp_adjust_rect(&New_Width, &New_Height, &Width1, &Height1, Mode_Stretch);

   if( nAlgorithm == BT_RESIZE_COLORONCOLOR || nAlgorithm == BT_RESIZE_HALFTONE ) {
      hBitmap_New = bt_bmp_create_24bpp(New_Width, New_Height);
      HDC memDC2 = CreateCompatibleDC(nullptr);
      SelectObject(memDC2, hBitmap_New);

      if( nAlgorithm == BT_RESIZE_COLORONCOLOR ) {
         SetStretchBltMode(memDC2, COLORONCOLOR);
      } else {
         POINT Point;
         GetBrushOrgEx(memDC2, &Point);
         SetStretchBltMode(memDC2, HALFTONE);
         SetBrushOrgEx(memDC2, Point.x, Point.y, nullptr);
      }

      StretchBlt(memDC2, 0, 0, New_Width, New_Height, memDC1, 0, 0, bm.bmWidth, bm.bmHeight, SRCCOPY);
      DeleteDC(memDC2);
   }

   DeleteDC(memDC1);

   if( nAlgorithm == BT_RESIZE_BILINEAR ) {
      hBitmap_New = bt_BiLinearInterpolation(hBitmap1, New_Width, New_Height);
   }

   hmg_ret_HBITMAP(hBitmap_New);
}

//*****************************************************************************************************************************
//* BT_BMP_PASTE (hBitmap_D, x1, y1, Width1, Height1, hBitmap_O, x2, y2, Width2, Height2, Mode_Stretch, Action, Color_Transp)
//*****************************************************************************************************************************

// Action
#define BT_BITMAP_OPAQUE       0
#define BT_BITMAP_TRANSPARENT  1

/*
BT_BMP_PASTE(HBITMAP1, x1, y1, width1, height1, HBITMAP2, x2, y2, width2, height2, stretch, action) --> .T.|.F.
*/
HB_FUNC( BT_BMP_PASTE )
{
   HBITMAP hBitmap_D = hmg_par_HBITMAP(1);
   INT x1            = hmg_par_INT(2);
   INT y1            = hmg_par_INT(3);
   INT Width1        = hmg_par_INT(4);
   INT Height1       = hmg_par_INT(5);

   HBITMAP hBitmap_O = hmg_par_HBITMAP(6);
   INT x2            = hmg_par_INT(7);
   INT y2            = hmg_par_INT(8);
   INT Width2        = hmg_par_INT(9);
   INT Height2       = hmg_par_INT(10);

   INT Mode_Stretch = hmg_par_INT(11);
   INT Action       = hmg_par_INT(12);
   COLORREF color_transp = hmg_par_COLORREF(13);

   HDC memDC_D = CreateCompatibleDC(nullptr);
   SelectObject(memDC_D, hBitmap_D);

   HDC memDC_O = CreateCompatibleDC(nullptr);
   SelectObject(memDC_O, hBitmap_O);

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (memDC_D, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(memDC_D, &Point);
   SetStretchBltMode(memDC_D, HALFTONE);
   SetBrushOrgEx(memDC_D, Point.x, Point.y, nullptr);

   switch( Action ) {
      case BT_BITMAP_OPAQUE:
         StretchBlt(memDC_D, x1, y1, Width1, Height1, memDC_O, x2, y2, Width2, Height2, SRCCOPY);
         break;
      case BT_BITMAP_TRANSPARENT:
         TransparentBlt(memDC_D, x1, y1, Width1, Height1, memDC_O, x2, y2, Width2, Height2, color_transp);
         break;
      default:
         hb_retl(false);
         return;
   }

   DeleteDC(memDC_D);
   DeleteDC(memDC_O);
   hb_retl(true);
}

//**********************************************************************************************************************
//* BT_BMP_PASTE_ALPHABLEND (hBitmap_D, x1, y1, Width1, Height1, hBitmap_O, x2, y2, Width2, Height2, Alpha, Mode_Stretch)
//**********************************************************************************************************************

// Alpha = 0 to 255
#define BT_ALPHABLEND_TRANSPARENT  0
#define BT_ALPHABLEND_OPAQUE       255

/*
BT_BMP_PASTE_ALPHABLEND(HBITMAP1, x1, y1, width1, height1, HBITMAP2, x2, y2, width2, height2) --> NIL
*/
HB_FUNC( BT_BMP_PASTE_ALPHABLEND )
{
   HBITMAP hBitmap_D = hmg_par_HBITMAP(1);
   INT x1            = hmg_par_INT(2);
   INT y1            = hmg_par_INT(3);
   INT Width1        = hmg_par_INT(4);
   INT Height1       = hmg_par_INT(5);

   HBITMAP hBitmap_O = hmg_par_HBITMAP(6);
   INT x2            = hmg_par_INT(7);
   INT y2            = hmg_par_INT(8);
   INT Width2        = hmg_par_INT(9);
   INT Height2       = hmg_par_INT(10);

   BYTE Alpha        = hmg_par_BYTE(11);
   INT Mode_Stretch  = hmg_par_INT(12);

   BLENDFUNCTION blend;
   blend.BlendOp             = AC_SRC_OVER;
   blend.BlendFlags          = 0;
   blend.AlphaFormat         = 0;
   blend.SourceConstantAlpha = Alpha;

   HDC memDC_D = CreateCompatibleDC(nullptr);
   SelectObject(memDC_D, hBitmap_D);

   HDC memDC_O = CreateCompatibleDC(nullptr);
   SelectObject(memDC_O, hBitmap_O);

   bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);

   //SetStretchBltMode (memDC_D, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(memDC_D, &Point);
   SetStretchBltMode(memDC_D, HALFTONE);
   SetBrushOrgEx(memDC_D, Point.x, Point.y, nullptr);

   AlphaBlend(memDC_D, x1, y1, Width1, Height1, memDC_O, x2, y2, Width2, Height2, blend);

   DeleteDC(memDC_D);
   DeleteDC(memDC_O);
}

//********************************************************************************
//* BT_BMP_CAPTURESCR (hWnd, x1, y1, Width1, Height1, Mode) ---> Return new_hBITMAP
//********************************************************************************

// Mode
#define BT_BITMAP_CAPTURE_DESKTOP     0
#define BT_BITMAP_CAPTURE_WINDOW      1
#define BT_BITMAP_CAPTURE_CLIENTAREA  2

/*
BT_BMP_CAPTURESCR(HWND, X, Y, WIDTH, HEIGHT) --> hBitmap
*/
HB_FUNC( BT_BMP_CAPTURESCR )
{
   HDC hDC;

   auto hWnd   = hmg_par_HWND(1);
   INT x1      = hmg_par_INT(2);
   INT y1      = hmg_par_INT(3);
   INT Width1  = hmg_par_INT(4);
   INT Height1 = hmg_par_INT(5);
   INT Mode    = hmg_par_INT(6);

   switch( Mode ) {
      case BT_BITMAP_CAPTURE_DESKTOP:
         // hWnd = GetDesktopWindow();
         hDC = GetDC(hWnd);
         break;
      case BT_BITMAP_CAPTURE_WINDOW:
         hDC = GetWindowDC(hWnd);
         break;
      case BT_BITMAP_CAPTURE_CLIENTAREA:
         hDC = GetDC(hWnd);
         break;
      default:
         hmg_ret_HBITMAP(nullptr);
         return;
   }

   HBITMAP hBitmap = bt_bmp_create_24bpp(Width1, Height1);

   HDC memDC = CreateCompatibleDC(nullptr);
   SelectObject(memDC, hBitmap);

   BitBlt(memDC, 0, 0, Width1, Height1, hDC, x1, y1, SRCCOPY);

   DeleteDC(memDC);

   switch( Mode ) {
      case BT_BITMAP_CAPTURE_DESKTOP:
      case BT_BITMAP_CAPTURE_WINDOW:
      case BT_BITMAP_CAPTURE_CLIENTAREA:
         ReleaseDC(hWnd, hDC);
   }

   hmg_ret_HBITMAP(hBitmap);
}

//**************************************************************************************************
//* BT_BMP_PROCESS (hBitmap, Action, Value)
//**************************************************************************************************

// Action                                       Value
#define BT_BMP_PROCESS_INVERT        0          // NIL
#define BT_BMP_PROCESS_GRAYNESS      1          // Gray_Level     = 0 to 100%
#define BT_BMP_PROCESS_BRIGHTNESS    2          // Light_Level    = -255 To +255
#define BT_BMP_PROCESS_CONTRAST      3          // Contrast_Angle = angle in radians
#define BT_BMP_PROCESS_MODIFYCOLOR   4          // { R = -255 To +255, G = -255 To +255, B = -255 To +255 }
#define BT_BMP_PROCESS_GAMMACORRECT  5          // {RedGamma = 0.2 To 5.0, GreenGamma = 0.2 To 5.0, BlueGamma = 0.2 To 5.0}

// Gray_Level = 0 To 100%
#define BT_BITMAP_GRAY_NONE          0
#define BT_BITMAP_GRAY_FULL          100

// Light_Level = -255 To +255
#define BT_BITMAP_LIGHT_BLACK        -255
#define BT_BITMAP_LIGHT_NONE         0
#define BT_BITMAP_LIGHT_WHITE        255

/*
BT_BMP_PROCESS(HBITMAP, action) --> .T.|.F.
*/
HB_FUNC( BT_BMP_PROCESS )
{
   struct bt_RGBCOLORBYTE
   {
      BYTE R;
      BYTE G;
      BYTE B;
   } ;

   #define bt_RGB_TO_GRAY(R, G, B)  static_cast<INT>(static_cast<FLOAT>(R) * 0.299 + static_cast<FLOAT>(G) * 0.587 + static_cast<FLOAT>(B) * 0.114)
   #define bt_GAMMA(index, gamma)   (HB_MIN(255, static_cast<INT>((255.0 * pow((static_cast<DOUBLE>(index) / 255.0), (1.0 / static_cast<DOUBLE>(gamma)))) + 0.5)))
   //  redGamma[i] = (byte)           Min (255, (int)(( 255.0 *Pow(i/255.0, 1.0/g_red)) + 0.5));

   LPBYTE            lp_Bits;
   bt_RGBCOLORBYTE * RGBcolor;
   BYTE              GrayValue;
   DOUBLE            GrayLevel  = 0;
   INT               LightLevel = 0, RLevel = 0, GLevel = 0, BLevel = 0;
   DOUBLE            ContrastAngle, ContrastConstant = 0, ContrastValue;
   DOUBLE            RedGamma, GreenGamma, BlueGamma;
   BYTE              RedGammaRamp[256];
   BYTE              GreenGammaRamp[256];
   BYTE              BlueGammaRamp[256];

   HBITMAP hBitmap = hmg_par_HBITMAP(1);
   INT Action = hmg_par_INT(2);

   switch( Action ) {
      case BT_BMP_PROCESS_INVERT:
         break;

      case BT_BMP_PROCESS_GRAYNESS:
         GrayLevel = static_cast<DOUBLE>(hb_parnd(3)) / 100.0;
         if( GrayLevel <= 0.0 || GrayLevel > 1.0 ) {
            hb_retl(false);
            return;
         }
         break;

      case BT_BMP_PROCESS_BRIGHTNESS:
         LightLevel = hmg_par_INT(3);
         if( (LightLevel < -255) || (LightLevel == 0) || (LightLevel > 255) ) {
            hb_retl(false);
            return;
         }
         break;

      case BT_BMP_PROCESS_CONTRAST:
         ContrastAngle = static_cast<DOUBLE>(hb_parnd(3));
         if( ContrastAngle <= 0.0 ) {
            hb_retl(false);
            return;
         }
         ContrastConstant = tan(ContrastAngle * M_PI / 180.0);
         break;

      case BT_BMP_PROCESS_MODIFYCOLOR:
         if( !HB_ISARRAY(3) || hb_parinfa(3, 0) != 3 ) {
            hb_retl(false);
            return;
         }
         RLevel = hb_parvni(3, 1);
         GLevel = hb_parvni(3, 2);
         BLevel = hb_parvni(3, 3);
         if( (HB_MIN(HB_MIN(RLevel, GLevel), BLevel) < -255) || (HB_MAX(HB_MAX(RLevel, GLevel), BLevel) > 255) ) {
            hb_retl(false);
            return;
         }
         break;

      case BT_BMP_PROCESS_GAMMACORRECT:
         if( !HB_ISARRAY(3) || hb_parinfa(3, 0) != 3 ) {
            hb_retl(false);
            return;
         }
         RedGamma   = static_cast<DOUBLE>(hb_parvnd(3, 1));
         GreenGamma = static_cast<DOUBLE>(hb_parvnd(3, 2));
         BlueGamma  = static_cast<DOUBLE>(hb_parvnd(3, 3));
         for( INT i = 0; i < 256; i++ ) {
            RedGammaRamp[i]   = static_cast<BYTE>(bt_GAMMA(i, RedGamma));
            GreenGammaRamp[i] = static_cast<BYTE>(bt_GAMMA(i, GreenGamma));
            BlueGammaRamp[i]  = static_cast<BYTE>(bt_GAMMA(i, BlueGamma));
         }
         break;

      default:
         hb_retl(false);
         return;
   }

   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   BITMAPINFO BI;
   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = bm.bmWidth;
   BI.bmiHeader.biHeight        = bm.bmHeight;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = 24;
   BI.bmiHeader.biCompression   = BI_RGB;
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   bm.bmWidthBytes = (bm.bmWidth * BI.bmiHeader.biBitCount + 31) / 32 * 4;
   DWORD nBytes_Bits = static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight));

   HGLOBAL hBits = GlobalAlloc(GHND, static_cast<DWORD>(nBytes_Bits));
   if( hBits == nullptr ) {
      hb_retl(false);
      return;
   } else {
      lp_Bits = static_cast<LPBYTE>(GlobalLock(hBits));
   }

   HDC memDC = CreateCompatibleDC(nullptr);
   GetDIBits(memDC, hBitmap, 0, bm.bmHeight, static_cast<LPVOID>(lp_Bits), &BI, DIB_RGB_COLORS);

   for( INT y = 0; y < bm.bmHeight; y++ ) {
      RGBcolor = ( bt_RGBCOLORBYTE * ) (lp_Bits + static_cast<LONG>(y) * bm.bmWidthBytes);

      for( INT x = 0; x < bm.bmWidth; x++ ) {
         switch( Action ) {
            case BT_BMP_PROCESS_INVERT:
               RGBcolor->R = 255 - RGBcolor->R;
               RGBcolor->G = 255 - RGBcolor->G;
               RGBcolor->B = 255 - RGBcolor->B;
               break;

            case BT_BMP_PROCESS_GRAYNESS:
               GrayValue   = static_cast<BYTE>(bt_RGB_TO_GRAY(RGBcolor->R, RGBcolor->G, RGBcolor->B));
               RGBcolor->R = static_cast<BYTE>(RGBcolor->R + (GrayValue - RGBcolor->R) * GrayLevel);
               RGBcolor->G = static_cast<BYTE>(RGBcolor->G + (GrayValue - RGBcolor->G) * GrayLevel);
               RGBcolor->B = static_cast<BYTE>(RGBcolor->B + (GrayValue - RGBcolor->B) * GrayLevel);
               break;

            case BT_BMP_PROCESS_BRIGHTNESS:
               RGBcolor->R = static_cast<BYTE>((RGBcolor->R + LightLevel < 0) ? 0 : ((RGBcolor->R + LightLevel > 255) ? 255 : (RGBcolor->R + LightLevel)));
               RGBcolor->G = static_cast<BYTE>((RGBcolor->G + LightLevel < 0) ? 0 : ((RGBcolor->G + LightLevel > 255) ? 255 : (RGBcolor->G + LightLevel)));
               RGBcolor->B = static_cast<BYTE>((RGBcolor->B + LightLevel < 0) ? 0 : ((RGBcolor->B + LightLevel > 255) ? 255 : (RGBcolor->B + LightLevel)));
               break;

            case BT_BMP_PROCESS_CONTRAST:
               ContrastValue = 128 + (RGBcolor->R - 128) * ContrastConstant;
               RGBcolor->R   = static_cast<BYTE>((ContrastValue < 0) ? 0 : ((ContrastValue > 255) ? 255 : ContrastValue));
               ContrastValue = 128 + (RGBcolor->G - 128) * ContrastConstant;
               RGBcolor->G   = static_cast<BYTE>((ContrastValue < 0) ? 0 : ((ContrastValue > 255) ? 255 : ContrastValue));
               ContrastValue = 128 + (RGBcolor->B - 128) * ContrastConstant;
               RGBcolor->B   = static_cast<BYTE>((ContrastValue < 0) ? 0 : ((ContrastValue > 255) ? 255 : ContrastValue));
               break;

            case BT_BMP_PROCESS_MODIFYCOLOR:
               RGBcolor->R = (RGBcolor->R + RLevel < 0) ? 0 : ((RGBcolor->R + RLevel > 255) ? 255 : (RGBcolor->R + RLevel));
               RGBcolor->G = (RGBcolor->G + GLevel < 0) ? 0 : ((RGBcolor->G + GLevel > 255) ? 255 : (RGBcolor->G + GLevel));
               RGBcolor->B = (RGBcolor->B + BLevel < 0) ? 0 : ((RGBcolor->B + BLevel > 255) ? 255 : (RGBcolor->B + BLevel));
               break;

            case BT_BMP_PROCESS_GAMMACORRECT:
               RGBcolor->R = RedGammaRamp[RGBcolor->R];
               RGBcolor->G = GreenGammaRamp[RGBcolor->G];
               RGBcolor->B = BlueGammaRamp[RGBcolor->B];
         }

         RGBcolor++;
      }
   }

   SetDIBits(memDC, hBitmap, 0, bm.bmHeight, lp_Bits, &BI, DIB_RGB_COLORS);
   DeleteDC(memDC);

   GlobalUnlock(hBits);
   GlobalFree(hBits);
   hb_retl(true);
}

//**************************************************************************************************
//* BT_BMP_FILTER3X3 (hBitmap, aFilter)
//**************************************************************************************************

struct bt_RGBCOLORBYTE
{
   BYTE R;
   BYTE G;
   BYTE B;
};

// Divisor  Bias
#define  BT_Kernel3x3Filter1  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 0 }                   // Smooth
#define  BT_Kernel3x3Filter2  { 0, 1, 0, 1, 4, 1, 0, 1, 0, 8, 0 }                   // Gaussian Smooth
#define  BT_Kernel3x3Filter3  { 0, -1, 0, -1, 9, -1, 0, -1, 0, 5, 0 }               // Sharpening
#define  BT_Kernel3x3Filter4  { -1, -1, -1, -1, 8, -1, -1, -1, -1, 1, 128 }         // Laplacian
#define  BT_Kernel3x3Filter5  { 1, 0, 0, 0, 0, 0, 0, 0, -1, 1, 128 }                // Emboss 135°
#define  BT_Kernel3x3Filter6  { 0, 1, 0, 0, 0, 0, 0, -1, 0, 2, 128 }                // Emboss 90° 50%

static bt_RGBCOLORBYTE bt_ConvolutionKernel3x3(bt_RGBCOLORBYTE * Y_previous, bt_RGBCOLORBYTE * Y_current, bt_RGBCOLORBYTE * Y_posterior, INT K[])
{
   bt_RGBCOLORBYTE RGBcolor;
   INT Divisor = K[9];
   INT Bias    = K[10];

   if( Divisor == 0 ) {
      Divisor = 1;
   }

   //   Y-1,X-1                    Y-1,X+0                    Y-1,X+1
   //   Y+0,X-1                  [Y+0,X+0]                  Y+0,X+1
   //   Y+1,X-1                    Y+1,X+0                    Y+1,X+1
   INT Red = ((Y_previous - 1)->R * K[0] + (Y_previous + 0)->R * K[1] + (Y_previous + 1)->R * K[2] +                     // Y_previous  = Y-1,X+0
             (Y_current - 1)->R * K[3] + (Y_current + 0)->R * K[4] + (Y_current + 1)->R * K[5] +                         // Y_current   = Y+0,X+0
             (Y_posterior - 1)->R * K[6] + (Y_posterior + 0)->R * K[7] + (Y_posterior + 1)->R * K[8] ) / Divisor + Bias; // Y_posterior = Y+1,X+0

   INT Green = ((Y_previous - 1)->G * K[0] + (Y_previous + 0)->G * K[1] + (Y_previous + 1)->G * K[2] +
               (Y_current - 1)->G * K[3] + (Y_current + 0)->G * K[4] + (Y_current + 1)->G * K[5] +
               (Y_posterior - 1)->G * K[6] + (Y_posterior + 0)->G * K[7] + (Y_posterior + 1)->G * K[8]) / Divisor + Bias;

   INT Blue = ((Y_previous - 1)->B * K[0] + (Y_previous + 0)->B * K[1] + (Y_previous + 1)->B * K[2] +
              (Y_current - 1)->B * K[3] + (Y_current + 0)->B * K[4] + (Y_current + 1)->B * K[5] +
              (Y_posterior - 1)->B * K[6] + (Y_posterior + 0)->B * K[7] + (Y_posterior + 1)->B * K[8]) / Divisor + Bias;

   #define bt_BoundRange(Value, RangeMin, RangeMax)  ((Value < RangeMin) ? RangeMin : ((Value > RangeMax) ? RangeMax : Value))

   RGBcolor.R = static_cast<BYTE>(bt_BoundRange(Red, 0, 255));
   RGBcolor.G = static_cast<BYTE>(bt_BoundRange(Green, 0, 255));
   RGBcolor.B = static_cast<BYTE>(bt_BoundRange(Blue, 0, 255));

   return RGBcolor;
}

/*
BT_BMP_FILTER3X3(HBITMAP, array) --> .T.|.F.
*/
HB_FUNC( BT_BMP_FILTER3X3 )
{
   #define N           3
   #define HALF        ((N - 1) / 2)
   #define nMATFILTER  (N * N + 2)

// bt_RGBCOLORBYTE *RGBcolor_O;
   bt_RGBCOLORBYTE * RGBcolor_D, RGBcolor_Ret;
   bt_RGBCOLORBYTE * RGBcolor_Yprevious_Xcurrent, * RGBcolor_Ycurrent_Xcurrent, * RGBcolor_Yposterior_Xcurrent;
   INT MatKernel3x3Filter[nMATFILTER];

   HBITMAP hBitmap = hmg_par_HBITMAP(1);
   if( !HB_ISARRAY(2) || hb_parinfa(2, 0) != nMATFILTER ) {
      hb_retl(false);
      return;
   }
   for( INT i = 0; i < nMATFILTER; i++ ) {
      MatKernel3x3Filter[i] = hb_parvni(2, i + 1);
   }

   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   BITMAPINFO BI;
   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = bm.bmWidth;
   BI.bmiHeader.biHeight        = -bm.bmHeight;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = 24;
   BI.bmiHeader.biCompression   = BI_RGB;
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   bm.bmWidthBytes = (bm.bmWidth * BI.bmiHeader.biBitCount + 31) / 32 * 4;
   DWORD nBytes_Bits = static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight));

   HGLOBAL hBits_O = GlobalAlloc(GHND, nBytes_Bits);
   if( hBits_O == nullptr ) {
      hb_retl(false);
      return;
   }

   HGLOBAL hBits_D = GlobalAlloc(GHND, nBytes_Bits);
   if( hBits_D == nullptr ) {
      GlobalFree(hBits_O);
      hb_retl(false);
      return;
   }

   LPBYTE lp_Bits_O = static_cast<LPBYTE>(GlobalLock(hBits_O));
   LPBYTE lp_Bits_D = static_cast<LPBYTE>(GlobalLock(hBits_D));

   HDC memDC = CreateCompatibleDC(nullptr);

   GetDIBits(memDC, hBitmap, 0, bm.bmHeight, static_cast<LPVOID>(lp_Bits_O), &BI, DIB_RGB_COLORS);

   for( INT y = 0; y < bm.bmHeight; y++ ) {
//     RGBcolor_O = (bt_RGBCOLORBYTE *) (lp_Bits_O + (LONG) (y) * bm.bmWidthBytes);
      RGBcolor_D = ( bt_RGBCOLORBYTE * ) (lp_Bits_D + static_cast<LONG>(y) * bm.bmWidthBytes);

      for( INT x = 0; x < bm.bmWidth; x++ ) {
         if( (y >= HALF && y < (bm.bmHeight - HALF)) && (x >= HALF && x < (bm.bmWidth - HALF)) ) {
            RGBcolor_Yprevious_Xcurrent  = ( bt_RGBCOLORBYTE * ) (lp_Bits_O + static_cast<LONG>(y - 1) * bm.bmWidthBytes + x * sizeof(bt_RGBCOLORBYTE));
            RGBcolor_Ycurrent_Xcurrent   = ( bt_RGBCOLORBYTE * ) (lp_Bits_O + static_cast<LONG>(y + 0) * bm.bmWidthBytes + x * sizeof(bt_RGBCOLORBYTE));
            RGBcolor_Yposterior_Xcurrent = ( bt_RGBCOLORBYTE * ) (lp_Bits_O + static_cast<LONG>(y + 1) * bm.bmWidthBytes + x * sizeof(bt_RGBCOLORBYTE));

            RGBcolor_Ret = bt_ConvolutionKernel3x3(RGBcolor_Yprevious_Xcurrent, RGBcolor_Ycurrent_Xcurrent, RGBcolor_Yposterior_Xcurrent, MatKernel3x3Filter);
            RGBcolor_D->R = RGBcolor_Ret.R;
            RGBcolor_D->G = RGBcolor_Ret.G;
            RGBcolor_D->B = RGBcolor_Ret.B;

/*
   #define BT_FILTER_NONE     0
   #define BT_FILTER_FULL   255
              INT Alpha = 200;  // transparent = color origin = 0 To 255 = opaque = full filter color
              RGBcolor_D->R = (BYTE)((RGBcolor_Ret.R * Alpha + RGBcolor_O->R * (255 - Alpha)) / 255);
              RGBcolor_D->G = (BYTE)((RGBcolor_Ret.G * Alpha + RGBcolor_O->G * (255 - Alpha)) / 255);
              RGBcolor_D->B = (BYTE)((RGBcolor_Ret.B * Alpha + RGBcolor_O->B * (255 - Alpha)) / 255);
 */
         }
//         RGBcolor_O ++;
         RGBcolor_D++;
      }
   }

   SetDIBits(memDC, hBitmap, 0, bm.bmHeight, lp_Bits_D, &BI, DIB_RGB_COLORS);

   DeleteDC(memDC);

   GlobalUnlock(hBits_O);
   GlobalUnlock(hBits_D);

   GlobalFree(hBits_O);
   GlobalFree(hBits_D);

   hb_retl(true);
}

//***********************************************************************************************************************
//* BT_BMP_TRANSFORM (hBitmap, Mode, Angle, Color_Fill_Bk) ---> Return New_hBitmap
//***********************************************************************************************************************

// Mode
#define BT_BITMAP_REFLECT_HORIZONTAL  1
#define BT_BITMAP_REFLECT_VERTICAL    2
#define BT_BITMAP_ROTATE              4

// Angle (mode rotate) = 0 to 360º
// Color_Fill_Bk (mode rotate) = color to fill the empty spaces the background

/*
BT_BMP_TRANSFORM(HBITMAP, mode, angle, backgroundColor) --> HBITMAP
*/
HB_FUNC( BT_BMP_TRANSFORM )
{
   XFORM xform1  = {1, 0, 0, 1, 0, 0}; // Normal
   XFORM xform2  = {1, 0, 0, 1, 0, 0}; // Normal
   XFORM xform_D = {1, 0, 0, 1, 0, 0}; // Normal

   const double pi = 3.141592;

   #define dABS(n)     (static_cast<double>(n) >= 0.0 ? static_cast<double>(n) : static_cast<double>(-n))
   #define SCALING(n)  (static_cast<double>(n) > 1.0 ? static_cast<double>(1.0 / n) : static_cast<double>(1.0))

   HBITMAP hBitmap_O      = hmg_par_HBITMAP(1);
   INT Mode               = hb_parnl(2);
   FLOAT Angle            = static_cast<FLOAT>(hb_parnd(3));
   COLORREF Color_Fill_Bk = hmg_par_COLORREF(4);

   HDC memDC1 = CreateCompatibleDC(nullptr);
   SelectObject(memDC1, hBitmap_O);
   BITMAP bm;
   GetObject(hBitmap_O, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   INT Width  = bm.bmWidth;
   INT Height = bm.bmHeight;

   HDC memDC2 = CreateCompatibleDC(nullptr);
   SetGraphicsMode(memDC2, GM_ADVANCED);

   if( (Mode & BT_BITMAP_REFLECT_HORIZONTAL) == BT_BITMAP_REFLECT_HORIZONTAL ) {
      xform1.eM11 = static_cast<FLOAT>(-1.0);
      xform1.eDx  = static_cast<FLOAT>(Width - 1);

      if( (Mode & BT_BITMAP_ROTATE) == BT_BITMAP_ROTATE ) {
         xform1.eDx = static_cast<FLOAT>(Width);
      }
   }

   if( (Mode & BT_BITMAP_REFLECT_VERTICAL) == BT_BITMAP_REFLECT_VERTICAL ) {
      xform1.eM22 = static_cast<FLOAT>(-1.0);
      xform1.eDy  = static_cast<FLOAT>(Height - 1);

      if( (Mode & BT_BITMAP_ROTATE) == BT_BITMAP_ROTATE ) {
         xform1.eDy = static_cast<FLOAT>(Height);
      }
   }

   if( (Mode & BT_BITMAP_ROTATE) == BT_BITMAP_ROTATE ) {
      if( (Angle <= 0.0) || (Angle > 360.0) ) {
         Angle = static_cast<FLOAT>(360.0);
      }

      // Angle = angulo en grados
      double radianes = (2 * pi) * static_cast<double>(Angle) / static_cast<double>(360.0);

      // x1,y1 = W,0
      // x2,y2 = W,H
      // x3,y3 = 0,H

      // A = angle in radians
      // new_x = (x * cos A) - (y * sin A)
      // new_y = (x * sin A) + (y * cos A)


      double x1 = static_cast<double>(Width) * cos(radianes);
      double y1 = static_cast<double>(Width) * sin(radianes);

      double x2 = (static_cast<double>(Width) * cos(radianes)) - (static_cast<double>(Height) * sin(radianes));
      double y2 = (static_cast<double>(Width) * sin(radianes)) + (static_cast<double>(Height) * cos(radianes));

      double x3 = -(static_cast<double>(Height) * sin(radianes));
      double y3 = (static_cast<double>(Height) * cos(radianes));

      xform2.eM11 = static_cast<FLOAT>(cos(radianes));
      xform2.eM12 = static_cast<FLOAT>(sin(radianes));
      xform2.eM21 = static_cast<FLOAT>(-sin(radianes));
      xform2.eM22 = static_cast<FLOAT>(cos(radianes));
      xform2.eDx  = static_cast<FLOAT>(0.0);
      xform2.eDy  = static_cast<FLOAT>(0.0);

      if( Angle <= 90.0 ) {
         xform2.eDx = static_cast<FLOAT>(-x3);
         xform2.eDy = static_cast<FLOAT>(0.0);

         Width = dABS((x3 - x1));

         Height = dABS(y2);
      }

      if( (Angle > 90.0) && (Angle <= 180.0) ) {
         xform2.eDx = static_cast<FLOAT>(-x2);
         xform2.eDy = static_cast<FLOAT>(-y3);

         Width = dABS(x2);

         Height = dABS((y3 - y1));
      }

      if( (Angle > 180.0) && (Angle <= 270.0) ) {
         xform2.eDx = static_cast<FLOAT>(-x1);
         xform2.eDy = static_cast<FLOAT>(-y2);

         Width = dABS((x3 - x1));

         Height = dABS(y2);
      }

      if( (Angle > 270.0) && (Angle <= 360.0) ) {
         xform2.eDx = static_cast<FLOAT>(0.0);
         xform2.eDy = static_cast<FLOAT>(-y1);

         Width = dABS(x2);

         Height = dABS((y3 - y1));
      }

      Width++;
      Height++;

      if( (Angle == 0.0) || (Angle == 180.0) || (Angle == 360.0) ) {
         Width  = bm.bmWidth;
         Height = bm.bmHeight;
      }
      if( (Angle == 90.0) || (Angle == 270.0) ) {
         Width  = bm.bmHeight;
         Height = bm.bmWidth;
      }
   }

   HBITMAP hBitmap_D = bt_bmp_create_24bpp(Width, Height);
   SelectObject(memDC2, hBitmap_D);

   //SetStretchBltMode (memDC2, COLORONCOLOR);
   POINT Point;
   GetBrushOrgEx(memDC2, &Point);
   SetStretchBltMode(memDC2, HALFTONE);
   SetBrushOrgEx(memDC2, Point.x, Point.y, nullptr);

   HBRUSH hBrush   = CreateSolidBrush(Color_Fill_Bk);
   HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(memDC2, hBrush));
   RECT rectang;
   SetRect(&rectang, 0, 0, Width, Height);
   FillRect(memDC2, &rectang, hBrush);

   CombineTransform(&xform_D, &xform1, &xform2);
   SetWorldTransform(memDC2, &xform_D);

   StretchBlt(memDC2, 0, 0, bm.bmWidth, bm.bmHeight, memDC1, 0, 0, bm.bmWidth, bm.bmHeight, SRCCOPY);

   SelectObject(memDC2, OldBrush);
   DeleteDC(memDC1);
   DeleteDC(memDC2);
   DeleteObject(hBrush);

   hmg_ret_HBITMAP(hBitmap_D);
}

//************************************************************************************************************
//* BT_BMP_CLIPBOARD_ISEMPTY () ---> Return TRUE (Empty clipboard: DIB format) or FALSE (Not empty clipboard)
//************************************************************************************************************

/*
BT_BMP_CLIPBOARD_ISEMPTY() --> .T.|.F.
*/
HB_FUNC( BT_BMP_CLIPBOARD_ISEMPTY )
{
   hb_retl(IsClipboardFormatAvailable(CF_DIB) ? false : true);
}

//************************************************************************************************************
//* BT_BMP_CLEAN_CLIPBOARD () ---> Return Success (TRUE or FALSE)
//************************************************************************************************************

/*
BT_BMP_CLEAN_CLIPBOARD(HWND) --> .T.|.F.
*/
HB_FUNC( BT_BMP_CLEAN_CLIPBOARD )
{
   if( !IsClipboardFormatAvailable(CF_DIB) ) {
      hb_retl(false);
      return;
   }

   auto hWnd = hmg_par_HWND(1);
   if( !OpenClipboard(hWnd) ) {
      hb_retl(false);
      return;
   }

   EmptyClipboard();
   CloseClipboard();

   hb_retl(true);
}

//*************************************************************************************************
//* BT_BMP_GET_CLIPBOARD (hWnd) ---> Return hBitmap (Success) or 0 (Failure or Clipboard Empty DIB format)
//*************************************************************************************************

/*
BT_BMP_GET_CLIPBOARD(HWND) --> HBITMAP
*/
HB_FUNC( BT_BMP_GET_CLIPBOARD )
{
   if( !IsClipboardFormatAvailable(CF_DIB) ) {
      hmg_ret_HBITMAP(nullptr);
      return;
   }

   auto hWnd = hmg_par_HWND(1);
   if( !OpenClipboard(hWnd) ) {
      hmg_ret_HBITMAP(nullptr);
      return;
   }

   HGLOBAL hClipboard = GetClipboardData(CF_DIB);
   if( hClipboard == nullptr ) {
      CloseClipboard();
      hmg_ret_HBITMAP(nullptr);
      return;
   }

   LPBYTE lp_Clipboard = static_cast<LPBYTE>(GlobalLock(hClipboard));

   LPBITMAPINFO lp_BI = ( LPBITMAPINFO ) lp_Clipboard;

   WORD nBytes_Offset = 0;
   if( lp_BI->bmiHeader.biBitCount == 1 ) {
      nBytes_Offset = sizeof(RGBQUAD) * 2;
   }
   if( lp_BI->bmiHeader.biBitCount == 4 ) {
      nBytes_Offset = sizeof(RGBQUAD) * 16;
   }
   if( lp_BI->bmiHeader.biBitCount == 8 ) {
      nBytes_Offset = sizeof(RGBQUAD) * 256;
   }

   LPBYTE lp_Bits = static_cast<LPBYTE>(lp_Clipboard + (sizeof(BITMAPINFOHEADER) + nBytes_Offset));

   BITMAPINFO   BI;
   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = lp_BI->bmiHeader.biWidth;
   BI.bmiHeader.biHeight        = lp_BI->bmiHeader.biHeight;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = 24;
   BI.bmiHeader.biCompression   = BI_RGB;
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   HDC memDC = CreateCompatibleDC(nullptr);

   LPBYTE lp_Bits2;
   HBITMAP hBitmap = CreateDIBSection(memDC, &BI, DIB_RGB_COLORS, ( VOID ** ) &lp_Bits2, nullptr, 0);
   SetDIBits(memDC, hBitmap, 0, BI.bmiHeader.biHeight, lp_Bits, lp_BI, DIB_RGB_COLORS);

   DeleteDC(memDC);

   GlobalUnlock(hClipboard);
   CloseClipboard();

   hmg_ret_HBITMAP(hBitmap);
}

//*************************************************************************************************
//* BT_BMP_PUT_CLIPBOARD (hBitmap) ---> Return Success (TRUE or FALSE)
//*************************************************************************************************

/*
BT_BMP_PUT_CLIPBOARD(HWND, HBITMAP) --> .T.|.F.
*/
HB_FUNC( BT_BMP_PUT_CLIPBOARD )
{
   auto hWnd = hmg_par_HWND(1);
   HBITMAP hBitmap = hmg_par_HBITMAP(2);

   BITMAP bm;
   GetObject(hBitmap, sizeof(BITMAP), reinterpret_cast<LPBYTE>(&bm));

   BITMAPINFO BI;
   BI.bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
   BI.bmiHeader.biWidth         = bm.bmWidth;
   BI.bmiHeader.biHeight        = bm.bmHeight;
   BI.bmiHeader.biPlanes        = 1;
   BI.bmiHeader.biBitCount      = 24;
   BI.bmiHeader.biCompression   = BI_RGB;
   BI.bmiHeader.biSizeImage     = 0;
   BI.bmiHeader.biXPelsPerMeter = 0;
   BI.bmiHeader.biYPelsPerMeter = 0;
   BI.bmiHeader.biClrUsed       = 0;
   BI.bmiHeader.biClrImportant  = 0;

   bm.bmWidthBytes = (bm.bmWidth * BI.bmiHeader.biBitCount + 31) / 32 * 4;

   DWORD nBytes_Bits  = static_cast<DWORD>(bm.bmWidthBytes * labs(bm.bmHeight));
   DWORD nBytes_Total = sizeof(BITMAPINFOHEADER) + nBytes_Bits;

   if( !OpenClipboard(hWnd) ) {
      hb_retl(false);
      return;
   }

   HGLOBAL hClipboard = GlobalAlloc(GHND, nBytes_Total);
   if( hClipboard == nullptr ) {
      CloseClipboard();
      hb_retl(false);
      return;
   }

   LPBYTE lp_Clipboard = reinterpret_cast<LPBYTE>(GlobalLock(hClipboard));

   memcpy(lp_Clipboard, &BI.bmiHeader, sizeof(BITMAPINFOHEADER));

   HDC memDC = CreateCompatibleDC(nullptr);
   GetDIBits(memDC, hBitmap, 0, bm.bmHeight, static_cast<LPVOID>(lp_Clipboard + sizeof(BITMAPINFOHEADER)), &BI, DIB_RGB_COLORS);

   GlobalUnlock(hClipboard);

   EmptyClipboard();
   SetClipboardData(CF_DIB, hClipboard);
   CloseClipboard();

   DeleteDC(memDC);

   hb_retl(true);
}

// ::::::::::::::::::::::::::::::::::::
// :::   MISCELLANEOUS Functions    :::
// ::::::::::::::::::::::::::::::::::::

//******************************************
//* BT_DELAY_EXECUTION (nMilliSeconds)
//******************************************

/*
BT_DELAY_EXECUTION() --> NIL
*/
HB_FUNC( BT_DELAY_EXECUTION )
{
   clock_t inicio = clock();
   clock_t ciclos = ( clock_t ) hb_parnl(1);

   while( clock() - inicio <= ciclos );
}

//*********************************************************
//* BT_DELAY_EXECUTION_WITH_DOEVENTS (nMilliSeconds)
//*********************************************************

/*
BT_DELAY_EXECUTION_WITH_DOEVENTS() --> NIL
*/
HB_FUNC( BT_DELAY_EXECUTION_WITH_DOEVENTS )
{
   MSG     Msg;
   clock_t inicio = clock();
   clock_t ciclos = ( clock_t ) hb_parnl(1);

   while( clock() - inicio <= ciclos ) {
      if( PeekMessage(( LPMSG ) &Msg, 0, 0, 0, PM_REMOVE) ) {
         TranslateMessage(&Msg);
         DispatchMessage(&Msg);
      }
   }
}

//*****************************************************
//* BT_SCR_SHOWCURSOR (lOnOff) ---> Show/Hide Cursor
//*****************************************************

/*
BT_SCR_SHOWCURSOR(.T.|.F.) --> numeric
*/
HB_FUNC( BT_SCR_SHOWCURSOR )
{
   hb_retni(ShowCursor(hb_parl(1)));
}

//***************************************************************************
//* BT_STRETCH_RECT (@Width1, @Height1, @Width2, @Height2, Mode_Stretch)
//***************************************************************************

/*
BT_STRETCH_RECT(@width1, @height1, @width2, @height2, modeStretch) --> .T.|.F.
*/
HB_FUNC( BT_STRETCH_RECT )
{
   INT Width1       = hmg_par_INT(1);
   INT Height1      = hmg_par_INT(2);
   INT Width2       = hmg_par_INT(3);
   INT Height2      = hmg_par_INT(4);
   INT Mode_Stretch = hb_parnl(5);

   if( HB_ISBYREF(1) && HB_ISBYREF(2) && HB_ISBYREF(3) && HB_ISBYREF(4) ) {
      bt_bmp_adjust_rect(&Width1, &Height1, &Width2, &Height2, Mode_Stretch);
      hb_storni(Width1, 1);
      hb_storni(Height1, 2);
      hb_storni(Width2, 3);
      hb_storni(Height2, 4);
      hb_retl(true);
   } else {
      hb_retl(false);
   }
}

//*******************************************************************************************************
//* BT_TEXTOUT_SIZE (hWnd, Text, FontName, FontSize, Type) --> { nW , nH }
//*******************************************************************************************************

/*
   // Type
   #define BT_TEXT_BOLD        2
   #define BT_TEXT_ITALIC      4
   #define BT_TEXT_UNDERLINE   8
   #define BT_TEXT_STRIKEOUT   16
 */

/*
BT_TEXTOUT_SIZE(HWND, text, fontName, fontSize, type) --> array
*/
HB_FUNC( BT_TEXTOUT_SIZE )
{
   auto hWnd = hmg_par_HWND(1);
#ifndef UNICODE
   TCHAR * lpText   = ( TCHAR * ) hb_parc(2);
   TCHAR * FontName = ( TCHAR * ) hb_parc(3);
#else
   TCHAR * lpText   = ( TCHAR * ) hb_osStrU16Encode(hb_parc(2));
   TCHAR * FontName = ( TCHAR * ) hb_osStrU16Encode(hb_parc(3));
#endif
   INT FontSize = hmg_par_INT(4);
   INT Type = hmg_par_INT(5);

   HDC hDC = GetDC(hWnd);

   INT Bold      = FW_NORMAL;
   INT Italic    = 0;
   INT Underline = 0;
   INT StrikeOut = 0;

   if( (Type & BT_TEXT_BOLD) == BT_TEXT_BOLD ) {
      Bold = FW_BOLD;
   }

   if( (Type & BT_TEXT_ITALIC) == BT_TEXT_ITALIC ) {
      Italic = 1;
   }

   if( (Type & BT_TEXT_UNDERLINE) == BT_TEXT_UNDERLINE ) {
      Underline = 1;
   }

   if( (Type & BT_TEXT_STRIKEOUT) == BT_TEXT_STRIKEOUT ) {
      StrikeOut = 1;
   }

   FontSize = FontSize * GetDeviceCaps(hDC, LOGPIXELSY) / 72;   // Size of font in logic points

   HFONT hFont = CreateFont(0 - FontSize, 0, 0, 0, Bold, Italic, Underline, StrikeOut,
                      DEFAULT_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, FontName);

   HFONT hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));

/*
   When GetTextExtentPoint32() returns the text extent, it assumes that the text is HORIZONTAL,
   that is, that the ESCAPEMENT is always 0. This is true for both the horizontal and
   vertical measurements of the text. Even if you use a font that specifies a nonzero
   escapement, this function doesn't use the angle while it computes the text extent.
   The app must convert it explicitly.
 */

   SIZE SizeText;
   GetTextExtentPoint32(hDC, lpText, lstrlen(lpText), &SizeText);
   hb_reta(2);
   HB_STORVNL(SizeText.cx, -1, 1);
   HB_STORVNL(SizeText.cy, -1, 2);

   SelectObject(hDC, hOldFont);
   DeleteObject(hFont);
   ReleaseDC(hWnd, hDC);
}

// TODO: remove unnecessary casts to double
