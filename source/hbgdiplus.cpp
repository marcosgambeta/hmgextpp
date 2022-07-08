/*
   Parts of this code is contributed and used here under permission of his
   author: Copyright 2007-2017 (C) P.Chornyj <myorg63@mail.ru>
 */

#include <mgdefs.h>

#define _HMG_STUB_
#include "hbgdiplus.h"
#undef _HMG_STUB_

DECLARE_FUNCPTR(GdiplusStartup);
DECLARE_FUNCPTR(GdiplusShutdown);

DECLARE_FUNCPTR(GdipCreateBitmapFromFile);
DECLARE_FUNCPTR(GdipCreateBitmapFromResource);
DECLARE_FUNCPTR(GdipCreateBitmapFromStream);
DECLARE_FUNCPTR(GdipCreateHBITMAPFromBitmap);
DECLARE_FUNCPTR(GdipDisposeImage);
DECLARE_FUNCPTR(GdipGetImageEncodersSize);
DECLARE_FUNCPTR(GdipGetImageEncoders);
DECLARE_FUNCPTR(GdipGetImageThumbnail);
DECLARE_FUNCPTR(GdipCreateBitmapFromHBITMAP);
DECLARE_FUNCPTR(GdipSaveImageToFile);

HMODULE g_GpModule         = nullptr;
static ULONG_PTR g_GpToken = 0;

/**
 */
GpStatus GdiplusInit(void)
{
   LPCTSTR lpFileName = TEXT("Gdiplus.dll");
   GDIPLUS_STARTUP_INPUT GdiplusStartupInput = { 1, nullptr, FALSE, FALSE };

   if( nullptr == g_GpModule )
      g_GpModule = LoadLibrary(lpFileName);

   if( nullptr == g_GpModule )
      return GdiplusNotInitialized;

   if( _EMPTY_PTR(g_GpModule, GdiplusStartup) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdiplusShutdown) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipCreateBitmapFromFile) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipCreateBitmapFromResource) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipCreateBitmapFromStream) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipCreateHBITMAPFromBitmap) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipDisposeImage) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipGetImageEncodersSize) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipGetImageEncoders) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipCreateBitmapFromHBITMAP) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipSaveImageToFile) )
      return NotImplemented;

   if( _EMPTY_PTR(g_GpModule, GdipGetImageThumbnail) )
      return NotImplemented;

   return fn_GdiplusStartup(&g_GpToken, &GdiplusStartupInput, nullptr);
}

HB_FUNC( GDIPLUSSHUTDOWN )
{
   if( nullptr != fn_GdiplusShutdown )
      fn_GdiplusShutdown(g_GpToken);

   if( HB_TRUE == hb_parldef(1, HB_TRUE) && ( nullptr != g_GpModule ) )
      FreeLibrary(g_GpModule);
}

HB_FUNC( GDIPCREATEBITMAPFROMFILE )
{
   GpBitmap * bitmap = ( GpBitmap * ) nullptr;

   if( nullptr != fn_GdipCreateBitmapFromFile )
   {
      HB_WCHAR * lpFName = ( HB_WCHAR * ) ( ( hb_parclen(1) == 0 ) ? nullptr : hb_mbtowc( hb_parc(1) ) );

      if( nullptr != lpFName )
      {
         hb_retni( fn_GdipCreateBitmapFromFile(lpFName, &bitmap) );

         hb_xfree(lpFName);
      }
      else
         hb_retni( InvalidParameter );
   }
   else
      hb_retni( NotImplemented );

   hb_storptr(bitmap, 2);
}

HB_FUNC( GDIPCREATEHBITMAPFROMBITMAP )
{
   HBITMAP hbitmap = ( HBITMAP ) nullptr;

   if( nullptr != fn_GdipCreateHBITMAPFromBitmap )
   {
      GpBitmap * bitmap = ( GpBitmap * ) hb_parptr(1);

      if( nullptr != bitmap )
      {
         ARGB argb = ( ARGB ) hb_parnl(3);

         hb_retni( fn_GdipCreateHBITMAPFromBitmap(bitmap, &hbitmap, argb) );
      }
      else
         hb_retni( InvalidParameter );
   }
   else
      hb_retni( NotImplemented );

   hb_storptr(hbitmap, 2);
}

HB_FUNC( GDIPDISPOSEIMAGE )
{
   if( nullptr != fn_GdipDisposeImage )
      hb_retni( fn_GdipDisposeImage(reinterpret_cast<GpImage*>(hb_parptr(1))) );
   else
      hb_retni( NotImplemented );
}
