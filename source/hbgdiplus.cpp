/*
   Parts of this code is contributed and used here under permission of his
   author: Copyright 2007-2017 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "mgdefs.hpp"

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

HMODULE g_GpModule = nullptr;
static ULONG_PTR g_GpToken = 0;

/**
 */
GpStatus GdiplusInit(void)
{
  LPCTSTR lpFileName = TEXT("Gdiplus.dll");
  GDIPLUS_STARTUP_INPUT GdiplusStartupInput = {1, nullptr, FALSE, FALSE};

  if (g_GpModule == nullptr)
  {
    g_GpModule = LoadLibrary(lpFileName);
  }

  if (g_GpModule == nullptr)
  {
    return GdiplusNotInitialized;
  }

  if (_EMPTY_PTR(g_GpModule, GdiplusStartup))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdiplusShutdown))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipCreateBitmapFromFile))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipCreateBitmapFromResource))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipCreateBitmapFromStream))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipCreateHBITMAPFromBitmap))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipDisposeImage))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipGetImageEncodersSize))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipGetImageEncoders))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipCreateBitmapFromHBITMAP))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipSaveImageToFile))
  {
    return NotImplemented;
  }

  if (_EMPTY_PTR(g_GpModule, GdipGetImageThumbnail))
  {
    return NotImplemented;
  }

  return fn_GdiplusStartup(&g_GpToken, &GdiplusStartupInput, nullptr);
}

HB_FUNC(HMG_GDIPLUSSHUTDOWN)
{
  if (fn_GdiplusShutdown != nullptr)
  {
    fn_GdiplusShutdown(g_GpToken);
  }

  if (hb_parldef(1, true) == true && (g_GpModule != nullptr))
  {
    FreeLibrary(g_GpModule);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GDIPLUSSHUTDOWN, HMG_GDIPLUSSHUTDOWN)
#endif

HB_FUNC(HMG_GDIPCREATEBITMAPFROMFILE)
{
  GpBitmap *bitmap = nullptr;

  if (fn_GdipCreateBitmapFromFile != nullptr)
  {
    HB_WCHAR *lpFName = (HB_WCHAR *)((hb_parclen(1) == 0) ? nullptr : hb_mbtowc(hb_parc(1)));

    if (lpFName != nullptr)
    {
      hb_retni(fn_GdipCreateBitmapFromFile(lpFName, &bitmap));

      hb_xfree(lpFName);
    }
    else
    {
      hb_retni(InvalidParameter);
    }
  }
  else
  {
    hb_retni(NotImplemented);
  }

  hb_storptr(bitmap, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GDIPCREATEBITMAPFROMFILE, HMG_GDIPCREATEBITMAPFROMFILE)
#endif

HB_FUNC(HMG_GDIPCREATEHBITMAPFROMBITMAP)
{
  HBITMAP hbitmap = nullptr;

  if (fn_GdipCreateHBITMAPFromBitmap != nullptr)
  {
    GpBitmap *bitmap = (GpBitmap *)hb_parptr(1);

    if (bitmap != nullptr)
    {
      ARGB argb = (ARGB)hb_parnl(3);

      hb_retni(fn_GdipCreateHBITMAPFromBitmap(bitmap, &hbitmap, argb));
    }
    else
    {
      hb_retni(InvalidParameter);
    }
  }
  else
  {
    hb_retni(NotImplemented);
  }

  hb_storptr(hbitmap, 2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GDIPCREATEHBITMAPFROMBITMAP, HMG_GDIPCREATEHBITMAPFROMBITMAP)
#endif

HB_FUNC(HMG_GDIPDISPOSEIMAGE)
{
  if (fn_GdipDisposeImage != nullptr)
  {
    hb_retni(fn_GdipDisposeImage(reinterpret_cast<GpImage *>(hb_parptr(1))));
  }
  else
  {
    hb_retni(NotImplemented);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GDIPDISPOSEIMAGE, HMG_GDIPDISPOSEIMAGE)
#endif
