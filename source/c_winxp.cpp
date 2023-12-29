/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2005 Andy Wos <andywos@unwired.com.au>
 *
 * Added to MiniGUI project (1.0 Experimental Build 9a)
 * by Jacek Kubica <kubica@wssk.wroc.pl>
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
#include <hbwinuni.hpp>

#ifndef __WINE_UXTHEME_H
#define __WINE_UXTHEME_H

#include <commctrl.h>

extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);

using HTHEME = HANDLE;

HRESULT WINAPI CloseThemeData(HTHEME hTheme);
HRESULT WINAPI DrawThemeBackground(HTHEME, HDC, int, int, const RECT *, const RECT *);

#define DTBG_CLIPRECT         0x00000001
#define DTBG_DRAWSOLID        0x00000002
#define DTBG_OMITBORDER       0x00000004
#define DTBG_OMITCONTENT      0x00000008
#define DTBG_COMPUTINGREGION  0x00000010
#define DTBG_MIRRORDC         0x00000020

struct _DTBGOPTS
{
   DWORD dwSize;
   DWORD dwFlags;
   RECT  rcClip;
};

using DTBGOPTS = _DTBGOPTS;
using PDTBGOPTS = _DTBGOPTS *;

HRESULT WINAPI DrawThemeBackgroundEx(HTHEME, HDC, int, int, const RECT *, const DTBGOPTS *);
HRESULT WINAPI DrawThemeEdge(HTHEME, HDC, int, int, const RECT *, UINT, UINT, RECT *);
HRESULT WINAPI DrawThemeIcon(HTHEME, HDC, int, int, const RECT *, HIMAGELIST, int);
HRESULT WINAPI DrawThemeParentBackground(HWND, HDC, RECT *);

#define DTT_GRAYED             0x1

HRESULT WINAPI DrawThemeText(HTHEME, HDC, int, int, LPCWSTR, int, DWORD, DWORD, const RECT *);

#define ETDT_DISABLE           0x00000001
#define ETDT_ENABLE            0x00000002
#define ETDT_USETABTEXTURE     0x00000004
#define ETDT_ENABLETAB         (ETDT_ENABLE | ETDT_USETABTEXTURE)

HRESULT WINAPI EnableThemeDialogTexture(HWND, DWORD);
HRESULT WINAPI EnableTheming(BOOL);
HRESULT WINAPI GetCurrentThemeName(LPWSTR, int, LPWSTR, int, LPWSTR, int);

#define STAP_ALLOW_NONCLIENT   (1 << 0)
#define STAP_ALLOW_CONTROLS    (1 << 1)
#define STAP_ALLOW_WEBCONTENT  (1 << 2)

DWORD WINAPI GetThemeAppProperties(void);
HRESULT WINAPI GetThemeBackgroundContentRect(HTHEME, HDC, int, int, const RECT *, RECT *);
HRESULT WINAPI GetThemeBackgroundExtent(HTHEME, HDC, int, int, const RECT *, RECT *);
HRESULT WINAPI GetThemeBackgroundRegion(HTHEME, HDC, int, int, const RECT *, HRGN *);
HRESULT WINAPI GetThemeBool(HTHEME, int, int, int, BOOL *);
HRESULT WINAPI GetThemeColor(HTHEME, int, int, int, COLORREF *);

HRESULT WINAPI GetThemeDocumentationProperty(LPCWSTR, LPCWSTR, LPWSTR, int);
HRESULT WINAPI GetThemeEnumValue(HTHEME, int, int, int, int *);
HRESULT WINAPI GetThemeFilename(HTHEME, int, int, int, LPWSTR, int);
HRESULT WINAPI GetThemeFont(HTHEME, HDC, int, int, int, LOGFONTW *);
HRESULT WINAPI GetThemeInt(HTHEME, int, int, int, int *);

#define MAX_INTLIST_COUNT  10

struct _INTLIST
{
   int iValueCount;
   int iValues[MAX_INTLIST_COUNT];
};

using INTLIST = _INTLIST;
using PINTLIST = _INTLIST *;

HRESULT WINAPI GetThemeIntList(HTHEME, int, int, int, INTLIST *);

struct _MARGINS
{
   int cxLeftWidth;
   int cxRightWidth;
   int cyTopHeight;
   int cyBottomHeight;
};

using MARGINS = _MARGINS;
using PMARGINS = _MARGINS *;

HRESULT WINAPI GetThemeMargins(HTHEME, HDC, int, int, int, RECT *, MARGINS *);
HRESULT WINAPI GetThemeMetric(HTHEME, HDC, int, int, int, int *);

enum THEMESIZE
{
   TS_MIN,
   TS_TRUE,
   TS_DRAW
};

HRESULT WINAPI GetThemePartSize(HTHEME, HDC, int, int, RECT *, THEMESIZE, SIZE *);
HRESULT WINAPI GetThemePosition(HTHEME, int, int, int, POINT *);

enum PROPERTYORIGIN
{
   PO_STATE,
   PO_PART,
   PO_CLASS,
   PO_GLOBAL,
   PO_NOTFOUND
};

HRESULT WINAPI GetThemePropertyOrigin(HTHEME, int, int, int, PROPERTYORIGIN *);
HRESULT WINAPI GetThemeRect(HTHEME, int, int, int, RECT *);
HRESULT WINAPI GetThemeString(HTHEME, int, int, int, LPWSTR, int);
BOOL WINAPI GetThemeSysBool(HTHEME, int);
COLORREF WINAPI GetThemeSysColor(HTHEME, int);
HBRUSH WINAPI GetThemeSysColorBrush(HTHEME, int);
HRESULT WINAPI GetThemeSysFont(HTHEME, int, LOGFONTW *);
HRESULT WINAPI GetThemeSysInt(HTHEME, int, int *);
int WINAPI GetThemeSysSize(HTHEME, int);
HRESULT WINAPI GetThemeSysString(HTHEME, int, LPWSTR, int);
HRESULT WINAPI GetThemeTextExtent(HTHEME, HDC, int, int, LPCWSTR, int, DWORD, const RECT *, RECT *);
HRESULT WINAPI GetThemeTextMetrics(HTHEME, HDC, int, int, TEXTMETRICW *);
HTHEME WINAPI GetWindowTheme(HWND);

#define HTTB_BACKGROUNDSEG          0x0000
#define HTTB_FIXEDBORDER            0x0002
#define HTTB_CAPTION                0x0004
#define HTTB_RESIZINGBORDER_LEFT    0x0010
#define HTTB_RESIZINGBORDER_TOP     0x0020
#define HTTB_RESIZINGBORDER_RIGHT   0x0040
#define HTTB_RESIZINGBORDER_BOTTOM  0x0080
#define HTTB_RESIZINGBORDER         (HTTB_RESIZINGBORDER_LEFT | HTTB_RESIZINGBORDER_TOP | HTTB_RESIZINGBORDER_RIGHT | HTTB_RESIZINGBORDER_BOTTOM)
#define HTTB_SIZINGTEMPLATE         0x0100
#define HTTB_SYSTEMSIZINGMARGINS    0x0200

HRESULT WINAPI HitTestThemeBackground(HTHEME, HDC, int, int, DWORD, const RECT *, HRGN, POINT, WORD *);
BOOL WINAPI IsAppThemed(void);
BOOL WINAPI IsThemeActive(void);
BOOL WINAPI IsThemeBackgroundPartiallyTransparent(HTHEME, int, int);
BOOL WINAPI IsThemeDialogTextureEnabled(void);
BOOL WINAPI IsThemePartDefined(HTHEME, int, int);
HTHEME WINAPI OpenThemeData(HWND, LPCWSTR);
void WINAPI SetThemeAppProperties(DWORD);
HRESULT WINAPI SetWindowTheme(HWND, LPCWSTR, LPCWSTR);

#endif /* __WINE_UXTHEME_H */

bool hmg_ArrayToRect(PHB_ITEM aRect, RECT * rc);
bool hmg_ArrayToPoint(PHB_ITEM aPoint, POINT * pt);
bool hmg_ArrayToColorRef(PHB_ITEM aCRef, COLORREF * cr);

using fnOpenThemeData = HTHEME (WINAPI *)(HWND hwnd, LPCWSTR pszClassList);
using fnCloseThemeData = HRESULT (WINAPI *)(HTHEME hTheme);
using fnDrawThemeBackground = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pRect, const RECT * pClipRect);
using fnGetThemeBackgroundContentRect = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pBoundingRect, RECT * pContentRect);
using fnDrawThemeText = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, LPCWSTR pszText, int iCharCount, DWORD dwTextFlags, DWORD dwTextFlags2, const RECT * pRect);
using fnHitTestThemeBackground = HRESULT (WINAPI *)(HTHEME hTheme, OPTIONAL HDC hdc, int iPartId, int iStateId, DWORD dwOptions, const RECT * pRect, OPTIONAL HRGN hrgn, POINT ptTest, OUT WORD * pwHitTestCode);
using fnIsAppThemed = BOOL (WINAPI *)(void);
using fnGetThemeSysColor = COLORREF (WINAPI *)(HTHEME hTheme, int iColorId);
using fnGetThemeSysFont = HRESULT (WINAPI *)(HTHEME hTheme, int iFontId, OUT LOGFONT * plf);
using fnDrawThemeIcon = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pRect, HIMAGELIST himl, int iImageIndex);
using fnGetThemeTextExtent = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, LPCWSTR pszText, int iCharCount, DWORD dwTextFlags, const RECT * pRect, OUT RECT * pExtent);
using fnDrawThemeParentBackground = HRESULT (WINAPI *)(HWND hwnd, HDC hdc, OPTIONAL RECT * prc);
using fnDrawThemeEdge = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT * pDestRect, UINT uEdge, UINT uFlags, OPTIONAL OUT RECT * pContentRect);
using fnGetThemeRect = HRESULT (WINAPI *)(HTHEME hTheme, int iPartId, int iStateId, int iPropId, RECT * pPoint);
using fnGetThemePartSize = HRESULT (WINAPI *)(HTHEME hTheme, HDC hdc, int iPartId, int iStateId, RECT * prc, THEMESIZE eSize, SIZE * psz);
using fnSetThemeAppProperties = void (WINAPI *)(DWORD dwFlags);
using fnGetThemeAppProperties = DWORD (WINAPI *)(void);
using fnGetWindowTheme = HTHEME (WINAPI *)(HWND hWnd);
using fnIsThemeActive = BOOL (WINAPI *)(void);
using fnSetWindowTheme = HRESULT (WINAPI *)(HWND hwnd, LPCWSTR pszSubAppName, LPCWSTR pszSubIdList);
using fnEnableThemeDialogTexture = HRESULT (WINAPI *)(HWND hwnd, DWORD dwFlags);
using fnGetThemeColor = HRESULT (WINAPI *)(HTHEME hTheme, int iPartId, int iStateId, int iPropId, COLORREF * pColor);

static HINSTANCE hUxTheme;

HINSTANCE InitUxTheme(void)
{
   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   return hUxTheme;
}

void EndUxTheme(void)
{
   if( hUxTheme != nullptr ) {
      FreeLibrary(hUxTheme);
      hUxTheme = nullptr;
   }
}

/*
HMG_INITUXTHEME() --> HANDLE
*/
HB_FUNC( HMG_INITUXTHEME )
{
   hmg_ret_HANDLE(InitUxTheme());
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( INITUXTHEME, HMG_INITUXTHEME )
#endif

/*
HMG_ENDUXTHEME() --> NIL
*/
HB_FUNC( HMG_ENDUXTHEME )
{
   EndUxTheme();
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( ENDUXTHEME, HMG_ENDUXTHEME )
#endif

/*
HMG_ISTHEMEACTIVE() --> .T.|.F.
*/
HB_FUNC( HMG_ISTHEMEACTIVE )
{
   BOOL bRet = FALSE;

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnIsThemeActive pfn = reinterpret_cast<fnIsThemeActive>(wapi_GetProcAddress(hUxTheme, "IsThemeActive"));
      if( pfn ) {
         bRet = static_cast<BOOL>(pfn());
      }
   }

   hb_retl(bRet);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( ISTHEMEACTIVE, HMG_ISTHEMEACTIVE )
#endif

/*
HMG_ISAPPTHEMED() --> .T.|.F.
*/
HB_FUNC( HMG_ISAPPTHEMED )
{
   BOOL bRet = FALSE;

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnIsAppThemed pfn = reinterpret_cast<fnIsAppThemed>(wapi_GetProcAddress(hUxTheme, "IsAppThemed"));
      if( pfn ) {
         bRet = static_cast<BOOL>(pfn());
      }
   }

   hb_retl(bRet);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( ISAPPTHEMED, HMG_ISAPPTHEMED )
#endif

/*
HMG_OPENTHEMEDATA(HWND, cp2) --> HANDLE
*/
HB_FUNC( HMG_OPENTHEMEDATA )
{
   HTHEME nRet = nullptr;
   auto hWnd = hmg_par_HWND(1);
   void * str = nullptr;

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnOpenThemeData pfn = reinterpret_cast<fnOpenThemeData>(wapi_GetProcAddress(hUxTheme, "OpenThemeData"));
      if( pfn ) {
         nRet = static_cast<HTHEME>(pfn(hWnd, (LPCWSTR) HB_PARSTR(2, &str, nullptr)));
      }
   }

   if( nRet != nullptr ) {
      hmg_ret_HANDLE(nRet);
   }

   hb_strfree(str);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( OPENTHEMEDATA, HMG_OPENTHEMEDATA )
#endif

/*
HMG_CLOSETHEMEDATA(HTHEME) --> HRESULT
*/
HB_FUNC( HMG_CLOSETHEMEDATA )
{
   HRESULT nRet = S_FALSE;

   HTHEME hTheme = ( HTHEME ) HB_PARNL(1);

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnCloseThemeData pfn = reinterpret_cast<fnCloseThemeData>(wapi_GetProcAddress(hUxTheme, "CloseThemeData"));
      if( pfn ) {
         nRet = static_cast<HRESULT>(pfn(hTheme));
      }
   }

   if( nRet != reinterpret_cast<LONG_PTR>(nullptr) ) {
      HB_RETNL(static_cast<LONG_PTR>(nRet));
   }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( CLOSETHEMEDATA, HMG_CLOSETHEMEDATA )
#endif

/*
HMG_DRAWTHEMEBACKGROUND(HTHEME, HDC, np3, np4, aRect, aClipRect) --> .T.|.F.
*/
HB_FUNC( HMG_DRAWTHEMEBACKGROUND )
{
   HRESULT nRet = S_FALSE;

   HTHEME hTheme   = ( HTHEME ) HB_PARNL(1);
   auto hDC = hmg_par_HDC(2);
   auto iPartId = hb_parni(3);
   auto iStateId = hb_parni(4);

   RECT pRect;
   RECT pClipRect;

   hmg_ArrayToRect(hb_param(5, Harbour::Item::ARRAY), &pRect);
   hmg_ArrayToRect(hb_param(6, Harbour::Item::ARRAY), &pClipRect);

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnDrawThemeBackground pfn = reinterpret_cast<fnDrawThemeBackground>(wapi_GetProcAddress(hUxTheme, "DrawThemeBackground"));
      if( pfn ) {
         nRet = static_cast<HRESULT>(pfn(hTheme, hDC, iPartId, iStateId, &pRect, &pClipRect));
      }
   }

   hb_retl((nRet == S_OK));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( DRAWTHEMEBACKGROUND, HMG_DRAWTHEMEBACKGROUND )
#endif

/*
HMG_DRAWTHEMEPARENTBACKGROUND(HWND, HDC, aRect) --> .T.|.F.
*/
HB_FUNC( HMG_DRAWTHEMEPARENTBACKGROUND )
{
   HRESULT nRet = S_FALSE;

   auto hWnd = hmg_par_HWND(1);
   auto hDC = hmg_par_HDC(2);
   RECT pRect;

   if( HB_ISARRAY(7) ) {
      hmg_ArrayToRect(hb_param(3, Harbour::Item::ARRAY), &pRect);
   }

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnDrawThemeParentBackground pfn = reinterpret_cast<fnDrawThemeParentBackground>(wapi_GetProcAddress(hUxTheme, "DrawThemeParentBackground"));
      if( pfn ) {
         nRet = static_cast<HRESULT>(pfn(hWnd, hDC, &pRect));
      }
   }

   hb_retl((nRet == S_OK));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( DRAWTHEMEPARENTBACKGROUND, HMG_DRAWTHEMEPARENTBACKGROUND )
#endif

/*
HMG_SETWINDOWTHEME(HWND, cp2, cp3) --> .T.|.F.
*/
HB_FUNC( HMG_SETWINDOWTHEME )
{
   HRESULT nRet = S_FALSE;

   auto hWnd = hmg_par_HWND(1);
   void * str1 = nullptr;
   void * str2 = nullptr;

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnSetWindowTheme pfn = reinterpret_cast<fnSetWindowTheme>(wapi_GetProcAddress(hUxTheme, "SetWindowTheme"));
      if( pfn ) {
         nRet = static_cast<HRESULT>(pfn(hWnd, (LPCWSTR) HB_PARSTR(2, &str1, nullptr), (LPCWSTR) HB_PARSTR(3, &str2, nullptr)));
      }
   }

   hb_retl((nRet == S_OK));

   hb_strfree(str1);
   hb_strfree(str2);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( SETWINDOWTHEME, HMG_SETWINDOWTHEME )
#endif

/*
HMG_ENABLETHEMEDIALOGTEXTURE(HWND, nFlags) --> .T.|.F.
*/
HB_FUNC( HMG_ENABLETHEMEDIALOGTEXTURE )
{
   HRESULT nRet = S_FALSE;

   auto hWnd = hmg_par_HWND(1);
   DWORD flags = hb_parnl(2);

   if( hUxTheme == nullptr ) {
      hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
   }

   if( hUxTheme ) {
      fnEnableThemeDialogTexture pfn = reinterpret_cast<fnEnableThemeDialogTexture>(wapi_GetProcAddress(hUxTheme, "EnableThemeDialogTexture"));
      if( pfn ) {
         nRet = static_cast<HRESULT>(pfn(hWnd, flags));
      }
   }

   hb_retl((nRet == S_OK));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( ENABLETHEMEDIALOGTEXTURE, HMG_ENABLETHEMEDIALOGTEXTURE )
#endif

/*
HMG_PTINRECT(aPoint, aRect) --> .T.|.F.
*/
HB_FUNC( HMG_PTINRECT )
{
   POINT point;
   RECT rect;

   if( (hmg_ArrayToPoint(hb_param(1, Harbour::Item::ANY), &point) && hmg_ArrayToRect(hb_param(2, Harbour::Item::ANY), &rect)) ) {
      hb_retl(PtInRect(&rect, point) ? true : false);
   } else {
     hb_retl(false);
   }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE( PTINRECT, HMG_PTINRECT )
#endif

bool hmg_ArrayToRect(PHB_ITEM aRect, RECT * rc)
{
   if( HB_IS_ARRAY(aRect) && hb_arrayLen(aRect) == 4 ) {
      rc->left   = hb_arrayGetNI(aRect, 1);
      rc->top    = hb_arrayGetNI(aRect, 2);
      rc->right  = hb_arrayGetNI(aRect, 3);
      rc->bottom = hb_arrayGetNI(aRect, 4);
      return true;
   }

   return false;
}

bool hmg_ArrayToPoint(PHB_ITEM aPoint, POINT * pt)
{
   if( HB_IS_ARRAY(aPoint) && hb_arrayLen(aPoint) == 2 ) {
      pt->x = hb_arrayGetNI(aPoint, 1);
      pt->y = hb_arrayGetNI(aPoint, 2);
      return true;
   }

   return false;
}

bool hmg_ArrayToColorRef(PHB_ITEM aCRef, COLORREF * cr)
{
   if( HB_IS_ARRAY(aCRef) && hb_arrayLen(aCRef) == 3 ) {
      auto r = static_cast<USHORT>(HB_arrayGetNL(aCRef, 1));
      auto g = static_cast<USHORT>(HB_arrayGetNL(aCRef, 2));
      auto b = static_cast<USHORT>(HB_arrayGetNL(aCRef, 3));
      *cr = RGB(r, g, b);
      return true;
   }

   return false;
}

PHB_ITEM hmg_RectToArray(RECT * rc)
{
   auto aRect = hb_itemArrayNew(4);

   HB_arraySetNL(aRect, 1, rc->left);
   HB_arraySetNL(aRect, 2, rc->top);
   HB_arraySetNL(aRect, 3, rc->right);
   HB_arraySetNL(aRect, 4, rc->bottom);

   return aRect;
}

PHB_ITEM hmg_PointToArray(POINT * pt)
{
   auto aPoint = hb_itemArrayNew(2);

   HB_arraySetNL(aPoint, 1, pt->x);
   HB_arraySetNL(aPoint, 2, pt->y);

   return aPoint;
}
