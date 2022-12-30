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
 * Parts of this code is contributed and used here under permission of his author:
 * Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 */

#include "mgdefs.h"
#include <shellapi.h>
#include <commctrl.h>
#include <math.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbwinuni.h>

#ifndef BCM_FIRST
#define BCM_FIRST         0x1600
#define BCM_SETIMAGELIST  (BCM_FIRST + 0x0002)
#endif

static HBRUSH CreateGradientBrush(HDC hDC, INT nWidth, INT nHeight, COLORREF Color1, COLORREF Color2);
HBITMAP HMG_LoadPicture(const char * FileName, int New_Width, int New_Height, HWND hWnd, int ScaleStretch, int Transparent, long BackgroundColor, int AdjustImage, HB_BOOL bAlphaFormat, int iAlpfaConstant);
HIMAGELIST HMG_SetButtonImageList(HWND hButton, const char * FileName, int Transparent, UINT uAlign);
BOOL bmp_SaveFile(HBITMAP hBitmap, TCHAR * FileName);
LRESULT CALLBACK  OwnButtonProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

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
INITBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> HWND
*/
HB_FUNC( INITBUTTON )
{
   void * WindowName;

   DWORD style = BS_NOTIFY | WS_CHILD | (hb_parl(14) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON); // JK

   if( hb_parl(10) )
   {
      style |= BS_FLAT;
   }

   if( !hb_parl(11) )
   {
      style |= WS_TABSTOP;
   }

   if( !hb_parl(12) )
   {
      style |= WS_VISIBLE;
   }

   if( hb_parl(13) )
   {
      style |= BS_MULTILINE;
   }

   hmg_ret_HWND(CreateWindowEx(0, WC_BUTTON, HB_PARSTR(2, &WindowName, nullptr), style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(6), hmg_par_int(7),
      hmg_par_HWND(1), hmg_par_HMENU(3), GetInstance(), nullptr));

   hb_strfree(WindowName);
}

/*
INITIMAGEBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> array
*/
HB_FUNC( INITIMAGEBUTTON )
{
   HWND himage;
   HICON hIcon;
   int Transparent = hb_parl(10) ? 0 : 1;
   HIMAGELIST himl;
   BUTTON_IMAGELIST bi;

   void * WindowName;
   void * IconName;

   LPCTSTR lpIconName = HB_PARSTR(14, &IconName, nullptr);

   HWND hwnd = hmg_par_HWND(1);

   DWORD style = BS_NOTIFY | WS_CHILD | (hb_parl(13) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON) | (hb_parc(14) == nullptr ? BS_BITMAP : BS_ICON); // JK

   if( hb_parl(9) )
   {
      style |= BS_FLAT;
   }

   if( !hb_parl(11) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(12) )
   {
      style |= WS_TABSTOP;
   }

   HWND hbutton = CreateWindowEx(0, WC_BUTTON, HB_PARSTR(2, &WindowName, nullptr), style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(6), hmg_par_int(7),
      hwnd, hmg_par_HMENU(3), GetInstance(), nullptr);

   if( HB_ISNIL(14) )
   {
      if( !hb_parl(17) )
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
   }
   else
   {
      if( !hb_parl(15) )
      {
         hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

         if( hIcon == nullptr )
         {
            hIcon = static_cast<HICON>(LoadImage(0, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
         }
      }
      else
      {
         hIcon = static_cast<HICON>(ExtractIcon(GetInstance(), lpIconName, hb_parni(16)));

         if( hIcon == nullptr )
         {
            hIcon = static_cast<HICON>(ExtractIcon(GetInstance(), TEXT("user.exe"), 0));
         }
      }

      if( hb_parl(17) )
      {
         ICONINFO sIconInfo;
         GetIconInfo(hIcon, &sIconInfo);
         BITMAP bm;
         GetObject(sIconInfo.hbmColor, sizeof(BITMAP), static_cast<LPVOID>(&bm));

         himl = ImageList_Create(bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0);

         bi.himl          = himl;
         bi.margin.left   = 10;
         bi.margin.top    = 10;
         bi.margin.bottom = 10;
         bi.margin.right  = 10;
         bi.uAlign        = 4;

         ImageList_AddIcon(bi.himl, hIcon);

         SendMessage(hbutton, BCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(&bi));

         DeleteObject(sIconInfo.hbmMask);
         DeleteObject(sIconInfo.hbmColor);
         DestroyIcon(hIcon);

         hb_reta(2);
         HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
         HB_STORVNL(reinterpret_cast<LONG_PTR>(himl), -1, 2);
      }
      else
      {
         SendMessage(hbutton, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_ICON), reinterpret_cast<LPARAM>(hIcon));

         hb_reta(2);
         HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
         HB_STORVNL(reinterpret_cast<LONG_PTR>(hIcon), -1, 2);
      }
   }

   hb_strfree(WindowName);
   hb_strfree(IconName);
}

/*
INITOWNERBUTTON(p1, p2, p3, nX, nY, nWidth, nHeight, p8, p9, p10, p11, p12, p13, p14) --> array
*/
HB_FUNC( INITOWNERBUTTON )
{
   HWND  himage;
   HICON hIcon;

   void * WindowName;
   void * ImageName;
   void * IconName;

   LPCTSTR lpWindowName = HB_PARSTR(2, &WindowName, nullptr);
   LPCTSTR lpImageName  = HB_PARSTR(8, &ImageName, nullptr);
   LPCTSTR lpIconName   = HB_PARSTR(14, &IconName, nullptr);

   HWND hwnd = hmg_par_HWND(1);

   DWORD style = BS_NOTIFY | WS_CHILD | BS_OWNERDRAW | (HB_ISNIL(14) ? BS_BITMAP : BS_ICON) | (hb_parl(13) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON);

   if( hb_parl(9) )
   {
      style |= BS_FLAT;
   }

   if( !hb_parl(11) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(12) )
   {
      style |= WS_TABSTOP;
   }

   HWND hbutton = CreateWindowEx(0, WC_BUTTON, lpWindowName, style,
      hmg_par_int(4), hmg_par_int(5), hmg_par_int(6), hmg_par_int(7),
      hwnd, hmg_par_HMENU(3), GetInstance(), nullptr);

   SetProp(hbutton, TEXT("oldbtnproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hbutton, GWLP_WNDPROC)));
   SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnButtonProc));

   int ImgStyle = hb_parl(10) ? 0 : LR_LOADTRANSPARENT;

   if( HB_ISNIL(14) )
   {
      himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(15, 0), 0), HB_MAX(hb_parnidef(16, 0), 0), LR_LOADMAP3DCOLORS | ImgStyle));

      if( himage == nullptr )
      {
         himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(15, 0), 0), HB_MAX(hb_parnidef(16, 0), 0), LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | ImgStyle));
      }

      hb_reta(2);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(himage), -1, 2);
   }
   else
   {
      hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

      if( hIcon == nullptr )
      {
         hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
      }

      if( hIcon == nullptr )
      {
         hIcon = ExtractIcon(GetInstance(), lpIconName, 0);
      }

      hb_reta(2);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(hbutton), -1, 1);
      HB_STORVNL(reinterpret_cast<LONG_PTR>(hIcon), -1, 2);
   }

   hb_strfree(WindowName);
   hb_strfree(ImageName);
   hb_strfree(IconName);
}

/*
_SETBTNPICTURE(p1, p2, p3, p4) --> HWND
*/
HB_FUNC( _SETBTNPICTURE )
{
   void * ImageName;
   LPCTSTR lpImageName = HB_PARSTR(2, &ImageName, nullptr);

   HWND hwnd = hmg_par_HWND(1);

   HWND himage = static_cast<HWND>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(3, 0), 0), HB_MAX(hb_parnidef(4, 0), 0), LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

   if( himage == nullptr )
   {
      himage = static_cast<HWND>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, HB_MAX(hb_parnidef(3, 0), 0), HB_MAX(hb_parnidef(4, 0), 0), LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
   }

   if( himage == nullptr )
   {
      himage = reinterpret_cast<HWND>(HMG_LoadPicture(hb_parc(2), hb_parni(3), hb_parni(4), hwnd, 0, 1, -1, 0, HB_FALSE, 255)); // TODO: hb_parc(2) -> lpImageName
   }

   SendMessage(hwnd, BM_SETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), reinterpret_cast<LPARAM>(himage));

   RegisterResource(himage, "BMP");
   hmg_ret_HWND(himage);

   hb_strfree(ImageName);
}

/*
_GETBTNPICTUREHANDLE(HWND) --> HWND
*/
HB_FUNC( _GETBTNPICTUREHANDLE )
{
   hmg_ret_HWND(reinterpret_cast<HWND>(SendMessage(hmg_par_HWND(1), BM_GETIMAGE, static_cast<WPARAM>(IMAGE_BITMAP), 0)));
}

/*
_SETMIXEDBTNPICTURE(p1, p2, p3) --> HANDLE
*/
HB_FUNC( _SETMIXEDBTNPICTURE )
{
   int Transparent = hb_parl(3) ? 0 : 1;
   HIMAGELIST himl = HMG_SetButtonImageList(hmg_par_HWND(1), hb_parc(2), Transparent, BUTTON_IMAGELIST_ALIGN_CENTER);
   RegisterResource(himl, "IMAGELIST");
   hmg_ret_HANDLE(himl);
}

// HMG 1.0 Experimental Build 8e

/*
_SETBTNICON(p1, p2) --> HANDLE
*/
HB_FUNC( _SETBTNICON )
{
   void * IconName;
   LPCTSTR lpIconName = HB_PARSTR(2, &IconName, nullptr);

   HICON hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

   if( hIcon == nullptr )
   {
      hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
   }

   SendMessage(hmg_par_HWND(1), BM_SETIMAGE, static_cast<WPARAM>(IMAGE_ICON), reinterpret_cast<LPARAM>(hIcon));

   RegisterResource(hIcon, "ICON");
   hmg_ret_HANDLE(hIcon);

   hb_strfree(IconName);
}

/*
_SETMIXEDBTNICON(p1, p2) --> HANDLE
*/
HB_FUNC( _SETMIXEDBTNICON )
{
   void * IconName;
   LPCTSTR lpIconName = HB_PARSTR(2, &IconName, nullptr);

   HICON hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR));

   if( hIcon == nullptr )
   {
      hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
   }

   ICONINFO sIconInfo;
   GetIconInfo(hIcon, &sIconInfo);
   BITMAP bm;
   GetObject(sIconInfo.hbmColor, sizeof(BITMAP), static_cast<LPVOID>(&bm));

   HIMAGELIST himl = ImageList_Create(bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0);

   BUTTON_IMAGELIST bi;
   bi.himl          = himl;
   bi.margin.left   = 10;
   bi.margin.top    = 10;
   bi.margin.bottom = 10;
   bi.margin.right  = 10;
   bi.uAlign        = 4;

   ImageList_AddIcon(bi.himl, hIcon);

   SendMessage(hmg_par_HWND(1), BCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(&bi));

   DeleteObject(sIconInfo.hbmMask);
   DeleteObject(sIconInfo.hbmColor);
   DestroyIcon(hIcon);

   RegisterResource(himl, "IMAGELIST");
   hmg_ret_HANDLE(himl);

   hb_strfree(IconName);
}

/*
DRAWBUTTON(p1, p2, p3, p4, p5, p6) --> NIL
*/
HB_FUNC( DRAWBUTTON )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(4));

   UINT iFocus     = hb_parni(2);
   UINT iState     = hb_parni(3);
   UINT iMouseOver = hb_parni(5);
   UINT iFlat      = hb_parni(6);

   if( iFocus == 1 || iMouseOver == 1 )
   {
      InflateRect(&pps->rcItem, -1, -1);
   }

   DrawFrameControl(pps->hDC, &pps->rcItem, DFC_BUTTON, (!iFlat) ? iState : (iState | DFCS_FLAT));

   if( iFocus == 1 )
   {
      HPEN   OldPen   = static_cast<HPEN>(SelectObject(pps->hDC, GetStockObject(BLACK_PEN)));
      HBRUSH OldBrush = static_cast<HBRUSH>(SelectObject(pps->hDC, GetStockObject(NULL_BRUSH)));

      InflateRect(&pps->rcItem, 1, 1);
      Rectangle(pps->hDC, pps->rcItem.left, pps->rcItem.top, pps->rcItem.right, pps->rcItem.bottom);

      SelectObject(pps->hDC, OldBrush);
      SelectObject(pps->hDC, OldPen);
   }
}

/*
   Function GETOWNBTNHANDLE return value of hwndItem DRAWITEMSTRUCT member
 */

/*
GETOWNBTNHANDLE(p1) --> HANDLE
*/
HB_FUNC( GETOWNBTNHANDLE )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hmg_ret_HANDLE(pps->hwndItem);
   }
}

/*
   Function GETOWNBTNSTATE return value of itemState DRAWITEMSTRUCT member
 */

/*
GETOWNBTNSTATE(p1) --> numeric
*/
HB_FUNC( GETOWNBTNSTATE )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hb_retnl(pps->itemState);
   }
}

/*
   Function GETOWNBTNDC return value of hDC DRAWITEMSTRUCT member
 */

/*
GETOWNBTNDC(p1) --> HANDLE
*/
HB_FUNC( GETOWNBTNDC )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hmg_ret_HANDLE(pps->hDC);
   }
}

/*
   Function GETOWNBTNITEMACTION return value of itemID DRAWITEMSTRUCT member
 */

/*
GETOWNBTNITEMID(p1) --> numeric
*/
HB_FUNC( GETOWNBTNITEMID )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hb_retnl(pps->itemID);
   }
}

/*
   Function GETOWNBTNITEMACTION return value of itemAction DRAWITEMSTRUCT member
 */

/*
GETOWNBTNITEMACTION(p1) --> numeric
*/
HB_FUNC( GETOWNBTNITEMACTION )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hb_retnl(pps->itemAction);
   }
}

/*
   Function GETOWNBTNCTLTYPE return value of CtlType DRAWITEMSTRUCT member
 */

/*
GETOWNBTNCTLTYPE(p1) --> numeric
*/
HB_FUNC( GETOWNBTNCTLTYPE )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));

   if( pps )
   {
      hb_retni(pps->CtlType);
   }
}

/*
   Function GETOWNBTNRECT return array with button rectangle coords
 */

/*
GETOWNBTNRECT(p1) --> array
*/
HB_FUNC( GETOWNBTNRECT )
{
   DRAWITEMSTRUCT * pps = reinterpret_cast<DRAWITEMSTRUCT*>(HB_PARNL(1));
   RECT rc = pps->rcItem;
   PHB_ITEM aMetr = hb_itemArrayNew(4);
   HB_arraySetNL(aMetr, 1, rc.left);
   HB_arraySetNL(aMetr, 2, rc.top);
   HB_arraySetNL(aMetr, 3, rc.right);
   HB_arraySetNL(aMetr, 4, rc.bottom);
   hb_itemReturnRelease(aMetr);
}

LRESULT CALLBACK OwnButtonProc(HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   WNDPROC OldWndProc = reinterpret_cast<WNDPROC>(GetProp(hButton, TEXT("oldbtnproc")));

   switch( Msg )
   {
      case WM_LBUTTONDBLCLK:
         SendMessage(hButton, WM_LBUTTONDOWN, wParam, lParam);
         break;

      case WM_MOUSEMOVE:
      {
         TRACKMOUSEEVENT tme;
         tme.cbSize      = sizeof(TRACKMOUSEEVENT);
         tme.dwFlags     = TME_LEAVE;
         tme.hwndTrack   = hButton;
         tme.dwHoverTime = 0;
         _TrackMouseEvent(&tme);

         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OBTNEVENTS"));
         }

         if( pSymbol )
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hb_vmPushNumInt(reinterpret_cast<LONG_PTR>(hButton));
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hButton, Msg, wParam, lParam);
      }
      case WM_MOUSELEAVE:
      {
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OBTNEVENTS"));
         }

         if( pSymbol )
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hb_vmPushNumInt(reinterpret_cast<LONG_PTR>(hButton));
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : DefWindowProc(hButton, Msg, wParam, lParam);
      }
   }

   return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
}

/*
 * Added in Build 16.12
 */

/*
CREATEBUTTONBRUSH(p1, p2, p3, p4, p5) --> HANDLE
*/
HB_FUNC( CREATEBUTTONBRUSH )
{
   hmg_ret_HANDLE(CreateGradientBrush(hmg_par_HDC(1), hb_parni(2), hb_parni(3), hmg_par_COLORREF(4), hmg_par_COLORREF(5)));
}

static HBRUSH CreateGradientBrush(HDC hDC, INT nWidth, INT nHeight, COLORREF Color1, COLORREF Color2)
{
   int r1 = GetRValue(Color1);
   int g1 = GetGValue(Color1);
   int b1 = GetBValue(Color1);
   int r2 = GetRValue(Color2);
   int g2 = GetGValue(Color2);
   int b2 = GetBValue(Color2);

   HDC hDCComp = CreateCompatibleDC(hDC);
   HBITMAP hBitmap = CreateCompatibleBitmap(hDC, nWidth, nHeight);
   SelectObject(hDCComp, hBitmap);

   RECT rcF;
   rcF.left   = 0;
   rcF.top    = 0;
   rcF.right  = nWidth;
   rcF.bottom = nHeight;

   int nCount = static_cast<int>(ceil(((nWidth > nHeight) ? nHeight : nWidth) / 2));

   HBRUSH hBrush;
   HBRUSH hBrushOld;

   for( int i = 0; i < nCount; i++ )
   {
      hBrush = CreateSolidBrush(RGB(r1 + (i * (r2 - r1) / nCount), g1 + (i * (g2 - g1) / nCount), b1 + (i * (b2 - b1) / nCount)));
      hBrushOld = reinterpret_cast<HBRUSH>(SelectObject(hDCComp, hBrush));
      FillRect(hDCComp, &rcF, hBrush);
      SelectObject(hDCComp, hBrushOld);
      DeleteObject(hBrush);
      InflateRect(&rcF, -1, -1);
   }

   HBRUSH hBrushPat = CreatePatternBrush(hBitmap);

   DeleteDC(hDCComp);
   DeleteObject(hBitmap);

   return hBrushPat;
}

HIMAGELIST HMG_SetButtonImageList(HWND hButton, const char * FileName, int Transparent, UINT uAlign)
{
   HBITMAP hBitmap = HMG_LoadPicture(FileName, -1, -1, nullptr, 0, 0, -1, 0, HB_FALSE, 255);
   if( hBitmap == nullptr )
   {
      return nullptr;
   }

   BITMAP Bmp;
   GetObject(hBitmap, sizeof(BITMAP), &Bmp);

   TCHAR TempPathFileName[MAX_PATH];
   GetTempPath(MAX_PATH, TempPathFileName);
   lstrcat(TempPathFileName, TEXT("_MG_temp.BMP"));
   bmp_SaveFile(hBitmap, TempPathFileName);
   DeleteObject(hBitmap);

   HIMAGELIST hImageList;

   if( Transparent == 1 )
   {
      hImageList = ImageList_LoadImage(GetResources(), TempPathFileName, Bmp.bmWidth, 6, CLR_DEFAULT, IMAGE_BITMAP, LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT);
   }
   else
   {
      hImageList = ImageList_LoadImage(GetResources(), TempPathFileName, Bmp.bmWidth, 6, CLR_NONE, IMAGE_BITMAP, LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS);
   }

   DeleteFile(TempPathFileName);

   BUTTON_IMAGELIST bi;
   bi.himl          = hImageList;
   bi.margin.left   = 10;
   bi.margin.top    = 10;
   bi.margin.bottom = 10;
   bi.margin.right  = 10;
   bi.uAlign        = uAlign;

   SendMessage(hButton, BCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(&bi));

   return hImageList;
}
