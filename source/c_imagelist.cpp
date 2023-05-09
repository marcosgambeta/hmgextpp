/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * IMAGELIST control source code
 * (C)2005 Janusz Pora <januszpora@onet.eu>
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
#include <commctrl.h>
#include <hbwinuni.hpp>

#if defined(__BORLANDC__)
WINCOMMCTRLAPI void WINAPI ImageList_EndDrag(void);
#endif

/*
INITIMAGELIST(cx, cy, flags, initial) --> HIMAGELIST
*/
HB_FUNC( INITIMAGELIST ) // InitImageList(cx, cy, mask, nCount)
{
   UINT style = ILC_COLOR32;

   if( hb_parl(3) )
   {
      style |= ILC_MASK;
   }

   InitCommonControls();
   HIMAGELIST himlIcons = ImageList_Create(hmg_par_int(1), hmg_par_int(2), style, hmg_par_int(4), 0);
   RegisterResource(himlIcons, "IMAGELIST");
   hmg_ret_HIMAGELIST(himlIcons);
}

/*
IL_ADD(p1, p2, p3, p4, p5, p6) --> numeric
*/
HB_FUNC( IL_ADD ) // IL_Add(himl, image, maskimage, ix, iy, imagecount)
{
   int ic = 1;
   if( hb_parni(6) )
   {
      ic = hb_parni(6);
   }

   void * strImageName1;
   LPCTSTR lpImageName1  = HB_PARSTR(2, &strImageName1, nullptr);
   HBITMAP himage1 = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName1, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT)); // handle to image
   if( himage1 == nullptr )
   {
      himage1 = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName1, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
   }
   if( himage1 == nullptr )
   {
      himage1 = HMG_LoadImage(hb_parc(2), nullptr);
   }
   hb_strfree(strImageName1);

   void * strImageName2 = nullptr;
   LPCTSTR lpImageName2  = HB_PARSTR(3, &strImageName1, nullptr);
   HBITMAP himage2 = nullptr; // handle to maskimage
   if( hb_parclen(3) )
   {
      himage2 = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      if( himage2 == nullptr )
      {
         himage2 = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }
      if( himage2 == nullptr )
      {
         himage2 = HMG_LoadImage(hb_parc(3), nullptr);
      }
   }
   hb_strfree(strImageName2);

   BITMAP bm;
   int lResult = -1;

   if( GetObject(himage1, sizeof(BITMAP), &bm) != 0 )
   {
      if( (hb_parni(4) * ic == bm.bmWidth) && (hb_parni(5) == bm.bmHeight) )
      {
         lResult = ImageList_Add(hmg_par_HIMAGELIST(1), himage1, himage2);
      }

      DeleteObject(himage1);
      if( himage2 != nullptr )
      {
         DeleteObject(himage2);
      }
   }

   hb_retni(lResult);
}

/*
IL_ADDMASKED(p1, p2, p3, p4, p5, p6) --> numeric
*/
HB_FUNC( IL_ADDMASKED ) // IL_AddMasked(himl, image, color, ix, iy, imagecount)
{
   COLORREF clrBk = CLR_NONE;
   if( hb_parnl(3) )
   {
      clrBk = hmg_par_COLORREF(3);
   }

   int ic = 1;
   if( hb_parni(6) )
   {
      ic = hb_parni(6);
   }

   void * strImageName;
   LPCTSTR lpImageName = HB_PARSTR(2, &strImageName, nullptr);
   HBITMAP himage1 = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT)); // handle to image
   if( himage1 == nullptr )
   {
      himage1 = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
   }
   if( himage1 == nullptr )
   {
      himage1 = HMG_LoadPicture(hb_parc(2), -1, -1, nullptr, 0, 1, -1, 0, false, 255);
   }
   hb_strfree(strImageName);

   BITMAP bm;
   int lResult = -1;

   if( GetObject(himage1, sizeof(BITMAP), &bm) != 0 )
   {
      if( (hb_parni(4) * ic == bm.bmWidth) && (hb_parni(5) == bm.bmHeight) )
      {
         lResult = ImageList_AddMasked(hmg_par_HIMAGELIST(1), himage1, clrBk);
      }

      DeleteObject(himage1);
   }

   hb_retni(lResult);
}

/*
IL_DRAW(HWND, HIMAGELIST, imageIndex, x, y) --> .T.|.F.
*/
HB_FUNC( IL_DRAW ) // BOOL IL_Draw(HWND hwnd, HIMAGELIST himl, int imageindex, cx, cy)
{
   HWND hwnd = hmg_par_HWND(1);

   HDC hdc;
   if( (hdc = GetDC(hwnd)) == nullptr )
   {
      hb_retl(false);
   }

   if( !ImageList_Draw(hmg_par_HIMAGELIST(2), hmg_par_int(3), hdc, hmg_par_int(4), hmg_par_int(5), ILD_TRANSPARENT) )
   {
      hb_retl(false);
   }

   ReleaseDC(hwnd, hdc);

   hb_retl(true);
}

/*
IL_REMOVE(HIMAGELIST, index) --> .T.|.F.
*/
HB_FUNC( IL_REMOVE ) // IL_Remove(hwnd, imageindex)
{
   hmg_ret_BOOL(ImageList_Remove(hmg_par_HIMAGELIST(1), hmg_par_int(2)));
}

/*
IL_SETBKCOLOR(HIMAGELIST, bkColor) --> COLORREF
*/
HB_FUNC( IL_SETBKCOLOR ) // IL_SetBkColor(hwnd, color)
{
   COLORREF clrBk = CLR_NONE;
   if( hb_parnl(2) )
   {
      clrBk = hmg_par_COLORREF(2);
   }

   hmg_ret_COLORREF(ImageList_SetBkColor(hmg_par_HIMAGELIST(1), clrBk));
}

/*
IL_ERASEIMAGE(HWND, p2, p3, p4, p5) --> NIL
*/
HB_FUNC( IL_ERASEIMAGE ) // IL_EraseImage(hwnd, ix, iy, dx, dy)
{
   RECT rcImage;
   SetRect(&rcImage, hmg_par_int(2), hmg_par_int(3), hmg_par_int(4) + hmg_par_int(2), hmg_par_int(5) + hmg_par_int(3));
   InvalidateRect(hmg_par_HWND(1), &rcImage, TRUE);
   UpdateWindow(hmg_par_HWND(1));
}

/*
IL_BEGINDRAG(HWND, HIMAGELIST, p3, p4, p5) --> .T.|.F.
*/
HB_FUNC( IL_BEGINDRAG ) // IL_BeginDrag(hwnd, himl, ImageInx, ix, iy)
{
   int cx;
   int cy;

   if( ImageList_GetIconSize(hmg_par_HIMAGELIST(2), &cx, &cy) )
   {
      RECT rcImage;
      SetRect(&rcImage, hmg_par_int(4) - 2, hmg_par_int(5) - 2, hmg_par_int(4) + cx + 2, hmg_par_int(5) + cy + 2);
      InvalidateRect(hmg_par_HWND(1), &rcImage, TRUE);
      UpdateWindow(hmg_par_HWND(1));
   }

   hmg_ret_BOOL(ImageList_BeginDrag(hmg_par_HIMAGELIST(2), hmg_par_int(3), 0, 0));
}

/*
IL_DRAGMOVE(x, y) --> .T.|.F.
*/
HB_FUNC( IL_DRAGMOVE ) // IL_DragMove(ix, iy)
{
   hmg_ret_BOOL(ImageList_DragMove(hmg_par_int(1), hmg_par_int(2)));
}

/*
IL_DRAGENTER(HWND, x, y) --> .T.|.F.
*/
HB_FUNC( IL_DRAGENTER ) // IL_DragEnter(hwnd, ix, iy)
{
   hmg_ret_BOOL(ImageList_DragEnter(hmg_par_HWND(1), hmg_par_int(2), hmg_par_int(3)));
}

/*
IL_ENDDRAG(HWND) --> NIL
*/
HB_FUNC( IL_ENDDRAG ) // IL_EndDrag(hwnd)
{
   ImageList_EndDrag();
   ImageList_DragLeave(hmg_par_HWND(1));
}

/*
IL_GETIMAGECOUNT(HIMAGELIST) --> numeric
*/
HB_FUNC( IL_GETIMAGECOUNT ) // IL_GetImageCount(himl)
{
   hmg_ret_int(ImageList_GetImageCount(hmg_par_HIMAGELIST(1)));
}
