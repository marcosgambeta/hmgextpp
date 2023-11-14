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
 * author:
 * Copyright 2007-2017 (C) P.Chornyj <myorg63@mail.ru>
 */

#if !defined(__WINNT__)
#define __WINNT__
#endif

#include "mgdefs.hpp"
#include <hbapierr.hpp>
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>

#define MAX_ITEM_TEXT  256

#include "c_menu.h"

// extern functions
#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR  WideToAnsi(LPWSTR);
#endif
HINSTANCE        GetResources(void);
extern HBITMAP   Icon2Bmp(HICON hIcon);
extern BOOL      SetAcceleratorTable(HWND, HACCEL);

/*
SETACCELERATORTABLE(HWND, HACCEL) --> NIL
*/
HB_FUNC( SETACCELERATORTABLE )
{
   auto hWndMain = hmg_par_HWND(1);
   HACCEL hAccel = hmg_par_HACCEL(2);

   if( hWndMain != nullptr && hAccel != nullptr ) {
      SetAcceleratorTable(hWndMain, hAccel);
   }
}

/*
ACCELERATORTABLE2ARRAY(HACCEL) --> array
*/
HB_FUNC( ACCELERATORTABLE2ARRAY )
{
   HACCEL hAccel = hmg_par_HACCEL(1);
   PHB_ITEM aAccels = hb_itemArrayNew(0);

   if( hAccel != nullptr ) {
      int cAccelEntries = CopyAcceleratorTable(hAccel, nullptr, 0);
      if( cAccelEntries > 0 ) {
         LPACCEL lpAccel = static_cast<LPACCEL>(hb_xalloc(cAccelEntries * sizeof(ACCEL)));
         if( lpAccel != nullptr ) {
            if( CopyAcceleratorTable(hAccel, lpAccel, cAccelEntries) ) {
               for( int i = 0; i < cAccelEntries; i++ ) {
                  PHB_ITEM aAccel = hb_itemArrayNew(3);
                  hb_arraySetNI(aAccel, 1, lpAccel[i].fVirt);
                  hb_arraySetNL(aAccel, 2, lpAccel[i].key);
                  hb_arraySetNL(aAccel, 3, lpAccel[i].cmd);
                  hb_arrayAddForward(aAccels, aAccel);
                  hb_itemRelease(aAccel);
               }
               hb_xfree(lpAccel);
            }
         }
      }
   }

   hb_itemReturnRelease(aAccels);
}

/*
ARRAY2ACCELERATORTABLE(array) --> HACCEL
*/
HB_FUNC( ARRAY2ACCELERATORTABLE )
{
   auto pArray = hb_param(1, Harbour::Item::ARRAY);
   int nLen;
   HACCEL hAccel = nullptr;

   if( pArray != nullptr && ((nLen = hb_arrayLen(pArray)) > 0)) {
      LPACCEL lpAccel = static_cast<LPACCEL>(hb_xalloc(nLen * sizeof(ACCEL)));
      if( lpAccel != nullptr ) {
         for( int i = 0; i < nLen; i++ ) {
            if( hb_arrayGetType(pArray, i + 1) & Harbour::Item::ARRAY ) {
               PHB_ITEM pAccel = hb_arrayGetItemPtr(pArray, i + 1);
               if( hb_arrayLen(pAccel) == 3 ) {
                  lpAccel[i].fVirt = static_cast<BYTE>(hb_arrayGetNI(pAccel, 1));
                  lpAccel[i].key   = static_cast<WORD>(hb_arrayGetNL(pAccel, 2));
                  lpAccel[i].cmd   = static_cast<WORD>(hb_arrayGetNL(pAccel, 3));
               }
            }
         }
         hAccel = CreateAcceleratorTable(lpAccel, nLen);
         hb_xfree(lpAccel);
      }
   }

   hmg_ret_HACCEL(hAccel);
}

// int WINAPI CopyAcceleratorTable(HACCEL hAccelSrc, LPACCEL lpAccelDst, int cAccelEntries)


HB_FUNC( COPYACCELERATORTABLE )
{
   HACCEL hAccelSrc = hmg_par_HACCEL(1);

   hb_retni(0);

   if( hAccelSrc != nullptr ) {
      int cAccelEntries = CopyAcceleratorTable(hAccelSrc, nullptr, 0);
      if( cAccelEntries > 0 ) {
         LPACCEL lpAccelDst = static_cast<LPACCEL>(hb_xalloc(cAccelEntries * sizeof(ACCEL)));
         if( lpAccelDst != nullptr ) {
            hb_retni(CopyAcceleratorTable(hAccelSrc, lpAccelDst, cAccelEntries));
            hb_storptr(lpAccelDst, 2);
         }
      }
   }
}

// HACCEL WINAPI CreateAcceleratorTable(LPACCEL lpAccel, int cAccelEntries)
HB_FUNC( CREATEACCELERATORTABLE )
{
   LPACCEL lpAccels = static_cast<LPACCEL>(hb_parptr(1));
   HACCEL  hAccel = nullptr;
   int cAccelEntries = hb_parni(2);

   if( lpAccels && (cAccelEntries > 0) ) {
      hAccel = CreateAcceleratorTable(lpAccels, cAccelEntries);
      hb_xfree(lpAccels);
   }

   hmg_ret_HACCEL(hAccel);
}

// BOOL WINAPI DestroyAcceleratorTable(HACCEL hAccel)
HB_FUNC( DESTROYACCELERATORTABLE )
{
   hb_retl(DestroyAcceleratorTable(hmg_par_HACCEL(1)) ? true : false);
}

// HACCEL WINAPI LoadAccelerators(HINSTANCE hInstance, LPCTSTR lpTableName)

/*
LOADACCELERATORS(HINSTANCE, cTableName|np2) --> HACCEL
*/
HB_FUNC( LOADACCELERATORS )
{
   HACCEL hAccel = nullptr;
   HINSTANCE hInstance = HB_ISNIL(1) ? GetResources() : hmg_par_HINSTANCE(1);
   LPCTSTR lpTableName;

   if( HB_ISNUM(2) ) {
      lpTableName = MAKEINTRESOURCE(hmg_par_WORD(2));
      hAccel = LoadAccelerators(hInstance, lpTableName);
   } else if( hb_parclen(2) > 0 ) {
      void * hTableName;
      lpTableName = HB_PARSTR(2, &hTableName, nullptr);
      hAccel = LoadAccelerators(hInstance, lpTableName);
      hb_strfree(hTableName);
   }

   hmg_ret_HACCEL(hAccel);
}

// HMENU WINAPI LoadMenu(HINSTANCE hInstance, LPCTSTR lpMenuName)

/*
LOADMENU(HINSTANCE, cMenuName|np2) --> HMENU
*/
HB_FUNC( LOADMENU )
{
   HMENU hMenu = nullptr;
   HINSTANCE hInstance = HB_ISNIL(1) ? GetResources() : hmg_par_HINSTANCE(1);
   LPCTSTR lpMenuName;

   if( HB_ISNUM(2) ) {
      lpMenuName = MAKEINTRESOURCE(hmg_par_WORD(2));
      hMenu = LoadMenu(hInstance, lpMenuName);
   } else if( HB_ISCHAR(2) ) {
      void * hMenuName;
      lpMenuName = HB_PARSTR(2, &hMenuName, nullptr);
      hMenu = LoadMenu(hInstance, lpMenuName);
      hb_strfree(hMenuName);
   }

   hmg_ret_HMENU(hMenu);
}

/*
_NEWMENUSTYLE(lp1) --> .T.|.F.
*/
HB_FUNC( _NEWMENUSTYLE )
{
   if( HB_ISLOG(1) ) {
      s_bCustomDraw = hb_parl(1);
   }

   hb_retl(s_bCustomDraw);
}

/*
_CLOSEMENU() --> .T.|.F.
*/
HB_FUNC( _CLOSEMENU )
{
   hb_retl(EndMenu());
}

/*
TRACKPOPUPMENU(HMENU, np2, np3, HWND, lp5) --> NIL
*/
HB_FUNC( TRACKPOPUPMENU )
{
   auto hwnd = hmg_par_HWND(4);
   SetForegroundWindow(hwnd); // hack for Microsoft "feature"
   TrackPopupMenu(hmg_par_HMENU(1), 0, hb_parni(2), hb_parni(3), 0, hwnd, nullptr);
   if( hb_pcount() > 4 && HB_ISLOG(5) && hb_parl(5) ) {
      PostMessage(hwnd, WM_NULL, 0, 0); // hack for tray menu closing
   }
}

/*
SETMENU(HWND, HMENU) --> NIL
*/
HB_FUNC( SETMENU )
{
   SetMenu(hmg_par_HWND(1), hmg_par_HMENU(2));
}

/*
SETMENUDEFAULTITEM(HMENU, np2) --> NIL
*/
HB_FUNC( SETMENUDEFAULTITEM )
{
   SetMenuDefaultItem(hmg_par_HMENU(1), hb_parni(2), FALSE);
}

/*
XCHECKMENUITEM(HMENU, np2) --> NIL
*/
HB_FUNC( XCHECKMENUITEM )
{
   CheckMenuItem(hmg_par_HMENU(1), hb_parni(2), MF_CHECKED);
}

/*
XUNCHECKMENUITEM(HMENU, np2) --> NIL
*/
HB_FUNC( XUNCHECKMENUITEM )
{
   CheckMenuItem(hmg_par_HMENU(1), hb_parni(2), MF_UNCHECKED);
}

/*
XENABLEMENUITEM(HMENU, np2) --> NIL
*/
HB_FUNC( XENABLEMENUITEM )
{
   EnableMenuItem(hmg_par_HMENU(1), hb_parni(2), MF_ENABLED);
}

/*
XDISABLEMENUITEM(HMENU, np2) --> NIL
*/
HB_FUNC( XDISABLEMENUITEM )
{
   EnableMenuItem(hmg_par_HMENU(1), hb_parni(2), MF_GRAYED);
}

/*
XDISABLECLOSEBUTTON(HWND, lp2) --> NIL
*/
HB_FUNC( XDISABLECLOSEBUTTON )
{
   EnableMenuItem(GetSystemMenu(hmg_par_HWND(1), FALSE), SC_CLOSE, MF_BYCOMMAND | (hb_parl(2) ? MF_ENABLED : MF_GRAYED));
}

/*
CREATEMENU() --> HMENU
*/
HB_FUNC( CREATEMENU )
{
   HMENU hMenu = CreateMenu();

   #ifndef __WINNT__
   if( s_bCustomDraw ) {
      SetMenuBarColor(hMenu, clrMenuBar1, TRUE);
   }
   #endif

   hmg_ret_HMENU(hMenu);
}

/*
CREATEPOPUPMENU() --> HMENU
*/
HB_FUNC( CREATEPOPUPMENU )
{
   hmg_ret_HMENU(CreatePopupMenu());
}

/*
APPENDMENUSTRING(HMENU, p2, p3, p4) -->  .T.|.F.
*/
HB_FUNC( APPENDMENUSTRING )
{
   void * strNewItem;
   LPCTSTR lpNewItem = HB_PARSTR(3, &strNewItem, nullptr);
   UINT style;

   if( s_bCustomDraw ) {
      UINT cch = HB_STRNLEN(lpNewItem, MAX_ITEM_TEXT * sizeof(TCHAR));

      auto lpMenuItem = static_cast<LPMENUITEM>(hb_xgrab((sizeof(MENUITEM))));
      ZeroMemory(lpMenuItem, sizeof(MENUITEM));
      lpMenuItem->cbSize     = hb_parni(2);
      lpMenuItem->uiID       = hb_parni(2);
      lpMenuItem->caption    = HB_STRNDUP(lpNewItem, cch);
      lpMenuItem->cch        = cch;
      lpMenuItem->hBitmap    = nullptr;
      lpMenuItem->hFont      = nullptr;
      lpMenuItem->uiItemType = hb_parni(4);
      lpMenuItem->hwnd       = nullptr;

      switch( hb_parni(4) ) {
         case 1:
            style = MF_OWNERDRAW | MF_MENUBREAK;
            break;
         case 2:
            style = MF_OWNERDRAW | MF_MENUBARBREAK;
            break;
         default:
            style = MF_OWNERDRAW;
      }

      hb_retl(AppendMenu(hmg_par_HMENU(1), style, hb_parni(2), reinterpret_cast<LPTSTR>(lpMenuItem)));
   } else {
      switch( hb_parni(4) ) {
         case 1:
            style = MF_STRING | MF_MENUBREAK; break;
         case 2:
            style = MF_STRING | MF_MENUBARBREAK; break;
         default:
            style = MF_STRING;
      }

      hb_retl(AppendMenu(hmg_par_HMENU(1), style, hb_parni(2), lpNewItem));
   }

   hb_strfree(strNewItem);
}

/*
APPENDMENUPOPUP(HMENU, p2, p3, p4, HFONT) --> .T.|.F.
*/
HB_FUNC( APPENDMENUPOPUP )
{
   void * strNewItem;
   LPCTSTR lpNewItem = HB_PARSTR(3, &strNewItem, nullptr);

   if( s_bCustomDraw ) {
      UINT cch = HB_STRNLEN(lpNewItem, MAX_ITEM_TEXT * sizeof(TCHAR));

      auto lpMenuItem = static_cast<LPMENUITEM>(hb_xgrabz((sizeof(MENUITEM))));
      lpMenuItem->cbSize     = hb_parni(2);
      lpMenuItem->uiID       = hb_parni(2);
      lpMenuItem->caption    = HB_STRNDUP(lpNewItem, cch);
      lpMenuItem->cch        = cch;
      lpMenuItem->hBitmap    = nullptr;
      lpMenuItem->hFont      = hmg_par_HFONT(5);
      lpMenuItem->uiItemType = hb_parni(4);

      hb_retl(AppendMenu(hmg_par_HMENU(1), MF_POPUP | MF_OWNERDRAW, hb_parni(2), reinterpret_cast<LPTSTR>(lpMenuItem)));
   } else {
      hb_retl(AppendMenu(hmg_par_HMENU(1), MF_POPUP | MF_STRING, hb_parni(2), lpNewItem));
   }

   hb_strfree(strNewItem);
}

/*
APPENDMENUSEPARATOR(HMENU) --> .T.|.F.
*/
HB_FUNC( APPENDMENUSEPARATOR )
{
   if( s_bCustomDraw ) {
      auto lpMenuItem = static_cast<LPMENUITEM>(hb_xgrabz((sizeof(MENUITEM))));
      lpMenuItem->uiItemType = 1000;
      hb_retl(AppendMenu(hmg_par_HMENU(1), MF_SEPARATOR | MF_OWNERDRAW, 0, reinterpret_cast<LPTSTR>(lpMenuItem)));
   } else {
      hb_retl(AppendMenu(hmg_par_HMENU(1), MF_SEPARATOR, 0, nullptr));
   }
}

/*
MODIFYMENUITEM(HMENU, p2, p3, cNewItem) --> .T.|.F.
*/
HB_FUNC( MODIFYMENUITEM )
{
   void * strNewItem;
   hb_retl(ModifyMenu(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND | MF_STRING, hb_parni(3), HB_PARSTR(4, &strNewItem, nullptr)));
   hb_strfree(strNewItem);
}

/*
INSERTMENUITEM(HMENU, p2, p3, cNewItem) --> .T.|.F.
*/
HB_FUNC( INSERTMENUITEM )
{
   void * strNewItem;
   hb_retl(InsertMenu(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND | MF_STRING, hb_parni(3), HB_PARSTR(4, &strNewItem, nullptr)));
   hb_strfree(strNewItem);
}

/*
REMOVEMENUITEM(HMENU, p2) --> .T.|.F.
*/
HB_FUNC( REMOVEMENUITEM )
{
   hb_retl(RemoveMenu(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND));
}

/*
MENUITEM_SETBITMAPS(HMENU, p2, p3) --> HBITMAP
*/
HB_FUNC( MENUITEM_SETBITMAPS )
{
   int Transparent = s_bCustomDraw ? 0 : 1;
   HBITMAP himage1 = HMG_LoadPicture(hb_parc(3), -1, -1, nullptr, 0, Transparent, -1, 0, false, 255);

   if( s_bCustomDraw ) {
      MENUITEM * pMENUITEM;

      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_DATA;

      if( GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo) ) {
         pMENUITEM = reinterpret_cast<MENUITEM*>(MenuItemInfo.dwItemData);
         if( pMENUITEM->hBitmap != nullptr ) {
            DeleteObject(pMENUITEM->hBitmap);
         }

         pMENUITEM->hBitmap = himage1;
      }
   } else {
      HBITMAP himage2 = HMG_LoadPicture(hb_parc(4), -1, -1, nullptr, 0, Transparent, -1, 0, false, 255);
      SetMenuItemBitmaps(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND, himage1, himage2);
   }

   hmg_ret_HBITMAP(himage1);
}

HB_FUNC( MENUITEM_SETCHECKMARKS )
{
   if( s_bCustomDraw ) {
      MENUITEMINFO MenuItemInfo;

      HBITMAP himage1 = HMG_LoadPicture(hb_parc(3), -1, -1, nullptr, 0, 0, -1, 0, false, 255);
      HBITMAP himage2 = HMG_LoadPicture(hb_parc(4), -1, -1, nullptr, 0, 0, -1, 0, false, 255);

      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_CHECKMARKS;

      if( GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo) ) {
         if( MenuItemInfo.hbmpChecked != nullptr ) {
            DeleteObject(MenuItemInfo.hbmpChecked);
         }

         MenuItemInfo.hbmpChecked = himage1;

         if( MenuItemInfo.hbmpUnchecked != nullptr ) {
            DeleteObject(MenuItemInfo.hbmpUnchecked);
         }

         MenuItemInfo.hbmpUnchecked = himage2;

         SetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo);
      }
   }
}

/*
MENUITEM_SETICON(HMENU, np2, cIconName) --> HBITMAP
*/
HB_FUNC( MENUITEM_SETICON )
{
   void * str;
   LPCTSTR lpIconName = HB_PARSTR(3, &str, nullptr);

   HICON hIcon = static_cast<HICON>(LoadImage(GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE | LR_DEFAULTCOLOR));
   if( hIcon == nullptr ) {
      hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR));
   }

   HBITMAP himage1 = Icon2Bmp(hIcon); // convert icon to bitmap

   if( s_bCustomDraw ) {
      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_DATA;
      if( GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo) ) {
         LPMENUITEM lpMenuItem = reinterpret_cast<LPMENUITEM>(MenuItemInfo.dwItemData);
         if( lpMenuItem->hBitmap != nullptr ) {
            DeleteObject(lpMenuItem->hBitmap);
         }
         lpMenuItem->hBitmap = himage1;
      }
   } else {
      SetMenuItemBitmaps(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND, himage1, himage1);
   }

   DestroyIcon(hIcon);
   hmg_ret_HBITMAP(himage1);
   hb_strfree(str);
}

HB_FUNC( MENUITEM_SETFONT )
{
   if( s_bCustomDraw ) {
      MENUITEMINFO MenuItemInfo;
      LPMENUITEM   lpMenuItem;

      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_DATA;

      if( GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo) ) {
         lpMenuItem = reinterpret_cast<LPMENUITEM>(MenuItemInfo.dwItemData);

         if( GetObjectType(hmg_par_HGDIOBJ(3)) == OBJ_FONT ) {
            if( lpMenuItem->hFont != nullptr ) {
               DeleteObject(lpMenuItem->hFont);
            }

            lpMenuItem->hFont = hmg_par_HFONT(3);
         }
      }
   }
}

HB_FUNC( XGETMENUCAPTION )
{
   if( s_bCustomDraw ) {
   #ifdef UNICODE
      LPSTR pStr;
   #endif
      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_DATA;
      GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo);

      MENUITEM * lpMenuItem = reinterpret_cast<MENUITEM*>(MenuItemInfo.dwItemData);

      if( lpMenuItem->caption != nullptr ) {
      #ifndef UNICODE
         hb_retclen(lpMenuItem->caption, lpMenuItem->cch);
      #else
         pStr = WideToAnsi(lpMenuItem->caption);
         hb_retclen(pStr, lpMenuItem->cch);
         hb_xfree(pStr);
      #endif
      } else {
         hb_retc("");
      }
   }
}

HB_FUNC( XSETMENUCAPTION )
{
   if( s_bCustomDraw ) {
   #ifndef UNICODE
      LPCSTR lpNewItem = hb_parc(3);
   #else
      LPWSTR lpNewItem = AnsiToWide(static_cast<char*>(hb_parc(3)));
      LPSTR  pStr;
   #endif
      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_DATA;
      GetMenuItemInfo(hmg_par_HMENU(1), hb_parni(2), FALSE, &MenuItemInfo);

      MENUITEM * lpMenuItem = reinterpret_cast<MENUITEM*>(MenuItemInfo.dwItemData);

      if( lpMenuItem->caption != nullptr ) {
         UINT cch = static_cast<UINT>(HB_STRNLEN(lpNewItem, MAX_ITEM_TEXT * sizeof(TCHAR)));

      #ifndef UNICODE
         hb_retclen(lpMenuItem->caption, lpMenuItem->cch);
      #else
         pStr = WideToAnsi(lpMenuItem->caption);
         hb_retclen(pStr, lpMenuItem->cch);
         hb_xfree(pStr);
      #endif

         hb_xfree(lpMenuItem->caption);

         lpMenuItem->cch     = cch;
         lpMenuItem->caption = HB_STRNDUP(lpNewItem, cch);
      } else {
         hb_retc("");
      }

#ifdef UNICODE
      hb_xfree(lpNewItem);
#endif
   }
}

HB_FUNC( XGETMENUCHECKSTATE )
{
   UINT state = GetMenuState(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND);

   if( state != 0xFFFFFFFF ) {
      hb_retl((state & MF_CHECKED) ? TRUE : FALSE);
   } else {
      hb_retl(false);
   }
}

HB_FUNC( XGETMENUENABLEDSTATE )
{
   UINT state = GetMenuState(hmg_par_HMENU(1), hb_parni(2), MF_BYCOMMAND);

   if( state != 0xFFFFFFFF ) {
      hb_retl(((state & MF_GRAYED) || (state & MF_DISABLED)) ? FALSE : TRUE);
   } else {
      hb_retl(false);
   }
}

HB_FUNC( ISMENU )
{
   hb_retl(IsMenu(hmg_par_HMENU(1)));
}

HB_FUNC( GETMENU )
{
   hmg_ret_HMENU(GetMenu(hmg_par_HWND(1)));
}

HB_FUNC( GETSYSTEMMENU )
{
   hmg_ret_HMENU(GetSystemMenu(hmg_par_HWND(1), FALSE));
}

HB_FUNC( GETMENUITEMCOUNT )
{
   hb_retni(GetMenuItemCount(hmg_par_HMENU(1)));
}

// Owner draw function

// ODA_DRAWENTIRE - This bit is set when the entire control needs to be drawn.
// ODA_FOCUS - This bit is set when the control gains or loses input focus. The itemState member should be checked to determine whether the control has focus.
// ODA_SELECT - This bit is set when only the selection status has changed. The itemState member should be checked to determine the new selection state.
//
// Owner draw state
//
// ODS_CHECKED - This bit is set if the menu item is to be checked. This bit is used only in a menu.
// ODS_DISABLED - This bit is set if the item is to be drawn as disabled.
// ODS_FOCUS - This bit is set if the item has input focus.
// ODS_GRAYED - This bit is set if the item is to be dimmed. This bit is used only in a menu.
// ODS_SELECTED - This bit is set if the item's status is selected.
// ODS_COMBOBOXEDIT - The drawing takes place in the selection field (edit control) of an ownerdrawn combo box.
// ODS_DEFAULT - The item is the default item.

HB_FUNC( _ONDRAWMENUITEM )
{
   LPDRAWITEMSTRUCT lpdis = reinterpret_cast<LPDRAWITEMSTRUCT>(HB_PARNL(1));
   MENUITEM * lpMenuItem = reinterpret_cast<MENUITEM*>(lpdis->itemData);

   if( lpdis->CtlType != ODT_MENU ) {
      return;
   }

   // draw SEPARATOR
   if( lpdis->itemID == 0 ) {
      DrawSeparator(lpdis->hDC, lpdis->rcItem);
      return;
   }

   HFONT oldfont;

   if( lpMenuItem->hFont != nullptr ) {
      oldfont = static_cast<HFONT>(SelectObject(lpdis->hDC, lpMenuItem->hFont));
   } else {
      oldfont = static_cast<HFONT>(SelectObject(lpdis->hDC, GetStockObject(DEFAULT_GUI_FONT)));
   }

   // save prev. colours state
   COLORREF clrText = SetTextColor(lpdis->hDC, clrText1);
   COLORREF clrBackground = SetBkColor(lpdis->hDC, clrBk1);
   UINT bkMode = SetBkMode(lpdis->hDC, TRANSPARENT);

   BOOL fSelected = FALSE;

   // set colours and flags ( fSelected etc. )
   if( ((lpdis->itemAction & ODA_SELECT) || (lpdis->itemAction & ODA_DRAWENTIRE)) && (!(lpdis->itemState & ODS_GRAYED)) ) {
      if( lpdis->itemState & ODS_SELECTED ) {
         clrText       = SetTextColor(lpdis->hDC, (lpMenuItem->uiItemType != 1) ? clrSelectedText1 : clrMenuBarSelectedText);
         clrBackground = SetBkColor(lpdis->hDC, (lpMenuItem->uiItemType != 1) ? clrSelectedBk1 : clrMenuBar1);
         fSelected     = TRUE;
      } else {
         clrText       = SetTextColor(lpdis->hDC, (lpMenuItem->uiItemType != 1) ? clrText1 : clrMenuBarText);
         clrBackground = SetBkColor(lpdis->hDC, (lpMenuItem->uiItemType != 1) ? clrBk2 : clrMenuBar2);
         fSelected     = FALSE;
      }
   }

   BOOL fGrayed   = FALSE;

   if( lpdis->itemState & ODS_GRAYED ) {
      clrText = SetTextColor(lpdis->hDC, (lpMenuItem->uiItemType != 1) ? clrGrayedText1 : clrMenuBarGrayedText);
      fGrayed = TRUE;
   }

   BOOL fChecked  = FALSE;

   if( lpdis->itemState & ODS_CHECKED ) {
      fChecked = TRUE;
   }

   // draw menu item bitmap background
   if( lpMenuItem->uiItemType != 1 ) {
      DrawBitmapBK(lpdis->hDC, lpdis->rcItem);
   }

   //draw menu item background
   DrawItemBk(lpdis->hDC, lpdis->rcItem, fSelected, fGrayed, lpMenuItem->uiItemType, ((lpMenuItem->hBitmap == nullptr) && (!fChecked)));

   // draw menu item border
   if( fSelected && (!fGrayed) ) {
      DrawSelectedItemBorder(lpdis->hDC, lpdis->rcItem, lpMenuItem->uiItemType, ((lpMenuItem->hBitmap == nullptr) && (!fChecked)));
   }

   // draw bitmap
   if( (lpMenuItem->hBitmap != nullptr) && (!fChecked) ) {
      DrawGlyph(
         lpdis->hDC,
         lpdis->rcItem.left + cx_delta - 2,
         lpdis->rcItem.top + cy_delta,
         bm_size,
         bm_size,
         lpMenuItem->hBitmap,
         static_cast<COLORREF>(RGB(125, 125, 125)),
         ((fGrayed) ? TRUE : FALSE),
         TRUE);

      if( fSelected && (lpMenuItem->uiItemType != 1) && (eMenuCursorType == Short) && bSelectedItemBorder3d ) {
         HPEN pen  = CreatePen(PS_SOLID, 1, clrSelectedItemBorder2);
         HPEN pen1 = CreatePen(PS_SOLID, 1, clrSelectedItemBorder4);

         HPEN oldPen = static_cast<HPEN>(SelectObject(lpdis->hDC, pen1));

         RECT rect;
         CopyRect(&rect, &lpdis->rcItem);
         rect.left  += (cx_delta / 2 - 2);
         rect.top   += (cy_delta / 2);
         rect.right  = rect.left + bm_size + cx_delta;
         rect.bottom = rect.top + bm_size + cy_delta;

         MoveToEx(lpdis->hDC, rect.left, rect.top, nullptr);

         LineTo(lpdis->hDC, rect.right, rect.top);
         SelectObject(lpdis->hDC, pen);
         LineTo(lpdis->hDC, rect.right, rect.bottom);
         LineTo(lpdis->hDC, rect.left, rect.bottom);
         SelectObject(lpdis->hDC, pen1);
         LineTo(lpdis->hDC, rect.left, rect.top);

         SelectObject(lpdis->hDC, oldPen);

         DeleteObject(pen);
         DeleteObject(pen1);
      }
   }

   // draw menu item text
   int iLen = HB_STRNLEN(lpMenuItem->caption, MAX_ITEM_TEXT * sizeof(TCHAR));

   if( lpMenuItem->uiItemType == 1 ) {
      DrawText(lpdis->hDC, lpMenuItem->caption, iLen, &lpdis->rcItem, DT_CENTER | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS | DT_EXPANDTABS);
   } else {
      lpdis->rcItem.left += (min_width + cx_delta + 2);
      DrawText(lpdis->hDC, lpMenuItem->caption, iLen, &lpdis->rcItem, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS | DT_EXPANDTABS);
      lpdis->rcItem.left -= (min_width + cx_delta + 2);
   }

   // draw menu item checked mark
   if( fChecked ) {
      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask = MIIM_CHECKMARKS;
      GetMenuItemInfo(reinterpret_cast<HMENU>(lpdis->hwndItem), lpdis->itemID, FALSE, &MenuItemInfo);
      SIZE size;
      size.cx = bm_size;      //GetSystemMetrics(SM_CXMENUCHECK);
      size.cy = bm_size;      //GetSystemMetrics(SM_CYMENUCHECK);
      DrawCheck(lpdis->hDC, size, lpdis->rcItem, fGrayed, fSelected, MenuItemInfo.hbmpChecked);
   }

   SelectObject(lpdis->hDC, oldfont);

   // restore prev. colours state
   SetTextColor(lpdis->hDC, clrText);
   SetBkColor(lpdis->hDC, clrBackground);
   SetBkMode(lpdis->hDC, bkMode);
}

VOID DrawSeparator( HDC hDC, RECT r )
{
   RECT rect;
   CopyRect(&rect, &r);
   rect.right = rect.left + min_width + cx_delta / 2;

   if( (EnabledGradient()) && (!IsColorEqual(clrImageBk1, clrImageBk2)) ) {
      FillGradient(hDC, &rect, FALSE, clrImageBk1, clrImageBk2);
   } else {
      HBRUSH brush = CreateSolidBrush(clrImageBk1);
      FillRect(hDC, &rect, brush);
      DeleteObject(brush);
   }

   CopyRect(&rect, &r);
   rect.left += (min_width + cx_delta / 2);

   if( (EnabledGradient()) && (!IsColorEqual(clrBk1, clrBk2)) ) {
      FillGradient(hDC, &rect, FALSE, clrBk1, clrBk2);
   } else {
      HBRUSH brush = CreateSolidBrush(clrBk1);
      FillRect(hDC, &rect, brush);
      DeleteObject(brush);
   }

   CopyRect(&rect, &r);

   HPEN pen = CreatePen(PS_SOLID, 1, clrSeparator1);
   HPEN oldPen = static_cast<HPEN>(SelectObject(hDC, pen));

   if( eSeparatorPosition == Right ) {
      rect.left += (min_width + cx_delta + 2);
   } else if( eSeparatorPosition == Middle ) {
      rect.left  += (min_width - cx_delta);
      rect.right -= (min_width - cx_delta);
   }

   rect.top += (rect.bottom - rect.top) / 2;
   MoveToEx(hDC, rect.left, rect.top, nullptr);
   LineTo(hDC, rect.right, rect.top);

   if( eSeparatorType == Double ) {
      HPEN pen1 = CreatePen(PS_SOLID, 1, clrSeparator2);
      HPEN oldPen1 = static_cast<HPEN>(SelectObject(hDC, pen1));

      rect.top += 1;
      MoveToEx(hDC, rect.left, rect.top, nullptr);
      LineTo(hDC, rect.right, rect.top);

      SelectObject(hDC, oldPen1);
      DeleteObject(pen1);
   }

   SelectObject(hDC, oldPen);
   DeleteObject(pen);
}

VOID DrawBitmapBK(HDC hDC, RECT r)
{
   RECT rect;
   CopyRect(&rect, &r);
   rect.right = rect.left + min_width + cx_delta / 2;

   if( (EnabledGradient()) && (!IsColorEqual(clrImageBk1, clrImageBk2)) ) {
      FillGradient(hDC, &rect, FALSE, clrImageBk1, clrImageBk2);
   } else {
      HBRUSH brush = CreateSolidBrush(clrImageBk1);
      FillRect(hDC, &rect, brush);
      DeleteObject(brush);
   }
}

VOID DrawItemBk(HDC hDC, RECT r, BOOL Selected, BOOL Grayed, UINT itemType, BOOL clear)
{
   RECT rect;
   CopyRect(&rect, &r);
   if( (!Selected) && (itemType != 1) ) {
      rect.left += (min_width + cx_delta / 2);
   }

   if( (Selected) && (itemType != 1) && (eMenuCursorType == Short) && (!clear) ) {
      rect.left += (min_width + cx_delta / 2);
   }

   if( !Grayed ) {
      if( Selected ) {
         if( EnabledGradient() ) {
            FillGradient(hDC, &rect,
               (itemType == 1) ? TRUE : gradientVertical,
               (itemType == 1) ? clrSelectedMenuBarItem1 : clrSelectedBk1,
               (itemType == 1) ? clrSelectedMenuBarItem2 : clrSelectedBk2);
         } else {
            HBRUSH brush = CreateSolidBrush((itemType == 1) ? clrSelectedMenuBarItem1 : clrSelectedBk1);
            FillRect(hDC, &rect, brush);
            DeleteObject(brush);
         }
      } else {
         if( EnabledGradient() && (!IsColorEqual(clrMenuBar1, clrMenuBar2) || (!IsColorEqual(clrBk1, clrBk2) && (itemType != 1))) ) {
            FillGradient(hDC, &rect,
               ((itemType == 1) ? TRUE : FALSE),
               ((itemType == 1) ? clrMenuBar1 : clrBk1),
               ((itemType == 1) ? clrMenuBar2 : clrBk2));
         } else {
            HBRUSH brush = CreateSolidBrush((itemType == 1) ? clrMenuBar1 : clrBk1);
            FillRect(hDC, &rect, brush);
            DeleteObject(brush);
         }
      }
   } else {
      if( EnabledGradient() ) {
         FillGradient(hDC, &rect, FALSE, clrGrayedBk1, clrGrayedBk2);
      } else {
         HBRUSH brush = CreateSolidBrush(clrGrayedBk1);
         FillRect(hDC, &rect, brush);
         DeleteObject(brush);
      }
   }
}

VOID DrawSelectedItemBorder(HDC hDC, RECT r, UINT itemType, BOOL clear)
{
   HPEN pen, pen1;

   if( itemType != 1 ) {
      pen  = CreatePen(PS_SOLID, 1, clrSelectedItemBorder1);
      pen1 = CreatePen(PS_SOLID, 1, clrSelectedItemBorder3);
   } else {
      pen  = CreatePen(PS_SOLID, 1, clrSelectedItemBorder2);
      pen1 = CreatePen(PS_SOLID, 1, clrSelectedItemBorder4);
   }

   HPEN oldPen = static_cast<HPEN>(SelectObject(hDC, pen));

   RECT rect;
   CopyRect(&rect, &r);
   if( (eMenuCursorType == Short) && (itemType != 1) && (!clear) ) {
      rect.left += (min_width + cx_delta / 2);
   }

   InflateRect(&rect, -1, -1);

   MoveToEx(hDC, rect.left, rect.top, nullptr);

   if( (itemType == 1) && bSelectedItemBorder3d ) {
      LineTo(hDC, rect.right, rect.top);
      SelectObject(hDC, pen1);
      LineTo(hDC, rect.right, rect.bottom);
      LineTo(hDC, rect.left, rect.bottom);
      SelectObject(hDC, pen);
      LineTo(hDC, rect.left, rect.top);
   } else {
      LineTo(hDC, rect.right, rect.top);
      LineTo(hDC, rect.right, rect.bottom);
      LineTo(hDC, rect.left, rect.bottom);
      LineTo(hDC, rect.left, rect.top);
   }

   SelectObject(hDC, oldPen);
   DeleteObject(pen);
   DeleteObject(pen1);
}

VOID DrawCheck(HDC hdc, SIZE size, RECT rect, BOOL disabled, BOOL selected, HBITMAP hbitmap)
{
   if( hbitmap != 0 ) {
      DrawGlyph(
         hdc,
         rect.left + cx_delta / 2,
         rect.top + cy_delta / 2 + 2,
         size.cx,
         size.cy,
         hbitmap,
         static_cast<COLORREF>(RGB(125, 125, 125)),
         ((disabled) ? TRUE : FALSE),
         TRUE);
   } else {
      HBRUSH brush;
      if( (selected) && (eMenuCursorType != Short) ) {
         brush = CreateSolidBrush(clrSelectedBk1);
      } else {
         brush = CreateSolidBrush(clrCheckMarkBk);
      }

      HBRUSH oldBrush = static_cast<HBRUSH>(SelectObject(hdc, brush));

      HPEN pen = CreatePen(PS_SOLID, 1, clrCheckMarkSq);
      HPEN oldPen = static_cast<HPEN>(SelectObject(hdc, pen));

      UINT w = (size.cx > min_width ? min_width : size.cx);
      UINT h = w;
      UINT x = rect.left + (min_width - w) / 2;
      UINT y = rect.top + (min_height + cy_delta - h) / 2;

      Rectangle(hdc, x, y, x + w, y + h);

      DeleteObject(pen);

      if( disabled ) {
         pen = CreatePen(PS_SOLID, 1, clrCheckMarkGr);
      } else {
         pen = CreatePen(PS_SOLID, 1, clrCheckMark);
      }

      SelectObject(hdc, pen);

      MoveToEx(hdc, x + 1, y + 5, nullptr);
      LineTo(hdc, x + 4, y + h - 2);
      MoveToEx(hdc, x + 2, y + 5, nullptr);
      LineTo(hdc, x + 4, y + h - 3);
      MoveToEx(hdc, x + 2, y + 4, nullptr);
      LineTo(hdc, x + 5, y + h - 3);
      MoveToEx(hdc, x + 4, y + h - 3, nullptr);
      LineTo(hdc, x + w + 2, y - 1);
      MoveToEx(hdc, x + 4, y + h - 2, nullptr);
      LineTo(hdc, x + w - 2, y + 3);

      SelectObject(hdc, oldPen);
      SelectObject(hdc, oldBrush);

      DeleteObject(pen);
      DeleteObject(brush);
   }
}

// Misc

HB_FUNC( SETMENUBITMAPHEIGHT )
{
   bm_size = hb_parni(1);
   min_height = min_width = bm_size + 4;
   hb_retni(bm_size);
}

HB_FUNC( GETMENUBITMAPHEIGHT )
{
   hb_retni(bm_size);
}

HB_FUNC( SETMENUSEPARATORTYPE )
{
   eSeparatorType = static_cast<SEPARATORTYPE>(hb_parni(1));
   eSeparatorPosition = static_cast<SEPARATORPOSITION>(hb_parni(2));
}

HB_FUNC( SETMENUSELECTEDITEM3D )
{
   bSelectedItemBorder3d = hmg_par_BOOL(1);
}

HB_FUNC( SETMENUCURSORTYPE )
{
   eMenuCursorType = static_cast<MENUCURSORTYPE>(hb_parni(1));
}

// Color Management of HMG menus

#ifndef __WINNT__
VOID SetMenuBarColor(HMENU hMenu, COLORREF clrBk, BOOL fSubMenu)
{
   MENUINFO MenuInfo;
   MenuInfo.cbSize = sizeof(MENUINFO);
   GetMenuInfo(hMenu, &MenuInfo);
   MenuInfo.fMask = MIM_BACKGROUND;
   if( fSubMenu ) {
      MenuInfo.fMask |= MIM_APPLYTOSUBMENUS;
   }
   MenuInfo.hbrBack = CreateSolidBrush(clrBk);
   SetMenuInfo(hMenu, &MenuInfo);
}
#endif

static BOOL IsColorEqual(COLORREF clr1, COLORREF clr2)
{
   return   ((GetRValue(clr1) == GetRValue(clr2))
          && (GetGValue(clr1) == GetGValue(clr2))
          && (GetBValue(clr1) == GetBValue(clr2))) ? TRUE : FALSE;
}

HB_FUNC( GETMENUCOLORS )
{
   PHB_ITEM aResult = hb_itemArrayNew(28);

   HB_arraySetNL(aResult, 1, clrMenuBar1);
   HB_arraySetNL(aResult, 2, clrMenuBar2);
   HB_arraySetNL(aResult, 3, clrMenuBarText);
   HB_arraySetNL(aResult, 4, clrMenuBarSelectedText);
   HB_arraySetNL(aResult, 5, clrMenuBarGrayedText);
   HB_arraySetNL(aResult, 6, clrSelectedMenuBarItem1);
   HB_arraySetNL(aResult, 7, clrSelectedMenuBarItem2);
   HB_arraySetNL(aResult, 8, clrText1);
   HB_arraySetNL(aResult, 9, clrSelectedText1);
   HB_arraySetNL(aResult, 10, clrGrayedText1);
   HB_arraySetNL(aResult, 11, clrBk1);
   HB_arraySetNL(aResult, 12, clrBk2);
   HB_arraySetNL(aResult, 13, clrSelectedBk1);
   HB_arraySetNL(aResult, 14, clrSelectedBk2);
   HB_arraySetNL(aResult, 15, clrGrayedBk1);
   HB_arraySetNL(aResult, 16, clrGrayedBk2);
   HB_arraySetNL(aResult, 17, clrImageBk1);
   HB_arraySetNL(aResult, 18, clrImageBk2);
   HB_arraySetNL(aResult, 19, clrSeparator1);
   HB_arraySetNL(aResult, 20, clrSeparator2);
   HB_arraySetNL(aResult, 21, clrSelectedItemBorder1);
   HB_arraySetNL(aResult, 22, clrSelectedItemBorder2);
   HB_arraySetNL(aResult, 23, clrSelectedItemBorder3);
   HB_arraySetNL(aResult, 24, clrSelectedItemBorder4);
   HB_arraySetNL(aResult, 25, clrCheckMark);
   HB_arraySetNL(aResult, 26, clrCheckMarkBk);
   HB_arraySetNL(aResult, 27, clrCheckMarkSq);
   HB_arraySetNL(aResult, 28, clrCheckMarkGr);

   hb_itemReturnRelease(aResult);
}

HB_FUNC( SETMENUCOLORS )
{
   auto pArray = hb_param(1, Harbour::Item::ARRAY);

   if( (pArray != nullptr) && (hb_arrayLen(pArray) >= 28) ) {
      clrMenuBar1             = static_cast<COLORREF>(HB_PARVNL(1, 1));
      clrMenuBar2             = static_cast<COLORREF>(HB_PARVNL(1, 2));
      clrMenuBarText          = static_cast<COLORREF>(HB_PARVNL(1, 3));
      clrMenuBarSelectedText  = static_cast<COLORREF>(HB_PARVNL(1, 4));
      clrMenuBarGrayedText    = static_cast<COLORREF>(HB_PARVNL(1, 5));
      clrSelectedMenuBarItem1 = static_cast<COLORREF>(HB_PARVNL(1, 6));
      clrSelectedMenuBarItem2 = static_cast<COLORREF>(HB_PARVNL(1, 7));
      clrText1                = static_cast<COLORREF>(HB_PARVNL(1, 8));
      clrSelectedText1        = static_cast<COLORREF>(HB_PARVNL(1, 9));
      clrGrayedText1          = static_cast<COLORREF>(HB_PARVNL(1, 10));
      clrBk1                  = static_cast<COLORREF>(HB_PARVNL(1, 11));
      clrBk2                  = static_cast<COLORREF>(HB_PARVNL(1, 12));
      clrSelectedBk1          = static_cast<COLORREF>(HB_PARVNL(1, 13));
      clrSelectedBk2          = static_cast<COLORREF>(HB_PARVNL(1, 14));
      clrGrayedBk1            = static_cast<COLORREF>(HB_PARVNL(1, 15));
      clrGrayedBk2            = static_cast<COLORREF>(HB_PARVNL(1, 16));
      clrImageBk1             = static_cast<COLORREF>(HB_PARVNL(1, 17));
      clrImageBk2             = static_cast<COLORREF>(HB_PARVNL(1, 18));
      clrSeparator1           = static_cast<COLORREF>(HB_PARVNL(1, 19));
      clrSeparator2           = static_cast<COLORREF>(HB_PARVNL(1, 20));
      clrSelectedItemBorder1  = static_cast<COLORREF>(HB_PARVNL(1, 21));
      clrSelectedItemBorder2  = static_cast<COLORREF>(HB_PARVNL(1, 22));
      clrSelectedItemBorder3  = static_cast<COLORREF>(HB_PARVNL(1, 23));
      clrSelectedItemBorder4  = static_cast<COLORREF>(HB_PARVNL(1, 24));
      clrCheckMark            = static_cast<COLORREF>(HB_PARVNL(1, 25));
      clrCheckMarkBk          = static_cast<COLORREF>(HB_PARVNL(1, 26));
      clrCheckMarkSq          = static_cast<COLORREF>(HB_PARVNL(1, 27));
      clrCheckMarkGr          = static_cast<COLORREF>(HB_PARVNL(1, 28));
   }
}

// Call this funtions on WM_DESTROY, WM_MEASUREITEM of menu owner window

HB_FUNC( _ONDESTROYMENU )
{
   HMENU menu = hmg_par_HMENU(1);

   if( IsMenu(menu) ) {
      bool bResult = _DestroyMenu(menu);

#ifdef _ERRORMSG_
      if( !bResult ) {
         MessageBox(nullptr, "Menu is not destroyed successfully", "Warning", MB_OK | MB_ICONWARNING);
      }
#endif
      if( hb_pcount() > 1 && hb_parl(2) ) {
         bResult = bResult && DestroyMenu(menu);
      }

      hb_retl(bResult);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
   }
}

static BOOL _DestroyMenu(HMENU menu)
{
   BOOL bResult = TRUE;

   for( int i = 0; i < GetMenuItemCount(menu); i++ ) { // TODO: move declarations to outside of the loop
      HMENU pSubMenu;

      MENUITEMINFO MenuItemInfo;
      MenuItemInfo.cbSize = sizeof(MENUITEMINFO);
      MenuItemInfo.fMask  = MIIM_CHECKMARKS | MIIM_DATA;
      GetMenuItemInfo(menu, i, TRUE, &MenuItemInfo);

      if( MenuItemInfo.hbmpChecked != nullptr ) {
         bResult = DeleteObject(MenuItemInfo.hbmpChecked);
         MenuItemInfo.hbmpChecked = nullptr;
      }

      if( MenuItemInfo.hbmpUnchecked != nullptr ) {
         bResult = bResult && DeleteObject(MenuItemInfo.hbmpUnchecked);
         MenuItemInfo.hbmpUnchecked = nullptr;
      }

      if( s_bCustomDraw ) {
         LPMENUITEM lpMenuItem;
         lpMenuItem = reinterpret_cast<LPMENUITEM>(MenuItemInfo.dwItemData);

         if( lpMenuItem->caption != nullptr ) {
            hb_xfree(lpMenuItem->caption);
            lpMenuItem->caption = nullptr;
         }

         if( lpMenuItem->hBitmap != nullptr ) {
            bResult = bResult && DeleteObject(lpMenuItem->hBitmap);
            lpMenuItem->hBitmap = nullptr;
         }

         if( GetObjectType(static_cast<HGDIOBJ>(lpMenuItem->hFont)) == OBJ_FONT ) {
            bResult = bResult && DeleteObject(lpMenuItem->hFont);
            lpMenuItem->hFont = nullptr;
         }

         hb_xfree(lpMenuItem);
      }

      pSubMenu = GetSubMenu(menu, i);

      if( pSubMenu != nullptr ) {
         bResult = bResult && _DestroyMenu(pSubMenu);
      }
   }

   return bResult;
}

HB_FUNC( _ONMEASUREMENUITEM )
{
   auto hwnd = hmg_par_HWND(1);

   if( IsWindow(hwnd) ) {
      HDC hdc = GetDC(hwnd);
      LPMEASUREITEMSTRUCT lpmis = reinterpret_cast<LPMEASUREITEMSTRUCT>(HB_PARNL(4));
      MENUITEM * lpMenuItem = reinterpret_cast<MENUITEM*>(lpmis->itemData);
      SIZE size = {0, 0};

      HFONT oldfont;
      if( GetObjectType(static_cast<HGDIOBJ>(lpMenuItem->hFont)) == OBJ_FONT ) {
         oldfont = static_cast<HFONT>(SelectObject(hdc, lpMenuItem->hFont));
      } else {
         oldfont = static_cast<HFONT>(SelectObject(hdc, GetStockObject(DEFAULT_GUI_FONT)));
      }

      if( lpMenuItem->uiItemType == 1000 ) {
         lpmis->itemHeight = 2 * cy_delta;
         lpmis->itemWidth  = 0;
      } else {
         GetTextExtentPoint32(hdc, lpMenuItem->caption, lpMenuItem->cch, &size);
      }

      if( lpMenuItem->uiItemType == 1 ) {
         lpmis->itemWidth = size.cx;
      } else if( lpmis->itemID > 0 ) {
         lpmis->itemWidth = min_width + cx_delta + size.cx + 8;
      }

      if( lpMenuItem->uiItemType != 1000 ) {
         lpmis->itemHeight = (size.cy > min_height ? size.cy : min_height);
         lpmis->itemHeight += cy_delta;
      }

      SelectObject(hdc, oldfont);

      ReleaseDC(hwnd, hdc);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
   }
}

HB_FUNC( _COLORMENU )
{
   auto hWnd = hmg_par_HWND(1);
   HMENU iMenu = GetMenu(hWnd);
   MENUINFO iMenuInfo;
   GetMenuInfo(iMenu, &iMenuInfo);
   iMenuInfo.cbSize = sizeof(MENUINFO);
   BOOL lSubMenu = HB_ISLOG(3) ? hb_parl(3) : FALSE;
   if( lSubMenu ) {
      iMenuInfo.fMask = MIM_BACKGROUND | MIM_APPLYTOSUBMENUS;
   } else {
      iMenuInfo.fMask = MIM_BACKGROUND;
   }
   INT nRed = HB_PARNI(2, 1);
   INT nGreen = HB_PARNI(2, 2);
   INT nBlue = HB_PARNI(2, 3);
   iMenuInfo.hbrBack = CreateSolidBrush(RGB(nRed, nGreen, nBlue));
   SetMenuInfo(iMenu, &iMenuInfo);
   DrawMenuBar(hWnd);
}
