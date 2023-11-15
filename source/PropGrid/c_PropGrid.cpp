/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Property Grid control source code
 * (C)2007-2011 Janusz Pora <januszpora@onet.eu>
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
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - https://harbour.github.io/
 *
 * "Harbour Project"
 * Copyright 1999-2022, https://harbour.github.io/
 *
 * "WHAT32"
 *  Copyright 2002 AJ Wos <andrwos@aust1.net>
 *
 * "HWGUI"
 * Copyright 2001-2009 Alexander S.Kresin <alex@belacy.belgorod.su>
 */

#define _WIN32_IE 0x0500
#define _WIN32_WINNT 0x0400

#define PG_DEFAULT   0
#define PG_CATEG     1
#define PG_STRING    2
#define PG_INTEGER   3
#define PG_DOUBLE    4
#define PG_SYSCOLOR  5
#define PG_COLOR     6
#define PG_LOGIC     7
#define PG_DATE      8
#define PG_FONT      9
#define PG_ARRAY     10
#define PG_ENUM      11
#define PG_FLAG      12
#define PG_SYSINFO   13
#define PG_IMAGE     14
#define PG_CHECK     15
#define PG_SIZE      16
#define PG_FILE      17
#define PG_FOLDER    18
#define PG_LIST      19
#define PG_USERFUN   20
#define PG_PASSWORD  21

#define CY_BITMAP    16
#define CX_BITMAP    16

#include <shlobj.h>
#include <windows.h>
#include <commctrl.h>
#include <hbapi.hpp>
#include <hbvm.hpp>
#include <hbstack.hpp>
#include <hbapiitm.hpp>
#include "mgdefs.hpp"
#include <string>

// TODO: revisar e corrigir as redefinições abaixo
#ifdef HB_STORC
  #undef HB_STORC
#endif
#ifdef HB_STORL
  #undef HB_STORL
#endif
#ifdef HB_STORNI
  #undef HB_STORNI
#endif
#ifdef HB_STORNL
  #undef HB_STORNL
#endif

#define HB_STORC(n, x, y)  hb_storvc(n, x, y)
#define HB_STORL(n, x, y)  hb_storvl(n, x, y)
#define HB_STORNI(n, x, y) hb_storvni(n, x, y)
#define HB_STORNL(n, x, y) hb_storvnl(n, x, y)

#ifdef MAKELONG
  #undef MAKELONG
#endif
#define MAKELONG(a, b)   (static_cast<LONG>((static_cast<WORD>(static_cast<DWORD_PTR>(a) & 0xffff)) | ((static_cast<DWORD>(static_cast<WORD>(static_cast<DWORD_PTR>(b) & 0xffff))) << 16)))

struct PROPGRD
{
   HWND        hPropGrid;
   HWND        hPropEdit;
   HWND        hInfoTitle;
   HWND        hInfoText;
   HWND        hInfoFrame;
   HWND        hHeader;
   HWND        hFramePG;
   HWND        hOkBtn;
   HWND        hApplyBtn;
   HWND        hCancelBtn;
   HWND        hHelpBtn;
   HTREEITEM   hItemActive;
   HTREEITEM   hItemEdit;
   RECT        rcInfo;                    // rectangle for the Info control
   WNDPROC     oldproc;                   // need to remember the old window procedure
   int         cxLeftPG;                  // Left Position PG.
   int         cyTopPG;                   // Top position PG
   int         cxWidthPG;                 // Width Frame PG
   int         cyHeightPG;                // Height frame PG
   int         cxMiddleEdge;
   int         stylePG;
   int         cyHeader;                  // Height header PG
   int         cyPG;                      // Height Tree PG
   int         cyInfo;                    // Height Info PG
   int         cyBtn;                     // Height button PG
   int         nIndent;
   bool        fDisable;
   bool        readonly;
   bool        lInfoShow;
   bool        lOkBtn;
   bool        lApplyBtn;
   bool        lCancelBtn;
   bool        lHelpBtn;
};

using PPROPGRD = PROPGRD *;

struct INSBTN
{
   UINT        fButtonDown;               // is the button2 up/down?
   int         nButton;                   // is the button2 ?
   int         ItemType;                  // type of Item Property
   bool        fMouseDown;                // is the mouse activating the button?
   WNDPROC     oldproc;                   // need to remember the old window procedure
   int         cxLeftEdge, cxRightEdge;   // size of the current window borders.
   int         cyTopEdge, cyBottomEdge;   // given these, we know where to insert our button
   int         uState;
   int         cxButton;
   bool        fMouseActive;
   HWND        himage;
   HTREEITEM   hItem;
   PROPGRD     ppgrd;
};

using PINSBTN = INSBTN *;

struct LPARAMDATA
{
   LPTSTR   ItemName;
   LPTSTR   ItemValue;
   LPTSTR   ItemData;
   bool     ItemDisabled;
   bool     ItemChanged;
   bool     ItemEdit;
   int      ItemType;
   int      ItemID;
   LPTSTR   ItemInfo;
   LPTSTR   ItemValueName;
};

using PLPARAMDATA = LPARAMDATA *;

struct fontProcData
{
   HWND        hWndPG;
   HTREEITEM   hParentItem;
   BYTE        lfCharSet;
   LOGFONT     *plfTreeView;
};

HWND              EditPG(HWND hWnd, RECT rc, HTREEITEM hItem, int ItemType, PROPGRD ppgrd, BOOL DisEdit);
HTREEITEM         GetNextItemPG(HWND TreeHandle, HTREEITEM hTreeItem);
LRESULT CALLBACK  OwnPropGridProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK  OwnFramePgProc(HWND hFramePG, UINT Msg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK  PGEditProc(HWND hEdit, UINT msg, WPARAM wParam, LPARAM lParam);
void              SetIndentLine(HWND hWnd, HTREEITEM hParent, RECT *rc, RECT *rcIndent, int nIndent);
void              _ToggleInfo(HWND hWndPg);

static COLORREF   m_crText, m_crTextCg, m_crBack, m_crBackCg, m_crLine, m_crTextDis;
static HIMAGELIST m_hImgList = nullptr;
static int        m_nHeightHeader = 0;

static bool InsertBtnPG(HWND hWnd, HTREEITEM hItem, int nBtn, int ItemType, PROPGRD pgrd)
{
   INSBTN * pbtn = reinterpret_cast<INSBTN*>(HeapAlloc(GetProcessHeap(), 0, sizeof(INSBTN)));

   if( pbtn == nullptr ) {
      return false;
   }

   pbtn->fButtonDown = FALSE;
   pbtn->nButton = nBtn;
   pbtn->ItemType = ItemType;
   pbtn->hItem = hItem;
   pbtn->cxButton = GetSystemMetrics(SM_CXVSCROLL);
   pbtn->ppgrd = pgrd;
   pbtn->himage = nullptr;       // todo

   // replace the old window procedure with our new one

   pbtn->oldproc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(PGEditProc)));

   // associate our button state structure with the window

   SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pbtn));

   // force the edit control to update its non-client area

   SetWindowPos(hWnd, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER);

   return true;
}

static void GetBtnPG(INSBTN *pbtn, RECT *rect)
{
   if( pbtn->nButton > 0 ) {
      rect->right -= pbtn->cxRightEdge;
      rect->top += pbtn->cyTopEdge;
      rect->bottom -= pbtn->cyBottomEdge;
      rect->left = rect->right - pbtn->cxButton;
      if( pbtn->cxRightEdge > pbtn->cxLeftEdge ) {
         OffsetRect(rect, pbtn->cxRightEdge - pbtn->cxLeftEdge, 0);
      }
   }
}

static void DrawInsBtnPG(HWND hWnd, INSBTN *pbtn, RECT *prect)
{
   HWND hBitmap = pbtn->himage;
   auto hdc = GetWindowDC(hWnd);
   if( pbtn->nButton ) {
      if( pbtn->fButtonDown == TRUE ) {
         DrawEdge(hdc, prect, EDGE_RAISED, BF_RECT | BF_FLAT | BF_ADJUST);
         FillRect(hdc, prect, GetSysColorBrush(COLOR_BTNFACE));
         OffsetRect(prect, 1, 1);
      } else {
         DrawEdge(hdc, prect, EDGE_RAISED, BF_RECT | BF_ADJUST);
         FillRect(hdc, prect, GetSysColorBrush(COLOR_BTNFACE));
      }

      if( hBitmap == nullptr ) {
         SetBkMode(hdc, TRANSPARENT);
         DrawText(hdc, "...", 3, prect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
      } else {
         int      wRow = prect->top;
         int      wCol = prect->left;
         int      wWidth = prect->right - prect->left;
         int      wHeight = prect->bottom - prect->top;

         auto hDCmem = CreateCompatibleDC(hdc);
         BITMAP   bitmap;
         DWORD    dwRaster = SRCCOPY;

         SelectObject(hDCmem, hBitmap);
         GetObject(hBitmap, sizeof(BITMAP), (LPVOID) &bitmap);
         if( wWidth && (wWidth != bitmap.bmWidth || wHeight != bitmap.bmHeight) ) {
            StretchBlt(hdc, wCol, wRow, wWidth, wHeight, hDCmem, 0, 0, bitmap.bmWidth, bitmap.bmHeight, dwRaster);
         } else {
            BitBlt(hdc, wCol, wRow, bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwRaster);
         }

         DeleteDC(hDCmem);
      }
   }

   ReleaseDC(hWnd, hdc);
}

static void LineVert(HDC hDC, int x, int y0, int y1)
{
   POINT Line[2];
   Line[0].x = x;
   Line[0].y = y0;
   Line[1].x = x;
   Line[1].y = y1;
   Polyline(hDC, Line, 2);
}

static void LineHorz(HDC hDC, int x0, int x1, int y)
{
   POINT Line[2];
   Line[0].x = x0;
   Line[0].y = y;
   Line[1].x = x1;
   Line[1].y = y;
   Polyline(hDC, Line, 2);
}

static BOOL InitPropGrd(HWND hWndPG, int col, int row, int width, int height, int indent, int datawidth, int style, bool readonly, bool lInfoShow,
   int cyInfo, int PGHeight, HWND hTitle, HWND hInfo, HWND hFrame, HWND hHeader, HWND hFramePG, HWND hBtnOk, HWND hBtnApply, HWND hBtnCancel, HWND hBtnHelp)
{
   PROPGRD * ppgrd;
   RECT rcCtrl;
   RECT rcButton;
   HWND  hwndButton;
   int num_buttons = 0;
   int x, y, h;
   int cyMargin = GetSystemMetrics(SM_CYFRAME);
   int cxMargin = GetSystemMetrics(SM_CYDLGFRAME);
   int buttonWidth;
   int buttonHeight = 0;
   ppgrd = reinterpret_cast<PROPGRD*>(HeapAlloc(GetProcessHeap(), 0, sizeof(PROPGRD)));

   if( !ppgrd ) {
      return FALSE;
   }
   TreeView_SetIndent(hWndPG, indent);
   indent = TreeView_GetIndent(hWndPG);

   GetWindowRect(hFrame, &rcCtrl);
   CopyRect(&ppgrd->rcInfo, &rcCtrl);

   ppgrd->hPropGrid = hWndPG;
   ppgrd->hPropEdit = nullptr;
   ppgrd->hInfoTitle = hTitle;
   ppgrd->hInfoText = hInfo;
   ppgrd->hInfoFrame = hFrame;
   ppgrd->hHeader = hHeader;
   ppgrd->hFramePG = hFramePG;
   ppgrd->hOkBtn = hBtnOk;
   ppgrd->hApplyBtn = hBtnApply;
   ppgrd->hCancelBtn = hBtnCancel;
   ppgrd->hHelpBtn = hBtnHelp;
   ppgrd->hItemActive = nullptr;
   ppgrd->hItemEdit = nullptr;
   ppgrd->cxLeftPG = col;
   ppgrd->cyTopPG = row;

   ppgrd->cxWidthPG = width;
   ppgrd->cyHeightPG = height;
   ppgrd->cyHeader = m_nHeightHeader;
   ppgrd->cyInfo = cyInfo;
   ppgrd->cyPG = PGHeight;
   ppgrd->cxMiddleEdge = width - datawidth;
   ppgrd->stylePG = style;
   ppgrd->fDisable = false;
   ppgrd->readonly = readonly;
   ppgrd->lInfoShow = lInfoShow;
   ppgrd->lOkBtn = ( hBtnOk != nullptr );
   ppgrd->lApplyBtn = ( hBtnApply != nullptr );
   ppgrd->lCancelBtn = ( hBtnCancel != nullptr );
   ppgrd->lHelpBtn = ( hBtnHelp != nullptr );
   ppgrd->nIndent = indent;
   m_crText = GetSysColor(COLOR_WINDOWTEXT);
   m_crTextCg = GetSysColor(COLOR_APPWORKSPACE);
   m_crTextDis = GetSysColor(COLOR_GRAYTEXT);
   m_crBack = GetSysColor(COLOR_WINDOW);
   m_crBackCg = GetSysColor(COLOR_ACTIVEBORDER);
   m_crLine = m_crTextCg;  //GetSysColor(COLOR_GRAYTEXT);

   if( ppgrd->lOkBtn ) {
      num_buttons++;
   }

   if( ppgrd->lApplyBtn ) {
      num_buttons++;
   }

   if( ppgrd->lCancelBtn ) {
      num_buttons++;
   }

   if( ppgrd->lHelpBtn ) {
      num_buttons++;
   }

   if( num_buttons > 0 ) {
      if( ppgrd->lOkBtn ) {
         // Move the first button "OK" below the tab control.
         hwndButton = ppgrd->hOkBtn;
         GetWindowRect(hwndButton, &rcButton);
         buttonWidth = rcButton.right - rcButton.left;
         buttonHeight = rcButton.bottom - rcButton.top;
         x = ppgrd->cxWidthPG - ( (cxMargin + buttonWidth) * num_buttons ) - GetSystemMetrics(SM_CXDLGFRAME);
         y = ppgrd->cyHeightPG - (buttonHeight+5);
         SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
         num_buttons--;
      }

      if( ppgrd->lCancelBtn ) {
         // Move the second button "Cancel" to the right of the first.
         hwndButton = ppgrd->hCancelBtn;
         if( buttonHeight == 0 ) {
            GetWindowRect(hwndButton, &rcButton);
            buttonWidth = rcButton.right - rcButton.left;
            buttonHeight = rcButton.bottom - rcButton.top;
         }
         x = ppgrd->cxWidthPG - ( (cxMargin + buttonWidth) * num_buttons ) - GetSystemMetrics(SM_CXDLGFRAME);
         y = ppgrd->cyHeightPG - (buttonHeight+5);

         SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
         EnableWindow(hwndButton, FALSE);
         num_buttons--;
      }

      if( ppgrd->lApplyBtn ) {
         // Move the thrid button "Apply" to the right of the second.
         hwndButton = ppgrd->hApplyBtn;
         if( buttonHeight == 0 ) {
            GetWindowRect(hwndButton, &rcButton);
            buttonWidth = rcButton.right - rcButton.left;
            buttonHeight = rcButton.bottom - rcButton.top;
         }
         x = ppgrd->cxWidthPG - ( (cxMargin + buttonWidth) * num_buttons ) - GetSystemMetrics(SM_CXDLGFRAME);
         y = ppgrd->cyHeightPG - (buttonHeight+5);

         SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
         EnableWindow(hwndButton, FALSE);

         num_buttons--;
      }

      if( ppgrd->lHelpBtn ) {
         // Move the thrid button "Help" to the right of the second.
         hwndButton = ppgrd->hHelpBtn;
         if( buttonHeight == 0 ) {
            GetWindowRect(hwndButton, &rcButton);
            buttonWidth = rcButton.right - rcButton.left;
            buttonHeight = rcButton.bottom - rcButton.top;
         }
         x = ppgrd->cxWidthPG - ( (cxMargin + buttonWidth) * num_buttons ) - GetSystemMetrics(SM_CXDLGFRAME);
         y = ppgrd->cyHeightPG - (buttonHeight+5);

         SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
      }

      ppgrd->cyBtn = buttonHeight + cyMargin +  GetSystemMetrics(SM_CYDLGFRAME);

      x = 0;
      y = ppgrd->cyPG+ ppgrd->cyHeader - ppgrd->cyBtn;
      h = ppgrd->cyPG -  ppgrd->cyBtn;

      SetWindowPos(hWndPG, 0, 0, 0, ppgrd->cxWidthPG, h, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER);
      SetWindowPos(hFrame, 0, x,    y,    0, 0, SWP_FRAMECHANGED | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
      SetWindowPos(hTitle, 0, x+10, y+10, 0, 0, SWP_FRAMECHANGED | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
      SetWindowPos(hInfo,  0, x+20, y+26, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);

      GetWindowRect(hFrame, &rcCtrl);
      CopyRect(&ppgrd->rcInfo, &rcCtrl);

   }

   // replace the old window procedure with our new one

   ppgrd->oldproc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWndPG, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnPropGridProc)));
   SetWindowLongPtr(hFramePG, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OwnFramePgProc));

   // associate our button state structure with the window

   SetWindowLongPtr(hFramePG,  GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hWndPG,    GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hHeader,   GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hBtnOk,    GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hBtnCancel,GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hBtnApply, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));
   SetWindowLongPtr(hBtnHelp,  GWLP_USERDATA, reinterpret_cast<LONG_PTR>(ppgrd));

   // force the edit control to update its non-client area

   SetWindowPos(hWndPG, 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER);

   return TRUE;
}

//----------------------------------------------------------------------------------

static BOOL InsertItem(HWND hwndHeader, LPSTR lpsz, int CurrIndex, int Width)   //HBITMAP hBitmap)
{
   HD_ITEM  hdi;

   hdi.mask = HDI_FORMAT | HDI_WIDTH;
   hdi.fmt = HDF_LEFT;        // Left-justify the item.
   if( lpsz ) {
      hdi.mask |= HDI_TEXT;   // The .pszText member is valid.
      hdi.pszText = lpsz;     // The text for the item.
      hdi.cxy = Width;        // The initial width.
      hdi.cchTextMax = lstrlen(hdi.pszText);  // The length of the string.
      hdi.fmt |= HDF_STRING;                    // This item is a string.
   }

   /* to do
   if( hBitmap ) {
      hdi.mask |= HDI_BITMAP; // The .hbm member is valid.
      hdi.cxy = 32;           // The initial width.
      hdi.hbm = hBitmap;      // The handle to the bitmap.
      hdi.fmt |= HDF_BITMAP;  // This item is a bitmap.
   }
   */

   if( Header_InsertItem(hwndHeader, CurrIndex, &hdi) == TRUE ) {
      return TRUE;
   }

   return FALSE;
}

//----------------------------------------------------------------------------------

static HWND CreateHeaderWindow(HWND hwndParent)
{
   HWND hwndHeader;

   if( (hwndHeader = CreateWindowEx(0, WC_HEADER, nullptr, WS_CHILD | WS_BORDER | HDS_BUTTONS | HDS_HORZ, 0, 0, 0, 0, hwndParent, nullptr, GetModuleHandle(nullptr), nullptr)) == nullptr ) {  // No application-defined data.
      return nullptr;
   }

   RECT rcParent;
   GetClientRect(hwndParent, &rcParent);

   WINDOWPOS wp;

   HD_LAYOUT hdl;
   hdl.prc = &rcParent;
   hdl.pwpos = &wp;

   if( Header_Layout(hwndHeader, &hdl) == FALSE ) {
      return nullptr;
   }

   SetWindowPos(hwndHeader, wp.hwndInsertAfter, wp.x, wp.y, wp.cx, wp.cy, wp.flags | SWP_SHOWWINDOW);
   m_nHeightHeader = wp.cy;

   return hwndHeader;
}

static void PropGridPaintButton(HDC hDC, RECT rc, BOOL bExpanded, int nIndent)
{
   int h = rc.bottom - rc.top;
   int x = rc.left + ( nIndent - 9 ) / 2;
   int y = rc.top + ( h - 9 ) / 2 + 1;

   HPEN hBoxPen = CreatePen(PS_SOLID, 1, m_crLine);
   HPEN hMrkPen = CreatePen(PS_SOLID, 1, RGB(0, 0, 0));
   HBRUSH hNewBrush = CreateSolidBrush(RGB(255, 255, 255));

   HPEN hOldPen = static_cast<HPEN>(SelectObject(hDC, hBoxPen));
   HBRUSH hOldBrush = static_cast<HBRUSH>(SelectObject(hDC, hNewBrush));

   // Draw the box

   Rectangle(hDC, x, y, x + 9, y + 9);

   // Now, the - or + sign

   SelectObject(hDC, hMrkPen);

   LineHorz(hDC, x + 2, x + 7, y + 4);     // '-'
   if( !bExpanded ) {
      LineVert(hDC, x + 4, y + 2, y + 7);  // '+'
   }

   SelectObject(hDC, hOldPen);
   SelectObject(hDC, hOldBrush);

   DeleteObject(hMrkPen);
   DeleteObject(hBoxPen);
   DeleteObject(hNewBrush);
}

static LRESULT PropGridOnCustomDraw ( HWND hWnd, LPARAM lParam )
{
   NMHDR          *pNMHDR = ( NMHDR FAR * ) lParam;
   NMTVCUSTOMDRAW *pCD = ( NMTVCUSTOMDRAW * ) pNMHDR;
   HBRUSH         m_brush = nullptr;
   PROPGRD        *ppgrd = ( PROPGRD * ) GetWindowLongPtr(hWnd, GWLP_USERDATA);
   int            nIndent = ppgrd->nIndent;

   LRESULT pResult = CDRF_SKIPDEFAULT;
   DWORD dwDrawStage = pCD->nmcd.dwDrawStage;

   if( dwDrawStage == CDDS_PREPAINT ) {
      m_hImgList = TreeView_GetImageList(hWnd, TVSIL_NORMAL);
      pResult = CDRF_NOTIFYITEMDRAW;
   } else if( dwDrawStage == CDDS_ITEMPREPAINT ) {
      pResult = CDRF_NOTIFYPOSTPAINT;
   } else if( dwDrawStage == CDDS_ITEMPOSTPAINT ) {
      HDC         hDC = pCD->nmcd.hdc;
      HPEN        hLinPen, hOldPen;
      HBRUSH      hBackBrush, hOldBrush, hIndentBrush;
      HTREEITEM   hItem = ( HTREEITEM ) pCD->nmcd.dwItemSpec, hParent = TreeView_GetParent(hWnd, hItem);
      RECT        rc = pCD->nmcd.rc;
      TV_DISPINFO tvdi;
      LPARAMDATA  *pItemData = nullptr;
      HWND        hPropEdit;
      LONG        hFont;
      RECT        rcText, rcItem, rcProp, rcEdit, rcCheck, rcIndent;
      TCHAR       szText[255];
      TCHAR       PropText[1024];
      TCHAR       PropInfo[1024];
      int         iImage, cx, cy, iCheck, style;

      if( rc.bottom >= SendMessage(hWnd, TVM_GETITEMHEIGHT, 0, 0) ) {
         hLinPen = CreatePen(PS_SOLID, 1, m_crLine);

         hOldPen = static_cast<HPEN>(SelectObject(hDC, hLinPen));
         hOldBrush = static_cast<HBRUSH>(SelectObject(hDC, m_brush));

         rcItem = rc;
         rcProp = rc;
         rcIndent = rc;
         rcProp.left = ppgrd->cxMiddleEdge + 1;
         rcProp.top += 1;
         rcText = rc;
         rcText.right = ppgrd->cxMiddleEdge - 1;
         rcIndent.right = 0;

         tvdi.item.mask = TVIF_CHILDREN | TVIF_HANDLE | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_STATE | TVIF_TEXT | TVIF_PARAM;
         tvdi.item.hItem = hItem;
         tvdi.item.pszText = szText;
         tvdi.item.cchTextMax = 255;
         if( !TreeView_GetItem(hWnd, &tvdi.item) ) {
            SelectObject(hDC, hOldBrush);
            SelectObject(hDC, hOldPen);
            DeleteObject(hLinPen);
            pResult = CDRF_SKIPDEFAULT;
            hb_retnl( pResult );
         }

         if( tvdi.item.lParam ) {
            pItemData = ( LPARAMDATA * ) tvdi.item.lParam;
            if( pItemData ) {
               strcpy(szText, pItemData->ItemName);
               if( pItemData->ItemType == PG_PASSWORD ) {
                     strcpy(PropText, "*****");
                  } else
                     strcpy(PropText, pItemData->ItemValue);

               strcpy(PropInfo, pItemData->ItemInfo);
            }
         }

         if( pCD->nmcd.uItemState & CDIS_FOCUS ) {
            ppgrd->hItemActive = hItem;
            ppgrd->fDisable = pItemData->ItemDisabled;
         }

         if( hParent ) {
            SetIndentLine(hWnd, hParent, &rc, &rcIndent, nIndent);
         }

         // Clear the background

         if( pCD->nmcd.uItemState & CDIS_FOCUS ) {
            hBackBrush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));
         } else {
            if( tvdi.item.cChildren == 1 ) {
               hBackBrush = CreateSolidBrush(m_crBackCg);
            } else {
               hBackBrush = CreateSolidBrush(m_crBack);
            }
         }

         hIndentBrush = CreateSolidBrush(m_crBackCg);
         FillRect(hDC, &rcItem, hBackBrush);
         FillRect(hDC, &rcIndent, hIndentBrush);
         DeleteObject(hBackBrush);
         DeleteObject(hIndentBrush);

         // Draw the grid lines

         DrawEdge(hDC, &rcItem, BDR_RAISEDOUTER, BF_BOTTOM);
         DrawEdge(hDC, &rcProp, BDR_SUNKENOUTER, BF_LEFT);

         // DrawEdge(hDC, &rcItem, BDR_SUNKENINNER, BF_BOTTOM);
         // DrawEdge(hDC, &rcProp, BDR_SUNKENINNER, BF_LEFT);
         //
         // Paint the buttons, if any
         //

         if( GetWindowLongPtr(hWnd, GWL_STYLE) & TVS_HASBUTTONS ) {
            if( tvdi.item.cChildren == 1 ) {
               PropGridPaintButton(hDC, rc, tvdi.item.state & TVIS_EXPANDED, nIndent);
            } else if( tvdi.item.cChildren == I_CHILDRENCALLBACK ) {
               PropGridPaintButton(hDC, rc, FALSE, nIndent);
            }

            // If we have buttons we must make room for them

            rc.left += nIndent;
            rcText.left = rc.left;
         }

         // Check if we have any check button to draw

         iCheck = tvdi.item.state >> 12;
         if( iCheck > 0 ) {
            rcCheck = rcProp;
            rcCheck.left += 4;
            rcCheck.top += 1;
            rcCheck.right = rcCheck.left + nIndent;
            rcCheck.bottom -= 2;

            if( iCheck == 1 ) {
               style = DFCS_BUTTONCHECK;
            } else { //if( iCheck == 2 )
               style = DFCS_BUTTONCHECK | DFCS_CHECKED;
            }

            if( pItemData->ItemDisabled ) {
               style = style | DFCS_INACTIVE;
            }

            DrawFrameControl( hDC, &rcCheck, DFC_BUTTON, style );

            rcProp.left = rcCheck.right;
         }

         // Check if we have any normal icons to draw

         if( m_hImgList ) {
            if( pCD->nmcd.uItemState & CDIS_SELECTED ) {
               iImage = tvdi.item.iSelectedImage;
            } else {
               iImage = tvdi.item.iImage;
            }

            if( iImage > 0 ) {
               ImageList_GetIconSize(m_hImgList, &cx, &cy);
               ImageList_DrawEx(m_hImgList, iImage - 1, hDC, rcProp.left + 5, rcProp.top + 1, 0, 0, CLR_DEFAULT, CLR_DEFAULT, ILD_NORMAL);
               rcProp.left += cx + 7;
            }
         }

         if( pCD->nmcd.uItemState & CDIS_FOCUS ) {
            SetTextColor(hDC, RGB(255, 255, 255));
            SetBkColor(hDC, GetSysColor(COLOR_HIGHLIGHT));
         } else {
            if( tvdi.item.cChildren == 1 ) {
               SetTextColor(hDC, m_crText);
               SetBkColor(hDC, m_crBackCg);
            } else {
               SetTextColor(hDC, m_crText);
               SetBkColor(hDC, m_crBack);
            }
         }

         if( !(ppgrd->hItemEdit == hItem) ) {
            if( pCD->nmcd.uItemState & CDIS_FOCUS ) {
               if( ppgrd->hItemEdit ) {
                  PostMessage(ppgrd->hPropEdit, WM_CLOSE, 0, 0);
                  ppgrd->hItemEdit = nullptr;
               }

               SetWindowText(ppgrd->hInfoTitle, szText);
               SetWindowText(ppgrd->hInfoText, PropInfo);

               if( !(pItemData->ItemDisabled) && (!(ppgrd->readonly)) && !(pItemData->ItemType == PG_CHECK) ) {
                  rcEdit = rcProp;
                  if( pItemData->ItemType == PG_SYSCOLOR ) {
                     rcEdit.left = ppgrd->cxMiddleEdge;
                  }

                  hFont = SendMessage(hWnd, WM_GETFONT, 0, 0);
                  hPropEdit = EditPG(hWnd, rcEdit, hItem, pItemData->ItemType, *ppgrd, pItemData->ItemEdit);
                  rcEdit = rcProp;
                  if( ppgrd->hItemEdit ) {
                     PostMessage(ppgrd->hPropEdit, WM_CLOSE, 0, 0);
                  }

                  ppgrd->hItemEdit = hItem;
                  SendMessage(hPropEdit, WM_SETFONT, (WPARAM) hFont, TRUE);
                  ppgrd->hPropEdit = hPropEdit;
               }
            }
         }

         // Calculate the text drawing rectangle

         rcProp.left += 4;

         DrawText(hDC, szText, -1, &rcText, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER | DT_CALCRECT);
         DrawText(hDC, PropText, -1, &rcProp, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER | DT_CALCRECT);
         rcText.right = ppgrd->cxMiddleEdge - 1;

         // Now, draw the text

         DrawText(hDC, szText, -1, &rcText, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER | DT_END_ELLIPSIS);

         if( pItemData->ItemDisabled ) {
            SetTextColor(hDC, m_crTextDis);
         }

         if( pItemData->ItemChanged ) {
            HFONT    hFontBold, hOldFont;
            HFONT    hFont = reinterpret_cast<HFONT>(SendMessage(hWnd, WM_GETFONT, 0, 0));
            LOGFONT  lf{};
            GetObject(hFont, sizeof(LOGFONT), &lf);
            lf.lfWeight |= FW_BOLD;

            hFontBold = CreateFontIndirect(&lf);
            hOldFont = static_cast<HFONT>(SelectObject(pCD->nmcd.hdc, hFontBold));

            DeleteObject(hFontBold);
            DrawText(hDC, PropText, -1, &rcProp, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER | DT_CALCRECT);
            DrawText(hDC, PropText, -1, &rcProp, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER);
            SelectObject(pCD->nmcd.hdc, hOldFont);
         } else {
            DrawText(hDC, PropText, -1, &rcProp, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER);
         }

         // Clean up

         SelectObject(hDC, hOldBrush);
         SelectObject(hDC, hOldPen);

         DeleteObject(hBackBrush);
         DeleteObject(hLinPen);

         pResult = CDRF_SKIPDEFAULT;
      }
   } else {
      pResult = CDRF_SKIPDEFAULT;
   }

   return pResult;
}

/*
INITPROPGRID() -->
*/
HB_FUNC( INITPROPGRID )
{
   INITCOMMONCONTROLSEX icex;
   icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
   icex.dwICC = ICC_TREEVIEW_CLASSES | ICC_DATE_CLASSES | ICC_USEREX_CLASSES;
   InitCommonControlsEx(&icex);

   int InfoStyle;
   int PGHeight;

   int x = hb_parni(2);
   int y = hb_parni(3);
   int w = hb_parni(4);
   int h = hb_parni(5);

   auto hwndParent = hmg_par_HWND(1);

   int style = WS_VISIBLE | WS_TABSTOP | WS_CHILD | TVS_HASBUTTONS | TVS_FULLROWSELECT | TVS_NOHSCROLL | TVS_SHOWSELALWAYS;
   if( hb_parl(12) ) {
      style = style  | TVS_SINGLEEXPAND;
   }

   int iHeight = hb_parni(10);

   auto hArray = hb_param(11, Harbour::Item::ARRAY);
   auto MsgArray = hb_param(17, Harbour::Item::ARRAY);

   HWND hFramePG = CreateWindowEx(WS_EX_CONTROLPARENT, "button", "", WS_CHILD | BS_GROUPBOX | WS_VISIBLE, x, y, w, h, hwndParent, nullptr, GetModuleHandle(nullptr), nullptr);

   SetProp(hFramePG, "oldframepgproc", reinterpret_cast<HWND>(GetWindowLongPtr(hFramePG, GWLP_WNDPROC)));

   HWND hHeader;

   if( hb_arrayLen(hArray) > 0 ) {
      hHeader = CreateHeaderWindow(hFramePG);
      InsertItem(hHeader, (char *) hb_arrayGetCPtr(hArray, 1), 1, w - hb_parni(7) + 3);
      InsertItem(hHeader, (char *) hb_arrayGetCPtr(hArray, 2), 2, w);
   } else {
      style = style | WS_BORDER;
      hHeader = nullptr;
      m_nHeightHeader = 0;
   }

   if( hb_parl(9) ) {
      PGHeight = h - iHeight - m_nHeightHeader;
      InfoStyle = WS_CHILD | WS_VISIBLE;
   } else {
      PGHeight = h - m_nHeightHeader;
      InfoStyle = WS_CHILD;
   }

   x = 0;
   y = 0;

   HWND hWndPG = CreateWindowEx(WS_EX_CLIENTEDGE, WC_TREEVIEW, "", style, x, y + m_nHeightHeader, w, PGHeight, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   HWND hFrame = CreateWindowEx(WS_EX_TRANSPARENT, "static", "", InfoStyle | SS_OWNERDRAW | SS_NOTIFY | WS_BORDER, /* SS_SUNKEN , */ x, y + PGHeight + m_nHeightHeader, w, iHeight, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   HWND hTitle = CreateWindowEx(WS_EX_TRANSPARENT, "static", "", InfoStyle | SS_NOTIFY, x + 10, y + PGHeight + m_nHeightHeader + 10, w - 20, 20, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   HWND hInfo = CreateWindowEx(WS_EX_TRANSPARENT, "static", "", InfoStyle | SS_NOTIFY, x + 20, y + PGHeight + m_nHeightHeader + 26, w - 30, iHeight - 36, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);

   HWND hBtnOk = nullptr;
   HWND hBtnApply = nullptr;
   HWND hBtnCancel = nullptr;
   HWND hBtnHelp = nullptr;

   if( hb_parl(13) ) {
      hBtnOk = CreateWindowEx(0, "button", hb_arrayGetCPtr(MsgArray, 4), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE, 0, 0, 70, 20, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   }

   if( hb_parl(14) ) {
      hBtnApply = CreateWindowEx(0, "button", hb_arrayGetCPtr(MsgArray, 1), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE, 0, 0, 70, 20, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   }

   if( hb_parl(15) ) {
      hBtnCancel = CreateWindowEx(0, "button", hb_arrayGetCPtr(MsgArray, 3), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE, 0, 0, 70, 20, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   }

   if( hb_parl(16) ) {
      hBtnHelp = CreateWindowEx(0, "button", hb_arrayGetCPtr(MsgArray, 2), BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE, 0, 0, 70, 20, hFramePG, nullptr, GetModuleHandle(nullptr), nullptr);
   }

   InitPropGrd(hWndPG, x, y, w, h, hb_parni(6), hb_parni(7), style, hb_parl(8), hb_parl(9), iHeight, PGHeight, hTitle, hInfo, hFrame, hHeader, hFramePG, hBtnOk, hBtnApply, hBtnCancel, hBtnHelp);

   hb_reta(10);
   hmg_storvhandle(hWndPG, -1, 1);
   hmg_storvhandle(hTitle, -1, 2);
   hmg_storvhandle(hInfo, -1, 3);
   hmg_storvhandle(hFrame, -1, 4);
   hmg_storvhandle(hHeader, -1, 5);
   hmg_storvhandle(hFramePG, -1, 6);
   hmg_storvhandle(hBtnOk, -1, 7);
   hmg_storvhandle(hBtnApply, -1, 8);
   hmg_storvhandle(hBtnCancel, -1, 9);
   hmg_storvhandle(hBtnHelp, -1, 10);
}

LRESULT CALLBACK OwnPropGridProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   // get the button state structure

   PROPGRD * ppgrd = ( PROPGRD * ) GetWindowLongPtr(hWnd, GWLP_USERDATA);
   WNDPROC OldWndProc = ppgrd->oldproc;

   switch( Msg ) {
      case WM_DESTROY:
      {
         OldWndProc = ppgrd->oldproc;
         HeapFree(GetProcessHeap(), 0, ppgrd);
         return CallWindowProc(OldWndProc, hWnd, Msg, wParam, lParam);
      }

      case WM_DRAWITEM:
      {
         LPDRAWITEMSTRUCT lpdis = ( LPDRAWITEMSTRUCT ) lParam;
         if( lpdis->itemID == static_cast<UINT>(-1) ) {  // empty item
            break;
         }

         HDC hDC = lpdis->hDC;
         RECT rc = lpdis->rcItem;
         HIMAGELIST himl = ( HIMAGELIST ) lpdis->itemData;
         int iImage = lpdis->itemID;
         COLORREF clrBackground;
         COLORREF clrForeground;
         if( lpdis->itemState & ODS_SELECTED ) {
            clrForeground = SetTextColor(hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
            clrBackground = SetBkColor(hDC, GetSysColor(COLOR_HIGHLIGHT));
         } else {
            clrForeground = SetTextColor(hDC, m_crText);
            clrBackground = SetBkColor(hDC, m_crBack);
         }

         rc.left += 2;

         if( himl ) {
            ImageList_Draw(himl, iImage, hDC, rc.left, rc.top, ILD_NORMAL);
            int cx, cy;
            ImageList_GetIconSize(himl, &cx, &cy);
            rc.left += cx;
         }

         char achTemp[256]; // temporary buffer
         SendMessage(lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) (LPCSTR) achTemp);
         rc.left += 6;
         if( lpdis->itemState & ODS_COMBOBOXEDIT ) {
            rc.right += 20;
            rc.bottom += 4;
            rc.top -= 4;
         } else {
            rc.right += 10;
            rc.bottom += 2;
         }

         DrawText(hDC, achTemp, -1, &rc, DT_LEFT | DT_NOPREFIX | DT_SINGLELINE | DT_VCENTER);

         // Restore the previous colors.

         SetTextColor(lpdis->hDC, clrForeground);
         SetBkColor(lpdis->hDC, clrBackground);
         break;
      }

      case WM_GETDLGCODE:
      {
         return DLGC_WANTALLKEYS;
      }

      case WM_NCCALCSIZE:
      {
         RECT rect;
         GetWindowRect(hWnd, &rect);
         OffsetRect(&rect, -rect.left, -rect.top);
         CallWindowProc(ppgrd->oldproc, hWnd, Msg, wParam, lParam);
         ppgrd->cxLeftPG = rect.left;
         // ppgrd->cxRightEdge = rect.right;
         ppgrd->cyTopPG = rect.top;
         // ppgrd->cyBottomEdge = rect.bottom;
         return 0;
      }

      case WM_NCPAINT:
      {
         m_crBack = GetSysColor(COLOR_WINDOW);
         CallWindowProc(ppgrd->oldproc, hWnd, Msg, wParam, lParam);
         return 0;
      }

      case WM_VSCROLL:
      {
         PostMessage(ppgrd->hPropEdit, WM_CLOSE, 0, 0);
         ppgrd->hItemEdit = nullptr;
         SetFocus(hWnd);
         break;
      }

      case WM_LBUTTONDBLCLK:
      {
         TV_ITEM tvi{};
         tvi.mask = TVIF_HANDLE | TVIF_STATE;
         tvi.stateMask = TVIS_STATEIMAGEMASK;
         tvi.hItem = ppgrd->hItemActive;
         TreeView_GetItem(hWnd, &tvi);

         int iCheck = tvi.state >> 12;

         if( iCheck > 0 && !ppgrd->fDisable ) {
            iCheck = iCheck == 2 ? 1 : 2;
            tvi.state = INDEXTOSTATEIMAGEMASK(iCheck);
            TreeView_SetItem(hWnd, &tvi);
            PostMessage(hWnd, WM_COMMAND, MAKEWPARAM(iCheck, BN_CLICKED), (LPARAM) ppgrd->hItemActive);
         }
         break;
      }

      case WM_LBUTTONUP:
      case WM_LBUTTONDOWN:
      case WM_KILLFOCUS:
      {
         if( !(reinterpret_cast<HWND>(wParam) == ppgrd->hPropEdit) ) {
            PostMessage(ppgrd->hPropEdit, WM_CLOSE, 0, 0);
            ppgrd->hItemEdit = nullptr;
         }
         break;
      }

      case NM_SETFOCUS:
      {
         if( !(reinterpret_cast<HWND>(wParam) == ppgrd->hPropEdit) ) {
            PostMessage(ppgrd->hPropEdit, WM_CLOSE, 0, 0);
            ppgrd->hItemEdit = nullptr;
         }
         break;
      }

      case WM_COMMAND:
      case WM_CHAR:
      case WM_NOTIFY:
      {
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OPROPGRIDEVENTS"));
         }

         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hWnd);
            hb_vmPushLong(Msg);
            hb_vmPushLong(wParam);
            hb_vmPushLong(lParam);
            hmg_vmPushHandle(ppgrd->hItemActive);
            hmg_vmPushHandle(ppgrd->hPropEdit);
            hb_vmDo(6);
         }

         long int r = hb_parnl(-1);

         return (r != 0) ? r : CallWindowProc(OldWndProc, hWnd, Msg, wParam, lParam);
      }
   }

   return CallWindowProc(OldWndProc, hWnd, Msg, wParam, lParam);
}

LRESULT CALLBACK OwnFramePgProc(HWND hFramePG, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   PROPGRD * ppgrd = ( PROPGRD * ) GetWindowLongPtr(hFramePG, GWLP_USERDATA);
   WNDPROC OldWndProc = reinterpret_cast<WNDPROC>(reinterpret_cast<LONG_PTR>(GetProp(hFramePG, "oldframepgproc")));

   switch( Msg ) {
      case WM_DESTROY:
      {
         SetWindowLongPtr(hFramePG, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(OldWndProc));
         RemoveProp(hFramePG, "oldframepgproc");
         break;
      }

      case WM_DRAWITEM:
      {
         auto hDC = GetWindowDC(GetParent(hFramePG));
         RECT rc = ppgrd->rcInfo;
         rc.left   += 1;
         rc.right  -= 1;
         rc.bottom -= 1;
         FillRect(hDC, &rc, GetSysColorBrush(COLOR_BTNFACE));
         ReleaseDC(hFramePG, hDC);
         break;
      }

      case WM_COMMAND:
      {
         if( lParam != 0 && HIWORD(wParam) == BN_CLICKED ) {
            if( ppgrd ) {
               if( pSymbol == nullptr ) {
                  pSymbol = hb_dynsymSymbol(hb_dynsymGet("PGBTNEVENTS"));
               }

               if( pSymbol != nullptr ) {
                  hb_vmPushSymbol(pSymbol);
                  hb_vmPushNil();
                  hmg_vmPushHandle(ppgrd->hPropGrid);
                  hb_vmPushLong(lParam);
                  hb_vmDo(2);
               }

               long int r = hb_parnl(-1);

               return (r != 0) ? r : CallWindowProc(OldWndProc, hFramePG, Msg, wParam, lParam);
            }
         } else {
            return CallWindowProc(OldWndProc, hFramePG, Msg, wParam, lParam);
         }
         break;
      }

      case WM_NOTIFY:
      {
         NMHDR * nmhdr = ( NMHDR * ) lParam;
         HWND hWndHD = nmhdr->hwndFrom;

         switch( nmhdr->code ) {
            //case HDN_ENDTRACK:
            //   break;

            case HDN_ITEMCHANGED:
            {
               HD_ITEM hdi;
               hdi.mask = HDI_WIDTH;
               Header_GetItem(hWndHD, 0, &hdi);
               int dWidth = ppgrd->cxMiddleEdge - hdi.cxy;
               ppgrd->cxMiddleEdge = hdi.cxy - 3;
               Header_GetItem(hWndHD, 1, &hdi);
               hdi.cxy += dWidth;
               RedrawWindow(ppgrd->hPropGrid, nullptr, nullptr, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW);
               break;
            }

            //case HDN_BEGINTRACK:
            //   break;

            //case HDN_ITEMCLICK:
            //   break;

            case HDN_ITEMDBLCLICK:
            {
               LPNMHEADER nmh = ( LPNMHEADER ) lParam;
               if( nmh->iItem == 0 ) {
                  if( pSymbol == nullptr ) {
                     pSymbol = hb_dynsymSymbol(hb_dynsymGet("EXPANDPG"));
                  }

                  if( pSymbol != nullptr ) {
                     hb_vmPushSymbol(pSymbol);
                     hb_vmPushNil();
                     hmg_vmPushHandle(ppgrd->hPropGrid);
                     hb_vmPushLong(0);
                     hb_vmDo(2);
                  }
               } else {
                  _ToggleInfo(ppgrd->hPropGrid);
               }
               break;
            }

            case NM_CUSTOMDRAW:
            {
               if( hWndHD == ppgrd->hPropGrid ) {
                  return PropGridOnCustomDraw(hWndHD, lParam);
               }
               break;
            }
         }
      }
   }

   return CallWindowProc(OldWndProc, hFramePG, Msg, wParam, lParam);
}

/*
PROPGRIDONCUSTOMDRAW() -->
*/
HB_FUNC( PROPGRIDONCUSTOMDRAW )
{
   hb_retnl(PropGridOnCustomDraw (hmg_par_HWND(1), (LPARAM) hb_parnl(2)));
}

void SetIndentLine(HWND hWnd, HTREEITEM hParent, RECT * rc, RECT * rcIndent, int nIndent)
{
   HTREEITEM   hGrand;

   // Check if the parent has a parent itself and process it

   hGrand = TreeView_GetParent(hWnd, hParent);
   if( hGrand ) {
      SetIndentLine(hWnd, hGrand, rc, rcIndent, nIndent);
   }

   rc->left += nIndent;
   rcIndent->right += nIndent;
}

/*
GETNOTIFYPROPGRIDITEM() -->
*/
HB_FUNC( GETNOTIFYPROPGRIDITEM )
{
   NMHDR          *pNMHDR = ( NMHDR FAR * ) HB_PARNL(1);
   NMTVCUSTOMDRAW *pCD = ( NMTVCUSTOMDRAW * ) pNMHDR;
   HTREEITEM      hItem = ( HTREEITEM ) pCD->nmcd.dwItemSpec;
   hmg_ret_HTREEITEM(hItem);
}

/*
ADDPGITEM() -->
*/
HB_FUNC( ADDPGITEM )
{
   auto hWndTV = hmg_par_HWND(1);

   auto pData = static_cast<LPARAMDATA*>(hb_xgrab((sizeof(LPARAMDATA))));
   ZeroMemory(pData, sizeof(LPARAMDATA));
   pData->ItemName      = hb_strndup(hb_parc(7), 255);
   pData->ItemValue     = hb_strndup(hb_parc(8), 1024);
   pData->ItemData      = hb_strndup(hb_parc(9), 1024);
   pData->ItemDisabled  = hb_parl(10);
   pData->ItemChanged   = hb_parl(11);
   pData->ItemEdit      = hb_parl(12);
   pData->ItemType      = hb_parni(13);
   pData->ItemID        = hb_parni(14);
   pData->ItemInfo      = hb_strndup(hb_parc(15), 1024);
   pData->ItemValueName = hb_strndup(hb_parc(16), 255);

   TV_ITEM tvi;
   tvi.mask           = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_PARAM | TVIF_STATE;
   tvi.stateMask      = TVIS_STATEIMAGEMASK;
   tvi.pszText        = (char *) hb_parc(3);
   tvi.cchTextMax     = 255;
   tvi.iImage         = hb_parni(4);
   tvi.iSelectedImage = hb_parni(5);
   tvi.state          = INDEXTOSTATEIMAGEMASK(hb_parni(6));
   tvi.lParam         = ( LPARAM ) pData;

   TV_INSERTSTRUCT is;
   #if ( defined(__BORLANDC__) && __BORLANDC__ <= 1410 )
   is.DUMMYUNIONNAME.item = tvi;
   #else
   is.item = tvi;
   #endif

   auto hPrev = hmg_par_HTREEITEM(2);

   if( hPrev == 0 ) {
      is.hInsertAfter = hPrev;
      is.hParent = nullptr;
   } else {
      is.hInsertAfter = TVI_LAST;
      is.hParent = hPrev;
   }

   hmg_ret_HTREEITEM(TreeView_InsertItem(hWndTV, &is));
}

static void Pg_SetData(HWND hWnd, HTREEITEM hItem, LPCTSTR cValue, LPCTSTR cData, BOOL lData)
{
   HWND TreeHandle = hWnd;
   HTREEITEM TreeItemHandle = hItem;

   TV_ITEM TreeItem{};
   TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
   TreeItem.hItem = TreeItemHandle;
   TreeView_GetItem(TreeHandle, &TreeItem);

   if( TreeItem.lParam ) {
      LPARAMDATA * pData = ( LPARAMDATA * ) TreeItem.lParam;
      if( pData ) {
         if( !(strcmp(pData->ItemValue, cValue) == 0) ) {
            pData->ItemValue = hb_strndup(cValue, 1024);
            pData->ItemChanged = true;
            PostMessage(TreeHandle, WM_COMMAND, MAKEWPARAM(pData->ItemType, EN_CHANGE), (LPARAM) TreeItemHandle);
         }

         if( !(strcmp(pData->ItemData, cData) == 0) && lData ) {
            pData->ItemData = hb_strndup(cData, 1024);
         }
      }
   }
}

/*
PG_SETDATAITEM() -->
*/
HB_FUNC( PG_SETDATAITEM )
{
   Pg_SetData(hmg_par_HWND(1), hmg_par_HTREEITEM(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), hb_parl(5));
}

/*
PG_ENABLEITEM() -->
*/
HB_FUNC( PG_ENABLEITEM )     //   Pg_EnableItem(TreeHandle, TreeItemHandle, lEnable);
{
   auto TreeHandle = hmg_par_HWND(1);
   auto TreeItemHandle = hmg_par_HTREEITEM(2);

   TV_ITEM TreeItem{};
   TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
   TreeItem.hItem = TreeItemHandle;
   TreeView_GetItem(TreeHandle, &TreeItem);

   if( TreeItem.lParam ) {
      LPARAMDATA * pData = ( LPARAMDATA * ) TreeItem.lParam;
      if( pData ) {
         pData->ItemDisabled = !hb_parl(3);
         PostMessage(TreeHandle, WM_SETREDRAW, TRUE, 0);
      }
   }
}

/*
PG_CHANGEITEM() -->
*/
HB_FUNC( PG_CHANGEITEM )     //   Pg_ChangeItem(TreeHandle, TreeItemHandle, lChange);
{
   auto TreeHandle = hmg_par_HWND(1);
   auto TreeItemHandle = hmg_par_HTREEITEM(2);

   TV_ITEM TreeItem{};
   TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
   TreeItem.hItem = TreeItemHandle;
   TreeView_GetItem(TreeHandle, &TreeItem);

   if( TreeItem.lParam ) {
      LPARAMDATA * pData = ( LPARAMDATA * ) TreeItem.lParam;
      if( pData ) {
         pData->ItemChanged = hb_parl(3);
      }
   }
}

/*
PG_GETITEM(HWND, HTREEITEM, ntype) --> xvalue
*/
HB_FUNC( PG_GETITEM )
{
   auto TreeHandle = hmg_par_HWND(1);
   auto TreeItemHandle = hmg_par_HTREEITEM(2);

   TV_ITEM TreeItem{};
   TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
   TreeItem.hItem = TreeItemHandle;
   TreeView_GetItem(TreeHandle, &TreeItem);

   LPARAMDATA * pData = ( LPARAMDATA * ) TreeItem.lParam;

   switch( hmg_par_int(3) ) {
      case 0:
      {
         hb_reta(10);
         HB_STORC(pData->ItemName, -1, 1);
         HB_STORC(pData->ItemValue, -1, 2);
         HB_STORC(pData->ItemData, -1, 3);
         HB_STORL(pData->ItemDisabled, -1, 4);
         HB_STORL(pData->ItemChanged, -1, 5);
         HB_STORL(pData->ItemEdit, -1, 6);
         HB_STORNI(pData->ItemType, -1, 7);
         HB_STORNI(pData->ItemID, -1, 8);
         HB_STORC(pData->ItemInfo, -1, 9);
         HB_STORC(pData->ItemValueName, -1, 10);
         break;
      }

      case 1:
      {
         hb_retc(pData->ItemName);
         break;
      }

      case 2:
      {
         hb_retc(pData->ItemValue);
         break;
      }

      case 3:
      {
         hb_retc(pData->ItemData);
         break;
      }

      case 4:
      {
         hb_retl(pData->ItemDisabled);
         break;
      }

      case 5:
      {
         hb_retl(pData->ItemChanged);
         break;
      }

      case 6:
      {
         hb_retni( pData->ItemEdit );
         break;
      }

      case 7:
      {
         hb_retni( pData->ItemType );
         break;
      }

      case 8:
      {
         hb_retni( pData->ItemID );
         break;
      }

      case 9:
      {
         hb_retc(pData->ItemInfo);
         break;
      }

      case 10:
      {
         hb_retc(pData->ItemValueName);
         break;
      }

      default:
      {
         hb_retc(pData->ItemValue);
      }
   }
}

HTREEITEM GetNextItemPG(HWND TreeHandle, HTREEITEM hTreeItem)
{
   HTREEITEM hTreeItemBack = hTreeItem;

   hTreeItem = TreeView_GetChild(TreeHandle, hTreeItem);

   if( hTreeItem == nullptr ) {
      hTreeItem = TreeView_GetNextSibling(TreeHandle, hTreeItemBack);
   }

   if( hTreeItem == nullptr ) {
      while( hTreeItem == nullptr && hTreeItemBack != nullptr ) {
         hTreeItemBack = TreeView_GetParent(TreeHandle, hTreeItemBack);
         hTreeItem = TreeView_GetNextSibling(TreeHandle, hTreeItemBack);
      }
   }

   return hTreeItem;
}

/*
PG_GETNEXTITEM(HWND, HTREEITEM) --> HTREEITEM
*/
HB_FUNC( PG_GETNEXTITEM )
{
   hmg_ret_HTREEITEM(GetNextItemPG(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
PG_GETROOT(HWND) --> HTREEITEM
*/
HB_FUNC( PG_GETROOT )
{
   hmg_ret_HTREEITEM(TreeView_GetRoot(hmg_par_HWND(1)));
}

/*
PG_ENSUREVISIBLE(HWND, HTREEITEM) --> .T.|.F.
*/
HB_FUNC( PG_ENSUREVISIBLE )
{
   hb_retl(TreeView_EnsureVisible(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
PG_ISVISIBLE(HWND, HTREEITEM) --> .T.|.F.
*/
HB_FUNC( PG_ISVISIBLE )
{
   bool lVisible = false;

   auto TreeHandle = hmg_par_HWND(1);
   auto ItemHandle = hmg_par_HTREEITEM(2);
   HTREEITEM ItemHdl = TreeView_GetFirstVisible(TreeHandle);

   while( ItemHdl != nullptr ) {
      if( ItemHdl == ItemHandle ) {
         lVisible = true;
         break;
      }
      ItemHdl = TreeView_GetNextVisible(TreeHandle, ItemHdl);
   }

   hb_retl(lVisible);
}

/*
PG_SEARCHID(HWND, id) --> HTREEITEM
*/
HB_FUNC( PG_SEARCHID ) // PG_SearchID(hWndPG, nID)
{
   LPARAMDATA * pData;
   TV_ITEM TreeItem{};
   auto TreeHandle = hmg_par_HWND(1);
   auto nID = hmg_par_int(2);

   HTREEITEM TreeItemHandle = TreeView_GetRoot(TreeHandle);

   while( TreeItemHandle != nullptr ) {
      TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
      TreeItem.hItem = TreeItemHandle;
      TreeView_GetItem(TreeHandle, &TreeItem);
      pData = ( LPARAMDATA * ) TreeItem.lParam;

      if( pData->ItemID == nID ) {
         break;
      }

      TreeItemHandle = GetNextItemPG(TreeHandle, TreeItemHandle);
   }

   hmg_ret_HTREEITEM(TreeItemHandle);
}

/*
PG_SEARCHCATEGORY(HWND, cName) --> HTREEITEM
*/
HB_FUNC( PG_SEARCHCATEGORY ) // PG_SearchCategory(hWndPG, cCategory)
{
   LPARAMDATA * pData;
   TV_ITEM TreeItem{};
   auto TreeHandle = hmg_par_HWND(1);
   LPTSTR cName = hb_strndup(hb_parc(2), 255); // temporary buffer
   HTREEITEM TreeItemHandle = TreeView_GetRoot(TreeHandle);
   while( TreeItemHandle != nullptr ) {
      TreeItem.mask = TVIF_HANDLE | TVIF_PARAM;
      TreeItem.hItem = TreeItemHandle;
      TreeView_GetItem(TreeHandle, &TreeItem);
      pData = ( LPARAMDATA * ) TreeItem.lParam;
      if( strcmp(pData->ItemName, cName) == 0 ) {
         break;
      }

      TreeItemHandle = GetNextItemPG(TreeHandle, TreeItemHandle);
   }

   hmg_ret_HTREEITEM(TreeItemHandle);
}

/*
PG_TOGGLEINFO(HWND) --> NIL
*/
HB_FUNC( PG_TOGGLEINFO ) // Pg_ToggleInfo(hWndPG)
{
   _ToggleInfo(hmg_par_HWND(1));
}

void _ToggleInfo(HWND hWndPG)
{
   PROPGRD  *ppgrd = ( PROPGRD * ) GetWindowLongPtr(hWndPG, GWLP_USERDATA);
   int      height, width;

   width = ppgrd->cxWidthPG;

   if( ppgrd->lInfoShow ) {
      ShowWindow(ppgrd->hInfoTitle, SW_HIDE);
      ShowWindow(ppgrd->hInfoText, SW_HIDE);
      ShowWindow(ppgrd->hInfoFrame, SW_HIDE);
      height = ppgrd->cyPG -  ppgrd->cyBtn + ppgrd->cyInfo;
      SetWindowPos(hWndPG, 0, 0, 0, width, height, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER);
      ppgrd->lInfoShow = false;
   } else {
      ShowWindow(ppgrd->hInfoTitle, SW_SHOW);
      ShowWindow(ppgrd->hInfoText, SW_SHOW);
      ShowWindow(ppgrd->hInfoFrame, SW_SHOW);
      height = ppgrd->cyPG -  ppgrd->cyBtn;
      SetWindowPos(hWndPG, 0, 0, 0, width, height, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER);
      ppgrd->lInfoShow = true;
   }
}

/*
ADDTREEITEMS(HWND, ap2, p3) --> numeric
*/
HB_FUNC( ADDTREEITEMS )
{
   auto h = hmg_par_HWND(1);
   int l = hb_parinfa(2, 0) - 1;
   auto hArray = hb_param(2, Harbour::Item::ARRAY);
   int c = ListView_GetItemCount(h);
   char * caption = const_cast<char*>(hb_arrayGetCPtr(hArray, 1));

   LV_ITEM  LI;
   LI.mask = LVIF_TEXT | LVIF_IMAGE;   // Browse+
   LI.state = 0;
   LI.stateMask = 0;
   LI.iImage = hb_parni(3);          // Browse+
   LI.iSubItem = 0;
   LI.iItem = c;
   LI.pszText = caption;
   ListView_InsertItem(h, &LI);

   for( int s = 1; s <= l; ++s ) {
      caption = const_cast<char*>(hb_arrayGetCPtr(hArray, s + 1));
      ListView_SetItemText(h, c, s, caption);
   }

   hb_retni(c);
}

/*
INITPROPGRIDIMAGELIST(HWND, HIMAGELIST) --> numeric
*/
HB_FUNC( INITPROPGRIDIMAGELIST )
{
   int cx = 0;
   HIMAGELIST himl = hmg_par_HIMAGELIST(2);
   if( himl != nullptr ) {
      SendMessage(hmg_par_HWND(1), TVM_SETIMAGELIST, (WPARAM) TVSIL_NORMAL, (LPARAM) himl);
      cx = ImageList_GetImageCount(himl);
   }
   hb_retni(cx);
}

/*
RESETPROPGRIDIMAGELIST(HWND, HTREEITEM, HBITMAP) --> numeric
*/
HB_FUNC( RESETPROPGRIDIMAGELIST )
{
   auto hWndPG = hmg_par_HWND(1);
   auto hItemPG = hmg_par_HTREEITEM(2);
   TV_ITEM TItem{};
   TItem.mask = TVIF_HANDLE | TVIF_IMAGE | TVIF_SELECTEDIMAGE;
   TItem.hItem = hItemPG;
   TreeView_GetItem(hWndPG, &TItem);
   HIMAGELIST himl = ( HIMAGELIST ) SendMessage(hWndPG, TVM_GETIMAGELIST, (WPARAM) TVSIL_NORMAL, 0);
   ImageList_Replace(himl, TItem.iImage - 1, hmg_par_HBITMAP(3), 0);
   SendMessage(hWndPG, TVM_SETIMAGELIST, (WPARAM) TVSIL_NORMAL, (LPARAM) himl);
   hb_retni(ImageList_GetImageCount(himl));
}

/*
PG_REDRAWITEM(HWND, HTREEITEM) --> .T.|.F.
*/
HB_FUNC( PG_REDRAWITEM )
{
   hb_retl(TreeView_SelectItem(hmg_par_HWND(1), hmg_par_HTREEITEM(2)));
}

/*
TREEVIEW_SETBOLDITEM(HWND, HTREEITEM, lBold) --> NIL
*/
HB_FUNC( TREEVIEW_SETBOLDITEM )
{
   TVITEM tvItem;
   tvItem.mask = TVIF_HANDLE | TVIF_STATE;
   tvItem.hItem = hmg_par_HTREEITEM(2);
   tvItem.stateMask = TVIS_BOLD;
   tvItem.state = hmg_par_BOOL(3) ? TVIS_BOLD : 0;
   TreeView_SetItem(hmg_par_HWND(1), &tvItem);
}

/*
SETNODECOLOR(np1, np2, np3) --> numeric
*/
HB_FUNC( SETNODECOLOR )
{
   LPNMTVCUSTOMDRAW lplvcd = ( LPNMTVCUSTOMDRAW ) HB_PARNL(1);
   lplvcd->clrText = hb_parni(3);
   lplvcd->clrTextBk = hb_parni(2);
   lplvcd->iLevel = 0;
   hb_retni(CDRF_NEWFONT);
}

/*
GETNOTIFYTREEITEM(p1) --> numeric
*/
HB_FUNC( GETNOTIFYTREEITEM )
{
   hb_retnl( (LONG_PTR) ((NMTREEVIEW FAR *) HB_PARNL(1))->itemNew.hItem );
}

/*
PGCOMBOADDSTRING(HWND, text, HIMAGELIST) --> NIL
*/
HB_FUNC( PGCOMBOADDSTRING )
{
   HIMAGELIST hILst = hmg_par_HIMAGELIST(3);
   char * cString = const_cast<char*>(hb_parc(2));
   DWORD dwIndex = SendMessage(hmg_par_HWND(1), CB_ADDSTRING, 0, (LPARAM) cString);
   if( hb_parnl(3) ) {
      SendMessage(hmg_par_HWND(1), CB_SETITEMDATA, dwIndex, (LPARAM) hILst);
   }
}

/*
PG_SETPICTURE(HWND, fileName, np3, np4) --> HBITMAP
*/
HB_FUNC( PG_SETPICTURE )
{
   hmg_ret_HBITMAP(HMG_LoadPicture(hb_parc(2), hb_parni(3), hb_parni(4), hmg_par_HWND(1), 0, 0, -1, 0, false, 255));
}

/*
CREATECOLORBMP1(HWND, color, width, height) --> HBITMAP
*/
HB_FUNC( CREATECOLORBMP1 ) // CreateColorBmp(hWnd, nColor, BmpWidh, BmpHeight)
{
   HBRUSH hBlackBrush = CreateSolidBrush(RGB(1, 1, 1));
   HBRUSH hBgBrush = CreateSolidBrush(RGB(255, 255, 255));

   auto handle = hmg_par_HWND(1);
   COLORREF clr = hb_parnl(2);
   int      width = HB_ISNIL(3) ? 20 : hb_parni(3);
   int      height = HB_ISNIL(4) ? 20 : hb_parni(4);
   auto imgDC = GetDC(handle);
   auto tmpDC = CreateCompatibleDC(imgDC);

   RECT rect;
   SetRect(&rect, 0, 0, width, height); // Size Bmp
   auto hBmp = CreateCompatibleBitmap(imgDC, width, height);

   SelectObject(tmpDC, hBmp);

   HBRUSH hOldBrush = reinterpret_cast<HBRUSH>(SelectObject(tmpDC, hBgBrush));
   FillRect(tmpDC, &rect, hBgBrush);

   rect.left += 1;
   rect.top += 1;
   rect.right -= 1;
   rect.bottom -= 1;

   SelectObject(tmpDC, hBlackBrush);
   DrawEdge(tmpDC, &rect, BDR_SUNKENINNER, BF_RECT);

   rect.top += 1;
   rect.left += 1;
   rect.right -= 1;
   rect.bottom -= 1;

   HBRUSH hColorBrush = CreateSolidBrush(clr);
   SelectObject(tmpDC, hColorBrush);

   FillRect(tmpDC, &rect, hColorBrush);

   SelectObject(tmpDC, hOldBrush);
   DeleteObject(hColorBrush);
   DeleteObject(hBlackBrush);
   DeleteObject(hBgBrush);

   DeleteDC(tmpDC);
   ReleaseDC(handle, imgDC);

   hmg_ret_HBITMAP(hBmp);
   DeleteObject(hBmp);
}

/*
CREATECOLORBMP() -->
*/
HB_FUNC( CREATECOLORBMP )  //CreateColorBmp(hWnd, nColor, BmpWidh, BmpHeight)
{
   HBRUSH   hOldBrush;
   HBRUSH   hColorBrush;
   HBRUSH   hBlackBrush = CreateSolidBrush(RGB(1, 1, 1));
   HBRUSH   hBgBrush = CreateSolidBrush(RGB(255, 255, 255));

   RECT     rect;
   auto handle = hmg_par_HWND(1);
   COLORREF clr = hb_parnl(2);
   int      width = hb_parni(3);
   int      height = hb_parni(4);
   auto imgDC = GetDC(handle);
   auto tmpDC = CreateCompatibleDC(imgDC);

   if( (width == 0) & (height == 0) ) {
      width = 20;
      height = 16;
   }

   SetRect(&rect, 0, 0, width, height); // Size Bmp
   auto hBmp = CreateCompatibleBitmap(imgDC, width, height);

   SelectObject(tmpDC, hBmp);

   hOldBrush = reinterpret_cast<HBRUSH>(SelectObject(tmpDC, hBgBrush));
   FillRect(tmpDC, &rect, hBgBrush);

   rect.left += 1;
   rect.top += 1;
   rect.right -= 1;
   rect.bottom -= 1;

   SelectObject(tmpDC, hBlackBrush);
   DrawEdge(tmpDC, &rect, BDR_SUNKENINNER, BF_RECT);

   rect.top += 1;
   rect.left += 1;
   rect.right -= 1;
   rect.bottom -= 1;

   hColorBrush = CreateSolidBrush(clr);
   SelectObject(tmpDC, hColorBrush);

   FillRect(tmpDC, &rect, hColorBrush);

   SelectObject(tmpDC, hOldBrush);
   DeleteObject(hBlackBrush);
   DeleteObject(hBgBrush);
   DeleteObject(hColorBrush);

   DeleteDC(imgDC);
   DeleteDC(tmpDC);

   hmg_ret_HBITMAP(hBmp);
}

/*
GET_IMAGELIST() -->
*/
HB_FUNC( GET_IMAGELIST )   //Get_ImageList(hWnd)
{
   hb_retnl( (LONG) SendMessage(hmg_par_HWND(1), CBEM_GETIMAGELIST, 0, 0) );
}

/*
IL_ADDMASKEDINDIRECT() -->
*/
HB_FUNC( IL_ADDMASKEDINDIRECT )  //IL_AddMaskedIndirect(hwnd , himage , color , ix , iy , imagecount)
{
   BITMAP   bm;
   auto himage = hmg_par_HBITMAP(2);
   COLORREF clrBk   = CLR_NONE;
   LRESULT  lResult = -1;
   int      ic      = 1;

   if( hb_parnl(3) ) {
      clrBk = hmg_par_COLORREF(3);
   }

   if( hb_parni(6) ) {
      ic = hb_parni(6);
   }

   if( GetObject(himage, sizeof(BITMAP), &bm) != 0 ) {
      if( ( hb_parni(4) * ic == bm.bmWidth ) && ( hb_parni(5) == bm.bmHeight ) ) {
         lResult = ImageList_AddMasked(hmg_par_HIMAGELIST(1), himage, clrBk);
      }

      DeleteObject(himage);
   }

   hb_retni( lResult );
}

/*
IL_GETIMAGESIZE() -->
*/
HB_FUNC( IL_GETIMAGESIZE ) //IL_GetImageSize(himage)
{
   int   cx, cy;

   ImageList_GetIconSize(hmg_par_HIMAGELIST(1), &cx, &cy);

   hb_reta(2);  // { cx, cy }
   HB_STORNI( cx, -1, 1 );
   HB_STORNI( cy, -1, 2 );
}

/*
GETDATEPICKER() -->
*/
HB_FUNC( GETDATEPICKER )
{
   hmg_ret_HWND(DateTime_GetMonthCal(hmg_par_HWND(1)));
}

HWND EditPG(HWND hWnd, RECT rc, HTREEITEM hItem, int ItemType, PROPGRD ppgrd , BOOL DisEdit)
{
   static PHB_SYMB   pSymbol = nullptr;
   HWND              hEdit;
   std::string       cClass;
   std::string       cName = "";
   int               Style = WS_CHILD | WS_VISIBLE;
   int               nBtn = 0;
   int               height = rc.bottom - rc.top - 1;

   switch( ItemType ) {
      case PG_DEFAULT:
      case PG_CATEG:
      case PG_STRING:
      case PG_INTEGER:
      case PG_DOUBLE:
      case PG_SYSINFO:
      case PG_SIZE:
      case PG_FLAG:
         Style = Style | WS_VISIBLE | ES_AUTOHSCROLL;
         if( DisEdit ) {
            Style = Style | ES_READONLY;
         }
         if( ItemType == PG_INTEGER ) {
             Style = Style  | ES_NUMBER;
         }
         cClass = "EDIT";
         break;

      case PG_COLOR:
      case PG_IMAGE:
      case PG_FILE:
      case PG_FOLDER:
      case PG_FONT:
      case PG_ARRAY:
      case PG_USERFUN:
         Style = Style | ES_AUTOHSCROLL;
         if( DisEdit ) {
            Style = Style | ES_READONLY;
         }
         cClass = "EDIT";
         nBtn = 1;
         break;

      case PG_PASSWORD:
         Style = Style | ES_AUTOHSCROLL | ES_PASSWORD;
         if( DisEdit ) {
            Style = Style | ES_READONLY;
         }
         cClass = "EDIT";
         break;
      case PG_LOGIC:
         Style = Style | WS_VSCROLL | CBS_DROPDOWNLIST | CBS_OWNERDRAWFIXED | CBS_HASSTRINGS;   // | CBS_AUTOHSCROLL;
         cClass = "COMBOBOX";
         height = 200;
         break;

      case PG_LIST:
         Style = Style | WS_VSCROLL | CBS_DROPDOWN | CBS_OWNERDRAWFIXED | CBS_HASSTRINGS; //| CBS_AUTOHSCROLL;;
         cClass = "COMBOBOX";
         height = 200;
         break;

      case PG_SYSCOLOR:
      case PG_ENUM:
         Style = Style | WS_VSCROLL | CBS_DROPDOWNLIST | CBS_OWNERDRAWFIXED | CBS_HASSTRINGS;   // | CBS_AUTOHSCROLL;;
         cClass = "COMBOBOX";       //WC_COMBOBOXEX;
         height = 200;
         break;

      case PG_DATE:
         // Style = Style; // | DTS_UPDOWN;
         cClass = DATETIMEPICK_CLASS;
         cName = "DateTime";
         break;

      default:
         cClass = "EDIT";
   }

   hEdit = CreateWindowEx(0,
                          cClass.c_str(),
                          cName.c_str(),
                          Style,
                          rc.left + 1,
                          rc.top - 1,
                          rc.right - rc.left - 1,
                          height,
                          hWnd,
                          hmg_par_HMENU(2),
                          GetModuleHandle(nullptr),
                          nullptr);

   switch( ItemType ) {
      case PG_LOGIC:
         SendMessage(hEdit, CB_SETITEMHEIGHT, -1, (LPARAM) rc.bottom - rc.top - 6);
         SendMessage(hEdit, CB_SETITEMHEIGHT, 0, (LPARAM) rc.bottom - rc.top);
         break;

      case PG_COLOR:
      case PG_DATE:
      case PG_USERFUN:
         break;

      case PG_ENUM:
      case PG_LIST:
      case PG_SYSCOLOR:
         {
            SendMessage(hEdit, CB_SETITEMHEIGHT, (WPARAM) - 1, (LPARAM) rc.bottom - rc.top - 6);
            break;
         }

      case PG_ARRAY:
         SendMessage(hEdit, CB_SETITEMHEIGHT, (WPARAM) - 1, (LPARAM) rc.bottom - rc.top - 6);
   }

   InsertBtnPG(hEdit, hItem, nBtn, ItemType, ppgrd);

   if( !pSymbol ) {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("_PGINITDATA"));
   }

   if( pSymbol ) {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hmg_vmPushHandle(hWnd);
      hmg_vmPushHandle(hEdit);
      hmg_vmPushHandle(hItem);
      hb_vmPushLong(ItemType);
      hb_vmDo(4);
   }

   return hEdit;
}

LRESULT CALLBACK PGEditProc(HWND hEdit, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB   pSymbol = nullptr;
   long int          r;
   WNDPROC           OldWndProc;
   HTREEITEM         hItem;
   HWND              hWndParent;
   RECT              * prect;
   RECT              oldrect;
   RECT              rect;
   POINT             pt;
   UINT              oldstate;

   // get the button state structure

   INSBTN            *pbtn = ( INSBTN * ) GetWindowLongPtr(hEdit, GWLP_USERDATA);
   OldWndProc = pbtn->oldproc;
   hItem = pbtn->hItem;
   hWndParent = GetParent(hEdit);

   switch( Msg ) {
      case WM_NCDESTROY:
         OldWndProc = pbtn->oldproc;
         HeapFree(GetProcessHeap(), 0, pbtn);
         return CallWindowProc(OldWndProc, hEdit, Msg, wParam, lParam);

      case WM_GETDLGCODE:
         return DLGC_WANTALLKEYS;   //+DLGC_WANTARROWS+DLGC_WANTCHARS+DLGC_HASSETSEL;

      case WM_NCCALCSIZE:
         {
            prect = ( RECT * ) lParam;
            oldrect = *prect;

            CallWindowProc(pbtn->oldproc, hEdit, Msg, wParam, lParam);
            SendMessage(hEdit, WM_SETREDRAW, 1, 0);

            if( pbtn->nButton ) {
               pbtn->cxLeftEdge = prect->left - oldrect.left;
               pbtn->cxRightEdge = oldrect.right - prect->right;
               pbtn->cyTopEdge = prect->top - oldrect.top;
               pbtn->cyBottomEdge = oldrect.bottom - prect->bottom;
               prect->right -= pbtn->cxButton;
            }

            return 0;
         }

      case WM_NCPAINT:
         {
            CallWindowProc(pbtn->oldproc, hEdit, Msg, wParam, lParam);
            if( pbtn->nButton ) {
               GetWindowRect(hEdit, &rect);
               OffsetRect(&rect, -rect.left, -rect.top);

               GetBtnPG(pbtn, &rect);

               DrawInsBtnPG(hEdit, pbtn, &rect);
            }

            return 0;
         }

      case WM_NCHITTEST:
         if( pbtn->nButton ) {
            pt.x = LOWORD(lParam);
            pt.y = HIWORD(lParam);

            GetWindowRect(hEdit, &rect);
            GetBtnPG(pbtn, &rect);

            if( PtInRect(&rect, pt) ) {
               return HTBORDER;
            }
         }
         break;

      case WM_NCLBUTTONDBLCLK:
      case WM_NCLBUTTONDOWN:
         if( pbtn->nButton ) {
            pt.x = LOWORD(lParam);
            pt.y = HIWORD(lParam);

            GetWindowRect(hEdit, &rect);
            pt.x -= rect.left;
            pt.y -= rect.top;
            OffsetRect(&rect, -rect.left, -rect.top);
            GetBtnPG(pbtn, &rect);

            if( PtInRect(&rect, pt) ) {
               SetCapture(hEdit);
               pbtn->fButtonDown = TRUE;
               pbtn->fMouseDown = true;
               DrawInsBtnPG(hEdit, pbtn, &rect);
            }
         }
         break;

      case WM_MOUSEMOVE:
         if( pbtn->nButton ) {
            if( pbtn->fMouseDown == true ) {
               pt.x = LOWORD(lParam);
               pt.y = HIWORD(lParam);
               ClientToScreen(hEdit, &pt);

               GetWindowRect(hEdit, &rect);

               pt.x -= rect.left;
               pt.y -= rect.top;
               OffsetRect(&rect, -rect.left, -rect.top);

               GetBtnPG(pbtn, &rect);

               oldstate = pbtn->fButtonDown;

               if( PtInRect(&rect, pt) ) {
                  pbtn->fButtonDown = 1;
               } else {
                  pbtn->fButtonDown = 0;
               }

               if( oldstate != pbtn->fButtonDown ) {
                  DrawInsBtnPG(hEdit, pbtn, &rect);
               }
            }
         }
         break;

      case WM_KEYDOWN:
         if( wParam == VK_DOWN ) {
            LPSTR cData[1024];
            GetWindowText(hEdit, (LPSTR) cData, 1024);
            Pg_SetData(GetParent(hEdit), pbtn->hItem, (LPSTR) cData, "", FALSE);
            PostMessage(GetParent(hEdit), WM_KEYDOWN, VK_DOWN, MAKEWPARAM(0, 0));
            SetFocus(GetParent(hEdit));
         }

         if( wParam == VK_UP ) {
            LPSTR cData[1024];
            GetWindowText(hEdit, (LPSTR) cData, 1024);
            Pg_SetData(GetParent(hEdit), pbtn->hItem, (LPSTR) cData, "", FALSE);
            PostMessage(GetParent(hEdit), WM_KEYDOWN, VK_UP, MAKEWPARAM(0, 0));
            SetFocus(GetParent(hEdit));
         }
         break;

      case WM_LBUTTONUP:
         if( pbtn->nButton ) {
            if( pbtn->fMouseDown == true ) {
               pt.x = LOWORD(lParam);
               pt.y = HIWORD(lParam);
               ClientToScreen(hEdit, &pt);

               GetWindowRect(hEdit, &rect);

               pt.x -= rect.left;
               pt.y -= rect.top;
               OffsetRect(&rect, -rect.left, -rect.top);

               GetBtnPG(pbtn, &rect);

               if( PtInRect(&rect, pt) ) {
                  PostMessage(hEdit, WM_COMMAND, MAKEWPARAM(pbtn->ItemType, BN_CLICKED), (LPARAM) hItem);
                  SetFocus(hEdit);
               }

               ReleaseCapture();
               pbtn->fButtonDown = FALSE;
               pbtn->fMouseDown = false;

               DrawInsBtnPG(hEdit, pbtn, &rect);
            }
         }
         break;

      case WM_CHAR:
         if( wParam == 13 ) {
            LPSTR cData[1024];
            GetWindowText(hEdit, (LPSTR) cData, 1024);
            Pg_SetData(GetParent(hEdit), pbtn->hItem, (LPSTR) cData, "", FALSE);
            PostMessage(GetParent(hEdit), WM_KEYDOWN, VK_DOWN, MAKEWPARAM(0, 0));
            SetFocus(GetParent(hEdit));
         }
         break;

      case WM_CREATE:
      case WM_NCCREATE:
      case WM_COMMAND:
      case WM_SETFOCUS:
         {
            if( !pSymbol ) {
               pSymbol = hb_dynsymSymbol(hb_dynsymGet("OPGEDITEVENTS"));
            }

            if( pSymbol ) {
               hb_vmPushSymbol(pSymbol);
               hb_vmPushNil();
               hmg_vmPushHandle(hEdit);
               hb_vmPushLong(Msg);
               hb_vmPushLong(wParam);
               hb_vmPushLong(lParam);
               hmg_vmPushHandle(hWndParent);
               hmg_vmPushHandle(hItem);
               hb_vmDo(6);
            }

            r = hb_parnl( -1 );

            if( r != 0 ) {
               return r;
            } else {
               return CallWindowProc(OldWndProc, hEdit, Msg, wParam, lParam);
            }
         }

      case WM_KILLFOCUS:
         {
            if( pbtn->ppgrd.hItemEdit ) {
               PostMessage(pbtn->ppgrd.hPropEdit, WM_CLOSE, 0, 0);
               pbtn->ppgrd.hItemEdit = nullptr;
            }

            if( !pSymbol ) {
               pSymbol = hb_dynsymSymbol(hb_dynsymGet("OPGEDITEVENTS"));
            }

            if( pSymbol ) {
               hb_vmPushSymbol(pSymbol);
               hb_vmPushNil();
               hmg_vmPushHandle(hEdit);
               hb_vmPushLong(Msg);
               hb_vmPushLong(wParam);
               hb_vmPushLong(lParam);
               hmg_vmPushHandle(hWndParent);
               hmg_vmPushHandle(hItem);
               hb_vmDo(6);
            }

            r = hb_parnl( -1 );

            if( r != 0 ) {
               return r;
            } else {
               return CallWindowProc(OldWndProc, hEdit, Msg, wParam, lParam);
            }
         }
   }

   return CallWindowProc(OldWndProc, hEdit, Msg, wParam, lParam);
}

#if defined(__BORLANDC__)
#pragma argsused
#endif

int CALLBACK enumFontFamilyProc(ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam)
{
#if defined(__MINGW32__)
   UNREFERENCED_PARAMETER( lpntme );
#endif
   if( lpelfe && lParam ) {
      if( FontType == TRUETYPE_FONTTYPE ) { //DEVICE_FONTTYPE | RASTER_FONTTYPE
         SendMessage(reinterpret_cast<HWND>(lParam), CB_ADDSTRING, 0, (LPARAM) (LPSTR) lpelfe->elfFullName);
      }
   }

   return 1;
}

static void enumFonts(HWND hWndEdit)// , BYTE lfCharSet)
{
   auto hDC = GetDC(nullptr);
   LOGFONT lf;
   lf.lfCharSet = ANSI_CHARSET;
   lf.lfPitchAndFamily = 0;
   strcpy(lf.lfFaceName, "\0");
   EnumFontFamiliesEx(hDC, &lf, (FONTENUMPROC) enumFontFamilyProc, (LPARAM) hWndEdit, 0);
   ReleaseDC(nullptr, hDC);
}

/*
PG_GETFONTS() -->
*/
HB_FUNC( PG_GETFONTS )
{
   enumFonts(hmg_par_HWND(1));
}

/*
DIALOGUNITSX() -->
*/
HB_FUNC( DIALOGUNITSX )
{
   int  baseunitX = LOWORD(GetDialogBaseUnits());

   hb_retni( ( hb_parni(1) * 4 )/ baseunitX );
}

/*
DIALOGUNITSY() -->
*/
HB_FUNC( DIALOGUNITSY )
{
   int  baseunitY =  HIWORD(GetDialogBaseUnits());

   hb_retni( ( hb_parni(1) * 8 )/ baseunitY );
}
