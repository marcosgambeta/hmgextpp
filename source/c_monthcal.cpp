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
 * Copyright 2006 (C) Grigory Filatov <gfilatov@gmail.com>
 */

#define _WIN32_IE  0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbvm.hpp>
#include <hbdate.hpp>
#include <hbwinuni.hpp>

extern HFONT PrepareFont(const TCHAR * FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline, DWORD StrikeOut, DWORD Angle, DWORD charset);
LRESULT CALLBACK OwnMCProc(HWND hmonthcal, UINT Msg, WPARAM wParam, LPARAM lParam);

/*
INITMONTHCAL(HWND, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) --> HANDLE
*/
HB_FUNC( INITMONTHCAL )
{
   int   bold      = FW_NORMAL;
   int   italic    = 0;
   int   underline = 0;
   int   strikeout = 0;
   int   angle     = 0;

   INITCOMMONCONTROLSEX icex;
   icex.dwSize = sizeof(icex);
   icex.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx(&icex);

   DWORD style = WS_BORDER | WS_CHILD | MCS_DAYSTATE;

   if( hb_parl(9) ) {
      style |= MCS_NOTODAY;
   }

   if( hb_parl(10) ) {
      style |= MCS_NOTODAYCIRCLE;
   }

   if( hb_parl(11) ) {
      style |= MCS_WEEKNUMBERS;
   }

   if( !hb_parl(12) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(13) ) {
      style |= WS_TABSTOP;
   }

   auto hmonthcal = CreateWindowEx(
      0,
      MONTHCAL_CLASS,
      TEXT(""),
      style,
      0,
      0,
      0,
      0,
      hmg_par_HWND(1),
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr);

   SetProp(hmonthcal, TEXT("oldmcproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hmonthcal, GWLP_WNDPROC)));
   SetWindowLongPtr(hmonthcal, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnMCProc)));

   if( hb_parl(14) ) {
      bold = FW_BOLD;
   }

   if( hb_parl(15) ) {
      italic = 1;
   }

   if( hb_parl(16) ) {
      underline = 1;
   }

   if( hb_parl(17) ) {
      strikeout = 1;
   }

   void * str;
   auto hfont = PrepareFont(HB_PARSTR(7, &str, nullptr), hb_parni(8), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET);
   hb_strfree(str);

   SendMessage(hmonthcal, WM_SETFONT, reinterpret_cast<WPARAM>(hfont), 1);

   RECT rc;
   MonthCal_GetMinReqRect(hmonthcal, &rc);
   SetWindowPos(hmonthcal, nullptr, hb_parni(3), hb_parni(4), rc.right, rc.bottom, SWP_NOZORDER);

   hb_reta(2);
   hmg_storvhandle(hmonthcal, -1, 1);
   hmg_storvhandle(hfont, -1, 2);
}

/*
SETMONTHCALVALUE(HWND, nYear, nMonth, nDay) --> NIL
*/
HB_FUNC( SETMONTHCALVALUE )
{
   auto hwnd = hmg_par_HWND(1);

   SYSTEMTIME sysTime{};
   sysTime.wYear         = hmg_par_WORD(2);
   sysTime.wMonth        = hmg_par_WORD(3);
   sysTime.wDay          = hmg_par_WORD(4);
   sysTime.wDayOfWeek    = LOWORD(SendMessage(hwnd, MCM_GETFIRSTDAYOFWEEK, 0, 0));
   //sysTime.wHour         = 0;
   //sysTime.wMinute       = 0;
   //sysTime.wSecond       = 0;
   //sysTime.wMilliseconds = 0;

   MonthCal_SetCurSel(hwnd, &sysTime);
}

/*
GETMONTHCALVALUE(HWND, np2) --> numeric
*/
HB_FUNC( GETMONTHCALVALUE )
{
   SYSTEMTIME st;

   SendMessage(hmg_par_HWND(1), MCM_GETCURSEL, 0, reinterpret_cast<LPARAM>(&st));

   switch( hb_parni(2) ) {
      case 1: hb_retni(st.wYear); break;
      case 2: hb_retni(st.wMonth); break;
      case 3: hb_retni(st.wDay);
   }
}

/*
GETMONTHCALDATE(HWND) --> data
*/
HB_FUNC( GETMONTHCALDATE )
{
   SYSTEMTIME st;
   SendMessage(hmg_par_HWND(1), MCM_GETCURSEL, 0, reinterpret_cast<LPARAM>(&st));
   long lJulian = hb_dateEncode(st.wYear, st.wMonth, st.wDay);
   hb_retdl(lJulian);
}

/*
SETPOSMONTHCAL(HWND, p2, p3, p4) --> NIL
*/
HB_FUNC( SETPOSMONTHCAL )
{
   auto hWndMonthCal = hmg_par_HWND(1);

   RECT rc;
   MonthCal_GetMinReqRect(hWndMonthCal, &rc);

   DWORD dwWidth = MonthCal_GetMaxTodayWidth(hWndMonthCal);

   if( dwWidth > static_cast<DWORD>(rc.right) ) {
      rc.right = dwWidth;
   }

   if( hb_parldef(4, false) ) {
      InflateRect(&rc, 6, 6);
   }

   SetWindowPos(hWndMonthCal, nullptr, hb_parni(2), hb_parni(3), rc.right, rc.bottom, SWP_NOZORDER);
}

/*
GETMONTHRANGE(HWND) --> array
*/
HB_FUNC( GETMONTHRANGE )
{
   SYSTEMTIME sysTime[2];
   memset(&sysTime, 0, sizeof(sysTime));
   int iCount = SendMessage(hmg_par_HWND(1), MCM_GETMONTHRANGE, GMR_DAYSTATE, reinterpret_cast<LPARAM>(&sysTime));

   hb_reta(3);
   HB_STORNI(iCount, -1, 1);
   HB_STORDL(hb_dateEncode(sysTime[0].wYear, sysTime[0].wMonth, sysTime[0].wDay), -1, 2);
   HB_STORDL(hb_dateEncode(sysTime[1].wYear, sysTime[1].wMonth, sysTime[1].wDay), -1, 3);
}

#ifndef BOLDDAY
#define BOLDDAY(ds, iDay)  if( iDay > 0 && iDay < 32 )( ds ) |= (0x00000001 << (iDay - 1))
#endif

/*
C_SETDAYSTATE(HWND, np2, ap3) --> NIL
*/
HB_FUNC( C_SETDAYSTATE )
{
   auto iCount = hb_parni(2);
   auto hArray = hb_param(3, Harbour::Item::ARRAY);
   int iSize = sizeof(MONTHDAYSTATE) * iCount;
   auto rgMonths = static_cast<LPMONTHDAYSTATE>(hb_xgrab(iSize));
   memset(rgMonths, 0, iSize);

   for( int i = 0; i < iCount; i++ ) {
      for( int j = 1; j <= 32; j++ ) {
         if( hb_arrayGetNI(hArray, i * 32 + j) == 1 ) {
            BOLDDAY(rgMonths[i], j);
         }
      }
   }

   SendMessage(hmg_par_HWND(1), MCM_SETDAYSTATE, iCount, reinterpret_cast<LPARAM>(rgMonths));
   hb_xfree(rgMonths);
}

/*
C_RETDAYSTATE(p1, p2, p3) --> NIL
*/
HB_FUNC( C_RETDAYSTATE )
{
   LPNMDAYSTATE pData = reinterpret_cast<NMDAYSTATE*>(HB_PARNL(1));
   auto iCount = hb_parni(2);
   auto hArray = hb_param(3, Harbour::Item::ARRAY);
   int iSize = sizeof(MONTHDAYSTATE) * iCount;
   auto rgMonths = static_cast<LPMONTHDAYSTATE>(hb_xgrab(iSize));
   memset(rgMonths, 0, iSize);

   for( int i = 0; i < iCount; i++ ) {
      for( int j = 1; j <= 32; j++ ) {
         if( hb_arrayGetNI(hArray, i * 32 + j) == 1 ) {
            BOLDDAY(rgMonths[i], j);
         }
      }
   }

   pData->prgDayState = rgMonths;
   hb_xfree(rgMonths);
}

/*
GETDAYSTATEDATA(p1) --> array
*/
HB_FUNC( GETDAYSTATEDATA )
{
   LPNMDAYSTATE pData = reinterpret_cast<NMDAYSTATE*>(HB_PARNL(1));
   hb_reta(2);
   HB_STORNI(pData->cDayState, -1, 1);
   HB_STORDL(hb_dateEncode(pData->stStart.wYear, pData->stStart.wMonth, pData->stStart.wDay), -1, 2);
}

LRESULT CALLBACK OwnMCProc(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   auto OldWndProc = reinterpret_cast<WNDPROC>(reinterpret_cast<LONG_PTR>(GetProp(hwnd, TEXT("oldmcproc"))));

   switch( Msg ) {
      case WM_DESTROY:
         SetWindowLongPtr(hwnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OldWndProc)));
         RemoveProp(hwnd, TEXT("oldmcproc"));
         break;

      case WM_MOUSEACTIVATE:
      case WM_SETFOCUS:
      case WM_KILLFOCUS:
         if( !pSymbol ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OMONTHCALEVENTS"));
         }

         if( pSymbol ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hwnd);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         long int r = hb_parnl(-1);

         if( r != 0 ) {
            return r;
         }
   }

   return CallWindowProc(OldWndProc, hwnd, Msg, wParam, lParam);
}
