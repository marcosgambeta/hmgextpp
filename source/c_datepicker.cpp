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

#define _WIN32_IE  0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbvm.hpp>
#include <hbdate.hpp>
#include <hbwinuni.hpp>

LRESULT CALLBACK  OwnPickProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);
extern HB_EXPORT double hb_timeStampPack(int iYear, int iMonth, int iDay, int iHour, int iMinutes, int iSeconds, int iMSec);

/*
INITDATEPICK(HWND, HMENU, nX, nY, nWidth, nHeight, p7, p8, lShowNone, lUpDown, lRightAlign, lVisible, lTabStop) --> handle
*/
HB_FUNC( INITDATEPICK )
{
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD;

   if( hb_parl(9) ) {
      style |= DTS_SHOWNONE;
   }

   if( hb_parl(10) ) {
      style |= DTS_UPDOWN;
   }

   if( hb_parl(11) ) {
      style |= DTS_RIGHTALIGN;
   }

   if( !hb_parl(12) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(13) ) {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(WS_EX_CLIENTEDGE,
                                 DATETIMEPICK_CLASS,
                                 TEXT("DateTime"),
                                 style,
                                 hmg_par_int(3),
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(6),
                                 hmg_par_HWND(1),
                                 hmg_par_HMENU(2),
                                 GetInstance(),
                                 nullptr);

   SetProp(hbutton, TEXT("oldpickproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hbutton, GWLP_WNDPROC)));
   SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnPickProc)));

   hmg_ret_HWND(hbutton);
}

/*
INITTIMEPICK(HWND, HMENU, nX, nY, nWidth, nHeight, p7, p8, lShowNone, lVisible, lTabStop) --> handle
*/
HB_FUNC( INITTIMEPICK )
{
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx(&i);

   DWORD style = WS_CHILD | DTS_TIMEFORMAT;

   if( hb_parl(9) ) {
      style |= DTS_SHOWNONE;
   }

   if( !hb_parl(10) ) {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(11) ) {
      style |= WS_TABSTOP;
   }

   auto hbutton = CreateWindowEx(WS_EX_CLIENTEDGE,
                                 DATETIMEPICK_CLASS,
                                 TEXT("DateTime"),
                                 style,
                                 hmg_par_int(3),
                                 hmg_par_int(4),
                                 hmg_par_int(5),
                                 hmg_par_int(6),
                                 hmg_par_HWND(1),
                                 hmg_par_HMENU(2),
                                 GetInstance(),
                                 nullptr);

   SetProp(hbutton, TEXT("oldpickproc"), reinterpret_cast<HWND>(GetWindowLongPtr(hbutton, GWLP_WNDPROC)));
   SetWindowLongPtr(hbutton, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(static_cast<WNDPROC>(OwnPickProc)));

   hmg_ret_HWND(hbutton);
}

LRESULT CALLBACK OwnPickProc(HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   auto OldWndProc = reinterpret_cast<WNDPROC>(reinterpret_cast<LONG_PTR>(GetProp(hButton, TEXT("oldpickproc"))));

   switch( Msg ) {
      case WM_ERASEBKGND:
         if( pSymbol == nullptr ) {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OPICKEVENTS"));
         }
         if( pSymbol != nullptr ) {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hmg_vmPushHandle(hButton);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }
         long int r = hb_parnl(-1);
         if( r != 0 ) {
            return r;
         }
#if 0
         else {
            return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
         }
#endif
   }

   return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
}

/*
SETDATEPICK(HWND, dDate) --> .T.|.F.
SETDATEPICK(HWND, nYear, nMonth, nDay) --> .T.|.F.
*/
HB_FUNC( SETDATEPICK )
{
   SYSTEMTIME sysTime{};

   if( hb_pcount() == 2 && HB_ISDATE(2) ) {
      int iYear, iMonth, iDay;
      long lJulian = hb_pardl(2);
      hb_dateDecode(lJulian, &iYear, &iMonth, &iDay);
      sysTime.wYear  = iYear;
      sysTime.wMonth = iMonth;
      sysTime.wDay   = iDay;
   } else if( hb_pcount() > 2 ) {
      sysTime.wYear  = hmg_par_WORD(2);
      sysTime.wMonth = hmg_par_WORD(3);
      sysTime.wDay   = hmg_par_WORD(4);
   } else {
      sysTime.wYear  = 2005; // date() ?
      sysTime.wMonth = 1;
      sysTime.wDay   = 1;
   }

   //sysTime.wDayOfWeek    = 0;
   //sysTime.wHour         = 0;
   //sysTime.wMinute       = 0;
   //sysTime.wSecond       = 0;
   //sysTime.wMilliseconds = 0;

   hb_retl(SendMessage(hmg_par_HWND(1), DTM_SETSYSTEMTIME, GDT_VALID, reinterpret_cast<LPARAM>(&sysTime)) == GDT_VALID ? true : false);
}

/*
SETTIMEPICK(HWND, nHour, nMinute, nSecond) --> .T.|.F.
*/
HB_FUNC( SETTIMEPICK )
{
   SYSTEMTIME sysTime{};

   sysTime.wYear         = 2005;
   sysTime.wMonth        = 1;
   sysTime.wDay          = 1;
   //sysTime.wDayOfWeek    = 0;
   sysTime.wHour         = hmg_par_WORD(2);
   sysTime.wMinute       = hmg_par_WORD(3);
   sysTime.wSecond       = hmg_par_WORD(4);
   //sysTime.wMilliseconds = 0;

   hb_retl(SendMessage(hmg_par_HWND(1), DTM_SETSYSTEMTIME, GDT_VALID, reinterpret_cast<LPARAM>(&sysTime)) == GDT_VALID ? true : false);
}

/*
GETDATEPICKDATE(HWND) --> date
*/
HB_FUNC( GETDATEPICKDATE )
{
   SYSTEMTIME st{};
   SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st));
   hb_retd(st.wYear, st.wMonth, st.wDay);
}

/*
GETDATEPICKYEAR(HWND) --> nYear
*/
HB_FUNC( GETDATEPICKYEAR )
{
   SYSTEMTIME st{};
   SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st));
   hb_retni(st.wYear);
}

/*
GETDATEPICKMONTH(HWND) --> nMonth
*/
HB_FUNC( GETDATEPICKMONTH )
{
   SYSTEMTIME st{};
   SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st));
   hb_retni(st.wMonth);
}

/*
GETDATEPICKDAY(HWND) --> nDay
*/
HB_FUNC( GETDATEPICKDAY )
{
   SYSTEMTIME st{};
   SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st));
   hb_retni(st.wDay);
}

/*
GETDATEPICKHOUR(HWND) --> nHour
*/
HB_FUNC( GETDATEPICKHOUR )
{
   SYSTEMTIME st{};
   hb_retni(SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st)) == GDT_VALID ? st.wHour : -1);
}

/*
GETDATEPICKMINUTE(HWND) --> nMinute
*/
HB_FUNC( GETDATEPICKMINUTE )
{
   SYSTEMTIME st{};
   hb_retni(SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st)) == GDT_VALID ? st.wMinute : -1);
}

/*
GETDATEPICKSECOND(HWND) --> nSeconds
*/
HB_FUNC( GETDATEPICKSECOND )
{
   SYSTEMTIME st{};
   hb_retni(SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st)) == GDT_VALID ? st.wSecond : -1);
}

/*
DTP_SETDATETIME(HWND, datetime) --> .T.|.F.
DTP_SETDATETIME(HWND, date) --> .T.|.F.
DTP_SETDATETIME(HWND, nYear, nMonth, nDay, nHour, nMinute, nSeconds, nMilliseconds) --> .T.|.F.
*/
HB_FUNC( DTP_SETDATETIME ) // TODO: deprecate bTimeToZero
{
   SYSTEMTIME sysTime{};

   auto bTimeToZero = false;

   if( HB_ISDATETIME(2) ) {
      int iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
      hb_timeStampUnpack(hb_partd(2), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec);
      sysTime.wYear         = iYear;
      sysTime.wMonth        = iMonth;
      sysTime.wDay          = iDay;
      sysTime.wDayOfWeek    = 0;
      sysTime.wHour         = iHour;
      sysTime.wMinute       = iMinute;
      sysTime.wSecond       = iSecond;
      sysTime.wMilliseconds = iMSec;
   } else if( HB_ISDATE(2) ) {
      int iYear, iMonth, iDay;
      long lJulian = hb_pardl(2);
      hb_dateDecode(lJulian, &iYear, &iMonth, &iDay);
      sysTime.wYear      = iYear;
      sysTime.wMonth     = iMonth;
      sysTime.wDay       = iDay;
      sysTime.wDayOfWeek = 0;
      bTimeToZero = true;
   } else {
      sysTime.wYear      = hb_parnidef(2, 2005);
      sysTime.wMonth     = hb_parnidef(3, 1);
      sysTime.wDay       = hb_parnidef(4, 1);
      sysTime.wDayOfWeek = 0;
      if( hb_pcount() >= 7 ) {
         sysTime.wHour         = hmg_par_WORD(5);
         sysTime.wMinute       = hmg_par_WORD(6);
         sysTime.wSecond       = hmg_par_WORD(7);
         sysTime.wMilliseconds = hmg_par_WORD(8);
      } else {
         bTimeToZero = true;
      }
   }

   if( bTimeToZero ) {
      sysTime.wHour         = 0;
      sysTime.wMinute       = 0;
      sysTime.wSecond       = 0;
      sysTime.wMilliseconds = 0;
   }

   hb_retl(SendMessage(hmg_par_HWND(1), DTM_SETSYSTEMTIME, GDT_VALID, reinterpret_cast<LPARAM>(&sysTime)) == GDT_VALID ? true : false);
}

/*
DTP_GETDATETIME(HWND) --> datetime
*/
HB_FUNC( DTP_GETDATETIME )
{
   SYSTEMTIME st{};
   SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st));
   hb_rettd(hb_timeStampPack(st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds));
}

/*
SETDATEPICKNULL(HWND) --> NIL
*/
HB_FUNC( SETDATEPICKNULL )
{
   SendMessage(hmg_par_HWND(1), DTM_SETSYSTEMTIME, GDT_NONE, 0);
}

/*
SETDATEPICKRANGE(HWND, date1, date2) --> .T.|.F.|NIL
*/
HB_FUNC( SETDATEPICKRANGE )
{
   SYSTEMTIME sysTime[2];
   char * cDate;
   DWORD y, m, d;
   WPARAM wLimit = 0;

   if( HB_ISDATE(2) && HB_ISDATE(3) ) {
      memset(&sysTime, 0, sizeof(sysTime));
      cDate = const_cast<char*>(hb_pards(2));
      if( !(cDate[0] == ' ') ) {
         y = static_cast<DWORD>((cDate[0] - '0') * 1000) + ((cDate[1] - '0') * 100) + ((cDate[2] - '0') * 10) + (cDate[3] - '0');
         sysTime[0].wYear = y;
         m = static_cast<DWORD>((cDate[4] - '0') * 10) + (cDate[5] - '0');
         sysTime[0].wMonth = m;
         d = static_cast<DWORD>((cDate[6] - '0') * 10) + (cDate[7] - '0');
         sysTime[0].wDay = d;
         wLimit |= GDTR_MIN;
      }

      cDate = const_cast<char*>(hb_pards(3));
      if( !(cDate[0] == ' ') ) {
         y = static_cast<DWORD>((cDate[0] - '0') * 1000) + ((cDate[1] - '0') * 100) + ((cDate[2] - '0') * 10) + (cDate[3] - '0');
         sysTime[1].wYear = y;
         m = static_cast<DWORD>((cDate[4] - '0') * 10) + (cDate[5] - '0');
         sysTime[1].wMonth = m;
         d = static_cast<DWORD>((cDate[6] - '0') * 10) + (cDate[7] - '0');
         sysTime[1].wDay = d;
         wLimit |= GDTR_MAX;
      }

      hb_retl(SendMessage(hmg_par_HWND(1), DTM_SETRANGE, wLimit, reinterpret_cast<LPARAM>(&sysTime)));
   }
}

/*
SETDATEPICKERDATEFORMAT(HWND, cFormat) --> .T.|.F.
*/
HB_FUNC( SETDATEPICKERDATEFORMAT )
{
   void * str;
   hb_retl(SendMessage(hmg_par_HWND(1), DTM_SETFORMAT, 0, reinterpret_cast<LPARAM>(HB_PARSTR(2, &str, nullptr))));
   hb_strfree(str);
}

HB_FUNC( DTP_ISCHECKED )
{
   SYSTEMTIME st{};
   hb_retl(SendMessage(hmg_par_HWND(1), DTM_GETSYSTEMTIME, 0, reinterpret_cast<LPARAM>(&st)) == GDT_VALID ? true : false);
}
