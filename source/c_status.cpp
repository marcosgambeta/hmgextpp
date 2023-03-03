/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

    "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
    www - https://harbour.github.io/

    "Harbour Project"
    Copyright 1999-2022, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   ---------------------------------------------------------------------------*/

#define _WIN32_IE  0x0501

#include "mgdefs.hpp"

#include <commctrl.h>

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR  WideToAnsi(LPWSTR);
#endif

HB_FUNC( INITMESSAGEBAR )
{
   HWND hWndSB;
   int  ptArray[40];  // Array defining the number of parts/sections
   int  nrOfParts = 1;

   hWndSB = CreateStatusWindow(WS_CHILD | WS_VISIBLE | SBT_TOOLTIPS, nullptr, hmg_par_HWND(1), hb_parni(2));

   if( hWndSB )
   {
      SendMessage(hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray);
   }

   hmg_ret_HANDLE(hWndSB);
}

HB_FUNC( INITITEMBAR )
{
   HWND  hWndSB;
   int   cSpaceInBetween = 8;
   int   ptArray[40]; // Array defining the number of parts/sections
   int   nrOfParts = 0;
   RECT  rect;
   HDC   hDC;
   WORD  displayFlags;
   HICON hIcon;
   int   style;
   int   cx;
   int   cy;

#ifndef UNICODE
   LPCSTR lpText     = hb_parc(2);
   LPCSTR lpIconName = hb_parc(6);
   LPCSTR lpTipText  = hb_parc(7);
#else
   LPWSTR lpText     = AnsiToWide(( char * ) hb_parc(2));
   LPWSTR lpIconName = AnsiToWide(( char * ) hb_parc(6));
   LPWSTR lpTipText  = AnsiToWide(( char * ) hb_parc(7));
#endif

   hWndSB = hmg_par_HWND(1);
   style  = GetWindowLong(( HWND ) GetParent(hWndSB), GWL_STYLE);

   switch( hb_parni(8) )
   {
      case 0:  displayFlags = 0; break;
      case 1:  displayFlags = SBT_POPOUT; break;
      case 2:  displayFlags = SBT_NOBORDERS; break;
      default: displayFlags = 0;
   }

   if( hb_parnl(5) )
   {
      nrOfParts = ( int ) SendMessage(hWndSB, SB_GETPARTS, 40, 0);
      SendMessage(hWndSB, SB_GETPARTS, ( WPARAM ) 40, ( LPARAM ) ( LPINT ) ptArray);
   }

   nrOfParts++;

   hDC = GetDC(hWndSB);
   GetClientRect(hWndSB, &rect);

   if( hb_parnl(5) == 0 )
   {
      ptArray[nrOfParts - 1] = rect.right;
   }
   else
   {
      for( int n = 0; n < nrOfParts - 1; n++ )
      {
         ptArray[n] -= hb_parni(4) - cSpaceInBetween;
      }

      if( style & WS_SIZEBOX )
      {
         if( nrOfParts == 2 )
         {
            ptArray[0] -= 21;
         }

         ptArray[nrOfParts - 1] = rect.right - rect.bottom - rect.top + 2;
      }
      else
      {
         ptArray[nrOfParts - 1] = rect.right;
      }
   }

   ReleaseDC(hWndSB, hDC);

   SendMessage(hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray);

   cy = rect.bottom - rect.top - 4;
   cx = cy;

   hIcon = ( HICON ) LoadImage(GetResources(), lpIconName, IMAGE_ICON, cx, cy, 0);

   if( hIcon == nullptr )
   {
      hIcon = ( HICON ) LoadImage(nullptr, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE);
   }

   if( !( hIcon == nullptr ) )
   {
      SendMessage(hWndSB, SB_SETICON, ( WPARAM ) nrOfParts - 1, ( LPARAM ) hIcon);
   }

   SendMessage(hWndSB, SB_SETTEXT, ( WPARAM ) ( ( nrOfParts - 1 ) | displayFlags ), ( LPARAM ) lpText);
   SendMessage(hWndSB, SB_SETTIPTEXT, ( WPARAM ) nrOfParts - 1, ( LPARAM ) lpTipText);

   hb_retni( nrOfParts );

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpText);
   hb_xfree(( TCHAR * ) lpIconName);
   hb_xfree(( TCHAR * ) lpTipText);
#endif
}

HB_FUNC( SETITEMBAR )
{
   HWND hWnd = hmg_par_HWND(1);
   int  iPos = hb_parni(3);
   WORD nFlags;

#ifndef UNICODE
   LPCSTR lpText = hb_parc(2);
#else
   LPWSTR lpText = AnsiToWide(( char * ) hb_parc(2));
#endif

   nFlags = HIWORD(SendMessage(hWnd, SB_GETTEXTLENGTH, ( WPARAM ) iPos, 0));
   SendMessage(hWnd, SB_SETTEXT, ( WPARAM ) ( iPos | nFlags ), ( LPARAM ) lpText);

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpText);
#endif
}

HB_FUNC( GETITEMBAR )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   HWND    hWnd = hmg_par_HWND(1);
   int     iPos = hb_parni(2);
   TCHAR * cString;

   cString = ( TCHAR * ) hb_xgrab((LOWORD(SendMessage(hWnd, SB_GETTEXTLENGTH, ( WPARAM ) iPos - 1, 0)) + 1) * sizeof(TCHAR));
   SendMessage(hWnd, SB_GETTEXT, ( WPARAM ) iPos - 1, ( LPARAM ) cString);

#ifndef UNICODE
   hb_retc( cString );
#else
   pStr = WideToAnsi(cString);
   hb_retc( pStr );
   hb_xfree(pStr);
#endif
   hb_xfree(cString);
}

HB_FUNC( REFRESHITEMBAR )
{
   HWND hWndSB;
   int  ptArray[40];  // Array defining the number of parts/sections
   int  nDev;
   int  s;
   int  nrOfParts;
   RECT rect;
   HDC  hDC;
   int  size;

   hWndSB    = hmg_par_HWND(1);
   size      = hb_parni(2);
   nrOfParts = ( int ) SendMessage(hWndSB, SB_GETPARTS, 40, 0);
   SendMessage(hWndSB, SB_GETPARTS, ( WPARAM ) 40, ( LPARAM ) ( LPINT ) ptArray);

   hDC = GetDC(hWndSB);
   GetClientRect(hWndSB, &rect);

   if( ( nrOfParts == 1 ) || ( IsZoomed( GetParent(hWndSB) ) ) || ( !(GetWindowLong(( HWND ) GetParent(hWndSB), GWL_STYLE) & WS_SIZEBOX) ) )
   {
      nDev = rect.right - ptArray[nrOfParts - 1];
   }
   else
   {
      nDev = rect.right - ptArray[nrOfParts - 1] - rect.bottom - rect.top + 2;
   }

   s = TRUE;
   if( rect.right > 0 )
   {
      for( int n = 0; n <= nrOfParts - 1; n++ )
      {

         if( n == 0 )
         {
            if( size >= ptArray[n] && nDev < 0 )
            {
               s = FALSE;
            }
            else
            {
               if( ptArray[n] + nDev < size )
               {
                  nDev = size - ptArray[n];
               }

               ptArray[n] += nDev;
            }
         }
         else if( s )
         {
            ptArray[n] += nDev;
         }

      }
   }

   ReleaseDC(hWndSB, hDC);

   SendMessage(hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray);
   hb_retni( nrOfParts );
}

HB_FUNC( KEYTOGGLE )
{
   BYTE pBuffer[256];
   WORD wKey = hmg_par_WORD(1);

   GetKeyboardState(pBuffer);

   if( pBuffer[wKey] & 0x01 )
   {
      pBuffer[wKey] &= 0xFE;
   }
   else
   {
      pBuffer[wKey] |= 0x01;
   }

   SetKeyboardState(pBuffer);
}

HB_FUNC( KEYTOGGLENT )
{
   BYTE wKey = hmg_par_BYTE(1);

   keybd_event(wKey, 0x45, KEYEVENTF_EXTENDEDKEY | 0, 0);
   keybd_event(wKey, 0x45, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
}

HB_FUNC( SETSTATUSITEMICON )
{
   HWND  hwnd;
   RECT  rect;
   HICON hIcon;
   int   cx;
   int   cy;

#ifndef UNICODE
   LPCSTR lpIconName = hb_parc(3);
#else
   LPWSTR lpIconName = AnsiToWide(( char * ) hb_parc(3));
#endif

   hwnd = hmg_par_HWND(1);

   // Unloads from memory current icon

   DestroyIcon(( HICON ) SendMessage(hwnd, SB_GETICON, hmg_par_WPARAM(2) - 1, ( LPARAM ) 0));

   GetClientRect(hwnd, &rect);

   cy = rect.bottom - rect.top - 4;
   cx = cy;

   hIcon = ( HICON ) LoadImage(GetResources(), lpIconName, IMAGE_ICON, cx, cy, 0);

   if( hIcon == nullptr )
   {
      hIcon = ( HICON ) LoadImage(nullptr, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE);
   }

   SendMessage(hwnd, SB_SETICON, hmg_par_WPARAM(2) - 1, ( LPARAM ) hIcon);

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpIconName);
#endif
}

HB_FUNC( SETSTATUSBARSIZE )
{
   HLOCAL hloc;
   LPINT  lpParts;

   HWND hwndStatus = hmg_par_HWND(1);
   int  nParts     = ( int ) hb_parinfa(2, 0);
   int  nWidth;

   // Set Widths from array

   hloc    = LocalAlloc(LHND, sizeof(int) * nParts);
   lpParts = ( LPINT ) LocalLock(hloc);

   nWidth = 0;

   for( int i = 0; i < nParts; i++ )
   {
      nWidth       = nWidth + HB_PARNI(2, i + 1);
      lpParts[i] = nWidth;
   }

   SendMessage(hwndStatus, SB_SETPARTS, ( WPARAM ) nParts, ( LPARAM ) lpParts);

   MoveWindow(hwndStatus, 0, 0, 0, 0, TRUE);

   LocalUnlock(hloc);
   LocalFree(hloc);
}

HB_FUNC( REFRESHPROGRESSITEM )       // RefreshProgressItem(HwndStatus, NrItem, hProgress)
{
   HWND hwndStatus = hmg_par_HWND(1);
   RECT rc;

   SendMessage(hwndStatus, SB_GETRECT, hmg_par_WPARAM(2) - 1, ( LPARAM ) &rc);
   SetWindowPos(hmg_par_HWND(3), 0, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
}

HB_FUNC( CREATEPROGRESSBARITEM )     // CreateProgressBarItem(HwndStatus, NrItem)
{
   HWND hwndStatus = hmg_par_HWND(1);
   HWND hwndProgressBar;
   RECT rc;
   int  style = WS_CHILD | PBS_SMOOTH;

   SendMessage(hwndStatus, SB_GETRECT, hmg_par_WPARAM(2) - 1, ( LPARAM ) &rc);
   if( hb_parni(3) )
   {
      style |= WS_VISIBLE;
   }

   if( ( hwndProgressBar = CreateWindowEx(
            0,
            PROGRESS_CLASS,
            ( LPCTSTR ) nullptr,
            style,
            rc.top,
            rc.left,
            rc.right - rc.left,
            rc.bottom - rc.top - 1, // No size or position.
            hwndStatus,             // Handle to the parent window.
            ( HMENU ) nullptr,         // ID for the progress window.
            GetInstance(),          // Current instance.
            ( LPVOID ) nullptr) ) != nullptr )
   {
      SendMessage(hwndProgressBar, PBM_SETRANGE, 0, MAKELONG(hb_parni(4), hb_parni(5)));
      SendMessage(hwndProgressBar, PBM_SETPOS, hmg_par_WPARAM(3), 0);

      hmg_ret_HANDLE(hwndProgressBar);
   }
   else // No application-defined data.
   {
      hmg_ret_HANDLE(nullptr);
   }
}

HB_FUNC( SETPOSPROGRESSBARITEM )     // SetPosProgressBarItem(HwndProgressBar, nPos)
{
   HWND hwndProgressBar = hmg_par_HWND(1);

   ShowWindow(hwndProgressBar, hb_parni(2) ? SW_SHOW : SW_HIDE);
   SendMessage(hwndProgressBar, PBM_SETPOS, hmg_par_WPARAM(2), 0);
}
