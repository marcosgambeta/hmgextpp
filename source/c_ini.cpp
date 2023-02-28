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

#include "mgdefs.hpp"

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR  WideToAnsi(LPWSTR);
#endif

HB_FUNC( GETPRIVATEPROFILESTRING )
{
   DWORD   nSize = 256;
   TCHAR * bBuffer;
   DWORD   dwLen;

#ifndef UNICODE
   LPCSTR lpSection  = HB_ISCHAR(1) ? hb_parc(1) : nullptr;
   LPCSTR lpEntry    = HB_ISCHAR(2) ? hb_parc(2) : nullptr;
   LPCSTR lpDefault  = hb_parc(3);
   LPCSTR lpFileName = hb_parc(4);
#else
   LPCWSTR lpSection  = HB_ISCHAR(1) ? AnsiToWide(( char * ) hb_parc(1)) : nullptr;
   LPCWSTR lpEntry    = HB_ISCHAR(2) ? AnsiToWide(( char * ) hb_parc(2)) : nullptr;
   LPCWSTR lpDefault  = AnsiToWide(( char * ) hb_parc(3));
   LPCWSTR lpFileName = AnsiToWide(( char * ) hb_parc(4));
   LPSTR   pStr;
#endif

   do
   {
      nSize  *= 2;
      bBuffer = ( TCHAR * ) hb_xgrab(sizeof(TCHAR) * nSize);
      dwLen   = GetPrivateProfileString(lpSection, lpEntry, lpDefault, bBuffer, nSize, lpFileName);
   }
   while( dwLen >= nSize - 1 );

   if( dwLen )
   {
#ifndef UNICODE
      hb_retclen(( TCHAR * ) bBuffer, dwLen);
#else
      pStr = WideToAnsi(bBuffer);
      hb_retc( pStr );
      hb_xfree(pStr);
      hb_xfree(( TCHAR * ) lpFileName);
      hb_xfree(( TCHAR * ) lpDefault);
#endif
   }
   else
   {
#ifndef UNICODE
      hb_retc( lpDefault );
#else
      pStr = WideToAnsi(( LPWSTR ) lpDefault);
      hb_retc( pStr );
      hb_xfree(pStr);
      hb_xfree(( TCHAR * ) lpDefault);
#endif
   }

   hb_xfree(bBuffer);
}

HB_FUNC( WRITEPRIVATEPROFILESTRING )
{
#ifndef UNICODE
   LPCSTR lpSection  = hb_parc(1);
   LPCSTR lpEntry    = HB_ISCHAR(2) ? hb_parc(2) : nullptr;
   LPCSTR lpData     = HB_ISCHAR(3) ? hb_parc(3) : nullptr;
   LPCSTR lpFileName = hb_parc(4);
#else
   LPCWSTR lpSection  = AnsiToWide(( char * ) hb_parc(1));
   LPCWSTR lpEntry    = HB_ISCHAR(2) ? AnsiToWide(( char * ) hb_parc(2)) : nullptr;
   LPCWSTR lpData     = HB_ISCHAR(3) ? AnsiToWide(( char * ) hb_parc(3)) : nullptr;
   LPCWSTR lpFileName = AnsiToWide(( char * ) hb_parc(4));
#endif

   hb_retl( WritePrivateProfileString(lpSection, lpEntry, lpData, lpFileName) );
}

HB_FUNC( DELINIENTRY )
{
#ifndef UNICODE
   LPCSTR lpSection  = hb_parc(1);
   LPCSTR lpEntry    = hb_parc(2);
   LPCSTR lpFileName = hb_parc(3);
#else
   LPCWSTR lpSection  = AnsiToWide(( char * ) hb_parc(1));
   LPCWSTR lpEntry    = AnsiToWide(( char * ) hb_parc(2));
   LPCWSTR lpFileName = AnsiToWide(( char * ) hb_parc(3));
#endif
   hb_retl( WritePrivateProfileString(lpSection,      // Section
                                      lpEntry,        // Entry
                                      nullptr,           // String
                                      lpFileName) ); // INI File
}

HB_FUNC( DELINISECTION )
{
#ifndef UNICODE
   LPCSTR lpSection  = hb_parc(1);
   LPCSTR lpFileName = hb_parc(2);
#else
   LPCWSTR lpSection  = AnsiToWide(( char * ) hb_parc(1));
   LPCWSTR lpFileName = AnsiToWide(( char * ) hb_parc(2));
#endif
   hb_retl( WritePrivateProfileString(lpSection,      // Section
                                      nullptr,           // Entry
                                      "",     // String
                                      lpFileName) ); // INI File
}

static TCHAR * FindFirstSubString(TCHAR * Strings)
{
   TCHAR * p = Strings;

   if( *p == 0 )
   {
      p = nullptr;
   }
   return p;
}

static TCHAR * FindNextSubString(TCHAR * Strings)
{
   TCHAR * p = Strings;

   p = p + lstrlen(Strings) + 1;
   if( *p == 0 )
   {
      p = nullptr;
   }
   return p;
}

static INT FindLenSubString(TCHAR * Strings)
{
   INT     i = 0;
   TCHAR * p = Strings;

   if( ( p = FindFirstSubString(p) ) != nullptr )
   {
      for( i = 1; ( p = FindNextSubString(p) ) != nullptr; i++ )
      {
         ;
      }
   }
   return i;
}

// (JK) HMG 1.0 Experimental build 6

HB_FUNC( _GETPRIVATEPROFILESECTIONNAMES )
{
   TCHAR   bBuffer[32767];
   TCHAR * p;
   INT     nLen;

#ifndef UNICODE
   LPCSTR lpFileName = hb_parc(1);
#else
   LPCWSTR lpFileName = AnsiToWide(( char * ) hb_parc(1));
   LPSTR   pStr;
#endif

   ZeroMemory(bBuffer, sizeof(bBuffer));
   GetPrivateProfileSectionNames(bBuffer, sizeof(bBuffer) / sizeof(TCHAR), lpFileName);

   p    = ( TCHAR * ) bBuffer;
   nLen = FindLenSubString(p);
   hb_reta(nLen);
   if( nLen > 0 )
   {
#ifndef UNICODE
      HB_STORC( ( p = FindFirstSubString(p) ), -1, 1 );
      for( INT i = 2; ( p = FindNextSubString(p) ) != nullptr; i++ )
      {
         HB_STORC( p, -1, i );
      }
#else
      p    = FindFirstSubString(p);
      pStr = WideToAnsi(p);
      HB_STORC( pStr, -1, 1 );
      for( INT i = 2; ( p = FindNextSubString(p) ) != nullptr; i++ )
      {
         pStr = WideToAnsi(p);
         HB_STORC( pStr, -1, i );
      }
      hb_xfree(pStr);
#endif
   }
}

// Used to retrieve all key/value pairs of a given section.

HB_FUNC( _GETPRIVATEPROFILESECTION )
{
   TCHAR   bBuffer[32767];
   TCHAR * p;
   INT     nLen;

#ifndef UNICODE
   LPCSTR lpSectionName = hb_parc(1);
   LPCSTR lpFileName    = hb_parc(2);
#else
   LPCWSTR lpSectionName = AnsiToWide(( char * ) hb_parc(1));
   LPCWSTR lpFileName    = AnsiToWide(( char * ) hb_parc(2));
   LPSTR   pStr;
#endif

   ZeroMemory(bBuffer, sizeof(bBuffer));
   GetPrivateProfileSection(lpSectionName, bBuffer, sizeof(bBuffer) / sizeof(TCHAR), lpFileName);
   p    = ( TCHAR * ) bBuffer;
   nLen = FindLenSubString(p);
   hb_reta(nLen);
   if( nLen > 0 )
   {
#ifndef UNICODE
      HB_STORC( ( p = FindFirstSubString(p) ), -1, 1 );
      for( INT i = 2; ( p = FindNextSubString(p) ) != nullptr; i++ )
      {
         HB_STORC( p, -1, i );
      }
#else
      p    = FindFirstSubString(p);
      pStr = WideToAnsi(p);
      HB_STORC( pStr, -1, 1 );
      for( INT i = 2; ( p = FindNextSubString(p) ) != nullptr; i++ )
      {
         pStr = WideToAnsi(p);
         HB_STORC( pStr, -1, i );
      }
      hb_xfree(pStr);
#endif
   }
}
