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
 */

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>

#if (defined(__BORLANDC__) && defined(_WIN64))
#define PtrToLong(p) (static_cast<LONG>(p))
#endif

/*
REGCLOSEKEY(HKEY) --> numeric
*/
HB_FUNC( REGCLOSEKEY )
{
   hb_retnl((RegCloseKey(hmg_par_HKEY(1)) == ERROR_SUCCESS) ? ERROR_SUCCESS : -1);
}

/*
REGOPENKEYEX(HKEY, cKey, p3, p4, p5) --> numeric
*/
HB_FUNC( REGOPENKEYEX )
{
   HKEY phwHandle;

   void * str;
   long lError = RegOpenKeyEx(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), 0, static_cast<REGSAM>(hb_parnl(4)), &phwHandle);
   hb_strfree(str);

   if( lError != ERROR_SUCCESS ) {
      hb_retnl(-1);
   } else {
      HB_STORNL(PtrToLong(phwHandle), 5);
      hb_retnl(0);
   }

}

HB_FUNC( REGOPENKEYEXA ) // INFO: deprecated
{
   HB_FUNC_EXEC( REGOPENKEYEX );
}

/*
REGQUERYVALUEEX(HKEY, cKey, p3, p4, p5, p6) --> numeric
*/
HB_FUNC( REGQUERYVALUEEX )
{
   DWORD lpType = hb_parnl(4);
   DWORD lpcbData = 0;

   void * str;
   long lError = RegQueryValueEx(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), nullptr, &lpType, nullptr, &lpcbData);
   hb_strfree(str);

   if( lError == ERROR_SUCCESS ) {
      BYTE * lpData = static_cast<BYTE*>(hb_xgrab(lpcbData + 1));
      void * str;
      lError = RegQueryValueEx(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), nullptr, &lpType, static_cast<BYTE*>(lpData), &lpcbData);
      hb_strfree(str);

      if( lError != ERROR_SUCCESS ) {
         hb_retnl(-1);
      } else {
         HB_STORNL(lpType, 4);
         hb_storc(reinterpret_cast<char*>(lpData), 5);
         HB_STORNL(lpcbData, 6);

         hb_retnl(0);
      }

      hb_xfree(lpData);
   } else {
      hb_retnl(-1);
   }
}

HB_FUNC( REGQUERYVALUEEXA ) // INFO: deprecated
{
  HB_FUNC_EXEC( REGQUERYVALUEEX );
}

/*
REGENUMKEYEX(HKEY, cKey, p3, p4, p5, p6, p7) --> numeric
*/
HB_FUNC( REGENUMKEYEX )
{
   TCHAR Buffer[255];
   DWORD dwBuffSize = 255;
   TCHAR Class[255];
   DWORD dwClass = 255;
   FILETIME ft;

   long bErr = RegEnumKeyEx(hmg_par_HKEY(1), hb_parnl(2), Buffer, &dwBuffSize, nullptr, Class, &dwClass, &ft);

   if( bErr != ERROR_SUCCESS ) {
      hb_retnl(-1);
   } else {
      hb_storc(static_cast<const char*>(Buffer), 3);
      HB_STORNL(dwBuffSize, 4);
      hb_storc(static_cast<const char*>(Class), 6);
      HB_STORNL(dwClass, 7);
      hb_retnl(0);
   }
}

HB_FUNC( REGENUMKEYEXA ) // INFO: deprecated
{
  HB_FUNC_EXEC( REGENUMKEYEX );
}

/*
REGSETVALUEEX(HKEY, cKey, p3, p4, p5) --> numeric
*/
HB_FUNC( REGSETVALUEEX )
{
   DWORD nType = hb_parnl(4);

   if( nType != REG_DWORD ) {
      void * str;
      hb_retnl((RegSetValueEx(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), 0, hb_parnl(4), reinterpret_cast<BYTE*>(const_cast<char*>(hb_parc(5))), static_cast<DWORD>(hb_parclen(5)) + 1) == ERROR_SUCCESS) ? 0 : -1);
      hb_strfree(str);
   } else {
      void * str;
      DWORD nSpace = hb_parnl(5);
      hb_retnl((RegSetValueEx(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), 0, hb_parnl(4), reinterpret_cast<BYTE*>(&nSpace), sizeof(REG_DWORD)) == ERROR_SUCCESS) ? 0 : -1);
      hb_strfree(str);
   }
}

HB_FUNC( REGSETVALUEEXA ) // INFO: deprecated
{
  HB_FUNC_EXEC( REGSETVALUEEX );
}

/*
REGCREATEKEY(HKEY, cKey, np3) --> numeric
*/
HB_FUNC( REGCREATEKEY )
{
   void * str;
   HKEY hKey;

   if( RegCreateKey(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr), &hKey) == ERROR_SUCCESS ) {
      HB_STORNL(PtrToLong(hKey), 3);
      hb_retnl(0);
   } else {
      hb_retnl(-1);
   }

   hb_strfree(str);
}

/*
REGENUMVALUE(HKEY, p2, p3, p4, p5, p6, p7, p8) --> numeric
*/
HB_FUNC( REGENUMVALUE )
{
   DWORD lpType = 1;
   TCHAR Buffer[255];
   DWORD dwBuffSize = 255;
   DWORD dwClass = 255;

   long lError = RegEnumValue(hmg_par_HKEY(1), hb_parnl(2), Buffer, &dwBuffSize, nullptr, &lpType, nullptr, &dwClass);

   if( lError != ERROR_SUCCESS ) {
      hb_retnl(-1);
   } else {
      hb_storc(static_cast<const char*>(Buffer), 3);
      HB_STORNL(dwBuffSize, 4);
      HB_STORNL(lpType, 6);
      HB_STORNL(dwClass, 8);
      hb_retnl(lError);
   }
}

HB_FUNC( REGENUMVALUEA ) // INFO: deprecated
{
  HB_FUNC_EXEC( REGENUMVALUE );
}

/*
REGDELETEKEY(HKEY, cKey) --> numeric
*/
HB_FUNC( REGDELETEKEY )
{
   void * str;
   hb_retnl(RegDeleteKey(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr)));
   hb_strfree(str);
}

/*
REGDELETEVALUE(HKEY, cKey) --> numeric
*/
HB_FUNC( REGDELETEVALUE )
{
   void * str;
   hb_retnl((RegDeleteValue(hmg_par_HKEY(1), HB_PARSTR(2, &str, nullptr)) == ERROR_SUCCESS) ? 0 : -1);
   hb_strfree(str);
}

HB_FUNC( REGDELETEVALUEA ) // INFO: deprecated
{
  HB_FUNC_EXEC( REGDELETEVALUE );
}

/*
REGCONNECTREGISTRY(cp1, HKEY) --> numeric
*/
HB_FUNC( REGCONNECTREGISTRY )
{
   void * str;
   HKEY phwHandle;

   long lError = RegConnectRegistry(HB_PARSTR(1, &str, nullptr), hmg_par_HKEY(2), &phwHandle);

   if( lError != ERROR_SUCCESS ) {
      hb_retnl(-1);
   } else {
      HB_STORNL(PtrToLong(phwHandle), 3);
      hb_retnl(lError);
   }

   hb_strfree(str);
}
