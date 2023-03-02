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

/*
   File:           c_cursor.c
   Contributors:   Jacek Kubica <kubica@wssk.wroc.pl>
                   Grigory Filatov <gfilatov@gmail.com>
   Description:    Mouse Cursor Shapes handling for MiniGUI
   Status:         Public Domain
 */

#include "mgdefs.hpp"
#include <hbwinuni.hpp>

HINSTANCE GetInstance(void);
HINSTANCE GetResources(void);

/*
LOADCURSOR(HINSTANCE, cCursor|nCursor) --> HANDLE|NIL
*/
HB_FUNC( LOADCURSOR )
{
   HINSTANCE hInstance = HB_ISNIL(1) ? nullptr : hmg_par_HINSTANCE(1);

   if( HB_ISCHAR(2) )
   {
      void * str;
      LPCTSTR lpCursorName = HB_PARSTR(2, &str, nullptr);
      hmg_ret_HANDLE(LoadCursor(hInstance, lpCursorName));
      hb_strfree(str);
   }
   else if( HB_ISNUM(2) )
   {
      hmg_ret_HANDLE(LoadCursor(hInstance, MAKEINTRESOURCE(hb_parni(2))));
   }
}

/*
LOADCURSORFROMFILE(cFileName) --> HANDLE
*/
HB_FUNC( LOADCURSORFROMFILE )
{
   void * str;
   hmg_ret_HANDLE(LoadCursorFromFile(HB_PARSTR(1, &str, nullptr)));
   hb_strfree(str);
}

/*
SETRESCURSOR(HCURSOR) --> HANDLE
*/
HB_FUNC( SETRESCURSOR )
{
   hmg_ret_HANDLE(SetCursor(( HCURSOR ) HB_PARNL(1)));
}

/*
FILECURSOR(cFileName) --> HANDLE
*/
HB_FUNC( FILECURSOR )
{
   void * str;
   hmg_ret_HANDLE(SetCursor(LoadCursorFromFile(HB_PARSTR(1, &str, nullptr))));
   hb_strfree(str);
}

/*
CURSORHAND() --> HANDLE
*/
HB_FUNC( CURSORHAND )
{
   hmg_ret_HANDLE(SetCursor(LoadCursor(nullptr, IDC_HAND)));
}

/*
SETWINDOWCURSOR(HWND, cp2|np2) --> NIL
*/
HB_FUNC( SETWINDOWCURSOR )
{
  if( HB_ISCHAR(2) )
  {
     void * str;
     LPCTSTR lpCursorName = HB_PARSTR(2, &str, nullptr);
     HCURSOR ch = LoadCursor(GetResources(), lpCursorName);

     if( ch == nullptr )
     {
        ch = LoadCursorFromFile(lpCursorName);
     }

     if( ch != nullptr )
     {
        SetClassLongPtr(hmg_par_HWND(1), GCLP_HCURSOR, ( LONG_PTR ) ch);
     }

     hb_strfree(str);
  }
  else if( HB_ISNUM(2) )
  {
     HCURSOR ch = LoadCursor(nullptr, MAKEINTRESOURCE(hb_parni(2)));

     if( ch != nullptr )
     {
        SetClassLongPtr(hmg_par_HWND(1), GCLP_HCURSOR, ( LONG_PTR ) ch);
     }
  }
}

/*
SETHANDCURSOR(HWND) --> NIL
*/
HB_FUNC( SETHANDCURSOR )
{
   SetClassLongPtr(hmg_par_HWND(1), GCLP_HCURSOR, ( LONG_PTR ) LoadCursor(nullptr, IDC_HAND));
}
