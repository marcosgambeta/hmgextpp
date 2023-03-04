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

#include <commctrl.h>

#include <hbvm.hpp>

#ifndef WC_EDIT
#define WC_EDIT  "Edit"
#endif

LRESULT CALLBACK  OwnEditProc(HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam);

HB_FUNC( INITMASKEDTEXTBOX )
{
   HWND hwnd;
   HWND hbutton;

   int style;
   int ExStyle;

   hwnd = hmg_par_HWND(1);

   style = WS_CHILD | ES_AUTOHSCROLL;

   if( hb_parl(9) )
   {
      style |= ES_UPPERCASE;
   }

   if( hb_parl(10) )
   {
      style |= ES_LOWERCASE;
   }

   if( hb_parl(12) )
   {
      style |= ES_RIGHT;
   }

   if( hb_parl(13) )
   {
      style |= ES_READONLY;
   }

   if( !hb_parl(14) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(15) )
   {
      style |= WS_TABSTOP;
   }

   ExStyle = hb_parl(16) ? 0 : WS_EX_CLIENTEDGE;

   hbutton = CreateWindowEx
             (
      ExStyle,
      WC_EDIT,
      TEXT(""),
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(11),
      hwnd,
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr
             );

   hmg_ret_HANDLE(hbutton);
}

HB_FUNC( INITTEXTBOX )
{
   HWND hwnd;           // Handle of the parent window/form.
   HWND hedit;          // Handle of the child window/control.

   int style;          // TEXTBOX window base style.
   int iExStyle;        // TEXTBOX window extended style.

   // Get the handle of the parent window/form.
   hwnd = hmg_par_HWND(1);

   style = WS_CHILD | ES_AUTOHSCROLL | BS_FLAT;

   if( hb_parl(12) )              // if <lNumeric> is TRUE, then ES_NUMBER style is added.
   {
      style |= ES_NUMBER;  // Set to a numeric TEXTBOX, so don't worry about other "textual" styles.
   }
   else
   {
      if( hb_parl(10) ) // if <lUpper> is TRUE, then ES_UPPERCASE style is added.
      {
         style |= ES_UPPERCASE;
      }

      if( hb_parl(11) ) // if <lLower> is TRUE, then ES_LOWERCASE style is added.
      {
         style |= ES_LOWERCASE;
      }
   }

   if( hb_parl(13) )  // if <lPassword> is TRUE, then ES_PASSWORD style is added.
   {
      style |= ES_PASSWORD;
   }

   if( hb_parl(14) )
   {
      style |= ES_RIGHT;
   }

   if( hb_parl(15) )
   {
      style |= ES_READONLY;
   }

   if( !hb_parl(16) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(17) )
   {
      style |= WS_TABSTOP;
   }

   iExStyle = hb_parl(18) ? 0 : WS_EX_CLIENTEDGE;

   // Creates the child control.
   hedit = CreateWindowEx
           (
      iExStyle,
      WC_EDIT,
      TEXT(""),
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(6),
      hwnd,
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr
           );

   SendMessage(hedit, ( UINT ) EM_LIMITTEXT, hmg_par_WPARAM(9), ( LPARAM ) 0);

   SetProp(( HWND ) hedit, TEXT("oldeditproc"), ( HWND ) GetWindowLongPtr(( HWND ) hedit, GWLP_WNDPROC));
   SetWindowLongPtr(hedit, GWLP_WNDPROC, ( LONG_PTR ) ( WNDPROC ) OwnEditProc);

   hmg_ret_HANDLE(hedit);
}

HB_FUNC( INITCHARMASKTEXTBOX )
{
   HWND hwnd;
   HWND hbutton;

   int style;
   int ExStyle;

   hwnd = hmg_par_HWND(1);

   style = WS_CHILD | ES_AUTOHSCROLL;

   if( hb_parl(9) )
   {
      style |= ES_UPPERCASE;
   }

   if( hb_parl(10) )
   {
      style |= ES_LOWERCASE;
   }

   if( hb_parl(12) )
   {
      style |= ES_RIGHT;
   }

   if( hb_parl(13) )
   {
      style |= ES_READONLY;
   }

   if( !hb_parl(14) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(15) )
   {
      style |= WS_TABSTOP;
   }

   ExStyle = hb_parl(16) ? 0 : WS_EX_CLIENTEDGE;

   hbutton = CreateWindowEx
             (
      ExStyle,
      WC_EDIT,
      TEXT(""),
      style,
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hb_parni(11),
      hwnd,
      hmg_par_HMENU(2),
      GetInstance(),
      nullptr
             );

   hmg_ret_HANDLE(hbutton);
}

LRESULT CALLBACK OwnEditProc(HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;
   long int        r;
   WNDPROC         OldWndProc;

   OldWndProc = ( WNDPROC ) ( LONG_PTR ) GetProp(hButton, TEXT("oldeditproc"));

   switch( Msg )
   {
      case WM_DESTROY:
         SetWindowLongPtr(hButton, GWLP_WNDPROC, ( LONG_PTR ) ( WNDPROC ) OldWndProc);
         RemoveProp(hButton, TEXT("oldeditproc"));
         break;

      case WM_CONTEXTMENU:
      case WM_CHAR:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol(hb_dynsymGet("OEDITEVENTS"));
         }

         if( pSymbol )
         {
            hb_vmPushSymbol(pSymbol);
            hb_vmPushNil();
            hb_vmPushNumInt(( LONG_PTR ) hButton);
            hb_vmPushLong(Msg);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);
         }

         r = hb_parnl( -1 );

         if( r != 0 )
         {
            return r;
         }
         else
         {
            return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
         }
   }

   return CallWindowProc(OldWndProc, hButton, Msg, wParam, lParam);
}
