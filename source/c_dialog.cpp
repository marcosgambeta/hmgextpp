/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * DIALOG form source code
 * (c)2005-2008 Janusz Pora <januszpora@onet.eu>
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

#define _WIN32_IE  0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

LRESULT CALLBACK HMG_DlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   if( !pSymbol )
   {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("DIALOGPROC"));
   }

   if( pSymbol )
   {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hb_vmPushNumInt(( LONG_PTR ) hWnd);
      hb_vmPushLong(message);
      hb_vmPushNumInt(wParam);
      hb_vmPushNumInt(lParam);
      hb_vmDo(4);
   }

   return hb_parnl(-1);
}

LRESULT CALLBACK HMG_ModalDlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB pSymbol = nullptr;

   if( !pSymbol )
   {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("MODALDIALOGPROC"));
   }

   if( pSymbol )
   {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hb_vmPushNumInt(( LONG_PTR ) hWnd);
      hb_vmPushLong(message);
      hb_vmPushNumInt(wParam);
      hb_vmPushNumInt(lParam);
      hb_vmDo(4);
   }

   return hb_parnl(-1);
}

/*
INITMODALDIALOG(HWND, np2) --> LRESULT
*/
HB_FUNC( INITMODALDIALOG )
{
   HB_RETNL(( LONG_PTR ) DialogBox(GetResources(), MAKEINTRESOURCE(hb_parni(2)), hmg_par_HWND(1), ( DLGPROC ) HMG_ModalDlgProc));
}

/*
INITDIALOG(HWND, np2) --> HANDLE
*/
HB_FUNC( INITDIALOG )
{
   hmg_ret_HANDLE(CreateDialog(GetResources(), MAKEINTRESOURCE(hb_parni(2)), hmg_par_HWND(1), ( DLGPROC ) HMG_DlgProc));
}

/*
GETDIALOGITEMHANDLE(HWND, nID) --> HANDLE
*/
HB_FUNC( GETDIALOGITEMHANDLE )
{
   hmg_ret_HANDLE(GetDlgItem(hmg_par_HWND(1), hmg_par_int(2)));
}

/*
CHECKDLGBUTTON(nIDButton, HWND) --> NIL // TODO: return .T. or .F.
*/
HB_FUNC( CHECKDLGBUTTON )
{
   CheckDlgButton(hmg_par_HWND(2), hmg_par_int(1), BST_CHECKED);
}

/*
UNCHECKDLGBUTTON(nIDButton, HWND) --> NIL // TODO: return .T. or .F.
*/
HB_FUNC( UNCHECKDLGBUTTON )
{
   CheckDlgButton(hmg_par_HWND(2), hmg_par_int(1), BST_UNCHECKED);
}

/*
SETDIALOGITEMTEXT(HWND, nId, cText) --> .T.|.F.
*/
HB_FUNC( SETDIALOGITEMTEXT )
{
   void * str;
   hb_retl(SetDlgItemText(hmg_par_HWND(1), hmg_par_int(2), HB_PARSTR(3, &str, nullptr)));
   hb_strfree(str);
}

/*
ENDDIALOG(HWND, nResult) --> NIL
*/
HB_FUNC( ENDDIALOG )
{
   EndDialog(hmg_par_HWND(1), hb_parni(2));
}

/*
ADDDIALOGPAGES(HWND, ap2, np3) --> NIL
*/
HB_FUNC( ADDDIALOGPAGES )
{
   HWND hwnd = hmg_par_HWND(1);
   int l = ( int ) hb_parinfa(2, 0) - 1;
   PHB_ITEM hArray = hb_param(2, Harbour::Item::ARRAY);

   TC_ITEM tie;
   tie.mask = TCIF_TEXT;
   tie.iImage = -1;

   for( int i = l; i >= 0; i = i - 1 )
   {
      tie.pszText = ( TCHAR * ) hb_arrayGetCPtr(hArray, i + 1);
      TabCtrl_InsertItem(hwnd, 0, &tie);
   }

   TabCtrl_SetCurSel(hwnd, hb_parni(3) - 1);
   TabCtrl_SetCurFocus(hwnd, hb_parni(3) - 1);
}

/*
GETDLGCTRLID(HWND) --> numeric
*/
HB_FUNC( GETDLGCTRLID )
{
   hb_retni(GetDlgCtrlID(hmg_par_HWND(1)));
}

/*
SETDLGITEMINT(HWND, nId, nValue, lSigned) --> NIL // TODO: return .T. or .F.
*/
HB_FUNC( SETDLGITEMINT )
{
   SetDlgItemInt(hmg_par_HWND(1), hmg_par_int(2), hmg_par_UINT(3), (hb_pcount() < 4 || HB_ISNIL(4) || !hb_parl(4)) ? false : true);
}

/*
GETDLGITEMTEXT(HWND, nId, np3) --> cText
*/
HB_FUNC( GETDLGITEMTEXT )
{
   int strlen = hb_parni(3);
   TCHAR * str = new TCHAR[strlen + 1];
   GetDlgItemText(hmg_par_HWND(1), hmg_par_int(2), str, strlen);
   HB_RETSTR(str);
   delete[] str;
}

/*
GETEDITTEXT(HWND, nId) --> cText
*/
HB_FUNC( GETEDITTEXT )
{
   HWND hDlg = hmg_par_HWND(1);
   int id = hmg_par_int(2);
   USHORT strlen = ( USHORT ) SendMessage(GetDlgItem(hDlg, id), WM_GETTEXTLENGTH, 0, 0);
   TCHAR * str = new TCHAR[strlen + 2];
   GetDlgItemText(hDlg, id, str, strlen + 1);
   HB_RETSTR(str);
   delete[] str;
}

/*
CHECKRADIOBUTTON(HWND, nIDFirstButton, nIDLastButton, nIDCheckButton) --> NIL // TODO: return .T. or .F.
*/
HB_FUNC( CHECKRADIOBUTTON )
{
   CheckRadioButton(hmg_par_HWND(1), hmg_par_int(2), hmg_par_int(3), hmg_par_int(4));
}

/*
ISDLGBUTTONCHECKED(HWND, nIDButton) --> .T.|.F.
*/
HB_FUNC( ISDLGBUTTONCHECKED )
{
   hb_retl(IsDlgButtonChecked(hmg_par_HWND(1), hmg_par_int(2)) == BST_CHECKED ? true : false);
}

static LPWORD lpwAlign(LPWORD lpIn)
{
   ULONG_PTR ul = ( ULONG_PTR ) lpIn;
   ul += 3;
   ul >>= 2;
   ul <<= 2;
   return ( LPWORD ) ul;
}

static int nCopyAnsiToWideChar(LPWORD lpWCStr, LPCSTR lpAnsiIn)
{
   int CodePage = GetACP();
   int nDstLen = MultiByteToWideChar(CodePage, 0, lpAnsiIn, -1, nullptr, 0);

   if( nDstLen > 0 )
   {
      LPWSTR pszDst = ( LPWSTR ) hb_xgrab(nDstLen * 2);

      MultiByteToWideChar(CodePage, 0, lpAnsiIn, -1, pszDst, nDstLen);

      for( int i = 0; i < nDstLen; i++ )
      {
         *(lpWCStr + i) = *(pszDst + i);
      }

      hb_xfree(pszDst);
   }

   return nDstLen;
}

HB_SIZE GetSizeDlgTemp(PHB_ITEM dArray, PHB_ITEM cArray)
{
   PHB_ITEM iArray;
   HB_SIZE lTemplateSize = 36;
   int nItem = ( int ) hb_arrayLen(cArray);
   HB_SIZE ln = hb_arrayGetCLen(dArray, 10);    //caption
   lTemplateSize += ln * 2;

   if( hb_arrayGetNI( dArray, 4 ) & DS_SETFONT )
   {
      ln = hb_arrayGetCLen(dArray, 11); //fontname
      lTemplateSize += ln * 2;
      lTemplateSize += 3;
   }

   for( int s = 0; s < nItem; s++ )
   {
      iArray = ( PHB_ITEM ) hb_arrayGetItemPtr(cArray, s + 1);
      lTemplateSize += 36;
      ln = hb_arrayGetCLen(iArray, 3);  //class
      lTemplateSize += ln * 2;
      ln = hb_arrayGetCLen(iArray, 10); //caption
      lTemplateSize += ln * 2;
   }

   return lTemplateSize;
}

PWORD CreateDlgTemplate(long lTemplateSize, PHB_ITEM dArray, PHB_ITEM cArray)
{
   LONG baseUnit = GetDialogBaseUnits();
   int baseunitX = LOWORD(baseUnit);
   int baseunitY = HIWORD(baseUnit);
   int nItem, x, y, w, h;
   WORD iPointSize;
   PHB_ITEM iArray;
   #ifndef _WIN64
   ULONG style, ExStyle, HelpId;
   #else
   HB_LONGLONG style, ExStyle, HelpId;
   #endif
   ULONG Id;
   char * strtemp;
   int nchar;

   PWORD pdlgtemplate = ( WORD * ) LocalAlloc(LPTR, lTemplateSize);

   PWORD pw = pdlgtemplate;

   ExStyle = HB_arrayGetNL( dArray, 5 );  //ExStyle
   style   = HB_arrayGetNL( dArray, 4 );  //style
   x       = hb_arrayGetNI( dArray, 6 );  //x
   y       = hb_arrayGetNI( dArray, 7 );  //y
   w       = hb_arrayGetNI( dArray, 8 );  //w
   h       = hb_arrayGetNI( dArray, 9 );  //h
   nItem   = ( int ) hb_arrayLen(cArray);

   *pw++   = 1;            // DlgVer
   *pw++   = 0xFFFF;       // Signature
   *pw++   = 0;            // LOWORD HelpID
   *pw++   = 0;            // HIWORD HelpID
   *pw++   = LOWORD(ExStyle);
   *pw++   = HIWORD(ExStyle);
   *pw++   = LOWORD(style);
   *pw++   = HIWORD(style);
   *pw++   = ( WORD ) nItem;                              // NumberOfItems
   *pw++   = ( WORD ) MulDiv(x, 4, baseunitX);          // x
   *pw++   = ( WORD ) MulDiv(y, 8, baseunitY);          // y
   *pw++   = ( WORD ) MulDiv(w, 4, baseunitX);          // cx
   *pw++   = ( WORD ) MulDiv(h, 8, baseunitY);          // cy
   *pw++   = 0;                                           // Menu
   *pw++   = 0;                                           // Class
   strtemp = ( char * ) hb_arrayGetCPtr(dArray, 10);    //caption
   nchar   = nCopyAnsiToWideChar( pw, strtemp );
   pw     += nchar;
   if( hb_arrayGetNI( dArray, 4 ) & DS_SETFONT )
   {
      iPointSize = ( WORD ) hb_arrayGetNI( dArray, 12 );                //fontsize
      *pw++      = iPointSize;
      *pw++      = ( WORD ) ( hb_arrayGetL( dArray, 13 ) ? 700 : 400 ); //bold
      *pw++      = ( WORD ) hb_arrayGetL( dArray, 14 );
      strtemp    = ( char * ) hb_arrayGetCPtr(dArray, 11);            //font
      nchar      = nCopyAnsiToWideChar( pw, strtemp );
      pw        += nchar;
   }

   for( int s = 0; s < nItem; s = s + 1 )
   {
      iArray = ( PHB_ITEM ) hb_arrayGetItemPtr(cArray, s + 1);
      pw     = lpwAlign(pw);

      HelpId  = HB_arrayGetNL( iArray, 11 ); //HelpId
      ExStyle = HB_arrayGetNL( iArray, 5 );  //exstyle
      style   = HB_arrayGetNL( iArray, 4 );  //style  item
      Id      = hb_arrayGetNI( iArray, 1 );

      *pw++ = LOWORD(HelpId);
      *pw++ = HIWORD(HelpId);
      *pw++ = LOWORD(ExStyle);
      *pw++ = HIWORD(ExStyle);
      *pw++ = LOWORD(style);
      *pw++ = HIWORD(style);
      *pw++ = ( WORD ) MulDiv(hb_arrayGetNI(iArray, 6), 4, baseunitX); // x
      *pw++ = ( WORD ) MulDiv(hb_arrayGetNI(iArray, 7), 8, baseunitY); // y
      *pw++ = ( WORD ) MulDiv(hb_arrayGetNI(iArray, 8), 4, baseunitX); // cx
      *pw++ = ( WORD ) MulDiv(hb_arrayGetNI(iArray, 9), 8, baseunitY); // cy
      *pw++ = ( WORD ) Id;                                                 // LOWORD (Control ID)
      *pw++ = 0;                                                           // HOWORD (Control ID)

      strtemp = ( char * ) hb_arrayGetCPtr(iArray, 3);                   //class
      nchar   = nCopyAnsiToWideChar( pw, strtemp );
      pw     += nchar;

      strtemp = ( char * ) hb_arrayGetCPtr(iArray, 10); //caption
      nchar   = nCopyAnsiToWideChar( pw, strtemp );
      pw     += nchar;
      *pw++   = 0; // Advance pointer over nExtraStuff WORD.
   }

   *pw = 0;       // Number of bytes of extra data.

   return pdlgtemplate;
}

/*
CREATEDLGTEMPLATE(HWND, ap2, ap3) --> LRESULT|HANDLE
*/
HB_FUNC( CREATEDLGTEMPLATE )
{
   PHB_ITEM dArray = hb_param(2, Harbour::Item::ARRAY);
   PHB_ITEM cArray = hb_param(3, Harbour::Item::ARRAY);
   BOOL modal = hb_arrayGetL(dArray, 3);
   HB_SIZE lTemplateSize = ( long ) GetSizeDlgTemp(dArray, cArray);
   PWORD pdlgtemplate = CreateDlgTemplate(( long ) lTemplateSize, dArray, cArray);

   if( modal )
   {
      LRESULT lResult = DialogBoxIndirect(GetResources(), ( LPDLGTEMPLATE ) pdlgtemplate, hmg_par_HWND(1), ( DLGPROC ) HMG_ModalDlgProc);
      LocalFree(pdlgtemplate);
      HB_RETNL(( LONG_PTR ) lResult);
   }
   else
   {
      HWND hwndDlg = CreateDialogIndirect(GetResources(), ( LPDLGTEMPLATE ) pdlgtemplate, hmg_par_HWND(1), ( DLGPROC ) HMG_DlgProc);
      LocalFree(pdlgtemplate);
      hmg_ret_HANDLE(hwndDlg);
   }
}

/*
INITEXCOMMONCONTROLS(np) --> NIL
*/
HB_FUNC( INITEXCOMMONCONTROLS )
{
   INITCOMMONCONTROLSEX i;

   i.dwSize = sizeof(INITCOMMONCONTROLSEX);

   switch( hb_parni(1) )
   {
      case 1:  i.dwICC = ICC_DATE_CLASSES;     break;
      case 2:  i.dwICC = ICC_TREEVIEW_CLASSES; break;
      case 3:  i.dwICC = ICC_INTERNET_CLASSES; break;
      default: i.dwICC = ICC_DATE_CLASSES;
   }

   InitCommonControlsEx(&i);
}
