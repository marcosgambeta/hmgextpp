/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Property Sheet control source code
 * (C)2008 Janusz Pora <januszpora@onet.eu>
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
 * Copyright 2002 AJ Wos <andrwos@aust1.net>
 *
 * "HWGUI"
 * Copyright 2001-2009 Alexander S.Kresin <alex@belacy.belgorod.su>
 */

#define _WIN32_IE 0x0500
#define _WIN32_WINNT 0x0400


#include <shlobj.h>
#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <prsht.h>
#include <hbapi.hpp>
#include <hbvm.hpp>
#include <hbstack.hpp>
#include <hbapiitm.hpp>
#include "mgdefs.hpp"

extern PWORD   CreateDlgTemplate(long lTemplateSize,  PHB_ITEM dArray,PHB_ITEM cArray);
extern long    GetSizeDlgTemp(PHB_ITEM dArray, PHB_ITEM cArray);

/****************************************************************************/
LRESULT CALLBACK HMG_PageDlgProc(HWND hWndDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_SYMB   pSymbol = nullptr;
   static PHB_SYMB   pSymbol2 = nullptr;
   static PHB_SYMB   pSymbol3 = nullptr;
   long int          r;
   int               nPage, nId;
   LPNMHDR     lpnmhdr;
   PSHNOTIFY   *psn;

   HWND        hWndParent = GetParent(hWndDlg);
   static PROPSHEETPAGE * ps;
   switch (message){
   // wNotifyCode = HIWORD(wParam); // notification code
   // wID = LOWORD(wParam);         // item, control, or accelerator identifier
   // hwndCtl = (HWND) lParam;      // handle of control

   case WM_INITDIALOG:
      {
      ps = (PROPSHEETPAGE *)lParam;
      if( !pSymbol2 )
      {
         pSymbol2 = hb_dynsymSymbol(hb_dynsymGet("INITPAGEDLGPROC"));
      }
      if( pSymbol2 )
      {
         hb_vmPushSymbol(pSymbol2);
         hb_vmPushNil();
         hmg_vmPushHandle(hWndDlg);
         hb_vmPushLong(ps->lParam);
         hmg_vmPushHandle(hWndParent);
         hb_vmDo(3);
      }
      return (TRUE);
      }
   case WM_DESTROY:
      break;
   case WM_COMMAND:
      break;
   case WM_NOTIFY:
      lpnmhdr  = (NMHDR FAR *)lParam;
      psn = (PSHNOTIFY *)lParam;

      nPage    = PropSheet_HwndToIndex(hWndParent,  hWndDlg);
      nId      = PropSheet_IndexToId( hWndParent,  nPage );

      if( !pSymbol3 )
      {
         pSymbol3 = hb_dynsymSymbol(hb_dynsymGet("BUTTONPAGEDLGPROC"));
      }
      if( pSymbol3 )
      {
         hb_vmPushSymbol(pSymbol3);
         hb_vmPushNil();
         hmg_vmPushHandle(hWndDlg);
         hb_vmPushLong((LONG) lpnmhdr->code);
         hb_vmPushLong(nId);
         hb_vmPushLong(nPage);
         hb_vmDo(4);
      }

      r = hb_parnl( -1 );

      switch(psn->hdr.code) {
         case PSN_APPLY:   //sent when OK or Apply button pressed
            {
            if (psn->lParam == FALSE)  // Apply pressed
            {
            if (r)
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_NOERROR);
            else
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_INVALID_NOCHANGEPAGE);
            }
            break;
            }
         case PSN_RESET:   //sent when Cancel button pressed
            {
            if (r) //Not finished yet.
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
            else
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_INVALID_NOCHANGEPAGE);
            break;
            }
         case PSN_QUERYCANCEL: //sent when Quit button pressed
            {
            if (r) //Not finished yet.
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
            else {
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, TRUE);
               return(TRUE);
               }
            break;
            }
         case PSN_KILLACTIVE:
            {
            if (r)
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
            else    //Not valid.
               SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, TRUE);
            break;
            }
         case PSN_SETACTIVE:
            //this will be ignored if the property sheet is not a wizard
            break;
         default:
            break;
         }
         break;

   default:
      break;
   }

   if( !pSymbol )
   {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("PAGEDLGPROC"));
   }

   if( pSymbol )
   {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hmg_vmPushHandle(hWndParent);
      hmg_vmPushHandle(hWndDlg);
      hb_vmPushLong(message);
      hb_vmPushLong(wParam);
      hb_vmPushLong(lParam);
      hb_vmDo(5);
   }

   return (FALSE);
}

/****************************************************************************/
LRESULT CALLBACK HMG_PropSheetProc(HWND hwndPropSheet, UINT message, LPARAM lParam)
{
   static PHB_SYMB  pSymbol = nullptr;

   switch(message){
   //called before the dialog is created,
   //hwndPropSheet = nullptr, lParam points
   //lpTemplate = {style, dwExtendStyle, cdit, x, y, cx, cy }  //ToDo

   case PSCB_PRECREATE:
      {
      LPDLGTEMPLATE lpTemplate = (LPDLGTEMPLATE)lParam;

      if(!(lpTemplate->style & WS_SYSMENU))
         lpTemplate->style |= WS_SYSMENU;
      }
   }
   if( !pSymbol )
   {
      pSymbol = hb_dynsymSymbol(hb_dynsymGet("PROPSHEETPROC"));
   }

   if( pSymbol )
   {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hmg_vmPushHandle(hwndPropSheet);
      hb_vmPushLong(message);
      hb_vmPushLong(lParam);
      hb_vmDo(3);
   }

   return (FALSE);
}

/****************************************************************************
 CreatePropertySeeetPage(_HMG_aPropSheetPagesTemp)
*****************************************************************************/

HB_FUNC( CREATEPROPERTYSEEETPAGE )
{
   HPROPSHEETPAGE hPage;
   PROPSHEETPAGE psp; memset(&psp, 0, sizeof(psp));
   PHB_ITEM sArray;
   char    *strTitle;
   char    *strHdTitle;
   char    *strSubHdTitle;
   int     idRC, PageStyle;

   sArray = hb_param(1, Harbour::Item::ARRAY);

   ZeroMemory ( &psp, sizeof(PROPSHEETPAGE) );

   strTitle      = (char *) hb_arrayGetCPtr(sArray, 1);  // Caption
   idRC          = hb_arrayGetNI( sArray, 2 );             // Id Dialog resource
   PageStyle     = hb_arrayGetNI( sArray, 3 );            // Page Style
   strHdTitle    = (char *) hb_arrayGetCPtr(sArray, 4);  // HdTitle
   strSubHdTitle = (char *) hb_arrayGetCPtr(sArray, 5);  // HdSubTitle

   psp.dwSize        = sizeof(PROPSHEETPAGE);
   psp.dwFlags       = PageStyle;
   psp.hInstance     = GetModuleHandle(nullptr);
#if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
   psp.DUMMYUNIONNAME.pszTemplate = MAKEINTRESOURCE(idRC);
   psp.DUMMYUNIONNAME2.pszIcon    = nullptr;
#else
   psp.pszTemplate   = MAKEINTRESOURCE(idRC);
   psp.pszIcon       = nullptr;
#endif
   psp.pfnDlgProc    = (DLGPROC)HMG_PageDlgProc;
   psp.pszTitle      = strTitle;
   psp.pszHeaderTitle = strHdTitle;
   psp.pszHeaderSubTitle = strSubHdTitle;

   psp.lParam        = idRC;

   hPage =  CreatePropertySheetPage(&psp);

   hmg_ret_HANDLE(hPage);
}

/****************************************************************************
 CreatePropertySheet(hWnd, ahPage, aPropSheet, modeless)
*****************************************************************************/

HB_FUNC( CREATEPROPERTYSHEET )
{
   HPROPSHEETPAGE *hpsp;
   PROPSHEETHEADER psh;
   HICON           hicon;

   PHB_ITEM sArray;
   PHB_ITEM pArray;
   char     *strPropSheet;
   int      s, idWM, nPages, idHeader, idIcon, Style;

   HWND hwnd = hmg_par_HWND(1);
   sArray = hb_param(2, Harbour::Item::ARRAY);
   pArray = hb_param(3, Harbour::Item::ARRAY);

   nPages = hb_arrayLen(sArray);

   hpsp = (HPROPSHEETPAGE *)malloc(sizeof(HPROPSHEETPAGE)* nPages );

   for( s = 0; s < nPages; s = s + 1 )
      hpsp[s] = static_cast<HPROPSHEETPAGE>(reinterpret_cast<PHB_ITEM>(hb_arrayGetNL(sArray, s + 1)));

   Style        = hb_arrayGetNI( pArray, 4 );
   idWM         = hb_arrayGetNI( pArray, 15 );
   idHeader     = hb_arrayGetNI( pArray, 17 );
   strPropSheet = (char *) hb_arrayGetCPtr(pArray, 10);  // Caption Property Sheet

   if( Style & PSP_USEHICON )
   {
      hicon = static_cast<HICON>(LoadImage(0, hb_arrayGetCPtr(pArray, 20), IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
      if( hicon == nullptr )
      {
         hicon = static_cast<HICON>(LoadImage(GetModuleHandle(nullptr), hb_arrayGetCPtr(pArray, 20), IMAGE_ICON, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }
    }
    else
      idIcon   = hb_arrayGetNI( pArray, 19 );

   //Fill out the PROPSHEETHEADER
   psh.dwSize           = sizeof(PROPSHEETHEADER);
   psh.dwFlags          = Style;
   psh.hwndParent       = hwnd;
   psh.hInstance        = GetModuleHandle(nullptr);
   #if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
     if( Style & PSP_USEHICON )
        psh.DUMMYUNIONNAME.hIcon       =  hicon;
     else
        psh.DUMMYUNIONNAME.pszIcon     =  MAKEINTRESOURCE(idIcon);
     psh.DUMMYUNIONNAME3.phpage         = hpsp;
     psh.DUMMYUNIONNAME4.pszbmWatermark = MAKEINTRESOURCE(idWM);
     psh.DUMMYUNIONNAME5.pszbmHeader    = MAKEINTRESOURCE(idHeader);
   #else
     if( Style & PSP_USEHICON )
        psh.hIcon       =  hicon;
     else
        psh.pszIcon     =  MAKEINTRESOURCE(idIcon);
     psh.phpage         = hpsp;
     psh.pszbmWatermark = MAKEINTRESOURCE(idWM);
     psh.pszbmHeader    = MAKEINTRESOURCE(idHeader);
   #endif
   psh.pszCaption       = strPropSheet;
   psh.nPages           = nPages;
   psh.pfnCallback      = reinterpret_cast<PFNPROPSHEETCALLBACK>(reinterpret_cast<void*>(HMG_PropSheetProc));

   if (hb_parl(4))
      hb_retnl( (LONG) PropertySheet(&psh) );
   else {
      if (PropertySheet(&psh) < 0)
      {
         MessageBox(nullptr, "Property Sheet could not be created", "Error",
             MB_OK | MB_ICONERROR | MB_DEFBUTTON1 | MB_APPLMODAL | MB_SETFOREGROUND);
         hb_retni( -1 );
      }

      hb_retnl(0);
   }
}

/****************************************************************************
 PropSheetIndexToHwnd( hWndPropSheet, iPageIndex )
*****************************************************************************/

HB_FUNC( PROPSHEETINDEXTOHWND )
{
  hmg_ret_HWND(PropSheet_IndexToHwnd(hmg_par_HWND(1), hb_parni(2)));
}

/****************************************************************************
 PropSheetHwndToIndex(hWndPropSheet, hWndPage)
*****************************************************************************/

HB_FUNC( PROPSHEETHWNDTOINDEX )
{
  hb_retni(PropSheet_HwndToIndex(hmg_par_HWND(1), hmg_par_HWND(2)));
}

HB_FUNC( PROPSHEETGETCURRENTPAGEHWND )
{
  hmg_ret_HWND(PropSheet_GetCurrentPageHwnd(hmg_par_HWND(1)));
}

/****************************************************************************
 PropSheetSetWizButtons(hWndPropSheet, nBtnStyle)
*****************************************************************************/

HB_FUNC( PROPSHEETSETWIZBUTTONS )
{
   HWND hwnd = hmg_par_HWND(1);
   int nBtnStyle = hb_parni(2);

   switch( nBtnStyle)
   {
      case 0:
         PropSheet_SetWizButtons(hwnd, PSWIZB_NEXT);
         break;
      case 1:
         PropSheet_SetWizButtons(hwnd, PSWIZB_BACK | PSWIZB_NEXT);
         break;
      case 2:
         PropSheet_SetWizButtons(hwnd, PSWIZB_BACK | PSWIZB_FINISH);
   }
}

/****************************************************************************
 PropSheet_Changed(hWndParent, hWndDlg)
*****************************************************************************/
HB_FUNC( PROPSHEET_CHANGED )
{
   HWND hWndParent = hmg_par_HWND(1);
   HWND hWndDlg    = hmg_par_HWND(2);

   PropSheet_Changed(hWndParent, hWndDlg);
}

/****************************************************************************
 PropSheet_UnChanged(hWndParent, hWndDlg)
*****************************************************************************/
HB_FUNC( PROPSHEET_UNCHANGED )
{
   HWND hWndParent = hmg_par_HWND(1);
   HWND hWndDlg    = hmg_par_HWND(2);

   PropSheet_UnChanged(hWndParent, hWndDlg);
}

/****************************************************************************
 DestroyPropSheet(hWndParent, hWndDlg)
*****************************************************************************/

HB_FUNC( DESTROYPROPSHEET )
{
   HWND hWnd    = hmg_par_HWND(1);
   HWND hWndDlg = hmg_par_HWND(2);

   if (SendMessage(hWndDlg, PSM_GETCURRENTPAGEHWND, 0, 0) == 0)
   {
      DestroyWindow(hWnd);
      hb_retl (TRUE);
   }
   else
   {
      SetWindowLongPtr(hWnd, DWLP_MSGRESULT, FALSE);
      hb_retl (FALSE);
   }
}

//------- Dialog functions -------------------------------

HB_FUNC( SENDDLGITEMMESSAGE )
{
   LRESULT lResult;

   lResult = SendDlgItemMessage(hmg_par_HWND(1), hb_parni(2), (UINT) hb_parnl(3),
                                 (WPARAM) hb_parni(4), (LPARAM) hb_parni(5));
   hb_retnl( lResult );
}

/****************************************************************************
 PropSheet_SetResult(hPropSheetDlg,lResult)
*****************************************************************************/
HB_FUNC( PROPSHEET_SETRESULT )
{
  SetWindowLongPtr(hmg_par_HWND(1), DWLP_MSGRESULT, (BOOL) hb_parl(2));
}

/****************************************************************************
 PropSheet_GetResult(hPropSheetDlg)
*****************************************************************************/
HB_FUNC( PROPSHEET_GETRESULT )
{
 int nRes;
  nRes = PropSheet_GetResult(hmg_par_HWND(1));
  if ( nRes > 0 )
     hb_retl (TRUE);
  else
     hb_retl (FALSE);
}

/****************************************************************************
 CreatePropSeeetPageIndirect(_HMG_aPropSheetPagesTemp, _HMG_aPropSheetTemplate, _HMG_aDialogItems)
*****************************************************************************/
HB_FUNC( CREATEPROPSEEETPAGEINDIRECT )
{
   PWORD    pdlgtemplate;

   HPROPSHEETPAGE hPage;
   PROPSHEETPAGE psp; memset(&psp, 0, sizeof(psp));
   char     *strTitle;
   char     *strHdTitle;
   char     *strSubHdTitle;
   int      PageStyle, idRC;

   PHB_ITEM sArray;
   PHB_ITEM dArray;
   PHB_ITEM cArray;

   long     lTemplateSize;

   sArray = hb_param(1, Harbour::Item::ARRAY);   //Property Sheet Array
   dArray = hb_param(2, Harbour::Item::ARRAY);   //Property Sheet Page Array
   cArray = hb_param(3, Harbour::Item::ARRAY);   //Page Controls Array

   lTemplateSize = GetSizeDlgTemp(dArray, cArray);
   pdlgtemplate = CreateDlgTemplate(lTemplateSize, dArray, cArray);

   strTitle       = (char *) hb_arrayGetCPtr(sArray, 1);  // Caption
   idRC           = hb_arrayGetNI( sArray, 2 );             // Id Dialog resource
   PageStyle      = hb_arrayGetNI( sArray, 3 );            // Page Style
   strHdTitle     = (char *) hb_arrayGetCPtr(sArray, 4);  // HdTitle
   strSubHdTitle  = (char *) hb_arrayGetCPtr(sArray, 5);  // SubHdTitle

   ZeroMemory ( &psp, sizeof(PROPSHEETPAGE) );

   psp.dwSize        = sizeof(PROPSHEETPAGE);
   psp.dwFlags       = PageStyle | PSP_DLGINDIRECT;
   psp.hInstance     = GetModuleHandle(nullptr);
#if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
   psp.DUMMYUNIONNAME.pResource = (DLGTEMPLATE*) pdlgtemplate;
   psp.DUMMYUNIONNAME2.pszIcon       = nullptr;
#else
   psp.pResource   = (DLGTEMPLATE*) pdlgtemplate;
   psp.pszIcon       = nullptr;
#endif
   psp.pfnDlgProc    = (DLGPROC) HMG_PageDlgProc;
   psp.pszTitle      = strTitle;
   psp.pszHeaderTitle = strHdTitle;
   psp.pszHeaderSubTitle = strSubHdTitle;

   psp.lParam        = idRC;

   hPage = CreatePropertySheetPage(&psp);

   hmg_ret_HANDLE(hPage);
}
