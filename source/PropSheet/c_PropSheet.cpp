//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// Property Sheet control source code
// (C)2008 Janusz Pora <januszpora@onet.eu>
//

// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this software; see the file COPYING. If not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
// visit the web site http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of Harbour Minigui.
//
// The exception is that, if you link the Harbour Minigui library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// Harbour-Minigui library code into it.

// Parts of this project are based upon:
//
// "Harbour GUI framework for Win32"
// Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2022, https://harbour.github.io/
//
// "WHAT32"
// Copyright 2002 AJ Wos <andrwos@aust1.net>
//
// "HWGUI"
// Copyright 2001-2009 Alexander S.Kresin <alex@belacy.belgorod.su>

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

extern PWORD CreateDlgTemplate(long lTemplateSize, PHB_ITEM dArray, PHB_ITEM cArray);
extern long GetSizeDlgTemp(PHB_ITEM dArray, PHB_ITEM cArray);

/****************************************************************************/
LRESULT CALLBACK HMG_PageDlgProc(HWND hWndDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;
  static PHB_SYMB pSymbol2 = nullptr;
  static PHB_SYMB pSymbol3 = nullptr;
  static PROPSHEETPAGE *ps = nullptr;

  HWND hWndParent = GetParent(hWndDlg);

  switch (message)
  {
    // wNotifyCode = HIWORD(wParam); // notification code
    // wID = LOWORD(wParam);         // item, control, or accelerator identifier
    // hwndCtl = (HWND) lParam;      // handle of control

  case WM_INITDIALOG: {
    ps = reinterpret_cast<PROPSHEETPAGE *>(lParam);
    if (!pSymbol2)
    {
      pSymbol2 = hb_dynsymSymbol(hb_dynsymGet("INITPAGEDLGPROC"));
    }
    if (pSymbol2)
    {
      hb_vmPushSymbol(pSymbol2);
      hb_vmPushNil();
      hmg_vmPushHWND(hWndDlg);
      hmg_vmPushWPARAM(ps->lParam);
      hmg_vmPushHWND(hWndParent);
      hb_vmDo(3);
    }
    return TRUE;
  }
  case WM_DESTROY:
    break;
  case WM_COMMAND:
    break;
  case WM_NOTIFY: {
    LPNMHDR lpnmhdr = reinterpret_cast<NMHDR FAR *>(lParam);
    auto psn = reinterpret_cast<PSHNOTIFY *>(lParam);

    int nPage = PropSheet_HwndToIndex(hWndParent, hWndDlg);
    int nId = static_cast<int>(PropSheet_IndexToId(hWndParent, nPage));

    if (!pSymbol3)
    {
      pSymbol3 = hb_dynsymSymbol(hb_dynsymGet("BUTTONPAGEDLGPROC"));
    }
    if (pSymbol3)
    {
      hb_vmPushSymbol(pSymbol3);
      hb_vmPushNil();
      hmg_vmPushHWND(hWndDlg);
      hb_vmPushLong((LONG)lpnmhdr->code);
      hb_vmPushLong(nId);
      hb_vmPushLong(nPage);
      hb_vmDo(4);
    }

    long int r = hb_parnl(-1);

    switch (psn->hdr.code)
    {
    case PSN_APPLY: // sent when OK or Apply button pressed
    {
      if (psn->lParam == FALSE)
      { // Apply pressed
        if (r)
        {
          SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_NOERROR);
        }
        else
        {
          SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_INVALID_NOCHANGEPAGE);
        }
      }
      break;
    }
    case PSN_RESET: // sent when Cancel button pressed
    {
      if (r)
      { // Not finished yet.
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
      }
      else
      {
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, PSNRET_INVALID_NOCHANGEPAGE);
      }
      break;
    }
    case PSN_QUERYCANCEL: // sent when Quit button pressed
    {
      if (r)
      { // Not finished yet.
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
      }
      else
      {
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, TRUE);
        return (TRUE);
      }
      break;
    }
    case PSN_KILLACTIVE: {
      if (r)
      {
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, FALSE);
      }
      else
      { // Not valid.
        SetWindowLongPtr(hWndDlg, DWLP_MSGRESULT, TRUE);
      }
      break;
    }
    case PSN_SETACTIVE:
      // this will be ignored if the property sheet is not a wizard
      break;
    default:
      break;
    }
    break;
  }

  default:
    break;
  }

  if (!pSymbol)
  {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("PAGEDLGPROC"));
  }

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hWndParent);
    hmg_vmPushHWND(hWndDlg);
    hmg_vmPushUINT(message);
    hmg_vmPushWPARAM(wParam);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(5);
  }

  return FALSE;
}

/****************************************************************************/
LRESULT CALLBACK HMG_PropSheetProc(HWND hwndPropSheet, UINT message, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  switch (message)
  {
    // called before the dialog is created,
    // hwndPropSheet = nullptr, lParam points
    // lpTemplate = {style, dwExtendStyle, cdit, x, y, cx, cy }  //ToDo

  case PSCB_PRECREATE: {
    auto lpTemplate = reinterpret_cast<LPDLGTEMPLATE>(lParam);

    if (!(lpTemplate->style & WS_SYSMENU))
    {
      lpTemplate->style |= WS_SYSMENU;
    }
  }
  }
  if (!pSymbol)
  {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("PROPSHEETPROC"));
  }

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hwndPropSheet);
    hmg_vmPushUINT(message);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(3);
  }

  return FALSE;
}

/****************************************************************************
 CreatePropertySeeetPage(_HMG_aPropSheetPagesTemp)
*****************************************************************************/

HB_FUNC(CREATEPROPERTYSEEETPAGE)
{
  auto sArray = hb_param(1, Harbour::Item::ARRAY);

  auto strTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 1));      // Caption
  auto idRC = hb_arrayGetNI(sArray, 2);                                // Id Dialog resource
  auto PageStyle = hb_arrayGetNI(sArray, 3);                           // Page Style
  auto strHdTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 4));    // HdTitle
  auto strSubHdTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 5)); // HdSubTitle

  PROPSHEETPAGE psp{};
  psp.dwSize = sizeof(PROPSHEETPAGE);
  psp.dwFlags = PageStyle;
  psp.hInstance = GetModuleHandle(nullptr);
  psp.pszTemplate = MAKEINTRESOURCE(idRC);
  psp.pszIcon = nullptr;
  psp.pfnDlgProc = reinterpret_cast<DLGPROC>(HMG_PageDlgProc);
  psp.pszTitle = strTitle;
  psp.pszHeaderTitle = strHdTitle;
  psp.pszHeaderSubTitle = strSubHdTitle;
  psp.lParam = idRC;

  HPROPSHEETPAGE hPage = CreatePropertySheetPage(&psp);

  hmg_ret_HANDLE(hPage);
}

/****************************************************************************
 CreatePropertySheet(hWnd, ahPage, aPropSheet, modeless)
*****************************************************************************/

HB_FUNC(CREATEPROPERTYSHEET)
{
  auto hwnd = hmg_par_HWND(1);
  auto sArray = hb_param(2, Harbour::Item::ARRAY);
  auto pArray = hb_param(3, Harbour::Item::ARRAY);

  int nPages = static_cast<int>(hb_arrayLen(sArray));
  auto hpsp = static_cast<HPROPSHEETPAGE *>(malloc(sizeof(HPROPSHEETPAGE) * nPages));
  for (auto s = 0; s < nPages; s = s + 1)
  {
    hpsp[s] = static_cast<HPROPSHEETPAGE>(reinterpret_cast<PHB_ITEM>(static_cast<LONG_PTR>(hb_arrayGetNL(sArray, s + 1))));
  }

  auto Style = hb_arrayGetNI(pArray, 4);
  auto idWM = hb_arrayGetNI(pArray, 15);
  auto idHeader = hb_arrayGetNI(pArray, 17);
  auto strPropSheet = const_cast<char *>(hb_arrayGetCPtr(pArray, 10)); // Caption Property Sheet

  HICON hicon = nullptr;
  int idIcon = 0;

  if (Style & PSP_USEHICON)
  {
    hicon = static_cast<HICON>(
        LoadImage(0, hb_arrayGetCPtr(pArray, 20), IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
    if (hicon == nullptr)
    {
      hicon = static_cast<HICON>(LoadImage(GetModuleHandle(nullptr), hb_arrayGetCPtr(pArray, 20), IMAGE_ICON, 0, 0,
                                           LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
    }
  }
  else
  {
    idIcon = hb_arrayGetNI(pArray, 19);
  }

  // Fill out the PROPSHEETHEADER
  PROPSHEETHEADER psh{};
  psh.dwSize = sizeof(PROPSHEETHEADER);
  psh.dwFlags = Style;
  psh.hwndParent = hwnd;
  psh.hInstance = GetModuleHandle(nullptr);
  if (Style & PSP_USEHICON)
  {
    psh.hIcon = hicon;
  }
  else
  {
    psh.pszIcon = MAKEINTRESOURCE(idIcon);
  }
  psh.phpage = hpsp;
  psh.pszbmWatermark = MAKEINTRESOURCE(idWM);
  psh.pszbmHeader = MAKEINTRESOURCE(idHeader);
  psh.pszCaption = strPropSheet;
  psh.nPages = nPages;
  psh.pfnCallback = reinterpret_cast<PFNPROPSHEETCALLBACK>(reinterpret_cast<void *>(HMG_PropSheetProc));

  if (hb_parl(4))
  {
    hb_retnl(static_cast<long>(PropertySheet(&psh)));
  }
  else
  {
    if (PropertySheet(&psh) < 0)
    {
      MessageBox(nullptr, "Property Sheet could not be created", "Error",
                 MB_OK | MB_ICONERROR | MB_DEFBUTTON1 | MB_APPLMODAL | MB_SETFOREGROUND);
      hb_retni(-1);
    }

    hb_retnl(0);
  }
}

/****************************************************************************
 PropSheetIndexToHwnd(hWndPropSheet, iPageIndex)
*****************************************************************************/

HB_FUNC(PROPSHEETINDEXTOHWND)
{
  hmg_ret_HWND(PropSheet_IndexToHwnd(hmg_par_HWND(1), hb_parni(2)));
}

/****************************************************************************
 PropSheetHwndToIndex(hWndPropSheet, hWndPage)
*****************************************************************************/

HB_FUNC(PROPSHEETHWNDTOINDEX)
{
  hb_retni(PropSheet_HwndToIndex(hmg_par_HWND(1), hmg_par_HWND(2)));
}

HB_FUNC(PROPSHEETGETCURRENTPAGEHWND)
{
  hmg_ret_HWND(PropSheet_GetCurrentPageHwnd(hmg_par_HWND(1)));
}

/****************************************************************************
 PropSheetSetWizButtons(hWndPropSheet, nBtnStyle)
*****************************************************************************/

HB_FUNC(PROPSHEETSETWIZBUTTONS)
{
  switch (hb_parni(2))
  {
  case 0:
    PropSheet_SetWizButtons(hmg_par_HWND(1), PSWIZB_NEXT);
    break;
  case 1:
    PropSheet_SetWizButtons(hmg_par_HWND(1), PSWIZB_BACK | PSWIZB_NEXT);
    break;
  case 2:
    PropSheet_SetWizButtons(hmg_par_HWND(1), PSWIZB_BACK | PSWIZB_FINISH);
  }
}

/****************************************************************************
 PropSheet_Changed(hWndParent, hWndDlg)
*****************************************************************************/
HB_FUNC(PROPSHEET_CHANGED)
{
  PropSheet_Changed(hmg_par_HWND(1), hmg_par_HWND(2));
}

/****************************************************************************
 PropSheet_UnChanged(hWndParent, hWndDlg)
*****************************************************************************/
HB_FUNC(PROPSHEET_UNCHANGED)
{
  PropSheet_UnChanged(hmg_par_HWND(1), hmg_par_HWND(2));
}

/****************************************************************************
 DestroyPropSheet(hWndParent, hWndDlg)
*****************************************************************************/

HB_FUNC(DESTROYPROPSHEET)
{
  if (SendMessage(hmg_par_HWND(2), PSM_GETCURRENTPAGEHWND, 0, 0) == 0)
  {
    DestroyWindow(hmg_par_HWND(1));
    hb_retl(true);
  }
  else
  {
    SetWindowLongPtr(hmg_par_HWND(1), DWLP_MSGRESULT, FALSE);
    hb_retl(false);
  }
}

//------- Dialog functions -------------------------------

HB_FUNC(SENDDLGITEMMESSAGE)
{
  hmg_ret_LRESULT(
      SendDlgItemMessage(hmg_par_HWND(1), hmg_par_int(2), hmg_par_UINT(3), hmg_par_WPARAM(4), hmg_par_LPARAM(5)));
}

/****************************************************************************
 PropSheet_SetResult(hPropSheetDlg,lResult)
*****************************************************************************/
HB_FUNC(PROPSHEET_SETRESULT)
{
  SetWindowLongPtr(hmg_par_HWND(1), DWLP_MSGRESULT, (BOOL)hb_parl(2));
}

/****************************************************************************
 PropSheet_GetResult(hPropSheetDlg)
*****************************************************************************/
HB_FUNC(PROPSHEET_GETRESULT)
{
  if (PropSheet_GetResult(hmg_par_HWND(1)) > 0)
  {
    hb_retl(true);
  }
  else
  {
    hb_retl(false);
  }
}

/****************************************************************************
 CreatePropSeeetPageIndirect(_HMG_aPropSheetPagesTemp, _HMG_aPropSheetTemplate, _HMG_aDialogItems)
*****************************************************************************/
HB_FUNC(CREATEPROPSEEETPAGEINDIRECT)
{
  auto sArray = hb_param(1, Harbour::Item::ARRAY); // Property Sheet Array
  auto dArray = hb_param(2, Harbour::Item::ARRAY); // Property Sheet Page Array
  auto cArray = hb_param(3, Harbour::Item::ARRAY); // Page Controls Array

  long lTemplateSize = GetSizeDlgTemp(dArray, cArray);
  PWORD pdlgtemplate = CreateDlgTemplate(lTemplateSize, dArray, cArray);

  auto strTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 1));      // Caption
  auto idRC = hb_arrayGetNI(sArray, 2);                                // Id Dialog resource
  auto PageStyle = hb_arrayGetNI(sArray, 3);                           // Page Style
  auto strHdTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 4));    // HdTitle
  auto strSubHdTitle = const_cast<char *>(hb_arrayGetCPtr(sArray, 5)); // SubHdTitle

  PROPSHEETPAGE psp{};
  psp.dwSize = sizeof(PROPSHEETPAGE);
  psp.dwFlags = PageStyle | PSP_DLGINDIRECT;
  psp.hInstance = GetModuleHandle(nullptr);
  psp.pResource = reinterpret_cast<DLGTEMPLATE *>(pdlgtemplate);
  psp.pszIcon = nullptr;
  psp.pfnDlgProc = reinterpret_cast<DLGPROC>(HMG_PageDlgProc);
  psp.pszTitle = strTitle;
  psp.pszHeaderTitle = strHdTitle;
  psp.pszHeaderSubTitle = strSubHdTitle;
  psp.lParam = idRC;

  HPROPSHEETPAGE hPage = CreatePropertySheetPage(&psp);

  hmg_ret_HANDLE(hPage);
}
