//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// DIALOG form source code
// (c)2005-2008 Janusz Pora <januszpora@onet.eu>
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
// Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
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
// Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

#define _WIN32_IE 0x0501

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

LRESULT CALLBACK HMG_DlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  if (pSymbol == nullptr) {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("DIALOGPROC"));
  }

  if (pSymbol != nullptr) {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hWnd);
    hmg_vmPushUINT(message);
    hmg_vmPushWPARAM(wParam);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(4);
  }

  return hb_parnl(-1);
}

LRESULT CALLBACK HMG_ModalDlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;

  if (pSymbol == nullptr) {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("MODALDIALOGPROC"));
  }

  if (pSymbol != nullptr) {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hWnd);
    hmg_vmPushUINT(message);
    hmg_vmPushWPARAM(wParam);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(4);
  }

  return hb_parnl(-1);
}

// HMG_INITMODALDIALOG(HWND, np2) --> LRESULT
HB_FUNC(HMG_INITMODALDIALOG)
{
  HB_RETNL(static_cast<LONG_PTR>(DialogBox(GetResources(), MAKEINTRESOURCE(hb_parni(2)), hmg_par_HWND(1),
                                           reinterpret_cast<DLGPROC>(HMG_ModalDlgProc))));
}

// HMG_INITDIALOG(HWND, np2) --> HANDLE
HB_FUNC(HMG_INITDIALOG)
{
  hmg_ret_HWND(CreateDialog(GetResources(), MAKEINTRESOURCE(hb_parni(2)), hmg_par_HWND(1),
                            reinterpret_cast<DLGPROC>(HMG_DlgProc)));
}

// HMG_GETDIALOGITEMHANDLE(HWND, nID) --> HANDLE
HB_FUNC(HMG_GETDIALOGITEMHANDLE)
{
  hmg_ret_HWND(GetDlgItem(hmg_par_HWND(1), hmg_par_int(2)));
}

// HMG_CHECKDLGBUTTON(nIDButton, HWND) --> NIL // TODO: return .T. or .F.
HB_FUNC(HMG_CHECKDLGBUTTON)
{
  CheckDlgButton(hmg_par_HWND(2), hmg_par_int(1), BST_CHECKED);
}

// HMG_UNCHECKDLGBUTTON(nIDButton, HWND) --> NIL // TODO: return .T. or .F.
HB_FUNC(HMG_UNCHECKDLGBUTTON)
{
  CheckDlgButton(hmg_par_HWND(2), hmg_par_int(1), BST_UNCHECKED);
}

// HMG_SETDIALOGITEMTEXT(HWND, nId, cText) --> .T.|.F.
HB_FUNC(HMG_SETDIALOGITEMTEXT)
{
  void *str;
  hb_retl(SetDlgItemText(hmg_par_HWND(1), hmg_par_int(2), HB_PARSTR(3, &str, nullptr)));
  hb_strfree(str);
}

// HMG_ENDDIALOG(HWND, nResult) --> NIL
HB_FUNC(HMG_ENDDIALOG)
{
  EndDialog(hmg_par_HWND(1), hb_parni(2));
}

// HMG_ADDDIALOGPAGES(HWND, ap2, np3) --> NIL
HB_FUNC(HMG_ADDDIALOGPAGES)
{
  auto hwnd = hmg_par_HWND(1);
  auto l = static_cast<int>(hb_parinfa(2, 0)) - 1;
  auto hArray = hb_param(2, Harbour::Item::ARRAY);

  TC_ITEM tie;
  tie.mask = TCIF_TEXT;
  tie.iImage = -1;

  for (int i = l; i >= 0; i = i - 1) {
    tie.pszText = const_cast<TCHAR *>(hb_arrayGetCPtr(hArray, i + 1));
    TabCtrl_InsertItem(hwnd, 0, &tie);
  }

  TabCtrl_SetCurSel(hwnd, hb_parni(3) - 1);
  TabCtrl_SetCurFocus(hwnd, hb_parni(3) - 1);
}

// HMG_GETDLGCTRLID(HWND) --> numeric
HB_FUNC(HMG_GETDLGCTRLID)
{
  hb_retni(GetDlgCtrlID(hmg_par_HWND(1)));
}

// HMG_SETDLGITEMINT(HWND, nId, nValue, lSigned) --> NIL // TODO: return .T. or .F.
HB_FUNC(HMG_SETDLGITEMINT)
{
  SetDlgItemInt(hmg_par_HWND(1), hmg_par_int(2), hmg_par_UINT(3),
                (hb_pcount() < 4 || HB_ISNIL(4) || !hb_parl(4)) ? false : true);
}

// HMG_GETDLGITEMTEXT(HWND, nId, np3) --> cText
HB_FUNC(HMG_GETDLGITEMTEXT)
{
  auto strlen = hb_parni(3);
  TCHAR *str = new TCHAR[strlen + 1];
  GetDlgItemText(hmg_par_HWND(1), hmg_par_int(2), str, strlen);
  HB_RETSTR(str);
  delete[] str;
}

// HMG_GETEDITTEXT(HWND, nId) --> cText
HB_FUNC(HMG_GETEDITTEXT)
{
  auto hDlg = hmg_par_HWND(1);
  auto id = hmg_par_int(2);
  auto strlen = static_cast<USHORT>(SendMessage(GetDlgItem(hDlg, id), WM_GETTEXTLENGTH, 0, 0));
  auto str = new TCHAR[strlen + 2];
  GetDlgItemText(hDlg, id, str, strlen + 1);
  HB_RETSTR(str);
  delete[] str;
}

// HMG_CHECKRADIOBUTTON(HWND, nIDFirstButton, nIDLastButton, nIDCheckButton) --> NIL // TODO: return .T. or .F.
HB_FUNC(HMG_CHECKRADIOBUTTON)
{
  CheckRadioButton(hmg_par_HWND(1), hmg_par_int(2), hmg_par_int(3), hmg_par_int(4));
}

// HMG_ISDLGBUTTONCHECKED(HWND, nIDButton) --> .T.|.F.
HB_FUNC(HMG_ISDLGBUTTONCHECKED)
{
  hb_retl(IsDlgButtonChecked(hmg_par_HWND(1), hmg_par_int(2)) == BST_CHECKED ? true : false);
}

static LPWORD lpwAlign(LPWORD lpIn)
{
  auto ul = reinterpret_cast<ULONG_PTR>(lpIn);
  ul += 3;
  ul >>= 2;
  ul <<= 2;
  return reinterpret_cast<LPWORD>(ul);
}

static int nCopyAnsiToWideChar(LPWORD lpWCStr, LPCSTR lpAnsiIn)
{
  int CodePage = GetACP();
  int nDstLen = MultiByteToWideChar(CodePage, 0, lpAnsiIn, -1, nullptr, 0);

  if (nDstLen > 0) {
    auto pszDst = static_cast<LPWSTR>(hb_xgrab(nDstLen * 2));

    MultiByteToWideChar(CodePage, 0, lpAnsiIn, -1, pszDst, nDstLen);

    for (auto i = 0; i < nDstLen; i++) {
      *(lpWCStr + i) = *(pszDst + i);
    }

    hb_xfree(pszDst);
  }

  return nDstLen;
}

HB_SIZE GetSizeDlgTemp(PHB_ITEM dArray, PHB_ITEM cArray)
{
  HB_SIZE lTemplateSize = 36;
  HB_SIZE ln = hb_arrayGetCLen(dArray, 10); // caption
  lTemplateSize += ln * 2;

  if (hb_arrayGetNI(dArray, 4) & DS_SETFONT) {
    ln = hb_arrayGetCLen(dArray, 11); // fontname
    lTemplateSize += ln * 2;
    lTemplateSize += 3;
  }

  PHB_ITEM iArray;
  auto nItem = static_cast<int>(hb_arrayLen(cArray));

  for (auto s = 0; s < nItem; s++) {
    iArray = static_cast<PHB_ITEM>(hb_arrayGetItemPtr(cArray, s + 1));
    lTemplateSize += 36;
    ln = hb_arrayGetCLen(iArray, 3); // class
    lTemplateSize += ln * 2;
    ln = hb_arrayGetCLen(iArray, 10); // caption
    lTemplateSize += ln * 2;
  }

  return lTemplateSize;
}

PWORD CreateDlgTemplate(HB_SIZE lTemplateSize, PHB_ITEM dArray, PHB_ITEM cArray)
{
  LONG baseUnit = GetDialogBaseUnits();
  int baseunitX = LOWORD(baseUnit);
  int baseunitY = HIWORD(baseUnit);
  WORD iPointSize;
  PHB_ITEM iArray;
#ifndef _WIN64
  ULONG style, ExStyle, HelpId;
#else
  HB_LONGLONG style, ExStyle, HelpId;
#endif
  ULONG Id;
  int nchar;

  auto pdlgtemplate = static_cast<PWORD>(LocalAlloc(LPTR, lTemplateSize));

  PWORD pw = pdlgtemplate;

  ExStyle = HB_arrayGetNL(dArray, 5); // ExStyle
  style = HB_arrayGetNL(dArray, 4);   // style
  auto x = hb_arrayGetNI(dArray, 6);  // x
  auto y = hb_arrayGetNI(dArray, 7);  // y
  auto w = hb_arrayGetNI(dArray, 8);  // w
  auto h = hb_arrayGetNI(dArray, 9);  // h
  auto nItem = static_cast<int>(hb_arrayLen(cArray));

  *pw++ = 1;      // DlgVer
  *pw++ = 0xFFFF; // Signature
  *pw++ = 0;      // LOWORD HelpID
  *pw++ = 0;      // HIWORD HelpID
  *pw++ = LOWORD(ExStyle);
  *pw++ = HIWORD(ExStyle);
  *pw++ = LOWORD(style);
  *pw++ = HIWORD(style);
  *pw++ = static_cast<WORD>(nItem);                               // NumberOfItems
  *pw++ = static_cast<WORD>(MulDiv(x, 4, baseunitX));             // x
  *pw++ = static_cast<WORD>(MulDiv(y, 8, baseunitY));             // y
  *pw++ = static_cast<WORD>(MulDiv(w, 4, baseunitX));             // cx
  *pw++ = static_cast<WORD>(MulDiv(h, 8, baseunitY));             // cy
  *pw++ = 0;                                                      // Menu
  *pw++ = 0;                                                      // Class
  auto strtemp = const_cast<char *>(hb_arrayGetCPtr(dArray, 10)); // caption
  nchar = nCopyAnsiToWideChar(pw, strtemp);
  pw += nchar;
  if (hb_arrayGetNI(dArray, 4) & DS_SETFONT) {
    iPointSize = static_cast<WORD>(hb_arrayGetNI(dArray, 12)); // fontsize
    *pw++ = iPointSize;
    *pw++ = static_cast<WORD>(hb_arrayGetL(dArray, 13) ? 700 : 400); // bold
    *pw++ = static_cast<WORD>(hb_arrayGetL(dArray, 14));
    strtemp = const_cast<char *>(hb_arrayGetCPtr(dArray, 11)); // font
    nchar = nCopyAnsiToWideChar(pw, strtemp);
    pw += nchar;
  }

  for (auto s = 0; s < nItem; s = s + 1) {
    iArray = static_cast<PHB_ITEM>(hb_arrayGetItemPtr(cArray, s + 1));
    pw = lpwAlign(pw);

    HelpId = HB_arrayGetNL(iArray, 11); // HelpId
    ExStyle = HB_arrayGetNL(iArray, 5); // exstyle
    style = HB_arrayGetNL(iArray, 4);   // style  item
    Id = hb_arrayGetNI(iArray, 1);

    *pw++ = LOWORD(HelpId);
    *pw++ = HIWORD(HelpId);
    *pw++ = LOWORD(ExStyle);
    *pw++ = HIWORD(ExStyle);
    *pw++ = LOWORD(style);
    *pw++ = HIWORD(style);
    *pw++ = static_cast<WORD>(MulDiv(hb_arrayGetNI(iArray, 6), 4, baseunitX)); // x
    *pw++ = static_cast<WORD>(MulDiv(hb_arrayGetNI(iArray, 7), 8, baseunitY)); // y
    *pw++ = static_cast<WORD>(MulDiv(hb_arrayGetNI(iArray, 8), 4, baseunitX)); // cx
    *pw++ = static_cast<WORD>(MulDiv(hb_arrayGetNI(iArray, 9), 8, baseunitY)); // cy
    *pw++ = static_cast<WORD>(Id);                                             // LOWORD(Control ID)
    *pw++ = 0;                                                                 // HOWORD(Control ID)

    strtemp = const_cast<char *>(hb_arrayGetCPtr(iArray, 3)); // class
    nchar = nCopyAnsiToWideChar(pw, strtemp);
    pw += nchar;

    strtemp = const_cast<char *>(hb_arrayGetCPtr(iArray, 10)); // caption
    nchar = nCopyAnsiToWideChar(pw, strtemp);
    pw += nchar;
    *pw++ = 0; // Advance pointer over nExtraStuff WORD.
  }

  *pw = 0; // Number of bytes of extra data.

  return pdlgtemplate;
}

// HMG_CREATEDLGTEMPLATE(HWND, ap2, ap3) --> LRESULT|HANDLE
HB_FUNC(HMG_CREATEDLGTEMPLATE)
{
  auto dArray = hb_param(2, Harbour::Item::ARRAY);
  auto cArray = hb_param(3, Harbour::Item::ARRAY);
  BOOL modal = hb_arrayGetL(dArray, 3);
  HB_SIZE lTemplateSize = GetSizeDlgTemp(dArray, cArray);
  PWORD pdlgtemplate = CreateDlgTemplate(lTemplateSize, dArray, cArray);

  if (modal) {
    LRESULT lResult = DialogBoxIndirect(GetResources(), reinterpret_cast<LPDLGTEMPLATE>(pdlgtemplate), hmg_par_HWND(1),
                                        reinterpret_cast<DLGPROC>(HMG_ModalDlgProc));
    LocalFree(pdlgtemplate);
    HB_RETNL(static_cast<LONG_PTR>(lResult));
  } else {
    HWND hwndDlg = CreateDialogIndirect(GetResources(), reinterpret_cast<LPDLGTEMPLATE>(pdlgtemplate), hmg_par_HWND(1),
                                        reinterpret_cast<DLGPROC>(HMG_DlgProc));
    LocalFree(pdlgtemplate);
    hmg_ret_HWND(hwndDlg);
  }
}

// HMG_INITEXCOMMONCONTROLS(np) --> NIL
HB_FUNC(HMG_INITEXCOMMONCONTROLS)
{
  INITCOMMONCONTROLSEX i{};
  i.dwSize = sizeof(INITCOMMONCONTROLSEX);

  switch (hb_parni(1))
  {
  case 1:
    i.dwICC = ICC_DATE_CLASSES;
    break;
  case 2:
    i.dwICC = ICC_TREEVIEW_CLASSES;
    break;
  case 3:
    i.dwICC = ICC_INTERNET_CLASSES;
    break;
  default:
    i.dwICC = ICC_DATE_CLASSES;
  }

  InitCommonControlsEx(&i);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITMODALDIALOG, HMG_INITMODALDIALOG)
HB_FUNC_TRANSLATE(INITDIALOG, HMG_INITDIALOG)
HB_FUNC_TRANSLATE(GETDIALOGITEMHANDLE, HMG_GETDIALOGITEMHANDLE)
HB_FUNC_TRANSLATE(CHECKDLGBUTTON, HMG_CHECKDLGBUTTON)
HB_FUNC_TRANSLATE(UNCHECKDLGBUTTON, HMG_UNCHECKDLGBUTTON)
HB_FUNC_TRANSLATE(SETDIALOGITEMTEXT, HMG_SETDIALOGITEMTEXT)
HB_FUNC_TRANSLATE(ENDDIALOG, HMG_ENDDIALOG)
HB_FUNC_TRANSLATE(ADDDIALOGPAGES, HMG_ADDDIALOGPAGES)
HB_FUNC_TRANSLATE(GETDLGCTRLID, HMG_GETDLGCTRLID)
HB_FUNC_TRANSLATE(SETDLGITEMINT, HMG_SETDLGITEMINT)
HB_FUNC_TRANSLATE(GETDLGITEMTEXT, HMG_GETDLGITEMTEXT)
HB_FUNC_TRANSLATE(GETEDITTEXT, HMG_GETEDITTEXT)
HB_FUNC_TRANSLATE(CHECKRADIOBUTTON, HMG_CHECKRADIOBUTTON)
HB_FUNC_TRANSLATE(ISDLGBUTTONCHECKED, HMG_ISDLGBUTTONCHECKED)
HB_FUNC_TRANSLATE(CREATEDLGTEMPLATE, HMG_CREATEDLGTEMPLATE)
HB_FUNC_TRANSLATE(INITEXCOMMONCONTROLS, HMG_INITEXCOMMONCONTROLS)
#endif
