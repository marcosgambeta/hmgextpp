//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
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
#include <hbwinuni.h>

#ifndef WC_COMBOBOX
#define WC_COMBOBOX TEXT("ComboBox")
#endif

HIMAGELIST HMG_ImageListLoadFirst(const char *FileName, int cGrow, int Transparent, int *nWidth, int *nHeight);
void HMG_ImageListAdd(HIMAGELIST himl, const char *FileName, int Transparent);

/*
HMG_INITCOMBOBOXEX(p1, p2, nX, nY, nWidth, p6, p7, nHeight, p9, p10, p11, p12, p13, p14, p15) -->
HWND
*/
HB_FUNC(HMG_INITCOMBOBOXEX)
{
  INITCOMMONCONTROLSEX icex;
  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC = ICC_USEREX_CLASSES;
  InitCommonControlsEx(&icex);

  DWORD style = WS_CHILD | WS_VSCROLL;

  if (!hb_parl(9))
  {
    style |= WS_VISIBLE;
  }

  if (!hb_parl(10))
  {
    style |= WS_TABSTOP;
  }

  style |= hb_parl(12) ? CBS_DROPDOWN : CBS_DROPDOWNLIST;

  if (hb_parl(13))
  {
    style |= CBS_NOINTEGRALHEIGHT;
  }

  auto hCombo = CreateWindowEx(0, WC_COMBOBOXEX, TEXT(""), style, hmg_par_int(3), hmg_par_int(4), hmg_par_int(5),
                               hmg_par_int(8), hmg_par_HWND(1), hmg_par_HMENU(2), GetInstance(), nullptr);

  // create ImageList from aImage array

  PHB_ITEM hArray;
  HIMAGELIST himl = nullptr;

  auto nCount = static_cast<int>(hb_parinfa(14, 0));

  if (nCount > 0)
  {
    int Transparent = hb_parl(7) ? 0 : 1;
    hArray = hb_param(14, Harbour::Item::ARRAY);

    for (auto s = 1; s <= nCount; s++)
    {
      const char *FileName = hb_arrayGetCPtr(hArray, s);

      if (himl == nullptr)
      {
        himl = HMG_ImageListLoadFirst(FileName, nCount, Transparent, nullptr, nullptr);
      }
      else
      {
        HMG_ImageListAdd(himl, FileName, Transparent);
      }
    }
  }

  if (himl == nullptr && HB_PARNL(15) > 0)
  {
    himl = hmg_par_HIMAGELIST(15);
  }

  // set imagelist for created ComboEx

  if (himl != nullptr)
  {
    SendMessage(hCombo, CBEM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(himl));
  }
  else
  {
    // extend combo without images
    SendMessage(hCombo, CBEM_SETEXTENDEDSTYLE, 0, CBES_EX_NOEDITIMAGE);
  }

  hmg_ret_HWND(hCombo);
}

/*
HMG_COMBOSETITEMHEIGHT(HWND, nHeight) --> NIL
*/
HB_FUNC(HMG_COMBOSETITEMHEIGHT)
{
  SendMessage(hmg_par_HWND(1), CB_SETITEMHEIGHT, static_cast<WPARAM>(-1), hb_parni(2));
}

/*
HMG_COMBOSHOWDROPDOWN(HWND) --> NIL
*/
HB_FUNC(HMG_COMBOSHOWDROPDOWN)
{
  SendMessage(hmg_par_HWND(1), CB_SHOWDROPDOWN, 1, 0);
}

/*
HMG_COMBOEDITSETSEL(HWND, np2, np3) --> numeric
*/
HB_FUNC(HMG_COMBOEDITSETSEL)
{
  hmg_ret_LRESULT(SendMessage(hmg_par_HWND(1), CB_SETEDITSEL, 0, MAKELPARAM(hb_parni(2), hb_parni(3))));
}

/*
HMG_COMBOGETEDITSEL(HWND) --> array
*/
HB_FUNC(HMG_COMBOGETEDITSEL)
{
  auto pos = static_cast<DWORD>(SendMessage(hmg_par_HWND(1), CB_GETEDITSEL, reinterpret_cast<WPARAM>(nullptr),
                                            reinterpret_cast<LPARAM>(nullptr)));
  hb_reta(2);
  HB_STORNI(LOWORD(pos), -1, 1);
  HB_STORNI(HIWORD(pos), -1, 2);
}

/*
HMG_COMBOSELECTSTRING(HWND, cp2) --> numeric
*/
HB_FUNC(HMG_COMBOSELECTSTRING)
{
  hmg_ret_LRESULT(
      SendMessage(hmg_par_HWND(1), CB_SELECTSTRING, static_cast<WPARAM>(-1), reinterpret_cast<LPARAM>(hb_parc(2))));
}

/* Added by P.Ch. 16.10. */

/*
HMG_COMBOFINDSTRING(HWND, cString) --> numeric
*/
HB_FUNC(HMG_COMBOFINDSTRING)
{
  void *Text;
  hmg_ret_LRESULT(SendMessage(hmg_par_HWND(1), CB_FINDSTRING, static_cast<WPARAM>(-1),
                              reinterpret_cast<LPARAM>(HB_PARSTR(2, &Text, nullptr))) +
                  1);
  hb_strfree(Text);
}

/*
HMG_COMBOFINDSTRINGEXACT(HWND, cString) --> numeric
*/
HB_FUNC(HMG_COMBOFINDSTRINGEXACT)
{
  void *Text;
  hmg_ret_LRESULT(SendMessage(hmg_par_HWND(1), CB_FINDSTRINGEXACT, static_cast<WPARAM>(-1),
                              reinterpret_cast<LPARAM>(HB_PARSTR(2, &Text, nullptr))) +
                  1);
  hb_strfree(Text);
}

// Modified by P.Ch. 16.10.

/*
HMG_COMBOGETSTRING(HWND, np2) --> cString
*/
HB_FUNC(HMG_COMBOGETSTRING)
{
  auto strlen = static_cast<int>(SendMessage(hmg_par_HWND(1), CB_GETLBTEXTLEN, hmg_par_WPARAM(2) - 1, 0));

  if (strlen > 0)
  {
    TCHAR *str = new TCHAR[strlen + 1];
    SendMessage(hmg_par_HWND(1), CB_GETLBTEXT, hmg_par_WPARAM(2) - 1, reinterpret_cast<LPARAM>(str));
    HB_RETSTR(str);
    delete[] str;
  }
  else
  {
    hb_retc_null();
  }
}

/*
HMG_COMBOADDSTRING(HWND, cString) --> NIL
*/
HB_FUNC(HMG_COMBOADDSTRING)
{
  void *String;
  SendMessage(hmg_par_HWND(1), CB_ADDSTRING, 0, reinterpret_cast<LPARAM>(HB_PARSTR(2, &String, nullptr)));
  hb_strfree(String);
}

/*
HMG_COMBOINSERTSTRING(HWND, cString, np3) --> NIL
*/
HB_FUNC(HMG_COMBOINSERTSTRING)
{
  void *String;
  SendMessage(hmg_par_HWND(1), CB_INSERTSTRING, hmg_par_WPARAM(3) - 1,
              reinterpret_cast<LPARAM>(HB_PARSTR(2, &String, nullptr)));
  hb_strfree(String);
}

// extend combo functions  (JK)  HMG 1.0 Exp. Build 8

/*
HMG_COMBOADDSTRINGEX(HWND, cString, np3) --> NIL
*/
HB_FUNC(HMG_COMBOADDSTRINGEX)
{
  void *Text;
  auto nImage = hb_parni(3);
  COMBOBOXEXITEM cbei;
  cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
  cbei.iItem = -1;
  cbei.pszText = const_cast<TCHAR *>(HB_PARSTR(2, &Text, nullptr));
  cbei.cchTextMax = static_cast<int>(hb_parclen(2));
  cbei.iImage = (nImage - 1) * 3;
  cbei.iSelectedImage = (nImage - 1) * 3 + 1;
  cbei.iOverlay = (nImage - 1) * 3 + 2;
  cbei.iIndent = 0;
  SendMessage(hmg_par_HWND(1), CBEM_INSERTITEM, 0, reinterpret_cast<LPARAM>(&cbei));
  hb_strfree(Text);
}

/*
HMG_COMBOINSERTSTRINGEX(HWND, cString, np3, np4) --> NIL
*/
HB_FUNC(HMG_COMBOINSERTSTRINGEX)
{
  void *Text;
  auto nImage = hb_parni(3);
  COMBOBOXEXITEM cbei;
  cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
  cbei.iItem = hb_parni(4) - 1;
  cbei.pszText = const_cast<TCHAR *>(HB_PARSTR(2, &Text, nullptr));
  cbei.cchTextMax = static_cast<int>(hb_parclen(2));
  cbei.iImage = (nImage - 1) * 3;
  cbei.iSelectedImage = (nImage - 1) * 3 + 1;
  cbei.iOverlay = (nImage - 1) * 3 + 2;
  cbei.iIndent = 0;
  SendMessage(hmg_par_HWND(1), CBEM_INSERTITEM, 0, reinterpret_cast<LPARAM>(&cbei));
  hb_strfree(Text);
}

/*
HMG_COMBOADDDATASTRINGEX(HWND, cString) --> NIL
*/
HB_FUNC(HMG_COMBOADDDATASTRINGEX)
{
  void *Text;
  COMBOBOXEXITEM cbei;
  cbei.mask = CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_OVERLAY;
  cbei.iItem = -1;
  cbei.pszText = const_cast<TCHAR *>(HB_PARSTR(2, &Text, nullptr));
  cbei.cchTextMax = static_cast<int>(hb_parclen(2));
  cbei.iImage = 0;
  cbei.iSelectedImage = 1;
  cbei.iOverlay = 2;
  cbei.iIndent = 0;
  SendMessage(hmg_par_HWND(1), CBEM_INSERTITEM, 0, reinterpret_cast<LPARAM>(&cbei));
  hb_strfree(Text);
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(INITCOMBOBOXEX, HMG_INITCOMBOBOXEX)
HB_FUNC_TRANSLATE(COMBOSETITEMHEIGHT, HMG_COMBOSETITEMHEIGHT)
HB_FUNC_TRANSLATE(COMBOSHOWDROPDOWN, HMG_COMBOSHOWDROPDOWN)
HB_FUNC_TRANSLATE(COMBOEDITSETSEL, HMG_COMBOEDITSETSEL)
HB_FUNC_TRANSLATE(COMBOGETEDITSEL, HMG_COMBOGETEDITSEL)
HB_FUNC_TRANSLATE(COMBOSELECTSTRING, HMG_COMBOSELECTSTRING)
HB_FUNC_TRANSLATE(COMBOFINDSTRING, HMG_COMBOFINDSTRING)
HB_FUNC_TRANSLATE(COMBOFINDSTRINGEXACT, HMG_COMBOFINDSTRINGEXACT)
HB_FUNC_TRANSLATE(COMBOGETSTRING, HMG_COMBOGETSTRING)
HB_FUNC_TRANSLATE(COMBOADDSTRING, HMG_COMBOADDSTRING)
HB_FUNC_TRANSLATE(COMBOINSERTSTRING, HMG_COMBOINSERTSTRING)
HB_FUNC_TRANSLATE(COMBOADDSTRINGEX, HMG_COMBOADDSTRINGEX)
HB_FUNC_TRANSLATE(COMBOINSERTSTRINGEX, HMG_COMBOINSERTSTRINGEX)
HB_FUNC_TRANSLATE(COMBOADDDATASTRINGEX, HMG_COMBOADDDATASTRINGEX)
#endif
