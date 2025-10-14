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
#include <hbwinuni.hpp>

void pascal DelResource(HANDLE hResource);

HB_FUNC(HMG_MAKELONG)
{
  hb_retnl(MAKELONG(hb_parni(1), hb_parni(2)));
}

HB_FUNC(HMG__ENABLESCROLLBARS)
{
  EnableScrollBar(hmg_par_HWND(1), hb_parni(2), hb_parni(3));
}

HB_FUNC(HMG_DELETEOBJECT)
{
  HANDLE hRes = hmg_par_HANDLE(1);

  if (hRes) {
    DelResource(hRes);
    hb_retl(DeleteObject(static_cast<HGDIOBJ>(hRes)));
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(HMG_IMAGELIST_DESTROY)
{
  auto himl = hmg_par_HIMAGELIST(1);
  DelResource(himl);
  hb_retl(ImageList_Destroy(himl));
}

HB_FUNC(HMG_SETFOCUS)
{
  hmg_ret_HWND(SetFocus(hmg_par_HWND(1)));
}

HB_FUNC(HMG_INSERTSHIFTTAB)
{
  keybd_event(VK_SHIFT, 0, 0, 0);
  keybd_event(VK_TAB, 0, 0, 0);
  keybd_event(VK_SHIFT, 0, KEYEVENTF_KEYUP, 0);
}

HB_FUNC(HMG_SYSTEMPARAMETERSINFO)
{
  hb_retl(SystemParametersInfoA(hmg_par_UINT(1), hmg_par_UINT(2), static_cast<VOID *>(const_cast<char *>(hb_parc(3))),
                                hmg_par_UINT(4)));
}

// GETTEXTWIDTH(HDC, cString, HFONT) --> numeric
HB_FUNC(HMG_GETTEXTWIDTH) // returns the width of a string in pixels
{
  auto hDC = hmg_par_HDC(1);
  HWND hWnd = nullptr;
  auto bDestroyDC = false;
  auto hFont = hmg_par_HFONT(3);
  HFONT hOldFont = nullptr;
  SIZE sz;
  void *String;
  LPCTSTR lpString = HB_PARSTR(2, &String, nullptr);

  if (!hDC) {
    bDestroyDC = true;
    hWnd = GetActiveWindow();
    hDC = GetDC(hWnd);
  }

  if (hFont) {
    hOldFont = static_cast<HFONT>(SelectObject(hDC, hFont));
  }

  GetTextExtentPoint32(hDC, lpString, lstrlen(lpString), &sz);

  if (hFont) {
    SelectObject(hDC, hOldFont);
  }

  if (bDestroyDC) {
    ReleaseDC(hWnd, hDC);
  }

  hb_retnl(sz.cx);

  hb_strfree(String);
}

HB_FUNC(HMG_KEYBD_EVENT)
{
  keybd_event(hmg_par_BYTE(1), static_cast<BYTE>(MapVirtualKey(hb_parni(1), 0)), hb_parl(2) ? KEYEVENTF_KEYUP : 0, 0);
}

HB_FUNC(HMG_INSERTVKEY)
{
  keybd_event(hmg_par_BYTE(1), 0, 0, 0);
}

HB_FUNC(_HMG_SETVSCROLLVALUE)
{
  SendMessage(hmg_par_HWND(1), WM_VSCROLL, MAKEWPARAM(SB_THUMBPOSITION, hb_parni(2)), 0);
}

HB_FUNC(_HMG_SETHSCROLLVALUE)
{
  SendMessage(hmg_par_HWND(1), WM_HSCROLL, MAKEWPARAM(SB_THUMBPOSITION, hb_parni(2)), 0);
}

HB_FUNC(HMG_SHOWCARET)
{
  hb_retl(ShowCaret(hmg_par_HWND(1)));
}

HB_FUNC(HMG_HIDECARET)
{
  hb_retl(HideCaret(hmg_par_HWND(1)));
}

HB_FUNC(HMG_DESTROYCARET)
{
  hb_retl(DestroyCaret());
}

HB_FUNC(HMG_CREATECARET)
{
  hb_retl(CreateCaret(hmg_par_HWND(1), hmg_par_HBITMAP(2), hmg_par_int(3), hmg_par_int(4)));
}

// CHANGESTYLE(hWnd, dwAdd, dwRemove, lExStyle)
// Action: Modifies the basic styles of a window
// Parameters: hWnd - handle to window
//             dwAdd - window styles to add
//             dwRemove - window styles to remove
//             lExStyle - TRUE for Extended style otherwise FALSE
// HMG 1.1 Expermental Build 12a
// (C)Jacek Kubica <kubica@wssk.wroc.pl>

HB_FUNC(HMG_CHANGESTYLE)
{
  auto hWnd = hmg_par_HWND(1);
  auto dwAdd = static_cast<LONG_PTR>(HB_PARNL(2));
  auto dwRemove = static_cast<LONG_PTR>(HB_PARNL(3));
  int iStyle = hb_parl(4) ? GWL_EXSTYLE : GWL_STYLE;
  LONG_PTR dwStyle, dwNewStyle;

  dwStyle = GetWindowLongPtr(hWnd, iStyle);
  dwNewStyle = (dwStyle & (~dwRemove)) | dwAdd;

  HB_RETNL(static_cast<LONG_PTR>(SetWindowLongPtr(hWnd, iStyle, dwNewStyle)));

  SetWindowPos(hWnd, nullptr, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER);
}

HB_FUNC(HMG_MOVEBTNTEXTBOX) // MoveBtnTextBox(hEdit, hBtn1, hBtn2, fBtn2, BtnWidth, width, height)
{
  auto hedit = hmg_par_HWND(1);
  auto hBtn1 = hmg_par_HWND(2);
  auto hBtn2 = hmg_par_HWND(3);
  BOOL fBtn2 = hb_parl(4);
  auto BtnWidth = hmg_par_int(5);
  int BtnWidth2;
  auto width = hmg_par_int(6);
  auto height = hmg_par_int(7);
  BOOL fBtns = (hb_parnl(2) > 0);

  BtnWidth = (BtnWidth >= GetSystemMetrics(SM_CYSIZE) ? BtnWidth : GetSystemMetrics(SM_CYSIZE) - 1);
  BtnWidth = (fBtns ? BtnWidth : 0);
  BtnWidth2 = (fBtn2 ? BtnWidth : 0);

  SetWindowPos(hedit, nullptr, 0, 0, width, height, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER);
  if (fBtns) {
    SetWindowPos(hBtn1, nullptr, width - BtnWidth - 4, -1, BtnWidth, height - 2, SWP_NOACTIVATE | SWP_NOZORDER);
    if (fBtn2) {
      SetWindowPos(hBtn2, nullptr, width - BtnWidth - BtnWidth2 - 4, -1, BtnWidth2, height - 2,
                   SWP_NOACTIVATE | SWP_NOZORDER);
    }
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(MAKELONG, HMG_MAKELONG)
HB_FUNC_TRANSLATE(_ENABLESCROLLBARS, HMG__ENABLESCROLLBARS)
HB_FUNC_TRANSLATE(DELETEOBJECT, HMG_DELETEOBJECT)
HB_FUNC_TRANSLATE(IMAGELIST_DESTROY, HMG_IMAGELIST_DESTROY)
HB_FUNC_TRANSLATE(SETFOCUS, HMG_SETFOCUS)
HB_FUNC_TRANSLATE(INSERTSHIFTTAB, HMG_INSERTSHIFTTAB)
HB_FUNC_TRANSLATE(SYSTEMPARAMETERSINFO, HMG_SYSTEMPARAMETERSINFO)
HB_FUNC_TRANSLATE(GETTEXTWIDTH, HMG_GETTEXTWIDTH)
HB_FUNC_TRANSLATE(KEYBD_EVENT, HMG_KEYBD_EVENT)
HB_FUNC_TRANSLATE(INSERTVKEY, HMG_INSERTVKEY)
HB_FUNC_TRANSLATE(SHOWCARET, HMG_SHOWCARET)
HB_FUNC_TRANSLATE(HIDECARET, HMG_HIDECARET)
HB_FUNC_TRANSLATE(DESTROYCARET, HMG_DESTROYCARET)
HB_FUNC_TRANSLATE(CREATECARET, HMG_CREATECARET)
HB_FUNC_TRANSLATE(CHANGESTYLE, HMG_CHANGESTYLE)
HB_FUNC_TRANSLATE(MOVEBTNTEXTBOX, HMG_MOVEBTNTEXTBOX)
#endif
