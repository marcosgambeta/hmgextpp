//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

// $BEGIN_LICENSE$
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
// $END_LICENSE$

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

#include "mgdefs.hpp"
#include <commctrl.h>
#include <mmsystem.h>
#include <hbwinuni.hpp>

#if 0
#if defined(__BORLANDC__)
#pragma warn - use // unused var
#if defined(_WIN64)
#ifndef UNICODE
HWND MCIWndCreateA(HWND hwndParent, HINSTANCE hInstance, DWORD dwStyle, LPCSTR szFile)
{
   return 0;
}
#else
HWND MCIWndCreateW(HWND hwndParent, HINSTANCE hInstance, DWORD dwStyle, LPCWSTR szFile)
{
   return 0;
}
#endif // UNICODE
#endif // _WIN64
#endif // __BORLANDC__
#endif

#include <vfw.h>

HB_FUNC(HMG_MESSAGEBEEP)
{
  hb_retl(MessageBeep(hb_parni(1)));
}

HB_FUNC(HMG_C_PLAYWAVE)
{
  int style = SND_ASYNC;
  HMODULE hmod = nullptr;

  if (hb_parl(2)) {
    style |= SND_RESOURCE;
    hmod = GetResources();
  } else {
    style |= SND_FILENAME;
  }

  if (hb_parl(3)) {
    style |= SND_SYNC;
  }

  if (hb_parl(4)) {
    style |= SND_NOSTOP;
  }

  if (hb_parl(5)) {
    style |= SND_LOOP;
  }

  if (hb_parl(6)) {
    style |= SND_NODEFAULT;
  }

  void *str;
  hb_retl(PlaySound(HB_PARSTR(1, &str, nullptr), hmod, style));
  hb_strfree(str);
}

HB_FUNC(HMG_STOPWAVE)
{
  hb_retl(PlaySound(nullptr, GetResources(), SND_PURGE));
}

HB_FUNC(HMG_INITPLAYER)
{
  DWORD style = WS_VISIBLE | WS_CHILD | WS_BORDER;

  if (hb_parl(7)) {
    style |= MCIWNDF_NOAUTOSIZEWINDOW;
  }

  if (hb_parl(8)) {
    style |= MCIWNDF_NOAUTOSIZEMOVIE;
  }

  if (hb_parl(9)) {
    style |= MCIWNDF_NOERRORDLG;
  }

  if (hb_parl(10)) {
    style |= MCIWNDF_NOMENU;
  }

  if (hb_parl(11)) {
    style |= MCIWNDF_NOOPEN;
  }

  if (hb_parl(12)) {
    style |= MCIWNDF_NOPLAYBAR;
  }

  if (hb_parl(13)) {
    style |= MCIWNDF_SHOWALL;
  }

  if (hb_parl(14)) {
    style |= MCIWNDF_SHOWMODE;
  }

  if (hb_parl(15)) {
    style |= MCIWNDF_SHOWNAME;
  }

  if (hb_parl(16)) {
    style |= MCIWNDF_SHOWPOS;
  }

  void *str;
  HWND hwnd = MCIWndCreate(hmg_par_HWND(1), nullptr, style, HB_PARSTR(2, &str, nullptr));
  hb_strfree(str);

  if (hwnd == nullptr) {
    MessageBox(0, TEXT("Player Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
  }

  MoveWindow(hwnd, hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), TRUE);
  hmg_ret_HWND(hwnd);
}

HB_FUNC(HMG_MCIFUNC)
{
  auto mcihand = hmg_par_HWND(1);
  auto func = hb_parni(2);

  switch (func)
  {
  case 1:
    hb_retnl(MCIWndPlay(mcihand));
    break;
  case 2:
    hb_retnl(MCIWndStop(mcihand));
    break;
  case 3:
    hb_retnl(MCIWndPause(mcihand));
    break;
  case 4:
    hb_retnl(MCIWndClose(mcihand));
    break;
  case 5:
    MCIWndDestroy(mcihand);
    hb_retnl(0);
    break;
  case 6:
    hb_retnl(MCIWndEject(mcihand));
    break;
  case 7:
    hb_retnl(MCIWndEnd(mcihand));
    break;
  case 8:
    hb_retnl(MCIWndHome(mcihand));
    break;
  case 9:
    hb_retnl(MCIWndOpen(mcihand, hb_parc(3), 0));
    break;
  case 10:
    hb_retnl(MCIWndOpenDialog(mcihand));
    break;
  case 11:
    hb_retnl(MCIWndPlayReverse(mcihand));
    break;
  case 12:
    hb_retnl(MCIWndResume(mcihand));
    break;
  case 13:
    MCIWndSetRepeat(mcihand, hb_parl(3));
    hb_retnl(0);
    break;
  case 14:
    hb_retnl(MCIWndSetSpeed(mcihand, hb_parni(3)));
    break;
  case 15:
    hb_retnl(MCIWndSetVolume(mcihand, hb_parni(3)));
    break;
  case 16:
    MCIWndSetZoom(mcihand, hb_parni(3));
    hb_retnl(0);
    break;
  case 17:
    hb_retnl(MCIWndGetLength(mcihand));
    break;
  case 18:
    hb_retnl(MCIWndGetPosition(mcihand));
    break;
  case 19:
    hb_retnl(MCIWndGetVolume(mcihand));
    break;
  case 20:
    hb_retnl(MCIWndSeek(mcihand, hb_parni(3)));
    break;
  default:
    hb_retnl(0);
  }
}

HB_FUNC(HMG_INITANIMATE)
{
  DWORD style = WS_CHILD;

  if (hb_parl(9)) {
    style |= WS_BORDER;
  }

  if (!hb_parl(10)) {
    style |= WS_VISIBLE;
  }

  if (hb_parl(6)) {
    style |= ACS_AUTOPLAY;
  }

  if (hb_parl(7)) {
    style |= ACS_CENTER;
  }

  if (hb_parl(8)) {
    style |= ACS_TRANSPARENT;
  }

  HWND hwnd = Animate_Create(hmg_par_HWND(1), nullptr, style, GetResources());

  if (hwnd == nullptr) {
    MessageBox(0, TEXT("AnimateBox Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
  }

  MoveWindow(hwnd, hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), TRUE);
  hmg_ret_HWND(hwnd);
}

HB_FUNC(HMG_OPENANIMATE)
{
  void *str;
  Animate_Open(hmg_par_HWND(1), HB_PARSTR(2, &str, nullptr));
  hb_strfree(str);
}

HB_FUNC(HMG_PLAYANIMATE)
{
  Animate_Play(hmg_par_HWND(1), 0, -1, 1);
}

HB_FUNC(HMG_SEEKANIMATE)
{
  Animate_Seek(hmg_par_HWND(1), hb_parni(2));
}

HB_FUNC(HMG_STOPANIMATE)
{
  Animate_Stop(hmg_par_HWND(1));
}

HB_FUNC(HMG_CLOSEANIMATE)
{
  Animate_Close(hmg_par_HWND(1));
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(MESSAGEBEEP, HMG_MESSAGEBEEP)
HB_FUNC_TRANSLATE(C_PLAYWAVE, HMG_C_PLAYWAVE)
HB_FUNC_TRANSLATE(STOPWAVE, HMG_STOPWAVE)
HB_FUNC_TRANSLATE(INITPLAYER, HMG_INITPLAYER)
HB_FUNC_TRANSLATE(MCIFUNC, HMG_MCIFUNC)
HB_FUNC_TRANSLATE(INITANIMATE, HMG_INITANIMATE)
HB_FUNC_TRANSLATE(OPENANIMATE, HMG_OPENANIMATE)
HB_FUNC_TRANSLATE(PLAYANIMATE, HMG_PLAYANIMATE)
HB_FUNC_TRANSLATE(SEEKANIMATE, HMG_SEEKANIMATE)
HB_FUNC_TRANSLATE(STOPANIMATE, HMG_STOPANIMATE)
HB_FUNC_TRANSLATE(CLOSEANIMATE, HMG_CLOSEANIMATE)
#endif
