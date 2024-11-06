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

// Parts of this code is contributed and used here under permission of his
// author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>

#define _WIN32_IE 0x0501
#define _WIN32_WINNT 0x0600

#include "mgdefs.hpp"
#include <hbapierr.hpp>
#include <hbapiitm.hpp>
#include <hbapicdp.hpp>
#include <hbwinuni.hpp>
#include <commctrl.h>

#if 0
#ifndef TTS_CLOSE
#define TTS_CLOSE 0x80
#endif
#ifndef TTM_POPUP
#define TTM_POPUP (WM_USER + 34)
#endif
#endif

#if 0
#if (defined(__BORLANDC__) && __BORLANDC__ < 1410)
typedef struct _tagEDITBALLOONTIP
{
   DWORD   cbStruct;
   LPCWSTR pszTitle;
   LPCWSTR pszText;
   INT     ttiIcon; // From TTI_*
} EDITBALLOONTIP, * PEDITBALLOONTIP;
#endif

#define ECM_FIRST 0x1500 // Edit control messages

#define EM_SHOWBALLOONTIP (ECM_FIRST + 3) // Show a balloon tip associated to the edit control
#define Edit_ShowBalloonTip(hwnd, peditballoontip)                                                                     \
  static_cast<BOOL>(SNDMSG((hwnd), EM_SHOWBALLOONTIP, 0, (LPARAM)(peditballoontip)))
#define EM_HIDEBALLOONTIP (ECM_FIRST + 4) // Hide any balloon tip associated with the edit control
#define Edit_HideBalloonTip(hwnd) static_cast<BOOL>(SNDMSG((hwnd), EM_HIDEBALLOONTIP, 0, 0))
#endif

#define HB_cdpGetU16(cdp, fCtrl, ch) hb_cdpGetU16(cdp, ch)

extern BOOL _isValidCtrlClass(HWND, LPCTSTR);

bool hmg_ArrayToPoint(PHB_ITEM aPoint, POINT *pt);
bool hmg_ArrayToRect(PHB_ITEM aPoint, RECT *rect);
bool hmg_ArrayToColorRef(PHB_ITEM aCRef, COLORREF *cr);
PHB_ITEM hmg_RectToArray(RECT *rc);

static auto g_bIsToolTipActive = true;
static auto g_bIsToolTipBalloon = false;

static int g_iToolTipMaxWidth = -1;

HB_FUNC(HMG_SETTOOLTIPACTIVATE)
{
  bool g_bOldToolTipActive = g_bIsToolTipActive;

  if (HB_ISLOG(1))
  {
    g_bIsToolTipActive = hb_parl(1);
  }

  hb_retl(g_bOldToolTipActive);
}

HB_FUNC(HMG_SETTOOLTIPBALLOON)
{
  bool g_bOldToolTipBalloon = g_bIsToolTipBalloon;

  if (HB_ISLOG(1))
  {
    g_bIsToolTipBalloon = hb_parl(1);
  }

  hb_retl(g_bOldToolTipBalloon);
}

HB_FUNC(HMG_SETTOOLTIPMAXWIDTH)
{
  bool g_iOldToolTipMaxWidth = g_iToolTipMaxWidth;

  if (HB_ISNUM(1))
  {
    g_iToolTipMaxWidth = hb_parni(1);
  }

  hb_retni(g_iOldToolTipMaxWidth);
}

/*
   nToolTip := InitToolTip(nFormHandle, hmg_SetToolTipBalloon())

   for ModalWindow : nToolTip := InitToolTip(, hmg_SetToolTipBalloon())
 */

/*
HMG_INITTOOLTIP(HWND, lBalloon) --> hTooltip
*/
HB_FUNC(HMG_INITTOOLTIP)
{
  HWND hwndParent = HB_ISNIL(1) ? nullptr : hmg_par_HWND(1);

  if (HB_ISNIL(1) ? true : IsWindow(hwndParent))
  { // hack for ModalWindow
    INITCOMMONCONTROLSEX icex = {sizeof(INITCOMMONCONTROLSEX), ICC_BAR_CLASSES};
    InitCommonControlsEx(&icex);

    DWORD dwStyle = WS_POPUP | TTS_ALWAYSTIP;

    if (hb_pcount() > 1)
    {
      if (HB_ISLOG(2) && hb_parl(2))
      {
        dwStyle |= TTS_BALLOON;
      }
    }
    else if (g_bIsToolTipBalloon)
    {
      dwStyle |= TTS_BALLOON;
    }

    /* Create a tooltip */
    auto hwndToolTip = CreateWindowEx(0, TOOLTIPS_CLASS, nullptr, dwStyle, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                                      CW_USEDEFAULT, hwndParent, nullptr, GetInstance(), nullptr);

    hmg_ret_HWND(hwndToolTip);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
HMG_SETTOOLTIP(hwndTool, cText, hwndToolTip) -->
*/
HB_FUNC(HMG_SETTOOLTIP)
{
  auto hwndTool = hmg_par_HWND(1);
  auto hwndToolTip = hmg_par_HWND(3);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    if (IsWindow(hwndTool))
    {
      void *str = nullptr;
      TOOLINFO ti{};
      ti.cbSize = sizeof(ti);
      ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
      ti.hwnd = GetParent(hwndTool);
      ti.uId = reinterpret_cast<UINT_PTR>(hwndTool);
      if (SendMessage(hwndToolTip, TTM_GETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti)))
      {
        SendMessage(hwndToolTip, TTM_DELTOOL, 0, reinterpret_cast<LPARAM>(&ti));
      }
      if (hb_parclen(2) > 0)
      {
        ti.lpszText = const_cast<TCHAR *>(HB_PARSTR(2, &str, nullptr));
      }
      hb_retl(SendMessage(hwndToolTip, TTM_ADDTOOL, 0, reinterpret_cast<LPARAM>(&ti)) ? true : false);
      SendMessage(hwndToolTip, TTM_ACTIVATE, g_bIsToolTipActive, 0);
      hb_strfree(str);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
  }
}

/*
   hmg_ShowBalloonTip(hWnd, cText [, cTitle] [, nTypeIcon])
 */
HB_FUNC(HMG_SHOWBALLOONTIP)
{
  WCHAR Text[512];
  WCHAR Title[512];

  PHB_CODEPAGE s_cdpHost = hb_vmCDP();

  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd))
  {
    EDITBALLOONTIP bl{};
    bl.cbStruct = sizeof(EDITBALLOONTIP);
    bl.pszTitle = nullptr;
    bl.pszText = nullptr;
    bl.ttiIcon = hb_parnidef(4, 0 /*TTI_NONE*/);

    if (HB_ISCHAR(2))
    {
      ZeroMemory(Text, sizeof(Text));
      int k = hb_parclen(2);
      auto s = hb_parc(2);
      for (auto i = 0; i < k; i++)
      {
        Text[i] = HB_cdpGetU16(s_cdpHost, TRUE, s[i]);
      }
      bl.pszText = Text;
    }

    if (HB_ISCHAR(3))
    {
      ZeroMemory(Title, sizeof(Title));
      int k = hb_parclen(3);
      auto s = hb_parc(3);
      for (auto i = 0; i < k; i++)
      {
        Title[i] = HB_cdpGetU16(s_cdpHost, TRUE, s[i]);
      }
      bl.pszTitle = Title;
    }

    Edit_ShowBalloonTip(hWnd, &bl);
  }
}

HB_FUNC(HMG_HIDEBALLOONTIP)
{
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd))
  {
    Edit_HideBalloonTip(hWnd);
  }
}

/*
   nToolTip := hmg_InitToolTipEx(nFormHandle [, aRect ][, cToolTip ][, cTitle ][, nIcon ][, nStyle
   ][, nFlags ])
 */
HB_FUNC(HMG_INITTOOLTIPEX)
{
  auto hwndParent = hmg_par_HWND(1);

  if (IsWindow(hwndParent))
  {
    auto aRect = hb_param(2, Harbour::Item::ANY);

    RECT rect;
    if (!hmg_ArrayToRect(aRect, &rect))
    {
      GetClientRect(hwndParent, &rect);
    }

    void *str1 = nullptr;
    LPTSTR lpszText = nullptr;
    void *str2 = nullptr;
    LPTSTR lpszTitle = HB_ISCHAR(4) ? const_cast<TCHAR *>(HB_PARSTR(4, &str2, nullptr)) : nullptr;

    if (hb_parclen(3) > 0)
    {
      lpszText = const_cast<TCHAR *>(HB_PARSTR(3, &str1, nullptr));
    }
    else if (HB_ISNUM(3))
    {
      lpszText = static_cast<LPTSTR>(MAKEINTRESOURCE(hb_parni(3)));
    }

    DWORD dwStyle = WS_POPUP;
    if (HB_ISNUM(6))
    {
      dwStyle |= hmg_par_DWORD(6);
    }

    UINT uFlags = 0;
    if (HB_ISNUM(7))
    {
      uFlags = hmg_par_UINT(7);
    }

    INITCOMMONCONTROLSEX icex = {sizeof(INITCOMMONCONTROLSEX), ICC_BAR_CLASSES};
    InitCommonControlsEx(&icex);

    /* Create a tooltip */
    auto hwndToolTip = CreateWindowEx(WS_EX_TOPMOST, TOOLTIPS_CLASS, nullptr, dwStyle, CW_USEDEFAULT, CW_USEDEFAULT,
                                      CW_USEDEFAULT, CW_USEDEFAULT, hwndParent, nullptr, GetInstance(), nullptr);

    SetWindowPos(hwndToolTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);

    /* Set up "tool" information. In this case, the "tool" is the entire parent window. */
    TOOLINFO ti{};
    ti.cbSize = sizeof(ti);
    ti.uFlags = uFlags;
    ti.hwnd = hwndParent;
    ti.uId = (UINT_PTR)hwndParent;
    ti.rect = rect;
    ti.hinst = GetInstance();
    ti.lpszText = lpszText;

    // Associate the tooltip with the "tool" window.
    SendMessage(hwndToolTip, TTM_ADDTOOL, 0, reinterpret_cast<LPARAM>(&ti));

    int nIcon = hb_parnidef(5, TTI_NONE);

    if (lpszTitle != nullptr)
    {
      SendMessage(hwndToolTip, TTM_SETTITLE, nIcon, reinterpret_cast<LPARAM>(lpszTitle));
    }

    if (g_iToolTipMaxWidth != -1)
    {
      SendMessage(hwndToolTip, TTM_SETMAXTIPWIDTH, 0, g_iToolTipMaxWidth);
    }

    SendMessage(hwndToolTip, TTM_ACTIVATE, g_bIsToolTipActive, 0);

    hmg_ret_HWND(hwndToolTip);

    hb_strfree(str1);
    hb_strfree(str2);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   ToolTip messages - TTM_messages
 */

/*
   TM_ACTIVATE - activates or deactivates a tooltip control,

   has no effect if g_bIsToolTipActive == FALSE ( after hmg_SetToolTipActivate(.F.) )
 */
HB_FUNC(HMG_TTM_ACTIVATE)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    if (g_bIsToolTipActive)
    {
      SendMessage(hwndToolTip, TTM_ACTIVATE, hmg_par_BOOL(2), 0);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_ADDTOOL - registers a tool with a tooltip control

   TTM_ADJUSTRECT -
   calculates   a   tooltip  control's  text  display  rectangle  from  its
   window  rectangle,    or   the   tooltip   window  rectangle  needed  to
   display a specified text display rectangle.

   TTM_DELTOOL - removes a tool from a tooltip control

   TTM_ENUMTOOLS -
   retrieves  the  information  that  a tooltip control maintains about the
   current  tool—that  is,  the  tool  for  which  the tooltip is currently
   displaying text.

   TTM_GETBUBBLESIZE - returns the width and height of a tooltip control

   TTM_GETCURRENTTOOL -
   retrieves the information for the current tool in a tooltip control
 */

/*
   TTM_GETDELAYTIME -
   retrieves  the initial, pop-up, and reshow durations currently set for a
   tooltip control
 */
HB_FUNC(HMG_TTM_GETDELAYTIME)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_GETDELAYTIME, hb_parnidef(2, TTDT_AUTOPOP), 0));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_GETMARGIN -
   retrieves  the  top,  left,  bottom, and right margins set for a tooltip
   window.  A margin is the distance, in pixels, between the tooltip window
   border and the text contained within the tooltip window
 */
HB_FUNC(HMG_TTM_GETMARGIN)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    RECT rect;

    SendMessage(hwndToolTip, TTM_GETMARGIN, 0, reinterpret_cast<LPARAM>(&rect));

    hb_itemReturnRelease(hmg_RectToArray(&rect));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_GETMAXTIPWIDTH - retrieves the maximum width for a tooltip window
 */
HB_FUNC(HMG_TTM_GETMAXTIPWIDTH)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_GETMAXTIPWIDTH, 0, 0));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_GETTEXT -
   retrieves the information a tooltip control maintains about a tool
 */

/*
   TTM_GETTIPBKCOLOR - retrieves the background color in a tooltip window
 */
HB_FUNC(HMG_TTM_GETTIPBKCOLOR)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_GETTIPBKCOLOR, 0, 0));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_GETTIPTEXTCOLOR  - retrieves the text color in a tooltip window
 */
HB_FUNC(HMG_TTM_GETTIPTEXTCOLOR)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_GETTIPTEXTCOLOR, 0, 0));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_GETTITLE -
   retrieve information concerning the title of a tooltip control
 */

/*
   TTM_GETTOOLCOUNT -
   retrieves a count of the tools maintained by a tooltip control
 */
HB_FUNC(HMG_TTM_GETTOOLCOUNT)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_GETTOOLCOUNT, 0, 0));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_GETTOOLINFO -
   retrieves the information that a tooltip control maintains about a tool

   TTM_HITTEST -
   tests  a  point to determine whether it is within the bounding rectangle
   of  the  specified  tool  and, if it is, retrieves information about the
   tool

   TTM_NEWTOOLRECT - sets a new bounding rectangle for a tool
 */

/*
   TTM_POP - removes a displayed tooltip window from view
 */
HB_FUNC(HMG_TTM_POP)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    SendMessage(hwndToolTip, TTM_POP, 0, 0);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_POPUP -
   causes the tooltip to display at the coordinates of the last mouse message
 */
HB_FUNC(HMG_TTM_POPUP)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    SendMessage(hwndToolTip, TTM_POPUP, 0, 0);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_RELAYEVENT -
   passes a mouse message to a tooltip control for processing
 */

/*
   TTM_SETDELAYTIME
   sets the initial, pop-up, and reshow durations for a tooltip control
 */
HB_FUNC(HMG_TTM_SETDELAYTIME)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    int nMilliSec = hb_parnidef(3, -1);

    if (nMilliSec < 0)
    {
      SendMessage(hwndToolTip, TTM_SETDELAYTIME, hb_parnidef(2, TTDT_AUTOPOP), -1);
    }
    else
    {
      SendMessage(hwndToolTip, TTM_SETDELAYTIME, hb_parnidef(2, TTDT_AUTOPOP),
                  static_cast<LPARAM>(static_cast<DWORD>(nMilliSec)));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_SETMARGIN  -
   sets  the  top,  left, bottom, and right margins for a tooltip window. A
   margin is the distance, in pixels, between the tooltip window border and
   the text contained within the tooltip window.
 */
HB_FUNC(HMG_TTM_SETMARGIN)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    RECT rect;

    if (hmg_ArrayToRect(hb_param(2, Harbour::Item::ANY), &rect))
    {
      SendMessage(hwndToolTip, TTM_SETMARGIN, 0, reinterpret_cast<LPARAM>(&rect));
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(2));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_SETMAXTIPWIDTH - sets the maximum width for a tooltip window
 */
HB_FUNC(HMG_TTM_SETMAXTIPWIDTH)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    hb_retni(SendMessage(hwndToolTip, TTM_SETMAXTIPWIDTH, 0, (LPARAM)hb_parnidef(2, g_iToolTipMaxWidth)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_SETTIPBKCOLOR - sets the background color in a tooltip window
 */
HB_FUNC(HMG_TTM_SETTIPBKCOLOR)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    auto cr = static_cast<COLORREF>(0);

    if (HB_ISNUM(2) || hmg_ArrayToColorRef(hb_param(2, Harbour::Item::ARRAY), &cr))
    {
      if (HB_ISNUM(2))
      {
        cr = static_cast<COLORREF>(HB_PARNL(2));
      }

      SendMessage(hwndToolTip, TTM_SETTIPBKCOLOR, cr, 0);
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(2));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_SETTIPTEXTCOLOR - sets the text color in a tooltip window
 */
HB_FUNC(HMG_TTM_SETTIPTEXTCOLOR)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    auto cr = static_cast<COLORREF>(0);

    if (HB_ISNUM(2) || hmg_ArrayToColorRef(hb_param(2, Harbour::Item::ANY), &cr))
    {
      if (HB_ISNUM(2))
      {
        cr = static_cast<COLORREF>(HB_PARNL(2));
      }

      SendMessage(hwndToolTip, TTM_SETTIPTEXTCOLOR, cr, 0);
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(2));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/* TODO
   TTM_SETTITLE - adds a standard icon and title string to a tooltip

   TTM_SETTOOLINFO -
   sets the information that a tooltip control maintains for a tool

   TTM_SETWINDOWTHEME - sets the visual style of a tooltip control
 */

/*
   TTM_TRACKACTIVATE - activates or deactivates a tracking tooltip
 */
HB_FUNC(HMG_TTM_TRACKACTIVATE)
{
  auto hwndToolTip = hmg_par_HWND(1);
  auto hwndTool = hmg_par_HWND(2);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS) && IsWindow(hwndTool))
  {
    TOOLINFO ti{};

    ti.cbSize = sizeof(ti);
    ti.hwnd = hwndTool;
    ti.uId = (UINT_PTR)hwndTool;

    SendMessage(hwndToolTip, TTM_TRACKACTIVATE, hb_parl(3), reinterpret_cast<LPARAM>(&ti));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
  }
}

/*
   TTM_TRACKPOSITION - sets the position of a tracking tooltip
 */
HB_FUNC(HMG_TTM_TRACKPOSITION)
{
  auto hwndToolTip = hmg_par_HWND(1);
  auto hwndTool = hmg_par_HWND(2);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS) && IsWindow(hwndTool))
  {
    POINT point;

    if (hmg_ArrayToPoint(hb_param(3, Harbour::Item::ARRAY), &point))
    {
      ClientToScreen(hwndTool, &point);

      SendMessage(hwndToolTip, TTM_TRACKPOSITION, 0, MAKELONG(point.x, point.y));
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(3));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
  }
}

/*
   TTM_UPDATE - forces the current tooltip to be redrawn
 */
HB_FUNC(HMG_TTM_UPDATE)
{
  auto hwndToolTip = hmg_par_HWND(1);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS))
  {
    SendMessage(hwndToolTip, TTM_UPDATE, 0, 0);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

/*
   TTM_UPDATETIPTEXT - sets the tooltip text for a tool
 */
HB_FUNC(HMG_TTM_UPDATETIPTEXT) // old HB_FUNC( UPDATETOOLTIPTEXT )
{
  auto hwndToolTip = hmg_par_HWND(1);
  auto hwndTool = hmg_par_HWND(2);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS) && IsWindow(hwndTool))
  {
    if (hb_parclen(3) > 0)
    {
      void *str = nullptr;
      TOOLINFO ti{};
      ti.cbSize = sizeof(ti);
      ti.hinst = nullptr;
      ti.hwnd = hwndTool;
      ti.uId = (UINT_PTR)hwndTool;
      ti.lpszText = const_cast<TCHAR *>(HB_PARSTR(3, &str, nullptr));
      SendMessage(hwndToolTip, TTM_UPDATETIPTEXT, 0, reinterpret_cast<LPARAM>(&ti));
      hb_strfree(str);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
  }
}

/*
   TTM_WINDOWFROMPOINT -
   allows  a  subclass  procedure  to cause a tooltip to display text for a
   window other than the one beneath the mouse cursor
 */
HB_FUNC(HMG_TTM_WINDOWFROMPOINT)
{
  auto hwndToolTip = hmg_par_HWND(1);
  auto hwndTool = hmg_par_HWND(2);

  if (_isValidCtrlClass(hwndToolTip, TOOLTIPS_CLASS) && IsWindow(hwndTool))
  {
    POINT point;

    if (hmg_ArrayToPoint(hb_param(3, Harbour::Item::ARRAY), &point))
    {
      ClientToScreen(hwndTool, &point);

      HB_RETNL(static_cast<LONG_PTR>(SendMessage(hwndToolTip, TTM_WINDOWFROMPOINT, 0, MAKELONG(point.x, point.y))));
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(3));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(SETTOOLTIPACTIVATE, HMG_SETTOOLTIPACTIVATE)
HB_FUNC_TRANSLATE(SETTOOLTIPBALLOON, HMG_SETTOOLTIPBALLOON)
HB_FUNC_TRANSLATE(SETTOOLTIPMAXWIDTH, HMG_SETTOOLTIPMAXWIDTH)
HB_FUNC_TRANSLATE(INITTOOLTIP, HMG_INITTOOLTIP)
HB_FUNC_TRANSLATE(SETTOOLTIP, HMG_SETTOOLTIP)
HB_FUNC_TRANSLATE(SHOWBALLOONTIP, HMG_SHOWBALLOONTIP)
HB_FUNC_TRANSLATE(HIDEBALLOONTIP, HMG_HIDEBALLOONTIP)
HB_FUNC_TRANSLATE(INITTOOLTIPEX, HMG_INITTOOLTIPEX)
#ifdef TTM_ACTIVATE
#undef TTM_ACTIVATE
#endif
HB_FUNC_TRANSLATE(TTM_ACTIVATE, HMG_TTM_ACTIVATE)
#ifdef TTM_GETDELAYTIME
#undef TTM_GETDELAYTIME
#endif
HB_FUNC_TRANSLATE(TTM_GETDELAYTIME, HMG_TTM_GETDELAYTIME)
#ifdef TTM_GETMARGIN
#undef TTM_GETMARGIN
#endif
HB_FUNC_TRANSLATE(TTM_GETMARGIN, HMG_TTM_GETMARGIN)
#ifdef TTM_GETMAXTIPWIDTH
#undef TTM_GETMAXTIPWIDTH
#endif
HB_FUNC_TRANSLATE(TTM_GETMAXTIPWIDTH, HMG_TTM_GETMAXTIPWIDTH)
#ifdef TTM_GETTIPBKCOLOR
#undef TTM_GETTIPBKCOLOR
#endif
HB_FUNC_TRANSLATE(TTM_GETTIPBKCOLOR, HMG_TTM_GETTIPBKCOLOR)
#ifdef TTM_GETTIPTEXTCOLOR
#undef TTM_GETTIPTEXTCOLOR
#endif
HB_FUNC_TRANSLATE(TTM_GETTIPTEXTCOLOR, HMG_TTM_GETTIPTEXTCOLOR)
#ifdef TTM_GETTOOLCOUNT
#undef TTM_GETTOOLCOUNT
#endif
HB_FUNC_TRANSLATE(TTM_GETTOOLCOUNT, HMG_TTM_GETTOOLCOUNT)
#ifdef TTM_POP
#undef TTM_POP
#endif
HB_FUNC_TRANSLATE(TTM_POP, HMG_TTM_POP)
#ifdef TTM_POPUP
#undef TTM_POPUP
#endif
HB_FUNC_TRANSLATE(TTM_POPUP, HMG_TTM_POPUP)
#ifdef TTM_SETDELAYTIME
#undef TTM_SETDELAYTIME
#endif
HB_FUNC_TRANSLATE(TTM_SETDELAYTIME, HMG_TTM_SETDELAYTIME)
#ifdef TTM_SETMARGIN
#undef TTM_SETMARGIN
#endif
HB_FUNC_TRANSLATE(TTM_SETMARGIN, HMG_TTM_SETMARGIN)
#ifdef TTM_SETMAXTIPWIDTH
#undef TTM_SETMAXTIPWIDTH
#endif
HB_FUNC_TRANSLATE(TTM_SETMAXTIPWIDTH, HMG_TTM_SETMAXTIPWIDTH)
#ifdef TTM_SETTIPBKCOLOR
#undef TTM_SETTIPBKCOLOR
#endif
HB_FUNC_TRANSLATE(TTM_SETTIPBKCOLOR, HMG_TTM_SETTIPBKCOLOR)
#ifdef TTM_SETTIPTEXTCOLOR
#undef TTM_SETTIPTEXTCOLOR
#endif
HB_FUNC_TRANSLATE(TTM_SETTIPTEXTCOLOR, HMG_TTM_SETTIPTEXTCOLOR)
#ifdef TTM_TRACKACTIVATE
#undef TTM_TRACKACTIVATE
#endif
HB_FUNC_TRANSLATE(TTM_TRACKACTIVATE, HMG_TTM_TRACKACTIVATE)
#ifdef TTM_TRACKPOSITION
#undef TTM_TRACKPOSITION
#endif
HB_FUNC_TRANSLATE(TTM_TRACKPOSITION, HMG_TTM_TRACKPOSITION)
#ifdef TTM_UPDATE
#undef TTM_UPDATE
#endif
HB_FUNC_TRANSLATE(TTM_UPDATE, HMG_TTM_UPDATE)
#ifdef TTM_UPDATETIPTEXT
#undef TTM_UPDATETIPTEXT
#endif
HB_FUNC_TRANSLATE(TTM_UPDATETIPTEXT, HMG_TTM_UPDATETIPTEXT)
#ifdef TTM_WINDOWFROMPOINT
#undef TTM_WINDOWFROMPOINT
#endif
HB_FUNC_TRANSLATE(TTM_WINDOWFROMPOINT, HMG_TTM_WINDOWFROMPOINT)
#endif
