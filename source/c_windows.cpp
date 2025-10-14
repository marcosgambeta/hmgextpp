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

// Parts of this code is contributed and used here under permission of his
// author: Copyright 2016-2017 (C) P.Chornyj <myorg63@mail.ru>

#define _WIN32_IE 0x0501

#if defined(__MINGW32__)
#define _WIN32_WINNT 0x0500
#endif // MINGW | XCC

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbvm.hpp>
#include <hbwinuni.hpp>
#include <hbthread.hpp>
#include <hbatomic.hpp>

#ifndef WC_STATIC
#define WC_STATIC "Static"
#endif

#define DEFAULT_LISTENER "EVENTS"
#define MAX_EVENTS 64

// local types
typedef struct tagAppEvent
{
  UINT message;
  PHB_ITEM bAction;
  BOOL active;
} APPEVENT, *APPEVENT_PTR;

typedef struct tagEventsHolder
{
  HWND hwnd;
  BOOL active;
  size_t count;
  HB_COUNTER used;
  APPEVENT events[MAX_EVENTS];
} EVENTSHOLDER, *EVENTSHOLDER_PTR;

typedef struct tagMyParam
{
  PHB_DYNS Listener;
} MYPARAMS;

typedef struct tagMyUserData
{
  UINT cbSize;
  MYPARAMS myParam;
#if defined(_WIN64)
} MYUSERDATA, *PMYUSERDATA;
#else
} MYUSERDATA, UNALIGNED *PMYUSERDATA;
#endif // _WIN64

typedef struct tagWinEvent
{
  UINT message;
  PHB_ITEM bBefore;
  PHB_ITEM bAction;
  PHB_ITEM bAfter;
  BOOL active;
} WINEVENT, *WINEVENT_PTR;

typedef struct tagWinEventsHolder
{
  HWND hwnd;
  BOOL active;
  size_t count;
  HB_COUNTER used;
  WINEVENT events[MAX_EVENTS];
} WINEVENTSHOLDER, *WINEVENTSHOLDER_PTR;

// extern functions
#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif
HINSTANCE GetInstance(void);
HINSTANCE GetResources(void);
extern void hmg_ErrorExit(LPCTSTR lpMessage, DWORD dwError, BOOL bExit);

// local functions
static size_t AppEventScan(EVENTSHOLDER *events, UINT message);
static LRESULT AppEventDo(EVENTSHOLDER *events, bool bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static LRESULT AppEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static bool AppEventRemove(HWND hWnd, const char *pszName, UINT message);
static size_t WinEventScan(WINEVENTSHOLDER *events, UINT message);
static LRESULT WinEventDo(WINEVENTSHOLDER *events, bool bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static LRESULT WinEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static bool WinEventRemove(HWND hWnd, const char *pszName, UINT message);

LRESULT CALLBACK MsgOnlyWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

// extern variables
extern HWND g_hWndMain;
extern HACCEL g_hAccel;

// static variables
static PHB_DYNS g_ListenerDyns = nullptr;

static HB_CRITICAL_NEW(s_lst_mtx);
#define HMG_LISTENER_LOCK hb_threadEnterCriticalSection(&s_lst_mtx)
#define HMG_LISTENER_UNLOCK hb_threadLeaveCriticalSection(&s_lst_mtx)

HB_FUNC(HMG_GETGLOBALLISTENER)
{
  if (g_ListenerDyns != nullptr) {
    hb_retc(hb_dynsymName(g_ListenerDyns));
  } else {
    hb_retc_null();
  }
}

HB_FUNC(HMG_SETGLOBALLISTENER)
{
  auto pszNewName = hb_parc(1);

  if (pszNewName && hb_dynsymIsFunction(hb_dynsymGet(pszNewName))) {
    HMG_LISTENER_LOCK;
    g_ListenerDyns = hb_dynsymGet(pszNewName);
    hb_retl(true);
    HMG_LISTENER_UNLOCK;
  } else {
    hb_retl(false);
  }
}

HB_FUNC(HMG_RESETGLOBALLISTENER)
{
  HMG_LISTENER_LOCK;
  g_ListenerDyns = hb_dynsymGet(DEFAULT_LISTENER);
  HMG_LISTENER_UNLOCK;
}

static size_t AppEventScan(EVENTSHOLDER *events, UINT message)
{
  size_t nPos = 0;

  for (size_t i = 0; i < events->count; i++) {
    if (message == events->events[i].message) {
      nPos = (i + 1);
      break;
    }
  }

  return nPos;
}

static bool AppEventRemove(HWND hWnd, const char *pszProp, UINT message)
{
  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pW);
#else
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      if (message != 0) {
        size_t nPos = AppEventScan(events, message);

        if (nPos > 0) {
                                                            // if found
          hb_itemRelease(events->events[nPos - 1].bAction); // delete old codeblock

          events->events[nPos - 1].message = 0;
          events->events[nPos - 1].bAction = nullptr;
          events->events[nPos - 1].active = FALSE;

          HB_ATOM_DEC(&events->used);
        }
      } else {
        for (size_t i = 0; i < events->count; i++) {
          // delete all not empty items with codeblocks
          if (events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction)) {
            hb_itemRelease(events->events[i].bAction);
          }
        }

        HB_ATOM_SET(&events->used, 0);
      }

      if (!HB_ATOM_GET(&events->used)) {
#ifdef UNICODE
        events = (EVENTSHOLDER *)RemoveProp(hWnd, pW);
#else
        events = (EVENTSHOLDER *)RemoveProp(hWnd, pszProp);
#endif

        hb_xfree(events); // delete events holder
      }

#ifdef UNICODE
      hb_xfree(pW);
#endif
      return true;
    }
  }

  return false;
}

static LRESULT AppEventDo(EVENTSHOLDER *events, bool bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  size_t nPos = AppEventScan(events, message);

  if ((nPos > 0) && events->active &&
      (events->events[nPos - 1].active &&
       ((events->events[nPos - 1].bAction != nullptr) && HB_IS_BLOCK(events->events[nPos - 1].bAction)))) {
    auto phWnd = hb_itemPutNInt(nullptr, reinterpret_cast<LONG_PTR>(hWnd));
    auto pmessage = hb_itemPutNS(nullptr, message);
    auto pwParam = hb_itemPutNInt(nullptr, static_cast<LONG_PTR>(wParam));
    auto plParam = hb_itemPutNInt(nullptr, static_cast<LONG_PTR>(lParam));

    hb_evalBlock(events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, nullptr);

    hb_itemRelease(phWnd);
    hb_itemRelease(pmessage);
    hb_itemRelease(pwParam);
    hb_itemRelease(plParam);

    if (true == bOnce) {
      AppEventRemove(hWnd, "ONCE", message);
    }

    return (LRESULT)hb_parnl(-1);
  }

  return (LRESULT)0;
}

static LRESULT AppEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT r = 0;

  if (IsWindow(hWnd)) {
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, TEXT("ONCE"));

    if (events != nullptr) {
      if (hWnd == events->hwnd) {
        r = AppEventDo(events, true, hWnd, message, wParam, lParam);
      }
    }

    events = (EVENTSHOLDER *)GetProp(hWnd, TEXT("ON"));

    if (events != nullptr) {
      if (hWnd == events->hwnd) {
        r = AppEventDo(events, false, hWnd, message, wParam, lParam);
      }
    }
  }

  return r;
}

HB_FUNC(HMG_APPEVENTS)
{
  BOOL bRes = FALSE;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd) && (message >= WM_APP && message <= (WM_APP + MAX_EVENTS))) {
    BOOL bInit = FALSE;
    const char *pszProp = hb_parldef(5, true) ? "ONCE" : "ON";
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pW);
#else
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif
    size_t nPos;

    if (events == nullptr) {
      events = (EVENTSHOLDER *)hb_xgrabz(sizeof(EVENTSHOLDER));
      events->hwnd = hWnd;
      events->active = hb_parldef(4, true);
      events->count = (size_t)sizeof(events->events) / sizeof(APPEVENT);

      HB_ATOM_SET(&events->used, 0);

      bInit = TRUE;
    }

    nPos = AppEventScan(events, message); // arleady exists ?

    if (nPos > 0) {
      hb_itemRelease(events->events[nPos - 1].bAction);
    } else {
      nPos = bInit ? 1 : AppEventScan(events, 0);
      if (nPos > 0) {
        HB_ATOM_INC(&events->used);
      }
    }

    if (nPos > 0) {
      events->events[nPos - 1].message = message;
      events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
      events->events[nPos - 1].active = hb_parldef(4, true);

      bRes = TRUE;
    }

    if (bInit) {
#ifdef UNICODE
      bRes = SetProp(hWnd, pW, static_cast<HANDLE>(events)) ? true : false;
#else
      bRes = SetProp(hWnd, pszProp, static_cast<HANDLE>(events)) ? true : false;
#endif
    }

#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_retl(bRes ? true : false);
}

HB_FUNC(HMG_APPEVENTSREMOVE)
{
  auto bDel = false;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd)) {
    const char *pszProp = hb_parldef(3, true) ? "ONCE" : "ON";

    bDel = AppEventRemove(hWnd, pszProp, message);
  }

  hb_retl(bDel);
}

HB_FUNC(HMG_APPEVENTSUPDATE)
{
  auto bUpd = false;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd)) {
    const char *pszProp = hb_parldef(5, true) ? "ONCE" : "ON";
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pW);
#else
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      if (message >= WM_APP && message <= (WM_APP + MAX_EVENTS)) {
        size_t nPos = AppEventScan(events, message); // arleady exists ?

        if (nPos > 0) {
          if (HB_IS_BLOCK(hb_param(3, Harbour::Item::ANY))) {
            hb_itemRelease(events->events[nPos - 1].bAction);
            events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
          }

          events->events[nPos - 1].active = hb_parldef(4, true);

          bUpd = true;
        }
      } else if (message == 0) {
        events->active = hb_parldef(4, events->active);

        bUpd = true;
      }
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_retl(bUpd);
}

HB_FUNC(HMG_ENUMAPPEVENTS)
{
  auto hWnd = hmg_par_HWND(1);
  const char *pszProp = hb_parldef(2, true) ? "ONCE" : "ON";
  auto aEvents = hb_itemArrayNew(0);

  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pW);
#else
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      for (size_t i = 0; i < events->count; i++) {
        auto aEvent = hb_itemArrayNew(3);

        hb_arraySetNInt(aEvent, 1, events->events[i].message);
        hb_arraySetL(aEvent, 2, events->events[i].active);

        if (events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction)) {
          hb_arraySet(aEvent, 3, hb_itemClone(events->events[i].bAction));
        } else {
          hb_arraySet(aEvent, 3, nullptr);
        }

        hb_arrayAddForward(aEvents, aEvent);

        hb_itemRelease(aEvent);
      }
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_itemReturnRelease(aEvents);
}

HB_FUNC(HMG_GETAPPEVENTSINFO)
{
  auto hWnd = hmg_par_HWND(1);
  const char *pszProp = hb_parldef(2, true) ? "ONCE" : "ON";
  PHB_ITEM aInfo;

  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pW);
#else
    EVENTSHOLDER *events = (EVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    aInfo = hb_itemArrayNew((events != nullptr) ? 4 : 0);

    if (events != nullptr) {
      hb_arraySetNInt(aInfo, 1, reinterpret_cast<LONG_PTR>(events->hwnd));
      hb_arraySetNS(aInfo, 2, events->count);
      hb_arraySetNS(aInfo, 3, (HB_ISIZ)HB_ATOM_GET(&events->used));
      hb_arraySetL(aInfo, 4, events->active);
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  } else {
    aInfo = hb_itemArrayNew(0);
  }

  hb_itemReturnRelease(aInfo);
}

static size_t WinEventScan(WINEVENTSHOLDER *events, UINT message)
{
  size_t nPos = 0;

  for (size_t i = 0; i < events->count; i++) {
    if (message == events->events[i].message) {
      nPos = (i + 1);
      break;
    }
  }

  return nPos;
}

static bool WinEventRemove(HWND hWnd, const char *pszProp, UINT message)
{
  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pW);
#else
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      if (message != 0) {
        size_t nPos = WinEventScan(events, message);

        if (nPos > 0) {
                                                            // if found
          hb_itemRelease(events->events[nPos - 1].bAction); // delete old codeblock

          events->events[nPos - 1].message = 0;
          events->events[nPos - 1].bAction = nullptr;
          events->events[nPos - 1].active = FALSE;

          HB_ATOM_DEC(&events->used);
        }
      } else {
        for (size_t i = 0; i < events->count; i++) {
          // delete all not empty items with codeblocks
          if (events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction)) {
            hb_itemRelease(events->events[i].bAction);
          }
        }

        HB_ATOM_SET(&events->used, 0);
      }

      if (!HB_ATOM_GET(&events->used)) {
#ifdef UNICODE
        events = (WINEVENTSHOLDER *)RemoveProp(hWnd, pW);
#else
        events = (WINEVENTSHOLDER *)RemoveProp(hWnd, pszProp);
#endif

        hb_xfree(events); // delete events holder
      }

#ifdef UNICODE
      hb_xfree(pW);
#endif
      return true;
    }
  }

  return false;
}

static LRESULT WinEventDo(WINEVENTSHOLDER *events, bool bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  size_t nPos = WinEventScan(events, message);

  if ((nPos > 0) && events->active &&
      (events->events[nPos - 1].active &&
       ((events->events[nPos - 1].bAction != nullptr) && HB_IS_BLOCK(events->events[nPos - 1].bAction)))) {
    auto phWnd = hb_itemPutNInt(nullptr, reinterpret_cast<LONG_PTR>(hWnd));
    auto pmessage = hb_itemPutNS(nullptr, message);
    auto pwParam = hb_itemPutNInt(nullptr, static_cast<LONG_PTR>(wParam));
    auto plParam = hb_itemPutNInt(nullptr, static_cast<LONG_PTR>(lParam));

    hb_evalBlock(events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, nullptr);

    hb_itemRelease(phWnd);
    hb_itemRelease(pmessage);
    hb_itemRelease(pwParam);
    hb_itemRelease(plParam);

    if (true == bOnce) {
      WinEventRemove(hWnd, "ONCE", message);
    }

    return (LRESULT)hb_parnl(-1);
  }

  return (LRESULT)0;
}

static LRESULT WinEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT r = 0;

  if (IsWindow(hWnd)) {
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, TEXT("ONCE"));

    if (events != nullptr) {
      if (hWnd == events->hwnd) {
        r = WinEventDo(events, true, hWnd, message, wParam, lParam);
      }
    }

    events = (WINEVENTSHOLDER *)GetProp(hWnd, TEXT("ON"));

    if (events != nullptr) {
      if (hWnd == events->hwnd) {
        r = WinEventDo(events, false, hWnd, message, wParam, lParam);
      }
    }
  }

  return r;
}

HB_FUNC(HMG_WINEVENTS)
{
  BOOL bRes = FALSE;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd) && (message <= (WM_APP + MAX_EVENTS))) {
    BOOL bInit = FALSE;
    const char *pszProp = hb_parldef(5, true) ? "ONCE" : "ON";
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pW);
#else
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif
    size_t nPos;

    if (events == nullptr) {
      events = (WINEVENTSHOLDER *)hb_xgrabz(sizeof(WINEVENTSHOLDER));
      events->hwnd = hWnd;
      events->active = hb_parldef(4, true);
      events->count = (size_t)sizeof(events->events) / sizeof(WINEVENT);

      HB_ATOM_SET(&events->used, 0);

      bInit = TRUE;
    }

    nPos = WinEventScan(events, message); // arleady exists ?

    if (nPos > 0) {
      hb_itemRelease(events->events[nPos - 1].bAction);
    } else {
      nPos = bInit ? 1 : WinEventScan(events, 0);
      if (nPos > 0) {
        HB_ATOM_INC(&events->used);
      }
    }

    if (nPos > 0) {
      events->events[nPos - 1].message = message;
      events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
      events->events[nPos - 1].active = hb_parldef(4, true);

      bRes = TRUE;
    }

    if (bInit) {
#ifdef UNICODE
      bRes = SetProp(hWnd, pW, static_cast<HANDLE>(events)) ? true : false;
#else
      bRes = SetProp(hWnd, pszProp, static_cast<HANDLE>(events)) ? true : false;
#endif
    }

#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_retl(bRes ? true : false);
}

HB_FUNC(HMG_WINEVENTSREMOVE)
{
  auto bDel = false;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd)) {
    const char *pszProp = hb_parldef(3, true) ? "ONCE" : "ON";

    bDel = WinEventRemove(hWnd, pszProp, message);
  }

  hb_retl(bDel);
}

HB_FUNC(HMG_WINEVENTSUPDATE)
{
  auto bUpd = false;
  auto hWnd = hmg_par_HWND(1);
  auto message = static_cast<UINT>(hb_parns(2));

  if (IsWindow(hWnd)) {
    const char *pszProp = hb_parldef(5, true) ? "ONCE" : "ON";
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pW);
#else
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      if (message <= (WM_APP + MAX_EVENTS)) {
        size_t nPos = WinEventScan(events, message); // arleady exists ?

        if (nPos > 0) {
          if (HB_IS_BLOCK(hb_param(3, Harbour::Item::ANY))) {
            hb_itemRelease(events->events[nPos - 1].bAction);
            events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
          }

          events->events[nPos - 1].active = hb_parldef(4, true);

          bUpd = true;
        }
      } else if (message == 0) {
        events->active = hb_parldef(4, events->active);

        bUpd = true;
      }
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_retl(bUpd);
}

HB_FUNC(HMG_ENUMWINEVENTS)
{
  auto hWnd = hmg_par_HWND(1);
  const char *pszProp = hb_parldef(2, true) ? "ONCE" : "ON";
  auto aEvents = hb_itemArrayNew(0);

  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pW);
#else
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    if (events != nullptr) {
      for (size_t i = 0; i < events->count; i++) {
        auto aEvent = hb_itemArrayNew(3);

        hb_arraySetNInt(aEvent, 1, events->events[i].message);
        hb_arraySetL(aEvent, 2, events->events[i].active);

        if (events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction)) {
          hb_arraySet(aEvent, 3, hb_itemClone(events->events[i].bAction));
        } else {
          hb_arraySet(aEvent, 3, nullptr);
        }

        hb_arrayAddForward(aEvents, aEvent);

        hb_itemRelease(aEvent);
      }
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  }

  hb_itemReturnRelease(aEvents);
}

HB_FUNC(HMG_GETWINEVENTSINFO)
{
  auto hWnd = hmg_par_HWND(1);
  const char *pszProp = hb_parldef(2, true) ? "ONCE" : "ON";
  PHB_ITEM aInfo;

  if (IsWindow(hWnd)) {
#ifdef UNICODE
    LPWSTR pW = AnsiToWide(pszProp);
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pW);
#else
    WINEVENTSHOLDER *events = (WINEVENTSHOLDER *)GetProp(hWnd, pszProp);
#endif

    aInfo = hb_itemArrayNew((events != nullptr) ? 4 : 0);

    if (events != nullptr) {
      hb_arraySetNInt(aInfo, 1, reinterpret_cast<LONG_PTR>(events->hwnd));
      hb_arraySetNS(aInfo, 2, events->count);
      hb_arraySetNS(aInfo, 3, events->used);
      hb_arraySetL(aInfo, 4, events->active);
    }
#ifdef UNICODE
    hb_xfree(pW);
#endif
  } else {
    aInfo = hb_itemArrayNew(0);
  }

  hb_itemReturnRelease(aInfo);
}

LRESULT CALLBACK MsgOnlyWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LONG_PTR lpUserData;

  switch (message)
  {
  case WM_CREATE: {
    PMYUSERDATA pUserData = (PMYUSERDATA)(((LPCREATESTRUCT)lParam)->lpCreateParams);
    if (pUserData) {
      SetLastError(0);

      SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pUserData));

      if (GetLastError() != 0) {
        return -1;
      } else {
        SetWindowPos(hWnd, 0, 0, 0, 0, 0, SWP_NOZORDER | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
      }
    }
    break;
  }
  case WM_NCDESTROY: {
    lpUserData = SetWindowLongPtr(hWnd, GWLP_USERDATA, static_cast<LONG_PTR>(0));
    if (lpUserData) {
      PMYUSERDATA pUserData = (PMYUSERDATA)lpUserData;

      if (pUserData->cbSize == sizeof(MYUSERDATA)) {
        hb_xfree(pUserData);
      }
    }
    break;
  }
  case WM_DESTROY: {
    WinEventRemove(hWnd, "ONCE", 0);
    WinEventRemove(hWnd, "ON", 0);
  }
  }

  LRESULT result = WinEventOn(hWnd, message, wParam, lParam);
  lpUserData = GetWindowLongPtr(hWnd, GWLP_USERDATA);

  if (lpUserData) {
    PMYUSERDATA pUserData = (PMYUSERDATA)lpUserData;
    PHB_DYNS pListenerDyns = pUserData->myParam.Listener;
    PHB_SYMB pListenerSymb = hb_dynsymSymbol(pListenerDyns);

    if (pListenerSymb) {
      if (hb_vmRequestReenter()) {
        hb_vmPushSymbol(pListenerSymb);
        hb_vmPushNil();
        hmg_vmPushHWND(hWnd);
        hmg_vmPushUINT(message);
        hmg_vmPushWPARAM(wParam);
        hmg_vmPushLPARAM(lParam);
        hb_vmDo(4);

        result = hb_parnl(-1);

        hb_vmRequestRestore();
      }
    }
  }

  return (result != 0) ? result : DefWindowProc(hWnd, message, wParam, lParam);
}

HB_FUNC(HMG_INITMESSAGEONLYWINDOW)
{
  HWND hwnd = nullptr;

  void *str;
  LPCTSTR lpClassName = HB_PARSTR(1, &str, nullptr);

  if (lpClassName) {
    WNDCLASSEX wcx{};
    wcx.cbSize = sizeof(wcx);
    wcx.lpfnWndProc = MsgOnlyWndProc;
    wcx.cbClsExtra = 0; // no extra class memory
    wcx.cbWndExtra = 0; // no extra window memory
    wcx.hInstance = GetInstance();
    wcx.lpszClassName = lpClassName;

    if (RegisterClassEx(&wcx)) {
      auto pszFuncName = hb_parc(2);

      if (pszFuncName && hb_dynsymIsFunction(hb_dynsymGet(pszFuncName))) {
        auto pUserData = static_cast<PMYUSERDATA>(hb_xgrabz(sizeof(MYUSERDATA)));

        pUserData->cbSize = sizeof(MYUSERDATA);
        pUserData->myParam.Listener = hb_dynsymGet(pszFuncName);

        hwnd = CreateWindowEx(0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(),
                              static_cast<LPVOID>(pUserData));
      } else {
        hwnd = CreateWindowEx(0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(), 0);
      }
    } else {
      hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
    }
  }

  hb_strfree(str);
  hmg_ret_HWND(hwnd);
}

// Modified by P.Ch. 17.06.
HB_FUNC(HMG_INITDUMMY)
{
  hmg_ret_HWND(
      CreateWindowEx(0, WC_STATIC, TEXT(""), WS_CHILD, 0, 0, 0, 0, hmg_par_HWND(1), nullptr, GetInstance(), nullptr));
}

// Modified by P.Ch. 17.06.
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT r = 0;
  PHB_SYMB g_ListenerSymb = hb_dynsymSymbol(g_ListenerDyns);

  if (message == WM_DESTROY) {
    AppEventRemove(hWnd, "ONCE", 0);
    AppEventRemove(hWnd, "ON", 0);

    if (IsWindow(g_hWndMain) && hWnd == g_hWndMain && g_hAccel != nullptr) {
      if (DestroyAcceleratorTable(g_hAccel)) {
        g_hAccel = nullptr;
      }
    }
  }

  if (message >= WM_APP && message <= (WM_APP + MAX_EVENTS)) {
    r = AppEventOn(hWnd, message, wParam, lParam);
  } else if (g_ListenerSymb) {
    if (hb_vmRequestReenter()) {
      hb_vmPushSymbol(g_ListenerSymb);
      hb_vmPushNil();
      hmg_vmPushHWND(hWnd);
      hmg_vmPushUINT(message);
      hmg_vmPushWPARAM(wParam);
      hmg_vmPushLPARAM(lParam);
      hb_vmDo(4);

      r = (LRESULT)hb_parnl(-1);
      hb_vmRequestRestore();
    }
  }

  return (r != 0) ? r : DefWindowProc(hWnd, message, wParam, lParam);
}

HB_FUNC(HMG_INITWINDOW)
{
  DWORD style = WS_POPUP;
  DWORD ExStyle;

  if (hb_parl(16)) {
    ExStyle = WS_EX_CONTEXTHELP;
  } else {
    ExStyle = 0;
    if (!hb_parl(6)) {
      style |= WS_MINIMIZEBOX;
    }

    if (!hb_parl(7)) {
      style |= WS_MAXIMIZEBOX;
    }
  }

  if (!hb_parl(8)) {
    style |= WS_SIZEBOX;
  }

  if (!hb_parl(9)) {
    style |= WS_SYSMENU;
  }

  if (!hb_parl(10)) {
    style |= WS_CAPTION;
  }

  if (hb_parl(11)) {
    ExStyle |= WS_EX_TOPMOST;
  }

  if (hb_parl(14)) {
    style |= WS_VSCROLL;
  }

  if (hb_parl(15)) {
    style |= WS_HSCROLL;
  }

  if (hb_parl(17)) {
    ExStyle |= WS_EX_PALETTEWINDOW;
  }

  if (hb_parl(18)) {
    // Panel
    style = WS_CHILD;
    ExStyle |= WS_EX_CONTROLPARENT | WS_EX_STATICEDGE;
  }

  void *str1;
  void *str2;

  auto hwnd = CreateWindowEx(ExStyle, HB_PARSTR(12, &str1, nullptr), HB_PARSTR(1, &str2, nullptr), style, hb_parni(2),
                             hb_parni(3), hb_parni(4), hb_parni(5), hmg_par_HWND(13), nullptr, GetInstance(), nullptr);

  hb_strfree(str1);
  hb_strfree(str2);

  if (hwnd != nullptr) {
    hmg_ret_HWND(hwnd);
  } else {
    MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
  }
}

HB_FUNC(HMG_INITMODALWINDOW)
{
  DWORD ExStyle = 0;

  if (hb_parl(13)) {
    ExStyle = WS_EX_CONTEXTHELP;
  }

  DWORD style = WS_POPUP;

  if (!hb_parl(7)) {
    style |= WS_SIZEBOX;
  }

  if (!hb_parl(8)) {
    style |= WS_SYSMENU;
  }

  if (!hb_parl(9)) {
    style |= WS_CAPTION;
  }

  if (hb_parl(11)) {
    style |= WS_VSCROLL;
  }

  if (hb_parl(12)) {
    style |= WS_HSCROLL;
  }

  void *str1;
  void *str2;

  auto hwnd = CreateWindowEx(ExStyle, HB_PARSTR(10, &str1, nullptr), HB_PARSTR(1, &str2, nullptr), style, hb_parni(2),
                             hb_parni(3), hb_parni(4), hb_parni(5), hmg_par_HWND(6), nullptr, GetInstance(), nullptr);

  hb_strfree(str1);
  hb_strfree(str2);

  if (hwnd != nullptr) {
    hmg_ret_HWND(hwnd);
  } else {
    MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
  }
}

HB_FUNC(HMG_INITSPLITCHILDWINDOW)
{
  DWORD style = WS_POPUP;

  if (!hb_parl(4)) {
    style |= WS_CAPTION;
  }

  if (hb_parl(7)) {
    style |= WS_VSCROLL;
  }

  if (hb_parl(8)) {
    style |= WS_HSCROLL;
  }

  void *str1;
  void *str2;

  auto hwnd =
      CreateWindowEx(WS_EX_STATICEDGE | WS_EX_TOOLWINDOW, HB_PARSTR(3, &str1, nullptr), HB_PARSTR(5, &str2, nullptr),
                     style, 0, 0, hb_parni(1), hb_parni(2), 0, nullptr, GetInstance(), nullptr);

  hb_strfree(str1);
  hb_strfree(str2);

  if (hwnd != nullptr) {
    hmg_ret_HWND(hwnd);
  } else {
    MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
  }
}

HB_FUNC(HMG_INITSPLITBOX)
{
  INITCOMMONCONTROLSEX icex{};
  icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icex.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES;
  InitCommonControlsEx(&icex);

  DWORD style =
      WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | RBS_BANDBORDERS | RBS_VARHEIGHT | RBS_FIXEDORDER;

  if (hb_parl(2)) {
    style |= CCS_BOTTOM;
  }

  if (hb_parl(3)) {
    style |= CCS_VERT;
  }

  auto hwndRB = CreateWindowEx(WS_EX_TOOLWINDOW | WS_EX_DLGMODALFRAME, REBARCLASSNAME, nullptr, style, 0, 0, 0, 0,
                               hmg_par_HWND(1), nullptr, GetInstance(), nullptr);

  // Initialize and send the REBARINFO structure.
  REBARINFO rbi;
  rbi.cbSize = sizeof(REBARINFO); // Required when using this struct.
  rbi.fMask = 0;
  rbi.himl = nullptr;
  SendMessage(hwndRB, RB_SETBARINFO, 0, reinterpret_cast<LPARAM>(&rbi));

  hmg_ret_HWND(hwndRB);
}

// Modified by P.Ch. 16.10.-16.12.,17.06.
HB_FUNC(HMG_REGISTERWINDOW)
{
  void *str1 = nullptr;
  LPCTSTR lpIconName = HB_ISCHAR(1) ? HB_PARSTR(1, &str1, nullptr)
                                    : (HB_ISNUM(1) ? MAKEINTRESOURCE(static_cast<WORD>(hb_parnl(1))) : nullptr);
  void *str2;
  LPCTSTR lpClassName = HB_PARSTR(2, &str2, nullptr);
  void *str3 = nullptr;
  LPCTSTR lpCursorName = HB_ISCHAR(4) ? HB_PARSTR(4, &str3, nullptr)
                                      : (HB_ISNUM(4) ? MAKEINTRESOURCE(static_cast<WORD>(hb_parnl(4))) : nullptr);

  WNDCLASS WndClass{};
  WndClass.style = CS_DBLCLKS | /*CS_HREDRAW | CS_VREDRAW |*/ CS_OWNDC;
  WndClass.lpfnWndProc = WndProc;
  WndClass.cbClsExtra = 0;
  WndClass.cbWndExtra = 0;
  WndClass.hInstance = GetInstance();

  // icon from resource
  auto hIcon = LoadIcon(GetResources(), lpIconName);
  // from file
  if (hIcon == nullptr && HB_ISCHAR(1)) {
    hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
  }
  WndClass.hIcon = ((hIcon != nullptr) ? hIcon : LoadIcon(nullptr, IDI_APPLICATION));

  // cursor from resource
  auto hCursor = LoadCursor(GetResources(), lpCursorName);
  // from file
  if ((hCursor == nullptr) && HB_ISCHAR(4)) {
    hCursor = LoadCursorFromFile(lpCursorName);
  }
  WndClass.hCursor = ((hCursor != nullptr) ? hCursor : LoadCursor(nullptr, IDC_ARROW));

  HBRUSH hBrush = nullptr;

  if (HB_ISARRAY(3)) {
    // old behavior (before 16.10)
    if (HB_PARNI(3, 1) == -1) {
      hBrush = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1);
    } else {
      hBrush = CreateSolidBrush(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3)));
    }
  } else if (HB_ISCHAR(3) || HB_ISNUM(3)) {
    void *str = nullptr;
    LPCTSTR lpImageName = HB_ISCHAR(3) ? HB_PARSTR(3, &str, nullptr)
                                       : (HB_ISNUM(3) ? MAKEINTRESOURCE(static_cast<WORD>(hb_parnl(3))) : nullptr);
    auto hImage = static_cast<HBITMAP>(
        LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
    if (hImage == nullptr && HB_ISCHAR(3)) {
      hImage = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0,
                                              LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
    }
    if (hImage == nullptr) {
      hImage = HMG_LoadImage(hb_parc(3), nullptr);
    }
    if (hImage != nullptr) {
      hBrush = CreatePatternBrush(hImage);
    }
    hb_strfree(str);
  }

  WndClass.hbrBackground = (hBrush != nullptr) ? hBrush : (hBrush = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1));
  WndClass.lpszMenuName = nullptr;
  WndClass.lpszClassName = lpClassName;

  if (!RegisterClass(&WndClass)) {
    hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);

  hmg_ret_HBRUSH(hBrush);
}

// Modified by P.Ch. 17.06.
HB_FUNC(HMG_REGISTERSPLITCHILDWINDOW)
{
  void *str1 = nullptr;
  LPCTSTR lpIcon = HB_ISCHAR(1) ? HB_PARSTR(1, &str1, nullptr)
                                : (HB_ISNIL(1) ? nullptr : MAKEINTRESOURCE(static_cast<WORD>(hb_parnl(1))));
  void *str2;
  LPCTSTR lpClassName = HB_PARSTR(2, &str2, nullptr);

  WNDCLASS WndClass{};
  WndClass.style = CS_OWNDC;
  WndClass.lpfnWndProc = WndProc;
  WndClass.cbClsExtra = 0;
  WndClass.cbWndExtra = 0;
  WndClass.hInstance = GetInstance();
  WndClass.hIcon = LoadIcon(GetInstance(), lpIcon);
  if (WndClass.hIcon == nullptr) {
    WndClass.hIcon = static_cast<HICON>(LoadImage(0, lpIcon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
  }

  if (WndClass.hIcon == nullptr) {
    WndClass.hIcon = LoadIcon(nullptr, IDI_APPLICATION);
  }

  WndClass.hCursor = LoadCursor(nullptr, IDC_ARROW);

  HBRUSH hbrush = nullptr;

  if (HB_PARNI(3, 1) == -1) {
    WndClass.hbrBackground = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1);
  } else {
    hbrush = CreateSolidBrush(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3)));
    WndClass.hbrBackground = hbrush;
  }

  WndClass.lpszMenuName = nullptr;
  WndClass.lpszClassName = lpClassName;

  if (!RegisterClass(&WndClass)) {
    hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
  }

  hb_strfree(str1);
  hb_strfree(str2);

  hmg_ret_HBRUSH(hbrush);
}

// Modified by P.Ch. 17.06.
HB_FUNC(HMG_UNREGISTERWINDOW)
{
  void *str;
  UnregisterClass(HB_PARSTR(1, &str, nullptr), GetInstance());
  hb_strfree(str);
}

HB_FUNC(HMG_MSC_VER)
{
#if defined(_MSC_VER)
  hb_retnl(_MSC_VER);
#else
  hb_retnl(0);
#endif
}

#define COMPILER_BUF_SIZE 80

HB_FUNC(HMG_BORLANDC)
{
#ifdef __BORLANDC__

  const char *pszName;
  char szSub[64];

  int iVerMajor;
  int iVerMinor;
  int iVerPatch;

  auto pszCompiler = static_cast<char *>(hb_xgrab(COMPILER_BUF_SIZE));
  szSub[0] = '\0';

#if (__BORLANDC__ >= 0x0590) // Version 5.9
#if (__BORLANDC__ >= 0x0620) // Version 6.2
  pszName = "Embarcadero C++";
#else
  pszName = "CodeGear C++";
#endif
#else
  pszName = "Borland C++";
#endif

#if (__BORLANDC__ >= 0x0500) // Version 5.x
  iVerMajor = __BORLANDC__ >> 8;
  iVerMinor = (__BORLANDC__ & 0xFF) >> 4;
  iVerPatch = __BORLANDC__ & 0xF;
#else // Version 4.x
  iVerMajor = __BORLANDC__ >> 8;
  iVerMinor = (__BORLANDC__ - 1 & 0xFF) >> 4;
  iVerPatch = 0;
#endif

  if (pszName) {
    if (iVerPatch != 0) {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch);
    } else if (iVerMajor != 0 || iVerMinor != 0) {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d0", pszName, szSub, iVerMajor, iVerMinor);
    } else {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub);
    }
  } else {
    hb_strncpy(pszCompiler, "(unknown)", COMPILER_BUF_SIZE - 1);
  }

#if defined(HB_ARCH_32BIT)
  hb_strncat(pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1);
#elif defined(HB_ARCH_64BIT)
  hb_strncat(pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1);
#endif

#else
  auto pszCompiler = static_cast<char *>(hb_xgrab(COMPILER_BUF_SIZE));
  strcpy(pszCompiler, "");
#endif // __BORLANDC__

  hb_retc_buffer(pszCompiler);
}

#include "mgver.h"

HB_FUNC(HMG_VERSION)
{
  auto pszVersion = static_cast<char *>(hb_xgrab(40));
  hb_snprintf(pszVersion, 40, "Harbour MiniGUI %d.%d.%d (%s)", MG_VER_MAJOR, MG_VER_MINOR, MG_VER_RELEASE,
              MG_VER_STATUS);
  hb_retc_buffer(pszVersion);
}

HB_FUNC(HMG_ISALPHA)
{
  void *str;
  LPCTSTR ch = HB_PARSTR(1, &str, nullptr);
  hb_retl(IsCharAlpha(ch[0]));
  hb_strfree(str);
}

HB_FUNC(HMG_ISDIGIT)
{
  void *str;
  LPCTSTR ch = HB_PARSTR(1, &str, nullptr);
  hb_retl((IsCharAlphaNumeric(ch[0]) && !IsCharAlpha(ch[0])));
  hb_strfree(str);
}

#ifdef UNICODE

HB_FUNC(HMG_LOWER)
{
  LPSTR pStr;
  TCHAR *Text = (TCHAR *)AnsiToWide(const_cast<char *>(hb_parc(1)));
  INT nLen;

  if (Text == nullptr) {
    hb_retc(nullptr);
    return;
  }

  nLen = lstrlen(Text) + 1;
  auto Buffer = static_cast<TCHAR *>(hb_xgrab(nLen * sizeof(TCHAR)));

  if (Buffer != nullptr) {
    lstrcpy(Buffer, Text);
    CharLower(Buffer);
    pStr = WideToAnsi(Buffer);
    hb_retc(pStr);
    hb_xfree(pStr);
  } else {
    hb_retc(nullptr);
  }

  hb_xfree(Text);
  hb_xfree(Buffer);
}

HB_FUNC(HMG_UPPER)
{
  LPSTR pStr;
  TCHAR *Text = (TCHAR *)AnsiToWide(const_cast<char *>(hb_parc(1)));
  INT nLen;

  if (Text == nullptr) {
    hb_retc(nullptr);
    return;
  }

  nLen = lstrlen(Text) + 1;
  auto Buffer = static_cast<TCHAR *>(hb_xgrab(nLen * sizeof(TCHAR)));

  if (Buffer != nullptr) {
    lstrcpy(Buffer, Text);
    CharUpper(Buffer);
    pStr = WideToAnsi(Buffer);
    hb_retc(pStr);
    hb_xfree(pStr);
  } else {
    hb_retc(nullptr);
  }

  hb_xfree(Text);
  hb_xfree(Buffer);
}

HB_FUNC(HMG_ISLOWER)
{
  void *str;
  LPCTSTR Text = HB_PARSTR(1, &str, nullptr);
  hb_retl(IsCharLower(Text[0]));
  hb_strfree(str);
}

HB_FUNC(HMG_ISUPPER)
{
  void *str;
  LPCTSTR Text = HB_PARSTR(1, &str, nullptr);
  hb_retl(IsCharUpper(Text[0]));
  hb_strfree(str);
}

#else

HB_FUNC_TRANSLATE(HMG_LOWER, LOWER)
HB_FUNC_TRANSLATE(HMG_UPPER, UPPER)
HB_FUNC_TRANSLATE(HMG_ISLOWER, ISLOWER)
HB_FUNC_TRANSLATE(HMG_ISUPPER, ISUPPER)

#endif // UNICODE

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(GETGLOBALLISTENER, HMG_GETGLOBALLISTENER)
HB_FUNC_TRANSLATE(SETGLOBALLISTENER, HMG_SETGLOBALLISTENER)
HB_FUNC_TRANSLATE(RESETGLOBALLISTENER, HMG_RESETGLOBALLISTENER)
HB_FUNC_TRANSLATE(APPEVENTS, HMG_APPEVENTS)
HB_FUNC_TRANSLATE(APPEVENTSREMOVE, HMG_APPEVENTSREMOVE)
HB_FUNC_TRANSLATE(APPEVENTSUPDATE, HMG_APPEVENTSUPDATE)
HB_FUNC_TRANSLATE(ENUMAPPEVENTS, HMG_ENUMAPPEVENTS)
HB_FUNC_TRANSLATE(GETAPPEVENTSINFO, HMG_GETAPPEVENTSINFO)
HB_FUNC_TRANSLATE(WINEVENTS, HMG_WINEVENTS)
HB_FUNC_TRANSLATE(WINEVENTSREMOVE, HMG_WINEVENTSREMOVE)
HB_FUNC_TRANSLATE(WINEVENTSUPDATE, HMG_WINEVENTSUPDATE)
HB_FUNC_TRANSLATE(ENUMWINEVENTS, HMG_ENUMWINEVENTS)
HB_FUNC_TRANSLATE(GETWINEVENTSINFO, HMG_GETWINEVENTSINFO)
HB_FUNC_TRANSLATE(INITMESSAGEONLYWINDOW, HMG_INITMESSAGEONLYWINDOW)
HB_FUNC_TRANSLATE(INITDUMMY, HMG_INITDUMMY)
HB_FUNC_TRANSLATE(INITWINDOW, HMG_INITWINDOW)
HB_FUNC_TRANSLATE(INITMODALWINDOW, HMG_INITMODALWINDOW)
HB_FUNC_TRANSLATE(INITSPLITCHILDWINDOW, HMG_INITSPLITCHILDWINDOW)
HB_FUNC_TRANSLATE(INITSPLITBOX, HMG_INITSPLITBOX)
HB_FUNC_TRANSLATE(REGISTERWINDOW, HMG_REGISTERWINDOW)
HB_FUNC_TRANSLATE(REGISTERSPLITCHILDWINDOW, HMG_REGISTERSPLITCHILDWINDOW)
HB_FUNC_TRANSLATE(UNREGISTERWINDOW, HMG_UNREGISTERWINDOW)
HB_FUNC_TRANSLATE(MSC_VER, HMG_MSC_VER)
HB_FUNC_TRANSLATE(BORLANDC, HMG_BORLANDC)
#endif
