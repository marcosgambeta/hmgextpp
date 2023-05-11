/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
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
 *
 * Parts of this code is contributed and used here under permission of his
 * author: Copyright 2016-2017 (C) P.Chornyj <myorg63@mail.ru>
 */

#define _WIN32_IE     0x0501

#if defined( __MINGW32__ )
#define _WIN32_WINNT  0x0500
#endif /* MINGW | XCC */

#include "mgdefs.hpp"

#include <commctrl.h>
#include <hbapiitm.hpp>
#include <hbvm.hpp>

#include <hbwinuni.hpp>
#include <hbthread.hpp>

#include <hbatomic.hpp>

#ifndef WC_STATIC
#define WC_STATIC         "Static"
#endif

#define DEFAULT_LISTENER  "EVENTS"
#define MAX_EVENTS        64

// local types
typedef struct tagAppEvent
{
   UINT     message;
   PHB_ITEM bAction;
   BOOL     active;
} APPEVENT, * APPEVENT_PTR;

typedef struct tagEventsHolder
{
   HWND       hwnd;
   BOOL       active;
   size_t     count;
   HB_COUNTER used;
   APPEVENT   events[MAX_EVENTS];
} EVENTSHOLDER, * EVENTSHOLDER_PTR;

typedef struct tagMyParam
{
   PHB_DYNS Listener;
} MYPARAMS;

typedef struct tagMyUserData
{
   UINT     cbSize;
   MYPARAMS myParam;
#if defined( _WIN64 )
} MYUSERDATA, * PMYUSERDATA;
#else
} MYUSERDATA, UNALIGNED * PMYUSERDATA;
#endif /* _WIN64 */

typedef struct tagWinEvent
{
   UINT message;
   PHB_ITEM bBefore;
   PHB_ITEM bAction;
   PHB_ITEM bAfter;
   BOOL active;
} WINEVENT, * WINEVENT_PTR;

typedef struct tagWinEventsHolder
{
   HWND hwnd;
   BOOL active;
   size_t count;
   HB_COUNTER used;
   WINEVENT events[MAX_EVENTS];
} WINEVENTSHOLDER, * WINEVENTSHOLDER_PTR;

// extern functions
#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR  WideToAnsi(LPWSTR);
#endif
HINSTANCE      GetInstance(void);
HINSTANCE      GetResources(void);
extern void    hmg_ErrorExit(LPCTSTR lpMessage, DWORD dwError, BOOL bExit);

// local functions
static size_t  AppEventScan(EVENTSHOLDER * events, UINT message);
static LRESULT AppEventDo(EVENTSHOLDER * events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static LRESULT AppEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static HB_BOOL AppEventRemove(HWND hWnd, const char * pszName, UINT message);
static size_t  WinEventScan(WINEVENTSHOLDER * events, UINT message);
static LRESULT WinEventDo(WINEVENTSHOLDER * events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static LRESULT WinEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static HB_BOOL WinEventRemove(HWND hWnd, const char * pszName, UINT message);

LRESULT CALLBACK MsgOnlyWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

// extern variables
extern HWND g_hWndMain;
extern HACCEL g_hAccel;

// static variables
static PHB_DYNS g_ListenerDyns = nullptr;

static HB_CRITICAL_NEW(s_lst_mtx);
#define HMG_LISTENER_LOCK    hb_threadEnterCriticalSection(&s_lst_mtx)
#define HMG_LISTENER_UNLOCK  hb_threadLeaveCriticalSection(&s_lst_mtx)

HB_FUNC( GETGLOBALLISTENER )
{
   if( g_ListenerDyns != nullptr )
   {
      hb_retc( hb_dynsymName(g_ListenerDyns) );
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( SETGLOBALLISTENER )
{
   const char * pszNewName = hb_parc(1);

   if( pszNewName && hb_dynsymIsFunction(hb_dynsymGet(pszNewName)) )
   {
      HMG_LISTENER_LOCK;
      g_ListenerDyns = hb_dynsymGet(pszNewName); hb_retl(true);
      HMG_LISTENER_UNLOCK;
   }
   else
   {
      hb_retl(false);
   }
}

HB_FUNC( RESETGLOBALLISTENER )
{
   HMG_LISTENER_LOCK;
   g_ListenerDyns = hb_dynsymGet(DEFAULT_LISTENER);
   HMG_LISTENER_UNLOCK;
}

static size_t AppEventScan(EVENTSHOLDER * events, UINT message)
{
   size_t nPos = 0;

   for( size_t i = 0; i < events->count; i++ )
   {
      if( message == events->events[i].message )
      {
         nPos = ( i + 1 ); break;
      }
   }

   return nPos;
}

static HB_BOOL AppEventRemove(HWND hWnd, const char * pszProp, UINT message)
{
   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         if( message != 0 )
         {
            size_t nPos = AppEventScan(events, message);

            if( nPos > 0 )                                           // if found
            {
               hb_itemRelease(events->events[nPos - 1].bAction); // delete old codeblock

               events->events[nPos - 1].message = 0;
               events->events[nPos - 1].bAction = nullptr;
               events->events[nPos - 1].active = FALSE;

               HB_ATOM_DEC( &events->used );
            }
         }
         else
         {
            for( size_t i = 0; i < events->count; i++ ) // delete all not empty items with codeblocks
            {
               if( events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction) )
               {
                  hb_itemRelease(events->events[i].bAction);
               }
            }

            HB_ATOM_SET(&events->used, 0);
         }

         if( !HB_ATOM_GET(&events->used) )
         {
   #ifdef UNICODE
            events = ( EVENTSHOLDER * ) RemoveProp(hWnd, pW);
   #else
            events = ( EVENTSHOLDER * ) RemoveProp(hWnd, pszProp);
   #endif

            hb_xfree(events); // delete events holder
         }

   #ifdef UNICODE
         hb_xfree(pW);
   #endif
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static LRESULT AppEventDo(EVENTSHOLDER * events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   size_t nPos = AppEventScan(events, message);

   if( ( nPos > 0 ) && events->active && ( events->events[nPos - 1].active &&
                                           ( ( events->events[nPos - 1].bAction != nullptr ) && HB_IS_BLOCK(events->events[nPos - 1].bAction) ) ) )
   {
      PHB_ITEM phWnd = hb_itemPutNInt(nullptr, ( LONG_PTR ) hWnd);
      PHB_ITEM pmessage = hb_itemPutNS(nullptr, message);
      PHB_ITEM pwParam = hb_itemPutNInt(nullptr, ( LONG_PTR ) wParam);
      PHB_ITEM plParam = hb_itemPutNInt(nullptr, ( LONG_PTR ) lParam);

      hb_evalBlock(events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, nullptr);

      hb_itemRelease(phWnd);
      hb_itemRelease(pmessage);
      hb_itemRelease(pwParam);
      hb_itemRelease(plParam);

      if( HB_TRUE == bOnce )
      {
         AppEventRemove(hWnd, "ONCE", message);
      }

      return ( LRESULT ) hb_parnl( -1 );
   }

   return ( LRESULT ) 0;
}

static LRESULT AppEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   LRESULT r = 0;

   if( IsWindow(hWnd) )
   {
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, TEXT("ONCE"));

      if( events != nullptr )
      {
         if( hWnd == events->hwnd )
         {
            r = AppEventDo(events, HB_TRUE, hWnd, message, wParam, lParam);
         }
      }

      events = ( EVENTSHOLDER * ) GetProp(hWnd, TEXT("ON"));

      if( events != nullptr )
      {
         if( hWnd == events->hwnd )
         {
            r = AppEventDo(events, HB_FALSE, hWnd, message, wParam, lParam);
         }
      }
   }

   return r;
}

HB_FUNC( APPEVENTS )
{
   BOOL bRes = FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) && ( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) ) )
   {
      BOOL bInit = FALSE;
      const char *   pszProp = hb_parldef(5, HB_TRUE) ? "ONCE" : "ON";
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif
      size_t nPos;

      if( events == nullptr )
      {
         events = ( EVENTSHOLDER * ) hb_xgrabz(sizeof(EVENTSHOLDER));
         events->hwnd = hWnd;
         events->active = hb_parldef(4, HB_TRUE);
         events->count = ( size_t ) sizeof(events->events) / sizeof(APPEVENT);

         HB_ATOM_SET(&events->used, 0);

         bInit = TRUE;
      }

      nPos = AppEventScan(events, message); // arleady exists ?

      if( nPos > 0 )
      {
         hb_itemRelease(events->events[nPos - 1].bAction);
      }
      else
      {
         nPos = bInit ? 1 : AppEventScan(events, 0);
         if( nPos > 0 )
         {
            HB_ATOM_INC( &events->used );
         }
      }

      if( nPos > 0 )
      {
         events->events[nPos - 1].message = message;
         events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
         events->events[nPos - 1].active = hb_parldef(4, HB_TRUE);

         bRes = TRUE;
      }

      if( bInit )
      {
   #ifdef UNICODE
         bRes = SetProp(hWnd, pW, ( HANDLE ) events) ? HB_TRUE : HB_FALSE;
   #else
         bRes = SetProp(hWnd, pszProp, ( HANDLE ) events) ? HB_TRUE : HB_FALSE;
   #endif
      }

   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_retl(bRes ? HB_TRUE : HB_FALSE);
}

HB_FUNC( APPEVENTSREMOVE )
{
   HB_BOOL bDel = HB_FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) )
   {
      const char * pszProp = hb_parldef(3, HB_TRUE) ? "ONCE" : "ON";

      bDel = AppEventRemove(hWnd, pszProp, message);
   }

   hb_retl(bDel);
}

HB_FUNC( APPEVENTSUPDATE )
{
   HB_BOOL bUpd = HB_FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) )
   {
      const char *   pszProp = hb_parldef(5, HB_TRUE) ? "ONCE" : "ON";
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         if( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) )
         {
            size_t nPos = AppEventScan(events, message); // arleady exists ?

            if( nPos > 0 )
            {
               if( HB_IS_BLOCK(hb_param(3, Harbour::Item::ANY)) )
               {
                  hb_itemRelease(events->events[nPos - 1].bAction);
                  events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
               }

               events->events[nPos - 1].active = hb_parldef(4, HB_TRUE);

               bUpd = HB_TRUE;
            }
         }
         else if( message == 0 )
         {
            events->active = hb_parldef(4, events->active);

            bUpd = HB_TRUE;
         }
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_retl(bUpd);
}

HB_FUNC( ENUMAPPEVENTS )
{
   HWND hWnd = hmg_par_HWND(1);
   const char * pszProp = hb_parldef(2, HB_TRUE) ? "ONCE" : "ON";
   PHB_ITEM aEvents = hb_itemArrayNew(0);

   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         for( size_t i = 0; i < events->count; i++ )
         {
            PHB_ITEM aEvent = hb_itemArrayNew(3);

            hb_arraySetNInt(aEvent, 1, events->events[i].message);
            hb_arraySetL( aEvent, 2, events->events[i].active );

            if( events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction) )
            {
               hb_arraySet(aEvent, 3, hb_itemClone(events->events[i].bAction));
            }
            else
            {
               hb_arraySet(aEvent, 3, nullptr);
            }

            hb_arrayAddForward( aEvents, aEvent );

            hb_itemRelease(aEvent);
         }
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_itemReturnRelease(aEvents);
}

HB_FUNC( GETAPPEVENTSINFO )
{
   HWND hWnd = hmg_par_HWND(1);
   const char * pszProp = hb_parldef(2, HB_TRUE) ? "ONCE" : "ON";
   PHB_ITEM aInfo;

   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      EVENTSHOLDER * events = ( EVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      aInfo = hb_itemArrayNew((events != nullptr) ? 4 : 0);

      if( events != nullptr )
      {
         hb_arraySetNInt(aInfo, 1, ( LONG_PTR ) events->hwnd);
         hb_arraySetNS(aInfo, 2, events->count);
         hb_arraySetNS(aInfo, 3, ( HB_ISIZ ) HB_ATOM_GET(&events->used));
         hb_arraySetL( aInfo, 4, events->active );
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }
   else
   {
      aInfo = hb_itemArrayNew(0);
   }

   hb_itemReturnRelease(aInfo);
}

static size_t WinEventScan(WINEVENTSHOLDER * events, UINT message)
{
   size_t nPos = 0;

   for( size_t i = 0; i < events->count; i++ )
   {
      if( message == events->events[i].message )
      {
         nPos = ( i + 1 ); break;
      }
   }

   return nPos;
}

static HB_BOOL WinEventRemove(HWND hWnd, const char * pszProp, UINT message)
{
   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         if( message != 0 )
         {
            size_t nPos = WinEventScan(events, message);

            if( nPos > 0 )                                           // if found
            {
               hb_itemRelease(events->events[nPos - 1].bAction); // delete old codeblock

               events->events[nPos - 1].message = 0;
               events->events[nPos - 1].bAction = nullptr;
               events->events[nPos - 1].active = FALSE;

               HB_ATOM_DEC( &events->used );
            }
         }
         else
         {
            for( size_t i = 0; i < events->count; i++ ) // delete all not empty items with codeblocks
            {
               if( events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction) )
               {
                  hb_itemRelease(events->events[i].bAction);
               }
            }

            HB_ATOM_SET(&events->used, 0);
         }

         if( !HB_ATOM_GET(&events->used) )
         {
   #ifdef UNICODE
            events = ( WINEVENTSHOLDER * ) RemoveProp(hWnd, pW);
   #else
            events = ( WINEVENTSHOLDER * ) RemoveProp(hWnd, pszProp);
   #endif

            hb_xfree(events); // delete events holder
         }

   #ifdef UNICODE
         hb_xfree(pW);
   #endif
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static LRESULT WinEventDo(WINEVENTSHOLDER * events, HB_BOOL bOnce, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   size_t nPos = WinEventScan(events, message);

   if( ( nPos > 0 ) && events->active && ( events->events[nPos - 1].active &&
                                           ( ( events->events[nPos - 1].bAction != nullptr ) && HB_IS_BLOCK(events->events[nPos - 1].bAction) ) ) )
   {
      PHB_ITEM phWnd = hb_itemPutNInt(nullptr, ( LONG_PTR ) hWnd);
      PHB_ITEM pmessage = hb_itemPutNS(nullptr, message);
      PHB_ITEM pwParam = hb_itemPutNInt(nullptr, ( LONG_PTR ) wParam);
      PHB_ITEM plParam = hb_itemPutNInt(nullptr, ( LONG_PTR ) lParam);

      hb_evalBlock(events->events[nPos - 1].bAction, phWnd, pmessage, pwParam, plParam, nullptr);

      hb_itemRelease(phWnd);
      hb_itemRelease(pmessage);
      hb_itemRelease(pwParam);
      hb_itemRelease(plParam);

      if( HB_TRUE == bOnce )
      {
         WinEventRemove(hWnd, "ONCE", message);
      }

      return ( LRESULT ) hb_parnl( -1 );
   }

   return ( LRESULT ) 0;
}

static LRESULT WinEventOn(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   LRESULT r = 0;

   if( IsWindow(hWnd) )
   {
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, TEXT("ONCE"));

      if( events != nullptr )
      {
         if( hWnd == events->hwnd )
         {
            r = WinEventDo(events, HB_TRUE, hWnd, message, wParam, lParam);
         }
      }

      events = ( WINEVENTSHOLDER * ) GetProp(hWnd, TEXT("ON"));

      if( events != nullptr )
      {
         if( hWnd == events->hwnd )
         {
            r = WinEventDo(events, HB_FALSE, hWnd, message, wParam, lParam);
         }
      }
   }

   return r;
}

HB_FUNC( WINEVENTS )
{
   BOOL bRes = FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) && ( message <= ( WM_APP + MAX_EVENTS ) ) )
   {
      BOOL bInit = FALSE;
      const char *      pszProp = hb_parldef(5, HB_TRUE) ? "ONCE" : "ON";
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif
      size_t nPos;

      if( events == nullptr )
      {
         events = ( WINEVENTSHOLDER * ) hb_xgrabz(sizeof(WINEVENTSHOLDER));
         events->hwnd = hWnd;
         events->active = hb_parldef(4, HB_TRUE);
         events->count = ( size_t ) sizeof(events->events) / sizeof(WINEVENT);

         HB_ATOM_SET(&events->used, 0);

         bInit = TRUE;
      }

      nPos = WinEventScan(events, message); // arleady exists ?

      if( nPos > 0 )
      {
         hb_itemRelease(events->events[nPos - 1].bAction);
      }
      else
      {
         nPos = bInit ? 1 : WinEventScan(events, 0);
         if( nPos > 0 )
         {
            HB_ATOM_INC( &events->used );
         }
      }

      if( nPos > 0 )
      {
         events->events[nPos - 1].message = message;
         events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
         events->events[nPos - 1].active = hb_parldef(4, HB_TRUE);

         bRes = TRUE;
      }

      if( bInit )
      {
   #ifdef UNICODE
         bRes = SetProp(hWnd, pW, ( HANDLE ) events) ? HB_TRUE : HB_FALSE;
   #else
         bRes = SetProp(hWnd, pszProp, ( HANDLE ) events) ? HB_TRUE : HB_FALSE;
   #endif
      }

   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_retl(bRes ? HB_TRUE : HB_FALSE);
}

HB_FUNC( WINEVENTSREMOVE )
{
   HB_BOOL bDel = HB_FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) )
   {
      const char * pszProp = hb_parldef(3, HB_TRUE) ? "ONCE" : "ON";

      bDel = WinEventRemove(hWnd, pszProp, message);
   }

   hb_retl(bDel);
}

HB_FUNC( WINEVENTSUPDATE )
{
   HB_BOOL bUpd = HB_FALSE;
   HWND hWnd = hmg_par_HWND(1);
   UINT message = ( UINT ) hb_parns(2);

   if( IsWindow(hWnd) )
   {
      const char *      pszProp = hb_parldef(5, HB_TRUE) ? "ONCE" : "ON";
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         if( message <= ( WM_APP + MAX_EVENTS ) )
         {
            size_t nPos = WinEventScan(events, message); // arleady exists ?

            if( nPos > 0 )
            {
               if( HB_IS_BLOCK(hb_param(3, Harbour::Item::ANY)) )
               {
                  hb_itemRelease(events->events[nPos - 1].bAction);
                  events->events[nPos - 1].bAction = hb_itemNew(hb_param(3, Harbour::Item::BLOCK));
               }

               events->events[nPos - 1].active = hb_parldef(4, HB_TRUE);

               bUpd = HB_TRUE;
            }
         }
         else if( message == 0 )
         {
            events->active = hb_parldef(4, events->active);

            bUpd = HB_TRUE;
         }
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_retl(bUpd);
}

HB_FUNC( ENUMWINEVENTS )
{
   HWND hWnd = hmg_par_HWND(1);
   const char * pszProp = hb_parldef(2, HB_TRUE) ? "ONCE" : "ON";
   PHB_ITEM aEvents = hb_itemArrayNew(0);

   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      if( events != nullptr )
      {
         for( size_t i = 0; i < events->count; i++ )
         {
            PHB_ITEM aEvent = hb_itemArrayNew(3);

            hb_arraySetNInt(aEvent, 1, events->events[i].message);
            hb_arraySetL( aEvent, 2, events->events[i].active );

            if( events->events[i].bAction != nullptr && HB_IS_BLOCK(events->events[i].bAction) )
            {
               hb_arraySet(aEvent, 3, hb_itemClone(events->events[i].bAction));
            }
            else
            {
               hb_arraySet(aEvent, 3, nullptr);
            }

            hb_arrayAddForward( aEvents, aEvent );

            hb_itemRelease(aEvent);
         }
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }

   hb_itemReturnRelease(aEvents);
}

HB_FUNC( GETWINEVENTSINFO )
{
   HWND hWnd = hmg_par_HWND(1);
   const char * pszProp = hb_parldef(2, HB_TRUE) ? "ONCE" : "ON";
   PHB_ITEM aInfo;

   if( IsWindow(hWnd) )
   {
   #ifdef UNICODE
      LPWSTR pW = AnsiToWide(pszProp);
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pW);
   #else
      WINEVENTSHOLDER * events = ( WINEVENTSHOLDER * ) GetProp(hWnd, pszProp);
   #endif

      aInfo = hb_itemArrayNew((events != nullptr) ? 4 : 0);

      if( events != nullptr )
      {
         hb_arraySetNInt(aInfo, 1, ( LONG_PTR ) events->hwnd);
         hb_arraySetNS(aInfo, 2, events->count);
         hb_arraySetNS(aInfo, 3, events->used);
         hb_arraySetL( aInfo, 4, events->active );
      }
   #ifdef UNICODE
      hb_xfree(pW);
   #endif
   }
   else
   {
      aInfo = hb_itemArrayNew(0);
   }

   hb_itemReturnRelease(aInfo);
}

LRESULT CALLBACK MsgOnlyWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   LONG_PTR lpUserData;
   LRESULT result;

   if( message == WM_CREATE )
   {
      PMYUSERDATA pUserData = ( PMYUSERDATA ) ( ( ( LPCREATESTRUCT ) lParam )->lpCreateParams );

      if( pUserData )
      {
         SetLastError(0);

         SetWindowLongPtr(hWnd, GWLP_USERDATA, ( LONG_PTR ) pUserData);

         if( GetLastError() != 0 )
         {
            return -1;
         }
         else
         {
            SetWindowPos(hWnd, 0, 0, 0, 0, 0, SWP_NOZORDER | SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
         }
      }
   }
   else if( message == WM_NCDESTROY )
   {
      lpUserData = SetWindowLongPtr(hWnd, GWLP_USERDATA, ( LONG_PTR ) 0);

      if( lpUserData )
      {
         PMYUSERDATA pUserData = ( PMYUSERDATA ) lpUserData;

         if( pUserData->cbSize == sizeof(MYUSERDATA) )
         {
            hb_xfree(pUserData);
         }
      }
   }
   else if( message == WM_DESTROY )
   {
      WinEventRemove(hWnd, "ONCE", 0);
      WinEventRemove(hWnd, "ON", 0);
   }

   result = WinEventOn(hWnd, message, wParam, lParam);
   lpUserData = GetWindowLongPtr(hWnd, GWLP_USERDATA);

   if( lpUserData )
   {
      PMYUSERDATA pUserData = ( PMYUSERDATA ) lpUserData;
      PHB_DYNS pListenerDyns = pUserData->myParam.Listener;
      PHB_SYMB pListenerSymb = hb_dynsymSymbol(pListenerDyns);

      if( pListenerSymb )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol(pListenerSymb);
            hb_vmPushNil();
            hmg_vmPushHandle(hWnd);
            hb_vmPushLong(message);
            hb_vmPushNumInt(wParam);
            hb_vmPushNumInt(lParam);
            hb_vmDo(4);

            result = ( LRESULT ) hb_parnl( -1 );

            hb_vmRequestRestore();
         }
      }
   }

   return ( result != 0 ) ? result : DefWindowProc(hWnd, message, wParam, lParam);
}

HB_FUNC( INITMESSAGEONLYWINDOW )
{
   HWND hwnd = nullptr;

   void *  hClassName;
   LPCTSTR lpClassName = HB_PARSTR(1, &hClassName, nullptr);

   if( lpClassName )
   {
      WNDCLASSEX wcx; memset(&wcx, 0, sizeof(WNDCLASSEX));

      wcx.cbSize = sizeof(wcx);
      wcx.lpfnWndProc = MsgOnlyWndProc;
      wcx.cbClsExtra = 0;                  // no extra class memory
      wcx.cbWndExtra = 0;                  // no extra window memory
      wcx.hInstance = GetInstance();
      wcx.lpszClassName = lpClassName;

      if( RegisterClassEx(&wcx) )
      {
         const char * pszFuncName = hb_parc(2);

         if( pszFuncName && hb_dynsymIsFunction(hb_dynsymGet(pszFuncName)) )
         {
            PMYUSERDATA pUserData = ( PMYUSERDATA ) hb_xgrabz(sizeof(MYUSERDATA));

            pUserData->cbSize = sizeof(MYUSERDATA);
            pUserData->myParam.Listener = hb_dynsymGet(pszFuncName);

            hwnd = CreateWindowEx(0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(), ( LPVOID ) pUserData);
         }
         else
         {
            hwnd = CreateWindowEx(0, lpClassName, 0, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, GetInstance(), 0);
         }
      }
      else
      {
         hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
      }
   }

   hb_strfree(hClassName);
   hmg_ret_HWND(hwnd);
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( INITDUMMY )
{
   hmg_ret_HWND(CreateWindowEx(0, WC_STATIC, TEXT(""), WS_CHILD, 0, 0, 0, 0, hmg_par_HWND(1), ( HMENU ) 0, GetInstance(), nullptr));
}

/* Modified by P.Ch. 17.06. */
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   LRESULT r = 0;
   PHB_SYMB g_ListenerSymb = hb_dynsymSymbol(g_ListenerDyns);

   if( message == WM_DESTROY )
   {
      AppEventRemove(hWnd, "ONCE", 0);
      AppEventRemove(hWnd, "ON", 0);

      if( IsWindow(g_hWndMain) && hWnd == g_hWndMain && g_hAccel != nullptr )
      {
         if( DestroyAcceleratorTable(g_hAccel) )
         {
            g_hAccel = nullptr;
         }
      }
   }

   if( message >= WM_APP && message <= ( WM_APP + MAX_EVENTS ) )
   {
      r = AppEventOn(hWnd, message, wParam, lParam);
   }
   else if( g_ListenerSymb )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushSymbol(g_ListenerSymb);
         hb_vmPushNil();
         hmg_vmPushHandle(hWnd);
         hb_vmPushLong(message);
         hb_vmPushNumInt(wParam);
         hb_vmPushNumInt(lParam);
         hb_vmDo(4);

         r = ( LRESULT ) hb_parnl( -1 );
         hb_vmRequestRestore();
      }
   }

   return ( r != 0 ) ? r : DefWindowProc(hWnd, message, wParam, lParam);
}

HB_FUNC( INITWINDOW )
{
   HWND hwnd;
   int style = WS_POPUP, ExStyle;

#ifndef UNICODE
   LPCSTR lpWindowName = hb_parc(1);
   LPCSTR lpClassName = hb_parc(12);
#else
   LPWSTR lpWindowName = AnsiToWide(( char * ) hb_parc(1));
   LPWSTR lpClassName = AnsiToWide(( char * ) hb_parc(12));
#endif

   if( hb_parl(16) )
   {
      ExStyle = WS_EX_CONTEXTHELP;
   }
   else
   {
      ExStyle = 0;
      if( !hb_parl(6) )
      {
         style |= WS_MINIMIZEBOX;
      }

      if( !hb_parl(7) )
      {
         style |= WS_MAXIMIZEBOX;
      }
   }

   if( !hb_parl(8) )
   {
      style |= WS_SIZEBOX;
   }

   if( !hb_parl(9) )
   {
      style |= WS_SYSMENU;
   }

   if( !hb_parl(10) )
   {
      style |= WS_CAPTION;
   }

   if( hb_parl(11) )
   {
      ExStyle |= WS_EX_TOPMOST;
   }

   if( hb_parl(14) )
   {
      style |= WS_VSCROLL;
   }

   if( hb_parl(15) )
   {
      style |= WS_HSCROLL;
   }

   if( hb_parl(17) )
   {
      ExStyle |= WS_EX_PALETTEWINDOW;
   }

   if( hb_parl(18) ) // Panel
   {
      style = WS_CHILD;
      ExStyle |= WS_EX_CONTROLPARENT | WS_EX_STATICEDGE;
   }

   hwnd = CreateWindowEx
          (
      ExStyle,
      lpClassName,
      lpWindowName,
      style,
      hb_parni(2),
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      hmg_par_HWND(13),
      ( HMENU ) nullptr,
      GetInstance(),
      nullptr
          );

   if( hwnd != nullptr )
   {
      hmg_ret_HWND(hwnd);
   }
   else
   {
      MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
   }

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpWindowName);
   hb_xfree(( TCHAR * ) lpClassName);
#endif
}

HB_FUNC( INITMODALWINDOW )
{
   HWND parent;
   HWND hwnd;
   int style;
   int ExStyle = 0;

#ifndef UNICODE
   LPCSTR lpWindowName = hb_parc(1);
   LPCSTR lpClassName = hb_parc(10);
#else
   LPWSTR lpWindowName = AnsiToWide(( char * ) hb_parc(1));
   LPWSTR lpClassName = AnsiToWide(( char * ) hb_parc(10));
#endif

   if( hb_parl(13) )
   {
      ExStyle = WS_EX_CONTEXTHELP;
   }

   parent = hmg_par_HWND(6);

   style = WS_POPUP;

   if( !hb_parl(7) )
   {
      style |= WS_SIZEBOX;
   }

   if( !hb_parl(8) )
   {
      style |= WS_SYSMENU;
   }

   if( !hb_parl(9) )
   {
      style |= WS_CAPTION;
   }

   if( hb_parl(11) )
   {
      style |= WS_VSCROLL;
   }

   if( hb_parl(12) )
   {
      style |= WS_HSCROLL;
   }

   hwnd = CreateWindowEx
          (
      ExStyle,
      lpClassName,
      lpWindowName,
      style,
      hb_parni(2),
      hb_parni(3),
      hb_parni(4),
      hb_parni(5),
      parent,
      ( HMENU ) nullptr,
      GetInstance(),
      nullptr
          );

   if( hwnd != nullptr )
   {
      hmg_ret_HWND(hwnd);
   }
   else
   {
      MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
   }

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpWindowName);
   hb_xfree(( TCHAR * ) lpClassName);
#endif
}

HB_FUNC( INITSPLITCHILDWINDOW )
{
   HWND hwnd;
   int style;

#ifndef UNICODE
   LPCSTR lpWindowName = hb_parc(5);
   LPCSTR lpClassName = hb_parc(3);
#else
   LPWSTR lpWindowName = AnsiToWide(( char * ) hb_parc(5));
   LPWSTR lpClassName = AnsiToWide(( char * ) hb_parc(3));
#endif

   style = WS_POPUP;

   if( !hb_parl(4) )
   {
      style |= WS_CAPTION;
   }

   if( hb_parl(7) )
   {
      style |= WS_VSCROLL;
   }

   if( hb_parl(8) )
   {
      style |= WS_HSCROLL;
   }

   hwnd = CreateWindowEx
          (
      WS_EX_STATICEDGE | WS_EX_TOOLWINDOW,
      lpClassName,
      lpWindowName,
      style,
      0,
      0,
      hb_parni(1),
      hb_parni(2),
      0,
      ( HMENU ) nullptr,
      GetInstance(),
      nullptr
          );

   if( hwnd != nullptr )
   {
      hmg_ret_HWND(hwnd);
   }
   else
   {
      MessageBox(0, TEXT("Window Creation Failed!"), TEXT("Error!"), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
   }

#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpWindowName);
   hb_xfree(( TCHAR * ) lpClassName);
#endif
}

HB_FUNC( INITSPLITBOX )
{
   HWND hwndOwner = hmg_par_HWND(1);
   REBARINFO rbi;
   HWND hwndRB;
   INITCOMMONCONTROLSEX icex;

   int style = WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | RBS_BANDBORDERS | RBS_VARHEIGHT | RBS_FIXEDORDER;

   if( hb_parl(2) )
   {
      style |= CCS_BOTTOM;
   }

   if( hb_parl(3) )
   {
      style |= CCS_VERT;
   }

   icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
   icex.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES;
   InitCommonControlsEx(&icex);

   hwndRB = CreateWindowEx
            (
      WS_EX_TOOLWINDOW | WS_EX_DLGMODALFRAME,
      REBARCLASSNAME,
      nullptr,
      style,
      0,
      0,
      0,
      0,
      hwndOwner,
      nullptr,
      GetInstance(),
      nullptr
            );

   // Initialize and send the REBARINFO structure.
   rbi.cbSize = sizeof(REBARINFO);   // Required when using this struct.
   rbi.fMask = 0;
   rbi.himl = nullptr;
   SendMessage(hwndRB, RB_SETBARINFO, 0, ( LPARAM ) &rbi);

   hmg_ret_HWND(hwndRB);
}

/* Modified by P.Ch. 16.10.-16.12.,17.06. */
HB_FUNC( REGISTERWINDOW )
{
   WNDCLASS WndClass;
   HBRUSH hBrush = 0;
   HICON hIcon;
   HCURSOR hCursor;

#ifndef UNICODE
   LPCTSTR lpIconName = HB_ISCHAR(1) ? hb_parc(1) : ( HB_ISNUM(1) ? MAKEINTRESOURCE(( WORD ) hb_parnl(1)) : nullptr );
#else
   LPWSTR lpIconName = HB_ISCHAR(1) ? AnsiToWide(( char * ) hb_parc(1)) : ( HB_ISNUM(1) ? ( LPWSTR ) MAKEINTRESOURCE(( WORD ) hb_parnl(1)) : nullptr );
#endif

   void *  hClassName;
   LPCTSTR lpClassName = HB_PARSTR(2, &hClassName, nullptr);
#ifndef UNICODE
   LPCSTR lpCursorName = HB_ISCHAR(4) ? hb_parc(4) : ( HB_ISNUM(4) ? MAKEINTRESOURCE(( WORD ) hb_parnl(4)) : nullptr );
#else
   LPWSTR lpCursorName = HB_ISCHAR(4) ? AnsiToWide(( char * ) hb_parc(4)) : ( HB_ISNUM(4) ? ( LPWSTR ) MAKEINTRESOURCE(( WORD ) hb_parnl(4)) : nullptr );
#endif

   WndClass.style = CS_DBLCLKS | /*CS_HREDRAW | CS_VREDRAW |*/ CS_OWNDC;
   WndClass.lpfnWndProc = WndProc;
   WndClass.cbClsExtra = 0;
   WndClass.cbWndExtra = 0;
   WndClass.hInstance = GetInstance();

   // icon from resource
   hIcon = LoadIcon(GetResources(), lpIconName);
   // from file
   if( hIcon == nullptr && HB_ISCHAR(1) )
   {
      hIcon = static_cast<HICON>(LoadImage(nullptr, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
   }
   WndClass.hIcon = ( ( hIcon != nullptr ) ? hIcon : LoadIcon(nullptr, IDI_APPLICATION) );

   // cursor from resource
   hCursor = LoadCursor(GetResources(), lpCursorName);
   // from file
   if( ( hCursor == nullptr ) && HB_ISCHAR(4) )
   {
      hCursor = LoadCursorFromFile(lpCursorName);
   }
   WndClass.hCursor = ( ( hCursor != nullptr ) ? hCursor : LoadCursor(nullptr, IDC_ARROW) );

   if( HB_ISARRAY(3) )  // old behavior (before 16.10)
   {
      if( HB_PARNI(3, 1) == -1 )
      {
         hBrush = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1);
      }
      else
      {
         hBrush = CreateSolidBrush(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3)));
      }
   }
   else if( HB_ISCHAR(3) || HB_ISNUM(3) )
   {
      HBITMAP hImage;
#ifndef UNICODE
      LPCTSTR lpImageName = HB_ISCHAR(3) ? hb_parc(3) : ( HB_ISNUM(3) ? MAKEINTRESOURCE(( WORD ) hb_parnl(3)) : nullptr );
#else
      LPWSTR lpImageName = HB_ISCHAR(3) ? AnsiToWide(( char * ) hb_parc(3)) : ( HB_ISNUM(3) ? ( LPWSTR ) MAKEINTRESOURCE(( WORD ) hb_parnl(3)) : nullptr );
#endif

      hImage = static_cast<HBITMAP>(LoadImage(GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));

      if( hImage == nullptr && HB_ISCHAR(3) )
      {
         hImage = static_cast<HBITMAP>(LoadImage(nullptr, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT));
      }

#ifdef UNICODE
      hb_xfree(( TCHAR * ) lpImageName);
#endif
      if( hImage == nullptr )
      {
         hImage = HMG_LoadImage(hb_parc(3), nullptr);
      }

      if( hImage != nullptr )
      {
         hBrush = CreatePatternBrush(hImage);
      }
   }

   WndClass.hbrBackground = ( hBrush != nullptr ) ? hBrush : ( hBrush = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1) );
   WndClass.lpszMenuName = nullptr;
   WndClass.lpszClassName = lpClassName;

   if( !RegisterClass(&WndClass) )
   {
      hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
   }

   hb_strfree(hClassName);
#ifdef UNICODE
   if( HB_ISCHAR(1) )
   {
      hb_xfree(( TCHAR * ) lpIconName);
   }
   if( HB_ISCHAR(4) )
   {
      hb_xfree(( TCHAR * ) lpCursorName);
   }
#endif
   hmg_ret_HBRUSH(hBrush);
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( REGISTERSPLITCHILDWINDOW )
{
   WNDCLASS WndClass;
   HBRUSH hbrush = 0;

#ifndef UNICODE
   LPCTSTR lpIcon = HB_ISCHAR(1) ? hb_parc(1) : ( HB_ISNIL(1) ? nullptr : MAKEINTRESOURCE(( WORD ) hb_parnl(1)) );
#else
   LPWSTR lpIcon = HB_ISCHAR(1) ? AnsiToWide(( char * ) hb_parc(1)) : ( HB_ISNIL(1) ? nullptr : ( LPWSTR ) MAKEINTRESOURCE(( WORD ) hb_parnl(1)) );
#endif

   void *  hClassName;
   LPCTSTR lpClassName = HB_PARSTR(2, &hClassName, nullptr);

   WndClass.style = CS_OWNDC;
   WndClass.lpfnWndProc = WndProc;
   WndClass.cbClsExtra = 0;
   WndClass.cbWndExtra = 0;
   WndClass.hInstance = GetInstance();
   WndClass.hIcon = LoadIcon(GetInstance(), lpIcon);
   if( WndClass.hIcon == nullptr )
   {
      WndClass.hIcon = static_cast<HICON>(LoadImage(0, lpIcon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE));
   }

   if( WndClass.hIcon == nullptr )
   {
      WndClass.hIcon = LoadIcon(nullptr, IDI_APPLICATION);
   }

   WndClass.hCursor = LoadCursor(nullptr, IDC_ARROW);

   if( HB_PARNI(3, 1) == -1 )
   {
      WndClass.hbrBackground = reinterpret_cast<HBRUSH>(COLOR_BTNFACE + 1);
   }
   else
   {
      hbrush = CreateSolidBrush(RGB(HB_PARNI(3, 1), HB_PARNI(3, 2), HB_PARNI(3, 3)));
      WndClass.hbrBackground = hbrush;
   }

   WndClass.lpszMenuName = nullptr;
   WndClass.lpszClassName = lpClassName;

   if( !RegisterClass(&WndClass) )
   {
      hmg_ErrorExit(TEXT("Window Registration Failed!"), 0, TRUE);
   }

   hb_strfree(hClassName);
#ifdef UNICODE
   hb_xfree(( TCHAR * ) lpIcon);
#endif
   hmg_ret_HBRUSH(hbrush);
}

/* Modified by P.Ch. 17.06. */
HB_FUNC( UNREGISTERWINDOW )
{
   void *  hClassName;
   LPCTSTR lpClassName = HB_PARSTR(1, &hClassName, nullptr);

   UnregisterClass(lpClassName, GetInstance());
   hb_strfree(hClassName);
}

HB_FUNC( MSC_VER )
{
#if defined( _MSC_VER )
   hb_retnl( _MSC_VER );
#else
   hb_retnl(0);
#endif
}

#define COMPILER_BUF_SIZE  80

HB_FUNC( BORLANDC )
{
   char * pszCompiler;

   #ifdef __BORLANDC__

   const char * pszName;
   char szSub[64];

   int iVerMajor;
   int iVerMinor;
   int iVerPatch;

   pszCompiler = ( char * ) hb_xgrab(COMPILER_BUF_SIZE);
   szSub[0] = '\0';

   #if ( __BORLANDC__ >= 0x0590 )    /* Version 5.9 */
      #if ( __BORLANDC__ >= 0x0620 ) /* Version 6.2 */
   pszName = "Embarcadero C++";
      #else
   pszName = "CodeGear C++";
      #endif
   #else
   pszName = "Borland C++";
   #endif

   #if ( __BORLANDC__ >= 0x0500 ) /* Version 5.x */
   iVerMajor = __BORLANDC__ >> 8;
   iVerMinor = ( __BORLANDC__ & 0xFF ) >> 4;
   iVerPatch = __BORLANDC__ & 0xF;
   #else /* Version 4.x */
   iVerMajor = __BORLANDC__ >> 8;
   iVerMinor = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
   iVerPatch = 0;
   #endif

   if( pszName )
   {
      if( iVerPatch != 0 )
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
      }
      else if( iVerMajor != 0 || iVerMinor != 0 )
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d0", pszName, szSub, iVerMajor, iVerMinor );
      }
      else
      {
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub );
      }
   }
   else
   {
      hb_strncpy(pszCompiler, "(unknown)", COMPILER_BUF_SIZE - 1);
   }

   #if defined( HB_ARCH_32BIT )
   hb_strncat(pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1);
   #elif defined( HB_ARCH_64BIT )
   hb_strncat(pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1);
   #endif

   #else
   pszCompiler = ( char * ) hb_xgrab(COMPILER_BUF_SIZE);
   strcpy(pszCompiler, "");
   #endif /* __BORLANDC__ */

   hb_retc_buffer(pszCompiler);
}

#include "mgver.h"

HB_FUNC( HMG_VERSION )
{
   char * pszVersion;

   pszVersion = ( char * ) hb_xgrab(40);
   hb_snprintf( pszVersion, 40, "Harbour MiniGUI %d.%d.%d (%s)",
                MG_VER_MAJOR, MG_VER_MINOR, MG_VER_RELEASE, MG_VER_STATUS );

   hb_retc_buffer(pszVersion);
}

HB_FUNC( HMG_ISALPHA )
{
#ifndef UNICODE
   LPSTR ch = ( char * ) hb_parc(1);
#else
   LPWSTR ch = AnsiToWide(( char * ) hb_parc(1));
#endif

   hb_retl(( BOOL ) IsCharAlpha(ch[0]));
}

HB_FUNC( HMG_ISDIGIT )
{
#ifndef UNICODE
   LPSTR ch = ( char * ) hb_parc(1);
#else
   LPWSTR ch = AnsiToWide(( char * ) hb_parc(1));
#endif

   hb_retl(( BOOL ) ( IsCharAlphaNumeric( ch[0] ) && !IsCharAlpha(ch[0]) ));
}

#ifdef UNICODE
HB_FUNC( HMG_LOWER )
{
   LPSTR pStr;
   TCHAR * Text = ( TCHAR * ) AnsiToWide(( char * ) hb_parc(1));
   INT nLen;
   TCHAR * Buffer;

   if( Text == nullptr )
   {
      hb_retc( nullptr );
      return;
   }

   nLen = lstrlen(Text) + 1;
   Buffer = ( TCHAR * ) hb_xgrab(nLen * sizeof(TCHAR));

   if( Buffer != nullptr )
   {
      lstrcpy(Buffer, Text);
      CharLower(Buffer);
      pStr = WideToAnsi(Buffer);
      hb_retc( pStr );
      hb_xfree(pStr);
   }
   else
   {
      hb_retc( nullptr );
   }

   hb_xfree(Text);
   hb_xfree(Buffer);
}

HB_FUNC( HMG_UPPER )
{
   LPSTR pStr;
   TCHAR * Text = ( TCHAR * ) AnsiToWide(( char * ) hb_parc(1));
   INT nLen;
   TCHAR * Buffer;

   if( Text == nullptr )
   {
      hb_retc( nullptr );
      return;
   }

   nLen = lstrlen(Text) + 1;
   Buffer = ( TCHAR * ) hb_xgrab(nLen * sizeof(TCHAR));

   if( Buffer != nullptr )
   {
      lstrcpy(Buffer, Text);
      CharUpper(Buffer);
      pStr = WideToAnsi(Buffer);
      hb_retc( pStr );
      hb_xfree(pStr);
   }
   else
   {
      hb_retc( nullptr );
   }

   hb_xfree(Text);
   hb_xfree(Buffer);
}

HB_FUNC( HMG_ISLOWER )
{
#ifndef UNICODE
   LPSTR Text = ( LPSTR ) hb_parc(1);
#else
   LPWSTR Text = AnsiToWide(( char * ) hb_parc(1));
#endif
   hb_retl(( BOOL ) IsCharLower(Text[0]));

#ifdef UNICODE
   hb_xfree(Text);
#endif
}

HB_FUNC( HMG_ISUPPER )
{
#ifndef UNICODE
   LPSTR Text = ( LPSTR ) hb_parc(1);
#else
   LPWSTR Text = AnsiToWide(( char * ) hb_parc(1));
#endif
   hb_retl(( BOOL ) IsCharUpper(Text[0]));

#ifdef UNICODE
   hb_xfree(Text);
#endif
}

#else

HB_FUNC_TRANSLATE(HMG_LOWER, LOWER)
HB_FUNC_TRANSLATE(HMG_UPPER, UPPER)
HB_FUNC_TRANSLATE(HMG_ISLOWER, ISLOWER)
HB_FUNC_TRANSLATE(HMG_ISUPPER, ISUPPER)

#endif /* UNICODE */
