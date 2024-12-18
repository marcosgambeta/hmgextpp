//
// Harbour MiniGUI Project source code:
// The definitions for minigui C-level code.
//
// Copyright 2015-2022 Grigory Filatov <gfilatov@gmail.com>
// www - http://www.hmgextended.com
//

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this software; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).

#ifndef MG_SETUP_H_
#define MG_SETUP_H_

#ifndef WINVER
  #if defined(__WIN98__)
    #define WINVER   0x0400      // version 4.0
  #else
    #define WINVER   0x0501      // version 5.0
  #endif
#endif // !WINVER

#ifndef _WIN32_WINNT
  #define _WIN32_WINNT   WINVER  // XP = 0x0501 , Vista = 0x0600
#endif // !_WIN32_WINNT

#ifndef _WIN32_IE
  #define _WIN32_IE 0x0501
#endif // !_WIN32_IE

#include "SET_COMPILE_HMG_UNICODE.ch"

#if defined(UNICODE) && !defined(_UNICODE)
  #define _UNICODE
#endif // UNICODE && !_UNICODE

#include <hbapi.hpp>

#ifndef NO_LEAN_AND_MEAN
  #define WIN32_LEAN_AND_MEAN
#endif // !NO_LEAN_AND_MEAN

#include <windows.h>
#include <tchar.h>

#ifndef NO_LEAN_AND_MEAN
  #undef  WIN32_LEAN_AND_MEAN
#endif // !NO_LEAN_AND_MEAN

#ifndef HMG_LEGACY_ON
//#define HMG_LEGACY_OFF
#endif

#if defined(_WIN64)
  #define HB_arraySetNL    hb_arraySetNLL
  #define HB_arrayGetNL    hb_arrayGetNLL
  #define HB_PARNI         hb_parvni
  #define HB_PARNL         hb_parnll
  #define HB_PARVNL        hb_parvnll
  #define HB_RETNL         hb_retnll
  #define HB_STORC         hb_storvc
  #define HB_STORNI        hb_storvni
  #define HB_STORNL        hb_stornll
  #define HB_STORVNL       hb_storvnll
  #define HB_STORL         hb_storvl
  #define HB_STORDL        hb_storvdl
#else
  #define HB_arraySetNL    hb_arraySetNL
  #define HB_arrayGetNL    hb_arrayGetNL
  #define HB_PARNL         hb_parnl
  #define HB_RETNL         hb_retnl
  #define HB_STORNL        hb_stornl
  #define HB_PARNI         hb_parvni
  #define HB_PARVNL        hb_parvnl
  #define HB_STORC         hb_storvc
  #define HB_STORNI        hb_storvni
  #define HB_STORVNL       hb_storvnl
  #define HB_STORL         hb_storvl
  #define HB_STORDL        hb_storvdl
#endif // _WIN64

#if defined(UNICODE)
  #define _isValidCtrlClass  _isValidCtrlClassW
#else
  #define _isValidCtrlClass  _isValidCtrlClassA
#endif // UNICODE

#if defined(__BORLANDC__) && ! defined(HB_ARCH_64BIT)
  #undef MAKELONG
  #define MAKELONG(a, b)  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                         ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif // __BORLANDC__ && !HB_ARCH_64BIT

//****************************************************************************************
//  MACRO DEFINITION FOR CALL DLL FUNCTION
//****************************************************************************************
extern HB_PTRUINT wapi_GetProcAddress(HMODULE hmodule, LPCSTR lpProcName);

#define HMG_DEFINE_DLL_FUNC( \
      _FUNC_NAME,             \
      _DLL_LIBNAME,           \
      _DLL_FUNC_RET,          \
      _DLL_FUNC_TYPE,         \
      _DLL_FUNC_NAMESTRINGAW, \
      _DLL_FUNC_PARAM,        \
      _DLL_FUNC_CALLPARAM,    \
      _DLL_FUNC_RETFAILCALL   \
      ) \
\
   _DLL_FUNC_RET _DLL_FUNC_TYPE _FUNC_NAME _DLL_FUNC_PARAM \
   { \
      typedef _DLL_FUNC_RET ( _DLL_FUNC_TYPE * PFUNC ) _DLL_FUNC_PARAM; \
      static PFUNC pfunc = NULL; \
      if( pfunc == NULL ) \
      { \
         HMODULE hLib = LoadLibrary( _DLL_LIBNAME ); \
         pfunc = ( PFUNC ) wapi_GetProcAddress(hLib, _DLL_FUNC_NAMESTRINGAW); \
      } \
      if( pfunc == NULL ) \
         return ( ( _DLL_FUNC_RET ) _DLL_FUNC_RETFAILCALL ); \
      else \
         return pfunc _DLL_FUNC_CALLPARAM; \
   }

#endif // MG_SETUP_H_

// declarations

// Minigui Resources control system
void RegisterResource(HANDLE hResource, LPCSTR szType);
void pascal DelResource(HANDLE hResource);

HINSTANCE GetInstance(void);
HINSTANCE GetResources(void);

HBITMAP HMG_LoadPicture(const char * pszName, int width, int height, HWND hWnd, int ScaleStretch, int Transparent, long BackgroundColor, int AdjustImage, bool bAlphaFormat, int iAlpfaConstant);
HBITMAP HMG_LoadImage(const char * pszImageName, const char * pszTypeOfRes);

// TODO: handles -> pointers (work in progress - HMG_USE_POINTERS is unstable yet)

// macros for parameters

#define hmg_par_BOOL(n)     static_cast<BOOL>(hb_parl(n))
#define hmg_par_BYTE(n)     static_cast<BYTE>(hb_parni(n))
#define hmg_par_COLORREF(n) static_cast<COLORREF>(hb_parnl(n))
#define hmg_par_DWORD(n)    static_cast<DWORD>(hb_parnl(n))
#define hmg_par_int(n)      static_cast<int>(hb_parni(n))
#define hmg_par_INT(n)      static_cast<INT>(hb_parni(n))
#define hmg_par_LONG(n)     static_cast<LONG>(hb_parnl(n))
#define hmg_par_LPARAM(n)   static_cast<LPARAM>(hb_parnl(n))
#define hmg_par_UINT(n)     static_cast<UINT>(hb_parni(n))
#define hmg_par_UINT_PTR(n) static_cast<UINT_PTR>(hb_parni(n))
#define hmg_par_WORD(n)     static_cast<WORD>(hb_parni(n))
#define hmg_par_WPARAM(n)   static_cast<WPARAM>(hb_parni(n))

#ifdef HMG_USE_POINTERS
   #define hmg_par_HACCEL(n)     reinterpret_cast<HACCEL>(hb_parptr(n))
   #define hmg_par_HANDLE(n)     reinterpret_cast<HANDLE>(hb_parptr(n))
   #define hmg_par_HBITMAP(n)    reinterpret_cast<HBITMAP>(hb_parptr(n))
   #define hmg_par_HBRUSH(n)     reinterpret_cast<HBRUSH>(hb_parptr(n))
   #define hmg_par_HCURSOR(n)    reinterpret_cast<HCURSOR>(hb_parptr(n))
   #define hmg_par_HDC(n)        reinterpret_cast<HDC>(hb_parptr(n))
   #define hmg_par_HFONT(n)      reinterpret_cast<HFONT>(hb_parptr(n))
   #define hmg_par_HGDIOBJ(n)    reinterpret_cast<HGDIOBJ>(hb_parptr(n))
   #define hmg_par_HICON(n)      reinterpret_cast<HICON>(hb_parptr(n))
   #define hmg_par_HIMAGELIST(n) reinterpret_cast<HIMAGELIST>(hb_parptr(n))
   #define hmg_par_HINSTANCE(n)  reinterpret_cast<HINSTANCE>(hb_parptr(n))
   #define hmg_par_HKEY(n)       reinterpret_cast<HKEY>(hb_parptr(n))
   #define hmg_par_HMENU(n)      reinterpret_cast<HMENU>(hb_parptr(n))
   #define hmg_par_HPEN(n)       reinterpret_cast<HPEN>(hb_parptr(n))
   #define hmg_par_HRGN(n)       reinterpret_cast<HRGN>(hb_parptr(n))
   #define hmg_par_HTREEITEM(n)  reinterpret_cast<HTREEITEM>(hb_parptr(n))
   #define hmg_par_HWND(n)       reinterpret_cast<HWND>(hb_parptr(n))
#else
   #define hmg_par_HACCEL(n)     reinterpret_cast<HACCEL>(HB_PARNL(n))
   #define hmg_par_HANDLE(n)     reinterpret_cast<HANDLE>(HB_PARNL(n))
   #define hmg_par_HBITMAP(n)    reinterpret_cast<HBITMAP>(HB_PARNL(n))
   #define hmg_par_HBRUSH(n)     reinterpret_cast<HBRUSH>(HB_PARNL(n))
   #define hmg_par_HCURSOR(n)    reinterpret_cast<HCURSOR>(HB_PARNL(n))
   #define hmg_par_HDC(n)        reinterpret_cast<HDC>(HB_PARNL(n))
   #define hmg_par_HFONT(n)      reinterpret_cast<HFONT>(HB_PARNL(n))
   #define hmg_par_HGDIOBJ(n)    reinterpret_cast<HGDIOBJ>(HB_PARNL(n))
   #define hmg_par_HICON(n)      reinterpret_cast<HICON>(HB_PARNL(n))
   #define hmg_par_HIMAGELIST(n) reinterpret_cast<HIMAGELIST>(HB_PARNL(n))
   #define hmg_par_HINSTANCE(n)  reinterpret_cast<HINSTANCE>(HB_PARNL(n))
   #define hmg_par_HKEY(n)       reinterpret_cast<HKEY>(HB_PARNL(n))
   #define hmg_par_HMENU(n)      reinterpret_cast<HMENU>(HB_PARNL(n))
   #define hmg_par_HPEN(n)       reinterpret_cast<HPEN>(HB_PARNL(n))
   #define hmg_par_HRGN(n)       reinterpret_cast<HRGN>(HB_PARNL(n))
   #define hmg_par_HTREEITEM(n)  reinterpret_cast<HTREEITEM>(HB_PARNL(n))
   #define hmg_par_HWND(n)       reinterpret_cast<HWND>(HB_PARNL(n))
#endif

// macros for returns

#define hmg_ret_BOOL(x)     hb_retl(x)
#define hmg_ret_COLORREF(x) hb_retnl(x)
#define hmg_ret_int(x)      hb_retni(x)
#define hmg_ret_LRESULT(x)  hb_retnl(static_cast<long>(x))

#ifdef HMG_USE_POINTERS
   #define hmg_ret_HACCEL(x)     hb_retptr(x)
   #define hmg_ret_HANDLE(x)     hb_retptr(x)
   #define hmg_ret_HBITMAP(x)    hb_retptr(x)
   #define hmg_ret_HBRUSH(x)     hb_retptr(x)
   #define hmg_ret_HCURSOR(x)    hb_retptr(x)
   #define hmg_ret_HDC(x)        hb_retptr(x)
   #define hmg_ret_HFONT(x)      hb_retptr(x)
   #define hmg_ret_HGDIOBJ(x)    hb_retptr(x)
   #define hmg_ret_HICON(x)      hb_retptr(x)
   #define hmg_ret_HIMAGELIST(x) hb_retptr(x)
   #define hmg_ret_HMENU(x)      hb_retptr(x)
   #define hmg_ret_HMONITOR(x)   hb_retptr(x)
   #define hmg_ret_HPEN(x)       hb_retptr(x)
   #define hmg_ret_HRGN(x)       hb_retptr(x)
   #define hmg_ret_HTREEITEM(x)  hb_retptr(x)
   #define hmg_ret_HWND(x)       hb_retptr(x)
#else
   #define hmg_ret_HACCEL(x)     HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HANDLE(x)     HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HBITMAP(x)    HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HBRUSH(x)     HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HCURSOR(x)    HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HDC(x)        HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HFONT(x)      HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HGDIOBJ(x)    HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HICON(x)      HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HIMAGELIST(x) HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HMENU(x)      HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HMONITOR(x)   HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HPEN(x)       HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HRGN(x)       HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HTREEITEM(x)  HB_RETNL(reinterpret_cast<LONG_PTR>(x))
   #define hmg_ret_HWND(x)       HB_RETNL(reinterpret_cast<LONG_PTR>(x))
#endif

#ifdef HMG_USE_POINTERS
#define hmg_vmPushHandle(x)      hb_vmPushPointer(x)
#define hmg_vmPushHWND(x)        hb_vmPushPointer(x)
#else
#define hmg_vmPushHandle(x)      hb_vmPushLong(reinterpret_cast<LONG_PTR>(x))
#define hmg_vmPushHWND(x)        hb_vmPushNumInt(reinterpret_cast<LONG_PTR>(x))
#endif

#ifdef HMG_USE_POINTERS
#define hmg_storvhandle(h, n1, n2) hb_storvptr(h, n1, n2)
#else
#define hmg_storvhandle(h, n1, n2) HB_STORVNL(reinterpret_cast<LONG_PTR>(h), n1, n2)
#endif

#define hmg_vmPushUINT(Msg) hb_vmPushLong(Msg)
#define hmg_vmPushWPARAM(wParam) hb_vmPushNumInt(wParam)
#define hmg_vmPushLPARAM(lParam) hb_vmPushNumInt(lParam)
