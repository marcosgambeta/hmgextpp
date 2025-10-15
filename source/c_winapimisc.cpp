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

#if defined(_MSC_VER)
#pragma warning(disable : 4996)
#endif
#include <commctrl.h>
#include <lmcons.h>
#include <shellapi.h>
#include <shlobj.h>
#include <shlwapi.h>

#include <hbapierr.hpp>
#include <hbapiitm.hpp>

// this has to be declared before hbapifs.h is included
#define _HB_FILE_INTERNAL_

#include <hbapifs.hpp>
#include "inkey.ch"
#include <hbwinuni.hpp>
#include <hbstack.hpp>
#include <string>

#if defined(_MSC_VER)
#define itoa(__value, __string, __radix) _itoa(__value, __string, __radix)
#endif

bool hmg_ArrayToRect(PHB_ITEM aRect, RECT *rc);
PHB_ITEM hmg_RectToArray(RECT *rc);
extern void hmg_ErrorExit(LPCTSTR lpMessage, DWORD dwError, BOOL bExit);

using SHGETFOLDERPATH = HMODULE(__stdcall *)(HWND, int, HANDLE, DWORD, LPTSTR);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif
BOOL SysRefresh(void);

HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName)
{
  FARPROC pProc;

  pProc = GetProcAddress(hModule, lpProcName);
  return (HB_PTRUINT)pProc;
}

// WaitRun function for Minigui With Pipe redirection
// Author: Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
// Parameters WaitRunPipe(cCommand,nShowWindow,cFile)
HB_FUNC(HMG_WAITRUNPIPE)
{
  HANDLE ReadPipeHandle;
  HANDLE WritePipeHandle; // not used here

#ifndef UNICODE
  LPSTR lpCommandLine = const_cast<char *>(hb_parc(1));
#else
  LPWSTR lpCommandLine = AnsiToWide(const_cast<char *>(hb_parc(1)));
#endif
  auto szFile = static_cast<const char *>(hb_parc(3));
  HB_FHANDLE nHandle;

  SECURITY_ATTRIBUTES sa{};
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle = 1;
  sa.lpSecurityDescriptor = nullptr;

  if (!hb_fsFile(szFile)) {
    nHandle = hb_fsCreate(szFile, 0);
  } else {
    nHandle = hb_fsOpen(szFile, 2);
    hb_fsSeek(nHandle, 0, 2);
  }

  if (!CreatePipe(&ReadPipeHandle, &WritePipeHandle, &sa, 0)) {
    hb_retnl(-1);
    return;
  }

  STARTUPINFO StartupInfo{};

  PROCESS_INFORMATION ProcessInfo{};
  ProcessInfo.hProcess = INVALID_HANDLE_VALUE;
  ProcessInfo.hThread = INVALID_HANDLE_VALUE;
  StartupInfo.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow = hmg_par_WORD(2);
  StartupInfo.hStdOutput = WritePipeHandle;
  StartupInfo.hStdError = WritePipeHandle;

  if (!CreateProcess(nullptr, lpCommandLine, 0, 0, FALSE, CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS, 0, 0,
                     &StartupInfo, &ProcessInfo)) {
    hb_retnl(-1);
    return;
  } else {
#ifdef UNICODE
    hb_xfree((TCHAR *)lpCommandLine);
#endif
  }

  auto Data = static_cast<char *>(hb_xgrab(1024));
  for (;;) {
    DWORD BytesRead;
    DWORD TotalBytes;
    DWORD BytesLeft;

    // Check for the presence of data in the pipe
    if (!PeekNamedPipe(ReadPipeHandle, Data, sizeof(Data), &BytesRead, &TotalBytes, &BytesLeft)) {
      hb_retnl(-1);
      return;
    }

    // If there is bytes, read them
    if (BytesRead) {
      if (!ReadFile(ReadPipeHandle, Data, sizeof(Data) - 1, &BytesRead, nullptr)) {
        hb_retnl(-1);
        return;
      }

      Data[BytesRead] = '\0';
      hb_fsWriteLarge(nHandle, Data, BytesRead);
    } else {
      // Is the console app terminated?
      if (WaitForSingleObject(ProcessInfo.hProcess, 0) == WAIT_OBJECT_0) {
        break;
      }
    }
  }

  CloseHandle(ProcessInfo.hThread);
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ReadPipeHandle);
  CloseHandle(WritePipeHandle);
  hb_fsClose(nHandle);
  hb_xfree(Data);
}

HB_FUNC(HMG_COPYRTFTOCLIPBOARD) // CopyRtfToClipboard(cRtfText) store cRTFText in Windows clipboard
{
  UINT cf;
  const char *cStr = HB_ISCHAR(1) ? hb_parc(1) : "";
  auto nLen = static_cast<int>(strlen(cStr));

  if ((nLen == 0) || !OpenClipboard(GetActiveWindow())) {
    return;
  }

  // Get Clipboard format id for RTF.
  cf = RegisterClipboardFormat(TEXT("Rich Text Format"));

  EmptyClipboard();

  auto hglbCopy = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, (nLen + 4) * sizeof(TCHAR));
  if (hglbCopy == nullptr) {
    CloseClipboard();
    return;
  }

  auto lptstrCopy = static_cast<char *>(GlobalLock(hglbCopy));
  memcpy(lptstrCopy, cStr, nLen * sizeof(TCHAR));
  lptstrCopy[nLen] = (TCHAR)0; // NULL character
  GlobalUnlock(hglbCopy);

  SetClipboardData(cf, hglbCopy);
  CloseClipboard();
}

HB_FUNC(HMG_COPYTOCLIPBOARD) // CopyToClipboard(cText) store cText in Windows clipboard
{
  const char *cStr = HB_ISCHAR(1) ? hb_parc(1) : "";
  auto nLen = static_cast<int>(strlen(cStr));

  if ((nLen == 0) || !OpenClipboard(GetActiveWindow())) {
    return;
  }

  EmptyClipboard();

  auto hglbCopy = GlobalAlloc(GMEM_DDESHARE, (nLen + 1) * sizeof(TCHAR));
  if (hglbCopy == nullptr) {
    CloseClipboard();
    return;
  }

  auto lptstrCopy = static_cast<char *>(GlobalLock(hglbCopy));
  memcpy(lptstrCopy, cStr, nLen * sizeof(TCHAR));
  lptstrCopy[nLen] = (TCHAR)0; // null character
  GlobalUnlock(hglbCopy);

  SetClipboardData(HB_ISNUM(2) ? hmg_par_UINT(2) : CF_TEXT, hglbCopy);
  CloseClipboard();
}

HB_FUNC(HMG_RETRIEVETEXTFROMCLIPBOARD)
{
  HGLOBAL hClipMem;
  LPSTR lpClip;

  if (IsClipboardFormatAvailable(CF_TEXT) && OpenClipboard(GetActiveWindow())) {
    hClipMem = GetClipboardData(CF_TEXT);
    if (hClipMem) {
      lpClip = static_cast<LPSTR>(GlobalLock(hClipMem));
      if (lpClip) {
        hb_retc(lpClip);
        GlobalUnlock(hClipMem);
      } else {
        hb_retc("");
      }
    } else {
      hb_retc(nullptr);
    }

    CloseClipboard();
  } else {
    hb_retc(nullptr);
  }
}

HB_FUNC(HMG_CLEARCLIPBOARD)
{
  if (OpenClipboard(hmg_par_HWND(1))) {
    EmptyClipboard();
    CloseClipboard();
    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

#if 0
HB_FUNC(HMG_GETRED) // TODO: deprecated (using waGetRValue from WinApi library)
{
   hb_retnl( GetRValue(hb_parnl(1)) );
}
#endif

#if 0
HB_FUNC(HMG_GETGREEN) // TODO: deprecated (using waGetGValue from WinApi library)
{
   hb_retnl( GetGValue(hb_parnl(1)) );
}
#endif

#if 0
HB_FUNC(HMG_GETBLUE) // TODO: deprecated (using waGetBValue from WinApi library)
{
   hb_retnl( GetBValue(hb_parnl(1)) );
}
#endif

HB_FUNC(HMG_GETKEYSTATE) // TODO: deprecated (using waGetKeyState from WinApi library)
{
  hb_retni(GetKeyState(hb_parni(1)));
}

HB_FUNC(HMG_KEYBOARDCLEARBUFFER)
{
  MSG Msg;

  while (PeekMessage(&Msg, nullptr, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE)) {
    ;
  }  
}

HB_FUNC(HMG_MOUSECLEARBUFFER)
{
  MSG Msg;

  while (PeekMessage(&Msg, nullptr, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)) {
    ;
  }  
}

#ifndef USER_TIMER_MINIMUM
#define USER_TIMER_MINIMUM 0x0000000A
#endif
#ifndef USER_TIMER_MAXIMUM
#define USER_TIMER_MAXIMUM 0x7FFFFFFF
#endif

HB_FUNC(HMG_INKEYGUI)
{
  UINT uElapse = hb_parnidef(1, USER_TIMER_MINIMUM);
  UINT_PTR uTimer;
  MSG Msg;
  BOOL bRet, bBreak = FALSE;
  UINT uRet = 0;

  if (uElapse == 0) {
    uElapse = USER_TIMER_MAXIMUM;
  }

  uTimer = SetTimer(nullptr, 0, uElapse, nullptr);

  while ((bRet = GetMessage(&Msg, nullptr, 0, 0)) != 0) {
    if (bRet == -1) {
      // handle the error and possibly exit
      hmg_ErrorExit(TEXT("INKEYGUI"), 0, TRUE);
    } else {
      switch (Msg.message) {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN: {
        bBreak = TRUE;
        uRet = static_cast<UINT>(Msg.wParam);
        break;
      }
      case WM_TIMER: {
        bBreak = (Msg.wParam == uTimer);
        break;
      }
      case WM_LBUTTONDOWN:
      case WM_RBUTTONDOWN: {
        bBreak = TRUE;
        uRet = (Msg.message == WM_LBUTTONDOWN) ? K_LBUTTONDOWN : K_RBUTTONDOWN;
        PostMessage(Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam);
        break; // TODO: unnecessary break
      }
      }
    }

    if (bBreak) {
      KillTimer(nullptr, uTimer);
      break;
    } else {
      TranslateMessage(&Msg); // Translates virtual key codes
      DispatchMessage(&Msg);  // Dispatches message to window
    }
  }

  hb_retns(uRet);
}

HB_FUNC(HMG_GETDC)
{
  hmg_ret_HDC(GetDC(hmg_par_HWND(1)));
}

HB_FUNC(HMG_RELEASEDC)
{
  hb_retl(ReleaseDC(hmg_par_HWND(1), hmg_par_HDC(2)));
}

HB_FUNC(HMG_HIWORD)
{
  hb_retni(HIWORD(hmg_par_DWORD(1)));
}

HB_FUNC(HMG_LOWORD)
{
  hb_retni(LOWORD(hmg_par_DWORD(1)));
}

HB_FUNC(HMG_C_GETSPECIALFOLDER) // Contributed By Ryszard Ry�ko
{
#ifdef UNICODE
  LPSTR pStr;
#endif
  auto lpBuffer = static_cast<TCHAR *>(hb_xgrab((MAX_PATH + 1) * sizeof(TCHAR)));
  LPITEMIDLIST pidlBrowse; // PIDL selected by user

  SHGetSpecialFolderLocation(GetActiveWindow(), hb_parni(1), &pidlBrowse);
  SHGetPathFromIDList(pidlBrowse, lpBuffer);

#ifndef UNICODE
  hb_retc(lpBuffer);
#else
  pStr = hb_osStrU16Decode(lpBuffer);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
  hb_xfree(lpBuffer);
}

// #define __WIN98__
#ifdef __WIN98__
// Based Upon Code Contributed By Jacek Kubica <kubica@wssl.wroc.pl>
// Updated by Vailton Renato <vailtom@gmail.com>
HB_FUNC(HMG_C_GETDLLSPECIALFOLDER)
{
  TCHAR szPath[MAX_PATH];
  HMODULE hModule = LoadLibrary("SHFolder.dll");

  if (hModule) {
    SHGETFOLDERPATH fnShGetFolderPath = (SHGETFOLDERPATH)wapi_GetProcAddress(hModule, "SHGetFolderPathA");

    if (fnShGetFolderPath) {
      if (fnShGetFolderPath(nullptr, hb_parni(1), nullptr, 0, szPath) == S_OK) {
        hb_retc(szPath);
      } else {
        hb_retc("");
      }
    }
    FreeLibrary(hModule);
  }
}

#endif // __WIN98__

// Memory Management Functions
using GetPhysicallyInstalledSystemMemory_ptr = BOOL(WINAPI *)(ULONGLONG *);

HB_FUNC(HMG_GETPHYSICALLYINSTALLEDSYSTEMMEMORY)
{
  HMODULE hDll = GetModuleHandle(TEXT("kernel32.dll"));

  hb_retnll(0);

  if (hDll != nullptr) {
    GetPhysicallyInstalledSystemMemory_ptr fn_GetPhysicallyInstalledSystemMemory =
        (GetPhysicallyInstalledSystemMemory_ptr)wapi_GetProcAddress(hDll, "GetPhysicallyInstalledSystemMemory");

    if (fn_GetPhysicallyInstalledSystemMemory != nullptr) {
      ULONGLONG ullTotalMemoryInKilobytes;

      if (fn_GetPhysicallyInstalledSystemMemory(&ullTotalMemoryInKilobytes)) {
        hb_retnll((HB_LONGLONG)ullTotalMemoryInKilobytes);
      }
    }
  }
}

using GlobalMemoryStatusEx_ptr = BOOL(WINAPI *)(MEMORYSTATUSEX *);
#define DIV (1024 * 1024)

HB_FUNC(HMG_MEMORYSTATUS)
{
  HMODULE hDll = GetModuleHandle(TEXT("kernel32.dll"));

  HB_RETNL(0);

  if (hDll != nullptr) {
    GlobalMemoryStatusEx_ptr fn_GlobalMemoryStatusEx =
        (GlobalMemoryStatusEx_ptr)wapi_GetProcAddress(hDll, "GlobalMemoryStatusEx");

    if (fn_GlobalMemoryStatusEx != nullptr) {
      MEMORYSTATUSEX mstex;

      mstex.dwLength = sizeof(mstex);

      if (fn_GlobalMemoryStatusEx(&mstex)) {
        switch (hb_parni(1)) {
        case 1: {
          hb_retnll(mstex.ullTotalPhys / DIV);
          break;
        }
        case 2: {
          hb_retnll(mstex.ullAvailPhys / DIV);
          break;
        }
        case 3: {
          hb_retnll(mstex.ullTotalPageFile / DIV);
          break;
        }
        case 4: {
          hb_retnll(mstex.ullAvailPageFile / DIV);
          break;
        }
        case 5: {
          hb_retnll(mstex.ullTotalVirtual / DIV);
          break;
        }
        case 6: {
          hb_retnll(mstex.ullAvailVirtual / DIV);
          break; // TODO: unnecessary break
        }
        }
      }
    } else {
      MEMORYSTATUS mst;

      mst.dwLength = sizeof(MEMORYSTATUS);
      GlobalMemoryStatus(&mst);

      switch (hb_parni(1)) {
      case 1: {
        HB_RETNL(mst.dwTotalPhys / DIV);
        break;
      }
      case 2: {
        HB_RETNL(mst.dwAvailPhys / DIV);
        break;
      }
      case 3: {
        HB_RETNL(mst.dwTotalPageFile / DIV);
        break;
      }
      case 4: {
        HB_RETNL(mst.dwAvailPageFile / DIV);
        break;
      }
      case 5: {
        HB_RETNL(mst.dwTotalVirtual / DIV);
        break;
      }
      case 6: {
        HB_RETNL(mst.dwAvailVirtual / DIV);
        break; // TODO: unnecessary break
      }
      }
    }
  }
}

// HMG_C_SHELLABOUT(HWND, cp2, cp3) --> NIL
HB_FUNC(HMG_C_SHELLABOUT)
{
  void *str1;
  void *str2;
  hb_retl(ShellAbout(hmg_par_HWND(1), HB_PARSTR(2, &str1, nullptr), HB_PARSTR(3, &str2, nullptr), hmg_par_HICON(4)));
  hb_strfree(str1);
  hb_strfree(str2);
}

// HMG_PAINTBKGND(HWND, p2) --> HANDLE
HB_FUNC(HMG_PAINTBKGND)
{
  HBRUSH hBrush;
  RECT recClie;

  auto hwnd = hmg_par_HWND(1);
  auto hdc = GetDC(hwnd);

  GetClientRect(hwnd, &recClie);

  if (hb_pcount() > 1 && !HB_ISNIL(2)) {
    hBrush = CreateSolidBrush(RGB(HB_PARNI(2, 1), HB_PARNI(2, 2), HB_PARNI(2, 3)));
    FillRect(hdc, &recClie, hBrush);
  } else {
    hBrush = reinterpret_cast<HBRUSH>((COLOR_BTNFACE + 1));
    FillRect(hdc, &recClie, hBrush);
  }

  ReleaseDC(hwnd, hdc);

  RegisterResource(hBrush, "BRUSH");
  hmg_ret_HBRUSH(hBrush);
}

// Functions Contributed  By Luiz Rafael Culik Guimaraes(culikr@uol.com.br)
HB_FUNC(HMG_GETWINDOWSDIR)
{
  TCHAR szBuffer[MAX_PATH + 1] = {0};

#ifdef UNICODE
  LPSTR pStr;
#endif

  GetWindowsDirectory(szBuffer, MAX_PATH);

#ifndef UNICODE
  hb_retc(szBuffer);
#else
  pStr = WideToAnsi(szBuffer);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

HB_FUNC(HMG_GETSYSTEMDIR)
{
  TCHAR szBuffer[MAX_PATH + 1] = {0};

#ifdef UNICODE
  LPSTR pStr;
#endif

  GetSystemDirectory(szBuffer, MAX_PATH);

#ifndef UNICODE
  hb_retc(szBuffer);
#else
  pStr = WideToAnsi(szBuffer);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

HB_FUNC(HMG_GETTEMPDIR)
{
  TCHAR szBuffer[MAX_PATH + 1] = {0};

#ifdef UNICODE
  LPSTR pStr;
#endif

  GetTempPath(MAX_PATH, szBuffer);

#ifndef UNICODE
  hb_retc(szBuffer);
#else
  pStr = WideToAnsi(szBuffer);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

// HMG_POSTMESSAGE(HWND, p2, p3, p4) --> numeric
HB_FUNC(HMG_POSTMESSAGE)
{
  hb_retnl(PostMessage(hmg_par_HWND(1), hmg_par_UINT(2), hb_parnl(3), hmg_par_LPARAM(4)));
}

// HMG_DEFWINDOWPROC(HWND, p2, p3, p4) --> numeric
HB_FUNC(HMG_DEFWINDOWPROC)
{
  HB_RETNL(static_cast<LONG_PTR>(DefWindowProc(hmg_par_HWND(1), hmg_par_UINT(2), hb_parnl(3), hmg_par_LPARAM(4))));
}

// HMG_GETSTOCKOBJECT(np) --> HANDLE
HB_FUNC(HMG_GETSTOCKOBJECT)
{
  hmg_ret_HGDIOBJ(GetStockObject(hb_parni(1)));
}

// HMG_GETNEXTDLGTABITEM(HWND1, HWND2, lp3) --> HANDLE
HB_FUNC(HMG_GETNEXTDLGTABITEM)
{
  hmg_ret_HWND(GetNextDlgTabItem(hmg_par_HWND(1), hmg_par_HWND(2), hb_parl(3)));
}

using LPFN_WOW64DISABLEWOW64FSREDIRECTION = BOOL(WINAPI *)(PVOID *);
using LPFN_WOW64REVERTWOW64FSREDIRECTION = BOOL(WINAPI *)(PVOID);

// HMG_SHELLEXECUTE(HWND, p2, p3, p4, p5) --> numeric
HB_FUNC(HMG_SHELLEXECUTE)
{
  void *str1 = nullptr;
  void *str2 = nullptr;
  void *str3 = nullptr;
  void *str4 = nullptr;
  BOOL bIsWow64 = FALSE;
  LPFN_WOW64DISABLEWOW64FSREDIRECTION fnDisable;
  PVOID OldValue = nullptr;
  auto bRestore = false;
  LPFN_WOW64REVERTWOW64FSREDIRECTION fnRevert;
  HMODULE hDll = GetModuleHandle(TEXT("kernel32.dll"));

  IsWow64Process(GetCurrentProcess(), &bIsWow64);

  if (bIsWow64) {
    fnDisable = (LPFN_WOW64DISABLEWOW64FSREDIRECTION)wapi_GetProcAddress(hDll, "Wow64DisableWow64FsRedirection");
    if (fnDisable != nullptr) {
      if (fnDisable(&OldValue)) {
        bRestore = true;
      }
    }
  }

  CoInitialize(nullptr);

  HB_RETNL(reinterpret_cast<LONG_PTR>(
      ShellExecute(hmg_par_HWND(1), HB_ISNIL(2) ? nullptr : HB_PARSTR(2, &str1, nullptr), HB_PARSTR(3, &str2, nullptr),
                   HB_ISNIL(4) ? nullptr : HB_PARSTR(4, &str3, nullptr),
                   HB_ISNIL(5) ? nullptr : HB_PARSTR(5, &str4, nullptr), hb_parni(6))));

  hb_idleSleep(1.0);

  if (bRestore) {
    fnRevert = (LPFN_WOW64REVERTWOW64FSREDIRECTION)wapi_GetProcAddress(hDll, "Wow64RevertWow64FsRedirection");
    if (fnRevert != nullptr) {
      fnRevert(OldValue);
    }
  }

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

// HMG_SHELLEXECUTEEX(HWND, cOperation, cFile, cParameters, cDirectory, np6) --> HANDLE
HB_FUNC(HMG_SHELLEXECUTEEX)
{
  void *str1 = nullptr;
  void *str2 = nullptr;
  void *str3 = nullptr;
  void *str4 = nullptr;

  SHELLEXECUTEINFO SHExecInfo{};
  SHExecInfo.cbSize = sizeof(SHExecInfo);
  SHExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
  SHExecInfo.hwnd = HB_ISNIL(1) ? GetActiveWindow() : hmg_par_HWND(1);
  SHExecInfo.lpVerb = HB_ISNIL(2) ? nullptr : HB_PARSTR(2, &str1, nullptr);
  SHExecInfo.lpFile = HB_PARSTR(3, &str2, nullptr);
  SHExecInfo.lpParameters = HB_ISNIL(4) ? nullptr : HB_PARSTR(4, &str3, nullptr);
  SHExecInfo.lpDirectory = HB_ISNIL(5) ? nullptr : HB_PARSTR(5, &str4, nullptr);
  SHExecInfo.nShow = hb_parni(6);

  hmg_ret_HANDLE(ShellExecuteEx(&SHExecInfo) ? SHExecInfo.hProcess : nullptr);

  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

// HMG_WAITRUN(cp1, np2) --> numeric
HB_FUNC(HMG_WAITRUN)
{
  DWORD dwExitCode;
  PROCESS_INFORMATION prInfo;

  STARTUPINFO stInfo{};
  stInfo.cb = sizeof(stInfo);
  stInfo.dwFlags = STARTF_USESHOWWINDOW;
  stInfo.wShowWindow = hmg_par_WORD(2);

  void *str;
  BOOL bResult = CreateProcess(nullptr, (LPTSTR)HB_PARSTR(1, &str, nullptr), nullptr, nullptr, TRUE,
                               CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS, nullptr, nullptr, &stInfo, &prInfo);
  hb_strfree(str);

  if (!bResult) {
    hb_retnl(-1);
    return;
  }

  WaitForSingleObject(prInfo.hProcess, INFINITE);
  GetExitCodeProcess(prInfo.hProcess, &dwExitCode);
  CloseHandle(prInfo.hThread);
  CloseHandle(prInfo.hProcess);
  hb_retnl(dwExitCode);
}

// WaitRunTerm contributed by Kevin Carmody (i@kevincarmody.com) 2007.11.16

// HMG_WAITRUNTERM(cCommandLine, cCurrentDirectory, nShowWindow, bWaitProc, nWaitMsec) --> numeric
HB_FUNC(HMG_WAITRUNTERM)
{
  auto pWaitProc = hb_param(4, Harbour::Item::BLOCK);
  BOOL bTerm = FALSE;
  DWORD dwExitCode;
  PROCESS_INFORMATION prInfo;

  STARTUPINFO stInfo{};
  stInfo.cb = sizeof(stInfo);
  stInfo.dwFlags = STARTF_USESHOWWINDOW;
  stInfo.wShowWindow = static_cast<WORD>(HB_ISNIL(3) ? 5 : hb_parni(3));

  void *str1 = nullptr;
  void *str2 = nullptr;
  BOOL bResult = CreateProcess(nullptr, (LPTSTR)HB_PARSTR(1, &str1, nullptr), nullptr, nullptr, TRUE,
                               CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS, nullptr,
                               HB_ISNIL(2) ? nullptr : HB_PARSTR(2, &str2, nullptr), &stInfo, &prInfo);
  hb_strfree(str1);
  hb_strfree(str2);

  if (!bResult) {
    hb_retnl(-2);
    return;
  }

  if (pWaitProc != nullptr) {
    ULONG ulNoSignal;
    ULONG ulWaitMsec = (HB_ISNIL(5) ? 2000 : hb_parnl(5));
    BOOL bWait;

    do {
      ulNoSignal = WaitForSingleObject(prInfo.hProcess, ulWaitMsec);

      if (ulNoSignal) {
        hb_evalBlock0(pWaitProc);
        bWait = hb_parl(-1);
        if (!bWait) {
          if (TerminateProcess(prInfo.hProcess, 0) != 0) {
            bTerm = TRUE;
          } else {
            bWait = TRUE;
          }
        }
      } else {
        bWait = FALSE;
      }
    } while (bWait);
  } else {
    WaitForSingleObject(prInfo.hProcess, INFINITE);
  }

  if (bTerm) {
    dwExitCode = static_cast<DWORD>(-1);
  } else {
    GetExitCodeProcess(prInfo.hProcess, &dwExitCode);
  }

  CloseHandle(prInfo.hThread);
  CloseHandle(prInfo.hProcess);
  hb_retnl(dwExitCode);
}

// HMG_ISEXERUNNING(cp1) --> .T.|.F.
HB_FUNC(HMG_ISEXERUNNING) // ( cExeNameCaseSensitive ) --> lResult
{
  void *str;
  HANDLE hMutex = CreateMutex(nullptr, FALSE, HB_PARSTR(1, &str, nullptr));
  hb_retl(GetLastError() == ERROR_ALREADY_EXISTS);

  if (hMutex != nullptr) {
    ReleaseMutex(hMutex);
  }

  hb_strfree(str);
}

// HMG_SETSCROLLPOS(HWND, np2, np3, lp4) --> numeric
HB_FUNC(HMG_SETSCROLLPOS)
{
  hb_retni(SetScrollPos(hmg_par_HWND(1), hb_parni(2), hb_parni(3), hb_parl(4)));
}

// HMG_GETLASTERROR() --> numeric
HB_FUNC(HMG_GETLASTERROR)
{
  hb_retnl(GetLastError());
}

// HMG_CREATEFOLDER(cPathName) --> .T.|.F.
HB_FUNC(HMG_CREATEFOLDER)
{
  void *str;
  hb_retl(CreateDirectory(HB_PARSTR(1, &str, nullptr), nullptr));
  hb_strfree(str);
}

// HMG_SETCURRENTFOLDER(cPathName) --> .T.|.F.
HB_FUNC(HMG_SETCURRENTFOLDER)
{
  void *str;
  hb_retl(SetCurrentDirectory(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

// HMG_REMOVEFOLDER(cPathName) --> .T.|.F.
HB_FUNC(HMG_REMOVEFOLDER)
{
  void *str;
  hb_retl(RemoveDirectory(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

HB_FUNC(HMG_GETCURRENTFOLDER)
{
  TCHAR Path[MAX_PATH + 1] = {0};

#ifdef UNICODE
  LPSTR pStr;
#endif
  GetCurrentDirectory(MAX_PATH, Path);
#ifndef UNICODE
  hb_retc(Path);
#else
  pStr = WideToAnsi(Path);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

// HMG_CREATESOLIDBRUSH(nRed, nGreen, nBlue) --> HANDLE
HB_FUNC(HMG_CREATESOLIDBRUSH)
{
  auto hBrush = CreateSolidBrush(static_cast<COLORREF>(RGB(hb_parni(1), hb_parni(2), hb_parni(3))));
  RegisterResource(hBrush, "BRUSH");
  hmg_ret_HBRUSH(hBrush);
}

// HMG_SETTEXTCOLOR(HDC, nRed, nGreen, nBlue) --> numeric
HB_FUNC(HMG_SETTEXTCOLOR)
{
  hb_retnl(static_cast<ULONG>(
      SetTextColor(hmg_par_HDC(1), static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4))))));
}

// HMG_SETBKCOLOR(HDC, nRed, nGreen, nBlue) --> numeric
HB_FUNC(HMG_SETBKCOLOR)
{
  hb_retnl(static_cast<ULONG>(
      SetBkColor(hmg_par_HDC(1), static_cast<COLORREF>(RGB(hb_parni(2), hb_parni(3), hb_parni(4))))));
}

// HMG_GETSYSCOLOR(np1) --> numeric
HB_FUNC(HMG_GETSYSCOLOR) // TODO: deprecated (using waGetSysColor from WinApi library)
{
  hb_retnl(GetSysColor(hb_parni(1)));
}

// This function returns the Windows Version on which the app calling the function
// is running.
//
// The return value is an 4-th dimensinal array containing the OS in the first,
// the servicepack or the system release number in the second, the build number
// in the third and extended OS information in the fourth array element.

// HMG_WINVERSION() --> array
HB_FUNC(HMG_WINVERSION)
{
#if defined(__BORLANDC__)
#define VER_SUITE_PERSONAL 0x00000200
#define VER_SUITE_BLADE 0x00000400
#endif

#ifdef UNICODE
  std::wstring szVersion;
  std::wstring szServicePack;
  std::wstring szBuild;
  std::wstring szVersionEx;
#else
  std::string szVersion;
  std::string szServicePack;
  std::string szBuild;
  std::string szVersionEx;
#endif

  OSVERSIONINFOEX osvi{};
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

  BOOL bOsVersionInfoEx = GetVersionEx((OSVERSIONINFO *)&osvi);
  if (!bOsVersionInfoEx) {
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    if (!GetVersionEx((OSVERSIONINFO *)&osvi)) {
      szVersion = TEXT("Unknown Operating System");
    }
  }

  if (szVersion.empty()) {
    switch (osvi.dwPlatformId) {
    case VER_PLATFORM_WIN32_NT: {
      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
        szVersion = TEXT("Windows Server 2003 family ");
      }

      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1) {
        szVersion = TEXT("Windows XP ");
      }

      if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0) {
        szVersion = TEXT("Windows 2000 ");
      }

      if (osvi.dwMajorVersion <= 4) {
        szVersion = TEXT("Windows NT ");
      }

      if (bOsVersionInfoEx) {
        if (osvi.wProductType == VER_NT_WORKSTATION) {
          if (osvi.dwMajorVersion == 10 && osvi.dwBuildNumber == 22000) {
            szVersion = TEXT("Windows 11 ");
          } else if (osvi.dwMajorVersion == 10 && osvi.dwMinorVersion == 0) {
            szVersion = TEXT("Windows 10 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 3) {
            szVersion = TEXT("Windows 8.1 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 2) {
            szVersion = TEXT("Windows 8 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1) {
            szVersion = TEXT("Windows 7 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0) {
            szVersion = TEXT("Windows Vista ");
          }

          if (osvi.dwMajorVersion == 4) {
            szVersionEx = TEXT("Workstation 4.0 ");
          } else if (osvi.wSuiteMask & VER_SUITE_PERSONAL) {
            szVersionEx = TEXT("Home Edition ");
          } else {
            szVersionEx = TEXT("Professional ");
          }
        } else if (osvi.wProductType == VER_NT_SERVER) {
          if (osvi.dwMajorVersion == 10 && osvi.dwMinorVersion == 0) {
            szVersion = TEXT("Windows Server 2016 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 3) {
            szVersion = TEXT("Windows Server 2012 R2 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 2) {
            szVersion = TEXT("Windows Server 2012 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1) {
            szVersion = TEXT("Windows Server 2008 R2 ");
          } else if (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0) {
            szVersion = TEXT("Windows Server 2008 ");
          } else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
            if (osvi.wSuiteMask & VER_SUITE_DATACENTER) {
              szVersionEx = TEXT("Datacenter Edition ");
            } else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE) {
              szVersionEx = TEXT("Enterprise Edition ");
            } else if (osvi.wSuiteMask & VER_SUITE_BLADE) {
              szVersionEx = TEXT("Web Edition ");
            } else {
              szVersionEx = TEXT("Standard Edition ");
            }
          } else if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0) {
            if (osvi.wSuiteMask & VER_SUITE_DATACENTER) {
              szVersionEx = TEXT("Datacenter Server ");
            } else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE) {
              szVersionEx = TEXT("Advanced Server ");
            } else {
              szVersionEx = TEXT("Server ");
            }
          } else {
            if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE) {
              szVersionEx = TEXT("Server 4.0, Enterprise Edition ");
            } else {
              szVersionEx = TEXT("Server 4.0 ");
            }
          }
        }
      } else {
        HKEY hKey;
        TCHAR szProductType[80];
        DWORD dwBufLen = 80;

        LONG lRetVal = RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("SYSTEM\\CurrentControlSet\\Control\\ProductOptions"), 0,
                                    KEY_QUERY_VALUE, &hKey);

        if (lRetVal != ERROR_SUCCESS) {
          szVersion = TEXT("Unknown Operating System");
        } else {
          lRetVal = RegQueryValueEx(hKey, TEXT("ProductType"), nullptr, nullptr,
                                    reinterpret_cast<LPBYTE>(szProductType), &dwBufLen);
          if ((lRetVal != ERROR_SUCCESS) || (dwBufLen > 80)) {
            szVersion = TEXT("Unknown Operating System");
          }
        }

        RegCloseKey(hKey);

        if (szVersion.compare(TEXT("Unknown Operating System")) != 0) {
          if (lstrcmpi(szProductType, TEXT("WINNT")) == 0) {
            szVersionEx = TEXT("Workstation ");
          }

          if (lstrcmpi(szProductType, TEXT("LANMANNT")) == 0) {
            szVersionEx = TEXT("Server ");
          }

          if (lstrcmpi(szProductType, TEXT("SERVERNT")) == 0) {
            szVersionEx = TEXT("Advanced Server ");
          }

#ifdef UNICODE
          szVersion.append(std::to_wstring(osvi.dwMajorVersion));
          szVersion.append(TEXT("."));
          szVersion.append(std::to_wstring(osvi.dwMinorVersion));
#else
          szVersion.append(std::to_string(osvi.dwMajorVersion));
          szVersion.append(TEXT("."));
          szVersion.append(std::to_string(osvi.dwMinorVersion));
#endif
        }
      }

      if (osvi.dwMajorVersion == 4 && lstrcmpi(osvi.szCSDVersion, TEXT("Service Pack 6")) == 0) {
        HKEY hKey;

        LONG lRetVal =
            RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Hotfix\\Q246009"),
                         0, KEY_QUERY_VALUE, &hKey);

        if (lRetVal == ERROR_SUCCESS) {
          szServicePack = TEXT("Service Pack 6a");
#ifdef UNICODE
          szBuild = std::to_wstring(osvi.dwBuildNumber & 0xFFFF);
#else
          szBuild = std::to_string(osvi.dwBuildNumber & 0xFFFF);
#endif
        } else {
          szServicePack = osvi.szCSDVersion;
#ifdef UNICODE
          szBuild = std::to_wstring(osvi.dwBuildNumber & 0xFFFF);
#else
          szBuild = std::to_string(osvi.dwBuildNumber & 0xFFFF);
#endif
        }

        RegCloseKey(hKey);
      } else {
        szServicePack = osvi.szCSDVersion;
#ifdef UNICODE
        szBuild = std::to_wstring(osvi.dwBuildNumber & 0xFFFF);
#else
        szBuild = std::to_string(osvi.dwBuildNumber & 0xFFFF);
#endif
      }
      break;
    }
    case VER_PLATFORM_WIN32_WINDOWS: {
      if ((osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion == 0)) {
        if (osvi.szCSDVersion[1] == 'B') {
          szVersion = TEXT("Windows 95 B");
          szServicePack = TEXT("OSR2");
        } else {
          if (osvi.szCSDVersion[1] == 'C') {
            szVersion = TEXT("Windows 95 C");
            szServicePack = TEXT("OSR2");
          } else {
            szVersion = TEXT("Windows 95");
            szServicePack = TEXT("OSR1");
          }
        }

#ifdef UNICODE
        szBuild = std::to_wstring(osvi.dwBuildNumber & 0x0000FFFF);
#else
        szBuild = std::to_string(osvi.dwBuildNumber & 0x0000FFFF);
#endif
      }

      if ((osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion == 10)) {
        if (osvi.szCSDVersion[1] == 'A') {
          szVersion = TEXT("Windows 98 A");
          szServicePack = TEXT("Second Edition");
        } else {
          szVersion = TEXT("Windows 98");
          szServicePack = TEXT("First Edition");
        }

#ifdef UNICODE
        szBuild = std::to_wstring(osvi.dwBuildNumber & 0x0000FFFF);
#else
        szBuild = std::to_string(osvi.dwBuildNumber & 0x0000FFFF);
#endif
      }

      if ((osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion == 90)) {
        szVersion = TEXT("Windows ME");
#ifdef UNICODE
        szBuild = std::to_wstring(osvi.dwBuildNumber & 0x0000FFFF);
#else
        szBuild = std::to_string(osvi.dwBuildNumber & 0x0000FFFF);
#endif
      }
      break; // TODO: unnecessary break
    }
    }
  }

  hb_reta(4);
  HB_ARRAYSETSTR(hb_stackReturnItem(), 1, szVersion.c_str());
  HB_ARRAYSETSTR(hb_stackReturnItem(), 2, szServicePack.c_str());
  HB_ARRAYSETSTR(hb_stackReturnItem(), 3, szBuild.c_str());
  HB_ARRAYSETSTR(hb_stackReturnItem(), 4, szVersionEx.c_str());
}

// HMG_GETDLLVERSION() --> array
HB_FUNC(HMG_GETDLLVERSION)
{
  DWORD dwMajorVersion = 0;
  DWORD dwMinorVersion = 0;
  DWORD dwBuildNumber = 0;

  void *str;
  HMODULE hModule = LoadLibrary(HB_PARSTR(1, &str, nullptr));
  hb_strfree(str);

  if (hModule != nullptr) {
    DLLGETVERSIONPROC fnDllGetVersion;

    fnDllGetVersion = (DLLGETVERSIONPROC)wapi_GetProcAddress(hModule, "DllGetVersion");

    if (fnDllGetVersion != nullptr) {
      DLLVERSIONINFO dvi{};
      dvi.cbSize = sizeof(dvi);

      if (fnDllGetVersion(&dvi) == S_OK) {
        dwMajorVersion = dvi.dwMajorVersion;
        dwMinorVersion = dvi.dwMinorVersion;
        dwBuildNumber = dvi.dwBuildNumber;
      }
    } else {
      MessageBox(nullptr, TEXT("Cannot get DllGetVersion function."), TEXT("DllGetVersion"), MB_OK | MB_ICONERROR);
    }

    FreeLibrary(hModule);
  }

  hb_reta(3);
  HB_STORVNL(dwMajorVersion, -1, 1);
  HB_STORVNL(dwMinorVersion, -1, 2);
  HB_STORVNL(dwBuildNumber, -1, 3);
}

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.0 Experimental Build 9a

// HMG_SELECTOBJECT(HDC, HGDIOBJ) --> HANDLE
HB_FUNC(HMG_SELECTOBJECT)
{
  hmg_ret_HGDIOBJ(SelectObject(hmg_par_HDC(1), hmg_par_HGDIOBJ(2)));
}

// HMG_FILLRECT(HWND|HDC, aRect, HBRUSH) --> numeric
// HMG_FILLRECT(HWND|HDC, nLeft, nTop, nRight, nBottom, HBRUSH) --> numeric
HB_FUNC(HMG_FILLRECT)
{
  auto hWnd = hmg_par_HWND(1);
  HDC hDC;
  auto bDC = false;

  if (IsWindow(hWnd)) {
    hDC = GetDC(hWnd);
    bDC = true;
  } else {
    hDC = hmg_par_HDC(1);
  }

  if (GetObjectType(static_cast<HGDIOBJ>(hDC)) == OBJ_DC) {
    RECT rc;
    auto iParam = 6;

    if (hmg_ArrayToRect(hb_param(2, Harbour::Item::ANY), &rc)) {
      iParam = 3;
    } else {
      rc.left = hb_parni(2);
      rc.top = hb_parni(3);
      rc.right = hb_parni(4);
      rc.bottom = hb_parni(5);
    }

    hb_retni(FillRect(hDC, &rc, hmg_par_HBRUSH(iParam)));

    if (bDC) {
      ReleaseDC(hWnd, hDC);
    }
  } else {
    hb_retni(0);
  }
}

#if defined(__MINGW32__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif // __MINGW32__

BOOL IsAppHung(IN HWND hWnd, OUT PBOOL pbHung)
{
  OSVERSIONINFO osvi;
  HINSTANCE hUser;

  if (!IsWindow(hWnd)) {
    return SetLastError(ERROR_INVALID_PARAMETER), FALSE;
  }

  osvi.dwOSVersionInfoSize = sizeof(osvi);

  // detect OS version
  GetVersionEx(&osvi);

  // get handle of USER32.DLL
  hUser = GetModuleHandle(TEXT("user32.dll"));

  if (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT) {
    BOOL(WINAPI * _IsHungAppWindow)(HWND);

    // found the function IsHungAppWindow
    *(FARPROC *)&_IsHungAppWindow = GetProcAddress(hUser, "IsHungAppWindow");
    if (_IsHungAppWindow == nullptr) {
      return SetLastError(ERROR_PROC_NOT_FOUND), FALSE;
    }

    // call the function IsHungAppWindow
    *pbHung = _IsHungAppWindow(hWnd);
  } else {
    DWORD dwThreadId = GetWindowThreadProcessId(hWnd, nullptr);

    BOOL(WINAPI * _IsHungThread)(DWORD);

    // found the function IsHungThread
    *(FARPROC *)&_IsHungThread = GetProcAddress(hUser, "IsHungThread");
    if (_IsHungThread == nullptr) {
      return SetLastError(ERROR_PROC_NOT_FOUND), FALSE;
    }

    // call the function IsHungThread
    *pbHung = _IsHungThread(dwThreadId);
  }

  return TRUE;
}

#if defined(__MINGW32__)
#pragma GCC diagnostic pop
#endif // __MINGW32__

HB_FUNC(HMG_ISAPPHUNG)
{
  BOOL bIsHung;

  if (IsAppHung(hmg_par_HWND(1), &bIsHung)) {
    hb_retl(bIsHung);
  } else {
    if (GetLastError() != ERROR_INVALID_PARAMETER) {
      MessageBox(nullptr, TEXT("Process not found"), TEXT("Warning"), MB_OK | MB_ICONWARNING);
    }
    hb_retl(false);
  }
}

#ifndef PROCESS_QUERY_LIMITED_INFORMATION
#define PROCESS_QUERY_LIMITED_INFORMATION (0x1000)
#endif

// EmptyWorkingSet([ProcessID]) ---> lBoolean
HB_FUNC(HMG_EMPTYWORKINGSET)
{
  // It removes as many pages as possible from the process working set (clean the working set
  // memory). This operation is useful primarily for testing and tuning.
  DWORD ProcessID;
  HANDLE hProcess;

  using Func_EmptyWorkingSet = BOOL(WINAPI *)(HANDLE);
  static Func_EmptyWorkingSet pEmptyWorkingSet = nullptr;

  if (pEmptyWorkingSet == nullptr) {
    HMODULE hLib = LoadLibrary(TEXT("Kernel32.dll"));
    pEmptyWorkingSet = (Func_EmptyWorkingSet)wapi_GetProcAddress(hLib, "K32EmptyWorkingSet");
  }

  if (pEmptyWorkingSet == nullptr) {
    HMODULE hLib = LoadLibrary(TEXT("Psapi.dll"));
    pEmptyWorkingSet = (Func_EmptyWorkingSet)wapi_GetProcAddress(hLib, "K32EmptyWorkingSet");
  }

  if (pEmptyWorkingSet != nullptr) {
    ProcessID = HB_ISNUM(1) ? hmg_par_DWORD(1) : GetCurrentProcessId();

    hProcess = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | PROCESS_SET_QUOTA, FALSE, ProcessID);
    if (hProcess != nullptr) {
      hb_retl(static_cast<BOOL>(pEmptyWorkingSet(hProcess)));

      CloseHandle(hProcess);
    } else {
      hb_retl(false);
    }
  } else {
    hb_retl(false);
  }
}

// Grigory Filatov <gfilatov@gmail.com> HMG 1.1 Experimental Build 10d
HB_FUNC(HMG_CLEANPROGRAMMEMORY)
{
  hb_retl(SetProcessWorkingSetSize(GetCurrentProcess(), (SIZE_T)-1, (SIZE_T)-1));
}

// Grigory Filatov <gfilatov@gmail.com> HMG 1.1 Experimental Build 11a
using _GETCOMPACTPATH = INT(WINAPI *)(LPTSTR pszOut, LPTSTR pszSrc, INT cchMax, DWORD dwFlags);

HB_FUNC(HMG_GETCOMPACTPATH)
{
  HINSTANCE handle = LoadLibrary(TEXT("shlwapi.dll"));

  if (handle) {
    _GETCOMPACTPATH pFunc;
    pFunc = (_GETCOMPACTPATH)wapi_GetProcAddress(handle, "PathCompactPathExA");
    hb_retni(pFunc(const_cast<LPTSTR>(hb_parc(1)), const_cast<LPTSTR>(hb_parc(2)), hmg_par_INT(3), hmg_par_DWORD(4)));
    FreeLibrary(handle);
  }
}

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.1 Experimental Build 11a
HB_FUNC(HMG_GETSHORTPATHNAME)
{
  HB_SIZE iRet;

#ifndef UNICODE
  char buffer[MAX_PATH + 1] = {0};
  LPCSTR lpszLongPath = hb_parc(1);
#else
  TCHAR buffer[MAX_PATH + 1] = {0};
  LPCWSTR lpszLongPath = AnsiToWide(const_cast<char *>(hb_parc(1)));
  LPSTR pStr;
#endif

  iRet = GetShortPathName(lpszLongPath, buffer, MAX_PATH);
  if (iRet < MAX_PATH) {
#ifndef UNICODE
    hb_retni(hb_storclen(buffer, (HB_SIZE)iRet, 2));
#else
    pStr = WideToAnsi(buffer);
    hb_retni(hb_storclen(pStr, (HB_SIZE)iRet, 2));
    hb_xfree(pStr);
#endif
  } else {
    hb_retni(hb_storc("", 2));
  }

#ifdef UNICODE
  hb_xfree((TCHAR *)lpszLongPath);
#endif
}

// HMG_DRAWTEXT(HDC, cText, nLeft, nTop, nRight, nBottom, nStyle) --> NIL
HB_FUNC(HMG_DRAWTEXT)
{
#ifndef UNICODE
  LPCSTR lpchText = hb_parc(2);
#else
  LPCWSTR lpchText = AnsiToWide(const_cast<char *>(hb_parc(2)));
#endif
  RECT rc;

  rc.left = hb_parni(3);
  rc.top = hb_parni(4);
  rc.right = hb_parni(5);
  rc.bottom = hb_parni(6);

  DrawText(hmg_par_HDC(1),    // device context
           lpchText,          // pointer to string
           lstrlen(lpchText), // length of  string
           &rc,               // rectangle
           hb_parni(7)        // draw style
  );

#ifdef UNICODE
  hb_xfree((TCHAR *)lpchText);
#endif
}

HB_FUNC(HMG_GETTEXTMETRIC)
{
  TEXTMETRIC tm;
  auto aMetr = hb_itemArrayNew(7);

  if (GetTextMetrics(hmg_par_HDC(1), // handle of device context
                     &tm             // address of text metrics structure
                     )) {
    // tmHeight
    // Specifies the height (ascent + descent) of characters.
    HB_arraySetNL(aMetr, 1, tm.tmHeight);

    // tmAveCharWidth Specifies the average width of characters in the font
    //(generally defined as the width of the letter x).
    // This value does not include the overhang required for bold or italic characters.
    HB_arraySetNL(aMetr, 2, tm.tmAveCharWidth);

    // tmMaxCharWidth
    // Specifies the width of the widest character in the font.
    HB_arraySetNL(aMetr, 3, tm.tmMaxCharWidth);

    // tmAscent
    // Specifies the ascent (units above the base line) of characters.
    HB_arraySetNL(aMetr, 4, tm.tmAscent);

    // tmDescent
    // Specifies the descent (units below the base line) of characters.
    HB_arraySetNL(aMetr, 5, tm.tmDescent);

    // tmInternalLeading
    // Specifies the amount of leading (space) inside the bounds set by the tmHeight member.
    // Accent marks and other diacritical characters may occur in this area.
    // The designer may set this member to zero.
    HB_arraySetNL(aMetr, 6, tm.tmInternalLeading);

    // tmExternalLeading
    // The amount of extra leading (space) that the application adds between rows.
    // Since this area is outside the font, it contains no marks and is not altered by text
    // output calls in either OPAQUE or TRANSPARENT mode.
    // The designer may set this member to zero.
    HB_arraySetNL(aMetr, 7, tm.tmExternalLeading);
  }

  hb_itemReturnRelease(aMetr);
}

HB_FUNC(HMG__GETCLIENTRECT)
{
  RECT rc;
  auto hWnd = hmg_par_HWND(1);

  if (IsWindow(hWnd)) {
    GetClientRect(hWnd, &rc);

    hb_itemReturnRelease(hmg_RectToArray(&rc));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError(1));
  }
}

// Grigory Filatov <gfilatov@gmail.com> HMG 1.1 Experimental Build 17d
HB_FUNC(HMG_ISOEMTEXT)
{
  auto pString = reinterpret_cast<LPBYTE>(const_cast<char *>(hb_parc(1)));
  WORD w = 0;
  auto wLen = static_cast<WORD>(hb_parclen(1));
  BOOL bOem = FALSE;

  while (w < wLen && !bOem) {
    bOem = pString[w] >= 128 && pString[w] <= 168;
    w++;
  }

  hb_retl(bOem);
}

// Harbour MiniGUI 1.3 Extended (Build 33)
// added by P.Chornyj
//
// Function GetObjectType()
// ------------------------
// The GetObjectType identifies the type of the specified object.
//
// Syntax
//   GetObjectType(nObject) --> nType
//
// Arguments
//   nObject is identifies the object
//
// Returns
//   If the function succeeds, the return value identifies the object.
// This value can be one of the following:
//   OBJ_PEN         1
//   OBJ_BRUSH       2
//   OBJ_DC          3
//   OBJ_METADC      4
//   OBJ_PAL         5
//   OBJ_FONT        6
//   OBJ_BITMAP      7
//   OBJ_REGION      8
//   OBJ_METAFILE    9
//   OBJ_MEMDC       10
//   OBJ_EXTPEN      11
//   OBJ_ENHMETADC   12
//   OBJ_ENHMETAFILE 13
//   OBJ_COLORSPACE  14
HB_FUNC(HMG_GETOBJECTTYPE)
{
  HB_RETNL(static_cast<LONG_PTR>(GetObjectType(hmg_par_HGDIOBJ(1))));
}

// Harbour MiniGUI 1.4 Extended (Build 47)
// added by Grigory Filatov
HB_FUNC(HMG_DRAGACCEPTFILES)
{
  DragAcceptFiles(hmg_par_HWND(1), hb_parl(2));
}

HB_FUNC(HMG_DRAGQUERYFILES)
{
  HDROP hDrop = (HDROP)HB_PARNL(1);
  int iFiles = DragQueryFile(hDrop, static_cast<UINT>(-1), 0, 0);
  TCHAR bBuffer[250];

#ifdef UNICODE
  LPSTR pStr;
#endif

  hb_reta(iFiles);

  for (auto i = 0; i < iFiles; i++) {
    DragQueryFile(hDrop, i, (TCHAR *)bBuffer, 249);
#ifndef UNICODE
    HB_STORC((TCHAR *)bBuffer, -1, i + 1);
#else
    pStr = WideToAnsi(bBuffer);
    HB_STORC(pStr, -1, i + 1);
    hb_xfree(pStr);
#endif
  }
}

HB_FUNC(HMG_DRAGFINISH)
{
  DragFinish((HDROP)HB_PARNL(1));
}

// HMG_CHARSETNAME() --> "UNICODE"|"ANSI"
HB_FUNC(HMG_CHARSETNAME)
{
#ifdef UNICODE
  HB_RETSTR(TEXT("UNICODE"));
#else
  hb_retc("ANSI");
#endif
}

// HMG_GETLOCALEINFO(npar) --> string
HB_FUNC(HMG_GETLOCALEINFO)
{
  auto LCType = hb_parni(1);

#ifndef UNICODE
  LPSTR cText;
#else
  LPWSTR cText;
  LPSTR pStr;
#endif

  cText = static_cast<LPTSTR>(hb_xgrab(HB_FILE_TYPE_MAX)); // TODO: auto

  GetLocaleInfo(LOCALE_USER_DEFAULT, LCType, cText, HB_FILE_TYPE_MAX);

#ifdef UNICODE
  pStr = WideToAnsi(cText);
  hb_retc(pStr);
  hb_xfree(pStr);
#else
  hb_retc(cText);
#endif
  hb_xfree(cText);
}

// Description:
// Creates the actual 'lnk' file (assumes COM has been initialized).
//
// Parameters:
// pszTargetfile    - File name of the link's target, must be a non-empty
//                   string.
//
// pszTargetargs    - Command line arguments passed to link's target, may
//                   be an empty string.
//
// pszLinkfile      - File name of the actual link file, must be a non-empty
//                   string.
//
// pszDescription   - Description of the linked item. If this is an empty
//                   string the description is not set.
//
// iShowmode        - ShowWindow() constant for the link's target. Use one of:
//                     1 (SW_SHOWNORMAL) = Normal window.
//                     3 (SW_SHOWMAXIMIZED) = Maximized.
//                     7 (SW_SHOWMINNOACTIVE) = Minimized.
//                   If this is zero the showmode is not set.
//
// pszCurdir        - Working directory of the active link. If this is
//                   an empty string the directory is not set.
//
// pszIconfile      - File name of the icon file used for the link.
//                   If this is an empty string the icon is not set.
//
// iIconindex       - Index of the icon in the icon file. If this is
//                   < 0 the icon is not set.
//
// Returns:
// HRESULT value >= 0 for success, < 0 for failure.
#ifndef UNICODE
static HRESULT CreateShortCut(LPSTR pszTargetfile, LPSTR pszTargetargs, LPSTR pszLinkfile, LPSTR pszDescription,
                              int iShowmode, LPSTR pszCurdir, LPSTR pszIconfile, int iIconindex)
#else
static HRESULT CreateShortCut(LPWSTR pszTargetfile, LPWSTR pszTargetargs, LPWSTR pszLinkfile, LPWSTR pszDescription,
                              int iShowmode, LPWSTR pszCurdir, LPWSTR pszIconfile, int iIconindex)
#endif
{
  HRESULT hRes;               // Returned COM result code
  IShellLink *pShellLink;     // IShellLink object pointer
  IPersistFile *pPersistFile; // IPersistFile object pointer
  WORD wszLinkfile[MAX_PATH]; // pszLinkfile as Unicode string

  hRes = E_INVALIDARG;
  if ((pszTargetfile != nullptr) && (lstrlen(pszTargetfile) > 0) && (pszTargetargs != nullptr) &&
      (pszLinkfile != nullptr) && (lstrlen(pszLinkfile) > 0) && (pszDescription != nullptr) && (iShowmode >= 0) &&
      (pszCurdir != nullptr) && (pszIconfile != nullptr) && (iIconindex >= 0)) {
    hRes = CoCreateInstance(CLSID_ShellLink,        // pre-defined CLSID of the IShellLink object
                            nullptr,                // pointer to parent interface if part of aggregate
                            CLSCTX_INPROC_SERVER,   // caller and called code are in same process
                            IID_IShellLink,         // pre-defined interface of the IShellLink object
                            (LPVOID *)&pShellLink); // Returns a pointer to the IShellLink object
    if (SUCCEEDED(hRes)) {
      // Set the fields in the IShellLink object
      pShellLink->lpVtbl->SetPath(pShellLink, pszTargetfile);
      pShellLink->lpVtbl->SetArguments(pShellLink, pszTargetargs);
      if (lstrlen(pszDescription) > 0) {
        pShellLink->lpVtbl->SetDescription(pShellLink, pszDescription);
      }
      if (iShowmode > 0) {
        pShellLink->lpVtbl->SetShowCmd(pShellLink, iShowmode);
      }
      if (lstrlen(pszCurdir) > 0) {
        pShellLink->lpVtbl->SetWorkingDirectory(pShellLink, pszCurdir);
      }
      if (lstrlen(pszIconfile) > 0 && iIconindex >= 0) {
        pShellLink->lpVtbl->SetIconLocation(pShellLink, pszIconfile, iIconindex);
      }

      // Use the IPersistFile object to save the shell link
      hRes = pShellLink->lpVtbl->QueryInterface(
          pShellLink,               // existing IShellLink object
          IID_IPersistFile,         // pre-defined interface of the IPersistFile object
          (LPVOID *)&pPersistFile); // returns a pointer to the IPersistFile object
      if (SUCCEEDED(hRes)) {
#ifndef UNICODE
        MultiByteToWideChar(CP_ACP, 0, pszLinkfile, -1, reinterpret_cast<LPWSTR>(wszLinkfile), MAX_PATH);
#else
        lstrcpy(reinterpret_cast<LPWSTR>(wszLinkfile), pszLinkfile);
#endif
        hRes = pPersistFile->lpVtbl->Save(pPersistFile, reinterpret_cast<LPCOLESTR>(wszLinkfile), TRUE);
        pPersistFile->lpVtbl->Release(pPersistFile);
      }
      pShellLink->lpVtbl->Release(pShellLink);
    }
  }
  return hRes;
}

//*************************************************************************

HB_FUNC(HMG_CREATELINK)
{
  int iShowmode;  // <Showmode> (optional)
  int iIconindex; // <Iconindex> (optional)
  HRESULT hRes;   // result of calling COM functions

#ifndef UNICODE
  LPSTR szTargetfile;  // <Targetfile>
  LPSTR szTargetargs;  // <Targetargs>
  LPSTR szLinkfile;    // <Linkfile>
  LPSTR szDescription; // <Description>
  LPSTR szCurdir;      // <Curdir> (optional)
  LPSTR szIconfile;    // <Iconfile> (optional)
  szTargetfile = const_cast<char *>(hb_parc(1));
  szTargetargs = HB_ISCHAR(2) ? const_cast<char *>(hb_parc(2)) : const_cast<char *>("");
  szLinkfile = const_cast<char *>(hb_parc(3));
  szDescription = HB_ISCHAR(4) ? const_cast<char *>(hb_parc(4)) : const_cast<char *>("");
  szCurdir = HB_ISCHAR(6) ? const_cast<char *>(hb_parc(6)) : const_cast<char *>("");
  szIconfile = HB_ISCHAR(7) ? const_cast<char *>(hb_parc(7)) : const_cast<char *>("");
#else
  LPWSTR szTargetfile;  // <Targetfile>
  LPWSTR szTargetargs;  // <Targetargs>
  LPWSTR szLinkfile;    // <Linkfile>
  LPWSTR szDescription; // <Description>
  LPWSTR szCurdir;      // <Curdir> (optional)
  LPWSTR szIconfile;    // <Iconfile> (optional)
  szTargetfile = AnsiToWide(const_cast<char *>(hb_parc(1)));
  szTargetargs = HB_ISCHAR(2) ? AnsiToWide(const_cast<char *>(hb_parc(2))) : (TCHAR *)"";
  szLinkfile = AnsiToWide(const_cast<char *>(hb_parc(3)));
  szDescription = HB_ISCHAR(4) ? AnsiToWide(const_cast<char *>(hb_parc(4))) : (TCHAR *)"";
  szCurdir = HB_ISCHAR(6) ? AnsiToWide(const_cast<char *>(hb_parc(6))) : (TCHAR *)"";
  szIconfile = HB_ISCHAR(7) ? AnsiToWide(const_cast<char *>(hb_parc(7))) : (TCHAR *)"";
#endif
  iShowmode = hb_parnidef(5, 0);
  iIconindex = hb_parnidef(8, 0);

  // Call CoInitialize() and create the link if OK.
  hRes = CoInitialize(nullptr);
  if (SUCCEEDED(hRes)) {
    hRes = CreateShortCut(szTargetfile,  // Targetfile
                          szTargetargs,  // Target arguments
                          szLinkfile,    // Short-cut filename
                          szDescription, // Short-cut description
                          iShowmode,     // Showmode constant
                          szCurdir,      // Working directory for linked file
                          szIconfile,    // Icon file shown for the link
                          iIconindex);   // Index of icon in the file
    if (SUCCEEDED(hRes)) {
      hb_retnl(hRes);
    }
  } else {
    hb_retnl(hRes);
  }

  // call CoUninitialize() and exit the program.
  CoUninitialize();
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(WAITRUNPIPE, HMG_WAITRUNPIPE)
HB_FUNC_TRANSLATE(COPYRTFTOCLIPBOARD, HMG_COPYRTFTOCLIPBOARD)
HB_FUNC_TRANSLATE(COPYTOCLIPBOARD, HMG_COPYTOCLIPBOARD)
HB_FUNC_TRANSLATE(RETRIEVETEXTFROMCLIPBOARD, HMG_RETRIEVETEXTFROMCLIPBOARD)
HB_FUNC_TRANSLATE(CLEARCLIPBOARD, HMG_CLEARCLIPBOARD)
HB_FUNC_TRANSLATE(GETRED, WAGETRVALUE)
HB_FUNC_TRANSLATE(HMG_GETRED, WAGETRVALUE)
HB_FUNC_TRANSLATE(GETGREEN, WAGETGVALUE)
HB_FUNC_TRANSLATE(HMG_GETGREEN, WAGETGVALUE)
HB_FUNC_TRANSLATE(GETBLUE, WAGETBVALUE)
HB_FUNC_TRANSLATE(HMG_GETBLUE, WAGETBVALUE)
HB_FUNC_TRANSLATE(GETKEYSTATE, WAGETKEYSTATE)
HB_FUNC_TRANSLATE(INKEYGUI, HMG_INKEYGUI)
HB_FUNC_TRANSLATE(GETDC, HMG_GETDC)
HB_FUNC_TRANSLATE(RELEASEDC, HMG_RELEASEDC)
HB_FUNC_TRANSLATE(HIWORD, HMG_HIWORD)
HB_FUNC_TRANSLATE(LOWORD, HMG_LOWORD)
HB_FUNC_TRANSLATE(C_GETSPECIALFOLDER, HMG_C_GETSPECIALFOLDER)
#ifdef __WIN98__
HB_FUNC_TRANSLATE(C_GETDLLSPECIALFOLDER, HMG_C_GETDLLSPECIALFOLDER)
#endif
HB_FUNC_TRANSLATE(GETPHYSICALLYINSTALLEDSYSTEMMEMORY, HMG_GETPHYSICALLYINSTALLEDSYSTEMMEMORY)
HB_FUNC_TRANSLATE(MEMORYSTATUS, HMG_MEMORYSTATUS)
HB_FUNC_TRANSLATE(C_SHELLABOUT, HMG_C_SHELLABOUT)
HB_FUNC_TRANSLATE(PAINTBKGND, HMG_PAINTBKGND)
HB_FUNC_TRANSLATE(GETWINDOWSDIR, HMG_GETWINDOWSDIR)
HB_FUNC_TRANSLATE(GETSYSTEMDIR, HMG_GETSYSTEMDIR)
HB_FUNC_TRANSLATE(GETTEMPDIR, HMG_GETTEMPDIR)
HB_FUNC_TRANSLATE(POSTMESSAGE, HMG_POSTMESSAGE)
HB_FUNC_TRANSLATE(DEFWINDOWPROC, HMG_DEFWINDOWPROC)
HB_FUNC_TRANSLATE(GETSTOCKOBJECT, HMG_GETSTOCKOBJECT)
HB_FUNC_TRANSLATE(GETNEXTDLGTABITEM, HMG_GETNEXTDLGTABITEM)
HB_FUNC_TRANSLATE(SHELLEXECUTE, HMG_SHELLEXECUTE)
HB_FUNC_TRANSLATE(SHELLEXECUTEEX, HMG_SHELLEXECUTEEX)
HB_FUNC_TRANSLATE(WAITRUN, HMG_WAITRUN)
HB_FUNC_TRANSLATE(WAITRUNTERM, HMG_WAITRUNTERM)
HB_FUNC_TRANSLATE(ISEXERUNNING, HMG_ISEXERUNNING)
HB_FUNC_TRANSLATE(SETSCROLLPOS, HMG_SETSCROLLPOS)
HB_FUNC_TRANSLATE(GETLASTERROR, HMG_GETLASTERROR)
HB_FUNC_TRANSLATE(CREATEFOLDER, HMG_CREATEFOLDER)
HB_FUNC_TRANSLATE(SETCURRENTFOLDER, HMG_SETCURRENTFOLDER)
HB_FUNC_TRANSLATE(REMOVEFOLDER, HMG_REMOVEFOLDER)
HB_FUNC_TRANSLATE(GETCURRENTFOLDER, HMG_GETCURRENTFOLDER)
HB_FUNC_TRANSLATE(CREATESOLIDBRUSH, HMG_CREATESOLIDBRUSH)
HB_FUNC_TRANSLATE(SETTEXTCOLOR, HMG_SETTEXTCOLOR)
HB_FUNC_TRANSLATE(SETBKCOLOR, HMG_SETBKCOLOR)
HB_FUNC_TRANSLATE(GETSYSCOLOR, WAGETSYSCOLOR)
HB_FUNC_TRANSLATE(WINVERSION, HMG_WINVERSION)
HB_FUNC_TRANSLATE(GETDLLVERSION, HMG_GETDLLVERSION)
HB_FUNC_TRANSLATE(SELECTOBJECT, HMG_SELECTOBJECT)
HB_FUNC_TRANSLATE(FILLRECT, HMG_FILLRECT)
HB_FUNC_TRANSLATE(ISAPPHUNG, HMG_ISAPPHUNG)
HB_FUNC_TRANSLATE(EMPTYWORKINGSET, HMG_EMPTYWORKINGSET)
HB_FUNC_TRANSLATE(CLEANPROGRAMMEMORY, HMG_CLEANPROGRAMMEMORY)
HB_FUNC_TRANSLATE(GETCOMPACTPATH, HMG_GETCOMPACTPATH)
HB_FUNC_TRANSLATE(GETSHORTPATHNAME, HMG_GETSHORTPATHNAME)
HB_FUNC_TRANSLATE(DRAWTEXT, HMG_DRAWTEXT)
HB_FUNC_TRANSLATE(GETTEXTMETRIC, HMG_GETTEXTMETRIC)
HB_FUNC_TRANSLATE(_GETCLIENTRECT, HMG__GETCLIENTRECT)
HB_FUNC_TRANSLATE(ISOEMTEXT, HMG_ISOEMTEXT)
HB_FUNC_TRANSLATE(GETOBJECTTYPE, HMG_GETOBJECTTYPE)
HB_FUNC_TRANSLATE(DRAGACCEPTFILES, HMG_DRAGACCEPTFILES)
HB_FUNC_TRANSLATE(DRAGQUERYFILES, HMG_DRAGQUERYFILES)
HB_FUNC_TRANSLATE(DRAGFINISH, HMG_DRAGFINISH)
HB_FUNC_TRANSLATE(CREATELINK, HMG_CREATELINK)
#endif
