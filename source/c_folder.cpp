//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// FOLDER form source code
// (c)2005-2009 Janusz Pora <januszpora@onet.eu>
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

#if defined(_MSC_VER)
#pragma warning(disable : 4996)
#endif
#include <commctrl.h>

#include <hbapiitm.hpp>
#include <hbvm.hpp>

#define FLBTN_OK 1
#define FLBTN_APPLY 2
#define FLBTN_CANCEL 3
#define FLBTN_HELP 4

#define FLN_FIRST (0U - 200U)
#define FLN_SETACTIVE (FLN_FIRST - 0)
#define FLN_KILLACTIVE (FLN_FIRST - 1)
#define FLN_APPLY (FLN_FIRST - 2)
#define FLN_RESET (FLN_FIRST - 3)
#define FLN_HELP (FLN_FIRST - 5)
#define FLN_QUERYCANCEL (FLN_FIRST - 6)
#define FLN_FINISH (FLN_FIRST - 7)

extern PWORD CreateDlgTemplate(long lTemplateSize, PHB_ITEM dArray, PHB_ITEM cArray);
extern long GetSizeDlgTemp(PHB_ITEM dArray, PHB_ITEM cArray);

extern HB_PTRUINT wapi_GetProcAddress(HMODULE hModule, LPCSTR lpProcName);

using fnIsAppThemed = BOOL(WINAPI *)(void);

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
#endif

static HINSTANCE hUxTheme;

struct _FPI;
using HFLDPAGEINFO = _FPI FAR *;
struct tag_FldPageInfo;

struct tag_FldPageInfo
{
  HWND hwndPage;
  DLGTEMPLATE *apRes;
  BOOL isDirty;
  LPCTSTR pszText;
  LPCTSTR pszTemplate;
  DWORD dwFlags;
  int idrc;
  BOOL useCallback;
  BOOL hasIcon;
};

using FLDPAGEINFO = tag_FldPageInfo;
using LPCFLDPAGEINFO = const FLDPAGEINFO FAR *;

// HFLDPAGE
struct tag_fldhdr
{
  HWND hwnd;        // Folder handle
  HWND hwndTab;     // tab control
  HWND hwndDisplay; // current child dialog box
  RECT rcDisplay;   // display rectangle for the tab control
  RECT rcFolder;    // rectangle for the folder control
  HFLDPAGEINFO FAR *fhpage;
  int nPages;
  int x;
  int y;
  int cx;
  int cy;
  int active_page;
  BOOL isInDirect;
  BOOL isModal;
  BOOL hasOk;
  BOOL hasApply;
  BOOL hasCancel;
  BOOL hasHelp;
  BOOL activeValid;
  BOOL ended;
  int width;
  int height;
  int nIdFld;
  int FolderStyle;
  HIMAGELIST hImageList;
};

using FLDHDRINFO = tag_fldhdr;

struct MyDLGTEMPLATEEX
{
  WORD dlgVer;
  WORD signature;
  DWORD helpID;
  DWORD exStyle;
  DWORD style;
  WORD cDlgItems;
  short x;
  short y;
  short cx;
  short cy;
};

struct PADDING_INFO
{
  int x;
  int y;
};

struct _FLHNOTIFY
{
  NMHDR hdr;
  LPARAM lParam;
};

using FLHNOTIFY = _FLHNOTIFY;
using LPFLHNOTIFY = _FLHNOTIFY FAR *;

VOID WINAPI FLD_FolderInit(HWND hWndDlg, FLDHDRINFO *pFhi);
DLGTEMPLATE *WINAPI FLD_SetStyleDlgRes(DLGTEMPLATE *pTemplate, DWORD resSize);
DLGTEMPLATE *WINAPI FLD_LockDlgRes(TCHAR *lpszResName);
BOOL WINAPI IsAppThemed(void);
VOID WINAPI FLD_SelChanged(HWND hWndDlg);
VOID WINAPI FLD_ChildDialogInit(HWND hWndDlg, HWND hWndParent, int idrc);
LRESULT CALLBACK HMG_PageFldProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
VOID WINAPI FLD_DialogAlign(HWND hWndDlg);
static BOOL FLD_PageInfo(DLGTEMPLATE *pTemplate, FLDHDRINFO *pFhi, int index, BOOL resize);
static BOOL FLD_DoCommand(HWND hWndDlg, WORD wID);
static BOOL FLD_Apply(HWND hWndDlg, LPARAM lParam);
static void FLD_Cancel(HWND hWndDlg, LPARAM lParam);
static BOOL FLD_ShowPage(HWND hWndDlg, int index, FLDHDRINFO *pFhi);
static void FLD_Changed(HWND hWndParent, HWND hWndDlg);
static void FLD_UnChanged(HWND hWndParent, HWND hWndDlg);
static LRESULT FLD_HwndToIndex(HWND hWndDlg, HWND hPageDlg);
static void FLD_CleanUp(HWND hWndDlg);
static void FLD_Help(HWND hwndDlg);
static void FLD_AddBitmap(HWND hWndFolder);
static BOOL FLD_isAppThemed(void);

static BOOL FLD_isAppThemed(void)
{
  BOOL bRet = FALSE;

  if (hUxTheme == nullptr) {
    hUxTheme = LoadLibraryEx(TEXT("uxtheme.dll"), nullptr, 0);
  }

  if (hUxTheme) {
    auto pfn = reinterpret_cast<fnIsAppThemed>(wapi_GetProcAddress(hUxTheme, "IsAppThemed"));
    if (pfn) {
      bRet = static_cast<BOOL>(pfn());
    }
  }

  return bRet;
}

LRESULT CALLBACK HMG_FldProc(HWND hWndDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;
  long int r;
  LPNMHDR lpnmhdr;
  FLDHDRINFO *pFhi;

  switch (message)
  {
  // wNotifyCode = HIWORD(wParam); // notification code
  // wID = LOWORD(wParam);         // item, control, or accelerator identifier
  // hwndCtl = (HWND) lParam;      // handle of control
  case WM_INITDIALOG:
    pFhi = reinterpret_cast<FLDHDRINFO *>(lParam);
    FLD_FolderInit(hWndDlg, pFhi);
    FLD_AddBitmap(hWndDlg);
    break;

  case WM_CLOSE:
    FLD_Cancel(hWndDlg, 0);
    break;

  case WM_MOVE:
    FLD_DialogAlign(hWndDlg);
    return FALSE;

  case WM_COMMAND:
    if (lParam != 0 && HIWORD(wParam) == BN_CLICKED) {
      if (!FLD_DoCommand(hWndDlg, LOWORD(wParam))) {
        pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));
        if (!pFhi) {
          return FALSE;
        }

        // No default handler, forward notification to active page
        if (pFhi->activeValid && pFhi->active_page != -1) {
          HFLDPAGEINFO *hfpi = pFhi->fhpage;
          auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
          HWND hwndPage = fpi->hwndPage;
          SendMessage(hwndPage, WM_COMMAND, wParam, lParam);
        }
      }
    }

    return TRUE;

  case WM_NOTIFY:
    lpnmhdr = reinterpret_cast<NMHDR FAR *>(lParam);
    if (lpnmhdr != 0) {
      if (lpnmhdr->code == TCN_SELCHANGE) {
        FLD_SelChanged(hWndDlg);
      }
    }

    return FALSE;
  }

  if (!pSymbol) {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("FOLDERPROC"));
  }

  if (pSymbol) {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hWndDlg);
    hmg_vmPushUINT(message);
    hmg_vmPushWPARAM(wParam);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(4);
  }

  r = hb_parnl(-1);
  if (r) {
    return TRUE;
  } else {
    return FALSE;
  }
}

LRESULT CALLBACK HMG_PageFldProc(HWND hWndDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
  static PHB_SYMB pSymbol = nullptr;
  long int r;
  int iSel;
  HWND hWndParent;
  FLDPAGEINFO *fpi;
  HFLDPAGEINFO *hfpi;
  FLDHDRINFO *pFhi;

  hWndParent = GetParent(hWndDlg);
  switch (message)
  {
  // wNotifyCode = HIWORD(wParam); // notification code
  // wID = LOWORD(wParam);         // item, control, or accelerator identifier
  // HWND hwndCtl = (HWND) lParam;      // handle of control
  case WM_INITDIALOG:
    pFhi = reinterpret_cast<FLDHDRINFO *>(lParam);
    iSel = TabCtrl_GetCurSel(pFhi->hwndTab);
    hfpi = pFhi->fhpage;
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[iSel]);

    FLD_ChildDialogInit(hWndDlg, hWndParent, fpi->idrc);
    return TRUE;
  }

  if (!pSymbol) {
    pSymbol = hb_dynsymSymbol(hb_dynsymGet("PAGEFLDPROC"));
  }

  if (pSymbol) {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hmg_vmPushHWND(hWndDlg);
    hmg_vmPushUINT(message);
    hmg_vmPushWPARAM(wParam);
    hmg_vmPushLPARAM(lParam);
    hb_vmDo(4);
  }

  r = hb_parnl(-1);
  if (r) {
    return TRUE;
  } else {
    return FALSE;
  }
}

// hmg_CreateFolderPageIndirect(_HMG_aFolderPagesTemp, _HMG_aFolderTemplate, _HMG_aDialogItems)
HB_FUNC(HMG_CREATEFOLDERPAGEINDIRECT)
{
  auto pfpi = static_cast<FLDPAGEINFO *>(LocalAlloc(LPTR, sizeof(FLDPAGEINFO)));

  long lTemplateSize;

  auto sArray = hb_param(1, Harbour::Item::ARRAY); // Folder Array
  auto dArray = hb_param(2, Harbour::Item::ARRAY); // Folder Page Array
  auto cArray = hb_param(3, Harbour::Item::ARRAY); // Page Controls Array
  lTemplateSize = GetSizeDlgTemp(dArray, cArray);
  auto pdlgtemplate = reinterpret_cast<DLGTEMPLATE *>(CreateDlgTemplate(lTemplateSize, dArray, cArray));
  ZeroMemory(pfpi, sizeof(FLDPAGEINFO));

#ifndef UNICODE
  auto strTitle = const_cast<TCHAR *>(hb_arrayGetCPtr(sArray, 1)); // Tab Title
#else
  auto strTitle = static_cast<TCHAR *>(AnsiToWide(static_cast<char *>(hb_arrayGetCPtr(sArray, 1)))); // Tab Title
#endif
  auto idRC = hb_arrayGetNI(sArray, 2);      // Id Dialog resource
  auto PageStyle = hb_arrayGetNI(sArray, 3); // Page Style
#ifndef UNICODE
  auto ImageName = const_cast<TCHAR *>(hb_arrayGetCPtr(sArray, 4));
#else
  auto ImageName = static_cast<TCHAR *>(AnsiToWide(static_cast<char *>(hb_arrayGetCPtr(sArray, 4))));
#endif

  pfpi->dwFlags = PageStyle;
  pfpi->pszText = strTitle;
  pfpi->idrc = idRC;
  pfpi->apRes = static_cast<DLGTEMPLATE *>(pdlgtemplate);
  pfpi->hwndPage = 0;
  pfpi->isDirty = FALSE;
  if (lstrlen(ImageName)) {
    pfpi->hasIcon = TRUE;
    pfpi->pszTemplate = ImageName;
  }

  HB_RETNL(reinterpret_cast<LONG_PTR>(reinterpret_cast<HFLDPAGEINFO>(pfpi)));

#ifdef UNICODE
  hb_xfree(strTitle);
  hb_xfree(ImageName);
#endif
}

// hmg_CreateFolderPage(_HMG_aFolderPagesTemp)
HB_FUNC(HMG_CREATEFOLDERPAGE)
{
  auto pfpi = static_cast<FLDPAGEINFO *>(LocalAlloc(LPTR, sizeof(FLDPAGEINFO)));

  auto sArray = hb_param(1, Harbour::Item::ARRAY);

  ZeroMemory(pfpi, sizeof(FLDPAGEINFO));

#ifndef UNICODE
  auto strTitle = const_cast<TCHAR *>(hb_arrayGetCPtr(sArray, 1)); // Caption
#else
  auto strTitle = static_cast<TCHAR *>(AnsiToWide(static_cast<char *>(hb_arrayGetCPtr(sArray, 1)))); // Caption
#endif
  auto idRC = hb_arrayGetNI(sArray, 2);      // Id Dialog resource
  auto PageStyle = hb_arrayGetNI(sArray, 3); // Page Style
#ifndef UNICODE
  auto caption = const_cast<TCHAR *>(hb_arrayGetCPtr(sArray, 4)); // Page Image
#else
  auto caption = static_cast<TCHAR *>(AnsiToWide(static_cast<char *>(hb_arrayGetCPtr(sArray, 4)))); // Page Image
#endif
  pfpi->dwFlags = PageStyle;
  pfpi->pszTemplate = MAKEINTRESOURCE(idRC);
  pfpi->pszText = strTitle;
  pfpi->idrc = idRC;

  pfpi->apRes = FLD_LockDlgRes(static_cast<TCHAR *>(MAKEINTRESOURCE(idRC)));
  pfpi->hwndPage = 0;
  pfpi->isDirty = FALSE;
  if (lstrlen(caption)) {
    pfpi->hasIcon = TRUE;
    pfpi->pszTemplate = caption;
  }

  HB_RETNL(reinterpret_cast<LONG_PTR>(reinterpret_cast<HFLDPAGEINFO>(pfpi)));

#ifdef UNICODE
  hb_xfree(strTitle);
  hb_xfree(caption);
#endif
}

// hmg_CreateDlgFolder(IdFld, _HMG_ActiveFolderHandle, aHwndFolderPages, _HMG_aFolderTemplate, _HMG_aDialogItems, _HMG_FolderInMemory)
HB_FUNC(HMG_CREATEDLGFOLDER)
{
  auto pFhi = static_cast<FLDHDRINFO *>(LocalAlloc(LPTR, sizeof(FLDHDRINFO)));
  DWORD dwDlgBase = GetDialogBaseUnits();
  int baseunitX = LOWORD(dwDlgBase), baseunitY = HIWORD(dwDlgBase);

  BOOL modal;
  LRESULT lResult;
  long lTemplateSize;
  int style;

  auto nIdFld = hmg_par_int(1);
  auto hWndDlg = hmg_par_HWND(2);

  auto sArray = hb_param(3, Harbour::Item::ARRAY); // aHwndFolderPages
  auto pArray = hb_param(4, Harbour::Item::ARRAY); //_HMG_aFolderTemplate
  auto cArray = hb_param(5, Harbour::Item::ARRAY); //_HMG_aDialogItems

  //  _HMG_aFolderTemplate := {0,ParentHandle,modal,style,styleEx
  //  ,x,y,w,h,caption,fontname,fontsize,bold,Italic,lApplyBtn,lCancelBtn} _HMG_aFolderTemplate ->
  //  {0,ParentHandle,modal,style,styleEx
  //  ,x,y,w,h,caption,fontname,fontsize,bold,Italic,lOkBtn,lApplyBtn,lCancelBtn, buttons , flat ,
  //  hottrack , vertical , bottom, multiline}
  modal = hb_arrayGetL(pArray, 3);

  auto nPages = static_cast<int>(hb_arrayLen(sArray));
  auto x = hb_arrayGetNI(pArray, 6);  // x
  auto y = hb_arrayGetNI(pArray, 7);  // y
  auto cx = hb_arrayGetNI(pArray, 8); // w
  auto cy = hb_arrayGetNI(pArray, 9); // h
  style = WS_CHILD | WS_VISIBLE;
  if (hb_arrayGetL(pArray, 19)) {
    style |= TCS_BUTTONS;
  }

  if (hb_arrayGetL(pArray, 20)) {
    style |= TCS_FLATBUTTONS;
  }

  if (hb_arrayGetL(pArray, 21)) {
    style |= TCS_HOTTRACK;
  }

  if (hb_arrayGetL(pArray, 22)) {
    style |= TCS_VERTICAL;
  }

  if (hb_arrayGetL(pArray, 23)) {
    style |= TCS_BOTTOM;
  }

  if (hb_arrayGetL(pArray, 24)) {
    style |= TCS_MULTILINE;
  }

  auto hfpi = static_cast<HFLDPAGEINFO *>(malloc(sizeof(HFLDPAGEINFO) * nPages));

  for (auto s = 0; s < nPages; s = s + 1) {
    hfpi[s] = static_cast<HFLDPAGEINFO>(reinterpret_cast<PHB_ITEM>(HB_arrayGetNL(sArray, s + 1)));
  }

  auto hwnd = reinterpret_cast<HWND>(HB_arrayGetNL(pArray, 2));

  // Fill out the FOLDERHEADERINFO
  pFhi->hwnd = hWndDlg;
  pFhi->fhpage = hfpi;
  pFhi->nPages = nPages;
  pFhi->x = MulDiv(x, 4, baseunitX);   // x
  pFhi->y = MulDiv(y, 8, baseunitY);   // y
  pFhi->cx = MulDiv(cx, 4, baseunitX); // cx
  pFhi->cy = MulDiv(cy, 8, baseunitY); // cy
  pFhi->hasOk = hb_arrayGetL(pArray, 15);
  pFhi->hasApply = hb_arrayGetL(pArray, 16);
  pFhi->hasCancel = hb_arrayGetL(pArray, 17);
  pFhi->hasHelp = hb_arrayGetL(pArray, 18);
  pFhi->activeValid = FALSE;
  pFhi->active_page = -1;
  pFhi->isInDirect = hmg_par_BOOL(6); // InMemory;
  pFhi->nIdFld = nIdFld;
  pFhi->FolderStyle = style;

  lTemplateSize = GetSizeDlgTemp(pArray, cArray);
  auto pdlgtemplate = reinterpret_cast<LPDLGTEMPLATE>(CreateDlgTemplate(lTemplateSize, pArray, cArray));

  if (modal) {
    lResult = DialogBoxIndirectParam(GetResources(), static_cast<LPDLGTEMPLATE>(pdlgtemplate), hwnd,
                                     reinterpret_cast<DLGPROC>(HMG_FldProc), reinterpret_cast<LPARAM>(pFhi));
    LocalFree(pdlgtemplate);
    HB_RETNL(static_cast<LONG_PTR>(lResult));
  } else {
    hWndDlg = CreateDialogIndirectParam(GetResources(), static_cast<LPDLGTEMPLATE>(pdlgtemplate), hwnd,
                                        reinterpret_cast<DLGPROC>(HMG_FldProc), reinterpret_cast<LPARAM>(pFhi));
    LocalFree(pdlgtemplate);
  }

  hmg_ret_HWND(hWndDlg); // TODO: verificar
}

// hmg_FolderHwndToIndex(hWndParent, hWndDlg)
HB_FUNC(HMG_FOLDERHWNDTOINDEX)
{
  auto iPageIndex = static_cast<int>(FLD_HwndToIndex(hmg_par_HWND(1), hmg_par_HWND(2)));

  hb_retni(iPageIndex);
}

// hmg_FolderGetCurrentPageHwnd(hWndParent)
HB_FUNC(HMG_FOLDERGETCURRENTPAGEHWND)
{
  auto hWndDlg = hmg_par_HWND(1);
  int iSel;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));
  HFLDPAGEINFO *hfpi;

  hfpi = pFhi->fhpage;
  iSel = TabCtrl_GetCurSel(pFhi->hwndTab);

  auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[iSel]);

  hmg_ret_HWND(fpi->hwndPage);
}

// hmg_Folder_Changed(hWndParent, hWndDlg)
HB_FUNC(HMG_FOLDER_CHANGED)
{
  auto hWndParent = hmg_par_HWND(1);
  auto hWndDlg = hmg_par_HWND(2);

  FLD_Changed(hWndParent, hWndDlg);
}

// hmg_Folder_UnChanged(hWndParent, hWndDlg)
HB_FUNC(HMG_FOLDER_UNCHANGED)
{
  auto hWndParent = hmg_par_HWND(1);
  auto hWndDlg = hmg_par_HWND(2);

  FLD_UnChanged(hWndParent, hWndDlg);
}

// hmg_Folder_IsDirty(hWndParent)
HB_FUNC(HMG_FOLDER_ISDIRTY)
{
  auto hWndParent = hmg_par_HWND(1);
  BOOL lPageDirty = FALSE;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi) {
    return;
  }

  for (auto i = 0; i < pFhi->nPages; i++) {
    HFLDPAGEINFO *hfpi = pFhi->fhpage;
    auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);

    // look to see if there's any dirty pages
    if (fpi->isDirty && !pFhi->activeValid) {
      lPageDirty = TRUE;
    }
  }

  hb_retl(static_cast<BOOL>(lPageDirty));
}

// hmg_Folder_IsFinish(hWndParent)
HB_FUNC(HMG_FOLDER_ISFINISH)
{
  auto hWndParent = hmg_par_HWND(1);
  BOOL lFooderFinish;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi->isModal) {
    lFooderFinish = !pFhi->activeValid;
  } else {
    lFooderFinish = pFhi->ended;
  }

  hb_retl(static_cast<BOOL>(lFooderFinish));
}

// hmg_Folder_GetIdFld(hWndParent)
HB_FUNC(HMG_FOLDER_GETIDFLD)
{
  auto hWndParent = hmg_par_HWND(1);
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi) {
    hb_retni(hmg_par_int(2));
  } else {
    hb_retni(pFhi->nIdFld);
  }
}

// hmg_Folder_GetTabHandle(hWndParent)
HB_FUNC(HMG_FOLDER_GETTABHANDLE)
{
  auto hWndParent = hmg_par_HWND(1);
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi) {
    hb_retnl(0); // TODO: 0 -> nullptr
  } else {
    hmg_ret_HWND(pFhi->hwndTab);
  }
}

// hmg_Folder_CleanUp(hWndParent)
HB_FUNC(HMG_FOLDER_CLEANUP)
{
  FLD_CleanUp(hmg_par_HWND(1));
}

// FLD_FolderInit()
VOID WINAPI FLD_FolderInit(HWND hWndDlg, FLDHDRINFO *pFhi)
{
  HFLDPAGEINFO *hfpi;
  FLDPAGEINFO *fpi;
  DLGTEMPLATE *pTemplate;
  OSVERSIONINFO osvi;

  DWORD dwDlgBase = GetDialogBaseUnits();
  int cxMargin = LOWORD(dwDlgBase) / 4;
  int cyMargin = HIWORD(dwDlgBase) / 8;
  TCITEM tie;
  RECT rcTab;
  HWND hwndButton;
  RECT rcButton;
  int nPages, style;

  INITCOMMONCONTROLSEX icc;

  icc.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icc.dwICC = ICC_TAB_CLASSES;
  InitCommonControlsEx(&icc);

  // Save a pointer to the FLDHDR structure.
  SetWindowLongPtr(hWndDlg, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pFhi));

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&osvi);

  nPages = pFhi->nPages;
  hfpi = pFhi->fhpage;
  style = pFhi->FolderStyle;

  // Create the tab control.
  pFhi->hwndTab =
      CreateWindowEx(0, WC_TABCONTROL, TEXT(""), style, 0, 0, 100, 100, hWndDlg, nullptr, GetInstance(), nullptr);

  if (pFhi->hwndTab == nullptr) {
    MessageBox(nullptr, TEXT("Tab Control for Folder could not be created"), TEXT("Error"),
               MB_OK | MB_ICONERROR | MB_DEFBUTTON1 | MB_APPLMODAL | MB_SETFOREGROUND);
  }

  tie.mask = TCIF_TEXT | TCIF_IMAGE;
  tie.iImage = -1;

  for (auto s = 0; s < nPages; s = s + 1) {
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[s]);
    tie.pszText = const_cast<LPTSTR>(fpi->pszText);
    TabCtrl_InsertItem(pFhi->hwndTab, s, &tie);
  }

  // Determine the bounding rectangle for all child dialog boxes.
  SetRectEmpty(&rcTab);

  // The x, y, cx, and cy members specify values in dialog box units.
  for (auto i = 0; i < nPages; i++) {
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);
    if (!pFhi->isInDirect) {
      pTemplate = static_cast<DLGTEMPLATE *>(fpi->apRes);
      FLD_PageInfo(pTemplate, pFhi, i, TRUE);
    }
  }

  if (pFhi->cx > rcTab.right) {
    rcTab.right = pFhi->cx;
  }

  if (pFhi->cy > rcTab.bottom) {
    rcTab.bottom = pFhi->cy;
  }

  rcTab.right = rcTab.right * LOWORD(dwDlgBase) / 4;
  rcTab.bottom = rcTab.bottom * HIWORD(dwDlgBase) / 8;

  // Calculate how large to make the tab control, so
  // the display area can accommodate all the child dialog boxes.
  TabCtrl_AdjustRect(pFhi->hwndTab, TRUE, &rcTab);
  OffsetRect(&rcTab, cxMargin - rcTab.left, cyMargin - rcTab.top);

  // Calculate the display rectangle.
  CopyRect(&pFhi->rcDisplay, &rcTab);
  TabCtrl_AdjustRect(pFhi->hwndTab, FALSE, &pFhi->rcDisplay);

  // Set the size and position of the tab control, buttons and dialog box.
  SetWindowPos(pFhi->hwndTab, nullptr, rcTab.left, rcTab.top, rcTab.right - rcTab.left, rcTab.bottom - rcTab.top,
               SWP_NOZORDER);

  // Created and position of the buttons
  {
    int x, y;
    auto num_buttons = 0;
    auto buttonWidth = 0;
    auto buttonHeight = 0;
    int cx = FLD_isAppThemed() ? 2 : 0;

    if (cx) {
      cx = osvi.dwMajorVersion >= 6 ? 4 : cx;
    }

    if (hUxTheme != nullptr) {
      FreeLibrary(hUxTheme);
      hUxTheme = nullptr;
    }

    rcButton.bottom = 0;
    rcButton.right = 0;
    if (pFhi->hasOk) {
      num_buttons++;
    }

    if (pFhi->hasApply) {
      num_buttons++;
    }

    if (pFhi->hasCancel) {
      num_buttons++;
    }

    if (pFhi->hasHelp) {
      num_buttons++;
    }

    if (num_buttons > 0) {
      y = rcTab.bottom + cyMargin;
      if (pFhi->hasOk) {
        // Move the first button "OK" below the tab control.
        hwndButton = GetDlgItem(hWndDlg, FLBTN_OK);
        GetWindowRect(hwndButton, &rcButton);
        buttonWidth = rcButton.right - rcButton.left;
        buttonHeight = rcButton.bottom - rcButton.top;
        x = rcTab.right - cx - ((cxMargin + buttonWidth) * num_buttons);

        SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
        num_buttons--;
      }

      if (pFhi->hasCancel) {
        // Move the second button "Cancel" to the right of the first.
        hwndButton = GetDlgItem(hWndDlg, FLBTN_CANCEL);
        if (buttonHeight == 0) {
          GetWindowRect(hwndButton, &rcButton);
          buttonWidth = rcButton.right - rcButton.left;
          buttonHeight = rcButton.bottom - rcButton.top;
        }

        x = rcTab.right - cx - ((cxMargin + buttonWidth) * num_buttons);

        SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
        num_buttons--;
      }

      if (pFhi->hasApply) {
        // Move the thrid button "Apply" to the right of the second.
        hwndButton = GetDlgItem(hWndDlg, FLBTN_APPLY);
        if (buttonHeight == 0) {
          GetWindowRect(hwndButton, &rcButton);
          buttonWidth = rcButton.right - rcButton.left;
          buttonHeight = rcButton.bottom - rcButton.top;
        }

        x = rcTab.right - cx - ((cxMargin + buttonWidth) * num_buttons);

        SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
        EnableWindow(hwndButton, FALSE);

        num_buttons--;
      }

      if (pFhi->hasHelp) {
        // Move the thrid button "Help" to the right of the second.
        hwndButton = GetDlgItem(hWndDlg, FLBTN_HELP);
        if (buttonHeight == 0) {
          GetWindowRect(hwndButton, &rcButton);
          buttonWidth = rcButton.right - rcButton.left;
          buttonHeight = rcButton.bottom - rcButton.top;
        }

        x = rcTab.right - cx - ((cxMargin + buttonWidth) * num_buttons);

        SetWindowPos(hwndButton, 0, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
      }
    }

    // Size the dialog box.
    SetWindowPos(hWndDlg, nullptr, 0, 0, rcTab.right + cyMargin + 2 * GetSystemMetrics(SM_CXDLGFRAME) + 2 * cx,
                 rcTab.bottom + 2 * cx + buttonHeight + 2 * cyMargin + 2 * GetSystemMetrics(SM_CYDLGFRAME) +
                     GetSystemMetrics(SM_CYCAPTION),
                 SWP_NOMOVE | SWP_NOZORDER);
  }

  // Simulate selection of the first item.
  FLD_SelChanged(hWndDlg);
}

DLGTEMPLATE *WINAPI FLD_SetStyleDlgRes(DLGTEMPLATE *pTemplate, DWORD resSize)
{
  LPVOID temp;

  temp = LocalAlloc(LPTR, resSize);
  if (!temp) {
    return FALSE;
  }
  memcpy(temp, pTemplate, resSize);
  pTemplate = static_cast<DLGTEMPLATE *>(temp);

  if (reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->signature == 0xFFFF) {
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style |= WS_CHILD | WS_TABSTOP | DS_CONTROL;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~DS_MODALFRAME;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~WS_CAPTION;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~WS_SYSMENU;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~WS_DISABLED;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~WS_THICKFRAME;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->exStyle |= WS_EX_CONTROLPARENT;
    //((MyDLGTEMPLATEEX*)pTemplate)->style &= ~WS_POPUP;
    reinterpret_cast<MyDLGTEMPLATEEX *>(pTemplate)->style &= ~WS_VISIBLE;
  } else {
    pTemplate->style |= WS_CHILD | WS_TABSTOP | DS_CONTROL;
    pTemplate->style &= ~DS_MODALFRAME;
    pTemplate->style &= ~WS_CAPTION;
    pTemplate->style &= ~WS_SYSMENU;
    pTemplate->style &= ~WS_DISABLED;
    pTemplate->style &= ~WS_THICKFRAME;
    //    pTemplate->style &= ~WS_POPUP;
    pTemplate->style &= ~WS_VISIBLE;
    pTemplate->dwExtendedStyle |= WS_EX_CONTROLPARENT;
  }
  return pTemplate;
}

//-----------------------------------------------------------------
// FLD_LockDlgRes - loads and locks a dialog template resource.
// Returns the address of the locked resource.
// lpszResName - name of the resource
//-----------------------------------------------------------------
DLGTEMPLATE *WINAPI FLD_LockDlgRes(TCHAR *lpszResName)
{
  DWORD resSize;
  HGLOBAL hglb;

  HRSRC hrsrc = FindResource(GetResources(), lpszResName, RT_DIALOG);

  resSize = SizeofResource(GetResources(), hrsrc);

  hglb = LoadResource(GetResources(), hrsrc);

  auto pTemplate = static_cast<DLGTEMPLATE *>(LockResource(hglb));
  pTemplate = FLD_SetStyleDlgRes(pTemplate, resSize);

  return pTemplate;
}

// FLD_SelChanged() - processes the TCN_SELCHANGE notification.()
VOID WINAPI FLD_SelChanged(HWND hWndDlg)
{
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  int iSel = TabCtrl_GetCurSel(pFhi->hwndTab);

  FLD_ShowPage(hWndDlg, iSel, pFhi);
}

// FLD_ChildDialogInit()
VOID WINAPI FLD_ChildDialogInit(HWND hWndDlg, HWND hWndParent, int idrc)
{
  RECT rcTab;

  static PHB_SYMB pSymbol = nullptr;

  DWORD dwDlgBase = GetDialogBaseUnits();
  int cxMargin = LOWORD(dwDlgBase) / 4;
  int cyMargin = HIWORD(dwDlgBase) / 8;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (pFhi) {
    GetWindowRect(pFhi->hwndTab, &rcTab);

    SetWindowPos(hWndDlg, nullptr, pFhi->rcDisplay.left + rcTab.left - cxMargin - 1,
                 pFhi->rcDisplay.top + rcTab.top - cyMargin, 0, 0, SWP_NOSIZE);

    pSymbol = hb_dynsymSymbol(hb_dynsymGet("INITPAGEFLDPROC"));
    if (pSymbol) {
      hb_vmPushSymbol(pSymbol);
      hb_vmPushNil();
      hmg_vmPushHWND(hWndParent);
      hmg_vmPushHWND(hWndDlg);
      hb_vmPushLong(idrc);
      hb_vmDo(3);
    }
  }
}

// FLD_DialogAlign()
VOID WINAPI FLD_DialogAlign(HWND hWndDlg)
{
  RECT rcTab;
  DWORD dwDlgBase = GetDialogBaseUnits();
  int cxMargin = LOWORD(dwDlgBase) / 4;
  int cyMargin = HIWORD(dwDlgBase) / 8;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  if (pFhi) {
    GetWindowRect(pFhi->hwndTab, &rcTab);
    SetWindowPos(pFhi->hwndDisplay, nullptr, pFhi->rcDisplay.left + rcTab.left - cxMargin - 1,
                 pFhi->rcDisplay.top + rcTab.top - cyMargin, 0, 0, SWP_NOSIZE);
  }
}

// FLD_PageInfo()
static BOOL FLD_PageInfo(DLGTEMPLATE *pTemplate, FLDHDRINFO *pFhi, int index, BOOL resize)
{
  int width, height;

  if (!pTemplate) {
    return FALSE;
  }

  auto p = reinterpret_cast<const WORD *>(pTemplate);

  if (reinterpret_cast<const MyDLGTEMPLATEEX *>(pTemplate)->signature == 0xFFFF) {
    // DLGTEMPLATEEX (not defined in any std. header file)
    p++;    // dlgVer
    p++;    // signature
    p += 2; // help ID
    p += 2; // ext style
    p += 2; // style
  } else {
    // DLGTEMPLATE
    p += 2; // style
    p += 2; // ext style
  }

  p++; // nb items
  p++; //   x
  p++; //   y
  width = static_cast<WORD>(*p);
  p++;
  height = static_cast<WORD>(*p);
  p++;

  // remember the largest width and height
  if (resize) {
    if (width > pFhi->cx) {
      pFhi->cx = width;
    }

    if (height > pFhi->cy) {
      pFhi->cy = height;
    }
  }

  // menu
  switch (static_cast<WORD>(*p))
  {
  case 0x0000:
    p++;
    break;
  case 0xffff:
    p += 2;
    break;
  default:
    p += lstrlenW(reinterpret_cast<LPCWSTR>(p)) + 1;
    break;
  }

  // class
  switch (static_cast<WORD>(*p))
  {
  case 0x0000:
    p++;
    break;
  case 0xffff:
    p += 2;
    break;
  default:
    p += lstrlenW(reinterpret_cast<LPCWSTR>(p)) + 1;
    break;
  }

  // Extract the caption
  auto fpi = reinterpret_cast<FLDPAGEINFO *>(pFhi->fhpage[index]);
  fpi->pszText = reinterpret_cast<LPCTSTR>(p);

  return TRUE;
}

// FLD_Changed()
static void FLD_Changed(HWND hWndParent, HWND hwndDirtyPage)
{
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi) {
    return;
  }

  // Set the dirty flag of this page.
  for (auto i = 0; i < pFhi->nPages; i++) {
    HFLDPAGEINFO *hfpi = pFhi->fhpage;
    auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);
    if (fpi->hwndPage == hwndDirtyPage) {
      fpi->isDirty = TRUE;
    }
  }

  // Enable the Apply button.
  if (pFhi->hasApply && pFhi->activeValid) {
    HWND hwndApplyBtn = GetDlgItem(hWndParent, FLBTN_APPLY);

    EnableWindow(hwndApplyBtn, TRUE);
  }
}

// FLD_UnChanged()
static void FLD_UnChanged(HWND hWndParent, HWND hwndCleanPage)
{
  BOOL noPageDirty = TRUE;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndParent, GWLP_USERDATA));

  if (!pFhi) {
    return;
  }

  for (auto i = 0; i < pFhi->nPages; i++) {
    // set the specified page as clean
    HFLDPAGEINFO *hfpi = pFhi->fhpage;
    auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);
    if (fpi->hwndPage == hwndCleanPage) {
      fpi->isDirty = FALSE;
    }

    // look to see if there's any dirty pages
    if (fpi->isDirty) {
      noPageDirty = FALSE;
    }
  }

  // Disable Apply button.
  if (pFhi->hasApply) {
    HWND hwndApplyBtn = GetDlgItem(hWndParent, FLBTN_APPLY);
    if (noPageDirty) {
      EnableWindow(hwndApplyBtn, FALSE);
    }
  }
}

// FLD_DoCommand()
static BOOL FLD_DoCommand(HWND hWndDlg, WORD wID)
{
  switch (wID)
  {
  case FLBTN_OK:
  case FLBTN_APPLY: {
    auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));
    HWND hwndApplyBtn = GetDlgItem(hWndDlg, FLBTN_APPLY);
    if (pFhi->activeValid) {
      if (FLD_Apply(hWndDlg, wID) == FALSE) {
        break;
      }

      if (wID == FLBTN_OK) {
        if (!pFhi) {
          return FALSE;
        }

        if (!pFhi->isModal) {
          pFhi->activeValid = FALSE;
        } else {
          pFhi->ended = TRUE;
        }
      } else {
        EnableWindow(hwndApplyBtn, FALSE);
      }
    } else {
      FLHNOTIFY fln;
      HFLDPAGEINFO *hfpi;
      HWND hwndPage;

      // Send FLN_FINISH to the current page.
      fln.hdr.hwndFrom = hWndDlg;
      fln.hdr.idFrom = 0;
      fln.lParam = 0;
      fln.hdr.code = FLN_FINISH;

      hfpi = pFhi->fhpage;
      auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
      hwndPage = fpi->hwndPage;

      SendMessage(hwndPage, WM_NOTIFY, 0, reinterpret_cast<LPARAM>(&fln));
    }
    break;
  }

  case FLBTN_CANCEL:
    FLD_Cancel(hWndDlg, wID);
    break;

  case FLBTN_HELP:
    FLD_Help(hWndDlg);
    break;

  default:
    return FALSE;
  }

  return TRUE;
}

// FLD_Apply()
static BOOL FLD_Apply(HWND hWndDlg, LPARAM lParam)
{
  HFLDPAGEINFO *hfpi;
  HWND hwndPage;
  FLHNOTIFY fln;
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  if (pFhi->active_page < 0) {
    return FALSE;
  }

  fln.hdr.hwndFrom = hWndDlg;
  fln.hdr.idFrom = 0;
  fln.lParam = 0;

  // Send FLN_KILLACTIVE to the current page.
  fln.hdr.code = FLN_KILLACTIVE;

  hfpi = pFhi->fhpage;
  auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
  hwndPage = fpi->hwndPage;

  if (SendMessage(hwndPage, WM_NOTIFY, 0, reinterpret_cast<LPARAM>(&fln)) != FALSE) {
    return FALSE;
  }

  // Send FLN_APPLY to all pages.
  fln.hdr.code = FLN_APPLY;
  fln.lParam = lParam;

  for (auto i = 0; i < pFhi->nPages; i++) {
    hfpi = pFhi->fhpage;
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);
    hwndPage = fpi->hwndPage;
    if (hwndPage) {
      SendMessage(hwndPage, WM_NOTIFY, static_cast<WPARAM>(lParam), reinterpret_cast<LPARAM>(&fln));
    }
  }

  if (lParam == FLBTN_OK) {
    pFhi->activeValid = FALSE;
  }

  if (pFhi->active_page >= 0) {
    fln.hdr.hwndFrom = hWndDlg;
    fln.hdr.idFrom = 0;
    fln.lParam = 0;
    if (lParam == FLBTN_OK) {
      fln.hdr.code = FLN_FINISH;
    } else {
      fln.hdr.code = FLN_SETACTIVE;
    }

    hfpi = pFhi->fhpage;
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
    hwndPage = fpi->hwndPage;

    SendMessage(hwndPage, WM_NOTIFY, static_cast<WPARAM>(lParam), reinterpret_cast<LPARAM>(&fln));
  }

  return TRUE;
}

// FLD_Cancel()
static void FLD_Cancel(HWND hWndDlg, LPARAM lParam)
{
  HFLDPAGEINFO *hfpi;
  HWND hwndPage;
  FLHNOTIFY fln;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  if (pFhi->active_page < 0) {
    return;
  }

  hfpi = pFhi->fhpage;
  auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
  hwndPage = fpi->hwndPage;

  fln.hdr.code = FLN_QUERYCANCEL;
  fln.hdr.hwndFrom = hWndDlg;
  fln.hdr.idFrom = 0;
  fln.lParam = 0;

  if (SendMessage(hwndPage, WM_NOTIFY, static_cast<WPARAM>(lParam), reinterpret_cast<LPARAM>(&fln))) {
    return;
  }

  fln.hdr.code = FLN_RESET;
  fln.lParam = lParam;

  for (auto i = 0; i < pFhi->nPages; i++) {
    hfpi = pFhi->fhpage;
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);
    hwndPage = fpi->hwndPage;

    if (hwndPage) {
      SendMessage(hwndPage, WM_NOTIFY, static_cast<WPARAM>(lParam), reinterpret_cast<LPARAM>(&fln));
    }
  }

  if (!pFhi->isModal) {
    pFhi->activeValid = FALSE;
  } else {
    pFhi->ended = TRUE;
  }

  fln.hdr.code = FLN_FINISH;
  fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
  hwndPage = fpi->hwndPage;
  if (hwndPage) {
    SendMessage(hwndPage, WM_NOTIFY, static_cast<WPARAM>(lParam), reinterpret_cast<LPARAM>(&fln));
  }
}

// FLD_Help()
static void FLD_Help(HWND hWndDlg)
{
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));
  HWND hwndPage;
  HFLDPAGEINFO *hfpi;
  FLHNOTIFY fln;

  if (pFhi->active_page < 0) {
    return;
  }

  hfpi = pFhi->fhpage;
  auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
  hwndPage = fpi->hwndPage;

  fln.hdr.code = FLN_HELP;
  fln.hdr.hwndFrom = hWndDlg;
  fln.hdr.idFrom = 0;
  fln.lParam = 0;

  SendMessage(hwndPage, WM_NOTIFY, 0, reinterpret_cast<LPARAM>(&fln));
}

// FLD_ShowPage()
static BOOL FLD_ShowPage(HWND hWndDlg, int index, FLDHDRINFO *pFhi)
{
  FLDPAGEINFO *fpi;
  HFLDPAGEINFO *hfpi = pFhi->fhpage;

  if (index == pFhi->active_page) {
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[index]);
    if (GetTopWindow(hWndDlg) != fpi->hwndPage) {
      SetWindowPos(fpi->hwndPage, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE);
    }

    return TRUE;
  }

  fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[index]);
  if (fpi->hwndPage == nullptr) {
    pFhi->hwndDisplay =
        CreateDialogIndirectParam(GetResources(), static_cast<DLGTEMPLATE *>(fpi->apRes), hWndDlg,
                                  reinterpret_cast<DLGPROC>(HMG_PageFldProc), reinterpret_cast<LPARAM>(pFhi));
    fpi->hwndPage = pFhi->hwndDisplay;
  } else {
    pFhi->hwndDisplay = fpi->hwndPage;
  }

  FLD_DialogAlign(hWndDlg);

  if (pFhi->active_page != -1) {
    fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[pFhi->active_page]);
    ShowWindow(fpi->hwndPage, SW_HIDE);
  }

  fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[index]);
  ShowWindow(fpi->hwndPage, SW_SHOW);

  // Synchronize current selection with tab control
  SendMessage(pFhi->hwndTab, TCM_SETCURSEL, index, 0);

  pFhi->active_page = index;
  pFhi->activeValid = TRUE;

  return TRUE;
}

// FLD_HwndToIndex()
static LRESULT FLD_HwndToIndex(HWND hWndDlg, HWND hPageDlg)
{
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  for (auto index = 0; index < pFhi->nPages; index++) {
    HFLDPAGEINFO *hfpi = pFhi->fhpage;
    auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[index]);
    if (fpi->hwndPage == hPageDlg) {
      return index;
    }
  }

  return -1;
}

// FLD_CleanUp()
static void FLD_CleanUp(HWND hWndDlg)
{
  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndDlg, GWLP_USERDATA));

  if (!pFhi) {
    return;
  }

  for (auto i = 0; i < pFhi->nPages; i++) {
    HFLDPAGEINFO *hfpi = pFhi->fhpage;
    auto fpi = reinterpret_cast<FLDPAGEINFO *>(hfpi[i]);

    if (fpi->hwndPage) {
      DestroyWindow(fpi->hwndPage);
    }
  }

  // If we created the bitmaps, destroy them
  ImageList_Destroy(pFhi->hImageList);
  LocalFree(pFhi->fhpage);

  GlobalFree(static_cast<HGLOBAL>(pFhi));
}

// FLD_AddBitmap()
static void FLD_AddBitmap(HWND hWndFolder)
{
  HIMAGELIST himl = nullptr;
  HBITMAP hbmp;
  FLDPAGEINFO *pfpi = nullptr;
  TC_ITEM tie;
  HDC hDC;
  int l;
  auto cx = 0;
  auto cy = 0;
  auto i = 0;

  auto pFhi = reinterpret_cast<FLDHDRINFO *>(GetWindowLongPtr(hWndFolder, GWLP_USERDATA));

  l = pFhi->nPages - 1;

  for (auto s = 0; s <= l; s++) {
    pfpi = reinterpret_cast<FLDPAGEINFO *>(pFhi->fhpage[s]);
    if (pfpi->hasIcon) {
      i = s + 1;
      break;
    }
  }

  if (i != 0) {

    himl = ImageList_LoadImage(GetResources(), pfpi->pszTemplate, 0, l, CLR_DEFAULT, IMAGE_BITMAP,
                               LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS);

    if (himl == nullptr) {
      himl = ImageList_LoadImage(GetResources(), pfpi->pszTemplate, 0, l, CLR_DEFAULT, IMAGE_BITMAP,
                                 LR_LOADTRANSPARENT | LR_LOADFROMFILE | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS);
    }

    if (himl != nullptr) {
      ImageList_GetIconSize(himl, &cx, &cy);
      ImageList_Destroy(himl);
    }

    if ((cx > 0) && (cy > 0)) {
      himl = ImageList_Create(cx, cy, ILC_COLOR8 | ILC_MASK, l + 1, l + 1);

      if (himl != nullptr) {
        for (auto s = 0; s <= l; s++) {
          pfpi = reinterpret_cast<FLDPAGEINFO *>(pFhi->fhpage[s]);

          hbmp = nullptr;

          if (pfpi->hasIcon) {
            hbmp = static_cast<HBITMAP>(LoadImage(GetResources(), pfpi->pszTemplate, IMAGE_BITMAP, cx, cy,
                                                  LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS));

            if (hbmp == nullptr) {
              hbmp = static_cast<HBITMAP>(
                  LoadImage(nullptr, pfpi->pszTemplate, IMAGE_BITMAP, cx, cy,
                            LR_LOADTRANSPARENT | LR_LOADFROMFILE | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS));
            }
          }

          if (hbmp != nullptr) {
            ImageList_AddMasked(himl, hbmp, CLR_DEFAULT);
            DeleteObject(hbmp);
          } else {
            hDC = GetDC(pFhi->hwndTab);
            hbmp = CreateCompatibleBitmap(hDC, cx, cy);
            ImageList_AddMasked(himl, hbmp, CLR_DEFAULT);
            DeleteObject(hbmp);
            ReleaseDC(pFhi->hwndTab, hDC);
          }
        }

        SendMessage(pFhi->hwndTab, TCM_SETIMAGELIST, 0, reinterpret_cast<LPARAM>(himl));

        for (auto s = 0; s <= l; s++) {
          tie.mask = TCIF_IMAGE;
          tie.iImage = s;
          TabCtrl_SetItem(static_cast<HWND>(pFhi->hwndTab), s, &tie);
        }
      }
    }
  }

  pFhi->hImageList = himl;
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CREATEFOLDERPAGEINDIRECT, HMG_CREATEFOLDERPAGEINDIRECT)
HB_FUNC_TRANSLATE(CREATEFOLDERPAGE, HMG_CREATEFOLDERPAGE)
HB_FUNC_TRANSLATE(CREATEDLGFOLDER, HMG_CREATEDLGFOLDER)
HB_FUNC_TRANSLATE(FOLDERHWNDTOINDEX, HMG_FOLDERHWNDTOINDEX)
HB_FUNC_TRANSLATE(FOLDERGETCURRENTPAGEHWND, HMG_FOLDERGETCURRENTPAGEHWND)
HB_FUNC_TRANSLATE(FOLDER_CHANGED, HMG_FOLDER_CHANGED)
HB_FUNC_TRANSLATE(FOLDER_UNCHANGED, HMG_FOLDER_UNCHANGED)
HB_FUNC_TRANSLATE(FOLDER_ISDIRTY, HMG_FOLDER_ISDIRTY)
HB_FUNC_TRANSLATE(FOLDER_ISFINISH, HMG_FOLDER_ISFINISH)
HB_FUNC_TRANSLATE(FOLDER_GETIDFLD, HMG_FOLDER_GETIDFLD)
HB_FUNC_TRANSLATE(FOLDER_GETTABHANDLE, HMG_FOLDER_GETTABHANDLE)
HB_FUNC_TRANSLATE(FOLDER_CLEANUP, HMG_FOLDER_CLEANUP)
#endif
