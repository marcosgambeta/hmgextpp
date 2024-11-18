//
// HBPRINTER - Harbour Win32 Printing library source code
//
// Copyright 2002-2005 Richard Rylko <rrylko@poczta.onet.pl>
//

#define NO_LEAN_AND_MEAN

#include "mgdefs.hpp"

#if defined(_MSC_VER)
#pragma warning(disable : 4996)
#endif
#include <hbapiitm.hpp>
#include <hbwinuni.hpp>

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4201) /* warning C4201: nonstandard extension used: nameless struct/union */
#endif
#include <olectl.h>
#if defined(_MSC_VER)
#pragma warning(pop)
#endif

#include <commctrl.h>

#define HB_PARC hb_parvc
#if defined(_WIN64)
#define HB_PARNL3 hb_parvnll
#else
#define HB_PARNL3 hb_parvnl
#endif

#if defined(_MSC_VER)
#define itoa(__value, __string, __radix) _itoa(__value, __string, __radix)
#define ltoa(__value, __string, __radix) _ltoa(__value, __string, __radix)
#endif

static HDC s_hDC = nullptr;
static HDC s_hDCRef = nullptr;
static HDC s_hDCtemp;
static DEVMODE *s_pDevMode = nullptr;
static DEVMODE *s_pDevMode2 = nullptr;
static DEVNAMES *s_pDevNames = nullptr;
static HANDLE s_hPrinter = nullptr;
static PRINTER_INFO_2 *s_pi2 = nullptr;
static PRINTER_INFO_2 *s_pi22 = nullptr; // to restore printer dev mode after print.
static PRINTER_DEFAULTS s_pd;
static PRINTDLG s_pdlg;
static DOCINFO s_di;
static int s_nFromPage = 0;
static int s_nToPage = 0;
static TCHAR s_PrinterName[128];
static TCHAR s_PrinterDefault[128];
static DWORD s_charset = DEFAULT_CHARSET;
static HFONT s_hfont;
static HPEN s_hpen;
static HBRUSH s_hbrush;
static int s_textjust = 0;
static int s_devcaps[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0};
static int s_preview = 0;
static int s_polyfillmode = 1;
static HRGN s_hrgn = nullptr;
static HBITMAP s_himgbmp;
static HBITMAP s_hbmp[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static OSVERSIONINFO s_osvi;

#ifdef UNICODE
LPWSTR AnsiToWide(LPCSTR);
LPSTR WideToAnsi(LPWSTR);
#endif
void rr_getdevmode(void);

HB_FUNC(RR_FINISH)
{
  s_pDevMode = nullptr;
  s_pDevMode2 = nullptr;
  s_pDevNames = nullptr;
  ClosePrinter(s_hPrinter);
  s_hPrinter = nullptr;
  s_pi2 = nullptr;
  memset(&s_pd, 0, sizeof(s_pd));
  memset(&s_pdlg, 0, sizeof(s_pdlg));
  memset(&s_di, 0, sizeof(s_di));
  s_nFromPage = 0;
  s_nToPage = 0;
  s_hfont = nullptr;
  s_hpen = nullptr;
  s_hbrush = nullptr;
  s_textjust = 0;
  memset(&s_devcaps, 0, sizeof(s_devcaps));
  s_devcaps[15] = 1;
  s_preview = 0;
  s_polyfillmode = 1;
  s_hrgn = nullptr;
  s_himgbmp = nullptr;
  memset(&s_hbmp, 0, sizeof(s_hbmp));
}

HB_FUNC(RR_PRINTERNAME)
{
#ifndef UNICODE
  hb_retc(s_PrinterName);
#else
  LPSTR pStr = WideToAnsi(s_PrinterName);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

HB_FUNC(RR_PRINTDIALOG)
{
  TCHAR *pDevice;

  memset(&s_pdlg, 0, sizeof(s_pdlg));
  s_pdlg.lStructSize = sizeof(s_pdlg);
  s_pdlg.hDevMode = nullptr;
  s_pdlg.hDevNames = nullptr;
  s_pdlg.Flags = PD_RETURNDC | PD_ALLPAGES;
  s_pdlg.hwndOwner = GetActiveWindow(); // Identifies the window that owns the dialog box.
  s_pdlg.hDC = nullptr;
  s_pdlg.nCopies = 1;
  s_pdlg.nFromPage = 1;
  s_pdlg.nToPage = (unsigned short int)-1;
  s_pdlg.nMinPage = 1;
  s_pdlg.nMaxPage = 0xFFFF;

  if (PrintDlg(&s_pdlg))
  {
    s_hDC = s_pdlg.hDC;

    if (s_hDC == nullptr)
    {
      lstrcpy(s_PrinterName, TEXT(""));
    }
    else
    {
      s_pDevMode = (LPDEVMODE)GlobalLock(s_pdlg.hDevMode);
      s_pDevNames = (LPDEVNAMES)GlobalLock(s_pdlg.hDevNames);

      // Note: pDevMode->dmDeviceName is limited to 32 characters.
      // if the printer name is greater than 32, like network printers,
      // the rr_getdc() function return a nullptr handle. So, I'm using
      // pDevNames instead pDevMode. (E.F.)
      // strcpy(PrinterName,pDevMode->dmDeviceName);

      pDevice = (TCHAR *)s_pDevNames + s_pDevNames->wDeviceOffset;
      lstrcpy(s_PrinterName, (TCHAR *)pDevice);

      HB_STORNI(s_pdlg.nFromPage, 1, 1);
      HB_STORNI(s_pdlg.nToPage, 1, 2);
      HB_STORNI(s_pDevMode->dmCopies > 1 ? s_pDevMode->dmCopies : s_pdlg.nCopies, 1, 3);
      if ((s_pdlg.Flags & PD_PAGENUMS) == PD_PAGENUMS)
      {
        HB_STORNI(2, 1, 4);
      }
      else if ((s_pdlg.Flags & PD_SELECTION) == PD_SELECTION)
      {
        HB_STORNI(1, 1, 4);
      }
      else
      {
        HB_STORNI(0, 1, 4);
      }

      rr_getdevmode();

      GlobalUnlock(s_pdlg.hDevMode);
      GlobalUnlock(s_pdlg.hDevNames);
    }
  }
  else
  {
    s_hDC = 0;
  }

  s_hDCRef = s_hDC;

  hmg_ret_HDC(s_hDC);
}

HB_FUNC(RR_GETDC)
{
  void *str;
  LPCTSTR pwszDevice = HB_PARSTR(1, &str, nullptr);

  if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
  {
    s_hDC = CreateDC(TEXT("WINSPOOL"), pwszDevice, nullptr, nullptr);
  }
  else
  {
    s_hDC = CreateDC(nullptr, pwszDevice, nullptr, nullptr);
  }

  if (s_hDC)
  {
    lstrcpy(s_PrinterName, pwszDevice);
    rr_getdevmode();
  }

  s_hDCRef = s_hDC;
  hmg_ret_HDC(s_hDC);

  hb_strfree(str);
}

void rr_getdevmode(void)
{
  DWORD dwNeeded = 0;

  memset(&s_pd, 0, sizeof(s_pd));
  s_pd.DesiredAccess = PRINTER_ALL_ACCESS;
  OpenPrinter(s_PrinterName, &s_hPrinter, nullptr);
  GetPrinter(s_hPrinter, 2, 0, 0, &dwNeeded);
  s_pi2 = (PRINTER_INFO_2 *)GlobalAlloc(GPTR, dwNeeded);
  GetPrinter(s_hPrinter, 2, reinterpret_cast<LPBYTE>(s_pi2), dwNeeded, &dwNeeded);
  s_pi22 = (PRINTER_INFO_2 *)GlobalAlloc(GPTR, dwNeeded);
  GetPrinter(s_hPrinter, 2, reinterpret_cast<LPBYTE>(s_pi22), dwNeeded, &dwNeeded);
  if (s_pDevMode)
  {
    s_pi2->pDevMode = s_pDevMode;
  }
  else if (s_pi2->pDevMode == nullptr)
  {
    dwNeeded = DocumentProperties(nullptr, s_hPrinter, s_PrinterName, nullptr, nullptr, 0);
    s_pDevMode2 = (DEVMODE *)GlobalAlloc(GPTR, dwNeeded);
    DocumentProperties(nullptr, s_hPrinter, s_PrinterName, s_pDevMode2, nullptr, DM_OUT_BUFFER);
    s_pi2->pDevMode = s_pDevMode2;
  }

  s_hfont = static_cast<HFONT>(GetCurrentObject(s_hDC, OBJ_FONT));
  s_hbrush = static_cast<HBRUSH>(GetCurrentObject(s_hDC, OBJ_BRUSH));
  s_hpen = static_cast<HPEN>(GetCurrentObject(s_hDC, OBJ_PEN));
}

HB_FUNC(EF_RESETPRINTER)
{
  if (s_pi22)
  {
    SetPrinter(s_hPrinter, 2, reinterpret_cast<LPBYTE>(s_pi22), 0);
  }

  GlobalFree(s_pi22);
  s_pi22 = nullptr;
}

HB_FUNC(RR_DELETEDC)
{
  if (s_pDevMode)
  {
    GlobalFree(s_pDevMode);
  }

  if (s_pDevMode2)
  {
    GlobalFree(s_pDevMode2);
  }

  if (s_pDevNames)
  {
    GlobalFree(s_pDevNames);
  }

  if (s_pi2)
  {
    GlobalFree(s_pi2);
  }

  DeleteDC(hmg_par_HDC(1));
}

HB_FUNC(RR_GETDEVICECAPS)
{
  TEXTMETRIC tm;
  auto xfont = hmg_par_HFONT(2);

  if (xfont != 0)
  {
    SelectObject(s_hDCRef, xfont);
  }

  GetTextMetrics(s_hDCRef, &tm);
  s_devcaps[1] = GetDeviceCaps(s_hDCRef, VERTSIZE);
  s_devcaps[2] = GetDeviceCaps(s_hDCRef, HORZSIZE);
  s_devcaps[3] = GetDeviceCaps(s_hDCRef, VERTRES);
  s_devcaps[4] = GetDeviceCaps(s_hDCRef, HORZRES);
  s_devcaps[5] = GetDeviceCaps(s_hDCRef, LOGPIXELSY);
  s_devcaps[6] = GetDeviceCaps(s_hDCRef, LOGPIXELSX);
  s_devcaps[7] = GetDeviceCaps(s_hDCRef, PHYSICALHEIGHT);
  s_devcaps[8] = GetDeviceCaps(s_hDCRef, PHYSICALWIDTH);
  s_devcaps[9] = GetDeviceCaps(s_hDCRef, PHYSICALOFFSETY);
  s_devcaps[10] = GetDeviceCaps(s_hDCRef, PHYSICALOFFSETX);

  s_devcaps[11] = tm.tmHeight;
  s_devcaps[12] = tm.tmAveCharWidth;
  s_devcaps[13] = ((s_devcaps[3] - tm.tmAscent) / tm.tmHeight);
  s_devcaps[14] = (s_devcaps[4] / tm.tmAveCharWidth);
  s_devcaps[15] = s_pi2->pDevMode->dmOrientation;
  s_devcaps[16] = tm.tmAscent;
  s_devcaps[17] = s_pi2->pDevMode->dmPaperSize;
  for (UINT i = 1; i <= hb_parinfa(1, 0); i++)
  {
    HB_STORNI(s_devcaps[i], 1, i);
  }

  if (xfont != 0)
  {
    SelectObject(s_hDCRef, s_hfont);
  }
}

HB_FUNC(RR_SETDEVMODE)
{
  DWORD what = hb_parnl(1);

  if (what == (s_pi2->pDevMode->dmFields & what))
  {
    s_pi2->pDevMode->dmFields = s_pi2->pDevMode->dmFields | what;

    if (what == DM_ORIENTATION)
    {
      s_pi2->pDevMode->dmOrientation = static_cast<short>(hb_parni(2));
    }

    if (what == DM_PAPERSIZE)
    {
      s_pi2->pDevMode->dmPaperSize = static_cast<short>(hb_parni(2));
    }

    if (what == DM_SCALE)
    {
      s_pi2->pDevMode->dmScale = static_cast<short>(hb_parni(2));
    }

    if (what == DM_COPIES)
    {
      s_pi2->pDevMode->dmCopies = static_cast<short>(hb_parni(2));
    }

    if (what == DM_DEFAULTSOURCE)
    {
      s_pi2->pDevMode->dmDefaultSource = static_cast<short>(hb_parni(2));
    }

    if (what == DM_PRINTQUALITY)
    {
      s_pi2->pDevMode->dmPrintQuality = static_cast<short>(hb_parni(2));
    }

    if (what == DM_COLOR)
    {
      s_pi2->pDevMode->dmColor = static_cast<short>(hb_parni(2));
    }

    if (what == DM_DUPLEX)
    {
      s_pi2->pDevMode->dmDuplex = static_cast<short>(hb_parni(2));
    }
  }

  DocumentProperties(nullptr, s_hPrinter, s_PrinterName, s_pi2->pDevMode, s_pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER);
  SetPrinter(s_hPrinter, 2, reinterpret_cast<LPBYTE>(s_pi2), 0);
  ResetDC(s_hDCRef, s_pi2->pDevMode);
  hmg_ret_HDC(s_hDCRef);
}

HB_FUNC(RR_SETUSERMODE)
{
  DWORD what = hb_parnl(1);

  if (what == (s_pi2->pDevMode->dmFields & what))
  {
    s_pi2->pDevMode->dmFields = s_pi2->pDevMode->dmFields | DM_PAPERSIZE | DM_PAPERWIDTH | DM_PAPERLENGTH;
    s_pi2->pDevMode->dmPaperSize = DMPAPER_USER;
    s_pi2->pDevMode->dmPaperWidth = static_cast<short>(hb_parnl(2));
    s_pi2->pDevMode->dmPaperLength = static_cast<short>(hb_parnl(3));
  }

  DocumentProperties(nullptr, s_hPrinter, s_PrinterName, s_pi2->pDevMode, s_pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER);
  SetPrinter(s_hPrinter, 2, reinterpret_cast<LPBYTE>(s_pi2), 0);
  ResetDC(s_hDCRef, s_pi2->pDevMode);
  hmg_ret_HDC(s_hDCRef);
}

#ifdef UNICODE
using _GETDEFAULTPRINTER = BOOL(WINAPI *)(LPWSTR, LPDWORD);
#define GETDEFAULTPRINTER "GetDefaultPrinterW"
#else
using _GETDEFAULTPRINTER = BOOL(WINAPI *)(LPSTR, LPDWORD);
#define GETDEFAULTPRINTER "GetDefaultPrinterA"
#endif
#define MAX_BUFFER_SIZE 254

HB_FUNC(RR_GETDEFAULTPRINTER)
{
  DWORD Needed, Returned;
  DWORD BuffSize = MAX_BUFFER_SIZE;
  LPPRINTER_INFO_5 PrinterInfo;
#ifdef UNICODE
  LPSTR pStr;
#endif

  if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
  { /* Windows 95 or 98 */
    EnumPrinters(PRINTER_ENUM_DEFAULT, nullptr, 5, nullptr, 0, &Needed, &Returned);
    PrinterInfo = (LPPRINTER_INFO_5)LocalAlloc(LPTR, Needed);
    EnumPrinters(PRINTER_ENUM_DEFAULT, nullptr, 5, reinterpret_cast<LPBYTE>(PrinterInfo), Needed, &Needed, &Returned);
    lstrcpy(s_PrinterDefault, PrinterInfo->pPrinterName);
    LocalFree(PrinterInfo);
  }
  else if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
  {
    if (s_osvi.dwMajorVersion == 5)
    { /* Windows 2000 or XP */
      BOOL bFlag;
      TCHAR lpPrinterName[MAX_BUFFER_SIZE];

      HMODULE hWinSpool = LoadLibrary(TEXT("winspool.drv"));
      if (!hWinSpool)
      {
        hb_retc("");
        return;
      }
      auto fnGetDefaultPrinter =
          reinterpret_cast<_GETDEFAULTPRINTER>(reinterpret_cast<void *>(GetProcAddress(hWinSpool, GETDEFAULTPRINTER)));
      if (!fnGetDefaultPrinter)
      {
        FreeLibrary(hWinSpool);
        hb_retc("");
        return;
      }

      bFlag = (*fnGetDefaultPrinter)(lpPrinterName, &BuffSize);
      lstrcpy(s_PrinterDefault, lpPrinterName);
      FreeLibrary(hWinSpool);
      if (!bFlag)
      {
        hb_retc("");
        return;
      }
    }
    else
    { /* Windows NT 4.0 or earlier */
      GetProfileString(TEXT("windows"), TEXT("device"), TEXT(""), s_PrinterDefault, BuffSize);
      _tcstok(s_PrinterDefault, TEXT(","));
    }
  }

#ifndef UNICODE
  hb_retc(s_PrinterDefault);
#else
  pStr = WideToAnsi(s_PrinterDefault);
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
}

#undef MAX_BUFFER_SIZE
#undef GETDEFAULTPRINTER

HB_FUNC(RR_GETPRINTERS)
{
  DWORD dwSize = 0;
  DWORD dwPrinters = 0;
  HGLOBAL pBuffer;
  HGLOBAL cBuffer;
  PRINTER_INFO_4 *pInfo4 = nullptr;
  PRINTER_INFO_5 *pInfo5 = nullptr;
  DWORD level;
  DWORD flags;
#ifdef UNICODE
  LPSTR pStr;
#endif

  s_osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&s_osvi);
  if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
  {
    level = 4;
    flags = PRINTER_ENUM_CONNECTIONS | PRINTER_ENUM_LOCAL;
  }
  else
  {
    level = 5;
    flags = PRINTER_ENUM_LOCAL;
  }

  EnumPrinters(flags, nullptr, level, nullptr, 0, &dwSize, &dwPrinters);

  pBuffer = static_cast<char *>(GlobalAlloc(GPTR, dwSize));
  if (pBuffer == nullptr)
  {
    hb_retc(",,");
    return;
  }

  EnumPrinters(flags, nullptr, level, static_cast<LPBYTE>(pBuffer), dwSize, &dwSize, &dwPrinters);

  if (dwPrinters == 0)
  {
    hb_retc(",,");
    GlobalFree(pBuffer);
    return;
  }

  cBuffer = static_cast<char *>(GlobalAlloc(GPTR, dwPrinters * 256));

  if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
  {
    pInfo4 = (PRINTER_INFO_4 *)pBuffer;
  }
  else
  {
    pInfo5 = (PRINTER_INFO_5 *)pBuffer;
  }

  for (DWORD i = 0; i < dwPrinters; i++)
  {
    if (s_osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
    {
#ifdef UNICODE
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), pInfo4->pPrinterName);
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), TEXT(","));
      if (pInfo4->Attributes == PRINTER_ATTRIBUTE_LOCAL)
      {
        lstrcat(reinterpret_cast<LPWSTR>(cBuffer), TEXT("local printer"));
      }
      else
      {
        lstrcat(reinterpret_cast<LPWSTR>(cBuffer), TEXT("network printer"));
      }
#else
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), pInfo4->pPrinterName);
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), ",");
      if (pInfo4->Attributes == PRINTER_ATTRIBUTE_LOCAL)
      {
        lstrcat(reinterpret_cast<LPSTR>(cBuffer), "local printer");
      }
      else
      {
        lstrcat(reinterpret_cast<LPSTR>(cBuffer), "network printer");
      }
#endif
      pInfo4++;
    }
    else
    {
#ifdef UNICODE
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), pInfo5->pPrinterName);
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), TEXT(","));
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), pInfo5->pPortName);
#else
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), pInfo5->pPrinterName);
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), ",");
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), pInfo5->pPortName);
#endif
      pInfo5++;
    }

    if (i < dwPrinters - 1)
    {
#ifdef UNICODE
      lstrcat(reinterpret_cast<LPWSTR>(cBuffer), TEXT(",,"));
#else
      lstrcat(reinterpret_cast<LPSTR>(cBuffer), ",,");
#endif
    }
  }

#ifndef UNICODE
  hb_retc(static_cast<const char *>(cBuffer));
#else
  pStr = WideToAnsi(reinterpret_cast<LPWSTR>(cBuffer));
  hb_retc(pStr);
  hb_xfree(pStr);
#endif
  GlobalFree(pBuffer);
  GlobalFree(cBuffer);
}

HB_FUNC(RR_STARTDOC)
{
  void *str;
  LPCTSTR lpText = HB_PARSTR(1, &str, nullptr);
  memset(&s_di, 0, sizeof(s_di));
  s_di.cbSize = sizeof(s_di);
  s_di.lpszDocName = lpText;
  StartDoc(s_hDC, &s_di);
  hb_strfree(str);
}

HB_FUNC(RR_STARTPAGE)
{
  StartPage(s_hDC);
  SetTextAlign(s_hDC, TA_BASELINE);
}

HB_FUNC(RR_ENDPAGE)
{
  EndPage(s_hDC);
}

HB_FUNC(RR_ENDDOC)
{
  EndDoc(s_hDC);
}

HB_FUNC(RR_ABORTDOC)
{
  AbortDoc(s_hDC);
  DeleteDC(s_hDC);
}

HB_FUNC(RR_DEVICECAPABILITIES)
{
  HGLOBAL cGBuffer, pGBuffer, nGBuffer, sGBuffer, bnGBuffer, bwGBuffer, bcGBuffer;
  TCHAR *cBuffer, *pBuffer, *nBuffer, *sBuffer, *bnBuffer, *bwBuffer, *bcBuffer;
  DWORD numpapers, numbins;
  LPPOINT lp;
  TCHAR buffer[sizeof(long) * 8 + 1];
#ifdef UNICODE
  LPSTR pStr;
#endif

#ifdef UNICODE
  numpapers = DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                                 AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_PAPERNAMES, nullptr, nullptr);
#else
  numpapers = DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_PAPERNAMES, nullptr, nullptr);
#endif
  if (numpapers > 0)
  {
    pGBuffer = GlobalAlloc(GPTR, numpapers * 64);
    nGBuffer = GlobalAlloc(GPTR, numpapers * sizeof(WORD));
    sGBuffer = GlobalAlloc(GPTR, numpapers * sizeof(POINT));
    cGBuffer = GlobalAlloc(GPTR, numpapers * 128);
    pBuffer = (TCHAR *)pGBuffer;
    nBuffer = (TCHAR *)nGBuffer;
    sBuffer = (TCHAR *)sGBuffer;
    cBuffer = (TCHAR *)cGBuffer;
#ifdef UNICODE
    DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                       AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_PAPERNAMES, pBuffer, s_pi2->pDevMode);
    DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                       AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_PAPERS, nBuffer, s_pi2->pDevMode);
    DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                       AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_PAPERSIZE, sBuffer, s_pi2->pDevMode);
#else
    DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_PAPERNAMES, pBuffer, s_pi2->pDevMode);
    DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_PAPERS, nBuffer, s_pi2->pDevMode);
    DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_PAPERSIZE, sBuffer, s_pi2->pDevMode);
#endif
    cBuffer[0] = 0;
    for (DWORD i = 0; i < numpapers; i++)
    {
      lstrcat(cBuffer, pBuffer);
      lstrcat(cBuffer, TEXT(","));
      lstrcat(cBuffer, _itot(*nBuffer, buffer, 10));
      lstrcat(cBuffer, TEXT(","));

      lp = (LPPOINT)sBuffer;
      lstrcat(cBuffer, _ltot(lp->x, buffer, 10));
      lstrcat(cBuffer, TEXT(","));
      lstrcat(cBuffer, _ltot(lp->y, buffer, 10));
      if (i < numpapers - 1)
      {
        lstrcat(cBuffer, TEXT(",,"));
      }
      pBuffer += 64;
      nBuffer += sizeof(WORD);
      sBuffer += sizeof(POINT);
    }

#ifndef UNICODE
    hb_storc(cBuffer, 1);
#else
    pStr = WideToAnsi(cBuffer);
    hb_storc(pStr, 1);
    hb_xfree(pStr);
#endif

    GlobalFree(cGBuffer);
    GlobalFree(pGBuffer);
    GlobalFree(nGBuffer);
    GlobalFree(sGBuffer);
  }
  else
  {
    hb_storc("", 1);
  }

#ifdef UNICODE
  numbins = DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                               AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_BINNAMES, nullptr, nullptr);
#else
  numbins = DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_BINNAMES, nullptr, nullptr);
#endif
  if (numbins > 0)
  {
    bnGBuffer = GlobalAlloc(GPTR, numbins * 24);
    bwGBuffer = GlobalAlloc(GPTR, numbins * sizeof(WORD));
    bcGBuffer = GlobalAlloc(GPTR, numbins * 64);
    bnBuffer = (TCHAR *)bnGBuffer;
    bwBuffer = (TCHAR *)bwGBuffer;
    bcBuffer = (TCHAR *)bcGBuffer;
#ifdef UNICODE
    DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                       AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_BINNAMES, bnBuffer, s_pi2->pDevMode);
    DeviceCapabilities(AnsiToWide(static_cast<LPSTR>(s_pi2->pPrinterName)),
                       AnsiToWide(static_cast<LPSTR>(s_pi2->pPortName)), DC_BINS, bwBuffer, s_pi2->pDevMode);
#else
    DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_BINNAMES, bnBuffer, s_pi2->pDevMode);
    DeviceCapabilities(s_pi2->pPrinterName, s_pi2->pPortName, DC_BINS, bwBuffer, s_pi2->pDevMode);
#endif
    bcBuffer[0] = 0;
    for (DWORD i = 0; i < numbins; i++)
    {
      lstrcat(bcBuffer, bnBuffer);
      lstrcat(bcBuffer, TEXT(","));
      lstrcat(bcBuffer, _itot(*bwBuffer, buffer, 10));

      if (i < numbins - 1)
      {
        lstrcat(bcBuffer, TEXT(",,"));
      }
      bnBuffer += 24;
      bwBuffer += sizeof(WORD);
    }

#ifndef UNICODE
    hb_storc(bcBuffer, 2);
#else
    pStr = WideToAnsi(bcBuffer);
    hb_storc(pStr, 2);
    hb_xfree(pStr);
#endif

    GlobalFree(bnGBuffer);
    GlobalFree(bwGBuffer);
    GlobalFree(bcGBuffer);
  }
  else
  {
    hb_storc("", 2);
  }
}

HB_FUNC(RR_SETPOLYFILLMODE)
{
  if (SetPolyFillMode(s_hDC, hmg_par_COLORREF(1)) != 0)
  {
    hb_retnl(hb_parnl(1));
  }
  else
  {
    hb_retnl(GetPolyFillMode(s_hDC));
  }
}

HB_FUNC(RR_SETTEXTCOLOR)
{
  if (SetTextColor(s_hDC, hmg_par_COLORREF(1)) != CLR_INVALID)
  {
    hb_retnl(hb_parnl(1));
  }
  else
  {
    hb_retnl(GetTextColor(s_hDC));
  }
}

HB_FUNC(RR_SETBKCOLOR)
{
  if (SetBkColor(s_hDC, hmg_par_COLORREF(1)) != CLR_INVALID)
  {
    hb_retnl(hb_parnl(1));
  }
  else
  {
    hb_retnl(GetBkColor(s_hDC));
  }
}

HB_FUNC(RR_SETBKMODE)
{
  if (hb_parni(1) == 1)
  {
    SetBkMode(s_hDC, TRANSPARENT);
  }
  else
  {
    SetBkMode(s_hDC, OPAQUE);
  }
}

HB_FUNC(RR_DELETEOBJECTS)
{
  for (UINT i = 2; i <= hb_parinfa(1, 0); i++)
  {
    DeleteObject(reinterpret_cast<HGDIOBJ>(HB_PARVNL(1, i)));
  }
}

HB_FUNC(RR_DELETEIMAGELISTS)
{
  for (UINT i = 1; i <= hb_parinfa(1, 0); i++)
  {
    ImageList_Destroy((HIMAGELIST)HB_PARNL3(1, i, 1));
  }
}

HB_FUNC(RR_SAVEMETAFILE)
{
  void *str;
  CopyEnhMetaFile((HENHMETAFILE)HB_PARNL(1), HB_PARSTR(2, &str, nullptr));
  hb_strfree(str);
}

HB_FUNC(RR_GETCURRENTOBJECT)
{
  auto what = hb_parni(1);
  HGDIOBJ hand;

  if (what == 1)
  {
    hand = GetCurrentObject(s_hDC, OBJ_FONT);
  }
  else if (what == 2)
  {
    hand = GetCurrentObject(s_hDC, OBJ_BRUSH);
  }
  else
  {
    hand = GetCurrentObject(s_hDC, OBJ_PEN);
  }

  hmg_ret_HGDIOBJ(hand);
}

HB_FUNC(RR_GETSTOCKOBJECT)
{
  hmg_ret_HGDIOBJ(GetStockObject(hb_parni(1)));
}

HB_FUNC(RR_CREATEPEN)
{
  hmg_ret_HPEN(CreatePen(hb_parni(1), hb_parni(2), hmg_par_COLORREF(3)));
}

HB_FUNC(RR_MODIFYPEN)
{
  int i;
  HPEN hp;

  LOGPEN ppn{};
  i = GetObject(hmg_par_HPEN(1), sizeof(LOGPEN), &ppn);
  if (i > 0)
  {
    if (hb_parni(2) >= 0)
    {
      ppn.lopnStyle = hmg_par_UINT(2);
    }

    if (hb_parnl(3) >= 0)
    {
      ppn.lopnWidth.x = hb_parnl(3);
    }

    if (hb_parnl(4) >= 0)
    {
      ppn.lopnColor = hmg_par_COLORREF(4);
    }

    hp = CreatePenIndirect(&ppn);
    if (hp != nullptr)
    {
      DeleteObject(hmg_par_HPEN(1));
      hmg_ret_HPEN(hp);
    }
    else
    {
      hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HPEN/hmg_ret_HPEN ?
    }
  }
  else
  {
    hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HPEN/hmg_ret_HPEN ?
  }
}

HB_FUNC(RR_SELECTPEN)
{
  SelectObject(s_hDC, hmg_par_HPEN(1));
  s_hpen = hmg_par_HPEN(1);
}

HB_FUNC(RR_CREATEBRUSH)
{
  LOGBRUSH pbr;

  pbr.lbStyle = hb_parni(1);
  pbr.lbColor = hmg_par_COLORREF(2);
  pbr.lbHatch = hmg_par_LONG(3);
  hmg_ret_HBRUSH(CreateBrushIndirect(&pbr));
}

HB_FUNC(RR_MODIFYBRUSH)
{
  int i;
  HBRUSH hb;

  LOGBRUSH ppn{};
  i = GetObject(hmg_par_HBRUSH(1), sizeof(LOGBRUSH), &ppn);
  if (i > 0)
  {
    if (hb_parni(2) >= 0)
    {
      ppn.lbStyle = hmg_par_UINT(2);
    }

    if (hb_parnl(3) >= 0)
    {
      ppn.lbColor = hmg_par_COLORREF(3);
    }

    if (hb_parnl(4) >= 0)
    {
      ppn.lbHatch = hb_parnl(4);
    }

    hb = CreateBrushIndirect(&ppn);
    if (hb != nullptr)
    {
      DeleteObject(hmg_par_HBRUSH(1));
      hmg_ret_HBRUSH(hb);
    }
    else
    {
      hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HBRUSH/hmg_ret_HBRUSH ?
    }
  }
  else
  {
    hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HBRUSHhmg_ret_HBRUSH ?
  }
}

HB_FUNC(RR_SELECTBRUSH)
{
  SelectObject(s_hDC, hmg_par_HBRUSH(1));
  s_hbrush = hmg_par_HBRUSH(1);
}

HB_FUNC(RR_CREATEFONT)
{
#ifndef UNICODE
  TCHAR *FontName = (TCHAR *)hb_parc(1);
#else
  TCHAR *FontName = AnsiToWide(const_cast<char *>(hb_parc(1)));
#endif
  auto FontSize = hb_parni(2);
  LONG FontWidth = hb_parnl(3);
  LONG Orient = hb_parnl(4);
  LONG Weight = hb_parnl(5);
  auto Italic = hb_parni(6);
  auto Underline = hb_parni(7);
  auto Strikeout = hb_parni(8);
  HFONT oldfont;
  LONG newWidth, FontHeight;
  TEXTMETRIC tm;
  BYTE bItalic, bUnderline, bStrikeOut;

  newWidth = FontWidth;
  if (FontSize <= 0)
  {
    FontSize = 10;
  }

  if (FontWidth < 0)
  {
    newWidth = 0;
  }

  if (Orient <= 0)
  {
    Orient = 0;
  }

  if (Weight <= 0)
  {
    Weight = FW_NORMAL;
  }
  else
  {
    Weight = FW_BOLD;
  }

  if (Italic <= 0)
  {
    bItalic = 0;
  }
  else
  {
    bItalic = 1;
  }

  if (Underline <= 0)
  {
    bUnderline = 0;
  }
  else
  {
    bUnderline = 1;
  }

  if (Strikeout <= 0)
  {
    bStrikeOut = 0;
  }
  else
  {
    bStrikeOut = 1;
  }

  FontHeight = -MulDiv(FontSize, GetDeviceCaps(s_hDCRef, LOGPIXELSY), 72);
  auto hxfont = CreateFont(FontHeight, newWidth, Orient, Orient, Weight, bItalic, bUnderline, bStrikeOut, s_charset,
                           OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName);
  if (FontWidth < 0)
  {
    oldfont = static_cast<HFONT>(SelectObject(s_hDC, hxfont));
    GetTextMetrics(s_hDC, &tm);
    SelectObject(s_hDC, oldfont);
    DeleteObject(hxfont);
    newWidth = static_cast<int>(static_cast<float>(-(tm.tmAveCharWidth + tm.tmOverhang)) * FontWidth / 100);
    hxfont = CreateFont(FontHeight, newWidth, Orient, Orient, Weight, bItalic, bUnderline, bStrikeOut, s_charset,
                        OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE, FontName);
  }

  hmg_ret_HFONT(hxfont);

#ifdef UNICODE
  hb_xfree(FontName);
#endif
}

HB_FUNC(RR_MODIFYFONT)
{
  int i;
  HFONT hf;
  LONG nHeight;

  LOGFONT ppn{};
  i = GetObject(hmg_par_HFONT(1), sizeof(LOGFONT), &ppn);
  if (i > 0)
  {
    //     if (hb_parc(2)!="")
    //       ppn.lfFaceName = hb_parc(2);

    if (hb_parni(3) > 0)
    {
      nHeight = -MulDiv(hb_parni(3), GetDeviceCaps(s_hDC, LOGPIXELSY), 72);
      ppn.lfHeight = nHeight;
    }

    if (hb_parnl(4) >= 0)
    {
      ppn.lfWidth = hmg_par_LONG(4) * ppn.lfWidth / 100;
    }

    if (hb_parnl(5) >= 0)
    {
      ppn.lfOrientation = hb_parnl(5);
      ppn.lfEscapement = hb_parnl(5);
    }

    if (hb_parnl(6) >= 0)
    {
      if (hb_parnl(6) == 0)
      {
        ppn.lfWeight = FW_NORMAL;
      }
      else
      {
        ppn.lfWeight = FW_BOLD;
      }
    }

    if (hb_parni(7) >= 0)
    {
      ppn.lfItalic = hmg_par_BYTE(7);
    }

    if (hb_parni(8) >= 0)
    {
      ppn.lfUnderline = hmg_par_BYTE(8);
    }

    if (hb_parni(9) >= 0)
    {
      ppn.lfStrikeOut = hmg_par_BYTE(9);
    }

    hf = CreateFontIndirect(&ppn);
    if (hf != nullptr)
    {
      DeleteObject(hmg_par_HFONT(1));
      hmg_ret_HFONT(hf);
    }
    else
    {
      hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HFONT/hmg_ret_HFONT ?
    }
  }
  else
  {
    hb_retnl(hmg_par_LONG(1)); // TODO: hmg_par_HFONT/hmg_ret_HFONT ?
  }
}

HB_FUNC(RR_SELECTFONT)
{
  SelectObject(s_hDC, hmg_par_HFONT(1));
  s_hfont = hmg_par_HFONT(1);
}

HB_FUNC(RR_SETCHARSET)
{
  s_charset = hmg_par_DWORD(1);
}

HB_FUNC(RR_TEXTOUT)
{
#ifndef UNICODE
  LPTSTR lpText = const_cast<LPTSTR>(hb_parc(1));
#else
  LPCTSTR lpText = AnsiToWide(const_cast<char *>(hb_parc(1)));
#endif
  HGDIOBJ xfont = hmg_par_HFONT(3);
  HFONT prevfont = nullptr;
  SIZE szMetric;
  auto lspace = hb_parni(4);

  if (xfont != 0)
  {
    prevfont = static_cast<HFONT>(SelectObject(s_hDC, xfont));
  }

  if (s_textjust > 0)
  {
    GetTextExtentPoint32(s_hDC, lpText, lstrlen(lpText), &szMetric);
    if (szMetric.cx < s_textjust)
    { // or can be for better look (szMetric.cx>(int) textjust*2/3)
      if (lspace > 0)
      {
        SetTextJustification(s_hDC, s_textjust - szMetric.cx, lspace);
      }
    }
  }

  hb_retl(TextOut(s_hDC, HB_PARNI(2, 2), HB_PARNI(2, 1) + s_devcaps[16], lpText, lstrlen(lpText)));
  if (xfont != 0)
  {
    SelectObject(s_hDC, prevfont);
  }

  if (s_textjust > 0)
  {
    SetTextJustification(s_hDC, 0, 0);
  }

#ifdef UNICODE
  hb_xfree((TCHAR *)lpText);
#endif
}

HB_FUNC(RR_DRAWTEXT)
{
#ifndef UNICODE
  LPCSTR pszData = hb_parc(3);
#else
  LPCWSTR pszData = AnsiToWide(const_cast<char *>(hb_parc(3)));
#endif
  int iLen = lstrlen(pszData);
  HGDIOBJ xfont = hmg_par_HFONT(5);
  HFONT prevfont = nullptr;
  RECT rect;
  UINT uFormat;

  SIZE sSize;
  auto iStyle = hb_parni(4);
  LONG w, h;

  SetRect(&rect, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1));

  if (xfont != 0)
  {
    prevfont = static_cast<HFONT>(SelectObject(s_hDC, xfont));
  }

  GetTextExtentPoint32(s_hDC, pszData, iLen, &sSize);
  w = sSize.cx; // text width
  h = sSize.cy; // text height

  // Center text vertically within rectangle
  if (w < rect.right - rect.left)
  {
    rect.top = rect.top + (rect.bottom - rect.top + h / 2) / 2;
  }
  else
  {
    rect.top = rect.top + (rect.bottom - rect.top - h / 2) / 2;
  }

  uFormat = DT_NOCLIP | DT_NOPREFIX | DT_WORDBREAK | DT_END_ELLIPSIS;

  if (iStyle == 0)
  {
    uFormat = uFormat | DT_LEFT;
  }
  else if (iStyle == 2)
  {
    uFormat = uFormat | DT_RIGHT;
  }
  else if (iStyle == 1)
  {
    uFormat = uFormat | DT_CENTER;
  }

  hb_retni(DrawText(s_hDC, pszData, -1, &rect, uFormat));
  if (xfont != 0)
  {
    SelectObject(s_hDC, prevfont);
  }

#ifdef UNICODE
  hb_xfree((TCHAR *)pszData);
#endif
}

HB_FUNC(RR_RECTANGLE)
{
  LONG_PTR xpen = HB_PARNL(3);
  LONG_PTR xbrush = HB_PARNL(4);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  hb_retni(Rectangle(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1)));
  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, s_hbrush);
  }
}

HB_FUNC(RR_CLOSEMFILE)
{
  DeleteEnhMetaFile(CloseEnhMetaFile(s_hDC));
}

HB_FUNC(RR_CREATEMFILE)
{
  RECT emfrect;
  SetRect(&emfrect, 0, 0, GetDeviceCaps(s_hDCRef, HORZSIZE) * 100, GetDeviceCaps(s_hDCRef, VERTSIZE) * 100);
  void *str;
  s_hDC = CreateEnhMetaFile(s_hDCRef, HB_PARSTR(1, &str, nullptr), &emfrect, TEXT("hbprinter\0emf file\0\0"));
  hb_strfree(str);
  SetTextAlign(s_hDC, TA_BASELINE);
  s_preview = 1;
  hmg_ret_HDC(s_hDC);
}

HB_FUNC(RR_DELETECLIPRGN)
{
  SelectClipRgn(s_hDC, nullptr);
}

HB_FUNC(RR_CREATERGN)
{
  POINT lpp;

  GetViewportOrgEx(s_hDC, &lpp);
  if (hb_parni(3) == 2)
  {
    hmg_ret_HRGN(CreateEllipticRgn(HB_PARNI(1, 2) + lpp.x, HB_PARNI(1, 1) + lpp.y, HB_PARNI(2, 2) + lpp.x,
                                   HB_PARNI(2, 1) + lpp.y));
  }
  else if (hb_parni(3) == 3)
  {
    hmg_ret_HRGN(CreateRoundRectRgn(HB_PARNI(1, 2) + lpp.x, HB_PARNI(1, 1) + lpp.y, HB_PARNI(2, 2) + lpp.x,
                                    HB_PARNI(2, 1) + lpp.y, HB_PARNI(4, 2) + lpp.x, HB_PARNI(4, 1) + lpp.y));
  }
  else
  {
    hmg_ret_HRGN(
        CreateRectRgn(HB_PARNI(1, 2) + lpp.x, HB_PARNI(1, 1) + lpp.y, HB_PARNI(2, 2) + lpp.x, HB_PARNI(2, 1) + lpp.y));
  }
}

HB_FUNC(RR_CREATEPOLYGONRGN)
{
  auto number = static_cast<int>(hb_parinfa(1, 0));
  POINT apoints[1024];

  for (auto i = 0; i <= number - 1; i++)
  {
    apoints[i].x = HB_PARNI(1, i + 1);
    apoints[i].y = HB_PARNI(2, i + 1);
  }

  hmg_ret_HRGN(CreatePolygonRgn(apoints, number, hb_parni(3)));
}

HB_FUNC(RR_COMBINERGN)
{
  auto rgnnew = CreateRectRgn(0, 0, 1, 1);

  CombineRgn(rgnnew, hmg_par_HRGN(1), hmg_par_HRGN(2), hb_parni(3));
  hmg_ret_HRGN(rgnnew);
}

HB_FUNC(RR_SELECTCLIPRGN)
{
  SelectClipRgn(s_hDC, hmg_par_HRGN(1));
  s_hrgn = hmg_par_HRGN(1);
}

HB_FUNC(RR_SETVIEWPORTORG)
{
  hb_retl(SetViewportOrgEx(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), nullptr));
}

HB_FUNC(RR_GETVIEWPORTORG)
{
  POINT lpp;

  hb_retl(GetViewportOrgEx(s_hDC, &lpp));
  HB_STORVNL(lpp.x, 1, 2);
  HB_STORVNL(lpp.y, 1, 1);
}

HB_FUNC(RR_SETRGB)
{
  hb_retnl(RGB(hb_parni(1), hb_parni(2), hb_parni(3)));
}

HB_FUNC(RR_SETTEXTCHAREXTRA)
{
  hb_retni(SetTextCharacterExtra(s_hDC, hb_parni(1)));
}

HB_FUNC(RR_GETTEXTCHAREXTRA)
{
  hb_retni(GetTextCharacterExtra(s_hDC));
}

HB_FUNC(RR_SETTEXTJUSTIFICATION)
{
  s_textjust = hb_parni(1);
}

HB_FUNC(RR_GETTEXTJUSTIFICATION)
{
  hb_retni(s_textjust);
}

HB_FUNC(RR_GETTEXTALIGN)
{
  hb_retni(GetTextAlign(s_hDC));
}

HB_FUNC(RR_SETTEXTALIGN)
{
  hb_retni(SetTextAlign(s_hDC, TA_BASELINE | hb_parni(1)));
}

HB_FUNC(RR_PICTURE)
{
  IStream *iStream;
  IPicture *iPicture;
  void *pGlobal;
  DWORD nReadByte;
  long lWidth, lHeight;
  int x, y, xe, ye;
  int r = HB_PARNI(2, 1);
  int c = HB_PARNI(2, 2);
  int dr = HB_PARNI(3, 1);
  int dc = HB_PARNI(3, 2);
  int tor = HB_PARNI(4, 1);
  int toc = HB_PARNI(4, 2);
  POINT lpp;

  void *str;
  auto hFile =
      CreateFile(HB_PARSTR(1, &str, nullptr), GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
  hb_strfree(str);

  if (hFile == INVALID_HANDLE_VALUE)
  {
    return;
  }

  auto nFileSize = GetFileSize(hFile, nullptr);
  auto hGlobal = GlobalAlloc(GMEM_MOVEABLE, nFileSize);
  pGlobal = GlobalLock(hGlobal);
  ReadFile(hFile, pGlobal, nFileSize, &nReadByte, nullptr);
  CloseHandle(hFile);
  GlobalUnlock(hGlobal);
  CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
  OleLoadPicture(iStream, nFileSize, TRUE, IID_IPicture, (LPVOID *)&iPicture);
  GlobalFree(hGlobal);
  iStream->lpVtbl->Release(iStream);
  if (iPicture == nullptr)
  {
    return;
  }

  iPicture->lpVtbl->get_Width(iPicture, &lWidth);
  iPicture->lpVtbl->get_Height(iPicture, &lHeight);
  if (dc == 0)
  {
    dc = static_cast<int>(static_cast<float>(dr) * lWidth / lHeight);
  }

  if (dr == 0)
  {
    dr = static_cast<int>(static_cast<float>(dc) * lHeight / lWidth);
  }

  if (tor <= 0)
  {
    tor = dr;
  }

  if (toc <= 0)
  {
    toc = dc;
  }

  x = c;
  y = r;
  xe = c + toc - 1;
  ye = r + tor - 1;
  GetViewportOrgEx(s_hDC, &lpp);
  auto hrgn1 = CreateRectRgn(c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y);
  if (s_hrgn == nullptr)
  {
    SelectClipRgn(s_hDC, hrgn1);
  }
  else
  {
    ExtSelectClipRgn(s_hDC, hrgn1, RGN_AND);
  }

  while (x < xe)
  {
    while (y < ye)
    {
      iPicture->lpVtbl->Render(iPicture, s_hDC, x, y, dc, dr, 0, lHeight, lWidth, -lHeight, nullptr);
      y += dr;
    }

    y = r;
    x += dc;
  }

  iPicture->lpVtbl->Release(iPicture);
  SelectClipRgn(s_hDC, s_hrgn);
  DeleteObject(hrgn1);
  hb_retni(0);
}

LPVOID rr_loadpicturefromresource(const TCHAR *resname, LONG *lwidth, LONG *lheight)
{
  IPicture *iPicture = nullptr;
  IStream *iStream = nullptr;
  PICTDESC picd;
  HGLOBAL hGlobalres;
  HGLOBAL hGlobal;
  HRSRC hSource;
  LPVOID lpVoid;
  int nSize;

  auto hbmpx = static_cast<HBITMAP>(LoadImage(GetResources(), resname, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));
  if (hbmpx != nullptr)
  {
    picd.cbSizeofstruct = sizeof(PICTDESC);
    picd.picType = PICTYPE_BITMAP;
    picd.bmp.hbitmap = hbmpx;
    OleCreatePictureIndirect(&picd, IID_IPicture, TRUE, (LPVOID *)&iPicture);
  }
  else
  {
    hSource = FindResource(GetResources(), resname, TEXT("HMGPICTURE"));
    if (hSource == nullptr)
    {
      return nullptr;
    }

    hGlobalres = LoadResource(GetResources(), hSource);
    if (hGlobalres == nullptr)
    {
      return nullptr;
    }

    lpVoid = LockResource(hGlobalres);
    if (lpVoid == nullptr)
    {
      return nullptr;
    }

    nSize = SizeofResource(GetResources(), hSource);
    hGlobal = GlobalAlloc(GPTR, nSize);
    if (hGlobal == nullptr)
    {
      return nullptr;
    }

    memcpy(hGlobal, lpVoid, nSize);
    FreeResource(hGlobalres);
    CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
    if (iStream == nullptr)
    {
      GlobalFree(hGlobal);
      return nullptr;
    }

    OleLoadPicture(iStream, nSize, TRUE, IID_IPicture, (LPVOID *)&iPicture);
    iStream->lpVtbl->Release(iStream);
    GlobalFree(hGlobal);
  }

  if (iPicture != nullptr)
  {
    iPicture->lpVtbl->get_Width(iPicture, lwidth);
    iPicture->lpVtbl->get_Height(iPicture, lheight);
  }

  return iPicture;
}

LPVOID rr_loadpicture(const TCHAR *filename, LONG *lwidth, LONG *lheight)
{
  IStream *iStream = nullptr;
  IPicture *iPicture = nullptr;
  HGLOBAL hGlobal;
  void *pGlobal;
  DWORD nReadByte;

  auto hFile = CreateFile(filename, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
  if (hFile == INVALID_HANDLE_VALUE)
  {
    return nullptr;
  }

  auto nFileSize = GetFileSize(hFile, nullptr);
  hGlobal = GlobalAlloc(GMEM_MOVEABLE, nFileSize + 4096);
  pGlobal = GlobalLock(hGlobal);
  ReadFile(hFile, pGlobal, nFileSize, &nReadByte, nullptr);
  CloseHandle(hFile);

  CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
  if (iStream == nullptr)
  {
    GlobalUnlock(hGlobal);
    GlobalFree(hGlobal);
    return nullptr;
  }

  OleLoadPicture(iStream, nFileSize, TRUE, IID_IPicture, (LPVOID *)&iPicture);
  GlobalUnlock(hGlobal);
  GlobalFree(hGlobal);
  iStream->lpVtbl->Release(iStream);
  iStream = nullptr;
  if (iPicture != nullptr)
  {
    iPicture->lpVtbl->get_Width(iPicture, lwidth);
    iPicture->lpVtbl->get_Height(iPicture, lheight);
  }

  return iPicture;
}

LPVOID rr_loadfromhbitmap(HBITMAP hbmpx, LONG *lwidth, LONG *lheight)
{
  IPicture *iPicture = nullptr;
  PICTDESC picd;

  picd.cbSizeofstruct = sizeof(PICTDESC);
  picd.picType = PICTYPE_BITMAP;
  picd.bmp.hbitmap = hbmpx;
  picd.bmp.hpal = nullptr;

  OleCreatePictureIndirect(&picd, IID_IPicture, TRUE, (LPVOID *)&iPicture);
  if (iPicture != nullptr)
  {
    iPicture->lpVtbl->get_Width(iPicture, lwidth);
    iPicture->lpVtbl->get_Height(iPicture, lheight);
  }

  return iPicture;
}

HB_FUNC(RR_DRAWPICTURE)
{
  void *str;
  LPCTSTR cFileName = HB_PARSTR(1, &str, nullptr);
  IPicture *ipic;
  int x, y, xe, ye;
  int r = HB_PARNI(2, 1);
  int c = HB_PARNI(2, 2);
  int dr = HB_PARNI(3, 1);
  int dc = HB_PARNI(3, 2);
  int tor = HB_PARNI(4, 1);
  int toc = HB_PARNI(4, 2);
  long lwidth = 0;
  long lheight = 0;
  RECT lrect;
  POINT lpp;
  int lw, lh;

  ipic = (IPicture *)rr_loadpicture(cFileName, &lwidth, &lheight);
  if (ipic == nullptr)
  {
    ipic = (IPicture *)rr_loadpicturefromresource(cFileName, &lwidth, &lheight);
  }
  if (ipic == nullptr && HB_ISNUM(1))
  {
    ipic = (IPicture *)rr_loadfromhbitmap(hmg_par_HBITMAP(1), &lwidth, &lheight);
  }
  if (ipic == nullptr)
  {
    hb_strfree(str);
    return;
  }

  lw = MulDiv(lwidth, s_devcaps[6], 2540);
  lh = MulDiv(lheight, s_devcaps[5], 2540);
  if (dc == 0)
  {
    dc = static_cast<int>(static_cast<float>(dr) * lw / lh);
  }

  if (dr == 0)
  {
    dr = static_cast<int>(static_cast<float>(dc) * lh / lw);
  }

  if (tor <= 0)
  {
    tor = dr;
  }

  if (toc <= 0)
  {
    toc = dc;
  }

  x = c;
  y = r;
  xe = c + toc - 1;
  ye = r + tor - 1;
  GetViewportOrgEx(s_hDC, &lpp);
  auto hrgn1 = CreateRectRgn(c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y);
  if (s_hrgn == nullptr)
  {
    SelectClipRgn(s_hDC, hrgn1);
  }
  else
  {
    ExtSelectClipRgn(s_hDC, hrgn1, RGN_AND);
  }

  while (x < xe)
  {
    while (y < ye)
    {
      SetRect(&lrect, x, y, dc + x, dr + y);
      ipic->lpVtbl->Render(ipic, s_hDC, x, y, dc, dr, 0, lheight, lwidth, -lheight, &lrect);
      y += dr;
    }

    y = r;
    x += dc;
  }

  ipic->lpVtbl->Release(ipic);
  SelectClipRgn(s_hDC, s_hrgn);
  DeleteObject(hrgn1);
  hb_retni(0);

  hb_strfree(str);
}

HB_FUNC(RR_CREATEIMAGELIST)
{
  void *str;
  LPCTSTR cFileName = HB_PARSTR(1, &str, nullptr);
  BITMAP bm;
  HIMAGELIST himl;
  int dx, number;

  auto hbmpx = static_cast<HBITMAP>(LoadImage(0, cFileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION));
  if (hbmpx == nullptr)
  {
    hbmpx =
        static_cast<HBITMAP>(LoadImage(GetModuleHandle(nullptr), cFileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION));
  }

  if (hbmpx == nullptr)
  {
    hb_strfree(str);
    return;
  }

  GetObject(hbmpx, sizeof(BITMAP), &bm);
  number = HB_ISNIL(2) ? 0 : hb_parni(2);
  if (number == 0)
  {
    number = bm.bmWidth / bm.bmHeight;
    dx = bm.bmHeight;
  }
  else
  {
    dx = bm.bmWidth / number;
  }

  himl = ImageList_Create(dx, bm.bmHeight, ILC_COLOR24 | ILC_MASK, number, 0);
  ImageList_AddMasked(himl, hbmpx, CLR_DEFAULT);
  hb_storni(dx, 3);
  hb_storni(bm.bmHeight, 4);
  DeleteObject(hbmpx);
  hmg_ret_HIMAGELIST(himl);

  hb_strfree(str);
}

HB_FUNC(RR_DRAWIMAGELIST)
{
  auto himl = hmg_par_HIMAGELIST(1);
  RECT rect;
  auto hwnd = GetActiveWindow();

  rect.left = HB_PARNI(3, 2);
  rect.top = HB_PARNI(3, 1);
  rect.right = HB_PARNI(4, 2);
  rect.bottom = HB_PARNI(4, 1);
  auto temp2dc = GetWindowDC(hwnd);
  auto tempdc = CreateCompatibleDC(temp2dc);
  auto hbmpx = CreateCompatibleBitmap(temp2dc, hb_parni(5), hb_parni(6));
  ReleaseDC(hwnd, temp2dc);
  SelectObject(tempdc, hbmpx);
  BitBlt(tempdc, 0, 0, hb_parni(5), hb_parni(6), tempdc, 0, 0, WHITENESS);
  if (hb_parnl(8) >= 0)
  {
    ImageList_SetBkColor(himl, hmg_par_COLORREF(8));
  }

  ImageList_Draw(himl, hb_parni(2) - 1, tempdc, 0, 0, hb_parni(7));
  if (hb_parnl(8) >= 0)
  {
    ImageList_SetBkColor(himl, CLR_NONE);
  }

  hb_retl(
      StretchBlt(s_hDC, rect.left, rect.top, rect.right, rect.bottom, tempdc, 0, 0, hb_parni(5), hb_parni(6), SRCCOPY));
  DeleteDC(tempdc);
  DeleteObject(hbmpx);
}

HB_FUNC(RR_POLYGON)
{
  auto number = static_cast<int>(hb_parinfa(1, 0));
  int styl = GetPolyFillMode(s_hDC);
  POINT apoints[1024];
  LONG_PTR xpen = HB_PARNL(3);
  LONG_PTR xbrush = HB_PARNL(4);

  for (auto i = 0; i <= number - 1; i++)
  {
    apoints[i].x = HB_PARNI(1, i + 1);
    apoints[i].y = HB_PARNI(2, i + 1);
  }

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  SetPolyFillMode(s_hDC, hb_parni(5));

  hb_retnl(Polygon(s_hDC, apoints, number));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, s_hbrush);
  }

  SetPolyFillMode(s_hDC, styl);
}

HB_FUNC(RR_POLYBEZIER)
{
  auto number = static_cast<DWORD>(hb_parinfa(1, 0));
  POINT apoints[1024];
  LONG_PTR xpen = HB_PARNL(3);

  for (DWORD i = 0; i <= number - 1; i++)
  {
    apoints[i].x = HB_PARNI(1, i + 1);
    apoints[i].y = HB_PARNI(2, i + 1);
  }

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  hb_retnl(PolyBezier(s_hDC, apoints, number));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_POLYBEZIERTO)
{
  auto number = static_cast<DWORD>(hb_parinfa(1, 0));
  POINT apoints[1024];
  LONG_PTR xpen = HB_PARNL(3);

  for (DWORD i = 0; i <= number - 1; i++)
  {
    apoints[i].x = HB_PARNI(1, i + 1);
    apoints[i].y = HB_PARNI(2, i + 1);
  }

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  hb_retnl(PolyBezierTo(s_hDC, apoints, number));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_GETTEXTEXTENT)
{
#ifndef UNICODE
  LPTSTR lpText = const_cast<LPTSTR>(hb_parc(1));
#else
  LPWSTR lpText = AnsiToWide(const_cast<char *>(hb_parc(1)));
#endif
  LONG_PTR xfont = HB_PARNL(3);
  SIZE szMetric;

  if (xfont != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xfont));
  }

  hb_retni(GetTextExtentPoint32(s_hDC, lpText, lstrlen(lpText), &szMetric));
  HB_STORNI(szMetric.cy, 2, 1);
  HB_STORNI(szMetric.cx, 2, 2);
  if (xfont != 0)
  {
    SelectObject(s_hDC, s_hfont);
  }

#ifdef UNICODE
  hb_xfree((TCHAR *)lpText);
#endif
}

HB_FUNC(RR_ROUNDRECT)
{
  LONG_PTR xpen = HB_PARNL(4);
  LONG_PTR xbrush = HB_PARNL(5);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  hb_retni(
      RoundRect(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1), HB_PARNI(3, 2), HB_PARNI(3, 1)));

  if (xbrush != 0)
  {
    SelectObject(s_hDC, static_cast<HBRUSH>(s_hbrush));
  }

  if (xpen != 0)
  {
    SelectObject(s_hDC, static_cast<HPEN>(s_hpen));
  }
}

HB_FUNC(RR_ELLIPSE)
{
  LONG_PTR xpen = HB_PARNL(3);
  LONG_PTR xbrush = HB_PARNL(4);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  hb_retni(Ellipse(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1)));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, s_hbrush);
  }
}

HB_FUNC(RR_CHORD)
{
  LONG_PTR xpen = HB_PARNL(5);
  LONG_PTR xbrush = HB_PARNL(6);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  hb_retni(Chord(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1), HB_PARNI(3, 2), HB_PARNI(3, 1),
                 HB_PARNI(4, 2), HB_PARNI(4, 1)));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, s_hbrush);
  }
}

HB_FUNC(RR_ARCTO)
{
  LONG_PTR xpen = HB_PARNL(5);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  hb_retni(ArcTo(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1), HB_PARNI(3, 2), HB_PARNI(3, 1),
                 HB_PARNI(4, 2), HB_PARNI(4, 1)));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_ARC)
{
  LONG_PTR xpen = HB_PARNL(5);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  hb_retni(Arc(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1), HB_PARNI(3, 2), HB_PARNI(3, 1),
               HB_PARNI(4, 2), HB_PARNI(4, 1)));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_PIE)
{
  LONG_PTR xpen = HB_PARNL(5);
  LONG_PTR xbrush = HB_PARNL(6);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HBRUSH>(xbrush));
  }

  hb_retni(Pie(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), HB_PARNI(2, 2), HB_PARNI(2, 1), HB_PARNI(3, 2), HB_PARNI(3, 1),
               HB_PARNI(4, 2), HB_PARNI(4, 1)));

  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }

  if (xbrush != 0)
  {
    SelectObject(s_hDC, s_hbrush);
  }
}

HB_FUNC(RR_FILLRECT)
{
  RECT rect;

  rect.left = HB_PARNI(1, 2);
  rect.top = HB_PARNI(1, 1);
  rect.right = HB_PARNI(2, 2);
  rect.bottom = HB_PARNI(2, 1);
  hb_retni(FillRect(s_hDC, &rect, hmg_par_HBRUSH(3)));
}

HB_FUNC(RR_FRAMERECT)
{
  RECT rect;

  rect.left = HB_PARNI(1, 2);
  rect.top = HB_PARNI(1, 1);
  rect.right = HB_PARNI(2, 2);
  rect.bottom = HB_PARNI(2, 1);
  hb_retni(FrameRect(s_hDC, &rect, hmg_par_HBRUSH(3)));
}

HB_FUNC(RR_LINE)
{
  LONG_PTR xpen = HB_PARNL(3);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  MoveToEx(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1), nullptr);
  hb_retni(LineTo(s_hDC, HB_PARNI(2, 2), HB_PARNI(2, 1)));
  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_LINETO)
{
  LONG_PTR xpen = HB_PARNL(2);

  if (xpen != 0)
  {
    SelectObject(s_hDC, reinterpret_cast<HPEN>(xpen));
  }

  hb_retni(LineTo(s_hDC, HB_PARNI(1, 2), HB_PARNI(1, 1)));
  if (xpen != 0)
  {
    SelectObject(s_hDC, s_hpen);
  }
}

HB_FUNC(RR_INVERTRECT)
{
  RECT rect;

  rect.left = HB_PARNI(1, 2);
  rect.top = HB_PARNI(1, 1);
  rect.right = HB_PARNI(2, 2);
  rect.bottom = HB_PARNI(2, 1);
  hb_retni(InvertRect(s_hDC, &rect));
}

HB_FUNC(RR_GETDESKTOPAREA)
{
  RECT rect;

  SystemParametersInfo(SPI_GETWORKAREA, 1, &rect, 0);

  hb_reta(4);
  HB_STORNI(rect.top, -1, 1);
  HB_STORNI(rect.left, -1, 2);
  HB_STORNI(rect.bottom - rect.top, -1, 3);
  HB_STORNI(rect.right - rect.left, -1, 4);
}

HB_FUNC(RR_GETCLIENTRECT)
{
  RECT rect;

  GetClientRect(reinterpret_cast<HWND>(HB_PARVNL(1, 7)), &rect);
  HB_STORNI(rect.top, 1, 1);
  HB_STORNI(rect.left, 1, 2);
  HB_STORNI(rect.bottom, 1, 3);
  HB_STORNI(rect.right, 1, 4);
  HB_STORNI(rect.bottom - rect.top + 1, 1, 5);
  HB_STORNI(rect.right - rect.left + 1, 1, 6);
}

HB_FUNC(RR_SCROLLWINDOW)
{
  ScrollWindow(hmg_par_HWND(1), hb_parni(2), hb_parni(3), nullptr, nullptr);
}

HB_FUNC(RR_PREVIEWPLAY)
{
  RECT rect;
  auto imgDC = GetWindowDC(hmg_par_HWND(1));
  auto tmpDC = CreateCompatibleDC(imgDC);
#ifndef UNICODE
  LPSTR FileName = const_cast<LPSTR>(HB_PARC(2, 1));
#else
  LPWSTR FileName = AnsiToWide(const_cast<char *>(HB_PARC(2, 1)));
#endif
  HENHMETAFILE hh = GetEnhMetaFile(FileName);

#ifdef UNICODE
  hb_xfree((TCHAR *)FileName);
#endif
  if (tmpDC == nullptr)
  {
    ReleaseDC(hmg_par_HWND(1), imgDC);
    hb_retl(false);
  }

  if (s_himgbmp != 0)
  {
    DeleteObject(s_himgbmp);
  }

  SetRect(&rect, 0, 0, HB_PARNI(3, 4), HB_PARNI(3, 3));
  s_himgbmp = CreateCompatibleBitmap(imgDC, rect.right, rect.bottom);
  DeleteObject(SelectObject(tmpDC, static_cast<HBITMAP>(s_himgbmp)));
  FillRect(tmpDC, &rect, static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)));
  PlayEnhMetaFile(tmpDC, hh, &rect);
  DeleteEnhMetaFile(hh);
  SendMessage(hmg_par_HWND(1), STM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(s_himgbmp));
  ReleaseDC(hmg_par_HWND(1), imgDC);
  DeleteDC(tmpDC);
  if (s_himgbmp == 0)
  {
    hb_retl(false);
  }
  else
  {
    hb_retl(true);
  }
}

HB_FUNC(RR_PLAYTHUMB)
{
  RECT rect;
  auto imgDC = GetWindowDC(reinterpret_cast<HWND>(HB_PARVNL(1, 5)));
#ifndef UNICODE
  LPSTR FileName = const_cast<LPSTR>(HB_PARC(2, 1));
  LPTSTR lpText = const_cast<LPTSTR>(hb_parc(3));
#else
  LPWSTR FileName = AnsiToWide(const_cast<char *>(HB_PARC(2, 1)));
  LPWSTR lpText = AnsiToWide(const_cast<char *>(hb_parc(3)));
#endif
  HENHMETAFILE hh = GetEnhMetaFile(FileName);
  int i;

  i = hb_parni(4) - 1;
  auto tmpDC = CreateCompatibleDC(imgDC);
  SetRect(&rect, 0, 0, HB_PARNI(1, 4), HB_PARNI(1, 3));
  s_hbmp[i] = CreateCompatibleBitmap(imgDC, rect.right, rect.bottom);
  DeleteObject(SelectObject(tmpDC, s_hbmp[i]));
  FillRect(tmpDC, &rect, static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)));
  PlayEnhMetaFile(tmpDC, hh, &rect);
  DeleteEnhMetaFile(hh);
  TextOut(tmpDC, rect.right / 2 - 5, rect.bottom / 2 - 5, lpText, lstrlen(lpText));
  SendMessage(reinterpret_cast<HWND>(HB_PARVNL(1, 5)), STM_SETIMAGE, IMAGE_BITMAP, reinterpret_cast<LPARAM>(s_hbmp[i]));
  ReleaseDC(reinterpret_cast<HWND>(HB_PARVNL(1, 5)), imgDC);
  DeleteDC(tmpDC);

#ifdef UNICODE
  hb_xfree((TCHAR *)FileName);
  hb_xfree((TCHAR *)lpText);
#endif
}

HB_FUNC(RR_PLAYENHMETAFILE)
{
  RECT rect;
#ifndef UNICODE
  LPSTR FileName = const_cast<LPSTR>(HB_PARC(1, 1));
#else
  LPWSTR FileName = AnsiToWide(const_cast<char *>(HB_PARC(1, 1)));
#endif
  HENHMETAFILE hh = GetEnhMetaFile(FileName);

  SetRect(&rect, 0, 0, HB_PARNI(1, 5), HB_PARNI(1, 4));
  PlayEnhMetaFile(hmg_par_HDC(2), hh, &rect);
  DeleteEnhMetaFile(hh);

#ifdef UNICODE
  hb_xfree((TCHAR *)FileName);
#endif
}

HB_FUNC(RR_LALABYE)
{
  if (hb_parni(1) == 1)
  {
    s_hDCtemp = s_hDC;
    s_hDC = s_hDCRef;
  }
  else
  {
    s_hDC = s_hDCtemp;
  }
}
