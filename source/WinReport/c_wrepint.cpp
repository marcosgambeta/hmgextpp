#define _WIN32_IE      0x0500
#define _WIN32_WINNT   0x0400
#include <shlobj.h>

#include <windows.h>
#include <commctrl.h>
#include <hbapi.hpp>
#include "mgdefs.hpp"

// #pragma argsused    // We need a lowercase!!!

void CALLBACK MessageBoxTimer(HWND hwnd, UINT uiMsg, UINT idEvent, DWORD dwTime)
{
   HB_SYMBOL_UNUSED(hwnd);
   HB_SYMBOL_UNUSED(uiMsg);
   HB_SYMBOL_UNUSED(idEvent);
   HB_SYMBOL_UNUSED(dwTime);
   PostQuitMessage(0);
}

UINT TimedMessageBox(HWND hwndParent, LPCTSTR ptszMessage, LPCTSTR ptszTitle, UINT flags, DWORD dwTimeout)
{
   /*
    *  Set a timer to dismiss the Message box.
    */
   UINT idTimer = SetTimer(nullptr, 0, dwTimeout, reinterpret_cast<TIMERPROC>(MessageBoxTimer));

   UINT uiResult = MessageBox(hwndParent, ptszMessage ? ptszMessage : "", ptszTitle ? ptszTitle : "", flags);

   /*
    *  Finished with the timer.
    */
   KillTimer(nullptr, idTimer);

   /*
    *  See if there is a WM_QUIT Message in the queue. If so,
    *  then you timed out. Eat the Message so you don't quit the
    *  entire application.
    */
   MSG msg;

   if( PeekMessage(&msg, nullptr, WM_QUIT, WM_QUIT, PM_REMOVE) )
   {
       /*
        *  If you timed out, then return zero.
        */
       uiResult = 0;
   }

   return uiResult;
}

/*
C_T_MSGRETRYCANCEL(message, title, timeout) --> numeric
*/
HB_FUNC( C_T_MSGRETRYCANCEL )
{
   hb_retni(TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_RETRYCANCEL | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3)));
}

/*
C_T_MSGOKCANCEL(message, title, timeout) --> numeric
*/
HB_FUNC( C_T_MSGOKCANCEL )
{
   hb_retni(TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_OKCANCEL | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3)));
}

/*
C_T_MSGYESNO(message, title, timeout) --> numeric
*/
HB_FUNC( C_T_MSGYESNO )
{
   hb_retni(TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_YESNO | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3)));
}

/*
C_T_MSGYESNO_ID(message, title, timeout) --> numeric
*/
HB_FUNC( C_T_MSGYESNO_ID )
{
   hb_retni(TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_YESNO | MB_ICONQUESTION | MB_SYSTEMMODAL | MB_DEFBUTTON2, hb_parni(3)));
}

/*
C_T_MSGBOX(message, title, timeout) --> NIL
*/
HB_FUNC( C_T_MSGBOX )
{
   TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_SYSTEMMODAL, hb_parni(3));
}

/*
C_T_MSGINFO(message, title, timeout) --> NIL
*/
HB_FUNC( C_T_MSGINFO )
{
   TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_OK | MB_ICONINFORMATION | MB_SYSTEMMODAL, hb_parni(3));
}

/*
C_T_MSGSTOP(message, title, timeout) --> NIL
*/
HB_FUNC( C_T_MSGSTOP )
{
   TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_OK | MB_ICONSTOP | MB_SYSTEMMODAL, hb_parni(3));
}

/*
C_T_MSGEXCLAMATION(message, title, timeout) --> NIL
*/
HB_FUNC( C_T_MSGEXCLAMATION )
{
   TimedMessageBox(nullptr, hb_parc(1), hb_parc(2), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL, hb_parni(3));
}

/*
_HMG_PRINTER_GETPRINTABLEAREAPHYSICALWIDTH(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALWIDTH )
{
   hb_retnl(GetDeviceCaps(hmg_par_HDC(1), PHYSICALWIDTH));
}

/*
_HMG_PRINTER_GETPRINTABLEAREAPHYSICALHEIGTH(HDC) --> numeric
*/
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALHEIGTH )
{
   hb_retnl(GetDeviceCaps(hmg_par_HDC(1), PHYSICALHEIGHT));
}
