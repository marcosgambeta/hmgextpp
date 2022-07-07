#define _WIN32_IE      0x0500
#define _WIN32_WINNT   0x0400
#include <shlobj.h>

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"

// #pragma argsused    // We need a lowercase!!!

void CALLBACK MessageBoxTimer(HWND hwnd, UINT uiMsg, UINT idEvent, DWORD dwTime)
{
   HB_SYMBOL_UNUSED(hwnd);
   HB_SYMBOL_UNUSED(uiMsg);
   HB_SYMBOL_UNUSED(idEvent);
   HB_SYMBOL_UNUSED(dwTime);
   PostQuitMessage(0);
}

UINT
TimedMessageBox(
    HWND hwndParent,
    LPCTSTR ptszMessage,
    LPCTSTR ptszTitle,
    UINT flags,
    DWORD dwTimeout)
{
    UINT idTimer;
    UINT uiResult;
    MSG msg;

    /*
     *  Set a timer to dismiss the Message box.
     */
    idTimer = SetTimer(NULL, 0, dwTimeout, (TIMERPROC)MessageBoxTimer);

    uiResult = MessageBox(hwndParent, ptszMessage ? ptszMessage : "", ptszTitle ? ptszTitle :"", flags);

    /*
     *  Finished with the timer.
     */
    KillTimer(NULL, idTimer);

    /*
     *  See if there is a WM_QUIT Message in the queue. If so,
     *  then you timed out. Eat the Message so you don't quit the
     *  entire application.
     */
    if (PeekMessage(&msg, NULL, WM_QUIT, WM_QUIT, PM_REMOVE)) {

        /*
         *  If you timed out, then return zero.
         */
        uiResult = 0;
    }

    return uiResult;
}

HB_FUNC( C_T_MSGRETRYCANCEL )
{
	int r;
	r = TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_RETRYCANCEL | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3));
	hb_retni ( r );
}

HB_FUNC( C_T_MSGOKCANCEL )
{
	int r;
	r = TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_OKCANCEL | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3));
	hb_retni ( r );
}

HB_FUNC( C_T_MSGYESNO )
{
	int r;
	r = TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_YESNO | MB_ICONQUESTION | MB_SYSTEMMODAL, hb_parni(3));
	hb_retni ( r );

}

HB_FUNC( C_T_MSGYESNO_ID )
{
	int r;
	r = TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_YESNO | MB_ICONQUESTION | MB_SYSTEMMODAL | MB_DEFBUTTON2, hb_parni(3));
	hb_retni ( r );
}

HB_FUNC( C_T_MSGBOX )
{
	TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_SYSTEMMODAL, hb_parni(3));
}

HB_FUNC( C_T_MSGINFO )
{
	TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_OK | MB_ICONINFORMATION | MB_SYSTEMMODAL, hb_parni(3));
}

HB_FUNC( C_T_MSGSTOP )
{
	TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_OK | MB_ICONSTOP | MB_SYSTEMMODAL, hb_parni(3));
}

HB_FUNC( C_T_MSGEXCLAMATION )
{
	TimedMessageBox(NULL, hb_parc(1), hb_parc(2), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL, hb_parni(3));
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALWIDTH )
{
	HDC hdc = (HDC) hb_parni(1);
	hb_retnl ( GetDeviceCaps ( hdc , PHYSICALWIDTH ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALHEIGTH )
{
	HDC hdc = (HDC) hb_parni(1);
	hb_retnl ( GetDeviceCaps ( hdc , PHYSICALHEIGHT ) );
}
