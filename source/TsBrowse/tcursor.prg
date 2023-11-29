#include "minigui.ch"
#include <hbclass.ch>

CLASS TCursor

   DATA hCursor
   DATA lPredef AS LOGICAL

   METHOD New(cResName, cPredef) CONSTRUCTOR
   METHOD End() INLINE IIf(::hCursor != 0, IIf(!::lPredef, DestroyCursor(::hCursor), NIL), NIL), ::hCursor := 0

ENDCLASS

METHOD New(cResName, cPredef) CLASS TCursor

   LOCAL nAt
   LOCAL aTypes

   IF !Empty(cPredef)
      cPredef := Upper(cPredef)
      IF (nAt := AScan({"ARROW", "IBEAM", "WAIT", "CROSS", "UPARROW", "SIZENWSE", "SIZENESW", "SIZEWE", "SIZENS"}, cPredef)) != 0
         aTypes = { IDC_ARROW, IDC_IBEAM, IDC_WAIT, IDC_CROSS, IDC_UPARROW, IDC_SIZENWSE, IDC_SIZENESW, IDC_SIZEWE, IDC_SIZENS }
         ::hCursor = hmg_LoadCursor(NIL, aTypes[nAt])
         ::lPredef = .T.
      ELSE
         SWITCH cPredef
         CASE "HAND"  ; ::hCursor = GetCursorHand()  ; ::lPredef = .F. ; EXIT
         CASE "STOP"  ; ::hCursor = GetCursorStop()  ; ::lPredef = .F. ; EXIT
         CASE "DRAG"  ; ::hCursor = GetCursorDrag()  ; ::lPredef = .F. ; EXIT
         CASE "CATCH" ; ::hCursor = GetCursorCatch() ; ::lPredef = .F.
         OTHERWISE
            MsgAlert("Wrong predefined cursor type!", "Alert")
         ENDSWITCH
      ENDIF
   ELSE
      ::hCursor = hmg_LoadCursor(GetInstance(), cResName)
      ::lPredef = .F.
   ENDIF

RETURN Self
