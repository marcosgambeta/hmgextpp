/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2021 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"

#ifdef _NAMES_LIST_
*-----------------------------------------------------------------------------*
FUNCTION _SetGetNamesList( cName, nIndex, lDelete )
*-----------------------------------------------------------------------------*
   STATIC _HMG_NAMESLIST

   IF HB_ISNIL( _HMG_NAMESLIST )
      _HMG_NAMESLIST := oHmgData()
   ENDIF

   IF PCount() == 1

      RETURN _HMG_NAMESLIST:Get( cName, 0 )

   ELSEIF PCount() == 2

      _HMG_NAMESLIST:Set( cName, nIndex )

   ELSEIF PCount() == 3

      IF lDelete
         _HMG_NAMESLIST:Del( cName )
      ELSE
         _HMG_NAMESLIST:Set( cName, NIL )
      ENDIF

   ENDIF 

RETURN _HMG_NAMESLIST

#endif

*-----------------------------------------------------------------------------*
#ifndef __XHARBOUR__
FUNCTION _SetGetGlobal( cVarName, xNewValue, ... )
#else
FUNCTION _SetGetGlobal( ... )
#endif
*-----------------------------------------------------------------------------*
   LOCAL xOldValue

#ifdef __XHARBOUR__
   LOCAL cVarName, xNewValue
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len( aParams )
#endif

   STATIC _HMG_STATIC

#ifdef __XHARBOUR__
   IF nParams > 1
      cVarName := aParams[ 1 ]
      xNewValue := aParams[ 2 ]
   ELSEIF nParams == 1
      cVarName := aParams[ 1 ]
   ENDIF
#endif
   IF HB_ISNIL( _HMG_STATIC )
      _HMG_STATIC := oHmgData()
   ENDIF

   SWITCH PCount()
   CASE 0
      RETURN _HMG_STATIC
   CASE 1
      RETURN _HMG_STATIC:Get( cVarName, NIL )
   CASE 2
      xOldValue := _HMG_STATIC:Get( cVarName, NIL )
      _HMG_STATIC:Set( cVarName, xNewValue )
      EXIT
   CASE 3
      _HMG_STATIC:Del( cVarName )
      EXIT
   ENDSWITCH

RETURN xOldValue

*-----------------------------------------------------------------------------*
FUNCTION _AddNewGlobal( cVarName, xValue )
*-----------------------------------------------------------------------------*
   // If cVarName not found, then ...
   IF _SetGetGlobal( cVarName ) == NIL
      // Add a new variable in the Pseudo-Variable List
      STATIC &cVarName AS GLOBAL VALUE xValue
   ENDIF

RETURN _SetGetGlobal( cVarName )

*-----------------------------------------------------------------------------*
FUNCTION CheckStatic()
*-----------------------------------------------------------------------------*
   LOCAL cInfo := ""
   LOCAL nCnt := 0
   LOCAL n

   _SetGetLogFile( GetStartUpFolder() + hb_ps() + "checkstatic.txt" )
   FErase( _SetGetLogFile() )

   cInfo += "Statics variables:" + CRLF
   cInfo += "=================="
   _LogFile( .F., cInfo )

   FOR n = 1 TO nStatics()
      if ! Empty( Static( n ) )
         cInfo := CRLF
         cInfo += hb_ntos( n ) + Replicate( "-", 55 ) + "> "
         _LogFile( .F., cInfo )
         Scan( Static( n ) )
         nCnt++
      ENDIF
   NEXT

   cInfo := CRLF
   cInfo += Replicate( "-", 59 ) + "> "
   _LogFile( .F., cInfo )

   _LogFile( .T., "Amount = " + hb_ntos( nCnt ) )
   _LogFile( .T., Replicate( "=", 60 ) )
   _LogFile( .T., GetExeFileName() )

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION Scan( u, cData )
*-----------------------------------------------------------------------------*
   LOCAL cType := ValType( u )
   LOCAL n
   LOCAL cInfo := ""

   STATIC nNested := 0

   DEFAULT cData := ""

   DoEvents()

   IF nNested > 50
      cInfo += CRLF
      cInfo += Space( nNested ) + "Break on 50 nested loops"
      cInfo += CRLF
      _LogFile( .T., cInfo )
   ELSE
      // We write the data and its containts
      _LogFile( .F., CRLF + Space( nNested ) + cType + " " + cData )
      IF .NOT. cType $ "UAO"
         _LogFile( .F., " " + cValToChar( u ) )
      ENDIF

      IF cType $ "AO"
         _LogFile( .F., " (Len = " + hb_ntos( Len( u ) ) + ")" )
      ENDIF

      IF cType == "A"

         FOR n := 1 TO Len( u )

            DoEvents()

            IF ValType( u[ n ] ) == ValType( u ) .AND. u[ n ] == u
               _LogFile( .T., " Direct reference to its container" )
            ELSE
               nNested++
               Scan( u[ n ], cData )
               nNested--
            ENDIF

         NEXT n

      ENDIF

   ENDIF

RETURN NIL

#ifndef __XHARBOUR__
#translate HB_DBG_VMVARSLEN   =>  __dbgVMVarSLen
#translate HB_DBG_VMVARSLIST  =>  __dbgVMVarSList
#translate HB_DBG_VMVARSGET   =>  __dbgVMVarSGet
#endif
*-----------------------------------------------------------------------------*
STATIC FUNCTION nStatics()
*-----------------------------------------------------------------------------*

RETURN HB_DBG_VMVARSLEN()

*-----------------------------------------------------------------------------*
STATIC FUNCTION Static( n )
*-----------------------------------------------------------------------------*

RETURN HB_DBG_VMVARSGET( HB_DBG_VMVARSLIST(), n )
