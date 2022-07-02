/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2022, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

----------------------------------------------------------------------------*/

#include "minigui.ch"
#include "error.ch"
#include "fileio.ch"
#include "hbmemvar.ch"
#include "hbver.ch"

#ifdef _TSBROWSE_
   MEMVAR _TSB_aControlhWnd
#endif
*-----------------------------------------------------------------------------*
PROCEDURE ErrorSys
*-----------------------------------------------------------------------------*
   ErrorBlock( { | oError | DefError( oError ) } )
   Set( _SET_HBOUTLOG, GetStartUpFolder() + hb_ps() + "error.log" )
   Set( _SET_HBOUTLOGINFO, MiniGUIVersion() )

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION DefError( oError )
*-----------------------------------------------------------------------------*
   LOCAL lOldSetState := ( Len( DToC( Date() ) ) == 10 )
   LOCAL cText
   LOCAL HtmArch
   LOCAL HtmText
   LOCAL n

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV .AND. ;
         oError:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure
   IF oError:genCode == EG_LOCK .AND. ;
         oError:canRetry
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
         oError:osCode == 32 .AND. ;
         oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
         oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF ! lOldSetState
      SET CENTURY ON
   ENDIF

   HtmArch := Html_ErrorLog()
   cText   := ErrorMessage( oError )

   Html_RawText( HtmArch, '<div class="record">' )
   Html_RawText( HtmArch, '<p class="updated">' )
   Html_LineText( HtmArch, 'Date: <span class="date">' + DToC( Date() ) + '</span> ' + 'Time: <span class="time">' + Time() + '</span>' )
   Html_LineText( HtmArch, 'Application: ' + GetExeFileName() )
   Html_LineText( HtmArch, 'User: ' + NetName() + " / " + GetUserName() )
   Html_LineText( HtmArch, 'Time from start: ' + TimeFromStart() )
   Html_RawText( HtmArch, '<span class="error">' + cText + '</span>' )
   Html_RawText( HtmArch, '</p>' )
   cText += CRLF + CRLF

   HTML_RawText( HtmArch, "<details><summary>" )
   HTML_RawText( HtmArch, PadC( " Stack Trace ", 79, "-" ) )
   HTML_RawText( HtmArch, '<br/></summary><span class="stacktrace">' )

   n := 1
   WHILE ! Empty( ProcName( ++n ) )
      HtmText := "Called from " + ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) + ")" + ;
         iif( ProcLine( n ) > 0, " in module: " + ProcFile( n ), "" ) + CRLF
      cText += HtmText
      Html_LineText( HtmArch, HtmText )
   ENDDO

   Html_RawText( HtmArch, "</span></details>" )

   SET CENTURY ( lOldSetState )

   IF _lShowDetailError()
      ErrorLog( HtmArch, oError )
   ENDIF

   Html_Line( HtmArch )
   Html_RawText( HtmArch, '</div>' )
   Html_End( HtmArch )

   ShowError( cText, oError )

   ExitProcess()

RETURN .F.

*-----------------------------------------------------------------------------*
STATIC FUNCTION ErrorMessage( oError )
*-----------------------------------------------------------------------------*
   // start error message
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "
   LOCAL n

   // add subsystem name if available
   IF ISCHARACTER( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUMBER( oError:subCode )
      cMessage += "/" + hb_ntos( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE !Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   // add OS error code if available
   IF !Empty( oError:osCode )
      cMessage += " (DOS Error " + hb_ntos( oError:osCode ) + ")"
   ENDIF

   IF ValType( oError:args ) == "A"
      cMessage += CRLF
      cMessage += "   Args:" + CRLF
      FOR n := 1 TO Len( oError:args )
         cMessage += ;
            "     [" + hb_ntos( n, 2 ) + "] = " + ValType( oError:args[ n ] ) + ;
            "   " + cValToChar( cValToChar( oError:args[ n ] ) ) + ;
            iif( ValType( oError:args[ n ] ) == "A", " length: " + ;
            hb_ntos( Len( oError:args[ n ] ) ), "" ) + iif( n < Len( oError:args ), CRLF, "" )
      NEXT
   ENDIF

RETURN cMessage

*-----------------------------------------------------------------------------*
STATIC PROCEDURE ShowError( cErrorMessage, oError )
*-----------------------------------------------------------------------------*
   LOCAL cMsg := "", bInit

   IF _SetGetGlobal( "_HMG_ShowError" ) == NIL
      STATIC _HMG_ShowError AS GLOBAL VALUE .T.
   ENDIF

   IF _SetGetGlobal( "_HMG_ShowError" )

      ASSIGN GLOBAL _HMG_ShowError := .F.
#ifdef _TSBROWSE_
      _TSB_aControlhWnd := {}
#endif
      IF ISBLOCK( _HMG_bOnErrorInit )
         cMsg := Eval( _HMG_bOnErrorInit, cMsg )
      ENDIF

      cMsg += iif( _lShowDetailError(), cErrorMessage, ErrorMessage( oError ) )

      IF ISLOGICAL( _HMG_lOnErrorStop ) .AND. _HMG_lOnErrorStop == .F.

         MsgStop( StrTran( cMsg, ";", CRLF ), 'Program Error', NIL, .F. )

      ELSE

         bInit := {|| iif( GetControlType( "Say_01", "oDlg" ) == "EDIT",, ( ;
            SetProperty( "oDlg", "Say_01", "FontColor", YELLOW ), ;
            SetProperty( "oDlg", "Say_01", "Alignment", "CENTER" ), ;
            SetProperty( "oDlg", "Say_02", "FontColor", YELLOW ), ;
            SetProperty( "oDlg", "Say_02", "Alignment", "CENTER" ) ) ) }

         IF AScan( _HMG_aFormType, 'A' ) == 0
            _HMG_MainWindowFirst := .F.
         ENDIF

         SET MSGALERT BACKCOLOR TO MAROON
         SET MSGALERT FONTCOLOR TO WHITE

         IF GetFontHandle( "DlgFont" ) == 0
            DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 14
         ENDIF

         IF _lShowDetailError()
            HMG_Alert_MaxLines( 35 )
         ENDIF

         AlertStop( cMsg, "Program Error", "ZZZ_B_STOP64", 64, { { 217, 67, 67 } }, .T., bInit )

      ENDIF

      ErrorLevel( 1 )

      IF ISBLOCK( _HMG_bOnErrorExit )
         Eval( _HMG_bOnErrorExit )
      ENDIF

      ReleaseAllWindows()

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE ErrorLog( nHandle, oErr )
*-----------------------------------------------------------------------------*
   STATIC _lAddError := .T.
   LOCAL nScope, nCount, tmp, cName, xValue

   IF _lAddError

      _lAddError := .F.

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " System Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "Workstation name...: " + NetName() )
      Html_LineText( nHandle, "Active user name...: " + GetUserName() )
      Html_LineText( nHandle, "Available memory...: " + strvalue( MemoryStatus( 2 ) ) + " MB" )
      Html_LineText( nHandle, "Current disk.......: " + DiskName() )
      Html_LineText( nHandle, "Current directory..: " + CurDir() )
      Html_LineText( nHandle, "Free disk space....: " + strvalue( Round( hb_DiskSpace( hb_DirBase() ) / ( 1024 * 1024 ), 0 ) ) + " MB" )
      Html_LineText( nHandle, "" )
      Html_LineText( nHandle, "Operating system...: " + OS() )
      Html_LineText( nHandle, "MiniGUI version....: " + MiniGUIVersion() )
      Html_LineText( nHandle, "Harbour version....: " + Version() )
#if ( __HARBOUR__ - 0 > 0x030200 )
      Html_LineText( nHandle, "Harbour built on...: " + hb_Version( HB_VERSION_BUILD_DATE_STR ) )
#else
      Html_LineText( nHandle, "Harbour built on...: " + hb_BuildDate() )
#endif
      Html_LineText( nHandle, "C/C++ compiler.....: " + hb_Compiler() )
      Html_LineText( nHandle, "Multi Threading....: " + iif( hb_mtvm(), "YES", "NO" ) )
      Html_LineText( nHandle, "VM Optimization....: " + iif( hb_VMMode() == 1, "YES", "NO" ) )

      IF hb_IsFunction( "Select" )
         Html_LineText( nHandle, "" )
         Html_LineText( nHandle, "Current Work Area..: " + strvalue( Eval( hb_macroBlock( "Select()" ) ) ) )
      ENDIF

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Environmental Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "SET ALTERNATE......: " + strvalue( Set( _SET_ALTERNATE ), .T. ) )
      Html_LineText( nHandle, "SET ALTFILE........: " + strvalue( Set( _SET_ALTFILE ) ) )
      Html_LineText( nHandle, "SET AUTOPEN........: " + strvalue( Set( _SET_AUTOPEN ), .T. ) )
      Html_LineText( nHandle, "SET AUTORDER.......: " + strvalue( Set( _SET_AUTORDER ) ) )
      Html_LineText( nHandle, "SET AUTOSHARE......: " + strvalue( Set( _SET_AUTOSHARE ) ) )

      Html_LineText( nHandle, "SET CENTURY........: " + strvalue( __SetCentury(), .T. ) )
      Html_LineText( nHandle, "SET COUNT..........: " + strvalue( Set( _SET_COUNT ) ) )

      Html_LineText( nHandle, "SET DATE FORMAT....: " + strvalue( Set( _SET_DATEFORMAT ) ) )
      Html_LineText( nHandle, "SET DBFLOCKSCHEME..: " + strvalue( Set( _SET_DBFLOCKSCHEME ) ) )
      Html_LineText( nHandle, "SET DEBUG..........: " + strvalue( Set( _SET_DEBUG ), .T. ) )
      Html_LineText( nHandle, "SET DECIMALS.......: " + strvalue( Set( _SET_DECIMALS ) ) )
      Html_LineText( nHandle, "SET DEFAULT........: " + strvalue( Set( _SET_DEFAULT ) ) )
      Html_LineText( nHandle, "SET DEFEXTENSIONS..: " + strvalue( Set( _SET_DEFEXTENSIONS ), .T. ) )
      Html_LineText( nHandle, "SET DELETED........: " + strvalue( Set( _SET_DELETED ), .T. ) )
      Html_LineText( nHandle, "SET DELIMCHARS.....: " + strvalue( Set( _SET_DELIMCHARS ) ) )
      Html_LineText( nHandle, "SET DELIMETERS.....: " + strvalue( Set( _SET_DELIMITERS ), .T. ) )
      Html_LineText( nHandle, "SET DIRCASE........: " + strvalue( Set( _SET_DIRCASE ) ) )
      Html_LineText( nHandle, "SET DIRSEPARATOR...: " + strvalue( Set( _SET_DIRSEPARATOR ) ) )

      Html_LineText( nHandle, "SET EOL............: " + strvalue( Asc( Set( _SET_EOL ) ) ) )
      Html_LineText( nHandle, "SET EPOCH..........: " + strvalue( Set( _SET_EPOCH ) ) )
      Html_LineText( nHandle, "SET ERRORLOG.......: " + strvalue( _GetErrorlogFile() ) )
      Html_LineText( nHandle, "SET EXACT..........: " + strvalue( Set( _SET_EXACT ), .T. ) )
      Html_LineText( nHandle, "SET EXCLUSIVE......: " + strvalue( Set( _SET_EXCLUSIVE ), .T. ) )
      Html_LineText( nHandle, "SET EXTRA..........: " + strvalue( Set( _SET_EXTRA ), .T. ) )
      Html_LineText( nHandle, "SET EXTRAFILE......: " + strvalue( Set( _SET_EXTRAFILE ) ) )

      Html_LineText( nHandle, "SET FILECASE.......: " + strvalue( Set( _SET_FILECASE ) ) )
      Html_LineText( nHandle, "SET FIXED..........: " + strvalue( Set( _SET_FIXED ), .T. ) )
      Html_LineText( nHandle, "SET FORCEOPT.......: " + strvalue( Set( _SET_FORCEOPT ), .T. ) )

      Html_LineText( nHandle, "SET HARDCOMMIT.....: " + strvalue( Set( _SET_HARDCOMMIT ), .T. ) )

      Html_LineText( nHandle, "SET IDLEREPEAT.....: " + strvalue( Set( _SET_IDLEREPEAT ), .T. ) )

      Html_LineText( nHandle, "SET LANGUAGE.......: " + strvalue( Set( _SET_LANGUAGE ) ) )

      Html_LineText( nHandle, "SET MARGIN.........: " + strvalue( Set( _SET_MARGIN ) ) )
      Html_LineText( nHandle, "SET MBLOCKSIZE.....: " + strvalue( Set( _SET_MBLOCKSIZE ) ) )
      Html_LineText( nHandle, "SET MFILEEXT.......: " + strvalue( Set( _SET_MFILEEXT ) ) )

      Html_LineText( nHandle, "SET OPTIMIZE.......: " + strvalue( Set( _SET_OPTIMIZE ), .T. ) )

      Html_LineText( nHandle, "SET PATH...........: " + strvalue( Set( _SET_PATH ) ) )
      Html_LineText( nHandle, "SET PRINTER........: " + strvalue( Set( _SET_PRINTER ), .T. ) )
      Html_LineText( nHandle, "SET PRINTFILE......: " + strvalue( Set( _SET_PRINTFILE ) ) )

      Html_LineText( nHandle, "SET SOFTSEEK.......: " + strvalue( Set( _SET_SOFTSEEK ), .T. ) )

      Html_LineText( nHandle, "SET TRIMFILENAME...: " + strvalue( Set( _SET_TRIMFILENAME ) ) )

      Html_LineText( nHandle, "SET UNIQUE.........: " + strvalue( Set( _SET_UNIQUE ), .T. ) )

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Detailed Work Area Items ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      hb_WAEval( {||
      IF hb_IsFunction( "Select" )
         Html_LineText( nHandle, "Work Area No ......: " + strvalue( Do( "Select" ) ) )
      ENDIF
      IF hb_IsFunction( "Alias" )
         Html_LineText( nHandle, "Alias .............: " + Do( "Alias" ) )
      ENDIF
      IF hb_IsFunction( "RecNo" )
         Html_LineText( nHandle, "Current Recno .....: " + strvalue( Do( "RecNo" ) ) )
      ENDIF
      IF hb_IsFunction( "dbFilter" )
         Html_LineText( nHandle, "Current Filter ....: " + Do( "dbFilter" ) )
      ENDIF
      IF hb_IsFunction( "dbRelation" )
         Html_LineText( nHandle, "Relation Exp. .....: " + Do( "dbRelation" ) )
      ENDIF
      IF hb_IsFunction( "IndexOrd" )
         Html_LineText( nHandle, "Index Order .......: " + strvalue( Do( "IndexOrd" ) ) )
      ENDIF
      IF hb_IsFunction( "IndexKey" )
         Html_LineText( nHandle, "Active Key ........: " + strvalue( Eval( hb_macroBlock( "IndexKey( 0 )" ) ) ) )
      ENDIF
      Html_LineText( nHandle, "" )
      RETURN .T.
      } )

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Internal Error Handling Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "Subsystem Call ....: " + oErr:subsystem() )
      Html_LineText( nHandle, "System Code .......: " + strvalue( oErr:subcode() ) )
      Html_LineText( nHandle, "Default Status ....: " + strvalue( oErr:candefault() ) )
      Html_LineText( nHandle, "Description .......: " + oErr:description() )
      Html_LineText( nHandle, "Operation .........: " + oErr:operation() )
      Html_LineText( nHandle, "Involved File .....: " + oErr:filename() )
      Html_LineText( nHandle, "Dos Error Code ....: " + strvalue( oErr:oscode() ) )

      HTML_RawText( nHandle, "</details>" )

      /* NOTE: Adapted from hb_mvSave() source in Harbour RTL. */
      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Available Memory Variables ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      FOR EACH nScope IN { HB_MV_PUBLIC, HB_MV_PRIVATE }

         nCount := __mvDbgInfo( nScope )
         FOR tmp := 1 TO nCount

            xValue := __mvDbgInfo( nScope, tmp, @cName )
            IF ValType( xValue ) $ "CNDTL" .AND. Left( cName, 1 ) <> "_"
               Html_LineText( nHandle, "      " + cName + " TYPE " + ValType( xValue ) + " [" + hb_CStr( xValue ) + "]" )
            ENDIF

         NEXT

      NEXT

      IF nCount > 0
         Html_LineText( nHandle, "" )
      ENDIF

      HTML_RawText( nHandle, "</details>" )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION strvalue( c, l )
*-----------------------------------------------------------------------------*

   SWITCH ValType( c )
   CASE "C"
   CASE "M" ; RETURN c
   CASE "N" ; RETURN hb_ntos( c )
   CASE "D" ; RETURN DToC( c )
   CASE "L" ; RETURN iif( hb_defaultValue( l, .F. ), iif( c, "ON", "OFF" ), iif( c, ".T.", ".F." ) )
   ENDSWITCH

RETURN ""

/* Date Created: 14/11/2005
   Author: Antonio Novo <antonionovo@gmail.com>
   Enable/Disable Error Detail */
*-----------------------------------------------------------------------------*
FUNCTION _lShowDetailError( lNewValue )
*-----------------------------------------------------------------------------*
   LOCAL cVarName := "_HMG" + ProcName()
   LOCAL lOldValue := _AddNewGlobal( cVarName, .T. )

   IF ISLOGICAL( lNewValue )
      _SetGetGlobal( cVarName, lNewValue )
   ENDIF

RETURN lOldValue

*-01-01-2003
*-Author: Antonio Novo
*-Create/Open the ErrorLog.Htm file
*-----------------------------------------------------------------------------*
FUNCTION HTML_ERRORLOG
*-----------------------------------------------------------------------------*
   LOCAL HtmArch
   LOCAL cErrorLogFile := _GetErrorlogFile()

   IF IsErrorLogActive()
#if ( __HARBOUR__ - 0 < 0x030200 )
      IF .NOT. File( cErrorLogFile )
         HtmArch := Html_Ini( cErrorLogFile, "Harbour MiniGUI Errorlog File" )
         IF HtmArch > 0
#else
      IF .NOT. hb_vfExists( cErrorLogFile )
         HtmArch := Html_Ini( cErrorLogFile, "Harbour MiniGUI Errorlog File" )
         IF HtmArch != NIL
#endif
            Html_Line( HtmArch )
         ENDIF
      ELSE
#if ( __HARBOUR__ - 0 < 0x030200 )
         HtmArch := FOpen( cErrorLogFile, FO_READWRITE )
         IF HtmArch > 0
            FSeek( HtmArch, __HTML_INSERT_OFFSET(), FS_END )
#else
         HtmArch := hb_vfOpen( cErrorLogFile, FO_WRITE )
         IF HtmArch != NIL
            hb_vfSeek( HtmArch, __HTML_INSERT_OFFSET(), FS_END )
#endif
         ENDIF
      ENDIF
   ENDIF

RETURN ( HtmArch )

*-30-12-2002
*-Author: Antonio Novo
*-HTML Page Head
*-----------------------------------------------------------------------------*
FUNCTION HTML_INI( ARCH, TITLE )
*-----------------------------------------------------------------------------*
   LOCAL HtmArch := -1, cTemplate

   IF IsErrorLogActive()
#if ( __HARBOUR__ - 0 < 0x030200 )
      HtmArch := FCreate( ARCH )
      IF FError() != 0
#else
      HtmArch := hb_vfOpen( ARCH, FO_CREAT + FO_TRUNC + FO_WRITE )
      IF HtmArch == NIL
#endif
         MsgStop( "Can`t open errorlog file " + ARCH, "Error" )
      ELSE
         cTemplate := __HTML_BODY_TEMPLATE()
         cTemplate := StrTran( cTemplate, "{{TITLE}}", TITLE )
         IF Set( _SET_CODEPAGE ) == "UTF8"
            cTemplate := StrTran( cTemplate, ["windows-1251"], ["utf-8"] )
         ENDIF
#if ( __HARBOUR__ - 0 < 0x030200 )
         FWrite( HtmArch, cTemplate )
#else
         hb_vfWrite( HtmArch, cTemplate )
#endif
      ENDIF
   ENDIF

RETURN ( HtmArch )

*-----------------------------------------------------------------------------*
PROCEDURE HTML_RAWTEXT( HTMARCH, LINEA )
*-----------------------------------------------------------------------------*
#if ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + CRLF )
#endif
   ENDIF

RETURN

*-30-12-2002
*-Author: Antonio Novo
*-HTM Page Line
*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINETEXT( HTMARCH, LINEA )
*-----------------------------------------------------------------------------*
#if ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + CRLF )
#endif
   ENDIF

RETURN

*-30-12-2002
*-Author: Antonio Novo
*-HTM Line
*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINE( HTMARCH )
*-----------------------------------------------------------------------------*
#if ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, "<HR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, "<HR>" + CRLF )
#endif
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE HTML_END( HTMARCH )
*-----------------------------------------------------------------------------*
#if ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, "</BODY></HTML>" )
      FClose( HTMARCH )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, "</BODY></HTML>" )
      hb_vfClose( HTMARCH )
#endif
   ENDIF

RETURN

// (JK) HMG 1.0 Build 6
*-----------------------------------------------------------------------------*
PROCEDURE _SetErrorLogFile( cFile )
*-----------------------------------------------------------------------------*
   _HMG_ErrorLogFile := IFEMPTY( cFile, GetStartUpFolder() + hb_ps() + "ErrorLog.htm", cFile )

RETURN

// Functions to insert HTML into error code - 24.05.21 Artyom Verchenko
*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_INSERT_OFFSET()
*-----------------------------------------------------------------------------*

RETURN ( -1 * Len( "</BODY></HTML>" ) )

*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_BODY_TEMPLATE()
*-----------------------------------------------------------------------------*

RETURN hb_base64Decode("PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PG1ldGEgY2hhcnNldD0id2luZG93cy0xMjUxIj48dGl0bGU+e3tUSVRMRX19PC90aXRsZT48c3R5bGU+Ym9keXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2JhY2tncm91bmQtY29sb3I6I2ZmZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO3BhZGRpbmc6MTVweH0uc3VtbWFyeSxkZXRhaWxzIHN1bW1hcnl7Y29sb3I6IzA2OTtiYWNrZ3JvdW5kOiNmZmM7Ym9yZGVyOjFweCBzb2xpZCAjOWFmO3BhZGRpbmc6NXB4O21hcmdpbjoxMHB4IDVweDtjdXJzb3I6cG9pbnRlcn0ubGlua3tkaXNwbGF5OmJsb2NrO2JhY2tncm91bmQ6I2NmYzt0ZXh0LWRlY29yYXRpb246bm9uZX1oMXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2ZvbnQtc2l6ZToxNTAlO2NvbG9yOiMwMGM7Zm9udC13ZWlnaHQ6NzAwO2JhY2tncm91bmQtY29sb3I6I2YwZjBmMH0udXBkYXRlZHtmb250LWZhbWlseTpzYW5zLXNlcmlmO2NvbG9yOiNjMDA7Zm9udC1zaXplOjExMCV9Lm5vcm1hbHRleHR7Zm9udC1mYW1pbHk6c2Fucy1zZXJpZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO2ZvbnQtd2VpZ2h0OjQwMDt0ZXh0LXRyYW5zZm9ybTpub25lO3RleHQtZGVjb3JhdGlvbjpub25lfS5sYXJnZS1zZWxlY3R7Zm9udC1zaXplOjEyNSU7cGFkZGluZzo4cHg7bWFyZ2luOjVweDtiYWNrZ3JvdW5kOiNjZGZ9PC9zdHlsZT48c2NyaXB0PmNvbnN0IGZpbHRlckJ5PShyLGUsbCxuPW51bGwpPT57bGV0IHQ9ci5tYXAobCk7dD10LnJlZHVjZSgoZSx0KT0+KHQgaW4gZXx8KGVbdF09MCksZVt0XSsrLGUpLHt9KSx0PU9iamVjdC5lbnRyaWVzKHQpLnJlZHVjZSgoZSxbdCxyXSk9PihlW3RdPVt0LHIsYFske3J9XSAke3R9YF0sZSkse30pO2NvbnN0IGM9ZG9jdW1lbnQucXVlcnlTZWxlY3RvcihlKTtPYmplY3QudmFsdWVzKHQpLnNvcnQoKGUsdCk9PnRbMV0tZVsxXSkuZm9yRWFjaCgoW2UsLHRdKT0+e2NvbnN0IHI9ZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgib3B0aW9uIik7ci52YWx1ZT1lLHIuaW5uZXJUZXh0PXQsYy5hcHBlbmRDaGlsZChyKX0pO2MuYWRkRXZlbnRMaXN0ZW5lcigiY2hhbmdlIixlPT57biYmbihjKTtjb25zdCB0PWUudGFyZ2V0LnZhbHVlO3IuZm9yRWFjaChlPT4oKGUsdCk9Pnt2YXIgcjsibnVsbCIhPT10PyhyPWwoZSksZS5zdHlsZS5kaXNwbGF5PXI9PT10P251bGw6Im5vbmUiLGNvbnNvbGUubG9nKGUsZS5zdHlsZS5kaXNwbGF5KSk6ZS5zdHlsZS5kaXNwbGF5PW51bGx9KShlLHQpKX0pfTtkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKCJET01Db250ZW50TG9hZGVkIixmdW5jdGlvbihlKXt2YXIgdD1bLi4uZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbCgiLnJlY29yZCIpXSxyPXQ9PmRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoIiNleHRyYS1wYW5lbCIpLnF1ZXJ5U2VsZWN0b3JBbGwoInNlbGVjdCIpLmZvckVhY2goZT0+dCE9PWUmJihlLnNlbGVjdGVkSW5kZXg9MCkpO2ZpbHRlckJ5KHQsIiNmaWx0ZXJCeURhdGUiLGU9PmUucXVlcnlTZWxlY3RvcigiLmRhdGUiKS5pbm5lclRleHQsciksZmlsdGVyQnkodCwiI2ZpbHRlckJ5U3RhY2t0cmFjZSIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuc3RhY2t0cmFjZSIpPy5jaGlsZE5vZGVzWzBdPy5kYXRhLnRyaW0oKSxyKSxmaWx0ZXJCeSh0LCIjZmlsdGVyQnlFcnJvciIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuZXJyb3IiKS5pbm5lclRleHQucmVwbGFjZSgvKFxyXG58XG58XHIpL2dtLCIiKS5yZXBsYWNlKC9cc3syLH0vZywiICIpLnRyaW0oKSxyKSxkb2N1bWVudC5xdWVyeVNlbGVjdG9yKCIjZXh0cmEtcGFuZWwiKS5zdHlsZS5kaXNwbGF5PW51bGwsY29uc29sZS5sb2coIkRPTUNvbnRlbnRMb2FkZWQiKX0pPC9zY3JpcHQ+PC9oZWFkPjxib2R5PjxoMSBzdHlsZT0idGV4dC1hbGlnbjpjZW50ZXIiPnt7VElUTEV9fTwvaDE+PGRpdiBpZD0iZXh0cmEtcGFuZWwiIHN0eWxlPSJkaXNwbGF5Om5vbmUiPjxzZWxlY3QgYXV0b2NvbXBsZXRlPSJvZmYiIGlkPSJmaWx0ZXJCeURhdGUiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRGF0ZTwvb3B0aW9uPjxvcHRpb24gdmFsdWU9Im51bGwiPkFsbCBkYXRlczwvb3B0aW9uPjwvc2VsZWN0PiA8c2VsZWN0IGF1dG9jb21wbGV0ZT0ib2ZmIiBpZD0iZmlsdGVyQnlTdGFja3RyYWNlIiBjbGFzcz0ibGFyZ2Utc2VsZWN0Ij48b3B0aW9uIHNlbGVjdGVkPSJzZWxlY3RlZCIgZGlzYWJsZWQ9ImRpc2FibGVkIiB2YWx1ZT0ibnVsbCI+RmlsdGVyIGJ5IFN0YWNrVHJhY2U8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgc3RhY2t0cmFjZXM8L29wdGlvbj48L3NlbGVjdD4gPHNlbGVjdCBhdXRvY29tcGxldGU9Im9mZiIgaWQ9ImZpbHRlckJ5RXJyb3IiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRXJyb3I8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgZXJyb3JzPC9vcHRpb24+PC9zZWxlY3Q+PC9kaXY+PC9ib2R5PjwvaHRtbD4=")
