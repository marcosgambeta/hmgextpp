//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// INI Files support procedures
// (c) 2003 Grigory Filatov
// (c) 2003 Janusz Pora
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

#include "minigui.ch"
#include "fileio.ch"

//---------------------------------------------------------------------------//
FUNCTION _SetGetLogFile(cFile)
//---------------------------------------------------------------------------//
   
   LOCAL cVarName := "_HMG_" + SubStr(ProcName(), 8)
   LOCAL cOld := _AddNewGlobal(cVarName, NIL)

   IF cFile != NIL
      _SetGetGlobal(cVarName, cFile)
      RETURN cFile
   ENDIF

RETURN cOld

//---------------------------------------------------------------------------//
FUNCTION _LogFile(lCrLf, ...)
//---------------------------------------------------------------------------//
   
   LOCAL hFile
   LOCAL i
   LOCAL xVal
   LOCAL cTp
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len(aParams)
   LOCAL cFile := hb_defaultValue(_SetGetLogFile(), GetStartUpFolder() + hb_ps() + "_MsgLog.txt")

   IF !Empty(cFile)
      hFile := iif(File(cFile), FOpen(cFile, FO_READWRITE), FCreate(cFile, FC_NORMAL))
      IF hFile == F_ERROR
         RETURN .F.
      ENDIF
      FSeek(hFile, 0, FS_END)
      IF nParams > 1
         IF (lCrLf := hb_defaultValue(lCrLf, .T.))
            FWrite(hFile, CRLF, 2)
         ENDIF
         IF nParams == 2 .AND. HB_ISNIL(aParams[2]) .AND. lCrLf
         ELSE
            FOR i := 2 TO nParams
               xVal := aParams[i]
               cTp := ValType(xVal)
               SWITCH cTp
               CASE "C"  ; xVal := iif(Empty(xVal), "'" + "'", Trim(xVal)) ; EXIT
               CASE "N"  ; xVal := hb_ntos(xVal)                           ; EXIT
               CASE "L"  ; xVal := iif(xVal, ".T.", ".F.")                 ; EXIT
               CASE "D"  ; xVal := hb_DToC(xVal, "DD.MM.YYYY")             ; EXIT
               CASE "A"  ; xVal := "ARRAY["  + hb_ntos(Len(xVal)) + "]"    ; EXIT
               CASE "H"  ; xVal :=  "HASH["  + hb_ntos(Len(xVal)) + "]"    ; EXIT
               CASE "B"  ; xVal := "'" + "B" + "'"                         ; EXIT
               CASE "T"  ; xVal := hb_TSToStr(xVal, .T.)                   ; EXIT
               CASE "U"  ; xVal := "NIL"                                   ; EXIT
               OTHERWISE ; xVal := "'" + cTp + "'"
               ENDSWITCH
               FWrite(hFile, xVal + Chr(9))
            NEXT
         ENDIF
      ELSE
         FWrite(hFile, CRLF, 2)
      ENDIF
      FClose(hFile)
   ENDIF

RETURN .T.

//---------------------------------------------------------------------------//
FUNCTION _BeginIni(cIniFile)
//---------------------------------------------------------------------------//
   
   LOCAL hFile

   IF At("\", cIniFile) == 0
      cIniFile := ".\" + cIniFile
   ENDIF

   IF Set(_SET_CODEPAGE) == "UTF8"

      hFile := iif(File(cIniFile), FOpen(cIniFile, FO_READ + FO_SHARED), HMG_CreateFile_UTF16LE_BOM(cIniFile))
      IF hFile == F_ERROR
         MsgInfo("Error opening a file INI. DOS ERROR: " + hb_ntos(FError()))
         Return -1
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF

      FClose(hFile)

   ELSE
      hFile := hb_vfOpen(cIniFile, iif(hb_vfExists(cIniFile), FO_READ + FO_SHARED, FO_CREAT + FO_READWRITE))
      IF hFile == NIL
         MsgInfo("Error opening a file INI. DOS ERROR: " + hb_ntos(FError()))
         Return -1
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF
      hb_vfClose(hFile)
  ENDIF

RETURN 0

//---------------------------------------------------------------------------//
FUNCTION _GetIni(cSection, cEntry, cDefault, uVar)
//---------------------------------------------------------------------------//
   
   LOCAL cVar As String

   IF !Empty(_HMG_ActiveIniFile)
      __defaultNIL(@cDefault, cVar)
      __defaultNIL(@uVar, cDefault)
      cVar  := hmg_GetPrivateProfileString(cSection, cEntry, xChar(cDefault), _HMG_ActiveIniFile)
   ELSE
      IF cDefault != NIL
         cVar := xChar(cDefault)
      ENDIF
   ENDIF

   uVar := xValue(cVar, ValType(uVar))

RETURN uVar

//---------------------------------------------------------------------------//
FUNCTION _SetIni(cSection, cEntry, cValue)
//---------------------------------------------------------------------------//
   
   LOCAL ret As Logical

   IF !Empty(_HMG_ActiveIniFile)
      ret := hmg_WritePrivateProfileString(cSection, cEntry, xChar(cValue), _HMG_ActiveIniFile)
   ENDIF

RETURN ret

//---------------------------------------------------------------------------//
FUNCTION _DelIniEntry(cSection, cEntry)
//---------------------------------------------------------------------------//
   
   LOCAL ret As Logical

   IF !Empty(_HMG_ActiveIniFile)
      ret := hmg_DelIniEntry(cSection, cEntry, _HMG_ActiveIniFile)
   ENDIF

RETURN ret

//---------------------------------------------------------------------------//
FUNCTION _DelIniSection(cSection)
//---------------------------------------------------------------------------//
   
   LOCAL ret As Logical

   IF !Empty(_HMG_ActiveIniFile)
      ret := hmg_DelIniSection(cSection, _HMG_ActiveIniFile)
   ENDIF

RETURN ret

//---------------------------------------------------------------------------//
FUNCTION _EndIni()
//---------------------------------------------------------------------------//
   _HMG_ActiveIniFile := ""

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION GetBeginComment
//---------------------------------------------------------------------------//
   
   LOCAL aLines
   LOCAL nLen
   LOCAL i
   LOCAL lTest := .T.
   LOCAL cComment := ""

   IF !Empty(_HMG_ActiveIniFile)
      aLines := hb_ATokens(StrTran(MemoRead(_HMG_ActiveIniFile), CRLF, Chr(10)), Chr(10))

      IF (nLen := Len(aLines)) > 0
         FOR i := 1 TO nLen
            aLines[i] := AllTrim(aLines[i])
            IF lTest
               IF hb_ULeft(aLines[i], 1) $ "#;"
                  cComment := aLines[i]
                  lTest := .F.
               ELSEIF !Empty(aLines[i])
                  lTest := .F.
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT i
      ENDIF
   ENDIF

RETURN SubStr(cComment, 2)

//---------------------------------------------------------------------------//
FUNCTION GetEndComment
//---------------------------------------------------------------------------//
   
   LOCAL aLines
   LOCAL nLen
   LOCAL i
   LOCAL lTest := .T.
   LOCAL cComment := ""

   IF !Empty(_HMG_ActiveIniFile)
      aLines := hb_ATokens(StrTran(MemoRead(_HMG_ActiveIniFile), CRLF, Chr(10)), Chr(10))

      IF (nLen := Len(aLines)) > 0
         FOR i := nLen TO 1 STEP -1
            aLines[i] := AllTrim(aLines[i])
            IF lTest
               IF hb_ULeft(aLines[i], 1) $ "#;"
                  cComment := aLines[i]
                  lTest := .F.
               ELSEIF !Empty(aLines[i])
                  lTest := .F.
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT i
      ENDIF
   ENDIF

RETURN SubStr(cComment, 2)

//---------------------------------------------------------------------------//
FUNCTION SetBeginComment(cComment)
//---------------------------------------------------------------------------//
   
   LOCAL aLines
   LOCAL nLen
   LOCAL i
   LOCAL lTest := .T.
   LOCAL cMemo := ""

   hb_default(@cComment, "")

   IF !Empty(_HMG_ActiveIniFile)
      aLines := hb_ATokens(StrTran(MemoRead(_HMG_ActiveIniFile), CRLF, Chr(10)), Chr(10))

      IF (nLen := Len(aLines)) > 0 .AND. Len(ATail(aLines)) == 0
         ASize(aLines, nLen - 1)
         nLen--
      ENDIF
      IF nLen > 0
         FOR i := 1 TO nLen
            aLines[i] := AllTrim(aLines[i])
            IF lTest
               IF hb_ULeft(aLines[i], 1) $ "#;"
                  IF Empty(cComment)
                     aLines[i] := ""
                  ELSE
                     IF !hb_ULeft(cComment := AllTrim(cComment), 1) $ "#;"
                        cComment := "#" + cComment
                     ENDIF
                     aLines[i] := cComment + CRLF
                  ENDIF
                  lTest := .F.
               ELSEIF Empty(aLines[i])
                  aLines[i] += CRLF
               ELSEIF Empty(cComment)
                  aLines[i] += CRLF
                  lTest := .F.
               ELSE
                  AAdd(aLines, NIL)
                  nLen++
                  AIns(aLines, i)
                  IF !hb_ULeft(cComment := AllTrim(cComment), 1) $ "#;"
                     cComment := "#" + cComment
                  ENDIF
                  aLines[i] := cComment + CRLF
                  lTest := .F.
               ENDIF
            ELSE
               aLines[i] += CRLF
            ENDIF
            cMemo := cMemo + aLines[i]
         NEXT i
         hb_MemoWrit(_HMG_ActiveIniFile, cMemo)
      ENDIF
   ENDIF

RETURN cComment

//---------------------------------------------------------------------------//
FUNCTION SetEndComment(cComment)
//---------------------------------------------------------------------------//
   
   LOCAL aLines
   LOCAL nLen
   LOCAL i
   LOCAL lTest := .T.
   LOCAL cMemo := ""

   hb_default(@cComment, "")
   cComment := AllTrim(cComment)

   IF !Empty(_HMG_ActiveIniFile)
      aLines := hb_ATokens(StrTran(MemoRead(_HMG_ActiveIniFile), CRLF, Chr(10)), Chr(10))
      nLen := Len(aLines)
      IF nLen > 0 .AND. hb_ULen(ATail(aLines)) == 0
         ASize(aLines, nLen - 1)
         nLen--
      ENDIF
      IF nLen > 0
         FOR i := nLen TO 1 STEP -1
            aLines[i] := AllTrim(aLines[i])
            IF lTest
               IF Empty(aLines[i])
                  // Remove empty trailing lines
               ELSEIF hb_ULeft(aLines[i], 1) $ "#;"
                  IF Empty(cComment)
                     // Remove previous comment
                  ELSE
                     // Replace previous comment
                     IF !hb_ULeft(cComment, 1) $ "#;"
                        cComment := "#" + cComment
                     ENDIF
                     cMemo := cComment + CRLF
                  ENDIF
                  lTest := .F.
               ELSEIF Empty(cComment)
                  // Do not add comment
                  lTest := .F.
               ELSE
                  // Add comment as the last line
                  IF !hb_ULeft(cComment, 1) $ "#;"
                     cComment := "#" + cComment
                  ENDIF
                  cMemo := CRLF + cComment + CRLF
                  // Add line
                  cMemo := aLines[i] + CRLF + cMemo
                  lTest := .F.
               ENDIF
            ELSE
               // Add line
               cMemo := aLines[i] + CRLF + cMemo
            ENDIF
         NEXT i
         IF hb_ULeft(cMemo, Len(CRLF)) == CRLF
            cMemo := SubStr(cMemo, Len(CRLF) + 1)
         ENDIF
         hb_MemoWrit(_HMG_ActiveIniFile, cMemo)
      ENDIF
   ENDIF

RETURN cComment

//---------------------------------------------------------------------------//
FUNCTION xChar(xValue)
//---------------------------------------------------------------------------//
   
   LOCAL cType := ValType(xValue)
   LOCAL cValue := ""
   LOCAL nDecimals := Set(_SET_DECIMALS)

   SWITCH cType
   CASE "C"
   CASE "M" ; cValue := xValue ; EXIT
   CASE "N" ; nDecimals := iif(xValue == Int(xValue), 0, nDecimals) ; cValue := LTrim(Str(xValue, 20, nDecimals)) ; EXIT
   CASE "D" ; cValue := DToS(xValue) ; EXIT
   CASE "L" ; cValue := iif(xValue, "T", "F") ; EXIT
   CASE "A" ; cValue := AToC(xValue) ; EXIT
   CASE "U"
   CASE "E" ; cValue := "NIL" ; EXIT
   CASE "B" ; cValue := "{||...}" ; EXIT
   CASE "O" ; cValue := "{" + xValue:className + "}"
   ENDSWITCH

RETURN cValue

//---------------------------------------------------------------------------//
FUNCTION xValue(cValue, cType)
//---------------------------------------------------------------------------//

   LOCAL xValue

   SWITCH cType
   CASE "C"
   CASE "M"  ; xValue := cValue            ; EXIT
   CASE "D"  ; xValue := SToD(cValue)      ; EXIT
   CASE "N"  ; xValue := Val(cValue)       ; EXIT
   CASE "L"  ; xValue := ( cValue == "T" ) ; EXIT
   CASE "A"  ; xValue := CToA(cValue)      ; EXIT
   OTHERWISE ; xValue := NIL // Nil, Block, Object
   ENDSWITCH

RETURN xValue

//---------------------------------------------------------------------------//
FUNCTION AToC(aArray)
//---------------------------------------------------------------------------//

   LOCAL elem
   LOCAL cElement
   LOCAL cType
   LOCAL cArray := ""

   FOR EACH elem IN aArray
      cElement := xChar(elem)
      IF (cType := ValType(elem)) == "A"
         cArray += cElement
      ELSE
         cArray += hb_ULeft(cType, 1) + Str(hb_ULen(cElement), 4) + cElement
      ENDIF
   NEXT

RETURN ("A" + Str(hb_ULen(cArray), 4) + cArray)

//---------------------------------------------------------------------------//
FUNCTION CToA(cArray)
//---------------------------------------------------------------------------//
   
   LOCAL cType
   LOCAL nLen
   LOCAL aArray := {}

   cArray := hb_USubStr(cArray, 6)    // strip off array and length
   DO WHILE hb_ULen(cArray) > 0
      nLen := Val(hb_USubStr(cArray, 2, 4))
      IF (cType := hb_ULeft(cArray, 1)) == "A"
         AAdd(aArray, CToA(hb_USubStr(cArray, 1, nLen + 5)))
      ELSE
         AAdd(aArray, xValue(hb_USubStr(cArray, 6, nLen), cType))
      ENDIF
      cArray := hb_USubStr(cArray, 6 + nLen)
   ENDDO

RETURN aArray

// JK HMG 1.0 experimental build 6
//---------------------------------------------------------------------------//
FUNCTION _GetSectionNames(cIniFile)
//---------------------------------------------------------------------------//
   // return 1-dimensional array with section list in cIniFile
   // or empty array if no sections are present
   
   LOCAL aSectionList := {}
   LOCAL aLista

   IF File(cIniFile)
      aLista := hmg__GetPrivateProfileSectionNames(cIniFile)
      IF !Empty(aLista)
         AEval(aLista, {|cVal|iif(Empty(cVal), NIL, AAdd(aSectionList, cVal))})
      ENDIF
   ELSE
      MsgStop("Can`t open " + cIniFile, "Error")
   ENDIF

RETURN aSectionList

//---------------------------------------------------------------------------//
FUNCTION _GetSection(cSection, cIniFile)
//---------------------------------------------------------------------------//
   // return 2-dimensional array with {key,value} pairs from section cSection in cIniFile
   
   LOCAL aKeyValueList := {}
   LOCAL aLista
   LOCAL i
   LOCAL n

   IF File(cIniFile)
      aLista := hmg__GetPrivateProfileSection(cSection, cIniFile)
      IF !Empty(aLista)
         FOR i := 1 TO Len(aLista)
            IF (n := At("=", aLista[i])) > 0
               AAdd(aKeyValueList, {Left(aLista[i], n - 1), SubStr(aLista[i], n + 1)})
            ENDIF
         NEXT i
      ENDIF
   ELSE
      MsgStop("Can`t open " + cIniFile, "Error")
   ENDIF

RETURN aKeyValueList
