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

#include "minigui.ch"

//---------------------------------------------------------------------------//
FUNCTION WindowsVersion()
//---------------------------------------------------------------------------//

   LOCAL cKey
   LOCAL aRetVal := Array(4)

   IF IsWin10OrLater()
      cKey := "SOFTWARE\Microsoft\Windows NT\CurrentVersion"
      aRetVal[1] := GetRegistryValue(HKEY_LOCAL_MACHINE, cKey, "ProductName")
      IF hb_osisWin11()
         aRetVal[1] := StrTran(aRetVal[1], "10", "11")
         aRetVal[2] := GetRegistryValue(HKEY_LOCAL_MACHINE, cKey, "DisplayVersion")
      ELSE
         aRetVal[2] := GetRegistryValue(HKEY_LOCAL_MACHINE, cKey, "ReleaseId")
      ENDIF
      aRetVal[3] := GetRegistryValue(HKEY_LOCAL_MACHINE, cKey, "CurrentBuild") + "." + ;
         hb_ntos(GetRegistryValue(HKEY_LOCAL_MACHINE, cKey, "UBR", "N"))
      aRetVal[4] := ""
   ELSE
      aRetVal := hmg_WinVersion()
   ENDIF

RETURN { aRetVal[1] + aRetVal[4] , aRetVal[2] , "Build " + aRetVal[3] }

//---------------------------------------------------------------------------//
FUNCTION _Execute(hWnd, cOperation, cFile, cParameters, cDirectory, nState)
//---------------------------------------------------------------------------//
RETURN hmg_ShellExecute(hb_defaultValue(hWnd, hmg_GetActiveWindow()) , ;
   cOperation /* possible values are 'edit', 'explore', 'find', 'open', 'print' */ , ;
   hb_defaultValue(cFile, "") , cParameters, cDirectory, hb_defaultValue(nState, SW_SHOWNORMAL))

//---------------------------------------------------------------------------//
PROCEDURE ShellAbout(cTitle, cMsg, hIcon)
//---------------------------------------------------------------------------//
   
   LOCAL nCount

   IF _SetGetGlobal("_HMG_ShellAbout") == NIL
      STATIC _HMG_ShellAbout AS GLOBAL VALUE 0
   ENDIF

   IF (nCount := _SetGetGlobal("_HMG_ShellAbout")) == 0

      ASSIGN GLOBAL _HMG_ShellAbout := ++nCount

      IF hmg_C_ShellAbout(hmg_GetActiveWindow(), cTitle, cMsg, hIcon)
         hmg_DestroyIcon(hIcon)
         ASSIGN GLOBAL _HMG_ShellAbout := --nCount
      ENDIF

   ENDIF

RETURN
