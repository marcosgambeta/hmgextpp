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
FUNCTION GetColor(aInitColor, aCustomColors, nFlags)
//---------------------------------------------------------------------------//

   LOCAL aRetVal[3]
   LOCAL nColor
   LOCAL nInitColor

   IF IsArrayRGB(aInitColor)
      nInitColor := RGB(aInitColor[1], aInitColor[2], aInitColor[3])
   ENDIF

   /* aCustomColors parameter must be the array with 16 RGB-colors elements if it is defined */

   IF hb_IsLogical(nFlags)  // for HMG compatibility
      IF !nFlags
         #define CC_RGBINIT                        1
         #define CC_PREVENTFULLOPEN        4
         #define CC_ANYCOLOR                256
         nFlags := hb_BitOr(CC_ANYCOLOR, CC_PREVENTFULLOPEN, CC_RGBINIT)
      ELSE
         /* default nFlags value is hb_BitOr(CC_ANYCOLOR, CC_FULLOPEN, CC_RGBINIT) */
         nFlags := NIL
      ENDIF
   ENDIF

   IF (nColor := hmg_ChooseColor(NIL, nInitColor, aCustomColors, nFlags)) != -1
      aRetVal := nRGB2Arr(nColor)
   ENDIF

RETURN aRetVal

//---------------------------------------------------------------------------//
FUNCTION GetFolder(cTitle, cInitPath, nFlags, lNewFolderButton, nFolderType) // JK HMG 1.0 Experimental Build 8
//---------------------------------------------------------------------------//
RETURN hmg_C_BrowseForFolder(NIL, cTitle, ;
   hb_defaultValue(nFlags, BIF_USENEWUI + BIF_VALIDATE + ;
   iif(hb_defaultValue(lNewFolderButton, .T.), 0, BIF_NONEWFOLDERBUTTON)), nFolderType, cInitPath)

//---------------------------------------------------------------------------//
FUNCTION BrowseForFolder(nFolderType, nFlags, cTitle, cInitPath) // Contributed By Ryszard Rylko
//---------------------------------------------------------------------------//
RETURN hmg_C_BrowseForFolder(NIL, cTitle, ;
   hb_defaultValue(nFlags, hb_BitOr(BIF_NEWDIALOGSTYLE, BIF_EDITBOX, BIF_VALIDATE)), nFolderType, cInitPath)

//---------------------------------------------------------------------------//
FUNCTION GetFile(aFilter, title, cIniFolder, multiselect, lNoChangeCurDir, nFilterIndex)
//---------------------------------------------------------------------------//
   
   LOCAL fileslist As Array
   LOCAL cFilter As String
   LOCAL n
   LOCAL files

   hb_default(@multiselect, .F.)

   IF hb_IsArray(aFilter)
      AEval(aFilter, {|x|cFilter += x[1] + Chr(0) + x[2] + Chr(0)})
      cFilter += Chr(0)
   ENDIF

   files := hmg_C_GetFile(cFilter, title, cIniFolder, multiselect, lNoChangeCurDir, hb_defaultValue(nFilterIndex, 1))

   IF multiselect

      IF Len(files) > 0

         IF hb_IsArray(files)

            FOR n := 1 TO Len(files)

               IF At("\\", files[n]) > 0 .AND. Left(files[n], 2) != "\\"

                  files[n] := StrTran(files[n] , "\\", "\")

               ENDIF

            NEXT

            fileslist := AClone(files)

         ELSE

            AAdd(fileslist, files)

         ENDIF

      ENDIF

   ELSE

      fileslist := files

   ENDIF

RETURN fileslist

//---------------------------------------------------------------------------//
FUNCTION Putfile(aFilter, title, cIniFolder, lNoChangeCurDir, cDefFileName, ;
   /*@*/nFilterIndex, lPromptOverwrite) //  p.d. 12/05/2016 added lPromptOverwrite
//---------------------------------------------------------------------------//
   
   LOCAL cFilter As String

   hb_default(@nFilterIndex, 1)

   IF hb_IsArray(aFilter)
      AEval(aFilter, {|x|cFilter += x[1] + Chr(0) + x[2] + Chr(0)})
      cFilter += Chr(0)
   ENDIF

RETURN hmg_C_PutFile(cFilter, title, cIniFolder, lNoChangeCurDir, hb_defaultValue(cDefFileName, ""), ;
   @nFilterIndex, hb_defaultValue(lPromptOverwrite, .F.))

//---------------------------------------------------------------------------//
FUNCTION GetFont(cInitFontName, nInitFontSize, lBold, lItalic, anInitColor, lUnderLine, lStrikeOut, nCharset)
//---------------------------------------------------------------------------//
   
   LOCAL RetArray
   LOCAL rgbcolor As Numeric

   IF IsArrayRGB(anInitColor)
      rgbcolor := RGB(anInitColor[1] , anInitColor[2] , anInitColor[3])
   ENDIF

   RetArray := hmg_ChooseFont(hb_defaultValue(cInitFontName, "") , hb_defaultValue(nInitFontSize, 0) , ;
      hb_defaultValue(lBold, .F.) , hb_defaultValue(lItalic, .F.) , rgbcolor, ;
      hb_defaultValue(lUnderLine, .F.) , hb_defaultValue(lStrikeOut, .F.) , hb_defaultValue(nCharSet, 0))

   IF Empty(RetArray[1])
      RetArray[5] := { NIL, NIL, NIL }
   ELSE
      rgbcolor := RetArray[5]
      RetArray[5] := nRGB2Arr(rgbcolor)
   ENDIF

RETURN RetArray
