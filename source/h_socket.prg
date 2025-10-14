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

#xtranslate hb_OsNewLine() => hb_eol()
//---------------------------------------------------------------------------//
FUNCTION httpconnect(Connection, Server, Port)
//---------------------------------------------------------------------------//

   LOCAL oUrl

   IF !( Lower(Left(Server, 7)) == "http://" )
      Server := "http://" + Server
   ENDIF

   oUrl := TUrl():New(Server + ":" + hb_ntos(Port))

   IF hb_IsString(Connection)

      Public &Connection

      IF Empty(oUrl)
         &Connection := NIL
      ELSE
         &Connection := TIpClientHttp():New(oUrl)

         IF !( &Connection ):Open()
            &Connection := NIL
         ENDIF
      ENDIF

   ELSE

      IF Empty(oUrl)
         Connection := NIL
      ELSE
         Connection := TIpClientHttp():New(oUrl)

         IF !Connection:Open()
            Connection := NIL
         ENDIF
      ENDIF

   ENDIF

RETURN NIL

//---------------------------------------------------------------------------//
FUNCTION httpgeturl(Connection, cPage, uRet)
//---------------------------------------------------------------------------//
   
   LOCAL cUrl
   LOCAL cResponse
   LOCAL cHeader
   LOCAL cRet
   LOCAL i

   cUrl := "http://"

   IF !Empty(Connection:oUrl:cUserid)
      cUrl += Connection:oUrl:cUserid
      IF !Empty(Connection:oUrl:cPassword)
         cUrl += ":" + Connection:oUrl:cPassword
      ENDIF
      cUrl += "@"
   ENDIF

   IF !Empty(Connection:oUrl:cServer)
      cUrl += Connection:oUrl:cServer
      IF Connection:oUrl:nPort > 0
         cUrl += ":" + hb_ntos(Connection:oUrl:nPort)
      ENDIF
   ENDIF

   cUrl += cPage

   IF Connection:Open(cUrl)

      cResponse := Connection:Read()
      IF !hb_IsString(cResponse)
         cResponse := "<No data returned>"
      ENDIF

      IF hb_IsLogical(uRet)

         cHeader := Connection:cReply
         IF !hb_IsString(cHeader)
            cHeader := "<No header returned>"
         ENDIF
         cHeader += hb_osNewLine()

         FOR i := 1 TO Len(Connection:hHeaders)
            cHeader += hb_HKeyAt(Connection:hHeaders, i) + ": " + hb_HValueAt(Connection:hHeaders, i) + hb_osNewLine()
         NEXT
         cHeader += hb_osNewLine()

         IF uRet                       // return DATA and HEADERS
            cRet := cHeader + cResponse
         ELSE                          // return HEADERS only
            cRet := cHeader
         ENDIF

      ELSE                             // return DATA only

         cRet := cResponse

      ENDIF

   ELSE

      cRet := "<Error opening URL>"

   ENDIF

RETURN cRet
