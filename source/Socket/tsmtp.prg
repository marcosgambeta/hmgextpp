//
// Harbour Project source code:
// TSMTP class
//
// Copyright 2001-2003 Matteo Baccan <baccan@infomedia.it>
// www - https://harbour.github.io/
//

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this software; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.

#include <common.ch>
#include <hbclass.ch>
#include <set.ch>

/****** TSMTP/TSMTP
*  NAME
*    TSMTP
*  PURPOSE
*    Create a SMTP connection
*  METHODS
*    TSMTP:new
*    TSMTP:connect
*    TSMTP:login
*    TSMTP:loginMD5
*    TSMTP:close
*    TSMTP:cleardata
*    TSMTP:setfrom
*    TSMTP:setreplyto
*    TSMTP:setsubject
*    TSMTP:setpriority
*    TSMTP:addto
*    TSMTP:addcc
*    TSMTP:addbcc
*    TSMTP:setdata
*    TSMTP:AddAttach
*    TSMTP:send
*    TSMTP:GetLastError
*    TSMTP:SetSendTimeout
*  EXAMPLE
*
*  SEE ALSO
*    TDecode
**********
*/
// RFC 821
CLASS TSMTP

  EXPORTED:
   METHOD New()

   METHOD Connect(cAddress, nPort, cHelo)
   METHOD Login(cUser, cPwd)
   METHOD LoginMD5( cUser, cPwd )
   METHOD Close()

   METHOD ClearData()
   METHOD SetFrom(cUser, cEmail)
   METHOD SetReplyTo(cReplyTo)
   METHOD SetSubject(cSubject)
   METHOD SetPriority(nPriority)

   METHOD AddTo(cUser, cEmail)
   METHOD AddCc(cUser, cEmail)
   METHOD AddBcc(cUser, cEmail)

   METHOD SetData(cMail, bHTML)

   METHOD AddAttach(cAttach)

   METHOD Send(bIgnoreTOError, bRequestReturnReceipt)
   METHOD GetLastError()

   METHOD SetSendTimeout(nMilliSec)

  HIDDEN:
   METHOD GetLines()

   CLASSDATA oSocket   HIDDEN
   CLASSDATA cFrom     HIDDEN
   CLASSDATA cReplyTo  HIDDEN
   CLASSDATA cEmail    HIDDEN
   CLASSDATA cSubject  HIDDEN
   CLASSDATA nPriority HIDDEN
   CLASSDATA aTo       HIDDEN
   CLASSDATA aCc       HIDDEN
   CLASSDATA aBcc      HIDDEN
   CLASSDATA cData     HIDDEN
   CLASSDATA bHTML     HIDDEN
   CLASSDATA aAttach   HIDDEN
   CLASSDATA cError    HIDDEN

ENDCLASS


METHOD TSMTP:New()
::oSocket := TSocket():New()
//::oSocket:SetDebug(.T.)
::ClearData()
return Self

//
// Connect to remore site
//
METHOD TSMTP:Connect(cAddress, nPort, cHelo)
local bRet, cErr

DEFAULT nPort TO 25
DEFAULT cHelo TO ::oSocket:GetLocalName()

bRet := ::oSocket:Connect(cAddress,nPort)

// If connect read banner string
if bRet
   // Consume banner
   cErr := ::GetLines()
   if LEFT(cErr, 3)=="220"
      // Send extended hello first (RFC 2821)
      if ::oSocket:SendString("EHLO " + cHelo + CHR(13) + CHR(10))
         cErr := ::GetLines()
         if !(LEFT(cErr, 3)=="250")
            // Send hello (RFC 821)
            if ::oSocket:SendString("HELO " + cHelo + CHR(13) + CHR(10))
               cErr := ::GetLines()
               if !(LEFT(cErr, 3)=="250")
                  ::cError := cErr
               else
                  bRet := .T.
               endif
            endif
         else
            bRet := .T.
         endif
      endif
   else
      ::cError := cErr
      bRet := .F.
   endif
endif

return bRet

// Receive lines (answer) from server 
METHOD TSMTP:GetLines()
local cLines := ""
local cLine

while (len(cLine := ::oSocket:ReceiveLine()) > 0)
   cLines += iif(len(cLine)>0, cLine + CHR(13) + CHR(10), "")
   if substr(cLine, 4, 1)==" " .OR. len(cLine)<=3 .OR. substr(cLine, 4, 1)==CHR(10)
      exit
   endif
enddo

return cLines

// Login to server
METHOD TSMTP:Login(cUser, cPwd)
local cErr    := ""
local bRet    := .F.
local oDecode := TDecode():new()

if ::oSocket:SendString("AUTH LOGIN" + CHR(13) + CHR(10))
   // Consume banner
   cErr := ::GetLines()
   if LEFT(cErr, 3)=="334"
      if ::oSocket:SendString(oDecode:Encode64(cUser) + CHR(13) + CHR(10))
         // Consume banner
         cErr := ::GetLines()
         if LEFT(cErr, 3)=="334"
            if ::oSocket:SendString(oDecode:Encode64(cPwd) + CHR(13) + CHR(10))
               // Consume banner
               cErr := ::GetLines()
               if LEFT(cErr, 3)=="235" .OR. LEFT(cErr, 3)=="335"
                  bRet := .T.
               endif
            endif
         endif
      endif
   endif
endif

if !bRet
   ::cError := cErr
endif

return bRet

// Login to server
METHOD TSMTP:LoginMD5( cUser, cPwd )
local cErr    := ""
local bRet    := .F.
local oDecode := TDecode():new()
local cDigest, hMac

if ::oSocket:SendString("AUTH CRAM-MD5" + CHR(13) + CHR(10))
   // Consume banner
   cErr := ::GetLines()
   if LEFT(cErr, 3)=="334"
      cDigest := substr(cErr, 5)
      hMac    := oDecode:hmac_md5( cUser, cPwd, cDigest )
      if ::oSocket:SendString(hMac + CHR(13) + CHR(10))
         // Consume banner
         cErr := ::GetLines()
         if LEFT(cErr, 3)=="235" .OR. LEFT(cErr, 3)=="335"
            bRet := .T.
         endif
      endif
   endif
endif

if !bRet
   ::cError := cErr
endif

return bRet


//
// Close socket
//
METHOD TSMTP:Close()
::oSocket:SendString("QUIT" + CHR(13) + CHR(10))
::GetLines()
return ::oSocket:Close()

//
// Clear data
//
METHOD TSMTP:ClearData()
::cFrom     := ""
::cReplyTo  := ""
::cEmail    := ""
::nPriority := 3
::aTo       := {}
::aCc       := {}
::aBcc      := {}
::cData     := ""
::bHTML     := .F.
::aAttach   := {}
::cError    := ""
return NIL

//
// Set From
//
METHOD TSMTP:SetFrom(cUser, cEmail)
::cFrom  := cUser
::cEmail := cEmail
return NIL

//
// Set Reply-To
//
METHOD TSMTP:SetReplyTo(cReplyTo)
::cReplyTo := cReplyTo
return NIL

//
// Set Subject
//
METHOD TSMTP:SetSubject(cSubject)
::cSubject := cSubject
return NIL

//
// Set Priority
//
METHOD TSMTP:SetPriority(nPriority)
::nPriority := nPriority
return NIL

//
// Add to
//
METHOD TSMTP:AddTo(cUser, cEmail)
aadd(::aTo, {cUser, cEmail})
return NIL

//
// Add cc
//
METHOD TSMTP:AddCc(cUser, cEmail)
aadd(::aCc, {cUser, cEmail})
return NIL

//
// Add Bcc
//
METHOD TSMTP:AddBcc(cUser, cEmail)
aadd(::aBcc, {cUser, cEmail})
return NIL

//
// Set data
//
METHOD TSMTP:SetData(cMail, bHTML)
DEFAULT bHTML TO .F.
::cData := cMail
::bHTML := bHTML
return NIL

//
// Add attach
//
METHOD TSMTP:AddAttach(cAttach)
aadd(::aAttach, cAttach)
return NIL

//
// Get Error
//
METHOD TSMTP:GetLastError()
return ::cError

//
// Set Send timeout
//
METHOD TSMTP:SetSendTimeout(nMilliSec)
::oSocket:SetSendTimeout(nMilliSec)
return NIL

//
// Send message
//
METHOD TSMTP:Send(bIgnoreTOError, bRequestReturnReceipt)
local bRet := .F.
local cHeader := ""
local cErr
local bMultipart := (LEN(::aAttach)>0)
local cMultipart := "----=_NextPart_000_0052_01C2F554.33353C20"
local oDecode := TDecode():new()
local nPos, aEmails, bMail, dDate
local cOldDateFormat, nOldEpoch, cOldLang

DEFAULT bIgnoreTOError TO .F.
DEFAULT bRequestReturnReceipt TO .F. // request a return receipt for your email

::cError := ""
if ::oSocket:SendString("MAIL FROM: " + ::cEmail + CHR(13) + CHR(10))
   // Banner
   cErr := ::GetLines()
   // Check 250
   if left(cErr, 3)=="250" .OR. left(cErr, 3)=="550"

      aEmails := array(0)
      AEVAL(::aTO,  {|aSub|AADD(aEmails, aSub[2])})
      AEVAL(::aCC,  {|aSub|AADD(aEmails, aSub[2])})
      AEVAL(::aBCC, {|aSub|AADD(aEmails, aSub[2])})

      bMail := .T.
      for nPos := 1 to len(aEmails)
         if bMail
            ::oSocket:SendString("RCPT TO: " + aEmails[nPos] + CHR(13) + CHR(10))
            cErr := ::GetLines()
            if !(LEFT(cErr, 3)=="250") .AND. !bIgnoreTOError
               ::cError := cErr
               bMail := .F.
            endif
         endif
      next

      if bMail
         // If all is OK I can prepare and send data
         if ::oSocket:SendString("DATA" + CHR(13) + CHR(10))
            // Banner
            cErr := ::GetLines()
            // Check 354 or 554
            if LEFT(cErr, 3)=="354" .OR. LEFT(cErr, 3)=="554"
               nOldEpoch := Set(_SET_EPOCH, 1980)
               cOldDateFormat := Set(_SET_DATEFORMAT, "mm/dd/yyyy")
               dDate := Date()
               cOldLang := Set(_SET_LANGUAGE, "EN")
               //Date: Sat, 14 Aug 2004 14:18:08 +0100
               cHeader := "Date: " + left(cDoW(dDate), 3) + ", " + ltrim(trans(Day(dDate), "99 "));
                     + trans(cMonth(dDate), "AAA") + " " + trans(Year(dDate), "9999 ") + Time();
                     + " " + GETTIMEZONEDIFF() +CHR(13)+CHR(10)

               Set(_SET_LANGUAGE, cOldLang)
               Set(_SET_DATEFORMAT, cOldDateFormat)
               Set(_SET_EPOCH, nOldEpoch)

               cHeader += "From: "     +::cFrom +" " +::cEmail +CHR(13)+CHR(10)
               cHeader += "Reply-To: " + iif(Empty(::cReplyTo), ::cFrom +" " +::cEmail, ::cReplyTo) +CHR(13)+CHR(10)

               cHeader += addAddress(::aTO, "To: ")
               cHeader += addAddress(::aCC, "CC: ")
               cHeader += addAddress(::aBCC, "BCC: ")

               cHeader += "Subject: "  +::cSubject +CHR(13)+CHR(10)

               //## add properties to modify it
               cHeader += "X-Mailer: Harbour TSMTP by Matteo Baccan" +CHR(13)+CHR(10)
               cHeader += "X-Priority: " + trans(::nPriority, "9 ") + "(";
                     + iif(::nPriority==1, "Highest", iif(::nPriority==5, "Low", "Normal")) + ")" +CHR(13)+CHR(10)
               if bRequestReturnReceipt
                  cHeader += "Disposition-Notification-To: " +::cEmail +CHR(13)+CHR(10)
               endif

               cHeader += "MIME-Version: 1.0" +CHR(13)+CHR(10)
               if bMultipart .OR. ::bHTML
                  cHeader += "Content-Type: multipart/mixed;" +CHR(13)+CHR(10)
                  cHeader += [        boundary="] +cMultipart +["] +CHR(13)+CHR(10)

                  // Empty line
                  cHeader += "" +CHR(13)+CHR(10)
                  cHeader += "This is a multi-part message in MIME format." +CHR(13)+CHR(10)
                  cHeader += "" +CHR(13)+CHR(10)

                  // Message
                  cHeader += "--" +cMultipart +CHR(13)+CHR(10)
                  if ::bHTML
                     cHeader += "Content-Type: text/html;" +CHR(13)+CHR(10)
                  else
                     cHeader += "Content-Type: text/plain;" +CHR(13)+CHR(10)
                     cHeader += [        charset="iso-8859-1"] +CHR(13)+CHR(10)
                  endif
                  cHeader += "Content-Transfer-Encoding: base64" +CHR(13)+CHR(10)
                  cHeader += "" +CHR(13)+CHR(10)
                  cHeader += oDecode:Encode64(::cData) +CHR(13)+CHR(10)
                  cHeader += "" +CHR(13)+CHR(10)

                  // Attach
                  for nPos := 1 to len(::aAttach)
                     cHeader += "--" +cMultipart +CHR(13)+CHR(10)
                     cHeader += "Content-Type: application/octet-stream;" +CHR(13)+CHR(10)
                     cHeader += [        name="] +cFileWithoutPath(::aAttach[nPos]) +["] +CHR(13)+CHR(10)
                     cHeader += "Content-Transfer-Encoding: base64" +CHR(13)+CHR(10)
                     cHeader += "Content-Disposition: attachment;" +CHR(13)+CHR(10)
                     cHeader += [        filename="] +cFileWithoutPath(::aAttach[nPos]) +["] +CHR(13)+CHR(10)
                     cHeader += "" +CHR(13)+CHR(10)
                     cHeader += oDecode:Encode64( memoread(::aAttach[nPos]), 57 ) +CHR(13)+CHR(10)
                     cHeader += "" +CHR(13)+CHR(10)
                  next

                  // End of mail
                  cHeader += "--" +cMultipart +"--"
               else
                  cHeader += "Content-Type: text/plain; charset=us-ascii" +CHR(13)+CHR(10)
                  cHeader += "Content-Transfer-Encoding: 7bit" +CHR(13)+CHR(10)

                  // Empty line
                  cHeader += "" +CHR(13)+CHR(10)

                  // Data
                  cHeader += ::cData
               endif

               // End of mail
               cHeader += CHR(13)+CHR(10) +"." +CHR(13)+CHR(10)

               if ::oSocket:SendString(cHeader)
                  cErr := ::GetLines()
                  if !(LEFT(cErr, 3)=="250" .OR. LEFT(cErr, 3)=="550")
                     ::cError := cErr
                  else
                     bRet := .T.
                  endif
               endif
            else
               ::cError := cErr
            endif
         endif
      endif
   else
      ::cError := cErr
   endif

endif

return bRet

* 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北�
STATIC FUNCTION addAddress(aEmail, cTok)
* 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北�
local cRet := ""

if len(aEmail) > 0
   cRet += cTok
   AEVAL(aEmail, {|aSub, nPos| cRet += iif(nPos==1, "", ","+CHR(13)+CHR(10)+"   ") + aSub[2]})
   cRet += CHR(13)+CHR(10)
endif

RETURN cRet

* 北北北北北北北北北北北北北北北北北北北北北�
Function GETTIMEZONEDIFF()
* 北北北北北北北北北北北北北北北北北北北北北�
Local cBias := "", cHour, cMin
Local nBias := GetTimeZoneBias() * (-1)

If nBias <= 0
   cBias := "-"
   nBias := nBias * (-1)
else
   cBias := "+"
endif

if nBias == 0
   cBias += "0000"
else
   cHour := PADL(INT(nBias / 60), 2, "0" )
   cMin  := PADL(INT(nBias % 60), 2, "0" )
   cBias += cHour+cMin
endif

Return cBias

* 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北�
STATIC Function cFileWithoutPath(cPathMask)
* 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北�
LOCAL n1 := RAt("\", cPathMask), n2 := RAt("/", cPathMask), n

        n := max(n1, n2)

Return IIf(n > 0 .AND. n < Len(cPathMask), ;
   Right(cPathMask, Len(cPathMask) - n), ;
   IIf(( n := At(":", cPathMask) ) > 0, ;
   Right(cPathMask, Len(cPathMask) - n), cPathMask))


#pragma BEGINDUMP

#include <windows.h>

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapifs.hpp>

#ifndef HB_LEGACY_LEVEL
#define FHANDLE HB_FHANDLE
#endif

HB_FUNC(GETTIMEZONEBIAS)
{
   TIME_ZONE_INFORMATION tzInfo;
   DWORD retval = GetTimeZoneInformation(&tzInfo);

   if( retval == TIME_ZONE_ID_INVALID )
   {
      hb_retnl(0);
   }
   else
   {
      hb_retnl(tzInfo.Bias + (retval == TIME_ZONE_ID_STANDARD ? tzInfo.StandardBias : tzInfo.DaylightBias));
   }
}

HB_FUNC_STATIC(MEMOREAD)
{
   auto pFileName = hb_param(1, Harbour::Item::STRING);

   if( pFileName )
   {
      FHANDLE fhnd = hb_fsOpen(hb_itemGetCPtr(pFileName), FO_READ | FO_SHARED | FO_PRIVATE);

      if( fhnd != FS_ERROR )
      {
         ULONG ulSize = hb_fsSeek(fhnd, 0, FS_END);

         if( ulSize != 0 )
         {
            auto pbyBuffer = static_cast<BYTE*>(hb_xgrab(ulSize + sizeof(char)));

            hb_fsSeek(fhnd, 0, FS_SET);
            hb_fsReadLarge(fhnd, pbyBuffer, ulSize);

            hb_retclen_buffer((char *) pbyBuffer, ulSize);
         }
         else
         {
            hb_retc(nullptr);
         }

         hb_fsClose(fhnd);
      }
      else
      {
         hb_retc(nullptr);
      }
   }
   else
   {
      hb_retc(nullptr);
   }
}

#pragma ENDDUMP
