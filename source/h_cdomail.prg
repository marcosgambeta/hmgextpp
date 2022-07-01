/*
 * Harbour TCDOMail Class v1.0
 * Copyright 2022 Grigory Filatov <gfilatov@gmail.com>
 *
 */

#include "minigui.ch"
#include "hbclass.ch"

#define CDO_SENDUSINGPICKUP  1  // Send message using local SMTP service pickup directory.
#define CDO_SENDUSINGPORT    2  // Send the message using SMTP over TCP/IP networking.

#define CDO_AUTH_ANONYMOUS   0  // No authentication
#define CDO_AUTH_BASIC       1  // BASIC clear text authentication
#define CDO_AUTH_NTLM        2  // NTLM, Microsoft proprietary authentication

#define CDO_LOW_PRIORITY     0  // Low importance.
#define CDO_NORMAL_PRIORITY  1  // Normal importance.
#define CDO_HIGH_PRIORITY    2  // High importance.

#define CDO_DSN_DEFAULT      0  // Delivery Status Notification


CLASS TCDOMail

   CLASSDATA bEmail
   DATA cSubject, cTextBody
   DATA cServer, nPort, cUser, cPass
   DATA lReceipt, nPriority
   DATA aOrigin, aRecipients, aFiles

   DATA CCopy AS CHARACTER INIT ""
   DATA nTimeout AS NUMERIC INIT 30

   VAR lSuccess AS LOGICAL INIT .F.

   METHOD New( cServer, nPort, cUser, cPass, ;
      cSubject, cText, nPriority, lReceipt, aOrigin, aRecipients, ;
      aFiles ) CONSTRUCTOR

   METHOD Activate()

ENDCLASS


METHOD New( cServer, nPort, cUser, cPass, ;
      cSubject, cText, nPriority, lReceipt, aOrigin, aRecipients, ;
      aFiles ) CLASS TCDOMail

   DEFAULT cText := "", cSubject := "", ;
      cServer := "", nPort := 465, cUser := "", cPass := "", ;
      lReceipt := .F., nPriority := CDO_NORMAL_PRIORITY, ;
      aOrigin := {}, aRecipients := {}, aFiles := {}

   ::cTextBody := cText
   ::cSubject := cSubject
   ::cServer := cServer
   ::nPort := nPort
   ::cUser := cUser
   ::cPass := cPass
   ::lReceipt := lReceipt
   ::nPriority := nPriority
   ::aOrigin := aOrigin
   ::aRecipients := aRecipients
   ::aFiles := aFiles

RETURN Self


METHOD Activate() CLASS TCDOMail

   LOCAL oEmailMsg, oError
   LOCAL cSchema := "http://schemas.microsoft.com/cdo/configuration/"
   LOCAL cEmailFromName, cEmailFrom, nEl, nLen, cTmp := ""

   IF ::bEmail != NIL
      Eval( ::bEmail, Self )
      RETURN NIL
   ENDIF

   TRY

      oEmailMsg := CREATEOBJECT ( "CDO.Message" )

      WITH OBJECT oEmailMsg

         cEmailFromName := ::aOrigin[ 1 ]
         cEmailFrom := ::aOrigin[ 2 ]

         IF Empty( cEmailFrom )
            cEmailFrom := cEmailFromName
         ELSE
            cEmailFrom := cEmailFromName + " <" + cEmailFrom + ">"
         ENDIF

         :From := cEmailFrom

         IF ( nLen := Len( ::aRecipients ) ) > 0

            FOR nEl := 1 TO nLen
               IF Empty( ::aRecipients[ nEl ][ 2 ] )
                  cTmp := cTmp + ::aRecipients[ nEl ][ 1 ] + iif( nEl = nLen, "", ";" )
               ELSE
                  cTmp := cTmp + ::aRecipients[ nEl ][ 1 ] + " <" + ::aRecipients[ nEl ][ 2 ] + ">" + iif( nEl = nLen, "", ";" )
               ENDIF
            NEXT

            :To = cTmp

         ENDIF

         :CC := ::CCopy
         :BCC := ""
         :Subject := ::cSubject
         IF "<" $ ::cTextBody .AND. ">" $ ::cTextBody
            :HTMLBody := ::cTextBody
         ELSE
            :TextBody := ::cTextBody
         ENDIF
         :BodyPart:Charset := "utf-8"

         IF ( nLen := Len( ::aFiles ) ) > 0
            FOR nEl := 1 TO nLen
               :AddAttachment( ::aFiles[ nEl ][ 1 ] ) // Full path must be informed
            NEXT
         ENDIF

         WITH OBJECT :configuration:Fields

            :Item( cSchema + "smtpserver" ):Value := ::cServer
            :Item( cSchema + "smtpserverport" ):Value := ::nPort
            :Item( cSchema + "sendusing" ):Value := CDO_SENDUSINGPORT
            :Item( cSchema + "smtpauthenticate" ):Value := CDO_AUTH_BASIC
            :Item( cSchema + "smtpusessl" ):Value := ( ::nPort == 465 )
            :Item( cSchema + "sendusername" ):Value := ::cUser
            :Item( cSchema + "sendpassword" ):Value := ::cPass
            :Item( cSchema + "smtpconnectiontimeout" ):Value := ::nTimeout

            :Update()

         END WITH

         WITH OBJECT oEmailMsg:Fields

            :Item( "urn:schemas:httpmail:importance" ):Value := ::nPriority
            :Item( "urn:schemas:mailheader:X-Priority" ):Value := ::nPriority - 1
            IF ::lReceipt
               :Item( "urn:schemas:mailheader:return-receipt-to" ):Value := cEmailFrom
               :Item( "urn:schemas:mailheader:disposition-notification-to" ):Value := cEmailFrom
            ENDIF

            :Update()

         END WITH

         :DSNOptions := CDO_DSN_DEFAULT

         :Send()

         ::lSuccess := .T.

      END WITH

   CATCH oError

      MsgStop ( "The email was not sent." + CRLF + ;
         "Error:      " + cValToChar( oError:GenCode ) + CRLF + ;
         "SubCode:   " + cValToChar( oError:SubCode ) + CRLF + ;
         "OSCode:    " + cValToChar( oError:OsCode ) + CRLF + ;
         "SubSystem: " + cValToChar( oError:SubSystem ) + CRLF + ;
         "Description:      " + oError:Description )

   END

RETURN NIL
