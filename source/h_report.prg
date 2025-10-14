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
#include "winprint.ch"

#define EVERYPAGE "EVERY PAGE"
#define MAXFONT 12
#define MINORFONT 6

MEMVAR ctitle, aheaders1, aheaders2, afields, awidths, atotals, nlpp, ldos, lpreview, cgraphic, nfi, nci, nff, ncf, ;
   lmul, cgrpby, chdrgrp, llandscape, ncpl, lselect, calias, nllmargin, aformats, npapersize, ntoprow, ldatetimestamp

MEMVAR _npage, angrpby, wfield, nlmargin, nfsize, ISEVERYPAGE, wfield1, crompe

MEMVAR aline, cgraphicalt, afieldsg, nposgrp, cFontName

STATIC aMessages

FUNCTION easyreport()

   PARAMETERS ctitle, ;
              aheaders1, aheaders2, ;
              afields, ;
              awidths, ;
              atotals, ;
              nlpp, ;
              ldos, ;
              lpreview, ;
              cgraphic, ;
              nfi, ;
              nci, ;
              nff, ;
              ncf, ;
              lmul, ;
              cgrpby, ;
              chdrgrp, ;
              llandscape, ;
              ncpl, ;
              lselect, ;
              calias, ;
              nllmargin, ;
              aformats, ;
              npapersize, ;
              ntoprow, ;
              ldatetimestamp, ;
              cFontName  // P.D. 2021/07/01

   LOCAL nlen
   LOCAL nlin
   LOCAL i
   LOCAL ncol
   LOCAL aresul
   LOCAL lmode
   LOCAL swt := 0
   LOCAL nran
   LOCAL grpby
   LOCAL cfile
   LOCAL k
   LOCAL cpagina
   LOCAL ntotalchar
   LOCAL norient
   LOCAL nRecNo
   LOCAL cType
   LOCAL aT
   LOCAL aW
   LOCAL aD

   MEMVAR cfilerepo

   PUBLIC _npage, angrpby, wfield, nlmargin, nfsize, ISEVERYPAGE, wfield1, crompe
   
   InitReportMessages()
   
   hb_Default(@cFontName, "Courier New")

   ldatetimestamp := !ldatetimestamp

   nlen := Len(afields)
   IF Len(awidths) > nlen
      ASize(awidths, nlen)
   ENDIF
   nlmargin := iif(nllmargin == NIL, 0, nllmargin)
   IF aformats == NIL
      aformats := AFill(Array(nlen), "")
   ENDIF
   IF atotals == NIL
      atotals := AFill(Array(nlen), .F.)
   ENDIF
   IF npapersize == NIL
      npapersize := DMPAPER_LETTER
   ENDIF
   _npage := 0
   grpby := cgrpby
   IF grpby != NIL
      grpby := Upper(grpby)
   ENDIF
   aresul := Array(nlen)
   angrpby := Array(nlen)
   FOR i := 1 TO nlen
      afields[i] := Upper(afields[i])
   NEXT i

   ISEVERYPAGE := ( grpby == EVERYPAGE )

   cfile := calias
   Select(calias)
   aFieldsg := (cfile)->(Array(FCount()))
   aT := (cfile)->(Array(FCount()))
   aW := (cfile)->(Array(FCount()))
   aD := (cfile)->(Array(FCount()))
   (cfile)->(AFields(aFieldsg, aT, aW, aD))
   lmode := .T.
   IF nlpp == NIL
      nlpp := 50
   ENDIF
   IF ldos
      lmode := .F.
      IF ncpl == NIL
         ncpl := 80
      ELSE
         IF ncpl = 132
            @ PRow(), PCol() SAY Chr(15)
         ENDIF
      ENDIF
      IF lpreview
         nran := random(9999999)
         cfilerepo := "T" + hb_ntos(nran) + ".prn"
         DO WHILE File("&cfilerepo")
            nran := random(9999999)
            cfilerepo := "T" + hb_ntos(nran) + ".prn"
         ENDDO
         SET PRINTER to &cfilerepo
         SET DEVICE TO PRINT
      ELSE
         SET DEVICE TO PRINT
      ENDIF
   ELSE
      ntotalchar := nlmargin + nlen - 1 // sum spaces between column
      AEval(awidths, {|w|ntotalchar += w})
      IF ncpl == NIL
         ncpl := ntotalchar
      ENDIF

      INIT PRINTSYS
      IF lselect
         IF lpreview
            SELECT BY DIALOG PREVIEW
         ELSE
            SELECT BY DIALOG
         ENDIF
      ELSE
         IF lpreview
            SELECT DEFAULT PREVIEW
         ELSE
            SELECT DEFAULT
         ENDIF
      ENDIF
      IF HBPRNERROR != 0
         RETURN NIL
      ENDIF

      SET PAGE PAPERSIZE npapersize
      nfsize := MAXFONT
      DEFINE FONT "f0" NAME (cFontName) SIZE nfsize
      norient := iif(llandscape, DMORIENT_LANDSCAPE, DMORIENT_PORTRAIT)

      FOR i := nfsize TO MINORFONT step - 0.25

         CHANGE FONT "f0" NAME (cFontName) SIZE i
         SET PAGE ORIENTATION norient FONT "f0"
         SELECT FONT "f0"
         IF ntotalchar <= HBPRNMAXCOL
            nfsize := i
            DEFINE FONT "f1" NAME (cFontName) SIZE nfsize BOLD
            DEFINE FONT "f2" NAME (cFontName) SIZE nfsize + 2 BOLD
            nlpp := iif(ISEVERYPAGE, HBPRNMAXROW - 4, HBPRNMAXROW - 2)
            ncpl := ntotalchar
            EXIT
         ELSE
            IF i == MINORFONT
               IF !llandscape
                  MsgStop(aMessages[1] + CRLF + CRLF + ;
                          aMessages[2] + CRLF + ;
                          aMessages[3] + CRLF + CRLF + ;
                          aMessages[4], aMessages[5])
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      NEXT

      SELECT FONT "f0"
      IF ncpl > HBPRNMAXCOL .OR. nlpp > HBPRNMAXROW
         cpagina := ""
         IF npapersize <= Len(apapeles)
            cpagina := apapeles[npapersize]
         ENDIF
         IF nlpp <= HBPRNMAXROW
            nlpp := iif(ISEVERYPAGE, HBPRNMAXROW - 4, HBPRNMAXROW - 2)
         ENDIF

         msgstop(aMessages[6]  + cpagina + " (" + hb_ntos(npapersize) + ")" + CRLF + ;
                 aMessages[7]  + hb_ntos(nfsize) + CRLF + ;
                 aMessages[8]  + Str(HBPRNMAXROW, 3) + CRLF + ;
                 aMessages[9]  + Str(nlpp, 3) + CRLF + ;
                 aMessages[10] + Str(HBPRNMAXCOL, 3) + CRLF + ;
                 aMessages[11] + Str(ncpl, 3), aMessages[19])
      ENDIF

      START DOC
      START PAGE
   ENDIF

   IF ntoprow == NIL
      nTopRow := 1
   ENDIF

   nlin := ntoprow
   IF cgraphic != NIL .AND. !ldos
      IF !File(cgraphic)
         msgstop(aMessages[12], aMessages[13])
       ELSE
         @nfi, nci + nlmargin PICTURE cgraphic SIZE nff - nfi - 4, ncf - nci - 3
      ENDIF
   ENDIF
   nlin := headers(aheaders1, aheaders2, awidths, nlin, ctitle, lmode, grpby, chdrgrp)
   AFill(aresul, 0)
   AFill(angrpby, 0)
   IF grpby != NIL
      nposgrp := AScan(afieldsg, grpby)
      IF nposgrp > 0
         wfield1 := afieldsg[nposgrp]
      ELSE
         wfield1 := "STR(nlpp)"
      ENDIF
      crompe := &wfield1
   ENDIF
   nRecNo := (cfile)->(RecNo())  //JP 18
   DO WHILE !(cfile)->(Eof())
      swt := 0
      imp_SUBTOTALES(@nlin, @ncol, @lmode, @swt, @grpby)

      ncol := nlmargin + 1
      FOR i := 1 TO nlen
         wfield := afields[i]
         IF Type(afields[i]) == "B"
            DO CASE
            CASE hb_IsChar(afields[i])
               wfield := Eval(&(afields[i]))
            OTHERWISE
               wfield := Eval(afields[i])
            ENDCASE
         ELSE
            wfield := afields[i]
            wfield := &wfield       // value
         ENDIF
         cType := ValType(wfield)

         IF lmode
            SWITCH cType
            CASE "C"
               IF hmg_IsOemText(wfield)
                  wfield := hb_OEMToANSI(wfield)
               ENDIF
               @ nlin, ncol SAY SubStr(wfield, 1, awidths[i]) font "f0" TO PRINT
               EXIT
            CASE "N"
               @ nlin, ncol SAY  iif(!( aformats[i] == "" ), Transform(wfield, aformats[i]), Str(wfield, awidths[i])) font "f0" TO PRINT
               EXIT
            CASE "D"
               @ nlin, ncol SAY SubStr(DToC(wfield), 1, awidths[i]) font "f0" TO PRINT
               EXIT
            CASE "L"
               @ nlin, ncol SAY iif(wfield, ".T.", ".F.") font "f0" TO PRINT
               EXIT
            CASE "M"
               FOR k := 1 TO MLCount(wfield, awidths[i]) STEP 1
                  @ nlin, ncol SAY justificalinea(MemoLine(wfield, awidths[i], k), awidths[i]) font "f0" TO PRINT
                  nlin := nlin + 1
                  // Imprimir otra p�gina?
                  imp_pagina(@nlin, @lmode, @grpby, @chdrgrp)
               NEXT k
               EXIT
            OTHERWISE
               @ nlin, ncol SAY Replicate("_", awidths[i]) font "f0" TO PRINT
            ENDSWITCH
         ELSE
            SWITCH cType
            CASE "C"
               @ nlin, ncol SAY SubStr(wfield, 1, awidths[i])
               EXIT
            CASE "N"
               @ nlin, ncol SAY iif(!( aformats[i] == "" ), Transform(wfield, aformats[i]), Str(wfield, awidths[i]))
               EXIT
            CASE "D"
               @ nlin, ncol SAY SubStr(DToC(wfield), 1, awidths[i])
               EXIT
            CASE "L"
               @ nlin, ncol SAY iif(wfield, ".T.", ".F.")
               EXIT
            CASE "M"
               FOR k := 1 TO MLCount(wfield, awidths[i])
                  @ nlin, ncol SAY justificalinea(MemoLine(wfield, awidths[i], k), awidths[i])
                  nlin := nlin + 1
                  IF nlin > nlpp
                     nlin := ntoprow
                     nlin := headers(aheaders1, aheaders2, awidths, nlin, ctitle, lmode, grpby, chdrgrp)
                  ENDIF
               NEXT k
               EXIT
            OTHERWISE
               @ nlin, ncol SAY Replicate("_", awidths[i])
            ENDSWITCH
         ENDIF

         ncol += awidths[i] + 1
         IF atotals[i]
            aresul[i] := aresul[i] + wfield
            swt := 1
            IF grpby != NIL
               angrpby[i] := angrpby[i] + wfield
            ENDIF
         ENDIF
      NEXT

      nlin++
      imp_pagina(@nlin, @lmode, @grpby, @chdrgrp)

      (cfile)->(dbSkip())
   ENDDO
   // FIN DE LA IMPRESI�N DEL �LTIMO REGISTRO DE LA TABLA
   // IMPRESI�N DEL SUBTOTAL DEL ULTIMO GRUPO DE LA TABLA EN CASO DE HABER SUBTOTALES
   IF swt == 1 // EXISTE COLUMNA DE TOTALES
      ncol := nlmargin + 1
      imp_SUBTOTALES(@nlin, @ncol, @lmode, @swt, @grpby)

      // IMPRESI�N DEL TOTAL GENERAL DEL RESUMEN
      IF lmode
         IF AScan(atotals, .T.) > 0
            @nlin, ncol SAY aMessages[20] font "f1" TO PRINT
         ENDIF
      ELSE
         IF AScan(atotals, .T.) > 0
            @nlin, ncol SAY aMessages[20]
         ENDIF
      ENDIF
      nlin++
      ncol := nlmargin + 1
      FOR i := 1 TO nlen STEP 1
         IF atotals[i]
            IF lmode
               @nlin, ncol SAY iif(!( aformats[i] == "" ), Transform(aresul[i], aformats[i]), Str(aresul[i], awidths[i])) font "f1" TO PRINT
            ELSE
               @nlin, ncol SAY iif(!( aformats[i] == "" ), Transform(aresul[i], aformats[i]), Str(aresul[i], awidths[i]))
            ENDIF
         ENDIF
         ncol += awidths[i] + 1
      NEXT i
      nlin++
      ncol := nlmargin + 1
      IF lmode
         @ nlin, ncol SAY " " font "f0" TO PRINT
      ELSE
         @ nlin, ncol SAY " "
         EJECT
      ENDIF
   ENDIF
   (cfile)->(dbGoto(nRecNo))  //JP 18
   IF ldos
      IF lpreview
         SET DEVICE TO SCREEN
         SET PRINTER TO
         mypreview(cfilerepo)
      ELSE
         SET DEVICE TO SCREEN
      ENDIF
   ELSE
      END PAGE
      END DOC
      RELEASE PRINTSYS
   ENDIF
   RELEASE _npage, angrpby, wfield, nlmargin, nfsize, ISEVERYPAGE, wfield1, crompe

RETURN NIL

STATIC FUNCTION headers(aheaders1, aheaders2, awidths, nlin, ctitle, lmode, grpby, chdrgrp)

   LOCAL i
   LOCAL ncol
   LOCAL nsum
   LOCAL ncenter
   LOCAL ncenter2
   LOCAL npostitle
   LOCAL ctitle1
   LOCAL ctitle2

   nsum := 0
   AEval(awidths, {|w|nsum += w})
   npostitle := At("|", ctitle)
   ctitle2 := ""
   IF npostitle > 0
      ctitle1 := Left(ctitle, npostitle - 1)
      ctitle2 := SubStr(ctitle, npostitle + 1, Len(ctitle))
   ELSE
      ctitle1 := ctitle
   ENDIF
   ncenter := ( nsum - Len(ctitle1) )/2
   IF Len(ctitle2) > 0
      ncenter2 := ( nsum - Len(ctitle2) )/2
   ENDIF
   _npage++
   IF lmode
      IF hmg_IsOemText(ctitle1)
         ctitle1 := hb_OEMToANSI(ctitle1)
      ENDIF
      @ nlin, 1 + nlmargin SAY _HMG_MESSAGE[9] font "f0" TO PRINT
      @ nlin, 6 + nlmargin SAY hb_ntos(_npage) font "f0" TO PRINT
      @ nlin, ncenter + nlmargin SAY ctitle1 font "f2" TO PRINT
      IF ldatetimestamp
         @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Date() font "f0" TO PRINT
      ENDIF
   ELSE
      @ nlin, 1 + nlmargin SAY _HMG_MESSAGE[9]
      @ nlin, 6 + nlmargin SAY hb_ntos(_npage)
      @ nlin, ncenter + nlmargin SAY ctitle1
      IF ldatetimestamp
         @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Date()
      ENDIF
   ENDIF
   nlin++

   IF Len(ctitle2) > 0
      IF lmode
         IF hmg_IsOemText(ctitle2)
            ctitle2 := hb_OEMToANSI(ctitle2)
         ENDIF
         @ nlin, ncenter2 + nlmargin SAY ctitle2 font "f2" TO PRINT
         IF ldatetimestamp
            @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Time() font "f0" TO PRINT
         ENDIF
      ELSE
         @ nlin, ncenter2 + nlmargin SAY ctitle2
         IF ldatetimestamp
            @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Time()
         ENDIF
      ENDIF
      nlin++
   ELSEIF ldatetimestamp
      IF lmode
         @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Time() font "f0" TO PRINT
      ELSE
         @ nlin, nsum - 10 + Len(afields) + nlmargin SAY Time()
      ENDIF
      nlin++
   ENDIF

   nlin++
   ncol := nlmargin + 1
   FOR i := 1 TO Len(awidths)
      IF lmode
         @ nlin, ncol SAY Replicate("-", awidths[i]) font "f0" TO PRINT
      ELSE
         @ nlin, ncol SAY Replicate("-", awidths[i])
      ENDIF
      ncol += awidths[i] + 1
   NEXT i

   IF Len(aheaders1) > 0
      nlin++
      ncol := nlmargin + 1
      FOR i := 1 TO Len(awidths)
         IF lmode
            IF hmg_IsOemText(aheaders1[i])
               aheaders1[i] := hb_OEMToANSI(aheaders1[i])
            ENDIF
            @ nlin, ncol SAY SubStr(aheaders1[i], 1, awidths[i]) font "f1" TO PRINT
         ELSE
            @ nlin, ncol SAY SubStr(aheaders1[i], 1, awidths[i])
         ENDIF
         ncol += awidths[i] + 1
      NEXT i
   ENDIF

   IF Len(aheaders2) > 0
      nlin++
      ncol := nlmargin + 1
      FOR i := 1 TO Len(awidths)
         IF lmode
            IF hmg_IsOemText(aheaders2[i])
               aheaders2[i] := hb_OEMToANSI(aheaders2[i])
            ENDIF
            @ nlin, ncol SAY SubStr(aheaders2[i], 1, awidths[i]) font "f1" TO PRINT
         ELSE
            @ nlin, ncol SAY SubStr(aheaders2[i], 1, awidths[i])
         ENDIF
         ncol += awidths[i] + 1
      NEXT i
   ENDIF

   nlin++
   ncol := nlmargin + 1
   FOR i := 1 TO  Len(awidths)
      IF lmode
         @ nlin, ncol SAY Replicate("-", awidths[i]) font "f0" TO PRINT
      ELSE
         @ nlin, ncol SAY Replicate("-", awidths[i])
      ENDIF
      ncol += awidths[i] + 1
   NEXT i
   nlin += 2
   IF grpby != NIL
      IF !ISEVERYPAGE
         IF lmode
            IF hmg_IsOemText(chdrgrp)
               chdrgrp := hb_OEMToANSI(chdrgrp)
            ENDIF
            cgrpby := &grpby
            IF hmg_IsOemText(cgrpby)
               cgrpby := hb_OEMToANSI(cgrpby)
            ENDIF
            @ nlin, 1 + nlmargin SAY "** " + chdrgrp + " ** " + hb_ValToStr(cgrpby) font "f1" TO PRINT
         ELSE
            @ nlin, 1 + nlmargin SAY "** " + chdrgrp + " ** " + hb_ValToStr(&grpby)
         ENDIF
         nlin++
      ENDIF

   ENDIF

RETURN nlin

STATIC FUNCTION mypreview(cfilerepo)

   LOCAL wr

   MEMVAR wfilerepo

   wfilerepo := cfilerepo
   wr := MemoRead(wfilerepo)
   IF hmg_IsOemText(wr)
      wr := hb_OEMToANSI(wr)
   ENDIF

   DEFINE WINDOW PRINT_PREVIEW ;
      AT 10, 10 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE "Preview ----- " + WFILEREPO ;
      MODAL

   @ 0, 0 EDITBOX EDIT_P ;
      WIDTH 630 ;
      HEIGHT 440 ;
      VALUE wr ;
      READONLY ;
      FONT "Courier new" ;
      SIZE 10

   END WINDOW
   CENTER WINDOW PRINT_PREVIEW
   ACTIVATE WINDOW PRINT_PREVIEW

   IF MSGYESNO(aMessages[14] , aMessages[15])
      RUN TYPE &WFILEREPO > PRN
   ENDIF
   IF File("&WFILErepo")
      ERASE &WFILEREPO
   ENDIF

RETURN NIL

STATIC FUNCTION JUSTIFICALINEA(WPR_LINE, WTOPE)

   LOCAL I
   LOCAL SPACE1 := Space(1)
   LOCAL WLARLIN := Len(Trim(WPR_LINE))

   FOR I := 1 TO WLARLIN
      IF WLARLIN == WTOPE
         EXIT
      ENDIF
      IF SubStr(WPR_LINE, I, 1) = SPACE1 .AND. SubStr(WPR_LINE, I - 1, 1) != SPACE1 .AND. SubStr(WPR_LINE, I + 1, 1) != SPACE1
         WPR_LINE := LTrim(SubStr(WPR_LINE, 1, I - 1)) + Space(2) + LTrim(SubStr(WPR_LINE, I + 1, Len(WPR_LINE) - I))
         WLARLIN++
      ENDIF
   NEXT I

RETURN WPR_LINE

FUNCTION extreport(cfilerep)

   LOCAL nContlin
   LOCAL i
   LOCAL ctitle
   LOCAL aheaders1
   LOCAL aheaders2
   LOCAL afields
   LOCAL awidths
   LOCAL atotals
   LOCAL aformats
   LOCAL nlpp
   LOCAL ncpl
   LOCAL nllmargin
   LOCAL calias
   LOCAL ldos
   LOCAL lpreview
   LOCAL lselect
   LOCAL cgraphic
   LOCAL lmul
   LOCAL nfi
   LOCAL nci
   LOCAL nff
   LOCAL ncf
   LOCAL cgrpby
   LOCAL chdrgrp
   LOCAL llandscape
   LOCAL lnodatetimestamp
   LOCAL cfont
   LOCAL creport
   LOCAL ipaper

   IF !File(cfilerep + ".rpt")
      msginfo("(" + cfilerep + ".rpt) " + aMessages[16])
      RETURN NIL
   ENDIF

   PUBLIC aline := {}

   creport := MemoRead(cfilerep + ".rpt")

   nContlin := MLCount(cReport)
   FOR i := 1 TO nContlin
      AAdd(Aline, MemoLine(cReport, 500, i, , .T.))
   NEXT i
   ctitle := leadato("REPORT", "TITLE", "")
   IF Len(ctitle) > 0
      ctitle := &ctitle
   ENDIF
   aheaders1 := leadatoh("REPORT", "HEADERS", "{}", 1)
   aheaders1 := &aheaders1
   aheaders2 := leadatoh("REPORT", "HEADERS", "{}", 2)
   aheaders2 := &aheaders2
   afields := leadato("REPORT", "FIELDS", "{}")
   IF Len(afields) == 0
      msginfo(aMessages[17], aMessages[13])
      RELEASE aline
      RETURN NIL
   ENDIF
   afields := &afields
   awidths := leadato("REPORT", "WIDTHS", "{}")
   IF Len(awidths) == 0
      msginfo(aMessages[18], aMessages[13])
      RELEASE aline
      RETURN NIL
   ENDIF
   awidths := &awidths
   atotals := leadato("REPORT", "TOTALS", NIL)
   IF atotals != NIL
      atotals := &atotals
   ENDIF
   aformats := leadato("REPORT", "NFORMATS", NIL)
   IF aformats != NIL
      aformats := &aformats
   ENDIF
   nlpp := Val(leadato("REPORT", "LPP", ""))
   ncpl := Val(leadato("REPORT", "CPL", ""))
   nllmargin := Val(leadato("REPORT", "LMARGIN", "0"))
   ntoprow := Val(leadato("REPORT", "TMARGIN", "1"))
   npapersize := leadato("REPORT", "PAPERSIZE", "DMPAPER_LETTER")
   IF npapersize = "DMPAPER_USER"
      npapersize := 255
   ENDIF
   IF Len(npapersize) == 0
      npapersize := NIL
   ELSE
      ipaper := AScan(apapeles, npapersize)
      IF ipaper == 0
         ipaper := 1
      ENDIF
      npapersize := ipaper
   ENDIF
   calias := leadato("REPORT", "WORKAREA", "")
   ldos := leadatologic("REPORT", "DOSMODE", .F.)
   lpreview := leadatologic("REPORT", "PREVIEW", .F.)
   lselect := leadatologic("REPORT", "SELECT", .F.)
   lmul := leadatologic("REPORT", "MULTIPLE", .F.)

   cgraphic := clean(leaimage("REPORT", "IMAGE", ""))
   IF Len(cgraphic) == 0
      cgraphic := NIL
   ENDIF
   nfi := Val((learowi("IMAGE", 1)))
   nci := Val((leacoli("IMAGE", 1)))
   nff := Val((learowi("IMAGE", 2)))
   ncf := Val((leacoli("IMAGE", 2)))
   cgraphicalt := ( leadato("DEFINE REPORT", "IMAGE", "") )
   IF Len(cgraphicalt) > 0  // para sintaxis DEFINE REPORT
      cgraphicalt := &cgraphicalt
      cgraphic := cgraphicalt[1]
      nfi := cgraphicalt[2]
      nci := cgraphicalt[3]
      nff := cgraphicalt[4]
      ncf := cgraphicalt[5]
   ENDIF
   cgrpby := clean(leadato("REPORT", "GROUPED BY", ""))
   IF Len(cgrpby) == 0
      cgrpby := NIL
   ENDIF
   chdrgrp := clean(leadato("REPORT", "HEADRGRP", ""))
   llandscape := leadatologic("REPORT", "LANDSCAPE", .F.)
   lnodatetimestamp := leadatologic("REPORT", "NODATETIMESTAMP", .F.)
   cfont := leadato("REPORT", "FONTNAME", NIL)

   easyreport(ctitle, aheaders1, aheaders2, afields, awidths, atotals, nlpp, ldos, lpreview, cgraphic, nfi, nci, nff, ncf, lmul, cgrpby, chdrgrp, llandscape, ncpl, lselect, calias, nllmargin, aformats, npapersize, ntoprow, lnodatetimestamp, cfont)
   RELEASE aline

RETURN NIL

STATIC FUNCTION leadato(cName, cPropmet, cDefault)

   LOCAL i
   LOCAL sw := 0
   LOCAL npos
   LOCAL cfvalue

   FOR i := 1 TO Len(aline)
      IF !( At(Upper(cname ) + " ", Upper(aline[i])) == 0 )
         sw := 1
      ELSE
         IF sw == 1
            npos := At(Upper(cPropmet) + " ", Upper(aline[i]))
            IF Len(Trim(aline[i])) == 0
               EXIT
            ENDIF
            IF npos > 0
               cfvalue := SubStr(aline[i], npos + Len(cPropmet), Len(aline[i]))
               cfvalue := Trim(cfvalue)
               IF Right(cfvalue, 1) = ";"
                  cfvalue := SubStr(cfvalue, 1, Len(cfvalue) - 1)
               ENDIF
               RETURN AllTrim(cfvalue)
            ENDIF
         ENDIF
      ENDIF
   NEXT i

RETURN cDefault

STATIC FUNCTION leaimage(cName, cPropmet, cDefault)

   LOCAL i
   LOCAL sw1 := 0
   LOCAL lin := 0
   LOCAL npos1
   LOCAL npos2

   // Unused Parameters
   HB_SYMBOL_UNUSED(cname)
   HB_SYMBOL_UNUSED(cpropmet)

   FOR i := 1 TO Len(aline)
      IF At("IMAGE", aline[i]) > 0
         npos1 := At("IMAGE", Upper(aline[i])) + 6
         npos2 := At("AT", Upper(aline[i])) - 1
         lin := i
         sw1 := 1
         EXIT
      ENDIF
   NEXT i
   IF sw1 == 1
      RETURN SubStr(aline[lin], npos1, npos2 - npos1 + 1)
   ENDIF

RETURN cDefault

STATIC FUNCTION leadatoh(cName, cPropmet, cDefault, npar)

   LOCAL i
   LOCAL sw1 := 0
   LOCAL lin := 0
   LOCAL npos1
   LOCAL npos2

   // Unused Parameters
   HB_SYMBOL_UNUSED(cname)
   HB_SYMBOL_UNUSED(cpropmet)

   FOR i := 1 TO Len(aline)
      IF At("HEADERS", aline[i]) > 0
         IF npar = 1
            npos1 := At(Upper("{"), Upper(aline[i]))
            npos2 := At(Upper("}"), Upper(aline[i]))
         ELSE
            npos1 := RAt(Upper("{"), Upper(aline[i]))
            npos2 := RAt(Upper("}"), Upper(aline[i]))
         ENDIF
         lin := i
         sw1 := 1
         EXIT
      ENDIF
   NEXT i
   IF sw1 == 1
      RETURN SubStr(aline[lin], npos1, npos2 - npos1 + 1)
   ENDIF

RETURN cDefault

STATIC FUNCTION leadatologic(cName, cPropmet, cDefault)

   LOCAL i
   LOCAL sw := 0

   FOR i := 1 TO Len(aline)
      IF At(Upper(cname) + " ", Upper(aline[i])) != 0
         sw := 1
      ELSE
         IF sw == 1
            IF At(Upper(cPropmet) + " ", Upper(aline[i])) > 0
               RETURN .T.
            ENDIF
            IF Len(Trim(aline[i])) == 0
               RETURN cDefault
            ENDIF
         ENDIF
      ENDIF
   NEXT i

RETURN cDefault

STATIC FUNCTION clean(cfvalue)
   cfvalue := StrTran(cfvalue, Chr(34), "")
   cfvalue := StrTran(cfvalue, "'", "")

RETURN cfvalue

STATIC FUNCTION learowi(cname, npar)

   LOCAL i
   LOCAL npos1
   LOCAL nrow := "0"

   // Unused Parameter
   HB_SYMBOL_UNUSED(cname)

   FOR i := 1 TO Len(aline)
      IF At("IMAGE ", Upper(aline[i])) != 0
         npos1 := At(iif(npar == 1,"AT","TO"), Upper(aline[i]))
         nrow := SubStr(aline[i], npos1 + 3, 4)
         EXIT
      ENDIF
   NEXT i

RETURN nrow

STATIC FUNCTION leacoli(cname, npar)

   LOCAL i
   LOCAL npos
   LOCAL ncol := "0"

   // Unused Parameter
   HB_SYMBOL_UNUSED(cname)

   FOR i := 1 TO Len(aline)
      IF At("IMAGE ", Upper(aline[i])) != 0
         npos := iif(npar == 1, At(",",aline[i]), RAt(",",aline[i]))
         ncol := SubStr(aline[i], npos + 1, 4)
         EXIT
      ENDIF
   NEXT i

RETURN ncol

STATIC PROCEDURE imp_SUBTOTALES(nlin, ncol, lmode, swt, grpby)

   LOCAL i
   LOCAL lHayTotals := ( AScan(atotals, .T.) > 0 )
   LOCAL cSubgrp := iif(!ISEVERYPAGE, aMessages[21], chdrgrp)

   ncol := nlmargin + 1
   IF grpby != NIL
      crompe := iif(ISEVERYPAGE, Str(nlin), crompe)
      wfield1 := iif(!ISEVERYPAGE, afieldsg[nposgrp], wfield1)
      IF !( &wfield1 == crompe ) .AND. !ISEVERYPAGE .OR. ( ISEVERYPAGE .AND. nlin >= nlpp )
         IF lmode
            IF lHayTotals
               @ nlin, 1 + nlmargin SAY cSubgrp font "f1" TO PRINT
               nlin++
               FOR i := 1 TO Len(afields)
                  IF atotals[i]
                     @ nlin, ncol SAY iif(!( aformats[i] == "" ), Transform(angrpby[i], aformats[i]), Str(angrpby[i], awidths[i])) font "f1" TO PRINT
                  ENDIF
                  ncol += awidths[i] + 1
               NEXT i
               nlin++
            ENDIF
         ELSE
            IF lHayTotals
               @ nlin, 1 + nlmargin SAY cSubgrp //'** Subtotal **'
               nlin++
               FOR i := 1 TO Len(afields)
                  IF atotals[i]
                     @ nlin, ncol SAY iif(!( aformats[i] == "" ), Transform(angrpby[i], aformats[i]), Str(angrpby[i], awidths[i]))
                  ENDIF
                  ncol += awidths[i] + 1
               NEXT i
               nlin++
            ENDIF
         ENDIF

         AFill(angrpby, 0)
         crompe := iif(!ISEVERYPAGE, &wfield1, Str(nlin))

         IF swt == 0 .AND. !ISEVERYPAGE
            IF lmode
               IF hmg_IsOemText(chdrgrp)
                  chdrgrp := hb_OEMToANSI(chdrgrp)
               ENDIF
               cgrpby := &grpby
               IF hmg_IsOemText(cgrpby)
                  cgrpby := hb_OEMToANSI(cgrpby)
               ENDIF
               @ nlin, 1 + nlmargin SAY "** " + chdrgrp + " ** " + iif(!ISEVERYPAGE, hb_ValToStr(cgrpby), "") font "f1" TO PRINT
            ELSE
               @ nlin, 1 + nlmargin SAY "** " + chdrgrp + " ** " + iif(!ISEVERYPAGE, hb_ValToStr(&grpby), "")
            ENDIF
            nlin++
         ENDIF
      ENDIF
   ENDIF

   ncol := nlmargin + 1
   IF nlin > nlpp
      nlin := ntoprow
      IF !ldos
         END PAGE
         START PAGE
      ENDIF
      nlin := headers(aheaders1, aheaders2, awidths, nlin, ctitle, lmode, grpby, chdrgrp)
   ENDIF

RETURN

STATIC PROCEDURE imp_pagina(nlin, lmode, grpby, chdrgrp)

   IF nlin > nlpp
      nlin := ntoprow
      IF !ldos
         END PAGE
         START PAGE
         IF cgraphic != NIL .AND. lmul .AND. !ldos
            IF !File(cgraphic)
               msgstop(aMessages[12], aMessages[13])
            ELSE
               @nfi, nci + nlmargin PICTURE cgraphic SIZE nff - nfi - 4, ncf - nci - 3
            ENDIF
         ENDIF
      ENDIF
      nlin := headers(aheaders1, aheaders2, awidths, nlin, ctitle, lmode, grpby, chdrgrp)
   ENDIF

RETURN

/********************************************************************************
// Internationalization
// P.D. : 01 July 2021
// (add your translation in your language, below)
********************************************************************************/
STATIC PROCEDURE InitReportMessages()
********************************************************************************

#ifdef _MULTILINGUAL_
   LOCAL cLang
#endif

   aMessages := {}

   AAdd(aMessages, "Please, decrease any of these (or both):") // 1
   AAdd(aMessages, "- the Number of Columns"                 ) // 2
   AAdd(aMessages, "- the Left Margin"                       ) // 3
   AAdd(aMessages, "or change orientation to LANDSCAPE."     ) // 4
   AAdd(aMessages, "Problem: The line exceeds page width!"   ) // 5

   AAdd(aMessages, "Page Size:"                              ) // 6
   AAdd(aMessages, "Font size = "                            ) // 7
   AAdd(aMessages, "Max. number of rows ="                   ) // 8
   AAdd(aMessages, "Number of rows set  ="                   ) // 9
   AAdd(aMessages, "Max. number of cols ="                   ) // 10
   AAdd(aMessages, "Number of cols set  ="                   ) // 11

   AAdd(aMessages, "The image file not found!"               ) // 12
   AAdd(aMessages, "Warning!"                                ) // 13

   AAdd(aMessages, "Print ?"                                 ) // 14
   AAdd(aMessages, "Question"                                ) // 15

   AAdd(aMessages, "cannot be found"                         ) // 16

   AAdd(aMessages, "Fields not defined"                      ) // 17
   AAdd(aMessages, "Widths not defined"                      ) // 18

   AAdd(aMessages, "Error!"                                  ) // 19
   AAdd(aMessages, "*** Total ***"                           ) // 20
   AAdd(aMessages, "** Subtotal **"                          ) // 21

#ifdef _MULTILINGUAL_

   cLang := Upper(Left(Set(_SET_LANGUAGE), 2))

   DO CASE
   CASE cLang == "EL"
      /////////////////////////////////////////////////////////////
      // GREEK - ��������
      /////////////////////////////////////////////////////////////
      aMessages[01] := "��������� ���� ��� �� �������� (� ���):"         // 1
      aMessages[02] := "- ��� ������ ������"                             // 2
      aMessages[03] := "- �� �������� ���������"                         // 3
      aMessages[04] := "� �������� LANDSCAPE �������������� �������."    // 4
      aMessages[05] := "��������: � ������ ��������� �� ������ �������!" // 5

      aMessages[06] := "������� �������:"                                // 6
      aMessages[07] := "������� �����/������ = "                         // 7
      aMessages[08] := "�������� ������� ������� ="                      // 8
      aMessages[09] := "������� ������� ��� ��������� ="                 // 9
      aMessages[10] := "�������� ������� ������ ="                       // 10
      aMessages[11] := "������� ������ ��� ��������� ="                  // 11

      aMessages[12] := "�� ������ ������� ��� �������!"                  // 12
      aMessages[13] := "�������!"                                        // 13

      aMessages[14] := "��������;"                                       // 14
      aMessages[15] := "�����������"                                     // 15

      aMessages[16] := "��� ������ �� ������"                            // 16

      aMessages[17] := "��� ��������� �����"                             // 17
      aMessages[18] := "��� �������� ������ �������"                     // 18

      aMessages[19] := "������!"                                         // 19

      aMessages[20] := "*** ������ ***"                                  // 20
      aMessages[21] := "** ��������� **"                                 // 21

   CASE  cLang == "PT"   // Portuguese
      /////////////////////////////////////////////////////////////
      // PORTUGUESE
      /////////////////////////////////////////////////////////////
      aMessages[01] := "Por favor, diminua qualquer um destes (ou ambos)"  // 1
      aMessages[02] := "- n�mero de colunas"                               // 2
      aMessages[03] := "- margem esquerda"                                 // 3
      aMessages[04] := "ou mude a orienta��o para PAISAGEM."               // 4
      aMessages[05] := "Problema: a linha excede a largura da p�gina !"    // 5

      aMessages[06] := "Tamanho da p�gina:"                                // 6
      aMessages[07] := "Tamanho da fonte = "                               // 7
      aMessages[08] := "M�x. n�mero de linhas ="                           // 8
      aMessages[09] := "N�mero de linhas definidas ="                      // 9
      aMessages[10] := "M�x. n�mero de colunas ="                          // 10
      aMessages[11] := "N�mero de colunas definidas ="                     // 11

      aMessages[12] := "O arquivo de imagem n�o foi encontrado !"          // 12
      aMessages[13] := "Aviso !"                                           // 13

      aMessages[14] := "Imprime ?"                                         // 14
      aMessages[15] := "Pergunta"                                          // 15

      aMessages[16] := "n�o pode ser encontrado"                           // 16

      aMessages[17] := "Campos n�o definidos"                              // 17
      aMessages[18] := "Larguras n�o definidas"                            // 18

      aMessages[19] := "Erro !"                                            // 19
      aMessages[20] := "*** Total ***"                                     // 20
      aMessages[21] := "** Subtotal **"                                    // 21

   CASE cLang == "IT" // TODO: aadd ou [nIndex]
      /////////////////////////////////////////////////////////////
      // ITALIAN
      ////////////////////////////////////////////////////////////
      AAdd(aMessages, "Si prega di diminuire qualsiasi di questi (o entrambi):") // 1
      AAdd(aMessages, "- il numero di colonne"                                 ) // 2
      AAdd(aMessages, "- il margine sinistro"                                  ) // 3
      AAdd(aMessages, "o cambiare orientamento in ORIZZONTALE."                ) // 4
      AAdd(aMessages, "Problema: la riga supera la larghezza della pagina!"    ) // 5

      AAdd(aMessages, "Dimensioni della pagina:"                               ) // 6
      AAdd(aMessages, "Dimensione del font = "                                 ) // 7
      AAdd(aMessages, "Max. numero di righe ="                                 ) // 8
      AAdd(aMessages, "Numero di righe impostate  ="                           ) // 9
      AAdd(aMessages, "Max. Numero di colonne ="                               ) // 10
      AAdd(aMessages, "Numero di colonne impostate ="                          ) // 11

      AAdd(aMessages, "Non trovo il file immagine!"                            ) // 12
      AAdd(aMessages, "Attenzione!"                                            ) // 13

      AAdd(aMessages, "Stampo ?"                                               ) // 14
      AAdd(aMessages, "Domanda"                                                ) // 15

      AAdd(aMessages, "non pu� essere trovato"                                 ) // 16

      AAdd(aMessages, "Campi non definiti"                                     ) // 17
      AAdd(aMessages, "Larghezze non definite"                                 ) // 18

      AAdd(aMessages, "Errore!"                                                ) // 19
      AAdd(aMessages, "*** Totale ***"                                         ) // 20
      AAdd(aMessages, "** Subtotale **"                                        ) // 21

   CASE cLang == "FR" // TODO: aadd ou [nIndex]
      /////////////////////////////////////////////////////////////
      // FRENCH
      ////////////////////////////////////////////////////////////
      AAdd(aMessages, "Veuillez diminuer l'un de ces �l�ments (ou les deux):") // 1
      AAdd(aMessages, "- Le nombre de colonnes"                              ) // 2
      AAdd(aMessages, "- La marge gauche"                                    ) // 3
      AAdd(aMessages, "o cambiare L'orientamento in ORIZZONTALE."            ) // 4
      AAdd(aMessages, "Probl�me: la ligne d�passe la largeur de la page!"    ) // 5

      AAdd(aMessages, "Taille de la page:"                                   ) // 6
      AAdd(aMessages, "Taille de police = "                                  ) // 7
      AAdd(aMessages, "Max. Nombre de lignes ="                              ) // 8
      AAdd(aMessages, "Nombre de lignes d�finies ="                          ) // 9
      AAdd(aMessages, "Max. Nombre de colonnes ="                            ) // 10
      AAdd(aMessages, "Nombre de colonnes d�finies ="                        ) // 11

      AAdd(aMessages, "Je ne trouve pas le fichier image!"                   ) // 12
      AAdd(aMessages, "Attention!"                                           ) // 13

      AAdd(aMessages, "Imprimer ?"                                           ) // 14
      AAdd(aMessages, "Demande"                                              ) // 15

      AAdd(aMessages, "ne peut �tre trouv�"                                  ) // 16

      AAdd(aMessages, "Champs non d�finis"                                   ) // 17
      AAdd(aMessages, "Largeurs non d�finies"                                ) // 18

      AAdd(aMessages, "Erreur!"                                              ) // 19
      AAdd(aMessages, "*** Le total ***"                                     ) // 20
      AAdd(aMessages, "** Sous-total **"                                     ) // 21

   ENDCASE

#endif

RETURN
