// Created By Pierpaolo Martinello Italy
// Part of this Program is made by Bicahi Esgici <esgici@gmail.com>

#include "minigui.ch"
#include "winprint.ch"
#include "miniprint.ch"
#include "hbclass.ch"

#TRANSLATE MSG	=> MSGBOX
#define NTrim(n) LTRIM(STR(n, 20, IIF(n == INT(n), 0, set(_SET_DECIMALS))))
#TRANSLATE ZAPS(<X>) => NTrim(<X>)
#TRANSLATE Test( <c> ) => MsgInfo( <c>, [<c>] )
#define MsgInfo( c ) MsgInfo( c, , , .F. )
#define MsgAlert( c ) MsgEXCLAMATION( c, , , .F. )
#define MsgStop( c ) MsgStop( c, , , .F. )

memvar endof_file, separator, atf
memvar An_Vari, aend
memvar Atutto, Anext, Aivarpt, ActIva, nomevar
memvar _aFnt,ritspl,abort
memvar last_pag
memvar string1
memvar start
memvar LStep
memvar Cstep
memvar pagina
memvar format
memvar _money
memvar _separator
memvar _euro
memvar atr
memvar vsect
memvar epar
memvar vpar
memvar chblk
memvar chkArg
memvar oneatleast, shd, sbt, sgh,insgh
memvar gcounter
memvar counter
memvar gcdemo
memvar grdemo

memvar align
memvar GHstring, GFstring, GTstring
memvar GField
memvar s_head, TTS
memvar s_col, t_col, wheregt
memvar gftotal, Gfexec, s_total
memvar nline
memvar nPag, nPgr, Tpg
memvar eLine, GFline
memvar maxrow, maxcol, mncl, mxH
memvar flob, mx_pg

memvar query_exp
memvar __arg1, __arg2
memvar xlwh,xfo,xfc,xbr,xpe,xwa,xbmp
memvar oWr
/*
*/
*------------------------------------------------------------------------------*
Function WinRepInt(filename,db_arc,_NREC,_MainArea,_psd,_prw)
*------------------------------------------------------------------------------*
local ritorna:=.T., handle, n, n_var, x_exact := set(1)
local str1:="", Vcb:="", lcnt:=0, a1:=0, a2:=0, al:=0, L1:=.F., L2:=.F.
local _object_ := "", Linea, sezione, cWord
private endof_file
public SEPARATOR := [/], atf := ""
default db_arc to dbf(), _nrec to 0, _MainArea to ""
default _prw to .F.

SET( _SET_DELETED , .T. )
SET CENTURY ON
// SET EPOCH TO Year(Date()) - 50

// init of object conversion
Public oWr   := WREPORT()
oWr:New()
oWr:argm     := {_MainArea,_psd,db_arc,_prw}
oWr:filename := filename

if valtype(_nrec)== "C"
   atf :=_nrec
   oWr:nrec := 1
else
   oWr:nrec := _nrec
endif

*- check for file's existence
if empty(filename) .OR. !file(filename)
   _object_ := valtype(filename)
   msgstop([Warning...]+CRLF+[Report not found ]+IF(_object_=="C",": "+Chr(34)+Filename+Chr(34),"!")+CRLF+[The type of argument is: ]+_object_,"")
   ritorna:=.F.
ElseIf !file(filename)
   MsgT(2,[Warning...]+CRLF+[The file ]+Chr(34)+FILENAME+Chr(34)+[ not exist!!!],,"STOP")
   ritorna:=.F.
Endif
if ritorna
   *- open the file, check for errors
   handle := FOPEN(filename,64)
   If Ferror() != 0
     msg("Error opening file : "+filename)
     ritorna := .F.
   Endif

   *- not at the end of file
   endof_file := .F.
endif
if ritorna
   Private An_Vari  :={}, aend  :={}
   Private Atutto   :={},Anext    :={},Aivarpt  :={},ActIva:={} , nomevar:={}
   Private _aFnt    :={}

   Private string1  := ""
   Private start    := ""
   _object_         := ""
   Private LStep    := 25.4 / 6          // 1/6 of inch
   Private Cstep    := 0
   //Private maxrow   := 0
   Private pagina   := 1
   Private Format   := {}
   Private _money   :=.F.
   Private _separator:=.T.           // Messo a vero per comodità sui conti
   Private _euro    := .T.           // Messo a vero per l'Europa
   Private atr      :=.T.

   // valore := {|x|val(substr(x[1],at("]",x[1])+1))}

   *- for < of lines allowed in box

   do while !endof_file
      Linea := oWr:fgetline(handle)
      if left(linea,1)=="["
         sezione := [A]+upper(substr(linea,2,AT("]", linea)-2))
         oWr:aStat[ "Define" ] := .F.
         oWr:aStat[ "Head" ]   := .F.
         oWr:aStat[ "Feet" ]   := .F.
         oWr:aStat[ "Body" ]   := .F.
         //aadd(&sezione,linea)

         DO case
            case sezione == "ADECLARE"
                 oWr:aStat[ "Define" ] := .T.

            case sezione == "AHEAD"
                 oWr:aStat[ "Head" ]   := .T.

            CASE sezione == "ABODY"
                 oWr:aStat[ "Body" ]   := .T.

            CASE sezione == "AFEET"
                 oWr:aStat[ "Feet" ]   := .T.

            CASE sezione == "AEND"
                 oWr:aStat[ "Head" ]   := .F.
                 oWr:aStat[ "Feet" ]   := .F.
                 oWr:aStat[ "Body" ]   := .F.

         EndCASE
      ElseIf left(linea,1) == "!"
          oWr:prndrv := if (upper(left(linea,10)) == "!MINIPRINT", "MINI", "HBPR")
      endif
      lcnt ++
      tokeninit(LINEA,";")       //set the command separator -> ONLY A COMMA /
      do While !tokenend()  //                             _____
         cWord := alltrim(tokennext(LINEA))
         //MSG(CWORD,[CWORD])
         _object_ := eval(oWr:aStat [ "TrSpace" ], CWORD, .T., lcnt)
         //msg(cWord+crlf+_object_,[linea ]+str(lcnt))
         if left(CWORD,1) != "#" .OR. left(CWORD,1) != "[" .AND. !empty(trim(_object_))
            if !empty(_object_)
               a1 := at("FONT", upper(_object_))
               if a1 > 0
                  _object_ := substr(_object_,1,a1-1)+upper(substr(_object_,a1))
               endif
               do case
                  CASE oWr:aStat[ "Define" ] == .T.
                       aadd(oWr:ADECLARE,{_object_,lcnt})

                  CASE oWr:aStat[ "Head" ] == .T.
                       aadd(oWr:aHead,{_object_,lcnt})

                  CASE oWr:aStat[ "Body" ] == .T.
                       aadd(oWr:ABody,{_object_,lcnt})

                  CASE oWr:aStat[ "Feet" ] == .T.
                       aadd(oWr:Afeet,{_object_,lcnt})
               endcase
            endif
         Endif
      ENDDO
    ENDDO
    release endof_file
    a1 := 0
    oWr:CountSect(.T.)
   // aeval(oWr:ahead,{|x,y|msg(x,[Ahead ]+zaps(y))})
   vsect  :={|x|{eval(oWr:Valore,oWr:aHead[1]),eval(oWr:Valore,oWr:aBody[1]),eval(oWr:Valore,oWr:aFeet[1]),nline,x}[at(x,"HBFL"+x)]}
   epar   :={|x|if( "(" $ X .OR."->" $ x,&(X),val(eval(vsect,x)))}

   vpar   :={|x,y|if(ascan(x,[y])!=0,y[ascan(x,[y])+1],NIL)}
   chblk  :={|x,y|if(ascan(x,y)>0,iif(len(X)>ascan(x,y),x[ascan(x,y)+1],""),"")}
   chkArg :={|x|if(ascan(x,{|aVal,y| aVal[1]== y})> 0 ,x[ascan(x,{|aVal,y| aVal[1]==y})][2],"KKK")}

   //msgbox( zaps(ascan(_aAlign,{|aVal,y| upper(aVal[1])== Y})),"FGFGFG")
   //ie:  eval(chblk,arrypar,[WIDTH]) RETURN A PARAMETER OF WIDTH

   FCLOSE(handle)

   str1:=upper(substr(oWr:Adeclare[1,1],at("/",oWr:Adeclare[1,1])+1))

   if "ASKP"  $ Str1
      IF msgyesno(" Print ?",[])
         ritorna := oWr:splash(oWr:aStat [ "lblsplash" ],if (oWr:PrnDrv = "HBPR","owr:doPr()","oWr:doMiniPr()") )
      endif
   else
        ritorna := oWr:splash(oWr:aStat [ "lblsplash" ],if (oWr:PrnDrv = "HBPR","owr:doPr()","oWr:doMiniPr()") )
   endif

   if "ASKR" $ Str1
      do while msgYesno("Reprint ?") == .T.
         ritorna := oWr:splash(oWr:aStat [ "lblsplash" ],if (oWr:PrnDrv = "HBPR","oWr:doPr()","oWr:doMiniPr()") )
      enddo
      filename := "" //release window all
   ENDIF

   SET( _SET_EXACT  , x_exact )
   release An_Vari,aend ,_aFnt,_Apaper,_abin ,_acharset,_aPen ,_aBrush ,_acolor
   release _aPoly ,_aBkmode ,_aRegion ,_aQlt,_aImgSty, Atutto ,Anext ,Aivarpt,ActIva
   release filtro ,string1,start, pagina ,_money, format, Atf, ritspl
   release _separator, _euro, atr, _t_font ,mx_ln_d,vsect ,epar,Vpar,chblk,chkArg
   release maxrow

   for n = 1 to len(nomevar)
       n_var:=nomevar[n]
       rele &n_var
   next
   rele nomevar,SEPARATOR
endif
return ritorna
/*
*/
*-----------------------------------------------------------------------------*
* Printing Procedure                //La Procedura di Stampa
*-----------------------------------------------------------------------------*
Function StampeEsegui(_MainArea,_psd,db_arc,_prw)
*-----------------------------------------------------------------------------*
   Local oldrec   := recno(), rtv := .F. ,;
         landscape:=.F., lpreview :=.F., lselect  :=.F. ,;
         str1:=[] , StrFlt := [], ;
         ncpl , nfsize, aprinters, ;
         lbody := 0, miocont:= 0, miocnt:= 0 ,;
         Amx_pg := {}

   Private ONEATLEAST := .F., shd := .T., sbt := .T., sgh := .T., insgh:=.F.
   if !empty(_MainArea)
       oWr:aStat [ "area1" ]  := substr(_MainArea,at("(",_MainArea)+1)
       oWr:aStat [ "FldRel" ] := substr(oWr:aStat [ "area1" ],at("->",oWr:aStat [ "area1" ])+2)
       oWr:aStat [ "FldRel" ] := substr(oWr:aStat [ "FldRel" ],1,iif(at(")",oWr:aStat [ "FldRel" ])>0,at(")",oWr:aStat [ "FldRel" ])-1,len(oWr:aStat [ "FldRel" ]))) //+(at("->",oWr:aStat [ "area1" ])))
       oWr:aStat [ "area1" ]  := left(oWr:aStat [ "area1" ],at("->",oWr:aStat [ "area1" ])-1)
   else
       oWr:aStat [ "area1" ]  := dbf()
       oWr:aStat [ "FldRel" ] :=""
   endif

   if oWr:PrnDrv = "HBPR"
      INIT PRINTSYS
      GET PRINTERS TO aprinters
   else
      aprinters := aprinters()
   endif

   Private counter   := {} , Gcounter := {}
   Private grdemo    := .F., gcdemo   := .F.
   Private Align     :=  0
   Private GHstring  := "", GFstring  := {}, GTstring := {}
   Private GField    := ""
   Private s_head    := "", TTS       := "Totale"

   Private s_col     :=  0, t_col     :=  0,  wheregt :=  0
   Private gftotal   := {}, Gfexec    := .F., s_total := ""
   Private nline     :=  mx_pg        := 0
   Private nPag      :=  0, nPgr      := 0, Tpg       :=  0
   Private last_pag  := .F., eLine    := 0, GFline    := .F.
   Public  maxrow    := 0 ,  maxcol   := 0,  mncl     := 0, mxH := 0
   Private abort     := 0

   ncpl := eval(oWr:Valore,oWr:Adeclare[1])
   str1 := upper(substr(oWr:Adeclare[1,1],at("/",oWr:Adeclare[1,1])+1))

   if "LAND" $ Str1 ;landscape:=.T.; endif
   if "SELE" $ Str1 ;lselect :=.T. ; endif
   if "PREV" $ Str1 ;lpreview:=.T. ; else;lpreview := _prw ; endif

   str1 := upper(substr(oWr:aBody[1,1],at("/",oWr:aBody[1,1])+1))
   flob := val(str1)

   if ncpl = 0
      ncpl   :=80
      nfsize :=12
   else
      do case
         case ncpl= 80
            nfsize:=12
         case ncpl= 96
            nfsize=10
         case ncpl= 120
            nfsize:=8
         case ncpl= 140
           nfsize:=7
         case ncpl= 160
           nfsize:=6
         otherwise
           nfsize:=12
      endcase
   endif
   if lselect .AND. lpreview
      hbprn:selectprinter("",.T.) // SELECT BY DIALOG PREVIEW
   endif
   if lselect .AND. (!lpreview)
      hbprn:selectprinter("",.T.) // SELECT BY DIALOG
   endif
   if !lselect .AND. lpreview
      if ascan(aprinters,_PSD) > 0
         hbprn:selectprinter(_PSD,.T.) // SELECT PRINTER _PSD PREVIEW
      else
         hbprn:selectprinter(NIL,.T.) // SELECT DEFAULT PREVIEW
      endif
   endif
   if !lselect .AND. !lpreview
      if ascan(aprinters,_PSD) > 0
        hbprn:selectprinter(_psd,.F.) // SELECT PRINTER _PSD
      else
        hbprn:selectprinter(NIL,.F.)   // SELECT default
      endif
   endif
   if HBPRNERROR != 0
      r_mem()
      return rtv
   endif
   DEFINE FONT "Fx" NAME "COURIER NEW" SIZE NFSIZE
   DEFINE FONT "F0" NAME "COURIER NEW" SIZE NFSIZE
   DEFINE FONT "F1" NAME "COURIER NEW" SIZE NFSIZE BOLD

   if landscape
      set page orientation DMORIENT_LANDSCAPE font "F0"
   else
      set page orientation DMORIENT_PORTRAIT  font "F0"
   endif

   select font "F0"
   select pen "P0"

   maxrow := int(HBPRN:DEVCAPS[1]/Lstep)
   //start doc
   aeval(oWr:adeclare,{|x,y|if(Y > 1 ,oWr:traduci(x[1],,x[2]),"")})
   if abort != 0
      r_mem()
      return nil
   endif
   if used()
      if !empty(atf)
         set filter to &atf
      endif
      oWr:aStat[ "end_pr" ] := oWr:quantirec( _mainarea )
   else
      oWr:aStat[ "end_pr" ] := oWr:quantirec( _mainarea )
   endif
   //msg(zaps(mx_pg)+CRLF+[oWr:Valore= ]+zaps(eval(oWr:Valore,oWr:aBody[1,1]))+CRLF+zaps(oWr:aStat[ "end_pr" ]),[tutte])

   START DOC NAME oWr:aStat [ "JobName" ]

   if empty(_MainArea)                // Mono Db List
      if lastrec() > 0 .OR. HB_ISARRAY(oWr:argm[3])
         Lbody := eval(oWr:Valore,oWr:aBody[1])
         mx_pg := INT(oWr:aStat[ "end_pr" ]/NOZERODIV(Lbody) )
         if (mx_pg * lbody) != mx_pg
             //  msgmulty({oWr:aStat[ "end_pr" ],lbody,mx_pg} )
             mx_pg ++
         endif
         mx_pg := ROUND( max(1,mx_pg), 0 )
         tpg   := mx_pg
         if valtype(oWr:argm[3]) != "A"
            Dbgotop()
         Endif
         if oWr:aStat [ "end_pr" ] != 0
            while !oWr:aStat [ "EndDoc" ]
                  oWr:TheHead()
                  oWr:TheBody()
            enddo
         Endif
      Else
         msgStop("No data to print! ","Attention")
      Endif
   else                              // Two Db List
      sele (oWr:aStat [ "area1" ])
      if !empty(atf)
         set filter to &atf
      endif
      Dbgotop()
      if lastrec()> 0
         lbody := eval(oWr:Valore,oWr:aBody[1])
         while !eof()
               sele (DB_ARC)
               StrFlt := oWr:aStat [ "FldRel" ]+" = "+ oWr:aStat [ "area1" ]+"->"+oWr:aStat [ "FldRel" ]
               DBEVAL( {|| miocont++},{|| &strFLT} )
               miocnt := int(miocont/NOZERODIV(lbody))
               if (miocnt * lbody) != miocont
                  miocnt ++
               endif
               tpg += miocnt
               aadd(Amx_pg,miocnt)
               miocont := 0
               sele (oWr:aStat [ "area1" ])
               dbskip()
         enddo
         go top
         if amx_pg[1] != 0
            while !eof()
                  sele (DB_ARC)
                  set filter to &strFLT
                  miocont ++
                  mx_pg  := aMx_pg[miocont]
                  go top
                  nPgr := 0
                  while !eof()
                        oWr:TheHead()
                        oWr:TheBody()
                  enddo
                  oWr:aStat [ "EndDoc" ]:=.F.
                  last_pag := .F.
                  set filter to
                  sele (oWr:aStat [ "area1" ])
                  dbskip()
            enddo
         Endif
      Else
         msgStop("No data to print! ","Attention")
      Endif
   Endif

   if oneatleast
      go top
      oWr:TheHead()
      oWr:TheFeet()
   endif
   end doc
   if len(oWr:aStat [ "ErrorLine" ]) > 0
      msgmulty(oWr:aStat [ "ErrorLine" ],"Error summary report:")
   Endif
   hbprn:setdevmode(256,1)
   if used();dbgoto(oldrec);endif
   R_mem(.T.)

Return !rtv
/*
*/
*-----------------------------------------------------------------------------*
Function R_mem(Last)
*-----------------------------------------------------------------------------*
   default last to .F.
   hbprn:end()
   if !last
      domethod("form_splash", "HIDE")
   Endif
   domethod("form_splash", "release")
   release miocont,counter,Gcounter,grdemo,gcdemo,Align,GField
   release s_head,s_col,gftotal,Gfexec,s_total,t_col,nline,nPag,nPgr,Tpg,last_pag,eLine,wheregt
   release GFline,mx_pg,maxrow,ONEATLEAST,shd,sbt,sgh,insgh,TTS, abort
return .F.
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION Proper(interm)             //Created By Piersoft 01/04/95 KILLED Bugs!
*-----------------------------------------------------------------------------*
   local outStr:="",capnxt:="",c_1
   do while chr(32) $ interm
      c_1:=substr(interm,1,at(chr(32),interm)-1)
      capnxt:=capnxt+upper(left(c_1,1))+right(c_1,len(c_1)-1)+" "
      interm:=substr(interm,len(c_1)+2,len(interm)-len(c_1))
   enddo
   outStr=capnxt+upper(left(interm,1))+right(interm,len(interm)-1)
RETURN outStr
/*
*/
*-----------------------------------------------------------------------------*
Function Color(GR,GR1,GR2)
*-----------------------------------------------------------------------------*
   LOCAL DATO
   if PCOUNT()=1 .AND. valtype(GR)=="C"
      if "," $ GR
         gr :=  STRTRAN(gr,"{","")
         gr :=  STRTRAN(gr,"}","")
         tokeninit(GR,",")
         IF oWr:PrnDrv = "HBPR"
            DATO := rgb( VAL(tokENNEXT(GR)),VAL(tokENNEXT(GR)),VAL(tokENNEXT(GR)) )
         else
            Dato := { VAL(tokENNEXT(GR)),VAL(tokENNEXT(GR)),VAL(tokENNEXT(GR)) }
         endif
      else
         dato := oWr:SetMyRgb(hbPrn:DXCOLORS(gr))
      Endif
   ELSEif PCOUNT()=1 .AND. HB_ISARRAY(GR)
         DATO := rgb(GR[1],GR[2],GR[3])
   elseIF PCOUNT()=3
      DATO := rgb(GR,GR1,GR2)
   endif

return DATO
/*
*/
*-----------------------------------------------------------------------------*
Function GROUP(GField, s_head, s_col, gftotal, wheregt, s_total, t_col, p_f_e_g)
*                1        2      3       4        5        6       7       8
*-----------------------------------------------------------------------------*
return oWr:GROUP(GField, s_head, s_col, gftotal, wheregt, s_total, t_col, p_f_e_g)
/*
*/
*-----------------------------------------------------------------------------*
Procedure gridRow(arg1)
*-----------------------------------------------------------------------------*
default arg1 to oWr:aStat ["r_paint"]
oWr:aStat ["r_paint"] := arg1
m->grdemo  := .T.
return
/*
*/
*-----------------------------------------------------------------------------*
Procedure gridCol(arg1)
*-----------------------------------------------------------------------------*
default arg1 to oWr:aStat ["r_paint"]
oWr:aStat ["r_paint"] := arg1
m->gcdemo := .T.
return
/*
*/
*-----------------------------------------------------------------------------*
static Funct AscArr(string)
*-----------------------------------------------------------------------------*
local aka :={}, cword := ""
default string to ""
      string := atrepl( "{",string,"")
      string := atrepl( "}",string,"")
      Tokeninit(string,",")
      do While !Tokenend()
         cWord  :=  Tokennext(String)
         aadd(aka,cword)
      enddo
return aka
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION ed_g_pic
*-----------------------------------------------------------------------------*
parameter __arg1,__arg2
local arg1, arg2
default __arg2 to .F.

if __arg2
   _euro:=.T.
endif

if Valtype(m->__arg1)="C"
   * MSG(VALtype(m->__arg1),[val1])
   *- make sure it fits on the screen
   arg1 := "@KS" + LTRIM(STR(MIN(LEN((m->__arg1)), 78)))
elseif HB_ISNUMERIC(m->__arg1)
   *- convert to a string
   * MSG(VALtype(m->__arg1),[val2])
   arg2 := STR(__arg1)
   *- look for a decimal point
   IF ("." $ arg2)
      *- return a picture reflecting a decimal point
      arg1 := REPLICATE("9", AT(".", arg2) - 1)+[.]

      arg1 := eu_point(arg1)+[.]+ REPLICATE("9", LEN(arg2) - LEN(arg1))
   ELSE
      *- return a string of 9's a the picture
      arg1 := REPLICATE("9", LEN(arg2))
      arg1 := eu_point(arg1)+if( _money,".00","")
   ENDIF
else
   *- well I just don't know.
   arg1 := ""
Endif

RETURN arg1
/*
*/
*-----------------------------------------------------------------------------*
Function eu_point(valore)
*-----------------------------------------------------------------------------*
local tappo:="",sep_conto:=0,n

For n = len(valore) to 1 step -1
    If substr(valore,n,1)==[9]
       if sep_conto = 3
          if _separator
             tappo := ","+tappo
          endif
          sep_conto := 0
       endif
       tappo := substr(valore,n,1)+TAPPO
       sep_conto ++
    Endif
Next
if _euro
   tappo:= "@E "+tappo
endif

return tappo
/*
*/
*-----------------------------------------------------------------------------*
Procedure dbselect(area)
*-----------------------------------------------------------------------------*
     if HB_ISNUMERIC(area)
        dbSelectArea( zaps(area) )
     elseif valtype(area)="C"
        select (area)
     endif
return
/*
*/
*-----------------------------------------------------------------------------*
Function Divisor(arg1,arg2)
*-----------------------------------------------------------------------------*
default arg2 to 1
return arg1/Nozerodiv(arg2)
/*
*/
*-----------------------------------------------------------------------------*
Function NoZeroDiv(nValue)
*-----------------------------------------------------------------------------*
return IIF(nValue=0,1,nValue)
/*
*/
*-----------------------------------------------------------------------------*
Function _dummy_( ... )
*-----------------------------------------------------------------------------*
return nil
/*
*/
*-----------------------------------------------------------------------------*
Procedure MsgMulty( xMesaj, cTitle ) // Created By Bicahi Esgici <esgici@gmail.com>
*-----------------------------------------------------------------------------*
   loca cMessage := ""

   IF xMesaj != NIL

      IF cTitle == NIL
         cTitle := PROCNAME(1) + "\" +   NTrim(PROCLINE(1))
      ENDIF

      IF VALTYPE(xMesaj) != "A"
         xMesaj := { xMesaj }
      ENDIF

      AEVAL( xMesaj, { | x1 | cMessage +=  Any2Strg( x1 ) + CRLF } )

      MsgInfo( cMessage, cTitle )

   ENDIF xMesaj != NIL

RETU
/*
*/
*-----------------------------------------------------------------------------*
FUNC Any2Strg( xAny )
*-----------------------------------------------------------------------------*
   loca cRVal  := "???",;
        nType  :=  0,;
        aCases := { { "A", { |  | "{...}" } },;
                    { "B", { |  | "{||}" } },;
                    { "C", { | x | x }},;
                    { "M", { | x | x   } },;
                    { "D", { | x | DTOC( x ) } },;
                    { "L", { | x | IIF( x,"On","Off") } },;
                    { "N", { | x | NTrim(x)  } },;
                    { "O", { |  | ":Object:" } },;
                    { "U", { |  | "<NIL>" } } }

   IF (nType := ASCAN(aCases, { | a1 | VALTYPE(xAny) == a1[1] }) ) > 0
      cRVal := EVAL( aCases[ nType, 2 ], xAny )
   ENDIF

RETU cRVal

/*
*/
*-----------------------------------------------------------------------------*
Function Msgt (nTimeout, Message, Title, Flags)
*-----------------------------------------------------------------------------*
* Created at 04/20/2005 By Pierpaolo Martinello Italy                         *
*-----------------------------------------------------------------------------*
        local switch:=.F., rtv:=0

        DEFAULT Message TO ""   ;  DEFAULT Title TO ""
        DEFAULT Flags   TO "MSGBOX"

        If ValType(nTimeout) != "U" .AND. ValType(nTimeout) = "C"
              Flags    :=  Title
              Title    :=  Message
              Message  :=  nTimeout
              switch   :=  .T.
        endif

        Flags:=UPPER(Flags)
        Message+= iif(empty(Message),"Empty string!","")

        if switch
           do case

              case "RETRYCANCEL" == FLAGS
                   rtv := MsgRetryCancel(Message,title)

              case "OKCANCEL" == FLAGS
                   rtv := MsgOkCancel(Message,Title)

              case "YESNO" == FLAGS
                   rtv := MsgYesNo(Message,Title)

              case "YESNO_ID" == FLAGS
                   rtv := MsgYesNo(Message,Title,.T.)

              case "INFO" == FLAGS
                   rtv := MsgInfo(Message,Title)

              case "STOP" == FLAGS
                   rtv := MsgStop(Message,Title)

              case "EXCLAMATION" == FLAGS
                   rtv := MsgExclamation(Message,Title)

              otherwise
                   MsgBox(Message,Title)
           endcase
        else
           do case

              case "RETRYCANCEL" == FLAGS
                   rtv := C_T_MSGRETRYCANCEL(Message,Title,nTimeout*1000)

              case "OKCANCEL" == FLAGS
                   rtv := C_T_MSGOKCANCEL(Message,Title,nTimeout*1000)

              case "YESNO" == FLAGS
                   rtv := C_T_MSGYESNO(Message,Title,nTimeout*1000)

              case "YESNO_ID" == FLAGS
                   rtv := C_T_MSGYESNO_ID(Message,Title,nTimeout*1000)

              case "INFO" == FLAGS
                   rtv := C_T_MSGINFO(Message,Title,nTimeout*1000)

              case "STOP" == FLAGS
                   rtv := C_T_MSGSTOP(Message,Title,nTimeout*1000)

              case "EXCLAMATION" == FLAGS
                   rtv := C_T_MSGEXCLAMATION(Message,Title,nTimeout*1000)

              otherwise
                   rtv := C_T_MSGBOX(Message,Title,nTimeout*1000)
           endcase
       endif
return rtv

/*
*/
*-----------------------------------------------------------------------------*
CREATE CLASS WREPORT
*-----------------------------------------------------------------------------*
DATA FILENAME         INIT ""
DATA NREC             INIT 0
DATA F_HANDLE         INIT 0 PROTECTED
DATA aDeclare         INIT {}
DATA AHead            INIT {}
DATA ABody            INIT {}
DATA AFeet            INIT {}
DATA Hb               INIT 0
DATA aCnt             INIT 0
DATA Valore           INIT {|x|val(substr(x[1],at("]",x[1])+1))}
DATA mx_ln_doc        INIT 0
DATA PRNDRV           INIT  "HBPR"
DATA argm             INIT {nil,nil, nil,nil}
DATA aStat            INIT { "Define"     => .F. , ;    // Define Section
                             "Head"       => .F. , ;    // Head Section
                             "Body"       => .F. , ;    // Body Section
                             "Feet"       => .F. , ;    // Feet section
                             "Filtro"     => .F. , ;
                             "r_paint"    => .T. , ;
                             "TempHead"   =>  "" , ;
                             "Ghead"      => .F. , ;
                             "P_F_E_G"    => .F. , ;
                             "GHline"     => .F. , ;
                             "TempFeet"   =>  "" , ;
                             "end_pr"     =>  0  , ;
                             "EndDoc"     => .F. , ;
                             "EntroIF"    => .F. , ;
                             "DelMode"    => .F. , ;
                             "ElseStat"   => .F. , ;
                             "ErrorLine"  =>  {} , ;
                             "OneError"   => .F. , ;
                             "area1"      =>  "" , ;
                             "FldRel"     =>  "" , ;
                             "ReadMemo"   =>  "" , ;
                             "lblsplash"  =>  "Attendere......... Creazione stampe!" , ;
                             "TrSpace"    => {|x,y,z|oWr:transpace(x,y,z)} , ;
                             "Yes_Memo"   => .F. , ;
                             "Yes_Array"  => .F. , ;
                             "JobName"    => "HbPrinter" , ;
                             "Test"       => "{|X|LTRIM(STR(X, 20, IIF(X == INT(X), 0, 2)))}" , ;
                             "Control"    => .F. , ;
                             "InlineSbt"  => .T. , ;
                             "InlineTot"  => .T. , ;
                             "Memofont"   => {}    ;
                             }

data Ach              INIT  {;
                            {"DMPAPER_FIRST",               1}; /*  */
                            ,{"DMPAPER_LETTER",              1}; /*   Letter 8 1/2 x 11 in               */
                            ,{"DMPAPER_LETTERSMALL",         2}; /*   Letter Small 8 1/2 x 11 in         */
                            ,{"DMPAPER_TABLOID",             3}; /*   Tabloid 11 x 17 in                 */
                            ,{"DMPAPER_LEDGER",              4}; /*   Ledger 17 x 11 in                  */
                            ,{"DMPAPER_LEGAL",               5}; /*   Legal 8 1/2 x 14 in                */
                            ,{"DMPAPER_STATEMENT",           6}; /*   Statement 5 1/2 x 8 1/2 in         */
                            ,{"DMPAPER_EXECUTIVE",           7}; /*   Executive 7 1/4 x 10 1/2 in        */
                            ,{"DMPAPER_A3",                  8}; /*   A3 297 x 420 mm                    */
                            ,{"DMPAPER_A4",                  9}; /*   A4 210 x 297 mm                    */
                            ,{"DMPAPER_A4SMALL",            10}; /*   A4 Small 210 x 297 mm              */
                            ,{"DMPAPER_A5",                 11}; /*   A5 148 x 210 mm                    */
                            ,{"DMPAPER_B4",                 12}; /*   B4 (JIS) 250 x 354                 */
                            ,{"DMPAPER_B5",                 13}; /*   B5 (JIS) 182 x 257 mm              */
                            ,{"DMPAPER_FOLIO",              14}; /*   Folio 8 1/2 x 13 in                */
                            ,{"DMPAPER_QUARTO",             15}; /*   Quarto 215 x 275 mm                */
                            ,{"DMPAPER_10X14",              16}; /*   10x14 in                           */
                            ,{"DMPAPER_11X17",              17}; /*   11x17 in                           */
                            ,{"DMPAPER_NOTE",               18}; /*   Note 8 1/2 x 11 in                 */
                            ,{"DMPAPER_ENV_9",              19}; /*   Envelope #9 3 7/8 x 8 7/8          */
                            ,{"DMPAPER_ENV_10",             20}; /*   Envelope #10 4 1/8 x 9 1/2         */
                            ,{"DMPAPER_ENV_11",             21}; /*   Envelope #11 4 1/2 x 10 3/8        */
                            ,{"DMPAPER_ENV_12",             22}; /*   Envelope #12 4 \276 x 11           */
                            ,{"DMPAPER_ENV_14",             23}; /*   Envelope #14 5 x 11 1/2            */
                            ,{"DMPAPER_CSHEET",             24}; /*   C size sheet                       */
                            ,{"DMPAPER_DSHEET",             25}; /*   D size sheet                       */
                            ,{"DMPAPER_ESHEET",             26}; /*   E size sheet                       */
                            ,{"DMPAPER_ENV_DL",             27}; /*   Envelope DL 110 x 220mm            */
                            ,{"DMPAPER_ENV_C5",             28}; /*   Envelope C5 162 x 229 mm           */
                            ,{"DMPAPER_ENV_C3",             29}; /*   Envelope C3  324 x 458 mm          */
                            ,{"DMPAPER_ENV_C4",             30}; /*   Envelope C4  229 x 324 mm          */
                            ,{"DMPAPER_ENV_C6",             31}; /*   Envelope C6  114 x 162 mm          */
                            ,{"DMPAPER_ENV_C65",            32}; /*   Envelope C65 114 x 229 mm          */
                            ,{"DMPAPER_ENV_B4",             33}; /*   Envelope B4  250 x 353 mm          */
                            ,{"DMPAPER_ENV_B5",             34}; /*   Envelope B5  176 x 250 mm          */
                            ,{"DMPAPER_ENV_B6",             35}; /*   Envelope B6  176 x 125 mm          */
                            ,{"DMPAPER_ENV_ITALY",          36}; /*   Envelope 110 x 230 mm              */
                            ,{"DMPAPER_ENV_MONARCH",        37}; /*   Envelope Monarch 3.875 x 7.5 in    */
                            ,{"DMPAPER_ENV_PERSONAL",       38}; /*   6 3/4 Envelope 3 5/8 x 6 1/2 in    */
                            ,{"DMPAPER_FANFOLD_US",         39}; /*   US Std Fanfold 14 7/8 x 11 in      */
                            ,{"DMPAPER_FANFOLD_STD_GERMAN", 40}; /*   German Std Fanfold 8 1/2 x 12 in   */
                            ,{"DMPAPER_FANFOLD_LGL_GERMAN", 41}; /*   German Legal Fanfold 8 1/2 x 13 in */
                            ,{"DMPAPER_ISO_B4",             42}; /*   B4 (ISO) 250 x 353 mm              */
                            ,{"DMPAPER_JAPANESE_POSTCARD",  43}; /*   Japanese Postcard 100 x 148 mm     */
                            ,{"DMPAPER_9X11",               44}; /*   9 x 11 in                          */
                            ,{"DMPAPER_10X11",              45}; /*   10 x 11 in                         */
                            ,{"DMPAPER_15X11",              46}; /*   15 x 11 in                         */
                            ,{"DMPAPER_ENV_INVITE",         47}; /*   Envelope Invite 220 x 220 mm       */
                            ,{"DMPAPER_RESERVED_48",        48}; /*   RESERVED--DO NOT USE               */
                            ,{"DMPAPER_RESERVED_49",        49}; /*   RESERVED--DO NOT USE               */
                            ,{"DMPAPER_LETTER_EXTRA",       50}; /*   Letter Extra 9 \275 x 12 in        */
                            ,{"DMPAPER_LEGAL_EXTRA",        51}; /*   Legal Extra 9 \275 x 15 in         */
                            ,{"DMPAPER_TABLOID_EXTRA",      52}; /*   Tabloid Extra 11.69 x 18 in        */
                            ,{"DMPAPER_A4_EXTRA",           53}; /*   A4 Extra 9.27 x 12.69 in           */
                            ,{"DMPAPER_LETTER_TRANSVERSE",  54}; /*   Letter Transverse 8 \275 x 11 in   */
                            ,{"DMPAPER_A4_TRANSVERSE",      55}; /*   A4 Transverse 210 x 297 mm         */
                            ,{"DMPAPER_LETTER_EXTRA_TRANSVERSE",56}; /* Letter Extra Transverse 9\275 x 12 in */
                            ,{"DMPAPER_A_PLUS",             57};   /* SuperA/SuperA/A4 227 x 356 mm      */
                            ,{"DMPAPER_B_PLUS",             58};   /* SuperB/SuperB/A3 305 x 487 mm      */
                            ,{"DMPAPER_LETTER_PLUS",        59};   /* Letter Plus 8.5 x 12.69 in         */
                            ,{"DMPAPER_A4_PLUS",            60};   /* A4 Plus 210 x 330 mm               */
                            ,{"DMPAPER_A5_TRANSVERSE",      61};   /* A5 Transverse 148 x 210 mm         */
                            ,{"DMPAPER_B5_TRANSVERSE",      62};   /* B5 (JIS) Transverse 182 x 257 mm   */
                            ,{"DMPAPER_A3_EXTRA",           63};   /* A3 Extra 322 x 445 mm              */
                            ,{"DMPAPER_A5_EXTRA",           64};   /* A5 Extra 174 x 235 mm              */
                            ,{"DMPAPER_B5_EXTRA",           65};   /* B5 (ISO) Extra 201 x 276 mm        */
                            ,{"DMPAPER_A2",                 66};   /* A2 420 x 594 mm                    */
                            ,{"DMPAPER_A3_TRANSVERSE",      67};   /* A3 Transverse 297 x 420 mm         */
                            ,{"DMPAPER_A3_EXTRA_TRANSVERSE",68};   /* A3 Extra Transverse 322 x 445 mm   */
                            ,{"DMPAPER_DBL_JAPANESE_POSTCARD",69};  /* Japanese Double Postcard 200 x 148 mm */
                            ,{"DMPAPER_A6",                  70 };  /*  A6 105 x 148 mm                 */
                            ,{"DMPAPER_JENV_KAKU2",          71 };  /*  Japanese Envelope Kaku #2       */
                            ,{"DMPAPER_JENV_KAKU3",          72 };  /*  Japanese Envelope Kaku #3       */
                            ,{"DMPAPER_JENV_CHOU3",          73 };  /*  Japanese Envelope Chou #3       */
                            ,{"DMPAPER_JENV_CHOU4",          74 };  /*  Japanese Envelope Chou #4       */
                            ,{"DMPAPER_LETTER_ROTATED",      75 };  /*  Letter Rotated 11 x 8 1/2 11 in */
                            ,{"DMPAPER_A3_ROTATED",          76 };  /*  A3 Rotated 420 x 297 mm         */
                            ,{"DMPAPER_A4_ROTATED",          77 };  /*  A4 Rotated 297 x 210 mm         */
                            ,{"DMPAPER_A5_ROTATED",          78 };  /*  A5 Rotated 210 x 148 mm         */
                            ,{"DMPAPER_B4_JIS_ROTATED",      79 };  /*  B4 (JIS) Rotated 364 x 257 mm   */
                            ,{"DMPAPER_B5_JIS_ROTATED",      80 };  /*  B5 (JIS) Rotated 257 x 182 mm   */
                            ,{"DMPAPER_JAPANESE_POSTCARD_ROTATED",81};    /*Japanese Postcard Rotated 148 x 100 mm */
                            ,{"DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED",82};/*double Japanese Postcard Rotated 148 x 200 mm */
                            ,{"DMPAPER_A6_ROTATED",          83 }; /*  A6 Rotated 148 x 105 mm         */
                            ,{"DMPAPER_JENV_KAKU2_ROTATED",  84 }; /*  Japanese Envelope Kaku #2 Rotated */
                            ,{"DMPAPER_JENV_KAKU3_ROTATED",  85 }; /*  Japanese Envelope Kaku #3 Rotated */
                            ,{"DMPAPER_JENV_CHOU3_ROTATED",  86 }; /*  Japanese Envelope Chou #3 Rotated */
                            ,{"DMPAPER_JENV_CHOU4_ROTATED",  87 }; /*  Japanese Envelope Chou #4 Rotated */
                            ,{"DMPAPER_B6_JIS",              88 }; /*  B6 (JIS) 128 x 182 mm           */
                            ,{"DMPAPER_B6_JIS_ROTATED",      89 }; /*  B6 (JIS) Rotated 182 x 128 mm   */
                            ,{"DMPAPER_12X11",               90 }; /*  12 x 11 in                      */
                            ,{"DMPAPER_JENV_YOU4",           91 }; /*  Japanese Envelope You #4        */
                            ,{"DMPAPER_JENV_YOU4_ROTATED",   92 }; /*  Japanese Envelope You #4 Rotated*/
                            ,{"DMPAPER_P16K",                93 }; /*  PRC 16K 146 x 215 mm            */
                            ,{"DMPAPER_P32K",                94 }; /*  PRC 32K 97 x 151 mm             */
                            ,{"DMPAPER_P32KBIG",             95 }; /*  PRC 32K(Big) 97 x 151 mm        */
                            ,{"DMPAPER_PENV_1",              96 }; /*  PRC Envelope #1 102 x 165 mm    */
                            ,{"DMPAPER_PENV_2",              97 }; /*  PRC Envelope #2 102 x 176 mm    */
                            ,{"DMPAPER_PENV_3",              98 }; /*  PRC Envelope #3 125 x 176 mm    */
                            ,{"DMPAPER_PENV_4",              99 }; /*  PRC Envelope #4 110 x 208 mm    */
                            ,{"DMPAPER_PENV_5",              100}; /*  PRC Envelope #5 110 x 220 mm    */
                            ,{"DMPAPER_PENV_6",              101}; /*  PRC Envelope #6 120 x 230 mm    */
                            ,{"DMPAPER_PENV_7",              102}; /*  PRC Envelope #7 160 x 230 mm    */
                            ,{"DMPAPER_PENV_8",              103}; /*  PRC Envelope #8 120 x 309 mm    */
                            ,{"DMPAPER_PENV_9",              104}; /*  PRC Envelope #9 229 x 324 mm    */
                            ,{"DMPAPER_PENV_10",             105}; /*  PRC Envelope #10 324 x 458 mm   */
                            ,{"DMPAPER_P16K_ROTATED",        106}; /*  PRC 16K Rotated                 */
                            ,{"DMPAPER_P32K_ROTATED",        107}; /*  PRC 32K Rotated                 */
                            ,{"DMPAPER_P32KBIG_ROTATED",     108}; /*  PRC 32K(Big) Rotated            */
                            ,{"DMPAPER_PENV_1_ROTATED",      109}; /*  PRC Envelope #1 Rotated 165 x 102 mm */
                            ,{"DMPAPER_PENV_2_ROTATED",      110}; /*  PRC Envelope #2 Rotated 176 x 102 mm */
                            ,{"DMPAPER_PENV_3_ROTATED",      111}; /*  PRC Envelope #3 Rotated 176 x 125 mm */
                            ,{"DMPAPER_PENV_4_ROTATED",      112}; /*  PRC Envelope #4 Rotated 208 x 110 mm */
                            ,{"DMPAPER_PENV_5_ROTATED",      113}; /*  PRC Envelope #5 Rotated 220 x 110 mm */
                            ,{"DMPAPER_PENV_6_ROTATED",      114}; /*  PRC Envelope #6 Rotated 230 x 120 mm */
                            ,{"DMPAPER_PENV_7_ROTATED",      115}; /*  PRC Envelope #7 Rotated 230 x 160 mm */
                            ,{"DMPAPER_PENV_8_ROTATED",      116}; /*  PRC Envelope #8 Rotated 309 x 120 mm */
                            ,{"DMPAPER_PENV_9_ROTATED",      117}; /*  PRC Envelope #9 Rotated 324 x 229 mm */
                            ,{"DMPAPER_PENV_10_ROTATED",     118}; /*  PRC Envelope #10 Rotated 458 x 324 mm */
                            ,{"DMPAPER_USER",                256};
                            ,{"DMBIN_FIRST",          1};   /* bin selections */
                            ,{"DMBIN_UPPER",          1};
                            ,{"DMBIN_ONLYONE",        1};
                            ,{"DMBIN_LOWER",          2};
                            ,{"DMBIN_MIDDLE",         3};
                            ,{"DMBIN_MANUAL",         4};
                            ,{"DMBIN_ENVELOPE",       5};
                            ,{"DMBIN_ENVMANUAL",      6};
                            ,{"DMBIN_AUTO",           7};
                            ,{"DMBIN_TRACTOR",        8};
                            ,{"DMBIN_SMALLFMT",       9};
                            ,{"DMBIN_LARGEFMT",      10};
                            ,{"DMBIN_LARGECAPACITY", 11};
                            ,{"DMBIN_CASSETTE",      14};
                            ,{"DMBIN_FORMSOURCE",    15};
                            ,{"DMBIN_LAST",          15};
                            ,{"DMBIN_USER",         256};     /* device specific bins start here */
                            ,{"ANSI_CHARSET",              0};  /*  _acharset :={; */
                            ,{"DEFAULT_CHARSET",           1};
                            ,{"SYMBOL_CHARSET",            2};
                            ,{"SHIFTJIS_CHARSET",        128};
                            ,{"HANGEUL_CHARSET",         129};
                            ,{"HANGUL_CHARSET",          129};
                            ,{"GB2312_CHARSET",          134};
                            ,{"CHINESEBIG5_CHARSET",     136};
                            ,{"OEM_CHARSET",             255};
                            ,{"JOHAB_CHARSET",           130};
                            ,{"HEBREW_CHARSET",          177};
                            ,{"ARABIC_CHARSET",          178};
                            ,{"GREEK_CHARSET",           161};
                            ,{"TURKISH_CHARSET",         162};
                            ,{"VIETNAMESE_CHARSET",      163};
                            ,{"THAI_CHARSET",            222};
                            ,{"EASTEUROPE_CHARSET",      238};
                            ,{"RUSSIAN_CHARSET",         204};
                            ,{"MAC_CHARSET",              77};
                            ,{"BALTIC_CHARSET",          186};
                            ,{"PS_SOLID",            0};       /* Pen Styles */
                            ,{"PS_DASH",             1};       /* -------  */
                            ,{"PS_DOT",              2};       /* .......  */
                            ,{"PS_DASHDOT",          3};       /* _._._._  */
                            ,{"PS_DASHDOTDOT",       4};       /* _.._.._  */
                            ,{"PS_NULL",             5};
                            ,{"PS_INSIDEFRAME",      6};
                            ,{"PS_USERSTYLE",        7};
                            ,{"PS_ALTERNATE",        8};
                            ,{"PS_STYLE_MASK",       0x0000000F};
                            ,{"BS_SOLID",            0};       /* Brush Styles */
                            ,{"BS_NULL",             1};
                            ,{"BS_HOLLOW",           1};
                            ,{"BS_HATCHED",          2};
                            ,{"BS_PATTERN",          3};
                            ,{"BS_INDEXED",          4};
                            ,{"BS_DIBPATTERN",       5};
                            ,{"BS_DIBPATTERNPT",     6};
                            ,{"BS_PATTERN8X8",       7};
                            ,{"BS_DIBPATTERN8X8",    8};
                            ,{"BS_MONOPATTERN",      9};
                            ,{"ALTERNATE",            1}; /* PolyFill() Modes */
                            ,{"WINDING",              2};
                            ,{"POLYFILL_LAST",        2};
                            ,{"TRANSPARENT",         1}; /* Background Modes */
                            ,{"OPAQUE",              2};
                            ,{"BKMODE_LAST",         2};
                            ,{"TA_NOUPDATECP",       0}; /* Text Alignment Options */
                            ,{"TA_UPDATECP",         1};
                            ,{"TA_LEFT",             0};
                            ,{"TA_RIGHT",            2};
                            ,{"TA_CENTER",           6};
                            ,{"LEFT",                0};
                            ,{"RIGHT",               2};
                            ,{"CENTER",              6};
                            ,{"TA_TOP",              0};
                            ,{"TA_BOTTOM",           8};
                            ,{"TA_BASELINE",         24};
                            ,{"TA_RTLREADING",       256};
                            ,{"TA_MASK",       (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)};
                            ,{"RGN_AND",             1};  /* CombineRgn() Styles */
                            ,{"RGN_OR",              2};
                            ,{"RGN_XOR",             3};
                            ,{"RGN_DIFF",            4};
                            ,{"RGN_COPY",            5};
                            ,{"RGN_MIN",       RGN_AND};
                            ,{"RGN_MAX",      RGN_COPY};
                            ,{"AND",                 1};
                            ,{"OR",                  2};
                            ,{"XOR",                 3};
                            ,{"DIFF",                4};
                            ,{"COPY",                5};
                            ,{"MIN",           RGN_AND};
                            ,{"MAX",          RGN_COPY};
                            ,{"DMCOLOR_MONOCHROME", 1}; /* color enable/disable for color printers */
                            ,{"DMCOLOR_COLOR",      2};
                            ,{"MONO",               1};
                            ,{"COLOR",              2};
                            ,{"DMRES_DRAFT",    -1};  /* print qualities */
                            ,{"DMRES_LOW",      -2};
                            ,{"DMRES_MEDIUM",   -3};
                            ,{"DMRES_HIGH",     -4};
                            ,{"DRAFT",          -1};
                            ,{"LOW",            -2};
                            ,{"MEDIUM",         -3};
                            ,{"HIGH",           -4};
                            ,{"ILD_NORMAL",     0x0000}; /* IMAGELIST DRAWING STYLES */
                            ,{"ILD_MASK",       0x0010};
                            ,{"ILD_BLEND25",    0x0002};
                            ,{"ILD_BLEND50",    0x0004};
                            ,{"DMDUP_SIMPLEX"   ,1};   /* duplex enable */
                            ,{"DMDUP_VERTICAL"  ,2};
                            ,{"DMDUP_HORIZONTAL",3};
                            ,{"OFF"             ,1};
                            ,{"SIMPLEX"         ,1};
                            ,{"VERTICAL"        ,2};
                            ,{"HORIZONTAL"      ,3};
                            ,{"DT_TOP"                 , 0x00000000};
                            ,{"DT_LEFT"                , 0x00000000};
                            ,{"DT_CENTER"              , 0x00000001};
                            ,{"DT_RIGHT"               , 0x00000002};
                            ,{"DT_VCENTER"             , 0x00000004};
                            ,{"DT_BOTTOM"              , 0x00000008};
                            ,{"DT_WORDBREAK"           , 0x00000010};
                            ,{"DT_SINGLELINE"          , 0x00000020};
                            ,{"DT_EXPANDTABS"          , 0x00000040};
                            ,{"DT_TABSTOP"             , 0x00000080};
                            ,{"DT_NOCLIP"              , 0x00000100};
                            ,{"DT_EXTERNALLEADING"     , 0x00000200};
                            ,{"DT_CALCRECT"            , 0x00000400};
                            ,{"DT_NOPREFIX"            , 0x00000800};
                            ,{"DT_INTERNAL"            , 0x00001000};
                            ,{"DT_EDITCONTROL"         , 0x00002000};
                            ,{"DT_PATH_ELLIPSIS"       , 0x00004000};
                            ,{"DT_END_ELLIPSIS"        , 0x00008000};
                            ,{"DT_MODIFYSTRING"        , 0x00010000};
                            ,{"DT_RTLREADING"          , 0x00020000};
                            ,{"DT_WORD_ELLIPSIS"       , 0x00040000};
                            ,{"DT_NOFULLWIDTHCHARBREAK", 0x00080000};
                            ,{"DT_HIDEPREFIX"          , 0x00100000};
                            ,{"DT_PREFIXONLY"          , 0x00200000}}  PROTECTED

METHOD New ()  CONSTRUCTOR
METHOD ISMONO()
METHOD SPLASH ()
METHOD DoPr ()
METHOD DoMiniPr ()
METHOD fGetline ()
METHOD Transpace ()
METHOD MACROCOMPILE ()
METHOD TRADUCI ()
METHOD LEGGIPAR ()
METHOD WHAT_ELE ()
METHOD MEMOSAY ()
METHOD PUTARRAY(row,col,arr,awidths,rowheight,vertalign,noframes,abrushes,apens,afonts,afontscolor,abitmaps,userfun)
METHOD HATCH ()
METHOD GROUP ()
METHOD GrHead ()
METHOD GFeet ()
METHOD UsaFont ()
METHOD Hgconvert ()
METHOD TheHead ()
METHOD TheBody ()
METHOD TheFeet ()
METHOD UsaColor ()
METHOD SETMYRGB ()
METHOD QUANTIREC ()
METHOD COUNTSECT ()
METHOD TheMiniHead ()
METHOD TheMiniBody ()
METHOD JUSTIFICALINEA ()
/*
METHOD SaveData()
*/
METHOD END()
*/
ENDCLASS
/*
*/
*-----------------------------------------------------------------------------*
METHOD New() CLASS WREPORT
*-----------------------------------------------------------------------------*
return self
/*
*/
*-----------------------------------------------------------------------------*
METHOD End() CLASS WREPORT
*-----------------------------------------------------------------------------*
release ::F_HANDLE,::aDeclare,::AHead,::ABody,::AFeet,::Hb,::Valore,::mx_ln_doc;
,       ::PRNDRV,::argm,::aStat
RELEASE ::ach , ::filename
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD IsMono(arg1) CLASS WREPORT
*-----------------------------------------------------------------------------*
Local en, rtv := .F.
for each en in arg1
    if HB_ISARRAY(en)
       exit
    Else
       rtv := .T.
       Exit
    Endif
next
return rtv
/*
*/
*-----------------------------------------------------------------------------*
METHOD COUNTSECT(EXEC) CLASS WREPORT
*-----------------------------------------------------------------------------*
DEFAULT EXEC TO .F.
    IF EXEC
       ::HB := eval(::Valore,::aHead[1])+ eval(::Valore,::aBody[1])
       ::mx_ln_doc := ::hb + eval(::Valore,::aFeet[1])
    ENDIF
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD Splash(etichetta,prc_init,sezione,rit) CLASS WREPORT
*-----------------------------------------------------------------------------*
   Local rtv ,cbWork :={|x| x }; private ritspl
   default etichetta to ::aStat[ "lblsplash" ], sezione to ""
   default prc_init to "_dummy_("+sezione+")"
   default rit to .F.
   ritspl := rit
   if _IsWIndowDefined("Form_splash")
      Setproperty ("FORM_SPLASH","Label_1","VALUE", ::aStat [ "lblsplash" ] )
      domethod("FORM_SPLASH", "SHOW")
      if ("doPr" $ prc_init,::doPr(),::doMiniPr())
      DOMETHOD("FORM_SPLASH", "RELEASE")
      return nil
   Endif
   if empty(etichetta)
      DEFINE WINDOW FORM_SPLASH AT 140 , 235 WIDTH 0 HEIGHT 0 MODAL NOSHOW NOSIZE NOSYSMENU NOCAPTION ;
      ON INIT if ("doPr" $ prc_init,::doPr(),::doMiniPr())
   else
      DEFINE WINDOW FORM_SPLASH AT 140 , 235 WIDTH 550 HEIGHT 240 MODAL NOSIZE NOCAPTION ;
      ON INIT if ("doPr" $ prc_init,::doPr(),::doMiniPr())
      DRAW RECTANGLE IN WINDOW Form_splash AT 2,2 TO 235, 548
   Endif
   DEFINE LABEL Label_1
          ROW    50
          COL    30
          WIDTH  480
          HEIGHT 122
          VALUE etichetta
          FONTNAME "Times New Roman"
          FONTSIZE 36
          FONTBOLD .T.
          FONTCOLOR {255,0,0}
          CENTERALIGN .T.
   END LABEL

END WINDOW

   center window Form_Splash
   activate window Form_Splash //NOWAIT
   rtv := ritspl
   release ritspl
return rtv
/*
*/
*-----------------------------------------------------------------------------*
METHOD DoPr() CLASS WREPORT
*-----------------------------------------------------------------------------*
*   ::argm:={_MainArea,_psd,db_arc,_prw}
*   stampeEsegui(_MainArea,_psd,db_arc,_prw)
    CursorWait()
    ritspl := stampeEsegui(::argm[1],::argm[2],::argm[3],::argm[4])
    CursorArrow()
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD DoMiniPr() CLASS WREPORT
*-----------------------------------------------------------------------------*
   CursorWait()
   ritspl := PrminiEsegui(::argm[1],::argm[2],::argm[3],::argm[4])
   CursorArrow()
return nil

/*
*/
*-----------------------------------------------------------------------------*
METHOD FgetLine(handle)  CLASS WREPORT
*-----------------------------------------------------------------------------*
local rt_line := "", chunk := "", bigchunk := "", at_chr13 :=0 , oldoffset := 0

        oldoffset := FSEEK(handle,0,1)
        DO WHILE .T.

          *- read in a chunk of the file
          chunk := ""
          chunk := Freadstr(handle,100)

          *- if we didn't read anything in, guess we're at the EOF
          IF LEN(chunk)=0
             endof_file := .T.

             IF !EMPTY(bigchunk)
               rt_line := bigchunk
             ENDIF
             EXIT
          elseif len(bigchunk) > 1024
             EXIT
          ENDIF

          *- add this chunk to the big chunk
          bigchunk := bigchunk+chunk

          *- if we've got a CR , we've read in a line
          *- otherwise we'll loop again and read in another chunk
          IF AT(CHR(13),bigchunk) > 0
             at_chr13 := AT(CHR(13),bigchunk)

             *- go back to beginning of line
             FSEEK(handle,oldoffset)

             *- read in from here to next CR (-1)
             rt_line := Freadstr(handle,at_chr13-1)

             *- move the pointer 1 byte
             FSEEK(handle,1,1)

             EXIT
          ENDIF
        ENDDO

        *- move the pointer 1 byte
        *- this should put us at the beginning of the next line
        FSEEK(handle,1,1)

RETURN rt_line

/*
*/
*-----------------------------------------------------------------------------*
METHOD Transpace(arg1,arg2,arg3) CLASS WREPORT // The core of parser
*-----------------------------------------------------------------------------*
     local al1 := .F., al2 := .F., extFnc := .F. , tmpstr := "" , n
     local nr  := "", opp := 0 , pt := "", cdbl := .F., cdc := 0
     local last_func  := rat(")",arg1), last_sapex := rat("'",arg1)
     local last_Dapex := rat(["],arg1), last_codeb := rat([}],arg1)
     Local arges := "" ;
         , aFsrc :={"SELECT"+CHR(7)+"FONT","DRAW"+CHR(7)+"TEXT","TEXTOUT"+CHR(7),"SAY"+CHR(7);
                   ,"PRINT" +CHR(7),"GET"+CHR(7)+"TEXT","DEFINE"+CHR(7)+"FONT"}
     Static xcl := .F.
     default arg2 to .T.
     arg1 := alltrim(arg1)
     arges := arg1
     // (#*&/) char exclusion
     if left(arges, 1) = chr(35) .OR. left(arges,1) = chr(38); arges := "" ;endif
     if left(arges, 2) = chr(47)+chr(47) ;arges := "" ;endif
     if left(arges, 2) = chr(47)+chr(42) ; xcl := .T. ;endif
     if right(arges, 2) = chr(42)+chr(47) ; xcl := .F. ;endif
     if left(arges, 1) = chr(42) .OR. empty(arges) .OR. xcl
        return ""
     endif
     if "SET SPLASH TO" $ arg1
        ::aStat [ "lblsplash" ] := substr(arg1,at("TO",arg1)+2)
        // msgbox("|"+::aStat [ "lblsplash" ]+"|" ,"Arges")
        return ""
     endif
     for n := 1 to len(arg1)
         pt := substr(arg1,n,1)
         if pt != chr(32)
            tmpstr := pt
            nr += pt
            if tmpstr == chr(40) //.OR. upper(substr(arg1,2,3)) = [VAR]  // (=chr(40)
               opp ++
               extFnc := .T.     // Interno a Funzione
            endif
            if tmpstr == chr(41) // ")"
               opp --
               extFnc := .F.    // Fine Funzione
            endif
            if tmpstr == Chr(34) .OR. tmpstr == "[" .OR. tmpstr == "'" .OR. tmpstr == [{]
               al1 := !al1
               if tmpstr == "{"
                  cdc ++
               endif
            endif
            if tmpstr == "]" .OR. n = last_Dapex .OR. n = last_sapex .OR. n = last_codeb  .OR. tmpstr == [}]
               al1 := .F.
               if tmpstr == [}]
                  cdc --
               endif
            endif
            if n >= last_func
               extFnc := .F.
               al2 := .F.
            endif
            if tmpstr = "|" .AND. cdc > 0
               extfnc := .T.
            elseif cdc < 1
               extfnc := .F.
            endif
            if Pt == "," .AND. extFnc == .F. .AND. al1 == .F. .AND. opp < 1
               nr := substr(nr,1,len(nr)-1) + chr(07)
            endif
         else
            if extFnc == .F.        //esterno a funzione
               if al1 == .F.
                  if opp < 1
                     nr += IIF(al2," ",chr(07)) //"/")
                  endif
               else
                  nr += pt
               endif
            else
               nr += pt
            ENDIF
         endif
     next
     nr := strtran(nr,chr(07)+chr(07),chr(07))
     tmpstr = left(ltrim(nr),1)
     if tmpstr = chr(60) .OR. tmpstr = "@" .OR. tmpstr = chr(07) //"<" ex "{"
        nr := substr(nr,2)
     endif
     if left(nr,1) = chr(07) .OR. left(nr,1) = chr(64)
        nr := substr(nr,2)
     endif
     if right(nr,1)=chr(62) //">" ex "}"
        nr := substr(nr,1,rat(">",nr)-1)
     endif
     if ")" == alltrim(nR) .OR. "(" == alltrim(nR)
        nr := ""
     endif
     nr := STRTRAN(nr,chr(07)+chr(07),chr(07))
     if arg2
        arg1 := upper(nr)
        aeval(aFsrc,{|x|if ( at(x,arg1) > 0, aadd(_aFnt,{upper(Nr),arg3}), Nil ) } )
     Endif

return nr
/*
*/
*-----------------------------------------------------------------------------*
METHOD MACROCOMPILE(cStr, lMesg,cmdline,section) CLASS WREPORT
*-----------------------------------------------------------------------------*
local bOld,xResult, dbgstr:="", lvl:= 0
default cmdline to 0, section to ""
if lMesg == NIL
  return &(cStr)
endif
bOld := ErrorBlock({|| break(NIL)})
BEGIN SEQUENCE
xResult := &(cStr)
RECOVER
if lMesg
    //msgBox(alltrim(cStr),"Error in evaluation of:")
    errorblock (bOld)
    if ::aStat [ "Control" ]
       MsgMiniGuiError("Program Report Interpreter" + CRLF + "Section " + section + CRLF + "I have found error on line " + ;
          zaps(cmdline) + CRLF + "Error is in: " + alltrim(cStr) + CRLF + "Please revise it!", "MiniGUI Error")
       Break
    else
       do case
          case SECTION = "STAMPEESEGUI"
               SECTION :="DECLARE : "
          case SECTION = "PORT:THEBODY"
               SECTION :="BODY       : "
          case SECTION = "PORT:THEMINIBODY"
               SECTION :="BODY       : "
          case SECTION = "WREPORT_THEHEAD"
               SECTION :="HEAD       : "
          case SECTION = "WREPORT_THEFEET"
               SECTION :="FEET         : "
       endcase

       dbgstr := section+zaps(cmdline)+" With: "+cStr
       aeval(::aStat[ "ErrorLine" ],{|x|if (dbgstr == x,  lvl:=1 ,"")} )
       if lvl < 1 .AND. cmdline > 0 //# ::aStat [ "ErrorLine" ]
          MSGSTOP(dbgstr,"MiniGui Extended Report Interpreter Error")
          aadd(::aStat [ "ErrorLine" ] , dbgstr )
       Endif
       ::aStat [ "OneError" ]  := .T.
       break
    Endif
endif
xResult := "**Error**:"+cStr
END SEQUENCE
errorblock (bOld)
return xResult
/*
*/
*-----------------------------------------------------------------------------*
METHOD Traduci(elemento,ctrl,cmdline) CLASS WREPORT  // The interpreter
*-----------------------------------------------------------------------------*
local string, ritorno :=.F., ev1th, sSection, dbg:=""
local TransPar:={}, ArryPar :={}, cWord
LOCAL oErrAntes, oErr, lMyError := .F., ifc:="",IEXE := .F.

sSection := iif(procname(1)="STAMPEESEGUI","DECLARE",substr(procname(1),4))
default ctrl to .F.
string:=alltrim(elemento)

if empty(string);return ritorno ;endif

if upper(left(string,8))="DEBUG_ON"
   ::aStat [ "Control" ] := .T.
elseif upper(left(string,8))="DEBUG_OF"
   ::aStat [ "Control" ] := .F.
elseif upper(left(string,9))=="SET"+chr(07)+"DEBUG"
   dbg := right(string,4)
   ::aStat [ "Control" ] := iif(val(dbg)> 0,.T.,iif(".T." $ dbg .OR. "ON" $ Dbg ,.T.,.F.))
endif

tokeninit(string,chr(07))      //set the command separator -> ONLY A BEL
do While !tokENEND()      //                             ----
   cWord  :=  tokENNEXT(String)
   if left(cword,1)="[" .AND. right(cword,1) != "]"
      cword :=substr(cword,2)+" "+tokENNEXT(String)
      do while .T.
         if right(cword,1)="]"
            cword:=substr(cword,1,len(cword)-1)
            cword:=strtran(cword,chr(4),"/")
            aadd(TransPar,cWord)
            exit
         else
            cword += " "+tokENNEXT(String)
         endif
      enddo
   elseif left(cword,1)="[" .AND. "]" $ cWord
          cWord:=substr(cword,at("[",cWord)+1,rat("]",cword)-2)
          cword:=strtran(cword,chr(4),"/")
          aadd(TransPar,cWord)
   else
       if "[" $ cWord .OR. ["] $ cWord .OR. ['] $ cWord
          cword:=strtran(cword,chr(4),"/")
          aadd(TransPar,cWord)
       else
          cword:=strtran(cword,chr(4),"/")
          aadd(TransPar,upper(cWord))
       endif
   endif
ENDDO
if "{" $ left(TransPar[1],2)
   ev1th := alltrim(substr(TransPar[1],at("||",TransPar[1])+2,at("}",Transpar[1])-4))
   if empty(ev1th)
      MsgMiniGuiError("Program Report Interpreter" + CRLF + "Section: " + procname(1) + ;
         " command n° " + zaps(cmdline) + CRLF + "Program terminated", "MiniGUI Error")
   endif
   do case
      case ev1th = ".T."
          adel(TransPar,1)

      case ev1th = ".F."
          adel(TransPar,1)
          return ritorno
   otherwise

      if eval(epar,ev1th)
         adel(TransPar,1)
         ritorno := .F.
      else
         if sSEction=="HEAD"
            nline ++
         endif
         ritorno := .T.
      endif
   endcase
endif
ifc := alltrim(upper(TransPar[1]))
oErrAntes := ERRORBLOCK({ |objErr| BREAK(objErr) } )
BEGIN SEQUENCE
      if ifc == "IF"     /// Start adaptation if else construct - 03/Feb/2008
         ::aStat [ "EntroIF" ] := .T.
         ifc := substr(string,at(chr(07),string)+1 )
         if &ifc //MACROCOMPILE(ifc,.T.,cmdline,ssection)
            // msgbox(ifc,"valido")
            ::aStat [ "DelMode" ] := .F.
         Else
            // msgstop(ifc, "Non valido")
            ::aStat [ "DelMode" ]:= .T.
         Endif
      Elseif "END" $ ifc
         TransPar := {}
         ::aStat [ "DelMode" ] := .F.
         ::aStat [ "ElseStat" ] := .F.
      Elseif "ELSE" $ ifc
         ::aStat [ "EntroIF" ] := .F.
         ::aStat [ "ElseStat" ] := .T.
      Endif
       //msgbox(iif( ::aStat [ "DelMode" ]," ::aStat [ "DelMode" ] .T.","::aStat [ "DelMode" ] .F.")+crlf+if( ::aStat [ "ElseStat" ]," ::aStat [ "ElseStat" ] .T.","::aStat [ "ElseStat" ] .F.")," risulta")
      if !::aStat [ "EntroIF" ] .AND. !::aStat [ "DelMode" ] // i am on false condition
         if ::aStat [ "ElseStat" ]
            //msginfo(ifc ,"Cancellato")
            adel(TransPar,1)    // i must erase else commands
         Endif
      Endif
      if ::aStat [ "EntroIF" ] .AND. ::aStat [ "DelMode" ] // i am on verified condition
         if ::aStat [ "DelMode" ] .AND. !::aStat [ "ElseStat" ]
            //msgbox(ifc ,"Cancellato")
            adel(TransPar,1)// i must erase if commands
         endif
      Endif

      aeval(transpar,{|x| iif(x != NIL,aadd(ArryPar,X), nil ) } )

      if ::aStat [ "Control" ] .AND. (UPPER(LEFT(STRING,5)) != "DEBUG")
         aeval(Arrypar,{|x,y|x:=nil,MsgBox("Section "+ssection+" Line is n° "+zaps(cmdline)+CRLF+"String =";
        +string+CRLF+CRLF+"Argument N°"+zaps(y)+[ ]+ArryPar[y],+::Filename+[ Pag n°]+zaps(npag))})
      endif
      ::leggipar(Arrypar,cmdline,substr(procname(1),4))
      RECOVER USING oErr
      if oErr != NIL
         lMyError := .T.
         MyErrorFunc(oErr)
      endif
END
ERRORBLOCK(oErrAntes)
   if lMyError .AND. ::aStat [ "Control" ]
      MsgBox("Error in  line n° "+zaps(cmdline)+CRLF+string,+::Filename+[ Pag n°]+zaps(npag) )
   endif
return ritorno
/*
*/
*-----------------------------------------------------------------------------*
METHOD Leggipar(ArryPar,cmdline,section) CLASS WREPORT // The core of  interpreter
*-----------------------------------------------------------------------------*
     local _arg1,_arg2, _arg3,__elex ,aX:={} , _varmem ,;
     blse := {|x| iif(val(x)> 0,.T.,iif(x=".T.".OR. x ="ON",.T.,.F.))}, al, _align
     string1 := ""
     if len(ArryPar) < 1 ;return .F. ;endif

     if ::PrnDrv = "MINI"
        //msginfo(arrypar[1],"Rmini")
        RMiniPar(ArryPar,cmdline,section)
     else
        m->MaxCol := hbprn:maxcol
        m->MaxRow := hbprn:maxrow
        do case
           case ArryPar[1]=[VAR]
                _varmem := ArryPar[2]
                If !__MVEXIST ( ArryPar[2] )
                   _varmem := ArryPar[2]
                   Public &_varmem
                   aadd(nomevar,_varmem)
                Endif
                if ArryPar[4] != "A"
                   &_varmem := xvalue(ArryPar[3],ArryPar[4])
                else
                   &_varmem := ::MACROCOMPILE("("+ArryPar[3]+")",.T.,cmdline,section)
                Endif

           case arryPar[1]==[GROUP]
                Group(arryPar[2],arryPar[3],arryPar[4],arryPar[5],arryPar[6],arryPar[7],arryPar[8],arryPar[9])
                /* Alternate method
                aX:={} ; aeval(ArryPar,{|x,y|if (Y >1,aadd(aX,x),Nil)})
                Hb_execFromarray("GROUP",ax)
                asize(ax,0)
                */
           case arryPar[1]==[ADDLINE]
                nline ++

           case arryPar[1]==[SUBLINE]
                nline --

           case ascan(arryPar,[HBPRN ]) > 0
                HBPRNMAXROW:=hbprn:maxrow

           case len(ArryPar)=1
                //msgExclamation(arrypar[1],"int Traduci")
                if "DEBUG_" != left(ArryPar[1],6) .AND. "ELSE" != left(ArryPar[1],4)
                   ::MACROCOMPILE(ArryPar[1],.T.,cmdline,section)
                Endif

           case ArryPar[1]+ArryPar[2]=[ENABLETHUMBNAILS]
                hbprn:thumbnails:=.T.

           case ArryPar[1]=[POLYBEZIER]
                hbprn:polybezier(&(arrypar[2]),eval(chblk,arrypar,[PEN]))

           case ArryPar[1]=[POLYBEZIERTO]
                hbprn:polybezierto(&(arrypar[2]),eval(chblk,arrypar,[PEN]))

           case ArryPar[1]+ArryPar[2]=[DEFINEBRUSH]
                hbprn:definebrush(Arrypar[3],::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_abrush");
                ,::UsaColor(eval(chblk,arrypar,[COLOR])),::HATCH(eval(chblk,arrypar,[HATCH])))

           case ArryPar[1]+ArryPar[2]=[CHANGEBRUSH]
                hbprn:changebrush(Arrypar[3],::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_abrush");
                ,color(eval(chblk,arrypar,[COLOR])),::HATCH(eval(chblk,arrypar,[HATCH])))

           case ArryPar[1]+ArryPar[2]=[CHANGEPEN]
                hbprn:modifypen(Arrypar[3],::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_apen"),val(eval(chblk,arrypar,[WIDTH])),color(eval(chblk,arrypar,[COLOR])))

           case ArryPar[1]+ArryPar[2]=[DEFINEIMAGELIST]
                hbprn:defineimagelist(Arrypar[3],eval(chblk,arrypar,[PICTURE]),eval(chblk,arrypar,[ICONCOUNT]))

           case ascan(arrypar,[IMAGELIST]) > 0 .AND. len(arrypar) > 6
                do case
                   case ascan(arryPar,[BLEND25]) > 0
                        hbprn:drawimagelist(eval(chblk,arrypar,[IMAGELIST]),val(eval(chblk,arrypar,[ICON]));
                        ,eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,ArryPar[4]);
                        ,ILD_BLEND25,::UsaColor(eval(chblk,arrypar,[BACKGROUND])))

                   case ascan(arryPar,[BLEND50]) > 0
                        hbprn:drawimagelist(eval(chblk,arrypar,[IMAGELIST]),val(eval(chblk,arrypar,[ICON]));
                        ,eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,ArryPar[4]);
                        ,ILD_BLEND50,::UsaColor(eval(chblk,arrypar,[BACKGROUND])))

                   case ascan(arryPar,[MASK]) > 0
                        hbprn:drawimagelist(eval(chblk,arrypar,[IMAGELIST]),val(eval(chblk,arrypar,[ICON]));
                        ,eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,ArryPar[4]);
                        ,ILD_MASK,::UsaColor(eval(chblk,arrypar,[BACKGROUND])))

                   otherwise
                        hbprn:drawimagelist(eval(chblk,arrypar,[IMAGELIST]),val(eval(chblk,arrypar,[ICON]));
                        ,eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,ArryPar[4]);
                        ,ILD_NORMAL,::UsaColor(eval(chblk,arrypar,[BACKGROUND])))

                endcase

           case ArryPar[1]+ArryPar[2]=[DEFINEPEN]
                hbprn:definepen(Arrypar[3],::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_apen"),val(eval(chblk,arrypar,[WIDTH])),color(eval(chblk,arrypar,[COLOR])))

           case ArryPar[1]+ArryPar[2]=[DEFINERECT]
                hbprn:definerectrgn(eval(chblk,arrypar,[REGION]),val(eval(chblk,arrypar,[AT]));
                ,val(Arrypar[7]),val(Arrypar[8]),val(Arrypar[9]))

           case ArryPar[1]+ArryPar[2]=[DEFINEROUNDECT]
                hbprn:defineroundrectrgn(eval(chblk,arrypar,[REGION]),val(eval(chblk,arrypar,[AT]));
                ,val(Arrypar[7]),val(Arrypar[8]),val(Arrypar[9]);
                ,eval(chblk,arrypar,[ELLIPSE]),Val(ArryPar[12]))

           case ArryPar[1]+ArryPar[2]=[DEFINEPOLYGON]
                hbprn:definepolygonrgn(eval(chblk,arrypar,[REGION]),&(eval(chblk,arrypar,[VERTEX]));
                ,eval(chblk,arrypar,[STYLE]))

           case ArryPar[1]+ArryPar[2]=[DEFINEELLIPTIC]
                hbprn:defineEllipticrgn(eval(chblk,arrypar,[REGION]),val(eval(chblk,arrypar,[AT]));
                ,eval(epar,ArryPar[7]),eval(epar,ArryPar[8]),eval(epar,ArryPar[9]))

           case ArryPar[1]+arryPar[2]=[DEFINEFONT]
                //                    1        2      3      4       5        6        7            8            9
                //hbprn:definefont(<cfont>,<cface>,<size>,<width>,<angle>,<.bold.>,<.italic.>,<.underline.>,<.strikeout.>)
                hbprn:definefont(iif(ascan(arryPar,[FONT])=2,ArryPar[3],NIL);
                           ,iif(ascan(arryPar,[NAME])=4,ArryPar[5],NIL);
                           ,iif(ascan(arryPar,[SIZE])=6,VAL(ArryPar[7]),NIL);
                           ,iif(ascan(arryPar,[WIDTH])!= 0, VAL(eval(chblk,arrypar,[WIDTH])),NIL);
                           ,iif(ascan(arryPar,[ANGLE])!= 0,VAL(eval(chblk,arrypar,[ANGLE])),NIL);
                           ,iif(ascan(arryPar,[BOLD])!= 0,1,"");
                           ,iif(ascan(arryPar,[ITALIC])!= 0,1,"");
                           ,iif(ascan(arryPar,[UNDERLINE])!= 0,1,"");
                           ,iif(ascan(arryPar,[STRIKEOUT])!= 0,1,""))

           case ArryPar[1]+arryPar[2]=[CHANGEFONT]
                hbprn:modifyfont(iif(ascan(arryPar,[FONT])=2,ArryPar[3],NIL);
                           ,iif(ascan(arryPar,[NAME])=4,ArryPar[5],NIL);
                           ,iif(ascan(arryPar,[SIZE])=6,VAL(ArryPar[7]),NIL);
                           ,iif(ascan(arryPar,[WIDTH])!= 0, VAL(eval(chblk,arrypar,[WIDTH])),NIL);
                           ,iif(ascan(arryPar,[ANGLE])!= 0,VAL(eval(chblk,arrypar,[ANGLE])),NIL);
                           ,iif(ascan(arryPar,[BOLD])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[NOBOLD])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[ITALIC])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[NOITALIC])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[UNDERLINE])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[NOUNDERLINE])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[STRIKEOUT])!=0,.T.,.F.);
                           ,iif(ascan(arryPar,[NOSTRIKEOUT])!=0,.T.,.F.))

           case ArryPar[1]+arryPar[2]=[COMBINEREGIONS]
                hbprn:combinergn(eval(chblk,arrypar,[TO]),ArryPar[3],ArryPar[4];
                ,iif( val(ArryPar[8])>0,val(ArryPar[8]),::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_aRegion")))

           case ascan(arryPar,"SELECT")=1 .AND. len(ArryPar)=3
                if len(ArryPar)=3
                   do case
                      case ascan(ArryPar,[PRINTER])=2
                           hbprn:selectprinter(arrypar[3])

                      case ascan(ArryPar,[FONT])=2
                           hbprn:selectfont(arrypar[3])

                      case ascan(ArryPar,[PEN])=2
                           hbprn:selectpen(arrypar[3])

                      case ascan(ArryPar,[BRUSH])=2
                           hbprn:selectbrush(arrypar[3])

                   endcase
                endif

           case ArryPar[1]+ArryPar[2]="SELECTCLIP" .AND. len(ArryPar)=4
                hbprn:selectcliprgn(eval(chblk,arrypar,[REGION]))

           case ascan(arryPar,"DELETE")=1 .AND. len(ArryPar)=4
                hbprn:deletecliprgn()

           case ascan(arryPar,"SET")=1
                do case
                   case ascan(arryPar,[COPIE])= 2
                        hbprn:setdevmode(256,val(eval(chblk,arrypar,[TO])))

                   case ascan(arryPar,[JOB])= 2
                        ::aStat [ "JobName" ] := eval(chblk,arrypar,[NAME])

                   case ascan(ArryPar,[PAGE])=2
                        _arg1:=eval(chblk,arrypar,[ORIENTATION])
                        _arg2:=eval(chblk,arrypar,[PAPERSIZE])
                        _arg3:=eval(chblk,arrypar,[FONT])
                        hbprn:setpage(iif(val(_arg1)>0,val(_arg1),iif([PORT]$ _arg1,1,2));
                        ,::what_ele(eval(chblk,arrypar,[PAPERSIZE]),::aCh,"_apaper"),_arg3)

                   case ascan(arryPar,[ALIGN])=3
                        if val(Arrypar[4])> 0
                           _align := val(Arrypar[4])
                        else
                           _align := ::what_ele(eval(chblk,arrypar,[ALIGN]),::aCh,"_aAlign")
                        endif
                        hbprn:settextalign(_align)

                   case ascan(arryPar,[RGB])=2
                        &(eval(chblk,arrypar,[TO])):=hbprn:setrgb(arrypar[1],arrypar[2],arrypar[3])

                   case ascan(arryPar,[SCALE])=3      //SET SCALE
                        if ascan(arryPar,[SCALE])> 0
                           hbprn:previewscale:=(val(eval(chblk,arrypar,[SCALE])))
                        elseif ascan(arryPar,[RECT])> 0
                           hbprn:previewrect:={eval(epar,arrypar[4]),eval(epar,arrypar[5]),eval(epar,arrypar[6]),eval(epar,arrypar[7])}
                        endif

                   case ascan(ArryPar,[DUPLEX])=2
                        hbprn:setdevmode(DM_DUPLEX,::what_ele(eval(chblk,arrypar,[DUPLEX]),::aCh,"_aDuplex"))

    	               case ascan(ArryPar,[PREVIEW])=2 .AND. len(arrypar)= 3
                        hbprn:PreviewMode := iif(eval(chblk,arrypar,[PREVIEW])=[OFF],.F.,.T.)

                   case ascan(arryPar,[BIN])=2
                        if val(Arrypar[3])> 0
                           hbprn:setdevmode(DM_DEFAULTSOURCE,val(Arrypar[3]))
                        else
                            hbprn:setdevmode(DM_DEFAULTSOURCE,::what_ele(eval(chblk,arrypar,[BIN]),::aCh,"_ABIN"))
                        endif

                   case ascan(arryPar,[PAPERSIZE])=2   //SET PAPERSIZE
                        hbprn:setdevmode(DM_PAPERSIZE,::what_ele(eval(chblk,arrypar,[PAPERSIZE]),::aCh,"_apaper"))

                   case ascan(arryPar,[PAPERSIZE])=3   //SET PAPERSIZE
                        // SET USER PAPERSIZE WIDTH <width> HEIGHT <height> => hbprn:setusermode(DMPAPER_USER,<width>,<height>)
                        hbprn:setusermode(256,val(eval(chblk,arrypar,[WIDTH])),val(eval(chblk,arrypar,[HEIGHT])))

                   case ascan(arryPar,[ORIENTATION])=2   //SET ORIENTATION
                        if [LAND] $ eval(chblk,arrypar,[ORIENTATION])
                           hbprn:setdevmode(DM_ORIENTATION,DMORIENT_LANDSCAPE) //DMORIENT_LANDSCAPE
                        else
                           hbprn:setdevmode(DM_ORIENTATION,DMORIENT_PORTRAIT) //DMORIENT_PORTRAIT
                        endif

                   case ascan(arryPar,[UNITS])=2
                        do case
                           case arryPar[3]==[ROWCOL] .OR. LEN(ArryPar)==2
                                hbprn:setunits(0)

                           case arryPar[3]==[MM]
                                hbprn:setunits(1)

                           case arryPar[3]==[INCHES]
                                hbprn:setunits(2)

                           case arryPar[3]==[PIXELS]
                                hbprn:setunits(3)

                        endcase

                   case ascan(arryPar,[BKMODE])=2
                        if val(Arrypar[3])> 0
                           hbprn:setbkmode(val(Arrypar[3]))
                        else
                            hbprn:setbkmode(::what_ele(eval(chblk,arrypar,[BKMODE]),::aCh,"_aBkmode"))
                        endif

                   case ascan(ArryPar,[CHARSET])=2
                        hbprn:setcharset(::what_ele(eval(chblk,arrypar,[CHARSET]),::aCh,"_acharset"))

                   case ascan(arryPar,[TEXTCOLOR])=2
                        hbprn:settextcolor( ::UsaColor( eval( chblk,arrypar,[TEXTCOLOR] ) ) )

                   case ascan(arryPar,[BACKCOLOR])=2
                        hbprn:setbkcolor( ::UsaColor( eval( chblk,arrypar,[BACKCOLOR] ) ) )

                   case ascan(arryPar,[ONEATLEAST])= 2
                        ONEATLEAST :=eval(blse,arrypar[3])

                   case ascan(arryPar,[THUMBNAILS])= 2
                        hbprn:thumbnails:=eval(blse,arrypar[3])

                   case ascan(arryPar,[EURO])=2
                        _euro:=eval(blse,arrypar[3])

                   case ascan(arryPar,[CLOSEPREVIEW])=2
                        hbprn:closepreview(eval(blse,arrypar[3]))

                   case ascan(arryPar,[SUBTOTALS])=2
                        m->sbt := (eval(blse,arrypar[3]))

                   case ascan(arryPar,[SHOWGHEAD])=2
                        m->sgh := (eval(blse,arrypar[3]))

                   case ascan(arryPar,[INLINESBT])=2
                       ::aStat["InlineSbt"] := (eval(blse,arrypar[3]))

                   case ascan(arryPar,[INLINETOT])=2
                       ::aStat["InlineTot"] := (eval(blse,arrypar[3]))

                   case ascan(arryPar,[TOTALSTRING])=2
                        m->TTS := eval( chblk,arrypar,[TOTALSTRING] )

                   case ascan(arryPar,[DEBUG])=2
                        if ascan(arryPar,[LIST])= 3
                           if npag < 2 .AND. len(arrypar) = 4
                              asize(ax,0)
                              do case
                                 case ascan(arryPar,[DECLARE])= 4
                                      aeval(::aDeclare, {|x|iif(x != NIL, aadd(ax, strzero(x[2], 4) + ") " + x[1]), nil)})

                                 case ascan(arryPar,[HEAD])= 4
                                      aeval(::aHead, {|x|iif(x != NIL, aadd(ax, strzero(x[2], 4) + ") " + x[1]), nil)})

                                 case ascan(arryPar,[BODY])= 4
                                      aeval(::aBody, {|x|iif(x != NIL, aadd(ax, strzero(x[2], 4) + ") " + x[1]), nil)})

                                 case ascan(arryPar,[FEET])= 4
                                      aeval(::aFeet, {|x|iif(x != NIL, aadd(ax, strzero(x[2], 4) + ") " + x[1]), nil)})

                              endcase
                              msgmulty(ax)
//                            mchoice( ax,,,,,"["+arrypar[4]+"]"+" section with len = "+ltrim(str(len(ax))))
                              asize(ax,0)
                           endif
                        else
                           ::aStat [ "Control" ] := eval(blse,arrypar[3])
                        endif

                   case ascan(arryPar,[MONEY])=2
                        _money:=eval(blse,arrypar[3])

                   case ascan(arryPar,[SEPARATOR])=2
                        _separator:=eval(blse,arrypar[3])

                   case ascan(arryPar,[JUSTIFICATION])=3
                        hbprn:settextjustification(val(ArryPar[4]))

                   case ascan(arryPar,[MARGINS])=3
                        hbprn:setviewportorg(val(eval(chblk,arrypar,[TOP])),val(eval(chblk,arrypar,[LEFT])))

                   case (ascan(ArryPar,[POLYFILL])=2 .AND. Arrypar[3]==[MODE])
                        if val(Arrypar[4])> 0
                            hbprn:setpolyfillmode(val(Arrypar[4]))
                        else
                            hbprn:setpolyfillmode(::what_ele(eval(chblk,arrypar,[MODE]),::aCh,"_apoly"))
                        endif

                   case ascan(ArryPar,[POLYFILL])=2 .AND. len(arrypar)=3
                        hbprn:setpolyfillmode(::what_ele(eval(chblk,arrypar,[POLYFILL]),::aCh,"_aPoly"))

                   case ascan(ArryPar,[VIEWPORTORG])=2
                        hbprn:setviewportorg(val(Arrypar[3]),Val(arrypar[4]))

                   case ascan(ArryPar,[TEXTCHAR])=2
                        hbprn:settextcharextra(Val(eval(chblk,arrypar,[EXTRA])))

                   case ArryPar[2]= [COLORMODE] //=1
                        hbprn:setdevmode(DM_COLOR,;
                        iif(val(arrypar[3])>0,val(arrypar[3]),iif(arrypar[3]=".T.",2;
                        ,::what_ele(eval(chblk,arrypar,[COLORMODE]),::aCh,"_acolor"))))

                   case ArryPar[2]= [QUALITY]   //=1
                        hbprn:setdevmode(DM_PRINTQUALITY,::what_ele(eval(chblk,arrypar,[QUALITY]),::aCh,"_aQlt"))

                endcase

           case ascan(arryPar,"GET")=1

                do case
                   case ascan(arryPar,[TEXTCOLOR])> 0
                        if len(ArryPar)> 3
                           &(eval(chblk,arrypar,[TO])):=hbprn:gettextcolor()
                        else
                           &(eval(chblk,arrypar,[TEXTCOLOR])):=hbprn:gettextcolor()
                        endif

                   case ascan(arryPar,[BACKCOLOR])> 0
                        if len(ArryPar)> 3
                           &(eval(chblk,arrypar,[TO])):=hbprn:getbkcolor()
                        else
                           &(eval(chblk,arrypar,[BACKCOLOR])):=hbprn:getbkcolor()
                        endif

                   case ascan(arryPar,[BKMODE])> 0
                        if len(ArryPar)> 3
                           &(eval(chblk,arrypar,[TO])):=hbprn:getbkmode()
                        else
                           &(eval(chblk,arrypar,[BKMODE])):=hbprn:getbkmode()
                        endif

                   case ascan(arryPar,[ALIGN])> 0
                        if len(ArryPar)> 4
                           &(eval(chblk,arrypar,[TO])):=hbprn:gettextalign()
                        else
                           &(eval(chblk,arrypar,[ALIGN])):=hbprn:gettextalign()
                        endif

                   case ascan(arryPar,[EXTENT])> 0
                        hbprn:gettextextent(eval(chblk,arrypar,[EXTENT]);
                        ,&(eval(chblk,arrypar,[TO])),iif(ascan(arryPar,[FONT])>0,eval(chblk,arrypar,[FONT]),NIL))

                   case ArryPar[1]+ArryPar[2]+ArryPar[3]+ArryPar[4]=[GETPOLYFILLMODETO]
                        &(eval(chblk,arrypar,[TO])):=hbprn:getpolyfillmode()

                   case ArryPar[2]+ArryPar[3]=[VIEWPORTORGTO]
                        hbprn:getviewportorg()
                        &(eval(chblk,arrypar,[TO])):=aclone(hbprn:viewportorg)

                   case ascan(ArryPar,[TEXTCHAR])=2
                        &(eval(chblk,arrypar,[TO])):=hbprn:gettextcharextra()

                   case ascan(arryPar,[JUSTIFICATION])=3
                        &(eval(chblk,arrypar,[TO])):=hbprn:gettextjustification()

                endcase

           case ascan(arryPar,[START])=1 .AND. len(ArryPar)=2
                if ArryPar[2]=[DOC]
                   hbprn:startdoc()
                elseif ArryPar[2]=[PAGE]
                   hbprn:startpage()
                endif

           case ascan(arryPar,[END])=1 .AND. len(ArryPar)=2
                if ArryPar[2]=[DOC]
                   hbprn:enddoc()
                elseif ArryPar[2]=[PAGE]
                   hbprn:endpage()
                endif

           case ascan(arryPar,[POLYGON])=1
                hbprn:polygon(&(arrypar[2]),eval(chblk,arrypar,[PEN]);
                ,eval(chblk,arrypar,[BRUSH]),eval(chblk,arrypar,[STYLE]))

           case ascan(arryPar,[DRAW])=5 .AND. ascan(arryPar,[TEXT])=6
                /*
                aeval(arrypar,{|x,y|msginfo(x,zaps(y)) } )
                #xcommand @ <row>,<col>,<row2>,<col2> DRAW TEXT <txt> [STYLE <style>] [FONT <cfont>];
                => hbprn:drawtext(<row>,<col>,<row2>,<col2>,<txt>,<style>,<cfont>)
                */
                //msgbox(zaps(::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_STYLE")),"GGGGG")
                al := ::UsaFont(arrypar)

                hbprn:drawtext(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]);
                ,eval(epar,ArryPar[3]),eval(epar,Arrypar[4]),eval(chblk,arrypar,[TEXT]);
                ,::what_ele(eval(chblk,arrypar,[STYLE]),::aCh,"_STYLE"), "Fx" )

                hbprn:settextalign( al[1] )
                hbprn:settexcolor ( al[2] )

           case ascan(arryPar,[RECTANGLE])=5
                //MSG([PEN=]+eval(chblk,arrypar,[PEN])+CRLF+[BRUSH=]+eval(chblk,arrypar,[BRUSH]),[RETTANGOLO])
                hbprn:rectangle(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]);
                ,eval(chblk,arrypar,[PEN]),eval(chblk,arrypar,[BRUSH]))

           case ascan(ArryPar,[FRAMERECT])=5 .OR. ascan(ArryPar,[FOCUSRECT])=5
                hbprn:framerect(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]);
                ,eval(chblk,arrypar,[BRUSH]))

           case ascan(ArryPar,[FILLRECT])=5
                hbprn:fillrect(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]);
                ,eval(chblk,arrypar,[BRUSH]))

           case ascan(ArryPar,[INVERTRECT])=5
                hbprn:invertrect(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]))

           case ascan(ArryPar,[ELLIPSE])=5
                hbprn:ellipse(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]);
                ,eval(chblk,arrypar,[PEN]), eval(chblk,arrypar,[BRUSH]))

           case ascan(arryPar,[RADIAL1])>0
                do case
                   case arrypar[5]=[ARC]
                        hbprn:arc(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]);
                        ,eval(epar,Arrypar[4]),eval(epar,Arrypar[7]),eval(epar,Arrypar[8]);
                        ,eval(epar,Arrypar[10]),eval(epar,Arrypar[11]),eval(chblk,arrypar,[PEN]))

                   case arrypar[3]=[ARCTO]
                        hbprn:arcto(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[5]);
                        ,eval(epar,Arrypar[6]),eval(epar,Arrypar[8]),eval(epar,Arrypar[9]);
                        ,eval(chblk,arrypar,[PEN]))

                   case arrypar[5]=[CHORD]
                        hbprn:chord(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]);
                        ,eval(epar,Arrypar[4]),eval(epar,Arrypar[7]),eval(epar,Arrypar[8]);
                        ,eval(epar,Arrypar[10]),eval(epar,Arrypar[11]),eval(chblk,arrypar,[PEN]);
                        ,eval(chblk,arrypar,[BRUSH]))

                   case arrypar[5]=[PIE]
                        hbprn:pie(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]);
                        ,eval(epar,Arrypar[4]),eval(epar,Arrypar[7]),eval(epar,Arrypar[8]);
                        ,eval(epar,Arrypar[10]),eval(epar,Arrypar[11]),eval(chblk,arrypar,[PEN]);
                        ,eval(chblk,arrypar,[BRUSH]))

                endcase

           case ASCAN(ArryPar,[LINETO])=3
                hbprn:lineto(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),iif(ASCAN(ArryPar,[PEN])= 4,ArryPar[5],NIL))

           case ascan(ArryPar,[LINE])=5
                hbprn:line(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,Arrypar[4]),iif(ASCAN(ArryPar,[PEN])= 6,ArryPar[7],NIL))

           case ascan(ArryPar,[PICTURE])=3
                /*
                            1     2       3      4     5     6     7       8      9      10
                #xcommand @<row>,<col> PICTURE <cpic> SIZE <row2>,<col2> [EXTEND <row3>,<col3>] ;
                            => hbprn:picture(<row>,<col>,<row2>,<col2>,<cpic>,<row3>,<col3>)
                                                 1     2     6      7      4      9      10
                */
                if "->" $ ArryPar[4] .OR. "(" $ ArryPar[4]
                   ArryPar[4]:= ::MACROCOMPILE(ArryPar[4],.T.,cmdline,section)
                Endif
                do case
                   case len(ArryPar)= 4
                           hbprn:picture(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),,,ArryPar[4])

                   case len(ArryPar)= 7
                        hbprn:picture(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[6]),eval(epar,Arrypar[7]),ArryPar[4])

                   case len(ArryPar)=10
                        if ascan(ArryPar,[EXTEND])=8
                           hbprn:picture(eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[6]),eval(epar,Arrypar[7]),ArryPar[4],eval(epar,ArryPar[9]),eval(epar,ArryPar[10]))
                        endif

                endcase

           case ascan(ArryPar,[ROUNDRECT])=5   //da rivedere
                /*
                @ <row>,<col>,<row2>,<col2> ROUNDRECT  [ROUNDR <tor>] [ROUNDC <toc>] [PEN <cpen>] [BRUSH <cbrush>];
                            => hbprn:roundrect(<row>,<col>,<row2>,<col2>,<tor>,<toc>,<cpen>,<cbrush>)
                RoundRect(row,col,torow,tocol,widthellipse,heightellipse,defpen,defbrush)
                */
               set exact on
               hbprn:roundrect( eval(epar,ArryPar[1]),eval(epar,ArryPar[2]),eval(epar,ArryPar[3]),eval(epar,ArryPar[4]);
               ,val(eval(chblk,arrypar,[ROUNDR])),val(eval(chblk,arrypar,[ROUNDC])),eval(chblk,arrypar,[PEN]),eval(chblk,arrypar,[BRUSH]))
               set exact off

           case ascan(ArryPar,[TEXTOUT])=3

                al := ::UsaFont(arrypar)

                if ascan(ArryPar,[FONT])=5
                   if "->" $ ArryPar[4] .OR. "(" $ ArryPar[4]
                      __elex:=ArryPar[4]
                      hbprn:textout(iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])),eval(epar,ArryPar[2]),&(__elex),"FX")
                   else
                      hbprn:textout(iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])),eval(epar,ArryPar[2]),ArryPar[4],"Fx")
                   endif
                elseif LEN(ArryPar)=4
                   if "->" $ ArryPar[4] .OR. "(" $ ArryPar[4]
                      __elex:=ArryPar[4]
                      hbprn:textout(iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])),eval(epar,ArryPar[2]),&(__elex))
                   else
                      hbprn:textout(iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])),eval(epar,ArryPar[2]),ArryPar[4])
                   endif
                ENDIF

                hbprn:settextalign( al[1] )
                hbprn:settexcolor ( al[2] )

           case ascan(ArryPar,[PRINT])=3 .OR. ascan(ArryPar,[SAY])= 3

                al := ::UsaFont(arrypar)

                hbprn:say(iif([LINE] $ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])),eval(epar,ArryPar[2]);
                         ,iif("->" $ ArryPar[4] .OR. [(] $ ArryPar[4],::MACROCOMPILE(ArryPar[4],.T.,cmdline,section),ArryPar[4]);
                         ,iif(ascan(hbprn:Fonts[2],eval(chblk,arrypar,[FONT]) )> 0,eval(chblk,arrypar,[FONT]),"FX")  ;
                         ,iif(ascan(arryPar,[COLOR])>0,::UsaColor(eval(chblk,arrypar,[COLOR])),NIL);
                         ,nil )
                         //,iif(ascan(arryPar,[ALIGN])>0,::what_ele(eval(chblk,arrypar,[ALIGN]),::aCh,"_aAlign"),NIL))

                hbprn:settextalign( al[1] )
                hbprn:settexcolor ( al[2] )

           case ascan(ArryPar,[MEMOSAY])=3

                al := ::UsaFont(arrypar)

                ::MemoSay(iif([LINE] $ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                   ,eval(epar,ArryPar[2]) ;
                   ,::MACROCOMPILE(ArryPar[4],.T.,cmdline,section) ;
                   ,iif(ascan(arryPar,[LEN])>0,iif(HB_ISARRAY(oWr:argm[3]),;
                                              ::MACROCOMPILE(eval(chblk,arrypar,[LEN]),.T.,cmdline,section) , ;
                                              val(eval(chblk,arrypar,[LEN]))),NIL) ;
                   ,iif(ascan(arryPar,[FONT])>0,"FX",NIL);
                   ,iif(ascan(arryPar,[COLOR])>0,::UsaColor(eval(chblk,arrypar,[COLOR])),NIL);
                   ,NIL ;
                   ;//,iif(ascan(arryPar,[ALIGN])>0,::what_ele(eval(chblk,arrypar,[ALIGN]),::aCh,"_aAlign"),NIL);
                   ,iif(ascan(arryPar,[.F.])>0,".F.","");
                   ,arrypar)

                hbprn:settextalign( al[1] )
                hbprn:settexcolor ( al[2] )

          case ascan(ArryPar,[PUTARRAY])=3

                al := ::UsaFont(arrypar)

                ::Putarray(iif([LINE] $ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                   ,eval(epar,ArryPar[2]) ;
                   ,::MACROCOMPILE(ArryPar[4],.T.,cmdline,section)    ;            //arr
                   ,iif(ascan(arryPar,[LEN])>0,::macrocompile(eval(chblk,arrypar,[LEN])),NIL) ; //awidts
                   ,nil                                                           ;      //rowheight
                   ,nil                                                           ;      //vertalign
                   ,(ascan(arryPar,[NOFRAME])>0)                                  ;      //noframes
                   ,nil                                                           ;      //abrushes
                   ,nil                                                           ;      //apens
                   ,iif(ascan(arryPar,[FONT])>0,NIL,NIL)                           ;      //afonts
                   ,iif(ascan(arryPar,[COLOR])> 0,::UsaColor(eval(chblk,arrypar,[COLOR])),NIL);//afontscolor
                   ,NIL                                                           ;      //abitmaps
                   ,nil )                                                                //userfun

                hbprn:settextalign( al[1] )
                hbprn:settexcolor ( al[2] )

          case ascan(ArryPar,[NEWPAGE])=1 .OR. ascan(ArryPar,[EJECT])=1
                hbprn:endpage()
                hbprn:startpage()

        endcase
     endif
return .T.
/*
*/
*-----------------------------------------------------------------------------*
METHOD WHAT_ELE(Arg1,Arg2,Arg3) CLASS WREPORT
*-----------------------------------------------------------------------------*
     local rtv ,sets:="",kl := 0 , ltemp := "" ,;
     Asr := {{"_APAPER","DMPAPER_A4"} ,{"_ABIN","DMBIN_AUTO"},{"_APEN","PS_SOLID"},;
             {"_ABRUSH","BS_SOLID"},{"_APOLY","ALTERNATE"},{"_ABKMODE","TRANSPARENT"},;
             {"_AALIGN","TA_LEFT"} ,{"_AREGION","RGN_AND"} ,{"_ACOLOR","MONO"}, ;
             {"_AQLT","DMRES_DRAFT"}, {"_STYLE","DT_TOP"}}
     default arg3 to "_APAPER"
     Arg3:=upper(Arg3)
     aeval(aSr,{|x| iif(x[1]== Arg3,ltemp:=x[2],"")})
     if !empty(ltemp)
        default arg1 to ltemp
        if arg3="_ACOLOR" .AND. arg1 = ".F."
           arg1 := "MONO"
        Endif
     endif

     rtv := ASCAN(arg2, {|aVal| aVal[1] == arg1})

     if rtv > 0
        sets := arg2[rtv,1]
        rtv  := arg2[rtv,2]
     else
        if arg3 = "TEST" //_AQLT"
           for kl:=01 to len(arg2)
               msg(arg1+CRLF+arg2[kl,1]+CRLF+zaps(arg2[kl,2]),arg3)
           next
        endif
     endif
     /*
     if arg3 = ""  //_ABKMODE"      //If you want test it ...
        msg(sets+" = "+zaps(rtv),arg3)
     endif
     */
return rtv
/*
*/
*-----------------------------------------------------------------------------*
METHOD MEMOSAY(row,col,argm1,argl1,argf1,argcolor1,argalign,onlyone,arrypar) CLASS WREPORT
*-----------------------------------------------------------------------------*
 local _Memo1:=argm1, mrow:=max(1,mlcount(_memo1,argl1)), arrymemo:={}, esci:=.F.
 Local units := hbprn:UNITS, k, mcl ,ain, str :="", typa := .F.
 default col to 0 ,row to 0, argl1 to 10, onlyone to ""

 if HB_ISARRAY(argm1)
    typa := .T.
    arrymemo := {}
    if ::IsMono(argm1)
       arrymemo := aclone(argm1)
    Else
       for each ain IN argm1
           aeval( ain,{|x,y| str += substr(hb_valtostr(x),1,argl1[y])+" " } )
           str := rtrim(str)
           aadd(arrymemo,str)
           STR := ""
       next ain
    Endif
 Else
   for k := 1 to mrow
       aadd(arrymemo,oWr:justificalinea(memoline(_memo1,argl1,k),argl1))
   next
 Endif
 if empty(onlyone)
    hbprn:say(iif(UNITS > 0.AND.units < 4,nline*lstep,nline),col,arrymemo[1],argf1,argcolor1,argalign)
    ::aStat [ "Yes_Memo" ]:= .T.
 else
    for mcl := 2 to len(arrymemo)
        nline ++
        if nline >= ::HB - 1
           ::TheFeet()
           ::TheHead()
           ::UsaFont(arrypar)
        endif
        hbprn:say(iif(UNITS > 0.AND.units < 4,nline*lstep,nline),col,arrymemo[mcl],argf1,argcolor1,argalign)
    next
    if !typa
       dbskip()
    Endif
 endif
 return self
/*
*/
*-----------------------------------------------------------------------------*
METHOD PUTARRAY(row,col,arr,awidths,rowheight,vertalign,noframes,abrushes,apens,afonts,afontscolor,abitmaps,userfun) CLASS Wreport
*-----------------------------------------------------------------------------*
local j,ltc,lxc,lvh,lnf:=!noframes,lafoc,lafo,labr,lape,xlwh1,labmp,lcol,lwh ;
,old_pfbu:={hbprn:PENS[hbprn:CURRPEN,2],hbprn:FONTS[hbprn:CURRFONT,2],hbprn:BRUSHES[hbprn:CURRBRUSH,2],hbprn:UNITS,hbprn:GetTextAlign()}
local IsMono

private xlwh,xfo,xfc,xbr,xpe,xwa,xbmp
if empty(arr)
   return nil
endif
::aStat [ "Yes_Array" ]:= .T.
// hbprn:setunits(0)
afonts:=if(afonts==NIL,"",afonts)
lafo:=if(valtype(afonts)=="C",afill(array(len(arr[1])),afonts),afonts)

abitmaps:=if(abitmaps==NIL,"",abitmaps)
labmp:=if(valtype(abitmaps)=="C",afill(array(len(arr[1])),abitmaps),abitmaps)

afontscolor:=if(afontscolor==NIL,0,afontscolor)
lafoc:=if(HB_ISNUMERIC(afontscolor),afill(array(len(arr[1])),afontscolor),afontscolor)

abrushes:=if(abrushes==NIL,"",abrushes)
labr:=if(valtype(abrushes)=="C",afill(array(len(arr[1])),abrushes),abrushes)

apens:=if(apens==NIL,"",apens)
lape:=if(valtype(apens)=="C",afill(array(len(arr[1])),apens),apens)

ltc:=if(awidths==NIL,afill(array(len(arr[1])),10),awidths)

lwh:=if(empty(rowheight),1,rowheight)
lvh:=if(vertalign==NIL,0,vertalign)
IsMono := ::Ismono(arr)
  do case
     case lvh==TA_CENTER ; lvh:=rowheight/2-0.5
     case lvh==TA_BOTTOM ; lvh:=rowheight-1
     otherwise           ; lvh:=0  && TA_TOP
  endcase
  lxc   := col
  xlwh1 := 0
  for j:=1 to if (IsMono,1 ,len(arr[1]))
      xlwh := lwh
      xfo  := lafo[j]
      xfc  := lafoc[j]
      xbr  := labr[j]
      xpe  := lape[j]
      if IsMono
         xwa := arr[::acnt]
      Else
         xwa  := arr[::acnt,j]
      Endif
      xbmp := labmp[j]
      if valtype(userfun)=="C"
         &userfun(::acnt,j,(nline*lstep),@xwa,@xlwh,@xfo,@xfc,@xbr,@xpe,@xbmp)
      endif
      if xlwh > xlwh1
         xlwh1 := xlwh
      endif
      if lnf
        @row,lxc,(nline*lstep)+xlwh+5,lxc+ltc[j] rectangle brush xbr pen xpe
      endif
      if !empty(xbmp)
        @row,lxc picture xbmp size xlwh,ltc[j]
      endif
      if HB_ISNUMERIC(xwa)
         lcol:= lxc+ltc[j]-0.5
         set text align TA_RIGHT
      else
         lcol:=lxc+1
         set text align TA_LEFT
      endif
      if oWr:PrnDrv = "HBPR"
        @row,lcol say xwa Font "FX" color xfc to print
      Else
      //FOR NOW DO NONE !!!
      /*
        _HMG_PRINTER_H_PRINT( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
         , nline*lstep , arg2, argf1 , argsize , afontscolor[1], afontscolor[2], afontscolor[3] ;
         , xwa, abold, aita, aunder, astrike;
         , iif(HB_ISARRAY(afontscolor), .T.,.F.) ;
         , iif(valtype(argf1)=="C", .T.,.F.) ;
         , iif(HB_ISNUMERIC(argsize), .T.,.F.) ;
         , argalign )
      */
      Endif
      lxc += ltc[j]
  next

 hbprn:selectpen(old_pfbu[1])
 hbprn:selectfont(old_pfbu[2])
 hbprn:selectbrush(old_pfbu[3])
 hbprn:setunits(old_pfbu[4])
 // hbprn:SetTextAlign(old_pfbu[5])
return self
/*
*/
*-----------------------------------------------------------------------------*
METHOD HATCH(arg1) CLASS WREPORT
*-----------------------------------------------------------------------------*
   local ritorno := 0 ,;
   Asr := {"HS_HORIZONTAL","HS_VERTICAL","HS_FDIAGONAL","HS_BDIAGONAL","HS_CROSS","HS_DIAGCROSS"}
   ritorno := max(0, ascan(Asr,arg1)-1 )
return ritorno
/*
*/
*-----------------------------------------------------------------------------*
METHOD GROUP(GField, s_head, s_col, gftotal, wheregt, s_total, t_col, p_f_e_g) CLASS WREPORT
*                1        2      3       4        5        6       7       8
*-----------------------------------------------------------------------------*
local ritorno := iif( indexord()> 0 ,.T.,.F. )
local posiz   := 0, P1 := 0, P2 := 0, P3 := 0, cnt := 1
local Aposiz  := {}, k, Rl, Rm, Rr, ghf:=""
local db_arc:=dbf() , units , tgftotal , nk, EXV := {||NIL},EXT := {||NIL}

      If ::PrnDrv = "HBPR"
         Units := hbprn:UNITS
      else
         Units := 3
      endif

      default S_TOTAL TO "", s_head to "", gftotal to ""
      default wheregt to [AUTO], s_col to [AUTO], t_col to 0
      default P_F_E_G to .F.

      Asize(counter,0)

      if HB_ISNUMERIC(s_col); s_col:=zaps(s_col); endif

      if valtype(P_F_E_G) == "C"
         ::aStat [ "P_F_E_G" ]  :=  (".T." $ upper(P_F_E_G))
      elseif valtype(P_F_E_G) == "L"
         ::aStat [ "P_F_E_G" ]  := P_F_E_G
      endif

      if !empty(gfield)
         ::aStat [ "Ghead" ]   :=.T.
         ::aStat[ "TempFeet" ] := trans((db_arc)->&(GField),"@!")
      endif
      if empty(GField)
         msgExclamation("Missing Field Name id Group declaration!")
         ritorno   := .F.
      else
         m->GField := GField
      endif

      if !empty(s_head)
         ::aStat [ "GHline" ]    := .T.
         m->s_head := s_head
         m->s_col  := val( s_col )
      else
         m->s_head :=""
      endif

      if !empty(s_total)
         GFline     := .T.
         IF "{||" = LEFT(S_total,3)
            EXT := alltrim(substr(S_Total,at("||",S_Total)+2,at("}",S_Total)-4))
            m->s_total := ::macrocompile( EXT )
         Else
            m->s_total := s_total
         Endif
         m->t_col   := Val( t_col )
      endif

      if valtype(gftotal)== "C"    // sistemazione per conti su colonne multiple
         && make an array for gftotal
         gftotal := AscArr(upper(gftotal) )
      endif
      && make an array for counters
      Aeval(gftotal, {||aadd(counter, 0), aadd(Gcounter, 0)})

      if !empty(gftotal) .OR. !empty(s_total)
         GFline    :=.T.
         m->gfexec :=.T.
         IF "{||" = LEFT(S_total,3)
            EXT := alltrim(substr(S_Total,at("||",S_Total)+2,at("}",S_Total)-4))
            // m->s_total := macrocompile("("+Any2Strg(eval({||EXT }))+ ")")
            m->s_total := ::macrocompile(EXT )
         Else
            m->s_total := s_total
         Endif

         m->t_col  := Val( t_col )
      endif

      && make autoset for stringHead position
      Aeval(::aBody,{|x,y|if(upper(m->gfield) $ upper(x[1]),Posiz :=y,"")})

      if posiz > 0  //IS A BODY DECLARED FIELD
         P1 := max(at("SAY",upper(::aBody[posiz,1]))+3,at("PRINT", upper(::aBody[posiz,1]) )+5)
         P2 := at("FONT",upper(::aBody[posiz,1]) )-2
         IF "{||" = LEFT(S_HEAD,3)
            EXV := alltrim(substr(S_HEAD,at("||",S_HEAD)+2,at("}",S_HEAD)-4))
         Endif
         GHstring:=substr(::aBody[posiz,1],1,P1)+;
         IIF("{||" = LEFT(S_HEAD,3), Any2Strg(eval({||exv })) ;
          ,"(["+ s_head+"]+"+::Hgconvert(substr(::aBody[posiz,1],P1+1,P2-p1))+")" ) ;
         +substr(::aBody[posiz,1],p2+1)
         if upper(s_col) != [AUTO]
            GHstring:=left(::aBody[posiz,1],at(chr(07),::aBody[posiz,1]))+s_col+chr(07)+substr(Ghstring,at("SAY",Ghstring))
         Endif
      else   // NOT DECLARED INTO BODY
         ghf := ::Hgconvert(gfield)
         Ghstring :=if (UNITS > 0 .AND. units < 4 ,"(NLINE*LSTEP)","NLINE")+CHR(07)+zaps(M->S_COL)+CHR(07)
         Ghstring +="SAY"+CHR(07)+"(["+s_head+"]+"+ghf+")"+CHR(07)+"FONT"+CHR(07)+"FNT01"
      endif

      // Gestisce l'automatismo del posizionamento dei subtotali
      && make autoset for Counter(s) position
      tgftotal   := aclone(gftotal)
      m->gftotal := aclone(gftotal)

      for each k in ::aBody
          P1 := at( "SAY", upper(k[1]) ); P2 := at( "PRINT", upper(k[1]) )
          P3 := at( "TEXTOUT", upper(k[1]) )
          if max(p3,max(p1,p2)) = p3
             P1 := P3 + 8
          elseif p2 > p1
             P1 := max(p2,p1) + 6
          elseif p2 < p1
             P1 := max(p2,p1) + 4
          endif
          Rl := substr(k[1],1,p1-1)
          Rm := substr(substr(k[1],p1),1,at(chr(07),substr(k[1],p1))-1)
          Rr := substr(substr(k[1],p1),at(chr(07),substr(k[1],p1)))
          for nk = 1 to len(tgftotal)
              if tgftotal[nk] $ upper(Rm)
                 rm := upper(rm)
                 if upper(tgftotal[nk]) $ Rm    &&  è maiuscolo
                    // msginfo(rm+CRLF+tgftotal[nk],"1")
                    rm:= strtran(rm,tgftotal[nk],"m->counter["+zaps(cnt)+"]")
                 Endif
/*
                 else
                    msginfo(rm+CRLF+tgftotal[nk],"2")
                    rm:= strtran(rm,lower(tgftotal[nk]),"m->counter["+zaps(cnt)+"]")
                 endif
                 msgbox(Rl+CRLF+Rm+CRLF+Rr,zaps(nk)+"-GFFFSTRING")
*/
                 aadd(GFstring,Rl+Rm+Rr)
                 tgftotal[nk]:=""
                 cnt ++
              endif
          next
      next
      //Aeval(gfstring,{|x| msgstop( zaps( len(gfstring) ) +crlf+x,"Gfstring" ) })
      if HB_ISNUMERIC(wheregt)
         wheregt:=zaps(wheregt)
      endif

      // I Gran Totali
      Aeval(GFstring,{|x| aadd(GTstring,strtran(x,"counter[","gcounter["))})
      if val(wheregt) > 0
         if len(wheregt) < 2
            for k=1 to len(GFstring)
                GTstring[k]:=left(GTstring[k],at(chr(07),GTstring[k]))+wheregt+chr(07)+substr(Gtstring[k],at("SAY",Gtstring[k]))
            next
         else
            GTstring[1]:=left(GTstring[1],at(chr(07),GTstring[1]))+wheregt+chr(07)+substr(Gtstring[1],at("SAY",Gtstring[1]))
         endif
      endif
return ritorno
/*
*/
*-----------------------------------------------------------------------------*
METHOD GrHead() CLASS WREPORT
*-----------------------------------------------------------------------------*
local db_arc:=dbf()
local ValSee:= iif(!empty(gfield),trans((db_arc)->&(GField),"@!"),"")

      if ValSee == ::aStat[ "TempHead" ]
         ::aStat [ "Ghead" ]    := .F.
      else
         ::aStat [ "Ghead" ]    := .T.
         ::aStat [ "TempHead" ] := ValSee
      endif

return ::aStat [ "Ghead" ]
/*
*/
*-----------------------------------------------------------------------------*
METHOD GFeet() CLASS WREPORT
*-----------------------------------------------------------------------------*
local db_arc:=dbf(), Gfeet
local ValSee:=if(!empty(gfield),trans((db_arc)->&(GField),"@!"),"")
      if ValSee == ::aStat[ "TempFeet" ]
         Gfeet := .F.
      else
         Gfeet := .T.
         ::aStat[ "TempFeet" ]:= ValSee
      endif
return Gfeet
/*
*/
*-----------------------------------------------------------------------------*
METHOD UsaFont(arrypar) CLASS WREPORT
*-----------------------------------------------------------------------------*
   Local al := { hbprn:gettextalign(), hbprn:gettexcolor() }

   hbprn:modifyfont("Fx", eval(chblk,arrypar,[FONT] ) , ;
         val(eval(chblk,arrypar,[SIZE])) ,;
         val(eval(chblk,arrypar,[WIDTH]) ) ,;
         val(eval(chblk,arrypar,[ANGLE]) ) ,;
         (ascan(arryPar,[BOLD])>0),!(ascan(arryPar,[BOLD])>0) , ;
         (ascan(arryPar,[ITALIC])>0),!(ascan(arryPar,[ITALIC])>0) ,;
         (ascan(arryPar,[UNDERLINE])>0) ,!(ascan(arryPar,[UNDERLINE])>0) ,;
         (ascan(arryPar,[STRIKEOUT])>0),!(ascan(arryPar,[STRIKEOUT])>0) )

   if ascan(arryPar,[COLOR]) > 0
      hbprn:settextcolor(::UsaColor(eval(chblk,arrypar,[COLOR])))
   Endif

   if ascan(arryPar,[ALIGN])>0
      hbprn:settextalign(::what_ele(eval(chblk,arrypar,[ALIGN]),::aCh,"_aAlign") )
   Endif

return al
/*
*/
*-----------------------------------------------------------------------------*
METHOD UsaColor(arg1) CLASS WREPORT
*-----------------------------------------------------------------------------*
   local ritorno:=arg1
   if "X" $ upper(arg1)
      arg1 := substr(arg1, at("X", arg1) + 1)
      IF ::PrnDrv = "HBPR"
         ritorno := Rgb(HEXATODEC(substr(arg1,-2));
                ,HEXATODEC(substr(arg1,5,2)),HEXATODEC(substr(arg1,3,2)) )
      Else
         ritorno := {HEXATODEC(substr(arg1,-2));
                ,HEXATODEC(substr(arg1,5,2) ),HEXATODEC(substr(arg1,3,2)) }
      endif
   else
      ritorno := color(arg1)
   endif
return ritorno
/*
*/
*-----------------------------------------------------------------------------*
METHOD SetMyRgb(dato) CLASS WREPORT
*-----------------------------------------------------------------------------*
   local HexNumber, r
   default dato to 0
   hexNumber := DECTOHEXA(dato)
   IF ::PrnDrv = "HBPR"
      r := Rgb(HEXATODEC(substr(HexNumber,-2));
              ,HEXATODEC(substr(HexNumber,5,2)),HEXATODEC(substr(HexNumber,3,2)) )
   else
      r:={HEXATODEC(substr(HexNumber,-2));
         ,HEXATODEC(substr(HexNumber,5,2)),HEXATODEC(substr(HexNumber,3,2)) }
   endif
return r

/*
*/
*-----------------------------------------------------------------------------*
METHOD Hgconvert(ltxt) CLASS WREPORT
*-----------------------------------------------------------------------------*
   do case // TODO: SWITCH
      case valtype(&ltxt)$"MC" ; return iif("trans" $ lower(ltxt),ltxt,"FIELD->"+ltxt)
      case valtype(&ltxt)=="N" ; return "str(FIELD->"+ltxt+")"
      case valtype(&ltxt)=="D" ; return "dtoc(FIELD->"+ltxt+")"
      case valtype(&ltxt)=="L" ; return "if(FIELD->"+ltxt+","+Chr(34)+".T."+Chr(34)+","+Chr(34)+".F."+Chr(34)+")"
   endcase
return ""
/*
*/
*-----------------------------------------------------------------------------*
METHOD TheHead() CLASS WREPORT
*-----------------------------------------------------------------------------*
local grd, nkol
         if nPgr == mx_pg; last_pag:=.T. ;endif
         START PAGE
         nPgr ++ ; nPag ++ ; nline := 0
         // Top of Form //La Testa
         if (grdemo .OR. gcdemo) .AND. nPgr < 2
            hbprn:modifypen("*",0,0.1,{255,255,255})
            if grdemo
                for grd= 0 to ::mx_ln_doc -1
                    @grd,0 say grd to print
                    @grd+1,0,grd+1,hbprn:maxcol LINE
                next
            endif
            if gcdemo
               for nkol = 0 to hbprn:maxcol
                   @ 0,nKol,::mx_ln_doc,nkol line
                   if int(nkol/10)-(nkol/10) = 0
                      @0,nKol say [*] to print
                   endif
               next
            endif
         endif
         if ::aStat ["r_paint"]        // La testa
            aeval(::aHead,{|x,y|if(Y>1 ,::traduci(x[1],,x[2]),"")})
         endif
         nline := iif(nPgr =1,iif(flob < 1,eval(::Valore,::aHead[1])-1,flob),eval(::Valore,::aHead[1])-1)
         shd := .T.
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD TheBody() CLASS WREPORT
*-----------------------------------------------------------------------------*
local db_arc:=dbf(), noline:=.F., subcolor, nxtp :=.F., n, an, al
Local sstring := "NLINE"+chr(07)+NTrim(t_col)+chr(07)+"SAY"+chr(07)
      sstring += chr(05)+chr(07)+substr(ghstring,at("FONT",ghstring))
      if HB_ISARRAY(::argm[3])
         al := len(::argm[3])
         for an = 1 to al
             ::aCnt := an
             for N = 2 TO LEN(::aBody)
                 if ::traduci(::aBody[N,1],.F.,::aBody[N,2]) //n-1)
                     noline := .T.
                 endif
                 if "MEMOSAY" $ upper(::aBody[N,1])
                    ::aStat [ "ReadMemo" ] := ::aBody[n,1] + chr(07)+".F."
                 endif
             next

             if  ::aStat [ "Yes_Memo" ]    //memo Fields
                 ::traduci(::aStat [ "ReadMemo" ])
                 if !noline
                    nline ++
                 Endif
                 noline := .F.
                 an := al
             else
                 if !noline
                    nline ++
                 Endif
                 noline := .F.
             endif
             if an < al
                insgh := ( nline >= ::hb ) //head group checker
                // @0,0 say "**"+if(m->insgh =.T.,[.T.],[.F.])  FONT "F1" to print
                if nline >= ::HB-1
                   ::TheFeet()
                   ::TheHead()
                   nxtp := .T.
                endif
             else
                 eline := nline
                 if eline < ::HB
                    nline := ::HB -1
                 endif
                 last_pag := .T.
                 ::TheFeet(.T.)
             endif
          next
      Else
         do While iif(used(),! (dbf())->(Eof()),nPgr < ::aStat [ "end_pr" ] )
                  ::aStat [ "GHline" ] := if (sbt =.F.,sbt ,::aStat [ "GHline" ] )

                  if nxtp .AND. ::aStat [ "GHline" ] .AND. ::aStat ["r_paint"] .AND. sgh // La seconda pagina
                     ::traduci(Ghstring)
                     // @nline,0 say "**"+if(m->insgh =.T.,[.T.],[.F.])  FONT "F1" to print
                     nxtp := .F. ; nline ++
                  endif

                  if ::GrHead() //.AND. ::aStat [ "GHline" ]    // La testata
                     if ::aStat ["r_paint"] .AND. (shd .OR. sbt) .AND. sgh .AND. !insgh
                        ::traduci(Ghstring)
                        // @nline,0 say "@@"+if(m->insgh =.T.,[.T.],[.F.])  FONT "F1" to print
                        nxtp := .F. ; nline ++
                     endif
                     insgh:=.F.
                  else
                      for N = 2 TO LEN(::aBody)
                           if grdemo .OR. gcdemo
                              if ::aStat ["r_paint"]
                                 if ::traduci(::aBody[N,1],,n-1)
                                    noline := .T.
                                 endif
                                 if "MEMOSAY" $ upper(::aBody[N,1])
                                    ::aStat [ "ReadMemo" ] := ::aBody[n,1] + chr(07)+".F."
                                 endif
                              endif
                           else
                              if ::traduci(::aBody[N,1],.F.,::aBody[N,2]) //n-1)
                                 noline := .T.
                              endif
                              if "MEMOSAY" $ upper(::aBody[N,1])
                                 ::aStat [ "ReadMemo" ] := ::aBody[n,1] + chr(07)+".F."
                              endif
                           endif
                      next
                      // qui i conteggi

                      Aeval(GFtotal,{|x,y| counter[y] += (db_arc)->&(x)}) //,msgmulty((db_arc)->&(x),x)
                      if  ::aStat [ "Yes_Memo" ]    //memo Fields
                          ::traduci(::aStat [ "ReadMemo" ])
                          if !noline
                             nline ++
                          Endif
                          noline := .F.
                      else
                          if !noline
                             nline ++
                          Endif
                          noline := .F.
                          dbskip()
                      endif

                      if Gfexec        // Display the subtotal of group
                         if ::GFeet()
                            if gfline .AND. sbt
                               ::traduci(strtran(sstring,chr(05),s_total))
                               if ::aStat["InlineSbt"]= .F.
                                  nline ++
                               Endif
                               get textcolor to subcolor
                               set textcolor BLUE
                               // @nline,t_col say GFSTRING[1] to print    // ONLY FOR DEBUG!!!
                               Aeval(GFstring,{|x|::traduci(x)})
                               set textcolor subcolor
                               nline ++
                            endif

                            Aeval(counter,{|x,y| gcounter[y] += x })
                            if ::aStat [ "P_F_E_G" ]
                               eline := nline
                               ::TheFeet()
                               ::TheHead()
                            endif
                            afill(Counter,0)
                         endif
                      endif

                      if !eof()
                         insgh := ( nline >= ::hb ) //head group checker
                         // @0,0 say "**"+if(m->insgh =.T.,[.T.],[.F.])  FONT "F1" to print
                         if nline >= ::HB-1
                            ::TheFeet()
                            ::TheHead()
                            nxtp := .T.
                         endif
                      else
                          if Gfexec  //len(gcounter) != 0     //display group total
                             if len(m->tts) > 0
                                ::traduci(strtran(sstring,chr(05),m->tts))
                                if ::aStat["InlineTot"]= .F.
                                   NLINE ++
                                Endif
                                Aeval(GTstring,{|x|::traduci(x)})
                                nline++
                                nline++
                             endif
                          endif
                          eline:=nline
                          if eline < ::HB
                             nline := ::HB -1
                          endif
                          last_pag:=.T.
                          ::TheFeet(.T.)
                          afill(GCounter,0)
                      endif
                  endif
          enddo
      Endif
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD TheFeet(last) CLASS WREPORT            //Feet // IL Piede
*-----------------------------------------------------------------------------*
// exclude:= FALSE
   default last to .F.
   if !last_pag
      eline := nline // if (eval(::Valore,::aBody[1])+eval(::Valore,::aBody[1]) < nline,nline,eline)
   endif
   aeval(::aFeet,{|x,y|if(Y>1 ,::traduci(x[1],iif(!(grdemo .OR. gcdemo),"",.F.),x[2]),"")})
   last_pag := last
   Last := .T.
   if ::PrnDrv = "HBPR"
      End PAGE
   Else
      if ( _HMG_MINIPRINT [23] == .T. , _HMG_PRINTER_ENDPAGE_PREVIEW (_HMG_MINIPRINT [19]) , _HMG_PRINTER_ENDPAGE ( _HMG_MINIPRINT [19] ) )
   Endif
   if last
      nPgr := 0
      ::aStat [ "EndDoc" ] := .T.
      ONEATLEAST := .F.
   endif
return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD Quantirec(_MainArea) CLASS WREPORT     //count record that will be print
*-----------------------------------------------------------------------------*
local conta:=0 , StrFlt :=""
Private query_exp
StrFlt := ::aStat [ "FldRel" ]+" = "+ ::aStat [ "area1" ]+"->"+::aStat [ "FldRel" ]
if HB_ISARRAY(::argm[3])    // {_MainArea,_psd,db_arc,_prw}
  Return len(::argm[3])
Endif
if !EMPTY(dbfilter())
   query_exp := dbfilter()
   DBGOTOP()
   if !empty(_MainArea)
      // msgbox(StrFlt)
      **count to conta FOR &StrFlt
      count to conta FOR &(::aStat [ "FldRel" ]) = (::aStat [ "area1" ])->&(::aStat [ "FldRel" ])
      //msgbox([conta= ]+zaps(conta)+CRLF+" "+CRLF+query_exp,[Trovati Cxx])
   else
      if left(query_exp,3)=="{||"  // codeblock
         DBEval( {|| conta  ++ }, &(query_exp) ,,,, .F. )
      Else
         DBEval( {|| conta  ++ }, {||&query_exp  } ,,,, .F. )
      Endif
   endif
   DBGOTOP()
else
   if valtype(::nrec)=="C"
      ::rec := val(::nrec)
   endif
   conta := If (::NREC < 1, lastrec(), ::NREC )
endif
// msgbox(+zaps(conta)+" per ["+query_exp+"]",[step 3])
return conta
/*
*/
*-----------------------------------------------------------------------------*
METHOD JustificaLinea(WPR_LINE,WTOPE) CLASS WREPORT
*-----------------------------------------------------------------------------*
LOCAL I, SPACE1 := SPACE(1)
LOCAL WLARLIN := LEN(TRIM(WPR_LINE))
FOR I=1 TO WLARLIN
   IF WLARLIN = WTOPE
      EXIT
   ENDIF
   IF SUBSTR(WPR_LINE,I,1)=SPACE1 .AND. SUBSTR(WPR_LINE,I-1,1)!=SPACE1 .AND. SUBSTR(WPR_LINE,I+1,1)!=SPACE1
      WPR_LINE := LTRIM(SUBSTR(WPR_LINE,1,I-1))+SPACE(2)+LTRIM(SUBSTR(WPR_LINE,I+1,LEN(WPR_LINE)-I))
      WLARLIN++
   ENDIF
NEXT I
RETURN WPR_LINE
/*
*/
*-----------------------------------------------------------------------------*
Method TheMiniHead() Class Wreport
*-----------------------------------------------------------------------------*
   if nPgr == mx_pg ; last_pag:=.T. ; endif
   START PRINTPAGE
      nPgr ++
      nPag ++
      nline := 0
      if oWr:aStat [ "r_paint" ]        // La testa
         aeval(oWr:aHead,{|x,y|if(Y>1 ,oWr:traduci(x[1],,x[2]),"")})
      endif

      nline := iif(nPgr =1,iif(flob < 1,eval(oWr:Valore,oWr:aHead[1])-1,flob),eval(oWr:Valore,oWr:aHead[1])-1)

return nil
/*
*/
*-----------------------------------------------------------------------------*
METHOD TheMiniBody() class Wreport
*-----------------------------------------------------------------------------*
local db_arc:=dbf(), noline:=.F., nxtp :=.F.
local oErrAntes, oErr, lMyError := .F., n , al ,an
   BEGIN SEQUENCE
      // buono ma da rivedere Yes_Memo  := .F.
      if HB_ISARRAY(::argm[3])
         al := len(::argm[3])
         for an = 1 to al
             ::aCnt := an
             for N = 2 TO LEN(::aBody)
                 if ::traduci(::aBody[N,1],.F.,::aBody[N,2]) //n-1)
                     noline := .T.
                 endif
                 if "MEMOSAY" $ upper(::aBody[N,1])
                    ::aStat [ "ReadMemo" ] := ::aBody[n,1] + chr(07)+".F."
                 endif
             next

             if  ::aStat [ "Yes_Memo" ]    //memo Fields
                 ::traduci(::aStat [ "ReadMemo" ])
                 if !noline
                    nline ++
                 Endif
                 noline := .F.
                 an := al
             else
                 if !noline
                    nline ++
                 Endif
                 noline := .F.
             endif
             if an < al
                insgh := ( nline >= ::hb ) //head group checker
                // @0,0 say "**"+if(m->insgh =.T.,[.T.],[.F.])  FONT "F1" to print
                if nline >= ::HB-1
                   ::TheFeet()
                   ::TheMiniHead()
                   nxtp := .T.
                endif
             else
                 eline := nline
                 if eline < ::HB
                    nline := ::HB -1
                 endif
                 last_pag := .T.
                 ::TheFeet(.T.)
             endif
          next
      Else

         do While iif(used(),! (dbf())->(Eof()),nPgr<oWr:aStat [ "end_pr" ])
                  if nxtp .AND. ::aStat [ "GHline" ] .AND. ::aStat [ "r_paint" ]
                     ::traduci(Ghstring)
                     nxtp := .F.
                     nline ++
                  endif
                  if ::GrHead() .AND. ::aStat [ "GHline" ]
                     if ::aStat [ "r_paint" ]
                        ::traduci(Ghstring)
                        nxtp := .F.
                     endif
                     nline ++
                     insgh:=.F.
                  else
                      for N= 2 TO LEN(oWr:aBody)
                           if grdemo .OR. gcdemo
                              if ::aStat [ "r_paint" ]
                                 if ::traduci(oWr:aBody[N,1],,n-1)
                                    noline := .T.
                                 endif
                                 if "MEMOSAY" $ upper(oWr:aBody[N,1])
                                    ::aStat [ "ReadMemo" ] := oWr:aBody[n,1] + chr(07)+".F."
                                 endif
                              endif
                           else
                              if ::traduci(oWr:aBody[N,1],.F.,oWr:aBody[N,2]) //n-1)
                                 noline := .T.
                              endif
                              if "MEMOSAY" $ upper(oWr:aBody[N,1])
                                 ::aStat [ "ReadMemo" ] := oWr:aBody[n,1] + chr(07)+".F."
                              endif
                           endif
                      next
                      // qui i conteggi
                      Aeval(GFtotal,{|x,y| counter[y] += (db_arc)->&(x)})
                      if ::aStat [ "Yes_Memo" ]  //memo Fields
                          ::traduci(oWr:aStat [ "ReadMemo" ])
                          if !noline
                             nline ++
                          Endif
                          noline := .F.
                      else
                          if !noline
                             nline ++
                          Endif
                          noline := .F.
                          dbskip()
                      endif

                      if Gfexec        // Display the subtotal of group
                         if ::GFeet()
                            if gfline
                               @nline,t_col PRINT iif(oWr:aStat [ "r_paint" ],s_total,[]) COLOR BLUE
                               if oWr:aStat["InlineSbt"]= .F.
                                  nline ++
                               Endif
                               // @nline,t_col say GFSTRING[1] to print    // ONLY FOR DEBUG!!!
                               Aeval(GFstring,{|x|oWr:traduci(x)})
                               nline ++
                            endif
                            Aeval(counter,{|x,y| gcounter[y] += x })
                            if ::aStat [ "P_F_E_G" ]
                               eline := nline
                               ::TheFeet()
                               ::TheMiniHead()
                            endif
                            afill(Counter,0)
                         endif
                      endif

                      if !eof()
                          insgh := ( nline >= oWr:HB ) //head group checker
                          if nline >= oWr:HB-1
                             ::TheFeet()
                             ::TheMiniHead()
                             nxtp := .T.
                          endif
                      else
                          if Gfexec  //.AND. gcounter != 0     //display total of group
                             if len(m->tts) > 0
                                @nline,t_col PRINT iif(oWr:aStat [ "r_paint" ],[Totale],[])
                                if ::aStat["InlineTot"]= .F.
                                   NLINE ++
                                Endif
                                Aeval(GTstring,{|x|oWr:traduci(x)})
                                nline++
                                nline++
                             endif
                          endif
                          eline:=nline
                          if eline < oWr:HB
                             nline:=oWr:HB -1
                          endif
                          last_pag:=.T.
                          ::TheFeet(.T.)
                          afill(GCounter,0)
                      endif
                  endif
          enddo
      Endif
      RECOVER USING oErr
      if oErr != NIL
         lMyError := .T.
         MyErrorFunc(oErr)
      endif

   END
   ERRORBLOCK(oErrAntes)
   if lMyError
      MsgBox("Ocurrio un error y el sintema no pudo completar la operacion")
   endif
return nil
