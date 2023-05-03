
#include "minigui.ch"
#include "miniprint.ch"
#include "hbclass.ch"

#TRANSLATE MSG        => MSGBOX
#TRANSLATE ZAPS(<X>) => ALLTRIM(STR(<X>))
#define NTRIM(n) LTrim(Str(n))
#TRANSLATE Test( <c> ) => MsgInfo( <c>, [<c>] )
#define MsgInfo( c ) MsgInfo( c, , , .F. )
#define MsgAlert( c ) MsgEXCLAMATION( c, , , .F. )
#define MsgStop( c ) MsgStop( c, , , .F. )

#define MGSYS  .F.

memvar nomevar,hbprn
memvar _money
memvar _separator
memvar _euro
memvar epar
memvar chblk
memvar oneatleast, shd, sbt, sgh,insgh
memvar gcounter
memvar counter
memvar gcdemo
memvar grdemo
memvar align
memvar GHstring, GFstring, GTstring
memvar GField
memvar s_head
memvar last_pag
memvar s_col, t_col,  wheregt
memvar gftotal, Gfexec, s_total
memvar nline
memvar nPag, nPgr, Tpg
memvar eLine, GFline
memvar maxrow, maxcol, mncl, mxH
memvar abort
memvar flob
memvar Cal
memvar Cxx
memvar _pW
memvar _pH
memvar lstep, atf, mx_pg

//memvar nlinepart
memvar _varmem, _varexec, _aalign

memvar LPAPERLENGTH
memvar LPAPERWIDTH
memvar LCOPIES
memvar LDEFAULTSOURCE
memvar LQUALITY
memvar LCOLOR
memvar LDUPLEX
memvar LCOLLATE
memvar NCOLLATE
memvar LPAPERSIZE

memvar oWr

*-----------------------------------------------------------------------------*
* Printing Procedure                //La Procedura di Stampa
*-----------------------------------------------------------------------------*
Procedure PrMiniEsegui(_MainArea,_psd,db_arc,_prw)
*-----------------------------------------------------------------------------*
         Local oldrec   := recno()
         local landscape:=.F.
         local lpreview :=.F.
         local lselect  :=.F.
         local str1:=[]
         local ncpl , nfsize
         local condition:=[]
         local aprinters
         local StrFlt:=""
         local lbody := 0, miocont := 0, miocnt := 0
         local Amx_pg :={}
         Private ONEATLEAST := .F., shd := .T., sbt := .T., sgh := .T., insgh:=.F.
         Private hbprn :=hbprinter():new()

         chblk  :={|x,y|if(ascan(x,y)>0,iif(len(X)>ascan(x,y),x[ascan(x,y)+1],""),"")}
         if !empty(_MainArea)
             oWr:aStat [ "area1" ]  :=substr(_MainArea,at("(",_MainArea)+1)
             oWr:aStat [ "FldRel" ] :=substr(oWr:aStat [ "area1" ],at("->",oWr:aStat [ "area1" ])+2)
             oWr:aStat [ "FldRel" ] :=substr(oWr:aStat [ "FldRel" ],1,iif(at(")",oWr:aStat [ "FldRel" ])>0,at(")",oWr:aStat [ "FldRel" ])-1,len(oWr:aStat [ "FldRel" ]))) //+(at("->",oWr:aStat [ "area1" ])))
             oWr:aStat [ "area1" ]  :=left(oWr:aStat [ "area1" ],at("->",oWr:aStat [ "area1" ])-1)
         else
             oWr:aStat [ "area1" ]:=dbf()
             oWr:aStat [ "FldRel" ]:=""
         endif

         aprinters := aprinters()

         Private counter   := {}  , Gcounter  := {}

         Private grdemo    := .F. , Gcdemo    := .F.

         Private Align     :=  0  , GHstring  := ""
         Private GFstring  := {}  , GTstring  := {}
         Private GField    := ""  , S_head    := ""

         Private s_col     :=  0  , Gftotal   := {}
         Private Gfexec    := .F. , S_total   := ""
         Private t_col     :=  0  , nline     := oWr:mx_ln_doc
         Private nPag      :=  0  , mx_pg     := 0
         Private nPgr      :=  0  , Tpg       := 0
         Private last_pag  := .F. , eLine     :=  0
         Private wheregt   :=  0  , GFline    := .F.
         Private abort     := 0
         Private maxcol    := 0,  maxrow :=0, mncl :=0, mxH :=0
//         Private _addoffset:=.F.

         ncpl := eval(oWr:Valore,oWr:Adeclare[1])
         str1 := upper(substr(oWr:Adeclare[1,1],at("/",oWr:Adeclare[1,1])+1))

         if "LAND" $ Str1 ;landscape:=.T.; endif
         if "SELE" $ Str1 ;lselect :=.T. ; endif
         if "PREV" $ Str1
            lpreview:=.T.
         else
            lpreview := _prw
         endif

         str1 := upper(substr(oWr:ABody[1,1],at("/",oWr:aBody[1,1])+1))
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
            SELECT PRINTER DIALOG PREVIEW
         endif
         if lselect .AND. (!lpreview)
            SELECT PRINTER DIALOG
         endif
         if !lselect .AND. lpreview
            if ascan(aprinters,_PSD) > 0
               SELECT PRINTER _PSD PREVIEW
            else
               SELECT PRINTER DEFAULT PREVIEW
            endif
         endif
         if !lselect .AND. !lpreview
            if ascan(aprinters,_PSD) > 0
               SELECT PRINTER _PSD
            else
               SELECT PRINTER default
            endif
         endif
         //msginfo(zaps(GETPRINTABLEAREAHORIZONTALOFFSET())+crlf+zaps(GETPRINTABLEAREAVERTICALOFFSET()),"H_offset")

         IF iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) == 0
            r_mem()
            return
         ENDIF
         if !MGSYS
           _HMG_SYSDATA [ 374 ]:=_hmg_printer_hdc
         endif
         mncl:=round(( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX ( _HMG_SYSDATA [ 374 ] ) / _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX ( _HMG_SYSDATA [ 374 ] ) * 25.4 ),2)

         Cal :=(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY ( _HMG_SYSDATA [ 374 ] ) / _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY ( _HMG_SYSDATA [ 374 ] ) )* 25.4 //,2)
         Cxx :=(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX ( _HMG_SYSDATA [ 374 ] ) / _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX ( _HMG_SYSDATA [ 374 ] ) )* 25.4 //,2)

         _pW :=round((2*cxx)+_HMG_PRINTER_GETPRINTERWIDTH(_HMG_SYSDATA [ 374 ]),0)
         _pH :=round((2*cal)+_HMG_PRINTER_GETPRINTERHEIGHT(_HMG_SYSDATA [ 374 ]),0)
         mncl:=round(mncl,2)

/*
mkm:=str(cxx)
Test(mkm)
         msgt(;
         "Largo "+zaps(_HMG_PRINTER_GETPRINTERWIDTH(_HMG_SYSDATA [ 374 ]))+crlf+;
         "Largo Fisico "+zaps(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALWIDTH(_HMG_SYSDATA [ 374 ]))+crlf+;
         "Larghezza pagina "+zaps(_pw)+crlf+;
         "Altezza  Pagina "+zaps(_pH)+crlf+;
         "Alto Printer "+zaps(_HMG_PRINTER_GETPRINTERHEIGHT(_HMG_SYSDATA [ 374 ]))+crlf+;
         "Alto Fisico "+zaps(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALHEIGTH(_HMG_SYSDATA [ 374 ]))+crlf+;
         "Alto Margine SUP "+zaps(cal)+crlf+;
         "Largo Margine Sx         "+zaps(cxx)+crlf+;
         "Offset sx "+zaps(mncl)+crlf+;
         "PHYSICALOFFSET X "+zaps(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX(_HMG_SYSDATA [ 374 ]))+crlf+;
         "AREALOGPIXELSX   "+zaps(_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX(_HMG_SYSDATA [ 374 ]))+crlf+;
         "PHYSICALOFFSET Y "+zaps(_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY (_HMG_SYSDATA [ 374 ]))+crlf+;
         "AREALOGPIXELSY Y "+zaps(_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY (_HMG_SYSDATA [ 374 ])),"Valori")
*/
         mxH := _HMG_PRINTER_GETPAGEHEIGHT(iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc))
         maxrow  := int(mxh/LStep)
         maxcol  := int(_HMG_PRINTER_GETPAGEWIDTH(iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc))/1.1811)
//   msginfo(str(mxH)+" =mxH"+CRLF+str(maxrow)+" =maxrow"+crlf+str(maxcol)+" =maxcol"+crlf+str(maxcol)+" =maxcol","PUTTY")

         aeval(oWr:adeclare,{|x,y|if(Y>1 ,oWr:traduci(x[1],,x[2]),"")})
         if abort != 0
            r_mem()
            return
         endif

         if used()
            if !empty(atf)
               //msg(atf,[atf])
               set filter to &atf
            endif
            oWr:aStat [ "end_pr" ] := oWr:quantirec(_mainarea)
         else
            oWr:aStat [ "end_pr" ] := oWr:quantirec(_mainarea)
         endif
         //msg(zaps(mx_pg)+CRLF+[oWr:Valore= ]+zaps(eval(oWr:Valore,oWr:aBody[1]))+CRLF+zaps(oWr:aStat [ "end_pr" ]),[tutte])

         START PRINTDOC NAME _HMG_SYSDATA [ 358 ]

         if empty(_MainArea)
            Lbody := eval(oWr:Valore,oWr:aBody[1])
            mx_pg := INT(oWr:aStat[ "end_pr" ]/NOZERODIV(Lbody) )
            if (mx_pg * lbody) != mx_pg
               mx_pg ++
            endif
            mx_pg :=ROUND( max(1,mx_pg), 0 )
            tpg := mx_pg
            if valtype(oWr:argm[3]) != "A"
               Dbgotop()
            Endif
            if oWr:aStat [ "end_pr" ] != 0
               while !oWr:aStat [ "EndDoc" ]
                     oWr:TheMiniHead()
                     oWr:TheMiniBody()
               enddo
            Endif
         else
            sele (oWr:aStat [ "area1" ])
            if !empty(atf)
               set filter to &atf
            endif
            Dbgotop()
            lbody:=eval(oWr:Valore,oWr:aBody[1])
            while !eof()
                  sele (DB_ARC)
                  StrFlt:= oWr:aStat [ "FldRel" ]+" = "+ oWr:aStat [ "area1" ]+"->"+oWr:aStat [ "FldRel" ]
                  DBEVAL( {|| miocont++},{|| &strFLT} )

                  // Vecchia versione
                  // miocnt:=miocont/NOZERODIV(eval(oWr:Valore,oWr:aBody[1]))*10
                  miocnt:= int(miocont/NOZERODIV(lbody))
                  if (miocnt * lbody) != miocont
                     miocnt ++
                  endif
                  tpg += miocnt
                  //msg(zaps(tpg)+crlf+zaps(miocnt),[Tpg1])
                  aadd(Amx_pg,miocnt)
                  miocont := 0
                  sele (oWr:aStat [ "area1" ])
                  dbskip()
            enddo
            go top
            while !eof()
                 sele (DB_ARC)
                 set filter to &strFLT
                 miocont ++
                 mx_pg:=aMx_pg[miocont]
                 go top
                 nPgr:=0
                 while !eof()
                       oWr:TheMiniHead()
                       oWr:TheMiniBody()
                 enddo
                 oWr:aStat [ "EndDoc" ]:=.F.
                 last_pag:=.F.
                 set filter to
                 sele (oWr:aStat [ "area1" ])
                 dbskip()
            enddo
         endif
         if oneatleast
            go top
            oWr:TheMiniHead()
            oWr:TheMiniBody()
         endif

         END PRINTDOC

         if used();dbgoto(oldrec);endif
         r_mem(.T.)
Return

/*
*-----------------------------------------------------------------------------*
Static Func addline()
*-----------------------------------------------------------------------------*
local ritorno:=.F.
      nlinepart ++
      if nlinepart >= oWr:mx_ln_doc    // gestione del salto pagina
         // @ (hbprn:maxrow)-1,(hbprn:maxcol)-10 PRINT  "Pagina "+ltrim(str(npag)) font ARIAL SIZE 12 RIGHT
         npag ++
         mx_pg ++
         END PRINTPAGE
         START PRINTPAGE
         ritorno:=.T.
      endif
return ritorno
*/
*-----------------------------------------------------------------------------*
Static Function memosay(arg1,arg2,argm1,argl1,argf1,argsize,abold,aita,aunder,astrike,argcolor1,argalign,onlyone)
*-----------------------------------------------------------------------------*
 local _Memo1:=argm1, k, mcl ,maxrow:=max(1,mlcount(_memo1,argl1))
 local arrymemo:={} , esci:=.F. ,str :="" , ain, typa := .F.
 default arg2 to 0 , arg1 to 0 , argl1 to 10, onlyone to "", argalign to "LEFT"

 if HB_ISARRAY(argm1)
    typa := .T.
    arrymemo := {}
    if oWr:IsMono(argm1)
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
    for k:=1 to maxrow
        aadd(arrymemo,oWr:justificalinea(memoline(_memo1,argl1,k),argl1))
    next
 Endif
 if empty(onlyone)
    _HMG_PRINTER_H_PRINT( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
   , arg1 ;
   , arg2  ;
   , argf1 ;
   , argsize ;
   , argcolor1[1] ;
   , argcolor1[2] ;
   , argcolor1[3] ;
   , arrymemo[1] ;
   , abold;
   , aita;
   , aunder;
   , astrike;
   , iif(HB_ISARRAY(argcolor1), .T.,.F.) ;
   , iif(valtype(argf1)=="C", .T.,.F.) ;
   , iif(HB_ISNUMERIC(argsize), .T.,.F.) ;
   , argalign )
   oWr:aStat [ "Yes_Memo" ] :=.T.
 else
     for mcl=2 to len(arrymemo)
         nline ++
         if nline >= oWr:HB-1
            oWr:TheFeet()
            oWr:TheMiniHead()
         endif
         _HMG_PRINTER_H_PRINT( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
         , nline*lstep , arg2, argf1 , argsize , argcolor1[1], argcolor1[2], argcolor1[3] ;
         , arrymemo[mcl], abold, aita, aunder, astrike;
         , iif(HB_ISARRAY(argcolor1), .T.,.F.) ;
         , iif(valtype(argf1)=="C", .T.,.F.) ;
         , iif(HB_ISNUMERIC(argsize), .T.,.F.) ;
         , argalign )
     next
     if !Typa
        dbskip()
     Endif
 endif
 return nil
/*
*/
*-----------------------------------------------------------------------------*
Function RMiniPar(ArryPar,cmdline,section)
*-----------------------------------------------------------------------------*
     local _arg1,Aclr,blse := {|x| iif(val(x)> 0,.T.,iif(x=".T.".OR. x ="ON",.T.,.F.))}
     local ax := {}

     if len(ArryPar) < 1 ;return .F. ;endif

     maxrow  := int(_HMG_PRINTER_GETPAGEHEIGHT(iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc))/LStep)
     maxcol  := int(_HMG_PRINTER_GETPAGEWIDTH(iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc))/1)
     /*
     msginfo(zaps(GETPRINTABLEAREAHORIZONTALOFFSET())+crlf+zaps(GETPRINTABLEAREAVERTICALOFFSET());
     +crlf+"Largo = "+zaps(GETPRINTABLEAREAWIDTH())+crlf+"Alto = "+zaps(GETPRINTABLEAREAHEIGHT()),"H_offset")
     */
//     msgmulty(arrypar)
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
                &_varmem := oWr:MACROCOMPILE("("+ArryPar[3]+")",.T.,cmdline,section)
             Endif

        case arryPar[1]==[ADDLINE]
             nline ++

        case arryPar[1]==[SUBLINE]
             nline --

        case len(ArryPar)=1
             if "DEBUG_" != left(ArryPar[1],6) .AND. "ELSE" != left(ArryPar[1],4)
                 oWr:MACROCOMPILE(ArryPar[1],.T.,cmdline,section)
             Endif

        case ascan(arryPar,[ONEATLEAST])= 2
             ONEATLEAST :=eval(blse,arrypar[3]) 

        case ascan(arryPar,[DEBUG])=2
             if ascan(arryPar,[LIST])= 3
                if npag < 2 .AND. len(arrypar) = 4
                   asize(ax,0)
                   do case
                      case ascan(arryPar,[DECLARE])= 4
                           aeval(oWr:aDeclare,{|x|if (x != NIL,aadd(ax, strzero(x[2], 4) + ") " + x[1] ),nil)} )

                      case ascan(arryPar,[HEAD])= 4
                           aeval( oWr:aHead,{|x|if (x != NIL,aadd(ax, strzero(x[2], 4) + ") " + x[1] ),nil)} )

                      case ascan(arryPar,[BODY])= 4
                           aeval( oWr:aBody,{|x|if (x != NIL,aadd(ax, strzero(x[2], 4) + ") " + x[1] ),nil)} )

                      case ascan(arryPar,[FEET])= 4
                           aeval( oWr:aFeet,{|x|if (x != NIL,aadd(ax, strzero(x[2], 4) + ") " + x[1] ),nil)} )

                   endcase
                   msgmulty(ax)
                   asize(ax,0)
                 endif
             Else
               oWr:aStat [ "Control" ] := eval(blse,arrypar[3])
             Endif

        case ascan(arryPar,[MAXROW]) > 0

        case ArryPar[1]+arryPar[2]="SELECTPRINTER"

                do case

                   case ascan(ArryPar,[DIALOG])=3               //OK
                       _hmg_printer_aPrinterProperties := _HMG_PRINTER_PrintDialog()

                   otherwise
               //if(MGSYS,_HMG_SYSDATA [ 375 ],_hmg_printer_name) := GetDefaultPrinter()
               _arg1:=eval(chblk,arrypar,[QUALITY])
               lQuality:=oWr:what_ele(_arg1,oWr:aCh,"_aQlt")
               // msgbox("|"+str(lquality)+"|","quality")
               _arg1:=eval(chblk,arrypar,[COLOR])
               * msgbox("|"+_arg1+"|","quality")
               lColor:= oWr:what_ele(_arg1,oWr:aCh,"_acolor")
               //msgbox("|"+str(lcolor)+"|","color")
               lCopies:=val(eval(chblk,arrypar,[COPIES]))
               //msgbox("|"+str(lcopies)+"|","copie")
               _arg1:=eval(chblk,arrypar,[DUPLEX])
               lDuplex :=oWr:what_ele(_arg1,oWr:aCh,"_aDuplex")
               //msgbox("|"+str(lduplex)+"|","Duplex")
               lPaperSize:=val(eval(chblk,arrypar,[PAPERSIZE]))
               //msgbox("|"+str(lpapersize)+"|","papersize")
               lPaperLength:=val(eval(chblk,arrypar,[PAPERLENGTH]))
               //msgbox("|"+str(lpaperlength)+"|","paperleng")
               lPaperWidth:=val(eval(chblk,arrypar,[PAPERWIDTH]))
               //msgbox("|"+str(lpaperwidth)+"|","paperW")
               lDefaultSource:=val(eval(chblk,arrypar,[DEFAULTSOURCE]))
               //msgbox("|"+str(ldefaultsource)+"|","DEf Source")
               lCollate:=val(eval(chblk,arrypar,[COLLATE]))
               //msgbox("|"+str(lcollate)+"|","Collate")
               _arg1:= eval(chblk,arrypar,[PRINTER])
               _arg1:=if(_arg1 ="",GetDefaultPrinter(),_arg1)
               //msgbox(_arg1,"stampante")
               _hmg_printer_aPrinterProperties:=_HMG_PRINTER_SetPrinterProperties ( ;
                if(ascan(ArryPar,[DEFAULT])=3,GetDefaultPrinter(),_arg1 ), ;
                if(ascan(arryPar,[ORIENTATION])!= 0,iif(val(_arg1) > 0,val(_arg1),iif([PORT]$ _arg1,1,2)) ,-999),;
                if ( lPaperSize     > 0 , lPaperSize      , -999 ) , ;
                if ( lPaperLength   > 0 , LPaperLength    , -999 ) , ;
                if ( lPaperWidth    > 0 , LPaperWidth     , -999 ) , ;
                if ( lCopies        > 0 , lCopies         , -999 ) , ;
                if ( lDefaultSource > 0 , LDefaultSource  , -999 ) , ;
                if ( lQuality      != 0 , lQuality        , -999 ) , ;
                if ( lColor         > 0 , lColor          , -999 ) , ;
                if ( lDuplex        > 0 , lDuplex         , -999 ) , ;
                if ( lCollate       > 0 , nCollate        , -999 ) )
               //msgbox(str(lcopies))

                EndCase
                if MGSYS
                  _HMG_SYSDATA [ 374 ] := _hmg_printer_aPrinterProperties [1]
                  _HMG_SYSDATA [ 375 ] := _hmg_printer_aPrinterProperties [2]
                  _HMG_SYSDATA [ 376 ] := _hmg_printer_aPrinterProperties [3]
                  _HMG_SYSDATA [ 377 ] := _hmg_printer_aPrinterProperties [4]
                  _HMG_SYSDATA [ 378 ] := if (ascan(ArryPar,[PREVIEW]) > 0 ,.T.,_HMG_SYSDATA [ 378 ])
                else
                  _hmg_printer_hdc       := _hmg_printer_aPrinterProperties [1]
                  _hmg_printer_name      := _hmg_printer_aPrinterProperties [2]
                  _hmg_printer_copies    := _hmg_printer_aPrinterProperties [3]
                  _hmg_printer_collate   := _hmg_printer_aPrinterProperties [4]
                  _hmg_printer_preview   :=  if (ascan(ArryPar,[PREVIEW]) > 0 ,.T.,_HMG_MINIPRINT [23] )
                endif
                _hmg_printer_InitUserMessages()
          if MGSYS
             _HMG_SYSDATA [ 379 ]  := strzero(Seconds() * 100,8 )
          else
            _hmg_printer_timestamp := strzero(Seconds() * 100,8 )
          endif

          if ascan(ArryPar,[TO])= 4
             _varmem:=ArryPar[5]
             if __mvexist(ArryPar[5])
                 //msginfo("Private BHX")
                &_varmem := if ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) != 0 , .T. , .F. )
             else
                Public &_varmem
                &_varmem := if ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) != 0 , .T. , .F. )
             endif
          endif
          //msgbox(_hmg_printer_timestamp,"timestamp")

     case ascan(arryPar,"SET")=1
             do case
                case ascan(arryPar,[EURO])=2
                     _euro:= eval(blse,arrypar[3]) // iif(val(arrypar[3])> 0,.T.,iif(arrypar[3]=".T.".OR.arrypar[3]="ON",.T.,.F.))
/*
                case ascan(arryPar,[ADDOFFSET])=2
                     _addoffset:= eval(blse,arrypar[3]) // iif(val(arrypar[3])> 0,.T.,iif(arrypar[3]=".T.".OR.arrypar[3]="ON",.T.,.F.))
*/

                case ascan(arryPar,[MONEY])=2
                     _money:= eval(blse,arrypar[3]) // iif(val(arrypar[3])> 0,.T.,iif(arrypar[3]=".T.".OR.arrypar[3]="ON",.T.,.F.))

                case ascan(arryPar,[SEPARATOR])=2
                     _separator:= eval(blse,arrypar[3]) // iif(val(arrypar[3])> 0,.T.,iif(arrypar[3]=".T.".OR.arrypar[3]="ON",.T.,.F.))

                case ascan(arryPar,[PREVIEW])=2
                     _hmg_printer_preview:= eval(blse,arrypar[3]) // iif(val(arrypar[3])> 0,.T.,iif(arrypar[3]=".T.".OR.arrypar[3]="ON",.T.,.F.))

                case ascan(arryPar,[JOB])= 2
                     _HMG_SYSDATA [ 358 ]:= eval(chblk,arrypar,[NAME])

                case arryPar[2]+arryPar[3] == "COPIETO"
                     _HMG_SYSDATA [ 376 ]:= val(eval(chblk,arrypar,[TO]))
          endcase

     case ascan(ArryPar,[PRINT])=3 .OR. ascan(ArryPar,[SAY])= 3
             Aclr := oWr:UsaColor(eval(chblk,arrypar,[COLOR]))
              //AEVAL(Aclr,{|X| MSGINFO(X,"Colore")})
             do case
                case ASCAN(ArryPar,[IMAGE]) > 0
                // msgexclamation(zaps(GETPRINTABLEAREAHORIZONTALOFFSET()))
                    //msg("IMAGE"+crlf+eval(chblk,arrypar,[IMAGE]))
                   _HMG_PRINTER_H_IMAGE ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
                   , eval(chblk,arrypar,[IMAGE]);
                   , iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                   , eval(epar,ArryPar[2]) ;
                   , val(eval(chblk,arrypar,[HEIGHT]));
                   , val(eval(chblk,arrypar,[WIDTH]));
                   , iif(ascan(ArryPar,[STRETCH])> 0,.T.,.F.))
                     // _HMG_PRINTER_IMAGE (iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc),"hmglogo.gif",25,25,20,16 )

                case ASCAN(ArryPar,[LINE]) > 0
                      // 1: hDC
                      // 2: y
                      // 3: x
                      // 4: toy
                      // 5: tox
                      // 6: width
                      // 7: R Color
                      // 8: G Color
                      // 9: B Color
                      // 10: lWindth
                      // 11: lColor
                      // @ 260,20 PRINT LINE TO 260,190 ==
                      //_HMG_PRINTER_LINE (iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc),260,20,260,190,,"1","2","3",.F.,.F. )

                    //Aclr:= color(eval(chblk,arrypar,[COLOR]))
                     _HMG_PRINTER_H_LINE ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
                     , iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                     , eval(epar,ArryPar[2]) ;
                     , eval(epar,ArryPar[6]) ;
                     , eval(epar,ArryPar[7]) ;
                     , val(eval(chblk,arrypar,[PENWIDTH])) ;
                     , Aclr[1] ;
                     , Aclr[2] ;
                     , Aclr[3] ;
                     , iif(ascan(arryPar,[PENWIDTH])>0, .T.,.F.);
                     , iif(ascan(arryPar,[COLOR])>0, .T.,.F.) )

                case ASCAN(ArryPar,[RECTANGLE]) > 0
                     // @ 20,20 PRINT RECTANGLE TO 50,190 ==
                     //_HMG_PRINTER_RECTANGLE (iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc),20,20,50,190,,"1","2","3",.F.,.F. )

                     //      Aclr:= color(eval(chblk,arrypar,[COLOR]))

                     if ASCAN(ArryPar,[ROUNDED])> 0
                        _HMG_PRINTER_H_ROUNDRECTANGLE ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
                        , iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                        , eval(epar,ArryPar[2]) ;
                        , eval(epar,ArryPar[6]) ;
                        , eval(epar,ArryPar[7]) ;
                        , val(eval(chblk,arrypar,[PENWIDTH])) ;
                        , Aclr[1] ;
                        , Aclr[2] ;
                        , Aclr[3] ;
                         , iif(ascan(arryPar,[PENWIDTH])>0, .T.,.F.);
                        , iif(ascan(arryPar,[COLOR])>0, .T.,.F.) )
                     else
                        _HMG_PRINTER_H_RECTANGLE ( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
                        , iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                        , eval(epar,ArryPar[2]) ;
                        , eval(epar,ArryPar[6]) ;
                        , eval(epar,ArryPar[7]) ;
                        , val(eval(chblk,arrypar,[PENWIDTH])) ;
                        , Aclr[1] ;
                        , Aclr[2] ;
                        , Aclr[3] ;
                        , iif(ascan(arryPar,[PENWIDTH])>0, .T.,.F.);
                        , iif(ascan(arryPar,[COLOR])>0, .T.,.F.) )

                    endif

             otherwise
                 //aEVAL(ARRYPAR,{|X| MSGINFO(X)})
                 //msgstop(zaps(len(Aclr)))
                 // 1:  Hdc
                 // 2:  y
                 // 3:  x
                 // 4:  FontName
                 // 5:  FontSize
                 // 6:  R Color
                 // 7:  G Color
                 // 8:  B Color
                 // 9:  Text
                 // 10: Bold
                 // 11: Italic
                 // 12: Underline
                 // 13: StrikeOut
                 // 14: Color Flag
                 // 15: FontName Flag
                 // 16: FontSize Flag

                 //    Aclr:= color(eval(chblk,arrypar,[COLOR]))
                _varexec:=Arrypar[4]
                     //msgbox(ARRYPAR[4]+CRLF+valtype(_varexec),[LEN AP=]+zaps(len(arrypar)))
                  if "->" $ ArryPar[4] .OR. [(] $ ArryPar[4]
                     ArryPar[4]:= trans(eval(epar,ArryPar[4]),"@A")
                     // MSGBOX(ARRYPAR[4],[ap4Post])
                  endif
                 _HMG_PRINTER_H_PRINT( iif(MGSYS,_HMG_SYSDATA [ 374 ],_hmg_printer_hdc) ;
                , iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                , eval(epar,ArryPar[2])  ;
                , eval(chblk,arrypar,[FONT]) ;
                , val(eval(chblk,arrypar,[SIZE])) ;
                , Aclr[1] ;
                , Aclr[2] ;
                , Aclr[3] ;
                , arrypar[4]    ; //if("->" $ ArryPar[4] .OR. [(] $ ArryPar[4],&ArryPar[4],ArryPar[4])  ;
                , iif(ascan(arryPar,[BOLD])!=0,.T.,.F.);
                , iif(ascan(arryPar,[ITALIC])!=0,.T.,.F.) ;
                , iif(ascan(arryPar,[UNDERLINE])!=0,.T.,.F.);
                , iif(ascan(arryPar,[STRIKEOUT])!=0,.T.,.F.);
                , iif(ascan(arryPar,[COLOR])>0, .T.,.F.) ;
                , iif(ascan(arryPar,[FONT])>0, .T.,.F.) ;
                , iif(ascan(arryPar,[SIZE])>0, .T.,.F.) ;
                , eval(chblk,arrypar,[ALIGN]))
             endcase

     case ascan(ArryPar,[MEMOSAY])=3
             memosay(iif([LINE]$ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1]) ) ,eval(epar,ArryPar[2]) ,&(ArryPar[4]) ;
          ,iif(ascan(arryPar,[LEN])>0,iif(HB_ISARRAY(oWr:argm[3]), ;
                                     oWr:MACROCOMPILE(eval(chblk,arrypar,[LEN]),.T.,cmdline,section) , ;
                                     val(eval(chblk,arrypar,[LEN]))),NIL) ;
          ,iif(ascan(arryPar,[FONT])>0,eval(chblk,arrypar,[FONT]),NIL);
          ,iif(ascan(arryPar,[SIZE])>0,val( eval(chblk,arrypar,[SIZE] ) ),NIL );
          ,iif(ascan(arryPar,[BOLD])!=0,.T.,.F.);
          ,iif(ascan(arryPar,[ITALIC])!=0,.T.,.F.) ;
          ,iif(ascan(arryPar,[UNDERLINE])!=0,.T.,.F.);
          ,iif(ascan(arryPar,[STRIKEOUT])!=0,.T.,.F.);
          ,iif(ascan(arryPar,[COLOR])>0,oWr:usacolor(eval(chblk,arrypar,[COLOR])),NIL);
          ,iif(ascan(arryPar,[ALIGN])>0,oWr:what_ele(eval(chblk,arrypar,[ALIGN]),_aAlign,"_aAlign"),NIL);
          ,iif(ascan(arryPar,[.F.])>0,".F.",""))

/*
//FOR NOW DO NONE !!!
     case ascan(ArryPar,[PUTARRAY])=3

                oWr:Putarray(iif([LINE] $ Arrypar[1],&(Arrypar[1]),eval(epar,ArryPar[1])) ;
                   ,eval(epar,ArryPar[2]) ;
                   ,oWr:MACROCOMPILE(ArryPar[4],.T.,cmdline,section)    ;            //arr
                   ,iif(ascan(arryPar,[LEN])>0,oWr:macrocompile(eval(chblk,arrypar,[LEN])),NIL) ; //awidts
                   ,nil                                                           ;      //rowheight
                   ,nil                                                           ;      //vertalign
                   ,(ascan(arryPar,[NOFRAME])>0)                                  ;      //noframes
                   ,nil                                                           ;      //abrushes
                   ,nil                                                           ;      //apens
                   ,iif(ascan(arryPar,[FONT])>0,NIL,NIL)                           ;      //afonts
                   ,iif(ascan(arryPar,[COLOR])> 0,oWr:UsaColor(eval(chblk,arrypar,[COLOR])),NIL);//afontscolor
                   ,NIL                                                           ;      //abitmaps
                   ,nil )                                                                //userfun
*/
     endcase
return .T.
/*
*/