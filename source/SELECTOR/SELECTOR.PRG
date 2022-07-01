/* ..........................................................................
     Name: Selector
   Params: aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval
   Return: Undefined
  Example: SELECTOR(aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval)

   Author: Written in Clipper 5.2e by Janusz Piwowarski <januszp@geocities.com>
  Version: 1.1
  Description: @...SELECT is a pseudo-SQL command used to retrieve data from databases
   ..........................................................................

  Syntax:

    @ [<array>] SELECT
        <xExpr> [AS <xcName> [LENGHT <nLenght> DEC <nDecimals>]]
            [GROUP|TOTAL|AVERAGE] [SUMMARY]
        [, <xExpr2> [AS <xcName2> [LENGHT <nLenght2> DEC <nDecimals2>]]
               [GROUP|TOTAL|AVERAGE] [SUMMARY]...]
        [FROM <xcnArea>] ;
        [OTM <xcnChild> SEEK <xValueFromMother> EQUAL <xValueFromChild>] ;
        [FOR <lCondition>] ;
        [WHILE <lCondition>] ;
        [REST] ;
        [TO <xcFile>] ;
        [ALIAS <xcAlias>] ;
        [ADDITIVE [IF <lCondition>]] ;
        [SUMMARY <aSummary>] ;
        [EVAL <xEval>] ;

  Arguments:

    <array> is a name of an variable used to "compile" @...SELECT. The
    default name is SelectList.

    <xExpr> is a database field name or expression

    AS <xcName> defines the field name in the target file. If not specified,
    field name is taken from <xExpr>.

    LENGHT <nLenght> DEC <nDecimals> are the lenght and the number of decimal
    places of the field in the target file. The defaults are taken from
    <xExpr>.

    GROUP specifies the unique key value in the target file.

    TOTAL specifies that <xExpr> is summarized by unique key value.

    AVERAGE specifies that <xExpr> is averaged by unique key value.

    SUMMARY specifies that <xExpr> is summarized to <aSummary>

    OTM <xcnChild> SEEK <xValueFromMother> EQUAL <xValueFromChild> specifies
    the "one to many" relation.

    FROM <xcnArea> is the @...SELECT work area. The default is current area.

    FOR <lCondition> specifies the conditional set of records to @..SELECT.

    WHILE <lCondition> specifies the set of records meeting the condition
    from the first record in the source area until the condition fails.

    REST specifies that @...SELECT begins from the current record, otherwise
    it begins from the first record.

    TO <xcFile> specifies the name of the target file. The default is
    "TEMP.DBF".

    ALIAS <xcAlias> specifies the name to associate with the target work
    area. If the <xcFile> is ommited, the default is "temp". Otherwise, the
    alias defaults to the target filename.

    ADDITIVE IF <lCondition> causes @...SELECT to continue the work with
    existing <xcFile> if <lCondition> is ommited or returns true.

    SUMMARY <aSummary> is the name of an existing and initialized array used
    to store totals.

    EVAL <xEval> specifies a expression that is evaluated either for each
    record processed.
*/

PROCEDURE Selector( aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval )

   LOCAL nLenSelectList := Len( aSelectList )
   LOCAL xExprValue, cExprValue, cExprType, nExprLen, aStruct := {}
   LOCAL lOTM := ( ValType( aOneToMany ) == "A" )
   LOCAL nExprAlias, nCurrExpr, nSelectFrom, nSelectChild, nSelectTemp, xValueFromMother, axExpr := {}
   LOCAL lCreateIndex := .F.
   LOCAL cIndexName, cIndexKey := ""
   LOCAL lAppend, lSummary := .NOT. ( aSummary == NIL )
   LOCAL nCurrent, cSeekExpr, nStartRec

   IF xcnFromArea == NIL

      nSelectFrom := Select()

   ELSE

      IF ValType( xcnFromArea ) == "N"

         nSelectFrom := xcnFromArea

      ELSEIF ValType( xcnFromArea ) == "C"

         nSelectFrom := Select( xcnFromArea )

      ENDIF

      dbSelectArea( nSelectFrom )

   ENDIF

   IF lOTM

      nSelectChild := Select( aOneToMany[ 1 ] )

   ENDIF

   IF xcFile == NIL

      xcFile := "TEMP.DBF"

      IF xcAlias == NIL

         xcAlias := "temp"

      ENDIF

   ELSEIF xcAlias == NIL

      xcAlias := SubStr( xcAlias := SubStr( xcFile, RAt( "\", xcFile ) + 1 ), 1, At( ".", xcAlias + "." ) - 1 )

   ENDIF

   IF cIndexName == NIL

      cIndexName := IF( ( nExprAlias := RAt( ".", xcFile ) ) > RAt( "\", xcFile ), SubStr( xcFile, 1, nExprAlias - 1 ), xcFile ) + IndexExt()

   ENDIF

   nStartRec := RecNo()
   dbGoto( LastRec() + 1 )

   IF lOTM

      ( nSelectChild )->( dbGoto( LastRec() + 1 ) )

   ENDIF

   FOR nCurrExpr := 1 TO nLenSelectList

      xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )
      cExprType := ValType( xExprValue )
      cExprValue := Transform( xExprValue, "" )
      nExprLen := Len( cExprValue )
      AAdd( aStruct, { iif( ( nExprAlias := At( "->", aSelectList[ nCurrExpr, 2 ] ) ) == 0, aSelectList[ nCurrExpr, 2 ], ;
         SubStr( aSelectList[ nCurrExpr, 2 ], nExprAlias + 2 ) ), cExprType, ;
         iif( aSelectList[ nCurrExpr, 3 ] <> NIL, aSelectList[ nCurrExpr, 3 ], ;
         iif( cExprType == "D", 8, nExprLen ) ), ;
         iif( aSelectList[ nCurrExpr, 4 ] <> NIL, aSelectList[ nCurrExpr, 4 ], ;
         iif( cExprType == "N" .AND. ( nExprAlias := At( ".", cExprValue ) ) > 0, ;
         nExprLen - nExprAlias, 0 ) ) } )

      IF aSelectList[ nCurrExpr, 5 ] == "G"

         IF .NOT. lCreateIndex

            lCreateIndex := .T.

         ELSE

            cIndexKey += "+"

         ENDIF

         IF cExprType == "C"

            cIndexKey += aStruct[ nCurrExpr, 1 ]

         ELSEIF cExprType == "N"

            cIndexKey += ( "Str(" + aStruct[ nCurrExpr, 1 ] + ")" )

         ELSEIF cExprType == "D"

            cIndexKey += ( "DToS(" + aStruct[ nCurrExpr, 1 ] + ")" )

         ELSEIF cExprType == "L"

            cIndexKey += ( "Iif(" + aStruct[ nCurrExpr, 1 ] + "," + Chr( 34 ) + "1" + Chr( 34 ) + "," + Chr( 34 ) + "0" + Chr( 34 ) + ")" )

         ENDIF

         AAdd( axExpr, aSelectList[ nCurrExpr, 1 ] )

      ELSEIF aSelectList[ nCurrExpr, 6 ] .AND. ( lSummary .AND. .NOT. lAdditive )

         AAdd( aSummary, 0 )

      ENDIF

   NEXT

   dbGoto( nStartRec )

   IF lAdditive

      nSelectTemp := Select( xcAlias )

   ELSE

      dbCreate( xcFile, aStruct )
      dbUseArea( .T., NIL, xcFile, xcAlias, .F. )
      nSelectTemp := Select()

      IF lCreateIndex

         dbCreateIndex( cIndexName, cIndexKey, ( &( "{||" + cIndexKey + "}" ) ) )

      ENDIF

      dbSelectArea( nSelectFrom )

   ENDIF

   IF .NOT. lRest

      dbGoTop()

   ENDIF

   IF lOTM

      DO WHILE .NOT. Eof() .AND. .NOT. ( nSelectChild )->( dbSeek( xValueFromMother := ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )

         dbSkip()

      ENDDO

   ENDIF

   DO WHILE .NOT. Eof() .AND. ( lWhile == NIL .OR. Eval( lWhile ) )

      IF ValType( xEval ) == "B"

         Eval( xEval )

      ENDIF

      IF lSelect == NIL .OR. Eval( lSelect )

         lAppend := .F.
         lCreateIndex := .NOT. lCreateIndex
         cSeekExpr := ""

         IF ValType( axExpr ) == "A" .AND. ValType( AEval( axExpr, {| e | cSeekExpr += ToString( Eval( e ) ) } ) ) == "A" .OR. .NOT. ( nSelectTemp )->( dbSeek( cSeekExpr, .F. ) )

            ( nSelectTemp )->( dbAppend() )
            lAppend := .T.

         ENDIF

         nCurrent := 0

         FOR nCurrExpr := 1 TO nLenSelectList

            IF aSelectList[ nCurrExpr, 6 ]

               ++nCurrent

            ENDIF

            IF .NOT. lCreateIndex .OR. lAppend .OR. aSelectList[ nCurrExpr, 5 ] $ "TA"

               xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )

               IF aSelectList[ nCurrExpr, 5 ] == "T"

                  ( nSelectTemp )->( FieldPut( nCurrExpr, FieldGet( nCurrExpr ) + xExprValue ) )

               ELSEIF aSelectList[ nCurrExpr, 5 ] == "A" .AND. .NOT. lAppend

                  ( nSelectTemp )->( FieldPut( nCurrExpr, ( FieldGet( nCurrExpr ) + xExprValue ) / 2 ) )

               ELSE

                  ( nSelectTemp )->( FieldPut( nCurrExpr, xExprValue ) )

               ENDIF

               IF aSelectList[ nCurrExpr, 6 ] .AND. lSummary

                  aSummary[ nCurrent ] := aSummary[ nCurrent ] + xExprValue

               ENDIF

            ENDIF

         NEXT

      ENDIF

      IF .NOT. lOTM

         ( nSelectFrom )->( dbSkip() )

      ELSE

         IF ( nSelectChild )->( dbSkip() ) .OR. ( nSelectChild )->( Eof() .OR. .NOT. xValueFromMother == Eval( aOneToMany[ 3 ] ) )

            dbSkip()

            IF lOTM

               DO WHILE .NOT. Eof() .AND. .NOT. ( nSelectChild )->( dbSeek( xValueFromMother := ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )

                  dbSkip()

               ENDDO

            ENDIF

         ENDIF

      ENDIF

   ENDDO

   dbSelectArea( nSelectTemp )
   dbGoTop()

RETURN

/* ..........................................................................
     Name: ToString
   Params: xType
   Return: String
  Example: ToString(String)
   ..........................................................................
*/

STATIC FUNCTION ToString( cStr )

   LOCAL cType := ValType( cStr )

   IF cType == "N"

      cStr := Str( cStr )

   ELSEIF cType == "D"

      cStr := DToS( cStr )

   ELSEIF cType == "L"

      cStr := iif( cStr, "1", "0" )

   ENDIF

RETURN cStr
