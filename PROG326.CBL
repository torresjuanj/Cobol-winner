       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG326.
      ******************************************************************
      * CHEQUE PARA EL PAGO DE NOMINA      ABR-27-2002                 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CONCPTOS.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\NOMIBANK.DYC".
               COPY "..\DYC\MVDOCTOS.DYC".
               COPY "..\DYC\TABLAEMP.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CONTRATO.DYC".
               COPY "..\DYC\NOMINOVE.DYC".
               COPY "..\SEL\INFORMES.SEL".
               COPY "..\SEL\REPORTES.SEL".
               COPY "..\DYC\TEMPINFO.DYC".
               COPY "..\DYC\CNCOMPTE.DYC".
               COPY "..\DYC\CNEGRESO.DYC".
               COPY "..\DYC\TBCONDOC.DYC".
               COPY "..\DYC\CPLANILL.DYC".
               COPY "..\DYC\CHCONTRO.DYC".
               COPY "..\DYC\CONTREXT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\MVDOCTOS.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CPLANILL.FD".
               COPY "..\FD\CONTREXT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CONCPTOS.FD".
               COPY "..\FD\TABLAEMP.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\NOMINOVE.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\TEMPINFO.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\CHCONTRO.FD".
               COPY "..\FD\CNEGRESO.FD".
               COPY "..\FD\TBCONDOC.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG326".
       77      ID-PRG              PIC X(7)  VALUE "PROG326".
       77      W-CHEQUE                  PIC 9(04)      VALUE ZEROS.
       77      W-TAMANO-DEVENGADOS       PIC 9(3)       VALUE ZEROS.
       77      L-CODIGO                  PIC 9(3)       VALUE ZEROS.
       77      M-NRO                     PIC 9(4)       VALUE 1.
       77      U-EMPRESA                 PIC 9(7)       VALUE ZEROS.
       77      K-EMPRESA                 PIC 9(7)       VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\MVDOCTOS.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CPLANILL.LBL".
               COPY "..\LBL\CONTREXT.LBL".
               COPY "..\LBL\CONTRATO.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
               COPY "..\LBL\NOMINOVE.LBL".
               COPY "..\LBL\CHCONTRO.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\LBL\TEMPINFO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\CNEGRESO.LBL".
               COPY "..\LBL\TBCONDOC.LBL".
      *
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\NCHEQNUE.WRK".
               COPY "..\WRK\RUTMONTO.WRK".
               COPY "..\WRK\LISTADOS.WRK".
               COPY "..\WRK\091_2X11.WRK".
      **** RELACION DE CHEQUES PAGADOS
       01      AREA-REPORTE.
        03     AREA-WORKING-REPORTE.
         05    WR-PAGINA                 PIC 9(4)       VALUE ZEROS.
         05    WR-LINEAS                 PIC 9(4)       VALUE 88.
         05    I-CODIGO-EMPRESA          PIC 9(07)      VALUE ZEROS.

        03     TITULO-R-1.
         05    FILLER            PIC X(26)   VALUE SPACES.
         05    FILLER            PIC X(29)   VALUE
               " RELACION DE CHEQUES PAGADOS ".
      *
        03     TITULO-R-2.
         05    FILLER            PIC X(10)    VALUE "PERIODO:".
         05    R-PERIODO         PIC ZZZ      VALUE ZEROS.
         05    FILLER            PIC X(12)    VALUE SPACES.
         05    FILLER            PIC X(11)    VALUE "NOMINA DEL".
         05    R-FECHA-DESDE     PIC ZZZZ/ZZ/ZZ VALUE ZEROS.
         05    FILLER            PIC X(4)     VALUE " AL ".
         05    R-FECHA-HASTA     PIC ZZZZ/ZZ/ZZ VALUE ZEROS.

        03     TITULO-R-2-A.
         05    FILLER            PIC X(10)   VALUE "EMPRESA:".
         05    R-CODIGO-EMPRESA  PIC 9(07)   VALUE ZEROS.
         05    FILLER            PIC X(01)   VALUE SPACES.
         05    R-NOMBRE-EMPRESA  PIC X(30)   VALUE SPACES.

        03     TITULO-R-3.
      *  05    FILLER            PIC 9(4)    USAGE IS COMP-1 VALUE 4626.
         05    FILLER            PIC X(75)    VALUE SPACES.
         05    FILLER            PIC X(8)     VALUE "NETO A".

        03     TITULO-R-4.
      *  05    FILLER            PIC 9(4)    USAGE IS COMP-1 VALUE 4626.
         05    FILLER            PIC X(06)    VALUE " Nro.".
         05    FILLER            PIC X(08)    VALUE " CHEQUE".
         05    FILLER            PIC X(14)    VALUE "  CODIGO".
         05    FILLER            PIC X(18)    VALUE "    N.I.T.".
         05    FILLER            PIC X(28)    VALUE
                                              "APELLIDOS Y NOMBRES".
         05    FILLER            PIC X(9)     VALUE "  PAGAR".

        03     LINEA-DETALLE-R.
      *  05    FILLER            PIC 9(4)    USAGE IS COMP-1 VALUE 4626.
         05    R-NRO             PIC Z(04)            VALUE SPACES.
         05    FILLER            PIC X(2)             VALUE SPACES.
         05    R-CHEQUE          PIC Z(06)-           VALUE SPACES.
         05    FILLER            PIC X(2)             VALUE SPACES.
         05    R-CODIGO          PIC X(10)            VALUE SPACES.
         05    R-NIT             PIC ZZZ,ZZZ,ZZZ,ZZZ- VALUE SPACES.
         05    FILLER            PIC X(1)             VALUE SPACES.
         05    R-NOMBRE          PIC X(29)            VALUE SPACES.
         05    R-NETO-A-PAGAR    PIC ZZZ,ZZZ,ZZZ-     VALUE SPACES.

       03     AREA-CONCEPTO-MVDOCTOS.
         05    C-DATO-MVDOCTOS           PIC X(20)    VALUE SPACES.
         05    FILLER                    PIC X(06)    VALUE " CHQ.#".
         05    C-CHEQ-DESDE              PIC 9(04)    VALUE ZEROS.
         05    FILLER                    PIC X(4)     VALUE
              " AL ".
         05    C-CHEQ-HASTA              PIC 9(04)    VALUE ZEROS.
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\MVDOCTOS.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\CONTRATO.DCL".
               COPY "..\DCL\NOMINOVE.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\CPLANILL.DCL".
               COPY "..\DCL\CHCONTRO.DCL".
               COPY "..\DCL\CONTREXT.DCL".
               COPY "..\DCL\REPORTES.DCL".
               COPY "..\DCL\TEMPINFO.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
               COPY "..\DCL\CNEGRESO.DCL".
               COPY "..\DCL\TBCONDOC.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
               MOVE WIN-NOMINA             TO  CON-PERIODO.
               IF WIN-TIPO-PAGO NOT = 2
                PERFORM I-VERI-TIPO-PAGO THRU F-VERI-TIPO-PAGO
                EXIT PROGRAM.
               IF WIN-PERIODO-ESTADO NOT = 77
                  PERFORM I-WIN-PERIODO-ESTADO THRU F-WIN-PERIODO-ESTADO
                  EXIT PROGRAM.
               MOVE W-IDENT-PROGRAMA          TO CON-PROGRAMA
               MOVE    4                    TO  IND-INFORMES.
               MOVE    1                    TO  IND-REPORTES.
               PERFORM I-LABEL-TABLAEMP     THRU F-LABEL-TABLAEMP.
               PERFORM I-LABEL-CONTRATO     THRU F-LABEL-CONTRATO.
               PERFORM I-LABEL-NOMINOVE     THRU F-LABEL-NOMINOVE.
               PERFORM I-LABEL-TEMPINFO     THRU F-LABEL-TEMPINFO.
               PERFORM I-LABEL-INFORMES     THRU F-LABEL-INFORMES.
               PERFORM I-LABEL-REPORTES     THRU F-LABEL-REPORTES.
               PERFORM  I-VARREPOR-RENC0911 THRU F-VARREPOR-RENC0911.
               MOVE WIN-EMPRESA               TO WK-COMPANIA
                                                 R-NOMBRE-EMPRESA
               MOVE "น"                       TO WK-RAYAS-U
               MOVE "ฬ"                       TO WK-RAYAS-P
               MOVE "ถ"                       TO WK-RAYA-U
               MOVE "ว"                       TO WK-RAYA-P
               MOVE ZEROS                     TO SW-ABRE
               OPEN INPUT NOMINOVE
               MOVE 1                         TO SW-ABRE
           IF  NOMINOVE-STATUS              =  "94"
               OPEN OUTPUT NOMINOVE
           ELSE
           IF  NOMINOVE-STATUS              >  "00"
               PERFORM RUTINA-ERROR-NOMINOVE.
               CLOSE NOMINOVE.
       010-BORRA-AREA-WORKING.
               PERFORM BORRA-LIQUINOV THRU
                       SALE-BORRA-LIQUINOV
               MOVE ZEROS                  TO  W-VALOR-DEVENGADOS
                                               W-VALOR-DESCUENTOS
                                               W-NETO-A-PAGAR.
       010-ABRE-ARCHIVOS.
               OPEN INPUT  CONCPTOS
                           TABLAEMP
                           CONTRATO
                           HOJAVIDA
                           NOMINOVE
               OPEN OUTPUT INFORMES
                           REPORTES.
       010-VALIDA-NOMINOVE.
               MOVE ZEROS                  TO  W-TPN-CODIGO-TABLA
                                               W-TPN-CODIGO-EMPRESA
                                               W-TPN-CODIGO-ANO
                                               W-TPN-CODIGO-PERIODO
                                               W-TPN-CODIGO-FILLER
                                               W-TPN-FECHA-DESDE
                                               W-TPN-FECHA-HASTA
                                               W-TPN-FECHA-PAGO
                                               W-TPN-SEMANAS-RFTE
                                               W-TPN-NUMERO-DIAS
                                               W-TPN-NUMERO-HORAS
               MOVE SPACES                 TO  W-TPN-FLAG-PERIODO
               MOVE ZEROS                  TO  TAB-EMP-CLAVE
                                               TAB-EMP-FILLER
               MOVE 011                    TO  TE011-CODIGO-TABLA
               MOVE CON-COD-EMPRESA        TO  TE011-CODIGO-EMPRESA
               MOVE CON-ANO                TO  TE011-CODIGO-ANO
               MOVE CON-PERIODO            TO  TE011-CODIGO-PERIODO
               MOVE TAB-EMP-CLAVE          TO  W-TAB-CLAVE
               MOVE ZEROS                  TO  I-ERROR
               READ TABLAEMP          INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  TABLAEMP-STATUS              =  "23"
               MOVE "90"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
               PERFORM RUTINA-ERROR-TABLAEMP.
           IF  TAB-EMP-CODIGO-TABLA         >  011
               MOVE "90"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE TE011-CODIGO-TABLA     TO  W-TPN-CODIGO-TABLA
               MOVE TE011-CODIGO-EMPRESA   TO  W-TPN-CODIGO-EMPRESA
               MOVE TE011-CODIGO-ANO       TO  W-TPN-CODIGO-ANO
               MOVE TE011-CODIGO-PERIODO   TO  W-TPN-CODIGO-PERIODO
               MOVE TE011-CODIGO-FILLER    TO  W-TPN-CODIGO-FILLER
               MOVE TE011-FECHA-DESDE      TO  W-TPN-FECHA-DESDE
               MOVE TE011-FECHA-HASTA      TO  W-TPN-FECHA-HASTA
               MOVE TE011-FECHA-PAGO       TO  W-TPN-FECHA-PAGO
               MOVE TE011-SEMANAS-RFTE      TO  W-TPN-SEMANAS-RFTE
               MOVE TE011-FLAG-PERIODO     TO  W-TPN-FLAG-PERIODO
               MOVE TE011-NUMERO-DIAS      TO  W-TPN-NUMERO-DIAS
               MOVE TE011-NUMERO-HORAS     TO  W-TPN-NUMERO-HORAS.
           MOVE W-TPN-CODIGO-EMPRESA       TO  W-NIVFRA1-MVDOCTOS
           MOVE W-TPN-CODIGO-PERIODO       TO  W-NIVFRA2-MVDOCTOS
           MOVE W-TPN-FECHA-PAGO       TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
               MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           PERFORM I-SELECCIONE-MVDOCTOS THRU F-SELECCIONE-MVDOCTOS.
       010-VALIDA-CONCPTOS.
               PERFORM BORRA-TABLANOV THRU
                       SALE-BORRA-TABLANOV
               PERFORM CARGA-TABLANOV THRU
                       SALE-CARGA-TABLANOV
               COMPUTE W-TAMANO-TABLANOV    =  I  -  1.
           IF  W-TAMANO-TABLANOV            >  ZEROS
               NEXT SENTENCE
           ELSE
               MOVE "01"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
       015-CEROS.
               MOVE ZEROS                  TO  NOV-EMPRESA
                                               NOV-CODIEMP
                                               NOV-CONCEPTO
                                               NOV-EMPRESA2
                                               NOV-CONCEPTO2
                                               NOV-CODIEMP2
                                               NOV-HORAS
                                               NOV-DIAS
                                               NOV-VLR
                                               NOV-FLAG-ORIGEN
                                               NOV-FLAG-ACUMULADOS
                                               NOV-ORIGEN-NOVEDAD.
       015-INICIO.
       020-PANTALLA.
               MOVE 03 TO W-COMP
               PERFORM I-BUSCA-CONSECUTIVO THRU F-BUSCA-CONSECUTIVO
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "บ"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "SISTEMA DE NOMINA บ"        LINE  2 POSITION 62
               DISPLAY "บ FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY "PROCESO CHEQUES DE NOMINA - " LINE 3 POSITION 31
               DISPLAY CON-COD-EMPRESA              LINE 03 POSITION 58
                                                          BLINK REVERSE
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "บ"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "ั"                          LINE  4 POSITION 33
               DISPLAY "ั"                          LINE  4 POSITION 54
               DISPLAY "บ PERIODO:"                 LINE  5 POSITION  1
               DISPLAY CON-PERIODO                  LINE  5 POSITION 12
               DISPLAY "DESDE:"                     LINE  5 POSITION 16
               DISPLAY "ณ DIAS PERIODO :"           LINE  5 POSITION 33
               DISPLAY "ณ SEMANAS RFTE  :"          LINE  5 POSITION 54
               DISPLAY "บ"                          LINE  5 POSITION 80
               DISPLAY "บ"                          LINE  6 POSITION  1
               DISPLAY "HASTA:"                     LINE  6 POSITION 16
               DISPLAY "ณ HORAS PERIODO:"           LINE  6 POSITION 33
               DISPLAY "ณ FECHA DE PAGO:"           LINE  6 POSITION 54
               DISPLAY "บ"                          LINE  6 POSITION 80
               DISPLAY WK-RAYA                      LINE  7 POSITION  1
               DISPLAY "ย"                          LINE  7 POSITION 29
               DISPLAY "ม"                          LINE  7 POSITION 33
               DISPLAY "ม"                          LINE  7 POSITION 54
               DISPLAY "ย"                          LINE  7 POSITION 57
               DISPLAY "บ DEVENGADOS :"             LINE  8 POSITION  1
               DISPLAY "ณ DESCUENTOS :"             LINE  8 POSITION 29
               DISPLAY "ณ NETO :"                   LINE  8 POSITION 57
               DISPLAY "บ"                          LINE  8 POSITION 80
               DISPLAY WK-RAYAS                     LINE  9 POSITION  1
               DISPLAY "ฯ"                          LINE  9 POSITION 29
               DISPLAY "ฯ"                          LINE  9 POSITION 57
               DISPLAY "บ"                          LINE 10 POSITION  1
               DISPLAY "บ"                          LINE 10 POSITION 80
               DISPLAY "บ"                          LINE 11 POSITION  1
               DISPLAY "บ"                          LINE 11 POSITION 80
               DISPLAY "บ"                          LINE 12 POSITION  1
               DISPLAY "บ"                          LINE 12 POSITION 80
               DISPLAY "บ"                          LINE 13 POSITION  1
               DISPLAY "บ"                          LINE 13 POSITION 80
               DISPLAY "บ"                          LINE 14 POSITION  1
               DISPLAY "PAGINA DE REINICIO :"       LINE 14 POSITION  3
               DISPLAY "บ"                          LINE 14 POSITION 80
               DISPLAY "บ"                          LINE 15 POSITION  1
               DISPLAY "บ"                          LINE 15 POSITION 80
               DISPLAY WK-RAYA                      LINE 16 POSITION  1
               DISPLAY "ว"                          LINE 16 POSITION  1
               DISPLAY "ถ"                          LINE 16 POSITION 80
               DISPLAY "บ"                          LINE 17 POSITION  1
               DISPLAY "COD BANCO: "                LINE 17 POSITION  3
               DISPLAY "No.PRIMER CHEQUE:"          LINE 17 POSITION 45
               DISPLAY "บ"                          LINE 17 POSITION 80
               DISPLAY WK-RAYA                      LINE 18 POSITION  1
               DISPLAY "ว"                          LINE 18 POSITION  1
               DISPLAY "ถ"                          LINE 18 POSITION 80
               DISPLAY "บ"                          LINE 19 POSITION  1
               DISPLAY "บ"                          LINE 19 POSITION 80
               DISPLAY "บ"                          LINE 20 POSITION  1
               DISPLAY "บ"                          LINE 20 POSITION 80
               DISPLAY WK-RAYA                      LINE 21 POSITION  1
               DISPLAY "ว"                          LINE 21 POSITION  1
               DISPLAY "ถ"                          LINE 21 POSITION 80
               DISPLAY "บ"                          LINE 22 POSITION  1
               DISPLAY "บ"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.

              MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
              MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
              DISPLAY " "  LINE 20 POSITION 12 CONTROL SG-CONTROL-STRING
              DISPLAY  SPACES LINE 19 POSITION 2 SIZE 78
              DISPLAY  SPACES LINE 20 POSITION 2 SIZE 78
              "LIST. DE CHEQUES   -------->" LINE 19 POSITION 05
               LABEL-INFORMES                LINE 19 POSITION 33
               NOM-PAPEL(IND-INFORMES)       LINE 19 POSITION 66
               DISPLAY
              "LIST. RESUMEN CHEQUES  ---->" LINE 20 POSITION 05
               LABEL-REPORTES                LINE 20 POSITION 33
               NOM-PAPEL(IND-REPORTES)       LINE 20 POSITION 66
            MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " "  LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
      *
       020-DISPLAY-DATOS-PERIODO.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               MOVE W-TPN-FECHA-DESDE      TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE  5 POSITION 23
               MOVE W-TPN-FECHA-HASTA      TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE  6 POSITION 23
               MOVE W-TPN-NUMERO-DIAS      TO  WI-Z2
               DISPLAY WI-Z2                        LINE  5 POSITION 51
               MOVE W-TPN-NUMERO-HORAS     TO  WI-Z3
               DISPLAY WI-Z3                        LINE  6 POSITION 50
               MOVE W-TPN-SEMANAS-RFTE      TO  WI-Z
               DISPLAY WI-Z                         LINE  5 POSITION 71
               MOVE W-TPN-FECHA-PAGO       TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE  6 POSITION 71.
       CONFIRMA-INFORME.
               PERFORM PAGINA-REINICIO THRU F-NUMERO-CHEQUE.
               DISPLAY " PRESIONE <S> PARA CONFIRMAR "
                                                    LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "S"
               GO TO CONFIRMA-INFORME.
               DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.

       INICIE-INFORME.
               MOVE WIN-CODIGO-EMPRESA     TO  R-CODIGO-EMPRESA
               MOVE CON-PERIODO            TO  R-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  R-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  R-FECHA-HASTA
               MOVE 88                     TO  WR-LINEAS
               MOVE ZEROS                  TO  WR-PAGINA
               MOVE ZEROS                  TO  WK-PAGINA
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               OPEN INPUT TEMPINFO
               MOVE ZEROS                  TO  TIMAE-CEDULA
                                               TIMAE-CEDULA4
               MOVE SPACES                 TO  TIMAE-CODIEMP
                                               TIMAE-CODIEMP5
               MOVE SPACES                 TO  TIMAE-NOMBRE
                                               TIMAE-NOMBRE2
                                               TIMAE-NOMBRE3
               MOVE ZEROS                  TO  TIMAE-CODICEN
                                               TIMAE-CODICEN4
                                               TIMAE-CODICEN5
           IF  WIN-ORDEN                    =  "A "
               START TEMPINFO KEY           >  TIMAE-CLAVE2
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
           IF  WIN-ORDEN                    =  "C "
               START TEMPINFO KEY           >  TIMAE-CLAVE1
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
           IF  WIN-ORDEN                    =  "N "
               START TEMPINFO KEY           >  TIMAE-CLAVE
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
           IF  WIN-ORDEN                    =  "CA"
           OR  WIN-ORDEN                    =  "KA"
               START TEMPINFO KEY           >  TIMAE-CLAVE3
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
           IF  WIN-ORDEN                    =  "CN"
           OR  WIN-ORDEN                    =  "KN"
               START TEMPINFO KEY           >  TIMAE-CLAVE4
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
               START TEMPINFO KEY           >  TIMAE-CLAVE5
                                      INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               DISPLAY "CEDULA :"                   LINE 12 POSITION  3
               DISPLAY "NOMBRE : "                  LINE 12 POSITION 26
               DISPLAY "DEVENGADOS :"               LINE 14 POSITION  3
               DISPLAY "DESCUENTOS :"               LINE 14 POSITION 31
               DISPLAY "NETO :"                     LINE 14 POSITION 59.
       080-SIGUE-INFORME.
               READ TEMPINFO                   NEXT RECORD
           AT  END
               GO TO FIN-INFORME.
               MOVE TIMAE-CEDULA           TO  CONTRATO-CEDULA
               READ CONTRATO          INVALID  KEY
               MOVE "97"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
               READ HOJAVIDA          INVALID  KEY
               MOVE "99"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               PERFORM BORRA-NOVEDADES THRU
                       SALE-BORRA-NOVEDADES
               PERFORM CARGA-NOVEDADES THRU
                       SALE-CARGA-NOVEDADES
           IF  W-TAMANO-NOVEDADES           = ZEROS
               GO TO 080-SIGUE-INFORME.
               PERFORM IMPRIME-CHEQUES THRU
                       SALE-IMPRIME-CHEQUES
               MOVE CONTRATO-CEDULA        TO  WI-Z12
               DISPLAY WI-Z12              LINE 12 POSITION 12
               DISPLAY HOJAVIDA-NOMBRE     LINE 12 POSITION 35
               MOVE W-VALOR-DEVENGADOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE 14 POSITION 17
               MOVE W-VALOR-DESCUENTOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE 14 POSITION 45
               MOVE W-NETO-A-PAGAR         TO  WI-Z8P
               DISPLAY WI-Z8P              LINE 14 POSITION 69
               ADD  W-NETO-A-PAGAR         TO  W-TOTAL-A-PAGAR
               ADD  W-VALOR-DEVENGADOS     TO  W-TOTAL-DEVENGADOS
               ADD  W-VALOR-DESCUENTOS     TO  W-TOTAL-DESCUENTOS
               MOVE W-TOTAL-DEVENGADOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  8 POSITION 17
               MOVE W-TOTAL-DESCUENTOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  8 POSITION 45
               MOVE W-TOTAL-A-PAGAR        TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  8 POSITION 69
               GO TO 080-SIGUE-INFORME.
       FIN-INFORME.
      *    IF  WK-REINICIO                  <  WK-PAGINA
               WRITE REG-REPORTES        FROM  ENC9-06         AFTER 1
               MOVE SPACES                 TO  R-CODIGO
               MOVE ZEROS                  TO  R-CHEQUE
               MOVE ZEROS                  TO  R-NIT
               MOVE "T O T A L E S . . ."  TO  R-NOMBRE
               MOVE W-TOTAL-A-PAGAR        TO  R-NETO-A-PAGAR
               IF W-TOTAL-A-PAGAR > 0
      * 03 Busca consecutivo de egresos.
      *
                  COMPUTE W-CHEQUE =  W-CHEQUE - 1
                  COMPUTE M-NRO    =  M-NRO    - 1
                  PERFORM I-CHEQUE    THRU F-CHEQUE
                  PERFORM ACEPTA-TRUCO
                  MOVE W-CHEQUE               TO  C-CHEQ-HASTA
                  PERFORM I-ACT-TBCONDOC    THRU  F-ACT-TBCONDOC
                  PERFORM I-CONT-TOTAL      THRU  F-CONT-TOTAL.

               MOVE M-NRO                  TO  R-NRO
               WRITE REG-REPORTES        FROM  LINEA-DETALLE-R AFTER 1
               WRITE REG-REPORTES        FROM  ENC9-06         AFTER 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1.
      * Actualiza fin de ciclo de nomina en la empresa.
               PERFORM I-ACT-EMPRESAS    THRU F-ACT-EMPRESAS.
       I-FIN-MODE.
               CLOSE CONCPTOS
                     TABLAEMP MVDOCTOS
                     CONTRATO
                     HOJAVIDA
                     NOMINOVE
                     INFORMES
                     REPORTES
                     TEMPINFO.
               CANCEL "PROG326.COB"
           IF WMODULO-CONTABLE = "S"
               CALL "PROG785.COB" USING AREA-LINK-NOMINA.

               CANCEL "PROG785.COB"
               CALL "PROG737.COB" USING AREA-LINK-NOMINA.
               CANCEL "PROG737.COB"
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.

       TITULOS-REPORTE.
               ADD  1                         TO  WR-PAGINA
               MOVE  WR-PAGINA                TO  ENC9-PAGINA.
               MOVE  TITULO-R-1               TO  E9-02-DESINF.
               MOVE  TITULO-R-2               TO  E9-03-DESINF.
               MOVE  TITULO-R-2-A             TO  E9-04-DESINF.
               MOVE  TITULO-R-3               TO  E9-07-TITU.
               MOVE  TITULO-R-4               TO  E9-08-TITU.
               PERFORM  I-REPORTES-RENC0911  THRU  F-REPORTES-RENC0911.
       SIGUE-TITULOS-REPORTE.
               MOVE ZEROS                  TO  WR-LINEAS.
       SALE-TITULOS-REPORTE.
               EXIT.
       ARMA-LINEA-REPORTE.
           IF  WR-LINEAS                    >  49
               PERFORM TITULOS-REPORTE THRU
                       SALE-TITULOS-REPORTE.
               MOVE W-CHEQUE               TO  R-CHEQUE
               MOVE M-NRO                  TO  R-NRO
               MOVE CONTRATO-CODIEMP          TO  R-CODIGO
               MOVE CONTRATO-CEDULA           TO  R-NIT
               MOVE HOJAVIDA-NOMBRE           TO  R-NOMBRE
               MOVE W-NETO-A-PAGAR         TO  R-NETO-A-PAGAR
               IF W-NETO-A-PAGAR > 0
               PERFORM I-GRABA-COMP-CPLANILL THRU F-GRABA-COMP-CPLANILL.
               PERFORM I-CHEQUE          THRU  F-CHEQUE
               ADD  1                      TO  W-CHEQUE
               ADD  1                      TO  M-NRO
               WRITE REG-REPORTES        FROM  LINEA-DETALLE-R AFTER 1.
               ADD  1                      TO  WR-LINEAS.
       SALE-ARMA-LINEA-REPORTE.
               EXIT.
       CALCULA-NETO-A-PAGAR.
           IF  T-NOV-CONCEPTO          (I)  <  499
               ADD  T-NOV-VLR          (I) TO  W-VALOR-DEVENGADOS
           ELSE
               ADD  T-NOV-VLR          (I) TO  W-VALOR-DESCUENTOS
               IF  W-TAMANO-DEVENGADOS          >  ZEROS
                   NEXT SENTENCE
               ELSE
                   MOVE I                  TO  W-TAMANO-DEVENGADOS.
       SALE-CALCULA-NETO-A-PAGAR.
               EXIT.
      *
      *
       I-LEE-NOMIBANK.
           OPEN INPUT NOMIBANK
           MOVE 0 TO SW-EOF.
           READ NOMIBANK INVALID KEY MOVE 1 TO SW-EOF.
           CLOSE NOMIBANK.
       F-LEE-NOMIBANK.
      *
       I-SELECCIONE-MVDOCTOS.
           PERFORM I-LABEL-MVDOCTOS   THRU F-LABEL-MVDOCTOS.
           PERFORM I-STATUS-MVDOCTOS  THRU F-STATUS-MVDOCTOS
           OPEN I-O MVDOCTOS.
       F-SELECCIONE-MVDOCTOS.
      ***************************************************************
      *                    I-ACTUALIZA-EMPRESAS                     *
      ***************************************************************
       I-ACT-EMPRESAS.
           OPEN    I-O  EMPRESAS.
           MOVE    "C"  TO WIN-FLAG-CIERRE.
           MOVE     0   TO WIN-PERIODO-ESTADO.
           REWRITE REC-EMPRESAS INVALID
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "No se puede grabar en registro empresas, paso de CIERRE."
            LINE 25 POSITION 01 BEEP
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
           CLOSE   EMPRESAS.
       F-ACT-EMPRESAS. EXIT.
      *
       PAGINA-REINICIO.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-REINICIO            TO  WK-PAGINA
                                               WI-Z4
               DISPLAY WI-Z4                        LINE 14 POSITION 24
               ACCEPT WK-REINICIO                   LINE 14 POSITION 24
                      CONVERT TAB NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WK-REINICIO                  =  ZEROS
               MOVE WK-PAGINA              TO  WK-REINICIO.
           IF  WK-REINICIO                  =  ZEROS
               ADD 1                       TO  WK-REINICIO.
               MOVE WK-REINICIO            TO  WK-PAGINA
                                               WI-Z4
               DISPLAY WI-Z4               LINE 14 POSITION 24.
       NUMERO-BANCO.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT BANC-CODIGO          LINE 17 POSITION 13
                      CONVERT  TAB NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  BANC-CODIGO                  =  ZEROS
               GO TO NUMERO-BANCO.
           PERFORM I-LEE-NOMIBANK THRU F-LEE-NOMIBANK
           IF SW-EOF  = 1
              DISPLAY "BANCO INVALIDO" LINE 24 POSITION 1
                   REVERSE BEEP GO NUMERO-BANCO.
           DISPLAY BANC-NOMBRE LINE 17 POSITION 16 REVERSE
           IF  CUP
               GO TO PAGINA-REINICIO.
           MOVE BANC-CODIGO            TO  WI-Z2  W-BANCO1
           DISPLAY WI-Z2               LINE 17 POSITION 13 REVERSE.
       NUMERO-CHEQUE.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE W-CHEQUE               TO  WI-Z7
               DISPLAY WI-Z7               LINE 17 POSITION 66
               ACCEPT W-CHEQUE             LINE 17 POSITION 66
                      CONVERT  TAB NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-CHEQUE                     =  ZEROS
               GO TO NUMERO-CHEQUE.
           IF  CUP GO TO NUMERO-BANCO.
           MOVE W-CHEQUE               TO  WI-Z7 C-CHEQ-DESDE
                                           WK-CHEQUE-INI.
            DISPLAY WI-Z7               LINE 17 POSITION 66.
       F-NUMERO-CHEQUE.
      *
       I-STATUS-MVDOCTOS.
               OPEN OUTPUT MVDOCTOS
               CLOSE MVDOCTOS.
       F-STATUS-MVDOCTOS.
      *
       I-WIN-PERIODO-ESTADO.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ       NO HA PROCESADO LA NOMINA DEFINITIVA;    บ"
            LINE 06 POSITION 17 BEEP REVERSE.
            DISPLAY "บ         NO PUEDE CONTINUAR CON ESTE PASO       บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 08 POSITION 17 BEEP REVERSE
            "   <ENTER>  CONTINUE  "
            LINE 09 POSITION 38 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-WIN-PERIODO-ESTADO.   EXIT.
      *
       I-VERI-TIPO-PAGO.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ       ESTA PROCESANTO PAGO EN CHEQUE Y LOS     บ"
            LINE 06 POSITION 17 BEEP REVERSE.
            DISPLAY "บ PARAMETROS INICIALES DE LA EMPRESA HAN SIDO    บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "บ   DEFINIDOS PARA PAGAR EN                      บ"
            LINE 08 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP REVERSE
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
      *     DISPLAY WIN-TIPO-PAGO LINE 11 POSITION 57
            IF WIN-TIPO-PAGO = 1
             DISPLAY "EFECTIVO" LINE 08 POSITION 48
            ELSE
             IF WIN-TIPO-PAGO = 3
              DISPLAY "CONSIGNACION" LINE 08 POSITION 48
             ELSE
              DISPLAY "  N.N.  " LINE 08 POSITION 48.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "     <ENTER> CONTINUE  "
            LINE 10 POSITION 30 BLINK.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
       F-VERI-TIPO-PAGO. EXIT.
      *
       I-CHEQUE.
              MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
              MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
              DISPLAY " "  LINE 22 POSITION 78 CONTROL SG-CONTROL-STRING
              DISPLAY "ULTIMO No. CHEQUE:"   LINE 22 POSITION 05
              DISPLAY W-CHEQUE         LINE 22 POSITION 35.
              DISPLAY "NRO CHEQUES:"   LINE 22 POSITION 45
              DISPLAY  M-NRO           LINE 22 POSITION 60.
              MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
              MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " "  LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-CHEQUE.
      *
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\CALCDIAS.PRO".
               COPY "..\PRO\LIQUINOV.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\CONTCHEQ.PRO".
               COPY "..\PRO\BUSCACON.PRO".
               COPY "..\PRO\PLANBANC.PRO".
               COPY "..\PRO\CHCONTRO.PRO".
               COPY "..\PRO\CONTREXT.PRO".
               COPY "..\PRO\CNEGRESO.PRO".

               COPY "..\PRO\NCHEQNUE.PRO".
               COPY "..\PRO\MONTESCT.PRO".
               COPY "..\PRO\TBCONDOC.PRO".
               COPY "..\PRO\VLRHORAS.PRO".
               COPY "..\PRO\RENC0911.PRO".
      *
               COPY "..\PLB\TABLAEMP.PLB".
               COPY "..\PLB\CONTRATO.PLB".
               COPY "..\PLB\NOMINOVE.PLB".
               COPY "..\PLB\TEMPINFO.PLB".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\CHCONTRO.PLB".
               COPY "..\PLB\REPORTES.PLB".
               COPY "..\PLB\CPLANILL.PLB".
               COPY "..\PLB\MVDOCTOS.PLB".
               COPY "..\PLB\NOMINOVP.PLB".
               COPY "..\PLB\CNEGRESO.PLB".
      *
               COPY "..\STA\TBCONDOC.STA".
      *****************************************************************
