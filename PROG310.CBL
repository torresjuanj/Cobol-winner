       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG310.
      ******************************************************************
      *  COMPROBANTES DE PAGO NOMINA      ABR-27-2002                  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\CONCPTOS.DYC".
               COPY "..\DYC\TABLAEMP.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CONTRATO.DYC".
               COPY "..\DYC\NOMINOVE.DYC".
               COPY "..\SEL\INFORMES.SEL".
               COPY "..\DYC\TEMPINFO.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CONCPTOS.FD".
               COPY "..\FD\TABLAEMP.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\NOMINOVE.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\TEMPINFO.FD".
       WORKING-STORAGE SECTION.
       77      ID-PRG              PIC X(7)  VALUE "PROG310".
       77      W-PROGRAMA           PIC X(7) VALUE "PROG310".
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA      PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CONTRATO.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\NOMINOVE.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\TEMPINFO.LBL".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
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
               COPY "..\WRK\LISTADOS.WRK".
       01      AREA-INFORME.
        03     AREA-WORKING-INFORME.
         05    I-CODIGO-EMPRESA          PIC 9(07)      VALUE ZEROS.

        03     TITULO-0.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                  VALUE 6960.
        05    FILLER                    PIC 9(4)       VALUE SPACES.
                                                        
        03     TITULO-1.
         05    FILLER                    PIC X(80)      VALUE SPACES.

        03     TITULO-2.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 4626.
         05    FILLER                    PIC X(10)      VALUE SPACES.
         05    I-FECHA-DESDE             PIC X(11)      VALUE ZEROS.
         05    FILLER                    PIC X(5)       VALUE SPACES.
         05    I-FECHA-HASTA             PIC X(11)      VALUE ZEROS.
         05    FILLER                    PIC X(15)      VALUE SPACES.
         05    I-CEDULA                  PIC ZZZZZ,ZZZ,ZZZ.
         05    FILLER                    PIC X(01)      VALUE "-".
         05    FILLER                    PIC X(10)      VALUE SPACES.
         05    I-PERIODO                 PIC ZZZ        VALUE ZEROS.

        03     TITULO-3.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 4626.
         05    FILLER                    PIC X(8)       VALUE SPACES.
         05    I-CODIGO                  PIC X(10)      VALUE SPACES.
         05    FILLER                    PIC X(9)       VALUE SPACES.
         05    I-NOMBRE                  PIC X(36)      VALUE SPACES.
         05    FILLER                    PIC X(9)       VALUE SPACES.
         05    I-SUELDO                  PIC ZZZZ,ZZZ-  VALUE ZEROS.

        03     TITULO-4.
         05    FILLER                    PIC X(01)      VALUE SPACES.
         05    I-EMPRESA                 PIC 9(07).
         05    FILLER                    PIC X(02)      VALUE SPACES.
         05    I-NOMEMP                  PIC X(40)      VALUE SPACES.

        03     TITULO-5.
         05    FILLER                    PIC X(80)      VALUE SPACES.

        03     TITULO-6.
         05    FILLER                    PIC X(80)      VALUE SPACES.

        03     LINEA-DETALLE.
         05    FILLER                    PIC XX         VALUE " ".
         05    I-NOV-CONCEPTO-X          PIC XXXX       VALUE SPACES.
         05    I-NOV-CONCEPTO            REDEFINES I-NOV-CONCEPTO-X
                                         PIC 999-.
         05    I-NOV-DESCRIPCION         PIC X(23)      VALUE SPACES.
         05    FILLER                    PIC X(3)       VALUE "  ".
         05    I-NOV-DIAS                PIC ZZZZ-      VALUE ZEROS.
         05    FILLER                    PIC XX         VALUE " ".
         05    I-NOV-HORAS               PIC ZZZ.ZZ-    VALUE ZEROS.
         05    FILLER                    PIC XX         VALUE " ".
         05    I-NOV-VLR-1               PIC ZZZZ,ZZZ-  VALUE ZEROS.
         05    FILLER                    PIC XX         VALUE " ".
         05    I-NOV-VLR-2               PIC ZZZZ,ZZZ-  VALUE ZEROS.
         05    FILLER                    PIC XX         VALUE " ".
         05    I-NOV-VLR-3               PIC ZZZZ,ZZZ-  VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
        03     LINEA-PASAN.
         05    FILLER                    PIC X(47)      VALUE
              "P A S A N                                      ".
         05    I-PASAN-VLR-1             PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
         05    I-PASAN-VLR-2             PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
         05    I-PASAN-VLR-3             PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
        03     LINEA-TOTAL-0.
         05    FILLER                    PIC X(80)      VALUE SPACES.
        03     LINEA-TOTAL-1.
         05    FILLER                    PIC X(47)      VALUE SPACES.
         05    I-DEVENGADOS              PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
         05    I-DESCUENTOS              PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
         05    I-NETO-A-PAGAR            PIC ZZZZZ,ZZZ- VALUE ZEROS.
         05    FILLER                    PIC X          VALUE " ".
        03     LINEA-TOTAL-2.
         05    FILLER                    PIC X(80)      VALUE SPACES.
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\CONTRATO.DCL".
               COPY "..\DCL\NOMINOVE.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\TEMPINFO.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
               COPY "..\PRO\CONTROLA.PRO".
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               MOVE WIN-NOMINA             TO  CON-PERIODO
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE    4                   TO  IND-INFORMES
               PERFORM I-PROCESO-AUDITE    THRU F-PROCESO-AUDITE.
               PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
               PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
               PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
               PERFORM I-LABEL-TEMPINFO    THRU F-LABEL-TEMPINFO.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.

               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT NOMINOVE
               MOVE 1                      TO  SW-ABRE
           IF  NOMINOVE-STATUS              =  "94"
               OPEN OUTPUT NOMINOVE
           ELSE
           IF  NOMINOVE-STATUS              >  "00"
               PERFORM RUTINA-ERROR-NOMINOVE.
               CLOSE NOMINOVE.
       010-ABRE-ARCHIVOS.
               OPEN INPUT  CONCPTOS
                           TABLAEMP
                           CONTRATO
                           HOJAVIDA
                           NOMINOVE
               OPEN OUTPUT INFORMES.
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
               READ TABLAEMP          WITH NO LOCK INVALID  KEY
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
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "º"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "WINNER FINANCIAL© º"        LINE  2 POSITION 62
               DISPLAY "º FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY "COMPROBANTES DE PAGO"
                                                    LINE  3 POSITION 28
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "º"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "Ñ"                          LINE  4 POSITION 33
               DISPLAY "Ñ"                          LINE  4 POSITION 54
               DISPLAY "º PERIODO:"                 LINE  5 POSITION  1
               DISPLAY CON-PERIODO                  LINE  5 POSITION 12
               DISPLAY "DESDE:"                     LINE  5 POSITION 16
               DISPLAY "³ DIAS PERIODO :"           LINE  5 POSITION 33
               DISPLAY "³ SEMANAS RFTE  :"           LINE  5 POSITION 54
               DISPLAY "º"                          LINE  5 POSITION 80
               DISPLAY "º"                          LINE  6 POSITION  1
               DISPLAY "HASTA:"                     LINE  6 POSITION 16
               DISPLAY "³ HORAS PERIODO:"           LINE  6 POSITION 33
               DISPLAY "³ FECHA DE PAGO:"           LINE  6 POSITION 54
               DISPLAY "º"                          LINE  6 POSITION 80
               DISPLAY WK-RAYA                      LINE  7 POSITION  1
               DISPLAY "Â"                          LINE  7 POSITION 29
               DISPLAY "Á"                          LINE  7 POSITION 33
               DISPLAY "Á"                          LINE  7 POSITION 54
               DISPLAY "Â"                          LINE  7 POSITION 57
               DISPLAY "º DEVENGADOS :"             LINE  8 POSITION  1
               DISPLAY "³ DESCUENTOS :"             LINE  8 POSITION 29
               DISPLAY "³ NETO :"                   LINE  8 POSITION 57
               DISPLAY "º"                          LINE  8 POSITION 80
               DISPLAY WK-RAYAS                     LINE  9 POSITION  1
               DISPLAY "Ï"                          LINE  9 POSITION 29
               DISPLAY "Ï"                          LINE  9 POSITION 57
               DISPLAY "º"                          LINE 10 POSITION  1
               DISPLAY "º"                          LINE 10 POSITION 80
               DISPLAY "º"                          LINE 11 POSITION  1
               DISPLAY "º"                          LINE 11 POSITION 80
               DISPLAY "º"                          LINE 12 POSITION  1
               DISPLAY "º"                          LINE 12 POSITION 80
               DISPLAY "º"                          LINE 13 POSITION  1
               DISPLAY "º"                          LINE 13 POSITION 80
               DISPLAY "º"                          LINE 14 POSITION  1
               DISPLAY "º"                          LINE 14 POSITION 80
               DISPLAY "º"                          LINE 15 POSITION  1
               DISPLAY "º"                          LINE 15 POSITION 80
               DISPLAY WK-RAYA                      LINE 16 POSITION  1
               DISPLAY "Ç"                          LINE 16 POSITION  1
               DISPLAY "¶"                          LINE 16 POSITION 80
               DISPLAY "º"                          LINE 17 POSITION  1
               DISPLAY "PAGINA DE REINICIO :"       LINE 17 POSITION  3
               DISPLAY "º"                          LINE 17 POSITION 80
               DISPLAY WK-RAYA                      LINE 18 POSITION  1
               DISPLAY "Ç"                          LINE 18 POSITION  1
               DISPLAY "¶"                          LINE 18 POSITION 80
               DISPLAY "º"                          LINE 19 POSITION  1
               DISPLAY "º"                          LINE 19 POSITION 80
               DISPLAY "º"                          LINE 20 POSITION  1
               DISPLAY "º"                          LINE 20 POSITION 80
               DISPLAY WK-RAYA                      LINE 21 POSITION  1
               DISPLAY "Ç"                          LINE 21 POSITION  1
               DISPLAY "¶"                          LINE 21 POSITION 80
               DISPLAY "º"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY "º"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.
               PERFORM  I-DISPLAY-LABEL       THRU  F-DISPLAY-LABEL.
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
               DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                                    LINE 24 POSITION 1
               DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32
               PERFORM ACEPTA-TRUCO
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
               GO TO CONFIRMA-INFORME.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       PAGINA-REINICIO.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-REINICIO            TO  WK-PAGINA
               WI-Z4
               DISPLAY WI-Z4                        LINE 17 POSITION 24
               ACCEPT WK-REINICIO                   LINE 17 POSITION 24
                      CONVERT
                      TAB
                      NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WK-REINICIO                  =  ZEROS
               MOVE WK-PAGINA              TO  WK-REINICIO.
           IF  WK-REINICIO                  =  ZEROS
               ADD 1                       TO  WK-REINICIO.
               MOVE WK-REINICIO            TO  WK-PAGINA
               WI-Z4
               DISPLAY WI-Z4                        LINE 17 POSITION 24
           IF  CUP
               GO TO CONFIRMA-INFORME.
       INICIE-INFORME.
               MOVE WIN-CODIGO-EMPRESA     TO  I-CODIGO-EMPRESA
               MOVE CON-PERIODO            TO  I-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  I-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  I-FECHA-HASTA
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
           WITH NO LOCK AT  END
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE TIMAE-CEDULA           TO  CONTRATO-CEDULA
               READ CONTRATO          WITH NO LOCK INVALID  KEY
               MOVE "97"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE CONTRATO-CEDULA        TO  HOJAVIDA-CEDULA
               READ HOJAVIDA          WITH NO LOCK INVALID  KEY
               MOVE "99"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               PERFORM BORRA-NOVEDADES THRU
                       SALE-BORRA-NOVEDADES
               PERFORM CARGA-NOVEDADES THRU
                       SALE-CARGA-NOVEDADES
           IF  W-TAMANO-NOVEDADES           = ZEROS
               GO TO 080-SIGUE-INFORME.
               PERFORM IMPRIME-PAGOS THRU
                       SALE-IMPRIME-PAGOS
               MOVE CONTRATO-CEDULA           TO  WI-Z12
               DISPLAY WI-Z12                       LINE 12 POSITION 12
               DISPLAY HOJAVIDA-NOMBRE         LINE 12 POSITION 35
               MOVE W-VALOR-DEVENGADOS     TO  WI-Z8P
               DISPLAY WI-Z8P                  LINE 14 POSITION 17
               MOVE W-VALOR-DESCUENTOS     TO  WI-Z8P
               DISPLAY WI-Z8P                  LINE 14 POSITION 45
               MOVE W-NETO-A-PAGAR         TO  WI-Z8P
               DISPLAY WI-Z8P                  LINE 14 POSITION 69
               ADD  W-NETO-A-PAGAR         TO  W-TOTAL-A-PAGAR
               ADD  W-VALOR-DEVENGADOS     TO  W-TOTAL-DEVENGADOS
               ADD  W-VALOR-DESCUENTOS     TO  W-TOTAL-DESCUENTOS
               MOVE W-TOTAL-DEVENGADOS     TO  WI-Z8P
               DISPLAY WI-Z8P                       LINE  8 POSITION 17
               MOVE W-TOTAL-DESCUENTOS     TO  WI-Z8P
               DISPLAY WI-Z8P                       LINE  8 POSITION 45
               MOVE W-TOTAL-A-PAGAR        TO  WI-Z8P
               DISPLAY WI-Z8P                       LINE  8 POSITION 69
               GO TO 080-SIGUE-INFORME.
       IMPRIME-PAGOS.
      *        MOVE WIN-CODIGO-EMPRESA     TO  I-CODIGO-EMPRESA
               MOVE CON-PERIODO            TO  I-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  WK-FECHA-STANDARD
               MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
               MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
               MOVE WK-MASCARA-FECHA       TO  I-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  WK-FECHA-STANDARD
               MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
               MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
               MOVE WK-MASCARA-FECHA       TO  I-FECHA-HASTA
               MOVE CONTRATO-CODIEMP          TO  I-CODIGO
               MOVE HOJAVIDA-NOMBRE           TO  I-NOMBRE
               MOVE CONTRATO-CEDULA           TO  I-CEDULA
               MOVE WIN-CODIGO-EMPRESA     TO  I-EMPRESA
               MOVE WIN-EMPRESA            TO  I-NOMEMP
               MOVE CONTRATO-SALARIO          TO  I-SUELDO
               PERFORM IMPRIME-ENCABEZADO THRU
                       SALE-IMPRIME-ENCABEZADO
               MOVE ZEROS                  TO  W-VALOR-DEVENGADOS
                                               W-VALOR-DESCUENTOS
               MOVE 1                      TO  I.
       SIGUE-IMPRIME-PAGOS.
               MOVE ZEROS                  TO  WK-LINEAS
               PERFORM IMPRIME-LINEA-DETALLE THRU
                       SALE-IMPRIME-LINEA-DETALLE
                       VARYING I         FROM  I  BY  1
                       UNTIL   I            >  W-TAMANO-NOVEDADES
                       OR      WK-LINEAS    =  17
           IF  I                            >  W-TAMANO-NOVEDADES
               NEXT SENTENCE
           ELSE
               PERFORM IMPRIME-PASAN THRU
                       SALE-IMPRIME-PASAN
               GO TO SIGUE-IMPRIME-PAGOS.
               PERFORM SALTA-LINEA-DETALLE THRU
                       SALE-SALTA-LINEA-DETALLE
                       UNTIL   WK-LINEAS    =  17
               PERFORM IMPRIME-TOTAL THRU
                       SALE-IMPRIME-TOTAL
               ADD  1                      TO  WK-PAGINA
           IF  WK-PAGINA                    =  3
               MOVE ZEROS                  TO  WK-PAGINA
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1.
       SALE-IMPRIME-PAGOS.
               EXIT.
       IMPRIME-ENCABEZADO.
               WRITE REG-INFORMES        FROM  TITULO-0        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-1        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-3        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-4        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-6        BEFORE 1.
       SALE-IMPRIME-ENCABEZADO.
               EXIT.
       IMPRIME-LINEA-DETALLE.
           IF  T-NOV-VLR               (I)  >  ZEROS
               MOVE ZEROS                  TO  I-NOV-VLR-1
                                               I-NOV-VLR-2
                                               I-NOV-VLR-3
               MOVE T-NOV-INDICE-TCN   (I) TO  J
               ADD  T-NOV-HORAS        (I) TO  T-TCN-HORAS        (J)
               ADD  T-NOV-DIAS         (I) TO  T-TCN-DIAS         (J)
               ADD  T-NOV-VLR          (I) TO  T-TCN-VLR          (J)
               MOVE T-NOV-CONCEPTO     (I) TO  I-NOV-CONCEPTO
               MOVE T-TCN-DESCRIPCION  (J) TO  I-NOV-DESCRIPCION
               MOVE T-NOV-DIAS         (I) TO  I-NOV-DIAS
               MOVE T-NOV-HORAS        (I) TO  I-NOV-HORAS
           IF  T-NOV-CONCEPTO          (I)  <  500
               MOVE T-NOV-VLR          (I) TO  I-NOV-VLR-1
               ADD  T-NOV-VLR          (I) TO  W-VALOR-DEVENGADOS
           ELSE
               MOVE T-NOV-VLR          (I) TO  I-NOV-VLR-2
               ADD  T-NOV-VLR          (I) TO  W-VALOR-DESCUENTOS.
           IF  T-NOV-VLR               (I)  >  ZEROS
               WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
               ADD  1                      TO  WK-LINEAS.
       SALE-IMPRIME-LINEA-DETALLE.
               EXIT.
       SALTA-LINEA-DETALLE.
               MOVE ZEROS                  TO  I-NOV-CONCEPTO
                                               I-NOV-DIAS
                                               I-NOV-HORAS
                                               I-NOV-VLR-1
                                               I-NOV-VLR-2
                                               I-NOV-VLR-3
               MOVE SPACES                 TO  I-NOV-DESCRIPCION
                                               I-NOV-CONCEPTO-X
               WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
               ADD  1                      TO  WK-LINEAS.
       SALE-SALTA-LINEA-DETALLE.
               EXIT.
       IMPRIME-PASAN.
               COMPUTE W-NETO-A-PAGAR       =  W-VALOR-DEVENGADOS
                                            -  W-VALOR-DESCUENTOS
               WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   BEFORE 1
               MOVE W-VALOR-DEVENGADOS     TO  I-PASAN-VLR-1
               MOVE W-VALOR-DESCUENTOS     TO  I-PASAN-VLR-2
               MOVE ZEROS                  TO  I-PASAN-VLR-3
               WRITE REG-INFORMES        FROM  LINEA-PASAN     BEFORE 1
               WRITE REG-INFORMES        FROM  LINEA-TOTAL-2   BEFORE 1
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1
               WRITE REG-INFORMES                              BEFORE 1
               ADD  1                      TO  WK-PAGINA
           IF  WK-PAGINA                    =  3
               MOVE ZEROS                  TO  WK-PAGINA
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1.
               PERFORM IMPRIME-ENCABEZADO THRU
                       SALE-IMPRIME-ENCABEZADO.
       SALE-IMPRIME-PASAN.
               EXIT.
       IMPRIME-TOTAL.
               COMPUTE W-NETO-A-PAGAR       =  W-VALOR-DEVENGADOS
                                            -  W-VALOR-DESCUENTOS
               MOVE W-VALOR-DEVENGADOS     TO  I-DEVENGADOS
               MOVE W-VALOR-DESCUENTOS     TO  I-DESCUENTOS
               MOVE W-NETO-A-PAGAR         TO  I-NETO-A-PAGAR
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1
               WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   BEFORE 1
               WRITE REG-INFORMES        FROM  LINEA-TOTAL-1   BEFORE 1
               WRITE REG-INFORMES        FROM  LINEA-TOTAL-2   BEFORE 1
               WRITE REG-INFORMES                              BEFORE 1.
       SALE-IMPRIME-TOTAL.
               EXIT.

       I-FIN-MODE.
               CLOSE CONCPTOS
                     TABLAEMP
                     CONTRATO
                     HOJAVIDA
                     NOMINOVE
                     INFORMES
                     TEMPINFO.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\LIQUINOV.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\CALCDIAS.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\VLRHORAS.PRO".
               COPY "..\PRO\DISLABEL.PRO".
      *
               COPY "..\PLB\TABLAEMP.PLB".
               COPY "..\PLB\CONTRATO.PLB".
               COPY "..\PLB\NOMINOVE.PLB".
               COPY "..\PLB\TEMPINFO.PLB".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\NOMINOVP.PLB".
      ******************************************************************

