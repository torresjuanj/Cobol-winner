       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG715.
      ******************************************************************
      *      CONVERSION MOVTO DE CONDICION 1 A CONDICION  0
      *       E INICIALIZACION EN CEROS DEL HISTORICO   ABR-27-2002    *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNDOCTOS.DYC".
           COPY "..\DYC\CNCONTRO.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\CNDTACUM.FD".
           COPY "..\FD\CNCATCTA.FD".
           COPY "..\FD\CNCONTRO.FD".
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\CNDOCTOS.FD".
           COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG715".
       77      ID-PRG         PIC X(7)       VALUE "PROG715".
       77      FEC-DES        PIC 9(06)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(06)      VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS             THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PANTALLA-1B          THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES           THRU F-ACEPTA-MES
           IF   CON-PROGRAMA  NOT   =    "738"
                PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP.
           PERFORM I-LABEL-ARCHIVOS       THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS        THRU F-ABRE-ARCHIVOS
           MOVE    0                        TO FIN-OK
           PERFORM I-DISPLAY              THRU F-DISPLAY
           PERFORM I-LEE-CNDOCTOS         THRU F-LEE-CNDOCTOS
                               UNTIL FIN-OK = 1
           MOVE 0 TO FIN-OK
           MOVE
           "LIMPIANDO SALDOS HISTORICO CARTERA CNDTACUM "
           TO WK-SISTEMA
           PERFORM I-DISPLAY3              THRU  F-DISPLAY3

           IF   CON-PROGRAMA  NOT   =    "738"
                PERFORM I-PROCESO-CNDTACUM THRU  F-PROCESO-CNDTACUM
                                           UNTIL FIN-OK = 1.
           PERFORM I-FIN-MODE              THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-START-CNDTACUM.
               MOVE ZEROS                  TO    CNDTACUM-CLAVE.
               START CNDTACUM  KEY NOT LESS CNDTACUM-CLAVE INVALID KEY
                 DISPLAY "ERROR START-CNDTACUM"  LINE 23 POSITION 1.
       F-START-CNDTACUM.
      *
       I-LABEL-ARCHIVOS.
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
           MOVE W-CODEMP                 TO LAB-CNDOCTOS-DIRCIA
           MOVE "น"                      TO WK-RAYAS-U
           MOVE "ฬ"                      TO WK-RAYAS-P
           MOVE "ถ"                      TO WK-RAYA-U
           MOVE "ว"                      TO WK-RAYA-P
           MOVE 1                        TO SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
              PERFORM I-ABRE-CNCONTRO THRU F-ABRE-CNCONTRO.
              OPEN I-O CNDTACUM
              OPEN INPUT  EMPRESAS CNCATCTA.
              OPEN I-O    CNDOCTOS .
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
       I-PANTALLA-1B.
           MOVE
           "CONVERSION MOVTO DE CONDICION 1 A CONDICION  0"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A TRABAJAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 14 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 15 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS              TO WK-EXCEPTION
               IF   CON-PROGRAMA  =    "738" OR
                    CON-PROGRAMA  =    "783" OR
                    CON-PROGRAMA  =    "733"
                    MOVE  CON-PERIODO  TO W-LONG
               ELSE
                    ACCEPT W-LONG      LINE 11 POSITION 50 REVERSE
                                       UPDATE TAB NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
               IF   CON-PROGRAMA  =    "783"
                    MOVE  13           TO W-LONG.

               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
           IF   CON-PROGRAMA     =    "733" OR
                CON-PROGRAMA     =    "783"
                GO F-CONFIRMA-COMP.
                    DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
               GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNDOCTOS.
              DISPLAY ALTTIP-DOCTOS          LINE 20 POSITION 36
              DISPLAY ALTCTA-DOCTOS          LINE 20 POSITION 56
              DISPLAY ALTMES-DOCTOS          LINE 21 POSITION 36
              DISPLAY RADIC-DOCTOS           LINE 22 POSITION 36
              DISPLAY NUMFRA-DOCTOS          LINE 21 POSITION 56
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK
                   GO F-LEE-CNDOCTOS.
            MOVE 0                TO   CONDICION-DOCTOS.
            REWRITE REG-DOCTOS.
       F-LEE-CNDOCTOS.
      *
       I-LECTURA-EMPRESA.
           MOVE ALTEMP-DOCTOS   TO WIN-CODIGO-EMPRESA.
           READ EMPRESAS WITH NO LOCK INVALID KEY
               MOVE  1    TO SW1-300.
       F-LECTURA-EMPRESA.
      *
       I-PROCESO-CNDTACUM.
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK
                   GO F-PROCESO-CNDTACUM.
              DISPLAY "...LIMPIANDO SALDOS DEL HISTORICO..."
                       LINE 18 POSITION 25 REVERSE
              DISPLAY  CNDTACUM-MAYOR LINE 19  POSITION 40
              DISPLAY  CNDTACUM-SUB   LINE 19  POSITION 45
              DISPLAY  CNDTACUM-AUX   LINE 19  POSITION 48
              MOVE     CNDTACUM-NIT     TO WI-Z9
              DISPLAY  WI-Z9          LINE 19  POSITION 52
              DISPLAY  CNDTACUM-FRA   LINE 19  POSITION 64
              MOVE CNDTACUM-MOVDEB(W-LONG) TO WI-Z9P
              DISPLAY WI-Z9P LINE 20 POSITION 40
              MOVE CNDTACUM-MOVCRE(W-LONG) TO WI-Z9P
              DISPLAY WI-Z9P LINE 21 POSITION 40
              MOVE CNDTACUM-SALDO(W-LONG)  TO WI-Z9P
              DISPLAY WI-Z9P LINE 22 POSITION 40

              MOVE 0    TO CNDTACUM-MOVDEB(W-LONG)
              MOVE 0    TO CNDTACUM-MOVCRE(W-LONG)
              MOVE 0    TO CNDTACUM-SALDO(W-LONG)
              REWRITE REG-CNDTACUM.
       F-PROCESO-CNDTACUM.

       I-DISPLAY.
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ TIPO COMPROBANTE :       NR.CUENTA   :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ NUMERO DE MES    :       NR. FACTURA :
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ SECUENCIA REGISTR:
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.

       I-DISPLAY3.
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"                       LINE 18 POSITION 15
               DISPLAY "บ CUENTA CONTABLE    :
      -       "      บ"                       LINE 19 POSITION 15
               DISPLAY "บ MOVIMIENTO DEBITOS :
      -       "      บ"                       LINE 20 POSITION 15

               DISPLAY "บ MOVIMIENTO CREDITOS:
      -       "      บ"                       LINE 21 POSITION 15
               DISPLAY "บ SALDOS             :
      -       "      บ"                       LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"                       LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY3.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************
       I-FIN-MODE.
           CLOSE EMPRESAS CNDTACUM CNCONTRO
                 CNDOCTOS CNCATCTA.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".

               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\CNDTACUM.PLB".

