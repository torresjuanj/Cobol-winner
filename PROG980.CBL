       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG980.
      ******************************************************************
      * ACTUALIZACION DE SALDOS                                        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNCONTRO.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG980".
       77      ID-PRG         PIC X(7)       VALUE "PROG980".
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".

               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           MOVE    ZEROS TO SW1.
           PERFORM I-PANTALLA-1B      THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES       THRU F-ACEPTA-MES
           PERFORM I-PROCESO-AUDITE   THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           PERFORM I-DISPLAY          THRU F-DISPLAY
           MOVE    0 TO FIN-OK
           PERFORM I-EVALUE-SALDO     THRU F-EVALUE-SALDO
           PERFORM I-FIN-MODE         THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-ABRE-CNCONTRO THRU F-ABRE-CNCONTRO.
           OPEN I-O   CNDTACUM.
       F-ABRE-ARCHIVOS.
      *
       I-PANTALLA-1B.
           MOVE "* A C T U A L I Z A C I O N   S A L D O  S"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  DIGITE EL MES A ACTUALIZAR:               บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ              [F2] TERMINAR                 บ"
                                               LINE 12 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE  14  TO W-LONG
               MOVE ZEROS                  TO  WK-EXCEPTION
               IF   CON-PROGRAMA  =    "738"  OR
                    CON-PROGRAMA  =    "733"  OR
                    CON-PROGRAMA  =    "980"
                    MOVE  14  TO W-LONG
               ELSE
                 ACCEPT W-LONG         LINE 11 POSITION 50 REVERSE
                                       UPDATE TAB NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS   LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF   CON-PROGRAMA  =  "738"  AND CON-PERIODO NOT = ZEROS
                    MOVE  CON-PERIODO  TO W-LONG.

               IF   CON-PROGRAMA  =  "733"
                    MOVE  CON-PERIODO  TO W-LONG.
               IF   CON-PROGRAMA  =  "980"
                    MOVE  14           TO W-LONG.

           IF  F2  PERFORM I-FIN-SALIR THRU F-FIN-MODE.
           IF W-LONG = 1  OR W-LONG = 14
                  MOVE 14    TO    MESW
           ELSE
                  COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
      *
       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-LEE-CNDTACUM-S.
              IF  FIN-OK =  1   GO  F-LEE-CNDTACUM-S.
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                 MOVE 1 TO FIN-OK.
              DISPLAY CON-ANO                LINE 20 POSITION 36
              DISPLAY CNDTACUM-MAYOR         LINE 20 POSITION 56
              DISPLAY CNDTACUM-SUB           LINE 20 POSITION 61
              DISPLAY CNDTACUM-AUX           LINE 20 POSITION 63
              DISPLAY CNDTACUM-NIT           LINE 22 POSITION 36
              DISPLAY CNDTACUM-FRA           LINE 21 POSITION 56.
              DISPLAY W-LONG             LINE 20 POSITION 39.
       F-LEE-CNDTACUM-S.
      *
       I-EVALUE-SALDO.
           DISPLAY
              "PROCESAMIENTO DE ACTUALIZACION DE SALDOS"
              LINE 18 POSITION 21 REVERSE.
              PERFORM I-START-CNDTACUM   THRU F-START-CNDTACUM
              MOVE    ZEROS     TO FIN-OK.
              PERFORM I-BUSCA-CNDTACUM   THRU F-BUSCA-CNDTACUM
                               UNTIL FIN-OK = 1.
              MOVE  1   TO W-LONG.
       F-EVALUE-SALDO.
      *
       I-BUSCA-CNDTACUM.
           PERFORM I-LEE-CNDTACUM-S THRU F-LEE-CNDTACUM-S
           IF FIN-OK = 1              GO F-BUSCA-CNDTACUM.
           MOVE    14  TO W-LONG
           MOVE    14  TO MESW.
       I-BUSCA.
           PERFORM I-SALDO-NUEVO    THRU F-SALDO-NUEVO
           IF  MESW = 14  AND W-LONG = 14
               MOVE  14 TO MESW
               MOVE   1 TO W-LONG
               GO I-BUSCA.

           IF  MESW = 14
               MOVE  1  TO MESW
           ELSE
               ADD   1  TO MESW.
           ADD 1  TO W-LONG.
           IF  W-LONG <  13
               GO I-BUSCA.
           PERFORM I-GRABA-CNDTACUM THRU F-GRABA-CNDTACUM.
       F-BUSCA-CNDTACUM.
      *
       I-SALDO-NUEVO.
           MOVE 0  TO CNDTACUM-SALDO(W-LONG)
           IF  MESW = 14  AND W-LONG = 14
               MOVE 0  TO CNDTACUM-SALDO(MESW).
           COMPUTE CNDTACUM-SALDO(W-LONG) = CNDTACUM-SALDO(MESW)
                                          + CNDTACUM-MOVDEB(W-LONG)
                                          + CNDTACUM-MOVCRE(W-LONG).
       F-SALDO-NUEVO.
      *
       I-GRABA-CNDTACUM.
           DISPLAY CON-ANO                LINE 20 POSITION 36
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY " "  LINE 24 POSITION 80
                         CONTROL SG-CONTROL-STRING.
           WRITE REG-CNDTACUM INVALID KEY REWRITE REG-CNDTACUM.
       F-GRABA-CNDTACUM.
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
           IF   CON-PROGRAMA   =    "733"
                GO F-CONFIRMA-COMP.
                    DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-SALIR THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
               GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO    W-NOMBRE.
       F-LEE-EMPRESAS.
      *
       I-DISPLAY.
      *        PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ TIPO DE COMPBTE  :       No.CUENTA   :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ RADICACION No.   :       No. FACTURA :
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ N.I.T.No.        :
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.

       I-DISPLAY2.
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY
              "PROCESAMIENTO DE ACTUALIZACION DE SALDOS"
              LINE 18 POSITION 21 REVERSE BEEP.
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ ANO CONTABLE     :       No.CUENTA   :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ MES CONTABLE     :       NR. FACTURA :
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ SECUENCIA REGISTR:
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY2.

      ***************************************************************
       I-FIN-MODE.
           DISPLAY
              "CIERRE DE ARCHIVOS... "
              LINE 18 POSITION 21 REVERSE BEEP.
           CLOSE CNDTACUM
                 CNCONTRO.
       I-FIN-SALIR.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PLB\CNDTACUM.PLB".

