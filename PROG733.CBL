       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG733.
      ******************************************************************
      *  PROCESA PROG715 Y EL PROG781  DESDE UN MES DETERMINADO 
      *   ABR-27-2002 
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
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG733".
       77      ID-PRG              PIC X(7)  VALUE "PROG733".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           MOVE 0 TO SW1 SW1 SW-EOF FIN-OK  WK-PAGINA W-CONTA CON-LIN.
           PERFORM I-PROCESO-AUDITE    THRU  F-PROCESO-AUDITE
           PERFORM I-DISPLAY           THRU  F-DISPLAY.
           PERFORM I-ACEPTA-MES        THRU  F-ACEPTA-MES.
           PERFORM I-CONFIRMA-COMP     THRU  F-CONFIRMA-COMP
           MOVE 733                      TO  CON-PROGRAMA
           PERFORM I-ACTUAL            THRU  F-ACTUAL.
           CALL   "PROG980.COB" USING AREA-LINK-NOMINA.
           PERFORM I-FIN-MODE          THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-FIN-MODE.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-ACTUAL.
               CANCEL "PROG733.COB"
               MOVE    W-LONG TO CON-PERIODO.
               PERFORM I-BORRA-MESES  THRU F-BORRA-MESES
                       VARYING CON-PERIODO FROM W-LONG  BY 1
                       UNTIL CON-PERIODO > 12.
       F-ACTUAL.
      *
       I-BORRA-MESES.
               CANCEL "PROG733.COB"
               CALL   "PROG715.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG715.COB".
               CALL   "PROG781.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG781.COB".
       F-BORRA-MESES.
      *
       I-ACEPTA-MES.
           MOVE ZEROS TO WK-EXCEPTION
           ACCEPT W-LONG LINE 11 POSITION 40 REVERSE
               CONVERT NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
           IF  F2  OR  ESC  PERFORM F-FIN-MODE  THRU   F-FIN-MODE.
           IF W-LONG = 0 OR W-LONG > 14
              DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-ACEPTA-MES.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 45 REVERSE.
               IF W-LONG = 1
                 COMPUTE MESW = W-LONG + 12
               ELSE
                 COMPUTE MESW = W-LONG - 1.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE  <C> CONTINUAR "
                               LINE 24 POSITION 1 REVERSE
                    DISPLAY " [F2] TERMINAR "
                               LINE 24 POSITION 32 REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-DISPLAY.
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.

               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
              LINE 09 POSITION 15 CONTROL SG-CONTROL-STRING
               DISPLAY "บ
      -       "      บ"
                                              LINE 10 POSITION 15
               DISPLAY "บ MES A PROCESAR   :
      -       "      บ"
                                              LINE 11 POSITION 15

               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 12 POSITION 15
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "[ESC MENU]  [F2 SALIR] "
           LINE 25 POSITION 30 CONTROL SG-CONTROL-STRING HIGH REVERSE.
           DISPLAY "SISTEMA-FINANCIERO"  LINE 3  POSITION 2
           DISPLAY
           "ACTUALIZACION EN LINEA DESDE UN MES DETERMINADO"
                     LINE    6  POSITION   25 REVERSE BEEP.
       RUTINA-ERROR-NOMINA.
       F-DISPLAY.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\PANTALLAS.PRO".



