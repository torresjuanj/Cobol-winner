       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG746.
      ******************************************************************
      *           EMISION DE FOLIOS LIBROS OFICIALES                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG746".
       77      ID-PRG              PIC X(7)  VALUE "PROG746".
       77      W-CIA               PIC X(7)  VALUE SPACES.
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".

               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\LISTADOS.WRK".

      *******************************************************************
      *   En esta area se definen todos los titulos del informe general *
      *   de todas las empresas.                                        *
      *******************************************************************

       01      AREA-INFORME.
        03     TITULO-0.
           05 FILLER         PIC X(65)      VALUE SPACES.
           05 I-NOMBRE-CIA   PIC X(03).
           05 FILLER         PIC X(01)      VALUE "-".
           05 PAGS-E         PIC 99999.
           05 FILLER         PIC X(01)      VALUE SPACES.
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".

       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               MOVE WEMPRESA            TO  I-NOMBRE-CIA
               PERFORM I-LABEL-ARCHIVOS   THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS
               PERFORM I-PANTALLAS        THRU  F-PANTALLAS
               PERFORM I-PROCESO-FOLIOS   THRU F-PROCESO-FOLIOS
               VARYING X FROM      W-CTAD      BY 1
                          UNTIL X > W-CTAH.

               PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.

      *
       I-LABEL-ARCHIVOS.
               MOVE     1                  TO  IND-INFORMES
               PERFORM I-LABEL-INFORMES  THRU  F-LABEL-INFORMES
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN INPUT  EMPRESAS
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
       I-CONFIRMA-COMP.
           DISPLAY "PRESIONE <S> PARA CONFIRMAR          "
                    LINE 24 POSITION 1
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT =  "S" GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.

       I-PANTALLAS.
           IF F2  PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
           PERFORM I-PANTALLA-1D      THRU F-PANTALLA-1D
           PERFORM I-ACEPTA-CTA-DESDE THRU F-ACEPTA-CTA-DESDE
           PERFORM I-ACEPTA-CTA-HASTA THRU F-ACEPTA-CTA-HASTA
           PERFORM I-ACEPTA-CIA       THRU F-ACEPTA-CIA
           PERFORM I-CONFIRMA-COMP    THRU F-CONFIRMA-COMP
           MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA.
       F-PANTALLAS.
      *
       I-PROCESO-FOLIOS.
           MOVE X      TO PAGS-E.
           MOVE W-CIA  TO I-NOMBRE-CIA.
              MOVE  SPACES         TO REG-INFORMES
              WRITE REG-INFORMES               AFTER  3.
              WRITE REG-INFORMES FROM TITULO-0 AFTER  1.
              MOVE  SPACES         TO REG-INFORMES
              WRITE REG-INFORMES               AFTER  PAGE.
           PERFORM I-DISPLAY         THRU F-DISPLAY.
       F-PROCESO-FOLIOS.
      *
       I-DISPLAY.
                DISPLAY "FOLIOS :"          LINE 20 POSITION 25.
                DISPLAY  PAGS-E             LINE 20 POSITION 35.
       F-DISPLAY.

      *
        
       I-FIN-MODE.
           CLOSE EMPRESAS
                 INFORMES.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *

       I-PANTALLA-1D.
               MOVE "** NUMERACION AUTOMATICA DE FOLIOS ** "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE No.FOLIO  DESDE  :                 บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ  TECLEE No.FOLIO  HASTA  :                 บ"
                                               LINE 14 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 15 POSITION 20
               DISPLAY "บ  TECLEE LA EMPRESA       :                 บ"
                                               LINE 16 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 17 POSITION 20
               DISPLAY  SPACES LINE 24 POSITION 1 SIZE 80
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 25 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 25 POSITION 1 SIZE 80
               "NUMERACION FOLIOS --------->" LINE 25 POSITION 05
                LABEL-INFORMES                LINE 25 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 25 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 25 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-PANTALLA-1D.

       I-ACEPTA-CTA-DESDE.
               MOVE ZEROS                  TO  WK-EXCEPTION.
       I-WCTAD.
               DISPLAY  "          " LINE 11 POSITION 52
                                     REVERSE.
               ACCEPT W-CTAD  LINE 11 POSITION 52
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-ACEPTA-CTA-DESDE.
      *
       I-ACEPTA-CTA-HASTA.
               MOVE ZEROS                  TO  WK-EXCEPTION.
       I-WCTAH.
               DISPLAY  "          " LINE 14 POSITION 52
                                     REVERSE.
               ACCEPT W-CTAH  LINE 14 POSITION 52
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF W-CTAH < W-CTAD
                  DISPLAY
                  "EL FOLIO HASTA NO PUEDE SER MENOR AL FOLIO DESDE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                  GO I-WCTAH.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-ACEPTA-CTA-HASTA.
      *
       I-ACEPTA-CIA.
               MOVE ZEROS                  TO  WK-EXCEPTION.
               DISPLAY  "          " LINE 16 POSITION 52
                                     REVERSE.
               ACCEPT W-CIA   LINE 16 POSITION 52
                      REVERSE  TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
               IF W-CIA = SPACES GO I-ACEPTA-CIA.
       F-ACEPTA-CIA.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PLB\INFORMES.PLB".
