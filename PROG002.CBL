      *****************************************************************
      * This application was written by Juan Jose Torres
      * and is protected under copyrigth and trademarks.
      * Licensed under MIT to be public at github for demostration 
      * purposes.
      * Winner Financial (r) - 1997-2003                  *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG002.
      ******************************************************************
      *        MANTENIMIENTO REGISTRO DE CONTROL EMPRESA ACTIVA        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      SW-NUEVA                  PIC 9          VALUE ZEROS.
       77      W-PERIODO                 PIC 9(03)      VALUE ZEROS.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\TECLADOS.WRK".
       01      AREA-WORKING-FECHA.
        03     WK-FECHA-ALFA-R.
         05    WK-FECHA-MES              PIC X(10)      VALUE SPACES.
         05    FILLER                    PIC X(3)       VALUE "   ".
         05    WK-FECHA-ANO              PIC 9(4)       VALUE ZEROS.
         05    FILLER                    PIC X(5)       VALUE SPACES.
        03     WK-FECHA-ALFA             REDEFINES WK-FECHA-ALFA-R
                                         PIC X(20).
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\EMPRESAS.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
               MOVE ZEROS                  TO  WRK-EMPRESAS
               MOVE CON-COD-EMPRESA        TO  WIN-CODIGO-EMPRESA.
               OPEN I-O EMPRESAS
               READ EMPRESAS WITH NO LOCK
               MOVE REC-EMPRESAS           TO  WRK-EMPRESAS
               COMPUTE CON-PERIODO          =  WIN-NOMINA
                                            +  1


               MOVE CON-PERIODO            TO  W-PERIODO
               DISPLAY PSW-2                        LINE  2 POSITION 17
                                             ERASE
               DISPLAY PSW-3                        LINE  3 POSITION 17
               DISPLAY PSW-4                        LINE  4 POSITION 17
               DISPLAY PSW-5                        LINE  5 POSITION 17
               DISPLAY PSW-6                        LINE  6 POSITION 17
               DISPLAY PSW-7                        LINE  7 POSITION 17
               DISPLAY PSW-8                        LINE  8 POSITION 17
               DISPLAY PSW-11                       LINE 11 POSITION 12
               DISPLAY PSW-12                       LINE 12 POSITION 12
               DISPLAY PSW-13                       LINE 13 POSITION 12
               DISPLAY PSW-14                       LINE 14 POSITION 12
               DISPLAY PSW-15                       LINE 15 POSITION 12
               DISPLAY PSW-16                       LINE 16 POSITION 12
               DISPLAY PSW-17                       LINE 17 POSITION 12
               DISPLAY PSW-18                       LINE 18 POSITION 12
               DISPLAY WRK-CODIGO-EMPRESA           LINE 05 POSITION 50
               DISPLAY WRK-FECHA-ANO                LINE 06 POSITION 50
               DISPLAY CON-PERIODO                  LINE 07 POSITION 50.
       011-EMPRESA.
               ACCEPT WRK-CODIGO-EMPRESA            LINE 05 POSITION 50
                      CONVERT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               GO TO 040-FIN.
           IF  WRK-CODIGO-EMPRESA           =  ZEROS
               MOVE CON-COD-EMPRESA        TO  WRK-CODIGO-EMPRESA.

               MOVE WRK-CODIGO-EMPRESA     TO  WIN-CODIGO-EMPRESA
               READ EMPRESAS          WITH NO LOCK INVALID  KEY
               DISPLAY " EMPRESA NO EXISTE "        LINE 24 POSITION 25
                                                    REVERSE
               GO TO 011-EMPRESA.
               DISPLAY WIN-EMPRESA  LINE  2 POSITION 4 BLINK.

           IF WIN-INACTIVA-EMP = "S"
             PERFORM I-ADVERTENCIA THRU F-ADVERTENCIA
             GO TO 040-FIN.

               MOVE REC-EMPRESAS           TO  WRK-EMPRESAS
               COMPUTE CON-PERIODO          =  WRK-NOMINA
                                            +  1
               MOVE CON-PERIODO            TO  W-PERIODO
               DISPLAY "                   "        LINE 24 POSITION 25.
       012-ANO.
           IF  F2
               GO TO 040-FIN.
           IF  CUP
               GO TO 011-EMPRESA.
            MOVE CON-ANO                TO  WRK-FECHA-ANO.
       013-PERIODO.
           IF  F2
               GO TO 040-FIN.
           IF  CUP
               GO TO 012-ANO.
           IF  W-PERIODO                    =  ZEROS
               MOVE CON-PERIODO            TO  W-PERIODO.
           IF  W-PERIODO                    <  1
               GO TO 013-PERIODO.


       035-GRABACION.
               MOVE WRK-CODIGO-EMPRESA     TO  WIN-CODIGO-EMPRESA
               MOVE ZEROS                  TO  SW-NUEVA
               READ EMPRESAS  WITH NO LOCK INVALID  KEY
               MOVE 1                      TO  SW-NUEVA.
               MOVE WRK-EMPRESAS           TO  REC-EMPRESAS
           IF  SW-NUEVA                     =  1
               WRITE REC-EMPRESAS
           ELSE
               REWRITE REC-EMPRESAS.
               MOVE WRK-CODIGO-EMPRESA     TO  CON-COD-EMPRESA
               MOVE WRK-FECHA-ANO          TO  CON-ANO
               MOVE W-PERIODO              TO  CON-PERIODO.
       040-FIN.
               CLOSE EMPRESAS.
       040-FINAL.
               EXIT PROGRAM.
       050-FT.
               STOP RUN.
      *
               COPY "..\PRO\TRUQUITO.PRO".
      *
       I-ADVERTENCIA.
            DISPLAY "������������������������������������������������ͻ"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "�              A T E N C I O N !                 �"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "�              EMPRESA INACTIVA                  �"
            LINE 10 POSITION 17 BEEP REVERSE.
            DISPLAY "������������������������������������������������ͼ"
            LINE 11 POSITION 17 BEEP REVERSE
            "  <ENTER> CONTINUE  "
            LINE 15 POSITION 19 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-ADVERTENCIA.  EXIT.
      ******************************************************************
