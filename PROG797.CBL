       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG797.
      ******************************************************************
      *           LISTADO DE SECUENCIAS    ABR-27-2002                 *
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
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG797".
       77      ID-PRG              PIC X(7)  VALUE "PROG797".
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
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
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(59)    VALUE SPACES.
         05    FILLER      PIC X(42)    VALUE
              "I N F O R M A C I O N     C O N T A B L E".
         05    FILLER      PIC X(65)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).

        03     TITULO-2.
         05    I-CAMPO     PIC X(165)   VALUE SPACES.
         05    FILLER      PIC X(11)    VALUE "Programa : ".
         05    I-PROGRAMA  PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(18)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.

        03     TITULO-3.
         05    FILLER      PIC X(230)   VALUE ALL "=".

        03     LINEA-DE-DETALLE.
         05    FILLER      PIC X(02)    VALUE " ".
         05    RADICTIP-DOCTOS-1   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-1   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-1   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-2   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-2   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-2   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-3   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-3   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-3   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-4   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-4   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-4   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-5   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-5   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-5   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-6   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-6   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-6   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-7   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-7   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-7   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-8   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-8   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-8   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-9   PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-9   PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-9   PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-10  PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-10  PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-10  PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-11  PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-11  PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-11  PIC 9(06).
         05    FILLER      PIC X(03)    VALUE " ".
         05    RADICTIP-DOCTOS-12  PIC 9(03).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADICDOC-DOCTOS-12  PIC 9(06).
         05    FILLER      PIC X(01)    VALUE "-".
         05    RADIC-AUTOMATICA-12  PIC 9(06).

      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".

       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS         THRU  F-USUARIOS.
               IF      SW-USER = 1        EXIT  PROGRAM.
               PERFORM I-PANTALLA-1B      THRU  F-PANTALLA-1B
               PERFORM I-ACEPTA-MES       THRU  F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP    THRU  F-CONFIRMA-COMP
               PERFORM I-LABEL-ARCHIVOS   THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS
               PERFORM I-PANTALLA         THRU  F-PANTALLA
               PERFORM I-DISPLAY-LABEL    THRU  F-DISPLAY-LABEL
               PERFORM I-PROCESO          THRU  F-PROCESO
               PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.

      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
       I-PANTALLA-1B.
           MOVE "           INFORMATIVO DE SECUENCIAS GRABADAS"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A REPORTAR :                บ"
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
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 50
                                                    REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 12
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.

      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
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
       I-LABEL-ARCHIVOS.
           MOVE W-PROGRAMA             TO  I-PROGRAMA
           MOVE WK-MASCARA-FECHA       TO  I-FECHA
           MOVE WK-MASCARA-HORA        TO  I-HORA
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE 3                      TO  IND-INFORMES.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           MOVE W-CODEMP               TO  LAB-CNDOCTOS-DIRCIA
           MOVE CON-ANO                TO  LAB-CNDOCTOS-ANO
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN INPUT  EMPRESAS CNDOCTOS
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
       I-PANTALLA.
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "บ"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "INFORMATIVO DE SECUENCIAS"  LINE  2 POSITION 25
               DISPLAY "บ"                          LINE  2 POSITION 80
               DISPLAY "บ FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "บ"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "บ"                          LINE  4 POSITION 80
               DISPLAY "บ"                          LINE  5 POSITION  1
               DISPLAY "บ"                          LINE  5 POSITION 80
               DISPLAY "บ"                          LINE  6 POSITION  1
               DISPLAY "บ"                          LINE  6 POSITION 80
               DISPLAY "บ"                          LINE  7 POSITION  1
               DISPLAY "บ"                          LINE  7 POSITION 80
               DISPLAY "บ"                          LINE  8 POSITION  1
               DISPLAY "บ"                          LINE  8 POSITION 80
               DISPLAY "บ"                          LINE  9 POSITION  1
               DISPLAY "บ"                          LINE  9 POSITION 80
               DISPLAY "บ"                          LINE 10 POSITION  1
               DISPLAY "บ"                          LINE 10 POSITION 80
               DISPLAY "บ"                          LINE 11 POSITION  1
               DISPLAY "บ"                          LINE 11 POSITION 80
               DISPLAY "บ"                          LINE 12 POSITION  1
               DISPLAY "บ"                          LINE 12 POSITION 80
               DISPLAY "บ"                          LINE 13 POSITION  1
               DISPLAY "บ"                          LINE 13 POSITION 80
               DISPLAY "บ"                          LINE 14 POSITION  1
               DISPLAY "บ"                          LINE 14 POSITION 80
               DISPLAY "บ"                          LINE 15 POSITION  1
               DISPLAY "บ"                          LINE 15 POSITION 80
               DISPLAY WK-RAYA                      LINE 16 POSITION  1
               DISPLAY "ว"                          LINE 16 POSITION  1
               DISPLAY "ถ"                          LINE 16 POSITION 80
               DISPLAY "บ"                          LINE 17 POSITION  1
               DISPLAY "[ F4 ] LLAVE PRINCIPAL  "   LINE 17 POSITION 05
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
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY "ARCHIVO DE IMPRESION ---->" LINE 22 POSITION 35
                        REVERSE BLINK
               DISPLAY " I797    .    "             LINE 22 POSITION 63
                                                          BLINK REVERSE
               DISPLAY CON-ANO                      LINE 22 POSITION 68
                                                          BLINK REVERSE
               DISPLAY W-LONG                       LINE 22 POSITION 70
                                                          BLINK REVERSE
               DISPLAY CON-COD-EMPRESA              LINE 22 POSITION 73
                                                          BLINK REVERSE
               DISPLAY "บ"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.
       CONFIRMA-INFORME.
               DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                                    LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32
                                                    REVERSE

               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA.

       I-PROCESO.
           PERFORM ACEPTA-TRUCO
           IF F2  PERFORM I-FIN-MODE  THRU  F-FIN-MODE
               ELSE
               IF F4
                   MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA
                   PERFORM I-INFORME  THRU  F-INFORME
                          UNTIL FIN-OK = 1
                ELSE
                         GO TO CONFIRMA-INFORME.
       F-PROCESO.
      *
       I-INFORME.
           IF FIN-OK = 1 GO F-INFORME.
           PERFORM I-PROCESO-CNDOCTOS THRU F-PROCESO-CNDOCTOS
                VARYING X FROM 1 BY 1 UNTIL X > 12.
           PERFORM I-DETALLE THRU F-DETALLE.
       F-INFORME.

       I-DETALLE.
                MOVE WK-MASCARA-FECHA       TO  I-FECHA
                MOVE WK-MASCARA-HORA        TO  I-HORA
                MOVE WEMPRESA               TO  TIT-CIA
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO W-CONTA
                ADD 1                      TO CON-LIN.
       F-DETALLE.

       I-PROCESO-CNDOCTOS.
           PERFORM I-LEE-CNDOCTOS   THRU F-LEE-CNDOCTOS.
           PERFORM I-MUEVE-INFORME   THRU F-MUEVE-INFORME.
           PERFORM I-DISPLAY         THRU F-DISPLAY.
       F-PROCESO-CNDOCTOS.
      *
       I-DISPLAY.
                DISPLAY "LLAVE PRINCIPAL:"  LINE 8 POSITION 05
                DISPLAY  RADIC-DOCTOS       LINE 8 POSITION 25.
       F-DISPLAY.
      *
       I-LEE-CNDOCTOS.
               IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
               READ CNDOCTOS NEXT WITH NO LOCK AT END MOVE 1 TO FIN-OK.
       F-LEE-CNDOCTOS.
      *
       I-MUEVE-INFORME.
               IF X = 1
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-1
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-1
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-1.
               IF X = 2
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-2
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-2
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-2.
               IF X = 3
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-3
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-3
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-3.
               IF X = 4
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-4
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-4
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-4.
               IF X = 5
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-5
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-5
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-5.
               IF X = 6
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-6
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-6
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-6.
               IF X = 7
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-7
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-7
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-7.
               IF X = 8
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-8
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-8
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-8.
               IF X = 9
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-9
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-9
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-9.
               IF X = 10
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-10
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-10
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-10.
               IF X = 11
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-11
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-11
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-11.
               IF X = 12
                MOVE RADICTIP-DOCTOS  TO RADICTIP-DOCTOS-12
                MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTOS-12
                MOVE RADIC-AUTOMATICA-DOCTOS TO RADIC-AUTOMATICA-12.
       F-MUEVE-INFORME.
      *
       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
          
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               MOVE 3                          TO  CON-LIN.
       F-TITULO-INFORME.
        
       I-FIN-MODE.
           CLOSE CNDOCTOS EMPRESAS
                 INFORMES.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\CNDOCTOS.PLB".
