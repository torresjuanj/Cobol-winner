       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG795.
      ******************************************************************
      *        IMPRESION DE NOTAS CREDITO CLIENTES                     *
      *                NOTAS  FORMA  MINERVA     ABR-27-2002           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CNDNOTAS.DYC".
               COPY "..\SEL\REPORTES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDNOTAS.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\REPORTES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG795".
       77      ID-PRG              PIC X(7)  VALUE "PROG795".
       77      WK-CHEQUEORD        PIC 9(05) VALUE ZEROS.
       77      WK-CHEQUE           PIC 9(10) VALUE ZEROS.
       77      WK-CODFAC           PIC 9(6)  VALUE ZEROS.
       77      KK                  PIC 9     VALUE ZEROS.
       77      AA                  PIC 9     VALUE ZEROS.
       77      WK-RADICA           PIC 9(6)  VALUE ZEROS.
       77      EGRE-INICIAL        PIC 9(6)  VALUE ZEROS.
       77      EGRE-FINAL          PIC 9(6)  VALUE ZEROS.
       77      ANIO                PIC 9(4)  VALUE ZEROS.
       77      WR-LINEAS           PIC 9(4)  VALUE ZEROS.
       77      IMPRE               PIC 9     VALUE ZEROS.
       77      LIN-SUELDO          PIC 9     VALUE ZEROS.
       77      WI-I10              PIC ZZZZZZZZZZ VALUE ZEROS.
       77      WK-EMPRE-INICIAL    PIC 9(3)  VALUE ZEROS.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNDNOTAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\CNINOTAS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\LISTADOS.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNDNOTAS.DCL".
               COPY "..\DCL\REPORTES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           MOVE WIN-CODIGO-EMPRESA   TO WK-EMPRE-INICIAL
           PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
           PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               MOVE "NOTAS DEBITO&CREDITO" TO  TIT-APLICACION
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
               MOVE     4                  TO   IND-REPORTES
               PERFORM I-LABEL-CNDNOTAS  THRU  F-LABEL-CNDNOTAS.
               PERFORM I-STATUS-CNDNOTAS THRU  F-STATUS-CNDNOTAS.
               PERFORM I-LABEL-REPORTES  THRU  F-LABEL-REPORTES.
               PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-LABEL-ARCHIVOS.
      *
      *
       I-DISPLAY-LABEL.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 20 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 20 POSITION 1 SIZE 80
               "FORMATO DE NOTAS DEB/CRED-->" LINE 20 POSITION 05
                LABEL-REPORTES                LINE 20 POSITION 33
                NOM-PAPEL(IND-REPORTES)       LINE 20 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY-LABEL.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           DISPLAY "TRABAJANDO MES : " LINE 08 POSITION 27 REVERSE.
           DISPLAY  W-LONG           LINE 08 POSITION 45 REVERSE
           MOVE ZEROS           TO   FIN-OK
           MOVE  1  TO SW-MODE
           IF IMPRE = 2
               PERFORM I-WINDOW-DATO1     THRU  F-WINDOW-DATO1
               PERFORM I-ACEPTA-CNDNOTAS    THRU  F-ACEPTA-CNDNOTAS.
           PERFORM I-IMPRIME-CNDNOTAS      THRU  F-IMPRIME-CNDNOTAS.
       F-MENU-OPCIONES.
      *
      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************
       I-FIN-MODE.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-DATO1.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 10 POSITION 20.
           DISPLAY "บ   REIMPRESION NOTAS CREDITO              บ"
                                        LINE 11 POSITION 20.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 12 POSITION 20.
           DISPLAY "บ   DESDE :        HASTA :                 บ"
                                        LINE 13 POSITION 20.
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 14 POSITION 20.
       F-WINDOW-DATO1.
      *
       I-ACEPTA-CNDNOTAS.
           DISPLAY SPACES  LINE 13  POSITION 33 SIZE 05 REVERSE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-INICIAL LINE 13 POSITION 33
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-ACEPTA-FINAL.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-INICIAL LINE 13 POSITION 33  REVERSE LOW
           IF  EGRE-INICIAL = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-ACEPTA-CNDNOTAS.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       I-ACEPTA-FINAL.
           DISPLAY SPACES  LINE 13  POSITION 49 SIZE 05 REVERSE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-FINAL   LINE 13 POSITION 49
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE  LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-ACEPTA-CNDNOTAS.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-FINAL   LINE 13 POSITION 49  REVERSE LOW
           IF  EGRE-FINAL   = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-ACEPTA-FINAL.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       F-ACEPTA-CNDNOTAS.
      *

       I-PANTALLA-1B.
           MOVE "            MOVIMIENTO A MODIFICAR "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  COMPROBANTE A                        บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ  GENERAR        :                     บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                       บ"
                                               LINE 13 POSITION 20
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS             TO  WK-EXCEPTION
               ACCEPT W-LONG              LINE 12 POSITION 42 REVERSE
                                          CONVERT TAB NO BEEP
                                          ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS        LINE 24 POSITION 1
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               IF  W-LONG  =  12
                   DISPLAY " NOTAS CREDITO" LINE 12 POSITION 45
                   GO F-ACEPTA-MES.
               IF  W-LONG  =  14
                   DISPLAY " NOTAS DEBITO " LINE 12 POSITION 45
                   GO F-ACEPTA-MES.
               GO I-ACEPTA-MES.
       F-ACEPTA-MES.

       I-CONFIRMA-COMP.
             MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
             MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
             DISPLAY " "
                    LINE   25  POSITION 01 CONTROL SG-CONTROL-STRING.

             DISPLAY "PRESS--> <C> CONTINUAR  "
                                    LINE 25 POSITION 1

             DISPLAY " [F9] REIMPRESION C.E."
                                    LINE 25 POSITION 35

             DISPLAY " [F2] SALIR" LINE 25 POSITION 67

             MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
             MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
             DISPLAY " "
                    LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
             DISPLAY "<C>"  LINE 25 POSITION 10
             DISPLAY "[F9]" LINE 25 POSITION 36
             DISPLAY "[F2]" LINE 25 POSITION 68
             PERFORM ACEPTA-TRUCO
             DISPLAY  WK-ESPACIOS  LINE 25 POSITION  1.
             IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
             IF  F6  MOVE 1  TO  IMPRE
                     GO F-CONFIRMA-COMP.
             IF  F8  MOVE 5  TO  LL
                     GO I-CONFIRMA-COMP.
             IF  F9  MOVE 2  TO  IMPRE
                MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
                MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
                DISPLAY " "   LINE   24  POSITION 80
                  CONTROL SG-CONTROL-STRING
                     GO F-CONFIRMA-COMP.
             IF  W-TRUCO                  NOT =  "C"
                          GO TO I-CONFIRMA-COMP.
             MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
             MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
             DISPLAY " "
                    LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING.
             MOVE 1  TO  IMPRE.
       F-CONFIRMA-COMP.
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\IMPNOTAS.PRO".
               COPY "..\PLB\CNDNOTAS.PLB".
               COPY "..\PLB\REPORTES.PLB".

