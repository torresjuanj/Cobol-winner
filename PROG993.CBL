       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG993.
      *----------------------------------------------------------------*
      * INFORME NOMIMENU       ABR-27-2002                                        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\NOMIMENU.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\SEL\REPORTES.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\NOMIMENU.FD".
           COPY "..\FD\INFORMES.FD".
           COPY "..\FD\REPORTES.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA PIC X(7) VALUE "PROG993".
       77  ID-PRG     PIC X(7) VALUE "PROG993".
       77  W-MENU                       PIC X(12)       VALUE SPACES.
      *
       01  LINEA.
           03 FILLER PIC X(2)  VALUE SPACES.
           03 PROGRA PIC X(10) VALUE SPACES.

           COPY "..\WRK\LISTADOS.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
      *
           COPY "..\LBL\REPORTES.LBL".
           COPY "..\LBL\NOMIMENU.LBL".
           COPY "..\LBL\INFORMES.LBL".
      *
       PROCEDURE DIVISION.
       PROGRAMA-PRINCIPAL SECTION.
       I-PROGRAMA-PRINCIPAL.
           PERFORM  I-LABEL-ARCHIVOS   THRU  F-LABEL-ARCHIVOS.
           PERFORM  I-PANTALLA         THRU  F-PANTALLA.
           PERFORM  I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS.
           PERFORM  I-PROCESO-INFORME  THRU  F-PROCESO-INFORME.
           PERFORM  I-FIN-MODE         THRU  F-FIN-MODE.
       F-PROGRAMA-PRINCIPAL.
      *
       I-LABEL-ARCHIVOS.
           MOVE  1  TO  IND-INFORMES.
           MOVE  993    TO  LAB-INFORMES-IDENT.
           MOVE  ZEROS  TO  LAB-INFORMES-ANO
                            LAB-INFORMES-CIA.
           MOVE DIR-PAPEL(IND-INFORMES)  TO LAB-INFORMES-PAPEL.
           MOVE  1  TO  IND-REPORTES.
           MOVE  993    TO  LAB-REPORTES-IDENT.
           MOVE  ZEROS  TO  LAB-REPORTES-ANO
                            LAB-REPORTES-CIA.
           MOVE DIR-PAPEL(IND-REPORTES)  TO LAB-REPORTES-PAPEL.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-PANTALLA.
           DISPLAY " " LINE 01 POSITION 01 ERASE
           "ARCHIVO DE IMPRESION ---->" LINE 23 POSITION 03 BLINK.
           PERFORM I-RECIBE-MENU THRU F-RECIBE-MENU.
       F-PANTALLA. EXIT.
      *
       I-RECIBE-MENU.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY " Nombre menu que desea imprimir "
            LINE 20 POSITION 02
           DISPLAY " <F2> TERMINAR " LINE 25 POSITION 01
           ACCEPT  W-MENU  LINE 20 POSITION 34
                           TAB NO BEEP
                        ON EXCEPTION WK-EX
                           PERFORM 999-EXCEPTION.
           IF F2 STOP RUN.
           IF  W-MENU  =  SPACES
             GO TO I-RECIBE-MENU.
       F-RECIBE-MENU. EXIT.
      *
       I-ABRE-ARCHIVOS.
           DISPLAY
           LABEL-INFORMES               LINE 23 POSITION 30 REVERSE
           NOM-PAPEL (IND-INFORMES)     LINE 23 POSITION 66 REVERSE.
           OPEN I-O    MENU.
           OPEN OUTPUT INFORMES REPORTES.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-PROCESO-INFORME.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE  SPACES TO ME-IDENT.
           MOVE  ZEROS  TO ME-LINEA.
           PERFORM  I-START-NOMIMENU-MAYOR-KEY00  THRU
                    F-START-NOMIMENU-MAYOR-KEY00.
           IF FIN-ARCH-NOMIMENU = 1
            GO TO F-PROCESO-INFORME.
           PERFORM I-LISTA-INFORME THRU F-LISTA-INFORME
            UNTIL FIN-ARCH-NOMIMENU = 1.
       F-PROCESO-INFORME. EXIT.
      *
       I-LISTA-INFORME.
           PERFORM I-LEE-C-NOMIMENU THRU F-LEE-C-NOMIMENU.
           DISPLAY ME-CLAVE LINE 25 POSITION 01.
           IF FIN-ARCH-NOMIMENU = 1 GO TO F-LISTA-INFORME.

           WRITE REG-INFORMES FROM REC-MENU AFTER 1.
           MOVE ME-PASSWORD   TO PROGRA.
           WRITE REG-REPORTES FROM LINEA AFTER 1.
       F-LISTA-INFORME. EXIT.
      *
       I-FIN-MODE.
           MOVE SPACES TO REG-INFORMES.
           WRITE REG-INFORMES AFTER PAGE.
           CLOSE MENU INFORMES.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE. EXIT.
      *
           COPY "..\START\NOMIMENU.M00".
      *
           COPY "..\LEE\NOMIMENU.SEQ".
      *
           COPY "..\PRO\TRUQUITO.PRO".
      ******************************************************************
