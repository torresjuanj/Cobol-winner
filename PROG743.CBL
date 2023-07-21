       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG743.
      ******************************************************************
      *         CATALOGO DE COMPBTES  ABR-27-2002                                 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG743".
       77      ID-PRG         PIC X(7)       VALUE "PROG743".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNCOMPTE.LBL".
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
      *                       T  I  T  U  L  O  S                       *
      *******************************************************************
       01      AREA-INFORME.
        03     TITULO-0.
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(47)    VALUE
              "C A T A L O G O  D E  C O M P R O B A N T E S".
         05    FILLER      PIC X(05)    VALUE SPACES.
         05    FILLER      PIC X(11)    VALUE "Programa : ".
         05    I-PROGRAMA  PIC X(08)    VALUE SPACES.

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).
         05    FILLER      PIC X(68)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.

        03     TITULO-3.
         05    FILLER      PIC X(16)   VALUE SPACES.
         05    FILLER      PIC X(80)   VALUE ALL "=".

        03     TITULO-A.
         05    FILLER      PIC X(15)    VALUE SPACES.
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(16)    VALUE "TIPO DE COMPBTE".
         05    FILLER      PIC X(38)    VALUE "        N O M B R E ".
         05    FILLER      PIC X(23)    VALUE "   CONSECUTIVO ".
         05    FILLER      PIC X(03)    VALUE " | ".
      *
        03     LINEA-DE-DETALLE.
         05    FILLER       PIC X(15)    VALUE SPACES.
         05    FILLER       PIC X(02)    VALUE "| ".
         05    I-CODIM-COMP PIC ZZZ,ZZZ,ZZZ.
         05    FILLER       PIC X(07).
         05    I-NOMBM-COMP PIC X(30).
         05    FILLER       PIC X(10).
         05    I-NUMER-COMP PIC ZZZ,ZZZ.
         05    FILLER       PIC X(13).
         05    FILLER       PIC X(02)    VALUE "| ".

        03     TITULO-TOTAL.
         05    FILLER      PIC X(05).
         05    FILLER      PIC X(16)  VALUE "TOTAL COMPBTES:".
         05    T-TOTAL     PIC ZZZZ.

               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
            MOVE 0 TO SW1  FIN-OK  WK-PAGINA W-CONTA CON-LIN
           PERFORM I-START-CNCOMPTE   THRU F-START-CNCOMPTE
           PERFORM I-PRIMER-CNCOMPTE  THRU F-PRIMER-CNCOMPTE
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-BUSCA-CNCOMPTE   THRU F-BUSCA-CNCOMPTE
                               UNTIL FIN-OK = 1.
           PERFORM I-TOTAL-INFORME    THRU F-TOTAL-INFORME.
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE W-PROGRAMA             TO  I-PROGRAMA
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE     1                  TO  IND-INFORMES
           PERFORM I-LABEL-INFORMES   THRU F-LABEL-INFORMES.
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNCOMPTE EMPRESAS
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.

       I-PRIMER-CNCOMPTE.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-CNCOMPTE THRU F-LEE-CNCOMPTE.
           PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PRIMER-CNCOMPTE.
      *
       I-BUSCA-CNCOMPTE.
           PERFORM I-LEE-CNCOMPTE THRU F-LEE-CNCOMPTE
           IF FIN-OK = 1   GO F-BUSCA-CNCOMPTE.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-BUSCA-CNCOMPTE.
      *
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-CNCOMPTE.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  LLAVM1-COMP
               START CNCOMPTE  KEY >   LLAVM1-COMP INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNCOMPTE.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNCOMPTE.
              MOVE CODIM-COMP TO WI-Z12C
              DISPLAY WI-Z12C     LINE 20 POSITION 28
              DISPLAY NOMBM-COMP  LINE 21 POSITION 16
              READ CNCOMPTE NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK.
       F-LEE-CNCOMPTE.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-EMPRESAS.
      *
       I-DISPLAY.
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY "CATALOGO COMPROBANTES " LINE 03 POSITION 25.
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ No.COMPTE :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 25 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 25 POSITION 2 SIZE 78
               "CATALOGO COMPROBANTES------>" LINE 25 POSITION 05
                LABEL-INFORMES                LINE 25 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 25 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE CODIM-COMP   TO I-CODIM-COMP
            MOVE NOMBM-COMP   TO I-NOMBM-COMP
            MOVE NUMCONS-COMP TO I-NUMER-COMP
            IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
            ADD 1                      TO CON-LIN.
            ADD 1                      TO W-CONTA.
       F-MUEVE-INFORME-1.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************

       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           IF WK-PAGINA = 1
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER 1
           ELSE
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE.
             
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               MOVE 6                          TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 3.
       F-TOTAL-INFORME.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE EMPRESAS
                 INFORMES
                 CNCOMPTE.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE ZEROS                  TO  CON-PERIODO
           CANCEL "PROG743.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PLB\INFORMES.PLB".

