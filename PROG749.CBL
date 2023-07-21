       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG749.
      ******************************************************************
      *    LISTADOS DE CONCEPTOS         ABR-27-2002                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\CNCATCTA.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\INFORMES.FD".
      ******************************************************************
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA                PIC X(7)       VALUE "PROG749".
       77      ID-PRG                    PIC X(7)       VALUE "PROG749".
       77      W-PAGINA-R                PIC 99         VALUE ZEROS.
       77      W-LINEAS-R                PIC 99         VALUE ZEROS.
       77      W-DEVENGADO               PIC S9(09)V99  VALUE ZEROS.
       77      W-DEDUCIDO                PIC S9(09)V99  VALUE ZEROS.
       77      W-TOTAL                   PIC S9(09)V99  VALUE ZEROS.
       77      SW-TIP                    PIC 9          VALUE ZEROS.
      ******************************************************************
        01      WK-CUE1.
          03    MAYOR             PIC 9(04)  VALUE ZEROS.
          03    SUB               PIC 9(02)  VALUE ZEROS.
          03    AUX-1             PIC 9(02)  VALUE ZEROS.
          03    AUX-2             PIC 9(02)  VALUE ZEROS.

       01      TITULO1.
               03 FILLER PIC X     VALUE SPACE.
               03 EMP-S  PIC X(40).

       01      TITULO2.
               03 FILLER PIC X      VALUE SPACE.
               03 FILLER PIC X(104) VALUE SPACES.
               03 FILLER PIC X(09)  VALUE "Pagina : ".
               03 PAG-S  PIC ZZZ.
       01      TITULO3.
               03 FILLER PIC X      VALUE SPACE.
               03 FILLER PIC X(104) VALUE SPACE.
               03 FILLER PIC X(09)  VALUE "Fecha  : ".
               03 FECHA-S PIC X(11).

       01      TITULO4.
               03 FILLER PIC X(7)   VALUE SPACE.
               03 FILLER PIC X(19)  VALUE "CONCEPTOS DE NOMINA".
               03 FILLER PIC X(26)  VALUE SPACE.
               03 FILLER PIC X(09)  VALUE "Hora   : ".
               03 HORA-S  PIC X(5).
       01      TITULO5.
         
           05  FILLER        PIC  X(10)  VALUE "Impresion ".
           05  I-LABEL       PIC  X(50)  VALUE  SPACES.
           05  FILLER        PIC  X(45)  VALUE  SPACES.
           05  FILLER        PIC  X(09)  VALUE  "Programa:".
           05  FILLER        PIC  X(01)  VALUE  SPACES.
           05  I-PROG-LABEL  PIC  X(08).

       01      TITULO6.
        
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(34)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(25)   VALUE SPACE.
               03 FILLER PIC X(11)   VALUE "N O M I N A".
               03 FILLER PIC X(35)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(10)   VALUE SPACE.
               03 FILLER PIC X(21)   VALUE "F A C T U R A C I O N".
               03 FILLER PIC X(10)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(36)   VALUE SPACE.
               03 FILLER PIC X(17)   VALUE "P R O M E D I O S".
               03 FILLER PIC X(27)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(08)   VALUE SPACE.
      *        03 FILLER PIC X(17)   VALUE "C U E N T A S".
               03 FILLER PIC X(06)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(07)   VALUE "  D I F".
       01      TITULO7.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(07)   VALUE SPACE.
               03 FILLER PIC X(17)   VALUE "C O N C E P T O S".
               03 FILLER PIC X(10)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(71)   VALUE ALL "-".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(41)   VALUE ALL "-".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(80)   VALUE ALL "-".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(31)   VALUE ALL "-".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(07)   VALUE "  I N I".

       01      TITULO8.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(34)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(09)   VALUE "  TIPO  |".
               03 FILLER PIC X(10)   VALUE "  ORIGEN |".
               03 FILLER PIC X(10)   VALUE "  CLASE  |".
               03 FILLER PIC X(11)   VALUE "  CAUSAC  |".
               03 FILLER PIC X(10)   VALUE "  VALOR  |".
               03 FILLER PIC X(12)   VALUE " PORCENTAJE|".
               03 FILLER PIC X(10)   VALUE " IMPRIME |".
               03 FILLER PIC X(10)   VALUE " PARAF.Y |".
               03 FILLER PIC X(10)   VALUE "  SOLO   |".
               03 FILLER PIC X(10)   VALUE " OTROS   |".
               03 FILLER PIC X(12)   VALUE "  AFECTA   |".
               03 FILLER PIC X(20)   VALUE "     CESANTIAS     |".
               03 FILLER PIC X(20)   VALUE "    PRIMA LEGAL    |".
               03 FILLER PIC X(18)   VALUE "    VACACIONES   |".
               03 FILLER PIC X(11)   VALUE "  AFECTA  |".
               03 FILLER PIC X(15)   VALUE "  AFECTA   |".
               03 FILLER PIC X(14)   VALUE "   CODIGO    |".
               03 FILLER PIC X(14)   VALUE "   CODIGO     ".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(07)   VALUE "  A D J".

       01      TITULO8A.
               
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(34)   VALUE SPACE.
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(09)   VALUE " NOVEDAD|".
               03 FILLER PIC X(10)   VALUE " NOVEDAD |".
               03 FILLER PIC X(10)   VALUE "  NOVEDAD|".
               03 FILLER PIC X(11)   VALUE "  NOVEDAD |".
               03 FILLER PIC X(10)   VALUE " NOVEDAD |".
               03 FILLER PIC X(12)   VALUE " RECARGO   |".
               03 FILLER PIC X(10)   VALUE " ANEXOS  |".
               03 FILLER PIC X(10)   VALUE "  ADMON  |".
               03 FILLER PIC X(10)   VALUE "ADMON SPF|".
               03 FILLER PIC X(10)   VALUE " DESCTOS |".
               03 FILLER PIC X(12)   VALUE " PARAFISC 2|".
               03 FILLER PIC X(20)   VALUE "VJ P1  P2  P3   %  |".
               03 FILLER PIC X(20)   VALUE "VJ P1  P2  P3   %  |".
               03 FILLER PIC X(18)   VALUE " P1  P2  P3   %  |".
               03 FILLER PIC X(11)   VALUE "RETEFUENTE|".
               03 FILLER PIC X(15)   VALUE "LEY 100/93 |".
               03 FILLER PIC X(14)   VALUE " CONTABLE 1. |".
               03 FILLER PIC X(14)   VALUE " CONTABLE 2.  ".
               03 FILLER PIC X       VALUE "|".
               03 FILLER PIC X(07)   VALUE "  N I A".

       01      RAY-S.
               
               03 FILLER PIC X      VALUE SPACE.
               03 FILLER PIC X(266) VALUE ALL "=".

       01      TITULO1R.
               
               03 EMP-SR PIC X(40).

       01      TITULO2R.
               03 FILLER PIC X(58)  VALUE SPACES.
               03 FILLER PIC X(09)  VALUE "Pagina : ".
               03 PAG-AB PIC ZZZ.

       01      TITULO3R.
               
               03 FILLER PIC X(58)  VALUE SPACE.
               03 FILLER PIC X(09)  VALUE "Fecha  : ".
               03 FECHA-AB PIC X(11).

       01      TITULO4R.
               03 FILLER PIC X(3)   VALUE SPACE.
               03 FILLER PIC X(19)  VALUE "CONCEPTOS DE NOMINA".
               03 FILLER PIC X(39)  VALUE SPACE.
               03 FILLER PIC X(09)  VALUE "Hora   : ".
               03 HORA-AB  PIC X(5).

       01      TITULO5R.
               03 FILLER PIC X(3)   VALUE SPACE.
               03 FILLER PIC X(58)  VALUE SPACE.
               03 FILLER PIC X(09)  VALUE "Prog.  : ".
               03 FILLER PIC X(08)  VALUE  "PROG749 ".

       01      RAY-S2    PIC X(130) VALUE ALL "=".

       01      TITULO6R.
               03 FILLER PIC X      VALUE SPACE.
               03 FILLER PIC X(3).
               03 FILLER PIC X(30)  VALUE SPACE.
               03 FILLER PIC X(5)   VALUE SPACE.
               03 FILLER PIC X(36)
                         VALUE "       CODIGOS   CONTABILIDAD       ".
               03 FILLER PIC X(05)  VALUE SPACE.
               03 FILLER PIC X(15)  VALUE "CONTABILIZACION".

       01      TITULO7R.
               
               03 FILLER PIC X      VALUE SPACE.
               03 FILLER PIC X(3).
               03 FILLER PIC X(30)
                         VALUE "    C O N C E P T O           ".
               03 FILLER PIC X(5)   VALUE SPACE.
               03 FILLER PIC X(12) VALUE "  COSTOS    ".
               03 FILLER PIC X(12) VALUE "   GASTOS   ".
               03 FILLER PIC X(09) VALUE SPACES.
               03 FILLER PIC X(10) VALUE "INDIVIDUAL".
               03 FILLER PIC X(13) VALUE SPACES.
               03 FILLER PIC X(12) VALUE "NIT-EMPRESA".

       01      TITULO9.
               03 FILLER PIC X(3) VALUE SPACE.
               03 DES-TI PIC X(30).
               03 FILLER  PIC X(03) VALUE SPACE.
               03 FILLER  PIC X(33) VALUE
               "P1: PROM.DEVENGADO ULTIMO ANO".
               03 FILLER  PIC X(29) VALUE
               "P2: PROM.HORAS EXTRAS        ".
               03 FILLER  PIC X(29) VALUE
               "P3: PROM.RECARGO NOCTURNO    ".
               03 FILLER              PIC X(2)       VALUE "M".

       01      DETALLE.
               03 FILLER PIC X    VALUE SPACE.
               03 CON-S  PIC 999.
               03 FILLER PIC X    VALUE SPACES.
               03 DES-S  PIC X(30).
               03 FILLER PIC X    VALUE SPACES.
               03 OTROS1.
                  05 TIP-S  PIC X.
                  05 FILLER PIC X    VALUE SPACES.
                  05 TD-S   PIC X(06).
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 ORI-S  PIC X.
                  05 FILLER PIC X    VALUE SPACES.
               03 ROTROS1 REDEFINES OTROS1.
                  05 COD-CON1 PIC ZZZZZZZZZZZZ.
               03 OTROS2.
                  05 OD-S   PIC X(06).
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 CLA-S  PIC X.
                  05 FILLER PIC X    VALUE SPACES.
                  05 CL-S   PIC X(06).
               03 ROTROS2 REDEFINES OTROS2.
                  05 FILLER   PIC X(2).
                  05 COD-CON2 PIC ZZZZZZZZZZZZ.
                  05 FILLER   PIC X(2).
               03 OTROS3.
                  05 FILLER PIC X(2).
                  05 CAU-S  PIC X.
                  05 FILLER PIC X.
                  05 CA-S   PIC X(06).
                  05 FILLER PIC X(3) VALUE SPACES.
               03 ROTROS3 REDEFINES OTROS3.
                  05 COD-CON3 PIC ZZZZZZZZZZZZ.
                  05 FILLER   PIC X.
               03 VLR-S  PIC X.
               03 FILLER PIC X     VALUE SPACES.
               03 VD-S   PIC X(06).
               03 FILLER PIC X(6)  VALUE SPACES.
               03 RECA-S PIC ZZZ.
               03 FILLER PIC X(9) VALUE SPACES.
               03 LIST-S PIC X.
               03 FILLER PIC X(8) VALUE SPACES.
               03 PARA-S PIC X.
               03 FILLER PIC X(9) VALUE SPACES.
               03 PRES-S PIC X.
               03 FILLER PIC X(9) VALUE SPACES.
               03 OTDS-S PIC X.
               03 FILLER PIC X(7) VALUE SPACES.
               03 APLI-S PIC X.
               03 FILLER PIC X(1) VALUE SPACES.
               03 PORC-S PIC ZZ.ZZ.

               03 FILLER PIC X(4) VALUE SPACES.
               03 CESA-VJO PIC X.
               03 FILLER PIC X(2) VALUE SPACES.
               03 CESA-1 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 CESA-2 PIC X.
               03 FILLER PIC X(2) VALUE SPACES.
               03 CESA-3 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 P-CE-S PIC ZZZ.

               03 FILLER PIC X(3) VALUE SPACES.
               03 PRIMA-VJO PIC X.
               03 FILLER PIC X(2) VALUE SPACES.
               03 PRIM-1 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 PRIM-2 PIC X.
               03 FILLER PIC X(2) VALUE SPACES.
               03 PRIM-3 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 P-LE-S PIC ZZZ.

               03 FILLER PIC X(3) VALUE SPACES.
               03 VACA-1 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 VACA-2 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 VACA-3 PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 P-VA-S PIC ZZZ.

               03 FILLER PIC X(4) VALUE SPACES.
               03 BAIS-S PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 P-IS-S PIC ZZZ.
               03 FILLER PIC X(4) VALUE SPACES.
               03 BARE-S PIC X.
               03 FILLER PIC X(3) VALUE SPACES.
               03 P-RE-S PIC ZZZ.

               03 FILLER PIC X(5) VALUE SPACES.
               03 CONTA-1   PIC  9999B99B9999 VALUE ZEROS.
               03 FILLER PIC X(4) VALUE SPACES.
               03 CONTA-2   PIC  9999B99B9999 VALUE ZEROS.
               03 FILLER PIC X(01) VALUE SPACES.
               03 I-CONCPTOS-CONT-INDIVIDUAL PIC X(01) VALUE SPACES.
               03 FILLER PIC X(01) VALUE SPACES.
               03 I-CONCPTOS-COLUMNA-LEGCON PIC ZZ VALUE ZEROS.
               03 FILLER PIC X(01) VALUE SPACES.
               03 I-CONCPTOS-CONT-NIT-EMPRESAS PIC X(01) VALUE SPACES.
               03 FILLER PIC X(01) VALUE SPACES.
               03 I-CONCPTOS-NOVFIJ-DEPEND PIC X(01) VALUE SPACES.

       01      TOTAL.
               
               03 FILLER PIC XX     VALUE  "[ ".
               03 NUMCON PIC ZZZ.
               03 FILLER PIC XX     VALUE  " ]".
               03 FILLER PIC XX     VALUE  SPACES.
               03 FILLER PIC X(09)  VALUE  "CONCEPTOS".

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.

           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CNCATCTA.LBL".
           COPY "..\LBL\INFORMES.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\CONCPTOS.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
           COPY "..\WRK\LISTADOS.WRK".
           COPY "..\LNK\CONTROLA.LNK".

       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
      *
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE    WEMPRESA            TO    EMP-S EMP-SR.
           PERFORM I-FECHA             THRU  F-FECHA.
           PERFORM I-LABEL-ARCHIVOS    THRU  F-LABEL-ARCHIVOS.
           PERFORM I-ABRE-ARCHIVOS     THRU  F-ABRE-ARCHIVOS
           MOVE 76 TO CON-LIN
           PERFORM I-PANTALLA          THRU  F-PANTALLA
           PERFORM I-DISPLAY-LABEL     THRU  F-DISPLAY-LABEL
           PERFORM I-START-CONCPTOS    THRU  F-START-CONCPTOS.
           PERFORM I-LIST-CONCPTOS     THRU  F-LIST-CONCPTOS
                   UNTIL FIN-OK = 1.
           PERFORM I-FIN               THRU  F-FIN.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE    3                   TO   IND-INFORMES
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O CONCPTOS.
               OPEN INPUT CNCATCTA.
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
       I-PANTALLA.
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "º"                          LINE  2 POSITION  1
               DISPLAY "WINNER FINANCIAL© º"        LINE  2 POSITION 62
               DISPLAY "º FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY "LISTADO CONCEPTOS NOMINA *" LINE  3 POSITION 30
                        REVERSE
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "º"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "º"                          LINE  5 POSITION  1
               DISPLAY "º"                          LINE  5 POSITION 80
               DISPLAY "º"                          LINE  6 POSITION  1
               DISPLAY "º"                          LINE  6 POSITION 80
               DISPLAY "º"                          LINE  7 POSITION  1
               DISPLAY "º"                          LINE  7 POSITION 80
               DISPLAY "º"                          LINE  8 POSITION  1
               DISPLAY "º"                          LINE  8 POSITION 80
               DISPLAY "º"                          LINE  9 POSITION  1
               DISPLAY "º"                          LINE  9 POSITION 80
               DISPLAY "º"                          LINE 10 POSITION  1
               DISPLAY "º"                          LINE 10 POSITION 80
               DISPLAY "º"                          LINE 11 POSITION  1
               DISPLAY "º"                          LINE 11 POSITION 80
               DISPLAY " F4 - DETALLADO "           LINE 12 POSITION 35
               DISPLAY " F5 - ABREVIADO "           LINE 14 POSITION 35
               DISPLAY "º"                          LINE 12 POSITION  1
               DISPLAY "º"                          LINE 12 POSITION 80
               DISPLAY "º"                          LINE 13 POSITION  1
               DISPLAY "º"                          LINE 13 POSITION 80
               DISPLAY "º"                          LINE 14 POSITION  1
               DISPLAY "º"                          LINE 14 POSITION 80
               DISPLAY "º"                          LINE 15 POSITION  1
               DISPLAY "º"                          LINE 15 POSITION 80
               DISPLAY "º"                          LINE 16 POSITION  1
               DISPLAY "º"                          LINE 16 POSITION 80
               DISPLAY "º"                          LINE 17 POSITION  1
               DISPLAY "º"                          LINE 17 POSITION 80
               DISPLAY "º"                          LINE 18 POSITION  1
               DISPLAY "º"                          LINE 18 POSITION 80
               DISPLAY "º"                          LINE 19 POSITION  1
               DISPLAY "º"                          LINE 19 POSITION 80
               DISPLAY "º"                          LINE 20 POSITION 80
               DISPLAY "º"                          LINE 20 POSITION  1
               DISPLAY WK-RAYAS                     LINE 21 POSITION  1
               DISPLAY "º"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY "º"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.
       020-DISPLAY-DATOS-PERIODO.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1.
       CONFIRMA-INFORME.
               PERFORM ACEPTA-TRUCO.
           IF  F2 PERFORM I-FIN THRU F-FIN.
           IF  F4 OR F5
               NEXT SENTENCE
           ELSE
               GO CONFIRMA-INFORME.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA.
      *
       I-START-CONCPTOS.
           MOVE 6 TO I.
           MOVE WK-MASCARA-FECHA TO FECHA-S FECHA-AB.
           MOVE WK-MASCARA-HORA  TO HORA-S HORA-AB.
           MOVE ZEROS TO CONCPTOS-CODIGO-CONCEPTO.
           MOVE SPACES TO CONCPTOS-KEY00.
           START CONCPTOS KEY  NOT LESS  CONCPTOS-KEY00 INVALID  KEY
               MOVE "98"       TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN THRU F-FIN.
       F-START-CONCPTOS.
      *
       I-LEE-CONCPTOS.
           READ CONCPTOS NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO FIN-OK.
           IF FIN-OK = 0
             IF CONCPTOS-CODIGO-CONCEPTO = ZEROS
                GO I-LEE-CONCPTOS.
       F-LEE-CONCPTOS.
      *
       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.
      *
       I-LIST-CONCPTOS.
           PERFORM I-LEE-CONCPTOS  THRU  F-LEE-CONCPTOS.
           IF FIN-OK = 1
              IF F4
                 WRITE REG-INFORMES FROM RAY-S AFTER 1
                 MOVE  LEIDOS  TO NUMCON
                 WRITE REG-INFORMES FROM TOTAL AFTER 1
                 WRITE REG-INFORMES FROM RAY-S AFTER 1
                 GO    F-LIST-CONCPTOS
              ELSE
                 WRITE REG-INFORMES FROM RAY-S2 AFTER 1
                 MOVE  LEIDOS  TO NUMCON
                 WRITE REG-INFORMES FROM TOTAL AFTER 1
                 WRITE REG-INFORMES FROM RAY-S2 AFTER 1
                 GO    F-LIST-CONCPTOS.

           ADD     1      TO    LEIDOS.
           PERFORM I-DISPLAY       THRU  F-DISPLAY.
           MOVE CONCPTOS-CODIGO-CONCEPTO TO CON-S.
           MOVE CONCPTOS-DESCRIPCION     TO DES-S.
           IF F4
             MOVE CONCPTOS-TIPO-NOVEDAD    TO TIP-S
             MOVE CONCPTOS-ORIGEN-NOVEDAD  TO ORI-S
             MOVE CONCPTOS-CLASE-NOVEDAD   TO CLA-S
             MOVE CONCPTOS-CAUSACION       TO CAU-S
             MOVE CONCPTOS-VALOR-NOVEDAD   TO VLR-S
             MOVE CONCPTOS-INDICE-OTROS-PF TO APLI-S
             MOVE CONCPTOS-PORC-OTROS-PF   TO PORC-S
             MOVE CONCPTOS-PORC-RECARGO    TO RECA-S
             MOVE CONCPTOS-AFEC-CESANTIAS  TO CESA-VJO
             MOVE CONCPTOS-AFEC-PRIMA-L    TO PRIMA-VJO
             MOVE CONCPTOS-CESAN-P1        TO CESA-1
             MOVE CONCPTOS-CESAN-P2        TO CESA-2
             MOVE CONCPTOS-CESAN-P3        TO CESA-3
             MOVE CONCPTOS-PRIMA-P1        TO PRIM-1
             MOVE CONCPTOS-PRIMA-P2        TO PRIM-2
             MOVE CONCPTOS-PRIMA-P3        TO PRIM-3
             MOVE CONCPTOS-VACAC-P1        TO VACA-1
             MOVE CONCPTOS-VACAC-P2        TO VACA-2
             MOVE CONCPTOS-VACAC-P3        TO VACA-3
             MOVE CONCPTOS-PORC-CESANTIAS  TO P-CE-S
             MOVE CONCPTOS-PORC-PRIMA-L    TO P-LE-S
             MOVE CONCPTOS-PORC-VACACION   TO P-VA-S
             MOVE CONCPTOS-BASE-RFTE        TO BAIS-S
             MOVE CONCPTOS-PORC-RFTE        TO P-IS-S
             MOVE CONCPTOS-BASE-LEY100     TO BARE-S
             MOVE CONCPTOS-PORC-LEY100     TO P-RE-S
             MOVE CONCPTOS-PARAFISCALES    TO PARA-S
             MOVE CONCPTOS-PRESTACIONES    TO PRES-S
             MOVE CONCPTOS-LISTADOS-INF    TO LIST-S
             PERFORM I-VALIDA-CUENTAS   THRU F-VALIDA-CUENTAS
             MOVE CONCPTOS-OTROS-DESCUENTOS   TO OTDS-S
             MOVE CONCPTOS-CONT-INDIVIDUAL TO I-CONCPTOS-CONT-INDIVIDUAL
             MOVE CONCPTOS-COLUMNA-LEGCON TO I-CONCPTOS-COLUMNA-LEGCON
             MOVE CONCPTOS-CONT-NIT-EMPRESAS TO
                  I-CONCPTOS-CONT-NIT-EMPRESAS
             MOVE CONCPTOS-NOVFIJ-DEPEND TO I-CONCPTOS-NOVFIJ-DEPEND
             PERFORM I-DETALLES  THRU  F-DETALLES
           ELSE
             MOVE CONCPTOS-CODIGO-CONTABLE  TO COD-CON1
             MOVE CONCPTOS-COD-CONTABLE2    TO COD-CON2
             PERFORM I-VALIDA-CUENTAS   THRU F-VALIDA-CUENTAS
             MOVE CONCPTOS-CONT-INDIVIDUAL  TO VLR-S
             MOVE CONCPTOS-CONT-NIT-EMPRESAS TO LIST-S.
           IF SW-TIP = ZEROS
             IF CONCPTOS-TIPO-NOVEDAD = "D"
                 MOVE " D E S C U E N T O S " TO DES-TI
                 MOVE 76 TO CON-LIN
                 MOVE  1 TO SW-TIP
              ELSE
                 MOVE " D E V E N G A D O S " TO DES-TI.
           IF CON-LIN > 57
              IF  F4
                PERFORM I-TITULOS   THRU F-TITULOS
              ELSE
                PERFORM I-TITULOS-R THRU F-TITULOS-R.
           PERFORM I-VALIDA-N   THRU F-VALIDA-N.
           WRITE REG-INFORMES FROM DETALLE AFTER 1.
           MOVE  SPACE TO   DETALLE.
           ADD 1 TO CON-LIN.
       F-LIST-CONCPTOS.
      *
       I-VALIDA-CUENTAS.
              MOVE ZEROS    TO SW-EOF.
              MOVE CONCPTOS-CODIGO-CONTABLE TO CONTA-1 CTAMAE
              PERFORM I-LEE-CNCATCTA  THRU F-LEE-CNCATCTA.
                 IF SW-EOF  = 1
                    MOVE  ZEROS    TO CONTA-1.
              MOVE CONCPTOS-COD-CONTABLE2   TO CONTA-2 CTAMAE
              PERFORM I-LEE-CNCATCTA  THRU F-LEE-CNCATCTA.
                 IF SW-EOF  = 1
                    MOVE  ZEROS    TO CONTA-2.
       F-VALIDA-CUENTAS.
      *
       I-DETALLES.
           IF   CONCPTOS-TIPO-NOVEDAD = "I" MOVE "INFORMATIVO" TO TD-S.
           IF   CONCPTOS-TIPO-NOVEDAD = "D" MOVE "DESCUENTO  " TO TD-S.
           IF   CONCPTOS-TIPO-NOVEDAD = "B" MOVE "BASICO     " TO TD-S.
           IF   CONCPTOS-TIPO-NOVEDAD = "P" MOVE "PAGO       " TO TD-S.

           IF   CONCPTOS-ORIGEN-NOVEDAD = "A" MOVE "AUTOMATICO" TO OD-S.
           IF   CONCPTOS-ORIGEN-NOVEDAD = "E" MOVE "EXCEPCION " TO OD-S.
           IF   CONCPTOS-ORIGEN-NOVEDAD = "T" MOVE "TABLA     " TO OD-S.
           IF   CONCPTOS-ORIGEN-NOVEDAD = "F" MOVE "FONDO EMPL" TO OD-S.

           IF   CONCPTOS-VALOR-NOVEDAD = "D" MOVE "DIAS      " TO VD-S.
           IF   CONCPTOS-VALOR-NOVEDAD = "H" MOVE "HORAS     " TO VD-S.
           IF   CONCPTOS-VALOR-NOVEDAD = "P" MOVE "PESOS     " TO VD-S.
           IF   CONCPTOS-VALOR-NOVEDAD = "%" MOVE "PORCENTAJE" TO VD-S.

           IF   CONCPTOS-CLASE-NOVEDAD = "N" MOVE "NORMAL    " TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "C" MOVE "CESANTIAS " TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "B" MOVE "BONIFICAC." TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "V" MOVE "VACACIONES" TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "S" MOVE "SUB.TRANSP" TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "I" MOVE "I.S.S.    " TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "L" MOVE "PRI.LEGAL " TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "X" MOVE "PRI.EXTRAL" TO CL-S.
           IF   CONCPTOS-CLASE-NOVEDAD = "R" MOVE "RETENCION " TO CL-S.

           IF   CONCPTOS-CAUSACION     = "T" MOVE "TOD PERI  " TO CA-S.
           IF   CONCPTOS-CAUSACION     = "I" MOVE "INTERCALAD" TO CA-S.
           IF   CONCPTOS-CAUSACION     = "S" MOVE "SEMESTRAL " TO CA-S.
           IF   CONCPTOS-CAUSACION     = "A" MOVE "ANUALMENTE" TO CA-S.
       F-DETALLES.
      *
       I-VALIDA-N.
            IF LIST-S      =  "N"  MOVE  " "  TO LIST-S.
            IF PARA-S      =  "N"  MOVE  " "  TO PARA-S.
            IF PRES-S      =  "N"  MOVE  " "  TO PRES-S.
            IF OTDS-S      =  "N"  MOVE  " "  TO OTDS-S.
            IF APLI-S      =  "N"  MOVE  " "  TO APLI-S.
            IF CESA-VJO    =  "N"  MOVE  " "  TO CESA-VJO.
            IF CESA-1      =  "N"  MOVE  " "  TO CESA-1.
            IF CESA-2      =  "N"  MOVE  " "  TO CESA-2.
            IF CESA-3      =  "N"  MOVE  " "  TO CESA-3.

            IF PRIMA-VJO   =  "N"  MOVE  " "  TO PRIMA-VJO.
            IF PRIM-1      =  "N"  MOVE  " "  TO PRIM-1.
            IF PRIM-2      =  "N"  MOVE  " "  TO PRIM-2.
            IF PRIM-3      =  "N"  MOVE  " "  TO PRIM-3.

            IF VACA-1      =  "N"  MOVE  " "  TO VACA-1.
            IF VACA-2      =  "N"  MOVE  " "  TO VACA-2.
            IF VACA-3      =  "N"  MOVE  " "  TO VACA-3.

            IF BAIS-S      = "N"  MOVE  " "  TO BAIS-S.
            IF BARE-S      = "N"  MOVE  " "  TO BARE-S.
       F-VALIDA-N.
      *
       I-TITULOS.
           MOVE W-PROGRAMA             TO  I-PROG-LABEL.
           MOVE LABEL-INFORMES         TO  I-LABEL.
           ADD 1 TO CON-PAG.
           MOVE CON-PAG TO PAG-S.
           IF CON-PAG > 1
                MOVE  SPACES  TO REG-INFORMES
                WRITE REG-INFORMES              AFTER 1
                WRITE REG-INFORMES FROM TITULO9 AFTER 1
                WRITE REG-INFORMES FROM TITULO1 AFTER PAGE
           ELSE
                 WRITE REG-INFORMES FROM TITULO1 AFTER 1.
           WRITE REG-INFORMES FROM TITULO2 AFTER 1
           WRITE REG-INFORMES FROM TITULO3 AFTER 1
           WRITE REG-INFORMES FROM TITULO4 AFTER 1
           WRITE REG-INFORMES FROM TITULO5 AFTER 1
           WRITE REG-INFORMES FROM RAY-S   AFTER 1
           WRITE REG-INFORMES FROM TITULO6 AFTER 1
           WRITE REG-INFORMES FROM TITULO7 AFTER 1
           WRITE REG-INFORMES FROM TITULO8 AFTER 1
           WRITE REG-INFORMES FROM TITULO8A AFTER 1
           WRITE REG-INFORMES FROM RAY-S   AFTER 1

           MOVE  SPACES  TO REG-INFORMES.
           MOVE  14      TO CON-LIN.
       F-TITULOS.
      *
       I-TITULOS-R.
           ADD 1 TO CON-PAG.
           MOVE CON-PAG TO PAG-AB.
           IF CON-PAG > 1
                 MOVE  SPACES  TO REG-INFORMES
                 WRITE REG-INFORMES              AFTER 1
                 WRITE REG-INFORMES FROM TITULO9 AFTER 1
                 WRITE REG-INFORMES FROM TITULO1R AFTER PAGE
           ELSE
                 WRITE REG-INFORMES FROM TITULO1R AFTER 1.
           WRITE REG-INFORMES FROM TITULO2R AFTER 1
           WRITE REG-INFORMES FROM TITULO3R AFTER 1
           WRITE REG-INFORMES FROM TITULO4R AFTER 1
           WRITE REG-INFORMES FROM TITULO5R AFTER 1
           WRITE REG-INFORMES FROM RAY-S2   AFTER 1
           WRITE REG-INFORMES FROM TITULO6R AFTER 1
           WRITE REG-INFORMES FROM TITULO7R AFTER 1
           WRITE REG-INFORMES FROM RAY-S2   AFTER 1
           MOVE  SPACES  TO REG-INFORMES.
           MOVE  14      TO CON-LIN.
       F-TITULOS-R.
      *
       I-DISPLAY.
           ADD 1 TO I.
           IF I > 18
              MOVE 6 TO I
              PERFORM I-BLANQUEA THRU F-BLANQUEA
                      UNTIL I > 18
              MOVE 6 TO I.
           DISPLAY LEIDOS                LINE I POSITION 15.
           DISPLAY CONCPTOS-CODIGO-CONCEPTO LINE I POSITION 20.
           DISPLAY CONCPTOS-DESCRIPCION     LINE I POSITION 25.
       F-DISPLAY.
      *
       I-BLANQUEA.
           ADD 1 TO I.
           DISPLAY SPACE                 LINE I POSITION 15 SIZE 63.
       F-BLANQUEA.
      *
       I-FECHA.
               ACCEPT WK-FECHA-HOY       FROM  DATE.
               ACCEPT WK-HORA-HOY        FROM  TIME.
               MOVE WK-DIA                 TO  WK-MASCARA-DIA
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
               MOVE WK-ANO                 TO  WK-MASCARA-ANO
               MOVE "-"                    TO  WK-RAYA-1
               MOVE "-"                    TO  WK-RAYA-2
               MOVE WK-HORAS               TO  WK-MASCARA-HORAS
               MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
               MOVE ":"                    TO  WK-PUNTOS
               COMPUTE WK-ANO-BISIESTO      =  WK-ANO-HASTA
                                            /  4
               COMPUTE WK-ANO-BISIESTO      =  WK-ANO-BISIESTO
                                            *  4
           IF  WK-ANO-BISIESTO              =  WK-ANO-HASTA
               MOVE 29                     TO  WK-DIAS-MES (2)
           ELSE
               MOVE 28                     TO  WK-DIAS-MES  (2).
       F-FECHA.
      *
       I-FIN.
           MOVE SPACES TO REG-INFORMES.
           WRITE REG-INFORMES AFTER PAGE.
               CLOSE CONCPTOS INFORMES CNCATCTA.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN.
      *
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\DISLABEL.PRO".
           COPY "..\PLB\INFORMES.PLB".

