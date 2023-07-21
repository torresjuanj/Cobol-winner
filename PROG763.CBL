       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG763.
      ******************************************************************
      * CONSULTA LIBRO AUXILIAR    ABR-27-2002                         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CNDOCTOS.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\CNDTACUM.FD".
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\CNCATCTA.FD".
           COPY "..\FD\CNNITCED.FD".
           COPY "..\FD\CNDOCTOS.FD".
           COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG763".
       77      ID-PRG         PIC X(7)       VALUE "PROG763".
       77      NIVEL-S        PIC 9          VALUE ZEROS.
       77      CUENTA-1       PIC 9(10)      VALUE 1000000000.
       77      CUENTA-2       PIC 9(10)      VALUE ZEROS.
       77      W-NIT-DESDE    PIC 9(12)      VALUE ZEROS.
       77      W-FRA-DESDE    PIC 9(06)      VALUE ZEROS.
       77      W-NIT-REFER    PIC 9(12)      VALUE ZEROS.
       01 WW-CNDTACUM-CLAVE.
        03  WW-CTA-HASTA   PIC 9(10)      VALUE ZEROS.
        03  W-NIT-HASTA    PIC 9(12)      VALUE ZEROS.
        03  W-FRA-HASTA    PIC 9(06)      VALUE ZEROS.

       01  W-TABLA-LIBRO-AUXILIAR.
        03  TABLA-LIBRO-AUXILIAR OCCURS 18 TIMES.
         05  CAMPO-1-LA            PIC X(04)   VALUE SPACES.
         05  CAMPO-2-LA            PIC X(02)   VALUE SPACES.
         05  CAMPO-3-LA            PIC X(04)   VALUE SPACES.
         05  CAMPO-4-LA            PIC X(64)   VALUE SPACES.
         05  CAMPO-5-LA            PIC X(18)   VALUE SPACES.
         05  CAMPO-6-LA            PIC X(18)   VALUE SPACES.
         05  CAMPO-7-LA            PIC X(18)   VALUE SPACES.
         05  CAMPO-8-LA            PIC X(18)   VALUE SPACES.
         05  CAMPO-9-LA            PIC 9(01)   VALUE ZEROS.
           COPY "..\LBL\CNDTACUM.LBL".
           COPY "..\LBL\CNCATCTA.LBL".
           COPY "..\LBL\CNDOCTOS.LBL".
           COPY "..\LBL\CNNITCED.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\INFORMES.LBL".
           COPY "..\LBL\USER-ACT.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
           COPY "..\WRK\TABLAMES.WRK".
       01      AREA-INFORME.
        03 LDET1.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-S     PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-S     PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-S     PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(03)    VALUE SPACES.
           05 LDET1-CAMPO-4.
             07 NOMCTA-S     PIC X(64)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALANT-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
      *
       03 LDETNIT.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-NIT   PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-NIT   PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-NIT   PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(05)     VALUE SPACES.
           05 LDETNIT-CAMPO-4.
             07 FILLER       PIC X(08)        VALUE SPACES.
             07 CODNIT-S     PIC ZZZ,ZZZ,ZZZ.
             07 FILLER       PIC X(04)        VALUE "    ".
             07 NOMNIT-S     PIC X(41)        VALUE SPACES.
           05 FILLER       PIC X(01)      VALUE "!".
           05 SALANT-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
      *
        03 LDETDET.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(12)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(23)     VALUE SPACES.
           05 LDETDET-CAMPO-4.
             07 FILLER       PIC X(09)     VALUE "No.Fra : ".
             07 CNDT-FRA-S   PIC Z99999.
             07 FILLER       PIC X(07)     VALUE SPACES.
             07 FILLER       PIC X(11)     VALUE "Fecha Fra: ".
             07 AFEC-FRA-S   PIC 9(04).
             07 FILLER       PIC X(01)     VALUE "/".
             07 MFEC-FRA-S   PIC 99.
             07 FILLER       PIC X(01)     VALUE "/".
             07 DFEC-FRA-S   PIC 99.
             07 FILLER       PIC X(21)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALANT-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".

        03 TITULO-5.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-M     PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-M     PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-M     PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(05)     VALUE SPACES.
           05 TITULO-5-CAMPO-4.
             07 RAD-TIP      PIC 9(03).
             07 FILLER       PIC X(01)     VALUE "-".
             07 RAD-DOC      PIC 999999.
             07 FILLER       PIC X(01)     VALUE "-".
             07 RAD-AUT      PIC 999999.
             07 FILLER       PIC X(01)     VALUE " ".
             07 NUMDOC-M     PIC ZZZZZZ.
             07 FILLER       PIC X(01)     VALUE " ".
             07 NUMFAC-M     PIC ZZZZZ9.
             07 FILLER       PIC X(02)     VALUE " ".
             07 FECHA-CONTAB-S PIC 9(08).
             07 FILLER       PIC X(02)     VALUE " ".
             07 CONCEPTO-M   PIC X(20).
             07 FILLER       PIC X(01).
             07 EMPRESA-M    PIC Z(07).
           05 FILLER       PIC X(01)     VALUE " ".
           05 FILLER       PIC X(10)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 VALDEB-M     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 VALCRE-M     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
      *
           COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\CNDTACUM.DCL".
           COPY "..\DCL\CNCATCTA.DCL".
           COPY "..\DCL\CNNITCED.DCL".
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\CNDOCTOS.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PANTALLA-1A    THRU F-PANTALLA-1A.
         RECIMES.
           PERFORM I-ACEPTA-MES     THRU F-ACEPTA-MES
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-PROCESO-CONSUL THRU F-PROCESO-CONSUL
                   UNTIL ESC OR CUP.
           IF CUP 
              CLOSE CNCATCTA CNNITCED CNDOCTOS CNDTACUM
              GO RECIMES.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-PANTALLA-1A.
           MOVE "***L I B R O   A U X I L I A R *** "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY
           "ษออออออออออออออออออออออออออออออออออออออออออออป"
           LINE 09 POSITION 20
           "บ  TECLEE EL MES A CONSULTAR:                บ"
           LINE 10 POSITION 20
           "ศออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 11 POSITION 20
           DISPLAY WK-ESPACIOS                  LINE 25 POSITION 1
           " [F2] TERMINAR " LINE 25 POSITION 01
           W-LONG LINE 10 POSITION 50 REVERSE.
       F-PANTALLA-1A.
      *
       I-ACEPTA-MES.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT W-LONG LINE 10 POSITION 50
                         REVERSE CONVERT NO BEEP UPDATE
                         ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-LONG  =  0 OR W-LONG > 14
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            " PERIODO ERRADO...DIGITE NUEVAMENTE"
            LINE 25 POSITION 1 REVERSE BLINK
            GO I-ACEPTA-MES.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 10 POSITION 54 REVERSE.
           IF W-LONG = 1 OR W-LONG = 14
             MOVE   14   TO MESW
           ELSE
             COMPUTE MESW = W-LONG - 1.
           DISPLAY WK-ESPACIOS                  LINE 25 POSITION 1.
       F-ACEPTA-MES. EXIT.
      *
       I-CONFIRMA-COMP.
            DISPLAY
            " PRESIONE <C> PARA CONFIRMAR " LINE 25 POSITION 1 REVERSE
            " [F2] TERMINAR " LINE 25 POSITION 32 REVERSE
            " [] REGRESAR "  LINE 25 POSITION 50 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  GO TO F-CONFIRMA-COMP.
           IF  W-TRUCO     NOT =  "C"    GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP. EXIT.
      *
       I-LABEL-ARCHIVOS.
           PERFORM I-LABEL-CNDOCTOS THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-CNDTACUM THRU F-LABEL-CNDTACUM.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT CNDTACUM
           OPEN INPUT CNNITCED CNCATCTA CNDOCTOS.
           ACCEPT WK-FECHA-HOY      FROM  DATE
           ACCEPT WK-HORA-HOY       FROM  TIME
           MOVE CON-ANO             TO  WK-MASCARA-ANO
           MOVE 30                  TO  WK-MASCARA-DIA
           IF W-LONG = 13
            MOVE WK-ABREVIA(MESW)   TO  WK-MASCARA-MES
           ELSE
            MOVE WK-ABREVIA(W-LONG) TO  WK-MASCARA-MES.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-PROCESO-CONSUL.
           PERFORM I-PANTALLA-1B THRU F-PANTALLA-1B.
         RECIDATOS.
           PERFORM I-DATOS-CONSUL   THRU F-DATOS-CONSUL.
           IF ESC GO TO F-PROCESO-CONSUL.
           IF CUP GO F-PROCESO-CONSUL.

           PERFORM I-PANTALLA-GEN   THRU F-PANTALLA-GEN.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
             VARYING I FROM 06 BY 1 UNTIL I > 23.
           PERFORM I-BUSCA-CTA THRU F-BUSCA-CTA.
           MOVE ZEROS TO WK-EXCEPTION.
       F-PROCESO-CONSUL. EXIT.
      *
       I-PANTALLA-1B.
           MOVE 01 TO SW-OPCION.
           PERFORM I-PANTALLA-1A THRU F-PANTALLA-1A.
           DISPLAY
           WK-NOMBRE-MES(W-LONG) LINE 10 POSITION 54 REVERSE
           "     ษออออออออออหอออออออออออออออออออหอออออออออออออออหอออออออ
      -    "ออออออออออออหออออออป" LINE 13 POSITION 01
           "     บ  CUENTA  บ NOMBRE DE CUENTA  บ    N.I.T.     บ NOMBRE
      -    " DE NIT     บFACTU.บ" LINE 14 POSITION 01
           "DESDEบฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐ
      -    "ฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐบ" LINE 15 POSITION 01
           "HASTAบฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐฐ
      -    "ฐฐฐฐฐฐฐฐฐฐฐฐบฐฐฐฐฐฐบ" LINE 16 POSITION 01
           "     ศออออออออออสอออออออออออออออออออสอออออออออออออออสอออออออ
      -    "ออออออออออออสออออออผ" LINE 17 POSITION 01.
       F-PANTALLA-1B. EXIT.
      *
       I-PANTALLA-GEN.
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออป" LINE 01 POSITION 01
           "บ
      -    "                   บ" LINE 02 POSITION 01
           "บ
      -    "                   บ" LINE 03 POSITION 01
           "บ
      -    "                   บ" LINE 04 POSITION 01
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออน" LINE 05 POSITION 01
           "บ
      -    "                   บ" LINE 06 POSITION 01
           "บ
      -    "                   บ" LINE 07 POSITION 01
           "บ
      -    "                   บ" LINE 08 POSITION 01
           "บ
      -    "                   บ" LINE 09 POSITION 01
           "บ
      -    "                   บ" LINE 10 POSITION 01
           "บ
      -    "                   บ" LINE 11 POSITION 01
           "บ
      -    "                   บ" LINE 12 POSITION 01
           "บ
      -    "                   บ" LINE 13 POSITION 01
           "บ
      -    "                   บ" LINE 14 POSITION 01
           "บ
      -    "                   บ" LINE 15 POSITION 01
           "บ
      -    "                   บ" LINE 16 POSITION 01
           "บ
      -    "                   บ" LINE 17 POSITION 01
           "บ
      -    "                   บ" LINE 18 POSITION 01
           "บ
      -    "                   บ" LINE 19 POSITION 01
           "บ
      -    "                   บ" LINE 20 POSITION 01
           "บ
      -    "                   บ" LINE 21 POSITION 01
           "บ
      -    "                   บ" LINE 22 POSITION 01
           "บ
      -    "                   บ" LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออผ" LINE 24 POSITION 01.
       F-PANTALLA-GEN. EXIT.
      *
       I-LIMPIA-PANTALLA.
           DISPLAY SPACES SIZE 78 LINE I POSITION 02.
       F-LIMPIA-PANTALLA. EXIT.
      *
       I-DATOS-CONSUL.
           PERFORM I-ACEPTA-CTA-DESDE THRU F-ACEPTA-CTA-DESDE.
           IF ESC GO TO F-DATOS-CONSUL.
           IF CUP GO TO F-DATOS-CONSUL.
         DAT-02.
           PERFORM I-NIT-DESDE        THRU F-NIT-DESDE.
           IF CUP GO TO I-DATOS-CONSUL.
           IF ESC GO TO F-DATOS-CONSUL.
         DAT-03.
           PERFORM I-FRA-DESDE        THRU F-FRA-DESDE.
           IF CUP GO TO DAT-02.
           IF ESC GO TO F-DATOS-CONSUL.
         DAT-04.
           PERFORM I-ACEPTA-CTA-HASTA THRU F-ACEPTA-CTA-HASTA.
           IF CUP GO TO I-DATOS-CONSUL.
           IF ESC GO TO F-DATOS-CONSUL.
         DAT-05.
           PERFORM I-NIT-HASTA        THRU F-NIT-HASTA.
           IF CUP GO TO DAT-04.
           IF ESC GO TO F-DATOS-CONSUL.
           PERFORM I-FRA-HASTA        THRU F-FRA-HASTA.
           IF CUP GO TO DAT-05.
       F-DATOS-CONSUL. EXIT.
      *
       I-ACEPTA-CTA-DESDE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CUENTA-1 LINE 15 POSITION 07 REVERSE
           ACCEPT CUENTA-1       LINE 15 POSITION 07
                                 UPDATE PROMPT NO BEEP REVERSE CONVERT
                                 ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  ESC  OR CUP GO TO F-ACEPTA-CTA-DESDE.
           MOVE CUENTA-1 TO CTAMAE W-CTADESDE.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            " CUENTA ERRADA .......DIGITE NUEVAMENTE "
            LINE 25 POSITION 1 REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            GO I-ACEPTA-CTA-DESDE.
           DISPLAY NOMCTA LINE 15 POSITION 18 SIZE 19.
           DISPLAY WK-ESPACIOS LINE 25 POSITION 1.
       F-ACEPTA-CTA-DESDE. EXIT.
      *
       I-NIT-DESDE.
           MOVE ZEROS TO WK-EXCEPTION.
           IF CODPGT = "N"
            MOVE ZEROS TO W-NIT-DESDE
            GO TO F-NIT-DESDE.
           DISPLAY "ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ" LINE 15 POSITION 38
           DISPLAY W-NIT-DESDE LINE 15 POSITION 38 REVERSE
           ACCEPT W-NIT-DESDE LINE 15 POSITION 38
                            UPDATE PROMPT NO BEEP REVERSE CONVERT
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           MOVE W-NIT-DESDE TO WI-Z12C.
           DISPLAY WI-Z12C LINE 15 POSITION 38.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO TO F-NIT-DESDE.
           IF W-NIT-DESDE = ZEROS GO TO F-NIT-DESDE.
           MOVE W-NIT-DESDE TO CODIM.
           PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            " NIT    ERRADO .......DIGITE NUEVAMENTE "
            LINE 25 POSITION 1 REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            GO I-NIT-DESDE.
           DISPLAY NOMBM LINE 15 POSITION 54 SIZE 19.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 1.
       F-NIT-DESDE. EXIT.
      *
       I-FRA-DESDE.
           MOVE ZEROS TO WK-EXCEPTION.
           IF INDDETAL = "N"
            MOVE ZEROS TO W-FRA-DESDE
            GO TO F-FRA-DESDE.
           DISPLAY W-FRA-DESDE LINE 15 POSITION 74 REVERSE
           ACCEPT W-FRA-DESDE LINE 15 POSITION 74
                            UPDATE PROMPT NO BEEP REVERSE CONVERT
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           MOVE W-FRA-DESDE TO WI-Z6.
           DISPLAY WI-Z6 LINE 15 POSITION 74.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO TO F-FRA-DESDE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 1.
       F-FRA-DESDE. EXIT.
      *
       I-ACEPTA-CTA-HASTA.
           MOVE  ZEROS        TO  WK-EXCEPTION.
           MOVE   9000000000  TO  CUENTA-2
           DISPLAY CUENTA-2 LINE 16 POSITION 07 REVERSE.
           ACCEPT  CUENTA-2 LINE 16 POSITION 07
                            REVERSE CONVERT NO BEEP UPDATE
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO TO  F-ACEPTA-CTA-HASTA.
           MOVE CUENTA-2 TO CTAMAE W-CTAHASTA WW-CTA-HASTA.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            " CUENTA ERRADA .......DIGITE NUEVAMENTE "
            LINE 25 POSITION 1 REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            GO I-ACEPTA-CTA-HASTA.
           IF CUENTA-2 < CUENTA-1
            DISPLAY
            "ESTA CUENTA NO PUEDE SE MENOR A LA CUENTA DESDE"
            LINE 25 POSITION 1 REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            GO I-ACEPTA-CTA-HASTA.
           DISPLAY NOMCTA LINE 16 POSITION 18 SIZE 19.
           DISPLAY WK-ESPACIOS LINE 25 POSITION 1.
       F-ACEPTA-CTA-HASTA.
      *
       I-NIT-HASTA.
           MOVE ZEROS TO WK-EXCEPTION.
           IF CODPGT = "N"
            MOVE ZEROS TO W-NIT-HASTA
            GO TO F-NIT-HASTA.
           DISPLAY "ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ" LINE 16 POSITION 38
           DISPLAY W-NIT-HASTA LINE 16 POSITION 38 REVERSE
           ACCEPT W-NIT-HASTA LINE 16 POSITION 38
                            UPDATE PROMPT NO BEEP REVERSE CONVERT
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           MOVE W-NIT-HASTA TO WI-Z12C.
           DISPLAY WI-Z12C LINE 16 POSITION 38.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO TO F-NIT-HASTA.
           IF W-NIT-HASTA = ZEROS GO TO F-NIT-HASTA.
           MOVE W-NIT-HASTA TO CODIM.
           PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            " NIT    ERRADO .......DIGITE NUEVAMENTE "
            LINE 25 POSITION 1 REVERSE BLINK
            GO I-NIT-HASTA.
           DISPLAY NOMBM LINE 16 POSITION 54 SIZE 19.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 1.
       F-NIT-HASTA. EXIT.

       I-FRA-HASTA.
           MOVE ZEROS TO WK-EXCEPTION.
           IF INDDETAL = "N"
            MOVE ZEROS TO W-FRA-HASTA
            GO TO F-FRA-HASTA.
           DISPLAY W-FRA-HASTA LINE 16 POSITION 74 REVERSE
           ACCEPT W-FRA-HASTA LINE 16 POSITION 74
                            UPDATE PROMPT NO BEEP REVERSE CONVERT
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           MOVE W-FRA-HASTA TO WI-Z6.
           DISPLAY WI-Z6 LINE 16 POSITION 74.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO TO F-FRA-HASTA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 1.
       F-FRA-HASTA. EXIT.
      *
       I-BUSCA-CTA.
           MOVE 8 TO NIVEL-S
           PERFORM I-START-CNDTACUM-E  THRU F-START-CNDTACUM-E
           IF FIN-OK = 1 GO TO F-BUSCA-CTA.
           PERFORM I-CEROS-VARIABLES   THRU F-CEROS-VARIABLES
           PERFORM I-CTA-DESDE-HASTA   THRU F-CTA-DESDE-HASTA
              UNTIL CNDTACUM-CLAVE > WW-CNDTACUM-CLAVE OR ESC
           IF ESC GO TO F-BUSCA-CTA END-IF
           MOVE ZEROS TO WK-EXCEPTION
           PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
            UNTIL CDN OR ESC
           IF ESC GO TO F-BUSCA-CTA END-IF
           MOVE 01 TO SW-LECTURA
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "HASTA AQUI LA CONSULTA SOLICITADA DE <ESC> PARA SALIR."
           LINE 25 POSITION 01
           PERFORM ACEPTA-TRUCO UNTIL ESC.
           MOVE ZEROS TO WK-EXCEPTION.
       F-BUSCA-CTA.
      *
       I-START-CNDTACUM-E.
           MOVE 0         TO   FIN-OK
           MOVE W-CTAD    TO   CNDTACUM-MAYOR
           MOVE W-SUBD    TO   CNDTACUM-SUB
           MOVE W-AUXD    TO   CNDTACUM-AUX
           MOVE W-NIT-DESDE TO CNDTACUM-NIT
           MOVE W-FRA-DESDE TO CNDTACUM-FRA
           START CNDTACUM KEY NOT LESS CNDTACUM-CLAVE INVALID KEY
            MOVE "98"      TO  STATUS-NOMINA
            PERFORM RUTINA-ERROR-NOMINA
            PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM-E.
      *
       I-CEROS-VARIABLES.
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                     W-TOTAL-SALANT  W-TOTAL-SALNVO
                     S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                     T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                     G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                     FIN-OK  WK-PAGINA.
           MOVE 06 TO CON-LIN.
           MOVE 01 TO SW-LECTURA.
           MOVE 0 TO W-TOTAL-DEBME45 W-TOTAL-CREDME45
                     S-TOTAL-DEBME45 S-TOTAL-CREDME45
                     T-TOTAL-DEBME45 T-TOTAL-CREDME45
                     G-TOTAL-DEBME45 G-TOTAL-CREDME45
           MOVE 0 TO W-TOTAL-DEBMY45 W-TOTAL-CREDMY45
                     S-TOTAL-DEBMY45 S-TOTAL-CREDMY45
                     T-TOTAL-DEBMY45 T-TOTAL-CREDMY45
                     G-TOTAL-DEBMY45 G-TOTAL-CREDMY45.
       F-CEROS-VARIABLES. EXIT.
      *
       I-CTA-DESDE-HASTA.
           DISPLAY SPACES LINE 25 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
           LINE 25 POSITION 01.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-CTA-DESDE-HASTA.
           IF CTA-TRAB >= W-CTADESDE AND
              CTA-TRAB <= W-CTAHASTA
            PERFORM I-MUEVE-VAR-CTA   THRU  F-MUEVE-VAR-CTA
            PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
            PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
            IF ESC GO TO F-CTA-DESDE-HASTA END-IF
            IF ESTACTA = "N"
             PERFORM I-MOVIMIENTO-CTA  THRU F-MOVIMIENTO-CTA.
       F-CTA-DESDE-HASTA.
      *
       I-LEE-CNDTACUM.
              MOVE 0 TO FIN-OK
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
               MOVE 1 TO FIN-OK.
              MOVE CNDTACUM-MAYOR   TO CT-MAY
              MOVE CNDTACUM-SUB     TO CT-SUB
              MOVE CNDTACUM-AUX     TO CT-AUX
              IF CNDTACUM-SALDO(MESW)    = 0 AND
                 CNDTACUM-MOVDEB(W-LONG) = 0 AND
                 CNDTACUM-MOVCRE(W-LONG) = 0 AND
                 CNDTACUM-SALDO(W-LONG)  = 0 AND
                 FIN-OK = 0 GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-MUEVE-VAR-CTA.
              MOVE CNDTACUM-MAYOR       TO MAYMAE
              MOVE CNDTACUM-SUB         TO SUBMAE
              MOVE CNDTACUM-AUX         TO AUXMAE.
       F-MUEVE-VAR-CTA.
      *
       I-LEE-CNCATCTA.
           MOVE 0 TO SW-EOF.
           READ CNCATCTA WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA. EXIT.
      *
       I-MUEVE-INFORME-1.
            DISPLAY SPACES LINE 25 POSITION 1 SIZE 80.
            MOVE MAYMAE              TO MAYCTA-S
                                        MAYCTA-NIT
                                        MAYCTA-M
            MOVE SUBMAE              TO SUBCTA-S
                                        SUBCTA-NIT
                                        SUBCTA-M
            MOVE AUXMAE              TO AUXCTA-S
                                        AUXCTA-NIT
                                        AUXCTA-M
            MOVE NOMCTA              TO NOMCTA-S.
            MOVE CNDTACUM-FRA        TO CNDT-FRA-S.
            MOVE CNDTACUM-ANOFRA     TO AFEC-FRA-S
            MOVE CNDTACUM-MESFRA     TO MFEC-FRA-S.
            MOVE CNDTACUM-DIAFRA     TO DFEC-FRA-S.
            MOVE CNDTACUM-SALDO(MESW)    TO SALANT-S
                                            SALANT-NIT
                                            SALANT-DET.
            MOVE CNDTACUM-MOVDEB(W-LONG) TO MOVDEB-S
                                            MOVDEB-NIT
                                            MOVDEB-DET.
            MOVE CNDTACUM-MOVCRE(W-LONG) TO MOVCRE-S
                                            MOVCRE-NIT
                                            MOVCRE-DET.
            MOVE CNDTACUM-SALDO(W-LONG)  TO SALNVO-S
                                            SALNVO-NIT
                                            SALNVO-DET.
           PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
       EVALUE-IMPRESION.
            MOVE 0 TO RUT-OK.
            PERFORM I-EVALUE-IMPRE1 THRU F-EVALUE-IMPRE1.
            IF RUT-OK = 1 GO F-EVALUE-IMPRESION.
            PERFORM I-EVALUE-IMPRE2 THRU F-EVALUE-IMPRE2.
            IF RUT-OK = 1 GO F-EVALUE-IMPRESION.
            PERFORM I-EVALUE-IMPRE3 THRU F-EVALUE-IMPRE3.
       F-EVALUE-IMPRESION.
      *
       I-EVALUE-IMPRE1.
           IF CNDTACUM-NIT = 0 AND CNDTACUM-FRA = 0
             MOVE 1 TO RUT-OK
             PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRE1.
      *
       IMPRE1.
           PERFORM IMPRENIV THRU F-IMPRENIV.
       F-IMPRE1. EXIT.
      *
       IMPRENIV.
           IF SW-LECTURA > 18
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
             UNTIL CDN OR ESC
            PERFORM I-LIMPIA-TABLA THRU F-LIMPIA-TABLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18
            MOVE 01 TO SW-LECTURA
            IF ESC GO TO F-IMPRENIV.
           MOVE MAYCTA-S TO CAMPO-1-LA (SW-LECTURA).
           MOVE SUBCTA-S TO CAMPO-2-LA (SW-LECTURA).
           MOVE AUXCTA-S TO CAMPO-3-LA (SW-LECTURA).
           MOVE LDET1-CAMPO-4 TO CAMPO-4-LA (SW-LECTURA).
           MOVE SALANT-S TO CAMPO-5-LA (SW-LECTURA).
           MOVE MOVDEB-S TO CAMPO-6-LA (SW-LECTURA).
           MOVE MOVCRE-S TO CAMPO-7-LA (SW-LECTURA).
           MOVE SALNVO-S TO CAMPO-8-LA (SW-LECTURA).
           MOVE  2  TO  CAMPO-9-LA (SW-LECTURA).
           ADD 1 TO SW-LECTURA.
       F-IMPRENIV.
      *
       I-LIMPIA-TABLA.
           MOVE SPACES TO TABLA-LIBRO-AUXILIAR (SW-LECTURA).
       F-LIMPIA-TABLA. EXIT.
      *
       I-MUESTRA-TABLA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-MUESTRA-TABLAS THRU F-MUESTRA-TABLAS.
         RETORNO-MUESTRA.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF SW-OPCION = 01
            DISPLAY
            "[CONTROL][->] Derecha [] Abajo [ESC] Regresa [F2] Salir"
             LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO UNTIL CRIGHT OR CDN OR F2 OR ESC
           ELSE
            IF SW-OPCION > 01 AND SW-OPCION < 03
             DISPLAY
             "[CONTROL][->] Derecha [] Abajo [CONTROL][<-] Izquierda [E
      -    "SC] Regresa [F2] Salir"
             LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO UNTIL CLEFT OR CDN OR CRIGHT OR F2
              OR ESC
            ELSE
             DISPLAY
            "[] Abajo [CONTROL][<-] Izquierda [ESC] Regresa [F2] Salir"
             LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO UNTIL CLEFT OR CDN OR F2 OR ESC.
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF CDN GO TO F-MUESTRA-TABLA.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ESC GO TO F-MUESTRA-TABLA.
           IF CRIGHT ADD  01 TO SW-OPCION END-IF
           IF CLEFT COMPUTE SW-OPCION = SW-OPCION - 01 END-IF
           PERFORM I-MUESTRA-TABLAS THRU F-MUESTRA-TABLAS.
           GO TO RETORNO-MUESTRA.
       F-MUESTRA-TABLA. EXIT.
      *
       I-MUESTRA-TABLAS.
           IF SW-OPCION = 01
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING I FROM 06 BY 1 UNTIL I > 23
            MOVE 6 TO CON-LIN
            PERFORM I-PRIMER-PANTALLA THRU F-PRIMER-PANTALLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18.
           IF SW-OPCION = 02
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING I FROM 06 BY 1 UNTIL I > 23
            MOVE 6 TO CON-LIN
            PERFORM I-SEGUND-PANTALLA THRU F-SEGUND-PANTALLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18.
           IF SW-OPCION = 03
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING I FROM 06 BY 1 UNTIL I > 23
            MOVE 6 TO CON-LIN
            PERFORM I-TERCER-PANTALLA THRU F-TERCER-PANTALLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18.
       F-MUESTRA-TABLAS. EXIT.
      *
       I-PRIMER-PANTALLA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02
           SPACES SIZE 78 LINE 02 POSITION 02
           " ** LIBRO AUXILIAR **                       MES:
      -    "                  " LINE 02 POSITION 02   REVERSE
           W-LONG LINE 02 POSITION 50
           WK-NOMBRE-MES(W-LONG) LINE 02 POSITION 53 REVERSE
           "   CODIGO   |                      D E S C R I P C I O N
      -    "                  " LINE 03 POSITION 02  REVERSE
           "   CUENTA   | RADICACION No. |No.DOC|No.SOP| FECHA  |   C O
      -    "N C E P T O   EMP " LINE 04 POSITION 02  REVERSE.
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF CAMPO-9-LA (SW-LECTURA) = 2
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           ELSE
            MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           CAMPO-1-LA (SW-LECTURA) LINE CON-LIN POSITION 02
           CAMPO-2-LA (SW-LECTURA) LINE CON-LIN POSITION 07
           CAMPO-3-LA (SW-LECTURA) LINE CON-LIN POSITION 10
           CAMPO-4-LA (SW-LECTURA) LINE CON-LIN POSITION 15.
           ADD 1 TO CON-LIN.
       F-PRIMER-PANTALLA. EXIT.
      *
       I-SEGUND-PANTALLA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02
           " ** LIBRO AUXILIAR **                       MES:
      -    "                 " LINE 02 POSITION 02   REVERSE
           W-LONG LINE 02 POSITION 50
           WK-NOMBRE-MES(W-LONG) LINE 02 POSITION 53 REVERSE
           "    CODIGO  |    S A L D O      |          M O V I M I E N T
      -    " O                " LINE 03 POSITION 02  REVERSE
           "    CUENTA  | A N T E R I O R   |  D E B I T O S    | C R E
      -    "D I T O S  |      " LINE 04 POSITION 02  REVERSE
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF CAMPO-9-LA (SW-LECTURA) = 2
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           ELSE
            MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           CAMPO-1-LA (SW-LECTURA) LINE CON-LIN POSITION 02
           CAMPO-2-LA (SW-LECTURA) LINE CON-LIN POSITION 07
           CAMPO-3-LA (SW-LECTURA) LINE CON-LIN POSITION 10
           CAMPO-5-LA (SW-LECTURA) LINE CON-LIN POSITION 15
           CAMPO-6-LA (SW-LECTURA) LINE CON-LIN POSITION 35
           CAMPO-7-LA (SW-LECTURA) LINE CON-LIN POSITION 55.
           ADD 1 TO CON-LIN.
       F-SEGUND-PANTALLA. EXIT.
      *
       I-TERCER-PANTALLA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02
           " ** LIBRO AUXILIAR **                       MES:
      -    "                 " LINE 02 POSITION 02   REVERSE
           W-LONG LINE 02 POSITION 50
           WK-NOMBRE-MES(W-LONG) LINE 02 POSITION 53 REVERSE
           "   CODIGO   |             MOVIMIENTO               |   N U E
      -    " V O              " LINE 03 POSITION 02  REVERSE
           "   CUENTA   |  D E B I T O S    |  C R E D I T O   |    S A
      -    "L D O       |     " LINE 04 POSITION 02  REVERSE.
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF CAMPO-9-LA (SW-LECTURA) = 2
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           ELSE
            MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           CAMPO-1-LA (SW-LECTURA) LINE CON-LIN POSITION 02
           CAMPO-2-LA (SW-LECTURA) LINE CON-LIN POSITION 07
           CAMPO-3-LA (SW-LECTURA) LINE CON-LIN POSITION 10
           CAMPO-6-LA (SW-LECTURA) LINE CON-LIN POSITION 15
           CAMPO-7-LA (SW-LECTURA) LINE CON-LIN POSITION 35
           CAMPO-8-LA (SW-LECTURA) LINE CON-LIN POSITION 55.
           ADD 1 TO CON-LIN.
       F-TERCER-PANTALLA. EXIT.
      *
       I-EVALUE-IMPRE2.
            IF CNDTACUM-NIT   > 0 AND CNDTACUM-FRA   = 0
               MOVE 1 TO RUT-OK
               MOVE CNDTACUM-NIT   TO CODIM
               PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
               IF SW-EOF = 1
                  MOVE CNDTACUM-NIT   TO CODNIT-S
                  MOVE "Error - CENTRO DE COSTO NO EXISTE" TO NOMNIT-S
                  PERFORM IMPRE2 THRU F-IMPRE2
               ELSE
                  MOVE CNDTACUM-NIT   TO CODNIT-S
                  MOVE NOMBM   TO NOMNIT-S
                  PERFORM IMPRE2 THRU F-IMPRE2.
       F-EVALUE-IMPRE2.
      *
       IMPRE2.
           IF SW-LECTURA > 18
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
             UNTIL CDN OR ESC
            PERFORM I-LIMPIA-TABLA THRU F-LIMPIA-TABLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18
            MOVE 01 TO SW-LECTURA
            IF ESC GO TO F-IMPRE2 END-IF.
           MOVE MAYCTA-NIT TO CAMPO-1-LA (SW-LECTURA).
           MOVE SUBCTA-NIT TO CAMPO-2-LA (SW-LECTURA).
           MOVE AUXCTA-NIT TO CAMPO-3-LA (SW-LECTURA).
           MOVE LDETNIT-CAMPO-4 TO CAMPO-4-LA (SW-LECTURA).
           MOVE SALANT-NIT TO CAMPO-5-LA (SW-LECTURA).
           MOVE MOVDEB-NIT TO CAMPO-6-LA (SW-LECTURA).
           MOVE MOVCRE-NIT TO CAMPO-7-LA (SW-LECTURA).
           MOVE SALNVO-NIT TO CAMPO-8-LA (SW-LECTURA).
           MOVE  2  TO  CAMPO-9-LA (SW-LECTURA).
           ADD 1 TO SW-LECTURA.
       F-IMPRE2.
      *
       I-EVALUE-IMPRE3.
            IF CNDTACUM-NIT   > 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
       F-EVALUE-IMPRE3.
      *
       IMPRE3.
           IF SW-LECTURA > 18
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
             UNTIL CDN OR ESC
            PERFORM I-LIMPIA-TABLA THRU F-LIMPIA-TABLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18
            MOVE 01 TO SW-LECTURA
            IF ESC GO TO F-IMPRE3 END-IF.
           MOVE SPACES TO CAMPO-1-LA (SW-LECTURA).
           MOVE SPACES TO CAMPO-2-LA (SW-LECTURA).
           MOVE SPACES TO CAMPO-3-LA (SW-LECTURA).
           MOVE LDETDET-CAMPO-4 TO CAMPO-4-LA (SW-LECTURA).
           MOVE SALANT-DET TO CAMPO-5-LA (SW-LECTURA).
           MOVE MOVDEB-DET TO CAMPO-6-LA (SW-LECTURA).
           MOVE MOVCRE-DET TO CAMPO-7-LA (SW-LECTURA).
           MOVE SALNVO-DET TO CAMPO-8-LA (SW-LECTURA).
           MOVE  2  TO  CAMPO-9-LA (SW-LECTURA).
           ADD 1 TO SW-LECTURA.
       F-IMPRE3.
      *
       I-MOVIMIENTO-CTA.
           IF CNDTACUM-MOVDEB (W-LONG) = 0 AND
              CNDTACUM-MOVCRE (W-LONG) = 0
              GO F-MOVIMIENTO-CTA.
           PERFORM I-ANALIZE-NOVEDADES  THRU F-ANALIZE-NOVEDADES.
       F-MOVIMIENTO-CTA.
      *
       I-ANALIZE-NOVEDADES.
           MOVE ZEROS TO RUT-OK WK-EXCEPTION.
           PERFORM I-START-CNDOCTOS-KEY01 THRU F-START-CNDOCTOS-KEY01.
           PERFORM I-EXTRAE-NOVEDADES THRU F-EXTRAE-NOVEDADES
                   UNTIL RUT-OK = 1 OR ESC.
           MOVE 0 TO RUT-OK.
       F-ANALIZE-NOVEDADES.
      *
       I-START-CNDOCTOS-KEY01.
               MOVE CNDTACUM-CLAVE    TO  ALT01-DOCTOS
               MOVE CNDTACUM-MAYOR    TO  MAYCTA-DOCTOS
               MOVE CNDTACUM-SUB      TO  SUBCTA-DOCTOS
               MOVE CNDTACUM-AUX      TO  AUXCTA-DOCTOS
               MOVE CNDTACUM-NIT      TO  ALTNIT-DOCTOS
               MOVE CNDTACUM-FRA      TO  NUMFRA-DOCTOS
               START CNDOCTOS KEY NOT LESS ALT01-DOCTOS INVALID KEY
                  MOVE 1 TO I-ERROR.
       F-START-CNDOCTOS-KEY01.
      *
       I-EXTRAE-NOVEDADES.
           IF I-ERROR             = 1 OR
              CNDOCTOS-STATUS  =  "23"
              MOVE 1 TO RUT-OK GO F-EXTRAE-NOVEDADES.
              MOVE   0   TO SW-IMPRESION-DOCTOS
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS
           IF RUT-OK = 1 GO F-EXTRAE-NOVEDADES.
           IF CNDTACUM-MAYOR   =   MAYCTA-DOCTOS AND
              CNDTACUM-SUB     =   SUBCTA-DOCTOS AND
              CNDTACUM-AUX     =   AUXCTA-DOCTOS AND
              CNDTACUM-NIT     =   ALTNIT-DOCTOS AND
              CNDTACUM-FRA     =   NUMFRA-DOCTOS
              PERFORM I-MUEVE-DATOS-CNDOCTOS THRU
                      F-MUEVE-DATOS-CNDOCTOS
              PERFORM I-IMPRIME-CNDOCTOS THRU F-IMPRIME-CNDOCTOS
              IF ESC GO TO F-EXTRAE-NOVEDADES END-IF
              PERFORM I-CEROS-MOVIMIENTO   THRU F-CEROS-MOVIMIENTO
           ELSE
               MOVE 1 TO RUT-OK.
       F-EXTRAE-NOVEDADES.
      *
       I-LEE-CNDOCTOS.
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
               MOVE 1 TO RUT-OK.
       F-LEE-CNDOCTOS.
      *
       I-MUEVE-DATOS-CNDOCTOS.
               MOVE RADICTIP-DOCTOS       TO  RAD-TIP
               MOVE RADICDOC-DOCTOS       TO  RAD-DOC
               MOVE RADIC-AUTOMATICA-DOCTOS TO  RAD-AUT
               MOVE ALTFRA-DOCTOS         TO  NUMDOC-M
               MOVE FECHA-CONTAB-DOCTOS   TO  FECHA-CONTAB-S
               MOVE CONCEPTO-DOCTOS       TO  CONCEPTO-M
               MOVE ALTEMP-DOCTOS         TO  EMPRESA-M
               IF ALTTIP-DOCTOS = 03
                  MOVE NUM-SOPORTE-DOCTOS   TO NUMFAC-M
               ELSE
                  IF ALTTIP-DOCTOS = 04
                     MOVE NUMFRA-DOCTOS  TO NUMFAC-M
                  ELSE
                     MOVE ALTFRA-DOCTOS TO NUMFAC-M.
               IF VALOR-DOCTOS IS POSITIVE
                 MOVE  VALOR-DOCTOS      TO  VALDEB-M.
               IF VALOR-DOCTOS IS NEGATIVE
                 MOVE VALOR-DOCTOS       TO  VALCRE-M.
       F-MUEVE-DATOS-CNDOCTOS.
      *
       I-IMPRIME-CNDOCTOS.
           IF SW-LECTURA > 18
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
             UNTIL CDN OR ESC
            PERFORM I-LIMPIA-TABLA THRU F-LIMPIA-TABLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18
            MOVE 01 TO SW-LECTURA
            IF ESC GO TO F-IMPRIME-CNDOCTOS END-IF.
           MOVE MAYCTA-M TO CAMPO-1-LA (SW-LECTURA).
           MOVE SUBCTA-M TO CAMPO-2-LA (SW-LECTURA).
           MOVE AUXCTA-M TO CAMPO-3-LA (SW-LECTURA).
           MOVE TITULO-5-CAMPO-4 TO CAMPO-4-LA (SW-LECTURA).
           MOVE SPACES     TO CAMPO-5-LA (SW-LECTURA).
           MOVE VALDEB-M   TO CAMPO-6-LA (SW-LECTURA).
           MOVE VALCRE-M   TO CAMPO-7-LA (SW-LECTURA).
           MOVE SPACES     TO CAMPO-8-LA (SW-LECTURA).
           MOVE  1  TO  CAMPO-9-LA (SW-LECTURA).
           ADD 1 TO SW-LECTURA.
       F-IMPRIME-CNDOCTOS.
      *
       I-CEROS-MOVIMIENTO.
               MOVE 0 TO VALDEB-M VALCRE-M.
       F-CEROS-MOVIMIENTO.
      *
       I-LEE-CNNITCED.
           MOVE 0 TO SW-EOF.
           READ CNNITCED WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED. EXIT.
      *
       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED CNDOCTOS CNDTACUM.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\PANTALLA.PRO".
      *
           COPY "..\PLB\CNDOCTOS.PLB".
           COPY "..\PLB\CNDTACUM.PLB".
      ******************************************************************
