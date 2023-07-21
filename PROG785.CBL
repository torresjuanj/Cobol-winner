       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG785.
      ******************************************************************
      * ACTUALIZACION CONTABLE AUTOMATICA  ABR-27-2002                 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CONTREXT.DYC".
           COPY "..\DYC\MVDOCTOS.DYC".
           COPY "..\DYC\NOMIBANK.DYC".
           COPY "..\DYC\CNDOCTOS.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\DYC\CONERROR.DYC".
           COPY "..\DYC\CNCONTRO.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\MVDOCTOS.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CONTREXT.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CONERROR.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG785".
       77      ID-PRG              PIC X(7)  VALUE "PROG785".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\MVDOCTOS.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\CONTREXT.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CONERROR.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\INFORME2.LBL".
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
         05    FILLER      PIC X(20)    VALUE SPACES.
         05    FILLER      PIC X(45)    VALUE
              "INCONSISTENCIAS DEL PROCESO DE ACTUALIZACION".
         05    FILLER      PIC X(15)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(11)    VALUE "Programa : ".
         05    I-PROGRAMA  PIC X(08)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(149)   VALUE ALL "=".


        03     TITULO-4.
         05    FILLER      PIC X(55).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(10).
         05    FILLER      PIC X(06).
         05    T-VDEB      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(02).
         05    T-VCRE      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(03).


        03     TITULO-LS.
         05    FILLER      PIC X(85).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).


        03     TITULO-LD.
         05    FILLER      PIC X(85).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).

        03     TITULO-A.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(10)    VALUE "  NUMERO  ".
         05    FILLER      PIC X(02)    VALUE " |".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(06)    VALUE "CUENTA".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "SUB".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "CTA".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(09)    VALUE "DOCUMENTO".
         05    FILLER      PIC X(15)    VALUE " |             ".
         05    FILLER      PIC X(21)    VALUE "     C O N C E P T O ".
         05    FILLER      PIC X(17)    VALUE "               |".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(17)    VALUE "D O C U M E N T O".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(15)    VALUE "   DIFERENCIA  ".

       03      TITULO-A1.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(10)    VALUE "SECUENCIA ".
         05    FILLER      PIC X(02)    VALUE " |".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(06)    VALUE "MAYOR ".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "CTA".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "AUX".
         05    FILLER      PIC X(02)    VALUE " |".
         05    FILLER      PIC X(11)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(49)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(17)    VALUE "     DEBITOS     ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(17)    VALUE "     CREDITOS    ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(17)    VALUE SPACES.


        03     LINEA-DE-DETALLE.
         05    FILLER      PIC X(01).
         05    I-RADICTIP-MVDOCTOS PIC 9(02).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-RADICDOC-MVDOCTOS PIC 9(06).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-SEC-AUTOMATICA  PIC 9(06).
         05    FILLER      PIC X(03).
         05    I-MAYCTA    PIC 9(04).
         05    FILLER      PIC X(04).
         05    I-SUB       PIC 9(02).
         05    FILLER      PIC X(04).
         05    I-AUX       PIC Z(04).
         05    FILLER      PIC X(03).
         05    I-FRA       PIC Z(06).
         05    FILLER      PIC X(06).
         05    I-CON       PIC X(40).
         05    FILLER      PIC X(02).
         05    I-VDEB      PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(02).
         05    I-VCRE      PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(04).
         05    I-VBAS      PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.

        03     TITULO-TOTAL.
         05    FILLER      PIC X(17)  VALUE "TOTAL FACTURAS:  ".
         05    T-TOTAL     PIC ZZZZZ.
         05    FILLER      PIC X(15)  VALUE SPACES.
         05    FILLER      PIC X(22)  VALUE "TOTAL ESTE DOCUMENTO :".
         05    FILLER      PIC X(38)  VALUE SPACES.
         05    T-TOTFAC    PIC $ZZ,ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(03).
         05    T-TOTBAS    PIC $ZZ,ZZZ,ZZZ,ZZZ.
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\MVDOCTOS.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\CONERROR.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
               COPY "..\DCL\CONTREXT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-PANTALLA-1B      THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES       THRU F-ACEPTA-MES
           PERFORM I-PROCESO-AUDITE   THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CNDTACUM  THRU F-STATUS-CNDTACUM
           PERFORM I-STATUS-CNDOCTOS  THRU F-STATUS-CNDOCTOS
           PERFORM I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS
           PERFORM I-TITULO-INFORME   THRU F-TITULO-INFORME.
           PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES.
           PERFORM I-START-MVDOCTOS   THRU F-START-MVDOCTOS
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-BUSCA-INCONS     THRU F-BUSCA-INCONS
                               UNTIL FIN-OK = 1
           COMPUTE W-TOTAL = G-TOTAL-DEBITOS + G-TOTAL-CREDITOS
           MOVE    0 TO FIN-OK
           PERFORM I-EVALUE-ACTUALIZACION THRU F-EVALUE-ACTUALIZACION
           PERFORM I-FIN-MODE             THRU  F-FIN-MODE.
       F-MAIN-PROCESS.

       I-CEROS-VARIABLES.
            MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS W-TOTAL
                    SW1    S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                    RUT-OK T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                    SW-99  G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                           FIN-OK  WK-PAGINA W-CONTA CON-LIN.
       F-CEROS-VARIABLES.

       I-EVALUE-ACTUALIZACION.
           IF SW1 = 0 AND W-TOTAL =  0
              PERFORM I-START-MVDOCTOS   THRU F-START-MVDOCTOS
              PERFORM I-DISPLAY          THRU F-DISPLAY
              PERFORM I-DISPLAY1         THRU F-DISPLAY1
              PERFORM I-BUSCA-MOVTO      THRU F-BUSCA-MOVTO
                                              UNTIL FIN-OK = 1
              PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES
           ELSE
               PERFORM I-GRABACION       THRU F-GRABACION
               MOVE SPACES  TO REG-INFORMES
               WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
               MOVE "TOTAL MOVTO DEL MES" TO I-CON
               MOVE W-TOTAL          TO I-VBAS
               MOVE G-TOTAL-DEBITOS  TO I-VDEB
               MOVE G-TOTAL-CREDITOS TO I-VCRE
               WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
               MOVE "97"      TO  STATUS-NOMINA
               MOVE "EVALUE-ACTUALIZACION  "  TO W-CONCEPTO
               PERFORM I-FLAG-ERROR  THRU F-FLAG-ERROR
               PERFORM RUTINA-ERROR-NOMINA.
       F-EVALUE-ACTUALIZACION.
      *
       I-GRABACION.
           PERFORM I-LABEL-CONERROR  THRU  F-LABEL-CONERROR.
           PERFORM I-STATUS-CONERROR THRU  F-STATUS-CONERROR
           OPEN    I-O CONERROR.
           MOVE     20                 TO  W-COMP.
           PERFORM I-BUSCA-CONTREXT  THRU  F-BUSCA-CONTREXT.
           MOVE    WK-CONSEC           TO  CONSEC-CONERROR.
           MOVE    CON-USERNAME        TO  USUARIO-CONERROR.
           MOVE    CON-COD-EMPRESA     TO  EMPRESA-CONERROR.
           MOVE    CON-PROGRAMA        TO  PROGRAMA-CONERROR
           MOVE    WK-FECHA-HOY        TO  FECHA-CONERROR
           MOVE    WK-HORA-HOY         TO  HORA-CONERROR
           MOVE    ZEROS               TO  SW-CONERROR.
           WRITE   REG-CONERROR.
           CLOSE   CONERROR.
       F-GRABACION.
      *
       I-LABEL-ARCHIVOS.
           MOVE W-PROGRAMA               TO I-PROGRAMA
           MOVE WEMPRESA                 TO TIT-CIA
           MOVE 1                        TO IND-INFORMES
           PERFORM I-LABEL-MVDOCTOS    THRU F-LABEL-MVDOCTOS.
           PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           MOVE CON-PROGRAMA             TO LAB-INFORMES-IDENT
      *    MOVE W-LONG                   TO LAB-INFORMES-PERIODO
           IF   CON-PROGRAMA = "786"
                MOVE W-LONG              TO LAB-MVDOCTOS-MES
                MOVE CON-ANO             TO LAB-MVDOCTOS-ANO.
           MOVE "น"                      TO WK-RAYAS-U
           MOVE "ฬ"                      TO WK-RAYAS-P
           MOVE "ถ"                      TO WK-RAYA-U
           MOVE "ว"                      TO WK-RAYA-P
           MOVE  1                       TO SW-ABRE.
           PERFORM I-DISPLAY-LABEL  THRU F-DISPLAY-LABEL.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNCATCTA CNNITCED
                       EMPRESAS CNCOMPTE
           OPEN OUTPUT INFORMES.
           OPEN I-O    MVDOCTOS CNDOCTOS.
           PERFORM  I-ABRE-CNCONTRO  THRU F-ABRE-CNCONTRO.
           OPEN I-O CNDTACUM.
       F-ABRE-ARCHIVOS.
      *
       I-PANTALLA-1B.
           MOVE "*** ACTUALIZACION EN LINEA ***"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  PROCESANDO EL MES:                        บ"
                                               LINE 11 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 12 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
           COMPUTE MES-XX =  W-CONT-MES + 2.
           IF MES-XX = 13     AND  WK-MES = 1
              MOVE  01       TO  MES-XX.
           IF MES-XX = 14     AND  WK-MES = 2
              MOVE 2 TO MES-XX.
           MOVE ZEROS TO MES-XX.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE WK-MES                 TO  W-LONG
               IF CON-PROGRAMA = "707" OR
                  CON-PROGRAMA = "786"
                  MOVE CON-PERIODO         TO  W-LONG.
               MOVE ZEROS                  TO  WK-EXCEPTION
      *         ACCEPT W-LONG                        LINE 11 POSITION 50
      *                                              REVERSE
      *         CONVERT
      *         TAB
      *         NO BEEP
      *         ON EXCEPTION WK-EX
      *         PERFORM 999-EXCEPTION.
      *         DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
      *         IF  W-LONG  =  0 OR W-LONG > 13
      *             DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
      *                                              LINE 24 POSITION 1
      *                                              REVERSE BLINK
      *             GO I-ACEPTA-MES.
      *         COMPUTE W-CONT-MES  = W-CONT-MES + 1.
      *         IF W-LONG  NOT =   W-CONT-MES
      *             PERFORM I-PERIODO-DIFERENTE THRU F-PERIODO-DIFERENTE
      *             EXIT PROGRAM.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
      *         IF  F2  PERFORM I-FIN-SALIR THRU F-FIN-MODE.
           IF W-LONG = 1 OR W-LONG = 14
                  COMPUTE MESW = W-LONG + 13
           ELSE
                  COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.

       I-PERIODO-DIFERENTE.
            MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "บ        ESTA INTENTANDO ACTUALIZAR UN MES       บ"
            LINE 10 POSITION 17 BEEP REVERSE.
            DISPLAY "บ          CONTABLE QUE YA FUE CERRADO O         บ"
            LINE 11 POSITION 17 BEEP REVERSE.
            DISPLAY "บ             O ES SUPERIOR AL ACTUAL            บ"
            LINE 12 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 13 POSITION 17 BEEP REVERSE
            "      <ENTER>  CONTINUE    "
            LINE 14 POSITION 19 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-PERIODO-DIFERENTE. EXIT.

       I-BUSCA-INCONS.
           DISPLAY
              "PROCESANDO INCONSISTENCIAS DEL MOVIMIENTO A ACTUALIZAR"
              LINE 24 POSITION 16.
           PERFORM I-LEE-MVDOCTOS THRU F-LEE-MVDOCTOS
           IF FIN-OK = 1   GO F-BUSCA-INCONS.
           PERFORM I-EVALUE-VR  THRU F-EVALUE-VR.
           MOVE MAYCTA-MVDOCTOS TO MAYMAE
           MOVE SUBCTA-MVDOCTOS TO SUBMAE
           MOVE AUXCTA-MVDOCTOS TO AUXMAE
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1 OR MAYCTA-MVDOCTOS = 0
              MOVE 1 TO SW1
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF ESTACTA = "S"
              MOVE 1 TO SW1
              MOVE "CTA NO PERMITE MVTO"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF AUXCTA-1-MVDOCTOS = 0 AND AUXCTA-2-MVDOCTOS > 0
              MOVE 1 TO SW1
              MOVE "AUX1 = 0 Y AUX2 > 0"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF CODPGT = "S"
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 1
                 MOVE "NIT/CEDULA ERRADA"    TO I-CON
                 MOVE 1 TO SW1
                 PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
                 GO I-BUSCA-INCONS.
           IF CODPGT = "S"  AND ALTNIT-MVDOCTOS = 0
              MOVE "ALTNIT = CEROS   "    TO I-CON
              MOVE 1 TO SW1
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
       F-BUSCA-INCONS.
      *
       I-EVALUE-VR.
            IF VALOR-MVDOCTOS IS POSITIVE
               ADD VALOR-MVDOCTOS TO G-TOTAL-DEBITOS.
            IF VALOR-MVDOCTOS IS NEGATIVE
               ADD VALOR-MVDOCTOS TO G-TOTAL-CREDITOS.
       F-EVALUE-VR.
      *
       I-BUSCA-MOVTO.
           PERFORM I-LEE-MVDOCTOS THRU F-LEE-MVDOCTOS
           IF FIN-OK = 1   GO F-BUSCA-MOVTO.
           MOVE MAYCTA-MVDOCTOS TO MAYMAE
           MOVE SUBCTA-MVDOCTOS TO SUBMAE
           MOVE AUXCTA-MVDOCTOS TO AUXMAE
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
           IF CODPGT = "S"
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 1
              MOVE "NIT/CEDULA ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
           MOVE 8 TO I.
           PERFORM I-NIVEL-1    THRU F-NIVEL-1
           PERFORM I-NIVEL-2    THRU F-NIVEL-2
           PERFORM I-NIVEL-3    THRU F-NIVEL-3
           PERFORM I-NIVEL-4    THRU F-NIVEL-4
           PERFORM I-NIVEL-5    THRU F-NIVEL-5
           PERFORM I-NIVEL-6    THRU F-NIVEL-6
           IF CODPGT = "S"
              PERFORM I-NIVEL-7    THRU F-NIVEL-7.
           IF INDDETAL = "S"
              PERFORM I-NIVEL-8     THRU F-NIVEL-8.
           PERFORM I-ACTUALIZE-MVDOCTOS THRU F-ACTUALIZE-MVDOCTOS.
       F-BUSCA-MOVTO.
      *
       I-EVALUE-SALDO.
           DISPLAY
              "PROCESAMIENTO DE ACTUALIZACION DE SALDOS"
              LINE 18 POSITION 21 REVERSE.
              PERFORM I-START-CNDTACUM   THRU F-START-CNDTACUM
              PERFORM I-BUSCA-CNDTACUM   THRU F-BUSCA-CNDTACUM
                               UNTIL FIN-OK = 1.
       F-EVALUE-SALDO.
      *
       I-BUSCA-CNDTACUM.
           PERFORM I-LEE-CNDTACUM-S THRU F-LEE-CNDTACUM-S
           IF FIN-OK = 1   GO F-BUSCA-CNDTACUM.
           PERFORM I-SALDO-NUEVO    THRU F-SALDO-NUEVO
           PERFORM I-GRABA-CNDTACUM THRU F-GRABA-CNDTACUM.
       F-BUSCA-CNDTACUM.
      *
       I-SALDO-NUEVO.
           COMPUTE CNDTACUM-SALDO(W-LONG) = CNDTACUM-SALDO(MESW)
                                          + CNDTACUM-MOVDEB(W-LONG)
                                          + CNDTACUM-MOVCRE(W-LONG).
       F-SALDO-NUEVO.

       I-REGISTRO-ERRADO.
           MOVE ALTMES-MVDOCTOS     TO   W-ALTMES-MVDOCTOS
           MOVE ALTTIP-MVDOCTOS     TO   LLAVM1-COMP
           PERFORM I-TIPO         THRU F-TIPO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-REGISTRO-ERRADO.

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

       I-START-MVDOCTOS.
               MOVE 0                TO  KEY00-MVDOCTOS
               START MVDOCTOS  KEY > KEY00-MVDOCTOS INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     MOVE  "START MVDOCTOS KEY > KEY00-MVDOCTOS"
                     TO W-CONCEPTO
                     PERFORM I-FLAG-ERROR1 THRU F-FLAG-ERROR1
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-MVDOCTOS.
      *
       I-START-CNCATCTA.
               MOVE ZEROS                  TO  CTAMAE
               START CNCATCTA  KEY >   CTAMAE       INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     MOVE "START CNCATCTA KEY > CTAMAE" TO W-CONCEPTO
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNCATCTA.
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
      *
       I-ACTUALIZE-MVDOCTOS.
           MOVE  REG-MVDOCTOS TO REG-DOCTOS.
           MOVE  1 TO CONDICION-MVDOCTOS  CONDICION-DOCTOS.
           WRITE REG-DOCTOS INVALID KEY REWRITE REG-DOCTOS.
           WRITE REG-MVDOCTOS INVALID KEY REWRITE REG-MVDOCTOS.
       F-ACTUALIZE-MVDOCTOS.
      *
       I-LEE-MVDOCTOS.
              DISPLAY RADICTIP-MVDOCTOS        LINE 20 POSITION 36
              DISPLAY MAYCTA-MVDOCTOS          LINE 20 POSITION 56
              DISPLAY SUBCTA-MVDOCTOS          LINE 20 POSITION 61
              DISPLAY AUXCTA-MVDOCTOS          LINE 20 POSITION 63
              DISPLAY RADIC-MVDOCTOS           LINE 21 POSITION 36
              DISPLAY NUMFRA-MVDOCTOS          LINE 21 POSITION 56
              DISPLAY ALTNIT-MVDOCTOS          LINE 22 POSITION 36
              READ MVDOCTOS NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK.
              IF FIN-OK = 1 GO F-LEE-MVDOCTOS.
              IF CONDICION-MVDOCTOS = 1 GO I-LEE-MVDOCTOS.
              PERFORM I-LECTURA-CNCATCTA THRU F-LECTURA-CNCATCTA.
               IF  INDDETAL = "N" AND NUMFRA-MVDOCTOS NOT = 0
                      MOVE    ZEROS  TO NUMFRA-MVDOCTOS.

               IF  CODPGT   = "N" AND ALTNIT-MVDOCTOS NOT = 0
                      MOVE    ZEROS  TO  ALTNIT-MVDOCTOS.
       F-LEE-MVDOCTOS.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO    W-NOMBRE.
       F-LEE-EMPRESAS.
      *
       I-TIPO.
           PERFORM I-LEE-COMPBTE THRU F-LEE-COMPBTE.
       F-TIPO.
      *
       I-LEE-COMPBTE.
             MOVE 0 TO SW-EOF.
             READ CNCOMPTE WITH NO LOCK INVALID KEY
                  MOVE 1 TO SW-EOF.
       F-LEE-COMPBTE.
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
            DISPLAY WK-ESPACIOS               LINE 24 POSITION 01.
            MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
            DISPLAY " " LINE 25 POSITION 1 CONTROL SG-CONTROL-STRING
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                LINE 03 POSITION 13 REVERSE
               DISPLAY "บ       **PROCESO DE CONTABILIZACION AUTOMATICA*
      -       "*     บ"
                                LINE 04 POSITION 13 REVERSE
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                LINE 05 POSITION 13 REVERSE.
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
            DISPLAY " " LINE 25 POSITION 1 CONTROL SG-CONTROL-STRING.
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
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
                MOVE MAYCTA-MVDOCTOS          TO I-MAYCTA
                MOVE SUBCTA-MVDOCTOS          TO I-SUB
                MOVE AUXCTA-MVDOCTOS          TO I-AUX
                MOVE ALTFRA-MVDOCTOS          TO I-FRA
                MOVE VRBASE-MVDOCTOS          TO I-VBAS
                MOVE RADICTIP-MVDOCTOS        TO I-RADICTIP-MVDOCTOS
                MOVE RADICDOC-MVDOCTOS        TO I-RADICDOC-MVDOCTOS
                MOVE RADIC-AUTOMATICA-MVDOCTOS TO I-SEC-AUTOMATICA
                PERFORM I-EVALUE-VALOR THRU F-EVALUE-VALOR.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO W-CONTA
                ADD 1                      TO CON-LIN.
       F-MUEVE-INFORME-1.
      *
       I-EVALUE-VALOR.
            IF VALOR-MVDOCTOS IS POSITIVE
               MOVE VALOR-MVDOCTOS  TO I-VDEB
               MOVE 0             TO I-VCRE
               ADD VALOR-MVDOCTOS TO W-TOTAL-DEBITOS
               ADD VALOR-MVDOCTOS TO S-TOTAL-DEBITOS
               ADD VALOR-MVDOCTOS TO T-TOTAL-DEBITOS
               ADD VALOR-MVDOCTOS TO G-TOTAL-DEBITOS.
            IF VALOR-MVDOCTOS IS NEGATIVE
               MOVE 0             TO I-VDEB
               MOVE VALOR-MVDOCTOS  TO I-VCRE
               ADD VALOR-MVDOCTOS TO W-TOTAL-CREDITOS
               ADD VALOR-MVDOCTOS TO S-TOTAL-CREDITOS
               ADD VALOR-MVDOCTOS TO T-TOTAL-CREDITOS
               ADD VALOR-MVDOCTOS TO G-TOTAL-CREDITOS.
       F-EVALUE-VALOR.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
       I-FRA-NUEVO.
           MOVE ALTFRA-MVDOCTOS     TO   W-ALTFRA-MVDOCTOS
           MOVE ALTMES-MVDOCTOS     TO   W-ALTMES-MVDOCTOS
           MOVE NUMFRA-MVDOCTOS     TO   W-NUMFRA-MVDOCTOS
           MOVE ALTTIP-MVDOCTOS     TO   LLAVM1-COMP.
           PERFORM I-TIPO           THRU F-TIPO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-FRA-NUEVO.

       I-TOTAL-DTO.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1
                                        REVERSE
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
       F-TOTAL-DTO.
      *

       I-TOTAL-MES.
                MOVE "TOTAL MES:"           TO T-CON1.
                MOVE  WK-NOMBRE-MES(W-ALTMES-MVDOCTOS) TO T-CON2.
                MOVE S-TOTAL-DEBITOS        TO T-VDEB.
                MOVE S-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1
                                        REVERSE
                MOVE 0 TO S-TOTAL-DEBITOS S-TOTAL-CREDITOS.
      *          PERFORM I-IMPR-TITULO-MES THRU F-IMPR-TITULO-MES.
       F-TOTAL-MES.
      *
       I-TOTAL-TIP.
                MOVE "TOTAL COMPROBANTE  "  TO T-CON
                MOVE T-TOTAL-DEBITOS        TO T-VDEB.
                MOVE T-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1
                                        REVERSE
                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
       F-TOTAL-TIP.
      *
       I-IMPR-TITULO-MES.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 1                     TO CON-LIN.
       F-IMPR-TITULO-MES.
      *
       I-IMPR-TITULO-TIP.
                PERFORM I-TIPO         THRU F-TIPO.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 2              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1
                                        REVERSE.
       F-IMPR-TITULO-TIP.
      *
       I-TOTAL-GEN.
                MOVE "TOTAL GENERAL   "     TO T-CON
                MOVE G-TOTAL-DEBITOS        TO T-VDEB.
                MOVE G-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                ADD 3                      TO CON-LIN.
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                          S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                          T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                          G-TOTAL-DEBITOS G-TOTAL-CREDITOS.
       F-TOTAL-GEN.
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
       I-LECTURA-CNCATCTA.
           MOVE MAYCTA-MVDOCTOS   TO MAYMAE.
           MOVE SUBCTA-MVDOCTOS   TO SUBMAE
           MOVE AUXCTA-MVDOCTOS   TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               MOVE  1    TO SW1-300.
       F-LECTURA-CNCATCTA.

       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
           
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
             
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A1 AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               MOVE 6                          TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
               MOVE C-TOTFAC       TO   T-TOTFAC
               MOVE C-TOTBAS       TO   T-TOTBAS
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 2
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 1
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 1
               ADD 2               TO   CON-LIN.
       F-TOTAL-INFORME.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCATCTA CNCOMPTE
                 CNNITCED CNDTACUM CNCONTRO
                 EMPRESAS CNDOCTOS MVDOCTOS
                 INFORMES.
       I-FIN-SALIR.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-STATUS-CNDOCTOS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNDOCTOS
               MOVE 1                      TO  SW-ABRE
               IF   CNDOCTOS-STATUS              =  "35"
                   OPEN OUTPUT CNDOCTOS
               ELSE
                   IF  CNDOCTOS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNDOCTOS
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE CNDOCTOS.
       F-STATUS-CNDOCTOS.
      *
       I-FLAG-ERROR.
            PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 07 POSITION 17      REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 08 POSITION 17 BEEP REVERSE BLINK
            DISPLAY "บ  NO EFECTUO LA CONTABILIZACION DEL MOVIMIENTO  บ"
            LINE 09 POSITION 17 BEEP REVERSE.
            DISPLAY "บ    VERIFIQUE EL PROCESO PASO A  PASO           บ"
            LINE 10 POSITION 17      REVERSE.
            DISPLAY "บ               <F8>  CONTINUE                   บ"
            LINE 11 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 12 POSITION 17 BEEP REVERSE
            DISPLAY  W-CONCEPTO  LINE 19 POSITION 20.
            PERFORM ACEPTA-TRUCO.
            IF F8  NEXT SENTENCE
               ELSE
               GO I-FLAG-ERROR.
       F-FLAG-ERROR.   EXIT.
      *
       I-FLAG-ERROR1.
            PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 07 POSITION 17      REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 08 POSITION 17 BEEP REVERSE BLINK
            DISPLAY "บ         EL ARCHIVO A CONTABILIZAR              บ"
            LINE 09 POSITION 17 BEEP REVERSE.
            DISPLAY "บ           SE HALLA SIN DATOS                   บ"
            LINE 10 POSITION 17.
            DISPLAY "บ           < F8 >  Para continuar               บ"
            LINE 11 POSITION 17      REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 12 POSITION 17 BEEP REVERSE
            DISPLAY  W-CONCEPTO  LINE 19 POSITION 20.
            PERFORM ACEPTA-TRUCO.
            IF F8   NEXT SENTENCE
               ELSE
               GO I-FLAG-ERROR.
       F-FLAG-ERROR1.   EXIT.
      *
       I-STATUS-CONERROR.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CONERROR
               MOVE 1                      TO  SW-ABRE
               IF  CONERROR-STATUS              =  "35"
                   OPEN OUTPUT CONERROR
               ELSE
                   IF  CONERROR-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CONERROR
                       PERFORM I-FIN-MODE      THRU F-FIN-MODE.
               CLOSE CONERROR.
       F-STATUS-CONERROR.


               COPY "..\PRO\ACTULINE.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\CONTREXT.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\STA\CNDTACUM.STA".

               COPY "..\PLB\MVDOCTOS.PLB".
               COPY "..\PLB\CONERROR.PLB".
               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".

