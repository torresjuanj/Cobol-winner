       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG781.
      ******************************************************************
      * ACTUALIZACION DE SALDOS         ABR-27-2002                    *
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
           COPY "..\DYC\CNDOCTOS.DYC".
           COPY "..\DYC\CNCONTRO.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG781".
       77      ID-PRG         PIC X(7)       VALUE "PROG781".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".

               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
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
         05    FILLER      PIC X(11)    VALUE "MES      : ".
         05    I-MES       PIC X(08)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(149)   VALUE ALL "=".

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
         05    I-RADICTIP-DOCTOS PIC 9(03).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-RADICDOC-DOCTOS PIC 9(06).
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
         05    FILLER      PIC X(04).
         05    I-NITS      PIC ZZZZ,ZZZ,ZZZ.
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
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
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           MOVE    ZEROS TO SW1.
           PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
           IF   CON-PROGRAMA  NOT =    "738"
                PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP.
           PERFORM I-PROCESO-AUDITE   THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CNDTACUM  THRU F-STATUS-CNDTACUM
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           PERFORM I-TITULO-INFORME   THRU F-TITULO-INFORME.
           PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES.
           PERFORM I-DISPLAY          THRU F-DISPLAY
           MOVE    ZEROS      TO W-TOTAL.
           MOVE    0 TO FIN-OK  SW1
           PERFORM I-EVALUE-ACTUALIZACION THRU  F-EVALUE-ACTUALIZACION
           IF   CON-PROGRAMA   =    "738"  OR
                CON-PROGRAMA   =    "733"
                PERFORM I-FIN-MODE   THRU  F-FIN-MODE.
           PERFORM I-EVALUE-SALDO    THRU  F-EVALUE-SALDO.
           PERFORM I-FIN-MODE        THRU  F-FIN-MODE.
       F-MAIN-PROCESS.

       I-CEROS-VARIABLES.
            MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS W-TOTAL
                           S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                    RUT-OK T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                    SW-99  G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                           FIN-OK  WK-PAGINA W-CONTA CON-LIN.
       F-CEROS-VARIABLES.

       I-EVALUE-ACTUALIZACION.
           IF SW1 = 0 AND W-TOTAL =  0
              PERFORM I-START-CNDOCTOS   THRU F-START-CNDOCTOS
              PERFORM I-DISPLAY          THRU F-DISPLAY
              PERFORM I-DISPLAY1         THRU F-DISPLAY1
              PERFORM I-BUSCA-MOVTO      THRU F-BUSCA-MOVTO
                               UNTIL FIN-OK = 1
              PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES
           ELSE
               MOVE SPACES  TO REG-INFORMES
               MOVE "TOTAL MOVTO DEL MES" TO I-CON
               MOVE W-TOTAL               TO I-VBAS
               MOVE G-TOTAL-DEBITOS       TO I-VDEB
               MOVE G-TOTAL-CREDITOS      TO I-VCRE
               WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
               MOVE "97"      TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA.
       F-EVALUE-ACTUALIZACION.
      *
       I-LABEL-ARCHIVOS.
           MOVE WK-NOMBRE-MES(W-LONG)  TO   I-MES
           MOVE WEMPRESA               TO   TIT-CIA
           MOVE 1                      TO   IND-INFORMES
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
           PERFORM I-DISPLAY-LABEL  THRU F-DISPLAY-LABEL.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-ABRE-CNCONTRO THRU F-ABRE-CNCONTRO.
           OPEN INPUT  CNCATCTA CNNITCED
                       EMPRESAS CNCOMPTE
           OPEN OUTPUT INFORMES.
           OPEN I-O   CNDOCTOS CNDTACUM.
       F-ABRE-ARCHIVOS.
      *
       I-PANTALLA-1B.
           MOVE "* A C T U A L I Z A C I O N   D E   S A L D O S *"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A ACTUALIZAR:               บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ              [F2] TERMINAR                 บ"
                                               LINE 12 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  WK-EXCEPTION
               IF   CON-PROGRAMA  =    "738"  OR
                    CON-PROGRAMA  =    "733"  OR
                    CON-PROGRAMA  =    "783"
                    MOVE  14  TO W-LONG
               ELSE
                 ACCEPT W-LONG         LINE 11 POSITION 50 REVERSE
                                       UPDATE TAB NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS   LINE 24 POSITION 1
               IF   CON-PROGRAMA  =    "783"
                    MOVE  13  TO W-LONG.

               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF   CON-PROGRAMA  =  "738"  AND CON-PERIODO NOT = ZEROS
                    MOVE  CON-PERIODO  TO W-LONG.

               IF   CON-PROGRAMA  =  "733"
                    MOVE  CON-PERIODO  TO W-LONG.

               COMPUTE W-CONT-MES  = W-CONT-MES + 1.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF  F2  PERFORM I-FIN-SALIR THRU F-FIN-MODE.
           IF W-LONG = 1  OR W-LONG = 14
                  MOVE 14    TO    MESW
           ELSE
                  COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.

       I-PERIODO-DIFERENTE.
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "บ   ESTA PROCESANDO UN MES QUE YA FUE CERRADO O  บ"
            LINE 09 POSITION 17 BEEP REVERSE.
            DISPLAY "บ        ES DIFERENTE AL MES DE TRABAJO          บ"
            LINE 10 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 11 POSITION 17 BEEP REVERSE
            "   <ENTER>   CONTINUE "
            LINE 12 POSITION 29 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-PERIODO-DIFERENTE. EXIT.

       I-BUSCA-INCONS.
           DISPLAY
              "PROCESANDO INCONSISTENCIAS DEL MOVIMIENTO A ACTUALIZAR"
              LINE 24 POSITION 16.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS
           IF FIN-OK = 1   GO F-BUSCA-INCONS.
           PERFORM I-EVALUE-VR  THRU F-EVALUE-VR.
           MOVE MAYCTA-DOCTOS TO MAYMAE
           MOVE SUBCTA-DOCTOS TO SUBMAE
           MOVE AUXCTA-DOCTOS TO AUXMAE
           MOVE ZEROS TO  SW-EOF.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1
              MOVE 1 TO SW1
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF SW-EOF = 1 OR MAYCTA-DOCTOS = 0
              MOVE 1 TO SW1
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF INDDETAL = "S"  AND NUMFRA-DOCTOS = 0
              DISPLAY "EN NUMFRA DOCTOS" LINE 18 POSITION 1
              MOVE "NUMFRA = CEROS   "    TO I-CON
              MOVE 1 TO SW1
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.

           IF ESTACTA = "S"
              MOVE 1 TO SW1
              MOVE "CTA NO PERMITE MVTO"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF AUXCTA-1-DOCTOS = 0 AND AUXCTA-2-DOCTOS > 0
              MOVE 1 TO SW1
              MOVE "AUX1 = 0 Y AUX2 > 0"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
           IF CODPGT = "S"
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 1
                 MOVE "NIT/CEDULA ERRADA"    TO I-CON
                 MOVE  ALTNIT-DOCTOS         TO I-NITS
                 MOVE 1 TO SW1
                 PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
                 GO I-BUSCA-INCONS.
           IF CODPGT = "S"  AND ALTNIT-DOCTOS = 0
              MOVE "ALTNIT = CEROS   "    TO I-CON
              MOVE 1 TO SW1
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO
              GO I-BUSCA-INCONS.
       F-BUSCA-INCONS.
      *
       I-EVALUE-VR.
            IF VALOR-DOCTOS IS POSITIVE
               ADD VALOR-DOCTOS TO G-TOTAL-DEBITOS.
            IF VALOR-DOCTOS IS NEGATIVE
               ADD VALOR-DOCTOS TO G-TOTAL-CREDITOS.
       F-EVALUE-VR.
      *
       I-BUSCA-MOVTO.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS
           IF FIN-OK = 1   GO F-BUSCA-MOVTO.
           MOVE MAYCTA-DOCTOS TO MAYMAE
           MOVE SUBCTA-DOCTOS TO SUBMAE
           MOVE AUXCTA-DOCTOS TO AUXMAE
      *
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           IF SW-EOF = 1
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO   THRU F-REGISTRO-ERRADO.
      *
           IF CODPGT = "S"
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 1
              MOVE "NIT/CEDULA ERRADA"    TO I-CON
              MOVE  ALTNIT-DOCTOS         TO I-NITS
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
      *
           IF MAYCTA-DOCTOS = 0
              MOVE "CTA CONTABLE ERRADA"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
      *
           IF INDDETAL = "S"  AND NUMFRA-DOCTOS = 0
              MOVE "NUMFRA = CEROS   "    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
      *
           IF ESTACTA = "S"
              MOVE "CTA NO PERMITE MVTO"    TO I-CON
              PERFORM I-REGISTRO-ERRADO THRU F-REGISTRO-ERRADO.
      *
           IF CODPGT = "S"  AND ALTNIT-DOCTOS = 0
              MOVE "ALTNIT = CEROS   "    TO I-CON
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
           PERFORM I-ACTUALIZE-CNDOCTOS THRU F-ACTUALIZE-CNDOCTOS.
       F-BUSCA-MOVTO.
      *
       I-EVALUE-SALDO.
           DISPLAY
              "PROCESAMIENTO DE ACTUALIZACION DE SALDOS"
              LINE 18 POSITION 21 REVERSE.
              ACCEPT WK-HORA-HOY        FROM  TIME
              DISPLAY   WK-HORA-HOY     LINE  22 POSITION 1
              PERFORM I-START-CNDTACUM   THRU F-START-CNDTACUM
              PERFORM I-BUSCA-CNDTACUM   THRU F-BUSCA-CNDTACUM
                               UNTIL FIN-OK = 1.
              ACCEPT WK-HORA-HOY        FROM  TIME
              DISPLAY   WK-HORA-HOY     LINE  22 POSITION 1.
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
           MOVE ALTMES-DOCTOS     TO   W-ALTMES-DOCTOS
           MOVE ALTTIP-DOCTOS     TO   LLAVM1-COMP
           PERFORM I-TIPO         THRU F-TIPO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-REGISTRO-ERRADO.
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
           IF   CON-PROGRAMA   =    "733" OR
                CON-PROGRAMA   =    "783"
                GO F-CONFIRMA-COMP.
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

       I-START-CNDOCTOS.
               MOVE 0                TO  KEY00-DOCTOS
               START CNDOCTOS  KEY NOT LESS  KEY00-DOCTOS INVALID KEY
                     MOVE 1         TO  FIN-OK.
       F-START-CNDOCTOS.
      *
       I-START-CNCATCTA.
               MOVE ZEROS                  TO  CTAMAE
               START CNCATCTA  KEY >   CTAMAE       INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNCATCTA.
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
      *
       I-ACTUALIZE-CNDOCTOS.
           MOVE  1          TO CONDICION-DOCTOS.
           WRITE REG-DOCTOS INVALID KEY REWRITE REG-DOCTOS.
       F-ACTUALIZE-CNDOCTOS.
      *
       I-LEE-CNDOCTOS.
              DISPLAY RADICTIP-DOCTOS        LINE 20 POSITION 36
              DISPLAY MAYCTA-DOCTOS          LINE 20 POSITION 56
              DISPLAY SUBCTA-DOCTOS          LINE 20 POSITION 61
              DISPLAY AUXCTA-DOCTOS          LINE 20 POSITION 63
              DISPLAY RADIC-DOCTOS           LINE 21 POSITION 36
              DISPLAY NUMFRA-DOCTOS          LINE 21 POSITION 56
              DISPLAY ALTNIT-DOCTOS          LINE 22 POSITION 56
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                        MOVE 1 TO FIN-OK.
              IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
              IF VALOR-DOCTOS =  0    GO I-LEE-CNDOCTOS.
              IF CONDICION-DOCTOS = 1 GO I-LEE-CNDOCTOS.
       F-LEE-CNDOCTOS.
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
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
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
                MOVE MAYCTA-DOCTOS          TO I-MAYCTA
                MOVE SUBCTA-DOCTOS          TO I-SUB
                MOVE AUXCTA-DOCTOS          TO I-AUX
                MOVE ALTFRA-DOCTOS          TO I-FRA
                MOVE VRBASE-DOCTOS          TO I-VBAS
                MOVE RADICTIP-DOCTOS        TO I-RADICTIP-DOCTOS
                MOVE RADICDOC-DOCTOS        TO I-RADICDOC-DOCTOS
                MOVE RADIC-AUTOMATICA-DOCTOS TO I-SEC-AUTOMATICA
                PERFORM I-EVALUE-VALOR THRU F-EVALUE-VALOR.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO W-CONTA
                ADD 1                      TO CON-LIN.
       F-MUEVE-INFORME-1.
      *
       I-EVALUE-VALOR.
            IF VALOR-DOCTOS IS POSITIVE
               MOVE VALOR-DOCTOS  TO I-VDEB
               MOVE 0             TO I-VCRE
               ADD VALOR-DOCTOS TO W-TOTAL-DEBITOS
               ADD VALOR-DOCTOS TO S-TOTAL-DEBITOS
               ADD VALOR-DOCTOS TO T-TOTAL-DEBITOS
               ADD VALOR-DOCTOS TO G-TOTAL-DEBITOS.
            IF VALOR-DOCTOS IS NEGATIVE
               MOVE 0             TO I-VDEB
               MOVE VALOR-DOCTOS  TO I-VCRE
               ADD VALOR-DOCTOS TO W-TOTAL-CREDITOS
               ADD VALOR-DOCTOS TO S-TOTAL-CREDITOS
               ADD VALOR-DOCTOS TO T-TOTAL-CREDITOS
               ADD VALOR-DOCTOS TO G-TOTAL-CREDITOS.
       F-EVALUE-VALOR.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************

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
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           DISPLAY
              "CIERRE DE ARCHIVOS... "
              LINE 18 POSITION 21 REVERSE BEEP.
           CLOSE CNCATCTA CNCOMPTE
                 CNNITCED CNDTACUM
                 EMPRESAS CNCONTRO
                 INFORMES.
       I-FIN-SALIR.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\NIVELE1.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\DISLABEL.PRO".

               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\STA\CNDTACUM.STA".
               COPY "..\PLB\INFORMES.PLB".

