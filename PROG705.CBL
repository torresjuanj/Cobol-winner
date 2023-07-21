       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG705.
      ******************************************************************
      * RETENCION EN LA FUENTE E I.C.A.  ABR-27-2002                   *
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
           COPY "..\DYC\CNRETENC.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNRETENC.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG705".
       77      ID-PRG         PIC X(7)       VALUE "PROG705".
       77      FEC-DES        PIC 9(06)      VALUE ZEROS.
       77      KK             PIC 9(01)      VALUE ZEROS.
       77      JJ             PIC 9(06)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(06)      VALUE ZEROS.
       77      W-TOTAL-BASE   PIC S9(15)V99    VALUE ZEROS.
       77      G-TOTAL-BASE   PIC S9(15)V99    VALUE ZEROS.
       77      S-TOTAL-BASE   PIC S9(15)V99    VALUE ZEROS.
       77      T-TOTAL-BASE   PIC S9(15)V99    VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.

       01      WK-CUENTA-CONTABLE.
        03     WK-MAYOR         PIC 9(04)      VALUE ZEROS.
        03     WK-SUBCTA        PIC 9(02)      VALUE ZEROS.
        03     WK-AUXCTA        PIC 9(04)      VALUE ZEROS.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNRETENC.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".

               COPY "..\WRK\USER-ACT.WRK".
      *        COPY "..\WRK\TABLAGEN.WRK".
      *        COPY "..\WRK\EMPRESAS.WRK".
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
      *  05    FILLER                    PIC X(2)       VALUE "M".
      *  05    FILLER                    PIC X(2)       VALUE "0".
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 6962.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 3602.
         05    TIT-CIA                   PIC X(28)    VALUE SPACES.
         05    FILLER                    PIC X(1)     VALUE "" .
         05    FILLER                    PIC X(04)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.

        03     TITULO-1.
      *  05    FILLER                    PIC X(2)       VALUE "M".
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4626.
         05    FILLER      PIC X(18)    VALUE SPACES.
         05    FILLER      PIC X(34)    VALUE
              "INFORME DE RETENCION EN LA FUENTE".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(09)    VALUE "Proceso: ".
         05    I-FECHA     PIC X(11).

        03     TITULO-2.
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(26)    VALUE
              "INFORME GENERADO DEL MES :".
         05    FILLER      PIC X(03)    VALUE SPACES.
         05    I-MES4      PIC X(18)    VALUE SPACES.
         05    I-TIPO      PIC X(12)    VALUE "GENERAL".
         05    FILLER      PIC X(15)    VALUE "       A¥O : ".
         05    I-ANO4      PIC Z,ZZZ.
         05    FILLER      PIC X(24)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(22)    VALUE SPACES.

        03     TITULO-2A.
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(29)    VALUE SPACES.
         05    I-MES5      PIC X(18)    VALUE SPACES.
         05    I-TIPO5     PIC X(12)    VALUE "GENERAL".
         05    FILLER      PIC X(15)    VALUE SPACES.
         05    I-ANO5      PIC Z,ZZZ.

        03     TITULO-3.
         05    FILLER      PIC X(137)   VALUE ALL "=".

        03     TITULO-4.
         05    FILLER      PIC X(31).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(30).
         05    FILLER      PIC X(06).
         05    T-BASE      PIC ZZZZZZ,ZZZ,ZZ9.99.
         05    FILLER      PIC X(01).
         05    T-VDEB      PIC ZZZZZZ,ZZZ,ZZ9.99.
         05    FILLER      PIC X(01).
         05    T-VCRE      PIC ZZZZZZ,ZZZ,ZZ9.99-.

        03     TITULO-LS.
         05    FILLER      PIC X(30).
         05    FILLER      PIC X(108) VALUE ALL "-".

        03     TITULO-LD.
         05    FILLER      PIC X(30).
         05    FILLER      PIC X(108) VALUE ALL "=".

        03     TITULO-A.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE "COMPROB.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(04)    VALUE "TIPO".
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(04)    VALUE "ENT.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(07)    VALUE "DOCTO.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(07)    VALUE "FECHA.".
         05    FILLER      PIC X(05)    VALUE "|".
         05    FILLER      PIC X(09)    VALUE " NIT ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(32)    VALUE " NOMBRE O RAZON SOCIAL".
         05    FILLER      PIC X(19)    VALUE "|".
         05    FILLER      PIC X(34)    VALUE "  V A L O R E S  ".
         05    FILLER      PIC X(02)    VALUE "|".

       03      TITULO-A1.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE "CONTABLE".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(04)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
      *DOCTO
         05    FILLER      PIC X(07)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(07)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(13)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(32)    VALUE SPACES.
         05    FILLER      PIC X(06)    VALUE "|".

         05    FILLER      PIC X(11)    VALUE "    BASE  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(18)    VALUE "    DEBITOS     ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    CREDITOS  ".
         05    FILLER      PIC X(01)    VALUE "|".

        03     TITULO-6.
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4626.
         05    FILLER      PIC X(10).
         05    FILLER      PIC X(020)   VALUE "CUENTA CONTABLE:".
         05    FILLER      PIC X(02).
         05    I-MAYOR     PIC 9(04).
         05    FILLER      PIC X(01).
         05    I-SUBCTA    PIC 9(02).
         05    FILLER      PIC X(01).
         05    I-AUXCTA    PIC 9(04).
         05    FILLER      PIC X(05).
         05    I-NOMCTA    PIC X(35).

        03     LINEA-DE-DETALLE.
      *  05    FILLER            PIC 9(04)  USAGE IS COMP-1 VALUE 4623.
         05    FILLER            PIC X(02)  VALUE SPACES.
         05    I-RADICDOC-DOCTOS PIC 999999.
         05    FILLER            PIC X(03)  VALUE SPACES.
         05    I-RADICTIP-DOCTOS PIC 9(03).
         05    FILLER            PIC X(03)  VALUE SPACES.
         05    I-CODEMP          PIC Z(07).
         05    FILLER            PIC X(03).
         05    I-NUMFRA          PIC Z(06).
         05    FILLER            PIC X(02) VALUE SPACES.
         05    I-FECHOY-DOCTOS   PIC ZZZZZZZZ.
         05    FILLER            PIC X(02)  VALUE SPACES.
         05    I-NIT             PIC ZZZ,ZZZ,ZZZ.
         05    FILLER            PIC X(02).
         05    I-EMP             PIC X(30).
         05    FILLER            PIC X(02).
         05    I-BASE            PIC ZZZZZ,ZZZ,ZZZ.99-.
         05    FILLER            PIC X(01).
         05    I-VDEB            PIC ZZZZZ,ZZZ,ZZZ.99-.
         05    FILLER            PIC X(01).
         05    I-VCRE            PIC ZZZZZ,ZZZ,ZZZ.99-.

        03     LINEA-CUENTA.
      *  05    FILLER            PIC 9(04)  USAGE IS COMP-1 VALUE 4623.
         05    FILLER            PIC X(30)  VALUE SPACES.
         05    Q-MAYCTA          PIC 9999.
         05    FILLER            PIC X(02)  VALUE SPACES.
         05    Q-SUBCTA          PIC 9(02).
         05    FILLER            PIC X(02)  VALUE SPACES.
         05    Q-AUXCTA          PIC 9999.
         05    FILLER            PIC X(03).
         05    Q-NOMCTA          PIC X(30).
         05    FILLER            PIC X(06).
         05    Q-BASE            PIC ZZZZZ,ZZZ,ZZZ.99-.
         05    FILLER            PIC X(01).
         05    Q-VDEB            PIC ZZZZZ,ZZZ,ZZZ.99-.
         05    FILLER            PIC X(01).
         05    Q-VCRE            PIC ZZZZZ,ZZZ,ZZZ.99-.
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNRETENC.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU  F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-PANTALLA-1B     THRU  F-PANTALLA-1B
           PERFORM I-ACEPTA-MES      THRU  F-ACEPTA-MES
           PERFORM I-CONFIRMA-COMP   THRU  F-CONFIRMA-COMP
           PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
           PERFORM I-ZEROS-VARIABLES THRU  F-ZEROS-VARIABLES
           MOVE ZEROS  TO FIN-OK  WK-PAGINA CON-LIN
           MOVE  7        TO  I.
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-ARMA-TEMPORAL    THRU F-ARMA-TEMPORAL
           IF LL =  1
              PERFORM I-GENERA-LISTADO1  THRU F-GENERA-LISTADO1
              PERFORM I-ZEROS-VARIABLES  THRU F-ZEROS-VARIABLES
              PERFORM I-GENERA-LISTADO3  THRU F-GENERA-LISTADO3.
           IF LL = 2
              PERFORM I-GENERA-LISTADO2  THRU F-GENERA-LISTADO2
              PERFORM I-ZEROS-VARIABLES  THRU F-ZEROS-VARIABLES
              PERFOrM I-GENERA-LISTADO3  THRU F-GENERA-LISTADO3.
           IF LL = 3
              PERFORM I-GENERA-LISTADO1  THRU F-GENERA-LISTADO1
              PERFORM I-ZEROS-VARIABLES  THRU F-ZEROS-VARIABLES
              PERFORM I-GENERA-LISTADO2  THRU F-GENERA-LISTADO2
              PERFORM I-ZEROS-VARIABLES  THRU F-ZEROS-VARIABLES
              PERFORM I-GENERA-LISTADO3  THRU F-GENERA-LISTADO3.
           PERFORM I-FIN-MODE            THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE WK-MASCARA-FECHA         TO I-FECHA
           MOVE WEMPRESA                 TO TIT-CIA
           MOVE    1                     TO IND-INFORMES
           PERFORM I-LABEL-CNRETENC    THRU F-LABEL-CNRETENC.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS
           MOVE LABEL-INFORMES         TO  I-PROGRAMA
           MOVE "¹"                    TO  WK-RAYAS-U
           MOVE "Ì"                    TO  WK-RAYAS-P
           MOVE "¶"                    TO  WK-RAYA-U
           MOVE "Ç"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS CNNITCED
           OPEN INPUT  CNCATCTA.
           OPEN OUTPUT INFORMES CNRETENC.
           CLOSE CNRETENC.
           OPEN I-O CNRETENC.
       F-ABRE-ARCHIVOS.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       ARMA EL TEMPORAL DE LOS ACUMULADOS                ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-ARMA-TEMPORAL.
               MOVE    ZEROS                TO  JJ.
           IF  W-LONG NOT  = 13
               MOVE    ZEROS                TO FIN-OK  JJ
               PERFORM I-LABEL-CNDOCTOS   THRU F-LABEL-CNDOCTOS
               OPEN    INPUT   CNDOCTOS
               PERFORM I-START-CNDOCTOS   THRU F-START-CNDOCTOS
               PERFORM I-PROCESO-CNDOCTOS THRU F-PROCESO-CNDOCTOS
                                               UNTIL FIN-OK = 1
               GO    F-ARMA-TEMPORAL
            ELSE
               MOVE   1   TO W-LONG.
       I-ARMA1.
               MOVE    ZEROS                TO FIN-OK
               PERFORM I-LABEL-CNDOCTOS   THRU F-LABEL-CNDOCTOS
               OPEN    INPUT   CNDOCTOS
               PERFORM I-START-CNDOCTOS   THRU F-START-CNDOCTOS
               PERFORM I-PROCESO-CNDOCTOS THRU F-PROCESO-CNDOCTOS
                                               UNTIL FIN-OK = 1
               ADD     1 TO  W-LONG.
               IF      W-LONG > 12 GO F-ARMA-TEMPORAL.
               CLOSE  CNDOCTOS.
               GO  I-ARMA1.
       F-ARMA-TEMPORAL.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-PROCESO-CNDOCTOS          Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-PROCESO-CNDOCTOS.
           IF FIN-OK  = 1           GO F-PROCESO-CNDOCTOS.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
           IF FIN-OK = 1            GO F-PROCESO-CNDOCTOS.
              MOVE W-LONG                 TO MES00-CNRETENC
                                             MES01-CNRETENC.
              MOVE RADICTIP-DOCTOS        TO RADICTIP-CNRETENC
              MOVE RADICDOC-DOCTOS        TO RADICDOC-CNRETENC
                                             RADICA01-CNRETENC
              MOVE CUENTA-DOCTOS          TO CUENTA00-CNRETENC
                                             CUENTA01-CNRETENC
                                             CUENTA02-CNRETENC.
              IF   CUENTA-DOCTOS = 2365050100
                   MOVE COSTO12-DOCTOS    TO NUMFRA-CNRETENC
              ELSE
                   MOVE NUMFRA-DOCTOS     TO NUMFRA-CNRETENC.
              MOVE ALTNIT-DOCTOS          TO NIT00-CNRETENC
                                             NIT01-CNRETENC
                                             NIT02-CNRETENC
              MOVE SIGNO-DOCTOS           TO SIGNO-CNRETENC.
              MOVE ALTEMP-DOCTOS          TO ALTEMP-CNRETENC.
              MOVE FECHOY-DOCTOS          TO FECHA-CONTAB-CNRETENC.
              MOVE VALOR-DOCTOS           TO VALOR-CNRETENC
              MOVE VRBASE-DOCTOS          TO VRBASE-CNRETENC
              MOVE FECDOC-DOCTOS          TO FECHA-DESDE-CNRETENC.
              ADD  1                      TO JJ
              MOVE JJ                     TO RADIC-AUTOMA-CNRETENC.
              WRITE   REG-CNRETENC.
       F-PROCESO-CNDOCTOS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     RUTINA   GENERACION DE LISTADO1 - ACUMULADO GLOBAL  ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-GENERA-LISTADO1.
           MOVE    ZEROS                  TO  FIN-OK CON-LIN
                                              WK-PAGINA.
           PERFORM I-START-CNRETENC-01  THRU F-START-CNRETENC-01.
           PERFORM I-PRIMER-REGISTRO    THRU F-PRIMER-REGISTRO
           PERFORM I-BUSCA-MES1         THRU F-BUSCA-MES1
                                        UNTIL FIN-OK = 1
           PERFORM I-TOTAL-CUENTA       THRU F-TOTAL-CUENTA.
       F-GENERA-LISTADO1.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-PRIMER-REGISTRO.          Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-PRIMER-REGISTRO.
           PERFORM I-ZEROS-VARIABLES THRU  F-ZEROS-VARIABLES.
           PERFORM I-LEE-CNRETENC    THRU  F-LEE-CNRETENC.
           MOVE    CUENTA00-CNRETENC   TO  W-NUMCTA-DOCTOS
           MOVE    MES00-CNRETENC      TO  W-ALTMES-DOCTOS
           MOVE    NIT00-CNRETENC      TO  W-NIT
           COMPUTE TB-CONSEC =  1900 + CON-ANO.
           MOVE    TB-CONSEC           TO I-ANO4 I-ANO5
           MOVE    CUENTA00-CNRETENC   TO CTAMAE.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC.
       F-PRIMER-REGISTRO.
      *
       I-BUSCA-MES1.
           IF FIN-OK = 1   GO F-BUSCA-MES1.
           IF CUENTA00-CNRETENC = W-NUMCTA-DOCTOS
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC
                     GO I-BUSCA-MES1
           ELSE
                    MOVE    NOMCTA              TO T-CON2
                    MOVE    "TOTAL CUENTA:"     TO T-CON1
                    PERFORM I-TOTAL-CUENTA    THRU F-TOTAL-CUENTA
                    MOVE    CUENTA00-CNRETENC   TO CTAMAE
                    PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
                    PERFORM I-ROMPE           THRU F-ROMPE
                    PERFORM I-FRA-NUEVO       THRU F-FRA-NUEVO
                    GO I-BUSCA-MES1.
       F-BUSCA-MES1.
      *
       I-FRA-NUEVO.
           MOVE CUENTA00-CNRETENC       TO  W-NUMCTA-DOCTOS
           MOVE NIT00-CNRETENC          TO  W-NIT
           MOVE MES00-CNRETENC          TO  W-ALTMES-DOCTOS
           MOVE NUMFRA-CNRETENC         TO  W-NUMFRA-DOCTOS
           PERFORM I-MUEVE-INFORME-1  THRU  F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNRETENC     THRU  F-LEE-CNRETENC.
       F-FRA-NUEVO.
      *
       I-MUEVE-INFORME-1.
               IF NIT00-CNRETENC > ZEROS
                 MOVE NIT00-CNRETENC TO CODIM
                 PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
                 IF SW-EOF = 1
                    MOVE "C.COSTO NO EXISTE" TO I-EMP
                 ELSE
                    MOVE NOMBM TO I-EMP.
                MOVE NUMFRA-CNRETENC          TO I-NUMFRA
                MOVE ALTEMP-CNRETENC          TO I-CODEMP
                MOVE NIT00-CNRETENC           TO I-NIT
                MOVE RADICTIP-CNRETENC        TO I-RADICTIP-DOCTOS
                MOVE RADICDOC-CNRETENC        TO I-RADICDOC-DOCTOS
                MOVE FECHA-CONTAB-CNRETENC    TO I-FECHOY-DOCTOS
                PERFORM I-EVALUE-VALOR      THRU F-EVALUE-VALOR.
                IF CON-LIN > 60
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                          TO CON-LIN.
       F-MUEVE-INFORME-1.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     RUTINA   GENERACION DE LISTADO2 - POR NIT ACUMULADO ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-GENERA-LISTADO2.
           MOVE    ZEROS                  TO  FIN-OK CON-LIN
                                              WK-PAGINA.
           MOVE  "POR  NIT"               TO I-TIPO I-TIPO5
           PERFORM I-START-CNRETENC-02  THRU F-START-CNRETENC-02.
           PERFORM I-PRIMER-REGIS2      THRU F-PRIMER-REGIS2
           PERFORM I-BUSCA-MES2         THRU F-BUSCA-MES2
                                             UNTIL FIN-OK = 1
           PERFORM I-TOTAL-NIT          THRU F-TOTAL-NIT
           PERFORM I-TOTAL-CUENTA       THRU F-TOTAL-CUENTA.
       F-GENERA-LISTADO2.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-BUSCA-MES2                Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-PRIMER-REGIS2.
           PERFORM I-ZEROS-VARIABLES THRU  F-ZEROS-VARIABLES.
           PERFORM I-LEE-CNRETENC    THRU  F-LEE-CNRETENC.
           MOVE    CUENTA00-CNRETENC   TO  W-NUMCTA-DOCTOS
           MOVE    MES00-CNRETENC      TO  W-ALTMES-DOCTOS
           MOVE    NIT00-CNRETENC      TO  W-NIT
           COMPUTE TB-CONSEC =  1900 + CON-ANO.
           MOVE    TB-CONSEC           TO I-ANO4 I-ANO5
           MOVE    CUENTA00-CNRETENC   TO CTAMAE.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-MVTO-INFORME-1  THRU F-MVTO-INFORME-1.
           PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC.
       F-PRIMER-REGIS2.
      *
       I-BUSCA-MES2.
           IF FIN-OK = 1   GO F-BUSCA-MES2.
           IF CUENTA00-CNRETENC =  W-NUMCTA-DOCTOS
              IF W-NIT          =  NIT00-CNRETENC
                     PERFORM I-MVTO-INFORME-1  THRU F-MVTO-INFORME-1
                     PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC
                     GO I-BUSCA-MES2
              ELSE
                 PERFORM I-TOTAL-NIT       THRU F-TOTAL-NIT
                 MOVE    NIT00-CNRETENC      TO  W-NIT
                 PERFORM I-MVTO-INFORME-1  THRU F-MVTO-INFORME-1
                 PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC
                 GO I-BUSCA-MES2
           ELSE
                    PERFORM I-TOTAL-NIT       THRU F-TOTAL-NIT
                    MOVE    NIT00-CNRETENC      TO  W-NIT
                    MOVE    NOMCTA              TO T-CON2
                    MOVE    "TOTAL CUENTA:"     TO T-CON1
                    PERFORM I-TOTAL-CUENTA    THRU F-TOTAL-CUENTA
                    MOVE    CUENTA00-CNRETENC   TO CTAMAE
                    PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
                    PERFORM I-ROMPE           THRU F-ROMPE
                    PERFORM I-FRA-NUEVO2      THRU F-FRA-NUEVO2
                    GO I-BUSCA-MES2.
       F-BUSCA-MES2.
      *
       I-FRA-NUEVO2.
           MOVE CUENTA00-CNRETENC       TO  W-NUMCTA-DOCTOS
           MOVE NIT00-CNRETENC          TO  W-NIT
           MOVE MES00-CNRETENC          TO  W-ALTMES-DOCTOS
           MOVE NUMFRA-CNRETENC         TO  W-NUMFRA-DOCTOS
           PERFORM I-MVTO-INFORME-1   THRU  F-MVTO-INFORME-1
           PERFORM I-LEE-CNRETENC     THRU  F-LEE-CNRETENC.
       F-FRA-NUEVO2.
      *
       I-MVTO-INFORME-1.
               IF NIT00-CNRETENC > ZEROS
                 MOVE NIT00-CNRETENC TO CODIM
                 PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
                 IF SW-EOF = 1
                    MOVE "C.COSTO NO EXISTE" TO I-EMP
                 ELSE
                    MOVE NOMBM TO I-EMP.
                MOVE NUMFRA-CNRETENC          TO I-NUMFRA
                MOVE ALTEMP-CNRETENC          TO I-CODEMP
                MOVE NIT00-CNRETENC           TO I-NIT
                MOVE RADICTIP-CNRETENC        TO I-RADICTIP-DOCTOS
                MOVE RADICDOC-CNRETENC        TO I-RADICDOC-DOCTOS
                MOVE FECHA-CONTAB-CNRETENC    TO I-FECHOY-DOCTOS
      *NJ
                MOVE ZEROS                    TO I-NUMFRA
                MOVE ZEROS                    TO I-CODEMP
                MOVE NIT00-CNRETENC           TO I-NIT
                MOVE ZEROS                    TO I-RADICTIP-DOCTOS
                MOVE ZEROS                    TO I-RADICDOC-DOCTOS
                MOVE ZEROS                    TO I-FECHOY-DOCTOS
                PERFORM I-EVALUE-VALOR      THRU F-EVALUE-VALOR.
                MOVE    1   TO MM.
       F-MVTO-INFORME-1.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     RUTINA   GENERACION DE LISTADO3 - RESUMEN DE CUENTAS³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-GENERA-LISTADO3.
           MOVE    15        TO  MM.
           MOVE    ZEROS                  TO  FIN-OK CON-LIN
                                              WK-PAGINA.
           MOVE  "POR  CUENTA"            TO I-TIPO I-TIPO5
           PERFORM I-START-CNRETENC-02  THRU F-START-CNRETENC-02.
           PERFORM I-PRIMER-REGISTRO3   THRU F-PRIMER-REGISTRO3
           PERFORM I-BUSCA-MES3         THRU F-BUSCA-MES3
                                        UNTIL FIN-OK = 1
           PERFORM I-TOTAL-CTA3         THRU F-TOTAL-CTA3
           PERFORM I-TOTAL-GEN          THRU F-TOTAL-GEN.
       F-GENERA-LISTADO3.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-PRIMER-REGISTRO.          Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-PRIMER-REGISTRO3.
           PERFORM I-ZEROS-VARIABLES THRU  F-ZEROS-VARIABLES.
           PERFORM I-LEE-CNRETENC    THRU  F-LEE-CNRETENC.
           MOVE    CUENTA00-CNRETENC   TO  W-NUMCTA-DOCTOS
           COMPUTE TB-CONSEC =  1900 + CON-ANO.
           MOVE    TB-CONSEC           TO I-ANO4 I-ANO5
           MOVE    CUENTA00-CNRETENC   TO CTAMAE.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-EVALUE-VALOR    THRU F-EVALUE-VALOR.
           PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC.
       F-PRIMER-REGISTRO3.
      *
       I-BUSCA-MES3.
           IF FIN-OK = 1   GO F-BUSCA-MES3.
           IF CUENTA00-CNRETENC = W-NUMCTA-DOCTOS
                     PERFORM I-EVALUE-VALOR    THRU F-EVALUE-VALOR
                     PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC
                     GO I-BUSCA-MES3
           ELSE
                    MOVE    NOMCTA              TO T-CON2
                    MOVE    "TOTAL CUENTA:"     TO T-CON1
                    PERFORM I-TOTAL-CTA3      THRU F-TOTAL-CTA3
                    MOVE    CUENTA00-CNRETENC   TO CTAMAE
                    PERFORM I-EVALUE-VALOR    THRU F-EVALUE-VALOR
                    PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
                    MOVE CUENTA00-CNRETENC      TO W-NUMCTA-DOCTOS
                    PERFORM I-LEE-CNRETENC    THRU F-LEE-CNRETENC
                    GO I-BUSCA-MES3.
       F-BUSCA-MES3.
      *  .
       I-TOTAL-CTA3.
                MOVE T-TOTAL-DEBITOS        TO Q-VDEB.
                MOVE T-TOTAL-CREDITOS       TO Q-VCRE.
                MOVE T-TOTAL-BASE           TO Q-BASE.
                PERFORM I-IMPRIME-PANTALLA  THRU F-IMPRIME-PANTALLA.
                IF CON-LIN > 60
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-CUENTA AFTER 1
                ADD 1              TO CON-LIN
                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                T-TOTAL-BASE.
       F-TOTAL-CTA3.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       RUTINAS COMPARTIDAS POR LOS DOS LISTADOS          ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-ROMPE.
             IF CON-LIN > 55
                PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
             ELSE
                 WRITE REG-INFORMES  FROM  TITULO-6  AFTER 1
                 
                 MOVE     SPACES             TO REG-INFORMES
                 WRITE REG-INFORMES             AFTER 1
                 ADD  2                      TO CON-LIN.
       F-ROMPE.
      *
       I-EVALUE-VALOR.
            IF VALOR-CNRETENC IS POSITIVE
               MOVE VALOR-CNRETENC  TO I-VDEB
               MOVE 0               TO I-VCRE
               ADD VALOR-CNRETENC   TO W-TOTAL-DEBITOS
               ADD VALOR-CNRETENC   TO S-TOTAL-DEBITOS
               ADD VALOR-CNRETENC   TO G-TOTAL-DEBITOS
               ADD VALOR-CNRETENC   TO T-TOTAL-DEBITOS.

            IF VALOR-CNRETENC IS NEGATIVE
               MOVE 0               TO I-VDEB
               MOVE VALOR-CNRETENC  TO I-VCRE
               ADD VALOR-CNRETENC   TO W-TOTAL-CREDITOS
               ADD VALOR-CNRETENC   TO G-TOTAL-CREDITOS
               ADD VALOR-CNRETENC   TO S-TOTAL-CREDITOS
               ADD VALOR-CNRETENC   TO T-TOTAL-CREDITOS.

               MOVE 0                TO I-BASE
               MOVE VRBASE-CNRETENC  TO I-BASE.
               ADD  VRBASE-CNRETENC  TO W-TOTAL-BASE
               ADD  VRBASE-CNRETENC  TO S-TOTAL-BASE
               ADD  VRBASE-CNRETENC  TO G-TOTAL-BASE
               ADD  VRBASE-CNRETENC  TO T-TOTAL-BASE.
       F-EVALUE-VALOR.
      *
       I-ZEROS-VARIABLES.
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         S-TOTAL-BASE    T-TOTAL-BASE
                         W-TOTAL-BASE
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         G-TOTAL-BASE.
       F-ZEROS-VARIABLES.
      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
      *OK
       I-TOTAL-NIT.
                IF MM = ZEROS
                   PERFORM I-MVTO-INFORME-1  THRU F-MVTO-INFORME-1.
                MOVE  ZEROS TO MM.
                MOVE S-TOTAL-DEBITOS        TO I-VDEB.
                MOVE S-TOTAL-CREDITOS       TO I-VCRE.
                MOVE S-TOTAL-BASE           TO I-BASE.
                IF CON-LIN > 60
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                       TO CON-LIN.
                MOVE  ZEROS                 TO S-TOTAL-DEBITOS
                MOVE  ZEROS                 TO S-TOTAL-CREDITOS
                MOVE  ZEROS                 TO S-TOTAL-BASE.
       F-TOTAL-NIT.
      *  .
       I-TOTAL-CUENTA.
                MOVE    NOMCTA              TO T-CON2
                MOVE    "TOTAL CUENTA:"     TO T-CON1
                MOVE T-TOTAL-DEBITOS        TO T-VDEB.
                MOVE T-TOTAL-CREDITOS       TO T-VCRE.
                MOVE T-TOTAL-BASE           TO T-BASE.
                PERFORM I-IMPRIME-PANTALLA  THRU F-IMPRIME-PANTALLA.
                IF CON-LIN > 60
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                T-TOTAL-BASE.
       F-TOTAL-CUENTA.
      *
       I-TOTAL-GEN.
                MOVE "TOTAL GENERAL   "     TO T-CON
                MOVE  SPACES                TO I-NOMCTA.
                MOVE G-TOTAL-DEBITOS        TO T-VDEB.
                MOVE G-TOTAL-BASE           TO T-BASE.
                MOVE G-TOTAL-CREDITOS       TO T-VCRE.
                MOVE  1     TO KK.
                PERFORM I-IMPRIME-PANTALLA  THRU F-IMPRIME-PANTALLA.
                IF CON-LIN > 60
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-LIMPIE-PANTALLA THRU F-LIMPIE-PANTALLA.
                PERFORM I-ZEROS-VARIABLES THRU F-ZEROS-VARIABLES.
       F-TOTAL-GEN.
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
       I-LIMPIE-PANTALLA.
               MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY  " " LINE   24  POSITION   80 BEEP
                          CONTROL SG-CONTROL-STRING HIGH.
                DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
                DISPLAY WK-ESPACIOS            LINE 25 POSITION 01.
       F-LIMPIE-PANTALLA.

       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
               
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 2
               
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 2
               WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A1 AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               IF MM NOT = 15
               WRITE REG-INFORMES  FROM  TITULO-6  AFTER 1.
               
               MOVE     SPACES            TO REG-INFORMES
               WRITE REG-INFORMES                  AFTER 1
               MOVE 10                         TO  CON-LIN.
       F-TITULO-INFORME.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     LECTURA Y START DE ARCHIVOS                         ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-START-CNDOCTOS.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE ZEROS                  TO  ALT01-DOCTOS FIN-OK
               MOVE WK-CUENTA-CONTABLE     TO  CUENTA-DOCTOS.
               START CNDOCTOS  KEY NOT LESS ALT01-DOCTOS INVALID KEY
                     MOVE 1         TO  FIN-OK.
       F-START-CNDOCTOS.
      *
       I-START-CNRETENC-01.
               MOVE ZEROS                  TO  KEY01-CNRETENC FIN-OK
               START CNRETENC  KEY NOT LESS KEY01-CNRETENC INVALID KEY
                     MOVE 1         TO  FIN-OK.
       F-START-CNRETENC-01.
      *
       I-START-CNRETENC-02.
               MOVE ZEROS                  TO  KEY02-CNRETENC FIN-OK
               START CNRETENC  KEY NOT LESS KEY02-CNRETENC INVALID KEY
                     MOVE 1         TO  FIN-OK.
       F-START-CNRETENC-02.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNDOCTOS.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "
                      LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
               DISPLAY RADIC-DOCTOS           LINE 24 POSITION 21
               DISPLAY ALTTIP-DOCTOS          LINE 24 POSITION 47
               DISPLAY ALTMES-DOCTOS          LINE 24 POSITION 56
               DISPLAY ALTCTA-DOCTOS          LINE 24 POSITION 65
               READ CNDOCTOS NEXT RECORD WITH NO LOCK AT
                                  END MOVE 1 TO FIN-OK.
               IF  MAYCTA-DOCTOS  >  WK-MAYOR
                   MOVE 1  TO FIN-OK.
               IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
               MOVE    CUENTA-DOCTOS    TO CTAMAE.
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
      *NJ      IF  INDBASE = "N"        GO I-LEE-CNDOCTOS.
       F-LEE-CNDOCTOS.
      *
       I-LEE-CNRETENC.
               MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY " "
                      LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
               DISPLAY RADICDOC-CNRETENC       LINE 24 POSITION 21
               DISPLAY RADICTIP-CNRETENC       LINE 24 POSITION 47
               DISPLAY MES00-CNRETENC          LINE 24 POSITION 56
               DISPLAY CUENTA00-CNRETENC       LINE 24 POSITION 65
               READ CNRETENC NEXT RECORD WITH NO LOCK AT
                                  END MOVE 1 TO FIN-OK.
       F-LEE-CNRETENC.
      *
       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE MAYOR01-CNRETENC       TO I-MAYOR  Q-MAYCTA
               MOVE SUBCTA01-CNRETENC      TO I-SUBCTA Q-SUBCTA
               MOVE AUXCTA01-CNRETENC      TO I-AUXCTA Q-AUXCTA
               MOVE    NOMCTA              TO I-NOMCTA Q-NOMCTA.
       F-LEE-CNCATCTA.
      *
       I-LEE-CNNITCED.
               MOVE 0        TO SW-EOF.
               READ CNNITCED WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA   TO    I-EMP     W-NOMBRE.
       F-LEE-CNNITCED.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     PANTALLAZOS  Y CAPTURA DE INFORMACION               ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PANTALLA-1B.
           MOVE "** SOPORTE PARA RETENCION EN LA FUENTE** "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
                                               LINE 10 POSITION 20
               DISPLAY "º  TECLEE EL MES A REPORTAR :                º"
                                               LINE 11 POSITION 20
               DISPLAY "º  CUENTA CONTABLE :                         º"
                                               LINE 12 POSITION 20
               DISPLAY "º   [F2] TERMINAR                            º"
                                               LINE 13 POSITION 20
               DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-AYUDA-MES.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " "  LINE 20 POSITION 12  CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»" LINE 17 POSITION 01
                                CONTROL SG-CONTROL-STRING
           "º MES    : Para el acumulado de todo el a¤o, digite......
      -    "    [  13   ]      º" LINE 18 POSITION 01
           "º MES    : Para el acumulado de un solo mes, digite el mes
      -    "    [ 1..12 ]      º" LINE 19 POSITION 01
           "º
      -    "                   º" LINE 20 POSITION 01
           "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" LINE 21 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-AYUDA-MES.
      *
       I-AYUDA-TECLA.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 20 POSITION 12  CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»" LINE 17 POSITION 01
                                CONTROL SG-CONTROL-STRING
           "º  [F4]  : listado general:  No acumula Nit.
      -    "                   º" LINE 18 POSITION 01
           "º  [F5]  : listado acumulado por NIT O C‚dula.
      -    "                   º" LINE 19 POSITION 01
           "º  [F6]  : Los dos anteriores simultaneamente.
      -    "                   º" LINE 20 POSITION 01
           "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" LINE 21 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-AYUDA-TECLA.
      *
       I-IMPRIME-PANTALLA.
               MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY " "
                      LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
             ADD  1  TO  I.
             DISPLAY  W-NUMCTA-DOCTOS LINE  I   POSITION 10.
             IF KK  =   1
                DISPLAY  T-CON        LINE  I   POSITION 10 SIZE 18.
             DISPLAY  T-VDEB          LINE  I   POSITION 38.
             DISPLAY  T-VCRE          LINE  I   POSITION 58.
             IF  I > 17
                PERFORM I-DISPLAY      THRU F-DISPLAY
                MOVE  7   TO I.
       F-IMPRIME-PANTALLA.
     *
       I-ACEPTA-MES.
               PERFORM I-AYUDA-MES      THRU  F-AYUDA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 50
                                                    REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " CODIGO PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
           IF W-LONG NOT = 13
              MOVE WK-NOMBRE-MES(W-LONG)           TO I-MES4 I-MES5
           ELSE
              MOVE "ACUMULADO ANUAL"               TO I-MES4 I-MES5.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT WK-MAYOR                      LINE 12 POSITION 50
                                                    REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               move zeros to WK-SUBCTA WK-AUXCTA.
               PERFORM I-AYUDA-TECLA    THRU  F-AYUDA-TECLA.
       F-ACEPTA-MES.
      *
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
               DISPLAY
                    " PRESS TECLA DE FUNCION CONFIRMAR   "
                                           LINE 24 POSITION 1
                                           REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4
               MOVE   1      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  F5
               MOVE   2      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  F6
               MOVE   3      TO LL
               GO TO F-CONFIRMA-COMP.
           GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.
      *
       I-FIN-MODE.
           CLOSE CNNITCED
                 EMPRESAS
                 CNCATCTA
                 CNRETENC
                 INFORMES
                 CNDOCTOS.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\I-DISPLAY.PRO".

               COPY "..\PLB\CNRETENC.PLB".
               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\INFORMES.PLB".

