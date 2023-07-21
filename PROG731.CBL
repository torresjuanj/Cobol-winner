       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG731.
      ******************************************************************
      *      Traspaso del CNDTACUM a un cndt-ano-14                    *
      *               para el a¤o anterior  ABR-27-2002                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\CNDTACUM.DYC".
               COPY "..\DYC\CNDTACNU.DYC".
               COPY "..\DYC\CNCONTRO.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\CNDOCTOS.DYC".
               COPY "..\SEL\REPORTES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNDTACNU.FD".
               COPY "..\FD\REPORTES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA                PIC X(7)   VALUE "PROG731".
       77      ID-PRG                    PIC X(7)   VALUE "PROG731".
       77      W-CHEQUE                  PIC 9(12)      VALUE ZEROS.
       77      W-EMPRE                   PIC 9(07)      VALUE ZEROS.
       77      WK-EMPRE-INICIAL          PIC 9(07)      VALUE ZEROS.
       77      ANTICIPO                  PIC 9          VALUE ZEROS.
       77      CTA                       PIC 9          VALUE ZEROS.
       77      KK                        PIC 9          VALUE ZEROS.
       77      JJ                        PIC 9          VALUE ZEROS.
       77      AA                        PIC 99         VALUE ZEROS.
       77      ANTERIOR-1                PIC 9(12)V99   VALUE ZEROS.
       77      ANTERIOR-2                PIC 9(12)V99   VALUE ZEROS.
       77      WK-RADICA                 PIC 9(6)       VALUE ZEROS.
       77      NUM-SOPORTE               PIC 9(6)       VALUE ZEROS.
       77      WK-CONTROL                PIC 9(6)       VALUE ZEROS.
       77      WK-CUENTAUX               PIC 9(10)       VALUE ZEROS.
       01      WK-CUENTA1.
               03  WK-MAYOR              PIC 9(05)      VALUE ZEROS.
               03  WK-SUBCTA             PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR           PIC 9(03)      VALUE ZEROS.
       01      SUMATORIAS.
               03   W-DEB-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-DEB-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-CRE-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-CRE-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-TOT-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-TOT-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-REG-CNDTACUM             PIC  9(10)  VALUE ZEROS.
               03   W-REG-CNDTACNU             PIC  9(10)  VALUE ZEROS.
               03   W-VARIABLES-NOMINA         PIC  X(30)  VALUE SPACES.
       01      WK-CUENTA-V.
               03  WK-MAYOR-V.
                   07 MAYOR1-V            PIC 9          VALUE ZEROS.
                   07 MAYOR2-V            PIC 9          VALUE ZEROS.
                   07 MAYOR3-V            PIC 9          VALUE ZEROS.
                   07 MAYOR4-V            PIC 9          VALUE ZEROS.
                   07 MAYOR5-V            PIC 9          VALUE ZEROS.
               03  WK-SUBCTA-V            PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR-V.
                   07 AUX1-V              PIC 9          VALUE ZEROS.
                   07 AUX2-V              PIC 99         VALUE ZEROS.

       01      WK-CUENTA-N.
               03  WK-MAYOR-N.
                   07 MAYOR1-N            PIC 9          VALUE ZEROS.
                   07 MAYOR2-N            PIC 9          VALUE ZEROS.
                   07 MAYOR3-N            PIC 9          VALUE ZEROS.
                   07 MAYOR4-N            PIC 9          VALUE ZEROS.
               03  WK-SUBCTA-N            PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR-N.
                   07 AUX1-N              PIC 99         VALUE ZEROS.
                   07 AUX2-N              PIC 99         VALUE ZEROS.

       01  NOMLISTADOS.
           03   LISTA               OCCURS  12 TIMES.
              05  INICIAL           PIC  X      VALUE SPACES.
              05  PROGRAMA          PIC  999    VALUE ZEROS.
              05  ANO               PIC  9(04)  VALUE ZEROS.
              05  MES               PIC  99     VALUE ZEROS.
              05  CIA               PIC  9(07)  VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTE1.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CNDTACNU.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\RUTMONTO.WRK".
               COPY "..\WRK\LISTADOS.WRK".
       01      AREA-INFORME.
         03     AREA-WORKING-INFORME.
         05    I-CODIGO-EMPRESA          PIC 9(07)      VALUE ZEROS.
      **** LINEA 1
        03     TITULO-0.
         05    FILLER                    PIC 9(4)  USAGE IS COMP-1
                                        VALUE 6960.
      ******************************************************************
      *         DEFICINION DE LOS REPORTES                             *
      ******************************************************************
       01      AREA-REPORTE.
        03     AREA-WORKING-REPORTE.
         05    WR-PAGINA                 PIC 9(4)       VALUE ZEROS.
         05    WR-LINEAS                 PIC 9(4)       VALUE 88.
      *
        03     TITULO-R-0.
         05    TIT-CIA                   PIC X(40)      VALUE SPACES.
      *
        03     TITULO-R-1.
         05    FILLER                    PIC X(08)      VALUE
              "EMPRESA:".
         05    R-EMPRESA                 PIC X(20)      VALUE SPACES.
         05    FILLER                    PIC X(18)      VALUE SPACES.
         05    FILLER                    PIC X(22)      VALUE
              " ARCHIVO  CNDTACUM    ".
         05    FILLER                    PIC X(25)      VALUE SPACES.
         05    FILLER                    PIC X(10)       VALUE
              "PAGINA:  ".
         05    R-PAGINA                  PIC ZZZZ       VALUE ZEROS.
      *
        03     TITULO-R-2.
         05    FILLER                    PIC X(10)      VALUE
              "PERIODO:".
         05    R-PERIODO                 PIC X(13)      VALUE SPACES.
         05    FILLER                    PIC X(70)      VALUE SPACES.
         05    FILLER                    PIC X(08)      VALUE
                              "FECHA:  ".
         05    I-DIA-R                   PIC 99         VALUE ZEROS.
         05    FILLER                    PIC X          VALUE   "/".
         05    I-MES-R                   PIC 99         VALUE ZEROS.
         05    FILLER                    PIC X          VALUE   "/".
         05    I-ANO-R                   PIC 9(04)      VALUE ZEROS.
         05    FILLER                    PIC X(5)       VALUE SPACES.

        03     TITULO-R-8.
         05    FILLER                    PIC X(65)      VALUE SPACES.
         05    FILLER                    PIC X(28)      VALUE SPACES.
         05    FILLER                    PIC X(06)   VALUE "HORA: ".
         05    FILLER                    PIC X(5)       VALUE SPACES.
         05    I-HORAR                   PIC 99         VALUE ZEROS.
         05    FILLER                    PIC X          VALUE   ":".
         05    I-MINUR                   PIC 99         VALUE ZEROS.
      *
        03     TITULO-GEN.
         05    FILLER                    PIC X(09)      VALUE
              "    KEY00".
         05    FILLER                    PIC X(08)      VALUE SPACES.
         05    FILLER                    PIC X(33)      VALUE
              "    CTA.          NIT.     NUMFRA".

         05    FILLER                    PIC X(06)    VALUE SPACES.
         05    FILLER                    PIC X(14)      VALUE
              "ALTFRA   CTA. ".
         05    FILLER                    PIC X(03)      VALUE SPACES.

         05    FILLER                    PIC X(08)      VALUE
              "ALTPER4 ".
         05    FILLER                    PIC X(01)      VALUE SPACES.

         05    FILLER                    PIC X(18)      VALUE
              "FECDOC        CTA.".
         05    FILLER                    PIC X(03)      VALUE SPACES.
         05    FILLER                    PIC X(07)      VALUE
              "TIP/MES".
         05    FILLER                    PIC X(05)      VALUE SPACES.
         05   FILLER                     PIC X(06)   VALUE
              "FEC.CO".
         05    FILLER                    PIC X(01)      VALUE SPACES.
         05    FILLER                    PIC X(06)      VALUE
              "ALTO 8".
         05    FILLER                    PIC X(07)      VALUE SPACES.
         05    FILLER                    PIC X(06)      VALUE
              "ALTO 9".
         05    FILLER                    PIC X(20)      VALUE SPACES.
         05    FILLER                    PIC X(08)      VALUE
              "CONCEPTO".
         05    FILLER                    PIC X(14)      VALUE SPACES.
         05    FILLER                    PIC X(05)      VALUE
              "VALOR".
         05    FILLER                    PIC X(05)      VALUE SPACES.
         05    FILLER                    PIC X(09)      VALUE
              " VR.BASE ".
         05    FILLER                    PIC X(04)      VALUE SPACES.
         05   FILLER                     PIC X(05)   VALUE
              "SIGNO".
         05    FILLER                    PIC X(01)      VALUE SPACES.
         05    FILLER                    PIC X(05)      VALUE
              "COND.".
         05    FILLER                    PIC X(01)      VALUE SPACES.
         05    FILLER                    PIC X(11)      VALUE
              "SOPORTE    ".
      *
        03     TITULO-R-5.
         05    FILLER                    PIC X(135)     VALUE ALL "=".
      *
        03     LINEA-DETALLE.
         05    FILLER          PIC X(1)       VALUE SPACES.
         05    R-MAYOR         PIC ZZZZ       VALUE SPACES.
         05    FILLER          PIC X(1)       VALUE SPACES.
         05    R-SUB           PIC ZZ         VALUE SPACES.
         05    FILLER          PIC X(1)       VALUE SPACES.
         05    R-AUX           PIC ZZZZ       VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-NIT           PIC ZZZ,ZZZ,ZZZ  VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-VALOR         PIC ZZZ,ZZZ,ZZZ VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-NIVEL         PIC Z           VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-ESTACTA       PIC Z           VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-INDDETAL      PIC Z           VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-CODPGT        PIC Z           VALUE SPACES.
         05    FILLER          PIC X(02)      VALUE SPACES.
         05    R-CONCEPTO      PIC X(30)       VALUE SPACES.
      *
       03     AREA-CONCEPTO-MVDOCTOS.
         05    C-DATO-MVDOCTOS             PIC X(20)    VALUE SPACES.
         05    FILLER                    PIC X(06)    VALUE " CHQ.#".
         05    C-CHEQ-DESDE              PIC X(06)    VALUE ZEROS.
         05    FILLER                    PIC X(4)     VALUE
              " AL ".
         05    C-CHEQ-HASTA             PIC X(06)      VALUE ZEROS.
      *
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNDTACNU.DCL".
               COPY "..\DCL\REPORTES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           MOVE 0 TO SW1 SW1 SW-EOF FIN-OK  WK-PAGINA
                          W-CONTA CON-LIN JJ.
           PERFORM I-PROCESO-AUDITE    THRU  F-PROCESO-AUDITE
           PERFORM I-DISPLAY           THRU  F-DISPLAY.
           PERFORM I-ACEPTA-MES        THRU  F-ACEPTA-MES.
           PERFORM I-LABEL-ARCHIVOS    THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS     THRU  F-ABRE-ARCHIVOS
           PERFORM I-START-CNDTACNU    THRU  F-START-CNDTACNU
           PERFORM I-INICIE-INFORME    THRU  F-INICIE-INFORME.
           PERFORM I-PANTALLA          THRU  F-PANTALLA
           PERFORM I-INFORME-GENERAL   THRU  F-INFORME-GENERAL.
           PERFORM I-FIN-MODE          THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE  1                       TO IND-REPORTES.
               MOVE WEMPRESA                 TO TIT-CIA
               MOVE W-IDENT-PROGRAMA         TO CON-PROGRAMA
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
               COMPUTE LAB-CNDOCTOS-MES  =  W-LONG + 1.
               COMPUTE LAB-CNDOCTOS-ANO  =  CON-ANO + 1.
               PERFORM I-LABEL-CNDTACNU THRU F-LABEL-CNDTACNU.
               PERFORM I-LABEL-REPORTES    THRU F-LABEL-REPORTES.
               ACCEPT  WK-FECHA-HOY            FROM DATE.
               ACCEPT  WK-HORA-HOY             FROM  TIME.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
               PERFORM  I-DISPLAY-LABEL  THRU  F-DISPLAY-LABEL.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               PERFORM I-ABRE-CNCONTRO   THRU F-ABRE-CNCONTRO.
               OPEN INPUT  CNCATCTA
                           CNDTACNU.
               OPEN  I-O   EMPRESAS.
               OPEN  OUTPUT  CNDOCTOS CLOSE CNDOCTOS.
               OPEN  OUTPUT REPORTES.
               OPEN  I-O  CNDOCTOS.
       F-ABRE-ARCHIVOS.
      *
       I-FIN-MODE.
               CLOSE CNCATCTA
                     EMPRESAS
                     CNDOCTOS
                     CNDTACNU
                     CNCONTRO
                     REPORTES.
       F-FIN.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.
      *
       I-ACTUAL.
               CANCEL "PROG731.COB"
               MOVE    14    TO CON-PERIODO.
               CALL   "PROG715.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG715.COB".
               CALL   "PROG781.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG781.COB".
       F-ACTUAL.
      *
       I-BORRA-MESES.
               CANCEL "PROG731.COB"
               MOVE    J   TO CON-PERIODO.
               CALL   "PROG715.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG715.COB".
               CALL   "PROG781.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG781.COB".
       F-BORRA-MESES.
      *
       I-INICIE-INFORME.
               MOVE WIN-COD-EMPRESA        TO  R-EMPRESA
               MOVE CON-PERIODO            TO  R-PERIODO
               MOVE 88                     TO  WR-LINEAS
               MOVE ZEROS                  TO  WR-PAGINA
               MOVE ZEROS                  TO  W-TOTAL.
               MOVE ZEROS                  TO  WK-PAGINA.
       F-INICIE-INFORME.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       RUTINA  PRINCIPAL   DEL PROCESO DE CONVERSION     ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

       I-INFORME-GENERAL.
               PERFORM I-START-CNDTACNU    THRU  F-START-CNDTACNU
               MOVE  ZEROS                   TO    SW-EOF KK.
               PERFORM I-PROCESA-C14       THRU  F-PROCESA-C14
                        UNTIL  SW-EOF  = 1.
       F-INFORME-GENERAL.
      *
       I-DETALLE.
               MOVE CNDTACUM-MAYOR          TO  R-MAYOR
               MOVE CNDTACUM-SUB            TO  R-SUB
               MOVE CNDTACUM-AUX            TO  R-AUX
               MOVE CNDTACUM-NIT            TO  R-NIT
               MOVE CNDTACUM-SALDO(W-LONG)  TO  R-VALOR
               MOVE NIVEL                   TO  R-NIVEL
               MOVE INDDETAL                TO  R-INDDETAL
               MOVE ESTACTA                 TO  R-ESTACTA
               MOVE CODPGT                  TO  R-CODPGT
               WRITE REG-REPORTES        FROM  LINEA-DETALLE   AFTER  1.
       F-DETALLE.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     Proceso principal de depuracion y conversion        ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PROCESA-C14.
               MOVE ZEROS          TO   CNDTACUM-SALDO(W-LONG).
               PERFORM I-SUMA             THRU F-SUMA.
               MOVE   ZEROS               TO AA.
               PERFORM I-LECTURA-SEC      THRU   F-LECTURA-SEC.
               IF SW-EOF =  1  GO F-PROCESA-C14.

             IF  CNDTACNU-MOVDEB(W-LONG) = 0 AND
                 CNDTACNU-MOVCRE(W-LONG) = 0 AND
                 CNDTACNU-SALDO (W-LONG) = 0 GO I-PROCESA-C14.

             IF CNDTACNU-SALDO(W-LONG)  =  0  GO I-PROCESA-C14.

               PERFORM I-MVTO-CAMPOS     THRU F-MVTO-CAMPOS.
               MOVE CNDTACNU-MAYOR         TO CNDTACUM-MAYOR  MAYMAE
               MOVE CNDTACNU-SUB           TO CNDTACUM-SUB    SUBMAE
               MOVE CNDTACNU-AUX           TO CNDTACUM-AUX    AUXMAE

               PERFORM I-LEE-CNCATCTA      THRU F-LEE-CNCATCTA.
            IF FIN-OK = 1
              MOVE "CUENTA INEXISTENTE EN PUC  " TO  R-CONCEPTO
              PERFORM I-DETALLE                THRU  F-DETALLE
              MOVE    ZEROS                      TO  FIN-OK.

            IF CNDTACUM-SALDO(W-LONG) IS POSITIVE
               MOVE 1                       TO SIGNO-DOCTOS
               MOVE CNDTACUM-SALDO(W-LONG)  TO VALOR-DOCTOS
                                               ANTERIOR-2
               PERFORM I-MVTO-DOCTOS      THRU F-MVTO-DOCTOS.

            IF CNDTACUM-SALDO(W-LONG) IS NEGATIVE
               MOVE 2                       TO SIGNO-DOCTOS
               MOVE CNDTACUM-SALDO(W-LONG)  TO VALOR-DOCTOS
                                               ANTERIOR-2
               PERFORM I-MVTO-DOCTOS      THRU F-MVTO-DOCTOS.
           GO I-PROCESA-C14.
       F-PROCESA-C14.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Rutinas de validacion de campos y cuentas         ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

      *
       I-SUMA.
            ADD    1     TO  W-REG-CNDTACUM.
            PERFORM I-MOSTRAR-CAMPOS   THRU F-MOSTRAR-CAMPOS.
       F-SUMA.
      *
       I-MVTO-CAMPOS.
            MOVE CNDTACNU-MAYOR          TO CNDTACUM-MAYOR
            MOVE CNDTACNU-SUB            TO CNDTACUM-SUB
            MOVE CNDTACNU-AUX            TO CNDTACUM-AUX
            MOVE CNDTACNU-NIT            TO CNDTACUM-NIT
            MOVE CNDTACNU-FRA            TO CNDTACUM-FRA
            MOVE CNDTACNU-ALTER-1        TO CNDTACUM-ALTER-1
            MOVE CNDTACNU-NIT-ALT1       TO CNDTACUM-NIT-ALT1
            MOVE CNDTACNU-FRA-ALT1       TO CNDTACUM-FRA-ALT1
            MOVE CNDTACNU-ALTER-2        TO CNDTACUM-ALTER-2
            MOVE CNDTACNU-NOM-ALT2       TO CNDTACUM-NOM-ALT2
            MOVE CNDTACNU-FECHA-FRA      TO CNDTACUM-FECHA-FRA
            MOVE CNDTACNU-FECHA-VCTO     TO CNDTACUM-FECHA-VCTO.
            MOVE CNDTACNU-MOVDEB(W-LONG) TO CNDTACUM-MOVDEB(W-LONG)
            MOVE CNDTACNU-MOVCRE(W-LONG) TO CNDTACUM-MOVCRE(W-LONG)
            MOVE CNDTACNU-SALDO (W-LONG) TO CNDTACUM-SALDO(W-LONG).
       F-MVTO-CAMPOS.
      *
       I-MVTO-DOCTOS.
            IF  ESTACTA = "S"
                GO F-MVTO-DOCTOS.

            IF  CODPGT  = "S"  AND INDDETAL = "S" AND
                CNDTACUM-FRA = ZEROS
                GO F-MVTO-DOCTOS.

           IF CNDTACUM-NIT =  0    AND CNDTACUM-FRA = 0
              AND CODPGT   = "S"
              GO F-MVTO-DOCTOS.

           IF CNDTACUM-NIT =  0    AND CNDTACUM-FRA = 0
              AND INDDETAL = "S"
              GO F-MVTO-DOCTOS.

           IF CNDTACUM-NIT >  0    AND CNDTACUM-FRA = 0
              AND INDDETAL = "S"
              GO F-MVTO-DOCTOS.

            IF  ANTERIOR-1 = ANTERIOR-2
                MOVE "VALORE IGUALES "   TO  R-CONCEPTO
                PERFORM I-DETALLE      THRU  F-DETALLE
            ELSE
                MOVE VALOR-DOCTOS        TO ANTERIOR-1.

            MOVE CNDTACUM-MAYOR    TO MAYCTA-DOCTOS
            MOVE CNDTACUM-SUB      TO SUBCTA-DOCTOS
            MOVE CNDTACUM-AUX      TO AUXCTA-DOCTOS
            MOVE CUENTA-DOCTOS     TO CUENTA-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT04CTA-DOCTOS
                                      ALTCTA1-DOCTOS
                                      ALT09CTA-DOCTOS
                                      ALT12CTA-DOCTOS.
            MOVE CUENTA-DOCTOS     TO ALTCTA1-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT09CTA-DOCTOS
            MOVE CNDTACUM-NIT      TO ALTNIT-DOCTOS
                                      NITAUX-DOCTOS
            MOVE CNDTACUM-FRA      TO ALTFRA-DOCTOS
                                      NUMFRA-DOCTOS
            MOVE CNDTACUM-NOM-ALT2 TO CONCEPTO-DOCTOS
            MOVE CNDTACUM-ANOFRA   TO AA-COMP-DOCTOS
            MOVE CNDTACUM-MESFRA   TO MM-COMP-DOCTOS
            MOVE CNDTACUM-DIAFRA   TO DD-COMP-DOCTOS
            MOVE CNDTACUM-ANOVCT   TO AA-VCTO-DOCTOS
            MOVE CNDTACUM-MESVCT   TO MM-VCTO-DOCTOS
            MOVE CNDTACUM-DIAVCT   TO DD-VCTO-DOCTOS.
            MOVE ZEROS             TO VRBASE-DOCTOS
                                      CONDICION-DOCTOS
            MOVE MESW              TO ALTPER-DOCTOS
                                      ALTMES-DOCTOS
            MOVE 15  TO  ALTTIP-DOCTOS    ALTTIP03-DOCTOS
                         ALT09TIP-DOCTOS  ALTTIP08-DOCTOS
                         RADICTIP-DOCTOS  ALTTIP04-DOCTOS
                         ALTTIP12-DOCTOS
            MOVE 01                TO  ALTEMP-DOCTOS
            MOVE 1                 TO  RADICDOC-DOCTOS
            MOVE 1                 TO  ALTFRA-DOCTOS
                                       RADICDOC-DOCTOS
                                       RADICA04-DOCTOS
                                       RADICA02-DOCTOS
                                       COSTO12-DOCTOS
                                       RADICA05-DOCTOS
                                       ALTFRA09-DOCTOS.

            ADD  1                 TO  W-CONTA
            MOVE W-CONTA           TO  RADIC-AUTOMATICA-DOCTOS.
            MOVE ZEROS             TO  BANCO-DOCTOS
                                       BANCO-BENEF-DOCTOS
                                       CHEQUE-BENEF-DOCTOS
                                       CONSIGNACION-DOCTOS.
            MOVE WK-FECHA-HOY      TO  FECHOY-DOCTOS.
            WRITE  REG-DOCTOS.
       F-MVTO-DOCTOS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Captura de datos por teclado                      ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

       I-ACEPTA-MES.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE   13    TO W-LONG.
           DISPLAY W-LONG   LINE 11 POSITION 50 REVERSE.
           IF  F2  OR  ESC  PERFORM F-FIN  THRU   F-FIN-MODE.
           IF W-LONG = 0 OR W-LONG > 14
              DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-ACEPTA-MES.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 45 REVERSE.
               IF W-LONG = 1
                 COMPUTE MESW = W-LONG + 12
               ELSE
                 COMPUTE MESW = W-LONG - 1.
                 COMPUTE MESW = 14.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE  <C> CONTINUAR "
                               LINE 24 POSITION 1 REVERSE
                    DISPLAY " [F2] TERMINAR "
                               LINE 24 POSITION 32 REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-WIN-CTAS.
               DISPLAY " "  LINE 24  POSITION 80.
       F-WIN-CTAS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Start y lectura de archivos                       ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-START-CNDTACNU.
               MOVE   ZEROS                TO  CNDTACNU-CLAVE.
               MOVE   ZEROS                to  CNDTACNU-MAYOR
               START CNDTACNU  KEY >   CNDTACNU-CLAVE INVALID KEY
                 DISPLAY "ERROR START-CNDTACNU"  LINE 23 POSITION 1.
       F-START-CNDTACNU.
      *
       I-LECTURA-SEC.
           IF  SW-EOF   =   1
                      GO  F-LECTURA-SEC.
           MOVE ZEROS         TO  SW-EOF.
           READ CNDTACNU NEXT  RECORD AT END
                     MOVE 1 TO SW-EOF.
           IF  CNDTACNU-MAYOR > 4500 MOVE 1 TO SW-EOF.
       F-LECTURA-SEC.
      *
       I-LEE-CNDTACUM.
           MOVE ZEROS          TO  SW-EOF.
           READ CNDTACUM NEXT  RECORD AT END
                     GO  F-LEE-CNDTACUM.
           ADD    1     TO  W-REG-CNDTACNU.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNDTACNU.
           MOVE ZEROS          TO  SW-EOF.
           READ CNDTACNU NEXT  RECORD AT END
                     GO  F-LEE-CNDTACNU.
           ADD    1     TO  W-REG-CNDTACNU.
       F-LEE-CNDTACNU.
      *
       I-LEE-CNCATCTA.
           MOVE ZEROS          TO  FIN-OK.
           READ CNCATCTA INVALID KEY
                MOVE   1    TO  FIN-OK
                GO  F-LEE-CNCATCTA.
       F-LEE-CNCATCTA.

      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Dise¤o de Pantalla y visualizacion datos          ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

         I-MOSTRAR-CAMPOS.
            DISPLAY CNDTACUM-MAYOR LINE 16 POSITION 31
            DISPLAY CNDTACUM-SUB   LINE 16 POSITION 36
            DISPLAY CNDTACUM-AUX   LINE 16 POSITION 40

            DISPLAY CNDTACNU-MAYOR LINE 16 POSITION 50
            DISPLAY CNDTACNU-SUB   LINE 16 POSITION 55
            DISPLAY CNDTACNU-AUX   LINE 16 POSITION 59

            MOVE    CNDTACUM-NIT   TO WI-Z9.
            DISPLAY WI-Z9          LINE 19 POSITION 30
            MOVE    CNDTACNU-NIT   TO WI-Z9.
            DISPLAY WI-Z9          LINE 19 POSITION 50

            DISPLAY CNDTACUM-FRA   LINE 20 POSITION 30

            MOVE    CNDTACUM-MOVDEB(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 17 POSITION 30
            MOVE    CNDTACUM-MOVCRE(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 18 POSITION 30

            MOVE    CNDTACUM-MOVDEB(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 17 POSITION 50
            MOVE    CNDTACUM-MOVCRE(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 18 POSITION 50.
       F-MOSTRAR-CAMPOS.
      *
       I-DISPLAY.
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.

               DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ»"
              LINE 09 POSITION 15 CONTROL SG-CONTROL-STRING
               DISPLAY "º POR PRECAUCION TENGA UNA COPIA DEL MOVIMIENTO
      -       "1-14  º"
                                              LINE 10 POSITION 15
               DISPLAY "º MES A PROCESAR   :
      -       "      º"
                                              LINE 11 POSITION 15

               DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ¼"
                                              LINE 12 POSITION 15
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "[ESC MENU]  [F2 SALIR] "
           LINE 25 POSITION 30 CONTROL SG-CONTROL-STRING HIGH REVERSE.
           DISPLAY "WINNER FINANCIAL"  LINE 3  POSITION 2
           DISPLAY
           "CONVERSION DE UN CNDTACNU EN UN CNDTACUM"
                     LINE    6  POSITION   25 REVERSE BEEP.
       RUTINA-ERROR-NOMINA.
       F-DISPLAY.
      *
       I-PANTALLA.
               DISPLAY
           "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
            LINE 14 POSITION 15 CONTROL SG-CONTROL-STRING HIGH
           "º                  CNDTACUM                  CNDTACNU     º"
                                     LINE 15 POSITION 15

           "º  CUENTA                                                 º"
                                     LINE 16 POSITION 15

           "º  DEBITOS                                                º"
                                     LINE 17 POSITION 15

           "º  CREDITOS                                               º"
                                     LINE 18 POSITION 15

           "º  NIT                                                    º"
                                     LINE 19 POSITION 15

           "º                                                         º"
                                     LINE 20 POSITION 15

           "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
                       LINE 21 POSITION 15.
       F-PANTALLA.
      *
       Q-STATUS-CNDTACUM.
                   OPEN OUTPUT CNDTACUM CLOSE CNDTACUM
                   OPEN I-O    CNDTACUM
                   OPEN INPUT  CNCATCTA
                   PERFORM I-ACTUALIZE-CTAS THRU F-ACTUALIZE-CTAS
                   CLOSE CNCATCTA
                   CLOSE CNDTACUM.
       Q1-STATUS-CNDTACUM.
      *
       I-LEE-CNCATCTAC.
           READ CNCATCTA NEXT AT END MOVE 1 TO RUT-OK.
       F-LEE-CNCATCTAC.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\DISLABER.PRO".
               COPY "..\STA\CNDTACUM.STA".

               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\CNDTACNU.PLB".
               COPY "..\PLB\REPORTES.PLB".
               COPY "..\PLB\CNDOCTOS.PLB".


