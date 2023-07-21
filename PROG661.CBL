       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG661.
      ******************************************************************
      *INFORME DE CONTROL DE CHEQUES   ABR-27-2002                     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CHCONTRO.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\NOMIBANK.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\SEL\REPORTES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CHCONTRO.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(07)      VALUE "PROG661".
       77      ID-PRG              PIC X(7)       VALUE "PROG661".
       77      WK-DEPTO            PIC X(1)       VALUE SPACES.
       77      WK-ESTADO           PIC 9(1)       VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(7)       VALUE  SPACES.

       01      VALORES.
        03     VALOR-CAJA       PIC 9(12) VALUE ZEROS.
        03     VALOR-ENVIADO    PIC 9(12) VALUE ZEROS.
        03     VALOR-ENTREGO    PIC 9(12) VALUE ZEROS.
        03     VALOR-ANULA      PIC 9(12) VALUE ZEROS.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CHCONTRO.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
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
      *  05    FILLER         PIC X(2)     VALUE "M".
      *  05    FILLER         PIC X(2)     VALUE "0".
      *  05    FILLER         PIC 9(4)     USAGE IS COMP-1 VALUE 6962.
      *  05    FILLER         PIC 9(4)     USAGE IS COMP-1 VALUE 3602.
         05    TIT-CIA        PIC X(40)    VALUE SPACES.

        03     TITULO-1.
      *  05    FILLER         PIC X(2)     VALUE "M".
      *  05    FILLER         PIC X(2)     VALUE "0".
      *  05    FILLER         PIC 9(04)    USAGE IS COMP-1   VALUE 4623.
         05    FILLER         PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA        PIC X(11).
         05    FILLER         PIC X(35)    VALUE SPACES.
         05    FILLER         PIC X(33)    VALUE
              "CONTROL  DE  CHEQUES  BANCO -->  ".
         05    I-BANCO1       PIC X(15).
         05    FILLER         PIC X(13)    VALUE SPACES.
         05    FILLER         PIC X(08)    VALUE "Hora  : ".
         05    I-HORA         PIC X(05).

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "BANCO:".
         05    I-BANCO     PIC X(107)   VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(04)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(40)    VALUE SPACES.

        03     TITULO-3.
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(181)   VALUE ALL "=".

        03     TITULO-4.
         05    FILLER      PIC X(58).
         05    FILLER      PIC X(06) VALUE "TOTAL ".
         05    T-CON.
           07  T-DPTO      PIC X(14).
           07  T-ESTADO    PIC X(20).
         05    T-VDEB      PIC ZZZZZZ,ZZZ,ZZ9.99-.

        03     TITULO-LS.
         05    FILLER      PIC X(95).
         05    FILLER      PIC X(20) VALUE ALL "-".

        03     TITULO-LD.
         05    FILLER      PIC X(95).
         05    FILLER      PIC X(20) VALUE ALL "=".

        03     TITULO-ROMPE.
      *  05    FILLER      PIC 9(04) USAGE IS COMP-1   VALUE 4626.
         05    FILLER      PIC X(06).
         05    FILLER      PIC X(14) VALUE "PROCEDENCIA:".
         05    EL-DPTO     PIC X(35) VALUE SPACES.
         05    FILLER      PIC X(10) VALUE "ESTADO :".
         05    EL-ESTADO   PIC X(30) VALUE SPACES.

        03     TITULO-A.
      *  05    FILLER      PIC 9(04) USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(09)    VALUE "      ".
         05    FILLER      PIC X(10)    VALUE "FECHA".
         05    FILLER      PIC X(14)    VALUE "COMP".
         05    FILLER      PIC X(35)    VALUE "CODIGO Y NOMBRE".
         05    FILLER      PIC X(18)    VALUE "NOMBRE".
         05    FILLER      PIC X(07)    VALUE "CHEQ.".
         05    FILLER      PIC X(10)    VALUE "CHEQ.".
         05    FILLER      PIC X(09)    VALUE "     ".
         05    FILLER      PIC X(03)    VALUE "SW".
         05    FILLER      PIC X(10)    VALUE "FECHA".
         05    FILLER      PIC X(28)    VALUE "NOMBRE".
         05    FILLER      PIC X(25)    VALUE "NOMBRE".
         05    FILLER      PIC X(01)    VALUE "|".

       03      TITULO-A1.
      *  05    FILLER      PIC 9(04) USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(09)    VALUE "CONSEC".
         05    FILLER      PIC X(08)    VALUE "ELABO".
         05    FILLER      PIC X(17)    VALUE "EGRESO".
         05    FILLER      PIC X(31)    VALUE "DE LA EMPRESA".
         05    FILLER      PIC X(21)    VALUE "BENEFICIARIO".
         05    FILLER      PIC X(07)    VALUE "DESDE".
         05    FILLER      PIC X(10)    VALUE "HASTA".
         05    FILLER      PIC X(09)    VALUE "VALOR".
         05    FILLER      PIC X(03)    VALUE "SW".
         05    FILLER      PIC X(10)    VALUE "ENTREGA".
         05    FILLER      PIC X(28)    VALUE "ENTREGO".
         05    FILLER      PIC X(25)    VALUE "RECIBIO".
         05    FILLER      PIC X(01)    VALUE "|".
      *

        03     LINEA-DE-DETALLE.
      *  05      FILLER       PIC 9(04) USAGE IS COMP-1   VALUE 4623.
         05      FILLER                 PIC X(01).
         05    I-CONSEC       PIC ZZZ,ZZZ.
         05      FILLER                 PIC X(02).
         05    I-ELABORO      PIC 9(08).
         05      FILLER                 PIC X(02).
         05    I-EGRESO       PIC ZZZ,ZZZ.
         05      FILLER                 PIC X(02).
         05    I-COD-EMPRESA  PIC 9(07).
         05      FILLER                 PIC X(01) VALUE "-".
         05    I-NOM-EMPRESA  PIC X(28).
         05      FILLER                 PIC X(02).
         05    I-BENEFICIA    PIC X(25).
         05      FILLER                 PIC X(02).
         05    I-DESDE        PIC 9(4).
         05      FILLER                 PIC X(03) VALUE " - ".
         05    I-HASTA        PIC 9(04).
         05      FILLER                 PIC X(02).
         05    I-VALOR        PIC ZZZ,ZZZ,ZZZ.
         05      FILLER                 PIC X(02).
         05    I-ESTADO       PIC X(01).
         05      FILLER                 PIC X(02).
         05    I-ENTREGA      PIC Z(6).
         05      FILLER                 PIC X(02).
         05    I-NOM-ENTREGO  PIC X(25).
         05      FILLER                 PIC X(02).
         05    I-NOM-RECIBIO  PIC X(25).
         05      FILLER                 PIC X(02).
         05    I-DPTO         PIC X(01).
         05      FILLER                 PIC X(02).
         05    I-LUGAR        PIC X(01).

      *RESUMEN
      *
        03  LINEA-0.
      *  05 FILLER         PIC 9(04) USAGE IS COMP-1   VALUE 4626.
         05 FILLER         PIC X(03) VALUE  SPACES.
         05 FILLER         PIC X(35) VALUE "PROCEDENCIA_CHEQUE".
         05 FILLER         PIC X(33) VALUE "UBICACION".
         05 FILLER         PIC X(10) VALUE "VALOR".
      *
        03  LINEA-1.
         05 FILLER         PIC 9(04) USAGE IS COMP-1   VALUE 4626.
         05 FILLER         PIC X(03) VALUE SPACES.
         05 EL-DPTO1       PIC X(30) VALUE SPACES.
         05 EL-ESTADO1     PIC X(30) VALUE SPACES.
         05 FILLER         PIC X(02) VALUE "$ ".
         05 I-VALOR-RES    PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
        03     LINEA-SIMPLE.
         05    FILLER      PIC X(66).
         05    FILLER      PIC X(17) VALUE ALL "-".
      *
        03     LINEA-DOBLE.
         05    FILLER      PIC X(66).
         05    FILLER      PIC X(17) VALUE ALL "=".
      *
        03  LINEA-PANTALLA.
         05 FILLER         PIC X(01) VALUE SPACES.
         05 EL-DPTO2       PIC X(30) VALUE SPACES.
         05 EL-ESTADO2     PIC X(30) VALUE SPACES.
         05 FILLER         PIC X(03) VALUE "$".
         05 I-VALOR-RES2   PIC ZZZ,ZZZ,ZZ9.99.
      *
        03     LINEA-PANTS.
         05    FILLER      PIC X(61).
         05    FILLER      PIC X(17) VALUE ALL "-".
      *
        03     LINEA-PANTD.
         05    FILLER      PIC X(61).
         05    FILLER      PIC X(17) VALUE ALL "=".

               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\REPORTES.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CHCONTRO.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS         THRU F-USUARIOS.
           IF      SW-USER = 1        EXIT PROGRAM.
           PERFORM I-PANTALLA-1B      THRU F-PANTALLA-1B
           PERFORM I-BANCOS           THRU F-BANCOS
           PERFORM I-CONFIRMA-COMP    THRU F-CONFIRMA-COMP
           PERFORM I-OPCION-CONSULTA  THRU F-OPCION-CONSULTA.
           PERFORM I-OPCION-CONSULTA2 THRU F-OPCION-CONSULTA2
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA W-CONTA CON-LIN
           MOVE  5                         TO I.
           PERFORM I-START-CHCONTRO  THRU F-START-CHCONTRO.
           IF  LL =  1
               PERFORM I-START-CHCONTRO  THRU F-START-CHCONTRO.
           PERFORM I-DISPLAY             THRU F-DISPLAY
           PERFORM I-PRIMER-REGISTRO4    THRU F-PRIMER-REGISTRO4
           PERFORM I-BUSCA-MES           THRU F-BUSCA-MES
                                              UNTIL FIN-OK = 1
           PERFORM I-ROMPE-CONTROL       THRU I-ROMPE
           PERFORM I-TOTAL-DTO           THRU F-TOTAL-DTO
           PERFORM I-TOTAL-TIP           THRU F-TOTAL-TIP
           PERFORM I-TOTAL-GEN           THRU F-TOTAL-GEN
           PERFORM I-RESUMEN             THRU F-RESUMEN.
           PERFORM ACEPTA-TRUCO.
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE    WK-MASCARA-FECHA      TO  I-FECHA
           MOVE    WK-MASCARA-HORA       TO  I-HORA
           MOVE    WEMPRESA              TO  TIT-CIA
           MOVE    1                     TO  IND-INFORMES
           MOVE    1                     TO  IND-REPORTES
           MOVE    W-LONG                TO  W-BANCO1
           PERFORM I-LABEL-CHCONTRO    THRU  F-LABEL-CHCONTRO.
           PERFORM I-LABEL-CNDTACUM    THRU  F-LABEL-CNDTACUM.
           PERFORM I-LABEL-INFORMES    THRU  F-LABEL-INFORMES.
           PERFORM I-LABEL-REPORTES    THRU  F-LABEL-REPORTES.
           MOVE    LABEL-INFORMES        TO  I-PROGRAMA
           MOVE    "น"                   TO  WK-RAYAS-U
           MOVE    "ฬ"                   TO  WK-RAYAS-P
           MOVE    "ถ"                   TO  WK-RAYA-U
           MOVE    "ว"                   TO  WK-RAYA-P
           MOVE     1                    TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS CNNITCED HOJAVIDA CNDTACUM.
           OPEN I-O   CHCONTRO.
           OPEN OUTPUT INFORMES REPORTES.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
      *
       I-OPCION-CONSULTA.
              PERFORM I-AYUDA-CONSULTA  THRU F-AYUDA-CONSULTA.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO LL.
              IF   F3   MOVE 3  TO LL GO F-OPCION-CONSULTA.
              IF   F4   MOVE 4  TO LL GO F-OPCION-CONSULTA.
              IF   F5   MOVE 5  TO LL GO F-OPCION-CONSULTA.
              IF   F6   MOVE 6  TO LL GO F-OPCION-CONSULTA.
              IF   F7   MOVE 7  TO LL GO F-OPCION-CONSULTA.
              IF   F8   MOVE 8  TO LL GO F-OPCION-CONSULTA.
              IF   F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO        I-OPCION-CONSULTA.
       F-OPCION-CONSULTA.
      *
       I-OPCION-CONSULTA2.
              MOVE    ZEROS                 TO WK-EXCEPTION ANT2
              PERFORM I-AYUDA-CONSULTA2   THRU F-AYUDA-CONSULTA2.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO ANT2.
              IF W-TRUCO = "C"  AND MOD-USR(1) = "S"
                 MOVE 3  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "N"  MOVE 4  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "L"  MOVE 5  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "P"  MOVE 6  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "S"  MOVE 7  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "T"  MOVE 8  TO ANT2 GO F-OPCION-CONSULTA2.
              IF   F2           PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO      I-OPCION-CONSULTA2.
       F-OPCION-CONSULTA2.
      *
       I-PANTALLA-1B.
           MOVE "LI661 - CONTROL DE LOS CHEQUES       "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 08 POSITION 20
               DISPLAY "บ  CODIGO DEL BANCO :                        บ"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ   [F1] AYUDA                               บ"
                                               LINE 11 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 12 POSITION 20
               DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-BANCOS.
           MOVE       ZEROS             TO   WK-EXCEPTION.
           MOVE W-BANCO1 TO WI-Z12C.
           DISPLAY WI-Z2                LINE 09 POSITION 42
           ACCEPT W-BANCO1              LINE 09 POSITION 42
                   UPDATE PROMPT NO BEEP HIGH REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP  GO F-BANCOS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1  PERFORM
                  I-HELP-BANCOS  THRU F-HELP-BANCOS.
           IF  W-BANCO1    = ZEROS
               DISPLAY "Codigo del Banco no puede ser Ceros"
               LINE 25 POSITION 01 REVERSE BEEP
               GO I-BANCOS.
           MOVE W-BANCO1         TO WI-Z2   BANC-CLAVE W-LONG.
           PERFORM I-LEE-BANCOS THRU F-LEE-BANCOS.
           IF SW-EOF = 1
              DISPLAY "BANCO - INEXISTENTE  -  INTENTE NUEVAMENTE :"
              LINE 25 POSITION    01  REVERSE
              GO  I-BANCOS.
           MOVE    BANC-NOMBRE       TO  I-BANCO I-BANCO1.
           DISPLAY BANC-NOMBRE     LINE 09 POSITION 48 SIZE 15 REVERSE.
           DISPLAY SPACES LINE 25 POSITION 01 SIZE 80.
       F-BANCOS.
      *
       I-RESUMEN.
           PERFORM I-START-CHCONTRO  THRU F-START-CHCONTRO.
           PERFORM I-LEE-RESUMEN     THRU F-LEE-RESUMEN
                                          UNTIL FIN-OK = 1
           MOVE  VALOR-CAJA            TO I-VALOR-RES
           MOVE  SPACES                TO EL-DPTO1
           MOVE  "CHEQUES EN CAJA"     TO EL-ESTADO1
           WRITE REG-REPORTES  FROM LINEA-1         AFTER 4.
      *
           MOVE  VALOR-ENVIADO                  TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "ENVIADOS A EMPRESA MISION "   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM LINEA-1        AFTER 1.
      *
           MOVE  VALOR-ENTREGO                  TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "ENTREGADOS AL BENEFICIARIO"   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM LINEA-1         AFTER 1.
      *
           MOVE  VALOR-ANULA                    TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "ANULADOS                  "   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM LINEA-1         AFTER 1.
      *
           MOVE  S-TOTAL-CREDITOS               TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "TOTAL GENERAL             "   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM  LINEA-SIMPLE    AFTER 1.
           WRITE REG-REPORTES  FROM  LINEA-1         AFTER 1.
           WRITE REG-REPORTES  FROM  LINEA-DOBLE     AFTER 1.

           PERFORM I-LEE-CNDTACUM             THRU F-LEE-CNDTACUM.
           MOVE  CNDTACUM-SALDO(WK-MES)         TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "VALOR EN CONTABILIDAD     "   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM  LINEA-1       AFTER 3.
           WRITE REG-REPORTES  FROM  LINEA-1       AFTER 0.
      *
           MOVE  VALOR-CAJA                TO I-VALOR-RES
           MOVE  SPACES                    TO EL-DPTO1
           MOVE  "MENOS CHEQUES EN CAJA"   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM LINEA-1      AFTER 1.
      *
           COMPUTE  S-TOTAL-CREDITOS  = CNDTACUM-SALDO(WK-MES) +
                                        VALOR-CAJA
           MOVE  S-TOTAL-CREDITOS               TO I-VALOR-RES
           MOVE  SPACES                         TO EL-DPTO1
           MOVE  "SALDO EN BANCOS           "   TO EL-ESTADO1
           WRITE REG-REPORTES  FROM  LINEA-SIMPLE    AFTER 1.
           WRITE REG-REPORTES  FROM  LINEA-1         AFTER 1.
           WRITE REG-REPORTES  FROM  LINEA-1         AFTER 0.
           WRITE REG-REPORTES  FROM  LINEA-DOBLE     AFTER 1.
       F-RESUMEN.
      *
       I-PRIMER-REGISTRO4.
           PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO.
           IF FIN-OK = 1   GO F-PRIMER-REGISTRO4.
           MOVE    DPTO-CHCONTRO       TO WK-DEPTO
           MOVE    ESTADO-CHCONTRO     TO WK-ESTADO
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-TITULO-REPORTE  THRU F-TITULO-REPORTE
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO.
       F-PRIMER-REGISTRO4.
      *
       I-BUSCA-MES.
           IF FIN-OK = 1   GO F-BUSCA-MES.
           IF DPTO-CHCONTRO = WK-DEPTO
              IF WK-ESTADO = ESTADO-CHCONTRO
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO
                     GO      I-BUSCA-MES
              ELSE
                 PERFORM I-ROMPE-CONTROL      THRU I-ROMPE
                 PERFORM I-TOTAL-DTO          THRU F-TOTAL-DTO
                 PERFORM I-ROMPE1             THRU F-ROMPE-CONTROL
                 PERFORM I-FRA-NUEVO          THRU F-FRA-NUEVO
                 GO I-BUSCA-MES
           ELSE
                    PERFORM I-ROMPE-CONTROL  THRU I-ROMPE
                    PERFORM I-TOTAL-DTO      THRU F-TOTAL-DTO
                    PERFORM I-TOTAL-TIP      THRU F-TOTAL-TIP
                    PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
                    PERFORM I-FRA-NUEVO      THRU F-FRA-NUEVO
                    GO      I-BUSCA-MES.
       F-BUSCA-MES.
      *
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
                    DISPLAY
                    " PRESIONE <C> PARA CONFIRMAR   "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] PARA CANCELAR        "
                    LINE 24 POSITION 32 REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F8
               PERFORM I-ACEPTA-COMP     THRU F-ACEPTA-COMP
               PERFORM I-START-CHCONTRO  THRU F-START-CHCONTRO
               MOVE   1      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  F9
               PERFORM I-ACEPTA-COMP     THRU F-ACEPTA-COMP
               PERFORM I-START-CHCONTRO  THRU F-START-CHCONTRO
               MOVE   3      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  W-TRUCO                  NOT =  "C"
               GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-ACEPTA-COMP.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-COD                      LINE 12 POSITION 48
                                                 REVERSE
               CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                 LINE 24 POSITION 1.
       F-ACEPTA-COMP.

      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-CHCONTRO.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  KEY01-CHCONTRO FIN-OK.
               START CHCONTRO  KEY >   KEY01-CHCONTRO INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CHCONTRO.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
      *
       I-LEE-BANCOS.
           OPEN INPUT NOMIBANK.
           MOVE ZEROS TO SW-EOF.
           READ NOMIBANK WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
           CLOSE NOMIBANK.
       F-LEE-BANCOS.
      *
       I-LEE-CHCONTRO.
           IF FIN-OK = 1 GO F-LEE-CHCONTRO.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " " LINE   25  POSITION 80 CONTROL SG-CONTROL-STRING
           READ CHCONTRO NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK
                   GO F-LEE-CHCONTRO.
      *Controla el Dpto.
           IF MOD-USR(1) = "N" AND DPTO-CHCONTRO = "T"
                                   GO I-LEE-CHCONTRO.
           MOVE    VALOR-CHCONTRO    TO WI-Z9
           IF  I < 21
               DISPLAY WI-Z9                   LINE I  POSITION 24
               DISPLAY CHEQUE-DESDE-CHCONTRO   LINE I  POSITION 45.
           IF ANT2 = 8  GO I-EST.
           IF ANT2 = 3 AND DPTO-CHCONTRO =  "T" GO I-EST.
           IF ANT2 = 4 AND DPTO-CHCONTRO =  "N" GO I-EST.
           IF ANT2 = 5 AND DPTO-CHCONTRO =  "L" GO I-EST.
           IF ANT2 = 6 AND DPTO-CHCONTRO =  "S" GO I-EST.
           IF ANT2 = 7 AND DPTO-CHCONTRO =  "P" GO I-EST.
           GO I-LEE-CHCONTRO.
      *Controla el estado.
       I-EST.
           IF LL = 8  GO F-LEE-CHCONTRO.
           IF LL = 3 AND ESTADO-CHCONTRO =  0 GO F-LEE-CHCONTRO.
           IF LL = 4 AND ESTADO-CHCONTRO =  7 GO F-LEE-CHCONTRO.
           IF LL = 5 AND ESTADO-CHCONTRO =  6 GO F-LEE-CHCONTRO.
           IF LL = 6 AND ESTADO-CHCONTRO =  6 OR
              LL = 6 AND ESTADO-CHCONTRO =  7 GO F-LEE-CHCONTRO.
           IF LL = 7 AND ESTADO-CHCONTRO =  9 GO F-LEE-CHCONTRO.
           GO I-LEE-CHCONTRO.
       F-LEE-CHCONTRO.
      *
       I-LEE-RESUMEN.
           IF FIN-OK = 1 GO F-LEE-RESUMEN.
           READ CHCONTRO NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK
                   GO F-LEE-RESUMEN.
           IF MOD-USR(1) = "N" AND DPTO-CHCONTRO = "T"
                                   GO I-LEE-RESUMEN.
           IF ESTADO-CHCONTRO =  0
              COMPUTE VALOR-CAJA     =  VALOR-CAJA    + VALOR-CHCONTRO.
           IF ESTADO-CHCONTRO =  6
              COMPUTE VALOR-ENVIADO  =  VALOR-ENVIADO + VALOR-CHCONTRO.
           IF ESTADO-CHCONTRO =  7
              COMPUTE VALOR-ENTREGO  =  VALOR-ENTREGO + VALOR-CHCONTRO.
           IF ESTADO-CHCONTRO =  9
              COMPUTE VALOR-ANULA    =  VALOR-ANULA   + VALOR-CHCONTRO.
           COMPUTE S-TOTAL-CREDITOS = S-TOTAL-CREDITOS + VALOR-CHCONTRO.
           GO I-LEE-RESUMEN.
       F-LEE-RESUMEN.
      *
       I-LEE-CNNITCED.
               MOVE 0        TO SW-EOF.
               READ CNNITCED WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.
      *
       I-LEE-CNDTACUM.
           MOVE 0            TO SW-EOF.
           MOVE MAYCTA-BC(1) TO CNDTACUM-MAYOR
           MOVE SUBCTA-BC(1) TO CNDTACUM-SUB
           MOVE AUXCTA-BC(1) TO CNDTACUM-AUX
           MOVE ZEROS        TO CNDTACUM-NIT CNDTACUM-FRA
           READ CNDTACUM WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-EOF.
       F-LEE-CNDTACUM.
      *
       I-LEE-HOJAVIDA.
               MOVE 0        TO SW-EOF.
               READ HOJAVIDA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-HOJAVIDA.
      *
       I-COLOR-1.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-1.
      *
       I-COLOR-2.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-2.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
                MOVE  EMPRESA-CHCONTRO   TO WIN-CODIGO-EMPRESA
                READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW1.
                 MOVE NOMBRE-CHCONTRO       TO I-BENEFICIA.
                 MOVE ENTREGO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
                 PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA
                 IF  SW-EOF = 0
                    MOVE HOJAVIDA-NOMBRE     TO I-NOM-ENTREGO
                 ELSE
                    PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
                    MOVE NOMBM TO I-NOM-ENTREGO.

                 MOVE RECIBIO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
                 PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA
                 IF  SW-EOF = 0
                    MOVE HOJAVIDA-NOMBRE     TO I-NOM-RECIBIO
                 ELSE
                    PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
                    MOVE NOMBM TO I-NOM-RECIBIO.

                IF  CHEQUE-DESDE-CHCONTRO = SPACES
                    MOVE 0000  TO CHEQUE-DESDE-CHCONTRO
                    REWRITE REG-CHCONTRO.


                MOVE CONSEC-CHCONTRO        TO   I-CONSEC
                MOVE FEC-ELABORO-CHCONTRO   TO   I-ELABORO
                MOVE DPTO-CHCONTRO          TO   I-DPTO
                MOVE LUGAR-CHCONTRO         TO   I-LUGAR
                MOVE EGRESO-CHCONTRO        TO   I-EGRESO
                MOVE EMPRESA-CHCONTRO       TO   I-COD-EMPRESA
                MOVE WIN-EMPRESA            TO   I-NOM-EMPRESA
                MOVE CHEQUE-DESDE-CHCONTRO  TO   I-DESDE
                MOVE CHEQUE-HASTA-CHCONTRO  TO   I-HASTA
                MOVE VALOR-CHCONTRO         TO   I-VALOR
                MOVE ESTADO-CHCONTRO        TO   I-ESTADO
                MOVE FEC-ENTREGA-CHCONTRO   TO   I-ENTREGA
                MOVE ENTREGO-CHCONTRO       TO   I-NOM-ENTREGO
                MOVE RECIBIO-CHCONTRO       TO   I-NOM-RECIBIO

                PERFORM I-EVALUE-VALOR THRU F-EVALUE-VALOR.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO W-CONTA
                ADD 1                      TO CON-LIN.
       F-MUEVE-INFORME-1.
      *
       I-EVALUE-VALOR.
               MOVE VALOR-CHCONTRO  TO I-VALOR
               ADD VALOR-CHCONTRO   TO W-TOTAL-DEBITOS
               ADD VALOR-CHCONTRO   TO S-TOTAL-DEBITOS
               ADD VALOR-CHCONTRO   TO T-TOTAL-DEBITOS
               ADD VALOR-CHCONTRO   TO G-TOTAL-DEBITOS.
       F-EVALUE-VALOR.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
       I-ROMPE-CONTROL.
              MOVE EL-ESTADO              TO EL-ESTADO1 EL-ESTADO2
              MOVE EL-DPTO                TO EL-DPTO1   EL-DPTO2
              MOVE EL-ESTADO              TO T-ESTADO
              MOVE EL-DPTO                TO T-DPTO.

              IF DPTO-CHCONTRO = "N"
                 MOVE "NOMINAS"                  TO  EL-DPTO.
              IF DPTO-CHCONTRO = "L"
                 MOVE "PRESTACIONES"             TO  EL-DPTO.
              IF DPTO-CHCONTRO = "T"
                 MOVE "CONTABILIDAD"             TO  EL-DPTO.
              IF DPTO-CHCONTRO = "P"
                 MOVE "SUBSIDIO FAMILIAR"        TO  EL-DPTO.
              IF DPTO-CHCONTRO = "S"
                 MOVE "PAGOS ADICIONALES NOMINA" TO  EL-DPTO.

              IF ESTADO-CHCONTRO = "0"
                 MOVE "EN CAJA"                  TO  EL-ESTADO.

              IF ESTADO-CHCONTRO = "6"
                 MOVE "ENVIADO A LA EMPRESA USUARIA"   TO  EL-ESTADO.

              IF ESTADO-CHCONTRO = "7"
                 MOVE "ENTREGADO AL BENEFICIARIO"  TO  EL-ESTADO.

              IF ESTADO-CHCONTRO = "9"
                 MOVE "ANULADOS"                   TO  EL-ESTADO.
       I-ROMPE.
      *
       I-ROMPE1.
              WRITE REG-INFORMES  FROM  TITULO-ROMPE  AFTER 2
              MOVE  SPACES          TO REG-INFORMES
              WRITE REG-INFORMES AFTER 1.
       F-ROMPE-CONTROL.

       I-FRA-NUEVO.
           MOVE DPTO-CHCONTRO          TO WK-DEPTO
           MOVE ESTADO-CHCONTRO        TO WK-ESTADO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO.
       F-FRA-NUEVO.

       I-TOTAL-DTO.
                PERFORM I-COLOR-1      THRU F-COLOR-1
                MOVE W-TOTAL-DEBITOS     TO T-VDEB  I-VALOR-RES
                                                    I-VALOR-RES2
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
                WRITE REG-REPORTES  FROM  LINEA-1       AFTER 1.
                ADD 8                 TO  CON-LIN.
                DISPLAY LINEA-PANTALLA    LINE  I   POSITION 2.
                ADD  1  TO  I.
       F-TOTAL-DTO.
      *
       I-TOTAL-MES.
                MOVE "TOTAL MES:"           TO T-CON.
                MOVE S-TOTAL-DEBITOS        TO T-VDEB.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                MOVE 0 TO S-TOTAL-DEBITOS S-TOTAL-CREDITOS.
      *          PERFORM I-IMPR-TITULO-MES THRU F-IMPR-TITULO-MES.
       F-TOTAL-MES.
      *
       I-TOTAL-TIP.
                MOVE " COMPROBANTE  "         TO T-CON
                MOVE T-TOTAL-DEBITOS          TO T-VDEB I-VALOR-RES
                                                        I-VALOR-RES2
                 IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2

                MOVE SPACES           TO EL-DPTO1   EL-DPTO2
                MOVE "SUBTOTAL"       TO EL-ESTADO1 EL-ESTADO2.
                WRITE REG-REPORTES  FROM  LINEA-SIMPLE    AFTER 1.
                WRITE REG-REPORTES  FROM  LINEA-1         AFTER 1.
                WRITE REG-REPORTES  FROM  LINEA-DOBLE     AFTER 1.

                PERFORM I-COLOR-2      THRU F-COLOR-2
                DISPLAY LINEA-PANTALLA LINE I POSITION 2 LOW
                ADD  1  TO  I.
                MOVE SPACES TO REG-REPORTES
                WRITE REG-REPORTES AFTER 2.
                ADD 5              TO CON-LIN
                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
                PERFORM I-COLOR-1      THRU F-COLOR-1.
       F-TOTAL-TIP.
      *
       I-IMPR-TITULO-MES.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 1                     TO CON-LIN.
       F-IMPR-TITULO-MES.
      *
       I-IMPR-TITULO-TIP.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 2              TO CON-LIN.
       F-IMPR-TITULO-TIP.
      *
       I-TOTAL-GEN.
                MOVE "TOTAL GENERAL   "     TO T-CON
                MOVE G-TOTAL-DEBITOS        TO T-VDEB I-VALOR-RES
                                                      I-VALOR-RES2
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                ADD 3                      TO CON-LIN.

                MOVE SPACES           TO  EL-DPTO1   EL-DPTO2
                MOVE "TOTAL GENERAL"  TO  EL-ESTADO1 EL-ESTADO2.
                WRITE REG-REPORTES  FROM  LINEA-SIMPLE    AFTER 2.
                WRITE REG-REPORTES  FROM  LINEA-1         AFTER 1.
                WRITE REG-REPORTES  FROM  LINEA-DOBLE     AFTER 1.

                PERFORM I-COLOR-2   THRU  F-COLOR-2.
                DISPLAY LINEA-PANTS       LINE  I   POSITION 2 REVERSE.
                ADD  1  TO  I.
                DISPLAY LINEA-PANTALLA    LINE  I   POSITION 2 LOW.
                ADD  1  TO  I.
                DISPLAY LINEA-PANTD       LINE  I   POSITION 2 REVERSE.
                ADD  2  TO  I.

                PERFORM I-COLOR-1         THRU F-COLOR-1.
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                          S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                          T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                          G-TOTAL-DEBITOS G-TOTAL-CREDITOS.
       F-TOTAL-GEN.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
      *
       I-TITULO-INFORME.
               ADD 1 TO WK-PAGINA
               MOVE  WK-PAGINA TO I-PAGINA.
               MOVE    LABEL-INFORMES        TO  I-PROGRAMA
               WRITE REG-INFORMES  FROM  TITULO-0      AFTER PAGE
               WRITE REG-INFORMES  FROM  TITULO-0      AFTER 0
               WRITE REG-INFORMES  FROM  TITULO-1      AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-1      AFTER 0
               WRITE REG-INFORMES  FROM  TITULO-2      AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3      AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A      AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A1     AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3      AFTER 1
               PERFORM I-ROMPE-CONTROL   THRU F-ROMPE-CONTROL.
               MOVE 9                TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TITULO-REPORTE.
               MOVE  WK-PAGINA       TO  I-PAGINA.
               MOVE    LABEL-REPORTES        TO  I-PROGRAMA
               WRITE REG-REPORTES  FROM  TITULO-0  AFTER 1
               WRITE REG-REPORTES  FROM  TITULO-0  AFTER 0
               WRITE REG-REPORTES  FROM  TITULO-1  AFTER 1
               WRITE REG-REPORTES  FROM  TITULO-1  AFTER 0
               WRITE REG-REPORTES  FROM  TITULO-2  AFTER 1
               WRITE REG-REPORTES  FROM  TITULO-3  AFTER 1
               WRITE REG-REPORTES  FROM  LINEA-0   AFTER 1
               WRITE REG-REPORTES  FROM  TITULO-3  AFTER 1.
       F-TITULO-REPORTE.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNNITCED
                 EMPRESAS
                 CNDTACUM
                 HOJAVIDA
                 INFORMES
                 REPORTES
                 CHCONTRO.
      *
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG661.COB"
           CALL   "PROG037.COB" USING AREA-LINK-NOMINA.
           CANCEL "PROG037.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-AYUDA-CONSULTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "ษออออออหออออออออออออออออออออออออออออออออออออออออออออออออออป"
                        LINE 14 POSITION 12
           "บ      บ        OPCIONES DE CONSULTA  (Estado).           บ"
                        LINE 15 POSITION 12
           "ฬออออออฮออออออออออออออออออออออออออออออออออออออออออออออออออน"
                        LINE 16 POSITION 12
           "บ <F3> บ Cheques que estan sin entregar.                  บ"
                        LINE 17 POSITION 12
           "บ <F4> บ Cheques Entregados al Beneficiario.              บ"
                        LINE 18 POSITION 12
           "บ <F5> บ Cheques Entregados a la Empresa.                 บ"
                        LINE 19 POSITION 12
           "บ <F6> บ Cheques entregados                               บ"
                        LINE 20 POSITION 12
           "บ <F7> บ Cheques Anulados.                                บ"
                        LINE 21 POSITION 12
           "บ <F8> บ Todos los cheques.                               บ"
                        LINE 22 POSITION 12
           "ศออออออสออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                        LINE 23 POSITION 12.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-AYUDA-CONSULTA.
      *
       I-AYUDA-CONSULTA2.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "ษออออออหออออออออออออออออออออออออออออออออออออออออออออออออออป"
                        LINE 16 POSITION 12
           "บ < C >บ Contabilidad.                                    บ"
                        LINE 17 POSITION 12
           "บ < N >บ Nominas.                                         บ"
                        LINE 18 POSITION 12
           "บ < L >บ Liquidaciones.                                   บ"
                        LINE 19 POSITION 12
           "บ < P >บ Pagos adicionales de nomina.                     บ"
                        LINE 20 POSITION 12
           "บ < S >บ Subsidio Familiar                                บ"
                        LINE 21 POSITION 12
           "บ < T >บ Todos                                            บ"
                        LINE 22 POSITION 12
           "ศออออออสออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                        LINE 23 POSITION 12.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-AYUDA-CONSULTA2.
      *
       I-DISPLAY.
            PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
            MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY  " " LINE   25  POSITION   80 BEEP
                       CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออน" LINE 04 POSITION 01
           "บ
      -    "                   บ" LINE 05  POSITION 01
           "บ
      -    "                   บ" LINE 06  POSITION 01
           "บ
      -    "                   บ" LINE 07  POSITION 01
           "บ
      -    "                   บ" LINE 08  POSITION 01
           "บ
      -    "                   บ" LINE 09  POSITION 01
           "บ
      -    "                   บ" LINE 10  POSITION 01
           "บ
      -    "                   บ" LINE 11  POSITION 01
           "บ
      -    "                   บ" LINE 12  POSITION 01
           "บ
      -    "                   บ" LINE 13  POSITION 01
           "บ
      -    "                   บ" LINE 14  POSITION 01
           "บ
      -    "                   บ" LINE 15  POSITION 01
           "บ
      -    "                   บ" LINE 16  POSITION 01
           "บ
      -    "                   บ" LINE 17  POSITION 01
           "บ
      -    "                   บ" LINE 18  POSITION 01
           "บ
      -    "                   บ" LINE 19  POSITION 01
           "บ
      -    "                   บ" LINE 20  POSITION 01
           "บ
      -    "                   บ" LINE 21  POSITION 01
           "บ
      -    "                   บ" LINE 22  POSITION 01
           "บ
      -    "                   บ" LINE 23  POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออผ" LINE 24 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 12 CONTROL
                        SG-CONTROL-STRING
           DISPLAY  SPACES LINE 25 POSITION 2 SIZE 78
           "UBICACION DEL INFORMES----->" LINE 25 POSITION 05
            LABEL-INFORMES                LINE 25 POSITION 33
            NOM-PAPEL(IND-INFORMES)       LINE 25 POSITION 66
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 80 CONTROL
                        SG-CONTROL-STRING.
       F-DISPLAY.
      *
       I-WIN-CTAS.

       F-WIN-CTAS. EXIT.
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\HELBANKO.PRO".

               COPY "..\PLB\CHCONTRO.PLB".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\REPORTES.PLB".
