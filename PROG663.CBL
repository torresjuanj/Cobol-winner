       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG663.
      ******************************************************************
      *(saldos en Contabilidad) ABR-27-2002                            *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CHCONTRO.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\NOMIBANK.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CHCONTRO.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(07)      VALUE "PROG663".
       77      ID-PRG              PIC X(7)       VALUE "PROG663".
       77      W-VARIABLES-NOMINA  PIC X(7)       VALUE  SPACES.
       77      VALOR-CAJA          PIC 9(12)      VALUE  ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(04)      OCCURS 25 TIMES.
      *****************************************************************
               COPY "..\LBL\CHCONTRO.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
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
         05    FILLER         PIC X(15)    VALUE SPACES.
         05    FILLER         PIC X(50)    VALUE
              "CONTROL DE CHEQUES : SALDOS REALES EN BANCOS ".
         05    FILLER         PIC X(08)    VALUE SPACES.
         05    FILLER         PIC X(08)    VALUE "Hora  : ".
         05    I-HORA         PIC X(05).

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(80)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(40)    VALUE SPACES.

        03     TITULO-3.
      *  05    FILLER      PIC 9(04)    USAGE IS COMP-1   VALUE 4623.
         05    FILLER      PIC X(140)   VALUE ALL "=".
      *
        03  LINEA-0.
      *  05 FILLER         PIC 9(04) USAGE IS COMP-1   VALUE 4626.
         05 FILLER         PIC X(03) VALUE  SPACES.
         05 FILLER         PIC X(20) VALUE "B A N C O ".
         05 FILLER         PIC X(22) VALUE "VR.CONTABLE".
         05 FILLER         PIC X(23) VALUE "VR.CAJA".
         05 FILLER         PIC X(10) VALUE "SALDO REAL".
      *
        03  LINEA-1.
      *  05 FILLER         PIC 9(04) USAGE IS COMP-1   VALUE 4626.
         05 EL-BANCO1      PIC X(15) VALUE SPACES.
         05 FILLER         PIC X(05) VALUE SPACES.
         05 EL-CONTABLE1   PIC ZZZZ,ZZZ,ZZ9.99.
         05 FILLER         PIC X(05) VALUE SPACES.
         05 EL-CAJA1       PIC ZZZZ,ZZZ,ZZ9.99.
         05 FILLER         PIC X(05) VALUE SPACES.
         05 EL-SALDO1      PIC ZZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
        03  LINEA-PANTALLA.
         05 FILLER         PIC X(01) VALUE SPACES.
         05 EL-BANCO       PIC X(15) VALUE SPACES.
         05 FILLER         PIC X(02) VALUE SPACES.
         05 EL-CONTABLE    PIC ZZZZZ,ZZZ,ZZ9.99-.
         05 FILLER         PIC X(01) VALUE SPACES.
         05 EL-CAJA        PIC ZZZZZ,ZZZ,ZZ9.99-.
         05 FILLER         PIC X(01) VALUE SPACES.
         05 EL-SALDO       PIC ZZZZZ,ZZZ,ZZ9.99-.
         05 FILLER         PIC X(07) VALUE SPACES.
      *
        03     LINEA-PANTS.
         05    FILLER      PIC X(18).
         05    FILLER      PIC X(52) VALUE ALL "-".
         05    FILLER      PIC X(08) VALUE SPACES.
      *
        03     LINEA-PANTD.
         05    FILLER      PIC X(18).
         05    FILLER      PIC X(52) VALUE ALL "=".
         05    FILLER      PIC X(08) VALUE SPACES.

               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION  USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CHCONTRO.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
           PERFORM I-DISPLAY         THRU F-DISPLAY
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
           PERFORM I-START-NOMIBANK  THRU F-START-NOMIBANK.
           MOVE    0 TO  W-TOTAL-DEBITOS   S-TOTAL-DEBITOS
                         S-TOTAL-CREDITOS  G-TOTAL-DEBITOS
                         WK-PAGINA SW-EOF
           PERFORM I-PROCESA-NOMIBANK THRU F-PROCESA-NOMIBANK
                                           UNTIL SW-EOF = 1
           PERFORM I-TOTAL-GEN        THRU F-TOTAL-GEN.
           PERFORM ACEPTA-TRUCO.
           PERFORM I-FIN-MODE         THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE    6                     TO  I.
           MOVE    88                    TO  CON-LIN.
           MOVE    WK-MASCARA-FECHA      TO  I-FECHA
           MOVE    WK-MASCARA-HORA       TO  I-HORA
           MOVE    WEMPRESA              TO  TIT-CIA
           MOVE    1                     TO  IND-INFORMES
           PERFORM I-LABEL-CNDTACUM    THRU  F-LABEL-CNDTACUM.
           PERFORM I-LABEL-INFORMES    THRU  F-LABEL-INFORMES.
           MOVE    CON-USERNAME          TO LAB-INFORMES-CIA.
           MOVE    LABEL-INFORMES        TO  I-PROGRAMA
           MOVE    "น"                   TO  WK-RAYAS-U
           MOVE    "ฬ"                   TO  WK-RAYAS-P
           MOVE    "ถ"                   TO  WK-RAYA-U
           MOVE    "ว"                   TO  WK-RAYA-P
           MOVE     1                    TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNDTACUM NOMIBANK.
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      ***************************************************************
      *
       I-PROCESA-NOMIBANK.
           PERFORM I-LEE-BANCOS       THRU F-LEE-BANCOS.
           IF        SW-EOF = 1         GO F-PROCESA-NOMIBANK.
           MOVE    BANC-NOMBRE          TO EL-BANCO EL-BANCO1 W-BANCO1.
           MOVE    BANC-CODIGO          TO W-BANCO1.
           MOVE ZEROS TO FIN-OK.
           PERFORM I-START-CHCONTRO   THRU F-START-CHCONTRO.
      *JJ  IF      FIN-OK  = 1          GO I-PROCESA-NOMIBANK.
           PERFORM I-PROCESA-CHCONTRO THRU F-PROCESA-CHCONTRO.
      *JJ                                  UNTIL FIN-OK = 1.
      *JJ  CLOSE CHCONTRO.
       F-PROCESA-NOMIBANK.
      *
       I-PROCESA-CHCONTRO.
           MOVE ZEROS TO VALOR-CAJA   EL-CAJA   EL-CAJA1
           PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO.
           MOVE    VALOR-CAJA          TO EL-CAJA  EL-CAJA1.
      *
           MOVE ZEROS TO CNDTACUM-SALDO(WK-MES) EL-CONTABLE EL-CONTABLE1
           PERFORM I-LEE-CNDTACUM        THRU F-LEE-CNDTACUM.
           MOVE    CNDTACUM-SALDO(WK-MES)  TO  EL-CONTABLE EL-CONTABLE1
      *
           MOVE   ZEROS TO  S-TOTAL-CREDITOS EL-SALDO EL-SALDO1.
           COMPUTE  S-TOTAL-CREDITOS   = CNDTACUM-SALDO(WK-MES)   +
                                         VALOR-CAJA
           MOVE  S-TOTAL-CREDITOS     TO EL-SALDO EL-SALDO1.
           ADD CNDTACUM-SALDO(WK-MES) TO W-TOTAL-DEBITOS
           ADD VALOR-CAJA             TO S-TOTAL-DEBITOS
           ADD S-TOTAL-CREDITOS       TO G-TOTAL-DEBITOS
           IF  S-TOTAL-CREDITOS NOT =  ZEROS
               PERFORM I-IMPRIME-DETALLE  THRU F-IMPRIME-DETALLE.
       F-PROCESA-CHCONTRO.
      *
       I-IMPRIME-DETALLE.
            IF CON-LIN > 79
               PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            WRITE REG-INFORMES FROM LINEA-1          AFTER 2
            ADD 2                        TO CON-LIN
            COMPUTE MM = I / 2.
            COMPUTE UU = MM * 2
            DISPLAY  '    '            LINE I POSITION 76.
            IF UU = I
               PERFORM I-COLOR-3        THRU F-COLOR-3
            ELSE
               PERFORM I-COLOR-1        THRU F-COLOR-1.

             DISPLAY LINEA-PANTALLA  LINE I POSITION 2
            ADD  1  TO  I.
       F-IMPRIME-DETALLE.
      *
       I-TOTAL-GEN.
            MOVE "TOTAL GENERAL   "     TO EL-BANCO1   EL-BANCO.
            MOVE  W-TOTAL-DEBITOS       TO EL-CONTABLE EL-CONTABLE1
            MOVE  S-TOTAL-DEBITOS       TO EL-CAJA     EL-CAJA1
            MOVE  G-TOTAL-DEBITOS       TO EL-SALDO    EL-SALDO1.
            WRITE REG-INFORMES        FROM TITULO-3  AFTER 2
            WRITE REG-INFORMES        FROM LINEA-1   AFTER 1
            
            WRITE REG-INFORMES        FROM TITULO-3  AFTER 1
            ADD 3                       TO CON-LIN.
            PERFORM I-COLOR-2         THRU F-COLOR-2.
            DISPLAY LINEA-PANTS       LINE I POSITION 2 REVERSE.
            ADD  1  TO  I.
            DISPLAY LINEA-PANTALLA    LINE I POSITION 2 LOW.
            ADD  1  TO  I.
            DISPLAY LINEA-PANTD       LINE I POSITION 2 REVERSE.
            ADD  2  TO  I.
            PERFORM I-COLOR-1         THRU F-COLOR-1.
            MOVE 0 TO W-TOTAL-DEBITOS  S-TOTAL-DEBITOS
                      S-TOTAL-CREDITOS G-TOTAL-DEBITOS.
       F-TOTAL-GEN.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
      *
       I-TITULO-INFORME.
               ADD   1               TO  WK-PAGINA.
               MOVE  WK-PAGINA       TO  I-PAGINA.
               MOVE  LABEL-INFORMES        TO  I-PROGRAMA
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  LINEA-0   AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1.
               MOVE  5       TO CON-LIN.
       F-TITULO-INFORME.
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
       I-START-CHCONTRO.
               PERFORM I-LABEL-CHCONTRO  THRU  F-LABEL-CHCONTRO.
               MOVE ZEROS                  TO  SW-ABRE  FIN-OK.
               MOVE SPACES TO CHCONTRO-STATUS
               OPEN INPUT CHCONTRO
               MOVE 1                      TO  SW-ABRE
               IF  CHCONTRO-STATUS  =  "35"
                            MOVE  1 TO FIN-OK
                            GO F-START-CHCONTRO.
               MOVE ZEROS                 TO  KEY01-CHCONTRO.
               START CHCONTRO  KEY >   KEY01-CHCONTRO INVALID KEY
                            MOVE  1 TO FIN-OK
                            CLOSE      CHCONTRO.
       F-START-CHCONTRO.
      *
       I-START-NOMIBANK.
               MOVE ZEROS         TO  BANC-CLAVE    SW-EOF.
               START NOMIBANK  KEY >  BANC-CLAVE    INVALID KEY
                     MOVE "98"    TO  STATUS-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-NOMIBANK.
      *
       I-LEE-BANCOS.
           IF SW-EOF = 1 GO F-LEE-BANCOS.
           MOVE ZEROS TO SW-EOF.
           READ NOMIBANK NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO SW-EOF.
           IF MAYCTA-BC(1)     = 2105 GO F-LEE-BANCOS.
           IF MAYCTA-BC(1) NOT = 1110 GO I-LEE-BANCOS.
           IF BANC-CODIGO > 90     GO I-LEE-BANCOS.
       F-LEE-BANCOS.
      *
       I-LEE-CHCONTRO.
           IF FIN-OK = 1 GO F-LEE-CHCONTRO.
           READ CHCONTRO NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK
                   CLOSE CHCONTRO
                   GO F-LEE-CHCONTRO.
           DISPLAY  CHEQUE-DESDE-CHCONTRO   LINE I  POSITION 76.
           IF ESTADO-CHCONTRO =  0
              COMPUTE VALOR-CAJA   =  VALOR-CAJA  + VALOR-CHCONTRO.
           GO I-LEE-CHCONTRO.
       F-LEE-CHCONTRO.
      *
       I-LEE-CNDTACUM.
           MOVE 0            TO SW-1.
           MOVE MAYCTA-BC(1) TO CNDTACUM-MAYOR
           MOVE SUBCTA-BC(1) TO CNDTACUM-SUB
           MOVE AUXCTA-BC(1) TO CNDTACUM-AUX
           MOVE ZEROS        TO CNDTACUM-NIT CNDTACUM-FRA
           READ CNDTACUM WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
       F-LEE-CNDTACUM.
      *
       I-COLOR-1.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-1.
      *
       I-COLOR-3.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-3.
      *
       I-COLOR-2.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-2.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNDTACUM
                 NOMIBANK
                 INFORMES.
      *
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE WK-MES                 TO  CON-PERIODO
           CANCEL "PROG663.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-DISPLAY.
            MOVE    1                     TO  IND-INFORMES
            MOVE " *S A L D O  R E A L  D E  L O S  B A N C O S* "
            TO WK-SISTEMA
            PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
            MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
            DISPLAY  " " LINE   25  POSITION   80 BEEP
                       CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออน" LINE 04 POSITION 01
           "บ    B A N C O          VR.CONTABLE      VR.CAJA           V
      -    "R.REAL             บ" LINE 05  POSITION 01
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
           "     B A N C O          VR.CONTABLE      VR.CAJA           V
      -    "R.REAL            " LINE 05  POSITION 02
           DISPLAY  SPACES LINE 25 POSITION 2 SIZE 78
           "UBICACION DEL INFORMES----->" LINE 25 POSITION 05
            LABEL-INFORMES                LINE 25 POSITION 33
            NOM-PAPEL(IND-INFORMES)       LINE 25 POSITION 66
            PERFORM I-COLOR-2   THRU F-COLOR-2.
       F-DISPLAY.
      *
       I-WIN-CTAS.

       F-WIN-CTAS. EXIT.
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".

               COPY "..\PLB\CHCONTRO.PLB".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\CNDTACUM.PLB".

