       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG773.
      ******************************************************************
      * LIBRO MAYOR Y BALANCES       ABR-27-2002                       *
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
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(07) VALUE "PROG773".
       77      ID-PRG              PIC X(7)  VALUE "PROG773".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
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
      *
        03     TITULO-1.
         05    FILLER      PIC X(60)    VALUE SPACES.
         05    FILLER      PIC X(19)    VALUE "         N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC Z.
      *
        03     TITULO-2.
         05    FILLER      PIC X(09)    VALUE SPACES.
         05    FILLER      PIC X(25)    VALUE "LIBRO MAYOR Y BALANCES".
      *
        03     TITULO-2A.
         05    FILLER       PIC X(10)        VALUE SPACES.
         05    I-MES        PIC X(10)        VALUE SPACES.
         05    FILLER       PIC X(4)         VALUE " de ".
         05    I-ANO        PIC X(04)        VALUE SPACES.
         05    FILLER      PIC X(45)         VALUE SPACES.
      *
        03     TITULO-2B.
         05    FILLER      PIC X(22) VALUE "Fecha de Elaboracion: ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(08)         VALUE "  Hora: ".
         05    I-HORA      PIC X(05).
         05    I-CAMPO     PIC X(035)   VALUE SPACES.
         05    FILLER       PIC X(12)        VALUE "  Nivel No. ".
         05    NIVEL-S      PIC 9.
         05    NIVEL-NOM    PIC X(25)        VALUE SPACES.
         05    FILLER       PIC X(02)        VALUE "  ".
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
      *
        03     TITULO-3.
         05    FILLER      PIC X(233)   VALUE ALL "=".
      *
        03  TITULO-9.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(31)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(35)      VALUE
              "    S A L D O   A N T E R I O R    ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(35)      VALUE
              "        M O V I M I E N T O        ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(35)      VALUE
              "       N U E V O   S A L D O       ".
           05 FILLER         PIC X(01)      VALUE "|".
      *
        03  TITULO-9A.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(06)      VALUE "CUENTA".
           05 FILLER         PIC X(20)      VALUE
              "    N O M B R E     ".
           05 FILLER         PIC X(05)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE "  D E B I T O S  ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE " C R E D I T O S ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE "  D E B I T O S  ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE " C R E D I T O S ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE "  D E B I T O S  ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE " C R E D I T O S ".
           05 FILLER         PIC X(01)      VALUE "|".
      *
        03  LIN14-S.
            05 FILLER         PIC X(139)     VALUE ALL "-".
      *
        03  LIN15-S.
            05 FILLER         PIC X(141)     VALUE ALL "=".
      *
        03 LDET1.
           05 FILLER         PIC X(02)       VALUE "| ".
           05 CODCTA-S       PIC 9(04)       VALUE ZEROS.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 NOMCTA-S       PIC X(25)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALANT-DEB     PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALANT-CRE     PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVDEB-S       PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVCRE-S       PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALNVO-DEB     PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALNVO-CRE     PIC   ZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
      *
        03 LBLANCO.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(31)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LCONTIN.
           05 FILLER         PIC X(02)       VALUE "|".
           05 FILLER         PIC X(14)       VALUE SPACES.
           05 FILLER         PIC X(14)       VALUE "** CONTINUA **".
           05 FILLER         PIC X(02)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(17)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 TITULO-4.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(03)       VALUE SPACES.
           05 FILLER         PIC X(25)       VALUE
           " S U M A S  I G U A L E S".
           05 FILLER         PIC X(03)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-SALANT-DEB   PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-SALANT-CRE   PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-MOVDEB       PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-MOVCRE       PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-SALNVO-DEB   PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-SALNVO-CRE   PIC ZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
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
               MOVE    1                   TO   IND-INFORMES.
               MOVE 90 TO CON-LIN
               PERFORM I-PANTALLA-1B     THRU  F-PANTALLA-1B
               PERFORM I-ACEPTA-MES      THRU  F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP   THRU  F-CONFIRMA-COMP
               PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
               PERFORM I-PROCESO-F4      THRU  F-PROCESO-F4.
               PERFORM I-FIN-MODE        THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE W-NIT-EMP              TO  I-NITEMP
               MOVE W-NIT-EMP-EXT          TO  I-NITEXT
               MOVE SPACES                 TO  TIT-CIA
               MOVE    1                   TO   IND-INFORMES.
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                TO  LAB-INFORMES-ANO
                                               I-ANO
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNDTACUM
           OPEN INPUT  EMPRESAS CNNITCED CNCATCTA
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       PROCESO PRINCIPAL DEL PROGRAMA                    ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PROCESO-F4.
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE 4 TO NIVEL-S
            MOVE "*** CUENTAS MAYORES ***" TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-MAYOR THRU F-PROCESO-MAYOR
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F4.
      *
       I-PROCESO-MAYOR.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS MAYORES ..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-MAYOR.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-MAYOR.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û       I-MUEVE-INFORME               Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ

       I-MUEVE-INFORME-1.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            DISPLAY CNDTACUM-NIT           LINE 20 POSITION 28
            DISPLAY CNDTACUM-MAYOR         LINE 20 POSITION 56
            DISPLAY CNDTACUM-SUB           LINE 20 POSITION 61
            DISPLAY CNDTACUM-AUX           LINE 20 POSITION 63
            DISPLAY CNDTACUM-MESFRA        LINE 21 POSITION 28
            DISPLAY CNDTACUM-FRA           LINE 21 POSITION 56
            MOVE WK-NOMBRE-MES(W-LONG)       TO I-MES
            MOVE CNDTACUM-MAYOR              TO CODCTA-S.
            MOVE NOMCTA                      TO NOMCTA-S.

            IF CNDTACUM-SALDO(MESW) IS POSITIVE
               MOVE CNDTACUM-SALDO(MESW)   TO  SALANT-DEB.
      *MOVIMIENTO MES ACTUAL
            IF CNDTACUM-SALDO(MESW) IS NEGATIVE
                MOVE CNDTACUM-SALDO(MESW)   TO  SALANT-CRE.

            MOVE CNDTACUM-MOVDEB(W-LONG) TO MOVDEB-S

            MOVE CNDTACUM-MOVCRE(W-LONG) TO MOVCRE-S
      *SALDOS MES ANTERIOR
            IF CNDTACUM-SALDO(W-LONG) IS POSITIVE
            MOVE CNDTACUM-SALDO(W-LONG)   TO  SALNVO-DEB.

            IF CNDTACUM-SALDO(W-LONG) IS NEGATIVE
            MOVE CNDTACUM-SALDO(W-LONG)   TO  SALNVO-CRE.
            IF NIVEL = 1
               PERFORM I-ANALIZE-SALDOS  THRU F-ANALIZE-SALDOS
               ADD CNDTACUM-MOVDEB(W-LONG) TO W-TOTAL-DEBITOS
               ADD CNDTACUM-MOVCRE(W-LONG) TO W-TOTAL-CREDITOS.
            PERFORM EVALUE-IMPRESION     THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û       I-ANALIZA-SALDOS              Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-ANALIZE-SALDOS.
           IF CNDTACUM-SALDO(MESW) IS POSITIVE
               ADD CNDTACUM-SALDO(MESW)    TO T-TOTAL-DEBITOS.
           IF CNDTACUM-SALDO(MESW) IS NEGATIVE
               ADD CNDTACUM-SALDO(MESW)    TO T-TOTAL-CREDITOS.
           IF CNDTACUM-SALDO(W-LONG) IS POSITIVE
               ADD CNDTACUM-SALDO(W-LONG)  TO G-TOTAL-DEBITOS.
           IF CNDTACUM-SALDO(W-LONG) IS NEGATIVE
               ADD CNDTACUM-SALDO(W-LONG)  TO G-TOTAL-CREDITOS.
       F-ANALIZE-SALDOS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       IMPRESION DEL REPORTE                             ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA
               MOVE 0 TO W-TOTAL-DEBME45 W-TOTAL-CREDME45
                         S-TOTAL-DEBME45 S-TOTAL-CREDME45
                         T-TOTAL-DEBME45 T-TOTAL-CREDME45
                         G-TOTAL-DEBME45 G-TOTAL-CREDME45
               MOVE 0 TO W-TOTAL-DEBMY45 W-TOTAL-CREDMY45
                         S-TOTAL-DEBMY45 S-TOTAL-CREDMY45
                         T-TOTAL-DEBMY45 T-TOTAL-CREDMY45
                         G-TOTAL-DEBMY45 G-TOTAL-CREDMY45.
       F-CEROS-VARIABLES.
      *
      ***************************************************************
      *          E V A L U A C I O N    D E    C A M P O S          *
      ***************************************************************
       EVALUE-IMPRESION.
            MOVE 0 TO RUT-OK.
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   = 0
               MOVE 1 TO RUT-OK
               PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRESION.
      *
       IMPRE1.
           IF CON-LIN > 54
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
           PERFORM IMPRENIV THRU F-IMPRENIV.
       F-IMPRE1.
      *
       IMPRENIV.
           IF NIVEL = 1 OR NIVEL = 2
              ADD 0 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              ADD 1 TO CON-LIN.
           PERFORM INICIA-IMPRE  THRU  F-INICIA-IMPRE.
       F-IMPRENIV.
      *
       INICIA-IMPRE.
               MOVE 0 TO SALANT-DEB      SALANT-CRE
                         SALNVO-DEB      SALNVO-CRE.
       F-INICIA-IMPRE.
      *
       LINE-CIERRE.
            IF CON-LIN = 90
               MOVE  0  TO CON-LIN
               GO F-LINE-CIERRE.
            WRITE REG-INFORMES FROM LBLANCO AFTER 1
            WRITE REG-INFORMES FROM LCONTIN AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1
            MOVE  SPACES         TO REG-INFORMES.
            WRITE REG-INFORMES      AFTER PAGE.
            MOVE  0  TO CON-LIN.
       F-LINE-CIERRE.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************
       I-TITULO-INFORME.
           MOVE 0 TO CON-LIN
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER 1
         
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 8
           
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
          
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 3
        
           WRITE REG-INFORMES  FROM TITULO-9   AFTER 1
           
           WRITE REG-INFORMES  FROM TITULO-9A  AFTER 1
           
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
         
           MOVE 15                         TO  CON-LIN.
       F-TITULO-INFORME.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û       I-TOTAL-GENERAL               Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ

       I-TOTAL-GEN.
                MOVE T-TOTAL-DEBITOS        TO T-SALANT-DEB.
                MOVE T-TOTAL-CREDITOS       TO T-SALANT-CRE.
                MOVE W-TOTAL-DEBITOS        TO T-MOVDEB.
                MOVE W-TOTAL-CREDITOS       TO T-MOVCRE.
                MOVE G-TOTAL-DEBITOS        TO T-SALNVO-DEB.
                MOVE G-TOTAL-CREDITOS       TO T-SALNVO-CRE.
                IF CON-LIN > 54
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LIN14-S AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM LIN15-S AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-TOTAL-GEN.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       LECTURA Y START DE ARCHIVOS                       ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-START-CNDTACUM.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-LEE-CNCATCTA.
               MOVE CNDTACUM-MAYOR       TO MAYMAE
               MOVE CNDTACUM-SUB         TO SUBMAE
               MOVE CNDTACUM-AUX         TO AUXMAE
               MOVE 0        TO SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.
      *
       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED WITH NO LOCK
                 INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.
      *
       I-LEE-CNDTACUM.
              PERFORM INICIA-IMPRE   THRU   F-INICIA-IMPRE
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                MOVE 1 TO FIN-OK.
              IF CNDTACUM-SALDO(MESW)    = 0 AND
                 CNDTACUM-MOVDEB(W-LONG) = 0 AND
                 CNDTACUM-MOVCRE(W-LONG) = 0 AND
                 CNDTACUM-SALDO(W-LONG)  = 0 AND
                 FIN-OK = 0 GO I-LEE-CNDTACUM.
            DISPLAY CNDTACUM-NIT           LINE 22 POSITION 28
            DISPLAY CNDTACUM-MAYOR         LINE 22 POSITION 56
            DISPLAY CNDTACUM-SUB           LINE 22 POSITION 61
            DISPLAY CNDTACUM-AUX           LINE 22 POSITION 63.
       F-LEE-CNDTACUM.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA   TO  W-NOMBRE.
       F-LEE-EMPRESAS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       CAPTURA DE DATOS POR TECLADO                      ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG          LINE 11 POSITION 50 REVERSE
               CONVERT                TAB NO BEEP ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                             LINE 24 POSITION 1 REVERSE BLINK
                             GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF W-LONG = 1 OR W-LONG = 14
                  MOVE 14 TO MESW
               ELSE
                  COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1 REVERSE
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       RUTINAS DE IMPRESION Y PANTALLAS                  ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PANTALLA-1B.
               MOVE "* L I B R O  M A Y O R  Y  B A L A N C E S *"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
                                               LINE 10 POSITION 20
               DISPLAY "º  DIGITE EL MES A REPORTAR :                º"
                                               LINE 11 POSITION 20
               DISPLAY "º   [F2] TERMINAR                            º"
                                               LINE 12 POSITION 20
               DISPLAY "º                                            º"
                                               LINE 13 POSITION 20
               DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-PANTALLA-1B.
      *
       I-DISPLAY.
               MOVE "* L I B R O  M A Y O R  Y  B A L A N C E S *"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ»"
                                              LINE 19 POSITION 15
               DISPLAY "º N.I.T No.:               No.CUENTA   :
      -       "      º"
                                              LINE 20 POSITION 15
               DISPLAY "º MES      :               No. FACTURA :
      -       "      º"
                                              LINE 21 POSITION 15
               DISPLAY "º
      -       "      º"
                                              LINE 22 POSITION 15
               DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ¼"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU   F-DISPLAY-LABEL.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY.

       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS
                 INFORMES
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG773.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
       
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\DISLABEL.PRO".
      *
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".


