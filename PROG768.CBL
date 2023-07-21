       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG768.
      ****************************************************************
      *INVENTARIO&BALANCE-DIFERENTE NIVELES IMPRESION   ABR-27-2002  *
      ****************************************************************
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
       77      W-PROGRAMA          PIC X(07)      VALUE "PROG768".
       77      ID-PRG              PIC X(07)      VALUE "PROG768".
       77      WK-MAYOR            PIC 9(04)      VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(07)      VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
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
      *
        03     TITULO-1.
         05    FILLER      PIC X(08)       VALUE "NIT No.".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)       VALUE SPACES.
         05    I-NITEXT    PIC Z.
      *
        03     TITULO-2.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(22)    VALUE "INVENTARIO Y BALANCES".
      *
        03     TITULO-2A.
         05    FILLER      PIC X(12)    VALUE SPACES.
         05    I-MES       PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(7)     VALUE " de 1.9".
         05    I-ANO       PIC X(02)    VALUE SPACES.
         05    FILLER      PIC X(45)    VALUE SPACES.
      *
        03     TITULO-2B.
         05    FILLER      PIC X(22)  VALUE "Fecha de Elaboracion: ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(08)  VALUE "  Hora: ".
         05    I-HORA      PIC X(05).
         05    I-CAMPO     PIC X(035) VALUE SPACES.
         05    FILLER      PIC X(12)  VALUE "  Nivel No. ".
         05    NIVEL-S     PIC 9.
         05    NIVEL-NOM   PIC X(25)  VALUE SPACES.
         05    FILLER      PIC X(02)  VALUE "  ".
         05    FILLER      PIC X(08)  VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ   VALUE ZEROS.
         05    FILLER      PIC X(10)  VALUE SPACES.
      *
        03     TITULO-3.
         05    FILLER      PIC X(233) VALUE ALL "=".
      *
        03  TITULO-9.
           05 FILLER       PIC X(01)   VALUE "|".
           05 FILLER       PIC X(13)   VALUE " C O D I G O ".
           05 FILLER       PIC X(01)   VALUE "|".
           05 FILLER       PIC X(13)   VALUE SPACES.
           05 FILLER       PIC X(21)   VALUE SPACES.
           05 FILLER       PIC X(14)   VALUE SPACES.
           05 FILLER       PIC X(01)   VALUE "|".
           05 FILLER       PIC X(19)   VALUE "     S A L D O".
           05 FILLER       PIC X(01)   VALUE "|".
           05 FILLER       PIC X(20)   VALUE SPACES.
           05 FILLER       PIC X(35)   VALUE "T O T A L E S   P O R:".
           05 FILLER       PIC X(01)   VALUE "|".
      *
        03  TITULO-9A.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(13)    VALUE " C U E N T A ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(13)    VALUE SPACES.
           05 FILLER       PIC X(21)    VALUE "D E S C R I P C I O N".
           05 FILLER       PIC X(14)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(19)    VALUE "    A C T U A L    ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(17)    VALUE "   N I V E L  4  ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(17)    VALUE "   N I V E L  3  ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(15)    VALUE "   N I V E L  2".
           05 FILLER       PIC X(04)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
      *
        03  LIN14-S.
           05 FILLER       PIC X(141)     VALUE ALL "-".
       
        03  LIN15-S.
           05 FILLER       PIC X(141)     VALUE ALL "=".
      *
        03 LDET1.
           05 FILLER       PIC X(01)       VALUE "|".
           05 MAYCTA-S     PIC ZZZZ.
           05 FILLER       PIC X(01)       VALUE " ".
           05 SUBCTA-S     PIC 9(02).
           05 FILLER       PIC X(01)       VALUE " ".
           05 AUXCTA1-S    PIC 9(02).
           05 FILLER       PIC X(01)       VALUE " ".
           05 AUXCTA2-S    PIC 9(02).
           05 FILLER       PIC X(01)       VALUE "|".
           05 NOMCTA-S     PIC X(46)       VALUE SPACES.

           05 NIVEL-S1     PIC Z(02)       VALUE SPACES.

           05 FILLER       PIC X(01)       VALUE "|".
           05 SALACT-S     PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE "|".
           05 MOVNI4-S     PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE "|".
           05 MOVNI3-S     PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE "|".
           05 MOVNI2-S     PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE "|".
      *
        03 LDETNIT.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MAYCTA-NIT     PIC ZZZZ.
           05 FILLER         PIC X(01)       VALUE " ".
           05 SUBCTA-NIT     PIC 9(02).
           05 FILLER         PIC X(01)       VALUE " ".
           05 AUXCTA1-NIT    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE " ".
           05 AUXCTA2-NIT    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(05)       VALUE SPACES.
           05 CODNIT-S       PIC ZZZ,ZZZ,ZZZ.
           05 FILLER         PIC X(02)       VALUE SPACES.
           05 NOMNIT-S       PIC X(28)       VALUE SPACES.

           05 NIVEL-S2       PIC Z(02)       VALUE SPACES.

           05 FILLER         PIC X(01)       VALUE "|".
           05 SALACT-NIT     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI4-NIT     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI3-NIT     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI2-NIT     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LDETDET.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MAYCTA-DET     PIC ZZZZ.
           05 FILLER         PIC X(01)       VALUE " ".
           05 SUBCTA-DET     PIC 9(02).
           05 FILLER         PIC X(01)       VALUE " ".
           05 AUXCTA1-DET    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE " ".
           05 AUXCTA2-DET    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(14)       VALUE SPACES.
           05 FILLER         PIC X(07)       VALUE "Fra No.".
           05 DETALLE-S      PIC ZZ9999.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 FILLER         PIC X(10)       VALUE "Fecha Fra:".
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 AFEC-FRA-S     PIC 99.
           05 FILLER         PIC X(1)        VALUE "/".
           05 MFEC-FRA-S     PIC 99.
           05 FILLER         PIC X(1)        VALUE "/".
           05 DFEC-FRA-S     PIC 99.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALACT-DET     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI4-DET     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI3-DET     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVNI2-DET     PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LBLANCO.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(13)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(48)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LCONTIN.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(13)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(22)       VALUE SPACES.
           05 FILLER         PIC X(14)       VALUE "** CONTINUA **".
           05 FILLER         PIC X(12)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LCONTIN1.
           05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *".
      *
        03 LCONTIN2.
           05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * * * * * * *".
      *
        03 LCONTIN3.
           05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * *".
      *
        03 TITULO-4.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(27)      VALUE SPACES.
           05 FILLER         PIC X(25)      VALUE
           " S U M A S  I G U A L E S".
           05 FILLER         PIC X(09)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(19)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 T-VDEB         PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-VCRE         PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
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
               MOVE    ZEROS  TO LL.
               PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
               PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
               PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
               PERFORM I-PANTALLA-1      THRU F-PANTALLA-1
               PERFORM I-PROCESO-COMP    THRU F-PROCESO-COMP
               PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
                  MOVE W-NIT-EMP              TO  I-NITEMP
                  MOVE W-NIT-EMP-EXT          TO  I-NITEXT
                  MOVE WEMPRESA               TO  TIT-CIA.
               MOVE    1                   TO   IND-INFORMES.
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                TO  LAB-INFORMES-ANO
                                               I-ANO
               MOVE W-CODEMP               TO  LAB-CNDTACUM-DIRCIA
               MOVE "π"                    TO  WK-RAYAS-U
               MOVE "Ã"                    TO  WK-RAYAS-P
               MOVE "∂"                    TO  WK-RAYA-U
               MOVE "«"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNDTACUM
           OPEN INPUT  EMPRESAS CNNITCED CNCATCTA
           OPEN OUTPUT INFORMES.
           ACCEPT WK-FECHA-HOY       FROM  DATE
           ACCEPT WK-HORA-HOY        FROM  TIME
           IF WK-ANO           >= 0
              ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.
           MOVE CON-ANO                TO  WK-MASCARA-ANO
           MOVE 30                     TO  WK-MASCARA-DIA
           IF W-LONG = 13
              MOVE WK-ABREVIA(MESW)   TO  WK-MASCARA-MES
           ELSE
              MOVE WK-ABREVIA(W-LONG) TO  WK-MASCARA-MES.
           MOVE WK-MASCARA-FECHA  TO  I-FECHA
           MOVE WK-MASCARA-HORA   TO  I-HORA.
       F-ABRE-ARCHIVOS.
      *
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINA PRINCIPAL - PROCESO COMP.                  ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PROCESO-COMP.
           PERFORM ACEPTA-TRUCO
            IF F2  PERFORM I-FIN-MODE    THRU F-FIN-MODE.
            PERFORM I-DISPLAY            THRU F-DISPLAY.
            PERFORM I-CEROS-VARIABLES    THRU F-CEROS-VARIABLES
            PERFORM I-START-CNDTACUM     THRU F-START-CNDTACUM
            IF F4   PERFORM I-PROCESO-F4 THRU F-PROCESO-F4.
            IF F5   PERFORM I-PROCESO-F5 THRU F-PROCESO-F5.
            IF F6   PERFORM I-PROCESO-F6 THRU F-PROCESO-F6.
      *     IF F7   PERFORM I-PROCESO-F7 THRU F-PROCESO-F7.
      *     IF F8   PERFORM I-PROCESO-F8 THRU F-PROCESO-F8.
            PERFORM I-TOTAL-GEN          THRU F-TOTAL-GEN.
       F-PROCESO-COMP.

       I-PROCESO-F4.
            MOVE 4 TO NIVEL-S
            MOVE "*** CUENTAS MAYORES ***" TO NIVEL-NOM
            PERFORM I-PROCESO-MAYOR THRU F-PROCESO-MAYOR
                    UNTIL FIN-OK = 1.
       F-PROCESO-F4.
      *
       I-PROCESO-F5.
            MOVE 5 TO NIVEL-S
            MOVE "***   SUBCUENTAS    ***" TO NIVEL-NOM
            PERFORM I-PROCESO-SUB   THRU F-PROCESO-SUB
                    UNTIL FIN-OK = 1.
       F-PROCESO-F5.
      *
       I-PROCESO-F6.
            MOVE 6 TO NIVEL-S
            MOVE "***   AUXILIARES    ***" TO NIVEL-NOM
            PERFORM I-PROCESO-AUX   THRU F-PROCESO-AUX
                    UNTIL FIN-OK = 1.
       F-PROCESO-F6.
      *
       I-PROCESO-F7.
            MOVE 7 TO NIVEL-S
            MOVE "***    N.I.T.S.     ***" TO NIVEL-NOM
            PERFORM I-PROCESO-NITS  THRU F-PROCESO-NITS
                    UNTIL FIN-OK = 1.
       F-PROCESO-F7.
      *
       I-PROCESO-F8.
            MOVE 8 TO NIVEL-S
            MOVE "*** F A C T U R A S ***" TO NIVEL-NOM
            PERFORM I-PROCESO-FRAS  THRU F-PROCESO-FRAS
                    UNTIL FIN-OK = 1.
       F-PROCESO-F8.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-SUBPROCESOS             ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
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
       I-PROCESO-SUB.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO SUBCUENTAS      ..."
                               LINE 24 POSITION 1.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-SUB.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3 OR
              (NIVEL = 4 AND CNDTACUM-NIT = 0)
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-SUB.
      *
       I-PROCESO-AUX.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS AUXILIARES ..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-AUX.
           IF CNDTACUM-NIT > 0 OR CNDTACUM-FRA > 0
              GO I-PROCESO-AUX.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3 OR NIVEL = 4 OR
      *NJ     NIVEL = 5 OR NIVEL = 6
              NIVEL = 5
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-AUX.
      *
       I-PROCESO-NITS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS N.I.T.S..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK   = 1 OR CNDTACUM-FRA   > 0 GO F-PROCESO-NITS.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-NITS.
      *
       I-PROCESO-FRAS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-FRAS.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-FRAS.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS DE GENERACION DE INFORMES                 ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-MUEVE-INFORME-1.
            DISPLAY SPACES              LINE 24 POSITION 1 SIZE 80.
            DISPLAY CNDTACUM-NIT        LINE 20 POSITION 28
            DISPLAY CNDTACUM-MAYOR      LINE 20 POSITION 56
            DISPLAY CNDTACUM-SUB        LINE 20 POSITION 61
            DISPLAY CNDTACUM-AUX        LINE 20 POSITION 63
            DISPLAY CNDTACUM-MESFRA     LINE 21 POSITION 28
            DISPLAY CNDTACUM-FRA        LINE 21 POSITION 56
      *NJ   MOVE NIVEL  TO NIVEL-S1 NIVEL-S2
            MOVE MAYMAE              TO MAYCTA-S.
            MOVE MAYMAE              TO MAYCTA-NIT
            MOVE MAYMAE              TO MAYCTA-DET
            MOVE SUBMAE              TO SUBCTA-S.
            MOVE SUBMAE              TO SUBCTA-NIT
            MOVE SUBMAE              TO SUBCTA-DET
            MOVE AUX1                TO AUXCTA1-S.
            MOVE AUX1                TO AUXCTA1-NIT
            MOVE AUX1                TO AUXCTA1-DET
            MOVE AUX2                TO AUXCTA2-S.
            MOVE AUX2                TO AUXCTA2-NIT
            MOVE AUX2                TO AUXCTA2-DET
            MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
            MOVE NOMCTA              TO NOMCTA-S.
            MOVE CNDTACUM-FRA        TO DETALLE-S.
            MOVE CNDTACUM-ANOFRA     TO AFEC-FRA-S
            MOVE CNDTACUM-MESFRA     TO MFEC-FRA-S.
            MOVE CNDTACUM-DIAFRA     TO DFEC-FRA-S.

            MOVE ZEROS TO  SALACT-S  SALACT-NIT  SALACT-DET
                           MOVNI2-S  MOVNI2-NIT  MOVNI2-DET
                           MOVNI3-S  MOVNI3-NIT  MOVNI3-DET
                           MOVNI4-S  MOVNI4-NIT  MOVNI4-DET.
            IF NIVEL = 1 OR NIVEL = 2
               MOVE CNDTACUM-SALDO(W-LONG) TO MOVNI2-S
                                              MOVNI2-NIT
                                              MOVNI2-DET.

            IF NIVEL = 3
               MOVE CNDTACUM-SALDO(W-LONG) TO MOVNI3-S
                                              MOVNI3-NIT
                                              MOVNI3-DET.

      *NJ   IF NIVEL = 4  AND ESTACTA = "S"
            IF NIVEL = 4
               MOVE CNDTACUM-SALDO(W-LONG) TO MOVNI4-S
                                              MOVNI4-NIT
                                              MOVNI4-DET.
      *
      *NJ   IF NIVEL = 4 AND ESTACTA = "N"
      *        MOVE CNDTACUM-SALDO(W-LONG) TO SALACT-S
      *                                       SALACT-NIT
      *                                       SALACT-DET.

            IF NIVEL = 5
               MOVE CNDTACUM-SALDO(W-LONG) TO SALACT-S
                                              SALACT-NIT
                                              SALACT-DET.
            PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-EVALUE-IMPRESION        ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
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
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-EVALUE-IMPRE1           ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-EVALUE-IMPRE1.
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   = 0
               MOVE 1 TO RUT-OK
               PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRE1.
      *
       IMPRE1.
           IF CON-LIN > 55
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF NIVEL-S = 7 OR NIVEL-S = 8
               PERFORM IMPRENIT THRU F-IMPRENIT
            ELSE
               PERFORM IMPRENIV THRU F-IMPRENIV.
       F-IMPRE1.
      *
       IMPRENIV.
           IF NIVEL = 1  AND WK-PAGINA > 1
              WRITE REG-INFORMES FROM LBLANCO  AFTER 1
              WRITE REG-INFORMES FROM LIN14-S  AFTER 1
              WRITE REG-INFORMES FROM LCONTIN1 AFTER 2
              
              WRITE REG-INFORMES FROM LCONTIN2 AFTER 2
           
              WRITE REG-INFORMES FROM LCONTIN3 AFTER 2
              
              MOVE  0  TO CON-LIN
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
      *
           IF NIVEL = 1 OR NIVEL = 2  OR NIVEL = 3
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              
              ADD 2 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              ADD 1 TO CON-LIN.
       F-IMPRENIV.
      *
       IMPRENIT.
           IF ESTACTA = "S" OR
              CODPGT  = "S"
              PERFORM I-SUPER-NIV THRU F-SUPER-NIV
              GO F-IMPRENIT.
           WRITE REG-INFORMES FROM LDET1 AFTER 1
           ADD   1 TO CON-LIN.
       F-IMPRENIT.
      *
       I-SUPER-NIV.
           IF NIVEL = 1  AND WK-PAGINA > 1
              WRITE REG-INFORMES FROM LBLANCO  AFTER 1
              WRITE REG-INFORMES FROM LIN14-S  AFTER 1
              WRITE REG-INFORMES FROM LCONTIN1 AFTER 2
              
              WRITE REG-INFORMES FROM LCONTIN2 AFTER 2
           
              WRITE REG-INFORMES FROM LCONTIN3 AFTER 2
              
              MOVE  0  TO CON-LIN
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.

           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              
              ADD   2 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDET1 AFTER 1
             
              ADD   1 TO CON-LIN.
       F-SUPER-NIV.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-EVALUE-IMPRE2           ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
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
           IF CON-LIN > 55
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF INDDETAL = "S" AND NIVEL-S = 8
               WRITE REG-INFORMES FROM LBLANCO AFTER 1
               WRITE REG-INFORMES FROM LDETNIT AFTER 1
               
               ADD 2 TO CON-LIN
            ELSE 
               WRITE REG-INFORMES FROM LDETNIT AFTER 1
               ADD 1 TO CON-LIN.
       F-IMPRE2.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-EVALUE-IMPRE3           ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-EVALUE-IMPRE3.
            IF CNDTACUM-NIT   > 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
       F-EVALUE-IMPRE3.
      *
       IMPRE3.
            IF CON-LIN > 55
               PERFORM LINE-CIERRE THRU F-LINE-CIERRE
               PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            WRITE REG-INFORMES FROM LDETDET AFTER 1
            ADD 1 TO CON-LIN.
       F-IMPRE3.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-TITULOS Y TOTALES       ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       LINE-CIERRE.
            WRITE REG-INFORMES FROM LCONTIN AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1
            MOVE  0  TO CON-LIN.
       F-LINE-CIERRE.
      *
       I-TOTAL-GEN.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
      *NJ       WRITE REG-INFORMES FROM LIN14-S AFTER 1
      *NJ       WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM LIN15-S AFTER 1
                WRITE REG-INFORMES FROM LCONTIN1 AFTER 2
                WRITE REG-INFORMES FROM LCONTIN1 AFTER 0
                WRITE REG-INFORMES FROM LCONTIN2 AFTER 2
                WRITE REG-INFORMES FROM LCONTIN2 AFTER 0
                WRITE REG-INFORMES FROM LCONTIN3 AFTER 2
                WRITE REG-INFORMES FROM LCONTIN3 AFTER 0

                ADD 3                      TO CON-LIN.
       F-TOTAL-GEN.
      *
       I-TITULO-INFORME.
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           IF    WK-PAGINA >  1
                 MOVE SPACES     TO REG-INFORMES
                 WRITE REG-INFORMES    AFTER PAGE.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER 3.
           
           WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 2
           
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 2
           
           WRITE REG-INFORMES  FROM  TITULO-2B AFTER 1
           
           WRITE REG-INFORMES  FROM  LIN14-S   AFTER 1
         
           WRITE REG-INFORMES  FROM  TITULO-9  AFTER 1
           
           WRITE REG-INFORMES  FROM  TITULO-9A AFTER 1
          
           WRITE REG-INFORMES  FROM  LIN14-S   AFTER 1
           
           MOVE 12                         TO  CON-LIN.
       F-TITULO-INFORME.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS VARIAS GENERALES                          ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-START-CNDTACUM.
               MOVE ZEROS                   TO  CNDTACUM-CLAVE
               IF LL = 1    MOVE  WK-MAYOR  TO  CNDTACUM-MAYOR.
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

       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED
                 WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.
      *
       I-LEE-CNDTACUM.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            DISPLAY CNDTACUM-NIT                  LINE 20 POSITION 28
            DISPLAY CNDTACUM-MAYOR                LINE 20 POSITION 56
            DISPLAY CNDTACUM-SUB                  LINE 20 POSITION 61
            DISPLAY CNDTACUM-AUX                  LINE 20 POSITION 63
            DISPLAY CNDTACUM-MESFRA              LINE 21 POSITION 28
            DISPLAY CNDTACUM-FRA                 LINE 21 POSITION 56
            READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                MOVE 1 TO FIN-OK
                                GO F-LEE-CNDTACUM.
             IF CNDTACUM-SALDO(MESW)     = 0 AND
                 CNDTACUM-MOVDEB(W-LONG) = 0 AND
                 CNDTACUM-MOVCRE(W-LONG) = 0 AND
                 CNDTACUM-SALDO(W-LONG)  = 0 GO I-LEE-CNDTACUM.
             IF  CNDTACUM-SALDO(W-LONG)  = 0 GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-CEROS-VARIABLES.
                IF WK-PAGINA = 0
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
               MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK
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


      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS DE IMPRESION Y PANTALLAS                  ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PANTALLA-1B.
               MOVE "*** P768 - INVENTARIO Y BALANCES **"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 09 POSITION 20
               DISPLAY "∫                                            ∫"
                                               LINE 10 POSITION 20
               DISPLAY "∫  TECLEE EL MES A REPORTAR :                ∫"
                                               LINE 11 POSITION 20
               DISPLAY "∫                                            ∫"
                                               LINE 12 POSITION 20
               DISPLAY "∫  CUENTA DESDE:                             ∫"
                                               LINE 13 POSITION 20
               DISPLAY "∫                                            ∫"
                                               LINE 14 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 15 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS      TO  WK-EXCEPTION
               ACCEPT W-LONG       LINE 11 POSITION 50
                                   REVERSE CONVERT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                            LINE 24 POSITION 1 REVERSE BLINK
                             GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF W-LONG = 1 OR W-LONG = 14
                 COMPUTE MESW = 14
               ELSE
                 COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR  [F8] DESDE UNA CUENTA
      -    "   [F2] TERMINAR " LINE 24 POSITION 01 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F8
               MOVE      1     TO   LL
               MOVE ZEROS      TO  WK-EXCEPTION
               ACCEPT WK-MAYOR     LINE 13 POSITION 45
                                   REVERSE CONVERT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION
                                   GO F-CONFIRMA-COMP.
           IF  W-TRUCO             NOT =  "C"
               GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-PANTALLA-1.
               MOVE "** LI768 - INVENTARIO Y BALANCE **"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 06 POSITION 20
               DISPLAY "∫                                       ∫"
                                               LINE 07 POSITION 20
               DISPLAY "∫                                       ∫"

                                               LINE 08 POSITION 20
               DISPLAY "∫                                       ∫"
                                               LINE 09 POSITION 20
               DISPLAY "∫   [F4]  NIVEL CTAS MAYORES            ∫"
                                               LINE 10 POSITION 20
               DISPLAY "∫   [F5]  NIVEL SUBCUENTAS              ∫"
                                               LINE 11 POSITION 20
               DISPLAY "∫   [F6]  NIVEL AUXILIARES (5)          ∫"
                                               LINE 12 POSITION 20
               DISPLAY "∫                                       ∫"
                                               LINE 13 POSITION 20
               DISPLAY "∫                                       ∫"
                                               LINE 14 POSITION 20
               DISPLAY "∫    [F2] TERMINAR                      ∫"
                                               LINE 15 POSITION 20
               DISPLAY "∫                                       ∫"
                                               LINE 16 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 17 POSITION 20.
               DISPLAY WK-ESPACIOS       LINE 24 POSITION 1.
               PERFORM I-DISPLAY-LABEL   THRU   F-DISPLAY-LABEL.
       F-PANTALLA-1.
      *
       I-DISPLAY.
               MOVE  "         *** INVENTARIO Y BALANCE ***   "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
             DISPLAY
            "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                              LINE 19 POSITION 15
            "∫ N.I.T No.:               No.CUENTA   :              ∫"
                                              LINE 20 POSITION 15
            "∫ MES      :               No. FACTURA :              ∫"
                                              LINE 21 POSITION 15
            "∫                                                     ∫"
                                              LINE 22 POSITION 15
            "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.
      *
       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS
                 INFORMES
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG768.COB"
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
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".
