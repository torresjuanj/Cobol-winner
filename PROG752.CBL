       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG752.
      ******************************************************************
      *CARTERA COMPARATIVA   ABR-27-2002
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
           COPY "..\DYC\CNDTATEM.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\VENDEMPR.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CNDTATEM.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\VENDEMPR.FD".

       WORKING-STORAGE SECTION.
       77      W-PROGRAMA               PIC X(07)      VALUE "PROG752".
       77      ID-PRG                   PIC X(7)       VALUE "PROG752".
       77      NIVEL-S                  PIC 9          VALUE 8.
       77      WK-CTA-MAYOR             PIC 9(4)       VALUE ZEROS.
       77      W-VARIABLES-NOMINA       PIC X(7)       VALUE SPACES.
       77      WTASA-INTERES            PIC 9(2)V9(3)  VALUE 0.14.
       77      WVR-INTERESES             PIC 9(12)      VALUE 0.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
       01      NOMBRE-TEMPORAL.
        03     NOMBRE-TEMP-1              PIC X(10)  VALUE SPACES.
        03     NOMBRE-TEMP-2              PIC X(30)  VALUE SPACES.
       01      NOMBRE-CUENTA.
        03     CUENTA-TEMP-2              PIC X(14)  VALUE SPACES.
        03     CUENTA-TEMP-3              PIC X(26)  VALUE SPACES.
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\VENDEMPR.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNDTATEM.LBL".
               COPY "..\LBL\AUDITE.LBL".
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

        03     TITULO-1.
         05    FILLER      PIC X(19)    VALUE "         N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC ZZZ.


        03     TITULO-2.
         05    FILLER      PIC X(06)    VALUE SPACES.
         05    FILLER      PIC X(28)    VALUE
               "LIBRO DE CARTERA COMPARATIVA".

        03     TITULO-2A.
         05    FILLER      PIC X(10)        VALUE SPACES.
         05    I-MES       PIC X(10)        VALUE SPACES.
         05    FILLER      PIC X(4)         VALUE " de ".
         05    I-ANO       PIC X(04)        VALUE SPACES.
         05    FILLER      PIC X(45)        VALUE SPACES.

        03     TITULO-2B.
         05    FILLER      PIC X(15)    VALUE "FECHA DE CORTE:".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(08)    VALUE "  Hora: ".
         05    I-HORA      PIC X(05).
         05    FILLER      PIC X(30)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(15)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(233)   VALUE ALL "=".

        03  TITULO-9.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(64)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(18)    VALUE "  SALDO ANTERIOR  ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(10)    VALUE SPACES.
           05 FILLER       PIC X(19)    VALUE
              "M O V I M I E N T O".
           05 FILLER       PIC X(08)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(03)    VALUE SPACES.
           05 FILLER       PIC X(11)    VALUE "NUEVO SALDO".
           05 FILLER       PIC X(04)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
      *
       03  TITULO-9A.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(07)    VALUE " No.Fra".
           05 FILLER       PIC X(12)    VALUE " Fecha Fact ".
           05 FILLER       PIC X(12)    VALUE " Fecha Vcto ".
           05 FILLER       PIC X(12)    VALUE "Cod.Empresa ".
           05 FILLER       PIC X(09)    VALUE "Dias Venc".
           05 FILLER       PIC X(12)    VALUE " Vr Interes ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(01)    VALUE SPACES.
           05 MES-ANT      PIC X(15)    VALUE SPACES.
           05 FILLER       PIC X(02)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(18)    VALUE "  D E B I T O S  ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(18)    VALUE " C R E D I T O S ".
           05 FILLER       PIC X(01)    VALUE "|".
           05 FILLER       PIC X(01)    VALUE SPACES.
           05 MES-ACT      PIC X(15)    VALUE SPACES.
           05 FILLER       PIC X(02)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "|".
      *
       03  LIN14-S.
           05 FILLER       PIC X(142)   VALUE ALL "-".
       
        03  LIN15-S.
           05 FILLER       PIC X(142)   VALUE ALL "=".
      *
        03 LDET1.
           05 FILLER       PIC X(01)     VALUE "|".
           05 NOMCTA-S     PIC X(58)     VALUE SPACES.
           05 FILLER       PIC X(02)     VALUE SPACES.
           05 T-CUENTA     PIC Z(04).
           05 FILLER       PIC X(01)     VALUE "|".
           05 SALANT-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVDEB-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVCRE-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 SALNVO-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".

        03 LDET2.
           05 FILLER       PIC X(01)     VALUE "|".
           05 NOMCTA-G     PIC X(40)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
      *
       03 LDETNIT.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 NOMNIT-S     PIC X(29)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE SPACES.
      *    05 CODNIT-S     PIC ZZZ,ZZZ,ZZZ.
           05 NOMVEND-S    PIC X(20)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 FILLER       PIC X(07)     VALUE "PLAZO:".
           05 I-DIAS-P     PIC Z(3).
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 FILLER       PIC X(02)     VALUE " |".
           05 SALANT-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVDEB-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVCRE-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 SALNVO-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
      *
        03 LDETDET.
           05 FILLER       PIC X(01)     VALUE "|".
           05 CNDT-FRA-S   PIC Z99999.
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 FECHA-FRA-S  PIC X(11)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 FECHA-VCT-S  PIC X(11)     VALUE SPACES.
           05 FILLER       PIC X(05)     VALUE SPACES.
           05 CODIG-EMP-S  PIC X(03)     VALUE SPACES.
           05 FILLER       PIC X(06)     VALUE SPACES.
           05 I-DIAS-V     PIC ZZZZZ.
           05 FILLER       PIC X(02)     VALUE SPACES.
           05 VR-INTERES-S PIC ZZZ,ZZZ,ZZZ.
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 FILLER       PIC X(02)     VALUE " |".
           05 SALANT-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVDEB-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 MOVCRE-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 SALNVO-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
      *
       03 LBLANCO.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(64)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
        03 LCONTIN.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(22)     VALUE SPACES.
           05 FILLER       PIC X(14)     VALUE "** CONTINUA **".
           05 FILLER       PIC X(28)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "|".

        03 TITULO-4.
           05 FILLER       PIC X(01)     VALUE "|".
           05 FILLER       PIC X(29)     VALUE SPACES.
           05 FILLER       PIC X(35)     VALUE
           "     T O T A L E S       ".
           05 FILLER       PIC X(01)     VALUE "|".
           05 T-SALANT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 T-VDEB       PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 T-VCRE       PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
           05 T-SALNVO     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "|".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNDTATEM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\VENDEMPR.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS          THRU  F-USUARIOS.
               IF      SW-USER = 1         EXIT  PROGRAM.
               PERFORM I-PANTALLA-1B       THRU  F-PANTALLA-1B
               PERFORM I-ACEPTA-MES        THRU  F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP     THRU  F-CONFIRMA-COMP
               PERFORM I-PROCESO-AUDITE    THRU  F-PROCESO-AUDITE
               PERFORM I-LABEL-ARCHIVOS    THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS     THRU  F-ABRE-ARCHIVOS
               PERFORM I-PROCESO-TEMPORAL  THRU  F-PROCESO-TEMPORAL
               PERFORM I-PROCESO-CARTERA   THRU  F-PROCESO-CARTERA
               PERFORM I-FIN-MODE          THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE W-NIT-EMP              TO   I-NITEMP
               MOVE W-NIT-EMP-EXT          TO   I-NITEXT
               MOVE WEMPRESA               TO   TIT-CIA.
               MOVE  1                     TO   IND-INFORMES
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                TO  LAB-INFORMES-ANO
                                               I-ANO
               MOVE "π"                    TO  WK-RAYAS-U
               MOVE "Ã"                    TO  WK-RAYAS-P
               MOVE "∂"                    TO  WK-RAYA-U
               MOVE "«"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT CNDTACUM HOJAVIDA VENDEMPR
               OPEN INPUT  EMPRESAS CNNITCED
               OPEN I-O    CNCATCTA
               OPEN OUTPUT INFORMES CNDTATEM.
               CLOSE CNDTATEM
               OPEN I-O   CNDTATEM.
       F-ABRE-ARCHIVOS.
      *
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       I-PROCESO-TEMPORAL   THRU F-PROCESO-TEMPORAL      ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PROCESO-TEMPORAL.
            PERFORM I-DISPLAY         THRU   F-DISPLAY.
            PERFORM I-START-CNDTACUM  THRU   F-START-CNDTACUM
            PERFORM I-BUSCA-CTA-TEMP  THRU   F-BUSCA-CTA-TEMP
            PERFORM I-ORGANIZA-TEMP   THRU   F-ORGANIZA-TEMP
                    UNTIL FIN-OK = 1.
       F-PROCESO-TEMPORAL.
      *
       I-BUSCA-CTA-TEMP.
            PERFORM I-LEE-CNDTACUM    THRU  F-LEE-CNDTACUM
            IF FIN-OK = 1            GO TO  F-BUSCA-CTA-TEMP.
            MOVE CNDTACUM-MAYOR         TO  MAYMAE
            MOVE CNDTACUM-SUB           TO  SUBMAE
            MOVE CNDTACUM-AUX           TO  AUXMAE
            PERFORM I-LEE-CNCATCTA    THRU  F-LEE-CNCATCTA
            IF INDICE-CAR = "S"
               MOVE CNDTACUM-MAYOR   TO  WK-CTA-MAYOR
               MOVE 0                TO  CNDTACUM-SUB CNDTACUM-AUX
               PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            ELSE
               GO I-BUSCA-CTA-TEMP.
       F-BUSCA-CTA-TEMP.
      *
       I-ORGANIZA-TEMP.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS TEMPORAL.."
                               LINE 09 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU  F-LEE-CNDTACUM
           IF FIN-OK = 1         GO TO  F-ORGANIZA-TEMP.
           IF CNDTACUM-MAYOR = WK-CTA-MAYOR
                MOVE CNDTACUM-MAYOR       TO MAYMAE
                MOVE CNDTACUM-SUB         TO SUBMAE
                MOVE CNDTACUM-AUX         TO AUXMAE
                PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
                DISPLAY CNDTACUM-MAYOR  LINE 11 POSITION 28
                DISPLAY CNDTACUM-SUB    LINE 11 POSITION 34
                DISPLAY CNDTACUM-AUX    LINE 11 POSITION 37
                DISPLAY CNDTACUM-NIT    LINE 11 POSITION 41
                DISPLAY CNDTACUM-FRA    LINE 11 POSITION 56
                PERFORM I-GRABA-TEMPO   THRU F-GRABA-TEMPO.
           IF CNDTACUM-MAYOR > WK-CTA-MAYOR MOVE 1 TO FIN-OK.
       F-ORGANIZA-TEMP.
      *
       I-GRABA-TEMPO.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.

            MOVE CNDTACUM-NOM-ALT2 TO CNDTATEM-NOM-ALT2
                                           NOMBRE-TEMPORAL.
            MOVE CNDTACUM-CLAVE          TO CNDTATEM-CLAVE
            MOVE CNDTACUM-AUX            TO CNDTATEM-AUXA2
            MOVE CNDTACUM-ALTER-1        TO CNDTATEM-ALTER-1
            MOVE NOMBRE-TEMP-1           TO CNDTATEM-NOM-1
            MOVE CNDTACUM-NIT            TO CNDTATEM-NIT-ALT2
            MOVE CNDTACUM-FRA            TO CNDTATEM-FRA-ALT2
            MOVE CNDTACUM-FECHA-FRA      TO CNDTATEM-FECHA-FRA
            MOVE CNDTACUM-FECHA-VCTO     TO CNDTATEM-FECHA-VCTO
            MOVE CNDTACUM-SALDO(MESW)    TO CNDTATEM-SALDO(MESW)
            MOVE CNDTACUM-MOVDEB(W-LONG) TO CNDTATEM-MOVDEB(W-LONG)
            MOVE CNDTACUM-MOVCRE(W-LONG) TO CNDTATEM-MOVCRE(W-LONG)
            MOVE CNDTACUM-SALDO(W-LONG)  TO CNDTATEM-SALDO(W-LONG)
            WRITE  REG-CNDTATEM.
       F-GRABA-TEMPO.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥      I-PROCESO-CARTERA  THRU   F-PROCESO-CARTERA        ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PROCESO-CARTERA.
            PERFORM I-DISPLAY         THRU   F-DISPLAY.
            PERFORM I-START-CNDTATEM  THRU   F-START-CNDTATEM
            PERFORM I-BUSCA-CTA       THRU   F-BUSCA-CTA
            PERFORM I-CEROS-VARIABLES THRU   F-CEROS-VARIABLES
            PERFORM I-ORGANIZA-CART   THRU   F-ORGANIZA-CART
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN       THRU   F-TOTAL-GEN.
       F-PROCESO-CARTERA.
      *
       I-BUSCA-CTA.
            PERFORM I-LEE-CNDTATEM    THRU  F-LEE-CNDTATEM
            IF FIN-OK = 1             GO TO F-PROCESO-CARTERA.
            MOVE CNDTATEM-MAYOR       TO MAYMAE
            MOVE CNDTATEM-SUB         TO SUBMAE
            MOVE CNDTATEM-AUX         TO AUXMAE
            PERFORM I-LEE-CNCATCTA    THRU  F-LEE-CNCATCTA
            IF INDICE-CAR = "S"
               MOVE CNDTATEM-MAYOR   TO  WK-CTA-MAYOR
               MOVE 0                TO  CNDTATEM-SUB CNDTATEM-AUX
               PERFORM I-START-CNDTATEM  THRU F-START-CNDTATEM
            ELSE
               GO I-BUSCA-CTA.
       F-BUSCA-CTA.
      *
       I-ORGANIZA-CART.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS CARTERA..."
                               LINE 09 POSITION 25.
           PERFORM I-LEE-CNDTATEM THRU F-LEE-CNDTATEM
                   IF FIN-OK = 1 GO TO F-ORGANIZA-CART.
           IF CNDTATEM-MAYOR = WK-CTA-MAYOR
                MOVE CNDTATEM-MAYOR       TO MAYMAE
                MOVE CNDTATEM-SUB         TO SUBMAE
                MOVE CNDTATEM-AUX         TO AUXMAE
                PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
                DISPLAY CNDTATEM-MAYOR  LINE 11 POSITION 28
                DISPLAY CNDTATEM-SUB    LINE 11 POSITION 34
                DISPLAY CNDTATEM-AUX    LINE 11 POSITION 37
                DISPLAY CNDTATEM-NIT    LINE 11 POSITION 41
                DISPLAY CNDTATEM-FRA    LINE 11 POSITION 56
                PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           IF CNDTATEM-MAYOR > WK-CTA-MAYOR MOVE 1 TO FIN-OK.
       F-ORGANIZA-CART.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €        MUEVE CAMPOS INFORME         ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-MUEVE-INFORME-1.
            MOVE  ZEROS  TO I-DIAS-V.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
            MOVE NOMCTA              TO NOMCTA-S NOMCTA-G
                                        NOMBRE-CUENTA.
            MOVE CNDTATEM-AUXA2      TO T-CUENTA
            IF  T-CUENTA  = ZEROS MOVE ZEROS  TO T-CUENTA.
            MOVE CNDTATEM-FRA        TO CNDT-FRA-S.
            MOVE CNDTATEM-FECHA-FRA  TO WK-FECHA-STANDARD
            PERFORM I-FECHA-FORMATO  THRU F-FECHA-FORMATO
            MOVE WK-MASCARA-FECHA    TO FECHA-FRA-S
            MOVE CNDTATEM-FECHA-VCTO TO WK-FECHA-STANDARD
            PERFORM I-FECHA-FORMATO  THRU F-FECHA-FORMATO
            MOVE WK-MASCARA-FECHA    TO FECHA-VCT-S
            MOVE ZEROS               TO VR-INTERES-S I-DIAS-V
            IF CNDTATEM-FECHA-VCTO < WK-FECHA-HOY AND
               CNDTATEM-SALDO (W-LONG) NOT = 0
               PERFORM I-CALCULA-DIAS THRU F-CALCULA-DIAS.
            IF I-DIAS-V > 3000 MOVE ZEROS TO I-DIAS-V VR-INTERES-S.


            MOVE CNDTATEM-SALDO(MESW)    TO SALANT-S
                                            SALANT-NIT
                                            SALANT-DET.
            MOVE CNDTATEM-MOVDEB(W-LONG) TO MOVDEB-S
                                            MOVDEB-NIT
                                            MOVDEB-DET.
            MOVE CNDTATEM-MOVCRE(W-LONG) TO MOVCRE-S
                                            MOVCRE-NIT
                                            MOVCRE-DET.
            MOVE CNDTATEM-SALDO(W-LONG)  TO SALNVO-S
                                            SALNVO-NIT
                                            SALNVO-DET.
           PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
       I-CALCULA-DIAS.
           IF CNDTATEM-SALDO(W-LONG) > 0
              MOVE CNDTATEM-FECHA-VCTO      TO  WK-FECHA-DESDE
              MOVE WK-FECHA-HOY             TO  WK-FECHA-HASTA
              PERFORM CALCULE-DIAS-VIGENCIA
              MOVE WK-DIAS-VIGENCIA         TO   I-DIAS-V
              MOVE 0                       TO WVR-INTERESES
              COMPUTE WVR-INTERESES = CNDTATEM-SALDO(W-LONG)
                                    * WTASA-INTERES
                                    * WK-DIAS-VIGENCIA
                                    / 100
              MOVE WVR-INTERESES     TO VR-INTERES-S.
       F-CALCULA-DIAS.
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         W-TOTAL-SALANT  W-TOTAL-SALNVO
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         T-SALANT        T-SALNVO
                         FIN-OK          WK-PAGINA.
       F-CEROS-VARIABLES.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €        EVALUA LINEAS IMPRESION      ›
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
       I-EVALUE-IMPRE1.
            IF CNDTATEM-NIT   = 0 AND CNDTATEM-FRA   = 0
               MOVE 1 TO RUT-OK
               PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRE1.
      *
       I-EVALUE-IMPRE2.
            IF CNDTATEM-NIT   > 0 AND CNDTATEM-FRA   = 0
               MOVE 1 TO RUT-OK
               MOVE CNDTATEM-NIT   TO CODIM
               PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
               IF SW-EOF = 1
                  MOVE "Error - CENTRO DE COSTO NO EXISTE" TO NOMNIT-S
                  PERFORM IMPRE2 THRU F-IMPRE2
               ELSE
                  MOVE NOMBM   TO NOMNIT-S
                  PERFORM I-LEE-EMPRESA THRU F-LEE-EMPRESA
                  PERFORM IMPRE2 THRU F-IMPRE2.
       F-EVALUE-IMPRE2.
      *

       I-EVALUE-IMPRE3.
            IF CNDTATEM-NIT   > 0 AND CNDTATEM-FRA   > 0
                  MOVE 1 TO RUT-OK.
                  PERFORM IMPRE3 THRU F-IMPRE3.
       F-EVALUE-IMPRE3.
      *
       IMPRE1.
           IF CON-LIN > 55
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF NIVEL-S = 7
               PERFORM IMPRENIT THRU F-IMPRENIT
            ELSE
               PERFORM IMPRENIV THRU F-IMPRENIV.
       F-IMPRE1.
      *
       IMPRENIV.
              IF NIVEL   = 5   AND WK-PAGINA > 1
                    PERFORM LINE-CIERRE       THRU F-LINE-CIERRE
                    PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.

                 IF NIVEL   = 5   AND WK-PAGINA = 1
                    WRITE REG-INFORMES        FROM LBLANCO    AFTER 1
                    WRITE REG-INFORMES        FROM LDET2      AFTER 1
                    
                    WRITE REG-INFORMES        FROM LBLANCO    AFTER 1
                    ADD  03                     TO CON-LIN.
              IF NIVEL   = 5   GO F-IMPRENIV.
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              WRITE REG-INFORMES FROM LDET1 AFTER 1
             
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              ADD 3 TO CON-LIN.
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
           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              
              ADD   2 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              
              ADD   1 TO CON-LIN.
       F-SUPER-NIV.
      *
       IMPRE2.
            MOVE CNDTATEM-NOM-ALT2 TO NOMNIT-S.
           IF CON-LIN > 55
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
           PERFORM I-LEE-EMPRESA   THRU F-LEE-EMPRESA.
           IF NIVEL-S = 8
              WRITE REG-INFORMES FROM LBLANCO AFTER 1
              WRITE REG-INFORMES FROM LDETNIT AFTER 1
              
              ADD 2 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDETNIT AFTER 1
              ADD 1 TO CON-LIN.
       F-IMPRE2.
      *
       IMPRE3.
            IF CON-LIN > 55
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            WRITE REG-INFORMES FROM LDETDET AFTER 1
            MOVE 0 TO T-SALANT
            ADD CNDTATEM-SALDO(MESW)    TO  W-TOTAL-SALANT
            ADD CNDTATEM-MOVDEB(W-LONG) TO  W-TOTAL-DEBITOS
            ADD CNDTATEM-MOVCRE(W-LONG) TO  W-TOTAL-CREDITOS
            ADD CNDTATEM-SALDO(W-LONG)  TO  W-TOTAL-SALNVO
            ADD 1 TO CON-LIN.
       F-IMPRE3.
      *
       LINE-CIERRE.
            IF CON-LIN = 66  GO F-LINE-CIERRE.
            WRITE REG-INFORMES FROM LCONTIN AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1.
       F-LINE-CIERRE.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €        TITULOS DEL INFORME          ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
       I-TITULO-INFORME.
           MOVE 0 TO CON-LIN
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           MOVE SPACES TO REG-INFORMES
           WRITE REG-INFORMES AFTER PAGE.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER 3
         
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 2
          
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
           
           WRITE REG-INFORMES  FROM  TITULO-2B AFTER 1
           
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
          
           WRITE REG-INFORMES  FROM TITULO-9   AFTER 1
           
           WRITE REG-INFORMES  FROM TITULO-9A  AFTER 1
          
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
          
           WRITE REG-INFORMES  FROM LBLANCO    AFTER 1
           WRITE REG-INFORMES  FROM LDET2      AFTER 1
           
           WRITE REG-INFORMES  FROM LBLANCO    AFTER 1.
           MOVE 14               TO  CON-LIN.
       F-TITULO-INFORME.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €     TOTAL GENERAL DEL INFORME       ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
       I-TOTAL-GEN.
                MOVE W-TOTAL-SALANT         TO T-SALANT.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                MOVE W-TOTAL-SALNVO         TO T-SALNVO.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LIN14-S AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM LIN15-S AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-TOTAL-GEN.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       LECTURA Y ORGANIZACION DE ARCHIVOS                ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-START-CNDTACUM.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               MOVE WK-CTA-MAYOR           TO  CNDTACUM-MAYOR
               START CNDTACUM  KEY NOT LESS CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-START-CNDTATEM.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  CNDTATEM-CLAVE
               MOVE ZEROS                  TO  CNDTATEM-AUXA2
               MOVE SPACES                 TO  CNDTATEM-NOM-1
               MOVE ZEROS                  TO  CNDTATEM-NIT-ALT2
               MOVE ZEROS                  TO  CNDTATEM-FRA-ALT2
               START CNDTATEM  KEY NOT LESS CNDTATEM-ALTER-2 INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTATEM.
      *
       I-START-EMPRESAS.
           MOVE  ZEROS               TO  WIN-LLAVE-ALTER-03  SW1
           MOVE  CNDTATEM-NIT        TO  WIN-NIT
           START EMPRESAS  KEY <  WIN-LLAVE-ALTER-03
                 INVALID KEY
                 DISPLAY "EMPRESA NO EXISTE" LINE 22 POSITION 1
                 MOVE 1 TO SW1.
       F-START-EMPRESAS.
      *
        I-LEE-EMPRESA.
           MOVE   ZEROS TO I-DIAS-P  SW1.
           PERFORM I-START-EMPRESAS   THRU F-START-EMPRESAS.
           IF SW1 = 1 GO F-LEE-EMPRESA.
        I-LEE.
           READ  EMPRESAS NEXT RECORD WITH NO LOCK AT END
                                      GO F-LEE-EMPRESA.
           IF    CNDTATEM-NIT    =  WIN-NIT
                 MOVE WIN-PLAZO-FACTURA TO I-DIAS-P
                 PERFORM I-BUSCAR-VENDEDOR THRU F-BUSCAR-VENDEDOR
                 GO F-LEE-EMPRESA
           ELSE
                 GO I-LEE.
        F-LEE-EMPRESA.
      *
       I-LEE-NIT.
               MOVE ZEROS                  TO  WIN-NIT-LETRA LL
               MOVE ZEROS                  TO  WIN-NIT-EXTENSION
            READ CNNITCED WITH NO LOCK INVALID KEY
                     DISPLAY "ERROR CNNITCED"  LINE 23 POSITION 1
                     DISPLAY CODIM          LINE 22 POSITION 1
                     DISPLAY CNDTACUM-NIT   LINE 22 POSITION 15
                     MOVE  1   TO LL.
       F-LEE-NIT.
      *
       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.

       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED WITH NO LOCK
                 INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.
      *
       I-LEE-CNDTACUM.
            MOVE 0 TO FIN-OK
            READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                MOVE 1 TO FIN-OK.
            IF CNDTACUM-SALDO(MESW)    = 0 AND
               CNDTACUM-MOVDEB(W-LONG) = 0 AND
               CNDTACUM-MOVCRE(W-LONG) = 0 AND
               CNDTACUM-SALDO(W-LONG)  = 0 AND
               FIN-OK = 0 GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNDTATEM.
            MOVE 0 TO FIN-OK
            READ CNDTATEM NEXT RECORD WITH NO LOCK  AT END
                 MOVE 1 TO FIN-OK.
            IF CNDTATEM-SALDO(MESW)    = 0 AND
               CNDTATEM-MOVDEB(W-LONG) = 0 AND
               CNDTATEM-MOVCRE(W-LONG) = 0 AND
               CNDTATEM-SALDO(W-LONG)  = 0 AND
               FIN-OK = 0 GO I-LEE-CNDTATEM.
       F-LEE-CNDTATEM.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       PANTALLAZOS DEL PROGRAMA                          ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
     *
       I-PANTALLA-1B.
               MOVE "   *** LIBRO DE CARTERA COMPARATIVA *** "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 10 POSITION 20
               DISPLAY "∫  DIGITE EL MES A REPORTAR :                ∫"
                                               LINE 11 POSITION 20
               DISPLAY "∫   [F2] TERMINAR                            ∫"
                                               LINE 12 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-DISPLAY.
               MOVE "   *** LIBRO DE CARTERA COMPARATIVA *** "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕª"
                                              LINE 10 POSITION 15
               DISPLAY "∫
      -       "      ∫"
                                              LINE 11 POSITION 15
               DISPLAY "∫
      -       "      ∫"
                                              LINE 12 POSITION 15
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕº"
                                              LINE 13 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-DISPLAY.
      *
       I-DISPLAY-MOV.
               MOVE "   *** LIBRO DE CARTERA COMPARATIVA *** "
               TO WK-SISTEMA
      *         PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "MOVIMIENTO DE CUENTAS...." LINE 16 POSITION 25
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕª"
                                              LINE 18 POSITION 15
               DISPLAY "∫
      -       "      ∫"
                                              LINE 19 POSITION 15
               DISPLAY "∫
      -       "      ∫"
                                              LINE 20 POSITION 15
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕº"
                                              LINE 21 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY-MOV.
      *
       I-ACEPTA-MES.
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
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               MOVE WK-NOMBRE-MES(W-LONG)    TO  MES-ACT.
               IF W-LONG = 1 OR W-LONG = 14
                  MOVE 14 TO MESW
               ELSE
                  COMPUTE MESW = W-LONG - 1.
               MOVE WK-NOMBRE-MES(MESW)      TO  MES-ANT
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.

       I-BUSCAR-VENDEDOR.
           MOVE    WIN-CODIGO-EMPRESA    TO VENDEMPR-EMPRE01
                                            CODIG-EMP-S
           PERFORM I-START-VENDEMPR-01 THRU F-START-VENDEMPR-01.
           IF FIN-ARCH-VENDEMPR = 1
                 GO TO F-BUSCAR-VENDEDOR.
           PERFORM I-LEE-C-VENDEMPR THRU F-LEE-C-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
                 GO TO F-BUSCAR-VENDEDOR.
           IF WIN-CODIGO-EMPRESA  = VENDEMPR-EMPRESA
              MOVE VENDEMPR-VENDEDO      TO HOJAVIDA-CEDULA
              PERFORM I-LEE-HOJAVIDA     THRU F-LEE-HOJAVIDA
              MOVE HOJAVIDA-CEDULA          TO WI-Z12C
              DISPLAY WI-Z12C            LINE 18 POSITION 18
              IF FIN-ARCH-HOJAVIDA = 1
                 DISPLAY "Ofic.Principal" LINE 19 POSITION 03
                 MOVE SPACES TO NOMVEND-S
              ELSE
                MOVE HOJAVIDA-NOMBRE TO NOMVEND-S
              END-IF
           ELSE
              DISPLAY "Ofic.Principal" LINE 19 POSITION 03
              MOVE "Ofic.Principal"    TO NOMVEND-S.

       F-BUSCAR-VENDEDOR. EXIT.

       I-START-VENDEMPR-01.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           START VENDEMPR KEY NOT LESS LLAVE-VENDEMPR-ALT01
            INVALID KEY
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-START-VENDEMPR-01. EXIT.
      *
       I-LEE-C-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           READ VENDEMPR NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-LEE-C-VENDEMPR. EXIT.
      *
       I-LEE-HOJAVIDA.
           MOVE ZEROS TO FIN-ARCH-HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-HOJAVIDA.
       F-LEE-HOJAVIDA. EXIT.

       I-FECHA-FORMATO.
               MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO.
       F-FECHA-FORMATO.
      *
       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS HOJAVIDA
                 INFORMES VENDEMPR
                 CNDTATEM
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG752.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\CALCDIAS.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\DISLABEL.PRO".

               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".
