       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG741.
      ******************************************************************
      * LISTADO DEL P.U.C   ABR-27-2002                                           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG741".
       77      ID-PRG         PIC X(7)       VALUE "PROG741".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
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
         05    TIT-CIA     PIC X(45)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(06)    VALUE SPACES.
         05    FILLER      PIC X(21)    VALUE
              "PLAN UNICO DE CUENTAS".

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).
         05    FILLER      PIC X(39)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina :".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(01)    VALUE SPACES.
      *  05    I-CAMPO     PIC X(100)   VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(69)   VALUE ALL "_".


        03     TITULO-4.
         05    FILLER      PIC X(05).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(10).
         05    FILLER      PIC X(06).
         05    T-VDEB      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(02).
         05    T-VCRE      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(03).


        03     TITULO-LS.
         05    FILLER      PIC X(05).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).


        03     TITULO-LD.
         05    FILLER      PIC X(05).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).

        03     TITULO-A.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(07)    VALUE " CODIGO".
         05    FILLER      PIC X(02)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(41)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "N".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "T".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "D".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "B".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE "  %  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "T".
         05    FILLER      PIC X(01)    VALUE "|".

       03      TITULO-A1.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(07)    VALUE " CUENTA".
         05    FILLER      PIC X(02)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE SPACES.
         05    FILLER      PIC X(13)    VALUE "NOMBRE CUENTA".
         05    FILLER      PIC X(23)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "I".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "E".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "E".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "A".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE " BASE".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "I".
         05    FILLER      PIC X(01)    VALUE "|".

       03      TITULO-A2.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(09)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(41)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "V".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "R".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "T".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "S".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE "GRAVB".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(01)    VALUE "T".
         05    FILLER      PIC X(01)    VALUE "|".

       03     LINEA-DE-DETALLE.
         05    I-MAY1      PIC 9(01).
         05    I-MAY2      PIC 9(01).
         05    I-MAY3      PIC 9(01).
         05    I-MAY4      PIC 9(01).
         05    I-SUBMAE    PIC 9(02).
         05    I-AUX1      PIC 9(02).
         05    I-AUX2      PIC 9(02).
         05    FILLER      PIC X(02).
         05    I-NOMCTA    PIC X(40).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-NIVEL     PIC X(01).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-CODPGT    PIC X(01).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-INDDETAL  PIC X(01).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-INDBASE   PIC X(01).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-PORC-CTA  PIC Z(05).
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-ESTACTA   PIC X(01).
         05    FILLER      PIC X(01)    VALUE "|".

        03     TITULO-TOTAL.
         05    FILLER      PIC X(17)  VALUE "TOTAL CUENTAS:".
         05    T-TOTAL     PIC ZZZZZ.
         05    T-TOTFAC    PIC $ZZZ,ZZZ.
         05    T-TOTBAS    PIC $ZZZ,ZZZ.
         05    FILLER      PIC X(11)    VALUE "  PROGRAMA:".
         05    FILLER      PIC X(01)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(08)    VALUE SPACES.

      *************************************************************
      *         A R E A S      D E      I - C A M P O             *
      *************************************************************


        03     AREA-IMP1.
         05    FILLER      PIC X(59)            VALUE
               "INFORME GENERAL ORGANIZADO POR COMPROBANTE - MES - FACTU
      -        "RA".

        03     AREA-IMP2.
         05    FILLER      PIC X(59)            VALUE
               "INFORME GENERAL ORGANIZADO POR COMPROBANTE - MES - CUENT
      -        "A".

        03     AREA-IMP3.
         05    FILLER      PIC X(37)  VALUE
              "INFORME ORGANIZADO POR COMPROBANTE :".
         05    FILLER      PIC X(03)  VALUE SPACES.
         05    I-TCOMP     PIC 9(01)  VALUE ZEROS.
         05    FILLER      PIC X(03)  VALUE SPACES.
         05    I-NCOMP     PIC X(25)  VALUE SPACES.

       03      AREA-IMP4.
         05    FILLER      PIC X(30)    VALUE
              "INFORME GENERADO PARA EL MES :".
         05    FILLER      PIC X(03)    VALUE SPACES.
         05    I-MES4      PIC X(10)    VALUE SPACES.

        03     AREA-IMP5.
         05    FILLER      PIC X(30)    VALUE SPACES.
         05    FILLER      PIC X(17)    VALUE
              "INFORME GENERADO".
         05    FILLER      PIC X(14)    VALUE
              "DE LA FECHA : ".
         05    I-DIA1      PIC 99       VALUE ZEROS.
         05    FILLER      PIC X        VALUE "-".
         05    I-MES1      PIC X(3)     VALUE SPACES.
         05    FILLER      PIC X(3)     VALUE "-19".
         05    I-ANO1      PIC 99       VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(8)     VALUE "HASTA : ".
         05    I-DIA2      PIC 99       VALUE ZEROS.
         05    FILLER      PIC X        VALUE "-".
         05    I-MES2      PIC X(3)     VALUE SPACES.
         05    FILLER      PIC X(3)     VALUE "-19".
         05    I-ANO2      PIC 99       VALUE ZEROS.

        03     AREA-IMP6.
         05    FILLER      PIC X(34).
         05    FILLER      PIC X(31)    VALUE
               "REPORTE DE LA EMPRESA DE NIT : ".
         05    I-NEMPRESA  PIC ZZZ,ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(10).
         05    FILLER      PIC X(14)    VALUE
              "DE LA FECHA : ".
         05    I-DIAD      PIC 9(02).
         05    FILLER      PIC X(01)     VALUE "-".
         05    I-MESD      PIC X(03).
         05    FILLER      PIC X(03)     VALUE "-19".
         05    I-ANOD      PIC 9(02).
         05    FILLER      PIC X(10).
         05    FILLER      PIC X(08)     VALUE "HASTA : ".
         05    I-DIAH      PIC 9(02).
         05    FILLER      PIC X(01)     VALUE "-".
         05    I-MESH      PIC X(03).
         05    FILLER      PIC X(03)     VALUE "-19".
         05    I-ANOH      PIC 9(02).


      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
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
           PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
            MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS W-TOTAL
                    SW1  S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA W-CONTA CON-LIN
           PERFORM I-START-CNCATCTA   THRU F-START-CNCATCTA
           PERFORM I-PANTALLA-1C      THRU F-PANTALLA-1C.
           PERFORM I-CONFIRMA-COMP    THRU F-CONFIRMA-COMP.
           PERFORM I-START-CNCATCTA   THRU F-START-CNCATCTA
           PERFORM I-PRIMER-CNCATCTA  THRU F-PRIMER-CNCATCTA
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-BUSCA-CNCATCTA   THRU F-BUSCA-CNCATCTA
                               UNTIL FIN-OK = 1.
           PERFORM I-TOTAL-INFORME    THRU F-TOTAL-INFORME.
           PERFORM I-FIN-MODE    THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE W-PROGRAMA             TO  I-PROGRAMA
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE     1                  TO  IND-INFORMES
           PERFORM I-LABEL-INFORMES   THRU F-LABEL-INFORMES.
      *    MOVE    ZEROS               TO  LAB-INFORMES-PERIODO
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN I-O    CNCATCTA EMPRESAS
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
       I-ACEPTA-CTA-DESDE.
               MOVE ZEROS                  TO  WK-EXCEPTION.
       I-WCTAD.
               DISPLAY  "    " LINE 11 POSITION 52
                                     REVERSE.
               ACCEPT W-CTAD  LINE 11 POSITION 52
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAD TO MAYMAE
               MOVE 0      TO SUBMAE AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               DISPLAY NOMCTA  LINE 12 POSITION 30 REVERSE SIZE 25
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                   LINE 24 POSITION 1 REVERSE BLINK GO I-WCTAD.
               DISPLAY WK-ESPACIOS  LINE 24 POSITION 1.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               GO F-ACEPTA-CTA-DESDE.
       I-WSUBD.
               ACCEPT W-SUBD  LINE 11 POSITION 57
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAD TO MAYMAE
               MOVE W-SUBD TO SUBMAE
               MOVE 0      TO AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WSUBD.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1
               IF  CUP GO I-WCTAD.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       I-WAUXD.
               ACCEPT W-AUXD  LINE 11 POSITION 59
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAD TO MAYMAE
               MOVE W-SUBD TO SUBMAE
               MOVE W-AUXD TO AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WAUXD.
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 1.
               IF  CUP GO I-WSUBD.
               DISPLAY NOMCTA                     LINE 12 POSITION 23
                                                  REVERSE.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ACEPTA-CTA-DESDE.
      *
       I-ACEPTA-CTA-HASTA.
               MOVE ZEROS                  TO  WK-EXCEPTION.
       I-WCTAH.
               DISPLAY  "    " LINE 14 POSITION 52
                                     REVERSE.
               ACCEPT W-CTAH    LINE 14 POSITION 52
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAH TO MAYMAE
               MOVE 0      TO SUBMAE AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WCTAH.
               IF CUP GO I-WCTAH.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
               IF W-CTAH < W-CTAD
                  DISPLAY
                  "LA ESTA CUENTA NO PUEDE SE MENOR A LA CUENTA DESDE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                  GO I-WCTAH.
               DISPLAY NOMCTA  LINE 15 POSITION 30 REVERSE SIZE 25
               GO F-ACEPTA-CTA-HASTA.
       I-WSUBH.
               ACCEPT W-SUBH  LINE 14 POSITION 57
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAH TO MAYMAE
               MOVE W-SUBH TO SUBMAE
               MOVE 0      TO AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                  DISPLAY " CUENTA ERRADA .....DIGITE NUEVAMENTE"
                                               LINE 24 POSITION 1
                                               REVERSE BLINK
                  GO I-WSUBH.
               IF CUP GO I-WCTAH.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY NOMCTA             LINE 15 POSITION 25
                                               REVERSE BLINK.
               DISPLAY WK-ESPACIOS              LINE 24 POSITION 1.
       I-WAUXH.
               ACCEPT W-AUXH  LINE 14 POSITION 59
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               MOVE W-CTAH TO MAYMAE
               MOVE W-SUBH TO SUBMAE
               MOVE W-AUXH TO AUXMAE
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WAUXH.
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 1.
               IF  CUP GO I-WSUBH.
               DISPLAY NOMCTA                     LINE 15 POSITION 25
                                                  REVERSE BLINK.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ACEPTA-CTA-HASTA.

       I-PRIMER-CNCATCTA.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
           PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PRIMER-CNCATCTA.
      *
       I-BUSCA-CNCATCTA.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF FIN-OK = 1   GO F-BUSCA-CNCATCTA.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-BUSCA-CNCATCTA.
      *
       I-CONFIRMA-COMP.
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4
               MOVE   1      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  F5
               PERFORM I-PANTALLA-1D      THRU F-PANTALLA-1D
               PERFORM I-ACEPTA-CTA-DESDE THRU
                       F-ACEPTA-CTA-DESDE
               PERFORM I-ACEPTA-CTA-HASTA THRU
                       F-ACEPTA-CTA-HASTA
               MOVE   2      TO LL
               GO TO F-CONFIRMA-COMP.
           GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-CNCATCTA.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE W-CTADESDE             TO  CTAMAE
               START CNCATCTA  KEY >   CTAMAE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNCATCTA.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNCATCTA.
              IF LL NOT =  ZEROS
              DISPLAY CTAMAE                 LINE 20 POSITION 28
              DISPLAY NOMCTA                 LINE 21 POSITION 16.
              READ CNCATCTA NEXT RECORD WITH NO LOCK AT END
                                 MOVE 1 TO FIN-OK.
      *SE UTILIZAN PARA AGILIZAR EL PROCESO DE DIGITACION
      *       IF MAYMAE > 5100 AND MAYMAE < 5200
      *          MOVE 2 TO MAY2
      *          WRITE CNMAES
      *          GO I-LEE-CNCATCTA.

              IF  LL =  0 OR  LL = 1  GO F-LEE-CNCATCTA.
              IF  CTAMAE < W-CTADESDE  GO I-LEE-CNCATCTA.
              IF  CTAMAE > W-CTAHASTA  MOVE 1  TO FIN-OK.
       F-LEE-CNCATCTA.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-EMPRESAS.
      *
       I-DISPLAY.
      *NJ      PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ No.CUENTA :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.

       I-PANTALLA-1C.
               MOVE "        *** CATALOGO DE CUENTAS - PUC *** "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ   [F4]    INFORME GENERAL                  บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ   [F5]    INFORME ESPECIFICO               บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 14 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 15 POSITION 20
               DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
               DISPLAY " "  LINE 07 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 07 POSITION 2 SIZE 78
               "CATALOGO DE CUENTAS ------->" LINE 07 POSITION 05
                LABEL-INFORMES                LINE 07 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 07 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-PANTALLA-1C.
      *
       I-PANTALLA-1D.
               MOVE "       *** CATALOGO DE CUENTAS - PUC  *** "
               TO WK-SISTEMA
      *NJ      PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE No.CUENTA DESDE  :                 บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ  TECLEE No.CUENTA HASTA  :                 บ"
                                               LINE 14 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 15 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 16 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 17 POSITION 20
               DISPLAY "          [F2]   TERMINAR PROCESO             "
                                               LINE 19 POSITION 20
               DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.
       F-PANTALLA-1D.
      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE MAY1         TO I-MAY1
            MOVE MAY2         TO I-MAY2
            MOVE MAY3         TO I-MAY3
            MOVE MAY4         TO I-MAY4
            MOVE SUBMAE       TO I-SUBMAE
            MOVE AUX1         TO I-AUX1
            MOVE AUX2         TO I-AUX2
            MOVE NOMCTA       TO I-NOMCTA

            IF ESTACTA = "S"
               MOVE ESTACTA      TO I-ESTACTA
            ELSE
               MOVE SPACES       TO I-ESTACTA.

            IF CODPGT = "S"
               MOVE CODPGT       TO I-CODPGT
            ELSE
               MOVE SPACES TO I-CODPGT.

            IF NIVEL > 3
               MOVE NIVEL        TO I-NIVEL
            ELSE
               MOVE SPACES       TO I-NIVEL.

            IF INDDETAL = "S"
               MOVE INDDETAL     TO I-INDDETAL
            ELSE
               MOVE SPACES       TO I-INDDETAL.

            IF INDBASE = "S"
               MOVE INDBASE      TO I-INDBASE
            ELSE
               MOVE SPACES       TO I-INDBASE.


            MOVE PORC-CTA     TO I-PORC-CTA
            IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF NIVEL = 1 OR NIVEL = 2
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                
                ADD 2 TO CON-LIN
            ELSE
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO CON-LIN.
            ADD 1                      TO W-CONTA.
       F-MUEVE-INFORME-1.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
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

       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
     
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
      
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A1 AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A2 AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               MOVE 7                          TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
               MOVE 0              TO   T-TOTFAC
               MOVE 0              TO   T-TOTBAS
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 1
               WRITE REG-INFORMES  FROM TITULO-3     AFTER 1.
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 1.
               WRITE REG-INFORMES  FROM TITULO-3     AFTER 1.
       F-TOTAL-INFORME.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE EMPRESAS
                 INFORMES
                 CNCATCTA.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE ZEROS                  TO  CON-PERIODO
           CANCEL "PROG741.COB"
      *     CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *     CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PLB\INFORMES.PLB".
