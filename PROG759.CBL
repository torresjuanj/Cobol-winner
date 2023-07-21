       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG759.
      ******************************************************************
      * LIBRO DIARIO              ABR-27-2002                          *
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
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG759".
       77      ID-PRG         PIC X(7)       VALUE "PROG759".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      GENER          PIC 9          VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       77      WK-MAYOR       PIC 9(04)      VALUE ZEROS.
       77      WK-MAY         PIC 9(01)      VALUE ZEROS.
       77      W-FE           PIC ZZZZ/ZZ/ZZ   VALUE SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
        03      R-MAYCTA.
          05  R-MAY1       PIC 9.
          05  R-MAY2       PIC 9.
          05  R-MAY3       PIC 9.
          05  R-MAY4       PIC 9.
          05  R-MAY5       PIC 9.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
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
         05    TIT-CIA                   PIC X(40)      VALUE SPACES.

        03     TITULO-01.
         05    FILLER      PIC X(13)    VALUE SPACES.
      *  05    FILLER      PIC X(10)    VALUE "N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-NITEXT    PIC Z.

        03     TITULO-1.
         05    FILLER      PIC X(18)    VALUE SPACES.
         05    FILLER      PIC X(25)    VALUE
              "L I B R O   D I A R I O".

        03     TITULO-11.
         05    FILLER      PIC X(19)    VALUE SPACES.
         05    R-MES       PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(1)    VALUE SPACES.
         05    R-ANO       PIC 9999     VALUE ZEROS.
         
        03     TITULO-12.
         05    FILLER      PIC X(16)    VALUE SPACES.
         05    R-TITU      PIC X(25)    VALUE
              "TOTALES POR CUENTAS    ".

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.


        03     TITULO-3.
         05    FILLER      PIC X(93)   VALUE ALL "=".


        03     TITULO-4.
         05    FILLER      PIC X(19).
         05    T-CON.
           07  T-CON1      PIC X(23).
         05    R-NUMERO.
               07 R-ANO-NUM      PIC 9(04)       VALUE ZEROS.
               07 R-MES-NUM      PIC 99          VALUE ZEROS.
               07 R-TIPO         PIC 9(03)       VALUE ZEROS.
         05    FILLER      PIC X(5) VALUE SPACES.
         05    T-VDEB      PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
         05    FILLER      PIC X(03).
         05    T-VCRE      PIC ZZ,ZZZ,ZZZ,ZZ9.99-.

        03     TITULO-41.
         05    FILLER      PIC X(19).
         05    T-CON2      PIC X(29).
         05    FILLER      PIC X(5) VALUE SPACES.
         05    T-VDEB1     PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
         05    FILLER      PIC X(03).
         05    T-VCRE1     PIC ZZ,ZZZ,ZZZ,ZZ9.99-.

        03     TITULO-LS.
         05    FILLER                    PIC X(2)       VALUE "M".
         05    FILLER      PIC X(51).
         05    FILLER      PIC X(18) VALUE ALL "-".
         05    FILLER      PIC X(04).
         05    FILLER      PIC X(18) VALUE ALL "-".


        03     TITULO-LD.
         05    FILLER                    PIC X(2)       VALUE "M".
         05    FILLER      PIC X(51).
         05    FILLER      PIC X(18) VALUE ALL "=".
         05    FILLER      PIC X(04).
         05    FILLER      PIC X(18) VALUE ALL "=".

        03     TITULO-A.
         05    FILLER                    PIC X(2)       VALUE "M".
         05    FILLER      PIC X(08)    VALUE "COMPROB.".
         05    FILLER      PIC X(12)    VALUE "CUENTA".
         05    FILLER      PIC X(07)    VALUE SPACES.
         05    FILLER      PIC X(23)    VALUE "NOMBRE DE LA CUENTA".
         05    FILLER      PIC X(04)    VALUE SPACES.
         05    FILLER      PIC X(20)    VALUE "  D E B I T O S".
         05    FILLER      PIC X(17)    VALUE " C R E D I T O S".

        03     LINEA-DE-DETALLE.
         05    I-FECHOY-DOCTOS.
               07 I-ANO-HOY         PIC ZZZZ.
               07 I-MES-HOY         PIC 99.
               07 I-RADICTIP-DOCTOS PIC 9(03).
               07 FILLER            PIC Z(3).
         05    I-MAYCTA          PIC 9(4)     VALUE ZEROS.
         05    FILLER            PIC X(01)    VALUE SPACES.
         05    I-NOMCTA          PIC X(35)    VALUE SPACES.
         05    FILLER            PIC X(02)    VALUE SPACES.
         05    I-VDEB            PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER            PIC X(03).
         05    I-VCRE            PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.

      *************************************************************
      *         A R E A S      D E      I - C A M P O             *
      *************************************************************

               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
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
           PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
           PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
           PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
           PERFORM I-START-CNDOCTOS   THRU F-START-CNDOCTOS.
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA W-CONTA CON-LIN J
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-PRIMER-REGISTRO4 THRU F-PRIMER-REGISTRO4
           PERFORM I-BUSCA-MES    THRU F-BUSCA-MES
                               UNTIL FIN-OK = 1
           PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
           PERFORM I-TOTAL-TIP THRU F-TOTAL-TIP
      *
           PERFORM I-TOTAL-3         THRU F-TOTAL-3.
           CLOSE CNDOCTOS.
           OPEN INPUT CNDOCTOS.
           PERFORM I-START-CNDOCTO1   THRU F-START-CNDOCTO1.
           PERFORM I-TOTAL-GEN        THRU F-TOTAL-GEN
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE  1                     TO  IND-INFORMES
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE W-CODEMP               TO  LAB-CNDOCTOS-DIRCIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS CNDOCTOS CNNITCED CNCATCTA
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
       I-PANTALLA-1B.
           MOVE "*L I B R O  D I A R I O*"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY " "
                   LINE   18  POSITION 37 CONTROL SG-CONTROL-STRING
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  DIGITE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ      <F2> TERMINAR                         บ"
                                               LINE 13 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 50

               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " CODIGO PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                     BLINK
                   GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54.
       F-ACEPTA-MES.
      *
      *
       I-DIA-DESDE.
               MOVE ZEROS             TO  WK-EXCEPTION
               ACCEPT WK-DIA-DESDE        LINE 12 POSITION 50
                                           CONVERT TAB
                                          NO BEEP ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
               IF  WK-DIA-DESDE  =  0 OR WK-DIA-DESDE > 31
                   DISPLAY " DIA ERRADO ... DIGITE NUEVAMENTE"
                                            LINE 24 POSITION 1
                                             BLINK
                   GO I-DIA-DESDE.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-DIA-DESDE.
      *
       I-DIA-HASTA.
               MOVE ZEROS             TO  WK-EXCEPTION
               ACCEPT WK-DIA-HASTA        LINE 13 POSITION 50
                                           CONVERT TAB
                                          NO BEEP ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
               IF  WK-DIA-HASTA  =  0 OR WK-DIA-HASTA > 31
                   DISPLAY " DIA ERRADO ... DIGITE NUEVAMENTE"
                                            LINE 24 POSITION 1
                                             BLINK
                   GO I-DIA-HASTA.
               IF  WK-DIA-HASTA  <   WK-DIA-DESDE
                   DISPLAY " EL DIA ES MENOR QUE EL DIA DESDE"
                                            LINE 24 POSITION 1
                                             BLINK
                   GO I-DIA-HASTA.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-DIA-HASTA.
      *
       I-PRIMER-REGISTRO4.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
           MOVE ALTTIP08-DOCTOS        TO   W-ALTTIP-DOCTOS
           MOVE ALTMES-DOCTOS          TO   W-ALTMES-DOCTOS
           MOVE ALTFRA-DOCTOS          TO   W-ALTFRA-DOCTOS
           MOVE NUMFRA-DOCTOS          TO   W-NUMFRA-DOCTOS
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
       F-PRIMER-REGISTRO4.
      *
       I-BUSCA-MES.
           IF FIN-OK = 1   GO F-BUSCA-MES.
           IF ALTTIP08-DOCTOS = W-ALTTIP-DOCTOS
              IF WK-MAYOR        =  MAYCTA-DOCTOS
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS
                     GO I-BUSCA-MES
              ELSE
                 PERFORM I-TOTAL-DTO  THRU F-TOTAL-DTO
                 PERFORM I-FRA-NUEVO  THRU F-FRA-NUEVO
                 GO I-BUSCA-MES
           ELSE
                    PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
                    PERFORM I-TOTAL-TIP THRU F-TOTAL-TIP
                    PERFORM I-FRA-NUEVO THRU F-FRA-NUEVO
                    GO I-BUSCA-MES.
       F-BUSCA-MES.
      *
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                           LINE 24 POSITION 1

                    DISPLAY "       [F2] TERMINAR " LINE 24 POSITION 28

                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
               GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
       I-START-CNDOCTOS.
               MOVE ZEROS                  TO  ALT04-DOCTOS
               START CNDOCTOS  KEY >   ALT04-DOCTOS INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDOCTOS.
      *
       I-START-CNDOCTO1.
               MOVE ZEROS                  TO  ALT01-DOCTOS
               START CNDOCTOS  KEY >   ALT01-DOCTOS INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDOCTO1.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************

       I-LEE-CNDOCTOS.
              DISPLAY ALTTIP08-DOCTOS        LINE 20 POSITION 36
              DISPLAY ALTCTA-DOCTOS          LINE 20 POSITION 56
              DISPLAY ALTMES-DOCTOS          LINE 21 POSITION 36
              DISPLAY RADIC-DOCTOS           LINE 22 POSITION 36
              DISPLAY NUMFRA-DOCTOS          LINE 21 POSITION 56
              MOVE    FECHA-CONTAB-DOCTOS     TO  W-FE
              DISPLAY W-FE                  LINE 22 POSITION 61
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                        MOVE 1 TO FIN-OK.
              IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
              IF CONDICION-DOCTOS = 0 GO I-LEE-CNDOCTOS.
                 MOVE ALTNIT-DOCTOS TO CODIM
                 PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED.
              IF GENER  =    5
                 MOVE    ZEROS     TO MAYCTA-3-DOCTOS
                 MOVE    ZEROS     TO MAYCTA-4-DOCTOS.
       F-LEE-CNDOCTOS.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO      W-NOMBRE.
       F-LEE-EMPRESAS.
      *
       I-LEE-CNNITCED.
               MOVE 0        TO SW-EOF.
               READ CNNITCED WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO      W-NOMBRE.
       F-LEE-CNNITCED.

       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               MOVE  MAYCTA-DOCTOS     TO   MAYMAE WK-MAYOR
               MOVE  ZEROS             TO   SUBMAE
               MOVE  ZEROS             TO   AUXMAE.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE NOMCTA        TO    I-NOMCTA.
       F-LEE-CNCATCTA.
      *
       I-LEE1-CNCATCTA.
               MOVE 0        TO SW-EOF.
               MOVE  MAYCTA-1-DOCTOS   TO   MAY1.
               MOVE  MAYCTA-2-DOCTOS   TO   MAY2.
               MOVE  ZEROS             TO   MAY3.
               MOVE  ZEROS             TO   MAY4.
               MOVE  MAYMAE            TO   WK-MAYOR.
               MOVE  ZEROS             TO   SUBMAE
               MOVE  ZEROS             TO   AUXMAE.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE NOMCTA        TO    I-NOMCTA.
       F-LEE1-CNCATCTA.
      *
       I-DISPLAY.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY " "
                   LINE   18  POSITION 37 CONTROL SG-CONTROL-STRING

               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ TIPO COMPROBANTE :       NR.CUENTA   :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ NUMERO DE MES    :       NR. FACTURA :
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ SECUENCIA REGISTR:                 FECHA:
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 23 POSITION 15
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY WK-ESPACIOS
                   LINE   24  POSITION 01 CONTROL SG-CONTROL-STRING
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY " "
                   LINE   18  POSITION 37 CONTROL SG-CONTROL-STRING
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-DISPLAY.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO MOVIMIENTO DE CAMPOS...."
                  LINE 24 POSITION 1.
           IF GENER  =    5
              MOVE    ZEROS     TO MAYCTA-3-DOCTOS
              MOVE    ZEROS     TO MAYCTA-4-DOCTOS.
           MOVE ALTCTA-MAYOR-DOCTOS    TO I-MAYCTA
           MOVE RADICTIP-DOCTOS        TO I-RADICTIP-DOCTOS R-TIPO.
           MOVE CON-ANO                TO R-ANO-NUM I-ANO-HOY
           MOVE W-LONG                 TO R-MES-NUM I-MES-HOY
           PERFORM I-EVALUE-VALOR THRU F-EVALUE-VALOR.
           ADD 1                      TO W-CONTA.
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
            IF VALOR-DOCTOS = ZEROS
               MOVE 0             TO I-VDEB
               MOVE 0             TO I-VCRE.
       F-EVALUE-VALOR.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
       I-FRA-NUEVO.
           MOVE ALTTIP08-DOCTOS        TO   W-ALTTIP-DOCTOS
           MOVE ALTFRA-DOCTOS          TO   W-ALTFRA-DOCTOS
           MOVE ALTMES-DOCTOS          TO   W-ALTMES-DOCTOS
           MOVE NUMFRA-DOCTOS          TO   W-NUMFRA-DOCTOS
           IF GENER =  5
               PERFORM I-LEE1-CNCATCTA   THRU F-LEE1-CNCATCTA
            ELSE
               PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
       F-FRA-NUEVO.

      *
       I-TOTAL-DTO.
                MOVE W-TOTAL-DEBITOS        TO I-VDEB.
                MOVE W-TOTAL-CREDITOS       TO I-VCRE.
                IF CON-LIN > 54
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                MOVE SPACES TO REG-INFORMES
                ADD 1              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
       F-TOTAL-DTO.

       I-TOTAL-TIP.
                MOVE "TOTAL COMPROBANTE No. "  TO T-CON
                MOVE T-TOTAL-DEBITOS        TO T-VDEB.
                MOVE T-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 54
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4  AFTER 1
                
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES                AFTER 1
                ADD 4              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
       F-TOTAL-TIP.
      *
       I-TOTAL-GEN.
           IF CON-LIN > 44
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
           MOVE SPACES               TO   REG-INFORMES
           WRITE REG-INFORMES             AFTER 1
           WRITE REG-INFORMES      FROM TITULO-12        AFTER 1
           
           MOVE SPACES TO REG-INFORMES
           WRITE REG-INFORMES                       AFTER 1
           ADD 3              TO CON-LIN
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS FIN-OK
                          T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                          G-TOTAL-DEBITOS G-TOTAL-CREDITOS.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
           IF GENER =  5
               PERFORM I-LEE1-CNCATCTA   THRU F-LEE1-CNCATCTA
            ELSE
               PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
           PERFORM I-TOTAL-2         THRU F-TOTAL-2 UNTIL
                                    FIN-OK = 1.
           PERFORM I-TOTAL-PAR       THRU F-TOTAL-PAR.
           PERFORM I-TOTAL-3         THRU F-TOTAL-3.
       F-TOTAL-GEN.

       I-TOTAL-2.
           IF FIN-OK = 1   GO F-TOTAL-2.
           IF WK-MAYOR   =  MAYCTA-DOCTOS
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS
                     GO I-TOTAL-2
              ELSE
                 PERFORM I-TOTAL-PAR  THRU F-TOTAL-PAR
                 PERFORM I-FRA-NUEVO THRU  F-FRA-NUEVO
                 GO I-TOTAL-2.
       F-TOTAL-2.
      *
       I-TOTAL-PAR.
                MOVE W-TOTAL-DEBITOS        TO I-VDEB.
                MOVE W-TOTAL-CREDITOS       TO I-VCRE.
                MOVE SPACES                 TO I-FECHOY-DOCTOS.
                IF CON-LIN > 54
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
               IF GENER  =    5
                  MOVE I-MAYCTA     TO R-MAYCTA
                  MOVE    ZEROS     TO R-MAY3
                  MOVE    ZEROS     TO R-MAY4
                  MOVE    ZEROS     TO R-MAY5
                  MOVE  R-MAYCTA    TO I-MAYCTA.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                MOVE SPACES TO REG-INFORMES
                ADD 1              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
       F-TOTAL-PAR.


       I-TOTAL-3.
                MOVE ZEROS                  TO R-NUMERO.
                MOVE "T O T A L   G E N E R A  L "    TO T-CON2
                MOVE G-TOTAL-DEBITOS        TO T-VDEB1
                MOVE G-TOTAL-CREDITOS       TO T-VCRE1
                IF CON-LIN > 54
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-41 AFTER 1
                
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 4              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
        F-TOTAL-3.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
       I-TITULO-INFORME.
           MOVE  WK-NOMBRE-MES(W-LONG)   TO  R-MES
           COMPUTE R-ANO     = CON-ANO + 1900.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           IF  J  = ZEROS
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER 1
               
               MOVE   1            TO     J
            ELSE
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE.
               WRITE REG-INFORMES  FROM  TITULO-01 AFTER 1
               MOVE SPACES TO REG-INFORMES
               WRITE REG-INFORMES AFTER 2
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-11 AFTER 1
               
            WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
           
            WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
            WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
            WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
            MOVE 09                         TO  CON-LIN.
       F-TITULO-INFORME.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
            MOVE SPACES TO REG-INFORMES
            WRITE REG-INFORMES                  AFTER PAGE.
           CLOSE CNNITCED
                 EMPRESAS
                 CNCATCTA
                 INFORMES
                 CNDOCTOS.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG759.COB"
      *      CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *      CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\INFORMES.PLB".

