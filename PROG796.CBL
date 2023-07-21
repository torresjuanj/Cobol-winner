       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG796.
      ******************************************************************
      * COMPROBANTE DE DIARIO MENSUAL    ABR-27-2002                                *
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
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG796".
       77      ID-PRG         PIC X(7)       VALUE "PROG796".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       77      WK-MAYOR       PIC 9(04)      VALUE ZEROS.
       77      W-FE           PIC ZZZZ/ZZ/ZZ   VALUE SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
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
         05    TIT-CIA     PIC X(40)   VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(16)   VALUE SPACES.
         05    FILLER      PIC X(25)   VALUE "COMPROBANTE DE DIARIO ".

        03     TITULO-11.
         05    FILLER      PIC X(17)   VALUE SPACES.
         05    R-MES       PIC X(10)   VALUE SPACES.
         05    FILLER      PIC X(1)    VALUE SPACES.
         05    R-ANO       PIC 9999    VALUE ZEROS.
         
       03      TITULO-FE1.
         05    FILLER      PIC X(75)   VALUE SPACES.
         05    FILLER      PIC X(08)   VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ    VALUE ZEROS.
      *
        03     TITULO-2.
         05    FILLER      PIC X(020)   VALUE "COMPROBANTE No. :".
         05    I-NUMERO.
               07 I-ANO    PIC 9(04).
               07 I-MES    PIC 9(02).
               07 FILLER   PIC X(01)    VALUE "-".
               07 I-TIP    PIC 9(03).
         05    FILLER      PIC X(02).
         05    I-NCOMP     PIC X(25)    VALUE SPACES.
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15).
         05    FILLER      PIC X(04)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(30)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(157)   VALUE ALL "=".
      *
        03     TITULO-4.
         05    FILLER      PIC X(31).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(10).
         05    FILLER      PIC X(04).
         05    T-VDEB      PIC ZZZZ,ZZZ,ZZ9.99-.
         05    FILLER      PIC X(01).
         05    T-VCRE      PIC ZZZZ,ZZZ,ZZ9.99-.

        03     TITULO-LS.
         05    FILLER      PIC X(60).
         05    FILLER      PIC X(16) VALUE ALL "-".
         05    FILLER      PIC X(01).
         05    FILLER      PIC X(16) VALUE ALL "-".

        03     TITULO-LD.
         05    FILLER      PIC X(60).
         05    FILLER      PIC X(16) VALUE ALL "=".
         05    FILLER      PIC X(01).
         05    FILLER      PIC X(16) VALUE ALL "=".

        03     TITULO-A.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(12)    VALUE "   CODIGO   ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE " DOCTO".
         05    FILLER      PIC X(10)    VALUE "|".
         05    FILLER      PIC X(26)    VALUE "   C O N C E P T O ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(17)    VALUE "  V A L O R E S  ".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(14)    VALUE "   N.I.T.  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(20)    VALUE " NOMBRE DEL CLIENTE ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE " FACT.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "     NUMERO     ".
         05    FILLER      PIC X(01)    VALUE "|".

       03      TITULO-A1.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(12)    VALUE "  CONTABLE  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(35)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    DEBITOS     ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    CREDITOS    ".
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(04)    VALUE "EMP".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(14)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(20)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE "NUMERO".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    SECUENCIA   ".
         05    FILLER      PIC X(01)    VALUE "|".

        03     LINEA-DE-DETALLE.
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-MAYCTA    PIC 9(04).
         05    FILLER      PIC X(01).
         05    I-SUB       PIC 9(02).
         05    FILLER      PIC X(01).
         05    I-AUX       PIC 9(04).
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-FRA       PIC Z(06).
         05    FILLER      PIC X(02)    VALUE SPACES.
         05    I-CON       PIC X(34).
         05    FILLER      PIC X(01).
         05    I-VDEB      PIC ZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(02).
         05    I-VCRE      PIC ZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(01).
         05    I-CODEMP    PIC 9(07).
         05    FILLER      PIC X(01).
         05    I-NIT       PIC ZZZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(02).
         05    I-EMP       PIC X(20).
         05    FILLER      PIC X(01).
         05    I-NUMFRA          PIC Z(06).
         05    FILLER            PIC X(04) VALUE SPACES.
         05    I-RADICTIP-DOCTOS PIC 9(03).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-RADICDOC-DOCTOS PIC 9(06).
         05    FILLER            PIC X(01)  VALUE SPACES.

        03     TITULO-9.
          05    FILLER      PIC X(01)    VALUE " ".
         05    I-MAYCTA1   PIC 9(04).
         05    FILLER      PIC X(01).
         05    I-SUB1      PIC 9(02).
         05    FILLER      PIC X(01).
         05    I-AUX1      PIC 9(04).
         05    FILLER      PIC X(07)    VALUE SPACES.
         05    I-DETA      PIC X(40).

      *
       03      TITULO-FE5.
         05    FILLER             PIC  X(42)     VALUE SPACES.
         05    FILLER             PIC  X(04) VALUE "DEL ".
         05    I-FECHA-DESDE.
           07  I-ANO-DESDE        PIC  9(04) VALUE ZEROS.
           07  FILLER             PIC  X  VALUE  "/".
           07  I-MES-DESDE        PIC  99 VALUE ZEROS.
           07  FILLER             PIC  X  VALUE  "/".
           07  I-DIA-DESDE        PIC  99 VALUE ZEROS.
           07  FILLER             PIC  X(04) VALUE " AL ".
         05    I-FECHA-HASTA.
           07  I-ANO-HASTA        PIC  9(04) VALUE ZEROS.
           07  FILLER             PIC  X  VALUE  "/".
           07  I-MES-HASTA        PIC  99 VALUE ZEROS.
           07  FILLER             PIC  X  VALUE  "/".
           07  I-DIA-HASTA        PIC  99 VALUE ZEROS.
         05    FILLER             PIC X(09)  VALUE SPACES.
         05    FILLER             PIC X(08)  VALUE "Pagina: ".
      *JJ05    I-PAGINA           PIC ZZZZ   VALUE ZEROS.



               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
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
           PERFORM I-DATOS-CAB       THRU F-DATOS-CAB.
           PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA W-CONTA CON-LIN
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-START-CNDOCTOS   THRU F-START-CNDOCTOS.
           PERFORM I-PRIMER-REGISTRO4 THRU F-PRIMER-REGISTRO4
           PERFORM I-BUSCA-MES    THRU F-BUSCA-MES
                               UNTIL FIN-OK = 1
           MOVE "TOTAL CUENTA         : " TO T-CON
           PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
      *JJT PERFORM I-TOTAL-MES THRU F-TOTAL-MES
           PERFORM I-TOTAL-TIP THRU F-TOTAL-TIP
           PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
      *JJT PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE     1                  TO   IND-INFORMES
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE W-CODEMP               TO  LAB-CNDOCTOS-DIRCIA
           MOVE LABEL-INFORMES         TO  I-PROGRAMA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
           PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS CNDOCTOS CNCOMPTE CNNITCED CNCATCTA
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
       I-PANTALLA-1B.
           MOVE "**COMPROBANTE DE DIARIO MENSUAL**"
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
               DISPLAY "บ             DIA  DESDE    :                บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ             DIA  HASTA    :                บ"
                                               LINE 13 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20.
       F-PANTALLA-1B.
      *
       I-DATOS-CAB.
        CAB-1.
           PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
                   IF                 CUP  GO CAB-1.
                   IF                 ESC
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
         CAB-2.
           PERFORM I-DIA-DESDE       THRU F-DIA-DESDE
                   IF                 CUP  GO CAB-1.
                   IF                 ESC
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
         CAB-3.
           PERFORM I-DIA-HASTA       THRU F-DIA-HASTA
                   IF             CUP  GO CAB-2.
                   IF                 ESC
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
         CAB-4.
           PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
           IF CUP  GO CAB-1.
       F-DATOS-CAB.

       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 50

               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               IF  W-LONG  =  0 OR W-LONG > 13
                   DISPLAY " CODIGO PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                     BLINK
                   GO I-ACEPTA-MES.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54.
       F-ACEPTA-MES.
      *
       I-DIA-DESDE.
               MOVE ZEROS             TO  WK-EXCEPTION
               ACCEPT WK-DIA-DESDE        LINE 12 POSITION 50
                                           CONVERT TAB
                                          NO BEEP ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               IF            CUP  GO F-DIA-DESDE.
               IF  WK-DIA-DESDE  =  0 OR WK-DIA-DESDE > 31
                   DISPLAY " DIA ERRADO ... DIGITE NUEVAMENTE"
                                            LINE 24 POSITION 1
                                             BLINK
                   GO I-DIA-DESDE.
       F-DIA-DESDE.
      *
       I-DIA-HASTA.
               MOVE ZEROS             TO  WK-EXCEPTION
               ACCEPT WK-DIA-HASTA        LINE 13 POSITION 50
                                           CONVERT TAB
                                          NO BEEP ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
               IF  CUP                    GO F-DIA-HASTA.
               IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
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
       F-DIA-HASTA.
      *
       I-PRIMER-REGISTRO4.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
           MOVE ALTTIP-DOCTOS          TO   W-ALTTIP-DOCTOS
           MOVE ALTMES-DOCTOS          TO   W-ALTMES-DOCTOS
           MOVE ALTFRA-DOCTOS          TO   W-ALTFRA-DOCTOS
           MOVE NUMFRA-DOCTOS          TO   W-NUMFRA-DOCTOS
           MOVE ALTTIP-DOCTOS          TO   LLAVM1-COMP
           PERFORM I-TIPO   THRU F-TIPO
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS.
       F-PRIMER-REGISTRO4.
      *
       I-BUSCA-MES.
           IF FIN-OK = 1   GO F-BUSCA-MES.
           IF ALTTIP-DOCTOS = W-ALTTIP-DOCTOS
              IF WK-MAYOR        =  MAYCTA-DOCTOS
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS
                     GO I-BUSCA-MES
              ELSE
                 MOVE "TOTAL CUENTA         : " TO T-CON
                 PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
                 PERFORM I-FRA-NUEVO THRU F-FRA-NUEVO
                 GO I-BUSCA-MES
           ELSE
                    MOVE "TOTAL CUENTA         : " TO T-CON
                    PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
                    PERFORM I-TOTAL-TIP THRU F-TOTAL-TIP
                    MOVE ALTTIP-DOCTOS       TO I-TIP  LLAVM1-COMP
                    PERFORM I-TIPO           THRU F-TIPO
                    PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
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
           IF      CUP     GO F-CONFIRMA-COMP.
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
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************

       I-LEE-CNDOCTOS.
              IF FIN-OK  = 1            GO   F-LEE-CNDOCTOS.
              MOVE  ZEROS               TO   FIN-OK.
              DISPLAY ALTTIP-DOCTOS          LINE 20 POSITION 36
              DISPLAY ALTCTA-DOCTOS          LINE 20 POSITION 56
              DISPLAY ALTMES-DOCTOS          LINE 21 POSITION 36
              DISPLAY RADIC-DOCTOS           LINE 22 POSITION 36
              DISPLAY NUMFRA-DOCTOS          LINE 21 POSITION 56
              MOVE    FECHA-CONTAB-DOCTOS     TO  W-FE
              DISPLAY W-FE                  LINE 22 POSITION 61
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK.
              IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
              MOVE SPACES TO I-EMP.
              IF ALTNIT-DOCTOS > ZEROS
                 MOVE ALTNIT-DOCTOS TO CODIM
                 PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
                    IF SW-EOF = 1
                       MOVE "C.COSTO NO EXISTE" TO I-EMP
                    ELSE
                       MOVE NOMBM TO I-EMP.
       F-LEE-CNDOCTOS.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO    I-EMP     W-NOMBRE.
       F-LEE-EMPRESAS.
      *
       I-LEE-CNNITCED.
               MOVE 0        TO SW-EOF.
               READ CNNITCED WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO    I-EMP     W-NOMBRE.
       F-LEE-CNNITCED.

       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               MOVE  MAYCTA-DOCTOS     TO   MAYMAE WK-MAYOR I-MAYCTA1
               MOVE  ZEROS             TO   SUBMAE I-SUB1
               MOVE  ZEROS             TO   AUXMAE I-AUX1.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE NOMCTA        TO    I-DETA.
               WRITE REG-INFORMES FROM TITULO-9 AFTER 1
               
               MOVE SPACES TO REG-INFORMES
               ADD 1              TO CON-LIN.
               MOVE  SUBCTA-DOCTOS     TO   SUBMAE I-SUB1
               MOVE  ZEROS             TO   AUXMAE I-AUX1.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE NOMCTA        TO    I-DETA.
               WRITE REG-INFORMES FROM TITULO-9 AFTER 1
              
               MOVE SPACES TO REG-INFORMES
               ADD 1              TO CON-LIN.
       F-LEE-CNCATCTA.
      *
       I-TIPO.
           PERFORM I-LEE-COMPBTE THRU F-LEE-COMPBTE.
           IF SW-EOF NOT = 1
              MOVE  NOMBM-COMP    TO I-NCOMP
              MOVE  ALTTIP-DOCTOS TO I-TIP.
       F-TIPO.
      *
       I-LEE-COMPBTE.
             MOVE 0 TO SW-EOF.
             READ CNCOMPTE WITH NO LOCK INVALID KEY
                  MOVE 1 TO SW-EOF.
       F-LEE-COMPBTE.
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
               MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY WK-ESPACIOS
                   LINE   24  POSITION 01 CONTROL SG-CONTROL-STRING
               MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
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
                MOVE MAYCTA-DOCTOS          TO I-MAYCTA
                MOVE SUBCTA-DOCTOS          TO I-SUB
                MOVE AUXCTA-DOCTOS          TO I-AUX
                MOVE NUMFRA-DOCTOS          TO I-NUMFRA
                MOVE ALTFRA-DOCTOS          TO I-FRA
                MOVE CONCEPTO-DOCTOS        TO I-CON
                MOVE ALTEMP-DOCTOS          TO I-CODEMP
                MOVE ALTTIP-DOCTOS          TO I-TIP
                MOVE ALTNIT-DOCTOS          TO I-NIT
                MOVE RADICTIP-DOCTOS        TO I-RADICTIP-DOCTOS
                MOVE RADICDOC-DOCTOS        TO I-RADICDOC-DOCTOS
                PERFORM I-EVALUE-VALOR    THRU F-EVALUE-VALOR.
                IF CON-LIN > 73
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
            IF VALOR-DOCTOS = ZEROS
               MOVE 0             TO I-VDEB
               MOVE 0             TO I-VCRE.
       F-EVALUE-VALOR.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
       I-FRA-NUEVO.
           MOVE ALTTIP-DOCTOS          TO   W-ALTTIP-DOCTOS
           MOVE ALTFRA-DOCTOS          TO   W-ALTFRA-DOCTOS
           MOVE ALTMES-DOCTOS          TO   W-ALTMES-DOCTOS
           MOVE NUMFRA-DOCTOS          TO   W-NUMFRA-DOCTOS
           MOVE ALTTIP-DOCTOS          TO   LLAVM1-COMP.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA.
           PERFORM I-TIPO            THRU F-TIPO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
       F-FRA-NUEVO.

       I-TOTAL-DTO.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 73
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 4              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
       F-TOTAL-DTO.
      *
       I-TOTAL-MES.
                MOVE "TOTAL MES:"           TO T-CON1.
                MOVE  WK-NOMBRE-MES(W-ALTMES-DOCTOS) TO T-CON2.
                MOVE S-TOTAL-DEBITOS        TO T-VDEB.
                MOVE S-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 73
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 4              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO S-TOTAL-DEBITOS S-TOTAL-CREDITOS.
      *          PERFORM I-IMPR-TITULO-MES THRU F-IMPR-TITULO-MES.
       F-TOTAL-MES.
      *
       I-TOTAL-TIP.
                MOVE "TOTAL COMPROBANTE  "  TO T-CON
                MOVE T-TOTAL-DEBITOS        TO T-VDEB.
                MOVE T-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 73
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 4              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1

                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
       F-TOTAL-TIP.
      *
       I-IMPR-TITULO-MES.
                IF CON-LIN > 73
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 1                     TO CON-LIN.
       F-IMPR-TITULO-MES.
      *
       I-IMPR-TITULO-TIP.
                PERFORM I-TIPO         THRU F-TIPO.
                IF CON-LIN > 73
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 1              TO CON-LIN
                DISPLAY SPACES           LINE 24   POSITION 1
                DISPLAY "IMPRIMIENDO EL TIPO DE COMPROBANTE....."
                                        LINE 24   POSITION 1.
       F-IMPR-TITULO-TIP.
      *
       I-TOTAL-GEN.
                MOVE "TOTAL GENERAL   "     TO T-CON
                MOVE G-TOTAL-DEBITOS        TO T-VDEB.
                MOVE G-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 73
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
      *    MOVE    FECHOY-DOCTOS    TO  I-NUMERO.
           MOVE  W-LONG       TO    I-MES-DESDE   I-MES-HASTA I-MES
           MOVE  CON-ANO      TO    I-ANO-DESDE R-ANO
                                    I-ANO-HASTA I-ANO
           MOVE  WK-DIA-DESDE TO    I-DIA-DESDE
           MOVE  WK-DIA-HASTA TO    I-DIA-HASTA.
           MOVE  WK-NOMBRE-MES(W-LONG)   TO  R-MES

           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER PAGE.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER 3
               
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-11 AFTER 1
             
            WRITE REG-INFORMES  FROM  TITULO-FE1  AFTER 1.
            
            WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
          
            WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
            WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
            WRITE REG-INFORMES  FROM  TITULO-A1 AFTER 1
            WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
            MOVE 11                         TO  CON-LIN.
       F-TITULO-INFORME.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCOMPTE  CNNITCED
                 EMPRESAS
                 CNCATCTA
                 INFORMES
                 CNDOCTOS.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG796.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\INFORMES.PLB".

