       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG753.
      ******************************************************************
      * MOVIMIENTO DE REVISION OPERATIVA   ABR-27-2002                 *
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
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG753".
       77      ID-PRG         PIC X(7)       VALUE "PROG753".
       77      FEC-DES        PIC 9(06)      VALUE ZEROS.
       77      KK             PIC 9(01)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(06)      VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
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
         05    TIT-CIA     PIC X(45)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(20)    VALUE SPACES.
         05    FILLER      PIC X(42)    VALUE
              "R E V I S I O N    O P E R A T I V A ".
         05    FILLER      PIC X(30)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).

        03     TITULO-2.
         05    I-CAMPO     PIC X(115)   VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(04)    VALUE SPACES.
         05    I-PROGRAMA  PIC X(22)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(165)   VALUE ALL "=".


        03     TITULO-4.
         05    FILLER      PIC X(19).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(10).
         05    T-VDEB      PIC ZZZZZ,ZZZ,ZZ9.99-.
         05    FILLER      PIC X(01).
         05    T-VCRE      PIC ZZZZZ,ZZZ,ZZ9.99-.


        03     TITULO-LS.
         05    FILLER      PIC X(46).
         05    FILLER      PIC X(16) VALUE ALL "-".
         05    FILLER      PIC X(01).
         05    FILLER      PIC X(16) VALUE ALL "-".


        03     TITULO-LD.
         05    FILLER      PIC X(46).
         05    FILLER      PIC X(16) VALUE ALL "=".
         05    FILLER      PIC X(01).
         05    FILLER      PIC X(16) VALUE ALL "=".

        03     TITULO-A.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(10)    VALUE "  CODIGO  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE " DOCTO".
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(24)    VALUE "   C O N C E P T O ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(17)    VALUE "  V A L O R E S  ".
         05    FILLER      PIC X(08)    VALUE SPACES.
      *  05    FILLER      PIC X(02)    VALUE "|".
      *  05    FILLER      PIC X(12)    VALUE " INDICES".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(11)    VALUE "   N.I.T.  ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(20)    VALUE " NOMBRE DEL CLIENTE ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE " FACT.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "     NUMERO     ".
         05    FILLER      PIC X(01)    VALUE "|".
      *  05    FILLER      PIC X(12)    VALUE "    FECHA   ".
      *  05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(15)    VALUE "   OTROS       ".
         05    FILLER      PIC X(01)    VALUE "|".

       03      TITULO-A1.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(10)    VALUE " CONTABLE ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(25)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    DEBITOS     ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    CREDITOS    ".
         05    FILLER      PIC X(01)    VALUE "|".
      *  05    FILLER      PIC X(06)    VALUE "SG CD".
      *  05    FILLER      PIC X(07)    VALUE "EMP PE".
      *  05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(11)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(20)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(06)    VALUE "NUMERO".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(16)    VALUE "    SECUENCIA   ".
         05    FILLER      PIC X(01)    VALUE "|".
      *  05    FILLER      PIC X(12)    VALUE " DE PROCESO".
      *  05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(15)    VALUE "  SOPORTES     ".
         05    FILLER      PIC X(01)    VALUE "|".

        03     TITULO-6.
         05    FILLER      PIC X(020)   VALUE "TIPO DE COMPROBANTE:".
         05    FILLER      PIC X(02).
         05    I-TIP       PIC 9(02).
         05    FILLER      PIC X(05).
         05    I-NOMTIP    PIC X(35).

        03     TITULO-7.
         05    FILLER      PIC X(16).
         05    FILLER      PIC X(04)   VALUE "MES:".
         05    FILLER      PIC X(02).
         05    I-MES       PIC X(10).

        03     LINEA-DE-DETALLE.
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-MAYCTA    PIC 9(04).
         05    I-SUB       PIC 9(02).
         05    I-AUX       PIC 9(04).
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-FRA       PIC Z(06).
         05    FILLER      PIC X(01)    VALUE " ".
         05    I-CON       PIC X(25).
         05    FILLER      PIC X(01).
         05    I-VDEB      PIC ZZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(02).
         05    I-VCRE      PIC ZZZZ,ZZZ,ZZZ.ZZ-.
         05    FILLER      PIC X(01).
      *  05    I-SIGNO     PIC X(01).
      *  05    FILLER      PIC X(02).
      *  05    I-CONDIC    PIC X(01).
      *  05    FILLER      PIC X(01).
      *  05    I-CODEMP    PIC Z99.
      *  05    FILLER      PIC X(01).
      *  05    I-PER       PIC ZZ.
      *  05    FILLER      PIC X(02).
         05    I-NIT       PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01).
         05    I-EMP       PIC X(20).
         05    FILLER      PIC X(01).
         05    I-NUMFRA          PIC Z(06).
         05    FILLER            PIC X(01) VALUE SPACES.
         05    I-RADICTIP-DOCTOS PIC 9(02).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-RADICDOC-DOCTOS PIC 9(06).
         05    FILLER            PIC X(01)  VALUE "-".
         05    I-SEC-AUTOMATICA  PIC 9(06).
         05    FILLER            PIC X(02)  VALUE SPACES.
      *  05    I-FECHOY-DOCTOS.
      *        07 I-ANO-DOC      PIC ZZZZ.
      *        07 FILLER         PIC X.
      *        07 I-MES-DOC      PIC 99.
      *        07 FILLER         PIC X.
      *        07 I-DIA-DOC      PIC 99.
      *  05    FILLER            PIC X(01)  VALUE SPACES.
      *  05    I-SOPORTE         PIC ZZZZZZ.
      *  05    FILLER            PIC X(01)  VALUE SPACES.
      *  05    I-CIUDAD          PIC ZZ.

        03     TITULO-TOTAL.
         05    FILLER      PIC X(17)  VALUE "TOTAL FACTURAS:  ".
         05    T-TOTAL     PIC ZZZZZ.
         05    FILLER      PIC X(01)  VALUE SPACES.
         05    FILLER      PIC X(29)  VALUE "TOTAL ESTE DOCUMENTO :".
         05    FILLER      PIC X(30)  VALUE SPACES.
         05    T-TOTFAC    PIC $ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01).
         05    T-TOTBAS    PIC $ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.

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
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
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
           MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         FIN-OK  WK-PAGINA W-CONTA CON-LIN
           MOVE  7        TO  I.
           IF  LL =  1
               PERFORM I-START-CNDOCTOS  THRU F-START-CNDOCTOS.
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-PRIMER-REGISTRO4 THRU F-PRIMER-REGISTRO4
           PERFORM I-BUSCA-MES    THRU F-BUSCA-MES
                               UNTIL FIN-OK = 1
           MOVE "TOTAL ESTE DOCUMENTO : " TO T-CON
           PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
           PERFORM I-TOTAL-MES THRU F-TOTAL-MES
           PERFORM I-TOTAL-TIP THRU F-TOTAL-TIP
           PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN
           PERFORM ACEPTA-TRUCO.
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE WK-MASCARA-FECHA       TO  I-FECHA
           MOVE WK-MASCARA-HORA        TO  I-HORA
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE  1                     TO   IND-INFORMES
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE W-CODEMP               TO  LAB-CNDOCTOS-DIRCIA
           MOVE LABEL-INFORMES         TO  I-PROGRAMA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS CNCOMPTE CNNITCED
           OPEN I-O   CNDOCTOS.
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    ORGANIZADA DE ACUERDO A UN MES  DEFINIDO                 *
      ***************************************************************
       I-PANTALLA-1B.
           MOVE " REVISION OPERATIVA POR COMPROBANTES  "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  DIGITE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ  COMPROBANTE  No:                          บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
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
                   DISPLAY " CODIGO PERIODO ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
       I-PRIMER-REGISTRO4.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
           MOVE ALTTIP-DOCTOS       TO   W-ALTTIP-DOCTOS
           MOVE ALTMES-DOCTOS       TO   W-ALTMES-DOCTOS
           MOVE RADICDOC-DOCTOS     TO   W-ALTFRA-DOCTOS
           MOVE FECHA-CONTAB-DOCTOS TO   FEC-DES
           MOVE NUMFRA-DOCTOS       TO   W-NUMFRA-DOCTOS
           MOVE ALTTIP-DOCTOS       TO   LLAVM1-COMP
           PERFORM I-TIPO         THRU   F-TIPO
           MOVE WK-NOMBRE-MES(W-ALTMES-DOCTOS)  TO I-MES I-MES4
           MOVE AREA-IMP4           TO   I-CAMPO
           PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
       F-PRIMER-REGISTRO4.
      *
       I-BUSCA-MES.
           IF FIN-OK = 1   GO F-BUSCA-MES.
           IF ALTTIP-DOCTOS = W-ALTTIP-DOCTOS
              IF W-ALTFRA-DOCTOS = RADICDOC-DOCTOS
      *JJT
      *JJT           MOVE    FEC-DES             TO FECHA-CONTAB-DOCTOS
      *JJT           REWRITE REG-DOCTOS
                     PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
                     PERFORM I-LEE-CNDOCTOS    THRU F-LEE-CNDOCTOS
                     GO I-BUSCA-MES
              ELSE
                 MOVE "TOTAL ESTE DOCUMENTO : " TO T-CON
                 PERFORM I-TOTAL-DTO THRU F-TOTAL-DTO
                 PERFORM I-FRA-NUEVO THRU F-FRA-NUEVO
                 GO I-BUSCA-MES
           ELSE
                    MOVE "TOTAL ESTE DOCUMENTO : " TO T-CON
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
                    DISPLAY
                    " PRESIONE <C> PARA CONFIRMAR   "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F8] COMPROBANTE ESPECIAL "
                    LINE 24 POSITION 32 REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F8
               PERFORM I-ACEPTA-COMP     THRU F-ACEPTA-COMP
               PERFORM I-START-CNDOCTOS  THRU F-START-CNDOCTOS
               MOVE   1      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  F9
               PERFORM I-ACEPTA-COMP     THRU F-ACEPTA-COMP
               PERFORM I-START-CNDOCTOS  THRU F-START-CNDOCTOS
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

       I-START-CNDOCTOS.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  KEY00-DOCTOS
               MOVE W-COD                  TO  RADICTIP-DOCTOS
               START CNDOCTOS  KEY >   KEY00-DOCTOS INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDOCTOS.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************

       I-LEE-CNDOCTOS.
               MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY " "
                      LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
              DISPLAY RADIC-DOCTOS           LINE 24 POSITION 21
              DISPLAY ALTTIP-DOCTOS          LINE 24 POSITION 47
              DISPLAY ALTMES-DOCTOS          LINE 24 POSITION 56
              DISPLAY ALTCTA-DOCTOS          LINE 24 POSITION 65
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT
                   END MOVE 1 TO FIN-OK.
              IF FIN-OK = 1 GO F-LEE-CNDOCTOS.
              MOVE SPACES TO I-EMP.
              IF LL =  1  AND
                 ALTTIP-DOCTOS >  W-COD
                 MOVE 1  TO FIN-OK
                 GO F-LEE-CNDOCTOS.

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
               MOVE WIN-EMPRESA TO    I-EMP     W-NOMBRE
               MOVE WIN-NIT       TO    I-NEMPRESA.
       F-LEE-EMPRESAS.
      *
       I-LEE-CNNITCED.
               MOVE 0        TO SW-EOF.
               READ CNNITCED WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA TO    I-EMP     W-NOMBRE
               MOVE WIN-NIT       TO    I-NEMPRESA.
       F-LEE-CNNITCED.
      *
       I-TIPO.
           PERFORM I-LEE-COMPBTE THRU F-LEE-COMPBTE.
           IF SW-EOF = 1
               MOVE "Comprobante no existe" TO I-NOMTIP
           ELSE
               MOVE  NOMBM-COMP    TO I-NOMTIP  I-NCOMP
           MOVE  ALTTIP-DOCTOS TO I-TIP  I-TCOMP.
       F-TIPO.
      *
       I-LEE-COMPBTE.
             MOVE 0 TO SW-EOF.
             READ CNCOMPTE WITH NO LOCK INVALID KEY
                  MOVE 1 TO SW-EOF.
       F-LEE-COMPBTE.
      *
       I-IMPRIME-PANTALLA.
               MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY " "
                      LINE   24  POSITION 80 CONTROL SG-CONTROL-STRING
             ADD  1  TO  I.
             DISPLAY  W-ALTTIP-DOCTOS LINE  I   POSITION 04.
             IF KK  =   1
                DISPLAY  T-CON        LINE  I   POSITION 10 SIZE 18
              ELSE
                DISPLAY  I-NOMTIP     LINE  I   POSITION 10 SIZE 18.
             DISPLAY  T-VDEB          LINE  I   POSITION 38.
             DISPLAY  T-VCRE          LINE  I   POSITION 58.
       F-IMPRIME-PANTALLA.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
                MOVE MAYCTA-DOCTOS          TO I-MAYCTA
                MOVE SUBCTA-DOCTOS          TO I-SUB
                MOVE AUXCTA-DOCTOS          TO I-AUX
                MOVE NUMFRA-DOCTOS          TO I-NUMFRA
                MOVE ALTFRA-DOCTOS          TO I-FRA
                MOVE CONCEPTO-DOCTOS        TO I-CON
      *         MOVE CONDICION-DOCTOS       TO I-CONDIC
      *         MOVE SIGNO-DOCTOS           TO I-SIGNO
      *         MOVE ALTEMP-DOCTOS          TO I-CODEMP
      *         MOVE CIUDAD-DOCTOS          TO I-CIUDAD
      *         MOVE ALTPER-DOCTOS          TO I-PER
      *         MOVE BANCO01-DOCTOS         TO I-CIUDAD
      *         MOVE BANCO-DOCTOS           TO I-PER
                MOVE ALTTIP-DOCTOS          TO I-TIP
                MOVE ALTNIT-DOCTOS          TO I-NIT
      *         MOVE COSTO12-DOCTOS         TO I-SOPORTE
      *         MOVE CHEQUEORD-DOCTOS       TO I-SOPORTE
                MOVE RADICTIP-DOCTOS        TO I-RADICTIP-DOCTOS
                MOVE RADICDOC-DOCTOS        TO I-RADICDOC-DOCTOS
                MOVE RADIC-AUTOMATICA-DOCTOS       TO I-SEC-AUTOMATICA
      *         MOVE WK-ANO-DOCTOS          TO I-ANO-DOC
      *         MOVE WK-MES-DOCTOS          TO I-MES-DOC
      *         MOVE WK-DIA-DOCTOS          TO I-DIA-DOC

                PERFORM I-EVALUE-VALOR THRU F-EVALUE-VALOR.
                IF CON-LIN > 79
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
           MOVE ALTTIP-DOCTOS     TO   W-ALTTIP-DOCTOS
           MOVE RADICDOC-DOCTOS   TO   W-ALTFRA-DOCTOS
           MOVE FECHA-CONTAB-DOCTOS TO  FEC-DES
           MOVE ALTMES-DOCTOS     TO   W-ALTMES-DOCTOS
           MOVE NUMFRA-DOCTOS     TO   W-NUMFRA-DOCTOS
           MOVE ALTTIP-DOCTOS     TO   LLAVM1-COMP.
           PERFORM I-TIPO           THRU F-TIPO
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
       F-FRA-NUEVO.

       I-TOTAL-DTO.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS.
       F-TOTAL-DTO.
      *
       I-TOTAL-MES.
                MOVE "TOTAL MES:"           TO T-CON1.
                MOVE  WK-NOMBRE-MES(W-ALTMES-DOCTOS) TO T-CON2.
                MOVE S-TOTAL-DEBITOS        TO T-VDEB.
                MOVE S-TOTAL-CREDITOS       TO T-VCRE.
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
                MOVE "TOTAL COMPROBANTE  "  TO T-CON
                MOVE T-TOTAL-DEBITOS        TO T-VDEB.
                MOVE T-TOTAL-CREDITOS       TO T-VCRE.
                PERFORM I-IMPRIME-PANTALLA  THRU F-IMPRIME-PANTALLA.
                 IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 5              TO CON-LIN
                MOVE 0 TO T-TOTAL-DEBITOS T-TOTAL-CREDITOS.
       F-TOTAL-TIP.
      *
       I-IMPR-TITULO-MES.
                MOVE WK-NOMBRE-MES(ALTMES-DOCTOS)   TO I-MES.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                ADD 1                     TO CON-LIN.
       F-IMPR-TITULO-MES.
      *
       I-IMPR-TITULO-TIP.
                PERFORM I-TIPO         THRU F-TIPO.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 2
                ADD 2              TO CON-LIN.
       F-IMPR-TITULO-TIP.
      *
       I-TOTAL-GEN.
                MOVE "TOTAL GENERAL   "     TO T-CON
                MOVE G-TOTAL-DEBITOS        TO T-VDEB.
                MOVE G-TOTAL-CREDITOS       TO T-VCRE.
                MOVE  1     TO KK.
                PERFORM I-IMPRIME-PANTALLA  THRU F-IMPRIME-PANTALLA.
                IF CON-LIN > 79
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM TITULO-LS AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM TITULO-LD AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-LIMPIE-PANTALLA THRU F-LIMPIE-PANTALLA.
                MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                          S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                          T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                          G-TOTAL-DEBITOS G-TOTAL-CREDITOS.
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
       F-LIMPIE-PANTALLA.

       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
               
               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
             
               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-A1 AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-6  AFTER 1
               
               WRITE REG-INFORMES  FROM  TITULO-7  AFTER 1
               
               MOVE 8                          TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
               MOVE C-TOTFAC       TO   T-TOTFAC
               MOVE C-TOTBAS       TO   T-TOTBAS
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 2
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 1
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 1
               ADD 2               TO   CON-LIN.
       F-TOTAL-INFORME.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCOMPTE  CNNITCED
                 EMPRESAS
                 INFORMES
                 CNDOCTOS.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG753.COB"
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
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PRO\I-DISPLAY.PRO".
               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\INFORMES.PLB".


