       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG756.
      ******************************************************************
      * LIBRO AUXILIAR                ABR-27-2002                      *
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
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CNDOCTOS.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG756".
       77      ID-PRG         PIC X(7)       VALUE "PROG756".
       77      NIVEL-S        PIC 9          VALUE ZEROS.
       77      CUENTA-1       PIC 9(10)      VALUE ZEROS.
       77      CUENTA-2       PIC 9(10)      VALUE ZEROS.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
        03     NUM-SOPOR.
               05  NUM1                  PIC 9(4).
               05  NUM2                  PIC 9(2).
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\AUDITE.LBL".
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
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(19)    VALUE "         N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC ZZZ.


        03     TITULO-2.
         05    FILLER      PIC X(12)    VALUE SPACES.
         05    FILLER      PIC X(22)    VALUE "LIBRO AUXILIAR GENERAL".

        03     TITULO-2X.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(19)    VALUE "LIBRO AUXILIAR DE".
         05    VAR-TIT     PIC X(40)    VALUE SPACES.


        03     TITULO-2A.
         05    FILLER      PIC X(10)        VALUE SPACES.
         05    I-MES       PIC X(10)        VALUE SPACES.
         05    FILLER      PIC X(4)         VALUE " de ".
         05    I-ANO       PIC X(04)        VALUE SPACES.
         05    FILLER      PIC X(45)        VALUE SPACES.

        03     TITULO-2B.
         05    FILLER      PIC X(22)    VALUE "Fecha de Elaboracion: ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(08)    VALUE "  Hora: ".
         05    I-HORA      PIC X(05).
         05    I-CAMPO     PIC X(060)   VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    I-INF       PIC X(30)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(233)   VALUE ALL "=".

        03  TITULO-9.
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(12)    VALUE " C O D I G O".
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(24)    VALUE SPACES.
           05 FILLER       PIC X(21)    VALUE
              "D E S C R I P C I O N".
           05 FILLER       PIC X(24)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(18)    VALUE "    S A L D O     ".
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(10)    VALUE SPACES.
           05 FILLER       PIC X(19)    VALUE
              "M O V I M I E N T O".
           05 FILLER       PIC X(08)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(03)    VALUE SPACES.
           05 FILLER       PIC X(11)    VALUE " N U E V O ".
           05 FILLER       PIC X(04)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "!".
      *
       03  TITULO-9A.
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(12)    VALUE " C U E N T A".
           05 FILLER       PIC X(01)    VALUE " ".
           05 FILLER       PIC X(08)     VALUE SPACES.
           05 FILLER       PIC X(16)     VALUE "RADICACION No.  ".
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(06)     VALUE "No.DOC".
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(06)     VALUE "No.SOP".
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(08)     VALUE " FECHA  ".
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(06)     VALUE SPACES.
           05 FILLER       PIC X(09)     VALUE "CONCEPTO".
           05 FILLER       PIC X(06)     VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(18)    VALUE " A N T E R I O R  ".
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(18)    VALUE "  D E B I T O S  ".
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(18)    VALUE " C R E D I T O S ".
           05 FILLER       PIC X(01)    VALUE "!".
           05 FILLER       PIC X(04)    VALUE SPACES.
           05 FILLER       PIC X(11)    VALUE " S A L D O ".
           05 FILLER       PIC X(03)    VALUE SPACES.
           05 FILLER       PIC X(01)    VALUE "!".
      *
       03  LIN14-S.
           05 FILLER       PIC X(160)   VALUE ALL "-".
       
        03  LIN15-S.
           05 FILLER       PIC X(160)   VALUE ALL "=".
      *
        03 LDET1.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-S     PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-S     PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-S     PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(03)    VALUE SPACES.
           05 NOMCTA-S     PIC X(66)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALANT-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-S     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
      *
       03 LDETNIT.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-NIT   PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-NIT   PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-NIT   PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(13)    VALUE SPACES.
           05 CODNIT-S     PIC ZZZ,ZZZ,ZZZ.
           05 FILLER       PIC X(04)     VALUE "!   ".
           05 NOMNIT-S     PIC X(41)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALANT-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-NIT   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
      *
        03 LDETDET.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(12)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(23)     VALUE SPACES.
           05 FILLER       PIC X(09)     VALUE "No.Fra : ".
           05 CNDT-FRA-S   PIC Z99999.
           05 FILLER       PIC X(07)     VALUE "  ".
           05 FILLER       PIC X(09)     VALUE "Fec Fra: ".
           05 AFEC-FRA-S   PIC 9(04).
           05 FILLER       PIC X(01)     VALUE "/".
           05 MFEC-FRA-S   PIC 99.
           05 FILLER       PIC X(01)     VALUE "/".
           05 DFEC-FRA-S   PIC 99.
           05 FILLER       PIC X(05)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALANT-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVDEB-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MOVCRE-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 SALNVO-DET   PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
      *
       03 LBLANCO.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(12)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(69)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".

        03 LCONTIN.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(12)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(27)     VALUE SPACES.
           05 FILLER       PIC X(14)     VALUE "** CONTINUA **".
           05 FILLER       PIC X(28)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".


        03 TITULO-4.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(29)     VALUE SPACES.
           05 FILLER       PIC X(25)     VALUE
           "     T O T A L E S       ".
           05 FILLER       PIC X(28)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 T-SALANT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 T-VDEB       PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 T-VCRE       PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 T-SALNVO     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".

        03 TITULO-5.
           05 FILLER       PIC X(01)     VALUE "!".
           05 MAYCTA-M     PIC ZZ99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 SUBCTA-M     PIC 99.
           05 FILLER       PIC X(01)     VALUE " ".
           05 AUXCTA-M     PIC 9999.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(04)     VALUE SPACES.
           05 RAD-TIP      PIC 9(03).
           05 FILLER       PIC X(01)     VALUE "-".
           05 RAD-DOC      PIC 999999.
           05 FILLER       PIC X(01)     VALUE "-".
           05 RAD-AUT      PIC 999999.
           05 FILLER       PIC X(01)     VALUE " ".
           05 NUMDOC-M     PIC ZZZZZ.
           05 FILLER       PIC X(01)     VALUE " ".
           05 NUMFAC-M     PIC ZZZZ9.
           05 FILLER       PIC X(01)     VALUE " ".
           05 FECHA-CONTAB-S PIC 9(08).
           05 FILLER       PIC X(01)     VALUE " ".
           05 CONCEPTO-M   PIC X(22).
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 EMPRESA-M    PIC Z(07)     VALUE ZEROS.
           05 FILLER       PIC X(01)     VALUE SPACES.
           05 PERIODO-M    PIC Z(03)     VALUE ZEROS.
           05 FILLER       PIC X(10)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".
           05 VALDEB-M     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 VALCRE-M     PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER       PIC X(01)     VALUE "!".
           05 FILLER       PIC X(18)     VALUE SPACES.
           05 FILLER       PIC X(01)     VALUE "!".

      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
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
               PERFORM I-PANTALLA-1B    THRU F-PANTALLA-1B
               PERFORM I-ACEPTA-MES     THRU F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP  THRU F-CONFIRMA-COMP
               PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
               PERFORM I-PANTALLA-1C    THRU F-PANTALLA-1C
               PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
               PERFORM I-SELECCION      THRU F-SELECCION
               PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE W-NIT-EMP                TO I-NITEMP
               MOVE W-NIT-EMP-EXT            TO I-NITEXT
               MOVE WEMPRESA                 TO TIT-CIA
               MOVE  1                       TO IND-INFORMES
               PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                  TO LAB-INFORMES-ANO
                                                I-ANO
               MOVE LABEL-INFORMES           TO I-INF
               MOVE "น"                      TO WK-RAYAS-U
               MOVE "ฬ"                      TO WK-RAYAS-P
               MOVE "ถ"                      TO WK-RAYA-U
               MOVE "ว"                      TO WK-RAYA-P
               MOVE 1                        TO SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT CNDTACUM.
               OPEN INPUT  EMPRESAS CNNITCED
               OPEN I-O    CNCATCTA
               OPEN I-O  CNDOCTOS.
               OPEN OUTPUT INFORMES.
           ACCEPT WK-FECHA-HOY       FROM  DATE
           ACCEPT WK-HORA-HOY        FROM  TIME
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
      ***************************************************************
      *        P R O C E S O   D E   P A N T A L L A S              *
      ***************************************************************
       I-PANTALLA-1B.
               MOVE " L I B R O   A U X I L I A R  G E N E R A L"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  DIGITE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-PANTALLA-1C.
               MOVE " L I B R O   A U X I L I A R  G E N E R A L"
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
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1C.
      *
       I-PANTALLA-1D.
               MOVE " L I B R O   A U X I L I A R  G E N E R A L"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
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
      *
       I-DISPLAY.
               MOVE " L I B R O   A U X I L I A R  G E N E R A L"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 10 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 11 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 12 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 13 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-DISPLAY.
      *
       I-DISPLAY-MOV.
               MOVE " L I B R O   A U X I L I A R  G E N E R A L"
               TO WK-SISTEMA
      *         PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "MOVIMIENTO DE CUENTAS...." LINE 16 POSITION 25
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 18 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 19 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 21 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU  F-DISPLAY-LABEL.
       F-DISPLAY-MOV.
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    GENERAL DE CONTABLIDAD DEPENDIENDO SU ORDEN (FRAS-CTAS)  *
      ***************************************************************
       I-SELECCION.
           PERFORM ACEPTA-TRUCO
           IF F2  PERFORM I-FIN-MODE  THRU  F-FIN-MODE
              ELSE
                IF F4
                   MOVE 1 TO SW1
                   PERFORM I-PROCESO-F8     THRU F-PROCESO-F8
                ELSE
                   IF F5
                        MOVE 2 TO SW1
                        PERFORM I-PANTALLA-1D      THRU F-PANTALLA-1D
                        PERFORM I-ACEPTA-CTA-DESDE THRU
                                F-ACEPTA-CTA-DESDE
                        PERFORM I-ACEPTA-CTA-HASTA THRU
                                F-ACEPTA-CTA-HASTA
                        PERFORM I-CONFIRMA-COMP THRU F-CONFIRMA-COMP
                        PERFORM I-BUSCA-CTA     THRU F-BUSCA-CTA
                   ELSE
                        GO F-SELECCION.
       F-SELECCION.
      *
       I-PROCESO-F8.
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE 8 TO NIVEL-S
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            IF SW1 = 1
               PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
            ELSE
               PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
            PERFORM I-PROCESO-FRAS  THRU F-PROCESO-FRAS
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F8.
      *
       I-PROCESO-FRAS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
                               LINE 09 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-FRAS.
           PERFORM I-MUEVE-VAR-CTA   THRU  F-MUEVE-VAR-CTA
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
               DISPLAY CNDTACUM-MAYOR     LINE 11 POSITION 28
               DISPLAY CNDTACUM-SUB       LINE 11 POSITION 34
               DISPLAY CNDTACUM-AUX       LINE 11 POSITION 37
               DISPLAY CNDTACUM-NIT       LINE 11 POSITION 41
               DISPLAY CNDTACUM-FRA       LINE 11 POSITION 56
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
           IF ESTACTA  = "N"
              PERFORM I-MOVIMIENTO-CTA THRU F-MOVIMIENTO-CTA.
       F-PROCESO-FRAS.
      *
       I-BUSCA-CTA.
           PERFORM I-DISPLAY         THRU F-DISPLAY.
           MOVE 8 TO NIVEL-S
           PERFORM I-START-CNDTACUM-E  THRU F-START-CNDTACUM-E
           IF FIN-OK = 1 GO TO F-BUSCA-CTA.
           IF SW1 = 1
               PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
            ELSE
               PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
           PERFORM I-CEROS-VARIABLES   THRU F-CEROS-VARIABLES
           PERFORM I-CTA-DESDE-HASTA   THRU F-CTA-DESDE-HASTA
                   UNTIL CNDTACUM-CLAVE > W-CTAHASTA
           PERFORM I-TOTAL-GEN         THRU F-TOTAL-GEN.
       F-BUSCA-CTA.
      *
       I-CTA-DESDE-HASTA.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
                               LINE 09 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-CTA-DESDE-HASTA.
           IF CTA-TRAB        >= W-CTADESDE AND
              CTA-TRAB        <= W-CTAHASTA
              PERFORM I-MUEVE-VAR-CTA   THRU  F-MUEVE-VAR-CTA
              PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
              DISPLAY CNDTACUM-MAYOR     LINE 11 POSITION 28
              DISPLAY CNDTACUM-SUB       LINE 11 POSITION 34
              DISPLAY CNDTACUM-AUX       LINE 11 POSITION 37
              DISPLAY CNDTACUM-NIT       LINE 11 POSITION 41
              DISPLAY CNDTACUM-FRA       LINE 11 POSITION 56
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
              IF ESTACTA = "N"
                 PERFORM I-MOVIMIENTO-CTA  THRU F-MOVIMIENTO-CTA.
       F-CTA-DESDE-HASTA.
      *

      ***************************************************************
      *       ENTRADA DE DATOS POR PANTALLA                         *
      ***************************************************************

       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                      LINE 11 POSITION 50
                                                  REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                            LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF W-LONG = 1 OR W-LONG = 14
                 COMPUTE MESW = 14
               ELSE
                 COMPUTE MESW = W-LONG - 1.
               MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
               DISPLAY WK-ESPACIOS          LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
       I-ACEPTA-CTA-DESDE.
               MOVE ZEROS                  TO  WK-EXCEPTION.
       I-WCTAD.
               DISPLAY  "          " LINE 11 POSITION 52
                                     REVERSE.
              ACCEPT CUENTA-1        LINE 11 POSITION 52 REVERSE                       UPDATE PROMPT NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.

               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAD TO MAYMAE
               MOVE 0      TO SUBMAE AUXMAE
               MOVE  CUENTA-1     TO CTAMAE W-CTADESDE.
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               MOVE NOMCTA TO VAR-TIT.
               DISPLAY NOMCTA  LINE 22 POSITION 1
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WCTAD.
               MOVE NOMCTA TO VAR-TIT.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
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
               DISPLAY  "          " LINE 14 POSITION 52
                                     REVERSE.
               ACCEPT CUENTA-2  LINE 14 POSITION 52
                      REVERSE CONVERT TAB NO BEEP ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE W-CTAH TO MAYMAE
               MOVE 0      TO SUBMAE AUXMAE
               MOVE  CUENTA-2     TO CTAMAE W-CTAHASTA.
               PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
               IF SW-EOF = 1
                   DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                   GO I-WCTAH.
               IF CUP GO I-WAUXD.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
               IF W-CTAH < W-CTAD
                  DISPLAY
                  "LA ESTA CUENTA NO PUEDE SE MENOR A LA CUENTA DESDE"
                                                  LINE 24 POSITION 1
                                                  REVERSE BLINK
                  GO I-WCTAH.
               GO  F-ACEPTA-CTA-HASTA.

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
      ***************************************************************
      *  P R O C E S O    M O V I M I E N T O    D E   C U E N T A S*
      ***************************************************************
       I-MOVIMIENTO-CTA.
           IF CNDTACUM-MOVDEB (W-LONG) = 0 AND
              CNDTACUM-MOVCRE (W-LONG) = 0
              GO F-MOVIMIENTO-CTA.
           PERFORM I-ANALIZE-NOVEDADES  THRU F-ANALIZE-NOVEDADES.
       F-MOVIMIENTO-CTA.
      *
       I-ANALIZE-NOVEDADES.
           MOVE 0 TO RUT-OK.
           PERFORM I-START-CNDOCTOS-KEY01 THRU F-START-CNDOCTOS-KEY01.
           PERFORM I-EXTRAE-NOVEDADES THRU F-EXTRAE-NOVEDADES
                   UNTIL RUT-OK = 1.
           MOVE 0 TO RUT-OK.
       F-ANALIZE-NOVEDADES.
      ****************************************************************
      *                 EXTRAE NOVEDADES DE CADA CUENTA              *
      ****************************************************************
       I-EXTRAE-NOVEDADES.
           IF I-ERROR             = 1 OR
              CNDOCTOS-STATUS  =  "23"
              MOVE 1 TO RUT-OK GO F-EXTRAE-NOVEDADES.
              MOVE   0   TO SW-IMPRESION-DOCTOS
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS
           PERFORM I-DISPLAY-MOV       THRU F-DISPLAY-MOV
           DISPLAY MAYCTA-DOCTOS      LINE 19 POSITION 28
           DISPLAY SUBCTA-DOCTOS      LINE 19 POSITION 34
           DISPLAY AUXCTA-DOCTOS      LINE 19 POSITION 37
           DISPLAY ALTNIT-DOCTOS      LINE 19 POSITION 41
           DISPLAY NUMFRA-DOCTOS      LINE 19 POSITION 56
           IF RUT-OK = 1 GO F-EXTRAE-NOVEDADES.
           IF CNDTACUM-MAYOR   =   MAYCTA-DOCTOS AND
              CNDTACUM-SUB     =   SUBCTA-DOCTOS AND
              CNDTACUM-AUX     =   AUXCTA-DOCTOS AND
              CNDTACUM-NIT     =   ALTNIT-DOCTOS AND
              CNDTACUM-FRA     =   NUMFRA-DOCTOS
              PERFORM I-MUEVE-DATOS-CNDOCTOS THRU
                      F-MUEVE-DATOS-CNDOCTOS
              PERFORM I-IMPRIME-CNDOCTOS THRU F-IMPRIME-CNDOCTOS
              PERFORM I-CEROS-MOVIMIENTO   THRU F-CEROS-MOVIMIENTO
              REWRITE  REG-DOCTOS INVALID KEY
              DISPLAY "ERROR GRABACION CNDOCTOS" LINE 23 POSITION 1
           ELSE
               MOVE 1 TO RUT-OK.
       F-EXTRAE-NOVEDADES.
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
            DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                   LINE 24 POSITION 1
                                   REVERSE
            DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                      REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F8
               MOVE   1      TO LL
               GO TO F-CONFIRMA-COMP.
           IF  W-TRUCO     NOT =  "C"
               GO TO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.
     *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-START-CNDTACUM-E.
               MOVE 0         TO   FIN-OK
               MOVE W-CTAD    TO   CNDTACUM-MAYOR
               MOVE W-SUBD    TO   CNDTACUM-SUB
               MOVE W-AUXD    TO   CNDTACUM-AUX
               MOVE 0         TO   CNDTACUM-NIT
               MOVE 0         TO   CNDTACUM-FRA
               START CNDTACUM KEY > CNDTACUM-CLAVE  INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM-E.
      *
       I-START-CNDOCTOS-KEY01.
               MOVE CNDTACUM-CLAVE    TO  ALT01-DOCTOS
               MOVE CNDTACUM-MAYOR    TO  MAYCTA-DOCTOS
               MOVE CNDTACUM-SUB      TO  SUBCTA-DOCTOS
               MOVE CNDTACUM-AUX      TO  AUXCTA-DOCTOS
               MOVE CNDTACUM-NIT      TO  ALTNIT-DOCTOS
               MOVE CNDTACUM-FRA      TO  NUMFRA-DOCTOS
               START CNDOCTOS KEY NOT LESS ALT01-DOCTOS INVALID KEY
                  MOVE 1 TO I-ERROR.
       F-START-CNDOCTOS-KEY01.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNCATCTA.
               MOVE 0        TO SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.
      *
       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED
                 WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.

       I-LEE-CNDTACUM.
              MOVE 0 TO FIN-OK
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                     MOVE 1 TO FIN-OK.
              MOVE CNDTACUM-MAYOR   TO CT-MAY
              MOVE CNDTACUM-SUB     TO CT-SUB
              MOVE CNDTACUM-AUX     TO CT-AUX

              IF CNDTACUM-SALDO(MESW)    = 0 AND
                 CNDTACUM-MOVDEB(W-LONG) = 0 AND
                 CNDTACUM-MOVCRE(W-LONG) = 0 AND
                 CNDTACUM-SALDO(W-LONG)  = 0 AND
                 FIN-OK = 0 GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNDOCTOS.
              READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                         MOVE 1 TO RUT-OK.
       F-LEE-CNDOCTOS.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA   TO  W-NOMBRE.
       F-LEE-EMPRESAS.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************
       I-MUEVE-INFORME-1.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE MAYMAE              TO MAYCTA-S
                                        MAYCTA-NIT
                                        MAYCTA-M
            MOVE SUBMAE              TO SUBCTA-S
                                        SUBCTA-NIT
                                        SUBCTA-M
            MOVE AUXMAE              TO AUXCTA-S
                                        AUXCTA-NIT
                                        AUXCTA-M
            MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
            MOVE NOMCTA              TO NOMCTA-S.
            MOVE CNDTACUM-FRA        TO CNDT-FRA-S.
            MOVE CNDTACUM-ANOFRA     TO AFEC-FRA-S
            MOVE CNDTACUM-MESFRA     TO MFEC-FRA-S.
            MOVE CNDTACUM-DIAFRA     TO DFEC-FRA-S.
            MOVE CNDTACUM-SALDO(MESW)    TO SALANT-S
                                            SALANT-NIT
                                            SALANT-DET.
            MOVE CNDTACUM-MOVDEB(W-LONG) TO MOVDEB-S
                                            MOVDEB-NIT
                                            MOVDEB-DET.
            MOVE CNDTACUM-MOVCRE(W-LONG) TO MOVCRE-S
                                            MOVCRE-NIT
                                            MOVCRE-DET.
            MOVE CNDTACUM-SALDO(W-LONG)  TO SALNVO-S
                                            SALNVO-NIT
                                            SALNVO-DET.
           PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
       I-MUEVE-VAR-CTA.
              MOVE CNDTACUM-MAYOR       TO MAYMAE
              MOVE CNDTACUM-SUB         TO SUBMAE
              MOVE CNDTACUM-AUX         TO AUXMAE.
       F-MUEVE-VAR-CTA.
      *
       I-MUEVE-DATOS-CNDOCTOS.
               MOVE RADICTIP-DOCTOS       TO  RAD-TIP
               MOVE RADICDOC-DOCTOS       TO  RAD-DOC
               MOVE RADIC-AUTOMATICA-DOCTOS TO  RAD-AUT
               MOVE ALTFRA-DOCTOS         TO  NUMDOC-M
               MOVE FECHA-CONTAB-DOCTOS   TO  FECHA-CONTAB-S
               MOVE CONCEPTO-DOCTOS       TO  CONCEPTO-M
               MOVE ALTEMP-DOCTOS         TO  EMPRESA-M
               MOVE ALTPER-DOCTOS         TO  PERIODO-M
               IF ALTTIP-DOCTOS = 03
                  MOVE NUM-SOPORTE-DOCTOS   TO NUM1
                  MOVE NUM-SOPORTE-2-DOCTOS TO NUM2
                  MOVE NUM-SOPOR            TO NUMFAC-M
               ELSE
                  IF ALTTIP-DOCTOS = 04
                     MOVE NUMFRA-DOCTOS  TO NUMFAC-M
                  ELSE
                     MOVE ALTFRA-DOCTOS TO NUMFAC-M.
               IF VALOR-DOCTOS IS POSITIVE
                  MOVE  VALOR-DOCTOS      TO  VALDEB-M.
               IF VALOR-DOCTOS IS NEGATIVE
                  MOVE VALOR-DOCTOS       TO  VALCRE-M.
      *    PERFORM I-SUMAS THRU F-SUMAS.
       F-MUEVE-DATOS-CNDOCTOS.
      *
       I-SUMAS.
            IF VALOR-DOCTOS IS POSITIVE
            ADD VALOR-DOCTOS      TO W-TOTAL-DEBITOS.
            IF VALOR-DOCTOS IS NEGATIVE
            ADD VALOR-DOCTOS      TO W-TOTAL-CREDITOS.
       F-SUMAS.
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         W-TOTAL-SALANT  W-TOTAL-SALNVO
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         T-SALANT        T-SALNVO
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
       I-CEROS-MOVIMIENTO.
               MOVE 0 TO VALDEB-M VALCRE-M.
       F-CEROS-MOVIMIENTO.
      *
       I-IMPRIME-CNDOCTOS.
             IF CON-LIN > 75
                PERFORM LINE-CIERRE      THRU F-LINE-CIERRE
                IF SW1 = 1
                   PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
                ELSE
                   PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
             WRITE REG-INFORMES FROM TITULO-5 AFTER 1
             MOVE   1   TO SW-IMPRESION-DOCTOS
             PERFORM I-SUMAS THRU F-SUMAS.
             ADD   1                  TO CON-LIN.
       F-IMPRIME-CNDOCTOS.
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
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   = 0
               MOVE 1 TO RUT-OK
               PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRE1.
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
       I-EVALUE-IMPRE3.
            IF CNDTACUM-NIT   > 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
       F-EVALUE-IMPRE3.
      *
       IMPRE1.
           IF CON-LIN > 75
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              IF SW1 = 1
                 PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
              ELSE
                 PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
           IF NIVEL-S = 7
               PERFORM IMPRENIT THRU F-IMPRENIT
           ELSE
               PERFORM IMPRENIV THRU F-IMPRENIV.
       F-IMPRE1.
      *
       IMPRENIV.
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              ADD 1 TO CON-LIN.
       F-IMPRENIV.
      *
       IMPRENIT.
           IF ESTACTA = "S" OR
              CODPGT  = "S"
              PERFORM I-SUPER-NIV THRU F-SUPER-NIV
              GO F-IMPRENIT.
           WRITE REG-INFORMES FROM LBLANCO AFTER 1
           WRITE REG-INFORMES FROM LDET1 AFTER 1
           ADD   2 TO CON-LIN.
       F-IMPRENIT.
      *
       I-SUPER-NIV.
           IF NIVEL = 1 OR NIVEL = 2 OR NIVEL = 3
              WRITE REG-INFORMES FROM LDET1 AFTER 1
           
              ADD   1 TO CON-LIN
           ELSE
              WRITE REG-INFORMES FROM LDET1 AFTER 1
              
              ADD   1 TO CON-LIN.
       F-SUPER-NIV.
      *
       IMPRE2.
           IF CON-LIN > 75
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              IF SW1 = 1
                 PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
              ELSE
                 PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
              WRITE REG-INFORMES FROM LDETNIT AFTER 1
              
              ADD 1 TO CON-LIN.
       F-IMPRE2.
      *
       IMPRE3.
            IF CON-LIN > 75
              PERFORM LINE-CIERRE THRU F-LINE-CIERRE
              IF SW1 = 1
                 PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
              ELSE
                 PERFORM I-TITULO-INF-ESPEC THRU F-TITULO-INF-ESPEC.
            WRITE REG-INFORMES FROM LDETDET AFTER 1
            
            ADD 1 TO CON-LIN.
       F-IMPRE3.
      *
       LINE-CIERRE.
            IF CON-LIN = 75  GO F-LINE-CIERRE.
            WRITE REG-INFORMES FROM LBLANCO AFTER 1
            WRITE REG-INFORMES FROM LCONTIN AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1.
       F-LINE-CIERRE.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************
       I-TITULO-INFORME.
           MOVE 0 TO CON-LIN
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
          
           WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
           
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2B AFTER 1
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
           WRITE REG-INFORMES  FROM TITULO-9   AFTER 1
           WRITE REG-INFORMES  FROM TITULO-9A  AFTER 1
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
           MOVE 08                         TO  CON-LIN.
       F-TITULO-INFORME.

       I-TITULO-INF-ESPEC.
           MOVE 0 TO CON-LIN
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE
         
           WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2X AFTER 1
          
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2B AFTER 1
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
           WRITE REG-INFORMES  FROM TITULO-9   AFTER 1
           WRITE REG-INFORMES  FROM TITULO-9A  AFTER 1
           WRITE REG-INFORMES  FROM LIN14-S    AFTER 1
           MOVE 08                         TO  CON-LIN.
       F-TITULO-INF-ESPEC.
      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
      *
       I-TOTAL-GEN.
                MOVE 0                      TO T-SALANT.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                MOVE 0                      TO T-SALNVO.
                IF CON-LIN > 75
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LIN14-S AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM LIN15-S AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-TOTAL-GEN.
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS CNDOCTOS
                 INFORMES
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG756.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\PANTALLA.PRO".

               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".


