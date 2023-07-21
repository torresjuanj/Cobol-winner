       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG770.
      ****************************************************************
      * BALANCE DE PRUEBA     ABR-27-2002                            *
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
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG770".
       77      ID-PRG         PIC X(7)       VALUE "PROG770".
       77      WK-MAYOR       PIC 9(4)       VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
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

       01    REG-CNDTTRAB.
         03    CNDTTRAB-CLAVE.
           05    CNDTTRAB-MAYOR.
                  07 CNDTTRAB-MAYOR-1     PIC 9(001).
                  07 CNDTTRAB-MAYOR-2     PIC 9(001).
                  07 CNDTTRAB-MAYOR-3     PIC 9(001).
                  07 CNDTTRAB-MAYOR-4     PIC 9(001).
               05 CNDTTRAB-SUB           PIC 9(002).
               05 CNDTTRAB-AUX.
                  07 CNDTTRAB-AUX1       PIC 9(002).
                  07 CNDTTRAB-AUX2       PIC 9(002).
               05 CNDTTRAB-NIT           PIC 9(012).
               05 CNDTTRAB-FRA           PIC 9(006).
         03    CNDTTRAB-ALTER-1.
           05    CNDTTRAB-NIT-ALT1       PIC 9(0012).
           05    CNDTTRAB-FRA-ALT1       PIC 9(0006).
         03    CNDTTRAB-ALTER-2.
           05    CNDTTRAB-NOM-ALT2       PIC X(0040).
         03    CNDTTRAB-FECHA-FRA.
               05 CNDTTRAB-ANOFRA        PIC 9(04).
               05 CNDTTRAB-MESFRA        PIC 9(02).
               05 CNDTTRAB-DIAFRA        PIC 9(02).
         03    CNDTTRAB-FECHA-VCTO.
               05 CNDTTRAB-ANOVCT        PIC 9(02).
               05 CNDTTRAB-MESVCT        PIC 9(02).
               05 CNDTTRAB-DIAVCT        PIC 9(02).
         03    PERIODOS                  OCCURS 013 TIMES.
           05    CNDTTRAB-MOVDEB         PIC S9(15)V9(02) COMP-3.
           05    CNDTTRAB-MOVCRE         PIC S9(15)V9(02) COMP-3.
           05    CNDTTRAB-SALDO          PIC S9(15)V9(02) COMP-3.
      *******************************************************************
      *                       T  I  T  U  L  O  S                       *
      *******************************************************************

       01      AREA-INFORME.
        03     TITULO-0.
         05    FILLER      PIC X(12)    VALUE SPACES.
         05    TIT-CIA     PIC X(45)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(26)    VALUE SPACES.
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC ZZ.


        03     TITULO-2.
         05    FILLER      PIC X(24)    VALUE SPACES.
         05    FILLER      PIC X(22)    VALUE "BALANCE  DE  PRUEBA".

        03     TITULO-2A.
         05    FILLER       PIC X(25)        VALUE SPACES.
         05    I-MES        PIC X(10)        VALUE SPACES.
         05    FILLER       PIC X(4)         VALUE " DE ".
         05    I-ANO        PIC X(04)        VALUE SPACES.

        03     TITULO-2B.
         05    FILLER       PIC X(60)        VALUE "  ".
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(233)   VALUE ALL "=".

        03  TITULO-9.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(10)      VALUE "  CODIGO  ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(09)      VALUE SPACES.
           05 FILLER         PIC X(21)      VALUE
              "D E S C R I P C I O N".
           05 FILLER         PIC X(07)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE "   S A L D O   ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(08)      VALUE SPACES.
           05 FILLER         PIC X(19)      VALUE
              "M O V I M I E N T O".
           05 FILLER         PIC X(10)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE "   N U E V O   ".
           05 FILLER         PIC X(01)      VALUE "|".
      *
        03  TITULO-9A.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(10)      VALUE "  CUENTA  ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(08)      VALUE SPACES.
           05 FILLER         PIC X(10)      VALUE "Nivel No. ".
           05 NIVEL-S        PIC 9.
           05 FILLER         PIC X(01)      VALUE SPACES.
           05 NIVEL-NOM      PIC X(11)      VALUE SPACES.
           05 FILLER         PIC X(06)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE "A N T E R I O R".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE " D E B I T O S ".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE "C R E D I T O S".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(18)      VALUE "   S A L D O   ".
           05 FILLER         PIC X(01)      VALUE "|".
      *
        03  LIN14-S.
           05 FILLER         PIC X(126)     VALUE ALL "_".
       
        03  LIN15-S.
           05 FILLER         PIC X(126)     VALUE ALL "_".
      *
        03 LDET1.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MAYCTA-S       PIC ZZZZ.
           05 SUBCTA-S       PIC 9(02).
           05 AUXCTA1-S      PIC 9(02).
           05 AUXCTA2-S      PIC 9(02).
           05 FILLER         PIC X(01)       VALUE "|".
           05 NOMCTA-S       PIC X(37)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALANT-S       PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVDEB-S       PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVCRE-S       PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALNVO-S       PIC   ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LDETNIT.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MAYCTA-NIT     PIC ZZZZ.
           05 SUBCTA-NIT     PIC 9(02).
           05 AUXCTA1-NIT    PIC 9(02).
           05 AUXCTA2-NIT    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE "|".
           05 CODNIT-S       PIC ZZZ,ZZZ,ZZZ.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 NOMNIT-S       PIC X(25)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALANT-NIT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVDEB-NIT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVCRE-NIT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALNVO-NIT     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LDETDET.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MAYCTA-DET     PIC ZZZZ.
           05 SUBCTA-DET     PIC 9(02).
           05 AUXCTA1-DET    PIC 9(02).
           05 AUXCTA2-DET    PIC 9(02).
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 FILLER         PIC X(07)       VALUE "Fra No.".
           05 DETALLE-S      PIC ZZ9999.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 FILLER         PIC X(10)       VALUE "Fecha Fra:".
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 AFEC-FRA-S     PIC ZZZZ.
           05 FILLER         PIC X(1)        VALUE "/".
           05 MFEC-FRA-S     PIC 99.
           05 FILLER         PIC X(1)        VALUE "/".
           05 DFEC-FRA-S     PIC 99.
           05 FILLER         PIC X(01)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALANT-DET     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVDEB-DET     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 MOVCRE-DET     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 SALNVO-DET     PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
      *
        03 LBLANCO.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(10)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(37)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".

        03 LCONTIN.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(10)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(12)       VALUE SPACES.
           05 FILLER         PIC X(14)       VALUE "** CONTINUA **".
           05 FILLER         PIC X(10)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".

        03 LCONTIN1.
           05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *".
        03 LCONTIN2.
            05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * * * * * * *".
        03 LCONTIN3.
           05 FILLER         PIC X(60)       VALUE
           "* * * * * * * * * * * * * * * * * * *".

        03 TITULO-4.
           05 FILLER         PIC X(01)      VALUE "|".
           05 I-FECHA        PIC X(11).
           05 FILLER         PIC X(03)      VALUE " \ ".
           05 I-HORA         PIC X(05).
           05 FILLER         PIC X(05)      VALUE SPACES.
           05 FILLER         PIC X(25)      VALUE "SUMAS IGUALES".
           05 FILLER         PIC X(01)      VALUE "|".
           05 FILLER         PIC X(17)      VALUE SPACES.
           05 FILLER         PIC X(01)      VALUE "|".
           05 T-VDEB         PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 T-VCRE         PIC ZZZZZZ,ZZZ,ZZ9.99-.
           05 FILLER         PIC X(01)       VALUE "|".
           05 FILLER         PIC X(18)       VALUE SPACES.
           05 FILLER         PIC X(01)       VALUE "|".

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
         05    FILLER      PIC X(3)     VALUE " DE".
         05    I-ANO1      PIC 9(04)       VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(8)     VALUE "HASTA : ".
         05    I-DIA2      PIC 99       VALUE ZEROS.
         05    FILLER      PIC X        VALUE "-".
         05    I-MES2      PIC X(3)     VALUE SPACES.
         05    FILLER      PIC X(3)     VALUE " DE".
         05    I-ANO2      PIC 9(04)    VALUE ZEROS.

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
         05    FILLER      PIC X(03)     VALUE " DE".
         05    I-ANOD      PIC 9(04).
         05    FILLER      PIC X(10).
         05    FILLER      PIC X(08)     VALUE "HASTA : ".
         05    I-DIAH      PIC 9(02).
         05    FILLER      PIC X(01)     VALUE "-".
         05    I-MESH      PIC X(03).
         05    FILLER      PIC X(03)     VALUE " DE".
         05    I-ANOH      PIC 9(04).


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
               PERFORM I-LABEL-ARCHIVOS  THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU  F-ABRE-ARCHIVOS
               PERFORM I-PANTALLA-1      THRU F-PANTALLA-1
               PERFORM I-PROCESO-COMP    THRU F-PROCESO-COMP
               PERFORM I-FIN-MODE        THRU  F-FIN-MODE.
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
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P
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
      ***************************************************************
      *        P R O C E S O   D E   C O M P R O B A N T E S        *
      ***************************************************************
       I-PANTALLA-1.
               MOVE "*B A L A N C E  D E  P R U E B A*"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออป"
                                               LINE 06 POSITION 20
               DISPLAY "บ                                       บ"
                                               LINE 07 POSITION 20
               DISPLAY "บ                                       บ"

                                               LINE 08 POSITION 20
               DISPLAY "บ                                       บ"
                                               LINE 09 POSITION 20
               DISPLAY "บ   [F4]  NIVEL CTAS MAYORES            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ   [F5]  NIVEL SUBCUENTAS              บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ   [F6]  NIVEL AUXILIARES              บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F7]  NIVEL N.I.T.S                 บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ   [F8]  NIVEL FACTURAS                บ"
                                               LINE 14 POSITION 20
               DISPLAY "บ                                       บ"
                                               LINE 15 POSITION 20
               DISPLAY "บ                                       บ"
                                               LINE 16 POSITION 20
               DISPLAY "บ    [F2] TERMINAR                      บ"
                                               LINE 17 POSITION 20
               DISPLAY "บ                                       บ"
                                               LINE 18 POSITION 20
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 19 POSITION 20.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
               PERFORM I-DISPLAY-LABEL   THRU   F-DISPLAY-LABEL.
               DISPLAY "APERTURA ARCHIVO:WORDPAD" LINE 20 POSITION 20.
               DISPLAY "ORIENTACION: HORIZONTAL " LINE 21 POSITION 20.
               DISPLAY "MARGENES: IZQ 0 DER 0   " LINE 22 POSITION 20.
               DISPLAY "TAMAฅO  : CARTA         " LINE 23 POSITION 20.
       F-PANTALLA-1.

      ***************************************************************
      *    AREA DE PROCESOS PARA IMPRESION DE TIPOS DE COMPROBANTE  *
      ***************************************************************

       I-PROCESO-COMP.
           PERFORM ACEPTA-TRUCO
            IF F2  PERFORM I-FIN-MODE   THRU F-FIN-MODE.
            IF F4  PERFORM I-PROCESO-F4 THRU F-PROCESO-F4.
            IF F5  PERFORM I-PROCESO-F5 THRU F-PROCESO-F5.
            IF F6  PERFORM I-PROCESO-F6 THRU F-PROCESO-F6.
            IF F7  PERFORM I-PROCESO-F7 THRU F-PROCESO-F7.
            IF F8  PERFORM I-PROCESO-F8 THRU F-PROCESO-F8.
       F-PROCESO-COMP.

       I-PROCESO-F4.
            MOVE 4 TO NIVEL-S
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE "* MAYORES *" TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-MAYOR THRU F-PROCESO-MAYOR
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F4.

       I-PROCESO-F5.
            MOVE 5 TO NIVEL-S
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE "*SUBCUENTA*" TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-SUB   THRU F-PROCESO-SUB
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F5.

       I-PROCESO-F6.
            MOVE 6 TO NIVEL-S
            MOVE "*AUXILIAR* " TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-AUX   THRU F-PROCESO-AUX
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F6.

       I-PROCESO-F7.
            MOVE 7 TO NIVEL-S
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE "*N.I.T.S.* " TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-NITS  THRU F-PROCESO-NITS
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F7.

       I-PROCESO-F8.
            MOVE 8 TO NIVEL-S
            PERFORM I-DISPLAY         THRU F-DISPLAY.
            MOVE "*FACTURAS* " TO NIVEL-NOM
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
            PERFORM I-PROCESO-FRAS  THRU F-PROCESO-FRAS
                    UNTIL FIN-OK = 1
            PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-F8.
      ***************************************************************
      *    AQUI EMPIEZA EL PROCESO PARA REPORTAR INFORMACION        *
      *    GENERAL DE CONTABLIDAD DEPENDIENDO SU ORDEN (FRAS-CTAS)  *
      ***************************************************************
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
              NIVEL = 5 OR NIVEL = 6
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-AUX.

       I-PROCESO-NITS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS N.I.T.S..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK   = 1 OR CNDTACUM-FRA   > 0 GO F-PROCESO-NITS.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-NITS.

       I-PROCESO-FRAS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-FRAS.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PROCESO-FRAS.

       I-PANTALLA-1B.
               MOVE "*B A L A N C E  D E  P R U E B A*"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ  DIGITE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ  CUENTA DESDE:                             บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 14 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 15 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
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
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
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
       I-CONFIRMA-FACT.
                    DISPLAY " PRESIONE <C> PARA CONFIRMA-COMPR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
                    IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
                    IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-FACT
                     ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-FACT.

      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               IF LL = 1    MOVE  WK-MAYOR  TO CNDTACUM-MAYOR.
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************

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

       I-LEE-CNDTACUM.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            DISPLAY CNDTACUM-NIT                  LINE 20 POSITION 28
            DISPLAY CNDTACUM-MAYOR                LINE 20 POSITION 56
            DISPLAY CNDTACUM-SUB                  LINE 20 POSITION 61
            DISPLAY CNDTACUM-AUX                  LINE 20 POSITION 63
            DISPLAY CNDTACUM-MESFRA                LINE 21 POSITION 28
            DISPLAY CNDTACUM-FRA                 LINE 21 POSITION 56
            READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                                MOVE 1 TO FIN-OK
                                GO F-LEE-CNDTACUM.
             IF CNDTACUM-SALDO(MESW)     = 0 AND
                 CNDTACUM-MOVDEB(W-LONG) = 0 AND
                 CNDTACUM-MOVCRE(W-LONG) = 0 AND
                 CNDTACUM-SALDO(W-LONG)  = 0 GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA   TO  W-NOMBRE.
       F-LEE-EMPRESAS.
      *
       I-DISPLAY.
               MOVE  "                *** BALANCE DE PRUEBA***   "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ N.I.T No.:               No.CUENTA   :
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ MES      :               No. FACTURA :
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

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            DISPLAY CNDTACUM-NIT                  LINE 20 POSITION 28
            DISPLAY CNDTACUM-MAYOR                LINE 20 POSITION 56
            DISPLAY CNDTACUM-SUB                  LINE 20 POSITION 61
            DISPLAY CNDTACUM-AUX                  LINE 20 POSITION 63
            DISPLAY CNDTACUM-MESFRA                LINE 21 POSITION 28
            DISPLAY CNDTACUM-FRA                 LINE 21 POSITION 56
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
            MOVE CNDTACUM-SALDO(MESW)   TO SALANT-S
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
            IF NIVEL = 1
               ADD CNDTACUM-MOVDEB(W-LONG) TO W-TOTAL-DEBITOS
               ADD CNDTACUM-MOVCRE(W-LONG) TO W-TOTAL-CREDITOS.
            PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
      *
       I-TOTAL-GEN.
                MOVE W-TOTAL-DEBITOS        TO T-VDEB.
                MOVE W-TOTAL-CREDITOS       TO T-VCRE.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LIN14-S AFTER 1
                WRITE REG-INFORMES FROM TITULO-4 AFTER 1
                WRITE REG-INFORMES FROM LIN15-S AFTER 1
                ADD 3                      TO CON-LIN.
                PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-TOTAL-GEN.
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************

       I-TITULO-INFORME.
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE.
        
           WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1
         
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
           
           WRITE REG-INFORMES  FROM  TITULO-2B AFTER 1
           
           WRITE REG-INFORMES  FROM  LIN14-S   AFTER 1
          
           WRITE REG-INFORMES  FROM  TITULO-9  AFTER 1
           
           WRITE REG-INFORMES  FROM  TITULO-9A AFTER 1
          
           WRITE REG-INFORMES  FROM  LIN14-S   AFTER 1
          
           MOVE 8                         TO  CON-LIN.
       F-TITULO-INFORME.
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
       EVALUE-IMPRESION.
            MOVE 0 TO RUT-OK.
            PERFORM I-EVALUE-IMPRE1 THRU F-EVALUE-IMPRE1.
            IF RUT-OK = 1 GO F-EVALUE-IMPRESION.
            PERFORM I-EVALUE-IMPRE2 THRU F-EVALUE-IMPRE2.
            IF RUT-OK = 1 GO F-EVALUE-IMPRESION.
            PERFORM I-EVALUE-IMPRE3 THRU F-EVALUE-IMPRE3.
       F-EVALUE-IMPRESION.

       I-EVALUE-IMPRE1.
            IF CNDTACUM-NIT   = 0 AND CNDTACUM-FRA   = 0
               MOVE 1 TO RUT-OK
               PERFORM IMPRE1 THRU F-IMPRE1.
       F-EVALUE-IMPRE1.

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

       I-EVALUE-IMPRE3.
            IF CNDTACUM-NIT   > 0 AND CNDTACUM-FRA   > 0
                  MOVE 1 TO RUT-OK
                  PERFORM IMPRE3 THRU F-IMPRE3.
       F-EVALUE-IMPRE3.
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
      *
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
       IMPRE3.
            IF CON-LIN > 55
               PERFORM LINE-CIERRE THRU F-LINE-CIERRE
               PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            WRITE REG-INFORMES FROM LDETDET AFTER 1
            ADD 1 TO CON-LIN.
       F-IMPRE3.
      *
       LINE-CIERRE.
            WRITE REG-INFORMES FROM LCONTIN AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1
            MOVE  0  TO CON-LIN.
       F-LINE-CIERRE.
      *
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS
                 INFORMES
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG770.COB"
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