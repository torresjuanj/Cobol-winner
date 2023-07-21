       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG745.
      ******************************************************************
      * TIPOS DE INGRESOS            ABR-27-2002                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\TIPINGRE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\TIPINGRE.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG745".
       77      ID-PRG         PIC X(7)       VALUE "PROG745".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       77      CTA            PIC 9(06)      VALUE ZEROS.
       77      SIGNO          PIC 9          VALUE ZEROS.

       01      WK-CUENTA-V.
               03  WK-MAYOR-V.
                   07 MAYOR1-V            PIC 9          VALUE ZEROS.
                   07 MAYOR2-V            PIC 9          VALUE ZEROS.
                   07 MAYOR3-V            PIC 9          VALUE ZEROS.
                   07 MAYOR4-V            PIC 9          VALUE ZEROS.
                   07 MAYOR5-V            PIC 9          VALUE ZEROS.
               03  WK-SUBCTA-V            PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR-V.
                   07 AUX1-V              PIC 9          VALUE ZEROS.
                   07 AUX2-V              PIC 99         VALUE ZEROS.

       01      WK-CUENTA-N.
               03  WK-MAYOR-N.
                   07 MAYOR1-N            PIC 9          VALUE ZEROS.
                   07 MAYOR2-N            PIC 9          VALUE ZEROS.
                   07 MAYOR3-N            PIC 9          VALUE ZEROS.
                   07 MAYOR4-N            PIC 9          VALUE ZEROS.
               03  WK-SUBCTA-N            PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR-N.
                   07 AUX1-N              PIC 99         VALUE ZEROS.
                   07 AUX2-N              PIC 99         VALUE ZEROS.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\TIPINGRE.LBL".
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
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(08)    VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(19)    VALUE SPACES.
         05    FILLER      PIC X(44)    VALUE
              "CATALOGO  DE  CUENTAS  -  TIPO  DE INGRESOS ".
         05    FILLER      PIC X(30)    VALUE SPACES.
         05    FILLER      PIC X(11)    VALUE "Programa : ".
         05    I-PROGRAMA  PIC X(08)    VALUE SPACES.

        03     TITULO-2.
         05    FILLER      PIC X(08)    VALUE "Hora  : ".
         05    I-HORA      PIC X(05).
         05    FILLER      PIC X(102)    VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(01)    VALUE SPACES.
         05    I-CAMPO     PIC X(100)   VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(139)   VALUE ALL "=".


        03     TITULO-4.
         05    FILLER      PIC X(55).
         05    T-CON.
           07  T-CON1      PIC X(15).
           07  T-CON2      PIC X(10).
         05    FILLER      PIC X(06).
         05    T-VDEB      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(02).
         05    T-VCRE      PIC ZZ,ZZZ,ZZZ,ZZZ-.
         05    FILLER      PIC X(03).


        03     TITULO-LS.
         05    FILLER      PIC X(85).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "-".
         05    FILLER      PIC X(03).


        03     TITULO-LD.
         05    FILLER      PIC X(85).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).
         05    FILLER      PIC X(15) VALUE ALL "=".
         05    FILLER      PIC X(03).

        03     TITULO-A.
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(08)    VALUE "  TIPO  ".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(06)    VALUE "CUENTA".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "SUB".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "CTA".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE " ANT.".
         05    FILLER      PIC X(15)    VALUE " |             ".
         05    FILLER      PIC X(21)    VALUE "     D E T A L L E   ".
         05    FILLER      PIC X(17)    VALUE "               |".
         05    FILLER      PIC X(01)    VALUE SPACES.
         05    FILLER      PIC X(14)    VALUE " ACUMULADORES ".
         05    FILLER      PIC X(01)    VALUE SPACES.
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(06)    VALUE "SIGNO ".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(08)    VALUE "  BASE  ".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(07)    VALUE "PORCENT".
         05    FILLER      PIC X(03)    VALUE " | ".

       03      TITULO-A1.
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(08)    VALUE " EGRESO ".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(06)    VALUE "MAYOR ".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "CTA".
         05    FILLER      PIC X(03)    VALUE " | ".
         05    FILLER      PIC X(03)    VALUE "AUX".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(05)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(49)    VALUE SPACES.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE "  N.I.T.".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(09)    VALUE " FACTURA ".
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE SPACES.
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(08)    VALUE "GRAVABLE".
         05    FILLER      PIC X(02)    VALUE " |".
         05    FILLER      PIC X(09)    VALUE " CALCULO ".
         05    FILLER      PIC X(01)    VALUE "|".

        03     LINEA-DE-DETALLE.
         05    FILLER      PIC X(04).
         05    I-TIPE      PIC Z9999.
         05    FILLER      PIC X(05).
         05    I-MAYMAE    PIC 9(04).
         05    FILLER      PIC X(04).
         05    I-SUBMAE    PIC 9(02).
         05    FILLER      PIC X(03).
         05    I-AUXMAE    PIC 9(04).
         05    FILLER      PIC X(01).
         05    I-GRUCTA    PIC Z(05).
         05    FILLER      PIC X(03).
         05    I-NOMCTA    PIC X(49).
         05    FILLER      PIC X(05).
         05    I-CODPGT    PIC X(01).
         05    FILLER      PIC X(07).
         05    I-INDDETAL  PIC X(01).
         05    FILLER      PIC X(01).
      *  05    I-INDCART   PIC X(01).
         05    FILLER      PIC X(07).
         05    I-SIGNO     PIC 9(01).
         05    FILLER      PIC X(09).
         05    I-INDBASE   PIC X(01).
         05    FILLER      PIC X(08).
         05    I-PORC-CTA  PIC Z(04).
         05    FILLER      PIC X(04).

        03     LINEA-DE-DETALLE-UNO.
         05    FILLER      PIC X(05).
         05    I-TIPO      PIC Z9999.
         05    FILLER      PIC X(30).
         05    I-NOMBRE    PIC X(40).

        03     TITULO-TOTAL.
         05    FILLER      PIC X(32)
                      VALUE "TOTAL CUENTAS TIPOS DE INGRESOS:".
         05    T-TOTAL     PIC ZZZZZ.
      *  05    T-TOTFAC    PIC $ZZ,ZZZ,ZZZ,ZZZ.
      *  05    T-TOTBAS    PIC $ZZ,ZZZ,ZZZ,ZZZ.

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
               COPY "..\DCL\TIPINGRE.DCL".
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
            MOVE 0 TO SW1 SW1  FIN-OK  WK-PAGINA W-CONTA CON-LIN
           PERFORM I-START-TIPINGRE   THRU F-START-TIPINGRE
           PERFORM I-PRIMER-TIPINGRE  THRU F-PRIMER-TIPINGRE
           PERFORM I-DISPLAY          THRU F-DISPLAY
           PERFORM I-BUSCA-TIPINGRE   THRU F-BUSCA-TIPINGRE
                               UNTIL FIN-OK = 1.
           PERFORM I-TOTAL-INFORME    THRU F-TOTAL-INFORME.
           PERFORM I-FIN-MODE    THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE     1                  TO  IND-INFORMES
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  EMPRESAS
           OPEN I-O    TIPINGRE
           OPEN INPUT CNCATCTA.
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.

       I-PRIMER-TIPINGRE.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-TIPINGRE THRU F-LEE-TIPINGRE.
           PERFORM I-TITULO-INFORME THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PRIMER-TIPINGRE.
      *
       I-BUSCA-TIPINGRE.
           PERFORM I-LEE-TIPINGRE THRU F-LEE-TIPINGRE
           IF FIN-OK = 1   GO F-BUSCA-TIPINGRE.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-BUSCA-TIPINGRE.
      *
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-TIPINGRE.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  INGRESOS-CLAVE
               START TIPINGRE  KEY >   INGRESOS-CLAVE INVALID KEY
      *              MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-TIPINGRE
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-TIPINGRE.
      *
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-TIPINGRE.
              DISPLAY INGRESOS-CLAVE          LINE 20 POSITION 28
              DISPLAY ING-DESCRIP            LINE 21 POSITION 16
              READ TIPINGRE NEXT RECORD WITH NO LOCK AT END
                                 MOVE 1 TO FIN-OK.
       F-LEE-TIPINGRE.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-EMPRESAS.
      *
       I-DISPLAY.
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY " TIPOS INGRESOS" LINE 03 POSITION 25.
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
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 25 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 25 POSITION 2 SIZE 78
               "TIPOS DE INGRESOS --------->" LINE 25 POSITION 05
                LABEL-INFORMES                LINE 25 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 25 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 25 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY.

      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME-1.
            MOVE ZEROS        TO  K.
            MOVE ZEROS        TO  I.
            MOVE  INGRESOS-CLAVE   TO  I-TIPO.
            MOVE  ING-DESCRIP     TO  I-NOMBRE.
            IF CON-LIN > 74
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF CONS-ING   =  ZEROS
                    MOVE      1           TO  I
                    GO LOOP-2.
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE-UNO AFTER 1
                
                ADD 2 TO CON-LIN.
         LOOP-1.
            ADD        1      TO  K.
            IF   K >  8     GO F-MUEVE-INFORME-1.
            IF   CTA-MAYOR(K)  =   ZEROS    GO LOOP-1.
            MOVE CTA-MAYOR(K) TO I-MAYMAE  MAYMAE
            MOVE CTA-SUB(K)   TO I-SUBMAE  SUBMAE
            MOVE CTA-AUX(K)   TO I-AUXMAE  AUXMAE
            READ CNCATCTA   WITH NO LOCK INVALID KEY  GO LOOP-1.
            MOVE NOMCTA       TO I-NOMCTA
            MOVE ANTICIPO-ING TO I-GRUCTA
            MOVE CTA-SIGNO(K) TO I-SIGNO
            MOVE CODPGT       TO I-CODPGT
            MOVE INDDETAL     TO I-INDDETAL
            MOVE INDBASE      TO I-INDBASE
      *     MOVE INDICE-CAR   TO I-INDCART
            MOVE PORC-CTA     TO I-PORC-CTA.
         LOOP-2.
            IF CON-LIN > 74
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
             ADD 1                      TO W-CONTA.
            IF I = 1
                MOVE SPACES TO REG-INFORMES
                WRITE REG-INFORMES AFTER 1
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE-UNO AFTER 1
               
                ADD 2 TO CON-LIN
                GO  F-MUEVE-INFORME-1
            ELSE
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1                      TO CON-LIN.
            GO LOOP-1.
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
                IF CON-LIN > 74
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
               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               MOVE 6                          TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
      *        MOVE 0              TO   T-TOTFAC
      *        MOVE 0              TO   T-TOTBAS
               WRITE REG-INFORMES  FROM TITULO-4     AFTER 2
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 1.
       F-TOTAL-INFORME.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE EMPRESAS
                 CNCATCTA
                 INFORMES
                 TIPINGRE.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE ZEROS                  TO  CON-PERIODO
           CANCEL "PROG745.COB"
      *         CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *         CANCEL "PROG737.COB"
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