       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG738.
      ******************************************************************
      * actualizaci¢n de saldos de todo un aNo                         *
      *    desde el periodo 14 hasta el periodo 12     ABR-27-2002     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\CNDTACUM.DYC".
               COPY "..\DYC\CNDTACNU.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\CNDOCTOS.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNDTACNU.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG738".
       77      ID-PRG              PIC X(7)  VALUE "PROG738".
       77      W-CHEQUE                  PIC 9(12)      VALUE ZEROS.
       77      W-EMPRE                   PIC 9(07)      VALUE ZEROS.
       77      WK-EMPRE-INICIAL          PIC 9(07)      VALUE ZEROS.
       77      ANTICIPO                  PIC 9          VALUE ZEROS.
       77      CTA                       PIC 9          VALUE ZEROS.
       77      KK                        PIC 9          VALUE ZEROS.
       77      JJ                        PIC 9          VALUE ZEROS.
       77      AA                        PIC 99         VALUE ZEROS.
       77      WK-RADICA                 PIC 9(6)       VALUE ZEROS.
       77      NUM-SOPORTE               PIC 9(6)       VALUE ZEROS.
       77      WK-CONTROL                PIC 9(6)       VALUE ZEROS.
       77      WK-CUENTAUX               PIC 9(10)       VALUE ZEROS.
       01      WK-CUENTA1.
               03  WK-MAYOR              PIC 9(05)      VALUE ZEROS.
               03  WK-SUBCTA             PIC 9(02)      VALUE ZEROS.
               03  WK-AUXILIAR           PIC 9(03)      VALUE ZEROS.
       01      SUMATORIAS.
               03   W-DEB-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-DEB-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-CRE-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-CRE-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-TOT-CNDTACNU             PIC  9(12)  VALUE ZEROS.
               03   W-TOT-CNDTACUM             PIC  9(12)  VALUE ZEROS.
               03   W-REG-CNDTACUM             PIC  9(10)  VALUE ZEROS.
               03   W-REG-CNDTACNU             PIC  9(10)  VALUE ZEROS.
               03   W-VARIABLES-NOMINA         PIC  X(30)  VALUE SPACES.
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

       01  NOMLISTADOS.
           03   LISTA               OCCURS  12 TIMES.
             05  INICIAL            PIC  X      VALUE SPACES.
             05  PROGRAMA           PIC  9(03)  VALUE ZEROS.
             05  ANO                PIC  9(04)  VALUE ZEROS.
             05  MES                PIC  99     VALUE ZEROS.
             05  CIA                PIC  9(07)  VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CNDTACNU.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\RUTMONTO.WRK".
       01      AREA-INFORME.
         03     AREA-WORKING-INFORME.
           05    I-CODIGO-EMPRESA        PIC 9(07)    VALUE ZEROS.
      *
       03     AREA-CONCEPTO-MVDOCTOS.
         05    C-DATO-MVDOCTOS           PIC X(20)    VALUE SPACES.
         05    FILLER                    PIC X(06)    VALUE " CHQ.#".
         05    C-CHEQ-DESDE              PIC X(06)    VALUE ZEROS.
         05    FILLER                    PIC X(4)     VALUE
              " AL ".
         05    C-CHEQ-HASTA             PIC X(06)     VALUE ZEROS.
      *
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNDTACNU.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           MOVE 0 TO SW1 SW1 SW-EOF FIN-OK  W-CONTA CON-LIN.
           PERFORM I-PROCESO-AUDITE    THRU  F-PROCESO-AUDITE
           PERFORM I-DISPLAY           THRU  F-DISPLAY.
           PERFORM I-ACEPTA-MES        THRU  F-ACEPTA-MES.
           PERFORM I-CONFIRMA-COMP     THRU  F-CONFIRMA-COMP
           PERFORM I-LABEL-ARCHIVOS    THRU  F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CNDTACUM   THRU  F-STATUS-CNDTACUM
           PERFORM F-FIN               THRU  F-FIN-MODE.
           PERFORM I-START-CNDTACNU    THRU  F-START-CNDTACNU
           PERFORM I-INICIE-INFORME    THRU  F-INICIE-INFORME.
           PERFORM I-PANTALLA          THRU  F-PANTALLA
           PERFORM I-INFORME-GENERAL   THRU  F-INFORME-GENERAL.
           PERFORM I-FIN-MODE          THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE W-IDENT-PROGRAMA         TO CON-PROGRAMA
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
               COMPUTE LAB-CNDOCTOS-MES  =  W-LONG + 1.
               PERFORM I-LABEL-CNDTACNU    THRU F-LABEL-CNDTACNU.
               COMPUTE LAB-CNDTACNU-ANO  =  CON-ANO - 1.
               ACCEPT  WK-FECHA-HOY            FROM DATE.
               ACCEPT  WK-HORA-HOY             FROM  TIME.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CNCATCTA
                           CNDTACNU.
               OPEN  I-O   CNDTACUM
                           EMPRESAS.
      *        CLOSE CNDOCTOS.
               OPEN  I-O  CNDOCTOS.
       F-ABRE-ARCHIVOS.
      *
       I-FIN-MODE.
               CLOSE CNCATCTA
                     EMPRESAS
                     CNDOCTOS
                     CNDTACNU
                     CNDTACUM.
       F-FIN.
               PERFORM I-ACTUAL   THRU  F-ACTUAL.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.
      *
       I-ACTUAL.
               CANCEL "PROG738.COB"
               MOVE    14    TO CON-PERIODO.
               CALL   "PROG715.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG715.COB".
               MOVE    ZEROS TO CON-PERIODO.
               CALL   "PROG781.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG781.COB".
               PERFORM I-BORRA-MESES  THRU F-BORRA-MESES
                       VARYING J  FROM 1  BY 1 UNTIL J > 12.
               CANCEL "PROG738.COB".
               CALL   "PROG980.COB" USING AREA-LINK-NOMINA.
       F-ACTUAL.
      *
       I-BORRA-MESES.
               CANCEL "PROG738.COB"
               MOVE    J   TO CON-PERIODO.
               CALL   "PROG715.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG715.COB".
               CALL   "PROG781.COB" USING AREA-LINK-NOMINA
               CANCEL "PROG781.COB".
       F-BORRA-MESES.
      *


       I-INICIE-INFORME.
               MOVE ZEROS                  TO  W-TOTAL.
       F-INICIE-INFORME.
      *
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       RUTINA  PRINCIPAL   DEL PROCESO DE CONVERSION     ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

       I-INFORME-GENERAL.
               PERFORM I-START-CNDTACNU    THRU  F-START-CNDTACNU
               MOVE  ZEROS                   TO    SW-EOF KK.
               PERFORM I-PROCESA-C14       THRU  F-PROCESA-C14
                        UNTIL  SW-EOF  = 1.
       F-INFORME-GENERAL.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³     Proceso principal de depuracion y conversion        ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PROCESA-C14.
               PERFORM I-SUMA             THRU F-SUMA.
               MOVE   ZEROS               TO AA.
               PERFORM I-LECTURA-SEC      THRU   F-LECTURA-SEC.
               IF SW-EOF =  1  GO F-PROCESA-C14.

             IF  CNDTACNU-MOVDEB(W-LONG) = 0 AND
                 CNDTACNU-MOVCRE(W-LONG) = 0 AND
                 CNDTACNU-SALDO(W-LONG)  = 0 GO I-PROCESA-C14.


               PERFORM I-MVTO-CAMPOS     THRU F-MVTO-CAMPOS.
               MOVE CNDTACNU-MAYOR         TO CNDTACUM-MAYOR  MAYMAE
               MOVE CNDTACNU-SUB           TO CNDTACUM-SUB    SUBMAE
               MOVE CNDTACNU-AUX           TO CNDTACUM-AUX    AUXMAE

               PERFORM I-LEE-CNCATCTA      THRU F-LEE-CNCATCTA.

            IF CNDTACUM-MOVDEB(W-LONG) NOT = 0
               MOVE 1                 TO SIGNO-DOCTOS
               MOVE CNDTACUM-MOVDEB(W-LONG) TO VALOR-DOCTOS
               PERFORM I-MVTO-DOCTOS      THRU F-MVTO-DOCTOS.

            IF CNDTACUM-MOVCRE(W-LONG) NOT =  0
               MOVE 2                       TO SIGNO-DOCTOS
               MOVE CNDTACUM-MOVCRE(W-LONG) TO VALOR-DOCTOS
               PERFORM I-MVTO-DOCTOS      THRU F-MVTO-DOCTOS.

               GO I-PROCESA-C14.
       F-PROCESA-C14.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Rutinas de validacion de campos y cuentas         ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

      *
       I-SUMA.
            ADD    1     TO  W-REG-CNDTACUM.
            PERFORM I-MOSTRAR-CAMPOS   THRU F-MOSTRAR-CAMPOS.
       F-SUMA.
      *
       I-MVTO-CAMPOS.
            MOVE CNDTACNU-MAYOR          TO CNDTACUM-MAYOR
            MOVE CNDTACNU-SUB            TO CNDTACUM-SUB
            MOVE CNDTACNU-AUX            TO CNDTACUM-AUX
            MOVE CNDTACNU-NIT            TO CNDTACUM-NIT
            MOVE CNDTACNU-FRA            TO CNDTACUM-FRA
            MOVE CNDTACNU-ALTER-1        TO CNDTACUM-ALTER-1
            MOVE CNDTACNU-NIT-ALT1       TO CNDTACUM-NIT-ALT1
            MOVE CNDTACNU-FRA-ALT1       TO CNDTACUM-FRA-ALT1
            MOVE CNDTACNU-ALTER-2        TO CNDTACUM-ALTER-2
            MOVE CNDTACNU-NOM-ALT2       TO CNDTACUM-NOM-ALT2
            MOVE CNDTACNU-FECHA-FRA      TO CNDTACUM-FECHA-FRA
            MOVE CNDTACNU-FECHA-VCTO     TO CNDTACUM-FECHA-VCTO.
            MOVE CNDTACNU-MOVDEB(W-LONG) TO CNDTACUM-MOVDEB(W-LONG)
            MOVE CNDTACNU-MOVCRE(W-LONG) TO CNDTACUM-MOVCRE(W-LONG)
            MOVE CNDTACNU-SALDO (W-LONG) TO CNDTACUM-SALDO(W-LONG).
       F-MVTO-CAMPOS.
      *
       I-MVTO-DOCTOS.
      *     IF  GRUCTA < 4 OR ESTACTA = "S"
            IF  ESTACTA = "S"
                DISPLAY "CTA EXCLUIDA" LINE 24 POSITION 1
                DISPLAY CNDTACUM-CLAVE LINE 24 POSITION 20
                GO F-MVTO-DOCTOS.
            IF  CODPGT  = "S"  AND INDDETAL = "S" AND
                CNDTACUM-FRA = ZEROS
                DISPLAY "CTA EXCLUIDA" LINE 24 POSITION 1
                DISPLAY CNDTACUM-CLAVE LINE 24 POSITION 20
                GO F-MVTO-DOCTOS.

           IF CNDTACUM-NIT =  0    AND CNDTACUM-FRA = 0
              AND CODPGT   = "S"   GO F-MVTO-DOCTOS.
            MOVE CNDTACUM-MAYOR    TO MAYCTA-DOCTOS
            MOVE CNDTACUM-SUB      TO SUBCTA-DOCTOS
            MOVE CNDTACUM-AUX      TO AUXCTA-DOCTOS
            MOVE CUENTA-DOCTOS     TO CUENTA-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT04CTA-DOCTOS
                                      ALTCTA1-DOCTOS
                                      ALT09CTA-DOCTOS.
            MOVE CUENTA-DOCTOS     TO ALTCTA1-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT09CTA-DOCTOS
                                      ALT12CTA-DOCTOS
            MOVE CNDTACUM-NIT      TO ALTNIT-DOCTOS
            MOVE CNDTACUM-FRA      TO ALTFRA-DOCTOS
                                      NUMFRA-DOCTOS
            MOVE CNDTACUM-NOM-ALT2 TO CONCEPTO-DOCTOS
            MOVE CNDTACUM-ANOFRA   TO AA-COMP-DOCTOS
            MOVE CNDTACUM-MESFRA   TO MM-COMP-DOCTOS
            MOVE CNDTACUM-DIAFRA   TO DD-COMP-DOCTOS
            MOVE CNDTACUM-ANOVCT   TO AA-VCTO-DOCTOS
            MOVE CNDTACUM-MESVCT   TO MM-VCTO-DOCTOS
            MOVE CNDTACUM-DIAVCT   TO DD-VCTO-DOCTOS.
            MOVE ZEROS             TO VRBASE-DOCTOS
                                      CONDICION-DOCTOS
            MOVE MESW               TO ALTPER-DOCTOS
                                       ALTMES-DOCTOS
            MOVE 15  TO  ALTTIP-DOCTOS    ALTTIP03-DOCTOS
                         ALT09TIP-DOCTOS  ALTTIP08-DOCTOS
                         RADICTIP-DOCTOS  ALTTIP04-DOCTOS
                         ALTTIP12-DOCTOS
            MOVE 01                TO  ALTEMP-DOCTOS
            MOVE 1                 TO  RADICDOC-DOCTOS
            MOVE 1                 TO  ALTFRA-DOCTOS
                                       RADICDOC-DOCTOS
                                       RADICA04-DOCTOS
                                       RADICA02-DOCTOS
                                       COSTO12-DOCTOS
                                       RADICA05-DOCTOS
                                       ALTFRA09-DOCTOS.

            ADD  1                 TO  W-CONTA
            MOVE W-CONTA           TO  RADIC-AUTOMATICA-DOCTOS.

            MOVE WK-FECHA-HOY      TO  FECHOY-DOCTOS.
            MOVE  ZEROS            TO  BANCO-DOCTOS
                                       BANCO-BENEF-DOCTOS
                                       CHEQUE-BENEF-DOCTOS
                                       CONSIGNACION-DOCTOS.
            WRITE  REG-DOCTOS.
       F-MVTO-DOCTOS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Captura de datos por teclado                      ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

       I-ACEPTA-MES.
           MOVE ZEROS       TO WK-EXCEPTION
           MOVE   13        TO W-LONG.
           DISPLAY W-LONG   LINE 11 POSITION 50 REVERSE.
           IF  F2  OR  ESC  PERFORM F-FIN  THRU   F-FIN-MODE.
           IF W-LONG = 0 OR W-LONG > 14
              DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-ACEPTA-MES.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 45 REVERSE.
               IF W-LONG = 1
                 COMPUTE MESW = W-LONG + 12
               ELSE
                 COMPUTE MESW = W-LONG - 1.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE  <C> CONTINUAR "
                               LINE 24 POSITION 1 REVERSE
                    DISPLAY " [F2] TERMINAR "
                               LINE 24 POSITION 32 REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-WIN-CTAS.
               DISPLAY " "  LINE 24  POSITION 80.
       F-WIN-CTAS.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Start y lectura de archivos                       ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-START-CNDTACNU.
               MOVE   ZEROS                TO  CNDTACNU-CLAVE.
               START CNDTACNU  KEY >   CNDTACNU-CLAVE INVALID KEY
                 DISPLAY "ERROR START-CNDTACNU"  LINE 23 POSITION 1.
      *              PERFORM RUTINA-ERROR-CNDTACUM
      *              PERFORM I-FIN-MODE    THRU F-FIN-MODE.
       F-START-CNDTACNU.
      *
       I-LECTURA-SEC.
           IF  SW-EOF   =   1
                      GO  F-LECTURA-SEC.
           MOVE ZEROS         TO  SW-EOF.
           READ CNDTACNU NEXT  RECORD AT END
                     MOVE 1 TO SW-EOF.
       F-LECTURA-SEC.
      *
       I-LEE-CNDTACUM.
           MOVE ZEROS          TO  SW-EOF.
           READ CNDTACUM NEXT  RECORD AT END
                     GO  F-LEE-CNDTACUM.
           ADD    1     TO  W-REG-CNDTACNU.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNDTACNU.
           MOVE ZEROS          TO  SW-EOF.
           READ CNDTACNU NEXT  RECORD AT END
                     GO  F-LEE-CNDTACNU.
           ADD    1     TO  W-REG-CNDTACNU.
       F-LEE-CNDTACNU.
      *
       I-LEE-CNCATCTA.
           MOVE ZEROS          TO  FIN-OK.
           READ CNCATCTA INVALID KEY
                MOVE   1    TO  FIN-OK
                     GO  F-LEE-CNCATCTA.
       F-LEE-CNCATCTA.

      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       Dise¤o de Pantalla y visualizacion datos          ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

         I-MOSTRAR-CAMPOS.
            DISPLAY CNDTACUM-MAYOR LINE 16 POSITION 31
            DISPLAY CNDTACUM-SUB   LINE 16 POSITION 36
            DISPLAY CNDTACUM-AUX   LINE 16 POSITION 40

            DISPLAY CNDTACNU-MAYOR LINE 16 POSITION 50
            DISPLAY CNDTACNU-SUB   LINE 16 POSITION 55
            DISPLAY CNDTACNU-AUX   LINE 16 POSITION 59

            MOVE    CNDTACUM-NIT   TO WI-Z9.
            DISPLAY WI-Z9          LINE 19 POSITION 30
            MOVE    CNDTACNU-NIT   TO WI-Z9.
            DISPLAY WI-Z9          LINE 19 POSITION 50

            DISPLAY CNDTACUM-FRA   LINE 20 POSITION 30

            MOVE    CNDTACUM-MOVDEB(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 17 POSITION 30
            MOVE    CNDTACUM-MOVCRE(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 18 POSITION 30

            MOVE    CNDTACUM-MOVDEB(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 17 POSITION 50
            MOVE    CNDTACUM-MOVCRE(W-LONG)  TO WI-Z11P
            DISPLAY WI-Z11P           LINE 18 POSITION 50.
       F-MOSTRAR-CAMPOS.
      *
       I-DISPLAY.
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.

               DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ»"
              LINE 09 POSITION 15 CONTROL SG-CONTROL-STRING
               DISPLAY "º
      -       "      º"
                                              LINE 10 POSITION 15
               DISPLAY "º MES A PROCESAR   :
      -       "      º"
                                              LINE 11 POSITION 15

               DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -       "ÍÍÍÍÍÍ¼"
                                              LINE 12 POSITION 15
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "[ESC MENU]  [F2 SALIR] "
           LINE 25 POSITION 30 CONTROL SG-CONTROL-STRING HIGH REVERSE.
           DISPLAY "SISTEMA-FINANCIERO"  LINE 3  POSITION 2
           DISPLAY
           "CONVERSION DE UN CNDTACNU EN UN CNDTACUM"
                     LINE    6  POSITION   25 REVERSE BEEP.
       RUTINA-ERROR-NOMINA.
       F-DISPLAY.
      *
       I-PANTALLA.
               DISPLAY
           "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
            LINE 14 POSITION 15 CONTROL SG-CONTROL-STRING HIGH
           "º                  CNDTACUM                  CNDTACNU     º"
                                     LINE 15 POSITION 15

           "º  CUENTA                                                 º"
                                     LINE 16 POSITION 15

           "º  DEBITOS                                                º"
                                     LINE 17 POSITION 15

           "º  CREDITOS                                               º"
                                     LINE 18 POSITION 15

           "º  NIT                                                    º"
                                     LINE 19 POSITION 15

           "º                                                         º"
                                     LINE 20 POSITION 15

           "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
                       LINE 21 POSITION 15.
       F-PANTALLA.
      *
       I-STATUS-CNDTACUM.
                   OPEN OUTPUT CNDTACUM CLOSE CNDTACUM
                   OPEN I-O    CNDTACUM
                   OPEN INPUT  CNCATCTA
                   PERFORM I-ACTUALIZE-CTAS THRU F-ACTUALIZE-CTAS
                   CLOSE CNCATCTA
               CLOSE CNDTACUM.
       F-STATUS-CNDTACUM.

       I-ACTUALIZE-CTAS.
           MOVE    0               TO   RUT-OK.
           DISPLAY "ESPERE UN MOMENTO..."
                LINE 24 POSITION 1
           DISPLAY "VERIFICANDO HISTORICO:"
                LINE 24 POSITION 22
           DISPLAY LABEL-CNDTACUM LINE 24 POSITION 50
           PERFORM I-PROCESA-CNCATCTA THRU F-PROCESA-CNCATCTA
                   UNTIL RUT-OK = 1.
       F-ACTUALIZE-CTAS.

       I-PROCESA-CNCATCTA.
           IF RUT-OK = 1 GO F-PROCESA-CNCATCTA.
           PERFORM I-LEE-CNCATCTAC THRU F-LEE-CNCATCTAC
           PERFORM I-MUEVA-CTAS    THRU F-MUEVA-CTAS
           PERFORM I-GRABA-REGIACUM THRU F-GRABA-REGIACUM.
       F-PROCESA-CNCATCTA.

       I-MUEVA-CTAS.
           MOVE MAYMAE TO CNDTACUM-MAYOR
           MOVE SUBMAE TO CNDTACUM-SUB
           MOVE AUXMAE TO CNDTACUM-AUX
           MOVE 0      TO CNDTACUM-NIT
                          CNDTACUM-FRA
                          CNDTACUM-ALTER-1
                          CNDTACUM-FECHA-FRA
                          CNDTACUM-FECHA-VCTO.
           MOVE NOMCTA TO CNDTACUM-ALTER-2.
       F-MUEVA-CTAS.

       I-LEE-CNCATCTAC.
           READ CNCATCTA NEXT AT END MOVE 1 TO RUT-OK.
       F-LEE-CNCATCTAC.

       I-GRABA-REGIACUM.
           PERFORM I-ZEROS THRU F-ZEROS VARYING X FROM 1 BY 1
                      UNTIL X > 14.
           WRITE REG-CNDTACUM INVALID KEY REWRITE REG-CNDTACUM.
       F-GRABA-REGIACUM.
      *
       I-ZEROS.
           MOVE  ZEROS  TO  CNDTACUM-MOVDEB(X).
           MOVE  ZEROS  TO  CNDTACUM-MOVCRE(X).
           MOVE  ZEROS  TO  CNDTACUM-SALDO(X).
       F-ZEROS.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\PANTALLAS.PRO".

               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\CNDTACNU.PLB".
               COPY "..\PLB\CNDOCTOS.PLB".


