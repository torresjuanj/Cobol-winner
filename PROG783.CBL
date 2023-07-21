       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG783.
      ******************************************************************
      *  COMPTE DE CIERRE DE A•O                                       *
      *  CANCELACION DE CUENTAS DE RESULTADO    ABR-27-2002            *
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
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CNDOCTOS.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\EMPRESAS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(07) VALUE "PROG783".
       77      ID-PRG              PIC X(7)  VALUE "PROG783".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       77      WK-GANANCIA         PIC 9(10) VALUE  3605050000.
       77      WK-PERDIDA          PIC 9(10) VALUE  3610050000.
       77      WK-ACTIVO           PIC 9(10) VALUE  2406000000.
       77      WK-PASIVO           PIC 9(10) VALUE  2406000000.
       77      NIT-ACTIVO          PIC 9(12) VALUE  ZEROS.
       77      WK-IVA              PIC 9(10) VALUE  2408500200.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(04) OCCURS 25 TIMES.

        01      ANO-XX1.
          03    XX-ANO            PIC 9(04)  VALUE ZEROS.
          03    XX-MES            PIC 9(02)  VALUE ZEROS.
          03    XX-SUB            PIC 9(02)  VALUE ZEROS.

               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
           ACCEPT WK-FECHA-HOY FROM DATE
           IF WK-ANO           >= 0 OR
              WK-ANO           < 2000
              ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.
               PERFORM I-USUARIOS        THRU F-USUARIOS.
               IF      SW-USER = 1       EXIT PROGRAM.
               PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
               PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
               PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
               PERFORM I-ACEPTA-DATOS    THRU F-ACEPTA-DATOS.
               PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
               PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
               PERFORM I-PRIMER-REGISTRO THRU F-PRIMER-REGISTRO
               PERFORM I-DISPLAY         THRU F-DISPLAY.
               PERFORM I-PROCESO-INFORME THRU F-PROCESO-INFORME
                       UNTIL FIN-OK = 1.
               MOVE          0             TO SALDO.
               PERFORM I-EVALUE-UTILIDAD THRU F-EVALUE-UTILIDAD.
               PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE    CON-ANO               TO  XX-ANO.
               MOVE      13                  TO  XX-MES.
               MOVE      01                  TO  XX-SUB.
               PERFORM I-LABEL-CNDOCTOS    THRU  F-LABEL-CNDOCTOS.
               PERFORM I-LABEL-CNDTACUM    THRU  F-LABEL-CNDTACUM.
               MOVE "π"                      TO  WK-RAYAS-U
               MOVE "Ã"                      TO  WK-RAYAS-P
               MOVE "∂"                      TO  WK-RAYA-U
               MOVE "«"                      TO  WK-RAYA-P
               MOVE  1                       TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CNDTACUM CNNITCED
               OPEN INPUT  CNCATCTA
               OPEN OUTPUT CNDOCTOS.
               CLOSE CNDOCTOS.
               OPEN I-O    CNDOCTOS.
       F-ABRE-ARCHIVOS.
      *
       I-FIN-MODE.
            CLOSE CNCATCTA CNDOCTOS CNDTACUM CNNITCED.
            MOVE W-IDENT-PROGRAMA     TO CON-PROGRAMA
            CANCEL "PROG783.COB"
            CALL   "PROG715.COB" USING AREA-LINK-NOMINA
            CANCEL "PROG715.COB".
            CALL   "PROG781.COB" USING AREA-LINK-NOMINA
            CANCEL "PROG781.COB".
            CALL   "PROG731.COB" USING AREA-LINK-NOMINA
            CANCEL "PROG731.COB".
            EXIT PROGRAM.
            STOP RUN.
       F-FIN-MODE.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥               I-PRIMER-REGISTRO                         ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PRIMER-REGISTRO.
           PERFORM I-LEE-CNDTACUM       THRU F-LEE-CNDTACUM
           PERFORM I-LEE-CNCATCTA       THRU F-LEE-CNCATCTA
           IF NIVEL = 1
              PERFORM I-ACUMULE-NIVEL1  THRU F-ACUMULE-NIVEL1.
           MOVE    GRUCTA                 TO SW-INGRESO.
           MOVE    CTAMAE                 TO WK-CUENTA
           PERFORM I-MUEVE-INFORME-1    THRU F-MUEVE-INFORME-1.
       F-PRIMER-REGISTRO.

       I-ACUMULE-NIVEL1.
      *    IF CTAMAE = 1000000000
      *       MOVE CNDTACUM-SALDO(MESW) TO W-ACTIVOS.
      *    IF CTAMAE = 2000000000
      *       MOVE CNDTACUM-SALDO(MESW) TO W-PASIVOS.
           IF CTAMAE = 3000000000
              MOVE CNDTACUM-SALDO(MESW) TO W-PATRIMONIO.
           IF CTAMAE = 4000000000
              MOVE CNDTACUM-SALDO(MESW) TO W-INGRESOS.
           IF CTAMAE = 5000000000
              MOVE CNDTACUM-SALDO(MESW) TO W-GASTOS.
           IF CTAMAE = 7000000000
              MOVE CNDTACUM-SALDO(MESW) TO W-COSTOS.
       F-ACUMULE-NIVEL1.
      *
       I-PROCESO-INFORME.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF FIN-OK = 1 GO F-PROCESO-INFORME.
           IF GRUCTA = SW-INGRESO
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
              GO I-PROCESO-INFORME
           ELSE
              PERFORM I-REG-NUEVO   THRU F-REG-NUEVO
              GO I-PROCESO-INFORME.
       F-PROCESO-INFORME.
      *
       I-REG-NUEVO.
           IF NIVEL = 1
                 PERFORM I-ACUMULE-NIVEL1  THRU F-ACUMULE-NIVEL1.
           MOVE    GRUCTA                    TO SW-INGRESO.
           PERFORM I-MUEVE-INFORME-1       THRU F-MUEVE-INFORME-1.
       F-REG-NUEVO.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-MUEVE-INFORME.            ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-MUEVE-INFORME-1.
            IF  GRUCTA > 7 OR ESTACTA = "S"
                DISPLAY "CTA EXCLUIDA" LINE 24 POSITION 1
                DISPLAY CNDTACUM-CLAVE LINE 24 POSITION 20
                GO F-MUEVE-INFORME-1.
           IF CNDTACUM-SALDO(MESW) = 0 GO F-MUEVE-INFORME-1.
      *
           IF CNDTACUM-NIT =  0    AND CNDTACUM-FRA = 0
              AND INDDETAL = "S"   GO F-MUEVE-INFORME-1.

           IF CNDTACUM-NIT =  0    AND CNDTACUM-FRA = 0
              AND CODPGT   = "S"   GO F-MUEVE-INFORME-1.

           IF CNDTACUM-NIT >  0    AND CNDTACUM-FRA = 0
              AND INDDETAL = "S"   GO F-MUEVE-INFORME-1.

            IF   CTAMAE     <   4000000000  AND
                 J        NOT =  0          AND
                 CTAMAE   NOT =  WK-CUENTA
                 PERFORM I-EVALUE-CIERRE  THRU F-EVALUE-CIERRE
                 MOVE  CTAMAE               TO WK-CUENTA.

            IF   CTAMAE     >   4000000000  AND
                 K          =  0
                 PERFORM I-EVALUE-CIERRE  THRU F-EVALUE-CIERRE
                 MOVE 6 TO K.

            IF SW-INGRESO < 5  MOVE 26  TO W-COMP.
            IF SW-INGRESO > 3  MOVE 25  TO W-COMP
                               MOVE  1  TO XX-SUB.
            ADD 1                       TO W-CONTA
            PERFORM I-DATOS-CNDTACUM  THRU F-DATOS-CNDTACUM
            PERFORM I-EVALUE-SALDO    THRU F-EVALUE-SALDO
            PERFORM I-GRABA-DOCTOS    THRU F-GRABA-DOCTOS.
       F-MUEVE-INFORME-1.
      *
       I-EVALUE-SALDO.
            IF CNDTACUM-SALDO(MESW)      IS POSITIVE
               MOVE 2                    TO SIGNO-DOCTOS
               MOVE CNDTACUM-SALDO(MESW) TO VALOR-DOCTOS
               COMPUTE VALOR-DOCTOS = VALOR-DOCTOS * (-1)
               ADD  CNDTACUM-SALDO(MESW) TO W-ACTIVOS.

            IF CNDTACUM-SALDO(MESW) IS NEGATIVE
               MOVE 1                    TO SIGNO-DOCTOS
               MOVE CNDTACUM-SALDO(MESW) TO VALOR-DOCTOS
               COMPUTE VALOR-DOCTOS = VALOR-DOCTOS * (-1)
               ADD  CNDTACUM-SALDO(MESW) TO W-PASIVOS.
       F-EVALUE-SALDO.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-GRABA-DOCTOS.             ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-GRABA-DOCTOS.
           DISPLAY MAYMAE                 LINE 20 POSITION 27
           DISPLAY SUBMAE                 LINE 20 POSITION 33
           DISPLAY AUXMAE                 LINE 20 POSITION 36
           DISPLAY CNDTACUM-MAYOR         LINE 20 POSITION 53
           DISPLAY CNDTACUM-SUB           LINE 20 POSITION 58
           DISPLAY CNDTACUM-AUX           LINE 20 POSITION 61.
           DISPLAY NOMCTA                 LINE 21 POSITION 16.
           DISPLAY CNDTACUM-NIT           LINE 22 POSITION 27.
           WRITE REG-DOCTOS.
       F-GRABA-DOCTOS.

      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
      *OJO CONFIRMAR.
       I-EVALUE-CIERRE.
           COMPUTE SALDO = W-ACTIVOS + W-PASIVOS.
           MOVE    ZEROS     TO    W-ACTIVOS  W-PASIVOS.
           IF CNDTACUM-MAYOR < 2000
              MOVE WK-ACTIVO         TO CUENTA-DOCTOS
              MOVE "Cierre cuenta del activo      " TO
                                           CONCEPTO-DOCTOS
              MOVE SALDO             TO VALOR-DOCTOS
              MOVE 1                 TO SIGNO-DOCTOS.
           IF CNDTACUM-MAYOR > 2000
              MOVE WK-PASIVO         TO CUENTA-DOCTOS
              MOVE "Cierre cuenta del pasivo       " TO CONCEPTO-DOCTOS
              MOVE SALDO             TO VALOR-DOCTOS
              MOVE 2                 TO SIGNO-DOCTOS.
           IF CNDTACUM-MAYOR = 2408
              MOVE WK-IVA            TO CUENTA-DOCTOS
              MOVE "Cierre cuenta del pasivo       " TO CONCEPTO-DOCTOS
              MOVE SALDO             TO VALOR-DOCTOS
              MOVE 2                 TO SIGNO-DOCTOS.
            PERFORM I-DATOS-CIERRE    THRU I-DATOS-CIERRE
            PERFORM I-GRABA-DOCTOS    THRU F-GRABA-DOCTOS.
            ADD   1       TO XX-SUB.
            MOVE  ZEROS   TO W-CONTA.
       F-EVALUE-CIERRE.
      *
       I-EVALUE-UTILIDAD.
           COMPUTE SALDO = W-GASTOS + W-COSTOS + W-INGRESOS
           IF SALDO IS POSITIVE
              MOVE WK-PERDIDA        TO CUENTA-DOCTOS
              MOVE
              "PERDIDA DEL PRESENTE EJERCICIO" TO
                                           CONCEPTO-DOCTOS
              MOVE SALDO             TO VALOR-DOCTOS
              MOVE 1                 TO SIGNO-DOCTOS.
           IF SALDO IS NEGATIVE
              MOVE WK-GANANCIA       TO CUENTA-DOCTOS
              MOVE
              "UTILIDAD DEL PRESENTE EJERCICIO" TO
                                           CONCEPTO-DOCTOS
              MOVE SALDO                TO VALOR-DOCTOS
              MOVE 2                    TO SIGNO-DOCTOS.
            PERFORM I-DATOS-CIERRE    THRU I-DATOS-CIERRE
            PERFORM I-GRABA-DOCTOS    THRU F-GRABA-DOCTOS.
       F-EVALUE-UTILIDAD.
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-ACTIVOS     W-PASIVOS SALDO
                         W-PATRIMONIO  W-INGRESOS
                         W-COSTOS      W-GASTOS FIN-OK.
       F-CEROS-VARIABLES.
      *
       I-ACEPTA-MES.
           MOVE  13  TO W-LONG
           DISPLAY "PERIODO :   13 "   LINE 10 POSITION 40 .
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 54 .
           COMPUTE MESW = W-LONG - 1.
       F-ACEPTA-MES.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-REGRABACION.              ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-DATOS-CNDTACUM.
            IF   CTAMAE     <   4000000000  AND
                 J          =  0
                 MOVE  CTAMAE      TO WK-CUENTA.
            DISPLAY "PROCESANDO MOVIMIENTO DE CAMPOS...."
                  LINE 24 POSITION 1.
            MOVE CNDTACUM-MAYOR    TO MAYCTA-DOCTOS
            MOVE CNDTACUM-SUB      TO SUBCTA-DOCTOS
            MOVE CNDTACUM-AUX      TO AUXCTA-DOCTOS
            MOVE CUENTA-DOCTOS     TO ALTCTA1-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT04CTA-DOCTOS
                                      ALT12CTA-DOCTOS
                                      ALT09CTA-DOCTOS.
            MOVE CNDTACUM-NIT      TO ALTNIT-DOCTOS
                                      NITAUX-DOCTOS
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
            MOVE MESW              TO ALTPER-DOCTOS
                                      ALTMES-DOCTOS
            MOVE W-COMP TO    ALTTIP-DOCTOS    ALTTIP03-DOCTOS
                              ALT09TIP-DOCTOS  ALTTIP08-DOCTOS
                              ALTTIP12-DOCTOS
                              RADICTIP-DOCTOS  ALTTIP04-DOCTOS.
            MOVE 01                TO  ALTEMP-DOCTOS      J
            MOVE ANO-XX1           TO  RADICDOC-DOCTOS
            MOVE ANO-XX1           TO  ALTFRA-DOCTOS
                                       RADICDOC-DOCTOS
                                       COSTO12-DOCTOS
                                       RADICA04-DOCTOS
                                       RADICA05-DOCTOS
                                       RADICA02-DOCTOS
                                       ALTFRA09-DOCTOS.
            MOVE W-CONTA           TO  RADIC-AUTOMATICA-DOCTOS.
            MOVE WK-FECHA-HOY      TO  FECHOY-DOCTOS.
            MOVE ZEROS             TO  BANCO-DOCTOS
                                       CIUDAD-DOCTOS
                                       CIUDAD12-DOCTOS
                                       BANCO01-DOCTOS
                                       BANCO-BENEF-DOCTOS
                                       CHEQUE-BENEF-DOCTOS
                                       SW-CONS-DOCTOS
                                       SW-IMP-DOCTOS
                                       CONSIGNACION-DOCTOS.
       F-DATOS-CNDTACUM.
      *
       I-DATOS-CIERRE.
            ADD 1                  TO W-CONTA
            MOVE CUENTA-DOCTOS     TO ALTCTA1-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT12CTA-DOCTOS
                                      ALT09CTA-DOCTOS
            MOVE CUENTA-DOCTOS     TO CUENTA-DOCTOS
                                      ALTCTA-DOCTOS
                                      ALT04CTA-DOCTOS
                                      ALTCTA1-DOCTOS
                                      ALT09CTA-DOCTOS.
            MOVE ZEROS             TO ALTNIT-DOCTOS
                                      NITAUX-DOCTOS
            IF   CUENTA-DOCTOS = WK-ACTIVO
                 MOVE NIT-ACTIVO   TO ALTNIT-DOCTOS
                                      NITAUX-DOCTOS.
            IF   CUENTA-DOCTOS = WK-PASIVO
                 MOVE NIT-ACTIVO   TO ALTNIT-DOCTOS
                                      NITAUX-DOCTOS.

            MOVE ZEROS             TO ALTFRA-DOCTOS
                                      NUMFRA-DOCTOS
            MOVE CON-ANO           TO AA-COMP-DOCTOS
            MOVE MESW              TO MM-COMP-DOCTOS
            MOVE 31                TO DD-COMP-DOCTOS
            MOVE CON-ANO           TO AA-VCTO-DOCTOS
            MOVE MESW              TO MM-VCTO-DOCTOS
            MOVE 31                TO DD-VCTO-DOCTOS.
            MOVE ZEROS             TO VRBASE-DOCTOS
                                         CONDICION-DOCTOS
            MOVE MESW              TO ALTPER-DOCTOS
                                          ALTMES-DOCTOS
            MOVE W-COMP  TO  ALTTIP-DOCTOS    ALTTIP03-DOCTOS
                             ALT09TIP-DOCTOS  ALTTIP08-DOCTOS
                             ALTTIP12-DOCTOS
                             RADICTIP-DOCTOS  ALTTIP04-DOCTOS.
            MOVE 01                    TO  ALTEMP-DOCTOS
            MOVE ANO-XX1               TO  RADICDOC-DOCTOS
            MOVE ANO-XX1               TO  ALTFRA-DOCTOS
                                           COSTO12-DOCTOS
                                           RADICDOC-DOCTOS
                                           RADICA04-DOCTOS
                                           RADICA05-DOCTOS
                                           RADICA02-DOCTOS
                                           ALTFRA09-DOCTOS.
           MOVE W-CONTA               TO  RADIC-AUTOMATICA-DOCTOS.
           MOVE WK-FECHA-HOY          TO  FECHOY-DOCTOS.
           MOVE ZEROS             TO  BANCO-DOCTOS
                                      CIUDAD-DOCTOS
                                      CIUDAD12-DOCTOS
                                      BANCO01-DOCTOS
                                      BANCO-BENEF-DOCTOS
                                      SW-CONS-DOCTOS
                                      SW-IMP-DOCTOS
                                      CHEQUE-BENEF-DOCTOS
                                      CONSIGNACION-DOCTOS.
        F-DATOS-CIERRE.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       LECTURAS Y MANIPULACION DE ARCHIVOS               ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               MOVE 1354                   TO  CNDTACUM-MAYOR
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-LEE-CNDTACUM.
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                   MOVE 1 TO FIN-OK GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR < 1355
                 DISPLAY "ESPERE UN MOMENTO POR FAVOR..."
                      LINE 24 POSITION 1
                 DISPLAY CNDTACUM-MAYOR LINE 24 POSITION 50
                      GO I-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR > 3999  GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR = 1355  AND
                 CNDTACUM-SUB   = 15     GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR = 1355  AND
                 CNDTACUM-SUB   = 17    GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR = 2408  AND
                 CNDTACUM-SUB   = 50    GO I-LEE-CNDTACUM.

              IF CNDTACUM-MAYOR = 1355  AND
                 CNDTACUM-SUB   = 50     GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR = 2365   OR
                 CNDTACUM-MAYOR = 2367   OR
                 CNDTACUM-MAYOR = 2408   OR
                 CNDTACUM-MAYOR = 2368
                    GO F-LEE-CNDTACUM
              ELSE
                    GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNCATCTA.
           MOVE CNDTACUM-MAYOR TO MAYMAE
           MOVE CNDTACUM-SUB   TO SUBMAE
           MOVE CNDTACUM-AUX   TO AUXMAE
           MOVE 0        TO SW-EOF.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       PANTALLAS Y CAPTURAS DEL PROGRAMA                 ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-ACEPTA-DATOS.
           PERFORM I-CUENTA-GANANCIA  THRU F-CUENTA-GANANCIA
           IF         CUP               GO I-ACEPTA-DATOS.
        I-DAT-1.
           PERFORM I-CUENTA-PERDIDA   THRU F-CUENTA-PERDIDA
           IF         CUP               GO I-ACEPTA-DATOS.
        I-DAT-2.
           PERFORM I-CUENTA-ACTIVO    THRU F-CUENTA-ACTIVO
           IF         CUP               GO I-DAT-1.
       I-DAT-3.
           PERFORM I-NIT-ACTIVO       THRU F-NIT-ACTIVO
           IF         CUP               GO I-DAT-2.
        I-DAT-4.
           PERFORM I-CUENTA-PASIVO    THRU F-CUENTA-PASIVO
           IF         CUP               GO I-DAT-3.
       I-DAT-5.
           PERFORM I-CUENTA-IVA       THRU F-CUENTA-IVA.
           IF         CUP               GO I-DAT-4.
        I-DAT-6.
           PERFORM I-CONFIRMA-COMP    THRU F-CONFIRMA-COMP.
           IF         CUP               GO I-DAT-5.
       F-ACEPTA-DATOS.
      *
       I-CUENTA-GANANCIA.
           MOVE ZEROS               TO  WK-EXCEPTION.
           ACCEPT WK-GANANCIA     LINE 11 POSITION 33                               UPDATE PROMPT NO BEEP
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE  WK-GANANCIA  TO CTAMAE
           READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-CUENTA-GANANCIA.
           IF  ESTACTA = "S" GO I-CUENTA-GANANCIA.
           DISPLAY NOMCTA  LINE 11 POSITION 45  SIZE 30 LOW
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-CUENTA-GANANCIA.
      *
       I-CUENTA-PERDIDA.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           ACCEPT WK-PERDIDA     LINE 12 POSITION 33                               UPDATE PROMPT NO BEEP
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP   GO F-CUENTA-PERDIDA.
           MOVE  WK-PERDIDA  TO CTAMAE
           READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-CUENTA-PERDIDA.
           IF  ESTACTA = "S" GO I-CUENTA-PERDIDA.
           DISPLAY NOMCTA  LINE 12 POSITION 45  SIZE 30 LOW
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-CUENTA-PERDIDA.
      *
       I-CUENTA-ACTIVO.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           ACCEPT WK-ACTIVO     LINE 13 POSITION 33                               UPDATE PROMPT NO BEEP
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP   GO F-CUENTA-ACTIVO.
           MOVE  WK-ACTIVO  TO CTAMAE
           READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-CUENTA-ACTIVO.
           IF  ESTACTA = "S" GO I-CUENTA-ACTIVO.
           DISPLAY NOMCTA  LINE 13 POSITION 45  SIZE 30 LOW
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-CUENTA-ACTIVO.
      *
       I-CUENTA-PASIVO.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           ACCEPT WK-PASIVO     LINE 15 POSITION 33                               UPDATE PROMPT NO BEEP
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP   GO F-CUENTA-PASIVO.
           MOVE  WK-PASIVO  TO CTAMAE
           READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-CUENTA-PASIVO.
           IF  ESTACTA = "S" GO I-CUENTA-PASIVO.
           DISPLAY NOMCTA  LINE 15 POSITION 45  SIZE 30 LOW
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-CUENTA-PASIVO.
      *
       I-NIT-ACTIVO.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           ACCEPT NIT-ACTIVO     LINE 14 POSITION 33                               UPDATE PROMPT NO BEEP
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP   GO F-NIT-ACTIVO.
           MOVE  NIT-ACTIVO  TO CODIM
           READ CNNITCED WITH NO LOCK INVALID KEY
                DISPLAY " NIT INEXISTENTE......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-NIT-ACTIVO.
           DISPLAY NOMBM   LINE 14 POSITION 49  SIZE 25 LOW.
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-NIT-ACTIVO.
      *
       I-CUENTA-IVA.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           ACCEPT WK-IVA     LINE 16 POSITION 33
                                  UPDATE  NO BEEP
                                  ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP   GO F-CUENTA-IVA.
           MOVE  WK-IVA  TO CTAMAE
           READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY " CUENTA ERRADA .......DIGITE NUEVAMENTE"
                          LINE 24 POSITION 1 REVERSE BLINK
                          GO I-CUENTA-IVA.
           IF  ESTACTA = "S" GO I-CUENTA-IVA.
           DISPLAY NOMCTA  LINE 16 POSITION 45  SIZE 30 LOW
           DISPLAY WK-ESPACIOS      LINE 24 POSITION 1.
       F-CUENTA-IVA.
      *
       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1 REVERSE
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-CONFIRMA-COMP.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-PANTALLA-1B.
           MOVE "P783 - COMPBTE DE CIERRE DE A•O"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           MOVE "HIGH" TO SG-INTENSITY
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕª"                LINE 08 POSITION 07
           "∫    CANCELACION DE CUENTAS DE RESULTADO PARA EL CIERRE DE A
      -    "•O        ∫"                LINE 09 POSITION 07
           "∫
      -    "          ∫"                LINE 10 POSITION 07
           "∫  CUENTA GANANCIAS    :
      -    "          ∫"                LINE 11 POSITION 07
           "∫  CUENTA PERDIDAS     :
      -    "          ∫"                LINE 12 POSITION 07
           "∫  RFTE A FAVOR(Activo):
      -    "          ∫"                LINE 13 POSITION 07
           "∫  DIGITE NIT (Dian  ) :
      -    "          ∫"                LINE 14 POSITION 07
           "∫  RFTE A PAGAR(Pasivo):
      -    "          ∫"                LINE 15 POSITION 07
           "∫  CUENTA I.V.A        :
      -    "          ∫"                LINE 16 POSITION 07
           "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕº"                LINE 17 POSITION 07
           DISPLAY
           "NOTA:DEBE TENER DERECHOS A LOS PROGRAMAS P715,P731,P785"
                                 LINE 18 POSITION 07 BLINK
           DISPLAY WK-ESPACIOS          LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-DISPLAY.
           DISPLAY
           "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                          LINE 19 POSITION 15 LOW
           "∫ P.U.C   :                 HISTORIC:                 ∫"
                                          LINE 20 POSITION 15 LOW
           "∫                                                     ∫"
                                          LINE 21 POSITION 15 LOW
           "∫ N.I.T   :                   TAMANO:                 ∫"
                                          LINE 22 POSITION 15 LOW
           "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                          LINE 23 POSITION 15 LOW
           DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".

               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\CNDTACUM.PLB".
