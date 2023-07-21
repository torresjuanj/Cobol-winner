       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG300.
      ***********************************************************
      * PROCESO DE PRENOMINA            ABR 27-2002             *
      ***********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\CIERRANO.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
               COPY "..\DYC\CONCPTOS.DYC".
               COPY "..\DYC\TABLAEMP.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CONTRATO.DYC".
               COPY "..\DYC\NOMINOVE.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\SEL\INFORMES.SEL".
               COPY "..\SEL\REPORTES.SEL".
               COPY "..\SEL\LISTADOS.SEL".
               COPY "..\SEL\CERTIFIC.SEL".
               COPY "..\DYC\TEMPINFO.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\NOVEFIJA.DYC".
               COPY "..\DYC\RETENFTE.DYC".
               COPY "..\DYC\CTRLSUEL.DYC".
      ***************************************************************
      *                 DATA DIVISION                               *
      ***************************************************************
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\CIERRANO.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\CONCPTOS.FD".
               COPY "..\FD\TABLAEMP.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\NOMINOVE.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\LISTADOS.FD".
               COPY "..\FD\TEMPINFO.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\RETENFTE.FD".
               COPY "..\FD\CERTIFIC.FD".
               COPY "..\FD\NOVEFIJA.FD".
               COPY "..\FD\CTRLSUEL.FD".
      ***************************************************************
      *                 WORKING-STORAGE SECTION                     *
      ***************************************************************
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG300".
       77      ID-PRG              PIC X(7)  VALUE "PROG300".
       77  W-NOMEMP-DE3               PIC X(03) VALUE SPACES.
      ***************************************************************
      *                 LBLEL DE ARCHIVOS                           *
      ***************************************************************
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4) OCCURS 25 TIMES.
               COPY "..\LBL\CIERRANO.LBL".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
               COPY "..\LBL\CONTRATO.LBL".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\NOMINOVE.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\CERTIFIC.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\LBL\LISTADOS.LBL".
               COPY "..\LBL\TEMPINFO.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\NOVEFIJA.LBL".
               COPY "..\LBL\CTRLSUEL.LBL".
               COPY "..\LBL\RETENFTE.LBL".
      ***************************************************************
      *                 AREAS Y VARIABLES DE TRABAJO                *
      ***************************************************************
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\RUTMONTO.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\NFACTURA.WRK".
               COPY "..\WRK\PLANILLA.WRK".
               COPY "..\WRK\LISTADOS.WRK".
      ***************************************************************
      *           AREA DE CONTROL DEL LINKAGE-SECTION               *
      ***************************************************************
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
      *                 PROCEDURE DIVISION                          *
      ***************************************************************
       PROCEDURE DIVISION      USING  AREA-LINK-NOMINA.
      ***************************************************************
      *                DEFINICION DE DECLARATIVAS                   *
      ***************************************************************
       DECLARATIVES.
               COPY "..\DCL\CIERRANO.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\CONTRATO.DCL".
               COPY "..\DCL\NOMINOVE.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\REPORTES.DCL".
               COPY "..\DCL\LISTADOS.DCL".
               COPY "..\DCL\TEMPINFO.DCL".
               COPY "..\DCL\CERTIFIC.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\NOVEFIJA.DCL".
               COPY "..\DCL\CTRLSUEL.DCL".
               COPY "..\DCL\RETENFTE.DCL".
       END DECLARATIVES.
      ***************************************************************
      *                 ESTRUCTURA PRINCIPAL                        *
      ***************************************************************
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-REVISA-USUAPERS-ACCE THRU F-REVISA-USUAPERS-ACCE.
           IF WIN-PERIODO-ESTADO = 77
                  PERFORM I-WIN-PERIODO-ESTADO THRU F-WIN-PERIODO-ESTADO
                  EXIT PROGRAM.
           IF WIN-FLAG-CIERRE   NOT  = "I"
              PERFORM I-FLAG-CIERRE     THRU F-FLAG-CIERRE
              EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS     THRU F-LABEL-ARCHIVOS.
           PERFORM I-VENOMAES           THRU F-VENOMAES.
           PERFORM I-BORRA-LIQUINOV     THRU F-BORRA-LIQUINOV.
           PERFORM I-LIMPIE-HORAS       THRU F-LIMPIE-HORAS
           PERFORM I-ABRE-ARCHIVOS      THRU F-ABRE-ARCHIVOS.
           PERFORM I-VALIDA-NOMINOVE    THRU F-VALIDA-NOMINOVE.
           PERFORM I-CEROS-NOMINOVE     THRU F-CEROS-NOMINOVE.
           PERFORM I-PANTALLA           THRU F-PANTALLA.
           PERFORM I-ORDEN-PLANILLA     THRU F-ORDEN-PLANILLA.
      *    PERFORM I-APLICA-ISS-MENSUAL THRU F-APLICA-ISS-MENSUAL
           PERFORM I-APLICA-RETENFTE    THRU F-APLICA-RETENFTE.
           MOVE    0 TO FIN-OK RUT-OK SW-INFORME.
           PERFORM I-START-CONTRATO    THRU F-START-CONTRATO.
           PERFORM I-MUEVE-TEMPINFO    THRU F-MUEVE-TEMPINFO
                   UNTIL FIN-OK = 1.
           CLOSE   TEMPINFO            OPEN I-O TEMPINFO.
           IF RES-OK = "S"
             PERFORM I-LEY100-ANTRTEFTE  THRU F-LEY100-ANTRTEFTE
             PERFORM I-LIMPIE-HORAS      THRU F-LIMPIE-HORAS.
           PERFORM I-ORDEN-INFORME     THRU F-ORDEN-INFORME
           IF  SW-CESAN-IMP  NOT = 1
              PERFORM I-EVALUE-FECHAS     THRU F-EVALUE-FECHAS.

           PERFORM I-CARGUE-BASERFTE   THRU F-CARGUE-BASERFTE.
           PERFORM I-VALIDA-NOMINOVE   THRU F-VALIDA-NOMINOVE.
           MOVE    0 TO FIN-OK RUT-OK FIN-OK.
           DISPLAY SPACES LINE 12 POSITION 3 SIZE 76
           DISPLAY SPACES LINE 14 POSITION 3 SIZE 76
           PERFORM I-PROCESO-NOVEFIJAS THRU F-PROCESO-NOVEFIJAS.
           OPEN    I-O NOMINOVE
           PERFORM I-SIGUE-INFORME     THRU F-SIGUE-INFORME
                   UNTIL FIN-OK = 1.
           PERFORM I-TOTAL-INFORME     THRU F-TOTAL-INFORME.
           IF  SW-INFORME > ZEROS
                PERFORM I-NEGATIVOS THRU F-NEGATIVOS
                PERFORM I-FIN-MODE             THRU F-FIN-MODE.

           PERFORM I-CUENTA-REGISTROS  THRU F-CUENTA-REGISTROS.
           PERFORM I-START-NOMINOVE    THRU F-START-NOMINOVE.
           PERFORM I-LIMPIE-HORAS      THRU F-LIMPIE-HORAS.
           PERFORM I-LIMPIE-GVALORES   THRU F-LIMPIE-GVALORES
           MOVE    0 TO FIN-OK RUT-OK WK-LINEAS-L
                     VR-APLICA F-OTROS-DESCUENTOS F-OTROS-INGRESOS.
           PERFORM I-PROCESO-NOMINOVE      THRU F-PROCESO-NOMINOVE
           MOVE    SPACES                TO REG-INFORMES REG-LISTADOS
           WRITE   REG-INFORMES        FROM TOTAL-FAC AFTER 0

           MOVE    0 TO FIN-OK WK-LINEAS   RUT-OK WK-LINEAS-L
                        VR-APLICA.
           PERFORM I-LIMPIE-HORAS          THRU F-LIMPIE-HORAS.
           PERFORM I-MUEVE-CEROS           THRU F-MUEVE-CEROS.
           MOVE    0 TO FIN-OK WK-LINEAS   RUT-OK WK-LINEAS-L
                        VR-APLICA.
           PERFORM  I-ACTUALIZA-EMPRESAS  THRU F-ACTUALIZA-EMPRESAS.
           PERFORM I-FIN-MODE             THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      **********************************************************
      *                LABEL ARCHIVOS                          *
      **********************************************************
       I-LABEL-ARCHIVOS.
           MOVE    3                    TO  IND-INFORMES.
           MOVE    1                    TO  IND-CERTIFIC.
           MOVE    3                    TO  IND-LISTADOS.
           MOVE    4                    TO  IND-REPORTES.
           PERFORM I-LABEL-CIERRANO    THRU F-LABEL-CIERRANO.
           PERFORM I-STATUS-CIERRANO    THRU F-STATUS-CIERRANO.
           PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
           PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
           PERFORM I-LABEL-CTRLSUEL    THRU F-LABEL-CTRLSUEL.
           PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
           PERFORM I-LABEL-TEMPINFO    THRU F-LABEL-TEMPINFO.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-CERTIFIC    THRU F-LABEL-CERTIFIC.
           PERFORM I-LABEL-REPORTES    THRU F-LABEL-REPORTES.
           PERFORM I-LABEL-LISTADOS    THRU F-LABEL-LISTADOS.
      *JJT MOVE  ZEROS                   TO LAB-INFORMES-PERIODO
      *JJT MOVE  ZEROS                   TO LAB-CERTIFIC-PERIODO
      *JJT MOVE  ZEROS                   TO LAB-LISTADOS-PERIODO
      *JJT MOVE  ZEROS                   TO LAB-REPORTES-PERIODO
           MOVE "NIT/COD."                    TO   TITULO-4-NIT
           MOVE "   APELLIDOS Y NOMBRES    "  TO   TITULO-4-DET.
           MOVE WEMPRESA               TO  I-EMPRESA
           MOVE WIN-PRC-PRESTACIONAL   TO L-WIN-PRC-PRESTACIONAL
           MOVE WIN-PRC-ADMON          TO L-WIN-PRC-ADMON
           MOVE WIN-EMPRESA            TO  I-TITU-EMPRESA
           MOVE ZEROS                  TO  R-NIT-EMPRESA
           MOVE SPACES                 TO  R-EMPRESA
                                           R-DIRECCION-EMP
           ADD  1                      TO  WR-NUMERO-FACTURA.
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P.
       F-LABEL-ARCHIVOS.
      **********************************************************
      *                START NOMINOVE                          *
      **********************************************************
       I-START-NOMINOVE.
           MOVE ZEROS         TO  NOV-CLAVE2.
           START NOMINOVE KEY >  NOV-CLAVE2 INVALID  KEY
               MOVE "98"      TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-NOMINOVE.

       I-START-CONTRATO.
           MOVE ZEROS         TO  CONTRATO-CLAVE.
           START CONTRATO KEY >  CONTRATO-CLAVE  INVALID  KEY
               MOVE "98"      TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CONTRATO.

      **********************************************************
      *                  ABRE ARCHIVOS                         *
      **********************************************************
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CONCPTOS  TABLAEMP CNCATCTA
                           HOJAVIDA  NOMICIUD RETENFTE
               OPEN  I-O   CONTRATO  EMPRESAS
                           CIERRANO
               OPEN OUTPUT INFORMES LISTADOS CERTIFIC
                           REPORTES TEMPINFO.
       F-ABRE-ARCHIVOS.
      **********************************************************
      *                  DISPLAY PANTALLA                      *
      **********************************************************
       I-PANTALLA.
           PERFORM I-DISPLAY-HEAD THRU F-DISPLAY-HEAD
           PERFORM I-DISPLAY-BODY THRU F-DISPLAY-BODY.
           PERFORM I-DECODI-TIPOPERI THRU F-DECODI-TIPOPERI.
           DISPLAY "PROCESO PRENOMINA-->"
                   LINE  3 POSITION 28
           PERFORM I-ORIENTACION-VENT  THRU F-ORIENTACION-VENT.
           DISPLAY "บ Orden Planilla:"    LINE  8 POSITION  1
           DISPLAY "A,C,N,KA,KC,KN"       LINE  8 POSITION 21
           DISPLAY "SERIE No."            LINE  8 POSITION 36
           DISPLAY  CON-PERIODO           LINE  8 POSITION 46
           DISPLAY " Refte(S/N):"         LINE  8 POSITION 54.
       020-DISPLAY-DATOS-PERIODO.
           DISPLAY WK-ESPACIOS       LINE 24 POSITION  1.
       CONFIRMA-INFORME.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                   LINE 24 POSITION 1
           DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32

               PERFORM ACEPTA-TRUCO
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
      *     IF  F9
      *         PERFORM I-CAMBIO-CAMPOS-309  THRU F-CAMBIO-CAMPOS-309
      *          MOVE    1   TO   SW1-300
      *         MOVE "C"  TO W-TRUCO.
           IF  F8
               PERFORM I-CAMBIO-CAMPOS-309  THRU F-CAMBIO-CAMPOS-309
               MOVE "C"  TO W-TRUCO.
           IF  W-TRUCO    NOT =  "C"
               GO TO CONFIRMA-INFORME.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
           MOVE ZEROS TO WK-PAGINA.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE.
       F-PANTALLA.
      **********************************************************
      *                  ORDEN DE CLASIFICACION PLANILLA       *
      **********************************************************
       I-ORDEN-PLANILLA.
               MOVE ZEROS                  TO  WK-EXCEPTION
               DISPLAY WIN-ORDEN           LINE 08 POSITION 18
               ACCEPT WIN-ORDEN            LINE 08 POSITION 18
                      UPDATE PROMPT
                      NO BEEP REVERSE
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.

           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF WIN-ORDEN = "A " OR
              WIN-ORDEN = "C " OR
              WIN-ORDEN = "N " OR
              WIN-ORDEN = "KA" OR
              WIN-ORDEN = "CA" OR
              WIN-ORDEN = "KC" OR
              WIN-ORDEN = "CC" OR
              WIN-ORDEN = "KN" OR
              WIN-ORDEN = "CN"
             NEXT SENTENCE
           ELSE
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             "FAVOR ELIJA UN ORDEN DE PLANILLA QUE EXISTA "
             LINE 25 POSITION 01 BEEP  BLINK
             PERFORM ACEPTA-TRUCO
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             GO I-ORDEN-PLANILLA.

           DISPLAY WIN-ORDEN LINE 08 POSITION 18 .
       F-ORDEN-PLANILLA.

       I-APLICA-RETENFTE.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT RES-OK               LINE 08 POSITION 67
                      UPDATE PROMPT
                      NO BEEP REVERSE
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY RES-OK LINE 08 POSITION 67 .
           IF RES-OK = "N" GO F-APLICA-RETENFTE.
           IF RES-OK = "S"
              PERFORM I-START-RETENFTE   THRU F-START-RETENFTE
              MOVE  0 TO SW-3 I
              PERFORM I-CARGA-RETENFTE   THRU F-CARGA-RETENFTE
                   UNTIL SW-3 = 1.
       F-APLICA-RETENFTE.

       I-APLICA-ISS-MENSUAL.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE "N" TO RES-ISSOK.
               ACCEPT RES-ISSOK               LINE 19 POSITION 77
                      UPDATE PROMPT
                      NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY RES-ISSOK LINE 19 POSITION 77 .
       F-APLICA-ISS-MENSUAL.
      **********************************************************
      *                PREPARA TEMPINFO                        *
      **********************************************************
       I-PREPARA-TEMPINFO.
      *    OPEN OUTPUT TEMPINFO.
      *    MOVE ZEROS         TO  CONTRATO-CLAVE
      *    START CONTRATO KEY >  CONTRATO-CLAVE INVALID  KEY
      *        MOVE "98"      TO  STATUS-NOMINA
      *        PERFORM RUTINA-ERROR-NOMINA
      *        PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-PREPARA-TEMPINFO.
      **********************************************************
      *                ACTUALIZA-MUEVE A TEMPINFO              *
      **********************************************************
       I-MUEVE-TEMPINFO.
           PERFORM I-LEE-CONTRATO THRU F-LEE-CONTRATO.

           IF CONTRATO-SW3  >  6 OR FIN-OK = 1
              GO F-MUEVE-TEMPINFO.

           MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA

           IF SW-99 = 1  GO I-MUEVE-TEMPINFO.
           PERFORM I-GRABA-TIMAE  THRU F-GRABA-TIMAE.
       F-MUEVE-TEMPINFO.
      *
       I-GRABA-TIMAE.
           MOVE CONTRATO-SALARIO TO TIMAE-SALARIO.
           MOVE CONTRATO-CEDULA  TO TIMAE-CEDULA  TIMAE-CEDULA4
           MOVE CONTRATO-CODIEMP TO TIMAE-CODIEMP TIMAE-CODIEMP5
           MOVE HOJAVIDA-NOMBRE  TO TIMAE-NOMBRE  TIMAE-NOMBRE2
                                               TIMAE-NOMBRE3
           MOVE CONTRATO-CODICEN TO TIMAE-CODICEN TIMAE-CODICEN4
                                               TIMAE-CODICEN5

           MOVE CONTRATO-AFILIA-PENSION      TO TIMAE-AFILIA-PENSION
           MOVE CONTRATO-AFILIA-SALUD        TO TIMAE-AFILIA-SALUD
      *     MOVE HOJAVIDA-FONDO-PENSIONES     TO TIMAE-FONDO-PENSIONES
      *     MOVE HOJAVIDA-PROMOTORA-SALUD     TO TIMAE-PROMOTORA-SALUD
           IF CONTRATO-RETEN  > 0
              MOVE "D" TO TIMAE-RETEN
           ELSE
              MOVE SPACES TO TIMAE-RETEN.
           WRITE REC-TEMPINFO.
           MOVE 0  TO CONTRATO-DEDUC1 CONTRATO-ISS
            REWRITE REC-CONTRATO.
           DISPLAY
           "ORDENANDO LA INFORMACION....ESPERE UN MOMENTO POR FAVOR."
                  LINE 24 POSITION 1.
           PERFORM I-DISPLAY-TEMPINFO THRU F-DISPLAY-TEMPINFO.
       F-GRABA-TIMAE.

       I-DISPLAY-TEMPINFO.
           MOVE    TIMAE-CEDULA                 TO   WI-Z9
           DISPLAY WI-Z9                        LINE 12 POSITION 02
           DISPLAY TIMAE-NOMBRE                 LINE 12 POSITION 25.
           DISPLAY "CENTRO DE COSTO"            LINE 14 POSITION  3
           DISPLAY  TIMAE-CODICEN               LINE 14 POSITION 23.
           DISPLAY "BASE-RFTE:"                 LINE 14 POSITION 50
           MOVE CONTRATO-DEDUC1  TO WI-Z9P
           DISPLAY WI-Z9P                       LINE 14 POSITION 62.
       F-DISPLAY-TEMPINFO.
      ***************************************************************
      *                    ORDEN-INFORME                            *
      ***************************************************************
       I-ORDEN-INFORME.
           MOVE ZEROS  TO TIMAE-CEDULA  TIMAE-CEDULA4
           MOVE SPACES TO TIMAE-CODIEMP TIMAE-CODIEMP5
           MOVE SPACES TO TIMAE-NOMBRE  TIMAE-NOMBRE2  TIMAE-NOMBRE3
           MOVE ZEROS  TO TIMAE-CODICEN TIMAE-CODICEN4 TIMAE-CODICEN5
           PERFORM I-ORDEN-ALFABETICO THRU F-ORDEN-ALFABETICO
           PERFORM I-ORDEN-CODIGO     THRU F-ORDEN-CODIGO.
           PERFORM I-ORDEN-NIT        THRU F-ORDEN-NIT.
           PERFORM I-CC-ALFABETICO    THRU F-CC-ALFABETICO.
           PERFORM I-CC-NIT           THRU F-CC-NIT.
           PERFORM I-CC-CODIGO        THRU F-CC-CODIGO.
       F-ORDEN-INFORME.

       I-ORDEN-ALFABETICO.
           IF WIN-ORDEN                    =  "A "
              DISPLAY "ALFABETICO    " LINE 08 POSITION 21
              START TEMPINFO KEY >  TIMAE-CLAVE2 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-ALFABETICO.

       I-ORDEN-CODIGO.
           IF WIN-ORDEN                    =  "C "
              DISPLAY "CODIGO        "  LINE 08 POSITION 21
              MOVE "CODIGO"               TO  TITULO-4-NIT
              START TEMPINFO KEY > TIMAE-CLAVE1 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-CODIGO.

       I-ORDEN-NIT.
           IF WIN-ORDEN                    =  "N "
              DISPLAY "CEDULAS       "  LINE 08 POSITION 21
             START TEMPINFO KEY > TIMAE-CLAVE INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-NIT.

       I-CC-ALFABETICO.
           IF WIN-ORDEN                    =  "CA"
              OR  WIN-ORDEN                =  "KA"
              DISPLAY "C.C.- ALFABET " LINE 08 POSITION 21
               START TEMPINFO KEY >  TIMAE-CLAVE3 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-ALFABETICO.

       I-CC-NIT.
           IF  WIN-ORDEN                    =  "CN"
           OR  WIN-ORDEN                    =  "KN"
             DISPLAY "C.C.- CEDULAS  "  LINE 08 POSITION 21
               START TEMPINFO KEY > TIMAE-CLAVE4 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-NIT.

       I-CC-CODIGO.
           IF  WIN-ORDEN                    =  "CC"
           OR  WIN-ORDEN                    =  "KC"
             DISPLAY "C.C.- CODIGOS  "  LINE 08 POSITION 21
               START TEMPINFO KEY > TIMAE-CLAVE5 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-CODIGO.
      ***************************************************************
      *                    EVALUE FECHAS                            *
      ***************************************************************
       I-EVALUE-FECHAS.
           PERFORM I-MUEVE-CEROS       THRU F-MUEVE-CEROS.
           MOVE WK-MASCARA-FECHA       TO  I-FECHA
           MOVE WIN-CODIGO-EMPRESA     TO  I-CODIGO-EMPRESA
           MOVE CON-PERIODO            TO  I-PERIODO
           MOVE W-TPN-FECHA-DESDE      TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01 OR
               WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           MOVE WK-MASCARA-FECHA       TO  I-FECHA-DESDE C-FECHA-DESDE
           MOVE WK-MASCARA-HORA        TO  I-HORA
           MOVE W-TPN-FECHA-HASTA      TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01 OR
               WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           MOVE WK-MASCARA-FECHA       TO  I-FECHA-HASTA C-FECHA-HASTA
           MOVE W-TPN-FECHA-PAGO       TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01 OR
               WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           MOVE WK-MASCARA-FECHA       TO  I-FECHA-PAGO
           MOVE 88                     TO  WK-LINEAS
           DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           MOVE ZEROS                  TO  W-ULTIMO-CENTRO.
       F-EVALUE-FECHAS.

      ***************************************************************
      *                    I-SIGUE-INFORME                          *
      ***************************************************************
       I-SIGUE-INFORME.
           IF  SW-CESAN-IMP  NOT = 1
               MOVE "PRENOMINA DE "
                TO I-TITU-PLANILLA.
           PERFORM I-LIMPIE-W           THRU F-LIMPIE-W.
           PERFORM I-LEE-TEMPINFO       THRU F-LEE-TEMPINFO.
           IF FIN-OK = 1
                  PERFORM TOTALES-POR-CENTRO THRU
                          SALE-TOTALES-POR-CENTRO
                          GO   F-SIGUE-INFORME.
           IF  WIN-ORDEN  =  "KA"
               PERFORM I-ROMPE-CENTRO-KA THRU F-ROMPE-CENTRO-KA.
           IF  WIN-ORDEN  =  "KN"
               PERFORM I-ROMPE-CENTRO-KN THRU F-ROMPE-CENTRO-KN.
           IF  WIN-ORDEN  =  "KC"
               PERFORM I-ROMPE-CENTRO-KC THRU F-ROMPE-CENTRO-KC.
           IF RES-OK = "S"
              PERFORM I-ANALIZE-RETENFTE   THRU F-ANALIZE-RETENFTE.
           PERFORM I-ANALIZE-NOVEDADES  THRU F-ANALIZE-NOVEDADES.
           PERFORM I-ANALIZE-LEY100     THRU F-ANALIZE-LEY100.
           PERFORM I-ANALIZE-SINDICATO  THRU F-ANALIZE-SINDICATO.
           PERFORM I-ANALIZE-WDCTOS-POSITIV
              THRU F-ANALIZE-WDCTOS-POSITIV
           COMPUTE W-NETO-A-PAGAR       =    W-VALOR-DEVENGADOS
                                        +    W-VALOR-DESCUENTOS
           IF W-NETO-A-PAGAR IS NEGATIVE
              MOVE 1 TO SW-INFORME.
           MOVE    W-NETO-A-PAGAR       TO   TIMAE-NETO
           REWRITE REC-TEMPINFO.
           ADD     W-NETO-A-PAGAR       TO   S-NETO-A-PAGAR
           PERFORM I-ARMA-LINEA-DETALLE THRU F-ARMA-LINEA-DETALLE
           PERFORM I-DISPLAY-DATOS      THRU F-DISPLAY-DATOS.
           PERFORM I-SUMA-TOTALES       THRU F-SUMA-TOTALES.
       F-SIGUE-INFORME.

      ***************************************************************
      *                    I-DISPLAY-DATOS                          *
      ***************************************************************
       I-DISPLAY-DATOS.
           MOVE TIMAE-CEDULA           TO  WI-Z12
           DISPLAY WI-Z12              LINE 12 POSITION 02
           DISPLAY TIMAE-NOMBRE        LINE 12 POSITION 25
           MOVE W-VALOR-DEVENGADOS     TO  WI-Z8P
           DISPLAY WI-Z8P              LINE 05 POSITION 67
           MOVE W-VALOR-DESCUENTOS     TO  WI-Z8P
           DISPLAY WI-Z8P              LINE 06 POSITION 67
           MOVE W-NETO-A-PAGAR         TO  WI-Z8P
           DISPLAY WI-Z8P              LINE 07 POSITION 67.
       F-DISPLAY-DATOS.

       I-ANALIZE-NOVEDADES.
           MOVE 0 TO RUT-OK.
           PERFORM I-START-KEY00      THRU F-START-KEY00.
           PERFORM I-EXTRAE-NOVEDADES THRU F-EXTRAE-NOVEDADES
                   UNTIL RUT-OK = 1.
           MOVE 0 TO RUT-OK.
       F-ANALIZE-NOVEDADES.
      ***************************************************************
      *                  START NOMINOVE                             *
      ***************************************************************
       I-START-KEY00.
           MOVE WIN-CODIGO-EMPRESA     TO  NOV-EMPRESA
           MOVE TIMAE-CEDULA           TO  NOV-CODIEMP
           MOVE ZEROS                  TO  NOV-CONCEPTO
           MOVE ZEROS                  TO  I-ERROR
           START NOMINOVE KEY  >  NOV-CLAVE  INVALID  KEY
                 MOVE 1   TO  I-ERROR.
       F-START-KEY00.
      ****************************************************************
      *                 EXTRAE NOVEDADES DE CADA EMPLEADO            *
      ****************************************************************
       I-EXTRAE-NOVEDADES.
            IF I-ERROR             = 1 OR
                  NOMINOVE-STATUS  =  "23"
      *           PERFORM I-EMPL-SIN-NOV THRU F-EMPL-SIN-NOV
                  MOVE 1 TO RUT-OK GO F-EXTRAE-NOVEDADES.
           PERFORM I-LEE-NOMINOVE THRU F-LEE-NOMINOVE
           IF RUT-OK = 1 GO F-EXTRAE-NOVEDADES.
           IF  NOV-EMPRESA >  WIN-CODIGO-EMPRESA OR
               NOV-CODIEMP >  TIMAE-CEDULA
               MOVE 1 TO RUT-OK GO F-EXTRAE-NOVEDADES.
           PERFORM I-CEROS-BASES     THRU F-CEROS-BASES.
      *    PERFORM I-ACTUALIZA-FONDO THRU F-ACTUALIZA-FONDO.
           PERFORM I-SUMA-NOVEDADES  THRU F-SUMA-NOVEDADES.
       F-EXTRAE-NOVEDADES.
      ***************************************************************
      *                    I-ROMPE-CENTRO                           *
      ***************************************************************
       I-ROMPE-CENTRO-KA.
           IF  W-ULTIMO-CENTRO              =  ZEROS
              MOVE TIMAE-CODICEN          TO  W-ULTIMO-CENTRO
           ELSE
               IF W-ULTIMO-CENTRO          =  TIMAE-CODICEN
                  NEXT SENTENCE
               ELSE
                  PERFORM TOTALES-POR-CENTRO THRU
                          SALE-TOTALES-POR-CENTRO
                  MOVE TIMAE-CODICEN TO W-ULTIMO-CENTRO.
       F-ROMPE-CENTRO-KA.
      *
       I-ROMPE-CENTRO-KN.
           IF  W-ULTIMO-CENTRO              =  ZEROS
               MOVE TIMAE-CODICEN4         TO  W-ULTIMO-CENTRO
           ELSE
               IF W-ULTIMO-CENTRO          =  TIMAE-CODICEN4
                  NEXT SENTENCE
               ELSE
                  PERFORM TOTALES-POR-CENTRO THRU
                          SALE-TOTALES-POR-CENTRO
                  MOVE TIMAE-CODICEN4 TO W-ULTIMO-CENTRO.
       F-ROMPE-CENTRO-KN.

       I-ROMPE-CENTRO-KC.
           IF  W-ULTIMO-CENTRO              =  ZEROS
               MOVE TIMAE-CODICEN5         TO  W-ULTIMO-CENTRO
           ELSE
               IF W-ULTIMO-CENTRO          =  TIMAE-CODICEN5
                  NEXT SENTENCE
               ELSE
                  PERFORM TOTALES-POR-CENTRO THRU
                          SALE-TOTALES-POR-CENTRO
                  MOVE TIMAE-CODICEN5 TO W-ULTIMO-CENTRO.
       F-ROMPE-CENTRO-KC.
      ***************************************************************
      *                    I-TOTALES-POR-CENTRO                     *
      ***************************************************************
       TOTALES-POR-CENTRO.
           MOVE 021                  TO  TE021-CODIGO-TABLA
           MOVE WIN-CODIGO-EMPRESA   TO  TE021-CODIGO-EMPRESA
           MOVE ZEROS                TO  TE021-CODIGO-FILLER
           MOVE W-ULTIMO-CENTRO      TO  TE021-CODIGO-CENTRO-COSTO
           READ TABLAEMP          WITH NO LOCK INVALID  KEY
               MOVE "** ERRADO"          TO TE021-NOMBRE-CENTRO-COSTO.
           PERFORM I-BORRA-LINEA-DETALLE THRU F-BORRA-LINEA-DETALLE
           PERFORM I-TOTAL-CENTRO THRU F-TOTAL-CENTRO
           WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   AFTER 1
           WRITE REG-INFORMES        FROM  LINEA-TOTALES-A AFTER 1
           WRITE REG-INFORMES        FROM  LINEA-TOTALES-B AFTER 1
           WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   AFTER 1

           PERFORM I-SUBTOTAL-S-PRC-PREST THRU F-SUBTOTAL-S-PRC-PREST
           PERFORM I-LIMPIE-HORAS  THRU F-LIMPIE-HORAS.

           WRITE REG-LISTADOS        FROM  LINEA-TOTAL-0   AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTALES-A AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTALES-B AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTAL-0   AFTER 1
           ADD  4                      TO  WK-LINEAS.
           PERFORM I-BORRA-LINEA-TOTALES THRU F-BORRA-LINEA-TOTALES.
       SALE-TOTALES-POR-CENTRO.
      ***************************************************************
      *                I-PROCESO-NOMINOVE                           *
      ***************************************************************
       I-PROCESO-NOMINOVE.
           MOVE "   *CUADRO DE RESUMEN GENERAL*    "
             TO I-TITU-PLANILLA.
           PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
           PERFORM I-PRIMER-REGISTRO THRU F-PRIMER-REGISTRO.
      *    MOVE 0 TO RUT-OK.
           PERFORM I-PROCESS-NOMINOVE THRU F-PROCESS-NOMINOVE
                       UNTIL RUT-OK = 1
           PERFORM I-ULTIMO-REGISTRO THRU F-ULTIMO-REGISTRO.
           PERFORM I-TOTAL-GEN THRU F-TOTAL-GEN.
       F-PROCESO-NOMINOVE.
      *
       I-PRIMER-REGISTRO.
           PERFORM I-LEE-NOMINOVE THRU F-LEE-NOMINOVE.
           MOVE    NOV-EMPRESA2   TO   WNOV-EMPRESA2.
           MOVE    NOV-CONCEPTO2  TO   WNOV-CONCEPTO2.
           PERFORM I-MUEVE        THRU F-MUEVE.
           PERFORM I-SUME-NOMINOVE THRU F-SUME-NOMINOVE
           IF RUT-OK NOT = 1
           PERFORM I-LEE-NOMINOVE THRU F-LEE-NOMINOVE.
       F-PRIMER-REGISTRO.
      *
       I-PROCESS-NOMINOVE.
           IF RUT-OK = 1 GO TO F-PROCESS-NOMINOVE.
           IF NOV-EMPRESA2 = WNOV-EMPRESA2
              IF NOV-CONCEPTO2 = WNOV-CONCEPTO2
                 PERFORM I-SUME-NOMINOVE THRU F-SUME-NOMINOVE
                 PERFORM I-LEE-NOMINOVE  THRU F-LEE-NOMINOVE
                 GO F-PROCESS-NOMINOVE
              ELSE 
                 PERFORM COD-NUEVO THRU F-COD-NUEVO.
      *          GO F-PROCESS-NOMINOVE.
       F-PROCESS-NOMINOVE.
      *
       COD-NUEVO.
           PERFORM I-TOTAL-CONCEPTO THRU F-TOTAL-CONCEPTO
           MOVE    NOV-CONCEPTO2    TO   WNOV-CONCEPTO2.
           PERFORM I-MUEVE          THRU F-MUEVE.
           PERFORM I-SUME-NOMINOVE  THRU F-SUME-NOMINOVE
           PERFORM I-LEE-NOMINOVE   THRU F-LEE-NOMINOVE.
       F-COD-NUEVO.
      *
       I-MUEVE.
           PERFORM I-LEE-CONCPTOS       THRU F-LEE-CONCPTOS.
           MOVE    NOV-CONCEPTO         TO   I-NOV-CONCEPTO
           PERFORM I-MUEVE-CONCPTOS        THRU F-MUEVE-CONCPTOS.
           MOVE    W-CONCPTOS-DESCRIPCION  TO   I-NOV-DESCRIPCION.
       F-MUEVE.

       I-MUEVE-CONCPTOS.
           MOVE REC-CONCPTOS TO W-REC-CONCPTOS.
       F-MUEVE-CONCPTOS.
      *
       I-SUME-NOMINOVE.
           PERFORM I-SUMATORIA-BAS-DCTOS THRU F-SUMATORIA-BAS-DCTOS.
       F-SUME-NOMINOVE.
      *
       I-ULTIMO-REGISTRO.
           PERFORM I-TOTAL-CONCEPTO THRU F-TOTAL-CONCEPTO.
       F-ULTIMO-REGISTRO.
      *Imprime los titulos del cuatro de resumen.

       I-TITULOS-TOTALES.
           ADD  1         TO WK-PAGINA.
           MOVE WK-PAGINA TO I-PAGINA.

           IF   WK-LINEAS = 0  OR WK-LINEAS > 57
               MOVE  SPACE                 TO  REG-LISTADOS
               MOVE  SPACE                 TO  REG-INFORMES
               WRITE REG-INFORMES        AFTER PAGE
               MOVE  LABEL-INFORMES        TO  I-LISTADO
               WRITE REG-INFORMES        FROM  TITULO-0     AFTER 3
      *        
               WRITE REG-INFORMES        FROM  TITULO-1     AFTER 1
               WRITE REG-INFORMES        FROM  TITULO-2A    AFTER 1
               WRITE REG-INFORMES        FROM  TITULO-2     AFTER 1
               WRITE REG-INFORMES        FROM  TITULO-2B    AFTER 1
      *
               WRITE REG-LISTADOS        AFTER PAGE
               MOVE  LABEL-LISTADOS        TO  I-LISTADO
               WRITE REG-LISTADOS        FROM  TITULO-0     AFTER 3
      *        
               WRITE REG-LISTADOS        FROM  TITULO-1     AFTER 1
               WRITE REG-LISTADOS        FROM  TITULO-2A    AFTER 1
               WRITE REG-LISTADOS        FROM  TITULO-2     AFTER 1
               WRITE REG-LISTADOS        FROM  TITULO-2B    AFTER 1
           ELSE
               WRITE REG-INFORMES        FROM  TITULO-11    AFTER 2
               WRITE REG-LISTADOS        FROM  TITULO-11    AFTER 2.
      

           MOVE SPACES               TO  REG-INFORMES
           WRITE REG-INFORMES                              AFTER 1
           WRITE REG-INFORMES        FROM  TITULO-TOTAL-0  AFTER 1
           WRITE REG-INFORMES        FROM  TITULO-TOTAL-1  AFTER 1
           WRITE REG-INFORMES        FROM  TITULO-TOTAL-2  AFTER 1
           WRITE REG-INFORMES        FROM  TITULO-TOTAL-0  AFTER 1

           MOVE SPACES               TO  REG-LISTADOS
           WRITE REG-LISTADOS                              AFTER 1
           WRITE REG-LISTADOS        FROM  TITULO-TOTAL-0  AFTER 1
           WRITE REG-LISTADOS        FROM  TITULO-TOTAL-1  AFTER 1
           WRITE REG-LISTADOS        FROM  TITULO-TOTAL-2  AFTER 1
           WRITE REG-LISTADOS        FROM  TITULO-TOTAL-0  AFTER 1
           MOVE 12                     TO  WK-LINEAS.
       F-TITULOS-TOTALES.
      *
       I-TOTAL-CONCEPTO.
      *JJT    IF W-CONCPTOS-LISTADOS-INF = "S"
              PERFORM I-TOTAL-ANEXO  THRU F-TOTAL-ANEXO.

           IF W-NOV-VLR-DESCTOS IS POSITIVE
              COMPUTE W-NOV-VLR-DESCTOS = W-NOV-VLR-DESCTOS
                                        * (-1).

           IF W-DCTOS-OTROS-SIN-PF   IS POSITIVE
               COMPUTE W-DCTOS-OTROS-SIN-PF   = W-DCTOS-OTROS-SIN-PF
                                            * (-1).
           PERFORM I-PARAFISCALES       THRU F-PARAFISCALES.
           PERFORM I-DISPLAY-NOMINOVE   THRU F-DISPLAY-NOMINOVE.
           MOVE W-NOV-DIAS              TO  I-NOV-DIAS
           MOVE W-NOV-HORAS             TO  I-NOV-HORAS
           MOVE W-NOV-VLR-DEVENG        TO  I-NOV-VLR-DEVENG

           IF W-NOV-VLR-DESCTOS > 0 OR
              W-NOV-VLR-DESCTOS < 0
              MOVE W-NOV-VLR-DESCTOS    TO  I-NOV-VLR-DEVENG.

           MOVE W-DCTOS-OTROS-SIN-PF    TO  I-NOV-VLR-DESCTOS
           MOVE W-TOTAL-PARAFISCALES    TO  I-WTOTAL-PARAFISCALES
           MOVE S-TOTAL-PARAFISCALES    TO  I-STOTAL-PARAFISCALES.
           MOVE W-TOTAL-ADMINISTRACION  TO  I-WTOTAL-ADMINISTRACION.
           MOVE S-TOTAL-ADMINISTRACION  TO  I-STOTAL-ADMINISTRACION.
           PERFORM I-DETAIL1      THRU F-DETAIL1.
           PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-TOTAL-CONCEPTO.

       I-CEROS-VARIABLES.
           MOVE 0 TO W-NOV-DIAS              W-NOV-HORAS
                     W-NOV-VLR-DEVENG        W-NOV-VLR-DESCTOS
                     W-TOTAL-PARAFISCALES    S-TOTAL-PARAFISCALES
                     W-TOTAL-ADMINISTRACION  S-TOTAL-ADMINISTRACION
                     I-NOV-VLR-DEVENG        I-NOV-VLR-DESCTOS
                     I-WTOTAL-PARAFISCALES   I-STOTAL-PARAFISCALES
                     I-WTOTAL-ADMINISTRACION I-STOTAL-ADMINISTRACION
                     I-NOV-DIAS I-NOV-HORAS  I-WIN-PRC-PREST
                     I-WIN-PRC-ADMON         W-DCTOS-OTROS-SIN-PF.
       F-CEROS-VARIABLES.
      ***************************************************************
      *                    I-DISPLAY-NOMINOVE                       *
      ***************************************************************
       I-DISPLAY-NOMINOVE.
           DISPLAY W-CONCPTOS-CODIGO-CONCEPTO   LINE 13 POSITION 02
           DISPLAY W-CONCPTOS-DESCRIPCION       LINE 13 POSITION 09
           DISPLAY "PARAFISCALES:"              LINE 15 POSITION 49
           MOVE    W-TOTAL-PARAFISCALES         TO  WI-VALOR
           DISPLAY WI-VALOR                     LINE 15 POSITION 63
           DISPLAY "SUBTOTAL    :"              LINE 16 POSITION 49
           MOVE    S-TOTAL-PARAFISCALES         TO WI-VALOR
           DISPLAY WI-VALOR                     LINE 16 POSITION 63
           DISPLAY "ADMNISTRCION:"              LINE 17 POSITION 49
           MOVE    W-TOTAL-ADMINISTRACION       TO WI-VALOR
           DISPLAY WI-VALOR                     LINE 17 POSITION 63.
           DISPLAY "TOTAL       :"              LINE 18 POSITION 49
           MOVE    S-TOTAL-ADMINISTRACION       TO WI-VALOR
           DISPLAY WI-VALOR                     LINE 18 POSITION 63.
       F-DISPLAY-NOMINOVE.

       I-DETAIL1.
           IF   WK-LINEAS = 0  OR WK-LINEAS > 57
                PERFORM I-TITULOS-TOTALES THRU F-TITULOS-TOTALES.
           WRITE REG-INFORMES FROM LINEA-DETALLE-TOTAL AFTER 1.
           WRITE REG-LISTADOS FROM LINEA-DETALLE-TOTAL AFTER 1.
           ADD   1            TO  WK-LINEAS.
       F-DETAIL1.

       I-TOTAL-GEN.
            PERFORM I-DISPLAY-NOMINOVE   THRU F-DISPLAY-NOMINOVE.
           IF G-NOV-DESCTOS-PF  IS POSITIVE
              COMPUTE G-NOV-DESCTOS-PF    =    G-NOV-DESCTOS-PF
                                          *    (-1).
            ADD  G-NOV-DESCTOS-PF         TO   G-VALOR-DEVENGADOS
            MOVE G-VALOR-DEVENGADOS       TO   T-NOV-VLR-DEVENG
                                               I-VALOR-BASE

           IF G-VALOR-DESCUENTOS IS POSITIVE
              COMPUTE G-VALOR-DESCUENTOS  =    G-VALOR-DESCUENTOS
                                          *    (-1).
            MOVE G-VALOR-DESCUENTOS       TO   T-NOV-VLR-DESCTOS
            COMPUTE G-NETO-A-PAGAR        =    G-VALOR-DEVENGADOS
                                          +    G-VALOR-DESCUENTOS
            MOVE G-NETO-A-PAGAR           TO   R-NETO-A-PAGAR
            MOVE G-WTOTAL-PARAFISCALES    TO  T-WTOTAL-PARAFISCALES
            MOVE G-STOTAL-PARAFISCALES    TO  T-STOTAL-PARAFISCALES.
            MOVE G-WTOTAL-ADMINISTRACION  TO  T-WTOTAL-ADMINISTRACION.
            MOVE G-STOTAL-ADMINISTRACION  TO  T-STOTAL-ADMINISTRACION.
            MOVE G-STOTAL-ADMINISTRACION  TO  R-NETO-A-FACTURAR
            PERFORM I-DETAIL2    THRU F-DETAIL2.
       F-TOTAL-GEN.
      *
       I-DETAIL2.
           IF   WK-LINEAS = 0  OR WK-LINEAS > 57
                PERFORM I-TITULOS-TOTALES THRU F-TITULOS-TOTALES.
           WRITE REG-INFORMES FROM TITULO-TOTAL-0 AFTER 1.
           WRITE REG-INFORMES FROM TOTAL-TOTAL-1  AFTER 1.
           WRITE REG-INFORMES FROM TITULO-TOTAL-0 AFTER 1.
           WRITE REG-INFORMES FROM TOTAL-TOTAL-2  AFTER 1.
           WRITE REG-INFORMES FROM TOTAL-TOTAL-4  AFTER 1.
           WRITE REG-INFORMES FROM TOTAL-TOTAL-3  AFTER 1.
           WRITE REG-INFORMES FROM TOTAL-TOTAL-4  AFTER 1.

           WRITE REG-LISTADOS FROM TITULO-TOTAL-0 AFTER 1.
           WRITE REG-LISTADOS FROM TOTAL-TOTAL-1  AFTER 1.
           WRITE REG-LISTADOS FROM TITULO-TOTAL-0 AFTER 1.
           WRITE REG-LISTADOS FROM TOTAL-TOTAL-2  AFTER 1.
           WRITE REG-LISTADOS FROM TOTAL-TOTAL-4  AFTER 1.
           WRITE REG-LISTADOS FROM TOTAL-TOTAL-3  AFTER 1.
           WRITE REG-LISTADOS FROM TOTAL-TOTAL-4  AFTER 1.
           MOVE  0            TO  WK-LINEAS.
       F-DETAIL2.
      *
       I-INTER-CESANT.
            DISPLAY " " LINE 24 POSITION 1.
       F-INTER-CESANT.
      *
       I-WIN-PERIODO-ESTADO.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP
            DISPLAY "บ                 ATENCION!                      บ"
            LINE 05 POSITION 17 BEEP
            DISPLAY "บ  ** YA FUE PROCESADA LA NOMINA DEFINITIVA **   บ"
            LINE 06 POSITION 17 BEEP .
            DISPLAY "บ     EL PASO EN LA SERIE A SEGUIR SON LOS       บ"
            LINE 07 POSITION 17 BEEP .
            DISPLAY "บ PROCESOS DE PAGO (Consignacion-Cheque-Efectivo)บ"
            LINE 08 POSITION 17 BEEP .
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP
            "      <ENTER>    CONTINUE   "
            LINE 10 POSITION 27 BEEP  BLINK.
            PERFORM ACEPTA-TRUCO.
       F-WIN-PERIODO-ESTADO.   EXIT.
      *
       I-ACTUALIZA-EMPRESAS.
           MOVE CON-COD-EMPRESA TO WIN-CODIGO-EMPRESA.
           READ EMPRESAS WITH NO LOCK INVALID KEY
            GO TO F-ACTUALIZA-EMPRESAS.
           MOVE 76 TO WIN-PERIODO-ESTADO.
           REWRITE REC-EMPRESAS.
       F-ACTUALIZA-EMPRESAS. EXIT.
      *
       I-FIN-MODE.
           CLOSE CONCPTOS TABLAEMP CONTRATO HOJAVIDA CIERRANO
                 NOMINOVE EMPRESAS NOMICIUD CNCATCTA CERTIFIC
                 TEMPINFO RETENFTE INFORMES REPORTES LISTADOS.
           OPEN OUTPUT LISTADOS.
           CLOSE LISTADOS.

           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-FLAG-CIERRE.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP
            DISPLAY "บ                E R R O R !                     บ"
            LINE 05 POSITION 17 BEEP
            DISPLAY "บ       NO HA TERMINADO LA SERIE ANTERIOR        บ"
            LINE 06 POSITION 17 BEEP .
            DISPLAY "บ            VERIFIQUE EL PASO A SEGUIR          บ"
            LINE 07 POSITION 17 BEEP .
            DISPLAY "บ                  QUE LE HACE FALTA             บ"
            LINE 08 POSITION 17 BEEP .
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP
            "   <ENTER>   CONTINUE       "
            LINE 10 POSITION 27 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-FLAG-CIERRE.   EXIT.
      *
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\TIPOPERI.PRO".
           COPY "..\PRO\DISCAPDA.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\MONTOESC.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\NSUMANOV.PRO".
           COPY "..\PRO\LEEARCHI.PRO".
           COPY "..\PRO\ANEXOSNO.PRO".
           COPY "..\PRO\NOLEY100.PRO".
           COPY "..\PRO\DISTCOST.PRO".
           COPY "..\PRO\SINDICAT.PRO".
           COPY "..\PRO\RETENFTE.PRO".
           COPY "..\PRO\PARAFISC.PRO".
           COPY "..\PRO\USUAPERS.PRO".
           COPY "..\PRO\PROCNOFI.PRO".
           COPY "..\PRO\VENTNOMI.PRO".
           COPY "..\PRO\ANRTEFTE.PRO".
           COPY "..\PRO\NOMCUENT.PRO".
           COPY "..\PRO\VENOMAES.PRO".
           COPY "..\PRO\CTRLSUEL.PRO".
           COPY "..\PRO\MASCFECH.PRO".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\NOMINOVE.PLB".
           COPY "..\PLB\TEMPINFO.PLB".
           COPY "..\PLB\INFORMES.PLB".
           COPY "..\PLB\REPORTES.PLB".
           COPY "..\PLB\LISTADOS.PLB".
           COPY "..\PLB\CERTIFIC.PLB".
           COPY "..\PLB\CIERRANO.PLB".
           COPY "..\PLB\NOVEFIJA.PLB".
           COPY "..\PLB\CTRLSUEL.PLB".
      *
           COPY "..\STA\CIERRANO.STA".
           COPY "..\STA\NOVEFIJA.STA".
           COPY "..\STA\CONCPTOS.STA".
      *
           COPY "..\OPE\NOMINOVE.DEL".
           COPY "..\OPE\CTRLSUEL.WRI".
      *
           COPY "..\START\NOMINOVE.M00".
           COPY "..\START\CONTRATO.M00".
           COPY "..\START\CONCPTOS.M00".
      *
           COPY "..\LEE\NOMINOVE.SEQ".
           COPY "..\LEE\CONCPTOS.I00".
           COPY "..\LEE\TEMPINFO.I00".
           COPY "..\LEE\CONTRATO.I00".
           COPY "..\LEE\CTRLSUEL.I00".
           COPY "..\LEE\CONTRATO.SEQ".
      ******************************************************************
