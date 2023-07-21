       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG305.
      ***********************************************************
      * PROCESO DE NOMINA DEFINITIVA    ABR-27/2002             *
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
               COPY "..\DYC\TBREDECI.DYC".
               COPY "..\DYC\CNFACTUR.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
               COPY "..\DYC\CNCOMPTE.DYC".
               COPY "..\DYC\MVDOCTOS.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\CONCPTOS.DYC".
               COPY "..\DYC\TABLAEMP.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CONTRATO.DYC".
               COPY "..\DYC\NOMINOVE.DYC".
               COPY "..\SEL\INFORMES.SEL".
               COPY "..\SEL\LISTADOS.SEL".
               COPY "..\SEL\REPORTES.SEL".
               COPY "..\SEL\CERTIFIC.SEL".
               COPY "..\DYC\TEMPINFO.DYC".
               COPY "..\DYC\PRESTAMO.DYC".
               COPY "..\DYC\HISPAGOS.DYC".
               COPY "..\DYC\EXCEDENT.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\TBCONDOC.DYC".
               COPY "..\DYC\CNCOSTOS.DYC".
               COPY "..\DYC\CTRLSUEL.DYC".
      ***************************************************************
      *                 DATA DIVISION                               *
      ***************************************************************
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\CIERRANO.FD".
               COPY "..\FD\TBREDECI.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNFACTUR.FD".
               COPY "..\FD\CNCOSTOS.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\MVDOCTOS.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CONCPTOS.FD".
               COPY "..\FD\TABLAEMP.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\NOMINOVE.FD".
               COPY "..\FD\CERTIFIC.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\LISTADOS.FD".
               COPY "..\FD\TEMPINFO.FD".
               COPY "..\FD\PRESTAMO.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\EXCEDENT.FD".
               COPY "..\FD\HISPAGOS.FD".
               COPY "..\FD\TBCONDOC.FD".
               COPY "..\FD\CTRLSUEL.FD".
      ***************************************************************
      *                 WORKING-STORAGE SECTION                     *
      ***************************************************************
       WORKING-STORAGE SECTION.
       77      ID-PRG              PIC X(7)  VALUE "PROG305".
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG305".
       77      CIUD-1              PIC 9(05) VALUE ZEROS.
      ***************************************************************
      *                 LABEL DE ARCHIVOS                           *
      ***************************************************************
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4) OCCURS 25 TIMES.
      *        COPY "..\LBL\TABLAEMP.728".
      *        COPY "..\LBL\NOMINOVE.728".
      *        COPY "..\LBL\CONTRATO.728".
      *        COPY "..\LBL\AUDITE.728".

               COPY "..\LBL\TABLAEMP.LBL".
               COPY "..\LBL\NOMINOVE.LBL".
               COPY "..\LBL\CONTRATO.LBL".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TBREDECI.LBL".
               COPY "..\LBL\CIERRANO.LBL".
               COPY "..\LBL\CNFACTUR.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
               COPY "..\LBL\MVDOCTOS.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNCOSTOS.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\LBL\LISTADOS.LBL".
               COPY "..\LBL\TEMPINFO.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\CERTIFIC.LBL".
               COPY "..\LBL\PRESTAMO.LBL".
               COPY "..\LBL\EXCEDENT.LBL".
               COPY "..\LBL\HISPAGOS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\TBCONDOC.LBL".
               COPY "..\LBL\CTRLSUEL.LBL".
      ***************************************************************
      *                 AREAS Y VARIABLES DE TRABAJO                *
      ***************************************************************
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
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
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCOSTOS.DCL".
               COPY "..\DCL\CNFACTUR.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
               COPY "..\DCL\MVDOCTOS.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\EXCEDENT.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
               COPY "..\DCL\HISPAGOS.DCL".
               COPY "..\DCL\CONTRATO.DCL".
               COPY "..\DCL\NOMINOVE.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\REPORTES.DCL".
               COPY "..\DCL\LISTADOS.DCL".
               COPY "..\DCL\CERTIFIC.DCL".
               COPY "..\DCL\TBREDECI.DCL".
               COPY "..\DCL\TEMPINFO.DCL".
               COPY "..\DCL\PRESTAMO.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\TBCONDOC.DCL".
               COPY "..\DCL\CTRLSUEL.DCL".
       END DECLARATIVES.
      ***************************************************************
      *                 ESTRUCTURA PRINCIPAL                        *
      ***************************************************************
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROECSS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           IF WIN-PERIODO-ESTADO = 77
                  PERFORM I-WIN-PERIODO-ESTADO THRU F-WIN-PERIODO-ESTADO
                  EXIT PROGRAM.
           IF WIN-PERIODO-ESTADO NOT = 76
            PERFORM I-FALTA-NOMI300 THRU F-FALTA-NOMI300
            EXIT PROGRAM.
           IF WIN-FLAG-CIERRE   NOT  = "I"
                  PERFORM I-FLAG-CIERRE   THRU F-FLAG-CIERRE
                  EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS    THRU F-LABEL-ARCHIVOS.
           PERFORM I-VENOMAES THRU F-VENOMAES.
           PERFORM I-STATUS-HISPAGOS   THRU F-STATUS-HISPAGOS.
           PERFORM I-STATUS-EXCEDENT   THRU F-STATUS-EXCEDENT.
      *JJT PERFORM I-STATUS-NOMINOVE   THRU F-STATUS-NOMINOVE.
           PERFORM I-STATUS-CIERRANO   THRU F-STATUS-CIERRANO
           PERFORM I-BORRA-LIQUINOV    THRU F-BORRA-LIQUINOV.
           PERFORM I-LIMPIE-HORAS      THRU F-LIMPIE-HORAS
           PERFORM I-ABRE-ARCHIVOS     THRU F-ABRE-ARCHIVOS.
           PERFORM I-VALIDA-NOMINOVE   THRU F-VALIDA-NOMINOVE.
           PERFORM I-CEROS-NOMINOVE    THRU F-CEROS-NOMINOVE.
           MOVE "NOMINA DEL"             TO C-DATO
           PERFORM I-PANTALLA          THRU F-PANTALLA.
      *    PERFORM I-APLICA-ISS-MENSUAL THRU F-APLICA-ISS-MENSUAL
           MOVE    0 TO FIN-OK RUT-OK SW-INFORME
           PERFORM I-MUEVE-TEMPINFO    THRU F-MUEVE-TEMPINFO
                   UNTIL FIN-OK = 1.
           CLOSE   TEMPINFO            OPEN I-O TEMPINFO.
           PERFORM I-ORDEN-INFORME     THRU F-ORDEN-INFORME
           PERFORM I-EVALUE-FECHAS     THRU F-EVALUE-FECHAS.
           IF  SW-CESAN-IMP   = 1
               PERFORM I-CAMBIO-CAMPOS-309  THRU F-CAMBIO-CAMPOS-309.
           MOVE    0 TO FIN-OK RUT-OK.
           DISPLAY SPACES LINE 12 POSITION 3 SIZE 76
           DISPLAY SPACES LINE 14 POSITION 3 SIZE 76
           PERFORM I-ACTUALIZA-EMPRESAS  THRU F-ACTUALIZA-EMPRESAS.
           OPEN    I-O NOMINOVE
           PERFORM I-SIGUE-INFORME     THRU F-SIGUE-INFORME
                   UNTIL FIN-OK = 1.
           PERFORM I-TOTAL-INFORME     THRU F-TOTAL-INFORME.
           PERFORM I-CUENTA-REGISTROS  THRU F-CUENTA-REGISTROS.
           PERFORM I-START-NOMINOVE    THRU F-START-NOMINOVE.
           PERFORM I-LIMPIE-HORAS      THRU F-LIMPIE-HORAS.
           MOVE    0 TO FIN-OK RUT-OK WK-LINEAS-L SW-MODE
                     G-VALOR-DEVENGADOS  G-VALOR-DESCUENTOS
                     VR-APLICA F-OTROS-DESCUENTOS F-OTROS-INGRESOS.
      ***************************************************************
      *CUADRO RESUMEN GENERAL Y CONTABILIZACION NOMINA(CONTNOMI.PRO)
      ***************************************************************
           PERFORM I-PROCESO-NOMINOVE  THRU F-PROCESO-NOMINOVE
           MOVE    0 TO FIN-OK WK-LINEAS  RUT-OK SW-MODE
      ***************************************************************
      *GENERA Y CONTABILIZA FACTURA     (NFACTURA.PRO)              *
      ***************************************************************
           PERFORM I-PROCESO-FACTURA   THRU F-PROCESO-FACTURA.
           MOVE    SPACES                TO REG-INFORMES REG-LISTADOS
           WRITE   REG-INFORMES        FROM TOTAL-FAC AFTER 0
           WRITE   REG-LISTADOS        FROM TOTAL-FAC AFTER 0

           PERFORM I-CNCOSTOS-FACTURA  THRU F-CNCOSTOS-FACTURA.
      *    PERFORM I-START-NOMINOVE    THRU F-START-NOMINOVE.
      ***************************************************************
      *              PERFORM EN  PROVAPOR.PRO                       *
      ***************************************************************
           MOVE    0 TO FIN-OK WK-LINEAS  RUT-OK  WR-LINEAS
      *    PERFORM I-PROVISION-APORTES THRU F-PROVISION-APORTES
           MOVE    0 TO FIN-OK WK-LINEAS  RUT-OK  WR-LINEAS
      ***************************************************************
      *              PERFORM EN  ACTUALIZA.PRO                      *
      ***************************************************************
           PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
           PERFORM I-LIMPIE-GVALORES THRU F-LIMPIE-GVALORES.
           PERFORM I-LIMPIE-HORAS    THRU F-LIMPIE-HORAS.
           PERFORM I-MUEVE-CEROS     THRU F-MUEVE-CEROS.
           MOVE    0 TO FIN-OK WK-LINEAS   RUT-OK WK-LINEAS-L
                        G-VALOR-DEVENGADOS G-VALOR-DESCUENTOS
                        VR-APLICA.
           PERFORM I-FIN-MODE          THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      **********************************************************
      *                LABEL ARCHIVOS                          *
      **********************************************************
       I-LABEL-ARCHIVOS.
           MOVE "NIT/COD."                    TO   TITULO-4-NIT
           MOVE "   APELLIDOS Y NOMBRES    "  TO   TITULO-4-DET.
           MOVE WEMPRESA               TO  I-EMPRESA
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE WIN-EMPRESA            TO  I-TITU-EMPRESA
           MOVE ZEROS                  TO  R-NIT-EMPRESA
           MOVE SPACES                 TO  R-EMPRESA
                                           R-DIRECCION-EMP
      *     IF WIN-GENERA-FACTURA = "S"
           MOVE    3                    TO  IND-INFORMES.
           MOVE    1                    TO  IND-CERTIFIC.
           MOVE    3                    TO  IND-LISTADOS.
           MOVE    4                    TO  IND-REPORTES.
           PERFORM I-LABEL-CIERRANO    THRU F-LABEL-CIERRANO.
           PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
           PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
           PERFORM  I-LABEL-CTRLSUEL  THRU  F-LABEL-CTRLSUEL.
           PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
           PERFORM I-LABEL-TEMPINFO    THRU F-LABEL-TEMPINFO.
           PERFORM I-LABEL-CERTIFIC    THRU F-LABEL-CERTIFIC.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-REPORTES    THRU F-LABEL-REPORTES.
           PERFORM I-LABEL-LISTADOS    THRU F-LABEL-LISTADOS.

           MOVE WIN-EMPRESA              TO WK-COMPANIA
           MOVE "น"                      TO WK-RAYAS-U
           MOVE "ฬ"                      TO WK-RAYAS-P
           MOVE "ถ"                      TO WK-RAYA-U
           MOVE "ว"                      TO WK-RAYA-P.
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
      **********************************************************
      *                  ABRE ARCHIVOS                         *
      **********************************************************
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CONCPTOS  TABLAEMP CNCATCTA
                           HOJAVIDA NOMICIUD
               OPEN  I-O   CONTRATO  PRESTAMO EMPRESAS
                           CIERRANO  HISPAGOS
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
           DISPLAY "NOMINA DEFINITIVA-->"
                   LINE  3 POSITION 28
           PERFORM I-ORIENTACION-VENT  THRU F-ORIENTACION-VENT.
           DISPLAY "บ Orden Planilla:"    LINE  8 POSITION  1
           DISPLAY "A,C,N,KA,KC,KN"       LINE  8 POSITION 21
           DISPLAY "SERIE No."            LINE  8 POSITION 36
           DISPLAY  CON-PERIODO           LINE  8 POSITION 46
           DISPLAY " Refte(S/N):"         LINE  8 POSITION 54.
           DISPLAY "   ** NOMINA DEFINITIVA **  "
                        LINE  3 POSITION 25 REVERSE HIGH BLINK
      *    DISPLAY "REGISTRO No.:"              LINE 17 POSITION 09
           DISPLAY "บ"                          LINE 17 POSITION 80.
       020-DISPLAY-DATOS-PERIODO.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1.
       CONFIRMA-INFORME.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1 REVERSE
           DISPLAY " [F2]  TERMINAR " LINE 24 POSITION 32 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO WK-PAGINA.
           IF  F8
               PERFORM I-CAMBIO-CAMPOS-309  THRU F-CAMBIO-CAMPOS-309
               MOVE "C"  TO W-TRUCO
               GO F-PANTALLA.
           IF  W-TRUCO NOT =  "C"
               GO TO CONFIRMA-INFORME.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA.
      *
       I-APLICA-ISS-MENSUAL.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE "N" TO RES-ISSOK.
               ACCEPT RES-ISSOK               LINE 19 POSITION 77
                      UPDATE PROMPT
                      NO BEEP REVERSE
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY RES-ISSOK LINE 19 POSITION 77 REVERSE.
       F-APLICA-ISS-MENSUAL.
      **********************************************************
      *                ACTUALIZA-MUEVE A TEMPINFO              *
      **********************************************************
       I-MUEVE-TEMPINFO.
           PERFORM I-LEE-CONTRATO THRU F-LEE-CONTRATO.
           IF CONTRATO-SW3  >  6 OR FIN-OK = 1
              GO F-MUEVE-TEMPINFO.
           MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA
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
           MOVE CONTRATO-AFILIA-PENSION TO TIMAE-AFILIA-PENSION
           MOVE CONTRATO-AFILIA-SALUD   TO TIMAE-AFILIA-SALUD
      *    MOVE CONTRATO-FONDO-PENSIONES     TO TIMAE-FONDO-PENSIONES
      *    MOVE CONTRATO-PROMOTORA-SALUD     TO TIMAE-PROMOTORA-SALUD
           WRITE REC-TEMPINFO.
           DISPLAY
           "ORDENANDO LA INFORMACION....ESPERE UN MOMENTO POR FAVOR."
                 REVERSE LINE 24 POSITION 1.
           PERFORM I-DISPLAY-TEMPINFO THRU F-DISPLAY-TEMPINFO.
       F-GRABA-TIMAE.

       I-DISPLAY-TEMPINFO.
           MOVE    TIMAE-CEDULA    TO   WI-Z9
           DISPLAY WI-Z9           LINE 12 POSITION 09
           DISPLAY TIMAE-NOMBRE    LINE 12 POSITION 20 SIZE 59.
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
              DISPLAY "ALFABETICO" LINE 08 POSITION 21
              START TEMPINFO KEY >  TIMAE-CLAVE2 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-ALFABETICO.

       I-ORDEN-CODIGO.
           IF WIN-ORDEN                    =  "C "
              DISPLAY "CODIGO"  LINE 08 POSITION 21
              MOVE "CODIGO"               TO  TITULO-4-NIT
              START TEMPINFO KEY > TIMAE-CLAVE1 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-CODIGO.

       I-ORDEN-NIT.
           IF  WIN-ORDEN                    =  "N "
               DISPLAY "CEDULAS"  LINE 08 POSITION 21
             START TEMPINFO KEY > TIMAE-CLAVE INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-NIT.

       I-CC-ALFABETICO.
           IF  WIN-ORDEN                    =  "CA"
               OR  WIN-ORDEN                =  "KA"
               DISPLAY "C.C.- ALFABETICO"  LINE 08 POSITION 21
               START TEMPINFO KEY >  TIMAE-CLAVE3 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-ALFABETICO.

       I-CC-NIT.
           IF  WIN-ORDEN                    =  "CN"
           OR  WIN-ORDEN                    =  "KN"
             DISPLAY "C.C.- CEDULAS "  LINE 08 POSITION 21
               START TEMPINFO KEY > TIMAE-CLAVE4 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-NIT.

       I-CC-CODIGO.
           IF  WIN-ORDEN                    =  "CC"
           OR  WIN-ORDEN                    =  "KC"
             DISPLAY "C.C.- CODIGOS "  LINE 08 POSITION 21
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
           MOVE WK-MASCARA-FECHA         TO  I-FECHA
           MOVE WIN-CODIGO-EMPRESA       TO  I-CODIGO-EMPRESA R-CIA
           MOVE CON-PERIODO              TO  I-PERIODO R-PERIODO
           MOVE W-TPN-FECHA-DESDE        TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA    TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01 OR
               WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           MOVE WK-MASCARA-FECHA       TO  I-FECHA-DESDE C-FECHA-DESDE
                                           R-CONCEPTO-SALARIOS-FDESDE
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
                                           R-CONCEPTO-SALARIOS-FHASTA
           MOVE 88                     TO  WK-LINEAS
           PERFORM I-SELECCIONE-MVDOCTOS THRU F-SELECCIONE-MVDOCTOS.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
           MOVE ZEROS                  TO  W-ULTIMO-CENTRO.
       F-EVALUE-FECHAS.

       I-SELECCIONE-MVDOCTOS.
           PERFORM I-LABEL-MVDOCTOS   THRU F-LABEL-MVDOCTOS
           PERFORM I-STATUS-MVDOCTOS  THRU F-STATUS-MVDOCTOS
           OPEN I-O MVDOCTOS.
       F-SELECCIONE-MVDOCTOS.

      ***************************************************************
      *                    I-SIGUE-INFORME                          *
      ***************************************************************
       I-SIGUE-INFORME.
           IF  SW-CESAN-IMP  NOT = 1
               MOVE "NOMINA DEFINITIVA DE "
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
           PERFORM I-ANALIZE-NOVEDADES  THRU F-ANALIZE-NOVEDADES.
      *JJ  IF TIMAE-AFILIA-LEY100 = "S"
           PERFORM I-ANALIZE-LEY100     THRU F-ANALIZE-LEY100.
           PERFORM I-ANALIZE-SINDICATO  THRU F-ANALIZE-SINDICATO.
           PERFORM I-ANALIZE-WDCTOS-POSITIV
              THRU F-ANALIZE-WDCTOS-POSITIV
           COMPUTE W-NETO-A-PAGAR       =    W-VALOR-DEVENGADOS
                                        +    W-VALOR-DESCUENTOS
           IF W-NETO-A-PAGAR IS NEGATIVE
              MOVE 1 TO SW-INFORME.
           MOVE    W-NETO-A-PAGAR         TO TIMAE-NETO
           REWRITE REC-TEMPINFO.
           ADD     W-NETO-A-PAGAR         TO S-NETO-A-PAGAR
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
           PERFORM I-SUMA-NOVEDADES  THRU F-SUMA-NOVEDADES.
           PERFORM I-ACTUALIZE-FONDO THRU F-ACTUALIZE-FONDO.
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
               MOVE "** ERRADO"      TO TE021-NOMBRE-CENTRO-COSTO.
           PERFORM I-BORRA-LINEA-DETALLE THRU F-BORRA-LINEA-DETALLE
           PERFORM I-TOTAL-CENTRO THRU F-TOTAL-CENTRO
      * ESTO LO ADICIONE PARA NO IMPRIMIR LINEAS EN ZEROS
           IF S-NETO-A-PAGAR NOT = 0
             WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   AFTER 1
             WRITE REG-INFORMES        FROM  LINEA-TOTALES-A AFTER 1
             WRITE REG-INFORMES        FROM  LINEA-TOTALES-B AFTER 1
             WRITE REG-INFORMES        FROM  LINEA-TOTAL-0   AFTER 1
             ADD  4                      TO  WK-LINEAS.
           IF S-NETO-A-PAGAR NOT = 0
           PERFORM I-SUBTOTAL-S-PRC-PREST THRU F-SUBTOTAL-S-PRC-PREST
           WRITE REG-LISTADOS        FROM  LINEA-TOTAL-0   AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTALES-A AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTALES-B AFTER 1
           WRITE REG-LISTADOS        FROM  LINEA-TOTAL-0   AFTER 1.

           PERFORM I-LIMPIE-HORAS         THRU F-LIMPIE-HORAS.
           PERFORM I-BORRA-LINEA-TOTALES THRU F-BORRA-LINEA-TOTALES.
       SALE-TOTALES-POR-CENTRO.
      ***************************************************************
      *                    I-EVALUE-FECTER                          *
      ***************************************************************
      *I-EVALUE-FECTER.
      *    IF W-TPN-FECHA-HASTA >  CONTRATO-FECTER
      *       COMPUTE W-VALOR   =  WIN-SALMIN-ACT
      *                         /  30
      *                         *  W-TPN-NUMERO-DIAS
      *                         +  0.50
      *       IF W-VALOR-BASE-RFTE <  W-VALOR-ENTERO
      *        MOVE W-VALOR-ENTERO   TO  W-VALOR-BASE-RFTE.
      *
      *    IF  W-TPN-FECHA-HASTA            >  CONTRATO-FECTER
      *        MOVE W-TPN-FECHA-HASTA      TO  CONTRATO-FECTER
      *        REWRITE REC-CONTRATO.
      *F-EVALUE-FECTER.

      ***************************************************************
      *                I-PROCESO-NOMINOVE                           *
      ***************************************************************
       I-PROCESO-NOMINOVE.
           MOVE "   *CUADRO DE RESUMEN GENERAL*    "
             TO I-TITU-PLANILLA.
           MOVE 0 TO W-SALARIOS  W-PRESTACIONES W-SUBTOT.
           PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES.
           PERFORM I-PRIMER-REGISTRO  THRU F-PRIMER-REGISTRO.
      *    MOVE 0 TO RUT-OK.
           PERFORM I-PROCESS-NOMINOVE THRU F-PROCESS-NOMINOVE
                       UNTIL RUT-OK = 1
           PERFORM I-ULTIMO-REGISTRO  THRU F-ULTIMO-REGISTRO.
           PERFORM I-TOTAL-GEN        THRU F-TOTAL-GEN.
       F-PROCESO-NOMINOVE.
      *
       I-PRIMER-REGISTRO.
           PERFORM I-LEE-NOMINOVE  THRU F-LEE-NOMINOVE.
           MOVE    NOV-EMPRESA2      TO WNOV-EMPRESA2.
           MOVE    NOV-CONCEPTO2     TO WNOV-CONCEPTO2.
           PERFORM I-MUEVE         THRU F-MUEVE.
           PERFORM I-SUME-NOMINOVE THRU F-SUME-NOMINOVE
           IF RUT-OK NOT = 1
           PERFORM I-LEE-NOMINOVE  THRU F-LEE-NOMINOVE.
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
       F-PROCESS-NOMINOVE.
      *
       COD-NUEVO.
           PERFORM I-TOTAL-CONCEPTO  THRU F-TOTAL-CONCEPTO
           MOVE    NOV-CONCEPTO2       TO WNOV-CONCEPTO2.
           PERFORM I-MUEVE           THRU F-MUEVE.
           PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
           PERFORM I-SUME-NOMINOVE   THRU F-SUME-NOMINOVE
           PERFORM I-LEE-NOMINOVE    THRU F-LEE-NOMINOVE.
       F-COD-NUEVO.
      *
       I-MUEVE.
           PERFORM I-LEE-CONCPTOS       THRU F-LEE-CONCPTOS.
           MOVE    NOV-CONCEPTO         TO   I-NOV-CONCEPTO
           PERFORM I-MUEVE-CONCPTOS        THRU F-MUEVE-CONCPTOS.
           MOVE    W-CONCPTOS-DESCRIPCION  TO   I-NOV-DESCRIPCION.
       F-MUEVE.

       I-MUEVE-CONCPTOS.
      *TABLA 050 : CONCEPTOS NOMINA
           MOVE REC-CONCPTOS TO W-REC-CONCPTOS.
       F-MUEVE-CONCPTOS.
      *
       I-SUME-NOMINOVE.
           PERFORM I-SUMATORIA-BAS-DCTOS THRU F-SUMATORIA-BAS-DCTOS.
           IF W-CONCPTOS-CONT-NIT-EMPRESAS = "S"
              PERFORM I-CONT-INDIVIDUAL THRU F-CONT-INDIVIDUAL.
       F-SUME-NOMINOVE.
      *
       I-ULTIMO-REGISTRO.
           PERFORM I-TOTAL-CONCEPTO THRU F-TOTAL-CONCEPTO.
       F-ULTIMO-REGISTRO.

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
      *JC     IF W-CONCPTOS-LISTADOS-INF = "S"
              PERFORM I-TOTAL-ANEXO  THRU F-TOTAL-ANEXO.

           IF W-NOV-VLR-DESCTOS IS POSITIVE
              COMPUTE W-NOV-VLR-DESCTOS = W-NOV-VLR-DESCTOS
                                        * (-1).
           IF W-DCTOS-OTROS-SIN-PF   IS POSITIVE
               COMPUTE W-DCTOS-OTROS-SIN-PF   = W-DCTOS-OTROS-SIN-PF
                                            * (-1).
           IF W-CONCPTOS-CONT-NIT-EMPRESAS = "N"
           PERFORM I-CONT-NOMINA        THRU F-CONT-NOMINA.
           PERFORM I-PARAFISCALES       THRU F-PARAFISCALES.
           PERFORM I-DISPLAY-NOMINOVE   THRU F-DISPLAY-NOMINOVE.
           MOVE W-NOV-DIAS                TO I-NOV-DIAS
           MOVE W-NOV-HORAS               TO I-NOV-HORAS
           MOVE W-NOV-VLR-DEVENG          TO I-NOV-VLR-DEVENG

           IF W-NOV-VLR-DESCTOS > 0 OR
              W-NOV-VLR-DESCTOS < 0
              MOVE W-NOV-VLR-DESCTOS    TO  I-NOV-VLR-DEVENG.

           MOVE W-DCTOS-OTROS-SIN-PF    TO  I-NOV-VLR-DESCTOS
           MOVE W-TOTAL-PARAFISCALES    TO  I-WTOTAL-PARAFISCALES

           MOVE S-TOTAL-PARAFISCALES    TO  I-STOTAL-PARAFISCALES.
           MOVE W-TOTAL-ADMINISTRACION  TO  I-WTOTAL-ADMINISTRACION.
           MOVE S-TOTAL-ADMINISTRACION  TO  I-STOTAL-ADMINISTRACION.
           PERFORM I-DETAIL1          THRU F-DETAIL1.
           PERFORM I-GRABA-CNCOSTOS   THRU F-GRABA-CNCOSTOS.
           PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES.
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
                     I-WIN-PRC-ADMON         W-DCTOS-OTROS-SIN-PF
                     JT-SALARIOS             JT-PRESTACIONES
                     JT-WTOTAL-PARAFISCALES  JT-SUBTRANS
                     JT-WTOTAL-ADMINISTRACION.
       F-CEROS-VARIABLES.
      ***************************************************************
      *                    I-DISPLAY-NOMINOVE                       *
      ***************************************************************
       I-DISPLAY-NOMINOVE.
           DISPLAY W-CONCPTOS-CODIGO-CONCEPTO   LINE 13 POSITION 02
           DISPLAY W-CONCPTOS-DESCRIPCION       LINE 13 POSITION 09
           DISPLAY "PARAFISCALES:"              LINE 15 POSITION 09
           MOVE    W-TOTAL-PARAFISCALES         TO  WI-Z9
           DISPLAY WI-Z9                        LINE 15 POSITION 57
           DISPLAY "SUBTOTAL    :"              LINE 16 POSITION 09
           MOVE    S-TOTAL-PARAFISCALES         TO WI-Z9
           DISPLAY WI-Z9                        LINE 16 POSITION 57
           DISPLAY "ADMNISTRCION:"              LINE 17 POSITION 09
           MOVE    W-TOTAL-ADMINISTRACION       TO WI-Z9
           DISPLAY WI-Z9                        LINE 17 POSITION 57
           DISPLAY "TOTAL       :"              LINE 18 POSITION 09
           MOVE    S-TOTAL-ADMINISTRACION       TO WI-Z9
           DISPLAY WI-Z9                        LINE 18 POSITION 57.
       F-DISPLAY-NOMINOVE.
      *
       I-DETAIL1.
           IF   WK-LINEAS = 0  OR WK-LINEAS > 57
                PERFORM I-TITULOS-TOTALES THRU F-TITULOS-TOTALES.
           WRITE REG-INFORMES FROM LINEA-DETALLE-TOTAL AFTER 1
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
            MOVE "NOMINA DEL"               TO C-DATO
            PERFORM I-DATOS-NOMINA        THRU F-DATOS-NOMINA.
            PERFORM I-FECHA-NOMINA        THRU F-FECHA-NOMINA.
            MOVE WCTANOMI TO   CUENTA-MVDOCTOS  ALT09CTA-MVDOCTOS
                               ALTCTA-MVDOCTOS  ALT04CTA-MVDOCTOS
                               ALTCTA1-MVDOCTOS ALT12CTA-MVDOCTOS.
            MOVE WIN-NIT                    TO  ALTNIT-MVDOCTOS
            MOVE G-NETO-A-PAGAR             TO  VALOR-MVDOCTOS.
            COMPUTE VALOR-MVDOCTOS ROUNDED   =  VALOR-MVDOCTOS
                                           *   (-1).
            IF VALOR-MVDOCTOS IS POSITIVE
               MOVE 1 TO SIGNO-MVDOCTOS.
            IF VALOR-MVDOCTOS IS NEGATIVE
               MOVE 2 TO SIGNO-MVDOCTOS.
            PERFORM I-GRABA-MVDOCTOS    THRU  F-GRABA-MVDOCTOS.
            MOVE G-WTOTAL-PARAFISCALES    TO  T-WTOTAL-PARAFISCALES
            MOVE G-STOTAL-PARAFISCALES    TO  T-STOTAL-PARAFISCALES.
            MOVE G-WTOTAL-ADMINISTRACION  TO  T-WTOTAL-ADMINISTRACION.
            MOVE G-STOTAL-ADMINISTRACION  TO  T-STOTAL-ADMINISTRACION.
            MOVE G-STOTAL-ADMINISTRACION  TO  R-NETO-A-FACTURAR

            COMPUTE W-SUBTOT   = G-SUBSIDIO-TRANSPORTE
                               - G-DCTOS-SUBTRANS
                               + G-OTROS-DEV-SUBTRANS.

            COMPUTE W-SALARIOS = G-VALOR-DEVENGADOS
                               - W-PRESTACIONES
                               - W-SUBTOT.

            IF G-VALOR-DEVENGADOS > ZEROS OR
               G-VALOR-DEVENGADOS < ZEROS
               COMPUTE W-SALARIOS = G-VALOR-DEVENGADOS
                               - W-PRESTACIONES
                               - W-SUBTOT
               MOVE W-SALARIOS TO  JT-SALARIOS
            ELSE
               COMPUTE W-PRESTACIONES = W-PRESTACIONES
                                      + W-SUBTOT.
            MOVE G-WTOTAL-PARAFISCALES    TO  JT-WTOTAL-PARAFISCALES
            MOVE W-SUBTOT                 TO  JT-SUBTRANS

            MOVE W-PRESTACIONES           TO  JT-PRESTACIONES
            MOVE G-WTOTAL-ADMINISTRACION  TO  JT-WTOTAL-ADMINISTRACION
            PERFORM I-DETAIL2    THRU F-DETAIL2.
       F-TOTAL-GEN.

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
      ***************************************************************
      *                    I-ACTUALIZA-EMPRESAS                     *
      ***************************************************************
       I-ACTUALIZA-EMPRESAS.
           DISPLAY WK-ESPACIOS         LINE 24 POSITION  1.
           DISPLAY "ACTUALIZANDO CONSECUTIVO..." LINE 24 POSITION 1
                    REVERSE.
      ***************************************************************
      *RUTINA DE BUSQUEDA DEL No.CONSECUTIVO DEL COSTO A CONTABILIZAR
      ***************************************************************
           IF   SW-CESAN-IMP = 1
                MOVE 19 TO W-COMP
           ELSE
                MOVE 07 TO W-COMP.
           PERFORM I-BUSCA-CONSECUTIVO THRU F-BUSCA-CONSECUTIVO
           MOVE WR-RADIC-CONTABLE        TO  I-DOCTO-CONTABLE
                                             R-NRO-COSTO
                                             WK-COSTO.
           MOVE CON-PERIODO TO WIN-NOMINA
           PERFORM I-ACT-TBCONDOC       THRU F-ACT-TBCONDOC.
      ***************************************************************
      *ACTUALIZA EL PERIODO DE PAGO EN EL MAESTRO DE EMPRESAS
      ***************************************************************
           CLOSE    EMPRESAS.


           OPEN             I-O            EMPRESAS.
           MOVE CON-COD-EMPRESA        TO  WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS      THRU F-LEE-EMPRESAS.
           MOVE WIN-PRC-PRESTACIONAL   TO  L-WIN-PRC-PRESTACIONAL
           MOVE WIN-PRC-ADMON          TO  L-WIN-PRC-ADMON

           IF   SW-CESAN-IMP = 1
                MOVE   ZEROS           TO  WIN-PERIODO-ESTADO
                MOVE    "C"            TO  WIN-FLAG-CIERRE
           ELSE
                MOVE    77             TO  WIN-PERIODO-ESTADO
                MOVE   SPACES          TO  WIN-FLAG-CIERRE.
           MOVE CON-PERIODO            TO  WIN-NOMINA
           REWRITE REC-EMPRESAS.

       F-ACTUALIZA-EMPRESAS.
      *
       I-STATUS-MVDOCTOS.
      *        MOVE ZEROS                  TO  SW-ABRE
      *        OPEN INPUT MVDOCTOS
      *        MOVE 1                      TO  SW-ABRE
      *        IF  MVDOCTOS-STATUS              =  "35"
                   OPEN OUTPUT MVDOCTOS
      *        ELSE
      *            IF  MVDOCTOS-STATUS              >  "00"
      *                PERFORM RUTINA-ERROR-MVDOCTOS
      *                PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE MVDOCTOS.
       F-STATUS-MVDOCTOS.

       I-STATUS-CIERRANO.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CIERRANO
               MOVE 1                      TO  SW-ABRE
               IF  CIERRANO-STATUS              =  "35"
                   OPEN OUTPUT CIERRANO
               ELSE
                   IF  CIERRANO-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CIERRANO
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE CIERRANO.
       F-STATUS-CIERRANO.

       I-FIN-MODE.
           DISPLAY "CERRANDO ARCHIVOS..." LINE 24 POSITION 1
                  REVERSE BEEP.
           CLOSE CONCPTOS TABLAEMP CNCATCTA CIERRANO
                 CONTRATO HOJAVIDA NOMINOVE EMPRESAS NOMICIUD
                 PRESTAMO MVDOCTOS TEMPINFO INFORMES HISPAGOS
                 REPORTES LISTADOS CERTIFIC.

           OPEN OUTPUT LISTADOS.
           CLOSE LISTADOS.

           CANCEL "PROG305.COB"
           IF WMODULO-CONTABLE = "S"
            CALL "PROG785.COB" USING AREA-LINK-NOMINA
            CANCEL "PROG785.COB"
           END-IF
           CANCEL "PROG305.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

       I-GRABA-COMP-EGRESO.
             DISPLAY " "  LINE  24 POSITION 70.
       F-GRABA-COMP-EGRESO.

      *
       I-FLAG-CIERRE.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP REVERSE
            DISPLAY "บ                E R R O R  !                    บ"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ       NO HA TERMINADO LA SERIE ANTERIOR        บ"
            LINE 06 POSITION 17 BEEP REVERSE.
            DISPLAY "บ         VERIFIQUE EL PASO A SEGUIR             บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "บ               QUE LE HACE FALTA                บ"
            LINE 08 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP REVERSE
            "     <ENTER>   CONTINUE     "
            LINE 10 POSITION 27 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-FLAG-CIERRE.   EXIT.

       I-WIN-PERIODO-ESTADO.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP REVERSE
            DISPLAY "บ                 ATENCION!                      บ"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ  ** YA FUE PROCESADA LA NOMINA DEFINITIVA **   บ"
            LINE 06 POSITION 17 BEEP REVERSE.
            DISPLAY "บ          EL PASO EN LA SERIE SON LOS           บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "บ PROCESOS DE PAGO (Consignacion-Cheque-Efectivo)บ"
            LINE 08 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP REVERSE
            "        <ENTER> CONTINUE   "
            LINE 10 POSITION 27 BEEP REVERSE BLINK.
            PERFORM ACEPTA-TRUCO.
       F-WIN-PERIODO-ESTADO.   EXIT.
      *
       I-FALTA-NOMI300.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ                  E R R O R!                    บ"
            LINE 06 POSITION 17 BEEP REVERSE
            DISPLAY "บ   *** NO HA PROCESADO LA PRENOMINA ***         บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 08 POSITION 17 BEEP REVERSE
            "   <ENTER>     Continue"
            LINE 09 POSITION 27 BEEP REVERSE BLINK.
            PERFORM ACEPTA-TRUCO.
       F-FALTA-NOMI300. EXIT.
      *
       I-INTER-CESANT.
           IF W-PROGRAMA     = "PROG305" AND
              NOV-CONCEPTO   =  095
                   MOVE TIMAE-CEDULA TO CESAN-CEDULA00
              ELSE
                   GO R-INTER.
              READ CIERRANO  WITH NO LOCK INVALID KEY
                  GO R-INTER.
                 MOVE     1   TO  CESAN-SW-INTERESES
                 REWRITE  REC-CIERRANO.
        R-INTER.
           IF W-PROGRAMA     = "PROG305" AND
              NOV-CONCEPTO   =  096
              MOVE TIMAE-CEDULA TO CESAN-CEDULA00
              ELSE
                 GO F-INTER-CESANT.
              READ CIERRANO  WITH NO LOCK INVALID KEY
                  GO F-INTER-CESANT.
                 MOVE     1   TO  CESAN-SW-INTERESES
                 REWRITE  REC-CIERRANO.
       F-INTER-CESANT.
      *
      **************************************************
      *               OTROS PROCESOS                   *
      **************************************************
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TIPOPERI.PRO".
           COPY "..\PRO\DISCAPDA.PRO".
           COPY "..\PRO\MASCFECH.PRO".
           COPY "..\PRO\MONTOESC.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\NSUMANOV.PRO".
           COPY "..\PRO\LEEARCHI.PRO".
      *IMPRESION Y CONTABILIZACION DE LAS FACTURAS
           COPY "..\PRO\NFACTURA.PRO".
      *IMPRESION Y CONTABILIZACION DE LAS PROVISIONES
      *    COPY "..\PRO\PROVAPOR.PRO".
      *IMPESION DE ANEXOS A LA PLANILLA DE LA NOMINA
           COPY "..\PRO\ANEXOSNO.PRO".
      *CONTABILIZACION DE LA PLANILLA DE LA NOMINA
           COPY "..\PRO\CONTNOMI.PRO".
      *Grabaciขn de los datos de la Factura.
           COPY "..\PRO\CNFACTGR.PRO".
           COPY "..\PRO\CNCOSTGR.PRO".
           COPY "..\PRO\NOLEY100.PRO".
           COPY "..\PRO\SINDICAT.PRO".
           COPY "..\PRO\CONTINDI.PRO".
           COPY "..\PRO\PARAFISC.PRO".
           COPY "..\PRO\BUSCACON.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\TBCONDOC.PRO".
           COPY "..\PRO\VENTNOMI.PRO".
           COPY "..\PRO\ACTULFON.PRO".
           COPY "..\PRO\DISTCOST.PRO".
           COPY "..\PRO\NOMCUENT.PRO".
           COPY "..\PRO\VENOMAES.PRO".
           COPY "..\PRO\CTRLSUEL.PRO".
      *
           COPY "..\OPE\NOMINOVE.DEL".
           COPY "..\OPE\CTRLSUEL.WRI".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\NOMINOVE.PLB".
           COPY "..\PLB\MVDOCTOS.PLB".
           COPY "..\PLB\CIERRANO.PLB".
           COPY "..\PLB\CNCOSTOS.PLB".
           COPY "..\PLB\TEMPINFO.PLB".
           COPY "..\PLB\INFORMES.PLB".
           COPY "..\PLB\REPORTES.PLB".
           COPY "..\PLB\CERTIFIC.PLB".
           COPY "..\PLB\LISTADOS.PLB".
           COPY "..\PLB\CNFACTUR.PLB".
           COPY "..\PLB\CTRLSUEL.PLB".
      *
           COPY "..\STA\TBCONDOC.STA".
           COPY "..\STA\HISPAGOS.STA".
           COPY "..\STA\EXCEDENT.STA".
      *
           COPY "..\START\NOMINOVE.M00".
           COPY "..\START\CONTRATO.M00".
      *
           COPY "..\LEE\CONTRATO.I00".
           COPY "..\LEE\NOMINOVE.SEQ".
           COPY "..\LEE\CTRLSUEL.I00".
           COPY "..\LEE\CONTRATO.SEQ".
      ******************************************************************
