       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG210.
      ******************************************************************
      * CAPTURA DE NOVEDADES POR EXCEPCION   ABR-27-2002               *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\CONTRATO.DYC".
           COPY "..\DYC\NOMINOVE.DYC".
           COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\HOJAVIDA.FD".
           COPY "..\FD\CONTRATO.FD".
           COPY "..\FD\NOMINOVE.FD".
           COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      ID-PRG               PIC X(7)       VALUE "PROG210".
       77      W-PROGRAMA           PIC X(7)       VALUE "PROG210".
       77  W-NOMEMP-DE3               PIC X(03) VALUE SPACES.
      *
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA      PIC 9(4)       OCCURS 25 TIMES.
      *
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CONTRATO.LBL".
           COPY "..\LBL\HOJAVIDA.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\LBL\NOMINOVE.LBL".
           COPY "..\LBL\USER-ACT.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\CONCPTOS.WRK".
           COPY "..\WRK\TABLAGEN.WRK".
           COPY "..\WRK\CONTRATO.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TABLAEMP.DCL".
           COPY "..\DCL\CONCPTOS.DCL".
           COPY "..\DCL\HOJAVIDA.DCL".
           COPY "..\DCL\CONTRATO.DCL".
           COPY "..\DCL\NOMINOVE.DCL".
           COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
           COPY "..\PRO\CONTROLA.PRO".
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER   =   1   EXIT PROGRAM.
           PERFORM I-REVISA-USUAPERS-ACCE THRU F-REVISA-USUAPERS-ACCE.
           PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
           IF WIN-FLAG-CIERRE   NOT  = "I"
              PERFORM I-FLAG-CIERRE   THRU F-FLAG-CIERRE
              GO  TO 120-FIN-FIN.
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS.
           PERFORM I-STATUS-NOMINOVE THRU F-STATUS-NOMINOVE.
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS.
           PERFORM VALIDA-TABLA-PERIODOS THRU SALE-VALIDA-TABLA-PERIODOS
           IF  SW-3                         >  ZEROS
           OR  TAB-EMP-CODIGO-TABLA         >  011
               MOVE "90"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
           PERFORM BORRA-TABLANOV THRU SALE-BORRA-TABLANOV
           PERFORM CARGA-TABLANOV THRU SALE-CARGA-TABLANOV
           COMPUTE W-TAMANO-TABLANOV    =  I  -  1.
           IF  W-TAMANO-TABLANOV            >  ZEROS
               NEXT SENTENCE
           ELSE
               MOVE "01"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE 1                      TO  SW-RELIQUIDA.
           PERFORM I-CEROS-NOMINOVE THRU F-CEROS-NOMINOVE.
           PERFORM I-PANTALLA       THRU F-PANTALLA.
           PERFORM PANTALLA-OPCIONES
           PERFORM CARGA-TABLA-PERIODOS THRU SALE-CARGA-TABLA-PERIODOS.
       025-OPCION.
           DISPLAY W-ESPALF  LINE  4 POSITION 35 SIZE 19
           PERFORM I-CEROS-NOMINOVE THRU F-CEROS-NOMINOVE.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-OPCION             LINE  4 POSITION 35
                                                    NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP
               GO TO 025-OPCION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-OPCION                     =  "A"
           OR  W-OPCION                     =  "C"
           OR  W-OPCION                     =  "M"
           OR  W-OPCION                     =  "R"
               MOVE SPACES                 TO  SW-CONSULTA
               GO TO 025-SIGUE-OPCION
           ELSE
 
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               DISPLAY " OPCION ERRADA "            LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " TECLEE UNICAMENTE  A / C / M / R "
                                                    LINE 24 POSITION 18
                                                    REVERSE
               GO TO 025-OPCION.
       025-SIGUE-OPCION.
               DISPLAY WK-ESPACIOS   LINE 24 POSITION 1
               DISPLAY W-OPCION      LINE  4 POSITION 35
           IF  W-OPCION                     =  "A"
               DISPLAY "   ADICION  " LINE  4 POSITION 37
                                                    REVERSE
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY "  CONSULTA  " LINE  4 POSITION 37
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "M"
               DISPLAY "MODIFICACION" LINE  4 POSITION 37
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "R"
               DISPLAY     "RETIRO  " LINE  4 POSITION 37
                                                    REVERSE.
               MOVE 12                     TO  I-LINEA
               MOVE 12                     TO  K-LINEA
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       030-CEDULA.
               PERFORM OPCIONES-CEDULA
               MOVE ZEROS                  TO  SW-1
                                               WK-EXCEPTION
               MOVE NOV-CODIEMP            TO  CONTRATO-CEDULA
               MOVE CONTRATO-CEDULA        TO  WI-Z12
               DISPLAY WI-Z12              LINE  6 POSITION 12
               ACCEPT CONTRATO-CEDULA      LINE  6 POSITION 12
      *                 UPDATE
                        CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
           IF  CLR
               GO TO 030-CEDULA.
           IF  CUP
               GO TO 025-OPCION.
           IF  ERR
               MOVE ZEROS                  TO  WK-EX
               DISPLAY " CEDULA ERRADA "            LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CEDULA.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CONTRATO-CEDULA                 =  ZEROES
               MOVE NOV-CODIEMP            TO  CONTRATO-CEDULA.
               MOVE CONTRATO-CEDULA           TO  WI-Z12
               DISPLAY WI-Z12                       LINE  6 POSITION 12
           IF  CDN
               START CONTRATO KEY           >  CONTRATO-CLAVE
                                      INVALID  KEY
               GO TO 115-FIN-CONSULTAR.
           IF  CDN
               READ CONTRATO                   NEXT RECORD
           WITH NO LOCK AT  END
               GO TO 115-FIN-CONSULTAR.
           IF  CDN
               GO TO 030-VALIDA-ESTADO-EMPLEADO.
               MOVE 1                      TO  SW-1
               READ CONTRATO          WITH NO LOCK INVALID  KEY
               MOVE 2                      TO  SW-1.
           IF  SW-1                         =  2
               DISPLAY " CEDULA EMPLEADO NO EXISTE "
                                                    LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CEDULA.
           IF  CONTRATO-SW3                    =  7
           OR  CONTRATO-SW3                    =  8
               DISPLAY " EMPLEADO EN LIQUIDACION "  LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CEDULA
           ELSE
           IF  CONTRATO-SW3                    =  9
               DISPLAY " EMPLEADO RETIRADO          "
                                                    LINE 24 POSITION  1
                                                    REVERSE
               MOVE CONTRATO-FECRET           TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE 24 POSITION 20
                                                    REVERSE
               GO TO 030-CEDULA.
       030-VALIDA-ESTADO-EMPLEADO.
           IF  CONTRATO-SW3                    >  6
               START CONTRATO KEY           >  CONTRATO-CLAVE
                                      INVALID  KEY
               GO TO 115-FIN-CONSULTAR.
           IF  CONTRATO-SW3                    >  6
               READ CONTRATO                   NEXT RECORD
           WITH NO LOCK AT  END
               GO TO 115-FIN-CONSULTAR.
           IF  CONTRATO-SW3                    >  6
               GO TO 030-VALIDA-ESTADO-EMPLEADO.
       030-SIGUE-CEDULA.
               MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
               MOVE 1                      TO  SW-3
               READ HOJAVIDA       WITH NO LOCK    INVALID  KEY
               MOVE 2                      TO  SW-3.
           DISPLAY CONTRATO-CODIGO     LINE 7 POSITION 12
           MOVE CONTRATO-SALARIO       TO WI-SZ9P
           DISPLAY WI-SZ9P             LINE 8 POSITION 12
      ****** CONVERTIR FECHA-INGRESO A MASCARA-FECHA
           MOVE CONTRATO-FECING        TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 07 POSITION 40.

           IF  SW-3                         =  2
               DISPLAY " HOJA DE VIDA EMPLEADO NO EXISTE "
                                                    LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CEDULA.
           IF  CONTRATO-SW3                    =  0
               DISPLAY " EMPLEADO INGRESANDO "      LINE 24 POSITION  1
                                                    REVERSE.
           IF  CONTRATO-CONTRTO                =  3
               COMPUTE W-VALOR-HORA         =  CONTRATO-SALARIO
                                            /  180
           ELSE
               COMPUTE W-VALOR-HORA         =  CONTRATO-SALARIO
                                            /  240.
           PERFORM I-CALCULO-W-VALOR-HORA THRU F-CALCULO-W-VALOR-HORA.
               COMPUTE W-VALOR-DIA          =  CONTRATO-SALARIO
                                            /  30
           IF  WIN-TIPO-LIQUIDACION         =  "C"
           IF  W-HORAS-PERIODO              <  CONTRATO-HORAS-PERIODO
               MOVE W-HORAS-PERIODO        TO  CONTRATO-HORAS-PERIODO.
               MOVE CONTRATO-CEDULA     TO  WI-Z12
               DISPLAY WI-Z12           LINE  6 POSITION 12
               DISPLAY HOJAVIDA-NOMBRE  LINE  6 POSITION 26 SIZE 25
                                                REVERSE

               MOVE ZEROS                  TO  SW-PRIMERO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1.
       030-LECTURA-NOVEDADES.
               MOVE ZEROS                  TO  K
                                               M
               MOVE 11                     TO  K-LINEA
               MOVE 11                     TO  I-LINEA
               PERFORM BORRA-NOVEDADES THRU
                       SALE-BORRA-NOVEDADES
               PERFORM CARGA-NOVEDADES THRU
                       SALE-CARGA-NOVEDADES
               MOVE W-TAMANO-NOVEDADES     TO  W-TAMANO-AUTOMATICAS
               DISPLAY "NOVEDADES LEIDAS : "        LINE 24 POSITION  1
               DISPLAY W-TAMANO-NOVEDADES           LINE 24 POSITION 15.
           IF  W-OPCION                     =  "A"
               MOVE W-TAMANO-NOVEDADES     TO  K
               ADD  W-TAMANO-NOVEDADES     TO  K-LINEA.
               GO TO 060-LIQUIDA-NOVEDAD.
       040-CODIGO-CONCEPTO.
           IF  W-OPCION                     =  "C"
               GO TO 115-CONSULTAR.
           IF  W-OPCION                     =  "R"
               GO TO 105-ELIMINAR.
           IF  W-OPCION                     =  "A"
           OR  W-OPCION                     =  "M"
               NEXT SENTENCE
           ELSE
               GO TO 070-CODIGO-CONCEPTO.
               MOVE J                      TO  K
               MOVE I-LINEA                TO  K-LINEA
               PERFORM OPCIONES-NOVEDAD
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-CONCEPTO (J)     TO  W-TCN-CODIGO-CONCEPTO
           IF  W-TCN-CODIGO-CONCEPTO    =  ZEROS
               MOVE "E"                    TO  T-NOV-FLAG-ORIGEN    (L).
           DISPLAY W-TCN-CODIGO-CONCEPTO   LINE I-LINEA POSITION 2
           ACCEPT W-TCN-CODIGO-CONCEPTO    LINE I-LINEA POSITION 2
                  UPDATE CONVERT TAB NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  W-TCN-CODIGO-CONCEPTO    =  ZEROS
               MOVE T-NOV-CONCEPTO (J)     TO  W-TCN-CODIGO-CONCEPTO.
               MOVE W-TCN-CODIGO-CONCEPTO  TO  T-NOV-CONCEPTO (J)
               DISPLAY T-NOV-CONCEPTO (J)  LINE I-LINEA POSITION 2
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  F1    AND  W-TCN-CODIGO-CONCEPTO = ZEROS
               GO TO 080-GRABACION.
           IF  F3    AND  W-TCN-CODIGO-CONCEPTO = ZEROS
               GO TO 080-GRABACION.
           IF  F1
           OR  F3
               GO TO 080-GRABACION.
           IF  CUP
           AND W-OPCION                     =  "A"
           IF  I-LINEA                      =  12
           OR  J                        NOT >  W-TAMANO-AUTOMATICAS
               MOVE K                      TO  J
               MOVE K-LINEA                TO  I-LINEA
               GO TO 040-CODIGO-CONCEPTO
           ELSE
               SUBTRACT 1                FROM  J
               SUBTRACT 1                FROM  I-LINEA
           IF  J                        NOT >  W-TAMANO-AUTOMATICAS
               MOVE K                      TO  J
               MOVE K-LINEA                TO  I-LINEA
               GO TO 040-CODIGO-CONCEPTO
           ELSE
           IF  I-LINEA                      <  12
               ADD  1                      TO  K-LINEA
               SUBTRACT 1                FROM  M
               GO TO 065-DISPLAY-DATOS
           ELSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  CUP
           AND W-OPCION                     =  "M"
               SUBTRACT 1                FROM  J
               SUBTRACT 1                FROM  I-LINEA
           IF  J                            <  1
               MOVE 1                      TO  J
               MOVE 12                     TO  I-LINEA
               GO TO 040-CODIGO-CONCEPTO
           ELSE
           IF  I-LINEA                      <  12
               SUBTRACT 2                FROM  K
               SUBTRACT 1                FROM  K-LINEA
               SUBTRACT 1                FROM  M
               GO TO 065-DISPLAY-DATOS
           ELSE
               GO TO 040-CODIGO-CONCEPTO.

           IF  CDN
           AND J                        NOT >  W-TAMANO-NOVEDADES
               ADD  1                      TO  J
               ADD  1                      TO  I-LINEA
           IF  I-LINEA                      >  21
               SUBTRACT 1                FROM  K-LINEA
               ADD  1                      TO  M
               GO TO 065-DISPLAY-DATOS
           ELSE
               MOVE 1                      TO  SW-PRIMERO

               IF  CONTRATO-SW3                    >  1
                   AND CONTRATO-SW3                    <  6
                   MOVE 1                      TO  CONTRATO-SW3
                   REWRITE REC-CONTRATO
                   GO TO 040-CODIGO-CONCEPTO
               ELSE
                   GO TO 040-CODIGO-CONCEPTO.

           IF  T-NOV-CONCEPTO (J)       =  ZEROS
               DISPLAY " CONCEPTO EN CEROS "        LINE 24 POSITION  1
                                                    REVERSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  F9
           IF  W-OPCION                     =  "M"
               PERFORM RETIRE-NOVEDAD THRU
                       SALE-RETIRE-NOVEDAD
               SUBTRACT 1                FROM  K
               SUBTRACT 1                FROM  K-LINEA
               GO TO 060-LIQUIDA-NOVEDAD
           ELSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  CONTRATO-SW3                    >  1
           AND CONTRATO-SW3                    <  6
               MOVE 1                      TO  CONTRATO-SW3
               REWRITE REC-CONTRATO.
               MOVE 1                      TO  SW-PRIMERO
               MOVE ZEROS                  TO  SW-1
               MOVE 1                      TO  I.
       041-BUSCA-CONCEPTO.
           IF  T-TCN-CODIGO-CONCEPTO (I)    =  T-NOV-CONCEPTO (J)
               NEXT SENTENCE
           ELSE
               ADD  1                      TO  I
           IF  I                            >  W-TAMANO-TABLANOV
               MOVE 1                      TO  SW-1
           ELSE
               GO TO 041-BUSCA-CONCEPTO.
           IF  SW-1                         =  1
               MOVE   ZEROS    TO  W-TCN-CODIGO-CONCEPTO
               DISPLAY " CONCEPTO NO EXISTE " LINE 24 POSITION  1
                                                    REVERSE
               GO TO 040-CODIGO-CONCEPTO.
               DISPLAY T-TCN-DESCRIPCION (I) LINE I-LINEA POSITION 10
                                                    SIZE 29
               MOVE ZEROS                  TO  SW-1
               MOVE 1                      TO  L.
       042-BUSCA-DOBLES.
           IF  L                            =  J
           OR  L                            >  J
               NEXT SENTENCE
           ELSE
           IF  T-NOV-CONCEPTO (L)           =  T-NOV-CONCEPTO (J)
               MOVE 1                      TO  SW-1
           ELSE
               ADD  1                      TO  L
               GO TO 042-BUSCA-DOBLES.
           IF  SW-1                         =  1
               DISPLAY " CONCEPTO DUPLICADO "       LINE 24 POSITION  1
                                                    REVERSE
               GO TO 040-CODIGO-CONCEPTO.
               DISPLAY "                    "       LINE 24 POSITION  1
               MOVE I                      TO  T-NOV-INDICE-TCN  (J)
           IF  T-TCN-PORC-RECARGO    (I)    =  ZEROS
               MOVE 100                    TO  T-TCN-PORC-RECARGO  (I).
           IF  J                            >  W-TAMANO-NOVEDADES
               MOVE J                      TO  W-TAMANO-NOVEDADES
               MOVE ZEROS                  TO  T-NOV-HORAS       (J)
                                               T-NOV-DIAS        (J)
                                               T-NOV-VLR         (J)
                                               T-NOV-FLAG-ACUMULADOS (J)
                                               T-NOV-ORIGEN-NOVEDAD (J)
               MOVE "E"                    TO  T-NOV-FLAG-ORIGEN (J).
           IF  F1
           OR  F3
               GO TO 080-GRABACION.
       043-PREPARA-CONCEPTO.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION  1
           IF  T-TCN-VALOR-NOVEDAD    (I)   =  "H"
               GO TO 045-CANTIDAD-HORAS
           ELSE
           IF  T-TCN-VALOR-NOVEDAD    (I)   =  "D"
               GO TO 050-CANTIDAD-DIAS
           ELSE
           IF  T-TCN-VALOR-NOVEDAD    (I)   =  "P"
               GO TO 055-VALOR.
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "T"
           OR  T-TCN-ORIGEN-NOVEDAD   (I)   =  "A"
               GO TO 055-VALOR-TABLA.
           IF  T-TCN-VALOR-NOVEDAD    (I)   =  "%"
               COMPUTE T-NOV-VLR      (L)   = (CONTRATO-SALARIO
                                            *  T-TCN-PORC-RECARGO  (I)
                                            /  100)
                                            +  0.50
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P         LINE I-LINEA POSITION 57
               GO TO 060-LIQUIDA-NOVEDAD
           ELSE
               DISPLAY WI-SZ8P         LINE I-LINEA POSITION 69
               GO TO 060-LIQUIDA-NOVEDAD.
               DISPLAY " ERROR DE LOGICA CONCEPTO " LINE 24 POSITION  1
                                                    REVERSE
               GO TO 040-CODIGO-CONCEPTO.
       045-CANTIDAD-HORAS.
           PERFORM I-MENSAJE-HORAS THRU F-MENSAJE-HORAS.
           MOVE  ZEROS            TO  WK-EXCEPTION
           MOVE  T-NOV-HORAS (J)  TO  NOV-HORAS
           MOVE  T-NOV-HORAS (J)  TO  WI-SZ3P2
           DISPLAY  WI-SZ3P2  LINE  I-LINEA  POSITION 41
           ACCEPT  T-NOV-HORAS (J)  LINE  I-LINEA  POSITION 41
                                    CONVERT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY  SPACES SIZE 80 LINE 25 POSITION 01

           MOVE  T-NOV-HORAS (J)  TO  WI-SZ3P2
           DISPLAY  WI-SZ3P2  LINE I-LINEA  POSITION 41
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           MOVE ZEROS TO W-SW-VALSEX
           PERFORM I-VALIDE-SISTEMA-HORA THRU F-VALIDE-SISTEMA-HORA.
           IF W-SW-VALSEX = 1
            GO TO 045-CANTIDAD-HORAS.
           DISPLAY  SPACES SIZE 80 LINE 24 POSITION 01

           IF  T-NOV-HORAS (J)  =  ZEROS
            MOVE  NOV-HORAS  TO  T-NOV-HORAS (J).
           MOVE  T-NOV-HORAS (J)  TO  WI-SZ3P2
           DISPLAY  WI-SZ3P2  LINE I-LINEA POSITION 41
           IF  CUP  GO TO 040-CODIGO-CONCEPTO.
           IF  ERR  OR  T-NOV-HORAS (J) = ZEROS
            DISPLAY " HORAS ERRADAS " LINE 24 POSITION  1 REVERSE
            GO TO 045-CANTIDAD-HORAS.
           MOVE  T-NOV-HORAS (J)  TO  W-NUMERO-DE-HORAS
           IF WIN-SISTEMA-HORAS = "S"
            MOVE ZEROS TO W-SW-VALSEX
            PERFORM I-VALIDA-SEXAGECIMAL THRU F-VALIDA-SEXAGECIMAL
            IF W-SW-VALSEX = 1
             GO TO 045-CANTIDAD-HORAS.
           MOVE "E"              TO  T-NOV-FLAG-ORIGEN (J).
           MOVE T-NOV-HORAS (J)  TO  WI-SZ3P2
           DISPLAY  WI-SZ3P2  LINE  I-LINEA POSITION 41

           MOVE T-TCN-PORC-RECARGO (I)  TO  W-AUX-PORC-RECARGO.
           PERFORM I-LIQUIDACION-HORAS THRU F-LIQUIDACION-HORAS.
           MOVE W-AUX-NOV-VLR TO T-NOV-VLR (J).

           MOVE  T-NOV-VLR (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD (I)  =  "P"  OR
               T-TCN-TIPO-NOVEDAD (I)  =  "B"
            DISPLAY WI-SZ8P LINE I-LINEA POSITION 57
            DISPLAY WK-ESPACIOS LINE I-LINEA POSITION 69 SIZE 10
           ELSE
            DISPLAY WK-ESPACIOS LINE I-LINEA POSITION 57 SIZE 10
            DISPLAY WI-SZ8P LINE I-LINEA POSITION 69.
           GO TO 060-LIQUIDA-NOVEDAD.
       050-CANTIDAD-DIAS.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-DIAS        (J)  TO  NOV-DIAS
               MOVE T-NOV-DIAS        (J)  TO  WI-SZ3
               DISPLAY WI-SZ3         LINE I-LINEA POSITION 51
               ACCEPT T-NOV-DIAS (J)  LINE I-LINEA POSITION 51
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               MOVE T-NOV-DIAS        (J)  TO  WI-SZ3
               DISPLAY WI-SZ3         LINE I-LINEA POSITION 51
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION  1
           IF  T-NOV-DIAS             (J)   =  ZEROS
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "I"
               MOVE W-TPN-SEMANAS-RFTE TO  T-NOV-DIAS          (J)
           ELSE
               MOVE NOV-DIAS               TO  T-NOV-DIAS          (J).
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "I"
           IF  CONTRATO-FECING                 <  W-TPN-FECHA-DESDE
           AND CONTRATO-SW3                    <  1
               NEXT SENTENCE
           ELSE
           IF  T-NOV-DIAS             (J)   >  W-TPN-SEMANAS-RFTE
               MOVE W-TPN-SEMANAS-RFTE TO  T-NOV-DIAS          (J).
               MOVE T-NOV-DIAS         (J)  TO  WI-SZ3
               DISPLAY WI-SZ3          LINE I-LINEA POSITION 51
           IF  CUP
               GO TO 040-CODIGO-CONCEPTO.
           IF  ERR
           OR  T-NOV-DIAS              (J)  =  ZEROS
               DISPLAY " DIAS ERRADOS " LINE 24 POSITION  1 REVERSE
               GO TO 050-CANTIDAD-DIAS.
           IF  T-NOV-DIAS              (J)   =  NOV-DIAS
               NEXT SENTENCE
           ELSE
               MOVE "E"               TO  T-NOV-FLAG-ORIGEN   (J).
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "I"
               COMPUTE T-NOV-VLR      (J)   =  ZEROS
           ELSE
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "S"
               COMPUTE T-NOV-VLR      (J)   =  T-NOV-DIAS          (J)
                                            * (WIN-SUBTR-ACT
                                            /  30)
                                            *  T-TCN-PORC-RECARGO  (I)
                                            /  100
           ELSE
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "I"
               NEXT SENTENCE
           ELSE
               COMPUTE T-NOV-VLR      (J)   =  T-NOV-DIAS          (J)
                                            *  W-VALOR-DIA
                                            *  T-TCN-PORC-RECARGO  (I)
                                            /  100.
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 57
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 69 SIZE 10
           ELSE
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 57 SIZE 10
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 69.
               GO TO 060-LIQUIDA-NOVEDAD.
       055-VALOR.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-VLR         (J)  TO  NOV-VLR
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 57
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 69 SIZE 10
           ELSE
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 57 SIZE 10
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 69.
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               ACCEPT T-NOV-VLR       (J) LINE I-LINEA POSITION 57
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION
           ELSE
               ACCEPT T-NOV-VLR       (J) LINE I-LINEA POSITION 69
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 57
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 69 SIZE 10
           ELSE
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 57 SIZE 10
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 69.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION  1
           IF  T-NOV-VLR              (J)   =  ZEROS
               MOVE NOV-VLR           TO  T-NOV-VLR           (J).
           IF  CUP
               GO TO 040-CODIGO-CONCEPTO.
           IF  ERR
           OR  T-NOV-VLR              (J)   =  ZEROS
               DISPLAY " VALOR ERRADO " LINE 24 POSITION  1 REVERSE
               GO TO 055-VALOR.
           IF  T-NOV-VLR              (J)   =  NOV-VLR
               NEXT SENTENCE
           ELSE
               MOVE "E"               TO  T-NOV-FLAG-ORIGEN    (L).
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 57
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 69 SIZE 10
           ELSE
               DISPLAY WK-ESPACIOS    LINE I-LINEA POSITION 57 SIZE 10
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 69.
               GO TO 060-LIQUIDA-NOVEDAD.
       055-VALOR-TABLA.
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "T"
               MOVE ZEROS             TO  T-NOV-VLR         (J).
       060-LIQUIDA-NOVEDAD.
               PERFORM LIQUIDA-NOVEDADES THRU
                       SALE-LIQUIDA-NOVEDADES.
       060-SALE-LIQUIDA-NOVEDAD.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION  1
           IF  SW-2                   >  ZEROS
               DISPLAY " HAY   DESCUENTO(S) NO PROCESADO(S) "
                                      LINE 24 POSITION  1 REVERSE
               DISPLAY SW-2           LINE 24 POSITION  6 REVERSE.
       065-DISPLAY-DATOS.
               MOVE 12                     TO  I-LINEA
               MOVE M                      TO  L.
       065-SIGUE-DISPLAY-DATOS.
               ADD  1                      TO  L
           IF  L                            >  W-TAMANO-NOVEDADES
               NEXT SENTENCE
           ELSE
               PERFORM MUESTRA-NOVEDADES THRU
                       SALE-MUESTRA-NOVEDADES
               ADD  1                      TO  I-LINEA
           IF  I-LINEA                      >  21
           IF  W-OPCION                     =  "A"
               SUBTRACT 1                FROM  K-LINEA
               ADD  1                      TO  M
               GO TO 065-DISPLAY-DATOS
           ELSE
               NEXT SENTENCE
           ELSE
               GO TO 065-SIGUE-DISPLAY-DATOS.
       065-SALE-DISPLAY-DATOS.
               PERFORM BORRA-PANTALLA-NOVEDADES THRU
                       SALE-BORRA-PANTALLA-NOVEDADES
                       VARYING J-LINEA   FROM  I-LINEA  BY  1
                       UNTIL   J-LINEA      >  21
               MOVE K                      TO  J
               MOVE K-LINEA                TO  I-LINEA
               ADD  1                      TO  J
               ADD  1                      TO  I-LINEA
           IF  J                            <  1
               MOVE 1                      TO  J.
           IF  I-LINEA                      <  12
               MOVE 12                     TO  I-LINEA.
           IF  I-LINEA                      >  21
               SUBTRACT 1                FROM  K-LINEA
               ADD  1                      TO  M
               GO TO 065-DISPLAY-DATOS.
               COMPUTE W-TOTAL-A-PAGAR      =  W-TOTAL-DEVENGADOS
                                            -  W-TOTAL-DESCUENTOS
               MOVE W-TOTAL-DEVENGADOS     TO  WI-SZ11P
               DISPLAY WI-SZ11P            LINE  6 POSITION 65
               MOVE W-TOTAL-DESCUENTOS     TO  WI-SZ11P
               DISPLAY WI-SZ11P            LINE  7 POSITION 65
               MOVE W-TOTAL-A-PAGAR        TO  WI-SZ11P
               DISPLAY WI-SZ11P            LINE  8 POSITION 65
           IF  W-OPCION                     =  "R"
               GO TO 105-ELIMINAR.
               GO TO 040-CODIGO-CONCEPTO.
       070-CODIGO-CONCEPTO.
           IF  F1
               GO TO 025-OPCION
           ELSE
               GO TO 030-CEDULA.
       080-GRABACION.
               PERFORM GRABA-NOVEDADES THRU
                       SALE-GRABA-NOVEDADES
                       VARYING L         FROM  1  BY  1
                       UNTIL   L            >  W-TAMANO-NOVEDADES
           IF  F1
               GO TO 025-OPCION
           ELSE
               GO TO 030-CEDULA.
       105-ELIMINAR.
               DISPLAY " PRESIONE <R> RETIRAR "     LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1
               GO TO 030-CEDULA.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "R"
               GO TO 105-ELIMINAR.
               PERFORM RETIRA-NOVEDADES THRU
                       SALE-RETIRA-NOVEDADES
               GO TO 030-CEDULA.
       115-CONSULTAR.
               DISPLAY " PRESIONE <> CONSULTAR "   LINE 24 POSITION  1
                                                    REVERSE
               DISPLAY " <F1> OPCIONES "            LINE 24 POSITION 27
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
           IF  CDN
               READ CONTRATO NEXT RECORD WITH NO LOCK AT  END
               GO TO 115-FIN-CONSULTAR.
           IF  CDN
               GO TO 030-SIGUE-CEDULA.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1
               GO TO 115-SALE-CONSULTAR
           ELSE
               GO TO 115-CONSULTAR.
       115-FIN-CONSULTAR.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               DISPLAY " NO HAY MAS REGISTROS "     LINE 24 POSITION  1
                                                    REVERSE
               DISPLAY " <F1> OPCIONES "            LINE 24 POSITION 25
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       115-SALE-CONSULTAR.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               GO TO 025-OPCION.

       I-FIN-MODE.
       120-FIN.
               CLOSE CONCPTOS
                     TABLAEMP
                     CONTRATO
                     HOJAVIDA
                     NOMINOVE.
       120-FIN-FIN.
               EXIT PROGRAM.
       120-FT. STOP RUN.
       F-FIN-MODE.

       110-BORRAR.
               DISPLAY W-SPACES          LINE I-LINEA POSITION  2
               DISPLAY WK-ESPACIOS       LINE 24 POSITION  1.
       OPCIONES-NOVEDAD.
               DISPLAY WK-ESPACIOS       LINE 25 POSITION  1
               DISPLAY " F1 "            LINE 25 POSITION  3 REVERSE
               DISPLAY "Opciones"        LINE 25 POSITION  8
               DISPLAY " F2 "            LINE 25 POSITION 19 REVERSE
               DISPLAY "Terminar"        LINE 25 POSITION 24
               DISPLAY " F3 "            LINE 25 POSITION 35 REVERSE
               DISPLAY "Grabar"          LINE 25 POSITION 40
           IF  W-OPCION                  =  "M"
               DISPLAY " F9 "            LINE 25 POSITION 51 REVERSE
               DISPLAY "Borrar Novedad"  LINE 25 POSITION 56.
       SALE-OPCIONES-NOVEDAD.
               EXIT.
       OPCIONES-CEDULA.
               DISPLAY WK-ESPACIOS       LINE 25 POSITION  1
               DISPLAY " F1 "            LINE 25 POSITION  3 REVERSE
               DISPLAY "Opciones"        LINE 25 POSITION  8
               DISPLAY " F2 "            LINE 25 POSITION 19 REVERSE
               DISPLAY "Terminar"        LINE 25 POSITION 24
               DISPLAY "  "             LINE 25 POSITION 51 REVERSE
               DISPLAY "Siguiente Cedula" LINE 25 POSITION 55.
       SALE-OPCIONES-CEDULA.
               EXIT.

       I-LABEL-ARCHIVOS.
               COMPUTE CON-PERIODO          =  WIN-NOMINA
                                            +  1
               PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
               PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
               PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
       F-LABEL-ARCHIVOS.

       I-STATUS-NOMINOVE.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT NOMINOVE
               MOVE 1                      TO  SW-ABRE
           IF  NOMINOVE-STATUS              =  "94"
               OPEN OUTPUT NOMINOVE
           ELSE
           IF  NOMINOVE-STATUS              >  "00"
               PERFORM RUTINA-ERROR-NOMINOVE.
               CLOSE NOMINOVE.
       F-STATUS-NOMINOVE.

       I-ABRE-ARCHIVOS.
               OPEN INPUT CONCPTOS
                          HOJAVIDA
               OPEN I-O   TABLAEMP
                          CONTRATO
                          NOMINOVE.
       F-ABRE-ARCHIVOS.

       I-CEROS-NOMINOVE.
               MOVE ZEROS                  TO  NOV-EMPRESA
                                               NOV-CODIEMP
                                               NOV-CONCEPTO
                                               NOV-EMPRESA2
                                               NOV-CONCEPTO2
                                               NOV-CODIEMP2
                                               NOV-HORAS
                                               NOV-DIAS
                                               NOV-VLR
                                               NOV-FLAG-ORIGEN
                                               NOV-FLAG-ACUMULADOS
                                               NOV-ORIGEN-NOVEDAD.
       F-CEROS-NOMINOVE.

       I-PANTALLA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
           DISPLAY "º"                          LINE  2 POSITION  1
           DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
           DISPLAY "WINNER FINANCIAL  º"        LINE  2 POSITION 62
           DISPLAY "º Date :"                   LINE  3 POSITION  1
           DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
           DISPLAY "CAPTURA NOVEDADES MANUALES" LINE  3 POSITION 32
           DISPLAY "Time:"                      LINE  3 POSITION 68
           DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
           DISPLAY "º"                          LINE  3 POSITION 80
           DISPLAY "º Serie #:"                 LINE  4 POSITION  1
           DISPLAY CON-PERIODO                  LINE  4 POSITION 12
           DISPLAY "OPCION:"                    LINE  4 POSITION 28
           DISPLAY "º"                          LINE  4 POSITION 80
           DISPLAY WK-RAYAS                     LINE  5 POSITION  1
           DISPLAY "Ë"                          LINE  5 POSITION 24
           DISPLAY "Ë"                          LINE  5 POSITION 52
           DISPLAY "º ID No..:"                 LINE  6 POSITION  1
           DISPLAY "º"                          LINE  6 POSITION 24
           DISPLAY "º Devengados:"              LINE  6 POSITION 52
           DISPLAY "º"                          LINE  6 POSITION 80
           DISPLAY "º Codigo.:"                 LINE  7 POSITION 1
           DISPLAY "º Fecha Ingreso:"           LINE  7 POSITION 24
           DISPLAY "º Descuentos:"              LINE  7 POSITION 52
           DISPLAY "º"                          LINE  7 POSITION 80
           DISPLAY "º Sueldo.:"                 LINE  8 POSITION 1
           DISPLAY "º Ciclo de pago:"           LINE  8 POSITION 24
           DISPLAY "º Neto pago :"              LINE  8 POSITION 52
           DISPLAY "º"                          LINE  8 POSITION 80
           PERFORM I-DISPLAY-BODY THRU F-DISPLAY-BODY.
           DISPLAY "Ê"                          LINE  9 POSITION 24.
           DISPLAY "Ê"                          LINE  9 POSITION 52.
           PERFORM I-DECODI-TIPOPERI THRU F-DECODI-TIPOPERI.
       F-PANTALLA. EXIT.
      *
       I-DECODI-TIPOPERI.
           IF WIN-INDICATIVO-PERIODO = "Q"
            DISPLAY "Quincenal " LINE 08 POSITION 40 .
           IF WIN-INDICATIVO-PERIODO = "S"
            DISPLAY "SemanaL   " LINE 08 POSITION 40 .
           IF WIN-INDICATIVO-PERIODO = "D"
            DISPLAY "DecadaL   " LINE 08 POSITION 40 .
           IF WIN-INDICATIVO-PERIODO = "M"
            DISPLAY "Mensual   " LINE 07 POSITION 40 .
           IF WIN-INDICATIVO-PERIODO = "C"
            DISPLAY "Catorcenal" LINE 07 POSITION 40 .
       F-DECODI-TIPOPERI. EXIT.
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\PERIODOS.PRO".
           COPY "..\PRO\LIQUINOV.PRO".
           COPY "..\PRO\LIQUIACU.PRO".
           COPY "..\PRO\CALCDIAS.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\DISCAPDA.PRO".
           COPY "..\PRO\MASCFECH.PRO".
           COPY "..\PRO\USUAPERS.PRO".
           COPY "..\PRO\VALISEXA.PRO".
           COPY "..\PRO\VLRHORAS.PRO".
           COPY "..\PRO\MENSHORA.PRO".
           COPY "..\PRO\VALOHORA.PRO".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\NOMINOVE.PLB".
           COPY "..\PLB\NOMINOVP.PLB".
      ******************************************************************
