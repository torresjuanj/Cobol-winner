       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG120.
      ******************************************************************
      *  MANTENIMIENTO MAESTRO DE CONCPTOS          MAY 27 /2002
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\AUDICAMP.DYC".
           COPY "..\DYC\TBFOPRES.DYC".
           COPY "..\DYC\PRESTAMO.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\CNCATCTA.FD".
           COPY "..\FD\AUDICAMP.FD".
           COPY "..\FD\TBFOPRES.FD".
           COPY "..\FD\PRESTAMO.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA          PIC X(7)  VALUE "PROG120".
       77  ID-PRG              PIC X(7)  VALUE "PROG120".
       77  WK-NOM    PIC X(20).
       77  WK-FONCESAN         PIC 9(5)  VALUE ZEROS.
       77  WK-CHEQUEORD        PIC 9(5)  VALUE ZEROS.
       77  WK-RECIBOCAJ        PIC 9(6)  VALUE ZEROS.
       77  WK-CHEQUEPOR        PIC 9(5)  VALUE ZEROS.
       77  WK-SALARIO-ANT      PIC 9(8)  VALUE ZEROS.
       77  WK-SUBSIDIO-ANT     PIC 9(8)  VALUE ZEROS.
       77  W-VN-LIN  PIC  9(02)  VALUE  ZEROS.
       77  W-VN-COL  PIC  9(02)  VALUE  ZEROS.
       77  W-TN-LIN  PIC  9(02)  VALUE  ZEROS.
       77  W-TN-COL  PIC  9(02)  VALUE  ZEROS.
       01      WK-AREA-TABLAS.
        03     WK-CODIGO-TABLA           PIC X(3)       VALUE "050".
        03     WK-NOMBRE-TABLA           PIC X(30)      VALUE
               "     CONCEPTOS DE NOMINA      ".
       01 W-AUDI-CEDULA.
        03 W-AUDI-CEDULA-D PIC 9(09) VALUE ZEROS.
        03 W-AUDI-EMPRES-D PIC 9(03) VALUE ZEROS.
      *
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\CNCATCTA.LBL".
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\AUDICAMP.LBL".
           COPY "..\LBL\TBFOPRES.LBL".
           COPY "..\LBL\PRESTAMO.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\CONCPTOS.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\COMANDOS.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\CONCPTOS.DCL".
           COPY "..\DCL\CNCATCTA.DCL".
           COPY "..\DCL\AUDICAMP.DCL".
           COPY "..\DCL\TBFOPRES.DCL".
           COPY "..\DCL\PRESTAMO.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE  "  **CONCEPTOS**  "  TO TIT-APLICACION.
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE.
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS.
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS.
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           ACCEPT WK-FECHA-HOY       FROM  DATE
           IF WK-ANO >= 0  ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-DIA               TO  WK-MASCARA-DIA
           IF  WK-MES                <  01
           OR  WK-MES                >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
           MOVE WK-ANO                 TO  WK-MASCARA-ANO
           MOVE "ฤ"                    TO  WK-RAYA-1
           MOVE "ฤ"                    TO  WK-RAYA-2
           MOVE WK-HORAS               TO  WK-MASCARA-HORAS
           MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
           MOVE ":"                    TO  WK-PUNTOS
           MOVE CON-COD-EMPRESA        TO  WIN-CODIGO-EMPRESA
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P.
           MOVE 16                     TO  POS-DOWN.
           PERFORM I-STATUS-TODOS      THRU F-STATUS-TODOS.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-STATUS-TODOS.
           PERFORM I-STATUS-CONCPTOS THRU F-STATUS-CONCPTOS
           PERFORM I-STATUS-TBFOPRES THRU F-STATUS-TBFOPRES
           PERFORM I-STATUS-PRESTAMO THRU F-STATUS-PRESTAMO
           PERFORM I-STATUS-CNCATCTA THRU F-STATUS-CNCATCTA.
       F-STATUS-TODOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           OPEN I-O   CONCPTOS
           OPEN INPUT CNCATCTA TBFOPRES PRESTAMO EMPRESAS.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA  TO  I-LINEA
           MOVE 999      TO  WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS    TO  FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 999             TO   WK-EXCEPTION
              PERFORM I-WINDOW-FIN THRU F-WINDOW-FIN
              PERFORM I-SALIR      THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM  I-FIN-MODE THRU F-FIN-MODE
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
           IF  W-OPCION  =  "A"   AND PROSS-AKK = "S"
             PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
             PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC
             PERFORM I-ADICIONA     THRU F-ADICIONA UNTIL ESC OR CUP
             MOVE ZEROS             TO   WK-EXCEPTION
           ELSE
             IF  W-OPCION  =  "C"  AND  PROSS-CKK = "S"
               PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
               PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC
               PERFORM I-CONSULTA     THRU F-CONSULTA UNTIL ESC OR CUP
               MOVE ZEROS             TO   WK-EXCEPTION
             ELSE
               IF  W-OPCION  =  "M"  AND   PROSS-MKK = "S"
                 PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                 PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC
                 PERFORM I-MODIFICA     THRU F-MODIFICA UNTIL ESC OR CUP
                 MOVE ZEROS             TO    WK-EXCEPTION
               ELSE
                 IF  W-OPCION  =  "R"  AND  PROSS-RKK = "S"
                   PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                   PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC
                   PERFORM I-RETIRA       THRU F-RETIRA UNTIL ESC OR CUP
                   MOVE ZEROS             TO    WK-EXCEPTION.
       F-MENU-OPCIONES. EXIT.
      *
       I-LLAMA-WINDOW.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           PERFORM I-WINDOW-NOM THRU F-WINDOW-NOM.
       F-LLAMA-WINDOW.
      *
       I-DISPLAY-OPC.
           IF  W-OPCION                     =  "A"
               DISPLAY "Adicion"            LINE  3 POSITION 38
                                                    REVERSE
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY "Consulta"            LINE  3 POSITION 38
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "M"
               DISPLAY "Modificacion"    LINE  3 POSITION 38
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "R"
               DISPLAY "Retiro"            LINE  3 POSITION 38
                                                    REVERSE.
       F-DISPLAY-OPC. EXIT.
      *
       I-ADICIONA.
           MOVE ZEROS TO WK-TOTAL-CONCEPTO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO      THRU  F-CODIGO.
           IF   CUP OR ESC       GO    F-ADICIONA.
           PERFORM I-LEE-CONCPTOS THRU F-LEE-CONCPTOS.
           IF  SW-1  =  ZEROS
             PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
             PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
             PERFORM I-EXISTE         THRU F-EXISTE
             MOVE CONCPTOS-CODIGO-CONCEPTO  TO   WK-CODIGO-CONCEPTO
             MOVE SPACE              TO   REC-CONCPTOS
             MOVE WK-CODIGO-CONCEPTO TO CONCPTOS-CODIGO-CONCEPTO
             GO I-ADICIONA
           ELSE
               MOVE CONCPTOS-CODIGO-CONCEPTO  TO   WK-CODIGO-CONCEPTO
               MOVE SPACE                  TO   REC-CONCPTOS
               MOVE WK-CODIGO-CONCEPTO  TO   CONCPTOS-CODIGO-CONCEPTO
               PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
               PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP OR ESC
                  PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW
                  GO I-ADICIONA
               ELSE
                  PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-ADICIONA. EXIT.
      *
       I-CONSULTA.
           MOVE ZEROS TO SW-OK.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          OR
                   ESC          GO   F-CONSULTA.
           PERFORM I-LEE-CONCPTOS THRU F-LEE-CONCPTOS.
           IF  SW-1  =  1
               PERFORM I-NEXISTE        THRU F-NEXISTE
               MOVE CONCPTOS-CODIGO-CONCEPTO  TO   WK-CODIGO-CONCEPTO
               MOVE SPACE               TO   REC-CONCPTOS
               MOVE WK-CODIGO-CONCEPTO  TO   CONCPTOS-CODIGO-CONCEPTO
               GO I-CONSULTA
           ELSE
               PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
               PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
               PERFORM I-CONSULTAR      THRU F-CONSULTAR
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-CONSULTA.
      *
       I-MODIFICA.
           MOVE ZEROS TO WK-TOTAL-CONCEPTO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO       THRU  F-CODIGO.
           IF  CUP OR ESC  GO  F-MODIFICA.
           PERFORM I-LEE-CONCPTOS THRU F-LEE-CONCPTOS.
           IF SW-1  =  1
             PERFORM I-NEXISTE        THRU F-NEXISTE
             MOVE CONCPTOS-CODIGO-CONCEPTO  TO   WK-CODIGO-CONCEPTO
             MOVE SPACE                  TO   REC-CONCPTOS
             MOVE WK-CODIGO-CONCEPTO     TO   CONCPTOS-CODIGO-CONCEPTO
             GO I-MODIFICA
           ELSE
             PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
             PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
             PERFORM I-GUARDA-INFACT  THRU F-GUARDA-INFACT
             PERFORM I-DATOS          THRU F-DATOS
             IF CUP OR ESC
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW
               GO I-MODIFICA
             ELSE
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-MODIFICA. EXIT.
      *
       I-RETIRA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          OR
                   ESC          GO   F-RETIRA.
           PERFORM I-LEE-CONCPTOS THRU F-LEE-CONCPTOS.
           IF  SW-1  =  1
               PERFORM I-NEXISTE        THRU F-NEXISTE
               MOVE CONCPTOS-CODIGO-CONCEPTO  TO   WK-CODIGO-CONCEPTO
               MOVE SPACE               TO   REC-CONCPTOS
               MOVE WK-CODIGO-CONCEPTO  TO   CONCPTOS-CODIGO-CONCEPTO
               GO I-RETIRA
           ELSE
               PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
               PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
               PERFORM I-ELIMINAR       THRU F-ELIMINAR
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-RETIRA. EXIT.
      *****************************************************************
      *    CAPTURA CODIGO  DE  EMPRESA                                *
      *****************************************************************
       I-CODIGO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO SW-1 WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           ACCEPT CONCPTOS-CODIGO-CONCEPTO  LINE 2 POSITION 15
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           DISPLAY SPACES     LINE 25 POSITION 01
           MOVE    CONCPTOS-CODIGO-CONCEPTO TO WI-Z6 WK-CODIGO-CONCEPTO
           DISPLAY WI-Z6  LINE 2 POSITION 15.
           IF   CUP OR ESC    GO F-CODIGO
           ELSE
             IF  ERR
               MOVE ZEROS TO WK-EX
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
               MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
               DISPLAY " Codigo errado " LINE 25 POSITION 01
                                      CONTROL SG-CONTROL-STRING
               GO I-CODIGO
             ELSE
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE
               ELSE
                 IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
                   GO I-CODIGO
                 ELSE
                   IF CONCPTOS-CODIGO-CONCEPTO = ZEROS
                     MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
                     DISPLAY " Codigo en ceros " LINE 25 POSITION 01
                                            CONTROL SG-CONTROL-STRING
                     GO I-CODIGO.
       F-CODIGO. EXIT.
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS NOMINA/FACTURACION/CONTABLES     *
      *****************************************************************
       I-DATOS.
           PERFORM I-DATOS-CAB       THRU  F-DATOS-CAB.
           IF      ESC OR CUP        GO    F-DATOS.
           IF      F4                GO    DAT-1.
           IF      F5                GO    DAT-2.
           IF      F6                GO    DAT-3.
         DAT-1.
           PERFORM I-WINDOW-NOM      THRU  F-WINDOW-NOM
           PERFORM I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
           PERFORM I-DATOS-NOMINA    THRU  F-DATOS-NOMINA.
           IF      ESC               GO    F-DATOS.
           IF      CUP               GO    I-DATOS.
           IF      F6                GO    DAT-3.
         DAT-2.
           PERFORM I-WINDOW-CAB   THRU F-WINDOW-CAB
           PERFORM I-MUESTRA-CAB  THRU F-MUESTRA-CAB
           PERFORM I-WINDOW-CON      THRU  F-WINDOW-CON.
           PERFORM I-MUESTRA-CONTABLE   THRU  F-MUESTRA-CONTABLE
           PERFORM I-DATOS-CONTABLE     THRU  F-DATOS-CONTABLE.
           IF      ESC               GO    F-DATOS.
           IF      CUP               GO    DAT-1.
           IF      F4                GO    DAT-1.
         DAT-3.
           PERFORM I-WINDOW-CAB   THRU F-WINDOW-CAB
           PERFORM I-MUESTRA-CAB  THRU F-MUESTRA-CAB
           PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC.
           PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
           PERFORM I-DATOS-FACTURA   THRU  F-DATOS-FACTURA.
           IF      ESC               GO    F-DATOS.
           IF      CUP               GO    DAT-2.
           IF      F4                GO    DAT-1.
           IF      F5                GO    DAT-2.
         DAT-4.
           MOVE    ZEROS       TO   WK-EXCEPTION
           MOVE    SPACES      TO   W-TRUCO
           IF      W-OPCION   =   "A"
             PERFORM I-GRABACION THRU F-GRABACION
               UNTIL  W-TRUCO = "A" OR F1 OR CUP
           ELSE
             PERFORM I-REGRABACION THRU F-REGRABACION
               UNTIL  W-TRUCO = "M" OR F1 OR CUP.
           IF      CUP              GO   DAT-3.
       F-DATOS.
      *
       I-GRABACION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <A> ADICIONAR "
             LINE 25 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
             LINE 25 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC OR F1  GO F-GRABACION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "A"    GO TO I-GRABACION.
           MOVE CON-ANO        TO  WIN-FECHA-ANO.
           MOVE ZEROS          TO  WK-FECHA-DESDE.
           MOVE CON-ANO        TO  WK-ANO-DESDE.
           MOVE 01             TO  WK-MES-DESDE WK-DIA-DESDE.
           MOVE WK-FECHA-DESDE TO  WIN-FECSAL-ACT WIN-FECSUB-ACT.
           MOVE WR-SUBTR-ANT   TO  WIN-SUBTR-ANT.
           PERFORM I-AUDITORIA-CONCPTOS THRU F-AUDITORIA-CONCPTOS.
           WRITE REC-CONCPTOS.
       F-GRABACION. EXIT.
      *
       I-REGRABACION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <M> MODIFICAR "
           LINE 25 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
           LINE 25 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC OR F1  GO F-REGRABACION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "M"   GO I-REGRABACION.
           PERFORM I-AUDITORIA-CONCPTOS THRU F-AUDITORIA-CONCPTOS.
           REWRITE REC-CONCPTOS.
       F-REGRABACION. EXIT.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <R> RETIRAR "
           LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
           LINE 24 POSITION 25 REVERSE
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1
               GO F-ELIMINAR.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4
               PERFORM I-WINDOW-NOM       THRU F-WINDOW-NOM
               PERFORM I-MUESTRA-NOMINA   THRU F-MUESTRA-NOMINA.
           IF  F5
               PERFORM I-WINDOW-FAC       THRU F-WINDOW-FAC
               PERFORM I-MUESTRA-FACTURA  THRU F-MUESTRA-FACTURA.
           IF  F6
               PERFORM I-WINDOW-CON       THRU F-WINDOW-CON
               PERFORM I-MUESTRA-CONTABLE    THRU F-MUESTRA-CONTABLE.
           IF SF9
               DELETE CONCPTOS
               DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
               "Registro de tabla CONCPTOS ha sido eliminado..."
               LINE 25 POSITION 01 REVERSE BLINK
               GO TO F-ELIMINAR.
           IF  W-TRUCO                  NOT =  "R"
               GO I-ELIMINAR.
       F-ELIMINAR. EXIT.
      *
       I-CONSULTAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " Presione <> Consultar "
           LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> OPCIONES "
              LINE 24 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           IF  CDN
               PERFORM I-LEER-CONCPTOS THRU F-LEER-CONCPTOS
               IF SW-OK = 1
                  PERFORM I-FIN-CONSULTAR THRU F-FIN-CONSULTAR
                                          UNTIL F1
                  GO F-CONSULTAR
               ELSE
                  PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
                  PERFORM I-WINDOW-NOM     THRU F-WINDOW-NOM
                  PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
                  GO I-CONSULTAR.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4
               PERFORM I-WINDOW-NOM       THRU F-WINDOW-NOM
               PERFORM I-MUESTRA-NOMINA   THRU F-MUESTRA-NOMINA.
           IF  F5
               PERFORM I-WINDOW-FAC       THRU F-WINDOW-FAC
               PERFORM I-MUESTRA-FACTURA  THRU F-MUESTRA-FACTURA.
           IF  F6
               PERFORM I-WINDOW-CON       THRU F-WINDOW-CON
               PERFORM I-MUESTRA-CONTABLE    THRU F-MUESTRA-CONTABLE.
           IF  F1
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               GO F-CONSULTAR
           ELSE
               GO I-CONSULTAR.
       F-CONSULTAR.
      *
       I-FIN-CONSULTAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           DISPLAY " NO HAY MAS REGISTROS "
             LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> OPCIONES "
           LINE 24 POSITION 25 REVERSE
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           IF  F2   PERFORM I-FIN-MODE THRU F-FIN-MODE .
       F-FIN-CONSULTAR. EXIT.
      *
       I-EXISTE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "Registro ya Existe...de tecla para continuar."
           LINE 25 POSITION 01 REVERSE.
           PERFORM ACEPTA-TRUCO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "REGISTRO NO EXISTE... de tecla para continuar."
           LINE 25 POSITION 01 REVERSE.
           PERFORM ACEPTA-TRUCO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-NEXISTE. EXIT.
      ***********************************************************
      *       LECTURA DE ARCHIVOS                               *
      ***********************************************************
       I-LEE-CONCPTOS.
           MOVE ZEROS                  TO  SW-1
           MOVE WK-CODIGO-CONCEPTO     TO   CONCPTOS-CODIGO-CONCEPTO
           READ CONCPTOS      WITH NO LOCK   INVALID  KEY
                MOVE 1                 TO  SW-1.
       F-LEE-CONCPTOS.
      *
       I-LEER-CONCPTOS.
           READ CONCPTOS NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO SW-OK.
       F-LEER-CONCPTOS. EXIT.

       I-LEE-CNCATCTA.
           MOVE ZEROS TO RUT-OK.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                  MOVE 1 TO RUT-OK.
       F-LEE-CNCATCTA.
      *
       I-WINDOW-CAB.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING.
           DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออป" LINE 01 POSITION 01
           DISPLAY "บId.Concepto.:                       System Winner
      -            "     Fecha..:              บ" LINE 02 POSITION 01
           DISPLAY "บAbreviatura.:
      -            "                           บ" LINE 03 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -     "อออออออออออออออออออออออออออน" LINE 04 POSITION 01
           DISPLAY "บDescripcion.........:
      -     "                           บ" LINE 05 POSITION 01
           DISPLAY "บTipo   Novedad......:                  Se Causa en.
      -     "........:                  บ" LINE 06 POSITION 01
           DISPLAY "บOrigen Novedad......:                  Valor Noveda
      -     "d.......:                  บ" LINE 07 POSITION 01
           DISPLAY "บClase  Novedad......:                  Porcentaje.
      -     "........:                  บ" LINE 08 POSITION 01
           DISPLAY "บImprime Anexos......:
      -     "                           บ" LINE 09 POSITION 01
           DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออผ" LINE 10 POSITION 01.
           DISPLAY WK-MASCARA-FECHA            LINE 02 POSITION 67.
      *    DISPLAY WK-MASCARA-HORA             LINE 03 POSITION 67.
       F-WINDOW-CAB. EXIT.
      *
       I-WINDOW-NOM.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 10 POSITION 01
                                              CONTROL SG-CONTROL-STRING
           DISPLAY "บ                          APLICA COMO PARA BASE PAR
      -     "A                          บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -     "อออออออออออออออออออออออออออน"          LINE 12 POSITION 01
           DISPLAY "บSubsidio Transpte?..:                  PRIMA LEGAL.
      -     ".......?:         %        บ"          LINE 13 POSITION 01
           DISPLAY "บRetencion Fuente?...:         %          Todo lo De
      -     "vengado?:                  บ"          LINE 14 POSITION 01
           DISPLAY "บSeguridad Social?...:         %          Solo Horas
      -     " Extras?:                  บ"          LINE 15 POSITION 01
           DISPLAY "บSindicato?..........:         %          Solo Rec.N
      -     "octurno?:                  บ"          LINE 16 POSITION 01
           DISPLAY "บLiq.Prestac.Sociales:                  VACACIONES..
      -     ".......?:         %        บ"          LINE 17 POSITION 01
           DISPLAY "บNovefijas Sueldo?...:                    Todo lo De
      -     "vengado?:                  บ"          LINE 18 POSITION 01
           DISPLAY "บCESANTIAS?..........:         %          Solo Horas
      -     " Extras?:                  บ"          LINE 19 POSITION 01
           DISPLAY "บ  Todo lo Devengado?:                    Solo Rec.N
      -     "octurno?:                  บ"          LINE 20 POSITION 01
           DISPLAY "บ  Solo Horas Extras?:                  Prom.Vac.Dis
      -     "frutadas:                  บ"          LINE 21 POSITION 01
           DISPLAY "บ  Solo Rec.Nocturno?:                  Afecta Aum.S
      -     "ueldo?..:                  บ" LINE 22 POSITION 01 HIGH
           DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -     "อออออออออออออออออออออออออออผ" LINE 23 POSITION 01.
       F-WINDOW-NOM. EXIT.
      *
       I-WINDOW-FAC.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 10 POSITION 01
                                              CONTROL SG-CONTROL-STRING
           DISPLAY "บ                           APLICA MODULO FACTURACIO
      -     "N                          บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 12 POSITION 01
           DISPLAY "บCalcula Aportes Parafiscales + Servicio Administrac
      -     "ion?....:                  บ"          LINE 13 POSITION 01
           DISPLAY "บCalcula Solo Serv.Administracion SIN aportes parafi
      -     "scales?.:                  บ"          LINE 14 POSITION 01
           DISPLAY "บDeduce Conceptos de Prestaciones Sociales de la fac
      -     "tura?...:                  บ"          LINE 15 POSITION 01
           DISPLAY "บDeduce Otros Descuentos de la Factura?.............
      -     "........:                  บ"          LINE 16 POSITION 01
           DISPLAY "บAplica otra variable para Aportes Parafiscales?....
      -     "........:                  บ"          LINE 17 POSITION 01
           DISPLAY "บQue porcentaje Parafiscal aplicara SOLO a este conc
      -     "cepto?.:                   บ"          LINE 18 POSITION 01
           DISPLAY "บ
      -     "                           บ"          LINE 19 POSITION 01
           DISPLAY "บ
      -     "                           บ"           LINE 21 POSITION 01
           DISPLAY "บ
      -     "                           บ"           LINE 20 POSITION 01
           DISPLAY "บ
      -     "                           บ"  LINE 22 POSITION 01 HIGH
           DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออผ" LINE 23 POSITION 01.
       F-WINDOW-FAC. EXIT.
      *
       I-WINDOW-CON.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 10 POSITION 01
                                              CONTROL SG-CONTROL-STRING
           DISPLAY "บ                          APLICA MODULO CONTABLE
      -     "                           บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน"   LINE 12 POSITION 01
           DISPLAY "บNo.Cuenta Contable Gastos.....:
      -     "                           บ"         LINE 13 POSITION 01
           DISPLAY "บNo.Cuenta Contable Costos.....:
      -     "                           บ"         LINE 14 POSITION 01
           DISPLAY "บContabiliza movto Detalladmte?:
      -     "                           บ"         LINE 15 POSITION 01
           DISPLAY "บ      INFORMES A LA DIAN
      -     "                           บ"         LINE 16 POSITION 01
           DISPLAY "บAnexo Declaracion de Renta?...:
      -     "                           บ"         LINE 17 POSITION 01
           DISPLAY "บAcumula movto en Columna No...:
      -     "                           บ"         LINE 18 POSITION 01
           DISPLAY "บ
      -     "                           บ"         LINE 19 POSITION 01
           DISPLAY "บ       MODULO DE PRESTAMOS
      -     "                           บ"         LINE 20 POSITION 01
           DISPLAY "บCodigo Fondo Empleados/Coopert:
      -     "                           บ"  LINE 21 POSITION 01 HIGH
           DISPLAY "บ
      -     "                           บ"  LINE 22 POSITION 01 HIGH
           DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออผ" LINE 23 POSITION 01.
       F-WINDOW-CON. EXIT.

       I-FIN-MODE.
           CLOSE CONCPTOS CNCATCTA.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\DATOSCAB.PRO".
           COPY "..\PRO\AUDITABG.PRO".
           COPY "..\PRO\AYAUDICO.PRO".
           COPY "..\PRO\DATOSNOM.PRO".
           COPY "..\PRO\DATOSCON.PRO".
           COPY "..\PRO\DATOSFAC.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\COLOR200.PRO".
      *
           COPY "..\STA\CONCPTOS.STA".
           COPY "..\STA\AUDICAMP.STA".
           COPY "..\STA\TBFOPRES.STA".
           COPY "..\STA\PRESTAMO.STA".
           COPY "..\STA\CNCATCTA.STA".
           COPY "..\LEE\CNCATCTA.I00".
      *
      ******************************************************************
