       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG100.
      ******************************************************************
      *    MAESTRO DE EMPRESAS    ABR-27-2002                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\FONCESAN.DYC".
           COPY "..\DYC\CONTRATO.DYC".
           COPY "..\DYC\CJASUBSI.DYC".
           COPY "..\DYC\NOMICIUD.DYC".
           COPY "..\DYC\NOMIBANK.DYC".
           COPY "..\DYC\NOMICORP.DYC".
           COPY "..\DYC\CNCOMPTE.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\AUDICAMP.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\VENDEMPR.DYC".
           COPY "..\DYC\MODACONT.DYC".
           COPY "..\DYC\TARIESGO.DYC".
           COPY "..\DYC\TBRIEPOL.DYC".
           COPY "..\DYC\CONTDPTO.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\CJASUBSI.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\TBRIEPOL.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\FONCESAN.FD".
           COPY "..\FD\CONTRATO.FD".
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\NOMICIUD.FD".
           COPY "..\FD\NOMIBANK.FD".
           COPY "..\FD\NOMICORP.FD".
           COPY "..\FD\CNNITCED.FD".
           COPY "..\FD\CNCOMPTE.FD".
           COPY "..\FD\CNCATCTA.FD".
           COPY "..\FD\AUDICAMP.FD".
           COPY "..\FD\HOJAVIDA.FD".
           COPY "..\FD\VENDEMPR.FD".
           COPY "..\FD\MODACONT.FD".
           COPY "..\FD\TARIESGO.FD".
           COPY "..\FD\CONTDPTO.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG100".
       77      ID-PRG              PIC X(7)  VALUE "PROG100".
       77      WK-NOM    PIC X(20).
       77      WK-FONCESAN         PIC 9(5)  VALUE ZEROS.
       77      WK-CHEQUEORD        PIC 9(5)  VALUE ZEROS.
       77      WK-RECIBOCAJ        PIC 9(6)  VALUE ZEROS.
       77      WK-CHEQUEPOR        PIC 9(5)  VALUE ZEROS.
       77      WK-SALARIO-ANT      PIC 9(8)  VALUE ZEROS.
       77      WK-SUBSIDIO-ANT     PIC 9(8)  VALUE ZEROS.
       01 W-AUDI-CEDULA.
        03 W-AUDI-CEDULA-D PIC 9(09) VALUE ZEROS.
        03 W-AUDI-EMPRES-D PIC 9(03) VALUE ZEROS.
      *
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CONTRATO.LBL".
           COPY "..\LBL\NOMIACUM.LBL".
           COPY "..\LBL\NOMICIUD.LBL".
           COPY "..\LBL\NOMICORP.LBL".
           COPY "..\LBL\CNNITCED.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\LBL\FONCESAN.LBL".
           COPY "..\LBL\NOMIBANK.LBL".
           COPY "..\LBL\CNCOMPTE.LBL".
           COPY "..\LBL\CNCATCTA.LBL".
           COPY "..\LBL\CJASUBSI.LBL".
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\AUDICAMP.LBL".
           COPY "..\LBL\HOJAVIDA.LBL".
           COPY "..\LBL\VENDEMPR.LBL".
           COPY "..\LBL\TBRIEPOL.LBL".
           COPY "..\LBL\MODACONT.LBL".
           COPY "..\LBL\TARIESGO.LBL".
           COPY "..\LBL\CONTDPTO.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\VARIABLE.WRK".
      *    COPY "..\WRK\FONCESAN.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\VENDEMPR.WRK".
           COPY "..\WRK\COMANDOS.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\CNNITCED.DCL".
           COPY "..\DCL\CJASUBSI.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TABLAEMP.DCL".
           COPY "..\DCL\CONTRATO.DCL".
           COPY "..\DCL\NOMICIUD.DCL".
           COPY "..\DCL\NOMICORP.DCL".
           COPY "..\DCL\NOMIBANK.DCL".
           COPY "..\DCL\FONCESAN.DCL".
           COPY "..\DCL\CNCATCTA.DCL".
           COPY "..\DCL\CNCOMPTE.DCL".
           COPY "..\DCL\AUDICAMP.DCL".
           COPY "..\DCL\HOJAVIDA.DCL".
           COPY "..\DCL\VENDEMPR.DCL".
           COPY "..\DCL\MODACONT.DCL".
           COPY "..\DCL\TBRIEPOL.DCL".
           COPY "..\DCL\TARIESGO.DCL".
           COPY "..\DCL\CONTDPTO.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE  "*E M P R E S A S*"  TO TIT-APLICACION.
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
           PERFORM I-STATUS-CJASUBSI THRU F-STATUS-CJASUBSI.
           PERFORM I-STATUS-CNNITCED THRU F-STATUS-CNNITCED.
           PERFORM I-STATUS-NOMICIUD THRU F-STATUS-NOMICIUD.
           PERFORM I-STATUS-FONCESAN THRU F-STATUS-FONCESAN.
           PERFORM I-STATUS-NOMICORP THRU F-STATUS-NOMICORP.
           PERFORM I-STATUS-CNCATCTA THRU F-STATUS-CNCATCTA.
           PERFORM I-STATUS-CNCOMPTE THRU F-STATUS-CNCOMPTE.
           PERFORM I-STATUS-CONTDPTO THRU F-STATUS-CONTDPTO.
           PERFORM I-STATUS-NOMIBANK THRU F-STATUS-NOMIBANK.
       F-STATUS-TODOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-STATUS-VENDEMPR THRU F-STATUS-VENDEMPR.
           PERFORM I-STATUS-MODACONT THRU F-STATUS-MODACONT.
           PERFORM I-STATUS-TARIESGO THRU F-STATUS-TARIESGO.
           PERFORM I-STATUS-TBRIEPOL THRU F-STATUS-TBRIEPOL.
           OPEN I-O   EMPRESAS VENDEMPR CNNITCED
           OPEN INPUT NOMICIUD CJASUBSI FONCESAN NOMICORP
                      CNCATCTA CNCOMPTE NOMIBANK HOJAVIDA MODACONT
                      TARIESGO CONTDPTO TBRIEPOL.
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
      *    PERFORM I-WINDOW-NOM THRU F-WINDOW-NOM.
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
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO      THRU  F-CODIGO.
           IF   CUP OR ESC       GO    F-ADICIONA.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF  SW-1  =  ZEROS
             PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
             PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
             PERFORM I-EXISTE         THRU F-EXISTE
             MOVE WIN-CODIGO-EMPRESA  TO   WK-CODIGO
             MOVE SPACE               TO   REC-EMPRESAS
             MOVE WK-CODIGO           TO   WIN-CODIGO-EMPRESA
             GO I-ADICIONA
           ELSE
               MOVE WIN-CODIGO-EMPRESA  TO   WK-CODIGO
               MOVE SPACE               TO   REC-EMPRESAS WRK-EMPRESAS
               MOVE WK-CODIGO           TO   WIN-CODIGO-EMPRESA
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
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          OR
                   ESC          GO   F-CONSULTA.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF  SW-1  =  1
               PERFORM I-NEXISTE        THRU F-NEXISTE
               MOVE WIN-CODIGO-EMPRESA  TO   WK-CODIGO
               MOVE SPACE               TO   REC-EMPRESAS
               MOVE WK-CODIGO           TO   WIN-CODIGO-EMPRESA
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
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO       THRU  F-CODIGO.
           IF  CUP OR ESC  GO  F-MODIFICA.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF SW-1  =  1
             PERFORM I-NEXISTE        THRU F-NEXISTE
             MOVE WIN-CODIGO-EMPRESA  TO   WK-CODIGO
             MOVE SPACE               TO   REC-EMPRESAS
             MOVE WK-CODIGO           TO   WIN-CODIGO-EMPRESA
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
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          OR
                   ESC          GO   F-RETIRA.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF  SW-1  =  1
               PERFORM I-NEXISTE        THRU F-NEXISTE
               MOVE WIN-CODIGO-EMPRESA  TO   WK-CODIGO
               MOVE SPACE               TO   REC-EMPRESAS
               MOVE WK-CODIGO           TO   WIN-CODIGO-EMPRESA
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
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO SW-1 WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-CODIGO-EMPRESA  LINE 2 POSITION 15
                  UPDATE PROMPT TAB NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           DISPLAY SPACES     LINE 25 POSITION 01
           WIN-CODIGO-EMPRESA LINE 2 POSITION 15 LOW.
           IF   CUP OR ESC    GO F-CODIGO
           ELSE
             IF  ERR
               MOVE ZEROS TO WK-EX
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
               MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
               DISPLAY " Codigo errado " LINE 25 POSITION 01
                                      CONTROL SG-CONTROL-STRING
               GO I-CODIGO
             ELSE
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE
               ELSE
                 IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
                   GO I-CODIGO
                 ELSE
                   IF WIN-CODIGO-EMPRESA = ZEROS
                     MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
                     DISPLAY " Codigo en ceros " LINE 25 POSITION 01
                                            CONTROL SG-CONTROL-STRING
                     GO I-CODIGO.
           DISPLAY WIN-CODIGO-EMPRESA LINE 2 POSITION 15 LOW.
       F-CODIGO. EXIT.
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS NOMINA/FACTURACION/BANCOS      *
      *****************************************************************
       I-DATOS.
           PERFORM I-DATOS-CAB       THRU  F-DATOS-CAB.
           IF      ESC OR CUP        GO    F-DATOS.
           IF      F4                GO    DAT-1.
           IF      F5                GO    DAT-2.
           IF      F6                GO    DAT-3.
         DAT-1.
      *    PERFORM I-WINDOW-NOM      THRU  F-WINDOW-NOM
      *    PERFORM I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
      *    PERFORM I-DATOS-NOMINA    THRU  F-DATOS-NOMINA.
      *    IF      ESC               GO    F-DATOS.
      *    IF      CUP               GO    I-DATOS.
      *    IF      F6                GO    DAT-3.
         DAT-2.
      *    PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC.
      *    PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
      *    PERFORM I-DATOS-FACTURA   THRU  F-DATOS-FACTURA.
      *    IF      ESC               GO    F-DATOS.
      *    IF      CUP               GO    DAT-1.
      *    IF      F4                GO    DAT-1.
         DAT-3.
           PERFORM I-WINDOW-CON      THRU  F-WINDOW-CON.
           PERFORM I-MUESTRA-BANCO   THRU  F-MUESTRA-BANCO
           PERFORM I-DATOS-BANCO     THRU  F-DATOS-BANCO.
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
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS CABECERA                       *
      *****************************************************************
       I-DATOS-CAB.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF      CUP              GO     CAB-10.
         CAB-0.
           PERFORM I-NIT           THRU   F-NIT.
           IF    ESC OR CUP        GO F-DATOS-CAB.
           IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
         CAB-1.
           PERFORM I-EXTENSION     THRU   F-EXTENSION.
           IF    CUP               GO CAB-0.
           IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
           IF    ESC               GO F-DATOS-CAB.
         CAB-1A.
           PERFORM I-INACTIVA-EMP  THRU   F-INACTIVA-EMP.
           IF    CUP               GO CAB-1.
           IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
           IF    ESC               GO F-DATOS-CAB.
         CAB-2.
           PERFORM I-NOMBRE        THRU   F-NOMBRE.
           IF    CUP               GO CAB-1A.
           IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
           IF    ESC               GO F-DATOS-CAB.
         CAB-3.
           PERFORM I-RESPONSABLE-1   THRU   F-RESPONSABLE-1.
           IF    CUP               GO CAB-2.
           IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
           IF    ESC               GO F-DATOS-CAB.
      *  CAB-4.
      *    PERFORM I-RESPONSABLE-2   THRU   F-RESPONSABLE-2.
      *    IF    CUP               GO CAB-3.
      *    IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
      *    IF    ESC               GO F-DATOS-CAB.
      *  CAB-5.
      *    PERFORM I-RESPONSABLE-3   THRU   F-RESPONSABLE-3.
      *    IF    CUP               GO CAB-4.
      *    IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
      *    IF    ESC               GO F-DATOS-CAB.
      *  CAB-6.
      *    PERFORM I-RESPONSABLE-4   THRU   F-RESPONSABLE-4.
      *    IF    CUP               GO CAB-5.
      *    IF    F4 OR F5 OR F6    GO F-DATOS-CAB.
      *    IF    ESC               GO F-DATOS-CAB.
         CAB-7.
           PERFORM I-DIRECCION      THRU   F-DIRECCION.
           IF    CUP                 GO CAB-3.
           IF    F4 OR F5 OR F6      GO F-DATOS-CAB.
           IF    ESC                 GO F-DATOS-CAB.
         CAB-8.
           PERFORM I-CONTDPTO       THRU   F-CONTDPTO.
           IF    CUP                 GO CAB-7.
           IF    F4 OR F5 OR F6      GO F-DATOS-CAB.
           IF    ESC                 GO F-DATOS-CAB.
         CAB-9.
           PERFORM I-CIUDAD         THRU   F-CIUDAD.
           IF    CUP                 GO CAB-8.
           IF    F4 OR F5 OR F6      GO F-DATOS-CAB.
           IF    ESC                 GO F-DATOS-CAB.
         CAB-10.
           PERFORM I-TELEFONO       THRU   F-TELEFONO.
           IF    CUP                 GO CAB-9.
       F-DATOS-CAB. EXIT.
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS NOMINA                         *
      *****************************************************************
       I-DATOS-NOMINA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF   CUP   GO    NOM-15.
         NOM-0.
           PERFORM I-TPO-COMPNOM    THRU    F-TPO-COMPNOM.
         NOM-01.
           PERFORM I-PERIODO-ACTUAL THRU   F-PERIODO-ACTUAL.
           IF      CUP OR ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-011.
           PERFORM I-ORDEN          THRU   F-ORDEN.
           IF      CUP OR ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-1.
           PERFORM I-TARJETAS       THRU   F-TARJETAS.
           IF      CUP OR ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-2.
           PERFORM I-SALMIN-ANT     THRU   F-SALMIN-ANT.
           IF      CUP              GO     NOM-1.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-3.
           PERFORM I-TRANSP-ANT     THRU   F-TRANSP-ANT.
           IF      CUP              GO     NOM-2.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-4.
           PERFORM I-SALMIN-ACT     THRU   F-SALMIN-ACT.
           IF      CUP              GO     NOM-3.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-5.
           PERFORM I-SUBTR-ACT      THRU   F-SUBTR-ACT.
           IF      CUP              GO     NOM-4.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-6.
           PERFORM I-IND-SUBTR      THRU   F-IND-SUBTR.
           IF      CUP                  GO     NOM-5.
           IF      ESC OR F6 OR F5      GO     F-DATOS-NOMINA.
         NOM-7.
           PERFORM I-PERIODICIDAD   THRU   F-PERIODICIDAD.
           IF      CUP                  GO     NOM-6.
           IF      ESC OR F6 OR F5      GO     F-DATOS-NOMINA.
         NOM-8.
           PERFORM I-SISTEMA-HORAS  THRU   F-SISTEMA-HORAS.
           IF      CUP              GO     NOM-7.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-9.
           PERFORM I-HORAS-240          THRU   F-HORAS-240.
           IF      CUP              GO     NOM-8.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-10.
           PERFORM I-TIPO-LIQUIDACION   THRU   F-TIPO-LIQUIDACION.
           IF      CUP              GO      NOM-9.
           IF      ESC OR CUP OR F6 OR F5   GO   F-DATOS-NOMINA.
         NOM-11.
           PERFORM I-RIESGO-PROF    THRU   F-RIESGO-PROF.
           IF      CUP              GO     NOM-10.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-11A.
           PERFORM I-CONTRATO-ANUAL THRU   F-CONTRATO-ANUAL.
           IF      CUP              GO     NOM-11.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-11B.
           PERFORM I-DIAS-CONTRATO  THRU   F-DIAS-CONTRATO.
           IF      CUP              GO     NOM-11A.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-12.
           PERFORM I-PAGA-CAJA-TOE THRU F-PAGA-CAJA-TOE.
           IF      CUP              GO     NOM-11A.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
      *  NOM-11-1.
      *    IF WIN-SUBSIDIO = 1
      *       PERFORM I-COLS      THRU     F-COLS.
      *    IF      CUP              GO     NOM-12.
         NOM-13.
           PERFORM I-RAD-CON        THRU   F-RAD-CON.
           IF      CUP              GO     NOM-12.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-14.
           PERFORM  I-AUTOLIQU-TOE  THRU  F-AUTOLIQU-TOE.
           IF      CUP              GO     NOM-13.
           IF      ESC OR F6 OR F5  GO     F-DATOS-NOMINA.
         NOM-15.
           PERFORM I-FONDOS         THRU   F-FONDOS.
           IF       CUP             GO    NOM-14.
       F-DATOS-NOMINA. EXIT.
      *
       I-DATOS-FACTURA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF CUP GO FAC-13.
         FAC-0.
           PERFORM I-TPO-COMPROB         THRU  F-TPO-COMPROB.
      *     IF      ESC OR CUP OR F4 OR F6 GO   F-DATOS-FACTURA.
         FAC-1.
         FAC-1-1.
           PERFORM I-PRC-PARAFISCAL      THRU  F-PRC-PARAFISCAL.
           IF      CUP                   GO    FAC-0.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-1-2.
           PERFORM I-PRC-ADMON           THRU  F-PRC-ADMON.
           IF      CUP                   GO    FAC-1-1.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-2.
           PERFORM I-PRC-RETENCION       THRU  F-PRC-RETENCION.
           IF      CUP                   GO    FAC-1-2.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-3.
           PERFORM I-WIN-CTA-RETEFUENTE THRU F-WIN-CTA-RETEFUENTE.
           IF      CUP                   GO    FAC-2.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-31.
           PERFORM I-LIMITE-RFTE        THRU F-LIMITE-RFTE
           IF      CUP                   GO    FAC-3.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-32.
           PERFORM I-PRC-IVA             THRU  F-PRC-IVA.
           IF      CUP                   GO    FAC-31.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-33.
           PERFORM I-APLICA-PRC-IVA THRU F-APLICA-PRC-IVA.
           IF      CUP                   GO    FAC-32.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-4.
           PERFORM I-WIN-CUENTA-IVA THRU F-WIN-CUENTA-IVA.
           IF      CUP                   GO    FAC-33.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-5.
           PERFORM I-VENDEMPR-VENDEDO    THRU  F-VENDEMPR-VENDEDO.
           IF      CUP                   GO    FAC-4.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-6.
           PERFORM I-VENDEMPR-CODMODA    THRU  F-VENDEMPR-CODMODA.
           IF      CUP                   GO    FAC-5.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-7.
           PERFORM I-GENERA-FACTURA       THRU F-GENERA-FACTURA.
           IF      CUP                   GO    FAC-6.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-8.
           PERFORM I-PERIODO-FACT        THRU  F-PERIODO-FACT.
           IF      CUP                   GO    FAC-7.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-9.
           PERFORM I-PLAZO-FACTURA       THRU  F-PLAZO-FACTURA.
           IF      CUP                   GO    FAC-8.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-10.
           PERFORM I-REGIMEN-COMUN       THRU  F-REGIMEN-COMUN.
           IF      CUP                   GO    FAC-9.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-11.
           PERFORM I-POLIZA-VIDA         THRU  F-POLIZA-VIDA.
           IF      CUP                   GO    FAC-10.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-12.
           PERFORM I-RIESGO-POLIZA       THRU  F-RIESGO-POLIZA.
           IF      CUP                   GO    FAC-11.
           IF      ESC OR F4 OR F6       GO    F-DATOS-FACTURA.
         FAC-13.
           PERFORM I-FACTURA-DETALLADA   THRU   F-FACTURA-DETALLADA.
           IF      CUP                   GO    FAC-12.
         F-DATOS-FACTURA. EXIT.
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS BANCOS                         *
      *****************************************************************
       I-DATOS-BANCO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
      *    IF      CUP                GO     CON-7.
         CON-0.
           PERFORM  I-TPO-COMPBAN           THRU  F-TPO-COMPBAN.
         CON-1.
           PERFORM  I-TIPO-PAGO       THRU   F-TIPO-PAGO.
           IF       CUP OR ESC OR F4 OR F5   GO     F-DATOS-BANCO.
         CON-2.
           IF  WIN-TIPO-PAGO = 2
             PERFORM I-BANCO     THRU F-BANCO
           ELSE
             IF  WIN-TIPO-PAGO = 3
               PERFORM I-CORPO     THRU F-CORPO
             ELSE
               IF WIN-TIPO-PAGO = 1
                  MOVE    ZEROS TO WIN-COD-BANCO WIN-TIP-CTA
                  MOVE    SPACE TO BANC-NOMBRE
                  PERFORM I-MUESTRA-BANCO THRU F-MUESTRA-BANCO.
           IF      CUP              GO     CON-1.
           IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
         CON-3.
           PERFORM I-CTA-CTE        THRU   F-CTA-CTE.
           IF      CUP              GO     CON-2.
           IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
         CON-30.
           PERFORM I-ULTACTLIB      THRU   F-ULTACTLIB.
           IF      CUP              GO     CON-3.
           IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
         CON-30-1.
           PERFORM I-CIERRE-CONT    THRU   F-CIERRE-CONT.
           IF      CUP              GO     CON-30.
           IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
      *  CON-31.
      *    PERFORM I-MODULO-CONTABLE THRU  F-MODULO-CONTABLE.
      *    IF      CUP              GO     CON-30-1.
      *    IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
      *  CON-4.
      *    PERFORM I-PRILEG         THRU   F-PRILEG.
      *    IF      CUP              GO     CON-31.
      *    IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
      *  CON-5.
      *    PERFORM I-VACACIONES     THRU   F-VACACIONES.
      *    IF      CUP              GO     CON-4.
      *    IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
      *  CON-6.
      *    PERFORM I-CESANTIAS      THRU   F-CESANTIAS.
      *    IF      CUP              GO     CON-5.
      *    IF      ESC OR F4 OR F5  GO     F-DATOS-BANCO.
      *  CON-7.
      *    PERFORM I-WIN-CESAN-PROM        THRU   F-WIN-CESAN-PROM.
      *    IF      CUP              GO     CON-6.
       F-DATOS-BANCO.  EXIT.
      *****************************************************************
      *    DISPLAY DE DATOS - CABECERA/NOMINA/FACTURACION/BANCOS      *
      *****************************************************************
       I-MUESTRA-CAB.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
                WIN-CODIGO-EMPRESA LINE 02 POSITION 15 LOW
                WIN-EMPRESA        LINE 05 POSITION 21 REVERSE
           MOVE WIN-NIT            TO  WI-Z12C.
           DISPLAY WI-Z12C         LINE 03 POSITION 15 LOW
           WIN-NIT-LETRA           LINE 03 POSITION 30 LOW
           MOVE WIN-NIT-EXTENSION  TO WI-Z2
           DISPLAY
             WI-Z2                 LINE 03 POSITION 31 LOW
             WIN-INACTIVA-EMP      LINE 03 POSITION 66 LOW
             WIN-RESPONSABLE-1     LINE 06 POSITION 21 LOW
             WIN-DIRECCION         LINE 07 POSITION 21 LOW
             WIN-COD-CONTDPTO      LINE 08 POSITION 21 LOW
           MOVE WIN-COD-CONTDPTO   TO COD-DPTO.
           PERFORM I-LEE-CONTDPTO  THRU F-LEE-CONTDPTO.
           IF FIN-ARCH-CONTDPTO = 1 MOVE "N.N. " TO NOM-DPTO.
           DISPLAY NOM-DPTO        LINE 08 POSITION 26 LOW
           DISPLAY WIN-COD-CIUDAD  LINE 09 POSITION 21 LOW
           MOVE WIN-COD-CIUDAD     TO CIUD-CODIGO.
           PERFORM I-LEE-CIUDAD    THRU F-LEE-CIUDAD.
           IF FIN-OK = 1 MOVE " N.N. " TO CIUD-NOMBRE.
           DISPLAY CIUD-NOMBRE     LINE 09 POSITION 26 LOW
           MOVE WIN-NUM-TEL  TO  WI-Z10.
           DISPLAY WI-Z10    LINE 09 POSITION 58 LOW.
       F-MUESTRA-CAB. EXIT.
      *
       I-MUESTRA-NOMINA.
      *  PERFORM I-MOSTRAR-NOMINA THRU F-MOSTRAR-NOMINA.
       F-MUESTRA-NOMINA. EXIT.

       I-MOSTRAR-NOMINA.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    WIN-CODIM-NOMINA     TO   WI-Z3
           DISPLAY WI-Z3                LINE 11 POSITION 21 LOW
           DISPLAY WIN-NOMINA           LINE 13 POSITION 21 LOW
           DISPLAY WIN-FLAG-CIERRE      LINE 13 POSITION 58 LOW
           DISPLAY WIN-PERIODO-ESTADO   LINE 13 POSITION 63 LOW
           DISPLAY WIN-ORDEN     LINE 14 POSITION 21 LOW
           IF  WIN-ORDEN  =  "A "
            DISPLAY      "Alfabetico  " LINE 14 POSITION 24 LOW
           ELSE
            IF WIN-ORDEN  =  "C "
             DISPLAY     "Codigo      " LINE 14 POSITION 24 LOW
            ELSE
             IF     WIN-ORDEN  =  "CA"
                OR  WIN-ORDEN  =  "KA"
              DISPLAY    "CC&Alfabetic" LINE 14 POSITION 24 LOW
             ELSE
              IF     WIN-ORDEN  =  "CN"
                 OR  WIN-ORDEN  =  "KN"
               DISPLAY   "CC y Nit    " LINE 14 POSITION 24 LOW
              ELSE
               IF     WIN-ORDEN   =  "CC"
                  OR  WIN-ORDEN   =  "KC"
                DISPLAY  "CC&Codigo   " LINE 14 POSITION 24 LOW
               ELSE
                IF  WIN-ORDEN  =  "N "
                 DISPLAY "Nit         "  LINE 14 POSITION 24 LOW
                ELSE
                 DISPLAY "N.N         " LINE 14 POSITION 24 LOW.
           DISPLAY WIN-TARJETAS         LINE 14 POSITION 58 LOW.

           MOVE    WIN-SALMIN-ANT       TO   WI-Z8P
           DISPLAY WI-Z8P               LINE 15 POSITION 21 LOW
           MOVE    WIN-SUBTR-ANT        TO   WI-Z7P
           DISPLAY WI-Z7P               LINE 15 POSITION 58 LOW
           MOVE    WIN-SALMIN-ACT       TO   WI-Z8P
           DISPLAY WI-Z8P               LINE 16 POSITION 21 LOW
           MOVE    WIN-ANOSAL-ACT       TO  WK-MASCARA-ANO
           MOVE    WK-ABREVIA(WIN-MESSAL-ACT) TO  WK-MASCARA-MES
           MOVE    WIN-DIASAL-ACT       TO  WK-MASCARA-DIA
           DISPLAY WK-MASCARA-FECHA     LINE 16 POSITION 30 LOW
           DISPLAY WIN-IND-SUBTR        LINE 16 POSITION 57 LOW
           MOVE    WIN-SUBTR-ACT        TO   WI-Z8P
           DISPLAY WI-Z8P               LINE 16 POSITION 58 LOW

           MOVE    WIN-ANOSUB-ACT       TO  WK-MASCARA-ANO
           MOVE    WK-ABREVIA(WIN-MESSUB-ACT) TO  WK-MASCARA-MES
           MOVE    WIN-DIASUB-ACT       TO  WK-MASCARA-DIA
           DISPLAY WK-MASCARA-FECHA     LINE 16 POSITION 68 LOW

           MOVE    WIN-PAGO-MINIMO      TO   WI-Z8P
           DISPLAY WI-Z8P               LINE 17 POSITION 21 LOW
           MOVE    WIN-HORAS-PERIODO    TO   WI-Z3
           DISPLAY WI-Z3                LINE 17 POSITION 71 LOW
           DISPLAY "Horas"              LINE 17 POSITION 75 LOW
           DISPLAY WIN-INDICATIVO-PERIODO  LINE 17 POSITION 58 LOW
           IF WIN-INDICATIVO-PERIODO  =  "C"
            DISPLAY     "Catorcenal" LINE 17 POSITION 60 LOW
           ELSE
            IF WIN-INDICATIVO-PERIODO  =  "Q"
             DISPLAY    "Quincenal " LINE 17 POSITION 60 LOW
            ELSE
             IF WIN-INDICATIVO-PERIODO  =  "S"
              DISPLAY   "Semanal   " LINE 17 POSITION 60 LOW
             ELSE
              IF WIN-INDICATIVO-PERIODO  =  "D"
               DISPLAY  "Decadal   " LINE 17 POSITION 60 LOW
              ELSE
               IF WIN-INDICATIVO-PERIODO = "M"
                DISPLAY "Mensual   " LINE 17 POSITION 60 LOW
               ELSE
                DISPLAY "N.N.      " LINE 17 POSITION 60 LOW.

           DISPLAY WIN-SISTEMA-HORAS LINE 18 POSITION 21 LOW
           IF WIN-SISTEMA-HORAS = "S"
            DISPLAY  "Minutos   " LINE 18 POSITION 23 LOW
           ELSE
            IF WIN-SISTEMA-HORAS = "C"
             DISPLAY "Porcentaje" LINE 18 POSITION 23 LOW
            ELSE
             DISPLAY "N.N.      "   LINE 18 POSITION 23 LOW.

           DISPLAY WIN-HORAS-240 LINE 18 POSITION 58 LOW
           IF WIN-HORAS-240 = 1
            DISPLAY  "hor=sala/240" LINE 18 POSITION 60 LOW
           ELSE
            IF WIN-HORAS-240 = 2
             DISPLAY "hor=sala/hce" LINE 18 POSITION 60 LOW
            ELSE
             DISPLAY "N.N.        " LINE 18 POSITION 60 LOW.

           DISPLAY WIN-TIPO-LIQUIDACION LINE 19 POSITION 21 LOW
           IF WIN-TIPO-LIQUIDACION  =  "C"
            DISPLAY "Calendario" LINE 19 POSITION 23 LOW
           ELSE
            IF WIN-TIPO-LIQUIDACION = "L"
             DISPLAY "Laboral  " LINE 19 POSITION 23 LOW
            ELSE
             DISPLAY " N.N.    " LINE 19 POSITION 23 LOW.

           MOVE WIN-RIESGO-PROF   TO   WI-Z2 TIPO-TARIESGO.
           DISPLAY WI-Z2          LINE 19 POSITION 58 LOW
           PERFORM I-LEE-TARIESGO THRU F-LEE-TARIESGO.
           IF FIN-ARCH-TARIESGO = 1
              MOVE ZEROS  TO PORCEN-TARIESGO
              MOVE SPACES TO NOMBRE-TARIESGO.
           MOVE PORCEN-TARIESGO  TO WI-Z3P3
           DISPLAY WI-Z3P3       LINE 19 POSITION 61 LOW
           NOMBRE-TARIESGO       SIZE 11 LINE 19 POSITION 69 LOW

           DISPLAY WIN-CONTRATO-ANUAL    LINE 20 POSITION 21 LOW.
           DISPLAY WIN-DIAS-CONTRATO     LINE 20 POSITION 58 LOW.

           DISPLAY WIN-PAGA-CAJA-TOE     LINE 21 POSITION 21 LOW.
           IF WIN-PAGA-CAJA-TOE = "T"
            DISPLAY "Trabajador"         LINE 21 POSITION 23 LOW
           ELSE
            IF WIN-PAGA-CAJA-TOE = "E"
             DISPLAY "Empresa"           LINE 21 POSITION 23 LOW
            ELSE
             DISPLAY "N.N."              LINE 21 POSITION 23 LOW.

           DISPLAY WIN-SUBSIDIO          LINE 21 POSITION 58 LOW.
      *    DISPLAY WIN-CONS-SUBSIDIO     LINE 21 POSITION 76.
           MOVE    WIN-SUBSIDIO          TO COD-CJASUBSI
           PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI
           IF SW-EOF = 1
            MOVE "N.N" TO NOM-CJASUBSI.
           DISPLAY NOM-CJASUBSI      LINE 21 POSITION 61 SIZE 19 LOW.
           DISPLAY WIN-CONS-SUBSIDIO LINE 21 POSITION 76 LOW.

           DISPLAY WIN-AUTOLIQU-TOE LINE 22 POSITION 21 LOW
           IF WIN-AUTOLIQU-TOE = "T"
            DISPLAY "Trabajador  " LINE 22 POSITION 23 LOW
           ELSE
            IF WIN-AUTOLIQU-TOE = "E"
             DISPLAY "Empresa    " LINE 22 POSITION 23 LOW
            ELSE
             DISPLAY "N.N.       " LINE 22 POSITION 23 LOW.
           MOVE    WIN-FONDOS            TO COD-FONCESAN
           PERFORM I-LEE-FONCESAN-I00 THRU F-LEE-FONCESAN-I00
           IF FIN-ARCH-FONCESAN = 1
            MOVE "N.N" TO NOM-FONCESAN.
           DISPLAY WIN-FONDOS     LINE 22 POSITION 58 LOW.
           DISPLAY NOM-FONCESAN   LINE 22 POSITION 61 SIZE 19 LOW.
       F-MOSTRAR-NOMINA.

      *
       I-MUESTRA-FACTURA.
      *   PERFORM I-MOSTRAR-FACTURA THRU F-MOSTRAR-FACTURA.
       F-MUESTRA-FACTURA.

       I-MOSTRAR-FACTURA.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
           MOVE "LOW,"             TO SG-INTENSITY.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE    WIN-CODIM-COMP         TO   WI-Z2
           DISPLAY WI-Z2                  LINE 11 POSITION 21 LOW
           DISPLAY CIUD-CTA-PARAFISCAL    LINE 13 POSITION 29 LOW.
           DISPLAY CIUD-CTA-ADMON         LINE 13 POSITION 66 LOW.
           DISPLAY WIN-CTA-RETEFUENTE     LINE 14 POSITION 29 LOW.
           DISPLAY WIN-CUENTA-IVA         LINE 15 POSITION 29 LOW.
           DISPLAY CIUD-CTA-CXC           LINE 15 POSITION 58 LOW.
           PERFORM I-BUSCAR-VENDEDOR THRU F-BUSCAR-VENDEDOR.
           DISPLAY WIN-GENERA-FACTURA     LINE 17 POSITION 58 LOW
           MOVE WIN-PORC-POLIZA-VIDA TO WI-Z2 TIPO-TBRIEPOL.
           PERFORM I-LEE-TBRIEPOL THRU F-LEE-TBRIEPOL.
           IF FIN-ARCH-TBRIEPOL = 1
            MOVE ZEROS  TO PORCEN-TBRIEPOL
            MOVE SPACES TO NOMBRE-TBRIEPOL.
           MOVE WIN-PORC-POLIZA-VIDA TO WI-Z2
           DISPLAY WI-Z2             LINE 21 POSITION 58 LOW.
           MOVE PORCEN-TBRIEPOL      TO WI-Z2P3
           DISPLAY WI-Z2P3           LINE 21 POSITION 62 LOW
           MOVE NUMERO-TBRIEPOL      TO WI-Z6
           DISPLAY WI-Z6             LINE 21 POSITION 65 LOW
           WIN-FACTURA               LINE 18 POSITION 21 LOW
           WIN-ESTA-FACTURA          LINE 18 POSITION 23 LOW
           MOVE    WIN-PLAZO-FACTURA TO   WI-Z4
           DISPLAY WI-Z4             LINE 19 POSITION 21 LOW
           DISPLAY WIN-REGIMEN-COMUN LINE 19 POSITION 58 LOW
           MOVE    WIN-PRC-PRESTACIONAL   TO   WI-Z2P2
           DISPLAY WI-Z2P2           LINE 13 POSITION 21 LOW
           DISPLAY WIN-PRC-APLICA-PARAFIS LINE 13 POSITION 27 LOW
           MOVE    WIN-PRC-ADMON     TO   WI-Z2P2
           DISPLAY WI-Z2P2           LINE 13 POSITION 58 LOW
           DISPLAY WIN-PRC-APLICA-ADMON   LINE 13 POSITION 64 LOW
           MOVE    WIN-PORC-RETEFUENTE    TO   WI-Z3.
           DISPLAY WI-Z3             LINE 14 POSITION 21 LOW
           MOVE    WIN-LIMITE-RFTE        TO   WI-Z8P.
           DISPLAY WI-Z8P            LINE 14 POSITION 58 LOW
           DISPLAY WIN-PORC-IVA      LINE 15 POSITION 21 LOW
           DISPLAY WIN-APLICA-IVA    LINE 15 POSITION 27 LOW.
           DISPLAY WIN-POLIZA-VIDA   LINE 21 POSITION 21 LOW.
           DISPLAY WIN-FACTURA-DETALLADA  LINE 22 POSITION 21 LOW.
       F-MOSTRAR-FACTURA. EXIT.
      *
       I-BUSCAR-VENDEDOR.
           MOVE    WIN-CODIGO-EMPRESA    TO VENDEMPR-EMPRE01
           PERFORM I-START-VENDEMPR-01 THRU F-START-VENDEMPR-01.
           IF FIN-ARCH-VENDEMPR = 1      GO TO F-BUSCAR-VENDEDOR.
           PERFORM I-LEE-C-VENDEMPR THRU F-LEE-C-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1      GO TO F-BUSCAR-VENDEDOR.
           IF WIN-CODIGO-EMPRESA  =      VENDEMPR-EMPRESA
              PERFORM I-INFACT-VENDEMPR  THRU F-INFACT-VENDEMPR
              MOVE VENDEMPR-VENDEDO      TO CODIM
              PERFORM I-LEE-CNNITCED-I00 THRU F-LEE-CNNITCED-I00
              MOVE CODIM                 TO WI-Z12C
              DISPLAY WI-Z12C            LINE 16 POSITION 21 LOW
              IF FIN-ARCH-CNNITCED = 1
                 DISPLAY "N.N." LINE 16 POSITION 41
              ELSE
                 DISPLAY NOMBM   LINE 16 POSITION 41 LOW SIZE 30
                 PERFORM I-BUSCAR-MODACONT THRU F-BUSCAR-MODACONT
              END-IF
           ELSE
            MOVE ZEROS TO VENDEMPR-VENDEDO
                          VENDEMPR-EMPRESA VENDEMPR-EMPRE01
                          VENDEMPR-CODMODA WRK-REG-VENDEMPR.
       F-BUSCAR-VENDEDOR. EXIT.
      *
       I-INFACT-VENDEMPR.
           MOVE ZEROS TO WRK-REG-VENDEMPR.
           MOVE VENDEMPR-VENDEDO TO WRK-VENDEMPR-VENDEDO.
           MOVE VENDEMPR-EMPRESA TO WRK-VENDEMPR-EMPRESA.
           MOVE VENDEMPR-CODMODA TO WRK-VENDEMPR-CODMODA.
       F-INFACT-VENDEMPR. EXIT.
      *
       I-BUSCAR-MODACONT.
           MOVE VENDEMPR-CODMODA TO MODACONT-CODI.
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            MOVE ZEROS TO MODACONT-PRC-PAGO
                          MODACONT-PRC-ADMI.
           MOVE MODACONT-CODI TO WI-Z3
           DISPLAY WI-Z3 LINE 17 POSITION 21 LOW
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 17 POSITION 29 LOW.
       F-BUSCAR-MODACONT. EXIT.
      *
       I-MUESTRA-BANCO.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    WIN-CODIM-BANCO  TO  WI-Z2
           DISPLAY WI-Z2            LINE 11 POSITION 21 LOW
           MOVE    WIN-TIPO-PAGO    TO  WI-Z1
           DISPLAY WI-Z1            LINE 13 POSITION 21 LOW
           MOVE    WIN-COD-BANCO    TO  WI-Z2
           DISPLAY WI-Z2            LINE 14 POSITION 21 LOW
           MOVE    WIN-TIP-CTA      TO  WI-Z2
           DISPLAY WI-Z2            LINE 15 POSITION 21 LOW.
           IF WIN-TIPO-PAGO = 1
             DISPLAY "EFECTIVO       " LINE 13 POSITION 25 LOW.
           IF WIN-TIPO-PAGO = 2
             DISPLAY "CHEQUE         " LINE 13 POSITION 25 LOW.
           IF WIN-TIPO-PAGO = 3
             DISPLAY "CONSIGNACION   " LINE 13 POSITION 25 LOW.
           IF  WIN-TIPO-PAGO = 2
            MOVE  WIN-COD-BANCO  TO  BANC-CODIGO
            PERFORM I-LEE-BANCO THRU F-LEE-BANCO
            IF FIN-OK = 1
             MOVE "N.N." TO BANC-NOMBRE
            END-IF
            DISPLAY BANC-NOMBRE LINE 14 POSITION 27 SIZE 13 LOW
           ELSE
            IF  WIN-TIPO-PAGO = 3
             MOVE WIN-COD-BANCO    TO    CORP-CODIGO
             PERFORM I-LEE-CORPO   THRU  F-LEE-CORPO
             IF  FIN-OK = 1
              MOVE "N.N."   TO CORP-NOMBRE
             END-IF
             DISPLAY CORP-NOMBRE LINE 14 POSITION 27 SIZE 13 LOW
            ELSE
            MOVE    WK-EFECTIVO  TO  WI-Z10
            DISPLAY WI-Z10       LINE 13 POSITION 25 LOW.
           DISPLAY "AฅO:"        LINE 13 POSITION 59
           DISPLAY WIN-ULT-ACT-LIB-ANO LINE 13 POSITION 64 LOW.
           DISPLAY "MES:"        LINE 13 POSITION 69
           DISPLAY WIN-ULT-ACT-LIB-MES LINE 13 POSITION 74 LOW.
           DISPLAY "AฅO:"        LINE 14 POSITION 59
           DISPLAY WIN-CIERRE-MES-CONT-ANO LINE 14 POSITION 64 LOW.
           DISPLAY "MES:"        LINE 14 POSITION 69
           DISPLAY WIN-CIERRE-MES-CONT-MES LINE 14 POSITION 74 LOW.

      *    DISPLAY WIN-MODULO-CONTABLE LINE 15 POSITION 58 LOW
      *    DISPLAY WIN-TIPO-LIQ-PRIMA LINE 16 POSITION 58 LOW.
      *    DISPLAY WIN-TIPO-LIQ-VACAC LINE 17 POSITION 58 LOW.
      *    DISPLAY WIN-TIPO-LIQ-CESAN LINE 18 POSITION 58 LOW.
      *    DISPLAY WIN-CESAN-PROM     LINE 19 POSITION 58 LOW.
           IF   WIN-TIP-CTA = ZEROS OR
                WIN-TIP-CTA = SPACES
             MOVE    ZEROS       TO    WI-Z12  WI-Z10
             DISPLAY WI-Z12      LINE  15 POSITION 25 LOW
             GO F-MUESTRA-BANCO.
           IF   WIN-TIPO-PAGO = 2
             MOVE    BANC-CTA-CTE (WIN-TIP-CTA) TO WI-Z12
             DISPLAY WI-Z12      LINE  15 POSITION 25 LOW
             MOVE    BANC-CTA-CONTABLE (WIN-TIP-CTA) TO WI-Z12
             DISPLAY WI-Z12      LINE  16 POSITION 25 LOW
           ELSE
            IF WIN-TIPO-PAGO = 3
              MOVE    CORP-CTA-CTE (WIN-TIP-CTA) TO WI-Z12
              DISPLAY WI-Z12      LINE  15 POSITION 25 LOW
              MOVE    CORP-CTA-CONTABLE (WIN-TIP-CTA) TO WI-Z12
              DISPLAY WI-Z12      LINE  16 POSITION 25 LOW.
       F-MUESTRA-BANCO. EXIT.
      *****************************************************************
      *    CAPTURA DE CAMPOS MODULO PRINCIPAL O CABECERA              *
      *****************************************************************
       I-NIT.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS  TO  WK-EXCEPTION.
           MOVE WIN-NIT TO WI-Z12C.
           IF W-OPCION = "A" MOVE "-" TO WIN-NIT-LETRA.
           DISPLAY WI-Z12C LINE 3 POSITION 15
           WIN-NIT-LETRA   LINE 03 POSITION 30
           SPACES SIZE 15  LINE 03 POSITION 15.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-NIT  LINE 03 POSITION 15
                           UPDATE PROMPT TAB NO BEEP
                        ON EXCEPTION WK-EX
                           PERFORM 999-EXCEPTION.
           MOVE WIN-NIT TO WI-Z12C CODIM.
           DISPLAY WI-Z12C LINE 3 POSITION 15.
           IF AF10
            MOVE 1  TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-NIT.
           IF CUP OR ESC GO F-NIT.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF WIN-NIT = ZEROS GO I-NIT.
           DISPLAY WIN-NIT-LETRA LINE 03 POSITION 30.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-NIT.
       F-NIT. EXIT.
      *
       I-LEE-NIT.
           MOVE WIN-NIT TO CODIM.
           MOVE  ZEROS  TO LL.
           READ CNNITCED WITH NO LOCK INVALID KEY
                MOVE  1  TO  LL.
           IF LL = ZEROS GO F-LEE-NIT.
           MOVE WIN-EMPRESA       TO  NOMBM
           MOVE WIN-NIT           TO  CODIM
           MOVE WIN-DIRECCION     TO  DIREM
           MOVE WIN-COD-CIUDAD    TO  CIUDM
           MOVE WIN-NUM-TEL       TO  TELEM.
           MOVE    "E"            TO  FLAG-NIT.
           MOVE     0             TO  SW-CNNITCED.
           WRITE REGMA1  INVALID KEY REWRITE REGMA1.
       F-LEE-NIT.
      *
       I-EXTENSION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS  TO  WK-EXCEPTION.
           MOVE WIN-NIT-EXTENSION TO WI-Z2.
           DISPLAY WI-Z2 LINE 03 POSITION 31
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-NIT-EXTENSION LINE 03 POSITION 31
                                    UPDATE PROMPT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           MOVE WIN-NIT-EXTENSION TO WI-Z2.
           DISPLAY WI-Z2 LINE 03 POSITION 31.
           IF AF10
            MOVE  2 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-EXTENSION.
           IF CUP OR ESC GO F-EXTENSION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           MOVE WIN-NIT-EXTENSION TO WI-Z2
           DISPLAY WI-Z2 LINE 03 POSITION 31.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-EXTENSION.
       F-EXTENSION. EXIT.
      *
       I-INACTIVA-EMP.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY "La Sucursal esta Inmovil o Cesante? [Si/No]"
           LINE 25 POSITION 01 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO WK-EXCEPTION.
           IF WIN-INACTIVA-EMP = SPACES  AND W-OPCION = "A"
            MOVE "N" TO WIN-INACTIVA-EMP.
           DISPLAY WIN-INACTIVA-EMP LINE 03 POSITION 66
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-INACTIVA-EMP  LINE 03 POSITION 66
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           DISPLAY WIN-INACTIVA-EMP LINE 03 POSITION 66
           IF AF10
            MOVE  3 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-INACTIVA-EMP.
           IF CUP OR ESC GO F-INACTIVA-EMP.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-INACTIVA-EMP = "S" OR
              WIN-INACTIVA-EMP = "N"
              NEXT SENTENCE
           ELSE
              GO I-INACTIVA-EMP.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-INACTIVA-EMP.
       F-INACTIVA-EMP. EXIT.
      *
       I-NOMBRE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE  ZEROS  TO WK-EXCEPTION
           DISPLAY WIN-EMPRESA LINE 5 POSITION 21.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-EMPRESA LINE 5 POSITION 21
                               UPDATE TAB NO BEEP PROMPT
                            ON EXCEPTION WK-EX
                               PERFORM 999-EXCEPTION.
           DISPLAY WIN-EMPRESA LINE 5 POSITION 21.
           IF AF10
            MOVE 4 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-NOMBRE.
           IF CUP OR ESC GO F-NOMBRE.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF WIN-EMPRESA = SPACES GO I-NOMBRE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-NOMBRE.
       F-NOMBRE. EXIT.

       I-RESPONSABLE-1.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "REPRESENTANTE LEGAL   " LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY WIN-RESPONSABLE-1 LINE 06 POSITION 21
           ACCEPT WIN-RESPONSABLE-1 LINE 06 POSITION 21
                                    UPDATE PROMPT NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           DISPLAY WIN-RESPONSABLE-1 LINE 06 POSITION 21
           IF AF10
            MOVE 05 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RESPONSABLE-1.
           IF CUP OR ESC GO F-RESPONSABLE-1.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-RESPONSABLE-1.
       F-RESPONSABLE-1. EXIT.
      *
       I-RESPONSABLE-2.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "GERENTE GENERAL..............:"
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY WIN-RESPONSABLE-2 LINE 25 POSITION 31
           ACCEPT WIN-RESPONSABLE-2 LINE 25 POSITION 31
                                    UPDATE PROMPT NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-RESPONSABLE-2 LINE 25 POSITION 31
           IF AF10
            MOVE 06 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RESPONSABLE-2.
           IF CUP OR ESC GO F-RESPONSABLE-2.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-RESPONSABLE-2.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-RESPONSABLE-2. EXIT.
      *
       I-RESPONSABLE-3.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "GERENTE TALENTO HUMANO.......:"
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY WIN-RESPONSABLE-3 LINE 25 POSITION 31
           ACCEPT WIN-RESPONSABLE-3 LINE 25 POSITION 31
                                    UPDATE PROMPT NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-RESPONSABLE-3 LINE 25 POSITION 31
           IF AF10
            MOVE 07 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RESPONSABLE-3.
           IF CUP OR ESC GO F-RESPONSABLE-3.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-RESPONSABLE-3.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-RESPONSABLE-3. EXIT.
      *
       I-RESPONSABLE-4.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "GERENTE COOP/FONDO...........:"
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY WIN-RESPONSABLE-4 LINE 25 POSITION 31
           ACCEPT WIN-RESPONSABLE-4 LINE 25 POSITION 31
                                    UPDATE PROMPT NO BEEP TAB
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-RESPONSABLE-4 LINE 25 POSITION 31
           IF AF10
            MOVE 08 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RESPONSABLE-4.
           IF CUP OR ESC GO F-RESPONSABLE-4.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-RESPONSABLE-4.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-RESPONSABLE-4. EXIT.

       I-DIRECCION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-DIRECCION LINE 7 POSITION 21
           ACCEPT WIN-DIRECCION  LINE 7 POSITION 21
                                 UPDATE TAB NO BEEP PROMPT
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           DISPLAY WIN-DIRECCION LINE 7 POSITION 21.
           IF AF10
            MOVE 9 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-DIRECCION.
           IF CUP OR ESC GO F-DIRECCION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF WIN-DIRECCION = SPACES GO I-DIRECCION.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-DIRECCION.
       F-DIRECCION. EXIT.
      *
       I-CONTDPTO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[F1] AYUDA" LINE 25 POSITION 01 REVERSE.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WIN-COD-CONTDPTO  LINE 8 POSITION 21
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-COD-CONTDPTO  LINE 8 POSITION 21
                                     UPDATE TAB NO BEEP PROMPT
                                  ON EXCEPTION WK-EX
                                     PERFORM 999-EXCEPTION.
           IF F1
            PERFORM I-AYUDA-CONTDPTO THRU F-AYUDA-CONTDPTO UNTIL ESC
            MOVE ZEROS  TO  WK-EXCEPTION
            PERFORM  I-WINDOW-NOM      THRU  F-WINDOW-NOM
            PERFORM  I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
            GO TO I-CONTDPTO.
           IF AF10
            MOVE 10 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-CONTDPTO.
           IF CUP OR ESC GO TO F-CONTDPTO.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE WIN-COD-CONTDPTO TO COD-DPTO.
           PERFORM I-LEE-CONTDPTO THRU F-LEE-CONTDPTO.
           IF FIN-ARCH-CONTDPTO = 1
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Ciudad no existe..." LINE 25 POSITION 01
            CONTROL SG-CONTROL-STRING
            GO I-CONTDPTO.
           DISPLAY WIN-COD-CONTDPTO LINE 08 POSITION 21
           DISPLAY NOM-DPTO         LINE 08 POSITION 26
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-CONTDPTO.
       F-CONTDPTO. EXIT.
      *
       I-CIUDAD.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[F1] AYUDA" LINE 25 POSITION 01 REVERSE.
           MOVE ZEROS TO WK-EXCEPTION.
           IF ( WIN-COD-CIUDAD = ZEROS OR
                WIN-COD-CIUDAD = SPACES ) AND W-OPCION = "A"
                MOVE 01 TO WIN-COD-CIUDAD.
           DISPLAY WIN-COD-CIUDAD LINE 9 POSITION 21
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-COD-CIUDAD  LINE 9 POSITION 21
                                  UPDATE TAB NO BEEP PROMPT
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.

           IF F1
            PERFORM I-AYUDA-CIUDADES THRU F-AYUDA-CIUDADES UNTIL ESC
            MOVE ZEROS  TO  WK-EXCEPTION
            PERFORM  I-WINDOW-NOM      THRU  F-WINDOW-NOM
            PERFORM  I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
            GO TO I-CIUDAD.
           IF AF10
            MOVE 11 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-CIUDAD.
           IF CUP OR ESC GO TO F-CIUDAD.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE WIN-COD-CIUDAD TO CIUD-CODIGO.
           PERFORM I-LEE-CIUDAD THRU F-LEE-CIUDAD.
           IF FIN-OK = 1
             MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
             MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Ciudad no existe..." LINE 25 POSITION 01
             CONTROL SG-CONTROL-STRING
             GO I-CIUDAD.
           DISPLAY CIUD-NOMBRE LINE 09 POSITION 26.
           IF WIN-SUBSIDIO = 0
             MOVE CIUD-CAJASUB TO WIN-SUBSIDIO.
           MOVE WIN-SUBSIDIO TO COD-CJASUBSI.
           PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI.
           IF SW-EOF = 1
             MOVE "N.N.           " TO NOM-CJASUBSI.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
      *    DISPLAY WIN-SUBSIDIO LINE 21 POSITION 58.
      *    DISPLAY NOM-CJASUBSI LINE 21 POSITION 61 SIZE 19.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-CIUDAD.
       F-CIUDAD. EXIT.
      *
       I-TELEFONO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE WIN-NUM-TEL TO WI-Z10
           DISPLAY WI-Z10 LINE 9 POSITION 58
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-NUM-TEL LINE 9 POSITION 58
                              UPDATE PROMPT TAB NO BEEP
                           ON EXCEPTION WK-EX
                              PERFORM 999-EXCEPTION.
           MOVE WIN-NUM-TEL  TO  WI-Z10.
           DISPLAY WI-Z10 LINE 9 POSITION 58.
           IF AF10
            MOVE 12 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-TELEFONO.
           IF CUP OR ESC GO F-TELEFONO.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE WIN-NUM-TEL  TO  WI-Z10
           DISPLAY WI-Z10 LINE 9 POSITION 58.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
             GO I-TELEFONO.
       F-TELEFONO. EXIT.
      *****************************************************************
      *    CAPTURA DE CAMPOS MODULO NOMINA                            *
      *****************************************************************
       I-TPO-COMPNOM.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Comprobantes "
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE   ZEROS   TO   WK-EXCEPTION.
           IF  W-OPCION = "A"  AND
               ( WIN-CODIM-NOMINA = SPACES OR
                 WIN-CODIM-NOMINA = ZEROS )
             MOVE 07 TO WIN-CODIM-NOMINA.
           IF  W-OPCION = "M"  AND
               ( WIN-CODIM-NOMINA = SPACES OR
                 WIN-CODIM-NOMINA = ZEROS )
             MOVE 07 TO WIN-CODIM-NOMINA.
           MOVE    WIN-CODIM-NOMINA    TO   WI-Z3
           DISPLAY WI-Z3               LINE 11 POSITION 21
           IF AF10
            MOVE 13 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-TPO-COMPNOM.
           IF F1
             PERFORM I-HELP-COMPROBANTES THRU  F-HELP-COMPROBANTES
              UNTIL ESC
             MOVE ZEROS  TO  WK-EXCEPTION
             PERFORM I-WINDOW-NOM        THRU  F-WINDOW-NOM
             PERFORM I-MUESTRA-NOMINA    THRU  F-MUESTRA-NOMINA
             GO I-TPO-COMPNOM.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE    WIN-CODIM-NOMINA   TO   WI-Z3
           DISPLAY WI-Z3              LINE 11 POSITION 21
           IF  CUP OR ESC  GO F-TPO-COMPNOM.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE      WIN-CODIM-NOMINA   TO   LLAVM1-COMP.
           PERFORM   I-LEE-COMPRO   THRU   F-LEE-COMPRO.
           DISPLAY   NOMBM-COMP LINE 11 POSITION 25 SIZE 17
           IF   FIN-OK = 1
                PERFORM ACEPTA-TRUCO
                DISPLAY "ERROR EN COMPROBANTE"
                DISPLAY LLAVM1-COMP
                 GO  I-TPO-COMPNOM.
           IF   F4 AND W-OPCION = "A"   OR   F6  AND   W-OPCION = "A"
             GO   I-TPO-COMPNOM.
       F-TPO-COMPNOM. EXIT.
      *
       I-PERIODO-ACTUAL.
           IF W-OPCION = "A"
            MOVE ZEROS TO WIN-NOMINA
            MOVE "C" TO WIN-FLAG-CIERRE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY
           "Shift+F2 SOLO si es PRIMERA SERIE"
           LINE 25 POSITION 01 REVERSE.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-NOMINA LINE 13 POSITION 21
           WIN-FLAG-CIERRE    LINE 13 POSITION 58
           ACCEPT WN-OPCION   LINE 13 POSITION 57
                            UPDATE PROMPT TAB NO BEEP
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           IF SF2
              MOVE    ZEROS  TO  WK-EXCEPTION
              DISPLAY WIN-NOMINA LINE 13 POSITION 21
              MOVE    0  TO WIN-NOMINA
              MOVE "C" TO WIN-FLAG-CIERRE
              DISPLAY WIN-FLAG-CIERRE LINE 13 POSITION 58
              DISPLAY WIN-NOMINA LINE 13 POSITION 21.
           DISPLAY SPACES SIZE 2 LINE 13 POSITION 21.
           IF  CUP OR ESC  GO F-PERIODO-ACTUAL.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF DEL
             MOVE  0   TO  WIN-NOMINA
             DISPLAY WIN-NOMINA LINE 13 POSITION 21.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-PERIODO-ACTUAL.
           IF AF8
            PERFORM I-RECIBE-PERIODO THRU F-RECIBE-PERIODO.
       F-PERIODO-ACTUAL. EXIT.
      *
       I-RECIBE-PERIODO.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT WIN-NOMINA  LINE 13 POSITION 21
                              UPDATE PROMPT TAB NO BEEP
                           ON EXCEPTION WK-EX
                              PERFORM 999-EXCEPTION
           DISPLAY WIN-NOMINA LINE 13 POSITION 21.
           IF AF10
            MOVE 14 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RECIBE-PERIODO.
           IF  CUP OR ESC  GO F-RECIBE-PERIODO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-AYUDA-FLAG-CIERRE THRU F-AYUDA-FLAG-CIERRE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF WIN-FLAG-CIERRE =  SPACES
              MOVE "C" TO WIN-FLAG-CIERRE.
           ACCEPT WIN-FLAG-CIERRE LINE 13 POSITION 58
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION
           DISPLAY WIN-FLAG-CIERRE LINE 13 POSITION 58.
           PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
           PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
           PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
           PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
           IF AF10
            MOVE 15 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RECIBE-PERIODO.
           IF  CUP OR ESC  GO F-RECIBE-PERIODO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
      * Ahora para recibir el indicador paso NOMINA DEFINITIVA
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-AYUDA-PERIODO-ESTADO THRU F-AYUDA-PERIODO-ESTADO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-PERIODO-ESTADO LINE 13 POSITION 63
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION
           DISPLAY WIN-PERIODO-ESTADO LINE 13 POSITION 63.
           PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
           PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
           PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
           PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
           IF AF10
            MOVE 16 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RECIBE-PERIODO.
           IF  CUP OR ESC  GO F-RECIBE-PERIODO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-RECIBE-PERIODO. EXIT.
      *
       I-AYUDA-PERIODO-ESTADO.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออหออออออออออออออออออออออออออออออออออออออออป"
           LINE 04 POSITION 20 REVERSE
           "บ  00  บ INICIAR nueva Serie de N=nomina        บ"
           LINE 05 POSITION 20 REVERSE
           "บ  76  บ Prenomina realizada.                   บ"
           LINE 06 POSITION 20 REVERSE
           "บ  77  บ Nomina definitiva realizada.           บ"
           LINE 07 POSITION 20 REVERSE
           "บ  00  บ Cheque, consignac,o efectuvi realizado บ"
           LINE 08 POSITION 20 REVERSE
           "ศออออออสออออออออออออออออออออออออออออออออออออออออผ"
           LINE 09 POSITION 20 REVERSE.
       F-AYUDA-PERIODO-ESTADO. EXIT.
      *
       I-AYUDA-FLAG-CIERRE.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออหออออออออออออออออออออออออออออออออออออออออป"
           LINE 04 POSITION 20 REVERSE
           "บ   C  บ INICIO nueva Serie de Nomina o Liquidacบ"
           LINE 05 POSITION 20 REVERSE
           "บ   I  บ Al procesar una nueva serie de Nomina  บ"
           LINE 06 POSITION 20 REVERSE
           "บ   E  บ Al procesar una nueva serie de Liquid. บ"
           LINE 07 POSITION 20 REVERSE
           "บSPACE บ Al efectuar la nomina definitiva.      บ"
           LINE 08 POSITION 20 REVERSE
           "บ   C  บ Al efectuar cheque, davivienda o efect.บ"
           LINE 09 POSITION 20 REVERSE
           "ศออออออสออออออออออออออออออออออออออออออออออออออออผ"
           LINE 10 POSITION 20 REVERSE.
       F-AYUDA-FLAG-CIERRE. EXIT.
      *
       I-ORDEN.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY "[C]"              LINE 24 POSITION 02 REVERSE
           DISPLAY "Codigo    "       LINE 24 POSITION 06 REVERSE
           DISPLAY "[A]"              LINE 25 POSITION 02 REVERSE
           DISPLAY "Alfabetico"       LINE 25 POSITION 06 REVERSE
           DISPLAY "[N ]"             LINE 24 POSITION 20 REVERSE
           DISPLAY "Nit o C.C.     "  LINE 24 POSITION 32 REVERSE
           DISPLAY "[CC] o [KC]"      LINE 25 POSITION 20 REVERSE
           DISPLAY "C.DE.C Y CODIGO"  LINE 25 POSITION 32 REVERSE
           DISPLAY "[CN] o [KN]"      LINE 24 POSITION 48 REVERSE
           DISPLAY "C.DE.C Y NIT O C.C." LINE 24 POSITION 58 REVERSE
           DISPLAY "[CA] o [KA]"      LINE 25 POSITION 48 REVERSE
           DISPLAY "C.DE.C Y ALFABETICO" LINE 25 POSITION 58 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS   TO   WK-EXCEPTION
           DISPLAY WIN-ORDEN   LINE 14 POSITION 21
           ACCEPT  WIN-ORDEN   LINE 14 POSITION 21
                               UPDATE PROMPT NO BEEP TAB
                            ON EXCEPTION WK-EX
                               PERFORM 999-EXCEPTION.
           DISPLAY WIN-ORDEN  LINE 14 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF AF10
            MOVE 17 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-ORDEN.
           IF  CUP OR ESC  GO F-ORDEN.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-ORDEN  =  "A "
            DISPLAY       "Alfabetico   " LINE 14 POSITION 24
           ELSE
            IF WIN-ORDEN  =  "C "
             DISPLAY      "Codigo       " LINE 14 POSITION 24
            ELSE
             IF  WIN-ORDEN  =  "N "
              DISPLAY     "Nit          " LINE 14 POSITION 24
             ELSE
              IF   WIN-ORDEN   =  "CA"
                OR WIN-ORDEN   =  "KA"
               DISPLAY    "CC&Alfabetico" LINE 14 POSITION 24
              ELSE
               IF    WIN-ORDEN   =  "CN"
                 OR  WIN-ORDEN   =  "KN"
                DISPLAY   "CC&Nit       " LINE 14 POSITION 24
               ELSE
                IF     WIN-ORDEN  =  "CC"
                   OR  WIN-ORDEN  =  "KC"
                  DISPLAY "CC&Codigo    " LINE 14 POSITION 24
                ELSE
                 GO I-ORDEN.
           IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
            GO I-ORDEN.
       F-ORDEN. EXIT.
      *
       I-TARJETAS.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE   ZEROS   TO   WK-EXCEPTION
           IF W-OPCION = "A" MOVE "N" TO WIN-TARJETAS.
           DISPLAY "IMPRIME TITULOS EMPRESA S/N"
                   LINE 25 POSITION 1.
           DISPLAY WIN-TARJETAS  LINE 14 POSITION 58
           ACCEPT WIN-TARJETAS   LINE 14 POSITION 58
                                 UPDATE PROMPT NO BEEP TAB
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           DISPLAY WIN-TARJETAS  LINE 14 POSITION 58
           IF AF10
            MOVE 18 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-TARJETAS.
           IF  CUP OR ESC GO F-TARJETAS.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF     WIN-TARJETAS   =  "S"
              OR  WIN-TARJETAS   =  "N"
            NEXT SENTENCE
           ELSE
            GO TO I-TARJETAS.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
            GO I-TARJETAS.
       F-TARJETAS. EXIT.
      *
       I-SALMIN-ANT.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           MOVE  WIN-SALMIN-ANT  TO  WI-Z8P
           DISPLAY SPACES SIZE 10  LINE 15 POSITION 21
           DISPLAY WIN-SALMIN-ANT  LINE 15 POSITION 21
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-SALMIN-ANT  LINE 15 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-SALMIN-ANT         TO   WI-Z8P
           DISPLAY WI-Z8P                 LINE 15 POSITION 21
           IF AF10
            MOVE 19 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-SALMIN-ANT.
           IF  CUP OR ESC GO F-SALMIN-ANT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-SALMIN-ANT  =   ZEROS
             GO  I-SALMIN-ANT.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-SALMIN-ANT.
       F-SALMIN-ANT. EXIT.
      *
       I-TRANSP-ANT.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           MOVE  WIN-SUBTR-ANT  TO  WI-Z8P
           DISPLAY SPACES SIZE 8   LINE 15 POSITION 58
           DISPLAY WIN-SUBTR-ANT  LINE 15 POSITION 58
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-SUBTR-ANT  LINE 15 POSITION 58
                                   UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-SUBTR-ANT         TO   WI-Z8P
           DISPLAY WI-Z8P                 LINE 15 POSITION 58
           IF AF10
            MOVE 20 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-TRANSP-ANT.
           IF  CUP OR ESC GO F-TRANSP-ANT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-SUBTR-ANT  =   ZEROS
             GO  I-TRANSP-ANT.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-TRANSP-ANT.
       F-TRANSP-ANT. EXIT.
      *
       I-SALMIN-ACT.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           MOVE  WIN-SALMIN-ACT  TO  WI-Z8P  WK-SALARIO-ANT
           DISPLAY SPACES SIZE 10  LINE 16 POSITION 21
           DISPLAY WIN-SALMIN-ACT  LINE 16 POSITION 21
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-SALMIN-ACT  LINE 16 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-SALMIN-ACT         TO   WI-Z8P
           DISPLAY WI-Z8P                 LINE 16 POSITION 21
      *    PERFORM I-WIN-FECSAL-ACT THRU F-WIN-FECSAL-ACT.
           IF WIN-SALMIN-ACT NOT = WRK-SALMIN-ACT
            COMPUTE WIN-PAGO-MINIMO ROUNDED = WIN-SALMIN-ACT / 30.
           MOVE    WIN-PAGO-MINIMO  TO   WI-Z8P
           DISPLAY WI-Z8P           LINE 17 POSITION 21

           IF AF3
            PERFORM I-WIN-PAGO-MINIMO THRU F-WIN-PAGO-MINIMO.

           IF AF10
            MOVE 21 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-SALMIN-ACT.
           IF  CUP OR ESC GO F-SALMIN-ACT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-SALMIN-ACT  =   ZEROS
             GO  I-SALMIN-ACT.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-SALMIN-ACT.
       F-SALMIN-ACT. EXIT.


       I-WIN-PAGO-MINIMO.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           DISPLAY SPACES SIZE 7   LINE 17 POSITION 21
           DISPLAY WIN-PAGO-MINIMO LINE 17 POSITION 21
           ACCEPT  WIN-PAGO-MINIMO LINE 17 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-PAGO-MINIMO  TO   WI-Z8P
           DISPLAY WI-Z8P           LINE 17 POSITION 21
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF AF10
            MOVE 22 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-WIN-PAGO-MINIMO.
       F-WIN-PAGO-MINIMO. EXIT.
      *
       I-SUBTR-ACT.
           IF  WIN-IND-SUBTR = "N"
             MOVE ZEROS TO WIN-SUBTR-ACT
             MOVE WIN-SUBTR-ACT  TO  WI-Z7P
             DISPLAY WI-Z7P      LINE 16 POSITION 57
             GO F-SUBTR-ACT.
           MOVE    ZEROS          TO  WK-EXCEPTION.
           MOVE    WIN-SUBTR-ACT  TO  WI-Z7P  WK-SUBSIDIO-ANT
           DISPLAY SPACES SIZE 8  LINE 16 POSITION 58
           DISPLAY WIN-SUBTR-ACT  LINE 16 POSITION 58
           ACCEPT  WIN-SUBTR-ACT  LINE 16 POSITION 58
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           MOVE    WIN-SUBTR-ACT  TO  WI-Z7P
           DISPLAY WI-Z7P         LINE 16 POSITION 58
      *    PERFORM I-WIN-FECSUB-ACT THRU F-WIN-FECSUB-ACT.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF AF10
            MOVE 23 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-SUBTR-ACT.
           IF CUP OR ESC  GO F-SUBTR-ACT.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-SUBTR-ACT  =  ZEROS
             GO I-SUBTR-ACT.
           IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-SUBTR-ACT.
       F-SUBTR-ACT. EXIT.
      *
       I-IND-SUBTR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "[S] = APLICA SUBSIDIO TRANSPORTE  [N] = NO APLICA"
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF  W-OPCION = "A"
             MOVE "S" TO WIN-IND-SUBTR.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           DISPLAY WIN-IND-SUBTR LINE 16 POSITION 57
           ACCEPT  WIN-IND-SUBTR LINE 16 POSITION 57
                                 UPDATE PROMPT TAB NO BEEP
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           DISPLAY  WIN-IND-SUBTR   LINE 16 POSITION 57
           DISPLAY  SPACES SIZE 80  LINE 25 POSITION 01.
           IF AF10
            MOVE 24 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-IND-SUBTR.
           IF   CUP OR ESC    GO    F-IND-SUBTR.
           IF WIN-IND-SUBTR = SPACES
              MOVE "S" TO WIN-IND-SUBTR.
           DISPLAY   WIN-IND-SUBTR   LINE 16 POSITION 57
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-IND-SUBTR.
       F-IND-SUBTR. EXIT.
      *
       I-WIN-FECSAL-ACT.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           ACCEPT  WIN-FECSAL-ACT LINE 16 POSITION 31
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-FECSAL-ACT  TO   WI-FECHA
           DISPLAY WI-FECHA        LINE 16 POSITION 31
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF AF10
            MOVE 25 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-WIN-FECSAL-ACT.
       F-WIN-FECSAL-ACT. EXIT.

       I-WIN-FECSUB-ACT.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           DISPLAY WIN-FECSUB-ACT  LINE 16 POSITION 66
           ACCEPT  WIN-FECSUB-ACT  LINE 16 POSITION 66
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-FECSUB-ACT  TO   WI-FECHA
           DISPLAY WI-FECHA         LINE 16 POSITION 66
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF AF10
            MOVE 26 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-WIN-FECSUB-ACT.
       F-WIN-FECSUB-ACT. EXIT.

       I-PERIODICIDAD.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY "[S]"          LINE 24 POSITION 06 REVERSE
           DISPLAY "Semanal   "   LINE 24 POSITION 10 REVERSE
           DISPLAY "[D]"          LINE 24 POSITION 26 REVERSE
           DISPLAY "Decadal   "   LINE 24 POSITION 30 REVERSE
           DISPLAY "[Q]"          LINE 25 POSITION  6 REVERSE
           DISPLAY "Quincenal "   LINE 25 POSITION 10 REVERSE
           DISPLAY "[M]"          LINE 25 POSITION 26 REVERSE
           DISPLAY "Mensual   "   LINE 25 POSITION 30 REVERSE
           DISPLAY "[C]"          LINE 24 POSITION 46 REVERSE
           DISPLAY "Catorcenal"   LINE 24 POSITION 50 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE   ZEROS   TO   WK-EXCEPTION
           DISPLAY WIN-INDICATIVO-PERIODO LINE 17 POSITION 58
           ACCEPT  WIN-INDICATIVO-PERIODO LINE 17 POSITION 58
                                          UPDATE PROMPT NO BEEP TAB
                                       ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
           DISPLAY WIN-INDICATIVO-PERIODO LINE 17 POSITION 58
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF AF10
            MOVE 27 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-PERIODICIDAD.
           ACCEPT XX LINE 17 POSITION 58
                     UPDATE PROMPT TAB NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           DISPLAY WIN-INDICATIVO-PERIODO LINE 17 POSITION 58
           IF AF10
            MOVE 28 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-PERIODICIDAD.
           IF  CUP OR ESC  GO F-PERIODICIDAD.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-INDICATIVO-PERIODO   =  "C"
             MOVE  112  TO  WIN-HORAS-PERIODO
             DISPLAY     "Catorcenal" LINE 17 POSITION 60
           ELSE
            IF WIN-INDICATIVO-PERIODO  =  "Q"
              MOVE  120  TO  WIN-HORAS-PERIODO
              DISPLAY    "Quincenal " LINE 17 POSITION 60
            ELSE
             IF WIN-INDICATIVO-PERIODO = "S"
               MOVE 56  TO  WIN-HORAS-PERIODO
               DISPLAY   "Semanal   " LINE 17 POSITION 60
             ELSE
              IF WIN-INDICATIVO-PERIODO = "D"
               MOVE 80  TO  WIN-HORAS-PERIODO
               DISPLAY   "Decadal   " LINE 17 POSITION 60
              ELSE
               IF WIN-INDICATIVO-PERIODO = "M"
                 MOVE 240 TO WIN-HORAS-PERIODO
                 DISPLAY "Mensual   " LINE 17 POSITION 60
               ELSE
                GO TO I-PERIODICIDAD.
           MOVE WIN-HORAS-PERIODO TO WI-Z3
           DISPLAY WI-Z3 LINE 17 POSITION 71
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-PERIODICIDAD.
       F-PERIODICIDAD. EXIT.
      *
       I-SISTEMA-HORAS.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           " [S] minutoS      [C] PorCentaje."
           LINE 25 POSITION 01 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE  ZEROS  TO  WK-EXCEPTION.
           DISPLAY SPACES SIZE 7 LINE 18 POSITION 21
           DISPLAY WIN-SISTEMA-HORAS LINE 18 POSITION 21
           ACCEPT WIN-SISTEMA-HORAS LINE 18 POSITION 21
                                    UPDATE PROMPT NO BEEP TAB
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-SISTEMA-HORAS LINE 18 POSITION 21
           IF WIN-SISTEMA-HORAS = "S"
            DISPLAY  "Minutos Hora   " LINE 18 POSITION 23
           ELSE
            IF WIN-SISTEMA-HORAS = "C"
             DISPLAY "Porcentaje Hora" LINE 18 POSITION 23
            ELSE
             DISPLAY "N.N.           "   LINE 18 POSITION 23.
           IF AF10
            MOVE 29 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-SISTEMA-HORAS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP  GO F-SISTEMA-HORAS.
           IF WIN-SISTEMA-HORAS = "S" OR
              WIN-SISTEMA-HORAS = "C"
            NEXT SENTENCE
           ELSE
            GO I-SISTEMA-HORAS.
       F-SISTEMA-HORAS. EXIT.

       I-HORAS-240.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           " [1] Valor hora = sueldo  / 240                    "
           LINE 24 POSITION 01 REVERSE
           " [2] Valor hora = sueldo  / horas pactadas contrato"
           LINE 25 POSITION 01 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WIN-HORAS-240    LINE 18 POSITION 58
           ACCEPT  WIN-HORAS-240    LINE 18 POSITION 58
                                    UPDATE PROMPT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-HORAS-240 LINE 18 POSITION 58
           IF WIN-HORAS-240 =  1
            DISPLAY  "VrHora=sue/240      " LINE 18 POSITION 60
           ELSE
            IF WIN-HORAS-240 =  2
             DISPLAY "VrHora=sue/h.pactdas" LINE 18 POSITION 60
            ELSE
             DISPLAY "N.N.                " LINE 18 POSITION 60.
           IF AF10
            MOVE 30 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-HORAS-240.
           IF CUP OR ESC GO F-HORAS-240.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-HORAS-240 =  1  OR
              WIN-HORAS-240 =  2
            NEXT SENTENCE
           ELSE
            GO I-HORAS-240.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
            GO I-HORAS-240.
       F-HORAS-240. EXIT.

       I-TIPO-LIQUIDACION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY "[C] Calendario = 365 dias      "
           LINE 24 POSITION 01 REVERSE
           DISPLAY "[L] Laboral = 360 dias         "
           LINE 24 POSITION 46 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS  TO  WK-EXCEPTION
           DISPLAY WIN-TIPO-LIQUIDACION LINE 19 POSITION 21
           ACCEPT WIN-TIPO-LIQUIDACION  LINE 19 POSITION 21
                                        UPDATE PROMPT NO BEEP TAB
                                     ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
           DISPLAY WIN-TIPO-LIQUIDACION LINE 19 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           IF AF10
            MOVE 31 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-TIPO-LIQUIDACION.
           IF  CUP OR ESC  GO F-TIPO-LIQUIDACION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-TIPO-LIQUIDACION   =  "C"
             DISPLAY "Calendario" LINE 19 POSITION 23
           ELSE
            IF  WIN-TIPO-LIQUIDACION  =  "L"
              DISPLAY "Laboral  "  LINE 19 POSITION 23
            ELSE
              GO TO I-TIPO-LIQUIDACION.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-TIPO-LIQUIDACION.
       F-TIPO-LIQUIDACION. EXIT.
      *
       I-RIESGO-PROF.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-AYUDA-TARIESGO THRU F-AYUDA-TARIESGO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE WIN-RIESGO-PROF TO WI-Z2.
           DISPLAY WI-Z2          LINE 19 POSITION 58.
           ACCEPT WIN-RIESGO-PROF LINE 19 POSITION 58
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           MOVE WIN-RIESGO-PROF TO WI-Z2 TIPO-TARIESGO.
           DISPLAY WI-Z2          LINE 19 POSITION 58.
           PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
           PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
           PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
           PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
           IF AF10
            MOVE 32 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RIESGO-PROF.
           IF CUP OR ESC GO F-RIESGO-PROF.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           PERFORM   I-LEE-TARIESGO    THRU    F-LEE-TARIESGO.
           IF FIN-ARCH-TARIESGO = 1
            GO I-RIESGO-PROF.
           MOVE PORCEN-TARIESGO TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 19 POSITION 61
           NOMBRE-TARIESGO SIZE 11 LINE 19 POSITION 69.
       F-RIESGO-PROF. EXIT.
      *
       I-CONTRATO-ANUAL.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF WIN-CONTRATO-ANUAL = "S" OR
              WIN-CONTRATO-ANUAL = "N"
             NEXT SENTENCE
           ELSE
             MOVE "S" TO WIN-CONTRATO-ANUAL.

           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY
           "Contrato a termino fijo-Periodo Anual?S/N"
           LINE 25 POSITION 01 REVERSE.

           ACCEPT WIN-CONTRATO-ANUAL LINE 20 POSITION 21
                               UPDATE PROMPT TAB NO BEEP
                            ON EXCEPTION WK-EX
                               PERFORM 999-EXCEPTION.

           IF AF10
             MOVE 33 TO LAB-AUDICAMP-ORDCAM
             MOVE ZEROS TO SW-1
             PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
               UNTIL ESC OR SW-1 = 1
             PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
             PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
             PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
             PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
             GO TO I-CONTRATO-ANUAL.
           IF CUP OR ESC  GO F-CONTRATO-ANUAL.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF WIN-CONTRATO-ANUAL = "S" OR
              WIN-CONTRATO-ANUAL = "N"
             NEXT SENTENCE
           ELSE
             GO I-CONTRATO-ANUAL.
       F-CONTRATO-ANUAL. EXIT.
      *
       I-DIAS-CONTRATO.
           MOVE ZEROS TO WK-EXCEPTION.
           IF  WIN-CONTRATO-ANUAL = "S"
             MOVE  ZEROS  TO  WIN-DIAS-CONTRATO
             DISPLAY WIN-DIAS-CONTRATO LINE 20 POSITION 58
             GO  F-DIAS-CONTRATO.

           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "Duracion en dias para el termino del contrato"
           LINE 25 POSITION 01 REVERSE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.

           ACCEPT WIN-DIAS-CONTRATO  LINE 20 POSITION 58
                               UPDATE PROMPT TAB NO BEEP
                            ON EXCEPTION WK-EX
                               PERFORM 999-EXCEPTION.

           IF AF10
             MOVE 34 TO LAB-AUDICAMP-ORDCAM
             MOVE ZEROS TO SW-1
             PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
               UNTIL ESC OR SW-1 = 1
             PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
             PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
             PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
             PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
             GO TO I-DIAS-CONTRATO.
           IF CUP OR ESC  GO F-DIAS-CONTRATO.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF WIN-DIAS-CONTRATO > 000 AND
              WIN-DIAS-CONTRATO < 350
             NEXT SENTENCE
           ELSE
             GO I-DIAS-CONTRATO.
       F-DIAS-CONTRATO. EXIT.

       I-PAGA-CAJA-TOE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           "Pago parafiscales por [E] empresa, [T] Trabajador"
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WIN-PAGA-CAJA-TOE LINE 21 POSITION 21
           ACCEPT  WIN-PAGA-CAJA-TOE LINE 21 POSITION 21
                                     UPDATE PROMPT TAB NO BEEP
                                  ON EXCEPTION WK-EX
                                     PERFORM 999-EXCEPTION.
           DISPLAY WIN-PAGA-CAJA-TOE LINE 21 POSITION 23
           IF WIN-PAGA-CAJA-TOE = "T"
            DISPLAY  "Trabajador " LINE 21 POSITION 23
           ELSE
            IF WIN-PAGA-CAJA-TOE = "E"
             DISPLAY "Empresa    " LINE 21 POSITION 23
            ELSE
             DISPLAY "N.N.       " LINE 21 POSITION 23.
           IF AF10
            MOVE 35 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-PAGA-CAJA-TOE.
           IF CUP OR ESC GO F-PAGA-CAJA-TOE.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-PAGA-CAJA-TOE = "T" OR
              WIN-PAGA-CAJA-TOE = "E"
            NEXT SENTENCE
           ELSE
            GO I-PAGA-CAJA-TOE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
            GO I-PAGA-CAJA-TOE.
       F-PAGA-CAJA-TOE. EXIT.

       I-COLS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 3 LINE 21 POSITION 76.
           DISPLAY
           "Shift + F2: Asignar No.consecutivo automatico a la Sucursal"
               LINE 25 POSITION 1 REVERSE
           ACCEPT WN-OPCION LINE 21 POSITION 76
                            UPDATE PROMPT TAB NO BEEP
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           DISPLAY WIN-CONS-SUBSIDIO LINE 21 POSITION 76.
           IF  CUP OR ESC  GO F-COLS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  W-OPCION = "A" AND WIN-SUBSIDIO  = 1
               CLOSE CNCOMPTE
               MOVE 90 TO W-COMP
               PERFORM I-BUSCA-CONSECUTIVO THRU F-BUSCA-CONSECUTIVO
               MOVE    WR-RADIC-CONTABLE   TO  WIN-CONS-SUBSIDIO
               OPEN    INPUT CNCOMPTE.
           IF STABULAR
           DISPLAY
           "Consecutivo para archivo INFORMATIVO a la Caja Comp.Fliar"
               LINE 25 POSITION 1 REVERSE
              MOVE ZEROS TO WK-EXCEPTION
              DISPLAY "No." LINE 21 POSITION 72
              ACCEPT WIN-CONS-SUBSIDIO  LINE 21 POSITION 76
                                        UPDATE PROMPT TAB NO BEEP
                                        ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION
              DISPLAY WIN-CONS-SUBSIDIO LINE 21 POSITION 76.
           DISPLAY WIN-CONS-SUBSIDIO   LINE 21 POSITION 76.
       F-COLS. EXIT.

       I-RAD-CON.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           IF W-OPCION = "A" MOVE ZEROS TO WIN-CONS-SUBSIDIO.
           MOVE  ZEROS  TO   WK-EXCEPTION
           DISPLAY WIN-SUBSIDIO   LINE 21 POSITION 58
           ACCEPT WIN-SUBSIDIO    LINE 21 POSITION 58
                                  UPDATE PROMPT NO BEEP TAB
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           DISPLAY WIN-SUBSIDIO   LINE 21 POSITION 58
           IF F1
            PERFORM I-AYUDA-SUBSIDIO THRU F-AYUDA-SUBSIDIO UNTIL ESC
            MOVE ZEROS  TO  WK-EXCEPTION
            PERFORM  I-WINDOW-NOM      THRU  F-WINDOW-NOM
            PERFORM  I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
            GO TO I-RAD-CON.
           IF AF10
            MOVE 36 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-RAD-CON.
           IF  CUP OR ESC  GO F-RAD-CON.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           MOVE  WIN-SUBSIDIO  TO  COD-CJASUBSI
           PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI
           IF SW-EOF = 1 GO I-RAD-CON.
           DISPLAY WIN-SUBSIDIO   LINE 21 POSITION 58.
           DISPLAY NOM-CJASUBSI   LINE 21 POSITION 61 SIZE 15.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-RAD-CON.
       F-RAD-CON. EXIT.

       I-AUTOLIQU-TOE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "Para pago de autoliquidaciขn por [E] empresa,[T] Trabajador"
           LINE 25 POSITION 01 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WIN-AUTOLIQU-TOE LINE 22 POSITION 21
           ACCEPT  WIN-AUTOLIQU-TOE LINE 22 POSITION 21
                                    UPDATE PROMPT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY WIN-AUTOLIQU-TOE LINE 22 POSITION 21
           IF WIN-AUTOLIQU-TOE = "T"
            DISPLAY "Trabajador." LINE 22 POSITION 23
           ELSE
            IF WIN-AUTOLIQU-TOE = "E"
             DISPLAY "Empresa.   " LINE 22 POSITION 23
            ELSE
             DISPLAY "N.N.       " LINE 22 POSITION 23.
           IF AF10
            MOVE 37 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-AUTOLIQU-TOE.
           IF CUP OR ESC GO F-AUTOLIQU-TOE.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-AUTOLIQU-TOE = "T" OR
              WIN-AUTOLIQU-TOE = "E"
            NEXT SENTENCE
           ELSE
            GO I-AUTOLIQU-TOE.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
            GO I-AUTOLIQU-TOE.
       F-AUTOLIQU-TOE. EXIT.
      *
       I-FONDOS.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[F1] AYUDA" LINE 25 POSITION 01 REVERSE
           MOVE    ZEROS            TO   WK-EXCEPTION
           MOVE    WIN-FONDOS       TO   WI-Z2.
           DISPLAY WI-Z2            LINE 22 POSITION 58
           ACCEPT WIN-FONDOS LINE 22 POSITION 58
                             UPDATE PROMPT NO BEEP TAB
                          ON EXCEPTION WK-EX
                             PERFORM 999-EXCEPTION.
           MOVE    WIN-FONDOS     TO   WI-Z2.
           DISPLAY WI-Z2          LINE 22 POSITION 58
           IF F1
            PERFORM I-AYUDA-FONCESAN THRU
                    F-AYUDA-FONCESAN UNTIL ESC
            MOVE ZEROS  TO  WK-EXCEPTION
            PERFORM  I-WINDOW-NOM      THRU  F-WINDOW-NOM
            PERFORM  I-MUESTRA-NOMINA  THRU  F-MUESTRA-NOMINA
            GO TO I-FONDOS.
           IF AF10
            MOVE 38 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU  F-MUESTRA-NOMINA
            GO TO I-FONDOS.
           IF  CUP OR ESC  GO F-FONDOS.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-FONDOS.
           IF WIN-FONDOS = 0
              MOVE 1 TO WIN-FONDOS.
           MOVE WIN-FONDOS               TO LLAVM1-FONCESAN
           READ FONCESAN WITH NO LOCK INVALID KEY
            DISPLAY
           "Codigo FONDO CESANTIAS no existe.."
            LINE 25 POSITION 01 REVERSE
            PERFORM ACEPTA-TRUCO
            GO I-FONDOS.
           DISPLAY NOM-FONCESAN LINE 22 POSITION 61 SIZE 11.
       F-FONDOS. EXIT.
      *
      *****************************************************************
      *    CAPTURA DE CAMPOS MODULO FACTURACION                       *
      *****************************************************************
       I-TPO-COMPROB.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Comprobantes "
           LINE 25 POSITION 01 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS                            TO   WK-EXCEPTION
           IF      W-OPCION = "A" AND
                   ( WIN-CODIM-COMP = SPACES OR
                     WIN-CODIM-COMP = ZEROS )
               MOVE 04 TO WIN-CODIM-COMP.
           IF      W-OPCION = "M" AND
                   ( WIN-CODIM-COMP = SPACES OR
                     WIN-CODIM-COMP = ZEROS )
               MOVE 04 TO WIN-CODIM-COMP.
           MOVE    ZEROS                 TO   WK-EXCEPTION
           MOVE    WIN-CODIM-COMP        TO   WI-Z2
           DISPLAY WI-Z2                 LINE 11 POSITION 21
           IF AF10
            MOVE 39 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-TPO-COMPROB.
           IF F1
            PERFORM I-HELP-COMPROBANTES THRU F-HELP-COMPROBANTES
              UNTIL ESC
            PERFORM I-WINDOW-FAC        THRU F-WINDOW-FAC
            PERFORM I-MUESTRA-FACTURA   THRU F-MUESTRA-FACTURA
            GO I-TPO-COMPROB.
           MOVE     WIN-CODIM-COMP      TO   WI-Z2
           DISPLAY  WI-Z2               LINE 11 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           IF  CUP OR ESC  GO F-TPO-COMPROB.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE WIN-CODIM-COMP TO LLAVM1-COMP.
           PERFORM I-LEE-COMPRO THRU F-LEE-COMPRO.
           DISPLAY   NOMBM-COMP LINE 11 POSITION 25 SIZE 17
           IF  FIN-OK = 1 GO I-TPO-COMPROB.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-TPO-COMPROB.
       F-TPO-COMPROB. EXIT.

       I-PRC-PARAFISCAL.
           MOVE ZEROS                    TO   WK-EXCEPTION
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE WIN-PRC-PRESTACIONAL     TO   WI-Z2P2
           DISPLAY WI-Z2P2               LINE 13 POSITION 21
           ACCEPT WIN-PRC-PRESTACIONAL   LINE 13 POSITION 21
                                         UPDATE ECHO NO BEEP
                                      ON EXCEPTION WK-EX
                                         PERFORM 999-EXCEPTION.
           MOVE    WIN-PRC-PRESTACIONAL   TO   WI-Z2P2
           DISPLAY WI-Z2P2                LINE 13 POSITION 21
           IF AF10
            MOVE 40 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-PRC-PARAFISCAL.
           IF  CUP OR ESC  GO F-PRC-PARAFISCAL.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WIN-PRC-PRESTACIONAL  =  ZEROS
             GO I-PRC-PARAFISCAL.
           IF F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-PRC-PARAFISCAL.
           PERFORM I-APLICA-PRC-PARAFIS THRU F-APLICA-PRC-PARAFIS.
       F-PRC-PARAFISCAL. EXIT.
      *
       I-APLICA-PRC-PARAFIS.
           MOVE   ZEROS   TO   WK-EXCEPTION
           DISPLAY WIN-PRC-APLICA-PARAFIS   LINE 13 POSITION 27
           ACCEPT WIN-PRC-APLICA-PARAFIS    LINE 13 POSITION 27
                                            UPDATE ECHO NO BEEP
                                         ON EXCEPTION WK-EX
                                            PERFORM 999-EXCEPTION.
           DISPLAY WIN-PRC-APLICA-PARAFIS   LINE 13 POSITION 27
           IF AF10
            MOVE 41 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-APLICA-PRC-PARAFIS.
           IF  CUP OR ESC   GO F-APLICA-PRC-PARAFIS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WIN-PRC-PRESTACIONAL   <  40
             MOVE "S" TO WIN-PRC-APLICA-PARAFIS
           ELSE
             MOVE "N" TO WIN-PRC-APLICA-PARAFIS.
           DISPLAY WIN-PRC-APLICA-PARAFIS   LINE 13 POSITION 27
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-APLICA-PRC-PARAFIS.
       F-APLICA-PRC-PARAFIS. EXIT.
      *
       I-PRC-ADMON.
           MOVE    ZEROS           TO   WK-EXCEPTION
           MOVE    WIN-PRC-ADMON   TO   WI-Z2P2
           DISPLAY WI-Z2P2         LINE 13 POSITION 58
           ACCEPT  WIN-PRC-ADMON   LINE 13 POSITION 58
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE    WIN-PRC-ADMON   TO   WI-Z2P2
           DISPLAY WI-Z2P2         LINE 13 POSITION 58
           IF AF10
            MOVE 42 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-PRC-ADMON.
           IF  CUP OR ESC  GO F-PRC-ADMON.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-PRC-ADMON   =  ZEROS   GO   I-PRC-ADMON.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-PRC-ADMON.
           PERFORM I-APLICA-PRC-ADMON   THRU F-APLICA-PRC-ADMON.
       F-PRC-ADMON. EXIT.
      *
       I-APLICA-PRC-ADMON.
           MOVE ZEROS                     TO   WK-EXCEPTION
           DISPLAY WIN-PRC-APLICA-ADMON   LINE 13 POSITION 64
           ACCEPT WIN-PRC-APLICA-ADMON    LINE 13 POSITION 64
                                          UPDATE ECHO NO BEEP
                                       ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
           DISPLAY WIN-PRC-APLICA-ADMON   LINE 13 POSITION 64
           IF AF10
            MOVE 43 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-APLICA-PRC-ADMON.
           IF  CUP OR ESC   GO F-APLICA-PRC-ADMON.
           IF  F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WIN-PRC-PRESTACIONAL  < 40
             MOVE "S" TO WIN-PRC-APLICA-ADMON
           ELSE
             MOVE "N" TO WIN-PRC-APLICA-ADMON.
           DISPLAY WIN-PRC-APLICA-ADMON     LINE 13 POSITION 64
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-APLICA-PRC-ADMON.
       F-APLICA-PRC-ADMON. EXIT.
      *
       I-PRC-RETENCION.
           MOVE    ZEROS               TO   WK-EXCEPTION
           MOVE    WIN-PORC-RETEFUENTE TO   WI-Z3
           DISPLAY WI-Z3               LINE 14 POSITION 21
           ACCEPT  WIN-PORC-RETEFUENTE LINE 14 POSITION 21
                                       UPDATE PROMPT CONVERT TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           MOVE    WIN-PORC-RETEFUENTE TO   WI-Z3
           DISPLAY WI-Z3               LINE 14 POSITION 21
           IF AF10
            MOVE 44 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-PRC-RETENCION.
           IF  CUP OR ESC  GO F-PRC-RETENCION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF  WIN-PORC-RETEFUENTE = ZEROS
             MOVE WR-PORC-RETENCION TO WIN-PORC-RETEFUENTE WI-Z3
             DISPLAY WI-Z3          LINE 14 POSITION 21.

           DISPLAY WIN-CTA-RETEFUENTE     LINE 14 POSITION 29.
           MOVE    WIN-LIMITE-RFTE         TO   WI-Z8P.
           DISPLAY WI-Z8P                 LINE 14 POSITION 58.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-PRC-RETENCION.
       F-PRC-RETENCION. EXIT.
      *
       I-WIN-CTA-RETEFUENTE.
           MOVE  ZEROS          TO  WK-EXCEPTION.

           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY " Por favor verifique la cuenta contable. "
           LINE 25 POSITION 01 REVERSE

           DISPLAY WIN-CTA-RETEFUENTE LINE 14 POSITION 29.

           IF WIN-PORC-RETEFUENTE = ZEROS
            MOVE ZEROS TO WIN-CTA-RETEFUENTE.

           ACCEPT  WIN-CTA-RETEFUENTE LINE 14 POSITION 29
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           DISPLAY WIN-CTA-RETEFUENTE LINE 14 POSITION 29

           IF AF10
            MOVE 45 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-WIN-CTA-RETEFUENTE.

           IF  CUP OR ESC  GO F-WIN-CTA-RETEFUENTE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-WIN-CTA-RETEFUENTE.

           IF WIN-PORC-RETEFUENTE = ZEROS GO F-WIN-CTA-RETEFUENTE.

           MOVE WIN-CTA-RETEFUENTE TO CTAMAE.
           PERFORM I-LEE-CNCATCTA-I00 THRU F-LEE-CNCATCTA-I00.
           IF FIN-ARCH-CNCATCTA = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
            " Cuenta contable no existe en el PUC ... "
            LINE 25 POSITION 01 BEEP BLINK
            PERFORM ACEPTA-TRUCO
            GO I-WIN-CTA-RETEFUENTE.
           IF ESTACTA = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Cuenta no permite movimiento contable"
              LINE 25 POSITION 01 BEEP BLINK
              PERFORM ACEPTA-TRUCO
              GO I-WIN-CTA-RETEFUENTE.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01.
           DISPLAY NOMCTA LINE 24 POSITION 01.
       F-WIN-CTA-RETEFUENTE. EXIT.
      *
       I-LIMITE-RFTE.
           MOVE    ZEROS            TO   WK-EXCEPTION
           DISPLAY SPACES SIZE 6 LINE 14 POSITION 58
           DISPLAY WIN-LIMITE-RFTE  LINE 14 POSITION 58
           ACCEPT  WIN-LIMITE-RFTE  LINE 14 POSITION 58
                                    UPDATE PROMPT CONVERT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           MOVE    WIN-LIMITE-RFTE  TO   WI-Z8P
           DISPLAY WI-Z8P           LINE 14 POSITION 58
           IF AF10
            MOVE 46 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-LIMITE-RFTE.

           IF  CUP OR ESC   GO F-LIMITE-RFTE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF  WIN-LIMITE-RFTE   = ZEROS
             MOVE WK-LIMITE-RFTE TO WIN-LIMITE-RFTE WI-Z8P
             DISPLAY WI-Z8P      LINE 14 POSITION 58.

           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO F-LIMITE-RFTE.
       F-LIMITE-RFTE. EXIT.
      *
       I-PRC-IVA.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
                   SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE    ZEROS         TO   WK-EXCEPTION
           DISPLAY WIN-PORC-IVA  LINE 15 POSITION 21
           ACCEPT  WIN-PORC-IVA  LINE 15 POSITION 21
                                 UPDATE PROMPT CONVERT TAB NO BEEP
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           DISPLAY WIN-PORC-IVA  LINE 15 POSITION 21
           IF AF10
            MOVE 47 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-PRC-IVA.
           IF  CUP OR ESC  GO F-PRC-IVA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-PRC-IVA.
       F-PRC-IVA. EXIT.
      *
       I-APLICA-PRC-IVA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " Aplica IVA ? [ S / N ] "
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE   ZEROS   TO   WK-EXCEPTION

           IF WIN-APLICA-IVA = SPACE
            MOVE "N" TO WIN-APLICA-IVA.

           IF WIN-PORC-IVA = ZEROS
            MOVE "N" TO WIN-APLICA-IVA.

           DISPLAY WIN-APLICA-IVA   LINE 15 POSITION 27
           ACCEPT WIN-APLICA-IVA    LINE 15 POSITION 27
                                    UPDATE ECHO NO BEEP TAB
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
                   SPACES SIZE 80 LINE 25 POSITION 01.
           DISPLAY WIN-APLICA-IVA   LINE 15 POSITION 27
           IF AF10
            MOVE 48 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-APLICA-PRC-IVA.

           IF  CUP OR ESC   GO F-APLICA-PRC-IVA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-APLICA-PRC-IVA.

           IF WIN-APLICA-IVA = "S" OR
              WIN-APLICA-IVA = "N"
            NEXT SENTENCE
           ELSE
            GO I-APLICA-PRC-IVA.

           IF WIN-APLICA-IVA = "S"
            IF WIN-PORC-IVA = ZEROS
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             " El porcentanje es cero, verifique ... "
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO
             GO I-APLICA-PRC-IVA.
       F-APLICA-PRC-IVA. EXIT.

       I-WIN-CUENTA-IVA.
           MOVE  ZEROS          TO  WK-EXCEPTION.

           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY " Por favor verifique la cuenta. "
           LINE 25 POSITION 01 REVERSE

           DISPLAY WIN-CUENTA-IVA LINE 15 POSITION 29.

           IF WIN-PORC-IVA = ZEROS
            MOVE ZEROS TO WIN-CUENTA-IVA.

           ACCEPT  WIN-CUENTA-IVA LINE 15 POSITION 29
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           DISPLAY WIN-CUENTA-IVA LINE 15 POSITION 29

           IF AF10
            MOVE 49 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-WIN-CUENTA-IVA.

           IF  CUP OR ESC  GO F-WIN-CUENTA-IVA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-WIN-CUENTA-IVA.

           IF WIN-PORC-IVA = ZEROS GO F-WIN-CUENTA-IVA.

           MOVE WIN-CUENTA-IVA TO CTAMAE.
           PERFORM I-LEE-CNCATCTA-I00 THRU F-LEE-CNCATCTA-I00.
           IF FIN-ARCH-CNCATCTA = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
            " Cuenta contable no existe en el PUC ... "
            LINE 25 POSITION 01 BEEP BLINK
            PERFORM ACEPTA-TRUCO
            GO I-WIN-CUENTA-IVA.
           IF ESTACTA = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Cuenta no permite movimiento contable"
              LINE 25 POSITION 01 BEEP BLINK
              PERFORM ACEPTA-TRUCO
              GO I-WIN-CUENTA-IVA.

           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY NOMCTA LINE 24 POSITION 01.
       F-WIN-CUENTA-IVA. EXIT.
      *
       I-VENDEMPR-VENDEDO.
           MOVE ZEROS TO WK-EXCEPTION.
           IF VENDEMPR-VENDEDO = ZEROS
            MOVE WK-TOTAL-CONCEPTO TO VENDEMPR-VENDEDO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY SPACES SIZE 15 LINE 16 POSITION 21
           VENDEMPR-VENDEDO       LINE 16 POSITION 21
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-VENDEDOR     LINE 16 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE WIN-VENDEDOR TO VENDEMPR-VENDEDO WI-Z12C
           DISPLAY WI-Z12C LINE 16 POSITION 21
           IF CUP OR ESC GO F-VENDEMPR-VENDEDO.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
            GO I-VENDEMPR-VENDEDO.
           MOVE    VENDEMPR-VENDEDO   TO CODIM.
           PERFORM I-LEE-CNNITCED-I00 THRU F-LEE-CNNITCED-I00.
           IF FIN-ARCH-CNNITCED = 1
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "N.I.T No existe " LINE 25 POSITION 01 BEEP BLINK
              PERFORM ACEPTA-TRUCO
              GO I-VENDEMPR-VENDEDO.
           IF AF10
            MOVE 50 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-VENDEMPR-VENDEDO.

           IF  CUP OR ESC  GO F-VENDEMPR-VENDEDO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-VENDEMPR-VENDEDO.
           IF VENDEMPR-VENDEDO = ZEROS
              MOVE ZEROS TO VENDEMPR-CODMODA
              MOVE VENDEMPR-VENDEDO TO WI-Z12C
              MOVE VENDEMPR-CODMODA TO WI-Z3
              DISPLAY WI-Z12C LINE 16 POSITION 21
              WI-Z3           LINE 17 POSITION 21
            GO TO F-VENDEMPR-VENDEDO.

           MOVE VENDEMPR-VENDEDO TO CODIM.
           PERFORM I-LEE-CNNITCED-I00 THRU F-LEE-CNNITCED-I00.
           IF FIN-ARCH-CNNITCED = 1
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              " N.I.T no existe en Tabla de NITS"
              LINE 25 POSITION 01 BEEP BLINK
              PERFORM ACEPTA-TRUCO
              GO I-VENDEMPR-VENDEDO.
           DISPLAY NOMBM  LINE 16 POSITION 41 SIZE 30.
       F-VENDEMPR-VENDEDO. EXIT.

       I-VENDEMPR-CODMODA.
           IF VENDEMPR-VENDEDO = ZEROS
            GO F-VENDEMPR-CODMODA.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           " [F1] AYUDA " LINE 25 POSITION 01 REVERSE
           DISPLAY VENDEMPR-CODMODA LINE 17 POSITION 21
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT VENDEMPR-CODMODA LINE 17 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE VENDEMPR-CODMODA TO WI-Z3 MODACONT-CODI
           DISPLAY WI-Z3 LINE 17 POSITION 21
      *----------------------------------------------------------------*
      *   se mueve vendempr-vendedo a wk-total-concepto quien trabaja  *
      *   de variable auxiliar para conservar la cedula digitada despues
      *   que el usuario invoque la ayuda.                             *
      *----------------------------------------------------------------*
           IF F1
            MOVE VENDEMPR-VENDEDO TO WK-TOTAL-CONCEPTO
            PERFORM I-AYUDA-MODACONT THRU F-AYUDA-MODACONT UNTIL ESC
            PERFORM I-WINDOW-FAC        THRU F-WINDOW-FAC
            PERFORM I-MUESTRA-FACTURA   THRU F-MUESTRA-FACTURA
            GO I-VENDEMPR-VENDEDO.
           IF CUP OR ESC GO F-VENDEMPR-CODMODA.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
            GO I-VENDEMPR-CODMODA.
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Codigo modalidad contrato vendedor no existe ..."
              LINE 25 POSITION 01
              PERFORM ACEPTA-TRUCO
              GO I-VENDEMPR-CODMODA.
           MOVE    MODACONT-CODI  TO   WI-Z3
           DISPLAY WI-Z3          LINE 17 POSITION 21
           MOVE MODACONT-PRC-PAGO TO   WI-Z3P3
           DISPLAY WI-Z3P3        LINE 17 POSITION 25
           MOVE MODACONT-PRC-ADMI TO   WI-Z3P3
           DISPLAY WI-Z3P3        LINE 17 POSITION 33.
       F-VENDEMPR-CODMODA. EXIT.
      *
       I-GENERA-FACTURA.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE    ZEROS                TO   WK-EXCEPTION
           IF W-OPCION = "A" MOVE "S" TO WIN-GENERA-FACTURA.
           DISPLAY WIN-GENERA-FACTURA   LINE 17 POSITION 58
           ACCEPT  WIN-GENERA-FACTURA   LINE 17 POSITION 58
                                        UPDATE PROMPT TAB NO BEEP
                                        ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
           IF AF10
            MOVE 51 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-GENERA-FACTURA.
           IF  DEL
            MOVE SPACES TO WIN-GENERA-FACTURA
            DISPLAY WIN-GENERA-FACTURA LINE 17 POSITION 58
            GO F-GENERA-FACTURA.
           DISPLAY WIN-GENERA-FACTURA   LINE 17 POSITION 58
           IF  CUP OR ESC   GO F-GENERA-FACTURA.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF WIN-GENERA-FACTURA = "S" OR
              WIN-GENERA-FACTURA = "N"
              NEXT SENTENCE
           ELSE
              GO I-GENERA-FACTURA.
           IF F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
              GO I-GENERA-FACTURA.
       F-GENERA-FACTURA. EXIT.
      *
       I-PERIODO-FACT.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           IF W-OPCION = "A"
            MOVE ZEROS TO WIN-FACTURA
            MOVE 77    TO WIN-ESTA-FACTURA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "Shift+F2 Si es PRIMERA SERIE"
           LINE 25 POSITION 01 REVERSE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-FACTURA      LINE 18 POSITION 21
                   WIN-ESTA-FACTURA LINE 18 POSITION 23
           ACCEPT WN-OPCION LINE 18 POSITION 21
                            UPDATE PROMPT TAB NO BEEP
                         ON EXCEPTION WK-EX
                            PERFORM 999-EXCEPTION.
           IF SF2
              MOVE    ZEROS  TO  WK-EXCEPTION
              DISPLAY WIN-FACTURA LINE 18 POSITION 21
              MOVE    0  TO WIN-FACTURA
              DISPLAY WIN-FACTURA LINE 18 POSITION 21.
      *    DISPLAY SPACES SIZE 2 LINE 18 POSITION 40.
           IF  CUP OR ESC  GO F-PERIODO-FACT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F5 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-PERIODO-FACT.
           IF AF8
            PERFORM I-RECIBE-PERIODO-F THRU F-RECIBE-PERIODO-F.
       F-PERIODO-FACT. EXIT.
      *
       I-RECIBE-PERIODO-F.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT WIN-FACTURA LINE 18 POSITION 58
                              UPDATE PROMPT TAB NO BEEP
                           ON EXCEPTION WK-EX
                              PERFORM 999-EXCEPTION
           DISPLAY WIN-FACTURA LINE 18 POSITION 58.
           IF AF10
            MOVE 52 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-RECIBE-PERIODO-F.
           IF  CUP OR ESC  GO F-RECIBE-PERIODO-F.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
      * Ahora para recibir el indicador paso FACTURA DEFINITIVA
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-ESTA-FACTURA LINE 18 POSITION 23
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION
           DISPLAY WIN-ESTA-FACTURA LINE 18 POSITION 23.
           IF AF10
            MOVE 53 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-RECIBE-PERIODO-F.
           IF  CUP OR ESC  GO F-RECIBE-PERIODO-F.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-RECIBE-PERIODO-F. EXIT.
      *
       I-PLAZO-FACTURA.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE ZEROS                 TO   WK-EXCEPTION
           MOVE WIN-PLAZO-FACTURA     TO   WI-Z4
           DISPLAY WI-Z4              LINE 19 POSITION 21
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-PLAZO-FACTURA   LINE 19 POSITION 21
                                      UPDATE PROMPT CONVERT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    WIN-PLAZO-FACTURA  TO   WI-Z4
           DISPLAY WI-Z4              LINE 19 POSITION 21
           DISPLAY "Dias"             LINE 19 POSITION 26
           IF AF10
            MOVE 54 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-PLAZO-FACTURA.
           IF  CUP OR ESC  GO F-PLAZO-FACTURA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WIN-PLAZO-FACTURA = ZEROS
             GO I-PLAZO-FACTURA.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-PLAZO-FACTURA.
       F-PLAZO-FACTURA. EXIT.
      *
       I-REGIMEN-COMUN.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY "Regimen comun ? [S] Si [N] No "
           LINE 25 POSITION 1 REVERSE
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WIN-REGIMEN-COMUN LINE 19 POSITION 58
           ACCEPT  WIN-REGIMEN-COMUN LINE 19 POSITION 58
                                     UPDATE PROMPT TAB NO BEEP
                                  ON EXCEPTION WK-EX
                                     PERFORM 999-EXCEPTION.
           DISPLAY WIN-REGIMEN-COMUN LINE 19 POSITION 58
           IF AF10
            MOVE 55 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC     THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-REGIMEN-COMUN.
           IF CUP OR ESC GO F-REGIMEN-COMUN.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF WIN-REGIMEN-COMUN = "S" OR
              WIN-REGIMEN-COMUN = "N"
            NEXT SENTENCE
           ELSE
            GO I-REGIMEN-COMUN.
           IF F4 AND W-OPCION = "A" OR
              F5 AND W-OPCION = "A" OR
              F6 AND W-OPCION = "A"
            GO I-REGIMEN-COMUN.
       F-REGIMEN-COMUN. EXIT.

       I-POLIZA-VIDA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY "[S] Pขliza de vida Cobertura excluyente"
                      LINE 24 POSITION  6  REVERSE
           DISPLAY "[N] Pขliza de vida con cobertura Parcial"
                      LINE 25 POSITION 06  REVERSE
           MOVE ZEROS                   TO   WK-EXCEPTION
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY WIN-POLIZA-VIDA      LINE 21 POSITION 21
           IF WIN-POLIZA-VIDA = SPACES
              MOVE "N"   TO WIN-POLIZA-VIDA.
           ACCEPT  WIN-POLIZA-VIDA      LINE 21 POSITION 21
                                        UPDATE PROMPT TAB NO BEEP
                                        ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
           DISPLAY WIN-POLIZA-VIDA      LINE 21 POSITION 21
           IF AF10
            MOVE 56 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
                    UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW    THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC     THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB     THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-POLIZA-VIDA.
           IF  CUP OR ESC  GO F-POLIZA-VIDA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF WIN-POLIZA-VIDA    = "S" OR
              WIN-POLIZA-VIDA    = "N"
              NEXT SENTENCE
           ELSE
               GO I-POLIZA-VIDA.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-POLIZA-VIDA.
       F-POLIZA-VIDA. EXIT.

       I-RIESGO-POLIZA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-AYUDA-TBRIEPOL THRU F-AYUDA-TBRIEPOL.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE WIN-PORC-POLIZA-VIDA TO WI-Z2.
           DISPLAY WI-Z2          LINE 21 POSITION 58.
           ACCEPT WIN-PORC-POLIZA-VIDA LINE 21 POSITION 58
                                  UPDATE PROMPT TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           MOVE WIN-PORC-POLIZA-VIDA TO WI-Z2 TIPO-TBRIEPOL.
           DISPLAY WI-Z2          LINE 21 POSITION 58.
            PERFORM I-LLAMA-WINDOW    THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC     THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB     THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
           IF AF10
            MOVE 57 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
                    UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW    THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC     THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB     THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-RIESGO-POLIZA.
           IF CUP OR ESC GO F-RIESGO-POLIZA.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-RIESGO-POLIZA.
           PERFORM   I-LEE-TBRIEPOL    THRU    F-LEE-TBRIEPOL.
           IF FIN-ARCH-TBRIEPOL = 1
              DISPLAY "ERROR - FACTOR DE RIESGO NO EXISTE       "
              LINE 24 POSITION 6 REVERSE BLINK
              GO I-RIESGO-POLIZA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE WIN-PORC-POLIZA-VIDA TO WI-Z2
           DISPLAY WI-Z2          LINE 21 POSITION 58.
           MOVE    PORCEN-TBRIEPOL TO WI-Z2P3
           DISPLAY WI-Z2P3        LINE 21 POSITION 62.
           MOVE NUMERO-TBRIEPOL      TO WI-Z6
           DISPLAY WI-Z6          LINE 21 POSITION 65.
       F-RIESGO-POLIZA. EXIT.
      *
       I-FACTURA-DETALLADA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY "[S] Varios renglones de detalle y subtotal....  "
                      LINE 24 POSITION  6 REVERSE
           DISPLAY "[N] Solo imprime Subtotales                     "
                      LINE 25 POSITION 06 REVERSE

           IF WIN-FACTURA-DETALLADA = SPACE
             MOVE  "S"  TO  WIN-FACTURA-DETALLADA.

           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.

           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY WIN-FACTURA-DETALLADA LINE 22 POSITION 21
           ACCEPT  WIN-FACTURA-DETALLADA LINE 22 POSITION 21
                                         UPDATE PROMPT TAB NO BEEP
                                      ON EXCEPTION WK-EX
                                         PERFORM 999-EXCEPTION.

           DISPLAY WIN-FACTURA-DETALLADA LINE 22 POSITION 21
           IF AF10
            MOVE 58 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
                    UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW    THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-FAC      THRU  F-WINDOW-FAC
            PERFORM I-DISPLAY-OPC     THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB     THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-FACTURA THRU  F-MUESTRA-FACTURA
            GO TO I-FACTURA-DETALLADA.

           IF  CUP OR ESC  GO F-FACTURA-DETALLADA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF WIN-FACTURA-DETALLADA = "S" OR
              WIN-FACTURA-DETALLADA = "N"
              NEXT SENTENCE
           ELSE
               GO I-FACTURA-DETALLADA.

           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
             GO I-FACTURA-DETALLADA.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
       F-FACTURA-DETALLADA. EXIT.
      *****************************************************************
      *    CAPTURA DE CAMPOS MODULO BANCO                             *
      *****************************************************************
       I-TPO-COMPBAN.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Comprobantes "
             LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS  TO   WK-EXCEPTION
           IF      W-OPCION = "A" AND
                   ( WIN-CODIM-BANCO = SPACES OR
                     WIN-CODIM-BANCO = ZEROS )
                   MOVE 03 TO WIN-CODIM-BANCO.
           IF      W-OPCION = "M" AND
                   ( WIN-CODIM-BANCO = SPACES OR
                     WIN-CODIM-BANCO = ZEROS )
                   MOVE 03 TO WIN-CODIM-BANCO.
           MOVE    ZEROS                TO   WK-EXCEPTION
           MOVE    WIN-CODIM-BANCO      TO   WI-Z2
           DISPLAY WI-Z2                LINE 11 POSITION 21
           IF AF10
            MOVE 59 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON      THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-TPO-COMPBAN.
           IF F1
            PERFORM I-HELP-COMPROBANTES THRU F-HELP-COMPROBANTES
              UNTIL ESC
            MOVE    ZEROS               TO   WK-EXCEPTION
            PERFORM I-WINDOW-CON        THRU F-WINDOW-CON
            PERFORM I-MUESTRA-BANCO     THRU F-MUESTRA-BANCO
            GO TO I-TPO-COMPBAN.
           MOVE    WIN-CODIM-BANCO      TO   WI-Z2
           DISPLAY WI-Z2                LINE 11 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC GO F-TPO-COMPBAN.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE WIN-CODIM-BANCO TO LLAVM1-COMP.
           PERFORM I-LEE-COMPRO THRU F-LEE-COMPRO.
           DISPLAY   NOMBM-COMP LINE 11 POSITION 25 SIZE 17
           IF  FIN-OK = 1 GO I-TPO-COMPBAN.
           IF  F4 AND W-OPCION = "A" OR F6 AND W-OPCION = "A"
               GO I-TPO-COMPBAN.
       F-TPO-COMPBAN. EXIT.
      *
       I-TIPO-PAGO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY "[1]"               LINE 25 POSITION  1 REVERSE
           DISPLAY "[2]"               LINE 25 POSITION 16 REVERSE
           DISPLAY "[3]"               LINE 25 POSITION 29 REVERSE
           DISPLAY "EFECTIVO"          LINE 25 POSITION  5 REVERSE.
           DISPLAY "CHEQUE"            LINE 25 POSITION 20 REVERSE.
           DISPLAY "CONSIGNACION"      LINE 25 POSITION 33 REVERSE.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS               TO       WK-EXCEPTION
           DISPLAY WIN-TIPO-PAGO       LINE 13 POSITION 21
           ACCEPT  WIN-TIPO-PAGO       LINE 13 POSITION 21
                                       UPDATE PROMPT TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           DISPLAY WIN-TIPO-PAGO       LINE 13 POSITION 21
           IF AF10
            MOVE 60 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-TIPO-PAGO.
           IF   CUP OR ESC   GO F-TIPO-PAGO.
           IF   F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WIN-TIPO-PAGO = 1   OR
               WIN-TIPO-PAGO = 2   OR
               WIN-TIPO-PAGO = 3
               NEXT SENTENCE
           ELSE
               GO TO I-TIPO-PAGO.
           IF DEL MOVE ZEROS TO WIN-TIPO-PAGO.
           IF WIN-TIPO-PAGO = 1
              DISPLAY "EFECTIVO   " LINE 13 POSITION 25.
           IF WIN-TIPO-PAGO = 2
              DISPLAY "CHEQUE     " LINE 13 POSITION 25.
           IF WIN-TIPO-PAGO = 3
              DISPLAY "CONSIGNACION" LINE 13 POSITION 25.
           IF WIN-TIPO-PAGO = 0 DISPLAY "        " LINE 13 POSITION 25.
           DISPLAY WIN-TIPO-PAGO                   LINE 13 POSITION 21
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-TIPO-PAGO.
       F-TIPO-PAGO. EXIT.
      *
       I-BANCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Bancos "
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS                        TO   WK-EXCEPTION
           DISPLAY WIN-COD-BANCO   LINE 14 POSITION 21
           ACCEPT  WIN-COD-BANCO   LINE 14 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           IF AF10
            MOVE 61 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-BANCO.
           IF  F1
            PERFORM I-HELP-BANCOS   THRU  F-HELP-BANCOS UNTIL ESC
            MOVE    ZEROS           TO    WK-EXCEPTION
            PERFORM I-WINDOW-CON    THRU  F-WINDOW-CON
            PERFORM I-MUESTRA-BANCO THRU  F-MUESTRA-BANCO
            GO I-BANCO.
           IF  DEL  MOVE ZEROS TO WIN-COD-BANCO.
           DISPLAY WIN-COD-BANCO LINE 14 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC  GO F-BANCO.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE WIN-COD-BANCO  TO   BANC-CODIGO.
           PERFORM I-LEE-BANCO THRU F-LEE-BANCO.
           IF   FIN-OK = 1
             MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
             MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
             DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING
             DISPLAY "BANCO NO EXISTE" LINE 25 POSITION 01 REVERSE
             MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
             MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
             DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING
             GO I-BANCO.
           IF WIN-COD-BANCO > 99 GO TO I-BANCO.
           DISPLAY BANC-NOMBRE LINE 14 POSITION 27 SIZE 13.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
             GO I-BANCO.
       F-BANCO. EXIT.
      *
       I-CORPO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Corporaciones "
            LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS                        TO   WK-EXCEPTION
           DISPLAY WIN-COD-BANCO                LINE 14 POSITION 21
           ACCEPT  WIN-COD-BANCO   LINE 14 POSITION 21
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           IF AF10
            MOVE 61 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-CORPO.
           IF  F1
             PERFORM I-HELP-CORPORACIONES THRU F-HELP-CORPORACIONES
              UNTIL ESC
             MOVE    ZEROS               TO   WK-EXCEPTION
             PERFORM I-WINDOW-CON        THRU F-WINDOW-CON
             PERFORM I-MUESTRA-BANCO     THRU F-MUESTRA-BANCO
             GO I-CORPO.
           IF  DEL  MOVE ZEROS   TO  WIN-COD-BANCO.
           DISPLAY WIN-COD-BANCO  LINE 14 POSITION 21
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC  GO F-CORPO.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ZEROS TO FIN-OK.
           MOVE WIN-COD-BANCO  TO   CORP-CODIGO.
           PERFORM I-LEE-CORPO THRU F-LEE-CORPO.
           IF FIN-OK = 1
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
            DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING
            DISPLAY "CORPORACION NO EXISTE"
             LINE 25 POSITION 01 REVERSE
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
            DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING
            GO I-CORPO.
           DISPLAY CORP-NOMBRE LINE 14 POSITION 27 SIZE 13.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
             GO I-CORPO.
       F-CORPO. EXIT.
      *
       I-CTA-CTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " [ F1 ]  Help Corporaciones "
           LINE 25 POSITION 01 REVERSE
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF   WIN-TIPO-PAGO = 1   GO   F-CTA-CTE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           PERFORM I-WINDOW-CTAS  THRU F-WINDOW-CTAS.
           PERFORM I-DISPLAY-CTAS THRU F-DISPLAY-CTAS
             UNTIL I > 5.
           MOVE    ZEROS        TO   WK-EXCEPTION
           DISPLAY WIN-TIP-CTA  LINE 15 POSITION 21
           ACCEPT  WIN-TIP-CTA  LINE 15 POSITION 21
                                UPDATE PROMPT TAB NO BEEP
                             ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
           PERFORM I-WINDOW-CON      THRU F-WINDOW-CON
           PERFORM I-MUESTRA-BANCO THRU F-MUESTRA-BANCO
           IF AF10
            MOVE 62 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-CTA-CTE.
           IF  CUP OR ESC  GO F-CTA-CTE.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  WIN-TIP-CTA < 01 OR
               WIN-TIP-CTA > 05 GO I-CTA-CTE.
           IF WIN-TIPO-PAGO = 2
              IF  BANC-CTA-CTE (WIN-TIP-CTA) = ZEROS
                  GO I-CTA-CTE
              ELSE
                  NEXT SENTENCE
           ELSE
            IF WIN-TIPO-PAGO = 3
              IF  CORP-CTA-CTE (WIN-TIP-CTA) = ZEROS
                  GO I-CTA-CTE.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-CTA-CTE.
       F-CTA-CTE. EXIT.
      *
       I-ULTACTLIB.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION.
      *    DISPLAY SPACES        LINE 13 POSITION 59 SIZE 18
           DISPLAY "Shift + F2 Modificar Datos"
           LINE 24 POSITION 1 REVERSE
           ACCEPT XX LINE 13 POSITION 58
                     UPDATE PROMPT TAB NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF AF10
            MOVE 63 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-ULTACTLIB.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-ULTACTLIB.
           IF SF2   NEXT SENTENCE
           ELSE   GO TO F-ULTACTLIB.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
      *    DISPLAY SPACES        LINE 13 POSITION 59 SIZE 18
           DISPLAY "AฅO:" LINE 13 POSITION 59
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-ULT-ACT-LIB-ANO LINE 13 POSITION 64
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           DISPLAY WIN-ULT-ACT-LIB-ANO LINE 13 POSITION 64.
           DISPLAY "MES:" LINE 13 POSITION 69
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-ULT-ACT-LIB-MES LINE 13 POSITION 74
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           DISPLAY WIN-ULT-ACT-LIB-MES LINE 13 POSITION 74.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-ULTACTLIB.
       F-ULTACTLIB. EXIT.
      *
       I-CIERRE-CONT.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION.
      *    DISPLAY SPACES        LINE 14 POSITION 59 SIZE 18
           DISPLAY "Shift + F2 Modificar Datos"
           LINE 24 POSITION 1 REVERSE
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT XX LINE 14 POSITION 58
                     UPDATE PROMPT TAB NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF AF10
            MOVE 64 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-CIERRE-CONT.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-CIERRE-CONT.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-CIERRE-CONT.
           IF SF2   NEXT SENTENCE
           ELSE   GO TO F-CIERRE-CONT.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY "AฅO:" LINE 14 POSITION 59
           DISPLAY WIN-CIERRE-MES-CONT-ANO LINE 14 POSITION 64
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-CIERRE-MES-CONT-ANO LINE 14 POSITION 64
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           DISPLAY WIN-CIERRE-MES-CONT-ANO LINE 14 POSITION 64.
           DISPLAY "MES:" LINE 14 POSITION 69
           DISPLAY WIN-CIERRE-MES-CONT-MES LINE 14 POSITION 74
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-CIERRE-MES-CONT-MES LINE 14 POSITION 74
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           DISPLAY WIN-CIERRE-MES-CONT-MES LINE 14 POSITION 74.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-CIERRE-CONT.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-CIERRE-CONT.
       F-CIERRE-CONT. EXIT.

       I-MODULO-CONTABLE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY "(S) Contabiliza Automaticamente la nomina"
                  LINE 24 POSITION 1 REVERSE
           MOVE ZEROS TO WK-EXCEPTION.
           IF WIN-MODULO-CONTABLE = SPACE
            MOVE "S" TO WIN-MODULO-CONTABLE.
           DISPLAY WIN-MODULO-CONTABLE LINE 15 POSITION 58
           ACCEPT XX LINE 15 POSITION 58
                     UPDATE PROMPT TAB NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           DISPLAY WIN-MODULO-CONTABLE LINE 15 POSITION 58
           IF AF10
            MOVE 65 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-MODULO-CONTABLE.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-MODULO-CONTABLE.
           IF F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
            GO I-MODULO-CONTABLE.
           IF SF2 NEXT SENTENCE
           ELSE GO TO F-MODULO-CONTABLE.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT WIN-MODULO-CONTABLE LINE 15 POSITION 58
                                      UPDATE PROMPT TAB NO BEEP
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           IF WIN-MODULO-CONTABLE = "S" OR
              WIN-MODULO-CONTABLE = "N"
              NEXT SENTENCE
           ELSE
              GO I-MODULO-CONTABLE.
           DISPLAY WIN-MODULO-CONTABLE LINE 15 POSITION 58.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-MODULO-CONTABLE.
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-CIERRE-CONT.
       F-MODULO-CONTABLE. EXIT.
      *
       I-PRILEG.
      *    IF WIN-PRC-PRESTACIONAL > 40 GO F-PRILEG.
           MOVE    ZEROS               TO   WK-EXCEPTION
           DISPLAY WIN-TIPO-LIQ-PRIMA  LINE 16 POSITION 58
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           ACCEPT  WIN-TIPO-LIQ-PRIMA  LINE 16 POSITION 58
                                       UPDATE PROMPT TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           IF  WIN-TIPO-LIQ-PRIMA = SPACES
             MOVE "P1" TO WIN-TIPO-LIQ-PRIMA.
           DISPLAY WIN-TIPO-LIQ-PRIMA  LINE 16 POSITION 58
           IF AF10
            MOVE 66 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-PRILEG.
           IF  CUP OR ESC GO F-PRILEG.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-PRILEG.
       F-PRILEG. EXIT.

       I-VACACIONES.
           MOVE    ZEROS               TO   WK-EXCEPTION
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-TIPO-LIQ-VACAC  LINE 17 POSITION 58
           ACCEPT  WIN-TIPO-LIQ-VACAC  LINE 17 POSITION 58
                                       UPDATE PROMPT TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           IF  WIN-TIPO-LIQ-VACAC = SPACES
             MOVE "P1" TO WIN-TIPO-LIQ-VACAC.
           DISPLAY WIN-TIPO-LIQ-VACAC  LINE 17 POSITION 58
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF AF10
            MOVE 67 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-VACACIONES.
           IF  CUP OR ESC GO F-VACACIONES.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
               GO I-VACACIONES.
       F-VACACIONES. EXIT.

       I-CESANTIAS.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "P1: Promedio total devengado ultimo aคo"
           LINE 24 POSITION 01 REVERSE
           "P2: Promedio Horas Extras    "
           LINE 24 POSITION 46 REVERSE
           "P3: Todo lo devengado         "
           LINE 25 POSITION 01 REVERSE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE    ZEROS                TO   WK-EXCEPTION
           DISPLAY WIN-TIPO-LIQ-CESAN   LINE 18 POSITION 58
           ACCEPT  WIN-TIPO-LIQ-CESAN   LINE 18 POSITION 58
                                        UPDATE PROMPT TAB NO BEEP
                                     ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
           IF  WIN-TIPO-LIQ-CESAN = SPACES
             MOVE "P1" TO WIN-TIPO-LIQ-CESAN.
           DISPLAY WIN-TIPO-LIQ-CESAN   LINE 18 POSITION 58
           IF AF10
            MOVE 68 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-CESANTIAS.
           IF  CUP OR ESC GO F-CESANTIAS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
             GO I-CESANTIAS.
       F-CESANTIAS.
      *
       I-WIN-CESAN-PROM.
           IF  WIN-CESAN-PROM     =  SPACES
               MOVE "N" TO WIN-CESAN-PROM.
           IF  W-OPCION        =  "A"
               MOVE "N" TO WIN-CESAN-PROM.
           DISPLAY WIN-CESAN-PROM        LINE 19 POSITION 58
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-CESAN-PROM       LINE 19 POSITION 58
           MOVE    ZEROS                  TO WK-EXCEPTION
           ACCEPT  WIN-CESAN-PROM       LINE 19 POSITION 58
                                        UPDATE PROMPT TAB NO BEEP
                                        ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
           DISPLAY WIN-CESAN-PROM       LINE 19 POSITION 58
           IF AF10
            MOVE 69 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW
            PERFORM I-WINDOW-CON     THRU  F-WINDOW-CON
            PERFORM I-DISPLAY-OPC    THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU  F-MUESTRA-CAB
            PERFORM I-MUESTRA-BANCO  THRU  F-MUESTRA-BANCO
            GO TO I-WIN-CESAN-PROM.
           IF  CUP OR ESC GO F-WIN-CESAN-PROM.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE .
           IF  F4 AND W-OPCION = "A" OR F5 AND W-OPCION = "A"
             GO I-WIN-CESAN-PROM.
       F-WIN-CESAN-PROM.
      *
       I-START-COMP.
           START CNCOMPTE KEY NOT LESS LLAVM1-COMP.
       F-START-COMP.
      *
       I-DISPLAY-COMP.
           PERFORM   I-LEE-COMP THRU F-LEE-COMP.
           IF   FIN-OK = 1
                DISPLAY "**" LINE     I-LINEA POSITION 37 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 43 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-COMP.
           DISPLAY   CODIM-COMP       LINE I-LINEA POSITION 37.
           DISPLAY   NOMBM-COMP       LINE I-LINEA POSITION 43.
           IF I = 2  MOVE CODIM-COMP TO W-COMPB.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-COMP.

       I-DISPLAY-CTAS.
           MOVE      I TO WI-Z2.
           DISPLAY   WI-Z2                 LINE I-LINEA POSITION 50.
           IF WIN-TIPO-PAGO = 2
              MOVE      BANC-CTA-CTE (I) TO   WI-Z12
              DISPLAY   WI-Z12             LINE I-LINEA POSITION 58
              ADD   1   TO  I
              ADD   1   TO  I-LINEA
              IF        BANC-CTA-CTE (I) = ZEROS
                        MOVE 6 TO I
              ELSE
                        NEXT SENTENCE
           ELSE
           IF WIN-TIPO-PAGO = 3
              MOVE      CORP-CTA-CTE (I) TO   WI-Z12
              DISPLAY   WI-Z12             LINE I-LINEA POSITION 58
              ADD   1   TO  I
              ADD   1   TO  I-LINEA
              IF        CORP-CTA-CTE (I) = ZEROS
                        MOVE 6 TO I.
       F-DISPLAY-CTAS.
      *
       I-START-BCOS.
           START NOMIBANK KEY NOT LESS BANC-CLAVE.
       F-START-BCOS.
      *
       I-DISPLAY-BCOS.
           PERFORM   I-LEE-BANCOS     THRU F-LEE-BANCOS.
           IF   FIN-OK = 1
                DISPLAY "**" LINE     I-LINEA POSITION 50 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 58 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-BCOS.
           DISPLAY   BANC-CODIGO      LINE I-LINEA POSITION 50.
           DISPLAY   BANC-NOMBRE      LINE I-LINEA POSITION 58.
           IF I = 2  MOVE BANC-CODIGO TO W-BANCO.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-BCOS.
      *
       I-START-CORP.
           START NOMICORP KEY NOT LESS CORP-CLAVE.
       F-START-CORP.
      *
       I-DISPLAY-CORP.
           PERFORM   I-LEE-CORPOS     THRU F-LEE-CORPOS.
           IF   FIN-OK = 1
                DISPLAY "**" LINE     I-LINEA POSITION 50 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 58 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-CORP.
           DISPLAY   CORP-CODIGO      LINE I-LINEA POSITION 50.
           DISPLAY   CORP-NOMBRE      LINE I-LINEA POSITION 58.
           IF I = 2  MOVE CORP-CODIGO TO W-BANCO.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CORP.
      *
       I-GRABACION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <A> ADICIONAR "
             LINE 25 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
             LINE 25 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
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
           PERFORM I-LEE-NIT   THRU F-LEE-NIT.
           PERFORM I-AUDITORIA-EMPRESAS THRU F-AUDITORIA-EMPRESAS.
           WRITE REC-EMPRESAS
           PERFORM I-GRABAR-VENDEMPR THRU F-GRABAR-VENDEMPR.
           PERFORM I-CREA-ARCHIVOS THRU F-CREA-ARCHIVOS.
       F-GRABACION. EXIT.
      *
       I-GRABAR-VENDEMPR.
           PERFORM I-AUDITORIA-VENDEMPR THRU F-AUDITORIA-VENDEMPR.
           IF VENDEMPR-VENDEDO NOT = WRK-VENDEMPR-VENDEDO
            PERFORM I-GRABA-VENDEMPR THRU F-GRABA-VENDEMPR
            IF WRK-VENDEMPR-VENDEDO = ZEROS
             NEXT SENTENCE
            ELSE
             PERFORM I-BORRA-VENDEDOR THRU F-BORRA-VENDEDOR
            END-IF
           END-IF
           IF VENDEMPR-CODMODA NOT = WRK-VENDEMPR-CODMODA AND
              VENDEMPR-VENDEDO     = WRK-VENDEMPR-VENDEDO AND
              VENDEMPR-EMPRESA     = WRK-VENDEMPR-EMPRESA
            PERFORM I-REGRABA-VENDEMPR THRU F-REGRABA-VENDEMPR.
       F-GRABAR-VENDEMPR. EXIT.
      *
       I-REGRABA-VENDEMPR.
           REWRITE REG-VENDEMPR INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "No puede regrabar en archivo vendedor empresa ..."
            LINE 25 POSITION 01 BEEP
            ACCEPT RES-OK LINE 25 POSITION 79 OFF TAB NO BEEP.
       F-REGRABA-VENDEMPR. EXIT.
      *
       I-GRABA-VENDEMPR.
           IF VENDEMPR-VENDEDO = ZEROS
            GO TO F-GRABA-VENDEMPR.
           MOVE WIN-CODIGO-EMPRESA TO VENDEMPR-EMPRESA
                                      VENDEMPR-EMPRE01.

           WRITE REG-VENDEMPR INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "No puede grabar en archivo vendedor empresa ..."
            LINE 25 POSITION 01 BEEP
            ACCEPT RES-OK LINE 25 POSITION 79 OFF TAB NO BEEP.
       F-GRABA-VENDEMPR. EXIT.
      *
       I-BORRA-VENDEDOR.
           MOVE WRK-VENDEMPR-VENDEDO TO VENDEMPR-VENDEDO
           MOVE WRK-VENDEMPR-EMPRESA TO VENDEMPR-EMPRESA.
           PERFORM I-LEE-VENDEMPR THRU F-LEE-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
            GO F-BORRA-VENDEDOR.
           DELETE VENDEMPR INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "NO PUEDE BORRAR " LINE 25 POSITION 01 BEEP
            REG-VENDEMPR       LINE 25 POSITION 17
            ACCEPT RES-OK LINE 25 POSITION 79 OFF TAB NO BEEP.
       F-BORRA-VENDEDOR. EXIT.
      *
       I-LEE-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           READ VENDEMPR WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-LEE-VENDEMPR. EXIT.
      *
       I-CREA-ARCHIVOS.
           PERFORM I-LABEL-CONTRATO  THRU F-LABEL-CONTRATO.
           PERFORM I-STATUS-CONTRATO THRU F-STATUS-CONTRATO.

           PERFORM I-LABEL-TABLAEMP THRU F-LABEL-TABLAEMP.
           OPEN OUTPUT TABLAEMP CONTRATO.
           CLOSE TABLAEMP CONTRATO.
       F-CREA-ARCHIVOS. EXIT.
      *
       I-REGRABACION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <M> MODIFICAR "
           LINE 25 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
           LINE 25 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP OR ESC OR F1  GO F-REGRABACION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "M"   GO I-REGRABACION.
           PERFORM I-LEE-NIT       THRU     F-LEE-NIT.
           PERFORM I-AUDITORIA-EMPRESAS THRU F-AUDITORIA-EMPRESAS.
           REWRITE REC-EMPRESAS.
           PERFORM I-GRABAR-VENDEMPR THRU F-GRABAR-VENDEMPR.
       F-REGRABACION. EXIT.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <R> RETIRAR "
           LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> CANCELAR "
           LINE 24 POSITION 25 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
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
               PERFORM I-MUESTRA-BANCO    THRU F-MUESTRA-BANCO.
           IF SF9
               DELETE EMPRESAS
               DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
               "Registro de tabla empresas ha sido eliminado..."
               LINE 25 POSITION 01 REVERSE BLINK
               GO TO F-ELIMINAR.
           IF  W-TRUCO                  NOT =  "R"
               GO I-ELIMINAR.
       F-ELIMINAR. EXIT.
      *
       I-CONSULTAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " Presione <> Consultar "
           LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> OPCIONES "
              LINE 24 POSITION 27 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           IF  CDN
               PERFORM I-LEER-EMPRESAS THRU F-LEER-EMPRESAS
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
               PERFORM I-MUESTRA-BANCO    THRU F-MUESTRA-BANCO.
           IF  F1
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               GO F-CONSULTAR
           ELSE
               GO I-CONSULTAR.
       F-CONSULTAR.
      *
       I-FIN-CONSULTAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           DISPLAY " NO HAY MAS REGISTROS "
             LINE 24 POSITION 1 REVERSE
           DISPLAY " <F1> OPCIONES "
           LINE 24 POSITION 25 REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           IF  F2   PERFORM I-FIN-MODE THRU F-FIN-MODE .
       F-FIN-CONSULTAR. EXIT.
      *
       I-WINDOW-CAB.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING.
           DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออป" LINE 01 POSITION 01
           DISPLAY "บId. Interna.:                       System Winner
      -            "     Fecha..:              บ" LINE 02 POSITION 01
           DISPLAY "บNIT.CC.TI...:                        Opcion :
      -            "     Cesante:              บ" LINE 03 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 04 POSITION 01
           DISPLAY "บRazon Social.....:
      -            "                           บ" LINE 05 POSITION 01
           DISPLAY "บRepres.legal.....:
      -            "                           บ" LINE 06 POSITION 01
           DISPLAY "บDireccion........:
      -            "                           บ" LINE 07 POSITION 01
           DISPLAY "บDepartamento.....:
      -            "                           บ" LINE 08 POSITION 01
           DISPLAY "บCiudad...........:                          Tel...:
      -            "                           บ" LINE 09 POSITION 01
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
           DISPLAY "บ Comprobante:                            F5 Cartera
      -     "       F6 Forma de Pago    บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -     "อออออออออออออออออออออออออออน"          LINE 12 POSITION 01
           DISPLAY "บSerie.............:                    Fase Cierre.
      -     "...:                       บ"          LINE 13 POSITION 01
           DISPLAY "บAgrupacion........:                    Titulos.....
      -     "...:                       บ"          LINE 14 POSITION 01
           DISPLAY "บSue.minimo ant....:                    Aux.Trans.An
      -     "t..:                       บ"          LINE 15 POSITION 01
           DISPLAY "บSue.minimo act....:                    Aux.Trans Ac
      -     "t..:                       บ"          LINE 16 POSITION 01
           DISPLAY "บValor Dia.........:                    Ciclo Pago..
      -     "...:                       บ"          LINE 17 POSITION 01
           DISPLAY "บReporte Horas.....:                    Valor Hora..
      -     "...:                       บ"          LINE 18 POSITION 01
           DISPLAY "บLapso liquidacion.:                    Riesgo Prof.
      -     "...:                       บ"          LINE 19 POSITION 01
           DISPLAY "บContrato Anual....:                    Duracion dia
      -     "s..:                       บ"          LINE 20 POSITION 01
           DISPLAY "บPago Parafiscales.:                    Caja Compen
      -     "sac:                       บ"          LINE 21 POSITION 01
           DISPLAY "บPago S.S.Social...:                    Fondo Cesant
      -     "ias:                       บ" LINE 22 POSITION 01 HIGH
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
           DISPLAY "บ Comprobante:                          F4 Laborales
      -     "       F6 Forma de pago    บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน"   LINE 12 POSITION 01
           DISPLAY "บ%&Cta Aportes PFs.:                    % Outsourcin
      -     "g...:                      บ"         LINE 13 POSITION 01
           DISPLAY "บ%&Cta Antic.Rfte..:                    Vr.limite RF
      -     "....:                      บ"         LINE 14 POSITION 01
           DISPLAY "บ%&Cta I.V.A.......:                    Cta Deud.Com
      -     "erc.:                      บ"         LINE 15 POSITION 01
           DISPLAY "บNo.Id Asesor Empr.:
      -     "                           บ"         LINE 16 POSITION 01
           DISPLAY "บModalidad.Asesoria:                    Facturable..
      -     "S/N.:                      บ"         LINE 17 POSITION 01
           DISPLAY "บSerie Cartera.....:                    Periodo.....
      -     "....:                      บ"         LINE 18 POSITION 01
           DISPLAY "บPlazo Factura.....:                    Regimen Comu
      -     "n...:                      บ"         LINE 19 POSITION 01
           DISPLAY "บ*POLIZA DE SEGURO COLECTIVO*
      -     "                           บ"         LINE 20 POSITION 01
           DISPLAY "บCobertura Poliza..:                    Tipo Poliza
      -     "vida:                      บ"  LINE 21 POSITION 01 HIGH
           DISPLAY "บAplica Detalle Fra:
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
           DISPLAY "บ Comprobante:                          F4 Laborales
      -     "      F6 Forma de Pago     บ" LINE 11 POSITION 01
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออน" LINE 12 POSITION 01
           DISPLAY "บTipo de Pago......:                   Libros Oficia
      -     "les.:                      บ"          LINE 13 POSITION 01
           DISPLAY "บEntidad Financiera:                   Cierre Contab
      -     "le..:                      บ"          LINE 14 POSITION 01
           DISPLAY "บCuenta Corriente..:
      -     "                           บ"          LINE 15 POSITION 01
           DISPLAY "บCodigo P.U.C......:
      -     "                           บ"          LINE 16 POSITION 01
           DISPLAY "บ
      -     "                           บ"          LINE 17 POSITION 01
           DISPLAY "บ
      -     "                           บ"          LINE 18 POSITION 01
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
       F-WINDOW-CON. EXIT.
      *
       I-WINDOW-CTAS.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  "ษออออออหอออออออออออออออป" LINE 12 POSITION 47
                                         CONTROL SG-CONTROL-STRING
                                         REVERSE.
           DISPLAY  "บTipo  บ    Cuenta     บ" LINE 13 POSITION 47
                                                       REVERSE
           DISPLAY  "ฬออออออฮอออออออออออออออน" LINE 14 POSITION 47
                                                       REVERSE
           DISPLAY  "บ      บ               บ" LINE 15 POSITION 47
                                                       REVERSE
           DISPLAY  "บ      บ               บ" LINE 16 POSITION 47
                                                       REVERSE
           DISPLAY  "บ      บ               บ" LINE 17 POSITION 47
                                                       REVERSE
           DISPLAY  "บ      บ               บ" LINE 18 POSITION 47
                                                       REVERSE
           DISPLAY  "บ      บ               บ" LINE 19 POSITION 47
                                                       REVERSE
           DISPLAY  "ศออออออสอออออออออออออออผ" LINE 20 POSITION 47                                                    "
                                                       REVERSE.
       F-WINDOW-CTAS.
      *
       I-WINDOW-BCOS.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY  "ษอออออออออออออออออออออออออป" LINE 12 POSITION 47
                                            CONTROL SG-CONTROL-STRING
                                                       REVERSE.
           DISPLAY  "บCodigoบEntidad Financieraบ" LINE 13 POSITION 47
                                                       REVERSE.
           DISPLAY  "ฬออออออฮออออออออออออออออออน" LINE 14 POSITION 47
                                                       REVERSE.
           DISPLAY  "บ      บ                  บ" LINE 15 POSITION 47
                                                       REVERSE.
           DISPLAY  "บ      บ                  บ" LINE 16 POSITION 47
                                                       REVERSE.
           DISPLAY  "บ      บ                  บ" LINE 17 POSITION 47
                                                       REVERSE.
           DISPLAY  "บ      บ                  บ" LINE 18 POSITION 47
                                                       REVERSE.
           DISPLAY  "บ      บ                 บ" LINE 19 POSITION 47
                                                       REVERSE.
           DISPLAY  "ศออออออสออออออออออออออออออผ" LINE 20 POSITION 47
                                                       REVERSE.
       F-WINDOW-BCOS.
      *
      *
       I-EXISTE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "Registro ya Existe...de tecla para continuar."
           LINE 25 POSITION 01 REVERSE.
           PERFORM ACEPTA-TRUCO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "REGISTRO NO EXISTE... de tecla para continuar."
           LINE 25 POSITION 01 REVERSE.
           PERFORM ACEPTA-TRUCO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-NEXISTE. EXIT.
      ***********************************************************
      *       LECTURA DE ARCHIVOS                               *
      ***********************************************************
       I-LEE-CIUDAD.
           READ NOMICIUD WITH NO LOCK INVALID KEY MOVE 1 TO FIN-OK.
       F-LEE-CIUDAD. EXIT.
      *
       I-LEE-CONTDPTO.
           MOVE ZEROS TO FIN-ARCH-CONTDPTO.
           READ CONTDPTO WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-CONTDPTO.
       F-LEE-CONTDPTO. EXIT.
      *
       I-LEE-CJASUBSI.
           MOVE 0 TO SW-EOF
           READ CJASUBSI WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CJASUBSI. EXIT.
      *
       I-LEE-BANCO.
           READ NOMIBANK WITH NO LOCK INVALID KEY MOVE 1 TO FIN-OK.
       F-LEE-BANCO. EXIT.
      *
       I-LEE-BANCOS.
           READ NOMIBANK NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEE-BANCOS. EXIT.
      *
       I-LEE-CORPO.
           READ NOMICORP WITH NO LOCK INVALID KEY MOVE 1 TO FIN-OK.
       F-LEE-CORPO. EXIT.
      *
       I-LEE-CORPOS.
           READ NOMICORP NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEE-CORPOS. EXIT.
      *
       I-LEE-COMP.
           READ CNCOMPTE  NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEE-COMP. EXIT.
      *
       I-LEE-COMPRO.
           READ CNCOMPTE WITH NO LOCK INVALID KEY MOVE 1 TO FIN-OK.
       F-LEE-COMPRO. EXIT.
      *
       I-LEE-EMPRESAS.
           MOVE ZEROS TO SW-1.
           READ EMPRESAS WITH NO LOCK
            INVALID KEY MOVE 1 TO SW-1.
       F-LEE-EMPRESAS. EXIT.
      *
       I-LEER-EMPRESAS.
           READ EMPRESAS NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO SW-OK.
       F-LEER-EMPRESAS. EXIT.
      *
       I-LEE-MODACONT.
           MOVE ZEROS TO FIN-ARCH-MODACONT.
           READ MODACONT WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-MODACONT.
       F-LEE-MODACONT. EXIT.
      *
       I-START-VENDEMPR-01.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           START VENDEMPR KEY NOT LESS LLAVE-VENDEMPR-ALT01 INVALID KEY
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-START-VENDEMPR-01. EXIT.
      *
       I-LEE-C-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           READ VENDEMPR NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-LEE-C-VENDEMPR. EXIT.
      *
       I-LEE-HOJAVIDA.
           MOVE ZEROS TO FIN-ARCH-HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-HOJAVIDA.
       F-LEE-HOJAVIDA. EXIT.
      *
       I-LEE-TARIESGO.
           MOVE ZEROS TO FIN-ARCH-TARIESGO.
           READ TARIESGO WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-TARIESGO.
       F-LEE-TARIESGO. EXIT.

       I-LEE-TBRIEPOL.
           MOVE ZEROS TO FIN-ARCH-TBRIEPOL.
           READ TBRIEPOL WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-TBRIEPOL.
       F-LEE-TBRIEPOL. EXIT.
      *
       I-HELP-BANCOS.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO BANC-CLAVE FIN-OK.
           PERFORM I-START-BCOS   THRU F-START-BCOS.
           PERFORM I-WINDOW-BCOS  THRU F-WINDOW-BCOS.
           PERFORM I-DISPLAY-BCOS THRU F-DISPLAY-BCOS
             UNTIL I > 5.
        LEE-BCO.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                               UPDATE PROMPT
                               CONVERT
                               TAB
                               NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01 TO I
                MOVE 15 TO I-LINEA
                MOVE ZEROS TO FIN-OK
                MOVE W-BANCO TO BANC-CLAVE
                PERFORM I-START-BCOS   THRU F-START-BCOS
                PERFORM I-WINDOW-BCOS  THRU F-WINDOW-BCOS
                PERFORM I-DISPLAY-BCOS THRU F-DISPLAY-BCOS
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-BCO
                ELSE
                   GO LEE-BCO.
       F-HELP-BANCOS.
      *
       I-AYUDA-SUBSIDIO.
           MOVE   01   TO   I.
           MOVE   15   TO   I-LINEA.
           MOVE  ZEROS TO   LLAVM1-CJASUBSI FIN-OK W-COMPB.
           PERFORM   I-START-CJASUBSI   THRU   F-START-CJASUBSI.
           IF FIN-OK = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Tabla de cajas de subsidio sin datos ..."
            LINE 25 POSITION 01
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO TO F-AYUDA-SUBSIDIO.
           PERFORM   I-WINDOW-COMP      THRU   F-WINDOW-COMP.
           DISPLAY "Cod." LINE 13 POSITION 36.
           DISPLAY "Nombre Caja Subsidio" LINE 13 POSITION 45.
           PERFORM   I-DISPLAY-CJASUBSI THRU   F-DISPLAY-CJASUBSI
              UNTIL I > 5.
         LEE-SUBSIDIO.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                          UPDATE PROMPT CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF CDN
            MOVE 01 TO I
            MOVE 15 TO I-LINEA
            MOVE ZEROS TO FIN-OK
            MOVE W-COMPB TO LLAVM1-CJASUBSI
            PERFORM I-START-CJASUBSI THRU F-START-CJASUBSI
            PERFORM I-WINDOW-COMP    THRU F-WINDOW-COMP
            DISPLAY "COD." LINE 13 POSITION 36
            DISPLAY "Nombre Caja Subsidio" LINE 13 POSITION 45
            PERFORM I-DISPLAY-CJASUBSI THRU F-DISPLAY-CJASUBSI
              UNTIL I > 5
            IF FIN-OK = 1
             DISPLAY SPACE LINE 19 POSITION 78 REVERSE
             GO LEE-SUBSIDIO
            ELSE
             GO LEE-SUBSIDIO.
       F-AYUDA-SUBSIDIO. EXIT.
      *
       I-START-CJASUBSI.
           MOVE ZEROS TO FIN-OK.
           START CJASUBSI KEY NOT LESS LLAVM1-CJASUBSI INVALID
            MOVE 1 TO FIN-OK.
       F-START-CJASUBSI. EXIT.
      *
       I-DISPLAY-CJASUBSI.
           PERFORM   I-LEEC-CJASUBSI THRU F-LEEC-CJASUBSI.
           IF FIN-OK = 1
            DISPLAY "**"          LINE  I-LINEA  POSITION 37 REVERSE
            DISPLAY "Fin Archivo" LINE  I-LINEA  POSITION 43 REVERSE
            MOVE 6 TO I
            GO F-DISPLAY-CJASUBSI.
           DISPLAY   COD-CJASUBSI     LINE I-LINEA POSITION 37.
           DISPLAY   NOM-CJASUBSI     LINE I-LINEA POSITION 43.
           IF I = 2  MOVE COD-CJASUBSI TO W-COMPB.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CJASUBSI. EXIT.
      *
       I-LEEC-CJASUBSI.
           READ CJASUBSI NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEEC-CJASUBSI. EXIT.
      *
       I-AYUDA-CONTDPTO.
           MOVE   01   TO   I.
           MOVE   15   TO   I-LINEA.
           MOVE  ZEROS TO  LLAVM1-DPTO FIN-OK W-COMPB.
           PERFORM   I-START-CONTDPTO   THRU   F-START-CONTDPTO.
           IF FIN-OK = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Tabla departamento sin datos..." LINE 25 POSITION 01
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO TO F-AYUDA-CONTDPTO.
           PERFORM   I-WINDOW-COMP      THRU   F-WINDOW-COMP.
           DISPLAY "Cod." LINE 13 POSITION 36.
           DISPLAY "Nombre Dpto  " LINE 13 POSITION 45.
           PERFORM   I-DISPLAY-CONTDPTO THRU   F-DISPLAY-CONTDPTO
              UNTIL I > 5.
         LEE-CONTDPTO.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                          UPDATE PROMPT CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF CDN
            MOVE 01 TO I
            MOVE 15 TO I-LINEA
            MOVE ZEROS TO FIN-OK
            MOVE W-COMPB TO COD-DPTO
            PERFORM I-START-CONTDPTO THRU F-START-CONTDPTO
            PERFORM I-WINDOW-COMP    THRU F-WINDOW-COMP
            DISPLAY "Cod." LINE 13 POSITION 36
            DISPLAY "Nombre Dpto  " LINE 13 POSITION 45
            PERFORM I-DISPLAY-CONTDPTO THRU F-DISPLAY-CONTDPTO
              UNTIL I > 5
            IF FIN-OK = 1
             DISPLAY SPACE LINE 19 POSITION 78 REVERSE
             GO LEE-CONTDPTO
            ELSE
             GO LEE-CONTDPTO.
       F-AYUDA-CONTDPTO. EXIT.
      *
       I-START-CONTDPTO.
           MOVE ZEROS TO FIN-OK.
           START CONTDPTO KEY NOT LESS LLAVM1-DPTO INVALID
            MOVE 1 TO FIN-OK.
       F-START-CONTDPTO. EXIT.
      *
       I-DISPLAY-CONTDPTO.
           PERFORM   I-LEEC-CONTDPTO THRU F-LEEC-CONTDPTO.
           IF FIN-OK = 1
            DISPLAY "**"          LINE  I-LINEA  POSITION 37 REVERSE
            DISPLAY "Fin Archivo" LINE  I-LINEA  POSITION 43 REVERSE
            MOVE 6 TO I
            GO F-DISPLAY-CONTDPTO.
           DISPLAY   COD-DPTO         LINE I-LINEA POSITION 37.
           DISPLAY   NOM-DPTO         LINE I-LINEA POSITION 43.
           IF I = 2  MOVE COD-DPTO  TO W-COMPB.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CONTDPTO. EXIT.
      *
       I-LEEC-CONTDPTO.
           READ CONTDPTO NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEEC-CONTDPTO. EXIT.
      *
       I-AYUDA-CIUDADES.
           MOVE   01   TO   I.
           MOVE   15   TO   I-LINEA.
           MOVE  ZEROS TO   CIUD-CLAVE FIN-OK W-COMPB.
           PERFORM   I-START-NOMICIUD   THRU   F-START-NOMICIUD.
           IF FIN-OK = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Tabla de ciudades sin datos ..." LINE 25 POSITION 01
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO TO F-AYUDA-CIUDADES.
           PERFORM   I-WINDOW-COMP      THRU   F-WINDOW-COMP.
           DISPLAY "Cod." LINE 13 POSITION 36.
           DISPLAY "Nombre Ciudad" LINE 13 POSITION 45.
           PERFORM   I-DISPLAY-CIUDADES THRU   F-DISPLAY-CIUDADES
              UNTIL I > 5.
         LEE-CIUDADES.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                          UPDATE PROMPT CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF CDN
            MOVE 01 TO I
            MOVE 15 TO I-LINEA
            MOVE ZEROS TO FIN-OK
            MOVE W-COMPB TO CIUD-CODIGO
            PERFORM I-START-NOMICIUD THRU F-START-NOMICIUD
            PERFORM I-WINDOW-COMP    THRU F-WINDOW-COMP
            DISPLAY "Cod." LINE 13 POSITION 36
            DISPLAY "Nombre Ciudad" LINE 13 POSITION 45
            PERFORM I-DISPLAY-CIUDADES THRU F-DISPLAY-CIUDADES
              UNTIL I > 5
            IF FIN-OK = 1
             DISPLAY SPACE LINE 19 POSITION 78 REVERSE
             GO LEE-CIUDADES
            ELSE
             GO LEE-CIUDADES.
       F-AYUDA-CIUDADES. EXIT.
      *
       I-START-NOMICIUD.
           MOVE ZEROS TO FIN-OK.
           START NOMICIUD KEY NOT LESS CIUD-CLAVE INVALID
            MOVE 1 TO FIN-OK.
       F-START-NOMICIUD. EXIT.
      *
       I-DISPLAY-CIUDADES.
           PERFORM   I-LEEC-CIUDADES THRU F-LEEC-CIUDADES.
           IF FIN-OK = 1
            DISPLAY "**"          LINE  I-LINEA  POSITION 37 REVERSE
            DISPLAY "Fin Archivo" LINE  I-LINEA  POSITION 43 REVERSE
            MOVE 6 TO I
            GO F-DISPLAY-CIUDADES.
           DISPLAY   CIUD-CODIGO      LINE I-LINEA POSITION 37.
           DISPLAY   CIUD-NOMBRE      LINE I-LINEA POSITION 43.
           IF I = 2  MOVE CIUD-CODIGO TO W-COMPB.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CIUDADES. EXIT.
      *
       I-LEEC-CIUDADES.
           READ NOMICIUD NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-OK.
       F-LEEC-CIUDADES. EXIT.
      *
       I-HELP-COMPROBANTES.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-COMP FIN-OK W-COMPB.
           PERFORM I-START-COMP   THRU F-START-COMP.
           PERFORM I-WINDOW-COMP  THRU F-WINDOW-COMP.
           PERFORM I-DISPLAY-COMP THRU F-DISPLAY-COMP
                   UNTIL I > 5.
         LEE-COMPB.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                               UPDATE PROMPT
                               CONVERT
                               TAB
                               NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01 TO I
                MOVE 15 TO I-LINEA
                MOVE ZEROS TO FIN-OK
                MOVE W-COMPB TO LLAVM1-COMP
                PERFORM I-START-COMP   THRU F-START-COMP
                PERFORM I-WINDOW-COMP  THRU F-WINDOW-COMP
                PERFORM I-DISPLAY-COMP THRU F-DISPLAY-COMP
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 78 REVERSE
                   GO LEE-COMPB
                ELSE
                   GO LEE-COMPB.
       F-HELP-COMPROBANTES.
      *
       I-HELP-CORPORACIONES.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO CORP-CLAVE FIN-OK.
           PERFORM I-START-CORP   THRU F-START-CORP.
           PERFORM I-WINDOW-BCOS  THRU F-WINDOW-BCOS.
           PERFORM I-DISPLAY-CORP THRU F-DISPLAY-CORP
                   UNTIL I > 5.
        LEE-CORP.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                               UPDATE PROMPT
                               CONVERT
                               TAB
                               NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01 TO I
                MOVE 15 TO I-LINEA
                MOVE ZEROS TO FIN-OK
                MOVE W-BANCO TO CORP-CLAVE
                PERFORM I-START-CORP   THRU F-START-CORP
                PERFORM I-WINDOW-BCOS  THRU F-WINDOW-BCOS
                PERFORM I-DISPLAY-CORP THRU F-DISPLAY-CORP
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-CORP
                ELSE
                   GO LEE-CORP.
       F-HELP-CORPORACIONES. EXIT.
      *
       I-FIN-MODE.
           CLOSE EMPRESAS NOMIBANK NOMICIUD FONCESAN CNCATCTA CNCOMPTE
                 NOMICORP CJASUBSI HOJAVIDA VENDEMPR MODACONT
                 TARIESGO CONTDPTO TBRIEPOL CNNITCED.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\BUSCACON.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUDITAEM.PRO".
           COPY "..\PRO\AYAUDIEM.PRO".
           COPY "..\PRO\AYUFONCE.PRO".
           COPY "..\PRO\AUVENEMP.PRO".
           COPY "..\PRO\AYMODCON.PRO".
           COPY "..\PRO\AYRIESGO.PRO".
           COPY "..\PRO\AYRIEPOL.PRO".
           COPY "..\PRO\COLOR200.PRO".
      *
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\TABLAEMP.PLB".
      *
           COPY "..\STA\AUDICAMP.STA".
           COPY "..\STA\VENDEMPR.STA".
           COPY "..\STA\MODACONT.STA".
           COPY "..\STA\TARIESGO.STA".
           COPY "..\STA\FONCESAN.STA".
           COPY "..\STA\TBRIEPOL.STA".
           COPY "..\STA\NOMICORP.STA".
           COPY "..\STA\CNCATCTA.STA".
           COPY "..\STA\CJASUBSI.STA".
           COPY "..\STA\CONTDPTO.STA".
           COPY "..\STA\NOMICIUD.STA".
           COPY "..\STA\NOMIBANK.STA".
           COPY "..\STA\CNNITCED.STA".
           COPY "..\STA\CNCOMPTE.STA".
           COPY "..\STA\CONTRATO.STA".

           COPY "..\LEE\FONCESAN.I00".
           COPY "..\LEE\FONCESAN.SEQ".
           COPY "..\LEE\CNCATCTA.I00".
           COPY "..\LEE\CNNITCED.I00".
      *
           COPY "..\START\FONCESAN.I00".
      ******************************************************************
