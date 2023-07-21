       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG729.
      ******************************************************************
      *       CAPTURA Y ACTUALIZACION DE MVTO ABR-27-2002              *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CNDOCTOS.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\CNCONTRO.DYC".
               COPY "..\DYC\CNEGRESO.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\CNFACTUR.DYC".
               COPY "..\DYC\CNDNOTAS.DYC".
               COPY "..\DYC\CNDTACUM.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
               COPY "..\DYC\CNCOMPTE.DYC".
               COPY "..\DYC\NOMIBANK.DYC".
               COPY "..\DYC\BORRADOS.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\AUDIT729.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\AUDIT729.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNEGRESO.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\CNFACTUR.FD".
               COPY "..\FD\CNDNOTAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\BORRADOS.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\CNNITCED.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA                 PIC X(7)      VALUE "PROG729".
       77  ID-PRG                     PIC X(7)      VALUE "PROG729".
       77  WK-VLRDOC                  PIC S9(12)    VALUE  ZEROS.
       77  I-CON                      PIC X(20)     VALUE  ZEROS.
       77  I-FECHA                    PIC X(11)     VALUE  ZEROS.
       77  I-HORA                     PIC X(05)     VALUE  ZEROS.
       77  WK-DIFSAL                  PIC S9(15)V99 VALUE  ZEROS.
       77  WK-DIFSALT                 PIC S9(15)V99 VALUE  ZEROS.
       77  CONSECUTIVO                PIC 9         VALUE  ZEROS.
       77  ATRAS                      PIC 9         VALUE  ZEROS.
       77  WK-CREDITOS                PIC S9(15)V99 VALUE  ZEROS.
       77  WK-DEBITOS                 PIC S9(15)V99 VALUE  ZEROS.
       77  WK-RADICDOC                PIC 9(6)      VALUE  ZEROS.
       77  WK-CONCEPTO                PIC X(40)     VALUE SPACES.
       77  WK-NIT                     PIC 9(12)     VALUE ZEROS.
       77  WK-NIT1                    PIC 9(12)     VALUE ZEROS.
       77  WK-DD                      PIC 99        VALUE ZEROS.
       77  WK-AA                      PIC 9(04)     VALUE ZEROS.
       77  WK-MM                      PIC 99        VALUE ZEROS.
       77  WK-CUENTAS                 PIC 9(4)      VALUE ZEROS.
       77  WR-CIUDAD                  PIC 9(5)      VALUE ZEROS.
       77  CREA-NIT                   PIC X         VALUE SPACES.
       77  SW-VALORES                 PIC 9         VALUE ZEROS.
       77  NUM-REG                    PIC 9(6)      VALUE ZEROS.
       77  NRO-CALCULO                PIC 9(12)V99  VALUE 0.
       77  X-L                        PIC 9(3)      VALUE ZEROS.
       77  I-NITS                     PIC 9(9)      VALUE ZEROS.
       77  WK-CONCEP01                PIC X(45)     VALUE SPACES.
       77  WK-CONCEP02                PIC X(45)     VALUE SPACES.
      ****************************************************************
      *       TABLA DE REGISTROS - PARA MANEJO EN MEMORIA -          *
      ****************************************************************
       01  TABLA-REG.
           03 TAB-REG                 OCCURS    300 TIMES.
              05  TAB-CUENTA.
                  07  TAB-MAYCTA      PIC 9(04).
                  07  TAB-SUBCTA      PIC 9(02).
                  07  TAB-AUXCTA      PIC 9(04).
              05  TAB-NUMFRA          PIC 9(06).
              05  TAB-NIT             PIC 9(12).
              05  TAB-FECVEN          PIC 9(08).
              05  TAB-CONCEPTO        PIC X(60).
              05  TAB-VLRDOC          PIC S9(12)V99.
              05  TAB-VLRBAS          PIC 9(09).
              05  TAB-SIGNO           PIC 9.
              05  TAB-CONDICION       PIC 9.
              05  TAB-FLAG            PIC X.
              05  TAB-TIPREG          PIC X.
      ****************************************************************
              COPY "..\LBL\EMPRESAS.LBL".
              COPY "..\LBL\CNNITCED.LBL".
              COPY "..\LBL\NOMICIUD.LBL".
              COPY "..\LBL\CNDOCTOS.LBL".
              COPY "..\LBL\USER-ACT.LBL".
              COPY "..\LBL\CNCONTRO.LBL".
              COPY "..\LBL\CNEGRESO.LBL".
              COPY "..\LBL\CNFACTUR.LBL".
              COPY "..\LBL\BORRADOS.LBL".
              COPY "..\LBL\CNDNOTAS.LBL".
              COPY "..\LBL\AUDITE.LBL".
              COPY "..\LBL\CNDTACUM.LBL".
              COPY "..\LBL\CNCATCTA.LBL".
              COPY "..\LBL\CNCOMPTE.LBL".
              COPY "..\LBL\AUDIT729.LBL".
              COPY "..\LBL\NOMIBANK.LBL".

              COPY "..\WRK\USER-ACT.WRK".
              COPY "..\WRK\TEMPCONT.WRK".
              COPY "..\WRK\TABLAGEN.WRK".
              COPY "..\WRK\CNDOCTOS.WRK".
              COPY "..\WRK\VARIABLE.WRK".
              COPY "..\WRK\TABLAMES.WRK".
              COPY "..\WRK\TECLADOS.WRK".
              COPY "..\WRK\WK-RAYAS.WRK".
              COPY "..\WRK\IO-ERROR.WRK".
              COPY "..\WRK\NO-ERROR.WRK".
              COPY "..\WRK\LIQUINOV.WRK".
              COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION USING AREA-LINK-NOMINA.
       DECLARATIVES.
              COPY "..\DCL\CNNITCED.DCL".
              COPY "..\DCL\EMPRESAS.DCL".
              COPY "..\DCL\AUDITE.DCL".
              COPY "..\DCL\CNDNOTAS.DCL".
              COPY "..\DCL\CNDTACUM.DCL".
              COPY "..\DCL\NOMIBANK.DCL".
              COPY "..\DCL\CNDOCTOS.DCL".
              COPY "..\DCL\NOMICIUD.DCL".
              COPY "..\DCL\CNEGRESO.DCL".
              COPY "..\DCL\CNFACTUR.DCL".
              COPY "..\DCL\AUDIT729.DCL".
              COPY "..\DCL\BORRADOS.DCL".
              COPY "..\DCL\CNCONTRO.DCL".
              COPY "..\DCL\USER-ACT.DCL".
              COPY "..\DCL\CNCATCTA.DCL".
              COPY "..\DCL\CNCOMPTE.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE " ACT. MOVIMIENTO"      TO  TIT-APLICACION.
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS         THRU  F-USUARIOS.
           IF      SW-USER = 1        EXIT  PROGRAM.
           PERFORM I-PROCESO-AUDITE   THRU  F-PROCESO-AUDITE.
           PERFORM I-PANTALLA-1B      THRU  F-PANTALLA-1B.
           PERFORM I-ACEPTA-MES       THRU  F-ACEPTA-MES.
           PERFORM I-CONFIRMA-COMP    THRU  F-CONFIRMA-COMP.
           PERFORM I-LABEL-ARCHIVOS   THRU  F-LABEL-ARCHIVOS.
           PERFORM I-STATUS-CNDOCTOS  THRU  F-STATUS-CNDOCTOS
           PERFORM I-STATUS-CNDTACUM  THRU  F-STATUS-CNDTACUM
           PERFORM I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS
           MOVE    ZEROS                TO  RUT-OK FIN-OK CONSECUTIVO.
           PERFORM I-VERIFICA-FECHA   THRU  F-VERIFICA-FECHA.
           PERFORM I-MENU-OPCIONES    THRU  F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           PERFORM I-LABEL-CNDOCTOS    THRU F-LABEL-CNDOCTOS.
           PERFORM I-LABEL-BORRADOS    THRU F-LABEL-BORRADOS.
           PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
           MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT CNCATCTA EMPRESAS NOMIBANK.
           MOVE 1  TO SW-ABRE.
           OPEN I-O   CNDOCTOS.
           OPEN I-O   CNNITCED.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999     TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS           TO   FIN-OK

           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
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
           IF  PROSS-XKK = "N"
               NEXT SENTENCE
           ELSE
           MOVE ZEROS TO SW-ROMPE SW-EMPR WK-EMP-ROMPE.
           IF  W-OPCION  =  "A" AND PROSS-AKK = "S"
               PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
               PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC
               PERFORM I-ADICIONA     THRU F-ADICIONA
                                      UNTIL ESC
               MOVE ZEROS            TO    WK-EXCEPTION
           ELSE
             IF  W-OPCION  =  "C" AND PROSS-CKK = "S"
                 PERFORM I-LLAMA-WINDOW THRU  F-LLAMA-WINDOW
                 PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC
                 PERFORM I-CONSULTA     THRU  F-CONSULTA
                                        UNTIL ESC
                 MOVE ZEROS               TO  WK-EXCEPTION
             ELSE
               IF  W-OPCION  =  "M" AND PROSS-MKK = "S"
                   PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                   PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC
                   PERFORM I-MODIFICA    THRU  F-MODIFICA
                                         UNTIL ESC
                   MOVE ZEROS            TO    WK-EXCEPTION
               ELSE
                 IF  W-OPCION  =  "R" AND PROSS-RKK = "S"
                      PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                      PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC
                      PERFORM I-RETIRA      THRU  F-RETIRA
                                            UNTIL ESC
                      MOVE ZEROS            TO    WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-FIN-MODE.
                 CLOSE CNDOCTOS
                       CNCATCTA
                       NOMIBANK
                       CNNITCED
                       EMPRESAS
                 EXIT  PROGRAM.
                 STOP  RUN.
       F-FIN-MODE.
      *
       I-LLAMA-WINDOW.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
       F-LLAMA-WINDOW.
      *
       I-DISPLAY-OPC.
           IF  W-OPCION                     =  "A"
               DISPLAY " ADICION"            LINE  1 POSITION 01
                                           BLINK    REVERSE LOW
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY " CONSULTA"            LINE  1 POSITION 01
                                           BLINK    REVERSE LOW
           ELSE
           IF  W-OPCION                     =  "M"
               DISPLAY "  MODIFY "            LINE  1 POSITION 01
                                           BLINK    REVERSE LOW
           ELSE
           IF  W-OPCION                     =  "R"
               DISPLAY "  RETIRO "            LINE  1 POSITION 01
                                           BLINK    REVERSE LOW.
       F-DISPLAY-OPC.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS PRINCIPALES DEL PROGRAMA                  ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-ADICIONA                  ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-ADICIONA.
           PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC.
           PERFORM I-BORRA-TEMP   THRU  F-BORRA-TEMP
           PERFORM I-TIPCOM       THRU  F-TIPCOM.
           IF      CUP OR ESC       GO  F-ADICIONA.
         I-ACDOC.
           MOVE   ZEROS                  TO   RADICDOC-DOCTRA.
           PERFORM I-DOCUMENTO         THRU   F-DOCUMENTO.
           IF      CUP OR ESC            GO   F-ADICIONA.
           PERFORM I-LEE-CNDOCTOS      THRU   F-LEE-CNDOCTOS.
           IF  SW-1  =  1
               PERFORM I-SUMDOC        THRU   F-SUMDOC
               PERFORM I-DISUM         THRU   F-DISUM
               PERFORM I-EXISTE        THRU   F-EXISTE
               MOVE    1                 TO   SW-EMPR
               GO      I-ADICIONA
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA      THRU  F-BORRA-TABLA
               MOVE    W-LONG               TO  ALTMES-DOCTOS
               PERFORM I-DATOS            THRU  F-DATOS
               PERFORM I-LLAMA-WINDOW     THRU  F-LLAMA-WINDOW.
       F-ADICIONA.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-CONSULTA                  ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-CONSULTA.
           PERFORM I-DISPLAY-OPC   THRU  F-DISPLAY-OPC.
           PERFORM I-TIPCOM        THRU  F-TIPCOM.
           IF      CUP             OR
                   ESC             GO    F-CONSULTA.
           IF CDN
              PERFORM I-LEE1-CNDOCTOS THRU F-LEE1-CNDOCTOS
              PERFORM CONSU-1.
           MOVE   ZEROS                 TO  RADICDOC-DOCTRA.
           PERFORM I-DOCUMENTO        THRU  F-DOCUMENTO.
           IF      CUP OR ESC           GO  F-CONSULTA.
           IF CDN
              PERFORM I-LEE2-CNDOCTOS THRU  F-LEE2-CNDOCTOS
            ELSE
              PERFORM I-LEE-CNDOCTOS  THRU  F-LEE-CNDOCTOS.
        CONSU-1.
           IF  SW-1  =  0
               PERFORM I-NEXISTE      THRU  F-NEXISTE
               MOVE SPACE               TO  REG-DOCTOS
               GO I-CONSULTA
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA    THRU  F-BORRA-TABLA
               PERFORM I-SUMDOC         THRU  F-SUMDOC
               PERFORM I-DISUM          THRU  F-DISUM
               PERFORM I-CARGA-TABLA    THRU  F-CARGA-TABLA
               MOVE    ZEROS              TO  K
               PERFORM I-PANTA-7REG     THRU  F-PANTA-7REG
               PERFORM I-MOSTRAR-CAB    THRU  F-MOSTRAR-CAB
               PERFORM I-CONSULTAR      THRU  F-CONSULTAR
               PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW.
       F-CONSULTA.
      *
       I-CONSULTAR.
           DISPLAY " PRESIONE <> CONSULTAR " LINE 24 POSITION 1
           DISPLAY " <F1> OPCIONES        "   LINE 24 POSITION 27
           PERFORM ACEPTA-TRUCO
           PERFORM I-WIN-CTAS   THRU F-WIN-CTAS
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1 OR  ESC   DISPLAY WK-ESPACIOS LINE 24 POSITION 1
               GO  F-CONSULTAR.
           PERFORM I-BORRA-TABLA    THRU  F-BORRA-TABLA
           PERFORM I-LEER-CNDOCTOS  THRU  F-LEER-CNDOCTOS.
           IF SW-OK = 1
              PERFORM I-LEE1-CNDOCTOS THRU  F-LEE1-CNDOCTOS.
           PERFORM I-CARGA-TABLA      THRU  F-CARGA-TABLA
           PERFORM I-SUMDOC           THRU  F-SUMDOC
           PERFORM I-DISUM            THRU  F-DISUM
           MOVE    ZEROS                TO  K
           PERFORM I-PANTA-7REG       THRU  F-PANTA-7REG
           PERFORM I-MOSTRAR-CAB      THRU  F-MOSTRAR-CAB
           IF  F8   AND   ALTTIP-DOCTRA = 12
               PERFORM I-TIPO-12   THRU F-TIPO-12
               PERFORM I-VENTANA-NOTAS  THRU F-VENTANA-NOTAS
               PERFORM I-MUESTRA-NOTAS  THRU F-MUESTRA-NOTAS.
           IF  F8   AND   ALTTIP-DOCTRA = 14
               PERFORM I-TIPO-12   THRU F-TIPO-12
               PERFORM I-VENTANA-NOTAS  THRU F-VENTANA-NOTAS
               PERFORM I-MUESTRA-NOTAS  THRU F-MUESTRA-NOTAS.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
           GO I-CONSULTAR.
       F-CONSULTAR.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-MODIFICA                  ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-MODIFICA.
           PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC.
           PERFORM I-TIPCOM       THRU  F-TIPCOM.
           IF      CUP        OR
                   ESC              GO  F-MODIFICA.
           MOVE   ZEROS            TO  RADICDOC-DOCTRA.
           PERFORM I-DOCUMENTO    THRU  F-DOCUMENTO.
           IF      CUP OR ESC       GO  F-MODIFICA.
           PERFORM I-LEE-CNDOCTOS THRU  F-LEE-CNDOCTOS.
           IF  SW-1  =  0
               PERFORM I-NEXISTE        THRU  F-NEXISTE
               MOVE SPACE                 TO  REG-DOCTOS
               GO I-MODIFICA
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA    THRU F-BORRA-TABLA
               PERFORM I-SUMDOC         THRU F-SUMDOC
               PERFORM I-DISUM          THRU F-DISUM
               PERFORM I-CARGA-TABLA    THRU F-CARGA-TABLA
               MOVE ZEROS TO K
               PERFORM I-PANTA-7REG     THRU F-PANTA-7REG
               PERFORM I-MOSTRAR-CAB    THRU F-MOSTRAR-CAB
               PERFORM I-DATOS          THRU F-DATOS
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-MODIFICA.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-RETIRA                    ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-RETIRA.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-TIPCOM      THRU  F-TIPCOM.
           IF      CUP      OR    ESC
                   GO F-RETIRA.
           DISPLAY  "<F7>  RETIRO COMPROBANTE COMPLETO"
                         LINE 24 POSITION  1  BEEP.
           PERFORM ACEPTA-TRUCO.
           IF    F7  PERFORM  I-RETIRA-COMPLETO  THRU F-RETIRA-COMPLETO
                     PERFORM  I-LLAMA-WINDOW     THRU F-LLAMA-WINDOW
                     GO F-RETIRA.
           IF      CUP OR ESC
                   GO       F-RETIRA.
           MOVE   ZEROS            TO  RADICDOC-DOCTRA.
           PERFORM I-DOCUMENTO   THRU  F-DOCUMENTO.
           IF      CUP OR ESC
                   GO       F-RETIRA.
           PERFORM I-LEE-CNDOCTOS THRU F-LEE-CNDOCTOS.
           IF  SW-1  =  0
               PERFORM I-NEXISTE        THRU   F-NEXISTE
               MOVE SPACE                 TO   REG-DOCTOS
               GO I-RETIRA
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA    THRU  F-BORRA-TABLA
               PERFORM I-SUMDOC         THRU  F-SUMDOC
               PERFORM I-DISUM          THRU  F-DISUM
               PERFORM I-CARGA-TABLA    THRU  F-CARGA-TABLA
               MOVE    ZEROS            TO    K
               PERFORM I-PANTA-7REG     THRU  F-PANTA-7REG
               PERFORM I-MOSTRAR-CAB    THRU  F-MOSTRAR-CAB
               PERFORM I-ELIMINAR       THRU F-ELIMINAR
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-RETIRA.
      *
       I-ELIMINAR.
           DISPLAY " PRESIONE <R> RETIRAR "     LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF     F1    GO   F-ELIMINAR.
           IF     F2    PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF     F9
                  ACCEPT W-LONG LINE 11 POSITION 50 REVERSE
                  CONVERT NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION
                  MOVE "R"  TO W-TRUCO.
           IF  W-TRUCO NOT = "R" GO I-ELIMINAR.
           IF  CONDICION-DOCTOS = 1
               PERFORM I-ACTUALIZADO THRU F-ACTUALIZADO.
           IF SW-99 = 4
               MOVE  0     TO SW-99
               GO    F-ELIMINAR.
           PERFORM I-RETIRADA         THRU F-RETIRADA.
           PERFORM I-AUDITA-CAMPO     THRU F-AUDITA-CAMPO
           IF  ALTTIP-DOCTRA = 04
               PERFORM I-BORRA-CNFACTUR   THRU F-BORRA-CNFACTUR.
           IF  ALTTIP-DOCTRA = 12
               PERFORM I-BORRA-CNDNOTAS   THRU F-BORRA-CNDNOTAS.
           IF  ALTTIP-DOCTRA = 14
               PERFORM I-BORRA-CNDNOTAS   THRU F-BORRA-CNDNOTAS.
           IF ALTTIP-DOCTRA = 3
              PERFORM I-BORRA-EGRESO      THRU F-BORRA-EGRESO.
       F-ELIMINAR.
      *
       I-RETIRA-COMPLETO.
           DISPLAY "PRESIONE <R> RETIRA EL COMPROBANTE COMPLETO"
                     LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 45
           PERFORM ACEPTA-TRUCO
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1 OR  ESC   DISPLAY WK-ESPACIOS LINE 24 POSITION 1
                            GO  F-RETIRA-COMPLETO.
           MOVE 0 TO SW-OK.
           PERFORM  I-START-100-CNDOCTOS  THRU F-START-100-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO COMPLETO-3.
       COMPLETO-1.
           READ  CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                      GO COMPLETO-3.
           IF    RADICTIP-DOCTOS =  ALTTIP-DOCTRA
                 PERFORM I-GRABA-AUDITORIA  THRU F-GRABA-AUDITORIA
                 PERFORM I-AUDITA-CAMPO     THRU F-AUDITA-CAMPO
                 DELETE CNDOCTOS.
           IF  ALTTIP-DOCTRA = 04
               PERFORM I-BORRA-CNFACTUR   THRU F-BORRA-CNFACTUR.
           IF  ALTTIP-DOCTRA = 12
               PERFORM I-BORRA-CNDNOTAS   THRU F-BORRA-CNDNOTAS.
           IF  ALTTIP-DOCTRA = 14
               PERFORM I-BORRA-CNDNOTAS   THRU F-BORRA-CNDNOTAS.
           IF    ALTTIP-DOCTRA = 3
                 PERFORM I-BORRA-EGRESO      THRU F-BORRA-EGRESO.

           IF    RADICTIP-DOCTOS  =  ALTTIP-DOCTRA AND
                 CONDICION-DOCTOS = 1
                 MOVE 1     TO SW-99
                 PERFORM I-REVERSA-ACUM     THRU F-REVERSA-ACUM.

           IF    RADICTIP-DOCTOS >  ALTTIP-DOCTRA  GO COMPLETO-3.
           GO COMPLETO-1.
       COMPLETO-3.
           IF  SW-99  =  1
               MOVE  0   TO  SW-99.
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
       F-RETIRA-COMPLETO.
      *
       I-RETIRADA.
           PERFORM  I-START-110-CNDOCTOS  THRU F-START-110-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-RETIRADA.
           MOVE  ALTTIP-DOCTRA   TO  RADICTIP-DOCTOS.
           MOVE  RADICDOC-DOCTRA TO  RADICDOC-DOCTOS.
           MOVE  ZEROS           TO  RADIC-AUTOMATICA-DOCTOS.
        RETIRA-1.
           READ  CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                      GO F-RETIRADA.
           IF    RADICTIP-DOCTOS =  ALTTIP-DOCTRA   AND
                 RADICDOC-DOCTOS =  RADICDOC-DOCTRA
                 PERFORM I-GRABA-AUDITORIA  THRU F-GRABA-AUDITORIA
                 PERFORM I-REVERSA-ACUM     THRU F-REVERSA-ACUM
                 DELETE CNDOCTOS
                 GO RETIRA-1.
           MOVE  1 TO SW-1.
           IF    RADICTIP-DOCTOS >  ALTTIP-DOCTRA GO F-RETIRADA.
           IF    RADICDOC-DOCTOS > RADICDOC-DOCTRA GO F-RETIRADA.
           GO RETIRA-1.
       F-RETIRADA.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-DATOS                     ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-DATOS.
           IF W-OPCION = "M"
              IF MOD-USRKK(1) = "N" GO F-DATOS.
           IF W-OPCION = "M"
              IF TAB-CONDICION(1) = 1
                 PERFORM I-ACTUALIZADO THRU F-ACTUALIZADO.
           IF SW-99 = 4
               MOVE  0     TO SW-99
               GO    F-DATOS.
           PERFORM I-DATOS-CAB  THRU  F-DATOS-CAB.
           IF  ESC OR  CUP OR F1  GO  F-DATOS.
           MOVE    ZEROS          TO  WK-EXCEPTION
           MOVE    SPACES         TO  W-TRUCO
           IF SW-VALORES  = 0     GO  I-DTCONT.

           IF      W-OPCION   =   "A"
                   PERFORM I-GRABACION   THRU  F-GRABACION
                   UNTIL  W-TRUCO = "A"    OR  F1 OR CUP
           ELSE
                   PERFORM I-REGRABACION THRU F-REGRABACION
                   UNTIL  W-TRUCO = "M" OR F1 OR CUP.
           MOVE  0   TO CONSECUTIVO.
           IF  SW-99 = 1
               MOVE  0   TO  SW-99.
         I-DTCONT.
           IF      CUP
                   PERFORM I-DISUM   THRU  F-DISUM  GO I-DATOS.
       F-DATOS.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINA ORGANIZACION DE CAPTURA DATOS  I-DATOS-CAB.≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

       I-DATOS-CAB.
           IF      CUP             GO  CAB-09.
        CAB-1.
           PERFORM I-DOCU1       THRU  F-DOCU1.
                   IF             CUP  GO F-DATOS-CAB.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-2.
           PERFORM I-FECHA-DESDE THRU  F-FECHA-DESDE.
                   IF             CUP  GO CAB-1.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-3.
           PERFORM I-FECHA-HASTA1 THRU  F-FECHA-HASTA1.
                   IF             CUP  GO CAB-2.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-4.
           PERFORM I-EMPRESA     THRU  F-EMPRESA.
                   IF             CUP  GO CAB-3.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-5.
           PERFORM I-NIT1        THRU  F-NIT1.
                   IF             CUP  GO CAB-4.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-51.
           IF ALTTIP-DOCTRA = 1
              PERFORM I-BANCO-B     THRU  F-BANCO-B.
                     IF             CUP  GO CAB-5.
                     IF             ESC  GO F-DATOS-CAB.
         CAB-52.
           IF ALTTIP-DOCTRA = 1
              PERFORM I-CHEQUE-B    THRU  F-CHEQUE-B.
                   IF                CUP  GO CAB-51.
                   IF                ESC  GO F-DATOS-CAB.
         CAB-53.
           IF ALTTIP-DOCTRA = 1 OR ALTTIP-DOCTRA = 3
              PERFORM I-BANCO-P     THRU  F-BANCO-P.
                   IF                CUP  GO CAB-52.
                   IF                ESC  GO F-DATOS-CAB.
         CAB-6.
           IF ALTTIP-DOCTRA = 3
           PERFORM I-SOPORTE     THRU  F-SOPORTE.
                   IF             CUP  GO CAB-53.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-7.
           IF ALTTIP-DOCTRA = 3
           PERFORM I-SOPORTE-2   THRU  F-SOPORTE-2.
                   IF             CUP  GO CAB-6.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-8.
           PERFORM I-PERIODO     THRU  F-PERIODO.
                   IF             CUP  GO CAB-7.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-9.
           IF ALTTIP-DOCTRA = 12
              PERFORM I-NOTAS    THRU  F-NOTAS.
           IF ALTTIP-DOCTRA = 14
              PERFORM I-NOTAS    THRU  F-NOTAS.
                   IF             CUP  GO CAB-8.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-09.
           MOVE        8 TO I.
           MOVE    ZEROS TO J.
           MOVE    ZEROS TO WK-EXCEPTION.
           PERFORM I-CAPTURA-CTAS THRU F-CAPTURA-CTAS
                   UNTIL HOME OR J > 300 OR CUP OR F1 OR ESC.
           IF CUP  GO CAB-8.
       F-DATOS-CAB.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-CAPTURA-CUENTAS           ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-CAPTURA-CTAS.
           IF ALTTIP-DOCTRA  = 12 GO CAB-D.
           IF ALTTIP-DOCTRA  = 14 GO CAB-D.
              PERFORM I-BLQPAN  THRU F-CREA-NIT.
         CAB-D.
           ADD 1 TO I.
           ADD 1 TO J.
         CAB-DIS.
           IF  I  > 15
               MOVE    9              TO I
               PERFORM I-WIN-CTAS   THRU F-WIN-CTAS
               COMPUTE K = J - 1
               PERFORM I-PANTA-7REG THRU F-PANTA-7REG
               MOVE    9              TO I.
           IF      CUP                GO CAB-18.
           IF      F1                 GO F-CAPTURA-CTAS.
           MOVE   ZEROS               TO ATRAS.
         CAB-10.
           IF ALTTIP-DOCTRA = 12 OR ALTTIP-DOCTRA = 14
              PERFORM I-VENTANA-NOT   THRU F-VENTANA-NOT.
      *    DISPLAY "Line XX  Reg xx " LINE 02 POSITION 45  LOW.
      *    DISPLAY I LINE 01 POSITION 77     .
           DISPLAY J LINE 08 POSITION 13     .
           PERFORM I-MAYOR          THRU F-MAYOR.
           IF      F1 OR ESC          GO F-CAPTURA-CTAS.
           IF  ATRAS =  1  GO CAB-11.
           IF CUP
              IF J = 1 AND I = 9
                 GO F-CAPTURA-CTAS
              ELSE
                 IF I = 9
                    PERFORM I-WIN-CTAS  THRU F-WIN-CTAS
                    COMPUTE M = J - 8
                    MOVE    8  TO   N
                    PERFORM I-PANTA-ANT THRU F-PANTA-ANT
                    GO         CAB-10
                 ELSE
                    SUBTRACT 1 FROM I
                    SUBTRACT 1 FROM J
                    GO         CAB-10.
           IF      F1                 GO F-CAPTURA-CTAS.
           IF CDN   GO I-CAPTURA-CTAS.
           IF HOME
              PERFORM I-EVAL-SALDOS THRU F-EVAL-SALDOS
              IF WK-DIFSAL NOT = ZEROS
                 MOVE    SALDO           TO  WI-Z9P
                 DISPLAY WI-Z9P LINE 21 POSITION 62 REVERSE
                 DISPLAY " COMPROBANTE DESCUADRADO "
                 LINE 24 POSITION 15 REVERSE BLINK
                 PERFORM ACEPTA-TRUCO
                 GO CAB-10
              ELSE
                 MOVE ZEROS TO SW-VALORES
                 IF W-TOTAL-CREDITOS < ZEROS OR
                    W-TOTAL-DEBITOS  > ZEROS OR
                    WK-CONCEPTO      = "ANULADO"
                    MOVE 1 TO SW-VALORES
                    GO F-CAPTURA-CTAS
                 ELSE
                    IF TAB-CONCEPTO(J) = "_ELIMINADO_"
                       MOVE 1 TO SW-VALORES
                       GO F-CAPTURA-CTAS
                    ELSE
                       GO F-CAPTURA-CTAS.
           IF F9
              IF TAB-MAYCTA(J) = ZEROS GO CAB-DIS
              ELSE
                 PERFORM I-GRABA-AUDITORIA1 THRU F-GRABA-AUDITORIA1
                 PERFORM I-BORRA-REG THRU F-BORRA-REG
                 MOVE "R"              TO TAB-TIPREG(J)
                 MOVE "_ELIMINADO_"    TO TAB-CONCEPTO(J)
                 PERFORM I-PANTA-REG THRU F-PANTA-REG
                 GO CAB-10.
           IF TAB-TIPREG(J) = "R" GO  I-CAPTURA-CTAS.
           IF TAB-CONDICION(J) = 1 AND SW-99 = ZEROS
              GO  I-CAPTURA-CTAS.
         CAB-11.
           PERFORM I-SUBCTA      THRU  F-SUBCTA.
                   IF             CUP  GO CAB-10.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-12.
           PERFORM I-AUXCTA      THRU  F-AUXCTA.
                   IF             CUP  GO CAB-11.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-13.
           PERFORM I-CONCEPTO    THRU  F-CONCEPTO.
                   IF             CUP  GO CAB-12.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-14.
           PERFORM I-NIT         THRU  F-NIT.
                   IF             CUP  GO CAB-13.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-15.
           PERFORM I-FACTUR      THRU  F-FACTUR.
                   IF             CUP  GO CAB-14.
           IF      TAB-CONCEPTO(J)  =  "ANULADO"
                   MOVE ZEROS          TO TAB-VLRDOC(J)
                   MOVE TAB-VLRDOC(J)  TO  WI-Z11P
                   DISPLAY WI-Z11P     LINE I POSITION 54 REVERSE LOW
                   GO CAB-18.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-16.
           PERFORM I-VRBASE      THRU  F-VRBASE.
                   IF             CUP  GO CAB-15.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-17.
           PERFORM I-VRDOC       THRU  F-VRDOC.
                   IF             CUP  GO CAB-16.
           IF      F1             GO   F-CAPTURA-CTAS.
         CAB-18.
           MOVE   ZEROS            TO  ATRAS.
           PERFORM I-SIGNO       THRU  F-SIGNO.
                   IF             CUP  GO CAB-17.
           IF      F1             GO   F-CAPTURA-CTAS.
       F-CAPTURA-CTAS.
      *
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥     GRABACION Y REGRABACION DE LOS REGISTROS            ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-GRABACION.              ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-GRABACION.
           DISPLAY " PRESIONE <AVPAG> ADICIONAR " LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "              LINE 24 POSITION 35
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP OR F1          GO F-GRABACION.
           IF  F2                 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  PGDN               MOVE   "A"    TO W-TRUCO.
           IF  W-TRUCO NOT = "A"  GO TO I-GRABACION.
           IF  ALTTIP-DOCTRA = 04
               PERFORM I-GRABA-CNFACTUR   THRU F-GRABA-CNFACTUR.
           IF ALTTIP-DOCTRA = 12
               PERFORM I-GRABA-CNDNOTAS   THRU F-GRABA-CNDNOTAS.
           IF ALTTIP-DOCTRA = 14
               PERFORM I-GRABA-CNDNOTAS   THRU F-GRABA-CNDNOTAS.
           DISPLAY RADICDOC-DOCTRA LINE 03 POSITION 16
                                     REVERSE BLINK LOW.
           MOVE ZEROS TO X-L NUM-REG.
           PERFORM I-AUDITA-CAMPO     THRU F-AUDITA-CAMPO.
         I-GRABLOOP.
           PERFORM  I-MVTO-CAMPOS   THRU   F-MVTO-CAMPOS.
           ADD 1 TO X-L.
           IF  X-L > 300 OR TAB-MAYCTA (X-L) = ZEROS GO F-GRABACION.
           IF  TAB-TIPREG(X-L)    = "R"  GO I-GRABLOOP.
           ADD 1   TO  NUM-REG.
           IF  ALTTIP-DOCTRA = 4  AND TAB-NUMFRA(X-L) NOT = ZEROS
               MOVE NUMCONS-COMP       TO TAB-NUMFRA(X-L).
           IF  ALTTIP-DOCTRA = 12 AND TAB-NUMFRA(X-L) = 999999
               MOVE NUMCONS-COMP       TO TAB-NUMFRA(X-L).
           IF  ALTTIP-DOCTRA = 14 AND TAB-NUMFRA(X-L) = 999999
               MOVE NUMCONS-COMP       TO TAB-NUMFRA(X-L).
           PERFORM  I-MVTO-TABLA     THRU F-MVTO-TABLA.
           MOVE    0                    TO SW-99
           PERFORM I-REVERSA-ACUM     THRU F-REVERSA-ACUM
           MOVE    1                    TO CONDICION-DOCTOS
           MOVE    1                    TO SW-99.
           WRITE       REG-DOCTOS.
           GO I-GRABLOOP.
       F-GRABACION.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-REGRABACION.              ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-REGRABACION.
         I-REGRANOR.
           DISPLAY " PRESIONE <AVPAG> MODIFICAR "   LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "               LINE 24 POSITION 35
           PERFORM ACEPTA-TRUCO
           IF F8
              PERFORM I-FECHA-CONTAB    THRU    F-FECHA-CONTAB
              GO I-REGRABACION.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP OR ESC OR F1
               GO F-REGRABACION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  PGDN  MOVE "M"    TO W-TRUCO.
           IF  W-TRUCO                  NOT =  "M"
               GO I-REGRABACION.
           IF  ALTTIP-DOCTRA = 04
               PERFORM I-GRABA-CNFACTUR   THRU F-GRABA-CNFACTUR.
           IF ALTTIP-DOCTRA = 12
               PERFORM I-GRABA-CNDNOTAS   THRU F-GRABA-CNDNOTAS.
           IF ALTTIP-DOCTRA = 14
               PERFORM I-GRABA-CNDNOTAS   THRU F-GRABA-CNDNOTAS.
           PERFORM  I-START-110-CNDOCTOS  THRU F-START-110-CNDOCTOS.
           PERFORM I-BORRA-REC            THRU F-BORRA-REC.
           IF ALTTIP-DOCTRA = 3
              PERFORM I-BORRA-EGRESO      THRU F-BORRA-EGRESO.
           MOVE ZEROS TO X-L NUM-REG.
           PERFORM I-AUDITA-CAMPO         THRU F-AUDITA-CAMPO.
         I-REGRABLOOP.
           PERFORM  I-MVTO-CAMPOS         THRU F-MVTO-CAMPOS.
           ADD 1 TO X-L.
           IF  X-L > 300 OR TAB-MAYCTA (X-L) = ZEROS GO F-REGRABACION.
           IF      TAB-TIPREG(X-L)    = "R"  GO I-REGRABLOOP.
           ADD     1  TO NUM-REG.
           PERFORM  I-MVTO-TABLA    THRU   F-MVTO-TABLA.
           IF  SW-99  =  1
                 MOVE    0                    TO SW-99
                 PERFORM I-REVERSA-ACUM     THRU F-REVERSA-ACUM
                 MOVE    1                    TO SW-99
                 MOVE    1                    TO CONDICION-DOCTOS.
           REWRITE REG-DOCTOS INVALID KEY WRITE REG-DOCTOS.
           GO I-REGRABLOOP.
       F-REGRABACION.
      *
       I-MVTO-CAMPOS.
           MOVE ALTTIP-DOCTRA             TO ALTTIP-DOCTOS
                                             ALT09TIP-DOCTOS
                                             ALTTIP04-DOCTOS
                                             ALTTIP03-DOCTOS
                                             ALTTIP08-DOCTOS
                                             ALTTIP12-DOCTOS
                                             RADICTIP-DOCTOS.
           MOVE NUM-SOPORTE-DOCTRA        TO NUM-SOPORTE-DOCTOS
           MOVE CHEQ-HASTA-DOCTRA         TO CHEQ-HASTA-DOCTOS
           MOVE FECDOC-DOCTRA             TO FECDOC-DOCTOS.
           MOVE CIUDAD12-DOCTRA           TO CIUDAD12-DOCTOS.
           MOVE ALT07-DOCTRA              TO ALT07-DOCTOS.
           IF W-OPCION = "A"
              MOVE FECDOC-DOCTOS          TO FECHA-CONTAB-DOCTOS
           ELSE
              MOVE FECHA-CONTAB-DOCTRA    TO FECHA-CONTAB-DOCTOS.
           MOVE W-LONG                    TO ALTMES-DOCTOS
           MOVE ALTEMP-DOCTRA             TO ALTEMP-DOCTOS
           MOVE ALTPER-DOCTRA             TO ALTPER-DOCTOS.
           MOVE RADICDOC-DOCTRA           TO RADICDOC-DOCTOS.
           MOVE COSTO12-DOCTRA            TO COSTO12-DOCTRA.
           MOVE RADICDOC-DOCTOS           TO RADICA04-DOCTOS
                                             ALTFRA-DOCTOS
                                             ALTFRA09-DOCTOS
                                             RADICA02-DOCTOS
                                             RADICA05-DOCTOS.
           MOVE BANCO-DOCTRA              TO BANCO-DOCTOS
                                             BANCO01-DOCTOS
           MOVE BANCO-BENEF-DOCTRA        TO BANCO-BENEF-DOCTOS
           MOVE CHEQUE-BENEF-DOCTRA       TO CHEQUE-BENEF-DOCTOS
           MOVE CONSIGNACION-DOCTRA       TO CONSIGNACION-DOCTOS.
           IF   NITAUX-DOCTRA = ZEROS
                MOVE WK-NIT1              TO NITAUX-DOCTOS
           ELSE
                MOVE NITAUX-DOCTRA        TO NITAUX-DOCTOS.
      *
           IF   ALTTIP-DOCTRA = 2  AND W-OPCION = "A"
                MOVE ZEROS                TO SW-CONS-DOCTOS
           ELSE
                MOVE 1                    TO SW-CONS-DOCTOS.
      *
           IF   ALTTIP-DOCTRA = 1   OR ALTTIP-DOCTRA = 3
                IF W-OPCION = "A"
                MOVE ZEROS                TO SW-IMP-DOCTOS
           ELSE
                MOVE 1                    TO SW-IMP-DOCTOS.

           IF   FLAG-DOCTRA = SPACES
                MOVE "C"                  TO FLAG-DOCTOS
           ELSE
                MOVE FLAG-DOCTRA          TO FLAG-DOCTOS.
           IF   CIUDAD-DOCTRA = ZEROS
                MOVE WIN-COD-CONTDPTO     TO CIUDAD-DOCTOS
           ELSE
                MOVE CIUDAD-DOCTRA        TO CIUDAD-DOCTOS.
           MOVE  COMP-ANTER-DOCTRA        TO COMP-ANTER-DOCTOS.
       F-MVTO-CAMPOS.
      *
       I-MVTO-TABLA.
           MOVE    NUM-REG            TO RADIC-AUTOMATICA-DOCTOS.
           MOVE    TAB-MAYCTA(X-L)    TO MAYCTA-DOCTOS.
           MOVE    TAB-SUBCTA(X-L)    TO SUBCTA-DOCTOS.
           MOVE    TAB-AUXCTA(X-L)    TO AUXCTA-DOCTOS.
           MOVE    CUENTA-DOCTOS      TO ALTCTA-DOCTOS
                                         ALTCTA1-DOCTOS
                                         ALT04CTA-DOCTOS
                                         ALT12CTA-DOCTOS
                                         ALT09CTA-DOCTOS.
           MOVE    TAB-NUMFRA(X-L)    TO NUMFRA-DOCTOS.
           MOVE    TAB-NIT(X-L)       TO ALTNIT-DOCTOS.
           MOVE    TAB-CONCEPTO(X-L)  TO CONCEPTO-DOCTOS.
           MOVE    TAB-VLRDOC(X-L)    TO VALOR-DOCTOS.
           MOVE    TAB-VLRBAS(X-L)    TO VRBASE-DOCTOS.
           MOVE    TAB-SIGNO(X-L)     TO SIGNO-DOCTOS.
           MOVE    TAB-CONDICION(X-L) TO CONDICION-DOCTOS.
           DISPLAY "GRABANDO... ESPERE UN MOMENTO POR FAVOR"
                LINE 24 POSITION 1 REVERSE
           DISPLAY WK-MASCARA-HORA LINE 24 POSITION 60
           IF  ALTTIP-DOCTOS = 03
               PERFORM I-GRABA-COMP-EGRESO THRU F-GRABA-COMP-EGRESO.
           DISPLAY RADICDOC-DOCTRA   LINE 03 POSITION 16 REVERSE BLINK.
       F-MVTO-TABLA.
      *
       I-BORRA-REC.
           READ   CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                  GO F-BORRA-REC.
           IF     RADICTIP-DOCTOS > ALTTIP-DOCTRA   GO F-BORRA-REC.
           IF     RADICDOC-DOCTOS > RADICDOC-DOCTRA GO F-BORRA-REC.
           IF     SW-99  =  1
                         PERFORM I-REVERSA-ACUM     THRU F-REVERSA-ACUM.
           DELETE CNDOCTOS.
           GO     I-BORRA-REC.
       F-BORRA-REC.
      *
       I-GRABA-AUDITORIA.
           PERFORM I-STATUS-BORRADOS   THRU F-STATUS-BORRADOS.
           OPEN I-O BORRADOS.
           MOVE ALTTIP-DOCTOS           TO TIP-BORRADO.
           MOVE RADICDOC-DOCTOS         TO DOC-BORRADO.
           MOVE RADIC-AUTOMATICA-DOCTOS TO SEC-BORRADO.
           ACCEPT FECHA-BORRADO      FROM DATE.
           ACCEPT HORA-BORRADO       FROM TIME.
           MOVE SIGNO-DOCTOS           TO  SIGNO-BORRADO
           MOVE ALTEMP-DOCTOS          TO  EMPRESA-BORRADO
           MOVE VALOR-DOCTOS           TO  VALOR-BORRADO
           MOVE ALTNIT-DOCTOS          TO  NIT-BORRADO
           MOVE NUMFRA-DOCTOS          TO  NUMFRA-BORRADO
           MOVE CUENTA-DOCTOS          TO  CUENTA-BORRADO
           MOVE CON-USERNAME           TO  USUARIO-BORRADO
           MOVE FECHOY-DOCTOS          TO  FECHA-DOC-BORRADO
           WRITE  REC-BORRADOS  INVALID KEY REWRITE REC-BORRADOS.
           CLOSE BORRADOS.
       F-GRABA-AUDITORIA.
      *
      *
       I-GRABA-AUDITORIA1.
           PERFORM I-STATUS-BORRADOS   THRU F-STATUS-BORRADOS.
           OPEN I-O BORRADOS.
           MOVE ALTTIP-DOCTRA           TO TIP-BORRADO.
           MOVE RADICDOC-DOCTRA         TO DOC-BORRADO.
           MOVE J                       TO SEC-BORRADO
           ACCEPT FECHA-BORRADO      FROM DATE.
           ACCEPT HORA-BORRADO       FROM TIME.
           MOVE TAB-SIGNO(J)           TO  SIGNO-BORRADO
           MOVE ALTEMP-DOCTRA          TO  EMPRESA-BORRADO
           MOVE TAB-VLRDOC(J)          TO  VALOR-BORRADO
           MOVE TAB-NIT(J)             TO  NIT-BORRADO
           MOVE TAB-NUMFRA(J)          TO  NUMFRA-BORRADO
           MOVE TAB-CUENTA(J)          TO  CUENTA-BORRADO
           MOVE CON-USERNAME           TO  USUARIO-BORRADO
           MOVE FECHOY-DOCTOS          TO  FECHA-DOC-BORRADO
           WRITE  REC-BORRADOS  INVALID KEY REWRITE REC-BORRADOS.
           CLOSE BORRADOS.
       F-GRABA-AUDITORIA1.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS DE PROCESOS DIFERENTES                    ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
      *
       I-VERIFICA-FECHA.
             MOVE 1                    TO  WIN-CODIGO-EMPRESA
             PERFORM I-LEE-EMPRESA   THRU  F-LEE-EMPRESA.
             IF WIN-ULT-ACT-LIB-ANO  = CON-ANO   AND
                WIN-ULT-ACT-LIB-MES  = W-LONG
                PERFORM I-DISPLAY-ERROR  THRU F-DISPLAY-ERROR
                PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-VERIFICA-FECHA.
      *
       I-ACTUALIZADO.
           DISPLAY "DOCUMENTO EN CONDICION 1 *ACTUALIZADO*   "
           LINE 24   POSITION 01 REVERSE
           DISPLAY "<CTROL +  END>   PARA MODIFICARLO"
           LINE 24   POSITION 40 REVERSE LOW BLINK BEEP.
           PERFORM ACEPTA-TRUCO.
           IF CEND
               MOVE  1     TO SW-99
               MOVE  1     TO SW-SALDO
            ELSE
               MOVE 4      TO SW-99.
       F-ACTUALIZADO.
      *
       I-EVAL-SALDOS.
           DISPLAY "DEBITOS:"  LINE 19 POSITION 54
           DISPLAY "CREDITO:"  LINE 20 POSITION 54
           DISPLAY "SALDOS :"  LINE 21 POSITION 54.
           MOVE ZEROS TO WK-DIFSAL W-TOTAL-CREDITOS W-TOTAL-DEBITOS.
           MOVE ZEROS TO K.
         I-SUMTAB.
           ADD  1 TO K.
           IF   K  > 300 OR TAB-CUENTA(K) = ZEROS GO F-EVAL-SALDOS.
           COMPUTE WK-DIFSAL = WK-DIFSAL + TAB-VLRDOC(K).
           IF TAB-VLRDOC(K) < ZEROS
              ADD TAB-VLRDOC(K) TO W-TOTAL-CREDITOS
           ELSE
              ADD TAB-VLRDOC(K) TO W-TOTAL-DEBITOS.
           MOVE    W-TOTAL-DEBITOS   TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 19   POSITION 62 REVERSE LOW.
           MOVE    W-TOTAL-CREDITOS  TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 20   POSITION 62 REVERSE LOW.
           COMPUTE SALDO = W-TOTAL-CREDITOS + W-TOTAL-DEBITOS.
           MOVE    SALDO             TO  WI-Z9P
           DISPLAY WI-Z9P LINE 21   POSITION 62 REVERSE LOW.
           GO      I-SUMTAB.
       F-EVAL-SALDOS.
      *
       I-PANTA-ANT.
           ADD 1 TO M.
           ADD 1 TO N.
           IF N > 15  COMPUTE M = M - 1
                      COMPUTE N = N - 1
                      MOVE M TO J
                      MOVE N TO I  GO F-PANTA-ANT.
           DISPLAY TAB-MAYCTA(M)   LINE N POSITION 03 REVERSE LOW
           DISPLAY TAB-SUBCTA(M)   LINE N POSITION 09 REVERSE LOW
           DISPLAY TAB-AUXCTA(M)   LINE N POSITION 12 REVERSE LOW
           MOVE    TAB-NUMFRA(M)   TO   WI-Z6
           DISPLAY WI-Z6           LINE N POSITION 30 REVERSE LOW
           DISPLAY TAB-CONCEPTO(M) LINE N POSITION 17
                                   SIZE 12 REVERSE LOW.
           MOVE    TAB-VLRDOC(M)   TO   WI-Z11P.
           DISPLAY WI-Z11P         LINE N POSITION 54 REVERSE LOW
           MOVE    TAB-VLRBAS(M)   TO   WI-Z9.
           DISPLAY WI-Z9           LINE N POSITION 41 REVERSE LOW
           DISPLAY TAB-SIGNO(M)    LINE N POSITION 76 REVERSE LOW.
           IF      TAB-TIPREG(J) = "R"
                   DISPLAY TAB-CONCEPTO(M) LINE N POSITION 17
                                           SIZE 12 BLINK REVERSE LOW.
           GO I-PANTA-ANT.
       F-PANTA-ANT.
      *
       I-PANTA-REG.
           DISPLAY TAB-MAYCTA(J)   LINE I POSITION 03 REVERSE LOW
           DISPLAY TAB-SUBCTA(J)   LINE I POSITION 09 REVERSE LOW
           DISPLAY TAB-AUXCTA(J)   LINE I POSITION 12 REVERSE LOW
           MOVE    TAB-NUMFRA(J)   TO   WI-Z6
           DISPLAY WI-Z6           LINE I POSITION 30  REVERSE LOW
           DISPLAY TAB-CONCEPTO(J) LINE I POSITION 17
                                   SIZE 12 REVERSE LOW
           MOVE    TAB-VLRDOC(J)   TO   WI-Z11P.
           DISPLAY WI-Z11P         LINE I POSITION 54 REVERSE LOW
           MOVE    TAB-VLRBAS(J)   TO   WI-Z9.
           DISPLAY WI-Z9           LINE I POSITION 41 REVERSE LOW
           DISPLAY TAB-SIGNO(J)    LINE I POSITION 76 REVERSE LOW.
       F-PANTA-REG.
      *
       I-PANTA-7REG.
           MOVE  8     TO I.
         I-LOOP-K.
           ADD   1 TO K.
           ADD   1 TO I.
           IF    I  > 15 GO F-PANTA-7REG.
           IF    TAB-MAYCTA(K) = ZEROS  GO I-LOOP-K.
           DISPLAY TAB-MAYCTA(K)   LINE I POSITION 03 REVERSE LOW
           DISPLAY TAB-SUBCTA(K)   LINE I POSITION 09 REVERSE LOW
           DISPLAY TAB-AUXCTA(K)   LINE I POSITION 12 REVERSE LOW
           MOVE    TAB-NUMFRA(K)   TO   WI-Z6
           DISPLAY WI-Z6           LINE I POSITION 30 REVERSE LOW
           DISPLAY TAB-CONCEPTO(K) LINE I POSITION 17
                                   SIZE 12 REVERSE LOW
           MOVE    TAB-VLRDOC(K)   TO   WI-Z11P.
           DISPLAY WI-Z11P         LINE I POSITION 54 REVERSE LOW
           MOVE    TAB-VLRBAS(K)   TO   WI-Z9.
           DISPLAY WI-Z9           LINE I POSITION 41 REVERSE LOW
           DISPLAY TAB-SIGNO(K)    LINE I POSITION 76 REVERSE LOW.
           GO      I-LOOP-K.
       F-PANTA-7REG.
      *
       I-PANTA-MOS.
           MOVE  8     TO   LL.
           MOVE  0     TO   KR.
         I-LOOP-Q.
           ADD   1 TO KR.
           ADD   1 TO LL.
           IF    KR  > 250
                 GO F-PANTA-MOS.
           IF    TAB-MAYCTA(KR) = ZEROS  GO I-LOOP-Q.
           IF    LL  > 15
                 GO F-PANTA-MOS.
           DISPLAY TAB-MAYCTA(KR)   LINE LL POSITION 03 REVERSE LOW
           DISPLAY TAB-SUBCTA(KR)   LINE LL POSITION 09 REVERSE LOW
           DISPLAY TAB-AUXCTA(KR)   LINE LL POSITION 12 REVERSE LOW
           MOVE    TAB-NUMFRA(KR)   TO   WI-Z6
           DISPLAY WI-Z6           LINE LL POSITION 30 REVERSE LOW
           DISPLAY TAB-CONCEPTO(KR) LINE LL POSITION 17
                                   SIZE 12 REVERSE LOW
           MOVE    TAB-VLRDOC(KR)   TO   WI-Z11P.
           DISPLAY WI-Z11P         LINE LL POSITION 54 REVERSE LOW
           MOVE    TAB-VLRBAS(KR)   TO   WI-Z9.
           DISPLAY WI-Z9           LINE LL POSITION 41 REVERSE LOW
           DISPLAY TAB-SIGNO(KR)    LINE LL POSITION 76 REVERSE LOW.
           GO      I-LOOP-Q.
       F-PANTA-MOS.
      *
       I-MOSTRAR-CAB.
           MOVE ALTNIT-DOCTRA      TO WI-Z9O CODIM   WK-NIT WK-NIT1
           DISPLAY WI-Z9O          LINE 06 POSITION 16 REVERSE LOW
           PERFORM I-LEA-CNNITCED  THRU F-LEA-CNNITCED
           DISPLAY NOMBM        LINE 06 POSITION 32 REVERSE LOW SIZE 16
           IF  ALTNIT-DOCTRA < 1000
               MOVE NITAUX-DOCTRA    TO WI-Z9O CODIM   WK-NIT WK-NIT1
               DISPLAY WI-Z9O   LINE 06 POSITION 16 REVERSE LOW
               PERFORM I-LEA-CNNITCED  THRU F-LEA-CNNITCED
               DISPLAY NOMBM    LINE 06 POSITION 32 REVERSE LOW SIZE 16.

           DISPLAY ALTPER-DOCTRA        LINE 06 POSITION 76 REVERSE LOW
           DISPLAY ALTEMP-DOCTRA        LINE 05 POSITION 16 REVERSE LOW
           DISPLAY ALTTIP-DOCTRA        LINE 02 POSITION 16 REVERSE LOW
           DISPLAY ALTFRA-DOCTRA        LINE 03 POSITION 37 REVERSE LOW.
           DISPLAY RADICDOC-DOCTRA      LINE 03 POSITION 16 REVERSE LOW
           MOVE ALTEMP-DOCTRA        TO WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESA   THRU F-LEE-EMPRESA.
           DISPLAY ALTEMP-DOCTRA        LINE 05 POSITION 16 REVERSE LOW
           DISPLAY NUM-SOPORTE-DOCTRA   LINE 06 POSITION 56 REVERSE LOW
           DISPLAY CHEQ-HASTA-DOCTRA    LINE 06 POSITION 66 REVERSE LOW
           MOVE FECDOC-DOCTRA           TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA    LINE 04 POSITION 16.
           MOVE ALT07-DOCTRA           TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 04 POSITION 37.
           DISPLAY TAB-CONCEPTO(1)      LINE 17 POSITION 14 REVERSE LOW.

           IF ALTTIP-DOCTRA = 1
           MOVE BANCO-BENEF-DOCTRA       TO  WI-Z3   BANC-CLAVE
           PERFORM I-LEE-BANCOS        THRU  F-LEE-BANCOS
           DISPLAY WI-Z3               LINE  3 POSITION 61 REVERSE LOW
           DISPLAY BANC-NOMBRE         LINE 3 POSITION 65 REVERSE
                                       LOW SIZE 14
           MOVE CHEQUE-BENEF-DOCTRA    TO WI-Z7
           DISPLAY WI-Z7               LINE  4 POSITION 61 REVERSE LOW.

           IF ALTTIP-DOCTRA = 1  OR ALTTIP-DOCTRA = 3
           MOVE BANCO-DOCTRA           TO WI-Z6   BANC-CLAVE
           PERFORM I-LEE-BANCOS      THRU F-LEE-BANCOS
           DISPLAY WI-Z3             LINE 5 POSITION 61 REVERSE LOW
           DISPLAY BANC-NOMBRE       LINE 5 POSITION 66 REVERSE
                                            LOW SIZE 14.
       F-MOSTRAR-CAB.
      *
       I-MUESTRA-CAB.
           IF W-OPCION = "M"
              IF MOD-USRKK(1) = "N" GO F-MUESTRA-CAB.
       F-MUESTRA-CAB.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥     RUTINAS DE CAPTURA DE LOS CAMPOS                    ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-ACEPTA-MES.
           MOVE ZEROS TO WK-EXCEPTION
           ACCEPT W-LONG LINE 11 POSITION 50
               CONVERT NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1
           IF W-LONG = 0 OR W-LONG > 14
              DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-ACEPTA-MES.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 54 .
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       F-ACEPTA-MES.

       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32 .
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "C" GO TO I-CONFIRMA-COMP
           ELSE DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-TIPCOM.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY "F1 - AYUDA COMPROBANTES"
                    LINE 24 POSITION 1 REVERSE BLINK LOW
           DISPLAY ALTTIP-DOCTRA     LINE 02 POSITION 16 REVERSE LOW
           ACCEPT  ALTTIP-DOCTRA     LINE 02 POSITION 16
                                       UPDATE PROMPT
                                       NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY ALTTIP-DOCTRA     LINE 02 POSITION 16 REVERSE LOW
           IF  CUP OR ESC GO F-TIPCOM.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF F1
              OPEN INPUT CNCOMPTE
              PERFORM I-HELP-COMPROBANTES THRU  F-HELP-COMPROBANTES
              UNTIL ESC
              CLOSE  CNCOMPTE
              MOVE ZEROS  TO  WK-EXCEPTION
              PERFORM I-WIN-CTAS   THRU F-WIN-CTAS
              GO I-TIPCOM.
           IF W-OPCION = "C"  GO F-TIPCOM.
           MOVE ALTTIP-DOCTRA TO ALT09TIP-DOCTRA .
           MOVE ALTTIP-DOCTRA TO CODIM-COMP.
           OPEN INPUT CNCOMPTE.
           READ CNCOMPTE WITH NO LOCK INVALID KEY
               DISPLAY "COMPROBANTE  ERRADO   "
               LINE 24 POSITION 01 REVERSE
               CLOSE CNCOMPTE
               GO I-TIPCOM.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           DISPLAY NOMBM-COMP LINE 02 POSITION 28 SIZE 20 REVERSE LOW.
           CLOSE CNCOMPTE.
       F-TIPCOM.
      *
       I-DOCUMENTO.
           MOVE ZEROS                  TO  WK-EXCEPTION
      *    COMPUTE RADICDOC-DOCTRA = RADICDOC-DOCTRA + 1.
           DISPLAY RADICDOC-DOCTRA   LINE 03 POSITION 16 REVERSE LOW
           ACCEPT RADICDOC-DOCTRA    LINE 03 POSITION 16
                  UPDATE PROMPT NO BEEP REVERSE LOW
                  ON EXCEPTION WK-EX PERFORM 999-EXCEPTION.
           DISPLAY RADICDOC-DOCTRA LINE 03 POSITION 16 REVERSE LOW
           IF  CUP OR ESC GO F-DOCUMENTO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1
              PERFORM I-AUDIT         THRU  F-AUDIT
              GO TO I-DOCUMENTO.
           IF ALTTIP-DOCTRA = 12
              PERFORM I-TIPO-12    THRU  F-TIPO-12.
           IF ALTTIP-DOCTRA = 14
              PERFORM I-TIPO-12    THRU  F-TIPO-12.
           IF W-OPCION = "C"  GO F-DOCUMENTO.
           IF RADICDOC-DOCTRA = ZEROS
              DISPLAY " DOCUMENTO  ERRADO    "
              LINE 24 POSITION 01 REVERSE
              GO I-DOCUMENTO.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
       F-DOCUMENTO.
      *
       I-DOCU1.
           MOVE ZEROS              TO  WK-EXCEPTION
           DISPLAY ALTFRA-DOCTRA     LINE 03 POSITION 37 REVERSE LOW
           ACCEPT  ALTFRA-DOCTRA     LINE 03 POSITION 37
                  UPDATE PROMPT NO BEEP REVERSE LOW
                  ON EXCEPTION WK-EX PERFORM 999-EXCEPTION.
           DISPLAY ALTFRA-DOCTRA     LINE 03 POSITION 37 REVERSE LOW
           IF  CUP OR ESC GO F-DOCU1.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ALTFRA-DOCTRA = ZEROS
              DISPLAY " DOCUMENTO  ERRADO    "
              LINE 24 POSITION 01 REVERSE
              GO I-DOCU1.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
       F-DOCU1.
      *
       I-EMPRESA.
           DISPLAY "F1 - AYUDA EMPRESAS-SUCURSALES"
                    LINE 24 POSITION 1 REVERSE BLINK LOW
           MOVE W-CODEMP               TO ALTEMP-DOCTRA
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY ALTEMP-DOCTRA     LINE 05 POSITION 16 REVERSE LOW.
           ACCEPT  ALTEMP-DOCTRA     LINE 05 POSITION 16
                                       UPDATE PROMPT REVERSE LOW
                                       NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           IF  F8  AND
              ALTTIP-DOCTRA = 04
              PERFORM I-CIUDAD      THRU  F-CIUDAD
              GO F-EMPRESA.
           DISPLAY ALTEMP-DOCTRA   LINE 05 POSITION 16 REVERSE LOW
           IF  CUP OR ESC GO F-EMPRESA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1
               CLOSE EMPRESAS
               PERFORM I-AYUDA-EMPRE THRU F-AYUDA-EMPRE
               OPEN INPUT EMPRESAS
               GO I-EMPRESA.
           IF  ALTEMP-DOCTRA = ZEROS   GO I-EMPRESA.
           MOVE ALTEMP-DOCTRA TO WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESA THRU F-LEE-EMPRESA.
           MOVE    WIN-NIT   TO   WK-NIT1  WI-Z9O.

           IF  SW-1  =  1
                  GO I-EMPRESA.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           DISPLAY ALTEMP-DOCTRA LINE 05  POSITION 16 REVERSE LOW.
           DISPLAY WIN-EMPRESA  LINE 05 POSITION 28 REVERSE LOW SIZE 20.
           PERFORM I-LIMPIA      THRU   F-LIMPIA.
           PERFORM I-WIN-CTAS    THRU   F-WIN-CTAS.
           PERFORM I-PANTA-MOS   THRU   F-PANTA-MOS.
        F-EMPRESA.
      *
       I-FECHA-DESDE.
           DISPLAY SPACES LINE 4 POSITION 16 SIZE 11
           MOVE ZEROS                  TO WK-EXCEPTION
           MOVE CON-ANO                TO AA-COMP-DOCTRA
           MOVE W-LONG                 TO MM-COMP-DOCTRA
           IF   DD-COMP-DOCTRA = ZEROS
                MOVE 01 TO DD-COMP-DOCTRA.
           DISPLAY AA-COMP-DOCTRA      LINE 04 POSITION 16 REVERSE LOW
           DISPLAY MM-COMP-DOCTRA      LINE 04 POSITION 21 REVERSE LOW.
           DISPLAY DD-COMP-DOCTRA      LINE 04 POSITION 24 REVERSE LOW.
           DISPLAY "DIGITE MES MOVIMIENTO"
                   LINE 24 POSITION 01 REVERSE BLINK LOW.
           ACCEPT  MM-COMP-DOCTRA      LINE 04 POSITION 21 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY AA-COMP-DOCTRA      LINE 04 POSITION 16 REVERSE LOW
           DISPLAY MM-COMP-DOCTRA      LINE 04 POSITION 21 REVERSE LOW
           DISPLAY "DIGITE DIA MOVIMIENTO"
                  LINE 24 POSITION 01 REVERSE BLINK LOW.
           ACCEPT  DD-COMP-DOCTRA      LINE 04 POSITION 24 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-FECHA-DESDE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE FECDOC-DOCTRA TO   WK-FECHA-HOY.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 1
              DISPLAY "FECHA ERRADA   "
              LINE 24 POSITION 01 REVERSE
              GO I-FECHA-DESDE.
           IF W-OPCION = "A"  AND ALTTIP-DOCTRA = 4
              PERFORM I-VALIDA-HASTA  THRU F-VALIDA-HASTA.
           MOVE FECDOC-DOCTRA          TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 04 POSITION 16.

           DISPLAY SPACE LINE 24 POSITION 01 SIZE 22.
       F-FECHA-DESDE.
      *
       I-VALIDA-HASTA.
           MOVE FECDOC-DOCTRA        TO  WK-FECHA-STANDARD
           COMPUTE W-VALOR-ENTERO       =  WIN-PLAZO-FACTURA
                                        /  30
           ADD  W-VALOR-ENTERO         TO  WK-FECHA-STANDARD-MES
           COMPUTE W-VALOR-ENTERO       =  WIN-PLAZO-FACTURA
                                        -  W-VALOR-ENTERO
                                        *  30
           ADD  W-VALOR-ENTERO         TO  WK-FECHA-STANDARD-DIA
           IF  WK-FECHA-STANDARD-DIA        >  30
               SUBTRACT 30               FROM  WK-FECHA-STANDARD-DIA
               ADD  1                      TO  WK-FECHA-STANDARD-MES.
           IF  WK-FECHA-STANDARD-MES        >  12
               SUBTRACT 12               FROM  WK-FECHA-STANDARD-MES
               ADD  1                      TO  WK-FECHA-STANDARD-ANO.
           MOVE WK-FECHA-STANDARD      TO  ALT07-DOCTRA.
       F-VALIDA-HASTA.

       I-FECHA-HASTA1.
           DISPLAY SPACES LINE 4 POSITION 37 SIZE 11
           MOVE ZEROS                  TO  WK-EXCEPTION
           IF W-OPCION = "A" AND  ALTTIP-DOCTRA NOT = 4
              MOVE W-LONG  TO MM-VCTO-DOCTRA.
              DISPLAY W-LONG           LINE 04 POSITION 42 REVERSE LOW.
           IF W-OPCION = "A"
              MOVE CON-ANO TO AA-VCTO-DOCTRA
              DISPLAY CON-ANO LINE 04 POSITION 37 REVERSE LOW.

           IF AA-VCTO-DOCTRA   = ZEROS OR AA-VCTO-DOCTRA   < CON-ANO
              MOVE CON-ANO TO AA-VCTO-DOCTRA  .
           DISPLAY WK-MM               LINE 04 POSITION 42 REVERSE LOW
           DISPLAY WK-DD               LINE 04 POSITION 45 REVERSE LOW
           DISPLAY "DIGITE MES MOVIMIENTO"
                   LINE 24 POSITION 01 REVERSE BLINK LOW.
           ACCEPT  MM-VCTO-DOCTRA      LINE 04 POSITION 42 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-FECHA-HASTA1.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-DD               LINE 04 POSITION 45 REVERSE LOW
           IF DD-VCTO-DOCTRA = ZEROS
              MOVE 01 TO DD-VCTO-DOCTRA.
           DISPLAY "DIGITE DIA MOVIMIENTO"
                  LINE 24 POSITION 01 REVERSE BLINK LOW.
           ACCEPT  DD-VCTO-DOCTRA      LINE 04 POSITION 45 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-FECHA-HASTA1.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE ALT07-DOCTRA TO   WK-FECHA-HOY.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 1
              DISPLAY "FECHA ERRADA   "
              LINE 24 POSITION 01 REVERSE
              GO I-FECHA-HASTA1.
           DISPLAY DD-VCTO-DOCTRA      LINE 04 POSITION 45 REVERSE LOW.
           MOVE ALT07-DOCTRA           TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 04 POSITION 37.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 22.
       F-FECHA-HASTA1.
      *
       I-FECHA-CONTAB.
           PERFORM I-BLQPAN  THRU F-CREA-NIT.
           DISPLAY "FECHA CONTABILIZACION : AA-MM-DD"
                   LINE 21 POSITION 1 .
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-ANO-DOCTRA      LINE 21 POSITION 25
           DISPLAY WK-MES-DOCTRA      LINE 21 POSITION 28
           DISPLAY WK-DIA-DOCTRA      LINE 21 POSITION 31.
       I-FECHA-1.
           MOVE    CON-ANO          TO  WK-ANO-DOCTRA
           MOVE    ZEROS            TO  WK-EXCEPTION
           ACCEPT  WK-MES-DOCTRA      LINE 21 POSITION 28
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       I-FECHA-2.
           MOVE    ZEROS            TO  WK-EXCEPTION
           ACCEPT  WK-DIA-DOCTRA      LINE 21 POSITION 31
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO I-FECHA-1.
           MOVE FECHA-CONTAB-DOCTRA TO   WK-FECHA-HOY.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 1
              DISPLAY "FECHA ERRADA   "
              LINE 24 POSITION 01 REVERSE
              GO I-FECHA-1.
           DISPLAY WK-ANO-DOCTRA      LINE 21 POSITION 25
           DISPLAY WK-MES-DOCTRA      LINE 21 POSITION 28.
           DISPLAY WK-DIA-DOCTRA      LINE 21 POSITION 31.
       F-FECHA-CONTAB.
      *
       I-NIT.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "N.I.T/CEDULA: " LINE 19 POSITION 1.
           IF CODPGT = "N"
              MOVE ZEROS TO TAB-NIT(J) GO F-NIT.
           MOVE ZEROS                  TO  WK-EXCEPTION
           IF   TAB-NIT(J) = ZEROS
                NEXT SENTENCE
           ELSE
                MOVE TAB-NIT(J) TO WK-NIT.
           MOVE WK-NIT TO WI-Z9O.
           DISPLAY WI-Z9O             LINE 19 POSITION 15
           ACCEPT WK-NIT               LINE 19 POSITION 15
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE TAB-NIT(J)      TO WI-Z9O
           DISPLAY WI-Z9O      LINE 19 POSITION 15
           MOVE WK-NIT          TO TAB-NIT(J).
      *    IF  WK-NIT        < 1000
      *        DISPLAY "N.I.T/CEDULA no puede ser < 1000 "
      *        LINE 24 POSITION 01 REVERSE BEEP
      *        GO I-NIT.
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT1   THRU F-AYUDA-NIT1
               OPEN I-O CNNITCED
               GO I-NIT.
           IF  CUP OR ESC GO F-NIT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  TAB-NIT(J) = ZEROS
               DISPLAY "N.I.T/CEDULA no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT.
           MOVE TAB-NIT(J)      TO WI-Z9O CODIM
           DISPLAY WI-Z9O         LINE 19 POSITION 15
           PERFORM I-LEA-CNNITCED THRU F-LEA-CNNITCED
           IF SW-EOF = 1
              DISPLAY "NIT NO EXISTE - DESEA CREARLO?(S/N)"
              LINE 24 POSITION 01  REVERSE
              PERFORM I-CREA-NIT THRU F-CREA-NIT
              GO I-NIT.
           DISPLAY NOMBM  LINE 19 POSITION 31 SIZE 22.
           MOVE TAB-NIT(J)  TO  NITAUX-DOCTRA.
           IF  ALTTIP-DOCTRA = 12  GO F-NIT.
           IF  ALTTIP-DOCTRA = 14  GO F-NIT.
               PERFORM I-LIMPIA  THRU F-LIMPIA.
       F-NIT.
      *
       I-CREA-NIT.
           ACCEPT  CREA-NIT LINE 24 POSITION 51
           IF   CREA-NIT = "S" OR CREA-NIT = "N" NEXT SENTENCE
           ELSE GO I-CREA-NIT.
           IF CREA-NIT = "N" GO F-CREA-NIT.
           PERFORM I-WIN-NIT THRU F-WIN-NIT.
         I-ACNIT.
           MOVE TAB-NIT(J)      TO WI-Z9O CODIM
           DISPLAY WI-Z9O         LINE 20 POSITION 44
           ACCEPT  CODIM LINE 20 POSITION 44
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE CODIM             TO  TAB-NIT(J) WI-Z9O
           DISPLAY WI-Z9O         LINE 20 POSITION 44
           ACCEPT  DIGITO-CHEQUEO LINE 20 POSITION 59
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY DIGITO-CHEQUEO LINE 20 POSITION 59
           IF      CUP   GO I-BLQPAN.
           IF      F2    GO I-BLQPAN.
           IF      CODIM = ZEROS GO I-ACNIT.
         I-ACNOM.
           ACCEPT  NOMBM LINE 21 POSITION 44 SIZE 36
                   PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY NOMBM LINE 21 POSITION 44 SIZE 36
           IF      CUP   GO I-ACNIT.
           IF      F2    GO I-BLQPAN.
           IF      NOMBM =  SPACES GO I-ACNOM.
           DISPLAY " GRABA NIT  S/N : " LINE 22 POSITION 50.
         I-ACGRB.
           ACCEPT  RES-OK  LINE 22 POSITION 68
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF      CUP GO  I-ACNOM.
           IF      RES-OK  =  "S"  OR RES-OK = "N" NEXT SENTENCE
           ELSE    GO  I-ACGRB.
           IF      RES-OK  =  "N"  GO I-BLQPAN.
           MOVE    SPACES  TO  DIREM .
           MOVE    ZEROS   TO  TELEM
           MOVE    1       TO  CIUDM.
           WRITE   REGMA1.
         I-BLQPAN.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80
                      CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES LINE 19 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 20 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 21 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 22 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 23 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "DEBITOS:"  LINE 19 POSITION 54
           DISPLAY "CREDITO:"  LINE 20 POSITION 54
           DISPLAY "SALDOS :"  LINE 21 POSITION 54.
           MOVE    W-TOTAL-DEBITOS   TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 19   POSITION 62 REVERSE LOW.
           MOVE    W-TOTAL-CREDITOS  TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 20   POSITION 62 REVERSE LOW.
           COMPUTE SALDO = W-TOTAL-CREDITOS + W-TOTAL-DEBITOS.
           MOVE    SALDO             TO  WI-Z9P
           DISPLAY WI-Z9P LINE 21   POSITION 62 REVERSE LOW.
       F-CREA-NIT.
      *
       I-LIMPIA.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80
                      CONTROL SG-CONTROL-STRING.
      *    DISPLAY SPACES LINE 19 POSITION 30 SIZE 50.
           DISPLAY SPACES LINE 20 POSITION 30 SIZE 50.
           DISPLAY SPACES LINE 21 POSITION 30 SIZE 50.
           DISPLAY SPACES LINE 22 POSITION 30 SIZE 50.
           DISPLAY SPACES LINE 23 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "DEBITOS:"  LINE 19 POSITION 54
           DISPLAY "CREDITO:"  LINE 20 POSITION 54
           DISPLAY "SALDOS :"  LINE 21 POSITION 54.
           MOVE    W-TOTAL-DEBITOS   TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 19   POSITION 62 REVERSE LOW.
           MOVE    W-TOTAL-CREDITOS  TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 20   POSITION 62 REVERSE LOW.
           COMPUTE SALDO = W-TOTAL-CREDITOS + W-TOTAL-DEBITOS.
           MOVE    SALDO             TO  WI-Z9P
           DISPLAY WI-Z9P LINE 21   POSITION 62 REVERSE LOW.
       F-LIMPIA.
      *
       I-WIN-NIT.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "…ÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                       LINE 19 POSITION 32 CONTROL SG-CONTROL-STRING
           DISPLAY "∫ NIT No. ∫                                     ∫"
                                                 LINE 20 POSITION 32
           DISPLAY "∫ Nombre  ∫                                     ∫"
                                                 LINE 21 POSITION 32
           DISPLAY "»ÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                                 LINE 22 POSITION 32.
       F-WIN-NIT.
      *
       I-NIT1.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           MOVE ZEROS             TO  WK-EXCEPTION
           IF  ALTEMP-DOCTRA = 1  GO I-N.
           IF  WK-NIT1 = ZEROS
               MOVE    WIN-NIT   TO   WK-NIT1  WI-Z9O.
       I-N.
           MOVE    WK-NIT1        TO  WI-Z9O.
           DISPLAY WI-Z9O      LINE  06 POSITION 16 REVERSE LOW
           ACCEPT  WK-NIT1            LINE 06 POSITION 16
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE WK-NIT1         TO WI-Z9O
           DISPLAY WI-Z9O       LINE 06 POSITION 16 REVERSE LOW
           IF  CUP OR ESC GO F-NIT1.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT   THRU F-AYUDA-NIT
               OPEN I-O CNNITCED
               GO I-NIT1.
           IF  WK-NIT1   < 1000
               DISPLAY "N.I.T/CEDULA no puede ser < 1000 "
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT1.
           IF  WK-NIT1    = ZEROS
               DISPLAY "N.I.T/CEDULA no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT1.
           MOVE WK-NIT1          TO WI-Z9O  CODIM  WK-NIT
           DISPLAY WI-Z9O          LINE 06 POSITION 16 REVERSE LOW
           PERFORM I-LEA-CNNITCED THRU F-LEA-CNNITCED.
           IF SW-EOF = 1
              DISPLAY "NIT NO EXISTE - DESEA CREARLO?(S/N)"
              LINE 24 POSITION 01  REVERSE
              PERFORM I-CREA-NIT1 THRU F-CREA-NIT1
              GO I-NIT1.
           PERFORM I-LIMPIA  THRU F-LIMPIA.
           DISPLAY NOMBM  LINE 06 POSITION 32 SIZE 16 REVERSE LOW.
           PERFORM I-WIN-CTAS    THRU   F-WIN-CTAS.
           PERFORM I-PANTA-MOS   THRU   F-PANTA-MOS.
       F-NIT1.
      *
       I-CREA-NIT1.
           ACCEPT  CREA-NIT LINE 24 POSITION 51
           IF   CREA-NIT = "S" OR CREA-NIT = "N" NEXT SENTENCE
           ELSE                   GO   I-CREA-NIT1.
           IF CREA-NIT = "N"      GO   F-CREA-NIT1.
           PERFORM I-WIN-NIT      THRU F-WIN-NIT.
         I-ACNIT1.
           MOVE WK-NIT1           TO   WI-Z9O CODIM
           DISPLAY WI-Z9O         LINE 20 POSITION 44
           ACCEPT  CODIM          LINE 20 POSITION 44
                   UPDATE PROMPT  NO BEEP
                   ON EXCEPTION   WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE CODIM             TO   WI-Z9O
           DISPLAY WI-Z9O         LINE 20 POSITION 44
           ACCEPT  DIGITO-CHEQUEO LINE 20 POSITION 59
                   UPDATE PROMPT  NO BEEP
                   ON EXCEPTION   WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY DIGITO-CHEQUEO LINE 20 POSITION 59
           IF      CUP            GO I-BLQPAN1.
           IF      F2             GO I-BLQPAN1.
           IF      CODIM = ZEROS  GO I-ACNIT1.
         I-ACNOM1.
           ACCEPT  NOMBM LINE 21  POSITION 44 SIZE 36
                   PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY NOMBM LINE 21  POSITION 44 SIZE 36
           IF      CUP            GO I-ACNIT1.
           IF      F2             GO I-BLQPAN1.
           IF      NOMBM = SPACES GO I-ACNOM1.
           DISPLAY " GRABA NIT  S/N : " LINE 22 POSITION 50.
         I-ACGRB1.
           ACCEPT  RES-OK         LINE 22 POSITION 68
                   UPDATE PROMPT  NO BEEP
                   ON EXCEPTION   WK-EX
                      PERFORM 999-EXCEPTION.
           IF      CUP            GO  I-ACNOM1.
           IF      RES-OK  =  "S" OR RES-OK = "N" NEXT SENTENCE
           ELSE    GO  I-ACGRB1.
           IF      RES-OK  =  "N" GO I-BLQPAN1.
           MOVE    SPACES  TO  DIREM .
           MOVE    ZEROS   TO  TELEM
           MOVE    1       TO  CIUDM.
           WRITE   REGMA1.
         I-BLQPAN1.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80
                      CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES LINE 19 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 20 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 21 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 22 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 23 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24 POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "DEBITOS:"     LINE 19 POSITION 54
           DISPLAY "CREDITO:"     LINE 20 POSITION 54
           DISPLAY "SALDOS :"     LINE 21 POSITION 54.
           MOVE    W-TOTAL-DEBITOS   TO  WI-Z9P.
           DISPLAY WI-Z9P         LINE 19   POSITION 62 REVERSE LOW.
           MOVE    W-TOTAL-CREDITOS  TO  WI-Z9P.
           DISPLAY WI-Z9P LINE 20   POSITION 62 REVERSE LOW.
           COMPUTE SALDO = W-TOTAL-CREDITOS + W-TOTAL-DEBITOS.
           MOVE    SALDO             TO  WI-Z9P
           DISPLAY WI-Z9P LINE 21   POSITION 62 REVERSE LOW.
       F-CREA-NIT1.

       I-CIUDAD.
           OPEN INPUT NOMICIUD.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80 BEEP
                      CONTROL SG-CONTROL-STRING HIGH.
           DISPLAY "CIUDAD : " LINE 21 POSITION 1 .
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WR-CIUDAD           LINE 21 POSITION 15
           ACCEPT  WR-CIUDAD           LINE 21 POSITION 15
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY WR-CIUDAD    LINE 21 POSITION 15
           MOVE    WR-CIUDAD     TO  CIUD-CODIGO.
           READ NOMICIUD WITH NO LOCK INVALID KEY
                DISPLAY "ERROR LEE CIUDAD    "
                         LINE 23 POSITION 1
                         CLOSE   NOMICIUD
                         GO I-CIUDAD.
           DISPLAY CIUD-NOMBRE LINE 21 POSITION 31.
           CLOSE      NOMICIUD.
       F-CIUDAD.
      *
       I-FECHA-HASTA.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE ALT07-DOCTRA         TO  TAB-FECVEN(J).
           MOVE TAB-FECVEN(J)          TO  WI-Z6.
           DISPLAY WI-Z6               LINE 21 POSITION 48
           ACCEPT TAB-FECVEN(J)        LINE 21 POSITION 48
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-FECHA-HASTA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE TAB-FECVEN(J) TO   WK-FECHA-HOY.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 1
              DISPLAY "FECHA ERRADA   "
              LINE 24 POSITION 01 REVERSE
              GO I-FECHA-HASTA.
           DISPLAY WK-MASCARA-FECHA  LINE  21 POSITION 48.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 22.
       F-FECHA-HASTA.
      *
       I-SOPORTE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NUM-SOPORTE-DOCTRA  LINE 06 POSITION 61 REVERSE LOW
           ACCEPT NUM-SOPORTE-DOCTRA   LINE 06 POSITION 61
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  NUM-SOPORTE-DOCTRA  = ZEROS
               MOVE RADICDOC-DOCTRA TO NUM-SOPORTE-DOCTRA.
           DISPLAY NUM-SOPORTE-DOCTRA
                        LINE 06 POSITION 61 REVERSE LOW.
           IF  CUP OR ESC GO F-SOPORTE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  NUM-SOPORTE-DOCTRA = ZEROS
               DISPLAY "SOPORTE DEBE SER > 0  "
               LINE 24 POSITION 01 REVERSE
               GO I-SOPORTE.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-SOPORTE.
      *
       I-SOPORTE-2.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CHEQ-HASTA-DOCTRA  LINE 06 POSITION 76 REVERSE LOW
           ACCEPT  CHEQ-HASTA-DOCTRA  LINE 06 POSITION 76
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY CHEQ-HASTA-DOCTRA
                        LINE 06 POSITION 76 REVERSE LOW.
           IF  CUP OR ESC GO F-SOPORTE-2.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CHEQ-HASTA-DOCTRA = ZEROS
               DISPLAY "SOPORTE DEBE SER > 0  "
               LINE 24 POSITION 01 REVERSE
               GO I-SOPORTE-2.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-SOPORTE-2.
      *
       I-PERIODO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE MM-COMP-DOCTRA         TO  ALTPER-DOCTRA.
      *    DISPLAY ALTPER-DOCTRA       LINE 06 POSITION 76 REVERSE LOW
      *    ACCEPT ALTPER-DOCTRA        LINE 06 POSITION 76
      *                                UPDATE PROMPT REVERSE LOW
      *                                NO BEEP
      *            ON EXCEPTION WK-EX
      *               PERFORM 999-EXCEPTION.
      *    DISPLAY ALTPER-DOCTRA       LINE 06 POSITION 76 REVERSE LOW.
      *    IF  CUP OR ESC GO F-PERIODO.
      *    IF  F2  PERFORM I-FIN-MODE  THRU F-FIN-MODE.
      *    IF  ALTPER-DOCTRA = ZEROS
      *        MOVE MM-COMP-DOCTRA     TO  ALTPER-DOCTRA
      *        DISPLAY ALTPER-DOCTRA   LINE 06 POSITION 76 REVERSE LOW
      *        DISPLAY "PERIODO DEBE SER > 0  "
      *        LINE 24 POSITION 01 REVERSE
      *        GO I-PERIODO.
      *    DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-PERIODO.
      *
       I-BANCO-B.
           MOVE       ZEROS              TO WK-EXCEPTION.
           MOVE  BANCO-BENEF-DOCTRA            TO WI-Z3.
           DISPLAY WI-Z3                  LINE 3 POSITION 61
                                          REVERSE LOW
           ACCEPT BANCO-BENEF-DOCTRA      LINE 3 POSITION 61
                   UPDATE PROMPT NO BEEP  REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-BANCO-B.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1  PERFORM I-HELP-BANCOS  THRU F-HELP-BANCOS.
           IF  BANCO-BENEF-DOCTRA        = ZEROS
               DISPLAY "Codigo del Banco no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-BANCO-B.
           MOVE BANCO-BENEF-DOCTRA     TO WI-Z3   BANC-CLAVE.
           DISPLAY WI-Z3                  LINE 3 POSITION 61
                                          REVERSE LOW
           PERFORM I-LEE-BANCOS      THRU F-LEE-BANCOS.
           IF SW-EOF = 1
              DISPLAY "BANCO - INEXISTENTE  -  INTENTE NUEVAMENTE :"
              LINE 24 POSITION    01  REVERSE
              GO  I-BANCO-B.
           DISPLAY BANC-NOMBRE  LINE 03 POSITION 65 REVERSE LOW SIZE 14.
           DISPLAY SPACES       LINE 24 POSITION 01 SIZE 80.
       F-BANCO-B.
      *
       I-CHEQUE-B.
           MOVE       ZEROS              TO WK-EXCEPTION.
           MOVE  CHEQUE-BENEF-DOCTRA            TO WI-Z7.
           DISPLAY WI-Z7                   LINE 4 POSITION 61 REVERSE
           ACCEPT CHEQUE-BENEF-DOCTRA      LINE 4 POSITION 61
                   UPDATE PROMPT NO BEEP  REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-CHEQUE-B.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE  CHEQUE-BENEF-DOCTRA            TO WI-Z7.
           DISPLAY WI-Z7                   LINE 4 POSITION 61 REVERSE
           IF  CHEQUE-BENEF-DOCTRA        = ZEROS
               DISPLAY "Numero de cheque no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-CHEQUE-B.
           DISPLAY SPACES       LINE 24 POSITION 01 SIZE 80.
       F-CHEQUE-B.
      *
       I-BANCO-P.
           MOVE  ZEROS              TO WK-EXCEPTION.
           MOVE  BANCO-DOCTRA       TO WI-Z3.
           DISPLAY WI-Z3            LINE 5 POSITION 61 REVERSE LOW
           ACCEPT BANCO-DOCTRA      LINE 5 POSITION 61
                   UPDATE PROMPT NO BEEP  REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-BANCO-P.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1  PERFORM I-HELP-BANCOS  THRU F-HELP-BANCOS.
           IF  BANCO-DOCTRA        = ZEROS
               DISPLAY "Codigo del Banco no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-BANCO-P.
           MOVE BANCO-DOCTRA     TO WI-Z3   BANC-CLAVE.
           DISPLAY WI-Z3            LINE 5 POSITION 61 REVERSE LOW
           PERFORM I-LEE-BANCOS      THRU F-LEE-BANCOS.
           IF SW-EOF = 1
              DISPLAY "BANCO - INEXISTENTE  -  INTENTE NUEVAMENTE :"
              LINE 24 POSITION    01  REVERSE
              GO  I-BANCO-P.
           DISPLAY BANC-NOMBRE  LINE 05 POSITION 65 REVERSE LOW SIZE 14.
           DISPLAY SPACES       LINE 24 POSITION 01 SIZE 80.
       F-BANCO-P.
      *
       I-MAYOR.
           IF ALTTIP-DOCTRA  = 12 GO I-MAY.
           IF ALTTIP-DOCTRA  = 14 GO I-MAY.
           PERFORM I-BLQPAN  THRU F-CREA-NIT.
       I-MAY.
           DISPLAY
           "F1-AYUDA PUC    CUIDADO!!!!  No oprima ESC si no ha salvado"
                   LINE 24 POSITION 1 REVERSE BLINK LOW.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE TAB-MAYCTA(J)          TO  WK-CUENTAS.
           DISPLAY TAB-CONDICION(J)    LINE 17 POSITION 75 REVERSE LOW.
           DISPLAY TAB-MAYCTA(J)       LINE I  POSITION 03 REVERSE LOW.
           PERFORM I-DISPLAY-CTAS      THRU  F-DISPLAY-CTAS.
           ACCEPT TAB-MAYCTA(J)        LINE I POSITION 03 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF TAB-TIPREG(J) = "R"
              MOVE WK-CUENTAS TO TAB-MAYCTA(J)
              DISPLAY TAB-MAYCTA(J) LINE I POSITION 03 REVERSE LOW
              GO   F-MAYOR.
           IF TAB-CONDICION(J) =  1  AND SW-99 = ZEROS
              MOVE WK-CUENTAS TO TAB-MAYCTA(J)
              DISPLAY TAB-MAYCTA(J) LINE I POSITION 03 REVERSE LOW
              GO   F-MAYOR.
           IF  PGDN AND J > 1
               COMPUTE K = J - 1
               MOVE TAB-MAYCTA(K) TO TAB-MAYCTA(J).
           DISPLAY TAB-MAYCTA(J) LINE I POSITION 03 REVERSE LOW.
           IF  F1
               CLOSE CNCATCTA
               PERFORM I-AYUDA-CNCATCTA THRU F-AYUDA-CNCATCTA
               OPEN I-O CNCATCTA
               GO I-MAY.
           IF  CUP OR HOME  OR ESC OR F1
               DISPLAY SPACES LINE 24 POSITION 01 SIZE 30
               GO F-MAYOR.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE ZEROS TO  SUBMAE AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               DISPLAY "MAYOR  NO EXISTE    "
               LINE 24 POSITION 01 REVERSE
               GO I-MAYOR.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 30.
           DISPLAY NOMCTA LINE 20 POSITION 01 SIZE 30.
       F-MAYOR.
      *
       I-SUBCTA.
           DISPLAY
           "F1-AYUDA PUC    CUIDADO!!!!  No oprima ESC si no ha salvado"
                   LINE 24 POSITION 1 REVERSE BLINK LOW.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY TAB-SUBCTA(J)       LINE I POSITION 09 REVERSE LOW
           ACCEPT TAB-SUBCTA(J)        LINE I POSITION 09 REVERSE LOW
                                       UPDATE PROMPT
                                       NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  PGDN AND J > 1
               COMPUTE K = J - 1
               MOVE TAB-SUBCTA(K) TO TAB-SUBCTA(J).
           DISPLAY TAB-SUBCTA(J) LINE I POSITION 09 REVERSE LOW.
           IF  CUP
               MOVE   1                TO  ATRAS
               GO F-SUBCTA.
           IF   F1  GO F-SUBCTA.
           IF   F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE TAB-SUBCTA(J) TO SUBMAE
           MOVE ZEROS         TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               DISPLAY "SUBCTA NO EXISTE      "
               LINE 24 POSITION 01 REVERSE
               GO I-SUBCTA.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           DISPLAY NOMCTA LINE 21 POSITION 01 SIZE 30.
       F-SUBCTA.
      *
       I-AUXCTA.
           DISPLAY
           "F1-AYUDA PUC    CUIDADO!!!!  No oprima ESC si no ha salvado"
                   LINE 24 POSITION 1 REVERSE BLINK LOW.
           MOVE ZEROS            TO  WK-EXCEPTION
           DISPLAY TAB-AUXCTA(J) LINE I POSITION 12 REVERSE LOW
           ACCEPT TAB-AUXCTA(J)  LINE I POSITION 12 REVERSE LOW
                                 UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  PGDN AND J > 1
               COMPUTE K = J - 1
               MOVE TAB-AUXCTA(K) TO TAB-AUXCTA(J).
           DISPLAY TAB-AUXCTA(J) LINE I POSITION 12 REVERSE LOW.
           IF  F1  GO F-AUXCTA.
           IF  CUP
               MOVE   1                TO  ATRAS
               GO F-AUXCTA.
           IF  CUP    GO  I-AUXCTA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE TAB-SUBCTA(J) TO SUBMAE
           MOVE TAB-AUXCTA(J) TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               DISPLAY "AUXILIAR NO EXISTE"
               LINE 24 POSITION 01 REVERSE
               GO I-AUXCTA.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           DISPLAY NOMCTA LINE 22 POSITION 01 SIZE 30.
           IF ESTACTA = "S"
               DISPLAY "CTA. NO permite movim."
               LINE 24 POSITION 01 REVERSE
               GO I-AUXCTA.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-AUXCTA.
      *
       I-DISPLAY-CTAS.
           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE ZEROS TO  SUBMAE AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE SPACES TO NOMCTA.
           DISPLAY NOMCTA LINE 20 POSITION 01 SIZE 30
           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE TAB-SUBCTA(J) TO SUBMAE
           MOVE ZEROS         TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE SPACES TO NOMCTA.
           DISPLAY NOMCTA LINE 21 POSITION 01 SIZE 30

           MOVE TAB-MAYCTA(J) TO MAYMAE.
           MOVE TAB-SUBCTA(J) TO SUBMAE
           MOVE TAB-AUXCTA(J) TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE SPACES TO NOMCTA.
           DISPLAY NOMCTA LINE 22 POSITION 01 SIZE 30.
       F-DISPLAY-CTAS.
      *
       I-CONCEPTO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY TAB-CONCEPTO(J)     LINE 17 POSITION 14 REVERSE LOW
           ACCEPT  TAB-CONCEPTO(J)     LINE 17 POSITION 14 REVERSE LOW
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE TAB-CONCEPTO(J) TO WK-CONCEPTO.
           IF  PGDN AND J > 1
               COMPUTE K = J - 1
               MOVE TAB-CONCEPTO(K) TO TAB-CONCEPTO(J)
      *     DISPLAY TAB-CONCEPTO(J)     LINE I  POSITION 12.
           DISPLAY TAB-CONCEPTO(J)     LINE 17 POSITION 14 REVERSE LOW.
           DISPLAY TAB-CONCEPTO(J)     LINE I  POSITION 17
                                       SIZE 12 REVERSE LOW.
           IF  CUP OR F1  GO F-CONCEPTO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  TAB-CONCEPTO(J) = SPACE
               DISPLAY "DESCRIPCION - ERRADA -"
               LINE 24 POSITION 01 REVERSE
               GO I-CONCEPTO.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-CONCEPTO.
      *
       I-FACTUR.
           IF INDDETAL = "N"
              MOVE ZEROS TO TAB-NUMFRA(J)
              MOVE TAB-NUMFRA(J) TO WI-Z6
              DISPLAY WI-Z6 LINE I POSITION 30 REVERSE LOW
              GO F-FACTUR.
           IF W-OPCION = "A"  AND ALTTIP-DOCTRA = 4
              MOVE ALTFRA-DOCTRA TO TAB-NUMFRA(J).
           MOVE ZEROS  TO  WK-EXCEPTION
           MOVE TAB-NUMFRA(J) TO WI-Z6
           DISPLAY WI-Z6 LINE I POSITION 30 REVERSE LOW
           ACCEPT TAB-NUMFRA(J)     LINE I POSITION 30
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE TAB-NUMFRA(J) TO WI-Z6
           DISPLAY WI-Z6 LINE I POSITION 30  REVERSE LOW
           IF  CUP OR F1  GO F-FACTUR.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  TAB-NUMFRA(J) = ZEROS
               DISPLAY " FACTURA ERRADA       "
               LINE 24 POSITION 01 REVERSE
               GO I-FACTUR.
         I-CONTFAC.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           IF FACTM-COMP = "S"
              PERFORM I-CAPT-FECVEN THRU F-CAPT-FECVEN.
           IF CUP  GO I-FACTUR.
       F-FACTUR.
      *
       I-CAPT-FECVEN.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY  " " LINE   24  POSITION   80
                      CONTROL SG-CONTROL-STRING.
           PERFORM I-WIN-FECVEN THRU F-WIN-FECVEN.
         I-ACEFECVEN.
           PERFORM I-FECHA-HASTA THRU F-FECHA-HASTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY " "  LINE 24 POSITION 80
                         CONTROL SG-CONTROL-STRING.
           DISPLAY SPACES LINE 19 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 20 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 21 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 22 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 23 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
       F-CAPT-FECVEN.
      *
       I-WIN-FECVEN.
           DISPLAY "…ÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                       LINE 19 POSITION 32 CONTROL SG-CONTROL-STRING
           DISPLAY "∫ FACTURA ≥ FECHA DE VENCIMIENTO ∫"
                                          LINE 20 POSITION 32 REVERSE
           DISPLAY "∫         ≥                      ∫"
                                                 LINE 21 POSITION 32
           DISPLAY "»ÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                                 LINE 22 POSITION 32.
           MOVE TAB-NUMFRA(J) TO WI-Z6
           DISPLAY WI-Z6 LINE 21 POSITION 34 REVERSE LOW.
       F-WIN-FECVEN.
      *
       I-SUMDOC.
           MOVE ZEROS              TO WK-CREDITOS WK-DEBITOS.
           PERFORM  I-START-110-CNDOCTOS  THRU F-START-110-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-SUMDOC.
         I-LEEDOC.
           READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                PERFORM  I-SALDOS-T THRU F-SALDOS-T
                GO F-SUMDOC.
           IF RADICTIP-DOCTOS > ALTTIP-DOCTRA
              PERFORM I-SALDOS-T THRU F-SALDOS-T
              GO F-SUMDOC.
           IF RADICDOC-DOCTOS >  RADICDOC-DOCTRA
                PERFORM  I-SALDOS-T THRU F-SALDOS-T
                GO F-SUMDOC.
           IF VALOR-DOCTOS < 0
              ADD VALOR-DOCTOS TO WK-CREDITOS
           ELSE
              ADD VALOR-DOCTOS TO WK-DEBITOS.
           GO I-LEEDOC.
       F-SUMDOC.
      *
       I-SALDOS-T.
           MOVE ZEROS TO SW-VALORES.
           COMPUTE WK-DIFSALT = WK-CREDITOS - WK-DEBITOS.
           IF WK-CREDITOS < ZEROS OR
              WK-DEBITOS  > ZEROS MOVE 1 TO SW-VALORES.
           IF WK-DIFSALT = ZEROS GO F-SALDOS-T.
           MOVE ZEROS TO SW-VALORES.
       F-SALDOS-T.
      *
       I-DISUM.
           MOVE WK-CREDITOS TO WI-Z9P.
           DISPLAY WI-Z9P  LINE 20 POSITION 62 REVERSE LOW.
           MOVE WK-DEBITOS  TO WI-Z9P.
           DISPLAY WI-Z9P  LINE 19 POSITION 62 REVERSE LOW.
       F-DISUM.
      *
       I-VRDOC.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE TAB-VLRDOC(J)     TO  WI-Z11P.
           DISPLAY WI-Z11P            LINE I POSITION 54 REVERSE LOW
           ACCEPT TAB-VLRDOC(J)       LINE I POSITION 54
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE TAB-VLRDOC(J)  TO  WI-Z11P.
           DISPLAY WI-Z11P  LINE I POSITION 54  REVERSE LOW
           IF  CUP OR F1  GO F-VRDOC.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  TAB-VLRDOC(J) = ZEROS
               DISPLAY "VALOR NO DEBER SER 0  "
               LINE 24 POSITION 01 REVERSE
               GO I-VRDOC.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRDOC.
      *
       I-VRBASE.
           IF INDBASE = "N"
              MOVE ZEROS TO TAB-VLRBAS(J)
              MOVE TAB-VLRBAS(J) TO WI-Z9
              DISPLAY WI-Z9 LINE I POSITION 41 REVERSE LOW
              GO F-VRBASE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE TAB-VLRBAS(J)          TO  WI-Z9.
           DISPLAY WI-Z9             LINE I POSITION 41 REVERSE LOW
           ACCEPT TAB-VLRBAS(J)      LINE I POSITION 41
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE TAB-VLRBAS(J)          TO  WI-Z9.
           DISPLAY WI-Z9               LINE I POSITION 41 REVERSE LOW
           IF  CUP GO F-VRBASE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  TAB-VLRBAS(J) =  ZEROS
               DISPLAY "BASE   NO debe ser  0  "
               LINE 24 POSITION 01 REVERSE
               GO I-VRBASE.
      *
           IF  TAB-MAYCTA(J) = 2367  GO F-VRBASE.
           IF  TAB-MAYCTA(J) = 2368  GO F-VRBASE.
      *
           IF  TAB-MAYCTA(J) = 1355  AND TAB-SUBCTA(J) = 50
               COMPUTE WK-VLRDOC       ROUNDED
                                       =   TAB-VLRBAS(J)
                                       *   PORC-CTA
                                       /   1000
           COMPUTE W-TOTAL-MESES ROUNDED =
                   WK-VLRDOC            / 1000
           COMPUTE WK-VLRDOC            = W-TOTAL-MESES * 1000
           ELSE
               COMPUTE WK-VLRDOC
                                       =   TAB-VLRBAS(J)
                                       *   PORC-CTA
                                       /   100.
           MOVE WK-VLRDOC         TO  TAB-VLRDOC(J).
           PERFORM I-EVAL-SIGNO THRU  F-EVAL-SIGNO
           MOVE TAB-VLRDOC(J)     TO  WI-Z12P.
           DISPLAY WI-Z11P      LINE  I POSITION 54 REVERSE LOW.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRBASE.
      *
       I-SIGNO.
           DISPLAY
           "CUIDADO!!!!  NO OLVIDE GRABAR CADA 5 REGISTROS"
                   LINE 24 POSITION 1 REVERSE BLINK LOW.

           MOVE ZEROS             TO   WK-EXCEPTION
           DISPLAY TAB-SIGNO(J) LINE I POSITION 76 REVERSE LOW.
           ACCEPT  TAB-SIGNO(J) LINE I POSITION 76
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY TAB-SIGNO(J) LINE I POSITION 76 REVERSE LOW.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 26.
           IF  CUP OR F1  GO F-SIGNO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF   TAB-SIGNO(J) = 1 OR TAB-SIGNO(J) = 2 NEXT SENTENCE
           ELSE GO I-SIGNO.
           PERFORM I-EVAL-SIGNO THRU F-EVAL-SIGNO.
           MOVE TAB-VLRDOC(J)     TO WI-Z12P.
           DISPLAY WI-Z12P      LINE I POSITION 54 REVERSE LOW.
       F-SIGNO.
      *
       I-EVAL-SIGNO.
           IF TAB-SIGNO(J) = 2
              IF TAB-VLRDOC(J) < 0
                 NEXT SENTENCE
              ELSE
                 COMPUTE TAB-VLRDOC(J) = TAB-VLRDOC(J) * -1.
           IF TAB-SIGNO(J) = 1
              IF TAB-VLRDOC(J) > 0
                 NEXT SENTENCE
              ELSE
                 COMPUTE TAB-VLRDOC(J) = TAB-VLRDOC(J) * -1.
       F-EVAL-SIGNO.
      *
       I-COSTO.
           MOVE ZEROS                 TO  WK-EXCEPTION
           DISPLAY "COSTO No.: "  LINE 22 POSITION 1.
           DISPLAY WK-COSTO       LINE 22 POSITION 11
           ACCEPT  WK-COSTO       LINE 22 POSITION 11
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY WK-COSTO       LINE 22 POSITION 11.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-COSTO.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       LECTURAS Y MANIPULACION DE ARCHIVOS               ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-LEE-CNDOCTOS.
            MOVE  ZEROS             TO  SW-1.
           PERFORM  I-START-110-CNDOCTOS  THRU F-START-110-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-LEE-CNDOCTOS.
           MOVE  ALTTIP-DOCTRA     TO  RADICTIP-DOCTOS.
           MOVE  RADICDOC-DOCTRA   TO  RADICDOC-DOCTOS.
           MOVE  ZEROS             TO  RADIC-AUTOMATICA-DOCTOS.
           READ  CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                      GO F-LEE-CNDOCTOS.
           IF    RADICTIP-DOCTOS > ALTTIP-DOCTRA   GO F-LEE-CNDOCTOS.
           IF    RADICDOC-DOCTOS > RADICDOC-DOCTRA GO F-LEE-CNDOCTOS.
           MOVE  1 TO SW-1.
       F-LEE-CNDOCTOS.
      *
       I-LEE-CUENTAS.
           MOVE MAYCTA-DOCTOS TO MAYMAE.
           MOVE ZEROS TO SUBMAE AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE "N.N" TO NOMCTA.
           DISPLAY NOMCTA LINE 13 POSITION 35 REVERSE.
           MOVE MAYCTA-DOCTOS TO MAYMAE.
           MOVE SUBCTA-DOCTOS TO SUBMAE.
           MOVE ZEROS         TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE "N.N" TO NOMCTA.
           DISPLAY NOMCTA LINE 14 POSITION 35 REVERSE.
           MOVE MAYCTA-DOCTOS TO MAYMAE.
           MOVE SUBCTA-DOCTOS TO SUBMAE.
           MOVE AUXCTA-DOCTOS TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID
                         KEY MOVE "N.N" TO NOMCTA.
           DISPLAY NOMCTA LINE 15 POSITION 35 REVERSE.
       F-LEE-CUENTAS.
      *
       I-LEE-EMPRESA.
            MOVE  0   TO SW-1.
            READ EMPRESAS WITH NO LOCK INVALID KEY
                   MOVE 1  TO SW-1.
           DISPLAY WIN-CODIGO-EMPRESA LINE 05  POSITION 16 REVERSE LOW.
           DISPLAY WIN-EMPRESA LINE 05 POSITION 28 REVERSE LOW SIZE 20.
       F-LEE-EMPRESA.
      *
       I-LEA-CNNITCED.
           MOVE ZEROS TO SW-EOF.
           READ CNNITCED WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
       F-LEA-CNNITCED.
      *
       I-LEE-BANCOS.
           MOVE ZEROS TO SW-EOF.
           READ NOMIBANK WITH NO LOCK INVALID KEY
                MOVE   1    TO   SW-EOF.
       F-LEE-BANCOS.
      *
       I-LEER-CNDOCTOS.
            MOVE 0 TO SW-OK.
           PERFORM  I-START-110-CNDOCTOS  THRU F-START-110-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-LEER-CNDOCTOS.
        I-LEER.
           READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                MOVE 1 TO SW-OK GO F-LEER-CNDOCTOS.
           IF   RADICDOC-DOCTOS      = WK-RADICDOC
                AND RADICTIP-DOCTOS  = ALTTIP-DOCTRA
                GO I-LEER.
           MOVE RADICTIP-DOCTOS  TO ALTTIP-DOCTRA.
           MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTRA WK-RADICDOC.
       F-LEER-CNDOCTOS.
      *
       I-LEE1-CNDOCTOS.
            MOVE 0 TO SW-OK.
           PERFORM  I-START-000-CNDOCTOS  THRU F-START-000-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-LEE1-CNDOCTOS.
           READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                MOVE 1 TO SW-OK GO F-LEE1-CNDOCTOS.
           MOVE RADICTIP-DOCTOS  TO ALTTIP-DOCTRA.
           MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTRA WK-RADICDOC.
           MOVE  1    TO   SW-1.
       F-LEE1-CNDOCTOS.
      *
       I-LEE2-CNDOCTOS.
            MOVE 0 TO SW-OK.
           PERFORM  I-START-100-CNDOCTOS  THRU F-START-100-CNDOCTOS.
           IF FIN-START-CNDOCTOS = 1        GO F-LEE2-CNDOCTOS.
       LEE2-2.
           READ CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                MOVE 1 TO SW-OK GO F-LEE2-CNDOCTOS.
           MOVE RADICTIP-DOCTOS  TO ALTTIP-DOCTRA.
           MOVE RADICDOC-DOCTOS  TO RADICDOC-DOCTRA WK-RADICDOC.
           MOVE  1    TO   SW-1.
       F-LEE2-CNDOCTOS.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       VALIDACION DE LA INFORMACION                      ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 24 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 24 POSITION 01.
       F-NEXISTE.
      *
       I-MASC-NAC.
           MOVE ZEROS       TO SW-ERROR.
           MOVE WK-DIA      TO WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  13
           OR  WK-DIA                       <  01
           OR  WK-DIA                       >  31
               MOVE SPACES                 TO  WK-MASCARA-DIA
               MOVE SPACES                 TO  WK-MASCARA-MES
               MOVE SPACES                 TO  WK-MASCARA-ANO
               MOVE 1 TO SW-ERROR
               GO F-MASC-NAC
           ELSE
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
           MOVE WK-ANO                 TO  WK-MASCARA-ANO
           MOVE "-"                    TO  WK-RAYA-1
           MOVE "-"                    TO  WK-RAYA-2
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-HASTA
                                        /  4
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-BISIESTO
                                        *  4
           IF  WK-ANO-BISIESTO          =  WK-ANO-HASTA
               MOVE 29                 TO  WK-DIAS-MES (2)
           ELSE
               MOVE 28                 TO  WK-DIAS-MES  (2).
           ACCEPT WK-FECHA-HOY       FROM  DATE
           IF WK-ANO           >= 0
              ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.

           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-DIA                 TO  WK-MASCARA-DIA
           MOVE WK-HORA-HOY            TO  WK-MASCARA-HORA
           MOVE WK-HORAS               TO  WK-MASCARA-HORAS
           MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
           MOVE ":"                    TO  WK-PUNTOS
           MOVE WK-FECHA-HOY           TO  WK-FECHA-ACTUAL.

       F-MASC-NAC.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥               START   DE   LOS   ARCHIVOS               ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-START-000-CNDOCTOS.
            MOVE  ZEROS             TO  FIN-START-CNDOCTOS.
            MOVE  ZEROS             TO  RADICTIP-DOCTOS.
            MOVE  ZEROS             TO  RADICDOC-DOCTOS.
            MOVE  ZEROS             TO  RADIC-AUTOMATICA-DOCTOS.
            START CNDOCTOS KEY NOT LESS KEY00-DOCTOS INVALID  KEY
                DISPLAY "STAR ERROR       " LINE 24 POSITION 01 REVERSE
                MOVE  1 TO FIN-START-CNDOCTOS.
       F-START-000-CNDOCTOS.
      *
       I-START-100-CNDOCTOS.
            MOVE  ZEROS             TO  FIN-START-CNDOCTOS.
            MOVE  ALTTIP-DOCTRA     TO  RADICTIP-DOCTOS.
            MOVE  ZEROS             TO  RADICDOC-DOCTOS.
            MOVE  ZEROS             TO  RADIC-AUTOMATICA-DOCTOS.
            START CNDOCTOS KEY NOT LESS KEY00-DOCTOS INVALID  KEY
                DISPLAY "STAR ERROR       " LINE 24 POSITION 01 REVERSE
                MOVE  1 TO FIN-START-CNDOCTOS.
       F-START-100-CNDOCTOS.
      *
       I-START-110-CNDOCTOS.
            MOVE  ZEROS             TO  FIN-START-CNDOCTOS.
            MOVE  ALTTIP-DOCTRA     TO  RADICTIP-DOCTOS.
            MOVE  RADICDOC-DOCTRA   TO  RADICDOC-DOCTOS.
            MOVE  ZEROS             TO  RADIC-AUTOMATICA-DOCTOS.
            START CNDOCTOS KEY NOT LESS KEY00-DOCTOS INVALID  KEY
                DISPLAY "STAR ERROR       " LINE 24 POSITION 01 REVERSE
                MOVE  1 TO FIN-START-CNDOCTOS.
       F-START-110-CNDOCTOS.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINA MANIPULACION DE LAS TABLAS                 ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-BORRA-TEMP.
                        MOVE ZEROS  TO  RADICDOC-DOCTOS
                                        ALTNIT-DOCTRA
                                        WK-NIT1
                                        NITAUX-DOCTRA
                                        RADICDOC-DOCTRA
                                        ALTCTA1-DOCTra
                                        ALTEMP-DOCTRA
                                        ALTPER-DOCTRA
                                        ALTCTA-DOCTRA
                                        ALTMES-DOCTRA
                                        ALT07-DOCTRA
                                        ALT09-DOCTRA
                                        CONCEPTO-DOCTRA
                                        VALOR-DOCTRA
                                        VRBASE-DOCTRA
                                        CHEQUE-BENEF-DOCTRA
                                        BANCO-BENEF-DOCTRA
                                        BANCO-DOCTRA
                                        CONSIGNACION-DOCTRA
                                        SIGNO-DOCTRA
                                        CONDICION-DOCTRA
                                        CHEQ-HASTA-DOCTRA
                                        NUM-SOPORTE-DOCTRA.
                        MOVE SPACES TO  CONCEPTO-DOCTOS.
       F-BORRA-TEMP.
      *
       I-BORRA-TABLA.
              MOVE ZEROS  TO K.
         I-LOOP-BT.
              ADD  1  TO  K.
              IF   K   >  300 GO F-BORRA-TABLA.
              MOVE ZEROS  TO TAB-MAYCTA(K).
              MOVE ZEROS  TO TAB-SUBCTA(K).
              MOVE ZEROS  TO TAB-AUXCTA(K).
              MOVE ZEROS  TO TAB-NUMFRA(K).
              MOVE ZEROS  TO NITAUX-DOCTRA
              MOVE ZEROS  TO ALTNIT-DOCTRA
              MOVE ZEROS  TO TAB-NIT(K).
              MOVE ZEROS  TO TAB-FECVEN(K).
              MOVE SPACES TO TAB-CONCEPTO(K).
              MOVE ZEROS  TO TAB-VLRDOC(K).
              MOVE ZEROS  TO TAB-VLRBAS(K).
              MOVE ZEROS  TO TAB-SIGNO(K).
              MOVE SPACES TO TAB-TIPREG(K).
              MOVE ZEROS  TO TAB-CONDICION(K).
              GO   I-LOOP-BT.
       F-BORRA-TABLA.
      *
       I-BORRA-REG.
              MOVE ZEROS  TO TAB-NUMFRA(J).
              MOVE ZEROS  TO TAB-NIT(J).
              MOVE ZEROS  TO TAB-FECVEN(J).
              MOVE SPACES TO TAB-CONCEPTO(J).
              MOVE ZEROS  TO TAB-VLRDOC(J).
              MOVE ZEROS  TO TAB-VLRBAS(J).
              MOVE ZEROS  TO TAB-SIGNO(J).
              MOVE SPACES TO TAB-TIPREG(J).
              MOVE ZEROS  TO TAB-CONDICION(J).
       F-BORRA-REG.
      *
       I-CARGA-TABLA.
           MOVE  ALTTIP-DOCTRA   TO  RADICTIP-DOCTOS.
           MOVE  RADICDOC-DOCTRA TO  RADICDOC-DOCTOS.
           MOVE  ZEROS           TO  RADIC-AUTOMATICA-DOCTOS.
           START CNDOCTOS KEY NOT LESS KEY00-DOCTOS
                 INVALID  KEY GO  F-CARGA-TABLA.
           MOVE ZEROS  TO K.
         I-LOOP-CT.
           READ  CNDOCTOS NEXT RECORD WITH NO LOCK AT END
                                      GO F-CARGA-TABLA.
           IF    RADICTIP-DOCTOS > ALTTIP-DOCTRA   GO F-CARGA-TABLA.
           IF    RADICDOC-DOCTOS > RADICDOC-DOCTRA GO F-CARGA-TABLA.
           IF    RADICTIP-DOCTOS = ALTTIP-DOCTRA
                 GO LOOP-C1
              ELSE
                  GO F-CARGA-TABLA.
        LOOP-C1.
           IF    RADICDOC-DOCTOS = RADICDOC-DOCTRA
                   GO  LOOP-C2
                ELSE
                   GO F-CARGA-TABLA.
        LOOP-C2.
           ADD  1  TO  K.
           MOVE ALTFRA-DOCTOS        TO ALTFRA-DOCTRA
           MOVE NUM-SOPORTE-DOCTOS   TO NUM-SOPORTE-DOCTRA
           MOVE CHEQ-HASTA-DOCTOS    TO CHEQ-HASTA-DOCTRA
           MOVE FECDOC-DOCTOS        TO FECDOC-DOCTRA.
           MOVE COSTO12-DOCTOS       TO COSTO12-DOCTRA
           MOVE CIUDAD12-DOCTOS      TO CIUDAD12-DOCTRA.
           MOVE ALT07-DOCTOS         TO ALT07-DOCTRA.
           MOVE FECHA-CONTAB-DOCTOS  TO FECHA-CONTAB-DOCTRA.
           MOVE ALTMES-DOCTOS        TO W-LONG.
           MOVE ALTEMP-DOCTOS        TO ALTEMP-DOCTRA
           MOVE ALTPER-DOCTOS        TO ALTPER-DOCTRA.
           MOVE BANCO-DOCTOS         TO BANCO-DOCTRA.
           MOVE BANCO-BENEF-DOCTOS   TO BANCO-BENEF-DOCTRA.
           MOVE CHEQUE-BENEF-DOCTOS  TO CHEQUE-BENEF-DOCTRA.
           MOVE CONSIGNACION-DOCTOS  TO CONSIGNACION-DOCTRA.
           IF   ALTNIT-DOCTOS > ZEROS
                MOVE ALTNIT-DOCTOS   TO ALTNIT-DOCTRA.
           IF   NITAUX-DOCTOS > ZEROS
                MOVE NITAUX-DOCTOS   TO NITAUX-DOCTRA
           IF   NITAUX-DOCTRA = ZEROS AND
                ALTNIT-DOCTOS > ZEROS
                MOVE ALTNIT-DOCTOS   TO NITAUX-DOCTRA.

           MOVE SW-CONS-DOCTOS       TO SW-CONS-DOCTRA
           MOVE FLAG-DOCTOS          TO FLAG-DOCTRA
           MOVE CIUDAD-DOCTOS        TO CIUDAD-DOCTRA
           MOVE COMP-ANTER-DOCTOS    TO COMP-ANTER-DOCTRA
           IF   K   >  300 GO F-CARGA-TABLA.
           MOVE    MAYCTA-DOCTOS     TO TAB-MAYCTA(K)
           MOVE    SUBCTA-DOCTOS     TO TAB-SUBCTA(K)
           MOVE    AUXCTA-DOCTOS     TO TAB-AUXCTA(K)
           MOVE    NUMFRA-DOCTOS     TO TAB-NUMFRA(K)
           MOVE    ALTNIT-DOCTOS     TO TAB-NIT(K)
           MOVE    CONCEPTO-DOCTOS   TO TAB-CONCEPTO(K)
           MOVE    VALOR-DOCTOS      TO TAB-VLRDOC(K)
           MOVE    VRBASE-DOCTOS     TO TAB-VLRBAS(K)
           MOVE    SIGNO-DOCTOS      TO TAB-SIGNO(K)
           MOVE    CONDICION-DOCTOS  TO TAB-CONDICION(K).
           GO      I-LOOP-CT.
       F-CARGA-TABLA.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINAS DE IMPRESION Y PANTALLAS                  ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-WINDOW-CAB.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY SPACE ERASE LINE 01 POSITION 01
                         CONTROL SG-CONTROL-STRING.
           MOVE "NO" TO SG-BLINK
           MOVE "NO" TO SG-BEEP.
           MOVE CON-COD-EMPRESA TO WIN-CODIGO-EMPRESA
           READ EMPRESAS WITH NO LOCK INVALID KEY
               DISPLAY " EMPRESA NO EXISTE "
               LINE 06 POSITION 39 REVERSE
               GO F-WINDOW-CAB.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY SPACE LINE 01 POSITION 01
                         CONTROL SG-CONTROL-STRING.
           DISPLAY
           "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª" LINE 01 POSITION 01
           "∫ No.Comprobte:                                ∫  DATOS CONC
      -    "ILIACION BANCARIA  ∫" LINE 02 POSITION 01
           "∫ No.Documento:            Soporte:            ∫A consignar:
      -    "                   ∫" LINE 03 POSITION 01
           "∫ Fecha Desde.:            F-Hasta:            ∫No. Cheque :
      -    "                   ∫" LINE 04 POSITION 01
           "∫ Cod-Empresa.:                                ∫Bco Girador:
      -    "                   ∫" LINE 05 POSITION 01
           "∫ Nit-Empresa.:                                ∫Cheque Desde
      -    "    Cheq Hasta:    ∫" LINE 06 POSITION 01
           "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕ ÕÕÕÕÀÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕπ" LINE 07 POSITION 01.
       I-WIN-CTAS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " "  LINE 24 POSITION 80
                         CONTROL SG-CONTROL-STRING.
           DISPLAY
           "∫P.U.C Reg#:   ∫ Descripcion∫ Factura ∫ Base-Grvble ∫   Valor
      -    "res         ∫ Signo∫"
                          LINE 08 POSITION 01 LOW REVERSE.
           DISPLAY
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫"
             LINE 09 POSITION 01   CONTROL SG-CONTROL-STRING.
           DISPLAY
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 10 POSITION 01
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 11 POSITION 01
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 12 POSITION 01

           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 13 POSITION 01
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 14 POSITION 01
           "∫              ∫            ∫         ∫             ∫
      -    "            ∫      ∫" LINE 15 POSITION 01
           "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕ
      -    "ÀÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕπ" LINE 16 POSITION 01
           "∫ CONCEPTO:
      -    "∫ CONDICION :      ∫" LINE 17 POSITION 01
           "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    " ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº" LINE 18 POSITION 01.
           DISPLAY "DEBITOS:"  LINE 19 POSITION 54
           DISPLAY "CREDITO:"  LINE 20 POSITION 54
           DISPLAY "SALDOS :"  LINE 21 POSITION 54.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY SPACE LINE 25 POSITION 01  SIZE 80
                         CONTROL SG-CONTROL-STRING.
           DISPLAY " ESC:     "       LINE 25  POSITION 01
           DISPLAY " F2:      "       LINE 25  POSITION 11
           DISPLAY "INICIO      "    LINE 25  POSITION 21
           DISPLAY " F9:       "      LINE 25  POSITION 33
           DISPLAY "AVPAG:      "     LINE 25  POSITION 44
           DISPLAY " F7:          "   LINE 25  POSITION 57.
           DISPLAY " F8:      "       LINE 25  POSITION 71.

           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY SPACE LINE 24 POSITION 80
                         CONTROL SG-CONTROL-STRING.
           DISPLAY "MENU"            LINE 25  POSITION 06
           DISPLAY "SALIR"           LINE 25  POSITION 15
           DISPLAY "GRABAR"          LINE 25  POSITION 27
           DISPLAY "BORRAR"          LINE 25  POSITION 37
           DISPLAY "COPIAR"          LINE 25  POSITION 49
           DISPLAY "COMP/COMPL"      LINE 25  POSITION 61.
           DISPLAY "OTROS"           LINE 25  POSITION 75.


           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY " "  LINE 24 POSITION 80
                         CONTROL SG-CONTROL-STRING.
           DISPLAY WIN-EMPRESA LINE 01 POSITION 22 SIZE 40 REVERSE LOW
                           CONTROL SG-CONTROL-STRING.
           IF W-OPCION = "A"
              MOVE W-LONG  TO MM-COMP-DOCTRA
              MOVE CON-ANO TO AA-COMP-DOCTRA
              MOVE W-LONG  TO MM-VCTO-DOCTRA
              MOVE CON-ANO TO AA-VCTO-DOCTRA.
       F-WIN-CTAS.
       F-WINDOW-CAB.
      *
       I-PANTALLA-1B.
           MOVE "   **CAPTURA MOVIMIENTO CONTABLE** "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 10 POSITION 20
               DISPLAY "∫  TECLEE EL MES CONTABLE   :                ∫"
                                               LINE 11 POSITION 20
               DISPLAY "∫                                            ∫"
                                               LINE 12 POSITION 20
               DISPLAY "∫   [F2] TERMINAR                            ∫"
                                               LINE 13 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-DISPLAY-ERROR.
            MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "MES EN PROCESO" LINE 05 POSITION 25 REVERSE.
            DISPLAY WK-NOMBRE-MES(W-LONG) LINE 05 POSITION 39 REVERSE.
            DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "∫              A D V E R T E N C I A!            ∫"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "∫ ATENCION... No se puede modificar el movimiento∫"
            LINE 09 POSITION 17 BEEP REVERSE
            DISPLAY "∫ de este mes porque YA se encuentra impreso  en ∫"
            LINE 10 POSITION 17 BEEP REVERSE
            DISPLAY "∫ los LIBROS OFICIALES, con sus respectivos      ∫"
            LINE 11 POSITION 17 BEEP REVERSE
            DISPLAY "∫            FOLIOS.                             ∫"
            LINE 12 POSITION 17 BEEP REVERSE
            DISPLAY "∫ Si desea mas informacion remitase al DPTO. DE  ∫"
            LINE 13 POSITION 17 BEEP REVERSE
            DISPLAY "∫ SISTEMAS           GRACIAS!                    ∫"
            LINE 14 POSITION 17 BEEP REVERSE
            DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
            LINE 15 POSITION 17 BEEP REVERSE.
            PERFORM ACEPTA-TRUCO.
       F-DISPLAY-ERROR. EXIT.
      *
       I-DISPLAY.
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕª"
                                              LINE 19 POSITION 15
               DISPLAY "∫ TIPO COMPROBANTE :       No.CUENTA   :
      -       "      ∫"
                                              LINE 20 POSITION 15
               DISPLAY "∫ NUMERO DE MES    :       NR. FACTURA :
      -       "      ∫"
                                              LINE 21 POSITION 15
               DISPLAY "∫ SECUENCIA REGISTR:
      -       "      ∫"
                                              LINE 22 POSITION 15
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕº"
                                              LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
       F-DISPLAY.
      *
       I-STATUS-CNDOCTOS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNDOCTOS
               MOVE 1                      TO  SW-ABRE
               IF  CNDOCTOS-STATUS              =  "35"
                   OPEN OUTPUT CNDOCTOS
               ELSE
                   IF  CNDOCTOS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNDOCTOS
                       PERFORM I-FIN-MODE            THRU F-FIN-MODE.
               CLOSE CNDOCTOS.
       F-STATUS-CNDOCTOS.
      *
       I-STATUS-BORRADOS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT BORRADOS
               MOVE 1                      TO  SW-ABRE
               IF  BORRADOS-STATUS              =  "35"
                   OPEN OUTPUT BORRADOS
               ELSE
                   IF  BORRADOS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-BORRADOS
                       PERFORM I-FIN-MODE            THRU F-FIN-MODE.
               CLOSE BORRADOS.
       F-STATUS-BORRADOS.
      *
      ******************************************************************
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\MASCFECH.PRO".

           COPY "..\PRO\REVERSAC.PRO".
           COPY "..\PRO\NIVELES.PRO".
           COPY "..\PRO\BUSCACON.PRO".
           COPY "..\PRO\CNEGRES3.PRO".
           COPY "..\PRO\CNFAC729.PRO".
           COPY "..\PRO\CNNOT729.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\HELPNIT.PRO".
           COPY "..\PRO\HELPPUC.PRO".
           COPY "..\PRO\HELPNIT1.PRO".
           COPY "..\PRO\HELPEMPR.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\CNCONTRO.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\AUDIT729.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\AYUCOMPB.PRO".
      *    COPY "..\PRO\HELPCOMP.PRO".
           COPY "..\PRO\HELBANKO.PRO".

           COPY "..\PLB\CNDTACUM.PLB".
           COPY "..\STA\CNDTACUM.STA".
           COPY "..\PLB\CNEGRESO.PLB".
           COPY "..\PLB\CNDNOTAS.PLB".
           COPY "..\PLB\CNFACTUR.PLB".
           COPY "..\PLB\AUDIT729.PLB".
           COPY "..\PLB\BORRADOS.PLB".
           COPY "..\PLB\CNDOCTOS.PLB".
      ****************************************************************

