       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG333.
      *----------------------------------------------------------------*
      * ARCHIVO DIARIO CON FECHA DEL SISTEMA PARA PROCESO DE           *
      * ARCHIVO PLANO A DAVIVIENDA, POR CUENTA, POR VEZ ABR-27-2002    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\CONTRATO.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\DYC\TEMPINFO.DYC".
           COPY "..\DYC\NOMICORP.DYC".
           COPY "..\DYC\NOMICIUD.DYC".
           COPY "..\DYC\VMNOMINA.DYC".
           COPY "..\DYC\TBARCDAV.DYC".
           COPY "..\SEL\REPORTES.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\HOJAVIDA.FD".
           COPY "..\FD\CONTRATO.FD".
           COPY "..\FD\INFORMES.FD".
           COPY "..\FD\TEMPINFO.FD".
           COPY "..\FD\NOMICORP.FD".
           COPY "..\FD\NOMICIUD.FD".
           COPY "..\FD\VMNOMINA.FD".
           COPY "..\FD\TBARCDAV.FD".
           COPY "..\FD\REPORTES.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA              PIC X(7)       VALUE "PROG333".
       77  ID-PRG                  PIC X(7)       VALUE "PROG333".
       77  W-COD-CTA               PIC X(4)       VALUE SPACES.
       77  W-CTA-TEMP              PIC 9(12)      VALUE ZEROS.
       77  SW-CUENTA               PIC 9          VALUE ZEROS.
       77  W-VALOR-INCONSISTENCIA  PIC S9(12)V99  VALUE ZEROS.
       77  W-NUMER-INCONS          PIC 9(04)      VALUE ZEROS.
       77  W-VALIDA-CUENTA         PIC X(01)      VALUE SPACES.
      *
       01  W-TEMPORAL.
         03  WNIT-TEMP      PIC X(12)      VALUE ZEROS.
         03  WNIT-EXT       PIC X(01)      VALUE ZEROS.
      *
       03  AREA-CONCEPTO.
         05  C-DATO         PIC X(14)  VALUE  SPACES.
         05  C-FECHA-DESDE  PIC X(11)  VALUE  ZEROS.
         05  FILLER         PIC X(4)   VALUE  " AL ".
         05  C-FECHA-HASTA  PIC X(11)  VALUE  ZEROS.
      *
       01  CODIGO-PANTALLAS.
         03  CODIGO-PANTALLA  PIC 9(4)  OCCURS 25 TIMES.
      *
           COPY "..\LBL\NOMICORP.LBL".
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CONTRATO.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\LBL\HOJAVIDA.LBL".
           COPY "..\LBL\INFORMES.LBL".
           COPY "..\LBL\NOMICIUD.LBL".
           COPY "..\LBL\TEMPINFO.LBL".
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\VMNOMINA.LBL".
           COPY "..\LBL\TBARCDAV.LBL".
           COPY "..\LBL\REPORTES.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\TABLAGEN.WRK".
      *    COPY "..\WRK\CONTRATO.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
           COPY "..\WRK\DAVIVIEN.WRK".
           COPY "..\WRK\INFOR333.WRK".
           COPY "..\WRK\LISTADOS.WRK".
      *
       01  AREA-ARCHIVO.
         03  REG-A-0.
           05  A-TIPO-0           PIC 9(2)      VALUE 02.
           05  A-NOVEDAD-0        PIC X         VALUE SPACES.
           05  A-FECHA-PAGO-0     PIC 9(8).
           05  FILLER             PIC 9(12)     VALUE ZEROS.
           05  A-OFICINA-0        PIC 9(4)      VALUE 0081.
           05  A-RETIROS-0        PIC 9(4)      VALUE 0001.
           05  A-VALOR-RETIROS-0  PIC 9(11)V99.
           05  A-NUMERO-TOTAL     PIC 9(4).
           05  A-VALOR-TOTAL      PIC 9(11)V99.
           05  FILLER             PIC X(21)     VALUE ZEROS.
      *
         03  REG-A-1.
           05  A-TIPO-1            PIC 9(2)       VALUE 34.
           05  A-NOVEDAD-1         PIC X          VALUE SPACES.
           05  A-FECHA-PAGO-1      PIC 9(8).
           05  A-CUENTA-1          PIC X(12).
           05  A-OFICINA-1         PIC 9(4)       VALUE 0081.
           05  A-TALON-1           PIC 9(4).
           05  FILLER              PIC 9(13)      VALUE ZEROS.
           05  A-VALOR-DEPOSITO-1  PIC 9(11)V99.
           05  A-MOTIVO-1          PIC 9(2)       VALUE 06.
           05  A-REFERENCIA-1      PIC 9(4).
           05  A-FECHA-PAGO-1A     PIC 9(8).
           05  A-CEDULA            PIC 9(13).
      *
         03  REG-A-2.
           05  A-TIPO-2            PIC 9(2)       VALUE 55.
           05  A-NOVEDAD-2         PIC X          VALUE SPACES.
           05  A-FECHA-PAGO-2      PIC 9(8).
           05  A-CTA-EMPRESA-2     PIC 9(12).
           05  A-OFICINA-2         PIC 9(4)       VALUE 0081.
           05  A-TALON-2           PIC 9(4).
           05  FILLER              PIC 9(13)      VALUE ZEROS.
           05  A-VALOR-DEPOSITO-2  PIC 9(11)V99.
           05  A-MOTIVO-2          PIC 9(2)       VALUE 06.
           05  A-REFERENCIA-2      PIC 9(4).
           05  A-FECHA-PAGO-2A     PIC 9(8).
           05  A-NIT-EMPRESA-2A    PIC 9(13).
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\NOMICORP.DCL".
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TABLAEMP.DCL".
           COPY "..\DCL\CONCPTOS.DCL".
           COPY "..\DCL\HOJAVIDA.DCL".
           COPY "..\DCL\CONTRATO.DCL".
           COPY "..\DCL\INFORMES.DCL".
           COPY "..\DCL\TEMPINFO.DCL".
           COPY "..\DCL\NOMICIUD.DCL".
           COPY "..\DCL\VMNOMINA.DCL".
           COPY "..\DCL\TBARCDAV.DCL".
           COPY "..\DCL\REPORTES.DCL".
       END DECLARATIVES.
      *
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           IF WIN-PERIODO-ESTADO NOT = 77
            PERFORM I-WIN-PERIODO-ESTADO THRU F-WIN-PERIODO-ESTADO
            EXIT PROGRAM.
           IF WIN-TIPO-PAGO NOT = 3
            PERFORM I-VERI-TIPO-PAGO THRU F-VERI-TIPO-PAGO
            EXIT PROGRAM.
           IF WIN-COD-BANCO NOT = 1
            PERFORM I-VERI-COD-BANCO THRU F-VERI-COD-BANCO
            EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVO THRU F-LABEL-ARCHIVO
           PERFORM I-ABRE-ARCHIVOS THRU  F-ABRE-ARCHIVOS.
           MOVE ZEROS  TO  W-NETO-A-PAGAR A-NUMERO-TOTAL
           MOVE WIN-COD-CIUDAD         TO   CIUD-CODIGO
           PERFORM I-LEE-NOMICIUD    THRU F-LEE-NOMICIUD.
           MOVE CIUD-NOMBRE            TO   I-CIUDAD.
           MOVE CIUD-CODIGO            TO   I-COD-CIUDAD.
           PERFORM I-CARGA-TABLAEMP THRU F-CARGA-TABLAEMP.
           PERFORM I-STATUS-TABLANOV THRU F-STATUS-TABLANOV
           PERFORM I-PANTALLA        THRU F-PANTALLA
           PERFORM I-DATOS-GENERICOS THRU F-DATOS-GENERICOS
           PERFORM I-INICIE-START    THRU F-INICIE-START
           MOVE    0                 TO   FIN-OK
           PERFORM I-SUMA-REG-02 THRU F-SUMA-REG-02
                       UNTIL FIN-OK = 1.
           CLOSE TEMPINFO OPEN I-O TEMPINFO
           PERFORM I-INICIE-START    THRU F-INICIE-START
           MOVE    0                 TO   FIN-OK
           PERFORM I-PROCESO-DAVIV   THRU F-PROCESO-DAVIV
                       UNTIL FIN-OK = 1
           PERFORM I-TOTAL-ARCHIVO   THRU F-TOTAL-ARCHIVO.
           PERFORM I-ACT-TABLAEMP    THRU F-ACT-TABLAEMP.
           PERFORM I-ACT-EMPRESAS    THRU F-ACT-EMPRESAS.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVO.
           MOVE  W-PROGRAMA  TO  I-DES-NOMB-PROG.
           MOVE ZEROS TO WK-PAGINA-L WK-LINEAS-L W-VALOR-INCONSISTENCIA
                         W-NUMER-INCONS.
           MOVE W-NIT-EMP        TO    WNIT-TEMP.
           MOVE W-NIT-EMP-EXT    TO    W-DIGITO.
           MOVE W-DIG2           TO    WNIT-EXT.
           MOVE WIN-EMPRESA      TO    I-TITU-EMPRESA.
           MOVE WEMPRESA         TO    TIT-CIA.
           MOVE WIN-NOMINA       TO    CON-PERIODO.
           PERFORM I-LABEL-TABLAEMP   THRU  F-LABEL-TABLAEMP.
           PERFORM I-LABEL-TEMPINFO   THRU  F-LABEL-TEMPINFO.
           PERFORM I-LABEL-CONTRATO   THRU  F-LABEL-CONTRATO.
           MOVE  1               TO   IND-INFORMES.
           MOVE  1               TO   IND-REPORTES.
           PERFORM I-LABEL-INFORMES   THRU  F-LABEL-INFORMES.
           PERFORM I-LABEL-REPORTES   THRU  F-LABEL-REPORTES.

  
           PERFORM I-STATUS-TBARCDAV  THRU  F-STATUS-TBARCDAV.
       F-LABEL-ARCHIVO. EXIT.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-ABRE-TBARCDAV THRU F-ABRE-TBARCDAV.
           PERFORM I-ABRE-VMNOMINA THRU F-ABRE-VMNOMINA.
           OPEN INPUT  CONCPTOS TEMPINFO NOMICIUD CONTRATO HOJAVIDA
                       NOMICORP.
           OPEN I-O    TABLAEMP.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-ABRE-TBARCDAV.
           MOVE ZEROS TO SW-ABRE.
           OPEN I-O TBARCDAV WITH LOCK.
           MOVE 1     TO SW-ABRE.
           IF TBARCDAV-STATUS = "93"
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            "Sr. usuario archivo ocupado, [ENTER] Reintentar [F2] Salir"
            LINE 25 POSITION 01 BEEP REVERSE
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO
            IF F2 EXIT PROGRAM END-IF
            GO I-ABRE-TBARCDAV.
       F-ABRE-TBARCDAV. EXIT.
      *
       I-ABRE-VMNOMINA.
           ACCEPT   WK-FECHA-HOY  FROM  DATE.
           MOVE   WK-ANO      TO   TBARCDAV-ANO.
           MOVE   WK-MES      TO   TBARCDAV-MES.
           MOVE   WK-DIA      TO   TBARCDAV-DIA.
           MOVE   WIN-TIP-CTA TO   TBARCDAV-TCU.
           MOVE   01          TO   TBARCDAV-VEZ.
           PERFORM I-LEE-TBARCDAV     THRU  F-LEE-TBARCDAV.
           PERFORM I-LABEL-VMNOMINA   THRU  F-LABEL-VMNOMINA.
           PERFORM I-STATUS-VMNOMINA  THRU  F-STATUS-VMNOMINA.
           OPEN I-O VMNOMINA.
           MOVE LABEL-VMNOMINA TO I-NOM-ARCH-VMNO.
       F-ABRE-VMNOMINA. EXIT.
      *
       I-LEE-TBARCDAV.
           MOVE ZEROS TO RUT-OK.
           READ TBARCDAV INVALID KEY MOVE 1 TO RUT-OK.
           IF RUT-OK = 1
            MOVE "N" TO TBARCDAV-IND-ASCII
            WRITE REC-TBARCDAV INVALID
             DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
                     SPACES SIZE 80 LINE 25 POSITION 01
             "Se ha presentado un error en TBARCDAV"
             LINE 24 POSITION 01
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE
            END-WRITE
           ELSE
            IF TBARCDAV-IND-ASCII = "S"
             ADD 1 TO TBARCDAV-VEZ
             GO TO I-LEE-TBARCDAV
            END-IF
           END-IF.
       F-LEE-TBARCDAV. EXIT.
      *
       I-CARGA-TABLAEMP.
           MOVE ZEROS   TO  W-TPN-CODIGO-TABLA
                                W-TPN-CODIGO-EMPRESA
                                W-TPN-CODIGO-ANO
                                W-TPN-CODIGO-PERIODO
                                W-TPN-CODIGO-FILLER
                                W-TPN-FECHA-DESDE
                                W-TPN-FECHA-HASTA
                                W-TPN-FECHA-PAGO
                                W-TPN-SEMANAS-RFTE
                                W-TPN-NUMERO-DIAS
                                W-TPN-NUMERO-HORAS
               MOVE SPACES                 TO  W-TPN-FLAG-PERIODO
               MOVE ZEROS                  TO  TAB-EMP-CLAVE
                                               TAB-EMP-FILLER
               MOVE 011                    TO  TE011-CODIGO-TABLA
               MOVE CON-COD-EMPRESA        TO  TE011-CODIGO-EMPRESA
               MOVE CON-ANO                TO  TE011-CODIGO-ANO
               MOVE CON-PERIODO            TO  TE011-CODIGO-PERIODO
               MOVE TAB-EMP-CLAVE          TO  W-TAB-CLAVE
               MOVE ZEROS                  TO  I-ERROR
               READ TABLAEMP WITH NO LOCK INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  TABLAEMP-STATUS              =  "23"
               MOVE "90"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA
           ELSE
               PERFORM RUTINA-ERROR-TABLAEMP.
           IF  TAB-EMP-CODIGO-TABLA         >  011
               MOVE "90"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE TE011-CODIGO-TABLA     TO  W-TPN-CODIGO-TABLA
               MOVE TE011-CODIGO-EMPRESA   TO  W-TPN-CODIGO-EMPRESA
               MOVE TE011-CODIGO-ANO       TO  W-TPN-CODIGO-ANO
               MOVE TE011-CODIGO-PERIODO   TO  W-TPN-CODIGO-PERIODO
               MOVE TE011-CODIGO-FILLER    TO  W-TPN-CODIGO-FILLER
               MOVE TE011-FECHA-DESDE      TO  W-TPN-FECHA-DESDE
               MOVE TE011-FECHA-HASTA      TO  W-TPN-FECHA-HASTA
               MOVE TE011-FECHA-PAGO       TO  W-TPN-FECHA-PAGO
               MOVE TE011-SEMANAS-RFTE      TO  W-TPN-SEMANAS-RFTE
               MOVE TE011-FLAG-PERIODO     TO  W-TPN-FLAG-PERIODO
               MOVE TE011-NUMERO-DIAS      TO  W-TPN-NUMERO-DIAS
               MOVE TE011-NUMERO-HORAS     TO  W-TPN-NUMERO-HORAS.
           PERFORM I-VERIFICA-IND-ACT  THRU F-VERIFICA-IND-ACT.
       F-CARGA-TABLAEMP.
      *
       I-VERIFICA-IND-ACT.
           IF TE011-FLAG-ACT-DAV = "T"
            PERFORM I-YA-ACT-VMNOMINA THRU F-YA-ACT-VMNOMINA
            PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-VERIFICA-IND-ACT. EXIT.
      *
       I-YA-ACT-VMNOMINA.
            MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "บ Apreciado usuario :                            บ"
            LINE 09 POSITION 17 BEEP REVERSE.
            DISPLAY "บ Usted esta REPROCESANDO el envio de informacionบ"
            LINE 10 POSITION 17 BEEP REVERSE.
            DISPLAY "บ de  nomina a  pagos por DAVIVIENDA, proceso queบ"
            LINE 11 POSITION 17 BEEP REVERSE.
            DISPLAY "บ ya fue realizado por el usuario     el dia     บ"
            LINE 12 POSITION 17 BEEP REVERSE.
            DISPLAY "บ          a las            .                    บ"
            LINE 13 POSITION 17 BEEP REVERSE.
            DISPLAY "บ                                                บ"
            LINE 14 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 15 POSITION 17 BEEP REVERSE
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            TE011-USUA-ACT-DAV LINE 12 POSITION 51 BLINK
            MOVE TE011-FECH-ACT-DAV TO WI-FECHA
            DISPLAY WI-FECHA LINE 13 POSITION 19
            MOVE TE011-HORA-ACT-DAV TO WK-DESC-N8
            MOVE ":" TO WK-PUNTOSMT WK-PUNTOSHT
            MOVE "." TO WK-PUNTOSST
            MOVE PRI-DES TO WK-MASCARA-HORAT
            MOVE SEG-DES TO WK-MASCARA-MINUT
            MOVE TER-DES TO WK-MASCARA-SEGUT
            MOVE CUA-DES TO WK-MASCARA-MISET
            DISPLAY WK-MASCARA-HORAS-T LINE 13 POSITION 34
            MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "Favor de cualquier tecla para finalizar..."
            LINE 14 POSITION 19 BLINK.
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
       F-YA-ACT-VMNOMINA. EXIT.
      *
       I-STATUS-TABLANOV.
               COMPUTE W-TAMANO-TABLANOV    =  I  -  1.
           IF  W-TAMANO-TABLANOV            >  ZEROS
               NEXT SENTENCE
           ELSE
               MOVE "01"                   TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-STATUS-TABLANOV.
      *
       I-PANTALLA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           WK-RAYAS-ARRIBA              LINE  1 POSITION 01 ERASE
           "บ"                          LINE  2 POSITION  1
           WK-COMPANIA                  LINE  2 POSITION  3
           "SISTEMA DE NOMINA บ"        LINE  2 POSITION 62
           "บ FECHA:"                   LINE  3 POSITION  1
           WK-MASCARA-FECHA             LINE  3 POSITION 10
           "I333 - PAGOS CORPORACION DAVIVIENDA"
           LINE  3 POSITION 29 REVERSE
           "HORA:"                      LINE  3 POSITION 68
           WK-MASCARA-HORA              LINE  3 POSITION 74
           "บ"                          LINE  3 POSITION 80
           WK-RAYAS                     LINE  4 POSITION  1
           "ั"                          LINE  4 POSITION 33
           "ั"                          LINE  4 POSITION 54
           "บ PERIODO:"                 LINE  5 POSITION  1
           CON-PERIODO                  LINE  5 POSITION 12
           "DESDE:"                     LINE  5 POSITION 16
           "ณ DIAS PERIODO :"           LINE  5 POSITION 33
           "ณ SEMANAS RFTE  :"           LINE  5 POSITION 54
           "บ"                          LINE  5 POSITION 80
           "บ"                          LINE  6 POSITION  1
           "HASTA:"                     LINE  6 POSITION 16
           "ณ HORAS PERIODO:"           LINE  6 POSITION 33
           "ณ FECHA DE PAGO:"           LINE  6 POSITION 54
           "บ"                          LINE  6 POSITION 80
           WK-RAYA                      LINE  7 POSITION  1
           "ย"                          LINE  7 POSITION 29
           "ม"                          LINE  7 POSITION 33
           "ม"                          LINE  7 POSITION 54
           "ย"                          LINE  7 POSITION 57
           "บ DEVENGADOS :"             LINE  8 POSITION  1
           "ณ DESCUENTOS :"             LINE  8 POSITION 29
           "ณ NETO :"                   LINE  8 POSITION 57
           "บ"                          LINE  8 POSITION 80
           WK-RAYAS                     LINE  9 POSITION  1
           "ฯ"                          LINE  9 POSITION 29
           "ฯ"                          LINE  9 POSITION 57
           "บ"                          LINE 10 POSITION  1
           "บ"                          LINE 10 POSITION 80
           "บ"                          LINE 11 POSITION  1
           "บ"                          LINE 11 POSITION 80
           "บ"                          LINE 12 POSITION  1
           "บ"                          LINE 12 POSITION 80
           "บ"                          LINE 13 POSITION  1
           "บ"                          LINE 13 POSITION 80
           "บ"                          LINE 14 POSITION  1
           "บ"                          LINE 14 POSITION 80
           "บ"                          LINE 15 POSITION  1
           "บ"                          LINE 15 POSITION 80
           WK-RAYA                      LINE 16 POSITION  1
           "ว"                          LINE 16 POSITION  1
           "ถ"                          LINE 16 POSITION 80
           "บ"                          LINE 17 POSITION  1
           "PERIODO DAVIVIENDA :"       LINE 17 POSITION  3
           "บ"                          LINE 17 POSITION 80
           "FECHA DESDE :"              LINE 17 POSITION 30
           "FECHA PAGO  :"              LINE 17 POSITION 55
           WK-RAYA                      LINE 18 POSITION  1
           " FECHA PROCESO DAVIVIENDA : " LINE 19 POSITION 30
           "ว"                          LINE 18 POSITION  1
           "ถ"                          LINE 18 POSITION 80
           "บ"                          LINE 19 POSITION  1
           "บ"                          LINE 19 POSITION 80
           "บ"                          LINE 20 POSITION  1
           "บ"                          LINE 20 POSITION 80
           "บ"                          LINE 21 POSITION  1
           "บ"                          LINE 21 POSITION 80
           "บ"                          LINE 22 POSITION  1
           "บ"                          LINE 22 POSITION 80
           WK-RAYAS-ABAJO               LINE 23 POSITION  1.
       020-DISPLAY-DATOS-PERIODO.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
           MOVE W-TPN-FECHA-DESDE      TO  WI-FECHA
           DISPLAY WI-FECHA                     LINE  5 POSITION 23
           DISPLAY WI-FECHA                     LINE 17 POSITION 44
           MOVE W-TPN-FECHA-HASTA      TO  WI-FECHA
           DISPLAY WI-FECHA                     LINE  6 POSITION 23
           MOVE W-TPN-NUMERO-DIAS      TO  WI-Z2
           DISPLAY WI-Z2                        LINE  5 POSITION 51
           MOVE W-TPN-NUMERO-HORAS     TO  WI-Z3
           DISPLAY WI-Z3                        LINE  6 POSITION 50
           MOVE W-TPN-SEMANAS-RFTE      TO  WI-Z
           DISPLAY WI-Z                         LINE  5 POSITION 71
           MOVE W-TPN-FECHA-PAGO       TO  WI-FECHA
           DISPLAY WI-FECHA                     LINE  6 POSITION 71.
           MOVE W-TPN-CODIGO-EMPRESA       TO  W-NIVFRA1-MVDOCTOS
           MOVE W-TPN-CODIGO-PERIODO       TO  W-NIVFRA2-MVDOCTOS.
           DISPLAY W-TPN-CODIGO-PERIODO LINE 17 POSITION 24

           "INCONSISTENCIAS      ---->"     LINE 21 POSITION 03 BLINK
           LABEL-REPORTES                   LINE 21 POSITION 30 REVERSE
           NOM-PAPEL (IND-REPORTES)         LINE 21 POSITION 66 REVERSE

           "ARCHIVO DE IMPRESION ---->"     LINE 22 POSITION 03 BLINK
           LABEL-INFORMES                   LINE 22 POSITION 30 REVERSE
           NOM-PAPEL (IND-INFORMES)         LINE 22 POSITION 66 REVERSE.
       CONFIRMA-ARCHIVO.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR " LINE 24 POSITION 1
             REVERSE
           DISPLAY " [F2]  TERMINAR " LINE 24 POSITION 32 REVERSE
               PERFORM ACEPTA-TRUCO
           MOVE ZEROS TO SW-DAV.
           IF  F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO   NOT =  "C"
            GO TO CONFIRMA-ARCHIVO.
           PERFORM I-ELIJA-CORPORACION THRU F-ELIJA-CORPORACION.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
         FECHA-HASTA.
           PERFORM I-ACEPTA-FECHA-HASTA   THRU   F-ACEPTA-FECHA-HASTA
           IF   CUP   GO TO   CONFIRMA-ARCHIVO.
           PERFORM I-ACEPTA-FECHA-DAVIVIENDA THRU
                   F-ACEPTA-FECHA-DAVIVIENDA.
           IF   CUP   GO TO   FECHA-HASTA.
       F-PANTALLA. EXIT.
      *
       I-ELIJA-CORPORACION.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           SPACES SIZE 80 LINE 24 POSITION 01
           "Por favor elija corporaciขn donde se realizar  el pago."
           LINE 25 POSITION 01
           "<F8> CORPAVI ข BCO DEL ESTADO."    LINE 10 POSITION 02
           "<F9> DAVIVIENDA." LINE 11 POSITION 02.
           PERFORM ACEPTA-TRUCO UNTIL F9 OR F8.
           IF F8
            MOVE "N" TO W-VALIDA-CUENTA
            MOVE "TABLAS\TBARCBDE" TO LABEL-TBARCDAV
            DISPLAY "<F8> CORPAVI ข BCO DEL ESTADO"
                LINE 10 POSITION 02 REVERSE.
           IF F9
            MOVE "S" TO W-VALIDA-CUENTA
            DISPLAY "<F9> DAVIVIENDA." LINE 11 POSITION 02 REVERSE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
       F-ELIJA-CORPORACION. EXIT.
      *
       I-ACEPTA-FECHA-HASTA.
           MOVE ZEROS              TO  WK-EXCEPTION
           MOVE WK-FECHA-HASTA     TO  WK-FECHA-INVERTIDA WI-Z8
           DISPLAY WK-FECHA-HASTA  LINE 17 POSITION 69
           ACCEPT WK-FECHA-HASTA   LINE 17 POSITION 69
                                   CONVERT UPDATE TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           IF  F2   PERFORM I-FIN-MODE   THRU   F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE WK-FECHA-HASTA  TO  WK-FECHA-INVERTIDA W-FECHA
           DISPLAY W-FECHA     LINE 17 POSITION 69
           IF  CUP GO TO F-ACEPTA-FECHA-HASTA.
           IF  W-TPN-FECHA-DESDE   >   WK-FECHA-HASTA
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             DISPLAY "Fecha hasta es menor que la desde, esta errada. "
             LINE 25 POSITION 01 REVERSE
             GO TO I-ACEPTA-FECHA-HASTA.
           MOVE WK-FECHA-HASTA TO I-FECH-PAGO.
       F-ACEPTA-FECHA-HASTA. EXIT.
      *
       I-ACEPTA-FECHA-DAVIVIENDA.
           MOVE ZEROS             TO   WK-EXCEPTION
           MOVE WK-FECHA-HASTA    TO   WK-FECHA-DAVIV
           MOVE WK-FECHA-DAVIV    TO   WI-Z8
           DISPLAY WI-Z8          LINE 19 POSITION 58
           ACCEPT WK-FECHA-DAVIV  LINE 19 POSITION 58
                                  UPDATE TAB NO BEEP
                               ON EXCEPTION WK-EX
                                  PERFORM 999-EXCEPTION.
           MOVE WK-FECHA-DAVIV    TO   WI-Z8
           DISPLAY WI-Z8          LINE 19 POSITION 58
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  GO TO   F-ACEPTA-FECHA-DAVIVIENDA.
           IF  WK-FECHA-DAVIV = ZEROS
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Fecha en ceros, es errada." LINE 25 POSITION 01 BEEP
            GO TO I-ACEPTA-FECHA-DAVIVIENDA.
           MOVE WK-FECHA-DAVIV TO W-FECHA.
           MOVE W-FECHA        TO I-FECH-PROCESO.
       F-ACEPTA-FECHA-DAVIVIENDA. EXIT.

       I-INICIE-START.
           MOVE ZEROS                  TO  WK-LINEAS
           DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           PERFORM I-ORDEN-INFORME THRU F-ORDEN-INFORME
           DISPLAY "CEDULA :"          LINE 12 POSITION  3
           DISPLAY "NOMBRE : "         LINE 12 POSITION 26
           DISPLAY "DEVENGADOS :"      LINE 14 POSITION  3
           DISPLAY "DESCUENTOS :"      LINE 14 POSITION 31
           DISPLAY "NETO :"            LINE 14 POSITION 59.
       F-INICIE-START.

      ***************************************************************
      *                    ORDEN-INFORME                            *
      ***************************************************************
       I-ORDEN-INFORME.
           MOVE ZEROS  TO TIMAE-CEDULA  TIMAE-CEDULA4
           MOVE SPACES TO TIMAE-CODIEMP TIMAE-CODIEMP5
           MOVE SPACES TO TIMAE-NOMBRE  TIMAE-NOMBRE2  TIMAE-NOMBRE3
           MOVE ZEROS  TO TIMAE-CODICEN TIMAE-CODICEN4 TIMAE-CODICEN5
           DISPLAY "ORDEN:" LINE 10 POSITION 33
           PERFORM I-ORDEN-ALFABETICO THRU F-ORDEN-ALFABETICO
           PERFORM I-ORDEN-CODIGO     THRU F-ORDEN-CODIGO.
           PERFORM I-ORDEN-NIT        THRU F-ORDEN-NIT.
           PERFORM I-CC-ALFABETICO    THRU F-CC-ALFABETICO.
           PERFORM I-CC-NIT           THRU F-CC-NIT.
           PERFORM I-CC-CODIGO        THRU F-CC-CODIGO.
       F-ORDEN-INFORME.

       I-ORDEN-ALFABETICO.
           IF WIN-ORDEN                    =  "A "
              DISPLAY "ALFABETICO" LINE 10 POSITION 45
              START TEMPINFO KEY >  TIMAE-CLAVE2 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-ALFABETICO.

       I-ORDEN-CODIGO.
           IF WIN-ORDEN                    =  "C "
              DISPLAY "CODIGO"  LINE 10 POSITION 40
              START TEMPINFO KEY > TIMAE-CLAVE1 INVALID  KEY
              MOVE "96"      TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-CODIGO.

       I-ORDEN-NIT.
           IF  WIN-ORDEN                    =  "N "
               DISPLAY "CEDULAS"  LINE 10 POSITION 40
             START TEMPINFO KEY > TIMAE-CLAVE INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-ORDEN-NIT.

       I-CC-ALFABETICO.
           IF  WIN-ORDEN                    =  "CA"
               OR  WIN-ORDEN                =  "KA"
               DISPLAY "C.C.- ALFABETICO"  LINE 10 POSITION 40
               START TEMPINFO KEY >  TIMAE-CLAVE3 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-ALFABETICO.

       I-CC-NIT.
           IF  WIN-ORDEN                    =  "CN"
           OR  WIN-ORDEN                    =  "KN"
             DISPLAY "C.C.- CEDULAS "  LINE 10 POSITION 40
               START TEMPINFO KEY > TIMAE-CLAVE4 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-NIT.

       I-CC-CODIGO.
           IF  WIN-ORDEN                    =  "CC"
           OR  WIN-ORDEN                    =  "KC"
             DISPLAY "C.C.- CODIGOS "  LINE 10 POSITION 40
               START TEMPINFO KEY > TIMAE-CLAVE5 INVALID  KEY
               MOVE "96"                   TO  STATUS-NOMINA
              PERFORM RUTINA-ERROR-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CC-CODIGO.

       I-LEE-TEMPINFO.
           READ TEMPINFO NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO FIN-OK GO F-LEE-TEMPINFO.
           IF TIMAE-NETO = ZEROS OR
              TIMAE-NETO < ZEROS
              GO I-LEE-TEMPINFO.
       F-LEE-TEMPINFO.

       I-LEE-CONTRATO.
           READ CONTRATO WITH NO LOCK INVALID  KEY
               MOVE "97"  TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-LEE-CONTRATO.

       I-LEE-HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID  KEY
               MOVE "99"          TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-LEE-HOJAVIDA.

       I-DATOS-GENERICOS.
      * El archivo informes se crea aqui para no daคarlo cuando se
      * intente reprocesar el modulo en cualquier empresa.
           OPEN OUTPUT INFORMES REPORTES.

           MOVE WIN-COD-BANCO    TO CORP-CODIGO
           PERFORM I-LEE-CORPO THRU F-LEE-CORPO
           MOVE CORP-CTA-CTE(WIN-TIP-CTA) TO I-NRO-CUENTA-DB.

               MOVE WIN-CODIGO-EMPRESA     TO  I-CODIGO-EMPRESA
               MOVE CON-PERIODO            TO  I-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  I-FECHA-DESDE
                                               C-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  I-FECHA-HASTA
                                               C-FECHA-HASTA
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE 1                      TO  WK-PAGINA
               MOVE 88                     TO  WK-LINEAS
               SUBTRACT 1                FROM  WK-DIA-HASTA
               IF  WK-DIA-HASTA                <  1
                   SUBTRACT 1                FROM  WK-MES-HASTA
                   IF  WK-MES-HASTA             <  1
                      SUBTRACT 1            FROM  WK-ANO-HASTA
                      MOVE 12                 TO  WK-MES-HASTA.
              IF  WK-DIA-HASTA         <  1
                  MOVE WK-DIAS-MES        (WK-MES-HASTA)
                                          TO  WK-DIA-HASTA.
               DISPLAY WK-FECHA-DAVIV LINE 25 POSITION 01.
               DISPLAY A-FECHA-PAGO-0 LINE 25 POSITION 20.
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-0.
               MOVE ZEROS                  TO  A-TALON-1
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-1
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-1A.
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-2
                                               A-FECHA-PAGO-2A.
       F-DATOS-GENERICOS.

       I-LEE-CORPO.
           READ NOMICORP WITH NO LOCK INVALID KEY
                MOVE 1 TO FIN-OK.
       F-LEE-CORPO.
      *
       I-SUMA-REG-02.
           PERFORM I-LEE-TEMPINFO THRU F-LEE-TEMPINFO
           IF FIN-OK = 1 GO F-SUMA-REG-02.
      *    DISPLAY "PASE I-SUMA-REG-02" STOP " "
           ADD 1                  TO   A-NUMERO-TOTAL.
           ADD TIMAE-NETO         TO   W-NETO-A-PAGAR
           ADD TIMAE-NETO         TO   A-VALOR-TOTAL
           ADD TIMAE-NETO         TO   A-VALOR-DEPOSITO-2
           ADD TIMAE-NETO         TO   A-VALOR-RETIROS-0
           MOVE TIMAE-CEDULA      TO   CONTRATO-CEDULA
           PERFORM I-LEE-CONTRATO THRU F-LEE-CONTRATO
           IF  CONTRATO-SW3 > 6 GO F-SUMA-REG-02.
           MOVE CONTRATO-CEDULA      TO   HOJAVIDA-CEDULA
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
      * Si el pago se va a relizar por corpavi no valida cuentas.
           IF W-VALIDA-CUENTA = "S" OR
              CONTRATO-CUENTA = ZEROS
            PERFORM I-VAYA-VALIDE THRU F-VAYA-VALIDE.
       F-SUMA-REG-02. EXIT.
      *
       I-VAYA-VALIDE.
           MOVE    ZEROS          TO   W-CUENTA
           MOVE    CONTRATO-CUENTA   TO   W-CUENTA.
           PERFORM VALIDA-CUENTA THRU SALE-VALIDA-CUENTA
           IF  SW-CUENTA          =  ZEROS
               SUBTRACT TIMAE-NETO  FROM A-VALOR-TOTAL
               SUBTRACT TIMAE-NETO  FROM A-VALOR-DEPOSITO-2
               SUBTRACT TIMAE-NETO  FROM A-VALOR-RETIROS-0
               SUBTRACT 1         FROM A-NUMERO-TOTAL.
       F-VAYA-VALIDE. EXIT.
      *
       I-PROCESO-DAVIV.
           PERFORM I-LEE-TEMPINFO THRU F-LEE-TEMPINFO
           IF FIN-OK = 1 GO F-PROCESO-DAVIV.
           MOVE TIMAE-CEDULA           TO  CONTRATO-CEDULA
           PERFORM I-LEE-CONTRATO THRU F-LEE-CONTRATO
           IF  CONTRATO-SW3 > 6 GO I-PROCESO-DAVIV.
           MOVE CONTRATO-CEDULA      TO  HOJAVIDA-CEDULA
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
      * Si el pago se va a relizar por corpavi no valida cuentas.
           IF W-VALIDA-CUENTA = "S" OR
              CONTRATO-CUENTA = ZEROS
            MOVE    ZEROS TO W-CUENTA
            MOVE    CONTRATO-CUENTA  TO  W-CUENTA
            PERFORM VALIDA-CUENTA THRU SALE-VALIDA-CUENTA
           END-IF
           PERFORM I-MUEVE-DATOS THRU F-MUEVE-DATOS.
       F-PROCESO-DAVIV.

       I-MUEVE-DATOS.
           IF  WK-LINEAS  >  55 OR WK-LINEAS = 0
               PERFORM TITULOS-INFORME THRU SALE-TITULOS-INFORME.
           ADD  1                      TO  A-TALON-1
           MOVE A-TALON-1              TO  I-SECUENC
           MOVE CONTRATO-CUENTA           TO  I-CUENTA
      *    MOVE CONTRATO-CODIEMP          TO  I-CODIGO
           MOVE CONTRATO-CEDULA           TO  I-NIT
           MOVE HOJAVIDA-NOMBRE           TO  I-NOMBRE
           MOVE TIMAE-NETO             TO  I-NETO-A-PAGAR

      * Si el pago se va a relizar por corpavi no valida cuentas.
           IF W-VALIDA-CUENTA = "S" OR
              CONTRATO-CUENTA = ZEROS
            IF SW-CUENTA               =  ZEROS
             MOVE "CTA ERRADA"          TO  I-OBSERVACIONES
             WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
             ADD  1                      TO  WK-LINEAS
             MOVE 1                      TO  SW-DAV
             ADD 1 TO W-NUMER-INCONS
             MOVE W-NUMER-INCONS TO I-SECUENC
             ADD TIMAE-NETO TO W-VALOR-INCONSISTENCIA
             IF WK-LINEAS-L > 55 OR WK-LINEAS-L = 0
              PERFORM I-TITULOS-REPORTES THRU F-TITULOS-REPORTES
             END-IF
             WRITE REG-REPORTES   FROM   LINEA-DETALLE   AFTER  1
             ADD 1 TO WK-LINEAS-L
             GO TO F-MUEVE-DATOS
            END-IF.

           MOVE ZEROS TO SW1.
           PERFORM I-ACTUAL-VMNOMINA   THRU F-ACTUAL-VMNOMINA.
           IF SW1 = 1
               MOVE "CTA.DUPLIC"         TO  I-OBSERVACIONES
               WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
               SUBTRACT TIMAE-NETO  FROM A-VALOR-TOTAL
               SUBTRACT TIMAE-NETO  FROM A-VALOR-DEPOSITO-2
               SUBTRACT TIMAE-NETO  FROM A-VALOR-RETIROS-0
               ADD  1                      TO  WK-LINEAS
               MOVE 1                      TO  SW-DAV
            ADD 1 TO W-NUMER-INCONS
            MOVE W-NUMER-INCONS TO I-SECUENC
            ADD TIMAE-NETO TO W-VALOR-INCONSISTENCIA
            IF WK-LINEAS-L > 55 OR WK-LINEAS-L = 0
             PERFORM I-TITULOS-REPORTES THRU F-TITULOS-REPORTES
            END-IF
            WRITE REG-REPORTES   FROM   LINEA-DETALLE   AFTER  1
            ADD 1 TO WK-LINEAS-L
            GO TO F-MUEVE-DATOS.

           MOVE SPACES                 TO  I-OBSERVACIONES
           WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
           ADD  1                      TO  WK-LINEAS
           MOVE CONTRATO-CUENTA           TO  A-CUENTA-1
           MOVE TIMAE-NETO             TO  A-VALOR-DEPOSITO-1
           MOVE A-TALON-1              TO  A-REFERENCIA-1
           MOVE CONTRATO-CEDULA           TO  A-CEDULA

           ADD  TIMAE-NETO             TO  W-TOTAL-A-PAGAR
           MOVE CONTRATO-CEDULA           TO  WI-Z12
           DISPLAY WI-Z12              LINE 12 POSITION 12
           DISPLAY HOJAVIDA-NOMBRE        LINE 12 POSITION 35
           MOVE W-NETO-A-PAGAR         TO WI-Z8P
           DISPLAY WI-Z8P              LINE 14 POSITION 69.
       F-MUEVE-DATOS. EXIT.
      *
       I-ACTUAL-VMNOMINA.
           MOVE CONTRATO-CUENTA       TO VMNOMINA-CUENTA.
           MOVE CONTRATO-CEDULA       TO VMNOMINA-CEDULA.
           MOVE WIN-CODIGO-EMPRESA TO VMNOMINA-EMPRESA.
           MOVE CONTRATO-CEDULA       TO VMNOMINA-CEDULA-02.
           MOVE CON-PERIODO        TO VMNOMINA-PERIODO-DE-PAGO.
           MOVE WK-FECHA-DAVIV     TO VMNOMINA-FECHA-DAVIVIENDA.
           MOVE TIMAE-NETO         TO VMNOMINA-VALOR-DEPOSITO.
           MOVE "T"                TO VMNOMINA-IND-PROCESO.
           WRITE REC-VMNOMINA INVALID
            MOVE 1 TO SW1
            DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            DISPLAY
           "No puede grabar este regis. de movimiento diario para transm
      -    "ision a davivienda,"  LINE 24 POSITION 01 BEEP
           "El numero de cuenta : " LINE 25 POSITION 01 BEEP
           VMNOMINA-CUENTA          LINE 25 POSITION 23 BEEP REVERSE
           "cedula : "              LINE 25 POSITION 37 BEEP
           VMNOMINA-CEDULA          LINE 25 POSITION 46 BEEP REVERSE
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
       F-ACTUAL-VMNOMINA. EXIT.
      *
       I-TOTAL-ARCHIVO.
           MOVE  WIN-COD-BANCO  TO  CORP-CODIGO
           PERFORM  I-LEE-CORPO  THRU  F-LEE-CORPO
           MOVE  CORP-CTA-CTE(WIN-TIP-CTA)  TO  A-CTA-EMPRESA-2.
           MOVE  1                          TO  A-TALON-2
           MOVE  A-TALON-2                  TO  A-REFERENCIA-2
           MOVE  WK-FECHA-DAVIV             TO  A-FECHA-PAGO-2A
           MOVE  W-TEMPORAL                 TO  A-NIT-EMPRESA-2A
           MOVE  A-VALOR-TOTAL              TO  I-VALOR-CONSIG
           MOVE  W-NETO-A-PAGAR             TO  I-VALOR-TOTAL
           MOVE  W-VALOR-INCONSISTENCIA     TO  I-VALOR-INCONSISTENCIA.
           PERFORM  I-DISPLAY-DATOS  THRU  F-DISPLAY-DATOS.
           WRITE  REG-INFORMES  FROM  TITULO-5          BEFORE 1
           WRITE  REG-INFORMES  FROM  TITULO-6          BEFORE 1
           WRITE  REG-INFORMES  FROM  TITULO-7          BEFORE 1
           WRITE  REG-INFORMES  FROM  TITULO-REPORTE-7  BEFORE 1.
           WRITE  REG-INFORMES  FROM  TITULO-5          BEFORE 1.

           PERFORM  I-TOTAL-INCONSISTE  THRU  F-TOTAL-INCONSISTE.

           MOVE SPACES TO REG-INFORMES.
           WRITE REG-INFORMES AFTER PAGE.
           CLOSE INFORMES.
       F-TOTAL-ARCHIVO. EXIT.
      *
       I-TOTAL-INCONSISTE.
           IF  W-VALOR-INCONSISTENCIA = ZEROS
             CLOSE REPORTES
             GO F-TOTAL-INCONSISTE.

           IF WK-LINEAS-L > 55 OR WK-LINEAS-L = 0
            PERFORM   I-TITULOS-REPORTES   THRU   F-TITULOS-REPORTES
           END-IF
           WRITE   REG-REPORTES   FROM   TITULO-5   AFTER 1.
           ADD 1 TO WK-LINEAS-L.

           IF WK-LINEAS-L > 55 OR WK-LINEAS-L = 0
            PERFORM   I-TITULOS-REPORTES   THRU   F-TITULOS-REPORTES
           END-IF
           WRITE   REG-REPORTES   FROM   TITULO-REPORTE-7   AFTER 1.
           ADD 1 TO WK-LINEAS-L.

           IF WK-LINEAS-L > 55 OR WK-LINEAS-L = 0
            PERFORM   I-TITULOS-REPORTES   THRU   F-TITULOS-REPORTES
           END-IF
           WRITE   REG-REPORTES   FROM   TITULO-5   AFTER 1.
           ADD 1 TO WK-LINEAS-L.

           MOVE SPACES TO REG-REPORTES.
           WRITE REG-REPORTES AFTER PAGE.
           CLOSE  REPORTES.
       F-TOTAL-INCONSISTE. EXIT.
      *
       I-DISPLAY-DATOS.
           DISPLAY SPACES SIZE 80 LINE 24 POSITION 01
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
               DISPLAY WK-ESPACIOS                  LINE 12 POSITION  2
               DISPLAY WK-ESPACIOS                  LINE 14 POSITION  2
               DISPLAY "ARCHIVO      : "            LINE 11 POSITION 25
               DISPLAY "CUENTA No."                 LINE 12 POSITION 25
               DISPLAY A-CTA-EMPRESA-2              LINE 12 POSITION 36
               DISPLAY "FECHA PROCESO: "            LINE 13 POSITION 25
               MOVE WK-FECHA-HASTA         TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE 13 POSITION 40
               DISPLAY "TOTAL NOMINA : "            LINE 14 POSITION 25
               MOVE A-VALOR-TOTAL          TO  WI-NUMERO
               DISPLAY WI-NUMERO                    LINE 14 POSITION 40
               DISPLAY "No. REGISTROS: "            LINE 15 POSITION 25
               DISPLAY "RELACION:I333"              LINE 15 POSITION 65
               MOVE A-NUMERO-TOTAL         TO  WI-NUMERO
               DISPLAY WI-NUMERO                    LINE 15 POSITION 40.
               IF SW-DAV = 1 DISPLAY "CUENTAS ERRADAS"
                                    LINE 11 POSITION 63 REVERSE BLINK.
               PERFORM ACEPTA-TRUCO.
       F-DISPLAY-DATOS. EXIT.
      *
       I-TITULOS-REPORTES.
           MOVE  LABEL-REPORTES  TO  I-DES-ARCH-IMPR.
           ADD  1            TO  WK-PAGINA-L.
           MOVE WK-PAGINA-L  TO  I-PAGINA
           IF  WK-PAGINA-L = 1
            WRITE   REG-REPORTES   FROM  TITULO-0        AFTER  1
           ELSE
            WRITE   REG-REPORTES   FROM  TITULO-0        AFTER  PAGE
           END-IF
           WRITE   REG-REPORTES   FROM  TITULO-1        AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-2        AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-2B       AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-2C       AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-2D       AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-2E       AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-5        AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-3        AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-4        AFTER  1
           WRITE   REG-REPORTES   FROM  TITULO-5        AFTER  1.
           MOVE    10   TO   WK-LINEAS-L.
       F-TITULOS-REPORTES. EXIT.
      *
       TITULOS-INFORME.
           MOVE  LABEL-INFORMES  TO  I-DES-ARCH-IMPR.
           IF  WK-PAGINA                    >  1
               WRITE REG-INFORMES        FROM  TITULO-5     BEFORE PAGE.
               MOVE WK-PAGINA              TO  I-PAGINA
               WRITE REG-INFORMES        FROM  TITULO-0        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-1        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2B       BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2C       BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2D       BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2E       BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-3        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-4        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1.
               ADD  1                      TO  WK-PAGINA
               MOVE 10                     TO  WK-LINEAS.
       SALE-TITULOS-INFORME. EXIT.
      *
       I-LEE-NOMICIUD.
           MOVE 0 TO SW-EOF.
           READ NOMICIUD WITH NO LOCK INVALID KEY
             MOVE 1 TO SW-EOF.
       F-LEE-NOMICIUD.

       I-SELECCIONE-MVDOCTOS.
           ACCEPT WK-FECHA-HOY       FROM  DATE
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-FECHA-HOY           TO  WK-FECHA-STANDARD
           MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01 OR
               WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
           MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
           DISPLAY WK-DIA            LINE 20 POSITION 1.
           DISPLAY WK-ANO            LINE 20 POSITION 1.
           DISPLAY WK-MES            LINE 20 POSITION 1.
           DISPLAY W-CODEMP          LINE 20 POSITION 1.
           PERFORM ACEPTA-TRUCO.
       F-SELECCIONE-MVDOCTOS.
      *
       I-ACT-TABLAEMP.
           MOVE "T"                   TO     TE011-FLAG-ACT-DAV.
           MOVE COD-USR               TO     TE011-USUA-ACT-DAV.
           ACCEPT TE011-FECH-ACT-DAV  FROM   DATE.
           ACCEPT TE011-HORA-ACT-DAV  FROM   TIME.
           REWRITE REC-TABLAEMP INVALID
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "No se puede grabar en registro indicador de Transferencia"
            LINE 25 POSITION 01 BEEP
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
       F-ACT-TABLAEMP. EXIT.
      *
       I-ACT-EMPRESAS.
           OPEN    I-O  EMPRESAS.
           MOVE    "C"  TO WIN-FLAG-CIERRE.
           MOVE    ZEROS TO WIN-PERIODO-ESTADO.
           REWRITE REC-EMPRESAS INVALID
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "No se puede grabar en registro empresas, paso de CIERRE."
            LINE 25 POSITION 01 BEEP
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
           CLOSE   EMPRESAS.
       F-ACT-EMPRESAS. EXIT.
      *
       I-WIN-PERIODO-ESTADO.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 05 POSITION 17 BEEP
            DISPLAY "บ       NO HA PROCESADO LA NOMINA DEFINITIVA     บ"
            LINE 06 POSITION 17 BEEP
            DISPLAY "บ         NO PUEDE CONTINUAR CON ESTE PASO       บ"
            LINE 07 POSITION 17 BEEP
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 08 POSITION 17 BEEP
            "  <ENTER> CONTINUE "
            LINE 09 POSITION 29 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-WIN-PERIODO-ESTADO.   EXIT.
      *
       I-VERI-TIPO-PAGO.
            MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 05 POSITION 17 BEEP
            DISPLAY "บ   ESTA PROCESANDO PAGO EN CONSIGNACION Y LOS   บ"
            LINE 06 POSITION 17 BEEP
            DISPLAY "บ   PARAMETROS INICIALES DE LA EMPRESA HA SIDO   บ"
            LINE 07 POSITION 17 BEEP
            DISPLAY "บ    DEFINIDOS PARA PAGAR EN                     บ"
            LINE 08 POSITION 17 BEEP
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
      *     DISPLAY WIN-TIPO-PAGO LINE 11 POSITION 57
            IF WIN-TIPO-PAGO = 1
             DISPLAY "EFECTIVO" LINE 08 POSITION 48
            ELSE
             IF WIN-TIPO-PAGO = 2
              DISPLAY "CHEQUE  " LINE 08 POSITION 48
             ELSE
              DISPLAY "  N.N.  " LINE 08 POSITION 48.
            MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "Favor de cualquier tecla para finalizar..."
            LINE 14 POSITION 19 BLINK.
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
       F-VERI-TIPO-PAGO. EXIT.
      *
       I-FIN-MODE.
           CLOSE CONCPTOS NOMICORP NOMICIUD TABLAEMP CONTRATO
                 HOJAVIDA TEMPINFO VMNOMINA TBARCDAV.
           EXIT PROGRAM.
       120-FT. STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\CALCDIAS.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\VALIDAVI.PRO".
           COPY "..\PRO\VERICOBA.PRO".
      *
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\TEMPINFO.PLB".
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\INFORMES.PLB".
           COPY "..\PLB\VMNOMINA.PLB".
           COPY "..\PLB\REPORTES.PLB".
      *
           COPY "..\STA\VMNOMINA.STA".
           COPY "..\STA\TBARCDAV.STA".
      ******************************************************************


