       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG335.
      ******************************************************************
      *       PAGOS EN EFECTIVO ABR-27-2002                            *
      ******************************************************************
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
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG335".
       77      ID-PRG              PIC X(7)  VALUE "PROG335".
       77      W-COD-CTA                 PIC X(4)       VALUE SPACES.
       77      W-CTA-TEMP                PIC 9(12)      VALUE ZEROS.
       77      SW-CUENTA           PIC 9          VALUE ZEROS.
       01      W-TEMPORAL.
         03    WNIT-TEMP      PIC X(12)      VALUE ZEROS.
         03    WNIT-EXT       PIC X(01)      VALUE ZEROS.

       03     AREA-CONCEPTO.
         05    C-DATO                    PIC X(14)      VALUE SPACES.
         05    C-FECHA-DESDE             PIC X(11)      VALUE ZEROS.
         05    FILLER                    PIC X(4)       VALUE
              " AL ".
         05    C-FECHA-HASTA             PIC X(11)      VALUE ZEROS.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(4)       OCCURS 25 TIMES.
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
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\DAVIVIEN.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\LISTADOS.WRK".
      ******************************************************************
       01      AREA-ARCHIVO.
        03     REG-A-0.
         05    A-TIPO-0                  PIC 9(2)       VALUE 02.
         05    A-NOVEDAD-0               PIC X          VALUE SPACES.
         05    A-FECHA-PAGO-0            PIC 9(6).
         05    FILLER                    PIC 9(12)      VALUE ZEROS.
         05    A-OFICINA-0               PIC 9(4)       VALUE 0081.
         05    A-RETIROS-0               PIC 9(4)       VALUE 0001.
         05    A-VALOR-RETIROS-0         PIC 9(11)V99.
         05    A-NUMERO-TOTAL            PIC 9(4).
         05    A-VALOR-TOTAL             PIC 9(11)V99.
         05    FILLER                    PIC X(21)      VALUE ZEROS.

        03     REG-A-1.
         05    A-TIPO-1                  PIC 9(2)       VALUE 34.
         05    A-NOVEDAD-1               PIC X          VALUE SPACES.
         05    A-FECHA-PAGO-1            PIC 9(6).
         05    A-CUENTA-1                PIC X(12).
         05    A-OFICINA-1               PIC 9(4)       VALUE 0081.
         05    A-TALON-1                 PIC 9(4).
         05    FILLER                    PIC 9(13)      VALUE ZEROS.
         05    A-VALOR-DEPOSITO-1        PIC 9(11)V99.
         05    A-MOTIVO-1                PIC 9(2)       VALUE 06.
         05    A-REFERENCIA-1            PIC 9(4).
         05    A-FECHA-PAGO-1A           PIC 9(6).
         05    A-CEDULA                  PIC 9(13).

        03     REG-A-2.
         05    A-TIPO-2                  PIC 9(2)       VALUE 55.
         05    A-NOVEDAD-2               PIC X          VALUE SPACES.
         05    A-FECHA-PAGO-2            PIC 9(6).
         05    A-CTA-EMPRESA-2           PIC 9(12).
         05    A-OFICINA-2               PIC 9(4)       VALUE 0081.
         05    A-TALON-2                 PIC 9(4).
         05    FILLER                    PIC 9(13)      VALUE ZEROS.
         05    A-VALOR-DEPOSITO-2        PIC 9(11)V99.
         05    A-MOTIVO-2                PIC 9(2)       VALUE 06.
         05    A-REFERENCIA-2            PIC 9(4).
         05    A-FECHA-PAGO-2A           PIC 9(6).
         05    A-NIT-EMPRESA-2A          PIC 9(13).
      ******************************************************************
       01      AREA-INFORME.
        03     TITULO-0.
         05    FILLER          PIC X(02)   VALUE SPACES.
      *  05    FILLER          PIC 9(4)    USAGE IS COMP-1 VALUE 6962.
      *  05    FILLER          PIC 9(4)    USAGE IS COMP-1 VALUE 3602.
         05    TIT-CIA         PIC X(40)   VALUE SPACES.

        03     TITULO-1.
         05    FILLER          PIC X(02)   VALUE SPACES.
      *  05    FILLER          PIC 9(4)    USAGE IS COMP-1 VALUE 4626.
         05    FILLER          PIC X(23)   VALUE SPACES.
         05    FILLER          PIC X(29)   VALUE
              "  ***PAGOS EN EFECTIVO***    ".
         05    FILLER          PIC X(16)   VALUE SPACES.
         05    FILLER          PIC X(8)    VALUE "PAGINA:".
         05    I-PAGINA        PIC ZZZZ    VALUE ZEROS.

        03     TITULO-2.
         05    FILLER          PIC X(02)      VALUE SPACES.
      *  05    FILLER          PIC 9(4)     USAGE IS COMP-1 VALUE 4626.
         05    FILLER          PIC X(10)      VALUE "PERIODO:".
         05    I-PERIODO       PIC ZZZ        VALUE ZEROS.
         05    FILLER          PIC X(12)      VALUE SPACES.
         05    FILLER          PIC X(11)      VALUE "NOMINA DEL".
         05    I-FECHA-DESDE   PIC ZZZZ/ZZ/ZZ   VALUE ZEROS.
         05    FILLER          PIC X(4)       VALUE " AL ".
         05    I-FECHA-HASTA   PIC ZZZZ/ZZ/ZZ   VALUE ZEROS.
         05    FILLER          PIC X(12)      VALUE SPACES.
         05    FILLER          PIC X(08)      VALUE "FECHA : ".
         05    I-FECHA         PIC X(11)      VALUE SPACES.

        03     TITULO-2B.
         05    FILLER                    PIC X(02)      VALUE SPACES.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 4626.
         05    FILLER                    PIC X(17)      VALUE
              "LUGAR EN MISION: ".
         05    I-CODIGO-EMPRESA          PIC 9(07)      VALUE ZEROS.
         05    FILLER                    PIC X(01)      VALUE SPACES.
         05    I-TITU-EMPRESA            PIC X(35)      VALUE SPACES.
         05    FILLER                    PIC X(12)     VALUE SPACES.
         05    FILLER                    PIC X(08)  VALUE "HORA  : ".
         05    I-HORA                    PIC X(11)      VALUE SPACES.

       03     TITULO-2C.
         05    FILLER             PIC X(02)  VALUE SPACES.
         05    FILLER             PIC X(10)  VALUE "LISTADO: ".
         05    I-LISTADO          PIC X(30)  VALUE SPACES.
         05    FILLER             PIC X(28)  VALUE SPACES.
         05    FILLER             PIC X(08)  VALUE "CIUDAD: ".
         05    I-COD-CIUDAD       PIC 9(03)  VALUE ZEROS.
         05    FILLER             PIC X(01)  VALUE SPACES.
         05    I-CIUDAD           PIC X(13)  VALUE SPACES.

        03     TITULO-3.
         05    FILLER                    PIC X(85)      VALUE SPACES.
         05    FILLER                    PIC X(8)       VALUE
              "NETO A".

        03     TITULO-4.
         05    FILLER                    PIC X(03)      VALUE SPACES.
         05    FILLER                    PIC X(04)      VALUE "No.".
         05    FILLER                    PIC X(03)      VALUE SPACES.
         05    FILLER                    PIC X(13)      VALUE
              "   CUENTA".
         05    FILLER                    PIC X(13)      VALUE
              "OBSERVACIONES".
         05    FILLER                    PIC X(15)      VALUE
              "    N.I.T.".
         05    FILLER                    PIC X(31)      VALUE
              "APELLIDOS Y NOMBRES".
         05    FILLER                    PIC X(9)       VALUE
              "  PAGAR".

        03     TITULO-5.
         05    FILLER                    PIC X(92)      VALUE ALL "=".

        03     LINEA-DETALLE.
         05    FILLER                    PIC X(03)      VALUE SPACES.
         05    I-SECUENC                 PIC Z(04).
         05    FILLER                    PIC X(03)      VALUE SPACES.
         05    I-CUENTA                  PIC X(13)      VALUE SPACES.
         05    I-OBSERVACIONES           PIC X(12).
         05    I-NIT                     PIC ZZZ,ZZZ,ZZZ,ZZZ-
                                                        VALUE SPACES.
         05    I-NOMBRE                  PIC X(29)      VALUE SPACES.
         05    I-RESTO-LINEA-DETALLE.
          07   I-NETO-A-PAGAR            PIC ZZZ,ZZZ,ZZZ-.
      *  05    I-CODIGO                  PIC X(11)      VALUE SPACES.

        03     TITULO-6.
         05    FILLER                    PIC X(30)  VALUE SPACES.
         05    FILLER                    PIC X(36)  VALUE
               "T O T A L   P L A N I L L A........".
         05    FILLER                    PIC X(10)  VALUE SPACES.
         05    I-VALOR-TOTAL             PIC $ZZZ,ZZZ,ZZZ,ZZZ-.

        03     TITULO-7.
         05    FILLER                    PIC X(30)  VALUE SPACES.
         05    FILLER                    PIC X(36)  VALUE
               "T O T A L   E F E C T I V O ........".
         05    FILLER                    PIC X(10)  VALUE SPACES.
         05    I-VALOR-CONSIG            PIC $ZZZ,ZZZ,ZZZ,ZZZ-.
      ******************************************************************

               COPY "..\LNK\CONTROLA.LNK".
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
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS        THRU F-USUARIOS.
               IF      SW-USER = 1       EXIT PROGRAM.
               PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
               MOVE W-NIT-EMP              TO WNIT-TEMP
               MOVE W-NIT-EMP-EXT          TO WNIT-EXT
               MOVE WIN-EMPRESA            TO  I-TITU-EMPRESA
               PERFORM  I-LABEL-ARCHIVO  THRU  F-LABEL-ARCHIVO
               PERFORM  I-ABRE-ARCHIVOS  THRU  F-ABRE-ARCHIVOS.
               MOVE     ZEROS  TO  W-NETO-A-PAGAR A-NUMERO-TOTAL
               MOVE     WIN-COD-CIUDAD         TO   CIUD-CODIGO
               PERFORM  I-LEE-NOMICIUD    THRU F-LEE-NOMICIUD.
               MOVE     CIUD-NOMBRE            TO   I-CIUDAD.
               MOVE     CIUD-CODIGO            TO   I-COD-CIUDAD.
               PERFORM  I-CARGA-TABLAEMP THRU F-CARGA-TABLAEMP.
               PERFORM  I-STATUS-TABLANOV THRU F-STATUS-TABLANOV
               PERFORM  I-PANTALLA        THRU F-PANTALLA
               PERFORM  I-DATOS-GENERICOS THRU F-DATOS-GENERICOS
               PERFORM  I-INICIE-START    THRU F-INICIE-START
               MOVE     0                 TO   FIN-OK
               PERFORM  I-SUMA-REG-02 THRU F-SUMA-REG-02
                        UNTIL FIN-OK = 1.
               CLOSE TEMPINFO OPEN I-O TEMPINFO
               PERFORM I-INICIE-START    THRU F-INICIE-START
               MOVE    0                 TO   FIN-OK
               PERFORM I-PROCESO-DAVIV   THRU F-PROCESO-DAVIV
                       UNTIL FIN-OK = 1
               PERFORM I-TOTAL-ARCHIVO   THRU F-TOTAL-ARCHIVO.
      * Actualiza fin de ciclo de nomina en la empresa.
               PERFORM I-ACT-EMPRESAS    THRU F-ACT-EMPRESAS.
               PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      ***************************************************************
      *                    I-ACTUALIZA-EMPRESAS                     *
      ***************************************************************
       I-ACT-EMPRESAS.
           OPEN    I-O  EMPRESAS.
           MOVE    "C"  TO WIN-FLAG-CIERRE.
           MOVE     0   TO WIN-PERIODO-ESTADO.
           REWRITE REC-EMPRESAS INVALID
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "No se puede grabar en registro empresas, paso de CIERRE."
            LINE 24 POSITION 01 BEEP
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
           CLOSE   EMPRESAS.
       F-ACT-EMPRESAS. EXIT.
      *
       I-LABEL-ARCHIVO.
           IF WIN-TIPO-PAGO NOT = 1
            PERFORM I-VERI-TIPO-PAGO THRU F-VERI-TIPO-PAGO
            EXIT PROGRAM.
               MOVE WEMPRESA               TO  TIT-CIA
               MOVE WIN-NOMINA             TO  CON-PERIODO
               MOVE    1                   TO  IND-INFORMES
               PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
               PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
               PERFORM I-LABEL-TEMPINFO    THRU F-LABEL-TEMPINFO.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
       F-LABEL-ARCHIVO.
      *
       I-VERI-TIPO-PAGO.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
            DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 04 POSITION 17 BEEP REVERSE
            DISPLAY "บ              A T E N C I O N !                 บ"
            LINE 05 POSITION 17 BEEP REVERSE
            DISPLAY "บ      ESTA PROCESANDO PAGO EN EFECTIVO Y LOS    บ"
            LINE 06 POSITION 17 BEEP REVERSE.
            DISPLAY "บ    PARAMETROS INICIALES DE LA EMPRESA HA SIDO  บ"
            LINE 07 POSITION 17 BEEP REVERSE.
            DISPLAY "บ      DEFINIDOS PARA PAGAR EN                   บ"
            LINE 08 POSITION 17 BEEP REVERSE.
            DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
            LINE 09 POSITION 17 BEEP REVERSE
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
      *     DISPLAY WIN-TIPO-PAGO LINE 11 POSITION 57
            IF WIN-TIPO-PAGO = 3
             DISPLAY "CONSIGNACION" LINE 08 POSITION 48
            ELSE
             IF WIN-TIPO-PAGO = 2
              DISPLAY "CHEQUE  "    LINE 08 POSITION 48
             ELSE
              DISPLAY "  N.N.  "    LINE 08 POSITION 48.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            "     <ENTER>  CONTINUE "
            LINE 10 POSITION 30 BLINK.
            MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
            PERFORM ACEPTA-TRUCO.
       F-VERI-TIPO-PAGO. EXIT.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CONCPTOS TEMPINFO NOMICIUD
                           TABLAEMP CONTRATO
                           HOJAVIDA NOMICORP
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.

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
               READ TABLAEMP          INVALID  KEY
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
       F-CARGA-TABLAEMP.

       I-STATUS-TABLANOV.
               COMPUTE W-TAMANO-TABLANOV    =  I  -  1.
           IF  W-TAMANO-TABLANOV            >  ZEROS
               NEXT SENTENCE
           ELSE
               MOVE "01"                   TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-STATUS-TABLANOV.

       I-PANTALLA.
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "บ"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "WINNER FINANCIALฉ บ"        LINE  2 POSITION 62
               DISPLAY "บ FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY " PROCESO PAGO EN EFECTIVO " LINE  3 POSITION 29
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "บ"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "ั"                          LINE  4 POSITION 33
               DISPLAY "ั"                          LINE  4 POSITION 54
               DISPLAY "บ PERIODO:"                 LINE  5 POSITION  1
               DISPLAY CON-PERIODO                  LINE  5 POSITION 12
               DISPLAY "DESDE:"                     LINE  5 POSITION 16
               DISPLAY "ณ DIAS PERIODO :"           LINE  5 POSITION 33
               DISPLAY "ณ SEMANAS RFTE  :"           LINE  5 POSITION 54
               DISPLAY "บ"                          LINE  5 POSITION 80
               DISPLAY "บ"                          LINE  6 POSITION  1
               DISPLAY "HASTA:"                     LINE  6 POSITION 16
               DISPLAY "ณ HORAS PERIODO:"           LINE  6 POSITION 33
               DISPLAY "ณ FECHA DE PAGO:"           LINE  6 POSITION 54
               DISPLAY "บ"                          LINE  6 POSITION 80
               DISPLAY WK-RAYA                      LINE  7 POSITION  1
               DISPLAY "ย"                          LINE  7 POSITION 29
               DISPLAY "ม"                          LINE  7 POSITION 33
               DISPLAY "ม"                          LINE  7 POSITION 54
               DISPLAY "ย"                          LINE  7 POSITION 57
               DISPLAY "บ DEVENGADOS :"             LINE  8 POSITION  1
               DISPLAY "ณ DESCUENTOS :"             LINE  8 POSITION 29
               DISPLAY "ณ NETO :"                   LINE  8 POSITION 57
               DISPLAY "บ"                          LINE  8 POSITION 80
               DISPLAY WK-RAYAS                     LINE  9 POSITION  1
               DISPLAY "ฯ"                          LINE  9 POSITION 29
               DISPLAY "ฯ"                          LINE  9 POSITION 57
               DISPLAY "บ"                          LINE 10 POSITION  1
               DISPLAY "บ"                          LINE 10 POSITION 80
               DISPLAY "บ"                          LINE 11 POSITION  1
               DISPLAY "บ"                          LINE 11 POSITION 80
               DISPLAY "บ"                          LINE 12 POSITION  1
               DISPLAY "บ"                          LINE 12 POSITION 80
               DISPLAY "บ"                          LINE 13 POSITION  1
               DISPLAY "บ"                          LINE 13 POSITION 80
               DISPLAY "บ"                          LINE 14 POSITION  1
               DISPLAY "บ"                          LINE 14 POSITION 80
               DISPLAY "บ"                          LINE 15 POSITION  1
               DISPLAY "บ"                          LINE 15 POSITION 80
               DISPLAY WK-RAYA                      LINE 16 POSITION  1
               DISPLAY "ว"                          LINE 16 POSITION  1
               DISPLAY "ถ"                          LINE 16 POSITION 80
               DISPLAY "บ"                          LINE 17 POSITION  1
               DISPLAY "PERIODO DE PAGO    :"       LINE 17 POSITION  3
               DISPLAY "บ"                          LINE 17 POSITION 80
               DISPLAY "FECHA DESDE :"              LINE 17 POSITION 30
               DISPLAY "FECHA PAGO  :"              LINE 17 POSITION 55
               DISPLAY WK-RAYA                      LINE 18 POSITION  1
               DISPLAY " FECHA PROCESO DE PAGO    : "
                                                    LINE 20 POSITION 30
               DISPLAY "ว"                          LINE 18 POSITION  1
               DISPLAY "ถ"                          LINE 18 POSITION 80
               DISPLAY "บ"                          LINE 19 POSITION  1
               DISPLAY "บ"                          LINE 19 POSITION 80
               DISPLAY "บ"                          LINE 20 POSITION  1
               DISPLAY "บ"                          LINE 20 POSITION 80
               DISPLAY WK-RAYA                      LINE 21 POSITION  1
               DISPLAY "ว"                          LINE 21 POSITION  1
               DISPLAY "ถ"                          LINE 21 POSITION 80
               DISPLAY "บ"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY "บ"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.
               PERFORM  I-DISPLAY-LABEL       THRU  F-DISPLAY-LABEL.
       020-DISPLAY-DATOS-PERIODO.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               MOVE W-TPN-FECHA-DESDE      TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE  5 POSITION 23
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
       CONFIRMA-ARCHIVO.
               DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                                    LINE 24 POSITION 1
               DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32
               PERFORM ACEPTA-TRUCO
               MOVE ZEROS TO SW-DAV.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
               GO TO CONFIRMA-ARCHIVO.
               MOVE WIN-PERIODO-ESTADO     TO  WK-REINICIO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
               PERFORM I-PAGINA-REINICIO THRU F-PAGINA-REINICIO
               PERFORM I-ACEPTA-FECHA-DESDE THRU
                       F-ACEPTA-FECHA-DESDE
               PERFORM I-ACEPTA-FECHA-HASTA THRU
                       F-ACEPTA-FECHA-HASTA
               PERFORM I-ACEPTA-FECHA-DAVIVIENDA THRU
                       F-ACEPTA-FECHA-DAVIVIENDA.
       F-PANTALLA.

       I-PAGINA-REINICIO.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-REINICIO            TO  WK-PAGINA
               WI-Z4
               DISPLAY WI-Z4                        LINE 17 POSITION 24
               ACCEPT WK-REINICIO                   LINE 17 POSITION 24
                      CONVERT
                      TAB
                      NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WK-REINICIO                  =  ZEROS
               MOVE WK-PAGINA              TO  WK-REINICIO.
           IF  WK-REINICIO                  =  ZEROS
               ADD 1                       TO  WK-REINICIO.
               MOVE WK-REINICIO            TO  WK-PAGINA
               WI-Z4
               DISPLAY WI-Z4            LINE 17 POSITION 24.
       F-PAGINA-REINICIO.

       I-ACEPTA-FECHA-DESDE.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-FECHA-DESDE         TO  WK-FECHA-INVERTIDA
                                               WI-Z8
               DISPLAY WI-Z8                        LINE 17 POSITION 44
               ACCEPT WK-FECHA-DESDE                LINE 17 POSITION 44
                      CONVERT
                      TAB
                      NO BEEP
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WK-FECHA-DESDE               =  ZEROS
               MOVE WK-FECHA-INVERTIDA     TO  WK-FECHA-DESDE.
           IF  WK-FECHA-DESDE               =  ZEROS
               DISPLAY " FECHA ERRADA "
                                                    LINE 24 POSITION 1
                                                    REVERSE
               GO TO I-ACEPTA-FECHA-DESDE.
           MOVE WK-FECHA-DESDE         TO  WK-FECHA-INVERTIDA
                                               WI-FECHA
           DISPLAY WI-FECHA                     LINE 17 POSITION 44
           IF  CUP
               GO TO I-PAGINA-REINICIO.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-FECHA-DESDE.

       I-ACEPTA-FECHA-HASTA.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-FECHA-HASTA         TO  WK-FECHA-INVERTIDA
                                               WI-Z8
               DISPLAY WI-Z8                        LINE 17 POSITION 69
               ACCEPT WK-FECHA-HASTA                LINE 17 POSITION 69
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  WK-FECHA-DESDE               >  WK-FECHA-HASTA
               DISPLAY " FECHA ERRADA "
                                                    LINE 24 POSITION 1
                                                    REVERSE
               GO TO I-ACEPTA-FECHA-HASTA.
               MOVE WK-FECHA-HASTA         TO  WK-FECHA-INVERTIDA
                                               WI-FECHA
               DISPLAY WI-FECHA                     LINE 17 POSITION 69
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
           IF  CUP
               GO TO I-ACEPTA-FECHA-DESDE.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-FECHA-HASTA.

       I-ACEPTA-FECHA-DAVIVIENDA.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE WK-FECHA-DAVIV         TO  WI-Z8
               DISPLAY WI-Z8                        LINE 20 POSITION 58
               ACCEPT WK-FECHA-DAVIV                LINE 20 POSITION 58
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP
               GO TO I-ACEPTA-FECHA-HASTA.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-FECHA-DAVIVIENDA.

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
           READ TEMPINFO NEXT RECORD AT  END
                MOVE 1 TO FIN-OK GO F-LEE-TEMPINFO.
           IF TIMAE-NETO = ZEROS OR
              TIMAE-NETO < ZEROS
              GO I-LEE-TEMPINFO.
       F-LEE-TEMPINFO.

       I-LEE-CONTRATO.
           READ CONTRATO          INVALID  KEY
               MOVE "97"  TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-LEE-CONTRATO.

       I-LEE-HOJAVIDA.
           READ HOJAVIDA          INVALID  KEY
               MOVE "99"          TO  STATUS-NOMINA
               PERFORM RUTINA-ERROR-NOMINA
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-LEE-HOJAVIDA.

       I-DATOS-GENERICOS.
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
               DISPLAY WK-FECHA-DAVIV LINE 24 POSITION 01.
               DISPLAY A-FECHA-PAGO-0 LINE 24 POSITION 20.
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-0.
               MOVE ZEROS                  TO  A-TALON-1
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-1
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-1A.
               MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-2
                                               A-FECHA-PAGO-2A.
       F-DATOS-GENERICOS.

       I-LEE-CORPO.
           READ NOMICORP INVALID KEY
                MOVE 1 TO FIN-OK.
       F-LEE-CORPO.

       I-SUMA-REG-02.
           PERFORM I-LEE-TEMPINFO THRU F-LEE-TEMPINFO
           IF FIN-OK = 1 GO F-SUMA-REG-02.
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
           MOVE    ZEROS          TO   W-CUENTA
           MOVE    CONTRATO-CUENTA   TO   W-CUENTA
           PERFORM VALIDA-CUENTA THRU SALE-VALIDA-CUENTA
           IF  SW-CUENTA          =  ZEROS
               SUBTRACT TIMAE-NETO  FROM A-VALOR-TOTAL
               SUBTRACT TIMAE-NETO  FROM A-VALOR-DEPOSITO-2
               SUBTRACT TIMAE-NETO  FROM A-VALOR-RETIROS-0
               SUBTRACT 1         FROM A-NUMERO-TOTAL.
       F-SUMA-REG-02.

       I-PROCESO-DAVIV.
           PERFORM I-LEE-TEMPINFO THRU F-LEE-TEMPINFO
           IF FIN-OK = 1 GO F-PROCESO-DAVIV.
           MOVE TIMAE-CEDULA           TO  CONTRATO-CEDULA
           PERFORM I-LEE-CONTRATO THRU F-LEE-CONTRATO
           IF  CONTRATO-SW3 > 6 GO I-PROCESO-DAVIV.
           MOVE CONTRATO-CEDULA      TO  HOJAVIDA-CEDULA
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
           MOVE    ZEROS TO W-CUENTA
           MOVE    CONTRATO-CUENTA  TO  W-CUENTA
           PERFORM VALIDA-CUENTA THRU SALE-VALIDA-CUENTA
           PERFORM I-MUEVE-DATOS THRU F-MUEVE-DATOS.
       F-PROCESO-DAVIV.

       I-MUEVE-DATOS.
           IF  WK-LINEAS  >  55 OR WK-LINEAS = 0
               PERFORM TITULOS-INFORME THRU
                       SALE-TITULOS-INFORME.
           ADD  1                      TO  A-TALON-1
           MOVE A-TALON-1              TO  I-SECUENC
           MOVE CONTRATO-CUENTA           TO  I-CUENTA
      *    MOVE CONTRATO-CODIEMP          TO  I-CODIGO
           MOVE CONTRATO-CEDULA           TO  I-NIT
           MOVE HOJAVIDA-NOMBRE           TO  I-NOMBRE
           MOVE TIMAE-NETO             TO  I-NETO-A-PAGAR
           IF  SW-CUENTA               =  ZEROS
               MOVE "CTA. ERRADA"          TO  I-OBSERVACIONES
               WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
               ADD  1                      TO  WK-LINEAS
               MOVE 1                      TO  SW-DAV
               GO TO F-MUEVE-DATOS.
           MOVE SPACES                 TO  I-OBSERVACIONES
           WRITE REG-INFORMES        FROM  LINEA-DETALLE   BEFORE 1
           ADD  1                      TO  WK-LINEAS
           MOVE CONTRATO-CUENTA           TO  A-CUENTA-1
           MOVE TIMAE-NETO             TO  A-VALOR-DEPOSITO-1
           MOVE A-TALON-1              TO  A-REFERENCIA-1
           MOVE CONTRATO-CEDULA           TO  A-CEDULA
      *    WRITE REG-ARCHIVOS        FROM  REG-A-1.

           ADD  TIMAE-NETO             TO  W-TOTAL-A-PAGAR
           MOVE CONTRATO-CEDULA           TO  WI-Z12
           DISPLAY WI-Z12              LINE 12 POSITION 12
           DISPLAY HOJAVIDA-NOMBRE        LINE 12 POSITION 35
           MOVE W-NETO-A-PAGAR         TO WI-Z8P
           DISPLAY WI-Z8P              LINE 14 POSITION 69.
       F-MUEVE-DATOS.

       I-TOTAL-ARCHIVO.
           MOVE WIN-COD-BANCO    TO CORP-CODIGO
           PERFORM I-LEE-CORPO THRU F-LEE-CORPO
           MOVE CORP-CTA-CTE(WIN-TIP-CTA) TO A-CTA-EMPRESA-2.
           MOVE 1                      TO  A-TALON-2
           MOVE A-TALON-2              TO  A-REFERENCIA-2
           MOVE WK-FECHA-DAVIV         TO  A-FECHA-PAGO-2A
           MOVE W-TEMPORAL             TO  A-NIT-EMPRESA-2A
      *    WRITE REG-ARCHIVOS        FROM  REG-A-2.
           MOVE A-VALOR-TOTAL        TO    I-VALOR-CONSIG
           MOVE W-NETO-A-PAGAR       TO    I-VALOR-TOTAL
           PERFORM I-DISPLAY-DATOS THRU F-DISPLAY-DATOS.
           WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1
           WRITE REG-INFORMES        FROM  TITULO-6        BEFORE 1
           WRITE REG-INFORMES        FROM  TITULO-7        BEFORE 1
           WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1.
      *    MOVE "NOMINA DEL"         TO  C-DATO
      *    ADD     1                 TO  WR-RADIC-CONTABLE
      *    PERFORM I-CONT-DAVIV THRU F-CONT-DAVIV.
      *    ADD     1                 TO  WR-RADIC-CONTABLE
      *    PERFORM I-CONT-TOTAL THRU F-CONT-TOTAL.
       F-TOTAL-ARCHIVO.

       I-FIN-MODE.
               CLOSE CONCPTOS NOMICORP NOMICIUD
                     TABLAEMP CONTRATO
                     HOJAVIDA INFORMES TEMPINFO.
               EXIT PROGRAM.
       120-FT. STOP RUN.
       F-FIN-MODE.

       I-DISPLAY-DATOS.
      *        DISPLAY WK-ESPACIOS                  LINE 12 POSITION  2
      *        DISPLAY WK-ESPACIOS                  LINE 14 POSITION  2
      *        DISPLAY "ARCHIVO      : "            LINE 11 POSITION 25
      *        DISPLAY LABEL-ARCHIVOS-1             LINE 11 POSITION 40
      *        DISPLAY "CUENTA No."                 LINE 12 POSITION 25
      *        DISPLAY A-CTA-EMPRESA-2              LINE 12 POSITION 36
      *        DISPLAY "FECHA PROCESO: "            LINE 13 POSITION 25
      *        MOVE WK-FECHA-HASTA         TO  WI-FECHA
      *        DISPLAY WI-FECHA                     LINE 13 POSITION 40
      *        DISPLAY "TOTAL NOMINA : "            LINE 14 POSITION 25
      *        MOVE A-VALOR-TOTAL          TO  WI-NUMERO
      *        DISPLAY WI-NUMERO                    LINE 14 POSITION 40
      *        DISPLAY "No. REGISTROS: "            LINE 15 POSITION 25
      *        DISPLAY "RELACION:I335"              LINE 15 POSITION 65
      *        MOVE A-NUMERO-TOTAL         TO  WI-NUMERO
      *        DISPLAY WI-NUMERO                    LINE 15 POSITION 40.
      *        IF SW-DAV = 1 DISPLAY "CUENTAS ERRADAS"
      *                             LINE 11 POSITION 63 REVERSE BLINK.
      *        PERFORM ACEPTA-TRUCO.
       F-DISPLAY-DATOS.

       TITULOS-INFORME.
               MOVE LABEL-INFORMES        TO I-LISTADO
           IF  WK-PAGINA                    >  1
               WRITE REG-INFORMES        FROM  TITULO-5     BEFORE PAGE.
               MOVE WK-PAGINA              TO  I-PAGINA
               WRITE REG-INFORMES        FROM  TITULO-0        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-1        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2B       BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2C       BEFORE 1
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-3        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-4        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-5        BEFORE 1.
               ADD  1                      TO  WK-PAGINA
               MOVE 10                     TO  WK-LINEAS.
       SALE-TITULOS-INFORME.
               EXIT.

       I-LEE-NOMICIUD.
           MOVE 0 TO SW-EOF.
           READ NOMICIUD INVALID KEY
             MOVE 1 TO SW-EOF.
       F-LEE-NOMICIUD.
      *
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\CALCDIAS.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\VALIDAVI.PRO".
               COPY "..\PRO\DISLABEL.PRO".
      *
               COPY "..\PLB\TABLAEMP.PLB".
               COPY "..\PLB\CONTRATO.PLB".
               COPY "..\PLB\TEMPINFO.PLB".
               COPY "..\PLB\INFORMES.PLB".
      ******************************************************************

