       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG740.
      ******************************************************************
      * INFORME GENERAL Y DETALLADO DE LAS EMPRESAS ABR-27-2002        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\NOMICIUD.DYC".
           COPY "..\DYC\NOMIBANK.DYC".
           COPY "..\DYC\NOMICORP.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\SEL\REPORTES.SEL".
           COPY "..\SEL\LISTADOS.SEL".
           COPY "..\DYC\CJASUBSI.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CONCPTOS.FD".
               COPY "..\FD\TABLAEMP.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\NOMICORP.FD".
               COPY "..\FD\INFORMES.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\CJASUBSI.FD".
               COPY "..\FD\LISTADOS.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG740".
       77      ID-PRG              PIC X(7)  VALUE "PROG740".
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(4)       OCCURS 25 TIMES.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
               COPY "..\LBL\NOMICORP.LBL".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\LISTADOS.LBL".
               COPY "..\LBL\CJASUBSI.LBL".
      *
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\LISTADOS.WRK".

      *******************************************************************
      *   En esta area se definen todos los titulos del informe general *
      *   de todas las empresas.                                        *
      *******************************************************************
       01      AREA-INFORME.
        03     TITULO-0.
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(8)     VALUE "Fecha : ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(44)    VALUE SPACES.
         05    I-TITULO    PIC X(31)    VALUE
              "INFORME GENERAL DE LAS EMPRESAS".

        03     TITULO-2.
         05    FILLER      PIC X(8)     VALUE "Hora  : ".
         05    I-HORA      PIC X(5).
         05    FILLER      PIC X(137)   VALUE SPACES.
         05    FILLER      PIC X(8)     VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.

        03     LIN-5.
           05  FILLER        PIC  X(10)  VALUE "Impresion ".
           05  I-LABEL       PIC  X(50)  VALUE  SPACES.
           05  FILLER        PIC  X(90)  VALUE  SPACES.
           05  FILLER        PIC  X(09)  VALUE  "Programa:".
           05  FILLER        PIC  X(01)  VALUE  SPACES.
           05  I-PROG-LABEL  PIC  X(08).

        03     TITULO-3.
         05    FILLER      PIC X(230)   VALUE ALL "=".

        03     TITULO-4.
         05    FILLER      PIC X(14)    VALUE "COD.".
         05    FILLER      PIC X(22)    VALUE "NOMBRE DE LA EMPRESA".
         05    FILLER      PIC X(8)     VALUE "FACT".
         05    FILLER      PIC X(9)     VALUE "PORC. ".
         05    FILLER      PIC X(16)    VALUE "PORC. ".
         05    FILLER      PIC X(24)    VALUE "N.I.T.".
         05    FILLER      PIC X(25)    VALUE "DIRECCION".
         05    FILLER      PIC X(14)    VALUE "CIUDAD".
         05    FILLER      PIC X(13)    VALUE "TELEFONO".
         05    FILLER      PIC X(15)    VALUE "PERIODICIDAD".
         05    FILLER      PIC X(17)    VALUE "FORMA DE  ".
         05    FILLER      PIC X(15)    VALUE "BANCO".
         05    FILLER      PIC X(15)    VALUE "No.CUENTA   ".
         05    FILLER      PIC X(6)     VALUE "CODIGO".

        03     TITULO-5.
         05    FILLER      PIC X(45)    VALUE SPACES.
         05    FILLER      PIC X(09)    VALUE "PREST.".
         05    FILLER      PIC X(94)    VALUE "ADMON.".
         05    FILLER      PIC X(13)    VALUE "PAGO".
         05    FILLER      PIC X(44)    VALUE "PAGO".
         05    FILLER      PIC X(8)     VALUE "CONTABLE".

        03     LINEA-DE-DETALLE.
         05    I-COD       PIC ZZZZ.
         05    FILLER      PIC X(3).
         05    I-NOMEMP    PIC X(35).
         05    FILLER      PIC X(02).
         05    I-PORPRE    PIC ZZ.ZZ.
         05    FILLER      PIC X(01).
         05    I-FACTUR    PIC X.
         05    FILLER      PIC X(4).
         05    I-PORADM    PIC ZZ.ZZ.
         05    FILLER      PIC X(6).
         05    I-NIT       PIC ZZZ,ZZZ,ZZZ.
         05    I-LET       PIC X.
         05    I-EXT       PIC 99.
         05    FILLER      PIC X(3).
         05    I-DIREC     PIC X(30).
         05    FILLER      PIC X(3).
         05    I-CCIUDAD   PIC 99.
         05    FILLER      PIC X.
         05    I-CIUDAD    PIC X(8).
         05    FILLER      PIC X(3).
         05    I-TEL       PIC ZZ/ZZ/ZZ/ZZ.
         05    FILLER      PIC X(3).
         05    I-PPAG      PIC X.
         05    FILLER      PIC X.
         05    I-DPPAG     PIC X(10).
         05    FILLER      PIC X(3).
         05    I-FPAG      PIC X.
         05    FILLER      PIC X.
         05    I-DFPAG     PIC X(12).
         05    FILLER      PIC X(3).
         05    I-CODBCO    PIC 99.
         05    FILLER      PIC X.
         05    I-DBCO      PIC X(10).
         05    FILLER      PIC X(3).
         05    I-NRCTA     PIC 9(12).
         05    FILLER      PIC X(1).
         05    I-CODCON    PIC 9(10).
         05    FILLER      PIC X(01).
         05    I-WIN-FECHA-FACT PIC 9(01).

        03     TITULO-TOTALI.
         05    FILLER      PIC X(32)    VALUE "GRAN TOTAL EMPRESAS:".
         05    I-TOTAL     PIC ZZZZZ.
      *******************************************************************
      *  En esta area se definen todos los titulos del reporte          *
      *  Datos Comerciales de todas las empresas.                       *
      *******************************************************************
        03     TITULO-4R.
         05    FILLER      PIC X(15)    VALUE "COD.".
         05    FILLER      PIC X(30)    VALUE "NOMBRE DE LA EMPRESA".
         05    FILLER      PIC X(10)    VALUE "SALARIO".
         05    FILLER      PIC X(09)    VALUE "SUBSID".
         05    FILLER      PIC X(09)    VALUE "PAGO  ".
         05    FILLER      PIC X(15)    VALUE "PERIODICIDAD".
         05    FILLER      PIC X(10)    VALUE "PERIODO".
         05    FILLER      PIC X(10)    VALUE "HORAS  ".
         05    FILLER      PIC X(14)    VALUE "TIPO".
         05    FILLER      PIC X(18)    VALUE "TIPO".
         05    FILLER      PIC X(07)    VALUE "PORC. ".
         05    FILLER      PIC X(07)    VALUE "PORC. ".
         05    FILLER      PIC X(09)    VALUE "INACTIVA".
         05    FILLER      PIC X(12)    VALUE "GENERA".
         05    FILLER      PIC X(16)    VALUE "ORDEN  ".
         05    FILLER      PIC X(07)    VALUE "  CAJA ".
      *
        03     TITULO-5R.
         05    FILLER      PIC X(45)     VALUE SPACES.
         05    FILLER      PIC X(10)     VALUE "MINIMO".
         05    FILLER      PIC X(9)      VALUE "TRANSP".
         05    FILLER      PIC X(9)      VALUE "MINIMO".
         05    FILLER      PIC X(15)     VALUE "DE PAGO".
         05    FILLER      PIC X(10)     VALUE "ACTUAL ".
         05    FILLER      PIC X(10)     VALUE "PERIODO".
         05    FILLER      PIC X(14)     VALUE "LIQUIDACION".
         05    FILLER      PIC X(18)     VALUE "VACACIONES".
         05    FILLER      PIC X(7)      VALUE "PREST.".
         05    FILLER      PIC X(9)      VALUE "ADMON.".
         05    FILLER      PIC X(7)      VALUE "S/N".
         05    FILLER      PIC X(12)     VALUE "FACT. ".
         05    FILLER      PIC X(16)     VALUE "FACTURA".
         05    FILLER      PIC X(7)      VALUE "       ".

        03     LINEA-DETALLE.
         05    R-COD       PIC ZZZZ.
         05    FILLER      PIC X(3).
         05    R-NOMEMP    PIC X(35).
         05    FILLER      PIC X(3).
         05    R-SALMIN    PIC ZZZ,ZZZ.
         05    FILLER      PIC X(3).
         05    R-SUBTRA    PIC ZZ,ZZZ.
         05    FILLER      PIC X(3).
         05    R-PAGMIN    PIC ZZ,ZZZ.
         05    FILLER      PIC X(3).
         05    R-PPAG      PIC X.
         05    FILLER      PIC X.
         05    R-DPPAG     PIC X(9).
         05    FILLER      PIC X(5).
         05    R-PERIOD    PIC ZZZ.
         05    FILLER      PIC X(7).
         05    R-HORPER    PIC ZZZ.
         05    FILLER      PIC X(6).
         05    R-TLIQ      PIC X.
         05    FILLER      PIC X.
         05    R-DTLIQ     PIC X(11).
         05    FILLER      PIC X(3).
         05    R-TVAC      PIC Z.
         05    FILLER      PIC X.
         05    R-DTVAC     PIC X(13).
         05    FILLER      PIC X(2).
         05    R-PORPRE    PIC ZZ.ZZ.
         05    FILLER      PIC X(02).
         05    R-PORADM    PIC ZZ.ZZ.
         05    FILLER      PIC X(04).
         05    R-INACT     PIC X.
         05    FILLER      PIC X(08).
         05    R-GENFAC    PIC X.
         05    FILLER      PIC X(03).
         05    R-ORDFAC    PIC XX.
         05    FILLER      PIC X.
         05    R-DORFAC    PIC X(17).
         05    FILLER      PIC X(01).
         05    R-SUBSIDIO  PIC 99.
         05    FILLER      PIC X(01) VALUE SPACES.
         05    R-NOMBRE-SUBSIDIO PIC X(15).
         05    FILLER      PIC X(01) VALUE SPACES.
         05    R-SUC-SUBSIDIO PIC 9(03).
         05    FILLER      PIC X(01) VALUE SPACES.
         05    R-RIESGO    PIC 9(03).
         05    FILLER      PIC X(01) VALUE SPACES.
         05    R-HORAS     PIC X(01).
      *******************************************************************
      *  En esta area se definen todos los titulos del LISTADO          *
      *  Datos Comerciales de todas las empresas.                       *
      *******************************************************************
        03     TITULO-4L.
         05    FILLER      PIC X(06)    VALUE "  COD.".
         05    FILLER      PIC X(11)    VALUE SPACES.
         05    FILLER      PIC X(43)    VALUE "NOMBRE DE LA EMPRESA".
         05    FILLER      PIC X(30)    VALUE "DIRECCION".
         05    FILLER      PIC X(18)    VALUE "NIT".
         05    FILLER      PIC X(50)    VALUE "REP.LEGAL".
         05    FILLER      PIC X(50)    VALUE "OBJETO DEL CONTRATO".
         05    FILLER      PIC X(30)    VALUE "SUCURSALES".
      *
        03     LINEA-LISTADO.
         05    FILLER      PIC X(2).
         05    L-COD       PIC ZZZZ.
         05    FILLER      PIC X(3).
         05    L-NOMEMP    PIC X(40) VALUE ALL "-".
         05    FILLER      PIC X(3).
         05    L-DIREC     PIC X(30) VALUE ALL "-".
         05    FILLER      PIC X(3).
         05    L-NIT       PIC 999,999,999.
         05    FILLER      PIC X(1) VALUE "-".
         05    L-EXT       PIC 9.
         05    FILLER      PIC X(3).
         05    L-LEGAL     PIC X(40) VALUE ALL "-".
         05    FILLER      PIC X(4).
         05    R-OBJETO    PIC X(50) VALUE ALL "_".
         05    FILLER      PIC X(4).
         05    R-SUCURSAL  PIC X(30) VALUE ALL "_".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\REPORTES.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\NOMICORP.DCL".
               COPY "..\DCL\LISTADOS.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
               COPY "..\DCL\CJASUBSI.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               PERFORM I-LABEL-ARCHIVOS   THRU  F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS    THRU  F-ABRE-ARCHIVOS
               PERFORM I-PANTALLA         THRU  F-PANTALLA
               PERFORM I-OPCION           THRU  F-OPCION
               PERFORM I-DISPLAY-LABEL    THRU  F-DISPLAY-LABEL
               PERFORM I-PROCESO          THRU  F-PROCESO
               PERFORM I-FIN-MODE         THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE WEMPRESA                 TO TIT-CIA
               MOVE    3                     TO IND-INFORMES.
               MOVE    3                     TO IND-LISTADOS.
               MOVE    3                     TO IND-REPORTES.
               PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               PERFORM I-LABEL-REPORTES    THRU F-LABEL-REPORTES.
               PERFORM I-LABEL-LISTADOS    THRU F-LABEL-LISTADOS.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CONCPTOS TABLAEMP NOMIBANK NOMICIUD
                       NOMICORP CJASUBSI.
           OPEN I-O    EMPRESAS.
           OPEN OUTPUT INFORMES  REPORTES LISTADOS.
       F-ABRE-ARCHIVOS.
      *
       I-OPCION.
            MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
            DISPLAY " " LINE 23  POSITION 80
                         CONTROL SG-CONTROL-STRING BEEP REVERSE.
           DISPLAY SPACES       LINE 17 POSITION 05 SIZE 70.
           DISPLAY "[ F6 ] TODAS     "  LINE 17 POSITION 05 BEEP
           DISPLAY "[ F7 ] ACTIVAS   "  LINE 17 POSITION 25 BEEP.
           DISPLAY "[ F8 ] INACTIVAS "  LINE 17 POSITION 50 BEEP.
           PERFORM ACEPTA-TRUCO.
           IF F6            MOVE 1  TO SW5
                            GO F-OPCION.
           IF F7            MOVE 2  TO SW5
                            GO F-OPCION.
           IF F8            MOVE 3  TO SW5
                            GO F-OPCION.
           GO I-OPCION.
       F-OPCION.
      *
       I-FIN-MODE.
           CLOSE CONCPTOS TABLAEMP NOMICIUD NOMIBANK EMPRESAS NOMICORP
                 REPORTES INFORMES CJASUBSI LISTADOS.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-PROCESO.
            MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
            MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
            DISPLAY " " LINE 23  POSITION 80
                         CONTROL SG-CONTROL-STRING BEEP REVERSE.
            DISPLAY SPACES       LINE 17 POSITION 05 SIZE 70.
            DISPLAY "[F4] DATOS LIQUIDACION  "
                            LINE 17 POSITION 03  BEEP
            DISPLAY "[F5] DATOS FACTURACION  "
                            LINE 17 POSITION 30 BEEP
            DISPLAY "[F7] DATOS GENERALES  "
                            LINE 17 POSITION 56 BEEP
            PERFORM ACEPTA-TRUCO.
            IF F2  PERFORM I-FIN-MODE  THRU  F-FIN-MODE
               ELSE
            IF F4
               PERFORM I-START-EMPRESAS THRU F-START-EMPRESAS
               MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA
               PERFORM I-INFORME  THRU  F-INFORME
                          UNTIL FIN-OK = 1
               PERFORM I-TOTAL-INFORME THRU F-TOTAL-INFORME
            ELSE
               IF F5
                  PERFORM I-START-EMPRESAS THRU F-START-EMPRESAS
                  MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA
                  PERFORM I-REPORTE  THRU  F-REPORTE
                                           UNTIL FIN-OK = 1
                  PERFORM I-TOTAL-REPORTE THRU F-TOTAL-REPORTE
               ELSE
                  IF F7
                     PERFORM I-START-EMPRESAS THRU F-START-EMPRESAS
                     MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA
                     PERFORM I-LISTADO        THRU  F-LISTADO
                                                   UNTIL FIN-OK = 1
                     PERFORM I-TOTAL-LISTADO  THRU F-TOTAL-LISTADO
                  ELSE
                     GO TO CONFIRMA-INFORME.
       F-PROCESO.
      *
       I-DISPLAY.
                DISPLAY "EMPRESA:"          LINE 8 POSITION 05
                DISPLAY  WIN-CODIGO-EMPRESA LINE 8 POSITION 15
                DISPLAY  WIN-EMPRESA        LINE 8 POSITION 21.
       F-DISPLAY.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³              RUTINA GENERACION DE INFORMES              ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
      *
       I-INFORME.
                PERFORM I-LEE-EMPRESAS   THRU F-LEE-EMPRESAS.
                PERFORM I-LEE-CIUDAD     THRU F-LEE-CIUDAD.
                IF  WIN-TIPO-PAGO = 1
                   MOVE SPACES            TO I-DBCO
                   MOVE 0                 TO I-NRCTA
                   MOVE WK-EFECTIVO       TO I-CODCON.
                IF WIN-TIPO-PAGO = 2
                   PERFORM I-LEE-BANCOS   THRU F-LEE-BANCOS.
                IF WIN-TIPO-PAGO = 3
                   PERFORM I-LEE-CORPOR   THRU F-LEE-CORPOR.
                IF FIN-OK = 1 GO F-INFORME.
                PERFORM I-MUEVE-INFORME   THRU F-MUEVE-INFORME.
                PERFORM I-DISPLAY         THRU F-DISPLAY.
       F-INFORME.
      *
       I-REPORTE.
                PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
                IF FIN-OK = 1 GO F-REPORTE.
                PERFORM I-MUEVE-REPORTE THRU F-MUEVE-REPORTE.
                PERFORM I-DISPLAY         THRU F-DISPLAY.
       F-REPORTE.
      *
       I-LISTADO.
                PERFORM I-LEE-EMPRESAS  THRU F-LEE-EMPRESAS.
                IF FIN-OK = 1 GO F-LISTADO.
                PERFORM I-MUEVE-LISTADO THRU F-MUEVE-LISTADO.
                PERFORM I-DISPLAY       THRU F-DISPLAY.
       F-LISTADO.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-MOVIMIENTO DE LOS CAMPOS  Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-MUEVE-INFORME.
                MOVE WIN-CODIGO-EMPRESA     TO I-COD
                MOVE WIN-EMPRESA            TO I-NOMEMP
                MOVE WIN-NIT                TO I-NIT
                MOVE WIN-NIT-LETRA          TO I-LET
                MOVE WIN-NIT-EXTENSION      TO I-EXT
                MOVE WIN-DIRECCION          TO I-DIREC
                MOVE WIN-COD-CIUDAD         TO I-CCIUDAD
                MOVE WIN-NUM-TEL            TO I-TEL
                MOVE WIN-INDICATIVO-PERIODO TO I-PPAG
                MOVE WIN-TIPO-PAGO          TO I-FPAG
                MOVE WIN-COD-BANCO          TO I-CODBCO
                MOVE WIN-SUBSIDIO      TO I-WIN-FECHA-FACT
                MOVE WIN-PRC-PRESTACIONAL   TO I-PORPRE
                MOVE WIN-GENERA-FACTURA     TO I-FACTUR
                MOVE WIN-PRC-ADMON          TO I-PORADM
                PERFORM I-DETALLE THRU F-DETALLE
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU I-TITULO-INFORME.
                WRITE REG-INFORMES FROM LINEA-DE-DETALLE AFTER 1
                ADD 1 TO W-CONTA
                ADD 1 TO CON-LIN.
       F-MUEVE-INFORME.
      *
       I-MUEVE-LISTADO.
                MOVE WIN-CODIGO-EMPRESA     TO L-COD
                MOVE WIN-EMPRESA            TO L-NOMEMP
                MOVE WIN-NIT                TO L-NIT
                MOVE WIN-NIT-EXTENSION      TO L-EXT
                MOVE WIN-DIRECCION          TO L-DIREC
                MOVE WIN-RESPONSABLE-1      TO L-LEGAL

                IF CON-LIN > 55
                   PERFORM I-TITULO-LISTADO  THRU I-TITULO-LISTADO.
                WRITE REG-LISTADOS FROM LINEA-LISTADO    AFTER 1
                ADD 1 TO W-CONTA
                ADD 1 TO CON-LIN.
       F-MUEVE-LISTADO.
      *
       I-MUEVE-REPORTE.
                MOVE WIN-CODIGO-EMPRESA     TO R-COD
                MOVE WIN-EMPRESA            TO R-NOMEMP
                MOVE WIN-SALMIN-ACT         TO R-SALMIN
                MOVE WIN-SUBTR-ACT          TO R-SUBTRA
                MOVE WIN-PAGO-MINIMO        TO R-PAGMIN
                MOVE WIN-INDICATIVO-PERIODO TO R-PPAG
                MOVE WIN-NOMINA             TO R-PERIOD
                MOVE WIN-HORAS-PERIODO      TO R-HORPER
                MOVE WIN-TIPO-LIQUIDACION   TO R-TLIQ
                MOVE WIN-PRC-PRESTACIONAL   TO R-PORPRE
                MOVE WIN-PRC-ADMON          TO R-PORADM
                MOVE WIN-CONS-SUBSIDIO      TO R-SUC-SUBSIDIO
                MOVE WIN-RIESGO-PROF        TO R-RIESGO
                MOVE WIN-HORAS-240          TO R-HORAS
                MOVE WIN-INACTIVA-EMP       TO R-INACT
                MOVE WIN-GENERA-FACTURA     TO R-GENFAC
                MOVE WIN-ORDEN              TO R-ORDFAC
                PERFORM I-DETALLE THRU F-DETALLE
                MOVE WIN-SUBSIDIO           TO R-SUBSIDIO COD-CJASUBSI
                PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI.
           IF FIN-ARCH-CJASUBSI = 1
            MOVE "N.N." TO R-NOMBRE-SUBSIDIO
           ELSE
            MOVE NOM-CJASUBSI TO R-NOMBRE-SUBSIDIO
           END-IF
                IF CON-LIN > 55
                   PERFORM I-TITULO-REPORTE THRU F-TITULO-REPORTE.
                WRITE REG-REPORTES FROM LINEA-DETALLE AFTER 1
                ADD 1 TO W-CONTA
                ADD 1 TO CON-LIN.
       F-MUEVE-REPORTE.
      *
       I-DETALLE.
           IF WIN-INDICATIVO-PERIODO = "S"
                  MOVE "SEMANAL"    TO  I-DPPAG  R-DPPAG.
           IF WIN-INDICATIVO-PERIODO = "D"
                  MOVE "DECADAL"    TO  I-DPPAG  R-DPPAG.
           IF WIN-INDICATIVO-PERIODO = "C"
                  MOVE "CATORCENAL" TO  I-DPPAG  R-DPPAG.
           IF WIN-INDICATIVO-PERIODO = "Q"
                  MOVE "QUINCENAL"  TO  I-DPPAG  R-DPPAG.
           IF WIN-INDICATIVO-PERIODO = "M"
                  MOVE "MENSUAL"    TO  I-DPPAG  R-DPPAG.

      *    ****  MOVIMIENTO DE CAMPOS DE LA FORMA DEL PAGO ****

           IF WIN-TIPO-PAGO  = "1" MOVE "EFECTIVO"     TO I-DFPAG.
           IF WIN-TIPO-PAGO  = "2" MOVE "CHEQUE"       TO I-DFPAG.
           IF WIN-TIPO-PAGO  = "3" MOVE "CONSIGNACION" TO I-DFPAG.

      *    ****  MOVIMIENTO DE CAMPOS DE LA FORMA DE LIQUIDACION ****

           IF WIN-TIPO-LIQUIDACION = "C" MOVE "CALENDARIO" TO R-DTLIQ.
           IF WIN-TIPO-LIQUIDACION = "L" MOVE "LABORAL"    TO R-DTLIQ.

      *    ****  MOVIMIENTO DE CAMPOS PARA EL ORDENAMIENTO FACTURAS ****

           IF WIN-ORDEN = "A"
                        MOVE "ALFABETICO"          TO R-DORFAC.
           IF WIN-ORDEN = "C"
                        MOVE "CODIGO"              TO R-DORFAC.
           IF WIN-ORDEN = "CA"
                        MOVE "COSTOS Y ALFABETICO" TO R-DORFAC.
           IF WIN-ORDEN = "CN" OR "KN"
                        MOVE "COSTOS Y NIT O C.C." TO R-DORFAC.
           IF WIN-ORDEN = "CC" OR "KC"
                        MOVE "COSTOS Y CODIGO"     TO R-DORFAC.
       F-DETALLE.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-TITULOS DE LOS INFORMES   Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           IF WK-PAGINA = 1
               WRITE REG-INFORMES  FROM  TITULO-0 AFTER 1
           ELSE
               WRITE REG-INFORMES  FROM  TITULO-0 AFTER PAGE.
           MOVE "INFORME GENERAL DE LAS EMPRESAS" TO I-TITULO.
           WRITE REG-INFORMES  FROM  TITULO-1 AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2 AFTER 1
           MOVE W-PROGRAMA             TO  I-PROG-LABEL.
           MOVE LABEL-INFORMES         TO  I-LABEL.
           WRITE REG-INFORMES  FROM  LIN-5    AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-3 AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-4 AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-5 AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-3 AFTER 1
           MOVE 9                      TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-TITULO-REPORTE.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           MOVE "DATOS COMERCIALES PACTADOS CON LAS EMPRESAS" TO
               I-TITULO.

           IF  WK-PAGINA = 1
               WRITE REG-REPORTES  FROM  TITULO-0  AFTER 1
           ELSE
               WRITE REG-REPORTES  FROM  TITULO-0  AFTER PAGE.
           WRITE REG-REPORTES  FROM  TITULO-1  AFTER 1
           WRITE REG-REPORTES  FROM  TITULO-2  AFTER 1
           MOVE W-PROGRAMA       TO  I-PROG-LABEL.
           MOVE LABEL-REPORTES   TO  I-LABEL.
           WRITE REG-REPORTES  FROM  LIN-5    AFTER 1
           WRITE REG-REPORTES  FROM  TITULO-3  AFTER 1
           WRITE REG-REPORTES  FROM  TITULO-4R AFTER 1
           WRITE REG-REPORTES  FROM  TITULO-5R AFTER 1
           WRITE REG-REPORTES  FROM  TITULO-3  AFTER 1
           MOVE 9                      TO  CON-LIN.
       F-TITULO-REPORTE.
      *
       I-TITULO-LISTADO.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           IF WK-PAGINA = 1
               WRITE REG-LISTADOS  FROM  TITULO-0 AFTER 1
           ELSE
               WRITE REG-LISTADOS  FROM  TITULO-0 AFTER PAGE.
           WRITE REG-LISTADOS  FROM  TITULO-1 AFTER 1
           WRITE REG-LISTADOS  FROM  TITULO-2 AFTER 1
           MOVE W-PROGRAMA       TO  I-PROG-LABEL.
           MOVE LABEL-LISTADOS   TO  I-LABEL.
           WRITE REG-LISTADOS  FROM  LIN-5    AFTER 1
           WRITE REG-LISTADOS  FROM  TITULO-3 AFTER 1
           WRITE REG-LISTADOS  FROM  TITULO-4L AFTER 1
           WRITE REG-LISTADOS  FROM  TITULO-3 AFTER 1
           MOVE 9                      TO  CON-LIN.
       F-TITULO-LISTADO.
      *
      *    ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÝ
      *    Û         I-TOTALES DE LOS INFORMES   Ý
      *    ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÝ
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA TO I-TOTAL
               WRITE REG-INFORMES  FROM TITULO-TOTALI AFTER 2.
       F-TOTAL-INFORME.
      *
       I-TOTAL-REPORTE.
               MOVE W-CONTA TO I-TOTAL
               WRITE REG-REPORTES  FROM TITULO-TOTALI AFTER 2.
       F-TOTAL-REPORTE.
      *
       I-TOTAL-LISTADO.
               MOVE W-CONTA TO I-TOTAL
               WRITE REG-LISTADOS  FROM TITULO-TOTALI AFTER 2.
       F-TOTAL-LISTADO.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       LECTURA Y MANIPULACION DE ARCHIVOS                ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
      *
       I-START-EMPRESAS.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE SPACES                 TO  WIN-CLAVE
               START EMPRESAS    KEY NOT LESS  WIN-CLAVE.
       F-START-EMPRESAS.
      *
       I-LEE-EMPRESAS.
               READ EMPRESAS NEXT WITH NO LOCK
                    AT END MOVE 1 TO FIN-OK
                    GO F-LEE-EMPRESAS.
      ***
      * ESTAS RUTINAS PUEDEN UTILIZARSE PARA ACTUALIZAR PARAMETROS
      * EN EL ARCHIVO DE EMPRESAS
      *           PERFORM I-ACTUALIZE-DPTO THRU F-ACTUALIZE-DPTO
               IF  SW5 = 1  GO F-LEE-EMPRESAS.
               IF  SW5 = 2  AND  WIN-INACTIVA-EMP = "N"
      *JJT         MOVE   ZEROS        TO  WIN-PERIODO-ESTADO
      *JJT         MOVE    "C"         TO  WIN-FLAG-CIERRE
      *JJT         REWRITE REC-EMPRESAS
                   GO F-LEE-EMPRESAS.
               IF  SW5 = 3  AND  WIN-INACTIVA-EMP = "S"
                   GO F-LEE-EMPRESAS.
               GO I-LEE-EMPRESAS.
       F-LEE-EMPRESAS.
      *
       I-LEE-CIUDAD.
               MOVE WIN-COD-CIUDAD TO CIUD-CODIGO
               READ NOMICIUD WITH NO LOCK INVALID KEY
                    MOVE SPACES TO CIUD-NOMBRE.
               MOVE CIUD-NOMBRE TO I-CIUDAD.
       F-LEE-CIUDAD.
      *
       I-LEE-BANCOS.
               MOVE WIN-COD-BANCO TO BANC-CODIGO
               READ NOMIBANK WITH NO LOCK INVALID KEY
                    MOVE SPACES TO BANC-NOMBRE.
               MOVE BANC-NOMBRE TO I-DBCO.
               MOVE BANC-CTA-CTE(WIN-TIP-CTA)      TO I-NRCTA
               MOVE BANC-CTA-CONTABLE(WIN-TIP-CTA) TO I-CODCON.
       F-LEE-BANCOS.

       I-LEE-CORPOR.
               MOVE WIN-COD-BANCO TO CORP-CODIGO
               READ NOMICORP WITH NO LOCK INVALID KEY
                    MOVE SPACES TO BANC-NOMBRE.
               MOVE CORP-NOMBRE TO I-DBCO.
               MOVE CORP-CTA-CTE(WIN-TIP-CTA)      TO I-NRCTA
               MOVE CORP-CTA-CONTABLE(WIN-TIP-CTA) TO I-CODCON.
       F-LEE-CORPOR.
      *
       I-LEE-CJASUBSI.
           MOVE ZEROS TO FIN-ARCH-CJASUBSI.
           READ CJASUBSI WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-CJASUBSI.
       F-LEE-CJASUBSI. EXIT.
      *
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²³       PANTALLAS DEL PROGRAMA                            ³²²
      *  ²² ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ²²
      *  ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
      *
       I-PANTALLA.
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "º"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "LISTADO EMPRESAS "       LINE  2 POSITION 25
               DISPLAY "º"                          LINE  2 POSITION 80
               DISPLAY "º FECHA:"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY "HORA:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "º"                          LINE  3 POSITION 80
               DISPLAY WK-RAYAS                     LINE  4 POSITION  1
               DISPLAY "º"                          LINE  4 POSITION 80
               DISPLAY "º"                          LINE  5 POSITION  1
               DISPLAY "º"                          LINE  5 POSITION 80
               DISPLAY "º"                          LINE  6 POSITION  1
               DISPLAY "º"                          LINE  6 POSITION 80
               DISPLAY "º"                          LINE  7 POSITION  1
               DISPLAY "º"                          LINE  7 POSITION 80
               DISPLAY "º"                          LINE  8 POSITION  1
               DISPLAY "º"                          LINE  8 POSITION 80
               DISPLAY "º"                          LINE  9 POSITION  1
               DISPLAY "º"                          LINE  9 POSITION 80
               DISPLAY "º"                          LINE 10 POSITION  1
               DISPLAY "º"                          LINE 10 POSITION 80
               DISPLAY "º"                          LINE 11 POSITION  1
               DISPLAY "º"                          LINE 11 POSITION 80
               DISPLAY "º"                          LINE 12 POSITION  1
               DISPLAY "º"                          LINE 12 POSITION 80
               DISPLAY "º"                          LINE 13 POSITION  1
               DISPLAY "º"                          LINE 13 POSITION 80
               DISPLAY "º"                          LINE 14 POSITION  1
               DISPLAY "º"                          LINE 14 POSITION 80
               DISPLAY "º"                          LINE 15 POSITION  1
               DISPLAY "º"                          LINE 15 POSITION 80
               DISPLAY WK-RAYA                      LINE 16 POSITION  1
               DISPLAY "Ç"                          LINE 16 POSITION  1
               DISPLAY "¶"                          LINE 16 POSITION 80
               DISPLAY "º"                          LINE 17 POSITION  1
               DISPLAY "º"                          LINE 17 POSITION 80
               DISPLAY WK-RAYA                      LINE 18 POSITION  1
               DISPLAY "Ç"                          LINE 18 POSITION  1
               DISPLAY "¶"                          LINE 18 POSITION 80
               DISPLAY "º"                          LINE 19 POSITION  1
               DISPLAY "º"                          LINE 19 POSITION 80
               DISPLAY "º"                          LINE 20 POSITION  1
               DISPLAY "º"                          LINE 20 POSITION 80
               DISPLAY WK-RAYA                      LINE 21 POSITION  1
               DISPLAY "Ç"                          LINE 21 POSITION  1
               DISPLAY "¶"                          LINE 21 POSITION 80
               DISPLAY "º"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY "º"                          LINE 22 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 23 POSITION  1.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " " LINE 23  POSITION 80
                            CONTROL SG-CONTROL-STRING BEEP REVERSE.
               DISPLAY "LISTADO DE EMPRESAS "
                          LINE 13 POSITION 25.
       CONFIRMA-INFORME.
               DISPLAY " ELIJA SU OPCION               "
                                                    LINE 24 POSITION 1

               DISPLAY " [F2]  TERMINAR "           LINE 24 POSITION 32.
       F-PANTALLA.
      *
       I-DISPLAY-LABEL.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 20 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 19 POSITION 2 SIZE 78
               DISPLAY  SPACES LINE 20 POSITION 2 SIZE 78
               DISPLAY  SPACES LINE 21 POSITION 2 SIZE 78
               "UBICACION GENERALES ------->" LINE 19 POSITION 05
                LABEL-LISTADOS                LINE 19 POSITION 33
                NOM-PAPEL(IND-LISTADOS)       LINE 19 POSITION 66

               "UBICACION LIQUIDACION ----->" LINE 20 POSITION 05
                LABEL-INFORMES                LINE 20 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 20 POSITION 66

               "UBICACION FACTURACION ----->" LINE 21 POSITION 05
                LABEL-REPORTES                LINE 21 POSITION 33
                NOM-PAPEL(IND-REPORTES)       LINE 21 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY-LABEL.
      *
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\INFORMES.PLB".
           COPY "..\PLB\LISTADOS.PLB".
           COPY "..\PLB\REPORTES.PLB".
      ******************************************************************

