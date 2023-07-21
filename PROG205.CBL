       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG205.
      ******************************************************************
      *  CARACTERIZACION NOVEDADES DE  NOMINA      ABR-27-2002         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
           COPY "..\DYC\CONTRATO.DYC".
           COPY "..\DYC\NOMINOVE.DYC".
           COPY "..\DYC\NOVEFIJA.DYC".
           COPY "..\DYC\PRESTAMO.DYC".
           COPY "..\DYC\TEMPACUM.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\SEL\INFORMES.SEL".
           COPY "..\DYC\AUDINOCA.DYC".
           COPY "..\DYC\RETIRACT.DYC".
           COPY "..\DYC\CTRLSUEL.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\HOJAVIDA.FD".
           COPY "..\FD\CONTRATO.FD".
           COPY "..\FD\NOMINOVE.FD".
           COPY "..\FD\NOVEFIJA.FD".
           COPY "..\FD\PRESTAMO.FD".
           COPY "..\FD\TEMPACUM.FD".
           COPY "..\FD\INFORMES.FD".
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\AUDINOCA.FD".
           COPY "..\FD\RETIRACT.FD".
           COPY "..\FD\CTRLSUEL.FD".
       WORKING-STORAGE SECTION.
       01  ID-PRG            PIC X(7)    VALUE "PROG205".
       01  W-PROGRAMA        PIC X(7)    VALUE "PROG205".
      *
       01  WK-FLAG-ORIGEN    PIC X       VALUE "G".
       01  W-FECHA-INICIAL.
           03 W-FECHA-INICIAL-ANO   PIC 9(4).
           03 W-FECHA-INICIAL-MES   PIC 9(2).
           03 W-FECHA-INICIAL-DIA   PIC 9(2).
       01  W-FECHA-FINAL.
           03 W-FECHA-FINAL-ANO   PIC 9(4).
           03 W-FECHA-FINAL-MES   PIC 9(2).
           03 W-FECHA-FINAL-DIA   PIC 9(2).
       01  W-NOMEMP-DE3      PIC X(03)   VALUE SPACES.
      *
       01  CODIGO-PANTALLAS.
        03  CODIGO-PANTALLA  PIC 9(4)    OCCURS 25 TIMES.
      *
       01  FECHAS-VARIAS.
        03  WK-F-INI.
            05 F-ANO-INI        PIC 9(04) VALUE ZEROS.
            05 F-MES-INI        PIC 9(02) VALUE ZEROS.
            05 F-DIA-INI        PIC 9(02) VALUE ZEROS.
        03  WK-F-FIN.
            05 F-ANO-FIN        PIC 9(04) VALUE ZEROS.
            05 F-MES-FIN        PIC 9(02) VALUE ZEROS.
            05 F-DIA-FIN        PIC 9(02) VALUE ZEROS.
      *
        03  WK-F-VARIOS.
            05  KK              PIC 9    VALUE ZEROS.
            05  MES-KK0         PIC 99   VALUE ZEROS.
            05  MES-KK1         PIC 99   VALUE ZEROS.
            05  ANO-KK0         PIC 9(04)  VALUE ZEROS.
            05  ANO-KK1         PIC 9(04)   VALUE ZEROS.
      *
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\CONTRATO.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\LBL\HOJAVIDA.LBL".
           COPY "..\LBL\NOMINOVE.LBL".
           COPY "..\LBL\NOVEFIJA.LBL".
           COPY "..\LBL\PRESTAMO.LBL".
           COPY "..\LBL\TEMPACUM.LBL".
           COPY "..\LBL\INFORMES.LBL".
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\AUDINOCA.LBL".
           COPY "..\LBL\RETIRACT.LBL".
           COPY "..\LBL\CTRLSUEL.LBL".
      *
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\CONCPTOS.WRK".
           COPY "..\WRK\CONTRATO.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TABLAGEN.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
           COPY "..\WRK\LISTADOS.WRK".
           COPY "..\WRK\INFOR100.WRK".
      *
       01      AREA-INFORMES.
        03     AREA-WORKING-INFORMES.
         05    W-VALOR-FONDO             PIC 9(12)      VALUE ZEROS.
         05    W-TOTAL-APORTES           PIC 9(12)      VALUE ZEROS.
         05    W-TOTAL-ORDINARIOS        PIC 9(12)      VALUE ZEROS.
         05    W-TOTAL-EXTRA-ORDINARIOS  PIC 9(12)      VALUE ZEROS.
         05    W-TOTAL-OTROS             PIC 9(12)      VALUE ZEROS.
         05    W-TOTAL-TOTAL             PIC 9(12)      VALUE ZEROS.
        03     TITULO-0.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 6962.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 3602.
         05    I-EMPRESA                 PIC X(40)      VALUE SPACES.

        03     TITULO-1.
      *  05    FILLER                    PIC 9(4)       USAGE IS COMP-1
      *                                                 VALUE 4626.
         05    FILLER                    PIC X(10)      VALUE
              "EMPRESA:".
         05    I-CODIGO-EMPRESA          PIC 999        VALUE ZEROS.
         05    FILLER                    PIC X(21)      VALUE SPACES.
         05    FILLER                    PIC X(29)      VALUE
              " RELACION DESCUENTOS 'FEDAC' ".
         05    FILLER                    PIC X(21)      VALUE SPACES.
         05    FILLER                    PIC X(8)       VALUE
              "PAGINA:".
         05    I-PAGINA                  PIC ZZZZ       VALUE ZEROS.
        03     TITULO-2.
         05    FILLER                    PIC 9(4)       USAGE IS COMP-1
                                                        VALUE 4626.
         05    FILLER                    PIC X(10)      VALUE
              "PERIODO:".
         05    I-PERIODO                 PIC ZZZ        VALUE ZEROS.
         05    FILLER                    PIC X(17)      VALUE SPACES.
         05    FILLER                    PIC X(11)      VALUE
              "NOMINA DEL".
         05    I-FECHA-DESDE             PIC X(11)      VALUE ZEROS.
         05    FILLER                    PIC X(4)       VALUE
              " AL ".
         05    I-FECHA-HASTA             PIC X(11)      VALUE ZEROS.
        03     TITULO-3.
         05    FILLER                    PIC X(14)      VALUE
              "    N.I.T.".
         05    FILLER                    PIC X(31)      VALUE
              "APELLIDOS Y NOMBRES".
         05    FILLER                    PIC X(10)      VALUE
              " APORTES".
         05    FILLER                    PIC X(10)      VALUE
              "ORDINARIO".
         05    FILLER                    PIC X(10)      VALUE
              "EXTRAORD.".
         05    FILLER                    PIC X(10)      VALUE
              "  OTROS".
         05    FILLER                    PIC X(10)      VALUE
              "  TOTAL".
        03     TITULO-4.
         05    FILLER                    PIC X(96)      VALUE ALL "=".
        03     LINEA-FONDO.
         05    I-CEDULA                  PIC Z(12).
         05    FILLER                    PIC XX.
         05    I-NOMBRE-EMPLEADO         PIC X(30).
         05    FILLER                    PIC X.
         05    I-APORTES                 PIC ZZ,ZZZ.ZZ-.
         05    I-ORDINARIOS              PIC ZZ,ZZZ.ZZ-.
         05    I-EXTRA-ORDINARIOS        PIC ZZ,ZZZ.ZZ-.
         05    I-OTROS                   PIC ZZ,ZZZ.ZZ-.
         05    I-TOTAL                   PIC ZZZZ,ZZZ.ZZ.
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TABLAEMP.DCL".
           COPY "..\DCL\CONCPTOS.DCL".
           COPY "..\DCL\HOJAVIDA.DCL".
           COPY "..\DCL\CONTRATO.DCL".
           COPY "..\DCL\NOMINOVE.DCL".
           COPY "..\DCL\NOVEFIJA.DCL".
           COPY "..\DCL\PRESTAMO.DCL".
           COPY "..\DCL\TEMPACUM.DCL".
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\INFORMES.DCL".
           COPY "..\DCL\AUDINOCA.DCL".
           COPY "..\DCL\RETIRACT.DCL".
           COPY "..\DCL\CTRLSUEL.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
           COPY "..\PRO\CONTROLA.PRO".
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER   =   1   EXIT PROGRAM.
           PERFORM I-REVISA-USUAPERS-ACCE THRU F-REVISA-USUAPERS-ACCE.
           PERFORM I-PROCESO-AUDITE       THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS       THRU F-LABEL-ARCHIVOS.
           PERFORM I-ABRE-ARCHIVOS        THRU F-ABRE-ARCHIVOS.

           PERFORM  I-CTRLSUEL  THRU  F-CTRLSUEL.

           PERFORM VALIDA-TABLA-PERIODOS THRU SALE-VALIDA-TABLA-PERIODOS
           PERFORM BORRA-TABLANOV    THRU SALE-BORRA-TABLANOV
           PERFORM CARGA-TABLANOV    THRU SALE-CARGA-TABLANOV
           COMPUTE W-TAMANO-TABLANOV    =  I  -  1.
           IF  W-TAMANO-TABLANOV            >  ZEROS
               NEXT SENTENCE
           ELSE
               MOVE "01"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
           PERFORM I-CEROS-NOMINOVE THRU F-CEROS-NOMINOVE.
           PERFORM I-PANTALLA       THRU F-PANTALLA.

       025-OPCION.
           IF  SW-3                         =  ZEROS
               NEXT SENTENCE
           ELSE
               GO TO 030-ACEPTA-FECHA-DESDE.
           PERFORM I-DISPLAY-REPROCESO THRU F-DISPLAY-REPROCESO.
           DISPLAY " PERIODO YA CARACTERIZADO " LINE 24 POSITION  1
                                                    REVERSE
           DISPLAY "PRESIONE <R> PARA REPROCESAR LAS NOVEDADES"
                                                    LINE 24 POSITION 29
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           " <F8> Ver Periodos. " LINE 25 POSITION 01 REVERSE
           " <F2> Salir "         LINE 25 POSITION 23 REVERSE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT W-OPCION                      LINE 24 POSITION 73
                  NO BEEP ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF  CUP
               GO TO 025-OPCION.
           IF  F2
               GO TO 120-FIN.
           IF F8
            CLOSE TABLAEMP
            PERFORM I-MUESTRA-TABLAEMP-011 THRU F-MUESTRA-TABLAEMP-011
            OPEN I-O TABLAEMP
           PERFORM VALIDA-TABLA-PERIODOS THRU SALE-VALIDA-TABLA-PERIODOS
            PERFORM I-PANTALLA       THRU F-PANTALLA
            MOVE SPACES TO W-OPCION.
           IF  W-OPCION                     =  "R"
               NEXT SENTENCE
           ELSE
               GO TO 025-OPCION.

       025-BORRA-NOMINOVE.
           PERFORM I-PANTALLA       THRU F-PANTALLA.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
           DISPLAY "POR FAVOR ESPERE ..."       LINE 24 POSITION  1
                         BLINK REVERSE
           MOVE WIN-CODIGO-EMPRESA     TO  NOV-EMPRESA
           MOVE ZEROS                  TO  NOV-CODIEMP
           MOVE ZEROS                  TO  NOV-CONCEPTO
           MOVE ZEROS                  TO  I-ERROR
           START NOMINOVE KEY           >  NOV-CLAVE
                 INVALID  KEY MOVE 1 TO  I-ERROR.
           IF I-ERROR   NOT =  ZEROS
              IF  NOMINOVE-STATUS              =  "23"
               GO TO 025-SALE-BORRA-NOMINOVE
           ELSE
               PERFORM RUTINA-ERROR-NOMINOVE.

       025-SIGUE-BORRA-NOMINOVE.
           READ NOMINOVE NEXT RECORD WITH NO LOCK AT  END
               GO TO 025-SALE-BORRA-NOMINOVE.
           IF  NOV-EMPRESA    >  WIN-CODIGO-EMPRESA
               GO TO 025-SALE-BORRA-NOMINOVE.
           IF  NOV-FLAG-ORIGEN NOT =  "G"
               GO TO 025-SIGUE-BORRA-NOMINOVE.

           DELETE NOMINOVE RECORD
               GO TO 025-SIGUE-BORRA-NOMINOVE.

       025-SALE-BORRA-NOMINOVE.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.

       030-ACEPTA-FECHA-DESDE.
           IF WIN-FLAG-CIERRE    = "C" OR
              WIN-FLAG-CIERRE    = "I"
                  NEXT SENTENCE
           ELSE
               PERFORM I-FLAG-CIERRE   THRU F-FLAG-CIERRE
               GO  TO 120-FIN.

           IF TE011-FLAG-PERIODO = "L"
            PERFORM I-PERIODO-DIFERENTE THRU F-PERIODO-DIFERENTE
            GO TO 120-FIN.
               MOVE "G"                    TO  W-TPN-FLAG-PERIODO
           IF  W-TPN-NUMERO-DIAS            =  ZEROS
               MOVE W-DIAS-PERIODO         TO  W-TPN-NUMERO-DIAS
                                               TE011-NUMERO-DIAS.
           IF  W-TPN-NUMERO-HORAS           =  ZEROS
               MOVE W-HORAS-PERIODO        TO  W-TPN-NUMERO-HORAS
                                               TE011-NUMERO-HORAS.
               DISPLAY "Fecha inicial en formato AAAA/MM/DD"
                                LINE 24 POSITION  1 REVERSE
               DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
               " <F8> Ver Periodos. " LINE 25 POSITION 01 REVERSE
               " <F2> Salir "         LINE 25 POSITION 23 REVERSE.
               MOVE ZERO                   TO  WI-FECHA
           DISPLAY WI-FECHA            LINE  5 POSITION 18
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT W-TPN-FECHA-DESDE    LINE  5 POSITION 18
                                           UPDATE
      *                                    CONVERT
                                           TAB NO BEEP
                                           ON EXCEPTION WK-EX
                                           PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
               SPACES SIZE 80 LINE 25 POSITION 01
           IF  CUP GO TO 030-ACEPTA-FECHA-DESDE.
           IF  F2  GO TO 120-FIN.
           IF  F8
            CLOSE TABLAEMP
            PERFORM I-MUESTRA-TABLAEMP-011 THRU F-MUESTRA-TABLAEMP-011
            OPEN I-O TABLAEMP
            PERFORM I-PANTALLA       THRU F-PANTALLA
            GO TO 030-ACEPTA-FECHA-DESDE.
           IF  W-TPN-FECHA-DESDE       =  ZEROS
               MOVE TE011-FECHA-DESDE  TO  W-TPN-FECHA-DESDE.
      ****** CONVERTIR FECHA-DESDE A MASCARA-FECHA
           MOVE W-TPN-FECHA-DESDE      TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 05 POSITION 18.
      ******

           MOVE W-TPN-FECHA-DESDE      TO  TE011-FECHA-DESDE
                                           WK-FECHA-STANDARD
                                           W-FECHA-DESDE
           MOVE  ZEROS  TO KK.
      *   PARA ACEPTAR FECHA DE NOMINA DE DICIEMBRE EN ENERO
      *    IF WK-MES = 1
      *       IF  WK-FECHA-STANDARD-ANO    NOT =  CON-ANO
      *           MOVE 1                   TO  KK
      *           ADD  1                   TO  WK-FECHA-STANDARD-ANO
      *       IF  WK-FECHA-STANDARD-ANO    NOT =  CON-ANO
      *           DISPLAY "ANO ERRADO"     LINE 24 POSITION  1
      *           GO TO 030-ACEPTA-FECHA-DESDE.

      *    IF  CON-PERIODO                  >  1
      *    IF  WK-MES                       >  1
      *    IF  WK-FECHA-STANDARD-ANO    NOT =  CON-ANO
      *        DISPLAY "ANO ERRADO"     LINE 24 POSITION  1
      *        GO TO 030-ACEPTA-FECHA-DESDE.
      *
      *    IF  KK =  1    AND
      *        WK-FECHA-STANDARD-MES        <  12
      *        DISPLAY "MES ERRADO"   LINE 24 POSITION  1
      *        GO TO 030-ACEPTA-FECHA-DESDE.
      *
           PERFORM I-VERIFICAR-FECHA  THRU F-VERIFICAR-FECHA.
           IF WK-FECHA-STANDARD < WK-F-INI  OR
              WK-FECHA-STANDARD > WK-F-FIN
              GO  030-ACEPTA-FECHA-DESDE.

           IF  WK-FECHA-STANDARD-MES        <  1
           OR  WK-FECHA-STANDARD-MES        >  12
               DISPLAY "MES ERRADO"   LINE 24 POSITION  1
               GO TO 030-ACEPTA-FECHA-DESDE.


           IF  WK-FECHA-STANDARD-DIA        <  1
           OR  WK-FECHA-STANDARD-DIA        >  WK-DIAS-MES
                                               (WK-FECHA-STANDARD-MES)
               DISPLAY "DIA ERRADO"        LINE 24 POSITION  1
               GO TO 030-ACEPTA-FECHA-DESDE.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1.

       030-ACEPTA-FECHA-HASTA.
               DISPLAY "Fecha Final en formato AAAA/MM/DD"
                                   LINE 24 POSITION  1 REVERSE
               MOVE ZEROS                  TO  WI-FECHA
               DISPLAY WI-FECHA            LINE  6 POSITION 18
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-TPN-FECHA-HASTA    LINE  6 POSITION 18
                                           UPDATE
      *                                    CONVERT
                                           TAB NO BEEP
                                           ON EXCEPTION WK-EX
                                           PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  CUP
               GO TO 030-ACEPTA-FECHA-DESDE.
           IF  F2
               GO TO 120-FIN.
           IF  W-TPN-FECHA-HASTA            =  ZEROS
               MOVE TE011-FECHA-HASTA      TO  W-TPN-FECHA-HASTA.
      ****** CONVERTIR FECHA-HASTA A MASCARA-FECHA
           MOVE W-TPN-FECHA-HASTA         TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 06 POSITION 18.
      ******

           MOVE W-TPN-FECHA-HASTA      TO  TE011-FECHA-HASTA
                                           WK-FECHA-STANDARD
           MOVE  ZEROS  TO KK.
           PERFORM I-VERIFICAR-FECHA  THRU F-VERIFICAR-FECHA.
           IF WK-FECHA-STANDARD < WK-F-INI  OR
              WK-FECHA-STANDARD > WK-F-FIN
              GO  030-ACEPTA-FECHA-HASTA.

           IF  WK-FECHA-STANDARD-MES        <  1
           OR  WK-FECHA-STANDARD-MES        >  12
               DISPLAY "MES ERRADO"                 LINE 24 POSITION  1

               GO TO 030-ACEPTA-FECHA-HASTA.
           IF  WK-FECHA-STANDARD-DIA        <  1
           OR  WK-FECHA-STANDARD-DIA        >  WK-DIAS-MES
                                               (WK-FECHA-STANDARD-MES)
               DISPLAY "DIA ERRADO"                 LINE 24 POSITION  1

               GO TO 030-ACEPTA-FECHA-HASTA.
           IF  W-TPN-FECHA-HASTA            <  W-TPN-FECHA-DESDE
               DISPLAY "FECHA FINAL DEBE SER MAYOR QUE FECHA INICIAL"
                                LINE 24 POSITION  1 REVERSE

               GO TO 030-ACEPTA-FECHA-HASTA.
               DISPLAY WK-ESPACIOS   LINE 24 POSITION  1.
       030-ACEPTA-DIAS.
               DISPLAY "TECLEE EL NUMERO DE DIAS A PAGAR"  REVERSE
                                   LINE 24 POSITION  1
               MOVE W-TPN-NUMERO-DIAS      TO  WI-Z2
               DISPLAY WI-Z2               LINE  5 POSITION 45
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-TPN-NUMERO-DIAS    LINE  5 POSITION 45
                      UPDATE CONVERT  TAB  NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               GO TO 120-FIN.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  CUP
               GO TO 030-ACEPTA-FECHA-HASTA.
           IF  ERR
               MOVE ZEROS                  TO  WK-EX
               DISPLAY " DIAS  ERRADOS "   LINE 24 POSITION  1

               GO TO 030-ACEPTA-DIAS.
           IF  W-TPN-NUMERO-DIAS            =  ZEROS
               MOVE TE011-NUMERO-DIAS      TO  W-TPN-NUMERO-DIAS.
               MOVE W-TPN-NUMERO-DIAS      TO  TE011-NUMERO-DIAS
               MOVE W-TPN-NUMERO-DIAS      TO  WI-Z2
               DISPLAY WI-Z2 LINE  5 POSITION 45
           IF  W-TPN-NUMERO-DIAS            =  ZEROS
               DISPLAY " DIAS  ERRADOS "            LINE 24 POSITION  1
               GO TO 030-ACEPTA-DIAS.
           DISPLAY WK-ESPACIOS LINE 24 POSITION  1.
       030-ACEPTA-HORAS.
               DISPLAY "TECLEE EL NUMERO DE HORAS A PAGAR"
                        LINE 24 POSITION  1 REVERSE
               MOVE W-TPN-NUMERO-HORAS     TO  WI-Z3
               DISPLAY WI-Z3               LINE  6 POSITION 45
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-TPN-NUMERO-HORAS   LINE  6 POSITION 45
                      UPDATE  CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               GO TO 120-FIN.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  CUP
               GO TO 030-ACEPTA-DIAS.
           IF  ERR
               MOVE ZEROS                  TO  WK-EX
               DISPLAY " HORAS ERRADAS "   LINE 24 POSITION  1

               GO TO 030-ACEPTA-HORAS.
           IF  W-TPN-NUMERO-HORAS           =  ZEROS
               MOVE TE011-NUMERO-HORAS     TO  W-TPN-NUMERO-HORAS.
               MOVE W-TPN-NUMERO-HORAS     TO  TE011-NUMERO-HORAS
               MOVE W-TPN-NUMERO-HORAS     TO  WI-Z3
               DISPLAY WI-Z3  LINE  6 POSITION 45
           IF  W-TPN-NUMERO-HORAS           =  ZEROS
               DISPLAY " HORAS ERRADAS "  LINE 24 POSITION  1
               GO TO 030-ACEPTA-HORAS.
           DISPLAY WK-ESPACIOS LINE 24 POSITION  1.
      ***************************************************************
      *             ACEPTA W-TPN-SEMANAS-RFTE
      ***************************************************************
       030-ACEPTA-SEMANAS-RFTE.
           MOVE 120  TO W-TPN-SEMANAS-RFTE.
       030-ACEPTA-FECHA-PAGO.
           DISPLAY "Fecha de pago en formato AAAA/MM/DD"
                                   LINE 24 POSITION  1 REVERSE
           MOVE ZEROS         TO  WI-FECHA
           DISPLAY WI-FECHA   LINE  7 POSITION 18
           MOVE ZEROS TO  WK-EXCEPTION
           ACCEPT W-TPN-FECHA-PAGO    LINE  7 POSITION 18
                          UPDATE
      *                   CONVERT
                          TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS LINE 24 POSITION  1
           IF  CUP
               GO TO 030-ACEPTA-SEMANAS-RFTE.
           IF  F2
               GO TO 120-FIN.
           IF  W-TPN-FECHA-PAGO   < WK-FECHA-HOY
               DISPLAY "FECHA DE PAGO NO PUEDE SER MENOR A FECHA HOY"
               LINE 24 POSITION 1  BEEP
               GO 030-ACEPTA-FECHA-PAGO.
           IF  W-TPN-FECHA-PAGO    =  ZEROS
               MOVE TE011-FECHA-PAGO       TO  W-TPN-FECHA-PAGO.
      ****** CONVERTIR FECHA-PAGO  A MASCARA-FECHA
           MOVE W-TPN-FECHA-PAGO          TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA    THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA   LINE 07 POSITION 18.
      ******
           MOVE W-TPN-FECHA-PAGO       TO  TE011-FECHA-PAGO
                                           WK-FECHA-STANDARD

           PERFORM I-VERIFICAR-FECHA  THRU F-VERIFICAR-FECHA.
           IF WK-FECHA-STANDARD < WK-F-INI  OR
              WK-FECHA-STANDARD > WK-F-FIN
              GO  030-ACEPTA-FECHA-PAGO.

           IF  WK-FECHA-STANDARD-MES        <  1
           OR  WK-FECHA-STANDARD-MES        >  12
               DISPLAY "MES ERRADO"         LINE 24 POSITION  1

               GO TO 030-ACEPTA-FECHA-PAGO.
           IF  WK-FECHA-STANDARD-DIA        <  1
           OR  WK-FECHA-STANDARD-DIA        >  WK-DIAS-MES
                                            (WK-FECHA-STANDARD-MES)
               DISPLAY "DIA ERRADO"         LINE 24 POSITION  1

               GO TO 030-ACEPTA-FECHA-PAGO.
           IF  W-TPN-FECHA-PAGO             <  W-TPN-FECHA-DESDE
               DISPLAY "FECHA DE PAGO DEBE SER MAYOR QUE FECHA INICIAL"
                             LINE 24 POSITION  1 REVERSE
               GO TO 030-ACEPTA-FECHA-PAGO.
           DISPLAY WK-ESPACIOS         LINE 24 POSITION  1.
       030-ARMA-TABLA-PERIODO-NOMINA.
               MOVE 011                    TO  TE011-CODIGO-TABLA
               MOVE CON-COD-EMPRESA        TO  TE011-CODIGO-EMPRESA
               MOVE CON-ANO                TO  TE011-CODIGO-ANO
               MOVE CON-PERIODO            TO  TE011-CODIGO-PERIODO
               MOVE ZEROS                  TO  TE011-CODIGO-FILLER
               MOVE W-TPN-FECHA-DESDE      TO  TE011-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  TE011-FECHA-HASTA
               MOVE W-TPN-FECHA-PAGO       TO  TE011-FECHA-PAGO
      ***************************************************************
      *                    W-TPN-SEMANAS-RFTE                        *
      ***************************************************************
               MOVE W-TPN-SEMANAS-RFTE     TO  TE011-SEMANAS-RFTE
               MOVE W-TPN-FLAG-PERIODO     TO  TE011-FLAG-PERIODO
               MOVE W-TPN-NUMERO-DIAS      TO  TE011-NUMERO-DIAS
               MOVE W-TPN-NUMERO-HORAS     TO  TE011-NUMERO-HORAS
               MOVE SPACES                 TO  TE011-FILLER.
       030-CONFIRMA-PROCESO.
               DISPLAY " PRESIONE <C> CONFIRMAR "   LINE 24 POSITION  1
                              REVERSE
               DISPLAY " <F2> TERMINAR "   LINE 24 POSITION 27
                              REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  CUP
               GO TO 030-ACEPTA-FECHA-PAGO.
           IF  F2
               GO TO 120-FIN.
           IF  W-TRUCO                  NOT =  "C"
               GO TO 030-CONFIRMA-PROCESO.
           IF  W-OPCION                     =  "R"
               REWRITE REC-TABLAEMP
           ELSE
               WRITE REC-TABLAEMP.
              PERFORM I-ACTUAL-FLAG-CIERRE  THRU F-ACTUAL-FLAG-CIERRE.
       030-LECTURA-NOVEDADES.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               DISPLAY "POR FAVOR ESPERE ..."       LINE 24 POSITION  1
                                                    BLINK REVERSE
               MOVE W-TPN-FECHA-DESDE      TO  WK-FECHA-DESDE
               SUBTRACT 1                FROM  WK-ANO-DESDE
               MOVE 01                     TO  WK-MES-DESDE
               MOVE 01                     TO  WK-DIA-DESDE
               PERFORM CARGA-TABLA-PERIODOS THRU
                       SALE-CARGA-TABLA-PERIODOS.
               MOVE W-TPN-NUMERO-DIAS      TO  W-DIAS-PERIODO
               MOVE W-TPN-NUMERO-HORAS     TO  W-HORAS-PERIODO
               MOVE ZEROS                  TO  K
               MOVE 11                     TO  K-LINEA
               MOVE 11                     TO  I-LINEA
               PERFORM BORRA-NOVEDADES THRU
                       SALE-BORRA-NOVEDADES
               MOVE ZEROS                  TO  J
                                               I
                                               M.
       030-SIGUE-GENERAR-AUTOMATICAS.
           ADD  1                      TO  I
           IF  I                            >  W-TAMANO-TABLANOV
               GO TO 030-DISPLAY-DATOS
           ELSE
              IF   T-TCN-ORIGEN-NOVEDAD   (I)   =  "A"
                OR T-TCN-ORIGEN-NOVEDAD   (I)   =  "T"
                OR T-TCN-ORIGEN-NOVEDAD   (I)   =  "F"
                OR T-TCN-ORIGEN-NOVEDAD   (I)   =  "H"
                NEXT SENTENCE
              ELSE
                GO TO 030-SIGUE-GENERAR-AUTOMATICAS.

           IF   T-TCN-CAUSACION        (I)   =  "T"
                NEXT SENTENCE
           ELSE
             IF T-TCN-CAUSACION        (I)   =  "S"
                AND SW-SEMESTRAL             =  1
                NEXT SENTENCE
             ELSE
                IF T-TCN-CAUSACION     (I)   =  "A"
                   AND SW-ANUAL              =  1
                   NEXT SENTENCE
                ELSE
                  IF T-TCN-CAUSACION   (I)   =  "P"
                     AND SW-IMPARES          =  1
                     NEXT SENTENCE
                  ELSE
                    IF  T-TCN-CAUSACION        (I)   =  "I"
                      AND SW-IMPARES              =  0
                      NEXT SENTENCE
                    ELSE
                      GO TO 030-SIGUE-GENERAR-AUTOMATICAS.

               ADD  1                      TO  J
               MOVE T-TCN-CODIGO-CONCEPTO      (I)
                                  TO  T-NOV-CONCEPTO    (J)
                                      W-AUDINOCA-CONCEPTO
           PERFORM I-AUDITORIA-AUDINOCA THRU F-AUDITORIA-AUDINOCA
               MOVE I             TO  T-NOV-INDICE-TCN  (J)
               MOVE ZEROS         TO  T-NOV-HORAS       (J)
                                  T-NOV-DIAS            (J)
                                  T-NOV-VLR             (J)
               MOVE T-TCN-ORIGEN-NOVEDAD (I) TO T-NOV-ORIGEN-NOVEDAD(J)
               MOVE "G"           TO  T-NOV-FLAG-ORIGEN (J)
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "F"
           OR  T-TCN-ORIGEN-NOVEDAD   (I)   =  "H"
               NEXT SENTENCE
           ELSE
              IF T-TCN-VALOR-NOVEDAD    (I)   =  "H"
                 MOVE W-TPN-NUMERO-HORAS     TO  T-NOV-HORAS       (J)
              ELSE
               IF  T-TCN-VALOR-NOVEDAD    (I)   =  "D"
                 IF T-TCN-CLASE-NOVEDAD    (I)   =  "I"
                   MOVE W-TPN-SEMANAS-RFTE  TO  T-NOV-DIAS        (J)
               ELSE
                 MOVE W-TPN-NUMERO-DIAS      TO  T-NOV-DIAS        (J).


               MOVE J                      TO  W-TAMANO-AUTOMATICAS
                                               W-TAMANO-NOVEDADES
               GO TO 030-SIGUE-GENERAR-AUTOMATICAS.
       030-DISPLAY-DATOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY SPACES LINE 25 POSITION 1 CONTROL SG-CONTROL-STRING
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               MOVE 12                     TO  I-LINEA
               MOVE M                      TO  L.
       030-SIGUE-DISPLAY-DATOS.
               ADD  1                      TO  L
           IF  L                            >  W-TAMANO-NOVEDADES
               NEXT SENTENCE
           ELSE
               PERFORM MUESTRA-NOVEDADES THRU
                       SALE-MUESTRA-NOVEDADES
               ADD  1                      TO  I-LINEA
           IF  I-LINEA                      >  21
             NEXT SENTENCE
           ELSE
             GO TO 030-SIGUE-DISPLAY-DATOS.
       030-SALE-DISPLAY-DATOS.
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
               GO TO 030-DISPLAY-DATOS.
       040-CODIGO-CONCEPTO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.

           MOVE J                      TO  K
           MOVE I-LINEA                TO  K-LINEA
           PERFORM OPCIONES-NOVEDAD
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE T-NOV-CONCEPTO (J)     TO  W-TCN-CODIGO-CONCEPTO
           DISPLAY W-TCN-CODIGO-CONCEPTO LINE I-LINEA POSITION  2
           ACCEPT W-TCN-CODIGO-CONCEPTO  LINE I-LINEA POSITION  2
                      UPDATE CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  W-TCN-CODIGO-CONCEPTO    =  ZEROS
               MOVE T-NOV-CONCEPTO (J)     TO  W-TCN-CODIGO-CONCEPTO.
           MOVE W-TCN-CODIGO-CONCEPTO  TO  T-NOV-CONCEPTO (J)
           DISPLAY T-NOV-CONCEPTO (J)  LINE I-LINEA POSITION  2
               DISPLAY WK-ESPACIOS     LINE 24 POSITION  1
           IF  F2
               GO TO 120-FIN.
           IF  F1
             PERFORM I-AUDINOCA-FINAL THRU F-AUDINOCA-FINAL
               GO TO 065-DETERMINAR-PRIMA.
           IF  CUP
               SUBTRACT 1                FROM  J
               SUBTRACT 1                FROM  I-LINEA
           IF  J                            <  1
               MOVE 1                      TO  J
               MOVE 12                     TO  I-LINEA
               GO TO 040-CODIGO-CONCEPTO
           ELSE
           IF  I-LINEA                      <  12
               ADD  1                      TO  K-LINEA
               SUBTRACT 1                FROM  M
               GO TO 030-DISPLAY-DATOS
           ELSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  CDN
           AND J                        NOT >  W-TAMANO-NOVEDADES
               ADD  1                      TO  J
               ADD  1                      TO  I-LINEA
           IF  I-LINEA                      >  21
               SUBTRACT 1                FROM  K-LINEA
               ADD  1                      TO  M
               GO TO 030-DISPLAY-DATOS
           ELSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  T-NOV-CONCEPTO (J)       =  ZEROS
               DISPLAY " CONCEPTO EN CEROS "        LINE 24 POSITION  1
                                REVERSE
               GO TO 040-CODIGO-CONCEPTO.
           IF  F9
               PERFORM RETIRE-NOVEDAD THRU
                       SALE-RETIRE-NOVEDAD
               SUBTRACT 1                FROM  K
               SUBTRACT 1                FROM  K-LINEA
               GO TO 030-DISPLAY-DATOS.
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
               DISPLAY " CONCEPTO NO EXISTE "       LINE 24 POSITION  1
                                  REVERSE
               GO TO 040-CODIGO-CONCEPTO.
               DISPLAY T-TCN-DESCRIPCION (I)
                       LINE I-LINEA POSITION 09 SIZE 29
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
               MOVE I                      TO  T-NOV-INDICE-TCN      (J)
           IF  J                            >  W-TAMANO-NOVEDADES
               MOVE ZEROS    TO  T-NOV-HORAS           (J)
                             T-NOV-DIAS            (J)
                             T-NOV-VLR             (J)
                             T-NOV-FLAG-ACUMULADOS (J)
               MOVE "G"      TO  T-NOV-FLAG-ORIGEN     (J)
               MOVE J        TO  W-TAMANO-NOVEDADES.
       043-PREPARA-CONCEPTO.
           DISPLAY WK-ESPACIOS LINE 24 POSITION  1
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "F" OR
               T-TCN-ORIGEN-NOVEDAD   (I)   =  "H"
               GO TO 060-SIGUIENTE-NOVEDAD.
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
               DISPLAY WI-SZ8P        LINE I-LINEA POSITION 57
               GO TO 060-SIGUIENTE-NOVEDAD
           ELSE
               DISPLAY WI-SZ9P        LINE I-LINEA POSITION 69
               GO TO 060-SIGUIENTE-NOVEDAD.
       045-CANTIDAD-HORAS.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-HORAS       (J)  TO  NOV-HORAS
               MOVE T-NOV-HORAS       (J)  TO  WI-SZ4P2
               DISPLAY WI-SZ4P2       LINE I-LINEA POSITION 40
               ACCEPT T-NOV-HORAS     (J)  LINE I-LINEA POSITION 40
                                                    CONVERT
                                                    TAB
                                                    NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               MOVE T-NOV-HORAS       (J)  TO  WI-SZ4P2
               DISPLAY WI-SZ4P2       LINE I-LINEA POSITION 40
           IF  F2
               GO TO 120-FIN.
               DISPLAY WK-ESPACIOS    LINE 24 POSITION  1
           IF  T-NOV-HORAS            (J)   =  ZEROS
               MOVE NOV-HORAS              TO  T-NOV-HORAS         (J).
           IF  CUP
               GO TO 040-CODIGO-CONCEPTO.
           IF  ERR
               MOVE ZEROS                  TO  WK-EX
               DISPLAY " HORAS ERRADAS "            LINE 24 POSITION  1

               GO TO 045-CANTIDAD-HORAS.
           IF  T-NOV-HORAS            (J)   =  ZEROS
               DISPLAY " HORAS ERRADAS "            LINE 24 POSITION  1
                                REVERSE
               GO TO 045-CANTIDAD-HORAS.
               COMPUTE T-NOV-VLR      (J)   =  T-NOV-HORAS         (J)
                                            *  W-VALOR-HORA
                                            *  T-TCN-PORC-RECARGO  (I)
                                            /  100
                                            +  0.50
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P LINE I-LINEA POSITION 57
           ELSE
               DISPLAY WI-SZ9P LINE I-LINEA POSITION 69.
               GO TO 060-SIGUIENTE-NOVEDAD.
       050-CANTIDAD-DIAS.
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "L"
               MOVE ZEROS                  TO  T-NOV-DIAS          (J)
               GO TO 060-SIGUIENTE-NOVEDAD.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-DIAS        (J)  TO  NOV-DIAS
               MOVE T-NOV-DIAS        (J)  TO  WI-SZ5
               DISPLAY WI-SZ5         LINE I-LINEA POSITION 50
               ACCEPT T-NOV-DIAS  (J) LINE I-LINEA POSITION 50
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               MOVE T-NOV-DIAS  (J)  TO  WI-SZ5
               DISPLAY WI-SZ5   LINE I-LINEA POSITION 50
           IF  F2
               GO TO 120-FIN.
               DISPLAY WK-ESPACIOS LINE 24 POSITION  1
           IF  CUP
               GO TO 040-CODIGO-CONCEPTO.
           IF  T-NOV-DIAS             (J)   =  ZEROS
               MOVE NOV-DIAS               TO  T-NOV-DIAS          (J).
           IF  ERR
           OR  T-NOV-DIAS             (J)   =  ZEROS
               DISPLAY " DIAS ERRADOS "             LINE 24 POSITION  1

               GO TO 050-CANTIDAD-DIAS.
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "I"
               COMPUTE T-NOV-VLR      (J)   =  ZEROS
           ELSE
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "I"
               NEXT SENTENCE
           ELSE
               COMPUTE T-NOV-VLR      (J)   =  T-NOV-DIAS          (J)
                                            *  W-VALOR-DIA
                                            *  T-TCN-PORC-RECARGO  (I)
                                            /  100.
           MOVE T-NOV-VLR             (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "I"
               NEXT SENTENCE
           ELSE
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P  LINE I-LINEA POSITION 57
           ELSE
               DISPLAY WI-SZ9P  LINE I-LINEA POSITION 69.
               GO TO 060-SIGUIENTE-NOVEDAD.
       055-VALOR.
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "T"
           OR  T-TCN-ORIGEN-NOVEDAD   (I)   =  "A"
               GO TO 055-VALOR-TABLA.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE T-NOV-VLR         (J)  TO  NOV-VLR
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P LINE I-LINEA POSITION 57
           ELSE
               DISPLAY WI-SZ9P LINE I-LINEA POSITION 69.
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               ACCEPT T-NOV-VLR       (J)           LINE I-LINEA
                                                            POSITION 57
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION
           ELSE
               ACCEPT T-NOV-VLR (J) LINE I-LINEA POSITION 69
                      CONVERT TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P      LINE I-LINEA POSITION 57
           ELSE
               DISPLAY WI-SZ9P      LINE I-LINEA POSITION 69.
           IF  F2
               GO TO 120-FIN.
               DISPLAY WK-ESPACIOS  LINE 24 POSITION  1
           IF  CUP
               GO TO 040-CODIGO-CONCEPTO.
           IF  T-NOV-VLR            (J)   =  ZEROS
               MOVE NOV-VLR                TO  T-NOV-VLR         (J).
           IF  ERR
           OR  T-NOV-VLR              (J)   =  ZEROS
               DISPLAY " VALOR ERRADO "             LINE 24 POSITION  1
                         REVERSE
               GO TO 055-VALOR.
               MOVE T-NOV-VLR         (J)  TO  WI-SZ8P
           IF  T-TCN-TIPO-NOVEDAD     (I)   =  "P"
           OR  T-TCN-TIPO-NOVEDAD     (I)   =  "B"
               DISPLAY WI-SZ8P LINE I-LINEA POSITION 57
           ELSE
               DISPLAY WI-SZ9P LINE I-LINEA POSITION 69.
               GO TO 060-SIGUIENTE-NOVEDAD.
       055-VALOR-TABLA.
           IF  T-TCN-ORIGEN-NOVEDAD   (I)   =  "T"
               MOVE ZEROS                  TO  T-NOV-VLR         (J).
       060-SIGUIENTE-NOVEDAD.
               GO TO 030-DISPLAY-DATOS.
       065-DETERMINAR-PRIMA.
               MOVE ZEROS                  TO  I.
       065-BUSCA-CONCEPTO-PRIMA.
               ADD  1                      TO  I
           IF  I                            >  W-TAMANO-TABLANOV
               GO TO 080-GRABACION
           ELSE
           IF  T-TCN-CLASE-NOVEDAD    (I)   =  "L"
           OR  T-TCN-CODIGO-CONCEPTO  (I)   =  021
           OR  T-TCN-CODIGO-CONCEPTO  (I)   =  031
               NEXT SENTENCE
           ELSE
               GO TO 065-BUSCA-CONCEPTO-PRIMA.
               MOVE ZEROS                  TO  L.
       065-UBICA-NOVEDAD-PRIMA.
               ADD  1                      TO  L
           IF  L                            >  W-TAMANO-NOVEDADES
               GO TO 065-BUSCA-CONCEPTO-PRIMA
           ELSE
           IF  T-TCN-CODIGO-CONCEPTO (I)    =  T-NOV-CONCEPTO (L)
               NEXT SENTENCE
           ELSE
               GO TO 065-UBICA-NOVEDAD-PRIMA.
               MOVE I                      TO  K
               OPEN OUTPUT TEMPACUM
               CLOSE       TEMPACUM
               OPEN I-O    TEMPACUM.
       070-CALCULAR-BASE-PRIMA.
           IF  T-TCN-CLASE-NOVEDAD    (K)   =  "L"
           OR  T-TCN-CODIGO-CONCEPTO  (K)   =  021
               NEXT SENTENCE
           ELSE
               GO TO 070-CALCULAR-BASE-CESANTIA.
               MOVE W-TPN-FECHA-HASTA      TO  WK-FECHA-HASTA
               MOVE W-TPN-FECHA-DESDE      TO  WK-FECHA-DESDE
           IF  WK-MES-DESDE                 >  6
               MOVE 07                     TO  WK-MES-DESDE
           ELSE
               MOVE 01                     TO  WK-MES-DESDE.
               MOVE 01                     TO  WK-DIA-DESDE
               MOVE WK-FECHA-HASTA         TO  W-FECHA-RETIRO
               MOVE WK-FECHA-DESDE         TO  W-FECHA-INICIAL
               MOVE WK-FECHA-HASTA         TO  W-FECHA-FINAL
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 41
                                                    SIZE 39
               DISPLAY "PREPARANDO BASE PRIMAS"     LINE 24 POSITION  1
               DISPLAY "POR FAVOR ESPERE ..."       LINE 24 POSITION 41
                                                    BLINK REVERSE
               CLOSE NOMINOVE
               GO TO 070-BORRAR-ACUMULADOS.
       070-CALCULAR-BASE-CESANTIA.
               MOVE W-TPN-FECHA-HASTA      TO  WK-FECHA-DESDE
               SUBTRACT 1                FROM  WK-ANO-DESDE
               MOVE 01                     TO  WK-MES-DESDE
               MOVE 01                     TO  WK-DIA-DESDE
               MOVE WK-FECHA-DESDE         TO  WK-FECHA-HASTA
               MOVE 12                     TO  WK-MES-HASTA
               MOVE 31                     TO  WK-DIA-HASTA
               MOVE WK-FECHA-DESDE         TO  W-FECHA-INGRESO
               MOVE WK-FECHA-HASTA         TO  W-FECHA-RETIRO
               MOVE WK-FECHA-DESDE         TO  W-FECHA-INICIAL
               MOVE WK-FECHA-HASTA         TO  W-FECHA-FINAL
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 41
                                                    SIZE 39
               DISPLAY "PREPARANDO BASE CESANTIAS"  LINE 24 POSITION  1
               DISPLAY "POR FAVOR ESPERE ..."       LINE 24 POSITION 41
                                                    BLINK
               CLOSE NOMINOVE.
       070-BORRAR-ACUMULADOS.
               MOVE 1                      TO  SW-ABRE.
               MOVE ZEROS                  TO  CONTRATO-CLAVE
               START CONTRATO KEY           >  CONTRATO-CLAVE
                                      INVALID  KEY
               MOVE "98"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
       070-SIGUE-BORRAR-ACUMULADOS.
               MOVE 1 TO SW-ABRE.
               READ CONTRATO                   NEXT RECORD
           WITH NO LOCK AT  END
               GO TO 070-SALE-BORRAR-ACUMULADOS.
           IF  CONTRATO-SW3                    <  7
               NEXT SENTENCE
           ELSE
               GO TO 070-SIGUE-BORRAR-ACUMULADOS.
               MOVE ZEROS                  TO  CONTRATO-DNOTRAB
                                               CONTRATO-DIAS-PROMEDIO
                                               CONTRATO-BASE-PRIMA
                                               CONTRATO-BASE-CESANTIA
                                               CONTRATO-BASE-VACACIONES
               MOVE 1                      TO  SW-ABRE.
               REWRITE REC-CONTRATO
               GO TO 070-SIGUE-BORRAR-ACUMULADOS.
       070-SALE-BORRAR-ACUMULADOS.
               MOVE ZEROS                  TO  I.
       070-SELECCIONA-PERIODOS.
               ADD  1                      TO  I
           IF  I                            =  W-TAMANO-PERIODOS
               GO TO 070-FIN-EMPRESA.
 
           IF  T-TPN-FECHA-HASTA (I)        <  W-FECHA-INICIAL
               GO TO 070-SELECCIONA-PERIODOS.
 
           IF  T-TPN-FECHA-HASTA (I)        >  W-FECHA-FINAL
               GO TO 070-SELECCIONA-PERIODOS.
 
           IF  T-TPN-FECHA-HASTA (I)        >  W-TPN-FECHA-DESDE
               GO TO 070-SELECCIONA-PERIODOS.
               PERFORM I-LABEL-NOMINOVE  THRU F-LABEL-NOMINOVE.
               MOVE WIN-CODIGO-EMPRESA     TO  LAB-NOVECIA
               MOVE T-TPN-CODIGO-ANO           (I)
                                           TO  LAB-NOMINOVE-ANO
               MOVE T-TPN-CODIGO-PERIODO       (I)
                                           TO  LAB-NOMINOVE-PERIODO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 41
                                                    SIZE 38
               DISPLAY T-TPN-CODIGO-ANO        (I)  LINE 24 POSITION 41
               DISPLAY T-TPN-CODIGO-PERIODO    (I)  LINE 24 POSITION 45
               DISPLAY ": del"                      LINE 24 POSITION 49
               MOVE T-TPN-FECHA-DESDE (I)  TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE 24 POSITION 55
               DISPLAY "al"                         LINE 24 POSITION 64
               MOVE T-TPN-FECHA-HASTA (I)  TO  WI-FECHA
               DISPLAY WI-FECHA                     LINE 24 POSITION 67
               OPEN INPUT  NOMINOVE
               MOVE WIN-CODIGO-EMPRESA     TO  NOV-EMPRESA2
               MOVE ZEROS                  TO  NOV-CONCEPTO2
               MOVE ZEROS                  TO  NOV-CODIEMP2
               START NOMINOVE KEY           >  NOV-CLAVE2
                                      INVALID  KEY
               GO TO 070-FIN-NOVEDADES.
       070-SELECCIONA-TMP.
               READ NOMINOVE                   NEXT RECORD
               WITH NO LOCK AT  END
               GO TO 070-FIN-NOVEDADES.
               MOVE 1                      TO  SW-ABRE.
               MOVE NOV-CODIEMP            TO  CONTRATO-CEDULA
               READ CONTRATO          WITH NO LOCK INVALID  KEY
               GO TO 070-SELECCIONA-TMP.
           IF  CONTRATO-SW3                    >  6
               GO TO 070-SELECCIONA-TMP.
           IF  CONTRATO-FECING                 >  T-TPN-FECHA-HASTA (I)
               GO TO 070-SELECCIONA-TMP.
           IF  CONTRATO-FECING                 >  W-FECHA-FINAL
               GO TO 070-SELECCIONA-TMP.
           IF  T-TCN-CODIGO-CONCEPTO  (K)   =  031
               NEXT SENTENCE
           ELSE
               GO TO 070-SIGUE-SELECCIONA-TMP.
               MOVE W-FECHA-FINAL          TO  WK-FECHA-DESDE
           IF  WK-MES-DESDE                 <  4
               SUBTRACT 1                FROM  WK-ANO-DESDE
               ADD 12                      TO  WK-MES-DESDE.
               SUBTRACT 3                FROM  WK-MES-DESDE
           IF  CONTRATO-FECAUM                 >  ZEROS
           IF  CONTRATO-FECAUM                 >  WK-FECHA-DESDE
               MOVE W-FECHA-INICIAL        TO  WK-FECHA-DESDE
           ELSE
               MOVE CONTRATO-FECAUM           TO  WK-FECHA-DESDE.
           IF  CONTRATO-FECING                 >  WK-FECHA-DESDE
               MOVE CONTRATO-FECING           TO  WK-FECHA-DESDE.
           IF  WK-FECHA-DESDE               >  T-TPN-FECHA-HASTA (I)
               GO TO 070-SELECCIONA-TMP.
       070-SIGUE-SELECCIONA-TMP.
               MOVE ZEROS                  TO  SW-1
               MOVE ZEROS                  TO  J.
       070-BUSCA-CONCEPTO-TMP.
               ADD  1                      TO  J
           IF  J                            >  W-TAMANO-TABLANOV
               MOVE 1                      TO  SW-1
               GO TO 070-SELECCIONA-TMP
           ELSE
           IF  T-TCN-CODIGO-CONCEPTO (J)    =  NOV-CONCEPTO2
               NEXT SENTENCE
           ELSE
               GO TO 070-BUSCA-CONCEPTO-TMP.
           IF  T-TCN-BASE-LIQUIDACION  (J)  =  "S"
           OR  T-TCN-AFEC-DIAS         (J)  =  "S"
           OR  T-TCN-AFEC-CESANTIAS    (J)  =  "S"
           OR  T-TCN-AFEC-PRIMA-L      (J)  =  "S"
           OR  T-TCN-BASE-VACACION     (J)  =  "S"
               NEXT SENTENCE
           ELSE
               GO TO 070-SELECCIONA-TMP.
               MOVE NOV-CODIEMP            TO  TACUM-CEDULA
               MOVE ZEROS                  TO  SW-3
               READ TEMPACUM          WITH NO LOCK INVALID  KEY
               MOVE 1                      TO  SW-3.
           IF  SW-3                         >  ZEROS
               MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
               DISPLAY HOJAVIDA-CEDULA LINE 24 POSITION 01
               READ HOJAVIDA          WITH NO LOCK INVALID  KEY
               GO TO 070-SELECCIONA-TMP.
           IF  SW-3                         >  ZEROS
               MOVE NOV-CODIEMP            TO  TACUM-CEDULA
               MOVE NOV-EMPRESA            TO  TACUM-EMPRESA
               MOVE NOV-CODIEMP            TO  TACUM-CEDULA1
               MOVE HOJAVIDA-NOMBRE           TO  TACUM-NOMBRE
               MOVE CONTRATO-SALARIO          TO  TACUM-SALARIO
               MOVE CONTRATO-HORAS-PERIODO    TO  TACUM-HORAS-PERIODO
               MOVE CONTRATO-FECING           TO  TACUM-FECING
               MOVE W-FECHA-FINAL          TO  TACUM-FECRET
               MOVE ZEROS                  TO  TACUM-VALOR  ( 1)
                                               TACUM-VALOR  ( 2)
                                               TACUM-VALOR  ( 3)
                                               TACUM-VALOR  ( 4)
                                               TACUM-VALOR  ( 5)
                                               TACUM-VALOR  ( 6)
                                               TACUM-VALOR  ( 7)
               MOVE CONTRATO-CONTRTO          TO  TACUM-VALOR  ( 8)
               MOVE WIN-HORAS-PERIODO      TO  TACUM-VALOR  ( 9)
               MOVE CONTRATO-TOMADOS          TO  TACUM-VALOR  (10)
               MOVE CONTRATO-PAGADOS          TO  TACUM-VALOR  (11)
               MOVE CONTRATO-DNOTRAB          TO  TACUM-VALOR  (12).
       070-SUMA-ACUMULADOS-TMP.
               MOVE ZEROS                  TO  SW-1
           IF  T-TCN-BASE-LIQUIDACION  (J)  =  "S"
      *  W-DIAS-PROMEDIO
               ADD  NOV-DIAS               TO  TACUM-VALOR          (1).
           IF  T-TCN-AFEC-DIAS         (J)  =  "S"
      *  W-DIAS-NO-TRABAJADOS
               ADD  NOV-DIAS               TO  TACUM-VALOR          (2).
           IF  T-TCN-AFEC-CESANTIAS    (J)  =  "S"
      *  W-VALOR-BASE-CESANTIAS (3)
               COMPUTE TACUM-VALOR     (3)  =  TACUM-VALOR          (3)
                                            +  NOV-VLR
                                            *  T-TCN-PORC-CESANTIAS (J)
                                            /  100.
           IF  T-TCN-AFEC-PRIMA-L      (J)  =  "S"
      *  W-VALOR-BASE-PRIMA-L
               COMPUTE TACUM-VALOR     (4)  =  TACUM-VALOR          (4)
                                            +  NOV-VLR
                                            *  T-TCN-PORC-PRIMA-L   (J)
                                            /  100.
           IF  T-TCN-TIPO-NOVEDAD      (J)  =  "B"
           OR  T-TCN-CODIGO-CONCEPTO   (J)  =  019
               NEXT SENTENCE
           ELSE
           IF  T-TCN-BASE-VACACION     (J)  =  "S"
      *  W-VALOR-BASE-VACACION  (5)
               COMPUTE TACUM-VALOR     (5)  =  TACUM-VALOR          (5)
                                            +  NOV-VLR
                                            *  T-TCN-PORC-VACACION  (J)
                                            /  100.
       070-SALE-SUMA-ACUMULADOS-TMP.
           IF  SW-3                         >  ZEROS
               WRITE REC-TEMPACUM
           ELSE
               REWRITE REC-TEMPACUM.
               GO TO 070-SELECCIONA-TMP.
       070-FIN-NOVEDADES.
               CLOSE NOMINOVE
               MOVE W-FECHA-INICIAL        TO  WK-FECHA-DESDE
               GO TO 070-SELECCIONA-PERIODOS.
       070-FIN-EMPRESA.
               CLOSE TEMPACUM
               OPEN INPUT  TEMPACUM
               MOVE ZEROS                  TO  I-CODIGO-EMPRESA
               MOVE ZEROS                  TO  TACUM-EMPRESA
                                               TACUM-CEDULA1
               START TEMPACUM KEY           >  TACUM-CLAVE1
                                      INVALID  KEY
               GO TO 075-SALE-DETERMINAR-PRIMA.
       070-ACTUALIZA-PRIMA-CONTRATO.
               READ TEMPACUM             NEXT  RECORD
                             WITH NO LOCK  AT  END
               GO TO 075-SALE-DETERMINAR-PRIMA.
               MOVE TACUM-CEDULA           TO  CONTRATO-CEDULA
               MOVE 1 TO SW-ABRE.
               READ CONTRATO          WITH NO LOCK INVALID  KEY
               GO TO 070-ACTUALIZA-PRIMA-CONTRATO.
               MOVE TACUM-VALOR      (1)   TO  CONTRATO-DIAS-PROMEDIO
               MOVE TACUM-VALOR      (2)   TO  CONTRATO-DNOTRAB
               MOVE TACUM-VALOR      (3)   TO  CONTRATO-BASE-CESANTIA
               MOVE TACUM-VALOR      (4)   TO  CONTRATO-BASE-PRIMA
               MOVE TACUM-VALOR      (5)   TO  CONTRATO-BASE-VACACIONES
               REWRITE REC-CONTRATO
               GO TO 070-ACTUALIZA-PRIMA-CONTRATO.
       075-SALE-DETERMINAR-PRIMA.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               CLOSE       TEMPACUM
               OPEN OUTPUT TEMPACUM
               CLOSE       TEMPACUM
               PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
               MOVE WIN-CODIGO-EMPRESA     TO  LAB-NOVECIA
               MOVE CON-ANO                TO  LAB-NOMINOVE-ANO
               MOVE CON-PERIODO            TO  LAB-NOMINOVE-PERIODO
               OPEN I-O    NOMINOVE.
       080-GRABACION.
               MOVE ZEROS                  TO  W-TOTAL-APORTES
                                               W-TOTAL-ORDINARIOS
                                               W-TOTAL-EXTRA-ORDINARIOS
                                               W-TOTAL-OTROS
                                               W-TOTAL-TOTAL
                                               WK-PAGINA
               MOVE WIN-CODIGO-EMPRESA     TO  I-CODIGO-EMPRESA
               MOVE CON-PERIODO            TO  I-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  WK-FECHA-STANDARD
               MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
               MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
      *        MOVE "19"                   TO  WK-MASCARA-SIGLO
               MOVE WK-MASCARA-FECHA       TO  I-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  WK-FECHA-STANDARD
               MOVE WK-FECHA-STANDARD-DIA  TO  WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
               MOVE "ERR"                  TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-FECHA-STANDARD-MES)
                                           TO  WK-MASCARA-MES.
               MOVE WK-FECHA-STANDARD-ANO  TO  WK-MASCARA-ANO
      *        MOVE "19"                   TO  WK-MASCARA-SIGLO
               MOVE WK-MASCARA-FECHA       TO  I-FECHA-HASTA
               MOVE 88                     TO  WK-LINEAS
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
               DISPLAY
                "NOVEDADES A PROCESAR:"    LINE 24 POSITION 1
               MOVE W-TAMANO-NOVEDADES     TO  WI-Z4
               DISPLAY WI-Z4               LINE 24 POSITION 23
               MOVE ZEROS                  TO  CONTRATO-CLAVE
               START CONTRATO KEY           >  CONTRATO-CLAVE
                                      INVALID  KEY
               MOVE "98"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
               MOVE W-TAMANO-NOVEDADES     TO  W-TAMANO-FIJAS.
       080-SIGUE-GRABACION.
               MOVE 1 TO SW-ABRE.
               READ CONTRATO                   NEXT RECORD
               WITH NO LOCK AT  END
               GO TO 115-FIN-CARACTERIZACION.

           IF  WK-MES-PRIMA > 2 AND WK-MES-PRIMA < 05
               MOVE ZEROS TO CONTRATO-PRIMA
               REWRITE REC-CONTRATO.

           IF  WK-MES-PRIMA > 7 AND WK-MES-PRIMA < 11
               DISPLAY WK-MES-PRIMA  LINE 22 POSITION 18
               MOVE ZEROS TO CONTRATO-PRIMA
               REWRITE REC-CONTRATO.
           DISPLAY WK-MES-PRIMA  LINE 22 POSITION 1

               MOVE CONTRATO-CEDULA           TO  HOJAVIDA-CEDULA
               READ HOJAVIDA          WITH NO LOCK INVALID  KEY
               DISPLAY CONTRATO-CEDULA LINE 24 POSITION 01
               ACCEPT X LINE 24 POSITION 01
               MOVE "99"                   TO  STATUS-NOMINA
               GO TO RUTINA-ERROR-NOMINA.
           IF  CONTRATO-SW3                    <  7
               NEXT SENTENCE
           ELSE
               GO TO 080-SIGUE-GRABACION.

             IF  WIN-CODIGO-EMPRESA = 001
                 NEXT SENTENCE
             ELSE
                 IF  CONTRATO-FECING > CONTRATO-FECRET
                     NEXT SENTENCE
                 ELSE
                     GO TO 080-SIGUE-GRABACION.

           IF  CONTRATO-FECING                 >  W-TPN-FECHA-HASTA
               GO TO 080-SIGUE-GRABACION.

           IF  CONTRATO-SALARIO                <  WIN-SALMIN-ACT
               DISPLAY  CONTRATO-CEDULA LINE 24 POSITION 01
               DISPLAY "EMPLEADO CON SUELDO INFERIOR AL MINIMO EMPRESA"
               LINE 24 POSITION 20
               GO TO 080-SIGUE-GRABACION.

               DISPLAY "CEDULA:"  LINE 24 POSITION 41
               MOVE CONTRATO-CEDULA  TO  WI-NUMERO
               DISPLAY WI-NUMERO  LINE 24 POSITION 50
           PERFORM I-CALCULO-W-VALOR-HORA THRU F-CALCULO-W-VALOR-HORA.
               COMPUTE W-VALOR-DIA          =  CONTRATO-SALARIO
                                            /  30
           IF  WIN-HORAS-PERIODO            >  CONTRATO-HORAS-PERIODO
               COMPUTE CONTRATO-HORAS-PERIODO  =  W-HORAS-PERIODO
                                            *  CONTRATO-HORAS-PERIODO
                                            /  WIN-HORAS-PERIODO.
               MOVE W-TAMANO-FIJAS         TO  W-TAMANO-NOVEDADES.
       080-NOVEDADES-FIJAS.
               MOVE WIN-CODIGO-EMPRESA     TO  NVF-EMPRESA
               MOVE CONTRATO-CEDULA           TO  NVF-CODIEMP
               MOVE ZEROS                  TO  NVF-CONCEPTO
               MOVE ZEROS                  TO  I-ERROR
               START NOVEFIJA KEY           >  NVF-CLAVE
                                      INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
               IF  NOVEFIJA-STATUS              =  "23"
                   GO TO 080-SALE-NOVEDADES-FIJAS
               ELSE
                   PERFORM RUTINA-ERROR-NOVEFIJA.
           MOVE W-TAMANO-NOVEDADES     TO  L
           ADD  1                      TO  L.
       080-SIGUE-NOVEDADES-FIJAS.
               READ NOVEFIJA NEXT RECORD WITH NO LOCK AT  END
               GO TO 080-SALE-NOVEDADES-FIJAS.
           IF  NVF-EMPRESA                  >  WIN-CODIGO-EMPRESA
           OR  NVF-CODIEMP                  >  CONTRATO-CEDULA
               GO TO 080-SALE-NOVEDADES-FIJAS.
           MOVE 1                      TO  I.
       080-BUSCA-DOBLE-NOVEDAD.
           IF  I    =  L
           OR  I    >  L
               NEXT SENTENCE
           ELSE
               IF  T-NOV-CONCEPTO (I)           =  NVF-CONCEPTO
                   GO TO 080-SIGUE-NOVEDADES-FIJAS
               ELSE
                    ADD  1        TO  I
                    GO TO 080-BUSCA-DOBLE-NOVEDAD.
           MOVE NVF-CONCEPTO           TO  T-NOV-CONCEPTO  (L)
           MOVE 1                      TO  I.
       080-SIGUE-MAS-NOVEDADES-FIJAS.
           IF  T-TCN-CODIGO-CONCEPTO (I)    =  T-NOV-CONCEPTO     (L)
               NEXT SENTENCE
           ELSE
               ADD  1                      TO  I
               IF  I                            >  W-TAMANO-TABLANOV
                   MOVE 200                    TO  I
                    GO TO 080-SALE-NOVEDADES-FIJAS
               ELSE
               GO TO 080-SIGUE-MAS-NOVEDADES-FIJAS.
           MOVE I                      TO  T-NOV-INDICE-TCN      (L)
           MOVE NVF-HORAS              TO  T-NOV-HORAS           (L)
           MOVE NVF-DIAS               TO  T-NOV-DIAS            (L)
           MOVE NVF-VLR                TO  T-NOV-VLR             (L)
           MOVE NVF-FLAG-ORIGEN        TO  T-NOV-FLAG-ORIGEN     (L)
           MOVE NVF-FLAG-ACUMULADOS    TO  T-NOV-FLAG-ACUMULADOS (L)
           MOVE T-TCN-ORIGEN-NOVEDAD(I) TO T-NOV-ORIGEN-NOVEDAD(L)
           MOVE L                      TO  W-TAMANO-NOVEDADES
           ADD  1                      TO  L
           IF  L                            >  TAMANO-TABLANOV
               NEXT SENTENCE
           ELSE
               GO TO 080-SIGUE-NOVEDADES-FIJAS.
       080-SALE-NOVEDADES-FIJAS.
               PERFORM LIQUIDA-NOVEDADES-FONDO THRU
                       SALE-LIQUIDA-NOVEDADES-FONDO
               PERFORM LIQUIDA-NOVEDADES THRU
                       SALE-LIQUIDA-NOVEDADES
               COMPUTE W-TOTAL-A-PAGAR      =  W-TOTAL-DEVENGADOS
                                            -  W-TOTAL-DESCUENTOS
               MOVE W-TOTAL-DEVENGADOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  5 POSITION 67
               MOVE W-TOTAL-DESCUENTOS     TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  6 POSITION 67
               MOVE W-TOTAL-A-PAGAR        TO  WI-Z8P
               DISPLAY WI-Z8P              LINE  7 POSITION 67
               PERFORM GRABA-NOVEDADES THRU
                       SALE-GRABA-NOVEDADES
                       VARYING L         FROM  1  BY  1
                       UNTIL   L            >  W-TAMANO-NOVEDADES
           IF  SW-FONDO                     >  ZEROS
           IF  WK-LINEAS                    >  49
               PERFORM TITULOS-INFORME-FONDO THRU
                       SALE-TITULOS-INFORME-FONDO.
           IF  SW-FONDO                     >  ZEROS
               ADD  1                      TO  WK-LINEAS.
               GO TO 080-SIGUE-GRABACION.
       115-FIN-CARACTERIZACION.
               MOVE ZEROS                  TO  I-CEDULA
               MOVE "T O T A L E S . . ."  TO  I-NOMBRE-EMPLEADO
               MOVE W-TOTAL-TOTAL          TO  I-TOTAL
               MOVE W-TOTAL-APORTES        TO  I-APORTES
               MOVE W-TOTAL-ORDINARIOS     TO  I-ORDINARIOS
               MOVE W-TOTAL-EXTRA-ORDINARIOS
                                           TO  I-EXTRA-ORDINARIOS
               MOVE W-TOTAL-OTROS          TO  I-OTROS
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1.

           PERFORM  I-CARAINGR  THRU  F-CARAINGR.
           PERFORM  I-CARARETI  THRU  F-CARARETI.
           PERFORM  I-CTRLSUEL  THRU  F-CTRLSUEL.
       I-FIN-MODE.
       120-FIN.
           CLOSE CONCPTOS TABLAEMP CONTRATO HOJAVIDA NOMINOVE NOVEFIJA
                 PRESTAMO INFORMES AUDINOCA RETIRACT.
       120-FIN-FIN.
               EXIT PROGRAM.
       120-FT. STOP RUN.
       F-FIN-MODE.
      *
       110-BORRAR.
               DISPLAY W-SPACES     LINE I-LINEA POSITION  2
               DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
       OPCIONES-NOVEDAD.
               DISPLAY WK-ESPACIOS  LINE 25 POSITION  1
               DISPLAY " F1 "       LINE 25 POSITION  3 REVERSE

               DISPLAY "Iniciar"    LINE 25 POSITION  8 REVERSE
               DISPLAY " F2 "       LINE 25 POSITION 19 REVERSE

               DISPLAY "Terminar"   LINE 25 POSITION 24 REVERSE
               DISPLAY " F9 "       LINE 25 POSITION 51 REVERSE

               DISPLAY "Borrar Novedad" LINE 25 POSITION 55 REVERSE.
       SALE-OPCIONES-NOVEDAD.
               EXIT.
       TITULOS-INFORME-FONDO.
               ADD  1                      TO  WK-PAGINA
           IF  WK-PAGINA                    >  1
               WRITE REG-INFORMES        FROM  TITULO-4     BEFORE PAGE.
               MOVE WK-PAGINA              TO  I-PAGINA
               WRITE REG-INFORMES        FROM  TITULO-1        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-2        BEFORE 1
               MOVE SPACES                 TO  REG-INFORMES
               WRITE REG-INFORMES                              BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-4        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-3        BEFORE 1
               WRITE REG-INFORMES        FROM  TITULO-4        BEFORE 1.
       SIGUE-TITULOS-INFORME-FONDO.
               MOVE ZEROS                  TO  WK-LINEAS.
       SALE-TITULOS-INFORME-FONDO.
               EXIT.
      *
       I-LABEL-ARCHIVOS.
           COMPUTE CON-PERIODO          =  WIN-NOMINA + 1.
           MOVE    1                     TO IND-INFORMES
           PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
           PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
           PERFORM I-LABEL-NOMINOVE    THRU F-LABEL-NOMINOVE.
           PERFORM I-LABEL-NOVEFIJA    THRU F-LABEL-NOVEFIJA.
           PERFORM I-LABEL-TEMPACUM    THRU F-LABEL-TEMPACUM.
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           PERFORM I-LABEL-AUDINOCA    THRU F-LABEL-AUDINOCA.
           PERFORM  I-LABEL-CTRLSUEL  THRU  F-LABEL-CTRLSUEL.

      *    MOVE  ZEROS                   TO LAB-INFORMES-PERIODO
           MOVE WIN-EMPRESA            TO  WK-COMPANIA I-EMPRESA
           MOVE "¹"                    TO  WK-RAYAS-U
           MOVE "Ì"                    TO  WK-RAYAS-P
           MOVE "¶"                    TO  WK-RAYA-U
           MOVE "Ç"                    TO  WK-RAYA-P
           MOVE "*"                    TO  W-OPCION.
           PERFORM  I-STATUS-NOMINOVE  THRU  F-STATUS-NOMINOVE.
           PERFORM  I-STATUS-PRESTAMO  THRU  F-STATUS-PRESTAMO.
           PERFORM  I-STATUS-NOVEFIJA  THRU  F-STATUS-NOVEFIJA.
           PERFORM  I-STATUS-AUDINOCA  THRU  F-STATUS-AUDINOCA.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-STATUS-RETIRACT THRU F-STATUS-RETIRACT.

           OPEN INPUT  CONCPTOS HOJAVIDA PRESTAMO RETIRACT.
           OPEN I-O    TABLAEMP CONTRATO NOMINOVE NOVEFIJA AUDINOCA.

           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS. EXIT.
      *
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
      *                                        NOV-FLAG-FILLER
                                               NOV-ORIGEN-NOVEDAD.
       F-CEROS-NOMINOVE.

       I-PANTALLA.
           DISPLAY "CARGUE AUTOMATICO DE NOVEDADES"
                   LINE  3 POSITION 28 REVERSE
           DISPLAY "INICIALIZA SERIE No." LINE 8 POSITION 28 REVERSE
           DISPLAY CON-PERIODO LINE  8 POSITION 50 REVERSE
           PERFORM I-DISPLAY-HEAD THRU F-DISPLAY-HEAD
           PERFORM I-DISPLAY-BODY THRU F-DISPLAY-BODY.
           PERFORM I-DECODI-TIPOPERI THRU F-DECODI-TIPOPERI.
       F-PANTALLA.
      *
       I-ACTUAL-FLAG-CIERRE.
           DISPLAY "Actualizando companys..." LINE 24 POSITION 1
                     BEEP.
           OPEN I-O   EMPRESAS
           MOVE CON-COD-EMPRESA        TO  WIN-CODIGO-EMPRESA
           READ EMPRESAS WITH NO LOCK      INVALID  KEY
               MOVE 1                  TO  I-ERROR.
               MOVE "I"                TO  WIN-FLAG-CIERRE.
               MOVE ZEROS              TO  WIN-PERIODO-ESTADO.
           REWRITE REC-EMPRESAS.
           CLOSE EMPRESAS.
       F-ACTUAL-FLAG-CIERRE.
      *
       I-VERIFICAR-FECHA.
            MOVE WK-FECHA-HOY   TO WK-F-INI WK-F-FIN.
            MOVE 01             TO F-DIA-INI
            MOVE 31             TO F-DIA-FIN
            COMPUTE ANO-KK0 = CON-ANO - 1
            COMPUTE ANO-KK1 = CON-ANO + 1
      *
            COMPUTE F-MES-INI  = WK-MES - 1.
            IF      F-MES-INI  = 0
                    MOVE 12       TO F-MES-INI
                    MOVE ANO-KK0  TO F-ANO-INI.
      *
            COMPUTE F-MES-FIN  =  WK-MES + 1.
            IF      F-MES-FIN  =  13
                    MOVE 01       TO F-MES-FIN
                    MOVE ANO-KK1  TO F-ANO-FIN.
       F-VERIFICAR-FECHA. EXIT.
      *
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\PERIODOS.PRO".
           COPY "..\PRO\LIQUINOV.PRO".
           COPY "..\PRO\LIQUIACU.PRO".
           COPY "..\PRO\LIQUPRES.PRO".
           COPY "..\PRO\MASCFECH.PRO".
           COPY "..\PRO\CALCDIAS.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\CONSTEMP.PRO".
           COPY "..\PRO\USUAPERS.PRO".
           COPY "..\PRO\VLRHORAS.PRO".
           COPY "..\PRO\AUDINOCA.PRO".
           COPY "..\PRO\VALOHORA.PRO".
           COPY "..\PRO\CARAINGR.PRO".
           COPY "..\PRO\TIEMPO.PRO".
           COPY "..\PRO\CARARETI.PRO".
           COPY "..\PRO\TIPOPERI.PRO".
           COPY "..\PRO\CTRLSUEL.PRO".
           COPY "..\PRO\DISCAPDA.PRO".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\PLB\NOMINOVE.PLB".
           COPY "..\PLB\NOVEFIJA.PLB".
           COPY "..\PLB\TEMPACUM.PLB".
           COPY "..\PLB\INFORMES.PLB".
           COPY "..\PLB\NOMINOVP.PLB".
           COPY "..\PLB\AUDINOCA.PLB".
           COPY "..\PLB\CTRLSUEL.PLB".
      *
           COPY "..\STA\PRESTAMO.STA".
           COPY "..\STA\AUDINOCA.STA".
           COPY "..\STA\NOMINOVE.STA".
           COPY "..\STA\NOVEFIJA.STA".
           COPY "..\STA\RETIRACT.STA".
      *
           COPY "..\OPE\AUDINOCA.WRI".
           COPY "..\OPE\NOMINOVE.REW".
           COPY "..\OPE\NOMINOVE.DEL".
           COPY "..\OPE\CTRLSUEL.WRI".
      *
           COPY "..\START\NOMINOVE.M00".
           COPY "..\START\RETIRACT.M00".
           COPY "..\START\CONTRATO.M00".
      *
           COPY "..\LEE\CONTRATO.I00".
           COPY "..\LEE\NOMINOVE.SEQ".
           COPY "..\LEE\RETIRACT.SEQ".
           COPY "..\LEE\CONTRATO.SEQ".
      ******************************************************************
