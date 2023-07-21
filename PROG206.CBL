       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG206.
      ******************************************************************
      *               MANTENIMIENTO DE TABLAS - NOMINA                 *
      *                      SERIES DE NOMINA    ABR-27-2002           *
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
           COPY "..\DYC\AUDINOCA.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\AUDINOCA.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG206".
       77      ID-PRG              PIC X(7)  VALUE "PROG206".
       77  W-IND-CAMBIO        PIC 9(01) VALUE ZEROS.
       77  W-CON-CAMBIO        PIC 999   VALUE ZEROS.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           OCCURS 25 TIMES.
         05    CODIGO-PANTALLA-ANO       PIC 9(04).
         05    CODIGO-PANTALLA-PERIODO   PIC 9(03).
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\CONCPTOS.WRK".
               COPY "..\WRK\TABLAGEN.WRK".
               COPY "..\LBL\CONCPTOS.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
               COPY "..\LBL\AUDINOCA.LBL".
      *
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
       01      WK-AREA-TABLAS.
        03     WK-CODIGO-TABLA           PIC X(3)       VALUE "011".
        03     WK-NOMBRE-TABLA           PIC X(30)      VALUE
               "      SERIES DE NOMINA        ".
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CONCPTOS.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
               COPY "..\DCL\AUDINOCA.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
               PERFORM I-LABEL-TABLAEMP    THRU F-LABEL-TABLAEMP.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO   WK-RAYAS-U
               MOVE "Ì"                    TO   WK-RAYAS-P
               MOVE "¶"                    TO   WK-RAYA-U
               MOVE "Ç"                    TO   WK-RAYA-P
               OPEN I-O   TABLAEMP.
               OPEN INPUT CONCPTOS.
       015-CEROS.
               MOVE ZEROS                  TO  TAB-EMP-CLAVE
               MOVE WK-CODIGO-TABLA        TO  TAB-EMP-CODIGO-TABLA
               MOVE SPACES                 TO  TAB-EMP-FILLER
               MOVE TAB-EMP-CLAVE          TO  W-TAB-CLAVE.
       015-INICIO.
               MOVE ZEROS                  TO  I-ERROR
               START TABLAEMP KEY           >  TAB-EMP-CLAVE
                                      INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  TABLAEMP-STATUS              =  "23"
               NEXT SENTENCE
           ELSE
               PERFORM RUTINA-ERROR-TABLAEMP.
       020-PANTALLA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "º"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "WINNER FINANCIAL© º"        LINE  2 POSITION 62
               DISPLAY "º Date :"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY WK-NOMBRE-TABLA              LINE  3 POSITION 26
               DISPLAY "Time:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "º"                          LINE  3 POSITION 80
               DISPLAY "º"                          LINE  4 POSITION  1
               DISPLAY "OPCION:"                    LINE  4 POSITION 28
               DISPLAY "º"                          LINE  4 POSITION 80
               DISPLAY WK-RAYAS                     LINE  5 POSITION  1
           "ºA¥O SERIE F-DESDE  F-HASTA  F-PAGO  F   USU F-GENERA ASCII
      -    "  USU F-PAGO-ASCII" LINE  6 POSITION  1
               DISPLAY "º"                          LINE  6 POSITION 80
               DISPLAY WK-RAYAS                     LINE  7 POSITION  1
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING

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
               DISPLAY "º"                          LINE 16 POSITION  1
               DISPLAY "º"                          LINE 16 POSITION 80
               DISPLAY "º"                          LINE 17 POSITION  1
               DISPLAY "º"                          LINE 17 POSITION 80
               DISPLAY "º"                          LINE 18 POSITION  1
               DISPLAY "º"                          LINE 18 POSITION 80
               DISPLAY "º"                          LINE 19 POSITION  1
               DISPLAY "º"                          LINE 19 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 20 POSITION  1
               DISPLAY WK-RAYA-ARRIBA               LINE 21 POSITION  1
               DISPLAY "³"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY " A "                        LINE 22 POSITION 19
                                                    REVERSE
               DISPLAY "Adicion "                   LINE 22 POSITION 23
               DISPLAY " C "                        LINE 22 POSITION 33
                                                    REVERSE
               DISPLAY "Consulta"                   LINE 22 POSITION 37
               DISPLAY " M "                        LINE 22 POSITION 48
                                                    REVERSE
               DISPLAY "Modificacion"               LINE 22 POSITION 52
               DISPLAY " R "                        LINE 22 POSITION 67
                                                    REVERSE
               DISPLAY "Retiro"                     LINE 22 POSITION 71
               DISPLAY "³"                          LINE 22 POSITION 80
               DISPLAY WK-RAYA-ABAJO                LINE 23 POSITION  1.
       025-OPCION.
               DISPLAY W-ESPALF                     LINE  4 POSITION 37
               DISPLAY W-ESPALF                     LINE  4 POSITION 48
               PERFORM 115-PANTALLA-CONSULTA
               PERFORM 015-CEROS
               MOVE ZEROS                  TO  I-ERROR
               START TABLAEMP KEY           >  TAB-EMP-CLAVE
                                      INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  TABLAEMP-STATUS              =  "23"
               NEXT SENTENCE
           ELSE
               PERFORM RUTINA-ERROR-TABLAEMP.
               MOVE ZEROS           TO  WK-EXCEPTION
               ACCEPT W-OPCION          LINE 4 POSITION 37
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
            IF W-OPCION = "A"
             IF PROSS-AKK NOT = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Sr. usuario, NO tiene derechos en adici¢n ..."
              LINE 25 POSITION 01
              GO TO 025-OPCION
             END-IF
            END-IF
            IF W-OPCION = "M"
             IF PROSS-MKK NOT = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Sr. usuario, NO tiene derechos en modificaci¢n..."
              LINE 25 POSITION 01
              GO TO 025-OPCION
             END-IF
            END-IF
            IF W-OPCION = "C"
             IF PROSS-CKK NOT = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Sr. usuario, NO tiene derechos en consulta..."
              LINE 25 POSITION 01
              GO TO 025-OPCION
             END-IF
            END-IF
            IF W-OPCION = "R"
             IF PROSS-RKK NOT = "S"
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
              "Sr. usuario, NO tiene derechos en retiro..."
              LINE 25 POSITION 01
              GO TO 025-OPCION
             END-IF
            END-IF
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
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
               DISPLAY W-OPCION                     LINE  4 POSITION 37
           IF  W-OPCION                     =  "A"
               DISPLAY " A D I C I O N "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY "   CONSULTA    "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "M"
               DISPLAY " MODIFICACION  "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "R"
               DISPLAY "  R E T I R O  "   LINE  4 POSITION 41
                                                    REVERSE.
               MOVE 8                      TO  I-LINEA
               MOVE 8                      TO  K-LINEA
               DISPLAY WK-ESPACIOS      LINE 24 POSITION  1.

       030-CODIGO-ANO.
           IF  W-OPCION                     =  "M"
           IF  I-LINEA                  NOT >  K-LINEA
               MOVE CODIGO-PANTALLA-ANO       (I-LINEA)
                                           TO  TE011-CODIGO-ANO.
           MOVE ZEROS               TO  WK-EXCEPTION
           MOVE TE011-CODIGO-ANO    TO  W-TPN-CODIGO-ANO WI-Z4
           DISPLAY WI-Z4            LINE I-LINEA  POSITION  2
           ACCEPT W-TPN-CODIGO-ANO  LINE I-LINEA  POSITION  2 CONVERT
                    UPDATE TAB  NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS     LINE 24 POSITION  1
           IF  F10
               MOVE TE011-CODIGO-ANO TO W-TPN-CODIGO-ANO.
           MOVE W-TPN-CODIGO-ANO     TO WI-Z4 TE011-CODIGO-ANO
           DISPLAY WI-Z4             LINE I-LINEA POSITION  2
           IF  CUP
               IF  W-OPCION          NOT =  "M"
               OR  I-LINEA           =  8
               GO TO 025-OPCION
           ELSE
               SUBTRACT 1            FROM  I-LINEA
               GO TO 030-CODIGO-ANO.
           IF W-OPCION NOT = "C"
            IF  W-TPN-CODIGO-ANO    =  ZEROS
               DISPLAY " A¥O PERIODO EN CEROS "     LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-ANO.

       030-CODIGO-PERIODO.
           DISPLAY SPACES SIZE 80     LINE 25 POSITION 01
           " <Ctrl>+<End> Auditoria " LINE 25 POSITION 01 REVERSE
           IF  W-OPCION               =  "M"
             IF  I-LINEA              NOT >  K-LINEA
               MOVE CODIGO-PANTALLA-PERIODO   (I-LINEA)
                                           TO  TE011-CODIGO-PERIODO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE TE011-CODIGO-PERIODO   TO  W-TPN-CODIGO-PERIODO WI-Z3
           DISPLAY WI-Z3               LINE I-LINEA POSITION  7
           ACCEPT W-TPN-CODIGO-PERIODO LINE I-LINEA POSITION  7 CONVERT
                  UPDATE TAB NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  F10
               MOVE TE011-CODIGO-PERIODO   TO  W-TPN-CODIGO-PERIODO.
           MOVE W-TPN-CODIGO-PERIODO TO WI-Z3 TE011-CODIGO-PERIODO
               DISPLAY WI-Z3  LINE I-LINEA POSITION  7
           IF  CUP
               GO TO 030-CODIGO-ANO.
           IF W-OPCION NOT = "C"
             IF  W-TPN-CODIGO-PERIODO         =  ZEROS
               DISPLAY " PERIODO EN CEROS " LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-PERIODO.

           MOVE ZEROS                  TO  SW1
           MOVE WK-CODIGO-TABLA        TO  TE011-CODIGO-TABLA
           MOVE WIN-CODIGO-EMPRESA     TO  W-TPN-CODIGO-EMPRESA
           MOVE W-TPN-CODIGO-EMPRESA   TO  TE011-CODIGO-EMPRESA
           MOVE ZEROS                  TO  TE011-CODIGO-FILLER
           MOVE W-TPN-CODIGO-ANO       TO  TE011-CODIGO-ANO
           MOVE W-TPN-CODIGO-PERIODO   TO  TE011-CODIGO-PERIODO
           READ TABLAEMP WITH NO LOCK INVALID  KEY
               MOVE 2                      TO  SW1.

           IF  CEND
             PERFORM I-CONSULTA-AUDINOCA THRU F-CONSULTA-AUDINOCA
             GO TO 015-CEROS.

           IF  SW1                          =  ZEROS
             IF  W-OPCION                     =  "A"
               DISPLAY " PERIODO DUPLICADO "        LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-ANO
             ELSE
               GO TO 035-DISPLAY-DATOS.

           IF  W-OPCION                     =  "C"
               START TABLAEMP KEY           >  TAB-EMP-CLAVE
                                      INVALID  KEY
               GO TO 115-FIN-CONSULTAR.

           IF  W-OPCION                     =  "C"
               MOVE 7                      TO  I-LINEA
               GO TO 115-CONSULTAR-SIGUIENTE.
               PERFORM 140-BORRAR-PANTALLA THRU
                       140-SALE-BORRAR-PANTALLA
               PERFORM 110-BORRAR
               MOVE 1                      TO  I
           IF  W-OPCION                 NOT =  "A"
               DISPLAY " PERIODO NO EXISTE "        LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-ANO.

       035-DISPLAY-DATOS.
           MOVE TE011-CODIGO-ANO TO  W-TPN-CODIGO-ANO WI-Z4
           DISPLAY WI-Z4             LINE I-LINEA POSITION  2
           MOVE TE011-CODIGO-PERIODO TO   W-TPN-CODIGO-PERIODO WI-Z3
           DISPLAY WI-Z3             LINE I-LINEA POSITION  7
           MOVE TE011-FECHA-DESDE    TO   W-TPN-FECHA-DESDE
                                          W-FECHA
           DISPLAY W-FECHA           LINE I-LINEA POSITION 11
           MOVE TE011-FECHA-HASTA    TO   W-TPN-FECHA-HASTA
                                          W-FECHA
           DISPLAY W-FECHA           LINE I-LINEA POSITION 20
           MOVE TE011-FECHA-PAGO     TO   W-TPN-FECHA-PAGO
                                          W-FECHA
           DISPLAY W-FECHA           LINE I-LINEA POSITION 29
           MOVE TE011-FLAG-PERIODO   TO  W-TPN-FLAG-PERIODO
           DISPLAY W-TPN-FLAG-PERIODO  LINE I-LINEA POSITION 38.
           DISPLAY
           TE011-FLAG-ACT-DAV LINE I-LINEA POSITION 40
           TE011-USUA-ACT-DAV LINE I-LINEA POSITION 42
           TE011-FECH-ACT-DAV LINE I-LINEA POSITION 46
           MOVE TE011-HORA-ACT-DAV TO WK-DESC-N8
           DISPLAY
           PRI-DES LINE I-LINEA POSITION 55
           ":"     LINE I-LINEA POSITION 57
           SEG-DES LINE I-LINEA POSITION 58
      *    ":"     LINE I-LINEA POSITION 60
      *    TER-DES LINE I-LINEA POSITION 61

           TE011-FLAG-TRA-DAV LINE I-LINEA POSITION 61
           TE011-USUA-TRA-DAV LINE I-LINEA POSITION 63
           TE011-FECH-TRA-DAV LINE I-LINEA POSITION 67
           MOVE TE011-HORA-TRA-DAV TO WK-DESC-N8
           DISPLAY
           PRI-DES LINE I-LINEA POSITION 76
           ":"     LINE I-LINEA POSITION 77
           SEG-DES LINE I-LINEA POSITION 78.
      *    ":"     LINE I-LINEA POSITION 73
      *    TER-DES LINE I-LINEA POSITION 74.
       040-FECHA-DESDE.
           IF  W-OPCION                     =  "R"
               GO TO 105-ELIMINAR.
           IF  W-OPCION                     =  "C"
               GO TO 115-CONSULTAR.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE TE011-FECHA-DESDE      TO  W-TPN-FECHA-DESDE
                                               W-FECHA
               DISPLAY W-FECHA          LINE I-LINEA POSITION 11
               ACCEPT W-TPN-FECHA-DESDE LINE I-LINEA POSITION 11
                                        UPDATE
                                        TAB NO BEEP
                                     ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
               MOVE W-TPN-FECHA-DESDE      TO  W-FECHA
               DISPLAY W-FECHA  LINE I-LINEA POSITION 11

           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS         LINE 24 POSITION  1
           IF  F10
               MOVE TE011-FECHA-DESDE      TO  W-TPN-FECHA-DESDE.
           IF  DEL
               MOVE ZEROS                  TO  W-TPN-FECHA-DESDE.
               MOVE W-TPN-FECHA-DESDE      TO  TE011-FECHA-DESDE
           IF  CUP
               GO TO 030-CODIGO-ANO.
       040-FECHA-HASTA.
               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE TE011-FECHA-HASTA      TO  W-TPN-FECHA-HASTA
                                               W-FECHA
               DISPLAY W-FECHA          LINE I-LINEA POSITION 20
               ACCEPT W-TPN-FECHA-HASTA LINE I-LINEA POSITION 20
                                        UPDATE
                                         TAB NO BEEP
                                     ON EXCEPTION WK-EX
                                        PERFORM 999-EXCEPTION.
               MOVE W-TPN-FECHA-HASTA      TO  W-FECHA
               DISPLAY W-FECHA  LINE I-LINEA POSITION 20
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS     LINE 24 POSITION  1
           IF  F10
               MOVE TE011-FECHA-HASTA  TO  W-TPN-FECHA-HASTA.
           IF  DEL
               MOVE ZEROS              TO  W-TPN-FECHA-HASTA.
               MOVE W-TPN-FECHA-HASTA  TO  TE011-FECHA-HASTA
           IF  CUP
               GO TO 040-FECHA-DESDE.
       040-FECHA-PAGO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE TE011-FECHA-PAGO       TO  W-TPN-FECHA-PAGO
                                           W-FECHA
           DISPLAY  W-FECHA          LINE I-LINEA  POSITION 29
           ACCEPT  W-TPN-FECHA-PAGO  LINE I-LINEA  POSITION 29
                                     UPDATE
                                      TAB NO BEEP
                                  ON EXCEPTION WK-EX
                                     PERFORM 999-EXCEPTION.
           MOVE  W-TPN-FECHA-PAGO  TO  W-FECHA
           DISPLAY  W-FECHA  LINE I-LINEA  POSITION 29
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-ESPACIOS LINE 24 POSITION  1
           IF  F10   MOVE TE011-FECHA-PAGO       TO  W-TPN-FECHA-PAGO.
           IF  DEL
            MOVE ZEROS                  TO  W-TPN-FECHA-PAGO.
            MOVE W-TPN-FECHA-PAGO       TO  TE011-FECHA-PAGO.
           IF  CUP  GO TO 040-FECHA-HASTA.
       040-ESTADO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE TE011-FLAG-PERIODO     TO  W-TPN-FLAG-PERIODO
           ACCEPT  W-TPN-FLAG-PERIODO  LINE I-LINEA POSITION 38
                                       UPDATE TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
      * Cambio de flag transmision ARCHIVO ASCII CORPORACION AHORRO    *
      *----------------------------------------------------------------*
           IF AF10
            MOVE SPACE TO TE011-FLAG-ACT-DAV.
           IF AF9
            MOVE SPACE TO TE011-FLAG-TRA-DAV.
           DISPLAY TE011-FLAG-ACT-DAV LINE I-LINEA POSITION 40
           DISPLAY TE011-FLAG-TRA-DAV LINE I-LINEA POSITION 61
      *----------------------------------------------------------------*
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1
           IF  F10  MOVE TE011-FLAG-PERIODO     TO  W-TPN-FLAG-PERIODO.
           IF  DEL
               MOVE SPACES                 TO  W-TPN-FLAG-PERIODO.
               MOVE W-TPN-FLAG-PERIODO     TO  TE011-FLAG-PERIODO
           IF  CUP  GO TO 040-FECHA-PAGO.
       040-ARMAR-REG.
               MOVE WK-CODIGO-TABLA        TO  TE011-CODIGO-TABLA
               MOVE W-TPN-CODIGO-EMPRESA   TO  TE011-CODIGO-EMPRESA
               MOVE ZEROS                  TO  TE011-CODIGO-FILLER
               MOVE W-TPN-CODIGO-ANO       TO  TE011-CODIGO-ANO
               MOVE W-TPN-CODIGO-PERIODO   TO  TE011-CODIGO-PERIODO
               MOVE W-TPN-FECHA-DESDE      TO  TE011-FECHA-DESDE
               MOVE W-TPN-FECHA-HASTA      TO  TE011-FECHA-HASTA
               MOVE W-TPN-FECHA-PAGO       TO  TE011-FECHA-PAGO
               MOVE W-TPN-FLAG-PERIODO     TO  TE011-FLAG-PERIODO
               MOVE SPACES                 TO  TE011-FILLER.
       080-GRABACION.
           IF  W-OPCION                     =  "M"
               GO TO 090-REGRABACION.
               DISPLAY " PRESIONE <A> ADICIONAR "   LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 27
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP
               GO TO 040-ESTADO.
           IF  F1
               GO TO 025-OPCION.
           IF  W-TRUCO                  NOT =  "A"
               GO TO 080-GRABACION.
               WRITE REC-TABLAEMP
               GO TO 090-FIN-ACTUALIZACION.
       090-REGRABACION.
               DISPLAY " PRESIONE <M> MODIFICAR "   LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 27
                                                    REVERSE
               DISPLAY " <> CONTINUAR SIN MODIFICAR "
                                                    LINE 24 POSITION 44
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP
               GO TO 040-ESTADO.
           IF  F1
               GO TO 025-OPCION.
           IF  CDN
               PERFORM 035-DISPLAY-DATOS
               GO TO 090-FIN-ACTUALIZACION.
           IF  W-TRUCO                  NOT =  "M"
               GO TO 090-REGRABACION.
               REWRITE REC-TABLAEMP.
       090-FIN-ACTUALIZACION.
               PERFORM 140-BORRAR-PANTALLA THRU
                       140-SALE-BORRAR-PANTALLA
               MOVE TE011-CODIGO-ANO       TO  CODIGO-PANTALLA-ANO
                                              (I-LINEA)
               MOVE TE011-CODIGO-PERIODO   TO  CODIGO-PANTALLA-PERIODO
                                              (I-LINEA)
               PERFORM 015-CEROS
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               ADD 1                       TO  I-LINEA
           IF  I-LINEA                      >  K-LINEA
               MOVE I-LINEA                TO  K-LINEA.
           IF  I-LINEA                      =  20
               PERFORM 115-PANTALLA-CONSULTA.
               GO TO 030-CODIGO-ANO.
       105-ELIMINAR.
               DISPLAY " PRESIONE <R> RETIRAR "     LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1
               GO TO 025-OPCION.
           IF  W-TRUCO                  NOT =  "R"
               GO TO 105-ELIMINAR.
               DELETE TABLAEMP
               GO TO 090-FIN-ACTUALIZACION.
       115-CONSULTAR.
           IF  I-LINEA                      <  19
               GO TO 115-CONSULTAR-SIGUIENTE.
               DISPLAY " PRESIONE <> CONSULTAR "   LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " <F1> OPCIONES "            LINE 24 POSITION 27
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
           IF  CDN
               GO TO 115-CONSULTAR-SIGUIENTE.
           IF  F1
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               GO TO 025-OPCION
           ELSE
               GO TO 115-CONSULTAR.
       115-PANTALLA-CONSULTA.
           IF  W-OPCION                     =  "C"
               MOVE 7                      TO  I-LINEA
           ELSE
               MOVE 8                      TO  I-LINEA.
           MOVE 8                      TO  K-LINEA
           PERFORM 115-BORRAR
               VARYING J-LINEA           FROM  8 BY 1
               UNTIL J-LINEA                >  19.
       115-CONSULTAR-SIGUIENTE.
           IF  I-LINEA                      =  19
               GO TO 115-PANTALLA-CONSULTA.
               READ TABLAEMP             NEXT  RECORD WITH NO LOCK
                                           AT  END
               GO TO 115-FIN-CONSULTAR.
           IF  TE011-CODIGO-TABLA           >  WK-CODIGO-TABLA
               GO TO 115-FIN-CONSULTAR.
               ADD 1                       TO  I-LINEA
               GO TO 035-DISPLAY-DATOS.
       115-FIN-CONSULTAR.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               DISPLAY " NO HAY MAS REGISTROS "     LINE 24 POSITION 1
                                                    REVERSE
               PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               GO TO 025-OPCION.
       115-BORRAR.
               MOVE ZEROS                  TO  CODIGO-PANTALLA-ANO
                                              (J-LINEA)
               MOVE ZEROS                  TO  CODIGO-PANTALLA-PERIODO
                                              (J-LINEA)
               DISPLAY W-SPACES                     LINE J-LINEA
                                                            POSITION 2.
       I-FIN-MODE.
               CLOSE CONCPTOS
                     TABLAEMP.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.

       110-BORRAR.
               DISPLAY W-SPACES     LINE I-LINEA POSITION 2
               DISPLAY WK-ESPACIOS  LINE 24 POSITION 1.
       140-BORRAR-PANTALLA.
               MOVE WK-CODIGO-TABLA        TO  W-TPN-CODIGO-TABLA
               MOVE WIN-CODIGO-EMPRESA     TO  W-TPN-CODIGO-EMPRESA
               MOVE ZEROS                  TO  W-TPN-CODIGO-FILLER
                                               W-TPN-FECHA-DESDE
                                               W-TPN-FECHA-HASTA
                                               W-TPN-NUMERO-DIAS
                                               W-TPN-NUMERO-HORAS
                                               W-TPN-FECHA-PAGO
               MOVE SPACES                 TO  W-TPN-FLAG-PERIODO.
       140-SALE-BORRAR-PANTALLA. EXIT.
      *
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\CAUDNOCA.PRO".
           COPY "..\PRO\MASCFECH.PRO".
           COPY "..\PRO\MASCHORA.PRO".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\AUDINOCA.PLB".
      *
           COPY "..\LEE\AUDINOCA.SEQ".
           COPY "..\LEE\CONCPTOS.I00".
           COPY "..\LEE\USUARIOS.I00".
      *
           COPY "..\START\AUDINOCA.M00".
      ******************************************************************
