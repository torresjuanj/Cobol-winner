       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG145.
      ******************************************************************
      * CENTROS DE COSTOS   ABR-27-2002                                           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\TARIESGO.DYC".
               COPY "..\DYC\FRIESGOS.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\TABLAEMP.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\TARIESGO.FD".
               COPY "..\FD\FRIESGOS.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\TABLAEMP.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)    VALUE "PROG145".
       77      ID-PRG              PIC X(7)    VALUE "PROG145".
       77      W-CODPAS            PIC 9(6)    VALUE ZEROS.
       77      COD1-TRAB           PIC 9(6)    VALUE ZEROS.
       77      CC-COD              PIC 9(6)    VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC 9(4)    VALUE ZEROS.
       01      WK-AREA-TABLAS.
        03     WK-CODIGO-TABLA     PIC X(3)    VALUE "021".
        03     WK-NOMBRE-TABLA     PIC X(30)   VALUE
               "     CENTROS  DE  COSTOS      ".
       01      COD-VER.
        03     COD-VER1            PIC X(1).
        03     COD-VER2            PIC 9(6).
       01      COD-ACT.
        03     COD-ACT1            PIC X(1).
        03     COD-ACT2            PIC 9(6).

               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\TARIESGO.LBL".
               COPY "..\LBL\FRIESGOS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\TABLAEMP.LBL".
      *
       01  WK-RAYAS-ARRIBA1.
           03  FILLER                   PIC X VALUE "ษ".
           03  FILLER                   PIC X(70) VALUE ALL "อ".
           03  FILLER                   PIC X VALUE "ป".
       01  WK-RAYAS-MEDIO1.
           03  FILLER                   PIC X(10) VALUE "บ".
           03  FILLER                   PIC X(01) VALUE "ณ".
           03  FILLER                   PIC X(60) VALUE ALL " ".
           03  FILLER                   PIC X VALUE "บ".
       01  WK-RAYAS-ABAJO1.
           03  FILLER                   PIC X VALUE "ศ".
           03  FILLER                   PIC X(70) VALUE ALL "อ".
           03  FILLER                   PIC X VALUE "ผ".

               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\TARIESGO.DCL".
               COPY "..\DCL\FRIESGOS.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\TABLAEMP.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-TABLAEMP THRU F-STATUS-TABLAEMP
           PERFORM I-STATUS-FRIESGOS THRU F-STATUS-FRIESGOS
           PERFORM I-STATUS-TARIESGO THRU F-STATUS-TARIESGO
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               PERFORM I-LABEL-TABLAEMP     THRU  F-LABEL-TABLAEMP.
               MOVE CON-COD-EMPRESA           TO  LAB-TABLAEMPCIA.
               PERFORM I-STATUS-TABLAEMP    THRU  F-STATUS-TABLAEMP.
               MOVE WIN-EMPRESA               TO  WK-COMPANIA
               MOVE "น"                       TO  WK-RAYAS-U
               MOVE "ฬ"                       TO  WK-RAYAS-P
               MOVE "ถ"                       TO  WK-RAYA-U
               MOVE "ว"                       TO  WK-RAYA-P.
               MOVE 16                        TO  POS-DOWN.
               MOVE "SYSTEM WINNER    "       TO  WK-SISTEMA.
               MOVE "CENTROS DE COSTOS "      TO  TIT-APLICACION.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   TABLAEMP.
               OPEN INPUT TARIESGO FRIESGOS.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           PERFORM I-SELECCIONA   THRU F-SELECCIONA
                                       UNTIL SALIDA OR ESC.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 999             TO   WK-EXCEPTION
              PERFORM I-WINDOW-FIN THRU F-WINDOW-FIN
              PERFORM I-SALIR      THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM  I-FIN-MODE  THRU F-FIN-MODE
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
             MOVE "HIGH" TO SG-INTENSITY
             IF W-OPCION = "A" AND PROSS-AKK = "S"
                MOVE  1  TO SW-MODE
                MOVE     SPACE TO REC-TABLAEMP
                PERFORM  I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                PERFORM  I-DISPLAY-OPCION   THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR            THRU F-CREAR UNTIL ESC
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M" AND PROSS-MKK = "S"
                  MOVE  2  TO SW-MODE
                  MOVE  SPACE TO REC-TABLAEMP
                  PERFORM  I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                  PERFORM  I-DISPLAY-OPCION   THRU F-DISPLAY-OPCION
                  MOVE     ZEROS      TO  TE021-CODIGO-CENTRO-COSTO
                  PERFORM I-START-TABLAEMP    THRU F-START-TABLAEMP
                  PERFORM  I-MODIFY-MODE      THRU F-MODIFY-MODE
                                                   UNTIL ESC
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C" AND PROSS-CKK = "S"
                   MOVE  SPACE TO REC-TABLAEMP
                   PERFORM I-MODO-CONSULTA THRU F-MODO-CONSULTA
                    UNTIL ESC
                   DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                   MOVE    ZEROS    TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R" AND PROSS-RKK = "S"
                    MOVE     SPACE               TO REC-TABLAEMP
                    PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                    PERFORM I-ELIMINA          THRU F-ELIMINA
                            UNTIL ESC
                      MOVE ZEROS TO WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-CREAR.
           PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
           MOVE 1 TO SW-MODE.
           PERFORM I-CODIGO               THRU  F-CODIGO.
           IF      CUP OR ESC               GO  F-CREAR.
           PERFORM I-LEE-TABLAEMP         THRU  F-LEE-TABLAEMP.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE           THRU  F-EXISTE
               GO I-CREAR
           ELSE
               MOVE    TE021-CODIGO-CENTRO-COSTO TO  W-CODPAS
               MOVE    SPACES               TO REC-TABLAEMP
               PERFORM I-MOSTRAR-TABLAEMP THRU  F-MOSTRAR-TABLAEMP
               PERFORM I-DATOS            THRU  F-DATOS
               IF CUP OR ESC                GO  I-CREAR.
       F-CREAR.
      *
       I-ELIMINA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
                   VARYING J FROM 7 BY 1 UNTIL J = 23
           PERFORM I-CODIGO       THRU F-CODIGO.
           IF      CUP              GO F-ELIMINA.
           IF      ESC              GO F-ELIMINA.
           PERFORM I-LEE-TABLAEMP THRU F-LEE-TABLAEMP.
           IF  SW-1  =  1
               PERFORM I-NEXISTE  THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
            MOVE ZEROS TO WK-EXCEPTION.
           MOVE 07 TO I-LINEA
           PERFORM I-MOSTRAR-TABLAEMP THRU F-MOSTRAR-TABLAEMP
           MOVE    SPACE                TO W-TRUCO.
           PERFORM I-ELIMINAR         THRU F-ELIMINAR
                   UNTIL HOME OR CDN OR ESC.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
           MOVE 2     TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM  I-CODIGO      THRU F-CODIGO.
           IF ESC      GO              F-MODIFY-MODE.
           PERFORM I-LEE-TABLAEMP THRU F-LEE-TABLAEMP.
           IF  SW-1  =  1
               MOVE 1               TO  CC-COD
               PERFORM I-NEXISTE  THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
              PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING J FROM 7 BY 1 UNTIL J = 08
             MOVE 07 TO I-LINEA
             PERFORM I-MOSTRAR-TABLAEMP THRU F-MOSTRAR-TABLAEMP
             PERFORM I-DATOS            THRU F-DATOS
             PERFORM I-LIMPIA-PANTALLA  THRU F-LIMPIA-PANTALLA
                     VARYING J          FROM 7 BY 1 UNTIL J = 23
             IF CUP OR ESC                GO I-MODIFY-MODE.
             PERFORM I-LEE-SIGUIENTE    THRU F-LEE-SIGUIENTE.
             IF  SW-1 = 1
                      MOVE 1   TO  CC-COD
                      MOVE ZEROS        TO TE021-CODIGO-CENTRO-COSTO
                      PERFORM I-START-CTAS THRU F-START-CTAS.
            PERFORM I-LIMPIA-PANTALLA      THRU F-LIMPIA-PANTALLA
                    VARYING J FROM 7 BY 1 UNTIL J = 23
            MOVE 07 TO I-LINEA
            PERFORM I-MOSTRAR-TABLAEMP THRU F-MOSTRAR-TABLAEMP.
       F-MODIFY-MODE.
      *
       I-MODO-CONSULTA.
           MOVE 01 TO I.
           MOVE 07 TO I-LINEA.
           MOVE ZEROS TO TE021-CODIGO-CENTRO-COSTO
           PERFORM I-START-TABLAEMP   THRU F-START-TABLAEMP.
           IF SW-1 = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "NO HAY DATOS..." LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            GO TO F-MODO-CONSULTA.
           PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA.
           PERFORM I-DISPLAY-OPCION   THRU F-DISPLAY-OPCION
           PERFORM I-DISPLAY-TABLAEMP THRU F-DISPLAY-TABLAEMP
                   UNTIL I > 16 OR SW-1 = 1.
         LEE-TABLAEMP.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY "COD: " LINE 03 POSITION 23.
           DISPLAY W-CODPAS LINE 03 POSITION 28.
           ACCEPT  W-CODPAS LINE 03 POSITION 28
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF ESC GO F-MODO-CONSULTA.
           PERFORM I-LIMPIA-PANTALLA  THRU F-LIMPIA-PANTALLA
                     VARYING J        FROM 7 BY 1 UNTIL J = 23.
           MOVE 01    TO I
           MOVE 07    TO I-LINEA
           IF SW-1 = 1 AND CDN
              MOVE ZEROS TO SW-1
              PERFORM I-START-TABLAEMP THRU F-START-TABLAEMP
              GO CONSU-1.
           IF  CDN GO CONSU-1.
           PERFORM I-START-TABLAEMP   THRU F-START-TABLAEMP
           MOVE    W-CODPAS             TO TE021-CODIGO-CENTRO-COSTO.
           PERFORM I-LEE-TABLAEMP     THRU F-LEE-TABLAEMP.
           IF  SW-1  =  ZEROS
               MOVE 07 TO I-LINEA
               PERFORM I-MOSTRAR-TABLAEMP  THRU F-MOSTRAR-TABLAEMP
               MOVE 08 TO I-LINEA
               PERFORM I-DISPLAY-TABLAEMP THRU F-DISPLAY-TABLAEMP
                        UNTIL I > 16
               GO LEE-TABLAEMP.
        CONSU-1.
                PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
                 VARYING J FROM 7 BY 1 UNTIL J = 23
                PERFORM I-DISPLAY-TABLAEMP THRU F-DISPLAY-TABLAEMP
                        UNTIL I > 16
                GO LEE-TABLAEMP.
       F-MODO-CONSULTA. EXIT.
      *
       I-DISPLAY-TABLAEMP.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "***  Fin Archivo ***"
                        LINE I-LINEA POSITION 20 REVERSE
                MOVE 18 TO I
                GO   F-DISPLAY-TABLAEMP.
           PERFORM I-MOSTRAR-TABLAEMP THRU F-MOSTRAR-TABLAEMP.
           ADD  1    TO   I.
           ADD  1    TO   I-LINEA.
       F-DISPLAY-TABLAEMP.
      *
       I-MOSTRAR-TABLAEMP.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
           DISPLAY TE021-NOMBRE-CENTRO-COSTO LINE I-LINEA POSITION 11
           MOVE    TE021-CODIGO-CENTRO-COSTO  TO WI-Z6.
           DISPLAY WI-Z6              LINE I-LINEA POSITION 03.
           DISPLAY TE021-TIPO-RIESGO  LINE I-LINEA POSITION 42
           DISPLAY TE021-ACT-RIESGO   LINE I-LINEA POSITION 63.
      *Tipo de Riesgo.
           MOVE TE021-TIPO-RIESGO        TO  TIPO-TARIESGO.
           READ TARIESGO WITH NO LOCK INVALID KEY
                MOVE    "** NO EXISTE  " TO NOMBRE-TARIESGO
                MOVE ZEROS               TO PORCEN-TARIESGO.
           DISPLAY NOMBRE-TARIESGO  LINE I-LINEA POSITION 45 SIZE 10.
           MOVE PORCEN-TARIESGO       TO WI-Z2P3.
           DISPLAY  WI-Z2P3  LINE I-LINEA POSITION 56.
      *Tipo de actividad.
           MOVE TE021-ACT-RIESGO        TO  COD-FRIESGOS.
           READ FRIESGOS WITH NO LOCK INVALID KEY
                MOVE    "** NO EXISTE  " TO NOM-FRIESGOS.
           DISPLAY NOM-FRIESGOS     LINE I-LINEA POSITION 71 SIZE 09.
       F-MOSTRAR-TABLAEMP. EXIT.
      *
       I-MOSTRAR-N-TABLAEMP.
           MOVE SPACES TO W-TRUCO
           MOVE ZEROS  TO WK-EXCEPTION
           IF I-LINEA = 8
              MOVE     9       TO I-LINEA
              MOVE     ZEROS   TO SW-99
              PERFORM I-START-TABLAEMP  THRU F-START-TABLAEMP.
           IF I-LINEA  > 22
              MOVE     9       TO I-LINEA
              PERFORM ACEPTA-TRUCO.
           IF CDN
              PERFORM I-LIMPIA-PANTALLA  THRU F-LIMPIA-PANTALLA
                      VARYING J          FROM 7 BY 1 UNTIL J = 23.
           IF SW-1  = 1 OR ESC
                   MOVE SPACES TO W-TRUCO
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE  COD1-TRAB        TO  TE021-CODIGO-CENTRO-COSTO
                   GO    F-MOSTRAR-N-TABLAEMP.
           PERFORM I-LEE-SIGUIENTE      THRU F-LEE-SIGUIENTE
           IF SW-99 = 0   AND SW-1 = 1 OR
              SW-1 = 1
                   MOVE    COD1-TRAB      TO  TE021-CODIGO-CENTRO-COSTO
                   GO   F-MOSTRAR-N-TABLAEMP.
           PERFORM I-MOSTRAR-TABLAEMP   THRU F-MOSTRAR-TABLAEMP.
           ADD  1   TO I-LINEA.
           GO      I-MOSTRAR-N-TABLAEMP.
       F-MOSTRAR-N-TABLAEMP. EXIT.
      *
       I-LIMPIA-PANTALLA.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
           DISPLAY SPACES SIZE 07   LINE J POSITION 03.
           DISPLAY SPACES SIZE 30   LINE J POSITION 11.
           DISPLAY SPACES SIZE 20   LINE J POSITION 42.
           DISPLAY SPACES SIZE 17   LINE J POSITION 63.
       F-LIMPIA-PANTALLA. EXIT.
      *
       I-CODIGO.
           MOVE ZEROS TO  WR-NUMERO-LIQUID.
           PERFORM    I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
                               VARYING J FROM 8 BY 1 UNTIL J = 23
           IF  W-OPCION = "M"
               DISPLAY SPACE LINE 25 POSITION 01 SIZE 80
               DISPLAY " [ F5 ]  SIGUIENTE REGISTRO "
                                 LINE 25 POSITION 01.
           MOVE   ZEROS      TO WK-EXCEPTION.
           MOVE   COD1-TRAB  TO TE021-CODIGO-CENTRO-COSTO
           ACCEPT TE021-CODIGO-CENTRO-COSTO  LINE 07 POSITION 03
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE        THRU F-FIN-MODE.
           IF CUP OR ESC GO F-CODIGO.
           MOVE   TE021-CODIGO-CENTRO-COSTO  TO COD1-TRAB.
           IF  W-OPCION = "A"
               MOVE "HIGH" TO SG-INTENSITY
               PERFORM I-VENTANA-CONCACTA   THRU F-VENTANA-CONCACTA
               PERFORM I-DISPLAY-OPCION     THRU F-DISPLAY-OPCION.
           MOVE     7   TO I-LINEA.
           PERFORM I-LEE-TABLAEMP           THRU F-LEE-TABLAEMP.
           PERFORM I-MOSTRAR-TABLAEMP       THRU F-MOSTRAR-TABLAEMP.
           IF  SW-1  =  ZEROS
               MOVE 09 TO I-LINEA
               PERFORM I-MOSTRAR-TABLAEMP  THRU F-MOSTRAR-TABLAEMP
               MOVE 10 TO I-LINEA
           ELSE
               MOVE 08 TO I-LINEA.
           PERFORM I-MOSTRAR-N-TABLAEMP     THRU F-MOSTRAR-N-TABLAEMP.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TE021-NOMBRE-CENTRO-COSTO LINE 07 POSITION 11
           ACCEPT  TE021-NOMBRE-CENTRO-COSTO LINE 07 POSITION 11
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF   ESC   GO TO      F-NOMBRE.
           DISPLAY TE021-NOMBRE-CENTRO-COSTO LINE 07 POSITION 11
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  GO TO F-NOMBRE.
           IF TE021-NOMBRE-CENTRO-COSTO = SPACES GO TO I-NOMBRE.
       F-NOMBRE. EXIT.
      *
       I-TE021-TIPO-RIESGO.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TE021-TIPO-RIESGO  LINE 07 POSITION 42
           ACCEPT  TE021-TIPO-RIESGO  LINE 07 POSITION 42
                   PROMPT ECHO NO BEEP UPDATE TAB
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY TE021-TIPO-RIESGO LINE 07 POSITION 42
           IF   F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF   F1
                PERFORM I-HELP-TARIESGO  THRU F-HELP-TARIESGO
                GO I-TE021-TIPO-RIESGO.
           IF   ESC OR CUP  GO TO     F-TE021-TIPO-RIESGO.
      *Tipo de Riesgo.
           MOVE TE021-TIPO-RIESGO  TO  TIPO-TARIESGO COD-VER.
           READ TARIESGO WITH NO LOCK INVALID KEY
                DISPLAY "TIPO NO EXISTE                "
                         LINE 25 POSITION 01
                GO I-TE021-TIPO-RIESGO.
           DISPLAY NOMBRE-TARIESGO  LINE 07 POSITION 45 SIZE 10.
           MOVE PORCEN-TARIESGO TO WI-Z2P3.
           DISPLAY  WI-Z2P3         LINE 07 POSITION 56.
       F-TE021-TIPO-RIESGO. EXIT.
      *
       I-TE021-ACT-RIESGO.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TE021-ACT-RIESGO  LINE 07 POSITION 63
           ACCEPT  TE021-ACT-RIESGO-ALF  LINE 07 POSITION 63
                   PROMPT ECHO NO BEEP UPDATE TAB
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           ACCEPT  TE021-ACT-RIESGO-NUM  LINE 07 POSITION 64
                   PROMPT ECHO NO BEEP UPDATE TAB
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY TE021-ACT-RIESGO LINE 07 POSITION 63
           IF   F1
                PERFORM I-HELP-FRIESGOS  THRU F-HELP-FRIESGOS
                GO I-TE021-ACT-RIESGO.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ESC OR CUP   GO TO F-TE021-ACT-RIESGO.

           MOVE TE021-ACT-RIESGO   TO  COD-FRIESGOS COD-ACT.
      *    IF COD-ACT1  NOT = COD-VER1
      *       DISPLAY "COD-ACT" COD-ACT1
      *       DISPLAY "COD-VER" COD-VER1
      *       PERFORM ACEPTA-TRUCO
      *         DISPLAY "ACTIVIDAD NO RELACIONADA CON RIESGO"
      *                  LINE 25 POSITION 01
      *         GO I-TE021-ACT-RIESGO.
           READ FRIESGOS WITH NO LOCK INVALID KEY
                DISPLAY "ACTIVIDAD CIIU NO EXISTE        "
                         LINE 25 POSITION 01
                GO I-TE021-ACT-RIESGO.
           DISPLAY NOM-FRIESGOS     LINE 07 POSITION 71 SIZE 09.
       F-TE021-ACT-RIESGO. EXIT.
      *
       I-DATOS.
           MOVE   COD1-TRAB     TO TE021-CODIGO-CENTRO-COSTO
           DISPLAY  TE021-NOMBRE-CENTRO-COSTO  LINE 07 POSITION 11
           MOVE    TE021-CODIGO-CENTRO-COSTO  TO WI-Z6.
           DISPLAY WI-Z6                       LINE 07 POSITION 03.
         I-D1.
           PERFORM I-NOMBRE         THRU F-NOMBRE.
           IF CUP   GO TO I-DATOS.
           IF ESC   GO TO F-DATOS.
         I-D2.
           PERFORM I-TE021-TIPO-RIESGO THRU F-TE021-TIPO-RIESGO.
           IF CUP   GO TO I-D1.
           IF ESC   GO TO F-DATOS.
         I-D3.
           PERFORM I-TE021-ACT-RIESGO THRU F-TE021-ACT-RIESGO.
           IF CUP   GO TO I-D2.
           IF ESC   GO TO F-DATOS.
           IF      SW-MODE = 1
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-GRABAR THRU F-GRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-DATOS
                   ELSE    NEXT SENTENCE
           ELSE
              IF   SW-MODE = 2
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-REGRABAR THRU F-REGRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-DATOS.
       F-DATOS.
      *
       I-EXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY "REGISTRO YA EXISTE" LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY SPACES SIZE 80       LINE 25 POSITION 01
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO.
       F-NEXISTE.
      *
       I-DISPLAY-OPCION.
           IF  W-OPCION = "A"
               DISPLAY "     A D D     "   LINE  02 POSITION 36 BLINK.
           IF  W-OPCION = "M"
               DISPLAY "  M O D I F Y  "   LINE  02 POSITION 36 BLINK.
           IF  W-OPCION = "C"
               DISPLAY " C O N S U L T "   LINE  02 POSITION 36 BLINK.
           IF  W-OPCION = "R"
               DISPLAY "  D E L E T E  "   LINE  02 POSITION 36 BLINK.
       F-DISPLAY-OPCION.
      *
       I-FIN-MODE.
           CLOSE TABLAEMP
                 TARIESGO
                 FRIESGOS.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-LEE-TABLAEMP.
           MOVE ZEROS    TO SW-1.
           MOVE WK-CODIGO-TABLA        TO  TE021-CODIGO-TABLA
           MOVE CON-COD-EMPRESA        TO  TE021-CODIGO-EMPRESA
           MOVE ZEROS                  TO  TE021-CODIGO-FILLER
           READ TABLAEMP   WITH NO LOCK  INVALID KEY
                MOVE   1                      TO  SW-1.
       F-LEE-TABLAEMP.
      *
       I-LEE-SIGUIENTE.
           MOVE ZEROS TO SW-1.
           READ TABLAEMP NEXT RECORD WITH NO LOCK AT END
                MOVE  01 TO SW-1
                GO F-LEE-SIGUIENTE.
           IF  TE021-CODIGO-TABLA    >  WK-CODIGO-TABLA
                MOVE  01 TO SW-1
                GO F-LEE-SIGUIENTE.
           MOVE 1 TO SW-99.
       F-LEE-SIGUIENTE.
      *
       I-START-TABLAEMP.
               MOVE ZEROS                  TO  TAB-EMP-CLAVE SW-1
               MOVE WK-CODIGO-TABLA        TO  TAB-EMP-CODIGO-TABLA
               MOVE CON-COD-EMPRESA        TO  TE021-CODIGO-EMPRESA
               MOVE SPACES                 TO  TE021-NOMBRE-CENTRO-COSTO
                                               TAB-EMP-FILLER.
       I-START.
               MOVE ZEROS                  TO  I-ERROR
               START TABLAEMP KEY NOT LESS TAB-EMP-CLAVE INVALID  KEY
                     MOVE 1            TO  SW-1.
       F-START-TABLAEMP.
      *

       I-START-CTAS.
            START TABLAEMP KEY >  TAB-EMP-CLAVE INVALID  KEY
                  MOVE 1            TO  SW-1.
       F-START-CTAS.
      *
       I-GRABAR.
           MOVE WK-CODIGO-TABLA        TO  TE021-CODIGO-TABLA
           MOVE CON-COD-EMPRESA        TO  TE021-CODIGO-EMPRESA
           MOVE ZEROS                  TO  TE021-CODIGO-FILLER
           PERFORM I-COLOR-2         THRU F-COLOR-2.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REC-TABLAEMP INVALID KEY REWRITE REC-TABLAEMP.
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
       F-GRABAR.
      *
       I-REGRABAR.
           PERFORM I-COLOR-2         THRU F-COLOR-2.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REC-TABLAEMP INVALID KEY STOP " ".
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
       F-REGRABAR.
      *
       I-ELIMINAR.
           PERFORM I-COLOR-2         THRU F-COLOR-2.
           DISPLAY
           "บ       [HOME] BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF HOME
              DELETE TABLAEMP INVALID KEY STOP " ".
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
       F-ELIMINAR.
      *
      *----------------------------------------------------------------*
      * .PRO CONCACTA consulta catalogo de cienta P.U.C.               *
      *----------------------------------------------------------------*
       I-VENTANA-CONCACTA.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING
           "บ CENTRO ณ NOMBRE DEL CENTRO DE COSTOS  ณ  TIPO DE RIESGO  %
      -    " ณ    C.I.I.U      บ"    LINE 05 POSITION 01
           DISPLAY "<< CENTRO DE COSTOS >>" LINE 03 POSITION 36.
           PERFORM I-COLOR-1         THRU F-COLOR-1.
           DISPLAY
           "ฬออออออออัออออออออออออออออออออออออออออออัอออออออออออออออออออ
      -    "อัอออออออออออออออออน"    LINE 04 POSITION 01
           DISPLAY
           "วฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -    "ฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ"    LINE 6  POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 07 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 08 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 09 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 10 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 11 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 12 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 13 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 14 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 15 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 16 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 17 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 18 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 19 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 20 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 21 POSITION 01
           "บ        ณ                              ณ
      -    " ณ                 บ"    LINE 22 POSITION 01
           "ศออออออออฯออออออออออออออออออออออออออออออฯอออออออออออออออออออ
      -    "อฯอออออออออออออออออผ"    LINE 23 POSITION 01
           SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           IF W-OPCION = "C"
              DISPLAY
              "<> av.linea"          LINE 25 POSITION 30
           PERFORM I-COLOR-1          THRU F-COLOR-1.
           PERFORM I-DISPLAY-OPCION   THRU F-DISPLAY-OPCION.
       F-VENTANA-CONCACTA. EXIT.
      *
       I-COLOR-1.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-1.
      *
       I-COLOR-2.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-COLOR-2.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\HERIESGO.PRO".
               COPY "..\PRO\HELPACTI.PRO".
               COPY "..\STA\TABLAEMP.STA".
               COPY "..\STA\FRIESGOS.STA".
               COPY "..\STA\TARIESGO.STA".
               COPY "..\PLB\TABLAEMP.PLB".
      ******************************************************************
