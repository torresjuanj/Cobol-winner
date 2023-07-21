       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG010.
      ******************************************************************
      *PLAN UNICO DE CUENTAS    MAY 01 DE 2002                         *
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
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\NOMICIUD.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA       PIC X(7)  VALUE "PROG010".
       77      ID-PRG         PIC X(7)    VALUE "PROG010".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
      *
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CNCATCTA THRU F-STATUS-CNCATCTA
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
               MOVE "WINNER FINANCIALฉ      " TO WK-SISTEMA.
               MOVE "PLAN UNICO DE CUENTAS"       TO TIT-APLICACION.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN I-O   CNCATCTA.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
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
             MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
             MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
             DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
             IF W-OPCION = "A" AND PROSS-AKK = "S"
                MOVE  1  TO SW-MODE
                MOVE  SPACE TO CNMAES
                PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M" AND PROSS-MKK = "S"
                  MOVE  2  TO SW-MODE
                  MOVE  SPACE TO CNMAES
                  PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  MOVE ZEROS TO CTA-TRAB CTAMAE
                  PERFORM I-START-CNCATCTA THRU F-START-CNCATCTA
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                           UNTIL ESC
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C" AND PROSS-CKK = "S"
                   MOVE  SPACE TO CNMAES
                   PERFORM I-MODO-CONSULTA THRU F-MODO-CONSULTA
                    UNTIL ESC
                   DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R" AND PROSS-RKK = "S"
                      MOVE  SPACE TO CNMAES
                    PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
                    PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                    PERFORM  I-ELIMINA        THRU F-ELIMINA
                           UNTIL ESC
                      MOVE ZEROS TO WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-CREAR.
           MOVE 1 TO SW-MODE.
           PERFORM I-CODIGO        THRU F-CODIGO.
           IF CUP OR ESC           GO   F-CREAR.
           PERFORM I-LEE-CATCTA    THRU F-LEE-CATCTA.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               GO I-CREAR
           ELSE
               MOVE    CTAMAE           TO   WK-CUENTA
               MOVE    SPACE            TO   CNMAES
               MOVE    WK-CUENTA        TO   CTAMAE
               MOVE    MAY1             TO   GRUCTA
               MOVE 09 TO I-LINEA
               PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP OR ESC            GO   I-CREAR.
       F-CREAR.
      *
       I-ELIMINA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 9 BY 1 UNTIL J = 22
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-ELIMINA.
           IF      ESC          GO   F-ELIMINA.
           PERFORM I-LEE-CATCTA THRU F-LEE-CATCTA.
           IF  SW-1  =  1
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
            MOVE ZEROS TO WK-EXCEPTION.
           MOVE 09 TO I-LINEA
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA
           PERFORM I-VALIDA-ANTES-BORRAR THRU F-VALIDA-ANTES-BORRAR.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL HOME OR CDN OR ESC.
       F-ELIMINA.
      *
       I-VALIDA-ANTES-BORRAR.
           MOVE ZEROS TO WK-CUENTA IND-VAL-NIV.
           MOVE CTAMAE TO WK-CUENTA.
           MOVE NIVEL TO IND-VAL-NIV.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF NIVEL > IND-VAL-NIV
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Sr. usuario EXISTEN niveles que dependen de este, no lo pue
      -    "de borrar..." LINE 25 POSITION 01 BEEP
            PERFORM ACEPTA-TRUCO
            GO TO I-ELIMINA.
           MOVE WK-CUENTA TO CTAMAE.
           PERFORM I-LEE-CATCTA THRU F-LEE-CATCTA.
       F-VALIDA-ANTES-BORRAR. EXIT.
      *
       I-MODIFY-MODE.
           MOVE 2     TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF F5
            PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE
            IF SW-1 = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NOT EXIST RECORDS" LINE 25 POSITION 01
             MOVE ZEROS TO   CTAMAE
             PERFORM I-START-CTAS THRU F-START-CTAS
             GO TO I-MODIFY-MODE
            END-IF
            MOVE CTAMAE TO CTA-TRAB
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
             VARYING J FROM 9 BY 1 UNTIL J = 22
            MOVE 09 TO I-LINEA
            PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA
            IF SW-1 = 1  MOVE ZEROS TO CTAMAE
              PERFORM I-START-CTAS THRU F-START-CTAS
              GO I-MODIFY-MODE
            ELSE
              GO I-MODIFY-MODE.
           IF ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-CATCTA THRU F-LEE-CATCTA.
           IF  SW-1  =  1
             PERFORM I-NEXISTE         THRU F-NEXISTE
             GO I-MODIFY-MODE
           ELSE
             PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING J FROM 9 BY 1 UNTIL J = 10
             MOVE 09 TO I-LINEA
             PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA
             PERFORM I-DATOS           THRU F-DATOS
             PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING J FROM 9 BY 1 UNTIL J = 22
             IF CUP OR ESC             GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU  F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE ZEROS TO   CTAMAE
                         PERFORM I-START-CTAS THRU F-START-CTAS.
            MOVE CTAMAE TO CTA-TRAB
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
             VARYING J FROM 9 BY 1 UNTIL J = 22
            MOVE 09 TO I-LINEA
            PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
       F-MODIFY-MODE.
      *
       I-MODO-CONSULTA.
           MOVE 01 TO I.
           MOVE 09 TO I-LINEA.
           MOVE ZEROS TO CTAMAE.
           MOVE ZEROS TO FIN-OK.
           PERFORM I-START-CNCATCTA   THRU F-START-CNCATCTA.
           IF SW-1 = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "NOT EXIST RECORDS" LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            GO TO F-MODO-CONSULTA.
           PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA.
           PERFORM I-DISPLAY-CNCATCTA THRU F-DISPLAY-CNCATCTA
                   UNTIL I > 13 OR SW-1 = 1.
         LEE-CNCATCTA.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY WK-CUENTA LINE 03 POSITION 31.
           ACCEPT WK-CUENTA LINE 03 POSITION 31 REVERSE
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN OR PGDN OR PGUP
            MOVE 01          TO   I
            MOVE 09          TO   I-LINEA
            MOVE ZEROS       TO   FIN-OK
            IF PGDN  MOVE WRK-VAR-PGDN TO CTAMAE
            ELSE
               MOVE WK-CUENTA TO CTAMAE
            END-IF
            IF PGUP
             MOVE ZEROS TO MAY3 MAY4 SUBMAE AUXMAE
             IF MAY1 = 1 COMPUTE MAY2 = MAY2 - 1 END-IF
             IF MAY1 > 1
               COMPUTE MAY1 = MAY1 - 1
               MOVE ZEROS TO MAY2
             END-IF
            END-IF
            PERFORM I-START-CNCATCTA   THRU F-START-CNCATCTA
                IF SW-1 = 1
                  DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                  "NOT EXIST RECORD..." LINE 25 POSITION 01
                  PERFORM ACEPTA-TRUCO
                  GO TO F-MODO-CONSULTA
                END-IF
                PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
                 VARYING J FROM 9 BY 1 UNTIL J = 22
                PERFORM I-DISPLAY-CNCATCTA THRU F-DISPLAY-CNCATCTA
                        UNTIL I > 13
                GO LEE-CNCATCTA.
       F-MODO-CONSULTA. EXIT.
      *
       I-DISPLAY-CNCATCTA.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY "End File" LINE I-LINEA POSITION 29 REVERSE
                MOVE 15 TO I
                GO   F-DISPLAY-CNCATCTA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF I = 1  MOVE CTAMAE TO WR-MATRICULA END-IF
           IF I = 2  MOVE CTAMAE TO WK-CUENTA END-IF
           IF I = 13 MOVE CTAMAE TO WRK-VAR-PGDN END-IF
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CNCATCTA.
      *
       I-MOSTRAR-CNCATCTA.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY MAYMAE                 LINE I-LINEA POSITION 02.
           DISPLAY SUBMAE                 LINE I-LINEA POSITION 06.
           MOVE AUX1 TO WI-Z2.
           DISPLAY WI-Z2                  LINE I-LINEA POSITION 08.
           MOVE AUX2 TO WI-Z2.
           DISPLAY WI-Z2                  LINE I-LINEA POSITION 10.
           DISPLAY NOMCTA                 LINE I-LINEA POSITION 13
            SIZE 38.
      * MUESTRA NIVEL
           DISPLAY NIVEL                  LINE I-LINEA POSITION 52.
      * MOSTRAR ENCABEZADO S/N
           IF ESTACTA NOT = "N"
            DISPLAY ESTACTA                LINE I-LINEA POSITION 54.
           IF W-OPCION NOT = "C"
            DISPLAY ESTACTA                LINE I-LINEA POSITION 54.
      * SI MANEJA NIT S/N
           IF CODPGT NOT = "N"
            DISPLAY CODPGT                 LINE I-LINEA POSITION 56.
           IF W-OPCION NOT = "C"
            DISPLAY CODPGT                 LINE I-LINEA POSITION 56.
      * MANEJA FACTURAS S/N
           IF INDDETAL NOT = "N"
            DISPLAY INDDETAL               LINE I-LINEA POSITION 58.
           IF W-OPCION NOT = "C"
            DISPLAY INDDETAL               LINE I-LINEA POSITION 58.
      * APLICA A PROVISIONES
           IF INDICE-PROV NOT = "N"
            DISPLAY INDICE-PROV            LINE I-LINEA POSITION 60.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-PROV            LINE I-LINEA POSITION 60.
      * Aplica ajustes-inflacion
           IF INDICE-AJINF NOT = "N"
            DISPLAY INDICE-AJINF           LINE I-LINEA POSITION 62.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-AJINF           LINE I-LINEA POSITION 62.
      * Aplica Base Gravable S/N
           IF INDBASE NOT = "N"
            DISPLAY INDBASE                LINE I-LINEA POSITION 64.
           IF W-OPCION NOT = "C"
            DISPLAY INDBASE                LINE I-LINEA POSITION 64.
      * % Aplica Base Grav.
           MOVE PORC-CTA TO WI-Z2.
           DISPLAY WI-Z2                  LINE I-LINEA POSITION 66.
      * Saldos Cartera
           IF INDICE-CAR NOT = "N"
            DISPLAY INDICE-CAR             LINE I-LINEA POSITION 69.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-CAR             LINE I-LINEA POSITION 69.
      * Saldos Bancos
           IF INDICE-BCOS NOT = "N"
            DISPLAY INDICE-BCOS            LINE I-LINEA POSITION 71.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-BCOS            LINE I-LINEA POSITION 71.
      * Saldos C.A.V.
           IF INDICE-RET NOT = "N"
            DISPLAY INDICE-RET             LINE I-LINEA POSITION 73.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-RET             LINE I-LINEA POSITION 73.
      * Otros
           IF INDICE-ANEX NOT = "N"
            DISPLAY INDICE-ANEX            LINE I-LINEA POSITION 75.
           IF W-OPCION NOT = "C"
            DISPLAY INDICE-ANEX            LINE I-LINEA POSITION 75.
      * Cuenta Perdida.
           IF IND-PYG-PER NOT = "N"
            DISPLAY IND-PYG-PER            LINE I-LINEA POSITION 77.
           IF W-OPCION NOT = "C"
            DISPLAY IND-PYG-PER            LINE I-LINEA POSITION 77.
      * Cuenta Utilidad.
           IF IND-PYG-UTI NOT = "N"
            DISPLAY IND-PYG-UTI            LINE I-LINEA POSITION 79.
           IF W-OPCION NOT = "C"
            DISPLAY IND-PYG-UTI            LINE I-LINEA POSITION 79.
       F-MOSTRAR-CNCATCTA. EXIT.
      *
       I-LIMPIA-PANTALLA.
           DISPLAY SPACES SIZE 10   LINE J POSITION 02.
           DISPLAY SPACES SIZE 38   LINE J POSITION 13.
           DISPLAY SPACES SIZE 01   LINE J POSITION 54.
           DISPLAY SPACES SIZE 01   LINE J POSITION 56.
           DISPLAY SPACES SIZE 01   LINE J POSITION 58.
           DISPLAY SPACES SIZE 01   LINE J POSITION 60.
           DISPLAY SPACES SIZE 01   LINE J POSITION 62.
           DISPLAY SPACES SIZE 01   LINE J POSITION 64.
           DISPLAY SPACES SIZE 02   LINE J POSITION 66.
           DISPLAY SPACES SIZE 01   LINE J POSITION 69.
           DISPLAY SPACES SIZE 01   LINE J POSITION 71.
           DISPLAY SPACES SIZE 01   LINE J POSITION 73.
           DISPLAY SPACES SIZE 01   LINE J POSITION 75.
           DISPLAY SPACES SIZE 01   LINE J POSITION 77.
           DISPLAY SPACES SIZE 01   LINE J POSITION 79.
       F-LIMPIA-PANTALLA. EXIT.
      *
       I-START-CNCATCTA.
           MOVE ZEROS TO SW-1.
           START CNCATCTA KEY NOT < CTAMAE INVALID KEY MOVE 1 TO SW-1.
       F-START-CNCATCTA. EXIT.
      *
       120-FT.
      *
       I-CODIGO.
           MOVE ZEROS TO IND-VAL-NIV.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO CTAMAE WR-NUMERO-LIQUID.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 9 BY 1 UNTIL J = 22
           IF  W-OPCION = "M"
               DISPLAY SPACE LINE 25 POSITION 01 SIZE 80
               DISPLAY " [ F5 ]  SIGUIENTE REGISTRO "
                              LINE 25 POSITION 01 REVERSE.
           IF  W-OPCION = "A"
               PERFORM I-GRUPO-WINDOW THRU F-GRUPO-WINDOW.
           MOVE   ZEROS TO WK-EXCEPTION.
           MOVE CT-MAY1 TO MAY1.
           ACCEPT MAY1 LINE 09 POSITION 02
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-OPCION = "A"
               MOVE "HIGH" TO SG-INTENSITY
               MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
               DISPLAY " " LINE 25 POSITION 80
               CONTROL SG-CONTROL-STRING
               PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
               PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
               MOVE 09 TO I-LINEA
               PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF F5 AND W-OPCION = "M" GO F-CODIGO.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           IF CUP OR ESC GO F-CODIGO.
           IF MAY1 = ZEROS OR MAY1 NOT NUMERIC GO I-CODIGO.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe cuenta mayor 1" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-CODIGO END-IF
             IF W-OPCION = "A"
              MOVE MAY1 TO GRUCTA
              GO TO I-1
             END-IF
             IF W-OPCION = "R" GO TO I-1 END-IF
           END-READ
           MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE 12 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 1 TO IND-VAL-NIV.
         I-1.
           MOVE ZEROS TO MAY2 MAY3 MAY4 SUBMAE AUXMAE.
           IF CUP MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE   ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 13 BY 1 UNTIL J = 14
           MOVE CT-MAY2 TO MAY2
           ACCEPT MAY2 LINE 09 POSITION 03
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO  I-CODIGO.
           IF MAY2   NOT NUMERIC GO I-1.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe cuenta mayor 2" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-1 END-IF
             IF W-OPCION = "A" GO TO I-2 END-IF
             IF W-OPCION = "R" GO TO I-2 END-IF
           END-READ
           IF CTAMAE = WR-NUMERO-LIQUID
            GO TO I-2
           ELSE
            MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE 13 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 2 TO IND-VAL-NIV.
         I-2.
           MOVE ZEROS TO MAY3 MAY4 SUBMAE AUXMAE.
           IF CUP MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE   ZEROS TO WK-EXCEPTION.
           MOVE CT-MAY3 TO MAY3
           ACCEPT MAY3 LINE 09 POSITION 04
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO  I-1.
           IF ESC GO  I-CODIGO.
           IF MAY3   NOT NUMERIC GO I-2.
         I-3.
           MOVE ZEROS TO MAY4 SUBMAE AUXMAE.
           IF CUP MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE   ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 14 BY 1 UNTIL J = 15
           MOVE CT-MAY4 TO MAY4
           ACCEPT MAY4 LINE 09 POSITION 05
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO  I-2.
           IF ESC GO  I-CODIGO.
           IF MAY4   NOT NUMERIC GO I-3.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe cuenta mayor 4" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-2 END-IF
             IF W-OPCION = "A" GO TO I-4 END-IF
             IF W-OPCION = "R" GO TO I-4 END-IF
           END-READ
           IF CTAMAE = WR-NUMERO-LIQUID
            GO TO I-4
           ELSE
            MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE 14 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 3 TO IND-VAL-NIV.
         I-4.
           MOVE ZEROS TO SUBMAE AUXMAE.
           IF CUP MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE   ZEROS  TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 15 BY 1 UNTIL J = 16
           MOVE CT-SUB TO SUBMAE.
           MOVE   SUBMAE TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 06
           ACCEPT SUBMAE LINE 09 POSITION 06
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           MOVE   SUBMAE TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 06
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO  I-3.
           IF ESC GO  I-CODIGO.
           IF SUBMAE NOT NUMERIC GO I-4.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe SUBCUENTA" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-4 END-IF
             IF W-OPCION = "A" GO TO I-5 END-IF
             IF W-OPCION = "R" GO TO I-5 END-IF
           END-READ
           IF CTAMAE = WR-NUMERO-LIQUID
            GO TO I-5
           ELSE
            MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE 15 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 4 TO IND-VAL-NIV.
         I-5.
           MOVE ZEROS TO AUX1 AUX2.
           IF CUP MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE   ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 16 BY 1 UNTIL J = 17
           MOVE CT-AUX1 TO AUX1.
           MOVE   AUX1   TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 08
           ACCEPT AUX1   LINE 09 POSITION 08
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           MOVE   AUX1   TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 08
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO  I-4.
           IF ESC GO  I-CODIGO.
           IF AUX1   NOT NUMERIC GO I-5.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe AUXILIAR 01" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-5 END-IF
             IF W-OPCION = "A" GO TO I-6 END-IF
             IF W-OPCION = "R" GO TO I-6 END-IF
           END-READ
           IF CTAMAE = WR-NUMERO-LIQUID
            GO TO I-6
           ELSE
            MOVE CTAMAE TO WR-NUMERO-LIQUID.
           MOVE 16 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 5 TO IND-VAL-NIV.
         I-6.
           MOVE   ZEROS TO WK-EXCEPTION.
           PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
            VARYING J FROM 17 BY 1 UNTIL J = 18
           MOVE CT-AUX2 TO AUX2.
           MOVE   AUX2   TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 10
           ACCEPT AUX2   LINE 09 POSITION 10
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           MOVE   AUX2   TO WI-92.
           DISPLAY WI-92 LINE 09 POSITION 10
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO  I-5.
           IF ESC GO  I-CODIGO.
           IF AUX2   NOT NUMERIC GO I-6.
           READ CNCATCTA WITH NO LOCK
            INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "NO existe AUXILIAR 02" LINE 25 POSITION 01
             IF W-OPCION = "M" GO TO I-6 END-IF
             IF W-OPCION = "A" GO TO F-CODIGO END-IF
             IF W-OPCION = "R" GO TO F-CODIGO END-IF
           END-READ
           IF CTAMAE = WR-NUMERO-LIQUID
            GO TO F-CODIGO.
           MOVE 17 TO I-LINEA.
           PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
           IF W-OPCION = "A" MOVE 6 TO IND-VAL-NIV.
       F-CODIGO.
      *
       I-DESCRIPCION.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
      * PRIMER NIVEL
           IF MAY1 > 0 AND MAY2 = 0 AND MAY3 = 0 AND MAY4 = 0 AND
              SUBMAE = 0 AND AUXMAE = ZEROS
            IF W-OPCION = "A" MOVE 1 TO NIVEL END-IF
            GO TO I-DESCRIPCION-N1.
      * SEGUNDO NIVEL
           IF MAY1 > 0 AND MAY2 > 0 AND MAY3 = 0 AND MAY4 = 0 AND
              SUBMAE = 0 AND AUXMAE = ZEROS
            IF IND-VAL-NIV < 1 AND W-OPCION = "A"
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Seคor usuario no existe NIVEL (1) inferior creado,DE <>"
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO UNTIL CUP
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             GO TO F-DESCRIPCION
            END-IF
            IF W-OPCION = "A"
              MOVE 2 TO NIVEL
              MOVE SPACE TO NOMCTA1
            END-IF
            GO TO I-DESCRIPCION-N2.
      * TERCER NIVEL
           IF MAY1 > 0 AND MAY2 > 0 AND MAY3 >= 0 AND MAY4 >= 0 AND
              SUBMAE = 0 AND AUXMAE = ZEROS
            IF IND-VAL-NIV < 2 AND W-OPCION = "A"
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Seคor usuario no existe NIVEL (2) inferior creado,DE <>"
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO UNTIL CUP
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             GO TO F-DESCRIPCION
            END-IF
            IF W-OPCION = "A"
              MOVE 3 TO NIVEL
              MOVE SPACE TO NOMCTA1 NOMCTA2
            END-IF
            GO TO I-DESCRIPCION-N3.
      * CUARTO NIVEL
           IF MAY1 > 0 AND MAY2 > 0 AND MAY3 >= 0 AND MAY4 >= 0 AND
              SUBMAE > 0 AND AUXMAE = ZEROS
            IF IND-VAL-NIV < 3 AND W-OPCION = "A"
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Seคor usuario no existe NIVEL (3) inferior creado,DE <>"
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO UNTIL CUP
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             GO TO F-DESCRIPCION
            END-IF
            IF W-OPCION = "A"
             MOVE 4 TO NIVEL
             MOVE SPACE TO NOMCTA1 NOMCTA2 NOMCTA3
            END-IF
            GO TO I-DESCRIPCION-N4.
      * QUINTO NIVEL
           IF MAY1 > 0 AND MAY2 > 0 AND MAY3 >= 0 AND MAY4 >= 0 AND
              SUBMAE > 0 AND AUX1 > 0 AND AUX2 = ZEROS
            IF IND-VAL-NIV < 4 AND W-OPCION = "A"
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Seคor usuario no existe NIVEL (4) inferior creado,DE <>"
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO UNTIL CUP
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             GO TO F-DESCRIPCION
            END-IF
            IF W-OPCION = "A"
             MOVE 5 TO NIVEL
             MOVE SPACE TO NOMCTA1 NOMCTA2 NOMCTA3 NOMCTA4
            END-IF
            GO TO I-DESCRIPCION-N5.
      * SEXTO NIVEL
           IF MAY1 > 0 AND MAY2 > 0 AND MAY3 >= 0 AND MAY4 >= 0 AND
              SUBMAE > 0 AND AUX1 > 0 AND AUX2 > 0
            IF IND-VAL-NIV < 5 AND W-OPCION = "A"
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "Seคor usuario no existe NIVEL (5) inferior creado,DE <>"
             LINE 25 POSITION 01 BEEP
             PERFORM ACEPTA-TRUCO UNTIL CUP
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             GO TO F-DESCRIPCION
            END-IF
            IF W-OPCION = "A"
             MOVE 6 TO NIVEL
             MOVE SPACE TO NOMCTA1 NOMCTA2 NOMCTA3 NOMCTA4 NOMCTA5
            END-IF
            GO TO I-DESCRIPCION-N6.

         I-DESCRIPCION-N1.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY NIVEL LINE 09 POSITION 52
           ACCEPT NOMCTA1 LINE 09 POSITION 13
                          PROMPT ECHO UPDATE NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA1 = SPACE GO TO I-DESCRIPCION-N1.
      *
         I-DESCRIPCION-N2.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           DISPLAY NIVEL LINE 09 POSITION 52
           ACCEPT NOMCTA2 LINE 09 POSITION 14
                  PROMPT ECHO UPDATE NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA2 = SPACE GO TO I-DESCRIPCION-N2.
      *
         I-DESCRIPCION-N3.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           DISPLAY NIVEL LINE 09 POSITION 52
           ACCEPT NOMCTA3 LINE 09 POSITION 15
                  PROMPT ECHO UPDATE NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA3 = SPACE GO TO I-DESCRIPCION-N3.
      *
         I-DESCRIPCION-N4.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           DISPLAY NIVEL LINE 09 POSITION 52.
           ACCEPT NOMCTA4 LINE 09 POSITION 16
                  PROMPT ECHO UPDATE NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA4 = SPACE GO TO I-DESCRIPCION-N4.
      *
         I-DESCRIPCION-N5.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           DISPLAY NIVEL LINE 09 POSITION 52
           ACCEPT NOMCTA5 LINE 09 POSITION 17
                  PROMPT ECHO UPDATE NO BEEP
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA5 = SPACE GO TO I-DESCRIPCION-N5.
      *
         I-DESCRIPCION-N6.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           DISPLAY NIVEL LINE 09 POSITION 52
           ACCEPT NOMCTA6 LINE 09 POSITION 18
                  PROMPT ECHO UPDATE NO BEEP SIZE 33
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-DESCRIPCION.
           IF NOMCTA6 = SPACES GO TO I-DESCRIPCION-N6.
       F-DESCRIPCION.
      *
       I-ENCABEZADO.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "TIT=Titulo o Encabezado-->NO Permitira Movimiento  "
                   LINE 24 POSITION 01
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "S" TO ESTACTA.
           DISPLAY ESTACTA  LINE 09 POSITION 54
           ACCEPT  ESTACTA  LINE 09 POSITION 54
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY ESTACTA  LINE 09 POSITION 54
           IF CUP  OR ESC GO F-ENCABEZADO.
           IF ESTACTA = "S" OR ESTACTA = "N"
              NEXT SENTENCE
           ELSE
              GO   I-ENCABEZADO.
       F-ENCABEZADO.
      *
       I-ACUM-TERC.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "TER=Activa Acumulado Historico por Terceros        "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO CODPGT
              DISPLAY CODPGT   LINE 09 POSITION 56
              GO F-ACUM-TERC.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "S" TO CODPGT.
           DISPLAY CODPGT   LINE 09 POSITION 56
           ACCEPT  CODPGT   LINE 09 POSITION 56
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY CODPGT   LINE 09 POSITION 56
           IF CUP OR ESC GO   F-ACUM-TERC.
           IF CODPGT = "S" OR CODPGT = "N"
              NEXT SENTENCE
           ELSE
              GO   I-ACUM-TERC.
       F-ACUM-TERC.
      *
       I-ACUM-FRA.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "DET=Activa Acumulado Historico por Factura         "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDDETAL
              DISPLAY INDDETAL LINE 09 POSITION 58
              GO F-ACUM-FRA.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "S" TO INDDETAL.
           DISPLAY INDDETAL LINE 09 POSITION 58
           ACCEPT  INDDETAL LINE 09 POSITION 58
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDDETAL LINE 09 POSITION 58
           IF CUP OR ESC GO F-ACUM-FRA.
           IF INDDETAL = "S" OR INDDETAL = "N"
              NEXT SENTENCE
           ELSE
              GO   I-ACUM-FRA.
       F-ACUM-FRA.
      *
       I-INDICE-PROV.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "DET=Activa Acumulado Historico de Provisiones      "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-PROV
              DISPLAY INDICE-PROV LINE 09 POSITION 60
              GO F-INDICE-PROV.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-PROV.
           DISPLAY INDICE-PROV LINE 09 POSITION 60
           ACCEPT  INDICE-PROV LINE 09 POSITION 60
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-PROV LINE 09 POSITION 60
           IF CUP OR ESC GO F-INDICE-PROV.
           IF INDICE-PROV = "S" OR INDICE-PROV = "N"
              NEXT SENTENCE
           ELSE
              GO   I-INDICE-PROV.
           IF INDICE-PROV = "S"
            PERFORM I-RECIBE-CTADEBCRE THRU F-RECIBE-CTADEBCRE
            PERFORM I-VENTANA-CONCACTA THRU F-VENTANA-CONCACTA
            MOVE 09 TO I-LINEA
            PERFORM I-MOSTRAR-CNCATCTA THRU F-MOSTRAR-CNCATCTA.
       F-INDICE-PROV.
      *
       I-RECIBE-CTADEBCRE.
           PERFORM I-VENTANA-CTADEBCRE THRU F-VENTANA-CTADEBCRE.
           PERFORM I-BUSCA-PRC-PROV    THRU F-BUSCA-PRC-PROV.
         I-DEB.
           PERFORM I-CTADEB-PROV THRU F-CTADEB-PROV.
           IF CUP GO TO F-RECIBE-CTADEBCRE.
           PERFORM I-CTACRE-PROV THRU F-CTACRE-PROV.
           IF CUP GO TO I-DEB.
       F-RECIBE-CTADEBCRE. EXIT.
      *
       I-VENTANA-CTADEBCRE.
           DISPLAY
           "ษออออออออออออออหออออออออออออออหออออออออออออออหออออออออออออป"
           LINE 10 POSITION 10 REVERSE
           "บ Cuenta Prov. บ Cuenta Prov. บ % Aportes    บ % Prestac. บ"
           LINE 11 POSITION 10 REVERSE
           "บ   Debito     บ   Credito    บ Parafiscales บ  Sociales  บ"
           LINE 12 POSITION 10 REVERSE
           "ฬออออออออออออออฮออออออออออออออฮออออออออออออออฮออออออออออออน"
           LINE 13 POSITION 10 REVERSE
           "บ              บ              บ              บ            บ"
           LINE 14 POSITION 10 REVERSE
           "ศออออออออออออออสออออออออออออออสออออออออออออออสออออออออออออผ"
           LINE 15 POSITION 10 REVERSE.
       F-VENTANA-CTADEBCRE. EXIT.
      *
       I-BUSCA-PRC-PROV.
           MOVE 1 TO CIUD-CODIGO.
           OPEN INPUT NOMICIUD.
           READ NOMICIUD WITH NO LOCK INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "NO EXISTE LA CIUDAD 1 "
            LINE 25 POSITION 01 BEEP
            PERFORM ACEPTA-TRUCO
            GO TO F-BUSCA-PRC-PROV.
           CLOSE NOMICIUD.
           MOVE ZEROS TO WRK-PRC-ACU-PROV-PF WRK-PRC-ACU-PROV-PS.
           COMPUTE WRK-PRC-ACU-PROV-PF = CIUD-PRC-PROV-ICBF +
                                         CIUD-PRC-PROV-SENA +
                                         CIUD-PRC-PROV-SUFA +
                                         CIUD-PRC-PROV-RIPR +
                                         CIUD-PRC-PENSION-PATRONO +
                                         CIUD-PRC-SALUD-PATRONO.
           COMPUTE WRK-PRC-ACU-PROV-PS = CIUD-PRC-PROV-CESA +
                                         CIUD-PRC-PROV-INCE +
                                         CIUD-PRC-PROV-VACA +
                                         CIUD-PRC-PROV-PRIM.
           MOVE WRK-PRC-ACU-PROV-PF TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 14 POSITION 44.
           MOVE WRK-PRC-ACU-PROV-PS TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 14 POSITION 58.
           DISPLAY CTADEB-PROV LINE 14 POSITION 12.
           DISPLAY CTACRE-PROV LINE 14 POSITION 27.
       F-BUSCA-PRC-PROV. EXIT.
      *
       I-CTADEB-PROV.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CTADEB-PROV LINE 14 POSITION 12
           ACCEPT CTADEB-PROV LINE 14 POSITION 12
                   PROMPT ECHO NO BEEP UPDATE TAB
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY CTADEB-PROV LINE 14 POSITION 12
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-CTADEB-PROV.
           IF CTADEB-PROV = ZEROS GO TO I-CTADEB-PROV.
       F-CTADEB-PROV. EXIT.
      *
       I-CTACRE-PROV.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CTACRE-PROV LINE 14 POSITION 27
           ACCEPT CTACRE-PROV LINE 14 POSITION 27
                   PROMPT ECHO NO BEEP UPDATE TAB
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY CTACRE-PROV LINE 14 POSITION 27
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO TO F-CTACRE-PROV.
           IF CTACRE-PROV = ZEROS GO TO I-CTACRE-PROV.
       F-CTACRE-PROV. EXIT.
      *
       I-INDICE-AJINF.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "DET=Activa Acumulado Historico Ajustes Inflacion   "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-AJINF
              DISPLAY INDICE-AJINF LINE 09 POSITION 62
              GO F-INDICE-AJINF.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-AJINF.
           DISPLAY INDICE-AJINF LINE 09 POSITION 62.
           ACCEPT  INDICE-AJINF LINE 09 POSITION 62
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-AJINF LINE 09 POSITION 62
           IF CUP OR ESC GO F-INDICE-AJINF.
           IF INDICE-AJINF = "S" OR INDICE-AJINF = "N"
              NEXT SENTENCE
           ELSE
              GO  I-INDICE-AJINF.
       F-INDICE-AJINF.

       I-APLICA-GRAV.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "BAS=Activa Acumulado Base Gravable para Impuestos  "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDBASE
              DISPLAY INDBASE  LINE 09 POSITION 64
              GO F-APLICA-GRAV.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDBASE.
           DISPLAY INDBASE  LINE 09 POSITION 64
           ACCEPT  INDBASE  LINE 09 POSITION 64
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDBASE  LINE 09 POSITION 64
           IF CUP OR ESC GO F-APLICA-GRAV.
           IF INDBASE = "S" OR INDBASE = "N"
              NEXT SENTENCE
           ELSE
              GO   I-APLICA-GRAV.
       F-APLICA-GRAV.
      *
       I-PORC-GRAV.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "% BSGB=Activa Porcentaje Base Gravable             "
                   LINE 24 POSITION 01
           IF INDBASE = "N"
              DISPLAY PORC-CTA LINE 09 POSITION 66
              MOVE ZEROS TO PORC-CTA
              GO   F-PORC-GRAV.
           MOVE    ZEROS    TO  WK-EXCEPTION.
           DISPLAY PORC-CTA LINE 09 POSITION 66
           ACCEPT  PORC-CTA LINE 09 POSITION 66
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY PORC-CTA LINE 09 POSITION 66
           IF CUP OR ESC GO F-PORC-GRAV.
           IF PORC-CTA NOT NUMERIC OR PORC-CTA = ZEROS
              GO   I-PORC-GRAV.
       F-PORC-GRAV.
      *
       I-CARTERA.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "CAR=Activa Acumulado Historico Deudores Nacional   "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-CAR
              DISPLAY INDICE-CAR LINE 09 POSITION 69
              GO F-CARTERA.
           MOVE    ZEROS      TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-CAR.
           DISPLAY INDICE-CAR LINE 09 POSITION 69
           ACCEPT  INDICE-CAR LINE 09 POSITION 69
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-CAR LINE 09 POSITION 69
           IF CUP OR ESC GO F-CARTERA.
           IF INDICE-CAR = "S" OR INDICE-CAR = "N"
              NEXT SENTENCE
           ELSE
              GO   I-CARTERA.
       F-CARTERA.
      *
       I-BANCOS.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "BCO=Activa Acumulado Historico MODULO DE BANCOS    "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-BCOS
              DISPLAY INDICE-BCOS LINE 09 POSITION 71
              GO F-BANCOS.
           MOVE    ZEROS       TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-BCOS.
           DISPLAY INDICE-BCOS LINE 09 POSITION 71
           ACCEPT  INDICE-BCOS LINE 09 POSITION 71
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-BCOS LINE 09 POSITION 71
           IF CUP OR ESC GO F-BANCOS.
           IF INDICE-BCOS = "S" OR INDICE-BCOS = "N"
              NEXT SENTENCE
           ELSE
              GO   I-BANCOS.
       F-BANCOS.
      *
       I-RTEFTE.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "CAV=Activa Acumulado Historico MODULO CORP.AHORR   "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-RET
              DISPLAY INDICE-RET   LINE 09 POSITION 73
              GO F-RTEFTE.
           MOVE    ZEROS       TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-RET.
           DISPLAY INDICE-RET  LINE 09 POSITION 73
           ACCEPT  INDICE-RET  LINE 09 POSITION 73
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-RET  LINE 09 POSITION 73
           IF CUP OR ESC GO F-RTEFTE.
           IF INDICE-RET = "S" OR INDICE-RET = "N"
              NEXT SENTENCE
           ELSE
              GO   I-RTEFTE.
       F-RTEFTE.
      *
       I-ANEXOS.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "ANX=Activa Acumulado Historico MODULO ANEXOS       "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO INDICE-ANEX
              DISPLAY INDICE-ANEX  LINE 09 POSITION 75
              GO F-ANEXOS.
           MOVE    ZEROS       TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO INDICE-ANEX.
           DISPLAY INDICE-ANEX LINE 09 POSITION 75
           ACCEPT  INDICE-ANEX LINE 09 POSITION 75
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY INDICE-ANEX LINE 09 POSITION 75
           IF CUP OR ESC GO F-ANEXOS.
           IF INDICE-ANEX = "S" OR INDICE-ANEX = "N"
              NEXT SENTENCE
           ELSE
              GO   I-ANEXOS.
       F-ANEXOS.
      *
       I-PERDIDAS.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "PER=Activa Cuenta Contable Cuenta de Perdidas      "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO IND-PYG-PER
              DISPLAY IND-PYG-PER  LINE 09 POSITION 77
              GO F-PERDIDAS.
           MOVE    ZEROS       TO  WK-EXCEPTION.
           DISPLAY IND-PYG-PER LINE 09 POSITION 77
           IF W-OPCION = "A" MOVE "N" TO IND-PYG-PER.
           ACCEPT  IND-PYG-PER LINE 09 POSITION 77
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY IND-PYG-PER LINE 09 POSITION 77
           IF CUP OR ESC GO F-PERDIDAS.
           IF IND-PYG-PER = "S" OR IND-PYG-PER = "N"
              NEXT SENTENCE
           ELSE
              GO   I-PERDIDAS.
       F-PERDIDAS.
      *
       I-UTILIDAD.
           DISPLAY SPACES LINE 24 POSITION 1
           DISPLAY SPACES LINE 25 POSITION 1
           DISPLAY "UTI=Activa Cuenta Contable Cuenta de Utilidades    "
                   LINE 24 POSITION 01
           IF ESTACTA = "S"
              MOVE "N" TO IND-PYG-UTI
              DISPLAY IND-PYG-UTI  LINE 09 POSITION 79
              GO F-UTILIDAD.
           MOVE    ZEROS       TO  WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO IND-PYG-UTI.
           DISPLAY IND-PYG-UTI LINE 09 POSITION 79
           ACCEPT  IND-PYG-UTI LINE 09 POSITION 79
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY IND-PYG-UTI LINE 09 POSITION 79
           IF CUP OR ESC GO F-UTILIDAD.
           IF IND-PYG-UTI = "S" OR IND-PYG-UTI = "N"
              NEXT SENTENCE
           ELSE
              GO   I-UTILIDAD.
       F-UTILIDAD.
      *
       I-DATOS.
           PERFORM I-DESCRIPCION THRU F-DESCRIPCION.
           IF CUP OR ESC GO F-DATOS.
         I-D1.
           PERFORM I-ENCABEZADO  THRU F-ENCABEZADO.
                   IF CUP GO I-DATOS.
                   IF ESC GO F-DATOS.
         I-D2.
           PERFORM I-ACUM-TERC   THRU F-ACUM-TERC.
                   IF CUP GO I-D1.
                   IF ESC GO F-DATOS.
         I-D3.
           PERFORM I-ACUM-FRA    THRU F-ACUM-FRA.
                   IF CUP GO I-D2.
                   IF ESC GO F-DATOS.
         I-D4.
           PERFORM I-INDICE-PROV  THRU F-INDICE-PROV.
                   IF CUP GO I-D3.
                   IF ESC GO F-DATOS.
         I-D5.
           PERFORM I-INDICE-AJINF  THRU F-INDICE-AJINF.
                   IF CUP GO I-D4.
                   IF ESC GO F-DATOS.
         I-D6.
           PERFORM I-APLICA-GRAV THRU F-APLICA-GRAV.
                   IF CUP GO I-D5.
                   IF ESC GO F-DATOS.
         I-D7.
           PERFORM I-PORC-GRAV   THRU F-PORC-GRAV.
                   IF CUP GO I-D6.
                   IF ESC GO F-DATOS.
         I-D8.
           PERFORM I-CARTERA     THRU F-CARTERA.
                   IF CUP GO I-D7.
                   IF ESC GO F-DATOS.
         I-D9.
           PERFORM I-BANCOS      THRU F-BANCOS.
                   IF CUP GO I-D8.
                   IF ESC GO F-DATOS.
         I-D10.
           PERFORM I-RTEFTE      THRU F-RTEFTE.
                   IF CUP GO I-D9.
                   IF ESC GO F-DATOS.
         I-D11.
           PERFORM I-ANEXOS      THRU F-ANEXOS.
                   IF CUP GO I-D10.
                   IF ESC GO F-DATOS.
         I-D12.
           PERFORM I-PERDIDAS    THRU F-PERDIDAS.
                   IF CUP GO I-D11.
                   IF ESC GO F-DATOS.
         I-D13.
           PERFORM I-UTILIDAD    THRU F-UTILIDAD.
                   IF CUP GO I-D12.
                   IF ESC GO F-DATOS.
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
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO.
       F-NEXISTE.
      *
       I-WINDOW-DATOS.
           DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออออออ"
      -    "ออออออออป"  LINE 06 POSITION 10.
           DISPLAY "บ Codigo      :                                     "
      -    "        บ"  LINE 09 POSITION 10.
           DISPLAY "บ Descripcion :                                     "
      -    "        บ"  LINE 08 POSITION 10.
           DISPLAY "บ Encabezados :                                     "
      -    "        บ"  LINE 09 POSITION 10.
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ"
      -    "ออออออออน"  LINE 10 POSITION 10.
           DISPLAY "บ                       Datos  Generales            "
      -    "        บ"  LINE 11 POSITION 10.
           DISPLAY "ฬออออออออออออออออออออออออออออออออัออออออออออออออออออ"
      -    "ออออออออน"  LINE 12 POSITION 10.
           DISPLAY "บ                                ณ                  "
      -    "        บ"  LINE 13 POSITION 10.
           DISPLAY "บ Acum. Saldos x Terceros  :  _  ณ Saldos   Cartera "
      -    " :  _   บ"  LINE 14 POSITION 10.
           DISPLAY "บ Acum. Saldos x Fra       :  _  ณ Saldos   Bancos  "
      -    " :  _   บ"  LINE 15 POSITION 10.
           DISPLAY "บ Aplica a Provisiones     :  _  ณ Saldos   C.A.V.  "
      -    " :  _   บ"  LINE 16 POSITION 10.
           DISPLAY "บ Aplica ajustes-inflacion :     ณ Otros            "
      -    " :  _   บ"  LINE 17 POSITION 10.
           DISPLAY "บ                          :     ณ                  "
      -    "        บ"  LINE 18 POSITION 10.
           DISPLAY "บ                          :     ณ                  "
      -    "        บ"  LINE 19 POSITION 10.
           DISPLAY "บ Aplica Base Gravable     :  _  ณ Cuenta Perdida   "
      -    " :  _   บ"  LINE 20 POSITION 10.
           DISPLAY "บ %  Aplica Base Grav.     :  _  ณ Cuenta Utilidad  "
      -    " :  _   บ"  LINE 21 POSITION 10.
           DISPLAY "ศออออออออออออออออออออออออออออออออฯออออออออออออออออออ"
      -    "ออออออออผ"  LINE 22 POSITION 10.
           DISPLAY "*Acumuladores*"        LINE 13 POSITION 18.
           DISPLAY "*Saldos Gerencia*"     LINE 13 POSITION 47.
           DISPLAY " *Retefuente* "        LINE 19 POSITION 18.
           DISPLAY "*Estados Financieros*" LINE 19 POSITION 47.

           MOVE "LOW" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY "Codigos"                    LINE 07 POSITION 12.
           DISPLAY "Descripcion"                LINE 08 POSITION 12.
           DISPLAY "Encabezados"                LINE 09 POSITION 12.
           DISPLAY "Acum. Saldos x Terceros"    LINE 14 POSITION 12.
           DISPLAY "Saldos   Cartera"           LINE 14 POSITION 45.
           DISPLAY "Acum. Saldos x Fra"         LINE 15 POSITION 12.
           DISPLAY "Saldos   Bancos"            LINE 15 POSITION 45.
           DISPLAY "Saldos   C.A.V "            LINE 16 POSITION 45.
           DISPLAY "Otros       "               LINE 17 POSITION 45.
           DISPLAY "Aplica Base Gravable"       LINE 20 POSITION 12.
           DISPLAY "Cuenta Perdida"             LINE 20 POSITION 45.
           DISPLAY "%  Aplica Base Grav."       LINE 21 POSITION 12.
           DISPLAY "Cuenta Utilidad"            LINE 21 POSITION 45.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY " Nivel :   "         LINE 09 POSITION 44 REVERSE.
           DISPLAY " Grupo :   "         LINE 09 POSITION 57 REVERSE.
           DISPLAY "[Mayor-Subcta-Aux1-Aux2]"
                         LINE 07 POSITION 42.
       F-WINDOW-DATOS.
      *
       I-VENTANA-CONCACTA.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80
           CONTROL SG-CONTROL-STRING
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY "ฬออออออออออหออออออออออออออออออออออออออออออออออออออหอ"
      -    "หอหอหอหอหอหอหออหอหอหอหอหอออน"  LINE 04 POSITION 01
           DISPLAY "บ CODIGO   บ                                      บN"
      -    "บTบTบDบPบIบBบ %บCบBบCบAบP Uบ"  LINE 05 POSITION 01
           DISPLAY "บ CUENTA   บ     NOMBRE DE CUENTA                 บI"
      -    "บIบEบEบRบNบAบBSบAบCบAบNบE Tบ"  LINE 06 POSITION 01
           DISPLAY "บ P.U.C.   บ                                      บV"
      -    "บTบRบTบOบFบSบGBบRบOบVบXบR Iบ"  LINE 07 POSITION 01
           DISPLAY "ฬออออออออออฮออออออออออออออออออออออออออออออออออออออฮอ"
      -    "ฮอฮอฮอฮอฮอฮอฮออฮอฮอฮอฮอฮอออน"  LINE 08 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 09 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 10 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 11 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 12 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 13 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 14 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 15 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 16 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 17 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 18 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 19 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 20 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 21 POSITION 01
           DISPLAY "บ          บ                                      บ "
      -    "บ บ บ บ บ บ บ  บ บ บ บ บ   บ"  LINE 22 POSITION 01
           DISPLAY "ศออออออออออสออออออออออออออออออออออออออออออออออออออสอ"
      -    "สอสอสอสอสอสอสออสอสอสอสอสอออผ"  LINE 23 POSITION 01.
       F-VENTANA-CONCACTA.

       I-BLANQUEA.
           DISPLAY SPACE LINE I-LINEA POSITION J-LINEA SIZE W-LONG.
       F-BLANQUEA.
      *
       I-DISPLAY-OPCION.
           IF  W-OPCION = "A"
               DISPLAY "Adicion"   LINE  02 POSITION 36.
           IF  W-OPCION = "M"
               DISPLAY "Modificar"   LINE  02 POSITION 36.
           IF  W-OPCION = "C"
               DISPLAY "Consultar"   LINE  02 POSITION 36.
           IF  W-OPCION = "R"
               DISPLAY "Retirar "   LINE  02 POSITION 36.
       F-DISPLAY-OPCION.
      *
       I-FIN-MODE.
           CLOSE CNCATCTA.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-LEE-CATCTA.
           MOVE ZEROS    TO SW-1.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                MOVE  01 TO SW-1.
       F-LEE-CATCTA.
      *
       I-LEE-SIGUIENTE.
           MOVE ZEROS TO SW-1.
           READ CNCATCTA NEXT RECORD WITH NO LOCK AT END
                MOVE  01 TO SW-1.
       F-LEE-SIGUIENTE.
      *
       I-START-CTAS.
           START CNCATCTA KEY > CTAMAE
                 INVALID  KEY STOP " ".
       F-START-CTAS.
      *
       I-GRABAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [ G ]  Grabar Registro     [  ]  Continua        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE CNMAES INVALID KEY STOP " ".
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [ G ]  Grabar Registro     [  ]  Continua        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE CNMAES INVALID KEY STOP " ".
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [HOME] BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF HOME
              DELETE CNCATCTA INVALID KEY STOP " ".
           DISPLAY SPACE LINE 25 POSITION 10 SIZE 60.
       F-ELIMINAR.
      *
       I-GRUPO-WINDOW.
           MOVE "HIGH" TO SG-INTENSITY
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY "ษออออออออออออออออออออออออป"      LINE 10 POSITION 23
                                       CONTROL SG-CONTROL-STRING REVERSE
           DISPLAY "บ       Grupos           บ"      LINE 11 POSITION 23
                                                                 REVERSE
           DISPLAY "ฬออออออออออออออออออออออออน"      LINE 12 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ1 Activo              บ"      LINE 13 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ2 Pasivo              บ"      LINE 14 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ3 Patrimonio          บ"      LINE 15 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ4 Ingresos            บ"      LINE 16 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ5 Gastos              บ"      LINE 17 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ6 Costos de ventas    บ"      LINE 18 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ7 Costos Servicios    บ"      LINE 19 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ8 Cuentas de Orden    บ"      LINE 20 POSITION 23
                                                                 REVERSE
           DISPLAY "บ ณ9 Ctas orden x contra บ"      LINE 21 POSITION 23
                                                                 REVERSE
           DISPLAY "ศออออออออออออออออออออออออผ"     LINE 22 POSITION 23
                                                                REVERSE.
       F-GRUPO-WINDOW.
      *
       I-STATUS-CNCATCTA.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNCATCTA
               MOVE 1                      TO  SW-ABRE
               IF  CNCATCTA-STATUS              =  "35"
                   OPEN OUTPUT CNCATCTA
               ELSE
                   IF  CNCATCTA-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNCATCTA
                       PERFORM I-FIN-MODE      THRU F-FIN-MODE.
               CLOSE CNCATCTA.
       F-STATUS-CNCATCTA.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\AUDITE.PRO".
      *        COPY "..\PRO\CONCACTA.PRO".
      ******************************************************************


