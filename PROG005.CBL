       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG005.
      ******************************************************************
      * MODULO DE USUARIOS    MAYO 01 DE 2002                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\MODULPRG.DYC".
               COPY "..\DYC\PROGRAMA.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\MODULPRG.FD".
               COPY "..\FD\PROGRAMA.FD".
       WORKING-STORAGE SECTION.
       77      ID-PRG           PIC X(7)  VALUE "PROG005".
       77      W-PROGRAMA       PIC X(7)  VALUE "PROG005".
       77      NOM-CTG          PIC X(25) VALUE SPACE.
       77      WK-USER1         PIC X(3)  VALUE ZEROS.
       77      WK-USER2         PIC X(3)  VALUE ZEROS.
       77      WK-PRG1          PIC X(10) VALUE SPACES.
       77      WK-PRG2          PIC X(10) VALUE SPACES.

               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\MODULPRG.LBL".
               COPY "..\LBL\PROGRAMA.LBL".
         01      LINEA-DETALLE.
            03   FILLER           PIC X(10) VALUE SPACES.
            03   I-CODIGO         PIC X(03) VALUE SPACES.
            03   FILLER           PIC X(10) VALUE SPACES.
            03   I-PROGRAMA       PIC X(08) VALUE SPACES.

         01      I-USUARIO.
            03   I-USU1           PIC X(02) VALUE SPACES.
            03   I-USU2           PIC X(01) VALUE SPACES.


               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\MODULPRG.DCL".
               COPY "..\DCL\PROGRAMA.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
               MOVE "WINNER FINANCIALฉ"    TO  WK-SISTEMA.
               MOVE "  USER SISTEM    "    TO  TIT-APLICACION.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN I-O USUARIOS.
               OPEN INPUT MODULPRG.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 998             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 998             TO   WK-EXCEPTION
              PERFORM I-WINDOW-FIN THRU F-WINDOW-FIN
              PERFORM I-SALIR      THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM  I-FIN-MODE  THRU F-FIN-MODE
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
             MOVE "HIGH" TO SG-INTENSITY
             MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
             MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
             DISPLAY " " LINE 24 POSITION 80
             CONTROL SG-CONTROL-STRING
             IF W-OPCION = "A"
                MOVE  1  TO SW-MODE
                MOVE  SPACE TO REG-USER
                PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                PERFORM  I-DISPLAY        THRU F-DISPLAY
                PERFORM  I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M"
                  MOVE  2  TO SW-MODE
                  MOVE  SPACE TO REG-USER
                  PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                  PERFORM  I-DISPLAY        THRU F-DISPLAY
                  PERFORM  I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                           UNTIL ESC OR CUP
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C"
                   MOVE  SPACE TO REG-USER
                   PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                   PERFORM  I-DISPLAY        THRU F-DISPLAY
                   PERFORM  I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R"
                      MOVE  SPACE TO REG-USER
                      PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                      PERFORM  I-DISPLAY        THRU F-DISPLAY
                      PERFORM  I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
                      PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM  I-ELIMINA        THRU F-ELIMINA
                               UNTIL ESC OR CUP
                      MOVE ZEROS TO WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-CREAR.
           MOVE 1 TO SW-MODE.
           PERFORM I-COD-USUARIO THRU F-COD-USUARIO.
           IF CUP OR ESC GO F-CREAR.
           MOVE SPACES TO COD-PRG.
           READ USUARIOS WITH NO LOCK INVALID KEY
                PERFORM I-CREA-USUARIO THRU F-CREA-USUARIO
                IF CUP
                   GO I-CREAR
                END-IF.
           PERFORM I-DISPLAY THRU F-DISPLAY.
         I-CR1.
           PERFORM I-PROGRAMA THRU F-PROGRAMA.
           IF CUP
              PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS
              PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
              GO I-CREAR.
           IF ESC GO F-CREAR.
           PERFORM I-LEE-USUARIOS THRU F-LEE-USUARIOS.
           IF  SW-1  = ZEROS
               PERFORM I-EXISTE THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CR1
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               MOVE ZEROS         TO   WK-EXCEPTION
               PERFORM I-BLANQUEA THRU F-BLANQUEA
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP OR ESC      GO   I-CR1.
           MOVE COD-USR TO WK-USER2.
           MOVE SPACE TO REG-USER.
           MOVE WK-USER2 TO COD-USR.
      *    PERFORM I-DISPLAY        THRU F-DISPLAY.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           GO I-CR1.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-COD-USUARIO     THRU F-COD-USUARIO.
           DISPLAY SPACES LINE 24 POSITION 40 SIZE 40.
           IF CUP OR ESC GO F-ELIMINA.
           PERFORM  I-DISPLAY        THRU F-DISPLAY.
       I-ELIMINA-PROGRAMAS.
           PERFORM I-PROGRAMA           THRU F-PROGRAMA.
           IF CUP  GO I-ELIMINA.
           IF ESC  GO F-ELIMINA.
           PERFORM I-LEE-USUARIOS THRU F-LEE-USUARIOS.
           IF  SW-1  =  1
               PERFORM I-NEXISTE THRU F-NEXISTE
               GO I-ELIMINA-PROGRAMAS
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           PERFORM I-DISPLAY        THRU F-DISPLAY
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           GO I-ELIMINA-PROGRAMAS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           PERFORM I-COD-USUARIO THRU F-COD-USUARIO.
           IF CUP OR ESC GO F-MODIFY-MODE.
           MOVE SPACES TO COD-PRG.
           READ USUARIOS WITH NO LOCK INVALID KEY
                DISPLAY " USUARIO NO EXISTE " LINE 24 POSITION 01
                GO I-MODIFY-MODE.
           PERFORM I-DISPLAY THRU F-DISPLAY.
           DISPLAY SPACE LINE 24 POSITION 01 REVERSE.
         I-MM1.
           PERFORM I-PROGRAMA THRU F-PROGRAMA.
           IF F10
              PERFORM I-MODIF-USER THRU F-MODIF-USER
              GO I-MM1.
           IF CUP
              PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS
              PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
              GO I-MODIFY-MODE.
           IF ESC GO F-MODIFY-MODE.
           PERFORM I-LEE-USUARIOS THRU F-LEE-USUARIOS.
           IF  SW-1  = 1
               PERFORM I-NEXISTE THRU F-NEXISTE
               GO I-MM1
           ELSE
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               MOVE ZEROS         TO   WK-EXCEPTION
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP OR ESC      GO   I-MM1.
           MOVE COD-USR TO WK-USER2.
           MOVE SPACE TO REG-USER.
           MOVE WK-USER2 TO COD-USR.
           PERFORM I-DISPLAY        THRU F-DISPLAY
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           GO I-MM1.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-COD-USUARIO     THRU F-COD-USUARIO.
           DISPLAY SPACES LINE 24 POSITION 40 SIZE 40.
           IF      CUP          GO   F-CONSUL-MODE.
           IF      ESC          GO   F-CONSUL-MODE.
           PERFORM I-HELP-PRG  THRU  F-HELP-PRG UNTIL ESC.
           MOVE SPACE TO REG-USER.
           PERFORM I-DISPLAY        THRU F-DISPLAY
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
       F-CONSUL-MODE.
      *
       I-COD-USUARIO.
           MOVE   ZEROS TO WK-EXCEPTION.
           DISPLAY " [ F1 ] HELP " LINE 24 POSITION 01
                      LOW BLINK REVERSE SIZE 13
           ACCEPT COD-USR LINE 10 POSITION 20
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC GO F-COD-USUARIO.
           IF  F1
             PERFORM I-HELP-USUARIOS  THRU F-HELP-USUARIOS UNTIL ESC
             MOVE WK-USER1 TO COD-USR
             PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
             PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
             GO I-COD-USUARIO.
           IF COD-USR = SPACES GO I-COD-USUARIO.
           MOVE COD-USR  TO WK-USER1.
           MOVE COD-PRG  TO WK-PRG1.
       F-COD-USUARIO.
      *
       I-PROGRAMA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            CONTROL SG-CONTROL-STRING.
           DISPLAY "<F3> AYUDA USUARIOS ASIGNADOS AL PROGRAMA"
            LINE 25 POSITION 01.

           MOVE  ZEROS TO WK-EXCEPTION.
           DISPLAY COD-USR LINE 10 POSITION 20
           DISPLAY COD-PRG LINE 19 POSITION 07.
           ACCEPT  COD-PRG LINE 19 POSITION 07
                   PROMPT ECHO NO BEEP UPDATE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY COD-PRG LINE 19 POSITION 07.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.

           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP OR ESC OR F10 GO F-PROGRAMA.
           IF F1
              PERFORM I-HELP-PRG  THRU  F-HELP-PRG UNTIL ESC
              MOVE WK-PRG1 TO COD-PRG
              MOVE WK-USER1 TO COD-USR
              PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS
              PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
              GO I-PROGRAMA.

           IF F3
             MOVE COD-PRG TO WK-PRG1
             MOVE COD-USR TO WK-USER1
             PERFORM I-HELP-PRG-MS THRU F-HELP-PRG-MS UNTIL ESC
             MOVE WK-PRG1 TO COD-PRG
             MOVE WK-USER1 TO COD-USR
             PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS
             PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
             GO I-PROGRAMA.

           IF SF10 GO F-PROGRAMA.
           IF COD-PRG = SPACES GO I-PROGRAMA.
       F-PROGRAMA.
      *
       I-DATOS.
           PERFORM I-DESCRIPCION THRU F-DESCRIPCION.
           IF CUP GO F-DATOS.
         I-DTS1.
           PERFORM I-PROCESOS    THRU F-PROCESOS.
           IF CUP GO I-DATOS.
         I-DTS2.
           MOVE 1 TO J.
           PERFORM I-MODULOS     THRU F-MODULOS
           VARYING K FROM 65 BY 1 UNTIL K > 74.
           PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
           PERFORM  I-DISPLAY        THRU F-DISPLAY
           PERFORM  I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
           PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
           IF CUP GO I-DTS1.
           MOVE ZEROS  TO WK-EXCEPTION.
           MOVE SPACES TO W-TRUCO.
           IF SW-MODE = 1
              PERFORM I-GRABAR THRU F-GRABAR
              UNTIL W-TRUCO = "G" OR CDN OR CUP.
           IF SW-MODE = 2
              PERFORM I-REGRABAR THRU F-REGRABAR
              UNTIL W-TRUCO = "G" OR CDN OR CUP.
           IF CUP GO I-DTS2.
       F-DATOS.
      *
       I-DESCRIPCION.
           OPEN  I-O PROGRAMA.
           MOVE  COD-PRG TO COD-PROGRAMA.
           READ  PROGRAMA WITH NO LOCK INVALID KEY
                          MOVE SPACES TO NOM-PROGRAMA.
           IF    W-OPCION = "A"
                 MOVE  NOM-PROGRAMA TO NOM-USUARIO.
           MOVE  ZEROS TO WK-EXCEPTION.
           DISPLAY NOM-USUARIO LINE 19 POSITION 18.
           ACCEPT NOM-USUARIO LINE 19 POSITION 18
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF    NOM-PROGRAMA NOT =  NOM-USUARIO
                 MOVE  COD-PRG      TO   COD-PROGRAMA
                 MOVE  NOM-USUARIO  TO   NOM-PROGRAMA
                 PERFORM I-GRA-PRG  THRU F-GRA-PRG.
           CLOSE PROGRAMA.
           IF ESC OR CUP GO F-DESCRIPCION.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF NOM-USUARIO = SPACES GO I-DESCRIPCION.
       F-DESCRIPCION.
      *
       I-GRA-PRG.
           WRITE REG-PROGRAMA INVALID KEY GO I-RGRA-PRG.
           GO F-GRA-PRG.
         I-RGRA-PRG.
           REWRITE REG-PROGRAMA.
       F-GRA-PRG.
      *
       I-PROCESOS.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-X.
           DISPLAY PROSS-X LINE 19 POSITION 51.
           ACCEPT PROSS-X LINE 19 POSITION 51
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF ESC OR CUP GO F-PROCESOS.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-X = "N" OR PROSS-X = "S" NEXT SENTENCE
           ELSE GO I-PROCESOS.
         I-PA.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-A.
           DISPLAY PROSS-A LINE 19 POSITION 53.
           ACCEPT PROSS-A LINE 19 POSITION 53
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP GO I-PROCESOS.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-A = "N" OR PROSS-A = "S" NEXT SENTENCE
           ELSE GO I-PA.
         I-PM.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-M.
           DISPLAY PROSS-M LINE 19 POSITION 55.
           ACCEPT PROSS-M LINE 19 POSITION 55
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP GO I-PA.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-M = "N" OR PROSS-M = "S" NEXT SENTENCE
           ELSE GO I-PM.
         I-PC.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-C.
           DISPLAY PROSS-C LINE 19 POSITION 57.
           ACCEPT PROSS-C LINE 19 POSITION 57
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP GO I-PM.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-C = "N" OR PROSS-C = "S" NEXT SENTENCE
           ELSE GO I-PC.
         I-PR.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-R.
           DISPLAY PROSS-R LINE 19 POSITION 59.
           ACCEPT PROSS-R LINE 19 POSITION 59
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP GO I-PC.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-R = "N" OR PROSS-R = "S" NEXT SENTENCE
           ELSE GO I-PR.
         I-PL.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO PROSS-L.
           DISPLAY PROSS-L LINE 19 POSITION 61.
           ACCEPT PROSS-L LINE 19 POSITION 61
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP GO I-PR.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PROSS-L = "N" OR PROSS-L = "S" NEXT SENTENCE
           ELSE GO I-PL.
       F-PROCESOS.
      *
       I-MODULOS.
           PERFORM I-HELP-MODULOS THRU F-HELP-MODULOS.
           MOVE  ZEROS TO WK-EXCEPTION.
           IF W-OPCION = "A" MOVE "N" TO MOD-USR(J).
           DISPLAY MOD-USR(J) LINE 19 POSITION K.
           ACCEPT MOD-USR(J) LINE 19 POSITION K
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP IF K = 65 MOVE 75 TO K
                            GO F-MODULOS
                  ELSE      MOVE 1  TO J
                            MOVE 65 TO K
                            GO I-MODULOS.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF MOD-USR(J) = "N" OR MOD-USR(J) = "S" NEXT SENTENCE
           ELSE GO I-MODULOS.
           ADD 1 TO J.
       F-MODULOS.
      *
       I-HELP-MODULOS.
           MOVE COD-PRG TO COD-MODPRG.
           READ MODULPRG WITH NO LOCK INVALID KEY GO F-HELP-MODULOS.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 20 REVERSE.
           DISPLAY "บ            P R O C E S S                 บ"
                                        LINE 07 POSITION 20 REVERSE.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 08 POSITION 20 REVERSE.
           DISPLAY "บณ 1                 ณ  6                 ณบ"
                                        LINE 09 POSITION 20 REVERSE.
           DISPLAY "บณ 2                 ณ  7                 ณบ"
                                        LINE 10 POSITION 20 REVERSE.
           DISPLAY "บณ 3                 ณ  8                 ณบ"
                                        LINE 11 POSITION 20 REVERSE.
           DISPLAY "บณ 4                 ณ  9                 ณบ"
                                        LINE 12 POSITION 20 REVERSE.
           DISPLAY "บณ 5                 ณ 10                 ณบ"
                                        LINE 13 POSITION 20 REVERSE.
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 14 POSITION 20 REVERSE.
           PERFORM I-MUESTRA-MODULOS THRU F-MUESTRA-MODULOS.
       F-HELP-MODULOS.
      *
       I-MUESTRA-MODULOS.
           MOVE    1   TO   X.
           MOVE    09  TO   I-LINEA.
           MOVE    23  TO   J-LINEA.
         LOOP1-MC.
           IF X > 10 GO F-MUESTRA-MODULOS.
           DISPLAY COD-MODULO(X) LINE I-LINEA POSITION J-LINEA.
           ADD 2 TO J-LINEA.
           DISPLAY NOM-MODULO(X) LINE I-LINEA POSITION J-LINEA.
           SUBTRACT 2 FROM J-LINEA.
           ADD 1 TO X.
           ADD 1 TO I-LINEA.
           IF I-LINEA = 14
              IF J-LINEA = 23  MOVE 09 TO I-LINEA
                               MOVE 44 TO J-LINEA
              ELSE             MOVE 09 TO J-LINEA
                               MOVE 23 TO J-LINEA.
           GO LOOP1-MC.
       F-MUESTRA-MODULOS.
      *
       I-MUESTRA-CAMPOS.
           DISPLAY COD-PRG LINE 19 POSITION 07.
           DISPLAY NOM-USUARIO LINE 19 POSITION 18.
           DISPLAY PROSS-X LINE 19 POSITION 51.
           DISPLAY PROSS-A LINE 19 POSITION 53.
           DISPLAY PROSS-M LINE 19 POSITION 55.
           DISPLAY PROSS-C LINE 19 POSITION 57.
           DISPLAY PROSS-R LINE 19 POSITION 59.
           DISPLAY PROSS-L LINE 19 POSITION 61.
           MOVE 1 TO J.
           MOVE 65 TO K.
         LOOP-IM.
           IF J > 10 GO F-MUESTRA-CAMPOS.
           DISPLAY MOD-USR(J) LINE 19 POSITION K.
           ADD 1 TO K
           ADD 1 TO J
           GO LOOP-IM.
       F-MUESTRA-CAMPOS.
      *
       I-DISPLAY.
           DISPLAY COD-USR     LINE 10 POSITION 20.
           DISPLAY "********"  LINE 10 POSITION 36.
           DISPLAY TIPO-USR    LINE 10 POSITION 57.
           DISPLAY NOM-USUARIO LINE 11 POSITION 20.
       F-DISPLAY.
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 25 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01.
       F-NEXISTE.
      *
       I-WINDOW-DATOS.
           PERFORM I-PANTA THRU F-PANTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "ออออออออออออออออป"
                                    LINE 07 POSITION 06
                                          CONTROL SG-CONTROL-STRING.
           DISPLAY "บ                          UPDATE USER SYSTEM
      -    "                บ"
                                    LINE 08 POSITION 06
           DISPLAY "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "ออออออออออออออออน"
                                    LINE 09 POSITION 06
           DISPLAY "บ User     :  ___     Key   : ________     Type :  _
      -    "                บ"
                                    LINE 10 POSITION 06
           DISPLAY "บ Name     :
      -    "                บ"
                                    LINE 11 POSITION 06
           DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "ออออออออออออออออผ"
                                    LINE 12 POSITION 06
           DISPLAY "ษออออออออออหออออออออออออออออออออออออออออออออหอออออออ
      -    "ออออออหออออออออออออป"   LINE 15 POSITION 05
           DISPLAY "บ          บ                                บ  Proce
      -    "ss    บ  Modules   บ"   LINE 16 POSITION 05
           DISPLAY "บ Program  บ       Description              บ X a m
      -    "c d p บ 1234567890 บ"   LINE 17 POSITION 05
           DISPLAY "ฬออออออออออฮออออออออออออออออออออออออออออออออฮอออออออ
      -    "ออออออฮออออออออออออน"   LINE 18 POSITION 05 .
           DISPLAY "บ          บ                                บ
      -    "      บ            บ"   LINE 19 POSITION 05 .
           DISPLAY "ศออออออออออสออออออออออออออออออออออออออออออออสอออออออ
      -    "ออออออสออออออออออออผ" LINE 20 POSITION 05   .
       F-WINDOW-DATOS.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "** ADD** "   LINE  03 POSITION 36
               LOW BLINK REVERSE.
           IF  W-OPCION = "M"
               DISPLAY "*MODIFY* "   LINE  03 POSITION 36
               LOW BLINK REVERSE.
           IF  W-OPCION = "C"
               DISPLAY "*CONSULT*"   LINE  03 POSITION 36
               LOW BLINK REVERSE.
           IF  W-OPCION = "R"
               DISPLAY "*DELETE* "   LINE  03 POSITION 36
               LOW BLINK REVERSE.
       F-DISPLAY-OPCION.
      *
       I-FIN-MODE.
           CLOSE USUARIOS MODULPRG
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-LEE-USUARIOS.
           MOVE ZEROS    TO SW-1.
           READ USUARIOS WITH NO LOCK INVALID KEY MOVE 01 TO SW-1.
       F-LEE-USUARIOS.
      *
       I-LEE-SIGUIENTE.
           MOVE ZEROS TO SW-1.
           READ USUARIOS NEXT RECORD WITH NO LOCK AT END
                                          MOVE 01 TO SW-1.
       F-LEE-SIGUIENTE.
      *
       I-START-CTAS.
           START USUARIOS KEY > LLAVE-USR
                 INVALID  KEY STOP " ".
       F-START-CTAS.
      *
       I-GRABAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                         LINE 24 POSITION 10 LOW.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-USER INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                            LINE 24 POSITION 10 LOW    .
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-USER INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-CONSULTA.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY " <> REGISTRO SIGUIENTE "
                     LINE 24 POSITION 01 LOW
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
           IF F2
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CDN
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF SW-1 = 0
              PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CONSULTA.
      *
       I-ELIMINAR.
           MOVE "HIGH" TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80
           CONTROL SG-CONTROL-STRING
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  CONTINUA        บ"
                        LINE 24 POSITION 10 LOW.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
           DELETE USUARIOS INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-ELIMINAR.
      *
       I-HELP-USUARIOS.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVE-USR FIN-OK WK-USER2 SW-1.
           PERFORM I-START-USER   THRU F-START-USER.
           PERFORM I-WINDOW-USER  THRU F-WINDOW-USER.
           PERFORM I-DISPLAY-USER THRU F-DISPLAY-USER
                   UNTIL I > 5.
         LEE-USER.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF UPDATE PROMPT
                          CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01 TO I
                MOVE 15 TO I-LINEA
                MOVE ZEROS TO FIN-OK SW-1
                MOVE WK-USER2          TO   COD-USR
                MOVE SPACES            TO   COD-PRG
                PERFORM I-START-USER   THRU F-START-USER
                PERFORM I-WINDOW-USER  THRU F-WINDOW-USER
                PERFORM I-DISPLAY-USER THRU F-DISPLAY-USER
                        UNTIL I > 5
                IF FIN-OK = 1
                   GO LEE-USER
                ELSE
                   GO LEE-USER.
       F-HELP-USUARIOS.
      *
       I-START-USER.
           START USUARIOS KEY NOT LESS LLAVE-USR.
       F-START-USER.
      *
       I-WINDOW-USER.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  "ษออออออหอออออออออออออออออออออออออออออออออออออป"
            LINE 12 POSITION 34 CONTROL SG-CONTROL-STRING REVERSE.
           DISPLAY  "บ COD. บ           USUARIO                   บ"
            LINE 13 POSITION 34 REVERSE.
           DISPLAY  "ฬออออออฮอออออออออออออออออออออออออออออออออออออน"
            LINE 14 POSITION 34 REVERSE.
           DISPLAY  "บ      บ                                     บ"
            LINE 15 POSITION 34 REVERSE.
           DISPLAY  "บ      บ                                     บ"
            LINE 16 POSITION 34 REVERSE.
           DISPLAY  "บ      บ                                     บ"
            LINE 17 POSITION 34 REVERSE.
           DISPLAY  "บ      บ                                     บ"
            LINE 18 POSITION 34 REVERSE.
           DISPLAY  "บ      บ                                    บ"
            LINE 19 POSITION 34 REVERSE.
           DISPLAY  "ศออออออสอออออออออออออออออออออออออออออออออออออผ"
            LINE 20 POSITION 34 REVERSE.
       F-WINDOW-USER. EXIT.
      *
       I-CREA-USUARIO.
           PERFORM I-PASUSR THRU F-PASUSR.
           IF CUP GO F-CREA-USUARIO.
         I-CU1.
           PERFORM I-TIPUSR THRU F-TIPUSR.
           IF CUP GO I-CREA-USUARIO.
         I-CU2.
           PERFORM I-NOMUSR THRU F-NOMUSR.
           IF CUP GO I-CU1.
           WRITE REG-USER.
       F-CREA-USUARIO.
      *
       I-MODIF-USER.
           PERFORM I-PASUSR THRU F-PASUSR.
           IF CUP GO F-MODIF-USER.
         I-MU1.
           PERFORM I-TIPUSR THRU F-TIPUSR.
           IF CUP GO I-MODIF-USER.
         I-MU2.
           PERFORM I-NOMUSR THRU F-NOMUSR.
           IF CUP GO I-MU1.
           REWRITE REG-USER.
       F-MODIF-USER.
      *
       I-NOMUSR.
           MOVE  ZEROS TO WK-EXCEPTION.
           DISPLAY NOM-USUARIO LINE 11 POSITION 20.
           ACCEPT NOM-USUARIO LINE 11 POSITION 20
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF ESC OR CUP GO F-NOMUSR.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF NOM-USUARIO = SPACES GO I-NOMUSR.
       F-NOMUSR.
      *
       I-PASUSR.
           MOVE  ZEROS TO WK-EXCEPTION.
           DISPLAY "********" LINE 10 POSITION 36.
           ACCEPT CLAVE-USR LINE 10 POSITION 36
                  PROMPT ECHO NO BEEP UPDATE OFF
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           DISPLAY "********" LINE 10 POSITION 36.
           IF ESC OR CUP GO F-PASUSR.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CLAVE-USR = SPACES GO I-PASUSR.
       F-PASUSR.
      *
       I-TIPUSR.
           MOVE "U" TO TIPO-USR.
           MOVE  ZEROS TO WK-EXCEPTION.
           DISPLAY TIPO-USR LINE 10 POSITION 57.
           ACCEPT TIPO-USR LINE 10 POSITION 57
                  PROMPT ECHO NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF ESC OR CUP GO F-TIPUSR.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF TIPO-USR = SPACES GO I-TIPUSR.
       F-TIPUSR.
      *JC
      * AYUDA PROGRAMA A QUE USUARIOS ESTA SIGNADO
       I-HELP-PRG-MS.
           MOVE 01               TO   I.
           MOVE 07               TO   I-LINEA.
           MOVE SPACES           TO   COD-PRG
                                      COD-USR.
           MOVE ZEROS            TO   FIN-OK WK-USER2 SW-1.
           PERFORM I-START-USER  THRU F-START-USER.
           PERFORM I-WINDOW-PRG-MS  THRU F-WINDOW-PRG-MS.
           PERFORM I-DISPLAY-PRG-MS THRU F-DISPLAY-PRG-MS
             UNTIL  I  > 10 OR SW-1 = 1.
           IF SW-1 = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            " Hasta aqui usuarios asignados al programa , <ESC> "
            LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO UNTIL ESC
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO F-HELP-PRG-MS.
         LEE-PRG-MS.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF UPDATE PROMPT
                          CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF   CDN
             MOVE  01        TO I
             MOVE  07        TO I-LINEA
             MOVE  ZEROS     TO FIN-OK SW-1
             MOVE  WK-USER2  TO COD-USR
             MOVE  WK-PRG2   TO COD-PRG
             PERFORM  I-START-USER   THRU  F-START-USER
             PERFORM  I-WINDOW-PRG-MS   THRU  F-WINDOW-PRG-MS
             PERFORM  I-DISPLAY-PRG-MS  THRU  F-DISPLAY-PRG-MS
               UNTIL I > 10 OR SW-1 = 1
            IF SW-1 = 1
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            " Hasta aqui usuarios asignados al    programa , <ESC> "
              LINE 25 POSITION 01
              PERFORM ACEPTA-TRUCO UNTIL ESC
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
              GO F-HELP-PRG-MS
            ELSE
              GO LEE-PRG-MS.
       F-HELP-PRG-MS. EXIT.
      *
       I-DISPLAY-PRG-MS.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.

           IF   SW-1 = 1
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 56 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-PRG-MS.

           IF   COD-PRG = SPACES
            DISPLAY
            NOM-USUARIO  LINE I-LINEA POSITION 25 SIZE 10
            GO I-DISPLAY-PRG-MS.

           IF   COD-PRG  = WK-PRG1 NEXT SENTENCE
           ELSE GO I-DISPLAY-PRG-MS.


           DISPLAY
            COD-PRG      LINE I-LINEA POSITION 37
            COD-USR      LINE I-LINEA POSITION 46
            PROSS-X      LINE I-LINEA POSITION 50
            PROSS-A      LINE I-LINEA POSITION 51 REVERSE
            PROSS-M      LINE I-LINEA POSITION 52
            PROSS-C      LINE I-LINEA POSITION 53 REVERSE
            PROSS-R      LINE I-LINEA POSITION 54
            PROSS-L      LINE I-LINEA POSITION 55 REVERSE
            MOD-USR (1)  LINE I-LINEA POSITION 57
            MOD-USR (2)  LINE I-LINEA POSITION 58 REVERSE
            MOD-USR (3)  LINE I-LINEA POSITION 59
            MOD-USR (4)  LINE I-LINEA POSITION 60 REVERSE
            MOD-USR (5)  LINE I-LINEA POSITION 61
            MOD-USR (6)  LINE I-LINEA POSITION 62 REVERSE
            MOD-USR (7)  LINE I-LINEA POSITION 63
            MOD-USR (8)  LINE I-LINEA POSITION 64 REVERSE
            MOD-USR (9)  LINE I-LINEA POSITION 65
            MOD-USR (10) LINE I-LINEA POSITION 66 REVERSE.

           IF I = 2  MOVE COD-USR  TO WK-USER2
                     MOVE COD-PRG  TO WK-PRG2.

           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-PRG-MS. EXIT.
      *
       I-HELP-PRG.
           MOVE 01               TO   I.
           MOVE 07               TO   I-LINEA.
           MOVE SPACES           TO   COD-PRG.
           MOVE COD-USR          TO   WK-USER1.
           MOVE ZEROS            TO   FIN-OK WK-USER2 SW-1.
           PERFORM I-START-USER  THRU F-START-USER.
           PERFORM I-WINDOW-PRG  THRU F-WINDOW-PRG.
           PERFORM I-DISPLAY-PRG THRU F-DISPLAY-PRG
             UNTIL  I  > 10 OR SW-1 = 1.
           MOVE WK-USER1 TO COD-USR.
           IF SW-1 = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            " Hasta aqui programas asignados a este usuario, <ESC> "
            LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO UNTIL ESC
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO F-HELP-PRG.
         LEE-PRG.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF UPDATE PROMPT
                          CONVERT TAB NO BEEP
                       ON EXCEPTION WK-EX
                          PERFORM 999-EXCEPTION.
           IF   CDN
             MOVE  01        TO I
             MOVE  07        TO I-LINEA
             MOVE  ZEROS     TO FIN-OK SW-1
             MOVE  WK-USER2  TO COD-USR
             MOVE  WK-PRG2   TO COD-PRG
             PERFORM  I-START-USER   THRU  F-START-USER
             PERFORM  I-WINDOW-PRG   THRU  F-WINDOW-PRG
             PERFORM  I-DISPLAY-PRG  THRU  F-DISPLAY-PRG
               UNTIL I > 10 OR SW-1 = 1
                MOVE WK-USER1 TO COD-USR
            IF SW-1 = 1
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
              " Hasta aqui programas asignados a este usuario, <ESC> "
              LINE 25 POSITION 01
              PERFORM ACEPTA-TRUCO UNTIL ESC
              DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
              GO F-HELP-PRG
            ELSE
              GO LEE-PRG.

       F-HELP-PRG.
      *
       I-DISPLAY-PRG.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF COD-USR NOT = WK-USER1  MOVE 1 TO SW-1.

           IF   SW-1 = 1
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 56 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-PRG.
           IF   COD-USR  = WK-USER1 NEXT SENTENCE
           ELSE GO I-DISPLAY-PRG.
           IF   COD-PRG = SPACES GO I-DISPLAY-PRG.
           DISPLAY   COD-PRG LINE I-LINEA POSITION 57.
           IF I = 2  MOVE COD-USR  TO WK-USER2
                     MOVE COD-PRG  TO WK-PRG2.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-PRG. EXIT.
      *
       I-DISPLAY-USER.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 37 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 43 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-USER.
           IF COD-PRG = SPACE
              NEXT SENTENCE
           ELSE
              GO I-DISPLAY-USER.
           DISPLAY   COD-USR      LINE I-LINEA POSITION 37.
           DISPLAY   NOM-USUARIO  LINE I-LINEA POSITION 43.
           IF I = 1  MOVE COD-USR TO WK-USER1.
           IF I = 2  MOVE COD-USR TO WK-USER2.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-USER.
      *
       I-WINDOW-PRG.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY  "ษออออออออออออออป"
                                LINE 04 POSITION 54
                                CONTROL SG-CONTROL-STRING
                                REVERSE.
           DISPLAY  "บณ PROGRAMAS ณ บ" LINE 05 POSITION 54 REVERSE.
           DISPLAY  "ฬออออออออออออออน" LINE 06 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 07 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 08 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 09 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 10 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 11 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 12 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 13 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 14 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณบ" LINE 15 POSITION 54 REVERSE.
           DISPLAY  "บณ           ณ บ" LINE 16 POSITION 54 REVERSE.
           DISPLAY  "ศออออออออออออออผ" LINE 17 POSITION 54 REVERSE.

       F-WINDOW-PRG.
      *
       I-WINDOW-PRG-MS.
           MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY
           "ษออออออออออออออออออออออออออออออออออออออออออออออป"
                                LINE 04 POSITION 23
                                CONTROL SG-CONTROL-STRING
                                REVERSE
           "บณ Name       Program  Usr PAMCRL 1234567890 ณ บ"
           LINE 05 POSITION 23 REVERSE
           "ฬออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 06 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 07 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 08 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 09 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 10 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 11 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 12 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 13 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 14 POSITION 23 REVERSE
           "บณ                                           ณบ"
           LINE 15 POSITION 23 REVERSE
           "บณ                                           ณ บ"
           LINE 16 POSITION 23 REVERSE
           "ศฯอออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 17 POSITION 23 REVERSE.
       F-WINDOW-PRG-MS.
      *
       I-BLANQUEA.
           MOVE SPACES       TO   NOM-USUARIO
                                  CLAVE-USR
                                  TIPO-USR
                                  PROCESOS-USR
                                  MOD-USR(1)
                                  MOD-USR(2)
                                  MOD-USR(3)
                                  MOD-USR(4)
                                  MOD-USR(5)
                                  MOD-USR(6)
                                  MOD-USR(7)
                                  MOD-USR(8)
                                  MOD-USR(9)
                                  MOD-USR(10).
       F-BLANQUEA.
      *
       120-FT.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
      ******************************************************************
