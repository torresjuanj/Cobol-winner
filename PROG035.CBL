       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG035.
      ******************************************************************
      *  TABLA DE PORCENTAJE RIESGOS PROFESIONALES MAY 1 DE 2002       *
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
               COPY "..\DYC\TARIESGO.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\TARIESGO.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG035".
       77      ID-PRG              PIC X(7)  VALUE "PROG035".
       77      W-VARIABLES-NOMINA  PIC X(8)  VALUE  SPACES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\TARIESGO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\TARIESGO.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE "RIESGOS PROFESIONALES  <%>" TO   TIT-APLICACION.
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE    THRU F-PROCESO-AUDITE.
           PERFORM I-LABEL-ARCHIVOS    THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-TARIESGO   THRU F-STATUS-TARIESGO
           PERFORM I-ABRE-ARCHIVOS     THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW      THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES     THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE          THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN I-O   TARIESGO.
       F-ABRE-ARCHIVOS.

       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           DISPLAY "RIESGOS PROFESIONALES"
                    LINE 03 POSITION 25
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.

           DISPLAY "[ ESC ] TERMINA" LINE 24 POSITION 01 REVERSE
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
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
             IF W-OPCION = "A"
                MOVE  1  TO SW-MODE
                PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M"
                  MOVE  2  TO SW-MODE
                  PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                           UNTIL ESC OR CUP
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C"
                   PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R"
                      PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                      PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM  I-ELIMINA        THRU F-ELIMINA
                               UNTIL ESC OR CUP
                      MOVE ZEROS TO WK-EXCEPTION
                   IF W-OPCION = "L"
                      PERFORM I-LISTA THRU F-LISTA.
       F-MENU-OPCIONES.
      *
       I-LISTA.
           DISPLAY " ".
       F-LISTA.
      *
       I-CREAR.
           PERFORM   I-CODIGO   THRU   F-CODIGO.
           IF   CUP OR ESC   GO   F-CREAR.
           PERFORM   I-LEE-TARIESGO THRU   F-LEE-TARIESGO.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOMBRE-TARIESGO
               MOVE ZEROS          TO   PORCEN-TARIESGO
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-TARIESGO.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-TARIESGO   THRU F-LEE-TARIESGO.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-TARIESGO.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-TARIESGO   THRU F-LEE-TARIESGO.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  TIPO-TARIESGO.
           MOVE SPACE TO NOMBRE-TARIESGO.
           MOVE ZEROS TO PORCEN-TARIESGO.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO KEY00-TARIESGO FIN-OK.
           PERFORM I-START-TARIESGO   THRU F-START-TARIESGO.
           PERFORM I-WINDOW-TARIESGO  THRU F-WINDOW-TARIESGO.
           PERFORM I-DISPLAY-TARIESGO THRU F-DISPLAY-TARIESGO
                   UNTIL I > 5.
         LEE-TARIESGO.
           ACCEPT W-TPO-CTA LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01                TO   I
                MOVE 15                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE W-TPO-CTA        TO   KEY00-TARIESGO
                PERFORM I-START-TARIESGO   THRU F-START-TARIESGO
                PERFORM I-WINDOW-TARIESGO  THRU F-WINDOW-TARIESGO
                PERFORM I-DISPLAY-TARIESGO THRU F-DISPLAY-TARIESGO
                        UNTIL I > 5
                IF SW-1 = 1
                   DISPLAY SPACE LINE 19 POSITION 64 REVERSE
                   IF I-LINEA = 16
                      MOVE 01 TO W-TPO-CTA
                      GO LEE-TARIESGO
                   ELSE
                      GO LEE-TARIESGO
                ELSE
                   GO LEE-TARIESGO.
       F-CONSUL-MODE.
      *
       I-DISPLAY-TARIESGO.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-TARIESGO.
           DISPLAY   TIPO-TARIESGO       LINE I-LINEA POSITION 24.
           DISPLAY   NOMBRE-TARIESGO     LINE I-LINEA POSITION 29.
           MOVE PORCEN-TARIESGO TO WI-Z3P3.
           DISPLAY  WI-Z3P3  LINE I-LINEA POSITION 56.
           IF I = 2  MOVE TIPO-TARIESGO TO W-TPO-CTA.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-TARIESGO.
      *
       I-START-TARIESGO.
           START TARIESGO KEY NOT LESS KEY00-TARIESGO.
       F-START-TARIESGO.
      *
       I-GRABAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-TARIESGO INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-TARIESGO INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE TARIESGO INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-ELIMINAR.
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 24 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 24 POSITION 01.
       F-NEXISTE.
      *
       I-DATOS.
           PERFORM I-NOMBRE     THRU F-NOMBRE.
           IF      CUP          GO   F-DATOS.
       I-NC.
           PERFORM   I-NUMCON   THRU   F-NUMCON.
           IF      CUP          GO   I-DATOS.
           IF      SW-MODE = 1
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-GRABAR THRU F-GRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-NC
                   ELSE    NEXT SENTENCE
           ELSE
              IF   SW-MODE = 2
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-REGRABAR THRU F-REGRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-NC.
       F-DATOS.
      *
       I-MUESTRA-CAMPOS.
           DISPLAY TIPO-TARIESGO   LINE 11 POSITION 31.
           DISPLAY NOMBRE-TARIESGO LINE 14 POSITION 28.
           MOVE PORCEN-TARIESGO TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 15 POSITION 40.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY TIPO-TARIESGO LINE 11 POSITION 31
           ACCEPT  TIPO-TARIESGO LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  TIPO-TARIESGO = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
            DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOMBRE-TARIESGO LINE 14 POSITION 28
           ACCEPT  NOMBRE-TARIESGO LINE 14 POSITION 28
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  TIPO-TARIESGO = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-NUMCON.
           MOVE   ZEROS   TO   WK-EXCEPTION.
           IF W-OPCION = "A"
            MOVE ZEROS TO PORCEN-TARIESGO
            DISPLAY PORCEN-TARIESGO LINE 15 POSITION 40.
           MOVE   ZEROS   TO   WK-EXCEPTION.
           DISPLAY SPACES SIZE 10  LINE 15 POSITION 40.
           DISPLAY PORCEN-TARIESGO LINE 15 POSITION 40.
           ACCEPT  PORCEN-TARIESGO LINE 15 POSITION 40
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE PORCEN-TARIESGO TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 15 POSITION 40.
           IF  CUP OR ESC GO F-NUMCON.
       F-NUMCON. EXIT.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "ADICIONA"
                                    LINE  9 POSITION 37 BLINK.
           IF  W-OPCION = "M"
               DISPLAY "MODIFICA"
                                    LINE  9 POSITION 37 BLINK.
           IF  W-OPCION = "C"
               DISPLAY "CONSULTA"
                                    LINE  9 POSITION 37 BLINK.
           IF  W-OPCION = "R"
               DISPLAY "ELIMINAR"
                                    LINE  9 POSITION 37 BLINK.
       F-DISPLAY-OPCION.
      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************

       I-LEE-TARIESGO.
               MOVE 0  TO   SW-1.
               READ TARIESGO INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-TARIESGO.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-TARIESGO.
           IF CDN
              REWRITE REG-TARIESGO.
           PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.

       I-CONSULTA.
           DISPLAY " <> REGISTRO SIGUIENTE "
                   LINE 24 POSITION 01 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
           IF F2
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CDN
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF SW-1 = 0
              PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-CONSULTA.

       I-LEE-SIGUIENTE.
           MOVE 0 TO SW-1.
           READ TARIESGO NEXT RECORD AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


       120-FT.
      *
       I-FIN-MODE.
           CLOSE TARIESGO.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-TARIESGO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออหอออออออออออออออออออออออออออออออออออออออออป"
           LINE 12 POSITION 20 REVERSE
           "บ TIP. บ   DESCRIPCION               % RIESGO    บ"
           LINE 13 POSITION 20 REVERSE
           "ฬออออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 14 POSITION 20 REVERSE
           "บ      บ                                         บ"
           LINE 15 POSITION 20 REVERSE
           "บ      บ                                         บ"
           LINE 16 POSITION 20 REVERSE
           "บ      บ                                         บ"
           LINE 17 POSITION 20 REVERSE
           "บ      บ                                         บ"
           LINE 18 POSITION 20 REVERSE
           "บ      บ                                        บ"
           LINE 19 POSITION 20 REVERSE
           "ศออออออสอออออออออออออออออออออออออออออออออออออออออผ"
           LINE 20 POSITION 20 REVERSE.
       F-WINDOW-TARIESGO. EXIT.
      *
       I-WINDOW-DATOS.
           DISPLAY
           "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 08 POSITION 20
           "บ                                          บ"
                                        LINE 09 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 10 POSITION 20
           "บ   TIPO : __                              บ"
                                        LINE 11 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 12 POSITION 20
           "บ                                          บ"
                                        LINE 13 POSITION 20
           "บ                                          บ"
                                        LINE 14 POSITION 20
           "บ                                          บ"
                                        LINE 15 POSITION 20
           "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 16 POSITION 20
           "DESCRIPCION:" LINE 13 POSITION 22
           "Porcentaje :" LINE 15 POSITION 22.
       F-WINDOW-DATOS.
      *
      *
       I-STATUS-TARIESGO.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT TARIESGO
               MOVE 1                      TO  SW-ABRE
               IF  TARIESGO-STATUS              =  "35"
                   OPEN OUTPUT TARIESGO
               ELSE
                   IF  TARIESGO-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-TARIESGO
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE TARIESGO.
       F-STATUS-TARIESGO.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\AUDITE.PRO".
      ******************************************************************


