       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG190.
      ******************************************************************
      *REGIMENES SEGURIDAD SOCIAL(SALUD CONTRIB,SUBSIDIADO)ABR-27-2002 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\TIPREGIM.DYC".
               COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\TIPREGIM.FD".
               COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG190".
       77      ID-PRG              PIC X(7)  VALUE "PROG190".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\TIPREGIM.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\TIPREGIM.DCL".
               COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS
           IF      SW-USER   =   1  EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-TIPREGIM THRU  F-STATUS-TIPREGIM.
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE "REGIMENES SEG.SOCIAL"  TO  TIT-APLICACION
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
                OPEN I-O   TIPREGIM.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
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
           PERFORM I-CODIGO        THRU F-CODIGO.
           IF CUP OR ESC           GO   F-CREAR.
           PERFORM I-LEE-TIPREGIM      THRU F-LEE-TIPREGIM.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOM-TIPREGIM COD-SUPER-TIPREGIM
               MOVE 0 TO NIT-TIPREGIM NIT-EXTENSION-TIPREGIM
               MOVE "-" TO NIT-LETRA-TIPREGIM
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-TIPREGIM.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-TIPREGIM   THRU F-LEE-TIPREGIM.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-TIPREGIM.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-TIPREGIM THRU F-LEE-TIPREGIM.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  COD-TIPREGIM.
           MOVE SPACE TO NOM-TIPREGIM COD-SUPER-TIPREGIM
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-TIPREGIM FIN-OK.
           PERFORM I-START-TIPREGIM   THRU F-START-TIPREGIM.
           PERFORM I-WINDOW-TIPREGIM  THRU F-WINDOW-TIPREGIM.
           PERFORM I-DISPLAY-TIPREGIM THRU F-DISPLAY-TIPREGIM
                   UNTIL I > 5 OR SW-1 = 1.
         LEE-TIPREGIM.
           MOVE WK-CODIGO TO WI-Z2.
           DISPLAY WI-Z2 LINE 11 POSITION 31.
           ACCEPT WK-CODIGO LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 1                 TO   I
                MOVE 15                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE WK-CODIGO         TO   COD-TIPREGIM
                PERFORM I-START-TIPREGIM   THRU F-START-TIPREGIM
                PERFORM I-WINDOW-TIPREGIM  THRU F-WINDOW-TIPREGIM
                PERFORM I-DISPLAY-TIPREGIM THRU F-DISPLAY-TIPREGIM
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-TIPREGIM
                ELSE
                   GO LEE-TIPREGIM.
       F-CONSUL-MODE.
      *
       I-DISPLAY-TIPREGIM.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-TIPREGIM.
           MOVE COD-TIPREGIM TO WI-Z2.
           DISPLAY   WI-Z2                LINE I-LINEA POSITION 18.
           DISPLAY   NOM-TIPREGIM         LINE I-LINEA POSITION 24.
           DISPLAY   COD-SUPER-TIPREGIM   LINE I-LINEA POSITION 61.
           MOVE NIT-TIPREGIM TO WI-Z12C.
           DISPLAY   WI-Z12C              LINE I-LINEA POSITION 41
                     NIT-LETRA-TIPREGIM   LINE I-LINEA POSITION 56.
           MOVE NIT-EXTENSION-TIPREGIM TO WI-Z2.
           DISPLAY   WI-Z2                LINE I-LINEA POSITION 57.
           IF I = 2  MOVE COD-TIPREGIM   TO WK-CODIGO.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-TIPREGIM.
      *
       I-START-TIPREGIM.
           MOVE ZEROS TO SW-1.
           START TIPREGIM KEY NOT LESS LLAVM1-TIPREGIM
                 INVALID KEY MOVE 1 TO SW-1.
       F-START-TIPREGIM.
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
              WRITE REG-TIPREGIM INVALID KEY STOP " ".
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
              REWRITE REG-TIPREGIM INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "บ       [ E ]  BORRA REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE TIPREGIM INVALID KEY STOP " ".
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
       I-DAT1.
           PERFORM I-NIT        THRU F-NIT.
           IF      CUP          GO   I-DATOS.
       I-DAT2.
           PERFORM I-COD-SUPER  THRU F-COD-SUPER.
           IF      CUP          GO   I-DAT1.

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
                   IF      CUP  GO I-DAT2.
       F-DATOS.
      *
       I-MUESTRA-CAMPOS.
           MOVE COD-TIPREGIM            TO WI-Z2.
           DISPLAY   WI-Z2            LINE 11 POSITION 31.
           DISPLAY NOM-TIPREGIM       LINE 14 POSITION 24.
           DISPLAY COD-SUPER-TIPREGIM LINE 15 POSITION 45.
           MOVE NIT-TIPREGIM TO WI-Z12C.
           DISPLAY WI-Z12C LINE 14 POSITION 40.
           DISPLAY NIT-LETRA-TIPREGIM LINE 14 POSITION 55.
           MOVE NIT-EXTENSION-TIPREGIM TO WI-Z2.
           DISPLAY WI-Z2 LINE 14 POSITION 56.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE COD-TIPREGIM TO WI-Z2.
           DISPLAY WI-Z2        LINE 11 POSITION 31.
           ACCEPT  COD-TIPREGIM LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  COD-TIPREGIM = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOM-TIPREGIM LINE 14 POSITION 24
           ACCEPT  NOM-TIPREGIM LINE 14 POSITION 24
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-TIPREGIM = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-COD-SUPER.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY COD-SUPER-TIPREGIM LINE 15 POSITION 45
           ACCEPT  COD-SUPER-TIPREGIM LINE 15 POSITION 45
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-COD-SUPER.
           IF  COD-SUPER-TIPREGIM = SPACES GO I-COD-SUPER.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-COD-SUPER.
      *
       I-NIT.
           MOVE ZEROS TO  WK-EXCEPTION
           DISPLAY NIT-TIPREGIM           LINE 14 POSITION 40
                   NIT-LETRA-TIPREGIM     LINE 14 POSITION 55
                   NIT-EXTENSION-TIPREGIM LINE 14 POSITION 56
           ACCEPT  NIT-TIPREGIM LINE 14 POSITION 40
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NIT.
           ACCEPT  NIT-EXTENSION-TIPREGIM LINE 14 POSITION 56
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NIT.
           IF  NIT-TIPREGIM = ZEROS GO I-NIT.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NIT. EXIT.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "ADICIONA"
                                    LINE  9 POSITION 37 LOW BLINK.
           IF  W-OPCION = "M"
               DISPLAY "MODIFICA"
                                    LINE  9 POSITION 37 LOW BLINK.
           IF  W-OPCION = "C"
               DISPLAY "CONSULTA"
                                    LINE  9 POSITION 37 LOW BLINK.
           IF  W-OPCION = "R"
               DISPLAY "ELIMINAR"
                                    LINE  9 POSITION 37 LOW BLINK.
       F-DISPLAY-OPCION.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************

       I-LEE-TIPREGIM.
               MOVE 0  TO   SW-1.
               READ TIPREGIM WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-TIPREGIM.

       I-ACTUALIZAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-TIPREGIM.
           IF CDN
              REWRITE REG-TIPREGIM.
           PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.

       I-CONSULTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
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
           READ TIPREGIM NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


       120-FT.
      *
       I-FIN-MODE.
           CLOSE TIPREGIM.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-TIPREGIM.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออหออออออออออออออออออออออออออออออออออออออออออออออป"
                                LINE 12 POSITION 15 REVERSE
           "บ COD. บ          DESCRIPCION              COD.SUPERB.บ"
                                LINE 13 POSITION 15 REVERSE
           "ฬออออออฮออออออออออออออออออออออออออออออออออออออออออออออน"
                                LINE 14 POSITION 15 REVERSE
           "บ      บ                                              บ"
                                LINE 15 POSITION 15 REVERSE
           "บ      บ                                              บ"
                                LINE 16 POSITION 15 REVERSE
           "บ      บ                                              บ"
                                LINE 17 POSITION 15 REVERSE
           "บ      บ                                              บ"
                                LINE 18 POSITION 15 REVERSE
           "บ      บ                                             บ"
                                LINE 19 POSITION 15 REVERSE
           "ศออออออสออออออออออออออออออออออออออออออออออออออออออออออผ"
                                LINE 20 POSITION 15 REVERSE.
       F-WINDOW-TIPREGIM.
      *
       I-WINDOW-DATOS.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 20.
           DISPLAY "บ  TIPOS DE REGIMENES DE SEGURIDAD SOCIAL  บ"
                        LINE 07 POSITION 20 REVERSE LOW
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 08 POSITION 20.
           DISPLAY "บ                                          บ"
                                        LINE 09 POSITION 20.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 10 POSITION 20.
           DISPLAY "บ   COD  : __                              บ"
                                        LINE 11 POSITION 20.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 12 POSITION 20.
           DISPLAY "บ                                          บ"
                                        LINE 13 POSITION 20.
           DISPLAY "บ                                          บ"
                                        LINE 14 POSITION 20.
           DISPLAY "บ                                          บ"
                                        LINE 15 POSITION 20.
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 16 POSITION 20.
           DISPLAY "DESCRIPCION"        LINE 13 POSITION 24 LOW.
           DISPLAY "COD.SUPERBANCARIA"  LINE 15 POSITION 24 LOW.
           DISPLAY "NIT"                LINE 13 POSITION 40 LOW
                   "DV"                 LINE 13 POSITION 56 LOW.
       F-WINDOW-DATOS.

               COPY "..\STA\TIPREGIM.STA".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\USUARIOS.PRO".

