       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG015.
      ******************************************************************
      *        CAPTURA     NACIONALIDADES  MAY 1 DE 2002               *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\TIPONACI.DYC".
               COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\TIPONACI.FD".
               COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      ID-PRG              PIC X(7) VALUE "PROG015".
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG015".
       77      W-VARIABLES-NOMINA  PIC X(8)  VALUE  SPACES.
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\TIPONACI.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\VARIABLE.WRK".
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
               COPY "..\DCL\TIPONACI.DCL".
               COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS
           IF      SW-USER  =  1    EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE " COUNTRY/PAISES   "   TO  TIT-APLICACION
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   TIPONACI.
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
           PERFORM I-LEE-TIPONACI      THRU F-LEE-TIPONACI.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOM-NAC
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-TIPONACI.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-TIPONACI   THRU F-LEE-TIPONACI.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-TIPONACI.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-TIPONACI THRU F-LEE-TIPONACI.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  COD-NAC.
           MOVE SPACE TO NOM-NAC.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-NAC FIN-OK.
           PERFORM I-START-TIPONACI   THRU F-START-TIPONACI.
           PERFORM I-WINDOW-TIPONACI  THRU F-WINDOW-TIPONACI.
           PERFORM I-DISPLAY-TIPONACI THRU F-DISPLAY-TIPONACI
                   UNTIL I > 5 OR SW-1 = 1.
         LEE-TIPONACI.
           MOVE WK-CODIGO TO WI-Z3.
           DISPLAY WI-Z3 LINE 11 POSITION 31.
           ACCEPT WK-CODIGO LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01                TO   I
                MOVE 15                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE WK-CODIGO         TO   COD-NAC
                PERFORM I-START-TIPONACI   THRU F-START-TIPONACI
                PERFORM I-WINDOW-TIPONACI  THRU F-WINDOW-TIPONACI
                PERFORM I-DISPLAY-TIPONACI THRU F-DISPLAY-TIPONACI
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-TIPONACI
                ELSE
                   GO LEE-TIPONACI.
       F-CONSUL-MODE.
      *
       I-DISPLAY-TIPONACI.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-TIPONACI.
           MOVE COD-NAC TO WI-Z3.
           DISPLAY   WI-Z3                LINE I-LINEA POSITION 24.
           DISPLAY   NOM-NAC        LINE I-LINEA POSITION 29.
           IF I = 2  MOVE COD-NAC  TO WK-CODIGO.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-TIPONACI.
      *
       I-START-TIPONACI.
           MOVE ZEROS TO SW-1.
           START TIPONACI KEY NOT LESS LLAVM1-NAC
                 INVALID KEY MOVE 1 TO SW-1.
       F-START-TIPONACI.
      *
       I-GRABAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                    LINE 24 POSITION 10  REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-TIPONACI INVALID KEY STOP " ".
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
                   LINE 24 POSITION 10  REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-TIPONACI INVALID KEY STOP " ".
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
                    LINE 24 POSITION 10  REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE TIPONACI INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-ELIMINAR.
      *
       I-EXISTE.
           DISPLAY "Record exist" LINE 24 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "Record not exist" LINE 24 POSITION 01.
       F-NEXISTE.
      *
       I-DATOS.
           PERFORM I-NOMBRE     THRU F-NOMBRE.
           IF      CUP          GO   F-DATOS.
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
       I-MUESTRA-CAMPOS.
           MOVE COD-NAC TO WI-Z3.
           DISPLAY   WI-Z3      LINE 11 POSITION 31.
           DISPLAY NOM-NAC LINE 14 POSITION 24.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE COD-NAC TO WI-Z3.
           DISPLAY WI-Z3        LINE 11 POSITION 31.
           ACCEPT  COD-NAC LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  COD-NAC = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOM-NAC LINE 14 POSITION 24
           ACCEPT  NOM-NAC LINE 14 POSITION 24
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-NAC = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "ADD     "
                                    LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "M"
               DISPLAY "MODIFY  "
                                    LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "C"
               DISPLAY "CONSULT "
                                    LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "R"
               DISPLAY "DELETE  "
                                    LINE  9 POSITION 37  BLINK.
       F-DISPLAY-OPCION.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************

       I-LEE-TIPONACI.
               MOVE 0  TO   SW-1.
               READ TIPONACI WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-TIPONACI.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-TIPONACI.
           IF CDN
              REWRITE REG-TIPONACI.
           PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.

       I-CONSULTA.
           DISPLAY " <> REGISTRO SIGUIENTE "
                   LINE 24 POSITION 01 REVERSE
           DISPLAY " <ESC> CANCEL   " LINE 24 POSITION 54 REVERSE
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
           READ TIPONACI NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


       120-FT.
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\USUARIOS.PRO".
      *
       I-FIN-MODE.
           CLOSE TIPONACI.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-TIPONACI.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY  "ษออออออหอออออออออออออออออออออออออออออออออออออป"
                                LINE 12 POSITION 20
                               REVERSE
           DISPLAY  "บ COD. บ          DESCRIPCION                บ"
                                LINE 13 POSITION 20 REVERSE .
           DISPLAY  "ฬออออออฮอออออออออออออออออออออออออออออออออออออน"
                                LINE 14 POSITION 20 REVERSE .
           DISPLAY  "บ      บ                                     บ"
                                LINE 15 POSITION 20 REVERSE .
           DISPLAY  "บ      บ                                     บ"
                                LINE 16 POSITION 20 REVERSE .
           DISPLAY  "บ      บ                                     บ"
                                LINE 17 POSITION 20 REVERSE .
           DISPLAY  "บ      บ                                     บ"
                                LINE 18 POSITION 20 REVERSE .
           DISPLAY  "บ      บ                                    บ"
                                LINE 19 POSITION 20 REVERSE .
           DISPLAY  "ศออออออสอออออออออออออออออออออออออออออออออออออผ"
                                LINE 20 POSITION 20 REVERSE .
       F-WINDOW-TIPONACI.
      *
       I-WINDOW-DATOS.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 20.
           DISPLAY "บ           COUNTRY/PAISES                 บ"
                                        LINE 07 POSITION 20 REVERSE .
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
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 15 POSITION 20.
           DISPLAY "DESCRIPCION" LINE 13 POSITION 24 .
       F-WINDOW-DATOS.

