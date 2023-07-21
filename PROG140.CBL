       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG140.
      ******************************************************************
      *        CAPTURA ACTIVIDADES ECONOMICAS ABR-27-2002              *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\FRIESGOS.DYC".
               COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\FRIESGOS.FD".
               COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG140".
       77      ID-PRG              PIC X(7)  VALUE "PROG140".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      WI-COD.
               03 WI-COD-ALF       PIC X(01).
               03 WI-COD-NUM       PIC 9(06).
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\FRIESGOS.LBL".
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
               COPY "..\DCL\FRIESGOS.DCL".
               COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS
           IF      SW-USER  =  1     EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-FRIESGOS THRU F-STATUS-FRIESGOS.
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW    THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE "  ** C I I U **   "   TO  TIT-APLICACION
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   FRIESGOS.
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
           IF CUP OR ESC             GO F-CREAR.
           PERFORM I-LEE-FRIESGOS  THRU F-LEE-FRIESGOS.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOM-FRIESGOS NOM2-FRIESGOS
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-FRIESGOS.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO       THRU F-CODIGO.
           IF CUP OR ESC            GO F-ELIMINA.
           PERFORM I-LEE-FRIESGOS THRU F-LEE-FRIESGOS.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-FRIESGOS.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-FRIESGOS THRU F-LEE-FRIESGOS.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  COD-FRIESGOS.
           MOVE SPACE TO NOM-FRIESGOS NOM2-FRIESGOS
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 11 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-FRIESGOS FIN-OK.
           PERFORM I-START-FRIESGOS   THRU F-START-FRIESGOS.
           PERFORM I-WINDOW-FRIESGOS  THRU F-WINDOW-FRIESGOS.
           PERFORM I-DISPLAY-FRIESGOS THRU F-DISPLAY-FRIESGOS
                   UNTIL I > 10 OR SW-1 = 1.
         LEE-FRIESGOS.
           ACCEPT WK-COSTO LINE 07 POSITION 35
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01                TO   I
                MOVE 11                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE WK-COSTO          TO   COD-FRIESGOS
                PERFORM I-START-FRIESGOS   THRU F-START-FRIESGOS
                PERFORM I-WINDOW-FRIESGOS  THRU F-WINDOW-FRIESGOS
                PERFORM I-DISPLAY-FRIESGOS THRU F-DISPLAY-FRIESGOS
                        UNTIL I > 10
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-FRIESGOS
                ELSE
                   GO LEE-FRIESGOS.
       F-CONSUL-MODE.
      *
       I-DISPLAY-FRIESGOS.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "***" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY " Fin Archivo " LINE I-LINEA POSITION 29 REVERSE
                MOVE 12 TO I
                GO   F-DISPLAY-FRIESGOS.
           MOVE COD-FRIESGOS TO WI-COD.
           DISPLAY   COD-FRIESGOS  LINE I-LINEA POSITION 02.
           DISPLAY   NOM-FRIESGOS  LINE I-LINEA POSITION 11 SIZE 68.
           IF  NOM2-FRIESGOS NOT = SPACES
               ADD   1   TO  I-LINEA
               DISPLAY   NOM2-FRIESGOS
                         LINE I-LINEA POSITION 11 SIZE 68.
           IF I = 2  MOVE COD-FRIESGOS    TO WK-COSTO.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-FRIESGOS.
      *
       I-START-FRIESGOS.
           MOVE ZEROS TO SW-1.
           START FRIESGOS KEY NOT LESS LLAVM1-FRIESGOS
                 INVALID KEY MOVE 1 TO SW-1.
       F-START-FRIESGOS.
      *
       I-GRABAR.
           DISPLAY
           "º       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-FRIESGOS INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           DISPLAY
           "º       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-FRIESGOS INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           DISPLAY
           "º       [ E ]  BORRA REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE FRIESGOS INVALID KEY STOP " ".
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
       I-DAT.
           PERFORM I-NOMBRE2    THRU F-NOMBRE2.
           IF      CUP          GO   I-DATOS.
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
                   IF      CUP  GO I-DAT.
       F-DATOS.
      *
       I-MUESTRA-CAMPOS.
           MOVE    COD-FRIESGOS  TO WI-COD
           DISPLAY WI-COD             LINE 11 POSITION 35.
           DISPLAY NOM-FRIESGOS       LINE 15 POSITION 02.
           DISPLAY NOM2-FRIESGOS      LINE 16 POSITION 02.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE COD-FRIESGOS TO WI-COD
           DISPLAY WI-COD       LINE 11 POSITION 35.
           ACCEPT  COD-FRIESGOS LINE 11 POSITION 35
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE COD-FRIESGOS TO WI-COD
           DISPLAY WI-COD       LINE 11 POSITION 35.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  COD-FRIESGOS = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOM-FRIESGOS LINE 15 POSITION 02
           ACCEPT  NOM-FRIESGOS LINE 15 POSITION 02
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-FRIESGOS = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS       LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-NOMBRE2.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOM2-FRIESGOS LINE 16 POSITION 02
           ACCEPT  NOM2-FRIESGOS LINE 16 POSITION 02
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE2.
           DISPLAY WK-ESPACIOS       LINE 24 POSITION 1.
       F-NOMBRE2.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "<<       A D D       >>"
                                    LINE  9 POSITION 30 BLINK.
           IF  W-OPCION = "M"
               DISPLAY "<<     M O D I F Y   >>"
                                    LINE  9 POSITION 30 BLINK.
           IF  W-OPCION = "C"
               DISPLAY "<<    C O N S U L T  >>"
                                    LINE  3 POSITION 30 BLINK.
           IF  W-OPCION = "R"
               DISPLAY "<<     D E L E T E   >>"
                                    LINE  9 POSITION 30 BLINK.
       F-DISPLAY-OPCION.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************
       I-LEE-FRIESGOS.
               MOVE 0  TO   SW-1.
               READ FRIESGOS WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-FRIESGOS.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-FRIESGOS.
           IF CDN
              REWRITE REG-FRIESGOS.
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
           READ FRIESGOS NEXT RECORD AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


       120-FT.
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\USUARIOS.PRO".
      *
       I-FIN-MODE.
           CLOSE FRIESGOS.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-FRIESGOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
           DISPLAY " <<    ACTIVIDAD POR RIESGOS    >> "
                                         LINE 02 POSITION 20.
           DISPLAY WK-RAYAS-ARRIBA     LINE 06 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 07 POSITION 1.
           DISPLAY WK-RAYAS            LINE 08 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 09 POSITION 1.
           DISPLAY WK-RAYAS            LINE 10 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 11 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 12 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 13 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 14 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 15 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 16 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 17 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 18 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 19 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 20 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 21 POSITION 1.
           DISPLAY WK-RAYAS-ABAJO      LINE 22 POSITION 1.
           DISPLAY "CODIGO:  "           LINE 07 POSITION 24
           DISPLAY "<< COD >>"           LINE 09 POSITION 02
           DISPLAY " << DESCRIPCION >> " LINE 09 POSITION 24.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING.
       F-WINDOW-FRIESGOS.
      *
       I-WINDOW-DATOS.
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
           DISPLAY WK-RAYAS-ARRIBA     LINE 06 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 07 POSITION 1.
           DISPLAY WK-RAYAS            LINE 08 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 09 POSITION 1.
           DISPLAY WK-RAYAS            LINE 10 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 11 POSITION 1.
           DISPLAY WK-RAYAS            LINE 12 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 13 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 14 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 15 POSITION 1.
           DISPLAY WK-RAYAS-MEDIO      LINE 16 POSITION 1.
           DISPLAY WK-RAYAS-ABAJO      LINE 17 POSITION 1.
           DISPLAY " <<       ACTIVIDAD POR RIESGOS       >> "
                                         LINE 07 POSITION 20 .
           DISPLAY "  CODIGO :"            LINE 11 POSITION 24 .
           DISPLAY " <<  D E S C R I P C I O N  >> " LINE 13 POSITION 24
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING.
       F-WINDOW-DATOS.
      *
       I-STATUS-FRIESGOS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT FRIESGOS
               MOVE 1                      TO  SW-ABRE
               IF  FRIESGOS-STATUS              =  "35"
                   OPEN OUTPUT FRIESGOS
               ELSE
                   IF  FRIESGOS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-FRIESGOS
                       PERFORM I-FIN-MODE            THRU F-FIN-MODE.
               CLOSE FRIESGOS.
       F-STATUS-FRIESGOS.
      *

