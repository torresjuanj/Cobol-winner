       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG065.
      ******************************************************************
      *CAJAS DE COMPENSACION FAMILIAR  MAY 1 DE 2002                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CJASUBSI.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\CONTDPTO.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CONTDPTO.FD".
               COPY "..\FD\CJASUBSI.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7) VALUE "PROG065".
       77      ID-PRG              PIC X(7) VALUE "PROG065".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CJASUBSI.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CONTDPTO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\CONTDPTO.DCL".
               COPY "..\DCL\CJASUBSI.DCL".
               COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER   =   1  EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CJASUBSI THRU F-STATUS-CJASUBSI.
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE "CAJAS COMPENSACION"   TO  TIT-APLICACION
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "¹"                    TO  WK-RAYAS-U
               MOVE "Ì"                    TO  WK-RAYAS-P
               MOVE "¶"                    TO  WK-RAYA-U
               MOVE "Ç"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   CJASUBSI CNNITCED.
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
             IF W-OPCION = "A" AND PROSS-AKK = "S"
                MOVE  1  TO SW-MODE
                PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M" AND PROSS-MKK = "S"
                  MOVE  2  TO SW-MODE
                  PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                           UNTIL ESC OR CUP
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C" AND PROSS-CKK = "S"
                   PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R" AND PROSS-RKK = "S"
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
           PERFORM I-LEE-CJASUBSI      THRU F-LEE-CJASUBSI.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               PERFORM  I-WIN    THRU   F-WINDOW-DATOS
               MOVE SPACE          TO   NOM-CJASUBSI
               MOVE ZEROS          TO   DPTO-CJASUBSI
               MOVE ZEROS          TO   NIT-CJASUBSI
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-CJASUBSI.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-CJASUBSI   THRU F-LEE-CJASUBSI.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-CJASUBSI.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           PERFORM  I-WIN    THRU   F-WINDOW-DATOS
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  COD-CJASUBSI.
           MOVE SPACE          TO   NOM-CJASUBSI.
           MOVE ZEROS          TO   DPTO-CJASUBSI
           MOVE ZEROS          TO   NIT-CJASUBSI
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-CJASUBSI FIN-OK.
           PERFORM I-START-CJASUBSI   THRU F-START-CJASUBSI.
           PERFORM I-WINDOW-CJASUBSI  THRU F-WINDOW-CJASUBSI.
           PERFORM I-DISPLAY-CJASUBSI THRU F-DISPLAY-CJASUBSI
                   UNTIL I > 5 OR SW-1 = 1.
         LEE-CJASUBSI.
           MOVE W-COMP TO WI-Z2.
           DISPLAY WI-Z2 LINE 11 POSITION 31.
           ACCEPT W-COMP LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01                TO   I
                MOVE 15                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE W-COMP            TO   LLAVM1-CJASUBSI
                PERFORM I-START-CJASUBSI   THRU F-START-CJASUBSI
                PERFORM I-WINDOW-CJASUBSI  THRU F-WINDOW-CJASUBSI
                PERFORM I-DISPLAY-CJASUBSI THRU F-DISPLAY-CJASUBSI
                        UNTIL I > 5
      *                 UNTIL I > 5 OR SW-1 = 1
                IF SW-1 = 1
                   DISPLAY SPACE LINE 19 POSITION 64 REVERSE
                   IF I-LINEA = 16
                      MOVE 01 TO WK-CODIGO
                      GO LEE-CJASUBSI
                   ELSE
                      GO LEE-CJASUBSI
                ELSE
                   GO LEE-CJASUBSI.
       F-CONSUL-MODE.
      *
       I-DISPLAY-CJASUBSI.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE          I-LINEA POSITION 24 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-CJASUBSI.
           MOVE COD-CJASUBSI TO WI-Z2.
           DISPLAY   WI-Z2                LINE I-LINEA POSITION 19.
           DISPLAY   NOM-CJASUBSI         LINE I-LINEA POSITION 24.
           MOVE NIT-CJASUBSI                TO WI-Z9  .
           DISPLAY WI-Z9                  LINE I-LINEA POSITION 48
           DISPLAY   DPTO-CJASUBSI      LINE I-LINEA POSITION 63.
           DISPLAY   NROAFI-CJASUBSI      LINE I-LINEA POSITION 68.
           IF I = 2  MOVE COD-CJASUBSI   TO W-COMP.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-CJASUBSI.
      *
       I-START-CJASUBSI.
           MOVE ZEROS TO SW-1.
           START CJASUBSI KEY NOT LESS LLAVM1-CJASUBSI
                 INVALID KEY MOVE 1 TO SW-1.
       F-START-CJASUBSI.
      *
       I-GRABAR.
           DISPLAY
           "º       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-CJASUBSI INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           DISPLAY
           "º       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-CJASUBSI INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           DISPLAY
           "º       [ E ]  BORRA REGISTRO     [  ]  CONTINUA        º"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE CJASUBSI INVALID KEY STOP " ".
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
           PERFORM I-CIUDAD     THRU F-CIUDAD
           IF      CUP          GO   I-DAT1.
       I-DAT3.
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
           MOVE COD-CJASUBSI TO WI-Z2.
           DISPLAY COD-CJASUBSI       LINE 11 POSITION 31.
           DISPLAY NOM-CJASUBSI       LINE 13 POSITION 31.
           MOVE NIT-CJASUBSI           TO  WI-Z9  .
           DISPLAY WI-Z9               LINE 14 POSITION 31
           DISPLAY DPTO-CJASUBSI    LINE 15 POSITION 31.
           DISPLAY NROAFI-CJASUBSI    LINE 16 POSITION 31.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE COD-CJASUBSI TO WI-Z2.
           DISPLAY   WI-Z2      LINE 11 POSITION 31.
           DISPLAY COD-CJASUBSI LINE 11 POSITION 31
           ACCEPT  COD-CJASUBSI LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  COD-CJASUBSI = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOM-CJASUBSI LINE 13 POSITION 31
           ACCEPT  NOM-CJASUBSI LINE 13 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-CJASUBSI = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS          LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-NIT.
           DISPLAY "<F1> Ayuda tabla de NITS"  LINE 24 POSITION 01.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE NIT-CJASUBSI           TO  WI-Z9  .
           DISPLAY WI-Z9               LINE 14 POSITION 31
           ACCEPT  NIT-CJASUBSI        LINE 14 POSITION 31
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE NIT-CJASUBSI    TO WI-Z9
           DISPLAY WI-Z9        LINE 14 POSITION 31
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT1   THRU F-AYUDA-NIT1
               OPEN I-O CNNITCED
               GO I-NIT.
           IF  CUP OR ESC GO F-NIT.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  NIT-CJASUBSI = ZEROS
               DISPLAY "N.I.T/CEDULA no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT.
           MOVE NIT-CJASUBSI    TO WI-Z9
           DISPLAY WI-Z9        LINE 14 POSITION 31
           MOVE NIT-CJASUBSI    TO CODIM
           PERFORM I-LEA-CNNITCED THRU F-LEA-CNNITCED
           IF SW-EOF = 1
              DISPLAY "N.I.T  -  INEXISTENTE  -  "
              LINE 24 POSITION 01  REVERSE
              GO I-NIT.
           PERFORM I-LIMPIA  THRU F-LIMPIA.
       F-NIT.
      *
        I-LIMPIA.
           MOVE "LOW," TO SG-INTENSITY.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 19 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 20 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 21 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 22 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 23 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 24 POSITION 01.
           DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      -    "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" LINE 25 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           MOVE "LOW," TO SG-INTENSITY.
       F-LIMPIA.
      *
       I-CIUDAD.
           OPEN INPUT CONTDPTO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY DPTO-CJASUBSI     LINE 15 POSITION 31
           ACCEPT  DPTO-CJASUBSI     LINE 15 POSITION 31
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    DPTO-CJASUBSI TO  COD-DPTO.
           IF  F1  PERFORM I-AYUDA-CIUD   THRU F-AYUDA-CIUD
                   CLOSE CONTDPTO
                   GO I-CIUDAD.
            READ CONTDPTO WITH NO LOCK INVALID KEY
                 DISPLAY "ERROR LEE DPTO CONTABLE"
                          LINE 23 POSITION 1
                          CLOSE   CONTDPTO
                          GO I-CIUDAD.
           DISPLAY NOM-DPTO LINE 15 POSITION 35.
           CLOSE      CONTDPTO.
           PERFORM I-LIMPIA  THRU F-LIMPIA.
       F-CIUDAD.
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

       I-LEE-CJASUBSI.
               MOVE 0  TO   SW-1.
               READ CJASUBSI WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-CJASUBSI.
      *
       I-LEA-CNNITCED.
           MOVE ZEROS TO SW-EOF.
           READ CNNITCED WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
       F-LEA-CNNITCED.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-CJASUBSI.
           IF CDN
              REWRITE REG-CJASUBSI.
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
      *
       I-LEE-SIGUIENTE.
           MOVE 0 TO SW-1.
           READ CJASUBSI NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.
      *
       120-FT.
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\HELDPTO.PRO".
               COPY "..\PRO\HELPNIT1.PRO".
               COPY "..\PRO\USUARIOS.PRO".
      *
       I-FIN-MODE.
           CLOSE CJASUBSI.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-CJASUBSI.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ÉÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍËÍÍÍÍÍÍ»"
           LINE 12 POSITION 15 REVERSE
           "º COD. º   N O M B R E       º   N.I.T        ºDPTOº      º"
           LINE 13 POSITION 15 REVERSE
           "ÌÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÎÍÍÍÍÎÍÍÍÍÍÍ¹"
           LINE 14 POSITION 15 REVERSE
           "º      º                     º                º    º      º"
           LINE 15 POSITION 15 REVERSE
           "º      º                     º                º    º      º"
           LINE 16 POSITION 15 REVERSE
           "º      º                     º                º    º      º"
           LINE 17 POSITION 15 REVERSE
           "º      º                     º                º    º      º"
           LINE 18 POSITION 15 REVERSE
           "º      º                     º                º    º     º"
           LINE 19 POSITION 15 REVERSE
           "ÈÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÊÍÍÍÍÍÍ¼"
           LINE 20 POSITION 15 REVERSE.
       F-WINDOW-CJASUBSI.
      *
       I-WINDOW-DATOS.
           DISPLAY "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
                                        LINE 06 POSITION 20.
           DISPLAY "º    CAJAS DE COMPENSACION FAMILIAR        º"
                                        LINE 07 POSITION 20 REVERSE.
           DISPLAY "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹"
                                        LINE 08 POSITION 20.
           DISPLAY "º                                          º"
                                        LINE 09 POSITION 20.
           DISPLAY "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹"
                                        LINE 10 POSITION 20.
           DISPLAY "º   COD  : __                              º"
                                        LINE 11 POSITION 20.
           DISPLAY "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹"
                                        LINE 12 POSITION 20.
       I-WIN.
           DISPLAY "º NOMBRE:                                  º"
                                        LINE 13 POSITION 20.
           DISPLAY "º NIT   :                                  º"
                                        LINE 14 POSITION 20.
           DISPLAY "º DPTO  :                                  º"
                                        LINE 15 POSITION 20.
           DISPLAY "º NUMERO:                                  º"
                                        LINE 16 POSITION 20.
           DISPLAY "º                                          º"
                                        LINE 17 POSITION 20.
           DISPLAY "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
                                        LINE 18 POSITION 20.
       F-WINDOW-DATOS.
      ******************************************************************

       I-STATUS-CJASUBSI.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CJASUBSI
               MOVE 1                      TO  SW-ABRE
               IF  CJASUBSI-STATUS              =  "35" OR
                   CJASUBSI-STATUS              =  "94"
                   OPEN OUTPUT CJASUBSI
               ELSE
                   IF  CJASUBSI-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CJASUBSI
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE CJASUBSI.
       F-STATUS-CJASUBSI.
      *


