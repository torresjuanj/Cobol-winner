       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG115.
      *----------------------------------------------------------------*
      * ENTIDADES QUE REALIZAN PRESTAMOS. ABR-27-2002                  *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\TBFOPRES.DYC".
           COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\TBFOPRES.FD".
           COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA          PIC X(07)   VALUE "PROG115".
       77  ID-PRG              PIC X(07)   VALUE "PROG115".
       77  W-VARIABLES-NOMINA  PIC X(07)   VALUE SPACES.
      *
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\TBFOPRES.LBL".
           COPY "..\LBL\USER-ACT.LBL".
      *
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TBFOPRES.DCL".
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
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE "FONDOS PRESTAMOS"   TO  TIT-APLICACION
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P.
           MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-STATUS-TBFOPRES THRU F-STATUS-TBFOPRES.
           OPEN I-O TBFOPRES.
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
       F-MENU-OPCIONES. EXIT.
      *
       I-LISTA.
           DISPLAY " ".
       F-LISTA.
      *
       I-CREAR.
           PERFORM I-CODIGO        THRU F-CODIGO.
           IF CUP OR ESC           GO   F-CREAR.
           PERFORM I-LEE-TBFOPRES      THRU F-LEE-TBFOPRES.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOM-TBFOPRES
               MOVE 0 TO NIT-TBFOPRES DV-TBFOPRES
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REG-TBFOPRES.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-TBFOPRES   THRU F-LEE-TBFOPRES.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REG-TBFOPRES.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-TBFOPRES THRU F-LEE-TBFOPRES.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  COD-TBFOPRES.
           MOVE SPACE TO NOM-TBFOPRES.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVM1-TBFOPRES FIN-OK.
           PERFORM I-START-TBFOPRES   THRU F-START-TBFOPRES.
           PERFORM I-WINDOW-TBFOPRES  THRU F-WINDOW-TBFOPRES.
           IF SW-1 = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            " No se encuentran datos ... [ESC] " LINE 25 POSITION 01
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO F-CONSUL-MODE.
           PERFORM I-DISPLAY-TBFOPRES THRU F-DISPLAY-TBFOPRES
                   UNTIL I > 5 OR SW-1 = 1.
         LEE-TBFOPRES.
           MOVE MES-XX TO WI-Z2.
           DISPLAY WI-Z2 LINE 11 POSITION 31.
           ACCEPT MES-XX LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 1                 TO   I
                MOVE 15                TO   I-LINEA
                MOVE ZEROS             TO   FIN-OK
                MOVE MES-XX            TO   COD-TBFOPRES
                PERFORM I-START-TBFOPRES   THRU F-START-TBFOPRES
                PERFORM I-WINDOW-TBFOPRES  THRU F-WINDOW-TBFOPRES
                PERFORM I-DISPLAY-TBFOPRES THRU F-DISPLAY-TBFOPRES
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-TBFOPRES
                ELSE
                   GO LEE-TBFOPRES.
       F-CONSUL-MODE.
      *
       I-DISPLAY-TBFOPRES.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 24 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-TBFOPRES.
           MOVE COD-TBFOPRES TO WI-Z2.
           DISPLAY   WI-Z2                LINE I-LINEA POSITION 24.
           DISPLAY   NOM-TBFOPRES         LINE I-LINEA POSITION 29.
           IF I = 2  MOVE COD-TBFOPRES   TO MES-XX.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-TBFOPRES. EXIT.
      *
       I-START-TBFOPRES.
           MOVE ZEROS TO SW-1.
           START TBFOPRES KEY NOT LESS LLAVM1-TBFOPRES
                 INVALID KEY MOVE 1 TO SW-1.
       F-START-TBFOPRES.
      *
       I-GRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-TBFOPRES INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-TBFOPRES INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           DISPLAY
           "บ       [ E ]  BORRA REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE TBFOPRES INVALID KEY STOP " ".
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
           IF      ESC          GO   F-DATOS.
         DAT-02.
           PERFORM I-NIT        THRU F-NIT.
           IF      CUP          GO   I-DATOS.
           IF      ESC          GO   F-DATOS.
         DAT-03.
           PERFORM I-DV         THRU F-DV.
           IF      CUP          GO   DAT-02.
           IF      ESC          GO   F-DATOS.
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
           MOVE COD-TBFOPRES TO WI-Z2.
           DISPLAY   WI-Z2      LINE 11 POSITION 31.
           DISPLAY NOM-TBFOPRES LINE 13 POSITION 31.
           MOVE NIT-TBFOPRES TO WI-Z12C.
           DISPLAY WI-Z12C      LINE 14 POSITION 28.
           DISPLAY DV-TBFOPRES  LINE 14 POSITION 44.
       F-MUESTRA-CAMPOS. EXIT.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE COD-TBFOPRES TO WI-Z2.
           DISPLAY WI-Z2        LINE 11 POSITION 31.
           ACCEPT  COD-TBFOPRES LINE 11 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  COD-TBFOPRES = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO. EXIT.
      *
       I-NOMBRE.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY NOM-TBFOPRES LINE 13 POSITION 31
           ACCEPT  NOM-TBFOPRES LINE 13 POSITION 31
                                UPDATE TAB NO BEEP PROMPT ECHO
                             ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
           DISPLAY NOM-TBFOPRES LINE 13 POSITION 31
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-TBFOPRES = SPACES GO I-NOMBRE.
       F-NOMBRE. EXIT.
      *
       I-NIT.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 15 LINE 14 POSITION 28.
           DISPLAY NIT-TBFOPRES LINE 14 POSITION 28
           ACCEPT  NIT-TBFOPRES LINE 14 POSITION 28
                                UPDATE TAB NO BEEP PROMPT ECHO
                             ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
           MOVE NIT-TBFOPRES TO WI-Z12C.
           DISPLAY WI-Z12C LINE 14 POSITION 28.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NIT.
           IF  NIT-TBFOPRES = ZEROS GO I-NIT.
       F-NIT. EXIT.
      *
       I-DV.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY DV-TBFOPRES LINE 14 POSITION 44
           ACCEPT  DV-TBFOPRES LINE 14 POSITION 44
                                UPDATE TAB NO BEEP PROMPT ECHO
                             ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
           DISPLAY DV-TBFOPRES LINE 14 POSITION 44.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-DV.
           IF  NIT-TBFOPRES = ZEROS GO I-NIT.
       F-DV. EXIT.
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

       I-LEE-TBFOPRES.
               MOVE 0  TO   SW-1.
               READ TBFOPRES WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-TBFOPRES.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-TBFOPRES.
           IF CDN
              REWRITE REG-TBFOPRES.
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
           READ TBFOPRES NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


      *
       I-FIN-MODE.
           CLOSE TBFOPRES.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-TBFOPRES.
           DISPLAY  "ษออออออัอออออออออออออออออออออออออออออออออออออป"
                                LINE 12 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ COD. ณ          DESCRIPCION                บ"
                                LINE 13 POSITION 20
                                                       REVERSE.
           DISPLAY  "วฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
                                LINE 14 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ      ณ                                     บ"
                                LINE 15 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ      ณ                                     บ"
                                LINE 16 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ      ณ                                     บ"
                                LINE 17 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ      ณ                                     บ"
                                LINE 18 POSITION 20
                                                       REVERSE.
           DISPLAY  "บ      ณ                                    บ"
                                LINE 19 POSITION 20
                                                       REVERSE.
           DISPLAY  "ศออออออฯอออออออออออออออออออออออออออออออออออออผ"
                                LINE 20 POSITION 20                                                    "
                                                       REVERSE.
       F-WINDOW-TBFOPRES.
      *
       I-WINDOW-DATOS.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 20.
           DISPLAY "บ   ENTIDADES FONDOS PRESTAMOS             บ"
                                        LINE 07 POSITION 20 REVERSE.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 08 POSITION 20.
           DISPLAY "บ                                          บ"
                                        LINE 09 POSITION 20.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 10 POSITION 20.
           DISPLAY "บ   COD  : __                              บ"
                                        LINE 11 POSITION 20.
           DISPLAY "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
                                        LINE 12 POSITION 20.
           DISPLAY "บ NOMBRE :                                 บ"
                                        LINE 13 POSITION 20.
           DISPLAY "บ NIT :                                    บ"
                                        LINE 14 POSITION 20.
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 15 POSITION 20.
       F-WINDOW-DATOS. EXIT.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
      *
           COPY "..\STA\TBFOPRES.STA".
      ******************************************************************
