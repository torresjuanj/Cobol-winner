       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG060.
      ******************************************************************
      * CATALOGO DE NITS                MAY 01 DE 2002                 *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\TIPONACI.DYC".
               COPY "..\DYC\CONTDPTO.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\TIPONACI.FD".
               COPY "..\FD\CONTDPTO.FD".
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\HOJAVIDA.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG060".
       77      ID-PRG              PIC X(7)  VALUE "PROG060".
       77      NIT-ED              PIC ZZZZZZ,ZZZ,ZZZ  VALUE SPACES.               COPY "..\LBL\AUDITE.LBL".
       77      W-VARIABLES-NOMINA  PIC X(8)  VALUE  SPACES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
               COPY "..\LBL\TIPONACI.LBL".
               COPY "..\LBL\CONTDPTO.LBL".
               COPY "..\LBL\USER-ACT.LBL".

               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\VARIABLE.WRK".

               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\TIPONACI.DCL".
               COPY "..\DCL\CONTDPTO.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE "TABLA DE NITS" TO   TIT-APLICACION.
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS         THRU F-USUARIOS.
           IF      SW-USER = 1        EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE   THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CNNITCED  THRU F-STATUS-CNNITCED
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW     THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES    THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE         THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
               MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   CNNITCED
               OPEN INPUT HOJAVIDA NOMICIUD EMPRESAS
                          CONTDPTO TIPONACI.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE         W-LINEA TO   I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           DISPLAY "TABLA DE NITS" LINE 03 POSITION 25.
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.

           DISPLAY "[ ESC ] TERMINA" LINE 24 POSITION 01
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
             IF W-OPCION = "A"  AND PROSS-AKK = "S"
                MOVE  1  TO SW-MODE
                PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M"  AND PROSS-MKK = "S"
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
                   PERFORM I-START-NIT-NOM   THRU F-START-NIT-NOM
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R"  AND PROSS-RKK = "S"
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
           PERFORM I-NIT          THRU F-NIT.
           IF CUP OR ESC          GO   F-CREAR.
           PERFORM I-LEE-NIT      THRU F-LEE-NIT.
           IF  SW-1  =  2
               PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
           IF  SW-EOF = 5
               MOVE  ZEROS              TO SW-1.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE         THRU F-EXISTE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE SPACE          TO   NOMBM
               PERFORM I-DATOS     THRU F-DATOS
               IF CUP              GO   I-CREAR.
           MOVE SPACE TO REGMA1.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-NIT     THRU F-NIT.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-NIT   THRU F-LEE-NIT.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN OR ESC.
           MOVE SPACE TO REGMA1.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-NIT     THRU F-NIT.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-NIT   THRU F-LEE-NIT.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           PERFORM I-LEE-SIGUIENTE      THRU F-LEE-SIGUIENTE.
           IF  SW-1 = 1  MOVE  1 TO  CODIM.
           MOVE SPACE TO NOMBM.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE 01        TO UU.
           MOVE 13        TO I-LINEA.
           MOVE ZEROS     TO FIN-OK.
           DISPLAY "  NOMBRE   : " LINE 09 POSITION 16
           PERFORM I-WINDOW-NITS   THRU  F-WINDOW-NITS.
           PERFORM I-DISPLAY-NITS  THRU  F-DISPLAY-NITS
                   UNTIL UU > 09 OR FIN-OK = 1.
           MOVE ZEROS                TO  WK-EXCEPTION
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24  POSITION 80
                        CONTROL SG-CONTROL-STRING .
           DISPLAY "DIGITE <> OR NOMBRE A BUSCAR-->: "
                   LINE 24 POSITION 20.
           DISPLAY "  NOMBRE   : " LINE 09 POSITION 16
           ACCEPT  WK-NMBRE       LINE 09 POSITION 31
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 24  POSITION 80
                        CONTROL  SG-CONTROL-STRING .
            IF F1  OR ESC GO  F-CONSUL-MODE.
            IF FIN-OK = 1
                   PERFORM I-START-NIT-NOM  THRU F-START-NIT-NOM
                   DISPLAY SPACE LINE 16 POSITION 72
                   GO I-CONSUL-MODE.
            IF WK-NMBRE NOT = SPACES
               PERFORM I-START-NIT-NOM  THRU F-START-NIT-NOM
               MOVE  SPACES   TO  WK-NMBRE.
            GO I-CONSUL-MODE.
       F-CONSUL-MODE.
      *
       I-DISPLAY-NITS.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 24  POSITION 80
                        CONTROL  SG-CONTROL-STRING .
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   FIN-OK = 1
                DISPLAY "**" LINE     I-LINEA POSITION 50
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 48
                MOVE 12 TO UU
                GO   F-DISPLAY-NITS.
           MOVE  CODIM       TO NIT-ED.
           DISPLAY   NIT-ED       LINE I-LINEA POSITION 02.
           DISPLAY   DIGITO-CHEQUEO LINE I-LINEA POSITION 17
           DISPLAY   NOMBM        LINE I-LINEA POSITION 19 SIZE 25
           DISPLAY   TELEM        LINE I-LINEA POSITION 45.
           DISPLAY   DIREM        LINE I-LINEA POSITION 54 SIZE 17.
           DISPLAY   CIUDM        LINE I-LINEA POSITION 72 SIZE 05.
      *    DISPLAY   FLAG-NIT     LINE I-LINEA POSITION 78.
           ADD   1   TO  UU.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-NITS.
      *
       I-GRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 15 .
           PERFORM ACEPTA-TRUCO.
           MOVE "C"    TO FLAG-NIT.
           IF W-TRUCO = "G"
              WRITE REGMA1 INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 15 .
           PERFORM ACEPTA-TRUCO.
           IF FLAG-NIT = SPACES
              MOVE "C"    TO FLAG-NIT.
           MOVE    1       TO SW-CNNITCED.
           IF W-TRUCO = "G"
              REWRITE REGMA1 INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
           MOVE    SPACES  TO FLAG-NIT.
           MOVE    ZEROS   TO SW-CNNITCED.
       F-REGRABAR.
      *
       I-ELIMINAR.
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                            LINE 24 POSITION 15 .
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE CNNITCED INVALID KEY STOP " ".
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
           PERFORM I-DATOS-CAB  THRU F-DATOS-CAB
           IF        CUP OR ESC   GO F-DATOS.
           IF      SW-MODE = 1
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-GRABAR THRU F-GRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-DATOS
                   ELSE    GO F-DATOS
           ELSE
              IF   SW-MODE = 2
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-REGRABAR THRU F-REGRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP OR ESC
                   IF      CUP  GO I-DATOS.
       F-DATOS.
      *
       I-DATOS-CAB.
           PERFORM I-NOMBRE     THRU F-NOMBRE.
           IF        CUP          GO F-DATOS-CAB.
           IF        ESC          GO F-DATOS-CAB.
       I-PAIS.
           PERFORM I-TIPONACI   THRU F-TIPONACI.
           IF        CUP          GO I-DATOS-CAB.
           IF        ESC          GO F-DATOS-CAB.
       I-DPTO.
           PERFORM I-CONTDPTO   THRU F-CONTDPTO
           IF        CUP          GO I-PAIS.
           IF        ESC          GO F-DATOS-CAB.
       I-CIUD.
           PERFORM I-CIUDAD     THRU F-CIUDAD.
           IF        CUP          GO I-DPTO.
           IF        ESC          GO F-DATOS-CAB.
       I-DIRECCION.
           PERFORM I-DIRE       THRU F-DIRE.
           IF        CUP          GO I-CIUD.
           IF        ESC          GO F-DATOS-CAB.
       I-TELEFONO.
           PERFORM I-TEL        THRU F-TEL
           IF        CUP          GO I-DIRECCION.
           IF        ESC          GO F-DATOS-CAB.
       F-DATOS-CAB.
      *
       I-MUESTRA-CAMPOS.
           MOVE  CODIM                TO NIT-ED.
           DISPLAY NIT-ED             LINE 09 POSITION 31.
           DISPLAY DIGITO-CHEQUEO     LINE 9 POSITION 46
           DISPLAY NOMBM              LINE 11 POSITION 30 SIZE 43
           MOVE CNNITCED-COD-PAIS     TO COD-NAC.
           READ TIPONACI WITH NO LOCK INVALID  KEY
                   DISPLAY "N.N"      LINE 12 POSITION 30.
           DISPLAY CNNITCED-COD-PAIS  LINE 12 POSITION 30
           DISPLAY NOM-NAC            LINE 12 POSITION 35.
           MOVE CNNITCED-COD-DPTO     TO COD-DPTO.
           READ CONTDPTO WITH NO LOCK INVALID  KEY
                   DISPLAY "N.N"      LINE 13 POSITION 30.
           DISPLAY CNNITCED-COD-DPTO  LINE 13 POSITION 30
           DISPLAY NOM-DPTO           LINE 13 POSITION 35.
           MOVE CIUDM  TO CIUD-CODIGO.
           READ NOMICIUD WITH NO LOCK INVALID  KEY
                   DISPLAY "N.N"      LINE 14 POSITION 30.
           DISPLAY CIUDM              LINE 14 POSITION 30
           DISPLAY CIUD-NOMBRE        LINE 14 POSITION 35.
           DISPLAY DIREM              LINE 15 POSITION 30.
           DISPLAY TELEM              LINE 16 POSITION 30.
       F-MUESTRA-CAMPOS.
      *
       I-NIT.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CODIM LINE 09 POSITION 31
           DISPLAY SPACES LINE 09 POSITION 31  SIZE 18
           ACCEPT  CODIM LINE 09 POSITION 31
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE  CODIM       TO NIT-ED.
           DISPLAY NIT-ED LINE 09 POSITION 31.
           ACCEPT  DIGITO-CHEQUEO LINE 09 POSITION 46
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY DIGITO-CHEQUEO LINE 09 POSITION 46.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NIT.
           IF  CODIM = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                   BEEP GO I-NIT.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NIT.
      *
       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY NOMBM LINE 11 POSITION 30 SIZE 43
           ACCEPT  NOMBM LINE 11 POSITION 30 SIZE 43
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOMBM = SPACES GO I-NOMBRE.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-NOMBRE.
      *
       I-TIPONACI.
           MOVE ZEROS                  TO  WK-EXCEPTION
           IF CNNITCED-COD-PAIS = ZEROS
           MOVE 57                     TO  CNNITCED-COD-PAIS.
           DISPLAY CNNITCED-COD-PAIS  LINE 12 POSITION 30
           ACCEPT  CNNITCED-COD-PAIS  LINE 12 POSITION 30
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-TIPONACI.
           MOVE CNNITCED-COD-PAIS  TO COD-NAC.
           READ TIPONACI WITH NO LOCK INVALID  KEY
                GO I-TIPONACI.
           DISPLAY NOM-NAC                    LINE 12 POSITION 35.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-TIPONACI.

       I-CONTDPTO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           IF CNNITCED-COD-DPTO = ZEROS
           MOVE 1                      TO  CNNITCED-COD-DPTO.
           DISPLAY CNNITCED-COD-DPTO  LINE 13 POSITION 30
           ACCEPT  CNNITCED-COD-DPTO  LINE 13 POSITION 30
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CONTDPTO.
           MOVE CNNITCED-COD-DPTO  TO COD-DPTO.
           READ CONTDPTO WITH NO LOCK INVALID  KEY
                GO I-CONTDPTO.
           DISPLAY NOM-DPTO                   LINE 13 POSITION 35.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CONTDPTO.

       I-CIUDAD.
           MOVE ZEROS                  TO  WK-EXCEPTION
           IF CIUDM             = ZEROS
           MOVE 1                      TO  CIUDM.
           DISPLAY CIUDM  LINE 14 POSITION 30
           ACCEPT  CIUDM  LINE 14 POSITION 30
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CIUDAD.
           MOVE CIUDM  TO CIUD-CODIGO.
           READ NOMICIUD WITH NO LOCK INVALID  KEY
                GO I-CIUDAD.
           DISPLAY CIUD-NOMBRE                LINE 14 POSITION 35.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CIUDAD.
      *
       I-DIRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY DIREM  LINE 15 POSITION 30
           ACCEPT  DIREM  LINE 15 POSITION 30
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-DIRE.
           IF  DIREM = SPACES GO I-DIRE.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-DIRE.
      *
       I-TEL.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY TELEM LINE 16 POSITION 30
           ACCEPT  TELEM LINE 16 POSITION 30
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-TEL.
           IF  TELEM = SPACES GO I-TEL.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-TEL.

       I-LEE-HOJAVIDA.
           MOVE ZEROS  TO SW-EOF.
           MOVE CODIM  TO HOJAVIDA-CLAVE.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
                   MOVE 1 TO SW-EOF
                   GO F-LEE-HOJAVIDA.
           MOVE    HOJAVIDA-EMPRESA    TO  WIN-CODIGO-EMPRESA
           READ EMPRESAS WITH NO LOCK INVALID KEY
                   MOVE 0 TO SW-EOF.
           MOVE    HOJAVIDA-NOMBRE     TO  NOMBM.
           MOVE    HOJAVIDA-CLAVE      TO  CODIM.
           MOVE    HOJAVIDA-DIR        TO  DIREM
           MOVE    WIN-COD-CIUDAD   TO  CIUDM
           MOVE    HOJAVIDA-TEL        TO  TELEM.
           MOVE    "N"              TO  FLAG-NIT.
           MOVE     0               TO  SW-CNNITCED.
           MOVE     5               TO  SW-EOF.
           WRITE   REGMA1 INVALID KEY REWRITE REGMA1.
       F-LEE-HOJAVIDA.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "ADICIONA"   LINE  7 POSITION 37 BLINK.
           IF  W-OPCION = "M"
               DISPLAY "MODIFICA"   LINE  7 POSITION 37 BLINK.
           IF  W-OPCION = "C"
               DISPLAY "CONSULTA"   LINE  7 POSITION 37 BLINK.
           IF  W-OPCION = "R"
               DISPLAY "ELIMINAR"   LINE  7 POSITION 37 BLINK.
       F-DISPLAY-OPCION.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************

       I-LEE-NIT.
               MOVE 0  TO   SW-1.
               READ CNNITCED WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-NIT.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REGMA1.
           IF CDN
              REWRITE REGMA1.
           PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.

       I-CONSULTA.
           DISPLAY " <> REGISTRO SIGUIENTE "
                   LINE 24 POSITION 01
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54
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
           MOVE ZEROS      TO SW-1 FIN-OK
           READ CNNITCED NEXT RECORD WITH NO LOCK AT END
                MOVE 1 TO FIN-OK  SW-1.
       F-LEE-SIGUIENTE.
      *
       I-START-NIT-NOM.
           MOVE  WK-NMBRE   TO  NOMBM.
           START CNNITCED  KEY NOT LESS LLAV-NOMB
                 INVALID KEY  MOVE SPACES TO WK-NMBRE
                 GO I-START-NIT-NOM.
       F-START-NIT-NOM.
      *
       I-STATUS-CNNITCED.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNNITCED
               MOVE 1                      TO  SW-ABRE
               IF  CNNITCED-STATUS              =  "35"
                   OPEN OUTPUT CNNITCED
               ELSE
                   IF  CNNITCED-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNNITCED
                       PERFORM I-FIN-MODE  THRU F-FIN-MODE.
               CLOSE CNNITCED.
       F-STATUS-CNNITCED.
      *
       I-FIN-MODE.
           CLOSE CNNITCED NOMICIUD HOJAVIDA EMPRESAS
                 TIPONACI CONTDPTO.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-NITS.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 24  POSITION 80
                        CONTROL  SG-CONTROL-STRING .
           DISPLAY
           "ษออออออออออออออออหอออออออออออออออออออออออออหออออออออหอออออออ
      -    "ออออออออออหออออออป"    LINE 10 POSITION 01

           "บ  N . I . T     บ      N o m b r e        บTelefonoบ  Direc
      -    "ccion     ณCiudadบ"    LINE 11 POSITION 01
           "ฬออออออออออออออออฮอออออออออออออออออออออออออฮออออออออฮอออออออ
      -    "ออออออออออฮออออออน"    LINE 12 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 13 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 14 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 15 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 16 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 17 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 18 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 19 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 20 POSITION 01
           "บ                บ                         บ        บ
      -    "          บ      บ"    LINE 21 POSITION 01
           "ศออออออออออออออออสอออออออออออออออออออออออออสออออออออสอออออออ
      -    "ออออออออออสออออออผ"    LINE 22 POSITION 01 .
       F-WINDOW-NITS.
      *
       I-WINDOW-DATOS.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24  POSITION 80
                        CONTROL SG-CONTROL-STRING .
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 15
           "บ                                                         บ"
                                        LINE 07 POSITION 15
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 08 POSITION 15
           "บ   NIT/CC No:                                            บ"
                                        LINE 09 POSITION 15
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 10 POSITION 15
           "บ  Nombre.....:                                           บ"
                                        LINE 11 POSITION 15
           "บ  Paกs.......:                                           บ"
                                        LINE 12 POSITION 15
           "บ  Dpto/State.:                                           บ"
                                        LINE 13 POSITION 15
           "บ  Ciudad/City:                                           บ"
                                        LINE 14 POSITION 15
           "บ  Direcciขn..:                                           บ"
                                        LINE 15 POSITION 15
           "บ  Telefono...:                                           บ"
                                        LINE 16 POSITION 15
           "บ                                                         บ"
                                        LINE 17 POSITION 15
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 18 POSITION 15.
       F-WINDOW-DATOS.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\AUDITE.PRO".

