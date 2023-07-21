       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG075.
      *----------------------------------------------------------------*
      * Modulo de liquidaciขn de vendedores, manejo tabla vendedor con *
      * su respectiva empresa y modalidad del contrato.  ABR-27-2002   *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\VENDEMPR.DYC".
           COPY "..\DYC\MODACONT.DYC".
           COPY "..\DYC\AUDICAMP.DYC".
           COPY "..\DYC\HOJAVIDA.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\VENDEMPR.FD".
           COPY "..\FD\MODACONT.FD".
           COPY "..\FD\AUDICAMP.FD".
           COPY "..\FD\HOJAVIDA.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA         PIC X(7)  VALUE  "PROG075".
       77  ID-PRG             PIC X(7)  VALUE  "PROG075".
       77  W-VARIABLES-NOMINA PIC X(7)  VALUE  SPACES.
       01 W-AUDI-CEDULA.
        03 W-AUDI-CEDULA-D PIC 9(09) VALUE ZEROS.
        03 W-AUDI-EMPRES-D PIC 9(03) VALUE ZEROS.
      *
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\VENDEMPR.LBL".
           COPY "..\LBL\MODACONT.LBL".
           COPY "..\LBL\AUDICAMP.LBL".
           COPY "..\LBL\HOJAVIDA.LBL".
      *
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\COMANDOS.WRK".
           COPY "..\WRK\VENDEMPR.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\VENDEMPR.DCL".
           COPY "..\DCL\MODACONT.DCL".
           COPY "..\DCL\AUDICAMP.DCL".
           COPY "..\DCL\HOJAVIDA.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER   =   1  EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS.
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS.
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW.
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE " Seller Company   "   TO  TIT-APLICACION
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P.
           MOVE 16                     TO  POS-DOWN.
           PERFORM I-STATUS-VENDEMPR THRU F-STATUS-VENDEMPR.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT MODACONT HOJAVIDA EMPRESAS.
           OPEN I-O VENDEMPR.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA.
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
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF W-OPCION = "M" AND PROSS-MKK = "S"
                  MOVE  2  TO SW-MODE
                  PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                   UNTIL ESC
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF W-OPCION = "C" AND PROSS-CKK = "S"
                   PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                    UNTIL ESC
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF W-OPCION = "R" AND PROSS-RKK = "S"
                      PERFORM  I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                      PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM  I-ELIMINA        THRU F-ELIMINA
                       UNTIL ESC
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
           IF ESC  GO F-CREAR.
         ADEMP.
           PERFORM I-EMPRESA       THRU F-EMPRESA.
           IF ESC  GO F-CREAR.
           IF CUP GO TO I-CREAR.
           PERFORM I-LEE-VENDEMPR  THRU F-LEE-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = ZEROS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            PERFORM I-EXISTE         THRU F-EXISTE
            GO I-CREAR
           ELSE
            PERFORM I-INICIA-REGISTRO THRU F-INICIA-REGISTRO
            PERFORM I-DATOS     THRU F-DATOS
            IF CUP              GO   ADEMP
            ELSE IF ESC GO F-CREAR.
       F-CREAR. EXIT.
      *
       I-INICIA-REGISTRO.
           MOVE ZEROS TO WRK-REG-VENDEMPR.
           MOVE VENDEMPR-EMPRESA TO VENDEMPR-EMPRE01.
           MOVE ZEROS TO VENDEMPR-CODMODA.
       F-INICIA-REGISTRO. EXIT.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-EMPRESA       THRU F-EMPRESA.
           IF ESC  GO F-CREAR.
           IF CUP GO TO I-CREAR.
           PERFORM I-LEE-VENDEMPR   THRU F-LEE-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
            PERFORM I-NEXISTE         THRU F-NEXISTE
            GO I-ELIMINA
           ELSE
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
             UNTIL W-TRUCO = "E" OR ESC.
           MOVE SPACES TO REG-VENDEMPR.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA. EXIT.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF ESC        GO   F-MODIFY-MODE.
         MOEMP.
           PERFORM I-EMPRESA       THRU F-EMPRESA.
           IF ESC  GO F-MODIFY-MODE.
           IF CUP GO TO I-MODIFY-MODE.
           PERFORM I-LEE-VENDEMPR THRU F-LEE-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
            PERFORM I-NEXISTE THRU F-NEXISTE
            GO I-MODIFY-MODE
           ELSE
            PERFORM I-DATOS-ACTUALES THRU F-DATOS-ACTUALES
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            PERFORM I-DATOS          THRU F-DATOS
            IF CUP  GO  MOEMP
            ELSE IF ESC  GO  F-MODIFY-MODE.
       F-MODIFY-MODE. EXIT.
      *
       I-DATOS-ACTUALES.
           MOVE VENDEMPR-VENDEDO  TO WRK-VENDEMPR-VENDEDO.
           MOVE VENDEMPR-EMPRESA  TO WRK-VENDEMPR-EMPRESA.
           MOVE VENDEMPR-CODMODA  TO WRK-VENDEMPR-CODMODA.
       F-DATOS-ACTUALES. EXIT.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVE-VENDEMPR FIN-ARCH-VENDEMPR.
           PERFORM I-START-VENDEMPR   THRU F-START-VENDEMPR.
           PERFORM I-WINDOW-VENDEMPR  THRU F-WINDOW-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "File is empty ... [ ESC ] continue"
            LINE 25 POSITION 01 BEEP
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO F-CONSUL-MODE.
           PERFORM I-DISPLAY-VENDEMPR THRU F-DISPLAY-VENDEMPR
            UNTIL I > 5 OR FIN-ARCH-VENDEMPR = 1.
         LEE-VENDEMPR.
           DISPLAY WK-CODIGO LINE 08 POSITION 40.
           ACCEPT WK-CODIGO LINE 08 POSITION 40
                         UPDATE TAB NO BEEP PROMPT ECHO
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           DISPLAY W-EMPR LINE 11 POSITION 40
           ACCEPT W-EMPR LINE 11 POSITION 40
                         UPDATE TAB NO BEEP PROMPT ECHO
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.

           IF CDN
            MOVE 01                TO   I
            MOVE 15                TO   I-LINEA
            MOVE ZEROS             TO   FIN-ARCH-VENDEMPR
            MOVE WK-CODIGO         TO   VENDEMPR-VENDEDO
            MOVE W-EMPR            TO   VENDEMPR-EMPRESA
            PERFORM I-START-VENDEMPR   THRU F-START-VENDEMPR
            PERFORM I-WINDOW-VENDEMPR  THRU F-WINDOW-VENDEMPR
            PERFORM I-DISPLAY-VENDEMPR THRU F-DISPLAY-VENDEMPR
             UNTIL I > 5
            IF FIN-ARCH-VENDEMPR = 1
             DISPLAY SPACE LINE 19 POSITION 64 REVERSE
             IF I-LINEA = 16
              MOVE 01 TO WK-CODIGO
              GO LEE-VENDEMPR
             ELSE
              GO LEE-VENDEMPR
            ELSE
             GO LEE-VENDEMPR.
       F-CONSUL-MODE. EXIT.
      *
       I-DISPLAY-VENDEMPR.
           PERFORM I-LEE-C-VENDEMPR THRU F-LEE-C-VENDEMPR.
           IF FIN-ARCH-VENDEMPR = 1
            DISPLAY "**" LINE I-LINEA POSITION 24 REVERSE
            "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
            MOVE 6 TO I
            GO F-DISPLAY-VENDEMPR.
           MOVE VENDEMPR-VENDEDO TO WI-Z12C.
           DISPLAY WI-Z12C LINE I-LINEA POSITION 21.
           DISPLAY VENDEMPR-EMPRESA LINE I-LINEA POSITION 37.
           DISPLAY VENDEMPR-CODMODA LINE I-LINEA POSITION 41.
           MOVE VENDEMPR-CODMODA TO MODACONT-CODI
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT
           IF FIN-ARCH-MODACONT = 1
            MOVE ZEROS TO MODACONT-PRC-PAGO
                          MODACONT-PRC-ADMI.
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3.
           DISPLAY WI-Z3P3        LINE I-LINEA POSITION 45.
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3.
           DISPLAY WI-Z3P3        LINE I-LINEA POSITION 53.
           IF I = 2  MOVE VENDEMPR-VENDEDO TO WK-CODIGO
            MOVE VENDEMPR-EMPRESA TO W-EMPR.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-VENDEMPR. EXIT.
      *
       I-START-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           START VENDEMPR KEY NOT LESS LLAVE-VENDEMPR
            INVALID KEY MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-START-VENDEMPR. EXIT.
      *
       I-GRABAR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[ G ]  Add record     " LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancel "        LINE 25 POSITION 35 REVERSE
           "[  ] Return "          LINE 25 POSITION 55 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
            PERFORM I-AUDITORIA-VENDEMPR THRU F-AUDITORIA-VENDEMPR
            WRITE REG-VENDEMPR INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "ญ atenciขn ! No puede grabar ... " LINE 25 POSITION 01
             REVERSE BEEP BLINK
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-GRABAR. EXIT.
      *
       I-REGRABAR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[ G ]  Add Record     " LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancel "        LINE 25 POSITION 35 REVERSE
           "[  ] Return "          LINE 25 POSITION 55 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
            PERFORM I-AUDITORIA-VENDEMPR THRU F-AUDITORIA-VENDEMPR
            REWRITE REG-VENDEMPR INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "ญ atention ! You can not create record. "
              LINE 25 POSITION 01
             REVERSE BEEP BLINK
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-REGRABAR. EXIT.
      *
       I-ELIMINAR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[ E ]  Delete record  " LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancel "        LINE 25 POSITION 35 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
            DELETE VENDEMPR INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "ญ atention ! You can not delete record..."
              LINE 25 POSITION 01
             REVERSE BEEP BLINK
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-ELIMINAR. EXIT.
      *
       I-EXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
           "Record is create...." LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-EXISTE. EXIT.
      *
       I-NEXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
           "Record not exist...." LINE 25 POSITION 01.
           PERFORM ACEPTA-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-NEXISTE. EXIT.
      *
       I-DATOS.
         DAT-01.
           PERFORM I-CODMODA    THRU F-CODMODA.
           IF CUP OR ESC   GO   F-DATOS.

           IF SW-MODE = 1
            MOVE ZEROS  TO WK-EXCEPTION
            MOVE SPACES TO W-TRUCO
            PERFORM I-GRABAR THRU F-GRABAR
             UNTIL W-TRUCO = "G" OR CUP OR ESC
            IF CUP GO I-DATOS
            ELSE NEXT SENTENCE
           ELSE
            IF SW-MODE = 2
             MOVE ZEROS  TO WK-EXCEPTION
             MOVE SPACES TO W-TRUCO
             PERFORM I-REGRABAR THRU F-REGRABAR
              UNTIL W-TRUCO = "G" OR CUP OR ESC
             IF CUP GO I-DATOS.
       F-DATOS. EXIT.
      *
       I-MUESTRA-CAMPOS.
           MOVE VENDEMPR-VENDEDO TO WI-Z12C HOJAVIDA-CEDULA.
           DISPLAY WI-Z12C LINE 08 POSITION 40
           DISPLAY SPACES SIZE 42 LINE 09 POSITION 21.
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
           IF FIN-ARCH-HOJAVIDA = 1
            DISPLAY "N.N."       LINE 09 POSITION 22
           ELSE
            DISPLAY HOJAVIDA-NOMBRE LINE 09 POSITION 22.
           MOVE VENDEMPR-EMPRESA TO WIN-CODIGO-EMPRESA
           DISPLAY VENDEMPR-EMPRESA LINE 11 POSITION 40
           DISPLAY SPACES SIZE 42 LINE 12 POSITION 21.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS
           IF FIN-ARCH-EMPRESAS = 1
            DISPLAY "N.N." LINE 12 POSITION 22
           ELSE
            DISPLAY WIN-EMPRESA LINE 12 POSITION 22 SIZE 40.
           MOVE VENDEMPR-CODMODA TO WI-Z3 MODACONT-CODI
           DISPLAY WI-Z3 LINE 14 POSITION 43.
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            MOVE ZEROS TO MODACONT-PRC-PAGO
                          MODACONT-PRC-ADMI.
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 15 POSITION 31.
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 15 POSITION 55.
       F-MUESTRA-CAMPOS. EXIT.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY SPACES SIZE 15 LINE 08 POSITION 40.
           DISPLAY VENDEMPR-VENDEDO LINE 08 POSITION 40.
           ACCEPT  VENDEMPR-VENDEDO LINE 08 POSITION 40
                                    UPDATE TAB NO BEEP PROMPT ECHO
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ESC GO F-CODIGO.
           IF CEND
            MOVE 1 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-CODIGO.
           IF VENDEMPR-VENDEDO = ZEROS
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Number ID is Equal of zeros..." LINE 25 POSITION 01
            REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO I-CODIGO.
           MOVE VENDEMPR-VENDEDO TO WI-Z12C HOJAVIDA-CEDULA.
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA.
           IF FIN-ARCH-HOJAVIDA = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Number Ident. not create......F10-ACCEPT ZEROS."
             LINE 25 POSITION 01 BEEP
            PERFORM ACEPTA-TRUCO
            IF F10 GO F-CODIGO
            ELSE
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO TO I-CODIGO.
           DISPLAY WI-Z12C LINE 08 POSITION 40
           HOJAVIDA-NOMBRE LINE 09 POSITION 22.
       F-CODIGO. EXIT.
      *
       I-EMPRESA.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY VENDEMPR-EMPRESA LINE 11 POSITION 40.
           ACCEPT  VENDEMPR-EMPRESA LINE 11 POSITION 40
                                    UPDATE TAB NO BEEP PROMPT ECHO
                                 ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ESC GO F-EMPRESA.
           IF CUP GO F-EMPRESA.
           IF CEND
            MOVE 2 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-CODIGO.
           IF VENDEMPR-EMPRESA = ZEROS
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Company is equal to zeros" LINE 25 POSITION 01
            REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO I-EMPRESA.
           MOVE VENDEMPR-EMPRESA TO WIN-CODIGO-EMPRESA.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF FIN-ARCH-EMPRESAS = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Company not create..." LINE 25 POSITION 01 BEEP
            PERFORM ACEPTA-TRUCO
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO TO I-EMPRESA.
           DISPLAY VENDEMPR-EMPRESA LINE 11 POSITION 40
           WIN-EMPRESA LINE 12 POSITION 22 SIZE 40.
       F-EMPRESA. EXIT.
      *
       I-CODMODA.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           SPACES SIZE 80 LINE 24 POSITION 01
           " [F1] AYUDA " LINE 25 POSITION 01 REVERSE
           DISPLAY VENDEMPR-CODMODA LINE 14 POSITION 43.
           ACCEPT  VENDEMPR-CODMODA LINE 14 POSITION 43
                                 UPDATE TAB NO BEEP PROMPT ECHO
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           IF F1
            PERFORM I-AYUDA-MODACONT THRU F-AYUDA-MODACONT UNTIL ESC
            PERFORM I-PANTA             THRU F-PANTA
            PERFORM I-WINDOW-DATOS      THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS    THRU F-MUESTRA-CAMPOS
            GO I-CODMODA.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  ESC GO F-CODMODA.
           IF  CUP GO F-CODMODA.
           IF CEND
            MOVE 3 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-CODMODA.
           IF VENDEMPR-CODMODA = ZEROS
            GO TO I-CODMODA.
           MOVE VENDEMPR-CODMODA TO WI-Z3 MODACONT-CODI.
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Code mode Seller not exist." LINE 25 POSITION 01 BEEP
            PERFORM ACEPTA-TRUCO
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO TO I-CODMODA.
           DISPLAY WI-Z3 LINE 14 POSITION 43
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 15 POSITION 31
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 15 POSITION 55.
       F-CODMODA. EXIT.
      *
       I-DISPLAY-OPCION.
           IF  W-OPCION = "A"
            DISPLAY " Add    " LINE  4 POSITION 37 BLINK.
           IF  W-OPCION = "M"
            DISPLAY "Modify  " LINE  4 POSITION 37 BLINK.
           IF  W-OPCION = "C"
            DISPLAY "Consult " LINE  4 POSITION 37 BLINK.
           IF  W-OPCION = "R"
            DISPLAY "Delete  " LINE  4 POSITION 37 BLINK.
       F-DISPLAY-OPCION. EXIT.
      *
       I-LEE-C-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           READ VENDEMPR NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-LEE-C-VENDEMPR. EXIT.
      *
       I-WINDOW-VENDEMPR.
           DISPLAY
           "ษอออออออออออออออหอออหอออหอออออออหออออออออป"
           LINE 12 POSITION 20 REVERSE
           "บ Number IdentifบComบModบ%pay   บ%admtionบ"
           LINE 13 POSITION 20 REVERSE
           "ฬอออออออออออออออฮอออฮอออฮอออออออฮออออออออน"
           LINE 14 POSITION 20 REVERSE
           "บ               บ   บ   บ       บ        บ"
           LINE 15 POSITION 20 REVERSE
           "บ               บ   บ   บ       บ        บ"
           LINE 16 POSITION 20 REVERSE
           "บ               บ   บ   บ       บ        บ"
           LINE 17 POSITION 20 REVERSE
           "บ               บ   บ   บ       บ        บ"
           LINE 18 POSITION 20 REVERSE
           "บ               บ   บ   บ       บ       บ"
           LINE 19 POSITION 20 REVERSE
           "ศอออออออออออออออสอออสอออสอออออออสออออออออผ"
           LINE 20 POSITION 20 REVERSE.
       F-WINDOW-VENDEMPR.
      *
       I-WINDOW-DATOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออออออออออออออออออออออออออออออออออออออป"
           LINE 05 POSITION 20
           "บ      MANAGER COMPANY ADN MODE CONTRACT   บ"
           LINE 06 POSITION 20 REVERSE
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 07 POSITION 20
           "บ Number Identif.:                         บ"
           LINE 08 POSITION 20
           "บ                                          บ"
           LINE 09 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 10 POSITION 20
           "บ Code Company...:                         บ"
           LINE 11 POSITION 20
           "บ                                          บ"
           LINE 12 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 13 POSITION 20
           "บ Mode Contract...:                        บ"
           LINE 14 POSITION 20
           "บ % pay  :         % Take admon  :         บ"
           LINE 15 POSITION 20
           "ศออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 16 POSITION 20.
       F-WINDOW-DATOS. EXIT.
      *
       I-LEE-VENDEMPR.
           MOVE ZEROS TO FIN-ARCH-VENDEMPR.
           READ VENDEMPR WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-VENDEMPR.
       F-LEE-VENDEMPR. EXIT.
      *
       I-LEE-HOJAVIDA.
           MOVE ZEROS TO FIN-ARCH-HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-HOJAVIDA.
       F-LEE-HOJAVIDA. EXIT.
      *
       I-LEE-EMPRESAS.
           MOVE ZEROS TO FIN-ARCH-EMPRESAS.
           READ EMPRESAS WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-EMPRESAS.
       F-LEE-EMPRESAS. EXIT.
      *
       I-LEE-MODACONT.
           MOVE ZEROS TO FIN-ARCH-MODACONT.
           READ MODACONT WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-MODACONT.
       F-LEE-MODACONT. EXIT.
      *
       I-FIN-MODE.
           CLOSE MODACONT VENDEMPR HOJAVIDA EMPRESAS.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUVENEMP.PRO".
           COPY "..\PRO\AYAUDIVE.PRO".
           COPY "..\PRO\AYMODCON.PRO".
      *
           COPY "..\STA\VENDEMPR.STA".
           COPY "..\STA\AUDICAMP.STA".
      ******************************************************************
