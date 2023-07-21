       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG080.
      *----------------------------------------------------------------*
      * Modulo de liquidaciขn de vendedores, manejo tabla modalidades  *
      * del contrato.        ABR-27-2002                               *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\MODACONT.DYC".
           COPY "..\DYC\AUDICAMP.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\MODACONT.FD".
           COPY "..\FD\AUDICAMP.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA         PIC X(7)  VALUE  "PROG080".
       77  ID-PRG             PIC X(7)  VALUE  "PROG080".
       77  W-VARIABLES-NOMINA PIC X(7)  VALUE  SPACES.
      *
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\MODACONT.LBL".
           COPY "..\LBL\AUDICAMP.LBL".
      *
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\COMANDOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\MODACONT.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\MODACONT.DCL".
           COPY "..\DCL\AUDICAMP.DCL".
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
           MOVE "MODALIDAD C. VENTA"   TO  TIT-APLICACION
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P.
           MOVE 16                     TO  POS-DOWN.
           PERFORM I-STATUS-MODACONT THRU F-STATUS-MODACONT.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           OPEN I-O MODACONT.
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
           PERFORM I-LEE-MODACONT  THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = ZEROS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            PERFORM I-EXISTE         THRU F-EXISTE
            GO I-CREAR
           ELSE
            PERFORM I-INICIA-REGISTRO THRU F-INICIA-REGISTRO
            PERFORM I-DATOS     THRU F-DATOS
            IF CUP              GO   I-CREAR
            ELSE IF ESC GO F-CREAR.
           MOVE SPACE TO REG-MODACONT.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR. EXIT.
      *
       I-INICIA-REGISTRO.
            MOVE ZEROS TO WRK-REG-MODACONT.
            MOVE ZEROS TO MODACONT-PRC-PAGO
                          MODACONT-PRC-ADMI.
       F-INICIA-REGISTRO. EXIT.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-MODACONT   THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            PERFORM I-NEXISTE         THRU F-NEXISTE
            GO I-ELIMINA
           ELSE
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
             UNTIL W-TRUCO = "E" OR ESC.
           MOVE SPACES TO REG-MODACONT.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA. EXIT.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-MODACONT THRU F-LEE-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            PERFORM I-NEXISTE THRU F-NEXISTE
            GO I-MODIFY-MODE
           ELSE
            PERFORM I-DATOS-ACTUALES THRU F-DATOS-ACTUALES
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            PERFORM I-DATOS          THRU F-DATOS
            IF CUP  GO  I-MODIFY-MODE
            ELSE IF ESC  GO  F-MODIFY-MODE.
           MOVE SPACE TO REG-MODACONT.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE. EXIT.
      *
       I-DATOS-ACTUALES.
           MOVE MODACONT-CODI     TO WRK-MODACONT-CODI.
           MOVE MODACONT-PRC-PAGO TO WRK-MODACONT-PRC-PAGO.
           MOVE MODACONT-PRC-ADMI TO WRK-MODACONT-PRC-ADMI.
       F-DATOS-ACTUALES. EXIT.
      *
       I-CONSUL-MODE.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE ZEROS TO LLAVE-MODACONT FIN-ARCH-MODACONT.
           PERFORM I-START-MODACONT   THRU F-START-MODACONT.
           PERFORM I-WINDOW-MODACONT  THRU F-WINDOW-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Archivo sin datos ... [ ESC ] continua"
            LINE 25 POSITION 01 BEEP
            MOVE ZEROS TO WK-EXCEPTION
            PERFORM ACEPTA-TRUCO UNTIL ESC
            GO F-CONSUL-MODE.
           PERFORM I-DISPLAY-MODACONT THRU F-DISPLAY-MODACONT
            UNTIL I > 5 OR FIN-ARCH-MODACONT = 1.
         LEE-MODACONT.
           MOVE W-EMPR TO WI-Z3.
           DISPLAY WI-Z3 LINE 11 POSITION 31.
           ACCEPT W-EMPR LINE 11 POSITION 31
                         UPDATE TAB NO BEEP PROMPT ECHO
                      ON EXCEPTION WK-EX
                         PERFORM 999-EXCEPTION.
           IF CDN
            MOVE 01                TO   I
            MOVE 15                TO   I-LINEA
            MOVE ZEROS             TO   FIN-ARCH-MODACONT
            MOVE W-EMPR            TO   LLAVE-MODACONT
            PERFORM I-START-MODACONT   THRU F-START-MODACONT
            PERFORM I-WINDOW-MODACONT  THRU F-WINDOW-MODACONT
            PERFORM I-DISPLAY-MODACONT THRU F-DISPLAY-MODACONT
             UNTIL I > 5
            IF FIN-ARCH-MODACONT = 1
             DISPLAY SPACE LINE 19 POSITION 64 REVERSE
             IF I-LINEA = 16
              MOVE 01 TO WK-CODIGO
              GO LEE-MODACONT
             ELSE
              GO LEE-MODACONT
            ELSE
             GO LEE-MODACONT.
       F-CONSUL-MODE. EXIT.
      *
       I-DISPLAY-MODACONT.
           PERFORM I-LEE-C-MODACONT THRU F-LEE-C-MODACONT.
           IF FIN-ARCH-MODACONT = 1
            DISPLAY "**" LINE I-LINEA POSITION 24 REVERSE
            "Fin Archivo" LINE I-LINEA POSITION 29 REVERSE
            MOVE 6 TO I
            GO F-DISPLAY-MODACONT.
           MOVE MODACONT-CODI TO WI-Z3.
           DISPLAY   WI-Z3                LINE I-LINEA POSITION 22.
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3.
           DISPLAY WI-Z3P3                LINE I-LINEA POSITION 29.
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3.
           DISPLAY WI-Z3P3                LINE I-LINEA POSITION 39.
           IF I = 2  MOVE MODACONT-CODI TO W-EMPR.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-MODACONT. EXIT.
      *
       I-START-MODACONT.
           MOVE ZEROS TO FIN-ARCH-MODACONT.
           START MODACONT KEY NOT LESS LLAVE-MODACONT
            INVALID KEY MOVE 1 TO FIN-ARCH-MODACONT.
       F-START-MODACONT. EXIT.
      *
       I-GRABAR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[ G ]  Grabar registro" LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancela"        LINE 25 POSITION 35 REVERSE
           "[  ] Regresa"          LINE 25 POSITION 55 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
            PERFORM I-AUDITORIA-MODACONT THRU F-AUDITORIA-MODACONT
            WRITE REG-MODACONT INVALID KEY
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
           "[ G ]  Grabar registro" LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancela"        LINE 25 POSITION 35 REVERSE
           "[  ] Regresa"          LINE 25 POSITION 55 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
            PERFORM I-AUDITORIA-MODACONT THRU F-AUDITORIA-MODACONT
            REWRITE REG-MODACONT INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "ญ atenciขn ! No puede grabar ... " LINE 25 POSITION 01
             REVERSE BEEP BLINK
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-REGRABAR. EXIT.
      *
       I-ELIMINAR.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "[ E ]  Borrar registro" LINE 25 POSITION 10 REVERSE
           "[ ESC ] Cancela"        LINE 25 POSITION 35 REVERSE
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
            DELETE MODACONT INVALID KEY
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             "ญ atenciขn ! No puede borrar ... " LINE 25 POSITION 01
             REVERSE BEEP BLINK
             PERFORM ACEPTA-TRUCO
             PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-ELIMINAR. EXIT.
      *
       I-EXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
           "Registro ya existe..." LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-EXISTE. EXIT.
      *
       I-NEXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
           "Registro no existe..." LINE 25 POSITION 01.
           PERFORM ACEPTA-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-NEXISTE. EXIT.
      *
       I-DATOS.
           IF CUP GO DAT-02.
         DAT-01.
           PERFORM I-PRC-PAGO   THRU F-PRC-PAGO.
           IF CUP OR ESC   GO   F-DATOS.
         DAT-02.
           PERFORM I-PRC-ADMI   THRU F-PRC-ADMI.
           IF      CUP          GO   DAT-01.
           IF ESC GO F-DATOS.

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
           MOVE MODACONT-CODI TO WI-Z3.
           DISPLAY WI-Z3 LINE 11 POSITION 31
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 13 POSITION 52
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 14 POSITION 52.
       F-MUESTRA-CAMPOS. EXIT.
      *
       I-CODIGO.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY MODACONT-CODI LINE 11 POSITION 31.
           ACCEPT  MODACONT-CODI LINE 11 POSITION 31
                                 UPDATE TAB NO BEEP PROMPT ECHO
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  ESC GO F-CODIGO.
           IF CEND
            MOVE 1 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-CODIGO.
           IF  MODACONT-CODI = ZEROS
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "Codigo debe ser diferente de ceros." LINE 25 POSITION 01
            REVERSE BEEP
            PERFORM ACEPTA-TRUCO
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            GO I-CODIGO.
           MOVE MODACONT-CODI TO WI-Z3
           DISPLAY WI-Z3 LINE 25 POSITION 01.
       F-CODIGO. EXIT.
      *
       I-PRC-PAGO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 13 POSITION 52.
           ACCEPT  MODACONT-PRC-PAGO LINE 13 POSITION 52
                                 UPDATE TAB NO BEEP PROMPT ECHO
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PAGO.
           IF  ESC GO F-PRC-PAGO.
           IF CEND
            MOVE 2 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-PRC-PAGO.
           IF MODACONT-PRC-PAGO = ZEROS
            GO TO I-PRC-PAGO.
           MOVE MODACONT-PRC-PAGO TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 13 POSITION 52.
       F-PRC-PAGO. EXIT.
      *
       I-PRC-ADMI.
           MOVE ZEROS TO WK-EXCEPTION
           IF W-OPCION = "A"
            MOVE 100.000 TO MODACONT-PRC-ADMI.
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 14 POSITION 52.
           ACCEPT  MODACONT-PRC-ADMI LINE 14 POSITION 52
                                 UPDATE TAB NO BEEP PROMPT ECHO
                              ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-ADMI.
           IF  ESC GO F-PRC-ADMI.
           IF CEND
            MOVE 3 TO LAB-AUDICAMP-ORDCAM
            MOVE ZEROS TO SW-1
            PERFORM I-MUESTRA-AUDICAMP THRU F-MUESTRA-AUDICAMP
             UNTIL ESC OR SW-1 = 1
            PERFORM I-PANTA THRU F-PANTA
            PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
            PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
            GO TO I-PRC-ADMI.
           IF MODACONT-PRC-ADMI = ZEROS
            GO TO I-PRC-ADMI.
           MOVE MODACONT-PRC-ADMI TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 14 POSITION 52.
       F-PRC-ADMI. EXIT.
      *
       I-DISPLAY-OPCION.
           IF  W-OPCION = "A"
            DISPLAY "ADICIONA" LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "M"
            DISPLAY "MODIFICA" LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "C"
            DISPLAY "CONSULTA" LINE  9 POSITION 37  BLINK.
           IF  W-OPCION = "R"
            DISPLAY "ELIMINAR" LINE  9 POSITION 37  BLINK.
       F-DISPLAY-OPCION. EXIT.
      *
       I-LEE-C-MODACONT.
           MOVE ZEROS TO FIN-ARCH-MODACONT.
           READ MODACONT NEXT RECORD WITH NO LOCK AT END
            MOVE 1 TO FIN-ARCH-MODACONT.
       F-LEE-C-MODACONT. EXIT.
      *
       I-WINDOW-MODACONT.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษออออออหอออออออออหออออออออออป"
           LINE 12 POSITION 20 REVERSE
           "บ Cod. บ % pago  บ % admon  บ"
           LINE 13 POSITION 20 REVERSE
           "ฬออออออฮอออออออออฮออออออออออน"
           LINE 14 POSITION 20 REVERSE
           "บ      บ         บ          บ"
           LINE 15 POSITION 20 REVERSE
           "บ      บ         บ          บ"
           LINE 16 POSITION 20 REVERSE
           "บ      บ         บ          บ"
           LINE 17 POSITION 20 REVERSE
           "บ      บ         บ          บ"
           LINE 18 POSITION 20 REVERSE
           "บ      บ         บ         บ"
           LINE 19 POSITION 20 REVERSE
           "ศออออออสอออออออออสออออออออออผ"
           LINE 20 POSITION 20 REVERSE.
       F-WINDOW-MODACONT.
      *
       I-WINDOW-DATOS.
           DISPLAY
           "ษออออออออออออออออออออออออออออออออออออออออออป"
           LINE 06 POSITION 20
           "บ        MODALIDAD CONTRATO VENTAS         บ"
           LINE 07 POSITION 20 LOW
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 08 POSITION 20
           "บ                                          บ"
           LINE 09 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 10 POSITION 20
           "บ   COD  : ___                             บ"
           LINE 11 POSITION 20
           "ฬออออออออออออออออออออออออออออออออออออออออออน"
           LINE 12 POSITION 20
           "บ % a pagar de base admon.    :            บ"
           LINE 13 POSITION 20
           "บ % base administraciขn       :            บ"
           LINE 14 POSITION 20
           "ศออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 15 POSITION 20.
       F-WINDOW-DATOS.
      *
       I-LEE-MODACONT.
           MOVE ZEROS TO FIN-ARCH-MODACONT.
           READ MODACONT WITH NO LOCK INVALID KEY
            MOVE 1 TO FIN-ARCH-MODACONT.
       F-LEE-MODACONT. EXIT.
      *
       I-FIN-MODE.
           CLOSE MODACONT.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\AUMODCON.PRO".
           COPY "..\PRO\AYAUDIMC.PRO".
      *
           COPY "..\STA\MODACONT.STA".
           COPY "..\STA\AUDICAMP.STA".
      ******************************************************************
