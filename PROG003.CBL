      *****************************************************************
      * This application was written by Juan Jose Torres
      * and is protected under copyrigth and trademarks.
      * Licensed under MIT to be public at github for demostration 
      * purposes.
      * Winner Financial (r) - 1997-2003                  *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG003.
      *----------------------------------------------------------------*
      * DERECHOS DE ACCESO POR PAQUETE WINNER FINANCIAL�               *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\TCNITEMP.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\TCNITEMP.FD".
           COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77  W-PROGRAMA          PIC X(07)   VALUE "PROG003".
       77  ID-PRG              PIC X(07)   VALUE "PROG003".
       77  W-VARIABLES-NOMINA  PIC X(7)  VALUE SPACES.
       77  W-TCNITEMP-NIT      PIC 9(12) VALUE ZEROS.
       77  W-TCNITEMP-DV       PIC 9(01) VALUE ZEROS.
      *
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\TCNITEMP.LBL".
           COPY "..\LBL\INFORMES.LBL".
      *
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\LISTADOS.WRK".
           COPY "..\WRK\INFOR012.WRK".
           COPY "..\WRK\091_2X11.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TCNITEMP.DCL".
           COPY "..\DCL\INFORMES.DCL".
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
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE " WINNER FINANCIAL�"   TO  TIT-APLICACION
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "�"                    TO  WK-RAYAS-U
           MOVE "�"                    TO  WK-RAYAS-P
           MOVE "�"                    TO  WK-RAYA-U
           MOVE "�"                    TO  WK-RAYA-P.
           MOVE 17                     TO  POS-DOWN.

           MOVE 1 TO IND-INFORMES.
           PERFORM I-LABEL-INFORMES THRU F-LABEL-INFORMES.
           MOVE ZEROS TO  WK-LINEAS WK-PAGINA.
           PERFORM  I-VARINFOR-INCA0911 THRU F-VARINFOR-INCA0911.
           MOVE "NIT MANEJADOS POR APLICACION WINNER" TO E9-03-DESINF.
           MOVE I-TITULO-1 TO E9-07-TITU.
           MOVE I-TITULO-2 TO E9-08-TITU.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-STATUS-TCNITEMP THRU F-STATUS-TCNITEMP.
           OPEN I-O TCNITEMP.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-MENU-OPCIONES.
           MOVE  W-LINEA  TO  I-LINEA
           MOVE  999      TO  WK-EXCEPTION
           PERFORM  I-CREAR-WINDOW  THRU  F-CREAR-WINDOW
           MOVE  ZEROS  TO  FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA UNTIL SALIDA OR ESC.
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
               PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
               PERFORM  I-CREAR  THRU  F-CREAR UNTIL ESC OR CUP
               MOVE ZEROS TO WK-EXCEPTION
               DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             ELSE
               IF W-OPCION = "M"
                 MOVE  2  TO SW-MODE
                 PERFORM  I-DISPLAY-OPCION  THRU  F-DISPLAY-OPCION
                 PERFORM  I-MODIFY-MODE  THRU  F-MODIFY-MODE
                  UNTIL ESC OR CUP
                 MOVE ZEROS TO WK-EXCEPTION
                 DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
               ELSE
                 IF W-OPCION = "C"
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                    UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                   DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                 ELSE
                   IF W-OPCION = "R"
                      PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM  I-ELIMINA        THRU F-ELIMINA
                       UNTIL ESC OR CUP
                      MOVE ZEROS TO WK-EXCEPTION
                      DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                   ELSE
                    IF W-OPCION = "L"
                      PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM I-LISTA THRU F-LISTA
                      MOVE ZEROS TO WK-EXCEPTION
                      DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-MENU-OPCIONES. EXIT.
      *
       I-CREAR.
           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
         DATOS-LLAVE-CR.
           PERFORM I-DATOS-LLAVE  THRU F-DATOS-LLAVE.
           IF CUP OR ESC GO F-CREAR.

           PERFORM I-LEE-TCNITEMP-I00 THRU F-LEE-TCNITEMP-I00.
           IF FIN-ARCH-TCNITEMP = ZEROS
             PERFORM I-MUESTRA-DATOS THRU F-MUESTRA-DATOS
             PERFORM I-EXISTE        THRU F-EXISTE
             GO I-CREAR.
           MOVE SPACES TO TCNITEMP-DATOS.
           PERFORM I-DATOS THRU F-DATOS.
           IF ESC GO F-CREAR.
           IF CUP GO DATOS-LLAVE-CR.
           PERFORM I-GRABAR THRU F-GRABAR.
       F-CREAR. EXIT.
      *
       I-MODIFY-MODE.
           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
         DATOS-LLAVE-MO.
           PERFORM I-DATOS-LLAVE  THRU F-DATOS-LLAVE.
           IF CUP OR ESC GO F-MODIFY-MODE.

           PERFORM I-LEE-TCNITEMP-I00 THRU F-LEE-TCNITEMP-I00.
           IF FIN-ARCH-TCNITEMP = 1
             PERFORM I-NEXISTE        THRU F-NEXISTE
             GO I-MODIFY-MODE.
           PERFORM I-MUESTRA-DATOS THRU F-MUESTRA-DATOS.
           PERFORM I-DATOS THRU F-DATOS.
           IF ESC GO F-MODIFY-MODE.
           IF CUP GO DATOS-LLAVE-MO.

           PERFORM I-REGRABAR THRU F-REGRABAR.
       F-MODIFY-MODE. EXIT.
      *
       I-CONSUL-MODE.
           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
         DATOS-LLAVE-CO.
           PERFORM I-DATOS-LLAVE  THRU F-DATOS-LLAVE.
           IF CUP OR ESC GO F-CONSUL-MODE.

           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
           GO DATOS-LLAVE-CO.
       F-CONSUL-MODE. EXIT.
      *
       I-CONSULTA-TCNITEMP.
           PERFORM I-START-TCNITEMP-NOMENOR-KEY00 THRU
                   F-START-TCNITEMP-NOMENOR-KEY00.
           IF FIN-ARCH-TCNITEMP = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             " NO HAY DATOS ... <ESC> " LINE 25 POSITION 01
             MOVE ZEROS TO WK-EXCEPTION
             PERFORM ACEPTA-TRUCO UNTIL ESC
             GO F-CONSULTA-TCNITEMP.

           MOVE 1 TO I.
           MOVE 13 TO I-LINEA.
           IF W-OPCION = "C"
            PERFORM I-DISPLAY-TCNITEMP THRU F-DISPLAY-TCNITEMP
             UNTIL FIN-ARCH-TCNITEMP = 1 OR I > 10
           ELSE
            PERFORM I-DISPLAY-TCNITEMP THRU F-DISPLAY-TCNITEMP
             UNTIL FIN-ARCH-TCNITEMP = 1.
       F-CONSULTA-TCNITEMP. EXIT.
      *
       I-DISPLAY-TCNITEMP.
           PERFORM I-LEE-C-TCNITEMP THRU F-LEE-C-TCNITEMP.
           IF FIN-ARCH-TCNITEMP = 1
             GO F-DISPLAY-TCNITEMP.

           IF  W-OPCION NOT = "C"
             IF I > 10
               PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
               MOVE 1  TO I
               MOVE 13 TO I-LINEA
             END-IF
           END-IF

           IF W-OPCION = "C"
             IF I = 2
               MOVE TCNITEMP-NIT TO W-TCNITEMP-NIT
               MOVE TCNITEMP-DV  TO W-TCNITEMP-DV
             END-IF
           END-IF

           MOVE TCNITEMP-NIT TO WI-Z12C.
           DISPLAY
            WI-Z12C               LINE I-LINEA POSITION 02
            TCNITEMP-DV           LINE I-LINEA POSITION 18
            TCNITEMP-RAZON-SOCIAL LINE I-LINEA POSITION 20
            TCNITEMP-DIREC-TRABAJ LINE I-LINEA POSITION 61.

           ADD 1 TO I.
           ADD 1 TO I-LINEA.
       F-DISPLAY-TCNITEMP. EXIT.
      *
       I-ELIMINA.
           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
         DATOS-LLAVE-EL.
           PERFORM I-DATOS-LLAVE  THRU F-DATOS-LLAVE.
           IF CUP OR ESC GO F-ELIMINA.

           PERFORM I-LEE-TCNITEMP-I00 THRU F-LEE-TCNITEMP-I00.
           IF FIN-ARCH-TCNITEMP = 1
             PERFORM I-NEXISTE        THRU F-NEXISTE
             GO I-ELIMINA.
           PERFORM I-MUESTRA-DATOS THRU F-MUESTRA-DATOS.

           PERFORM I-ELIMINAR THRU F-ELIMINAR.
       F-ELIMINA. EXIT.
      *
       I-LISTA.
           PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS.
           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-CONSULTA-TCNITEMP THRU F-CONSULTA-TCNITEMP.
           DISPLAY SPACES SIZE 80  LINE 24 POSITION 01
           LABEL-INFORMES          LINE 24 POSITION 33 REVERSE
           NOM-PAPEL(IND-INFORMES) LINE 24 POSITION 66 REVERSE
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-CONFIRMA          THRU F-CONFIRMA.
           IF CUP GO F-LISTA.
           PERFORM I-INICIO-INFORME THRU F-INICIO-INFORME.
       F-LISTA. EXIT.
      *
       I-INICIO-INFORME.
           OPEN OUTPUT INFORMES.

           MOVE ZEROS TO TCNITEMP-KEY00.
           PERFORM I-START-TCNITEMP-NOMENOR-KEY00 THRU
                   F-START-TCNITEMP-NOMENOR-KEY00.
           IF FIN-ARCH-TCNITEMP = 1
             CLOSE INFORMES
             GO F-INICIO-INFORME.

           PERFORM I-INT-INFORME THRU F-INT-INFORME
             UNTIL FIN-ARCH-TCNITEMP = 1.

           MOVE SPACES TO REG-INFORMES.
           WRITE REG-INFORMES BEFORE PAGE.
           CLOSE INFORMES.
       F-INICIO-INFORME. EXIT.
      *
       I-INT-INFORME.
           PERFORM I-LEE-C-TCNITEMP THRU F-LEE-C-TCNITEMP.
           IF FIN-ARCH-TCNITEMP = 1
             GO F-INT-INFORME.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
                   TCNITEMP-KEY00 LINE 25 POSITION 01.
  
           PERFORM I-TCNITEMP-INFORMES THRU F-TCNITEMP-INFORMES.

           IF WK-LINEAS > 60 OR WK-LINEAS = 0
            PERFORM I-TITULOS-INFORMES  THRU F-TITULOS-INFORMES.

           WRITE REG-INFORMES FROM I-DETALLE AFTER 1.
       F-INT-INFORME. EXIT.
      *
       I-TITULOS-INFORMES.
           ADD  1  TO  WK-PAGINA.
           MOVE  WK-PAGINA  TO  ENC9-PAGINA.
           PERFORM  I-INFORMES-INCA0911  THRU  F-INFORMES-INCA0911.
           MOVE  9  TO  WK-LINEAS.
       F-TITULOS-INFORMES. EXIT.
      *
       I-TCNITEMP-INFORMES.
           MOVE TCNITEMP-NIT          TO I-TCNITEMP-NIT.
           MOVE TCNITEMP-DV           TO I-TCNITEMP-DV.
           MOVE TCNITEMP-RAZON-SOCIAL TO I-TCNITEMP-RAZON-SOCIAL.
           MOVE TCNITEMP-DIREC-TRABAJ TO I-TCNITEMP-DIREC-TRABAJ.
       F-TCNITEMP-INFORMES. EXIT.
      *
       I-DATOS-LLAVE.
         TCNITEMP-NIT.
           PERFORM I-TCNITEMP-NIT THRU F-TCNITEMP-NIT.
           IF CUP OR ESC GO F-DATOS-LLAVE.
         TCNITEMP-DV.
           PERFORM I-TCNITEMP-DV  THRU F-TCNITEMP-DV.
           IF ESC GO F-DATOS-LLAVE.
           IF CUP GO TCNITEMP-NIT.
       F-DATOS-LLAVE. EXIT.
      *
       I-TCNITEMP-NIT.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 15 LINE 11 POSITION 02
           ACCEPT  W-TCNITEMP-NIT  LINE 11 POSITION 02
                                   UPDATE TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE  W-TCNITEMP-NIT  TO  TCNITEMP-NIT WI-Z12C.
           DISPLAY  WI-Z12C  LINE 11 POSITION 02.

           IF  F2   PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
           IF  CUP OR ESC  GO F-TCNITEMP-NIT.
           IF  CDN AND W-OPCION = "C"  GO F-TCNITEMP-NIT.

           IF W-TCNITEMP-NIT = ZEROS  GO I-TCNITEMP-NIT.
       F-TCNITEMP-NIT. EXIT.
      *
       I-TCNITEMP-DV.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT  W-TCNITEMP-DV   LINE 11 POSITION 18
                                   UPDATE TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           MOVE  W-TCNITEMP-DV TO TCNITEMP-DV.
           DISPLAY  TCNITEMP-DV  LINE 11 POSITION 18.

           IF  F2   PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
           IF  CUP OR ESC  GO F-TCNITEMP-DV.
           IF  CDN AND W-OPCION = "C"  GO F-TCNITEMP-DV.
       F-TCNITEMP-DV. EXIT.
      *
       I-MUESTRA-DATOS.
           DISPLAY
            TCNITEMP-RAZON-SOCIAL LINE 11 POSITION 20
            TCNITEMP-DIREC-TRABAJ LINE 11 POSITION 61.
       F-MUESTRA-DATOS. EXIT.
      *
       I-DATOS.
         TCNITEMP-RAZON-SOCIAL.
           PERFORM I-TCNITEMP-RAZON-SOCIAL THRU F-TCNITEMP-RAZON-SOCIAL.
           IF CUP OR ESC GO F-DATOS.
         TCNITEMP-DIREC-TRABAJ.
           PERFORM I-TCNITEMP-DIREC-TRABAJ THRU F-TCNITEMP-DIREC-TRABAJ.
           IF ESC GO F-DATOS.
           IF CUP GO TCNITEMP-RAZON-SOCIAL.
       F-DATOS. EXIT.
      *
       I-TCNITEMP-RAZON-SOCIAL.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT  TCNITEMP-RAZON-SOCIAL  LINE 11 POSITION 20
                                          UPDATE TAB NO BEEP
                                       ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
           DISPLAY  TCNITEMP-RAZON-SOCIAL  LINE 11 POSITION 20.

           IF  F2   PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
           IF  CUP OR ESC  GO F-TCNITEMP-RAZON-SOCIAL.

           IF TCNITEMP-RAZON-SOCIAL = SPACES GO I-TCNITEMP-RAZON-SOCIAL.
       F-TCNITEMP-RAZON-SOCIAL. EXIT.
      *
       I-TCNITEMP-DIREC-TRABAJ.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT  TCNITEMP-DIREC-TRABAJ  LINE 11 POSITION 61
                                          UPDATE TAB NO BEEP
                                       ON EXCEPTION WK-EX
                                          PERFORM 999-EXCEPTION.
           DISPLAY  TCNITEMP-DIREC-TRABAJ  LINE 11 POSITION 61.

           IF  F2   PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
           IF  CUP OR ESC  GO F-TCNITEMP-DIREC-TRABAJ.

           IF TCNITEMP-DIREC-TRABAJ = SPACES GO I-TCNITEMP-DIREC-TRABAJ.
       F-TCNITEMP-DIREC-TRABAJ. EXIT.
      *
       I-GRABAR.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACES TO W-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "�       [ G ]  GRABAR REGISTRO     [F1]  CANCELAR         �"
           LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "G" OR F1.
           IF W-TRUCO = "G"
            PERFORM I-GRABAR-TCNITEMP THRU F-GRABAR-TCNITEMP.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-GRABAR. EXIT.
      *
       I-GRABAR-TCNITEMP.
           PERFORM I-TCNITEMP-WRITE THRU F-TCNITEMP-WRITE.
           IF FIN-ARCH-TCNITEMP = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             " NO PUEDE GRABAR ... OJO <ENTER> " LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO.
       F-GRABAR-TCNITEMP. EXIT.
      *
       I-REGRABAR.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACES TO W-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "�       [ G ]  GRABAR REGISTRO     [F1]  CANCELAR         �"
           LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "G" OR F1.
           IF W-TRUCO = "G"
            PERFORM I-REGRABAR-TCNITEMP THRU F-REGRABAR-TCNITEMP.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-REGRABAR. EXIT.
      *
       I-REGRABAR-TCNITEMP.
           PERFORM I-TCNITEMP-REWRITE THRU F-TCNITEMP-REWRITE.
           IF FIN-ARCH-TCNITEMP = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             " NO PUEDE GRABAR ... OJO <ENTER> " LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO.
       F-REGRABAR-TCNITEMP. EXIT.
      *
       I-ELIMINAR.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACES TO W-TRUCO.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           "�       [ E ]  BORRA REGISTRO     [F1]  CANCELAR        �"
           LINE 25 POSITION 10 REVERSE.
           PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "E" OR F1.
           IF W-TRUCO = "E"
             PERFORM I-BORRA-TCNITEMP THRU F-BORRA-TCNITEMP.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-ELIMINAR. EXIT.
      *
       I-BORRA-TCNITEMP.
           PERFORM I-TCNITEMP-DELETE THRU F-TCNITEMP-DELETE.
           IF FIN-ARCH-TCNITEMP = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
             " NO PUEDE BORRAR ... OJO <ENTER> " LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO.
       F-BORRA-TCNITEMP. EXIT.
      *
       I-EXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY "REGISTRO YA EXISTE" LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-EXISTE. EXIT.
      *
       I-NEXISTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01 BEEP.
           PERFORM ACEPTA-TRUCO
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
       F-NEXISTE. EXIT.
      *
       I-DISPLAY-OPCION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF  W-OPCION = "A"
            DISPLAY " A D I C I O N A " LINE 05 POSITION 32 BLINK.
           IF  W-OPCION = "M"
            DISPLAY " M O D I F I C A " LINE 05 POSITION 32 BLINK.
           IF  W-OPCION = "C"
            DISPLAY " C O N S U L T A " LINE 05 POSITION 32 BLINK.
           IF  W-OPCION = "R"
            DISPLAY " E L I M I N A R " LINE 05 POSITION 32 BLINK.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
       F-DISPLAY-OPCION. EXIT.
      *
       I-FIN-MODE.
           CLOSE TCNITEMP.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-DATOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "������������������������������������������������������������
      -    "�����������ͻ" LINE 06 POSITION 01
           "�                  N.I.T.     ADMINISTRACION ACCESO WINNER
      -    "            �" LINE 07 POSITION 01
           "������������������������������������������������������������
      -    "�����������͹" LINE 08 POSITION 01
           "�     N.I.T.    �D�             RAZON SOCIAL               �
      -    "  LIBRERIA  �" LINE 09 POSITION 01
           "������������������������������������������������������������
      -    "�����������͹" LINE 10 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 11 POSITION 01
           "������������������������������������������������������������
      -    "�����������͹" LINE 12 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 13 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 14 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 15 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 16 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 17 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 18 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 19 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 20 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 21 POSITION 01
           "�               � �                                        �
      -    "            �" LINE 22 POSITION 01
           "������������������������������������������������������������
      -    "�����������ͼ" LINE 23 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
       F-WINDOW-DATOS. EXIT.
      *
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTAINF.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\CONFIRMA.PRO".
           COPY "..\PRO\INCA0911.PRO".
      *
           COPY "..\STA\TCNITEMP.STA".
      *
           COPY "..\LEE\TCNITEMP.I00".
           COPY "..\LEE\TCNITEMP.SEQ".
      *
           COPY "..\OPE\TCNITEMP.WRI".
           COPY "..\OPE\TCNITEMP.REW".
           COPY "..\OPE\TCNITEMP.DEL".
      *
           COPY "..\START\TCNITEMP.I00".
      *
           COPY "..\PLB\INFORMES.PLB".
      ******************************************************************
