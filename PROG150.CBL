       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG150.
      ******************************************************************
      *  LABORES                 ABR-27-2002                           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\NOMLABOR.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\NOMLABOR.FD".
               COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG150".
       77      ID-PRG              PIC X(7)  VALUE "PROG150".
       77      W-COD-LABOR         PIC 9(03) VALUE ZEROS.
       77      WK-LABOR            PIC 999   VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\NOMLABOR.LBL".
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
               COPY "..\DCL\NOMLABOR.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\AUDITE.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-NOMLABOR  THRU F-STATUS-NOMLABOR.
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-WINDOW   THRU F-CREAR-WINDOW
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE " LABORES "     TO  TIT-APLICACION
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P.
           MOVE 15                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN    I-O      NOMLABOR.
               OPEN    INPUT    EMPRESAS.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
           MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY "[ ESC ] TERMINA" LINE 24 POSITION 01 REVERSE
                                     CONTROL SG-CONTROL-STRING.
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
           MOVE 1 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO        THRU F-CODIGO.
           IF CUP OR ESC           GO   F-CREAR.
           PERFORM I-LEE-NOMLABOR      THRU F-LEE-NOMLABOR.
           IF  SW-1  =  ZEROS
               PERFORM I-EXISTE          THRU  F-EXISTE
               PERFORM I-MUESTRA-CAMPOS  THRU  F-MUESTRA-CAMPOS
               GO I-CREAR
           ELSE
               MOVE COD-LABOR           TO   W-COD-LABOR
               MOVE SPACE               TO   REG-NOMLABOR
               MOVE W-COD-LABOR         TO   COD-LABOR
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-CREAR.
           MOVE SPACE TO REG-NOMLABOR.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CREAR.
      *
       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-ELIMINA.
           PERFORM I-LEE-NOMLABOR   THRU F-LEE-NOMLABOR.
           IF  SW-1  =  1
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CUP.
           MOVE SPACE TO REG-NOMLABOR.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ELIMINA.
      *
       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-MODIFY-MODE.
           PERFORM I-LEE-NOMLABOR   THRU F-LEE-NOMLABOR.
           IF  SW-1  =  1
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS          THRU F-DATOS
               IF CUP                   GO   I-MODIFY-MODE.
           MOVE SPACE TO REG-NOMLABOR.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-MODIFY-MODE.
      *
       I-CONSUL-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY " [ F1 ]  Help  -  [ F5 ]  Siguiente Registro "
                                               LINE 24 POSITION 35.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF CUP OR ESC        GO   F-CONSUL-MODE.
           IF F1 PERFORM I-HELP-LABOR THRU F-HELP-LABOR UNTIL ESC
                 MOVE    ZEROS             TO  WK-EXCEPTION
                 PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS
                 PERFORM I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                 PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           IF F5
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE
              IF SW-1 = 1
                 MOVE SPACES TO KEY-NOMLABOR
                 PERFORM I-START-LABOR   THRU F-START-LABOR
                 PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE
              ELSE
                 NEXT SENTENCE
           ELSE
              PERFORM I-LEE-NOMLABOR   THRU F-LEE-NOMLABOR.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-CONSUL-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
       F-CONSUL-MODE.
      *
       I-START-LABOR.
           START NOMLABOR KEY NOT LESS KEY-NOMLABOR
                 INVALID KEY MOVE 1 TO FIN-OK.
       F-START-LABOR.
      *
       I-GRABAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  REGRESA         บ"
                  LINE 24 POSITION 10 REVERSE CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REG-NOMLABOR INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  REGRESA         บ"
                  LINE 24 POSITION 10 REVERSE CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REG-NOMLABOR INVALID KEY STOP " ".
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  REGRESA         บ"
                  LINE 24 POSITION 10 REVERSE CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE NOMLABOR INVALID KEY STOP " ".
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
           PERFORM I-NOMBRE THRU F-NOMBRE.
           IF  CUP  OR  ESC  GO  F-DATOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACES TO W-TRUCO.
           IF SW-MODE = 1
              PERFORM I-GRABAR   THRU F-GRABAR UNTIL
              W-TRUCO = "G" OR CUP
           ELSE
              PERFORM I-REGRABAR THRU F-REGRABAR UNTIL
              W-TRUCO = "G" OR CUP.
           IF CUP GO I-DATOS.
       F-DATOS.
      *
       I-MUESTRA-CAMPOS.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY COD-LABOR         LINE 12 POSITION 34
                                     CONTROL SG-CONTROL-STRING.
           DISPLAY NOM-LABOR         LINE 13 POSITION 34
                                     CONTROL SG-CONTROL-STRING.
       F-MUESTRA-CAMPOS.
      *
       I-CODIGO.
           MOVE ZEROS  TO  WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY "DIGITE NUMERO DE CODIGO"
           LINE 24 POSITION 01 REVERSE CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY SPACES LINE 12 POSITION 34 SIZE 02
                   CONTROL SG-CONTROL-STRING.
           ACCEPT  COD-LABOR LINE 12 POSITION 34
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
           DISPLAY COD-LABOR LINE 12 POSITION 34.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC OR F1 GO  F-CODIGO.
           IF  COD-LABOR = ZEROS GO I-CODIGO.
       F-CODIGO.
      *
       I-NOMBRE.
           MOVE ZEROS  TO  WK-EXCEPTION.
           MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY " NUMERO RESOLUCION APROBADA POR MINTRABAJO "
                LINE 24 POSITION 01 REVERSE CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY SPACES LINE 13 POSITION 34 SIZE 30
                      CONTROL SG-CONTROL-STRING.
           ACCEPT  NOM-LABOR LINE 13 POSITION 34
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
           DISPLAY NOM-LABOR LINE 13 POSITION 34
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-NOMBRE.
           IF  NOM-LABOR = SPACES GO I-NOMBRE.
       F-NOMBRE.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "ADICIONA"
                                    LINE  7 POSITION 38      .
           IF  W-OPCION = "M"
               DISPLAY "MODIFICA"
                                    LINE  7 POSITION 38      .
           IF  W-OPCION = "C"
               DISPLAY "CONSULTA"
                                    LINE  7 POSITION 38      .
           IF  W-OPCION = "R"
               DISPLAY "ELIMINAR"
                                    LINE  7 POSITION 38      .
       F-DISPLAY-OPCION.
      *
       I-MASCARA.
           MOVE ZEROS       TO SW-ERROR.
           MOVE WK-DIA      TO WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
           OR  WK-DIA                       <  01
           OR  WK-DIA                       >  31
           OR  WK-ANO                       <  90
               MOVE "ERR"                  TO  WK-MASCARA-DIA
               MOVE "ERR"                  TO  WK-MASCARA-MES
               MOVE "ERR"                  TO  WK-MASCARA-ANO
               MOVE 1 TO SW-ERROR
               GO F-MASCARA
           ELSE
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
           MOVE WK-ANO                 TO  WK-MASCARA-ANO
           MOVE "-"                    TO  WK-RAYA-1
           MOVE "-"                    TO  WK-RAYA-2
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-HASTA
                                        /  4
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-BISIESTO
                                        *  4
           IF  WK-ANO-BISIESTO          =  WK-ANO-HASTA
               MOVE 29                 TO  WK-DIAS-MES (2)
           ELSE
               MOVE 28                 TO  WK-DIAS-MES  (2).
       F-MASCARA.
      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************

       I-LEE-NOMLABOR.
               MOVE 0  TO   SW-1.
               READ NOMLABOR WITH NO LOCK INVALID  KEY
                    MOVE 1   TO  SW-1.
       F-LEE-NOMLABOR.

       I-ACTUALIZAR.
           MOVE SG-COLOR-TABLE(6) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF W-TRUCO =  "M"
              REWRITE REG-NOMLABOR.
           IF CDN
              REWRITE REG-NOMLABOR.
           PERFORM I-LEE-SIGUIENTE  THRU F-LEE-SIGUIENTE.
           PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.
      *
       I-LEE-SIGUIENTE.
           MOVE 0 TO SW-1.
           READ NOMLABOR NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1 GO F-LEE-SIGUIENTE.
       F-LEE-SIGUIENTE.
      *
       120-FT.
      *
       I-WINDOW-DATOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออป"
           LINE 09 POSITION 14 CONTROL SG-CONTROL-STRING
           "บ                   LABORES                         บ"
           LINE 10 POSITION 14 CONTROL SG-CONTROL-STRING
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 11 POSITION 14 CONTROL SG-CONTROL-STRING.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY
           "บ         CODIGO :                                  บ"
           LINE 12 POSITION 14 CONTROL SG-CONTROL-STRING
           "บ    DESCRIPCION :                                  บ"
           LINE 13 POSITION 14 CONTROL SG-CONTROL-STRING
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 14 POSITION 14 CONTROL SG-CONTROL-STRING.
       F-WINDOW-DATOS.
      *
      ********************************************************
      *      HELP  ANTICIPOS CESANTIAS                       *
      ********************************************************
       I-HELP-LABOR.
           MOVE 01 TO I.
           MOVE 15 TO I-LINEA.
           MOVE SPACES TO KEY-NOMLABOR.
           MOVE ZEROS  TO FIN-OK WK-LABOR.
           PERFORM I-START-LABOR     THRU F-START-LABOR.
           PERFORM I-WINDOW-LABOR    THRU F-WINDOW-LABOR.
           PERFORM I-DISPLAY-LABOR   THRU F-DISPLAY-LABOR
                   UNTIL I > 5.
         LEE-ISS.
           MOVE    ZEROS  TO   WK-EXCEPTION.
           ACCEPT SW-HELP LINE 24 POSITION 80 OFF
                               UPDATE PROMPT
                               CONVERT
                               TAB
                               NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CDN
                MOVE 01 TO I
                MOVE 15 TO I-LINEA
                MOVE ZEROS TO FIN-OK
                MOVE WK-LABOR TO COD-LABOR
                PERFORM I-START-LABOR     THRU F-START-LABOR
                PERFORM I-WINDOW-LABOR    THRU F-WINDOW-LABOR
                PERFORM I-DISPLAY-LABOR   THRU F-DISPLAY-LABOR
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 78 REVERSE
                   GO LEE-ISS
                ELSE
                   GO LEE-ISS.
       F-HELP-LABOR.
      *
       I-DISPLAY-LABOR.
           PERFORM   I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF   SW-1 = 1
                DISPLAY "**" LINE     I-LINEA POSITION 37 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 25 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-LABOR.
           DISPLAY   COD-LABOR       LINE I-LINEA POSITION 22.
           DISPLAY   NOM-LABOR       LINE I-LINEA POSITION 29.
           IF I = 2  MOVE COD-LABOR TO WK-LABOR.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-LABOR.
      *
       I-WINDOW-LABOR.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           MOVE "HIGH" TO SG-INTENSITY.
           DISPLAY  "ษอออออออหออออออออออออออออออออออออออออออออป"
           LINE 12 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ  COD  บ      D E S C R I P C I O N     บ"
           LINE 13 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "ฬอออออออฮออออออออออออออออออออออออออออออออน"
           LINE 14 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ       บ                                บ"
           LINE 15 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ       บ                                บ"
           LINE 16 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ       บ                                บ"
           LINE 17 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ       บ                                บ"
           LINE 18 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "บ       บ                                บ"
           LINE 19 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING
           DISPLAY  "ศอออออออสออออออออออออออออออออออออออออออออผ"
           LINE 20 POSITION 19 REVERSE
                                   CONTROL SG-CONTROL-STRING.
       F-WINDOW-LABOR.
      *
      ********************************************************
      *
       I-STATUS-NOMLABOR.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT NOMLABOR
               MOVE 1                      TO  SW-ABRE
               IF  NOMLABOR-STATUS              =  "35"
                   OPEN OUTPUT NOMLABOR
               ELSE
                   IF  NOMLABOR-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-NOMLABOR
                       PERFORM I-FIN-MODE       THRU F-FIN-MODE.
               CLOSE NOMLABOR.
       F-STATUS-NOMLABOR.



      *
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\AUDITE.PRO".
      *
       I-FIN-MODE.
           CLOSE
                 EMPRESAS
                 NOMLABOR.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

