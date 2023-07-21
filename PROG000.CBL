      ******************************************************************
      * This application was written by Juan Jose Torres
      * and is protected under copyrigth and trademarks.
      * Licensed under MIT to be public at github for demostration 
      * purposes.
      * Winner Financial (r) - 1997-2003                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG000.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\LICENCIA.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\NOMIMENU.DYC".
               COPY "..\DYC\CONERROR.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\DINOMBRE.DYC".
               COPY "..\DYC\CONTRATO.DYC".
       DATA DIVISION.
       FILE SECTION.               
               COPY "..\FD\LICENCIA.FD".
               COPY "..\FD\DINOMBRE.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CONERROR.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\NOMIMENU.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG000".
       77      ID-PRG              PIC X(7)  VALUE "PROG000".
       77      WK-EMPRESAS               PIC X(40)      VALUE ZEROS.
       77      SW2                       PIC 9          VALUE ZEROS.
       77      W-ID                      PIC X(8)       VALUE SPACES.
       77      W-IDENT                   PIC X(8)       VALUE SPACES.
       77      W-IDENT-RE                PIC X(8)       VALUE SPACES.
       77      W-TIPO                    PIC X          VALUE SPACES.
       77      W-MENU                    PIC X(8)       VALUE SPACES.
       77      W-EMPRESA1                PIC 9(07)      VALUE ZEROS.
       77      W-ANO                     PIC 9(4)       VALUE ZEROS.
       77      W-MES                     PIC 99         VALUE ZEROS.
       77      MENU-STATUS               PIC XX         VALUE SPACES.
       77      WK-DIF                    PIC 99         VALUE ZEROS.
               COPY "..\LBL\LICENCIA.LBL".
               COPY "..\LBL\DINOMBRE.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\CONTRATO.LBL".
               COPY "..\LBL\CONERROR.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\NOMIMENU.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
       01      WK-RAYA.
        03     WK-RAYA-P                 PIC X.
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     WK-RAYA-U                 PIC X.
       01      WK-RAYA-ARRIBA.
        03     FILLER                    PIC X          VALUE "�".
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     FILLER                    PIC X          VALUE "�".
       01      WK-RAYA-ABAJO.
        03     FILLER                    PIC X          VALUE "�".
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     FILLER                    PIC X          VALUE "�".
       01      WK-RAYAS.
        03     WK-RAYAS-P                PIC X.
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     WK-RAYAS-U                PIC X.
       01      WK-RAYAS-ARRIBA.
        03     FILLER                    PIC X          VALUE "�".
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     FILLER                    PIC X          VALUE "�".
       01      WK-RAYAS-ABAJO.
        03     FILLER                    PIC X          VALUE "�".
        03     FILLER                    PIC X(40)      VALUE ALL "�".
        03     FILLER                    PIC X          VALUE "�".
       01      WK-FECHA-ALFA.
         05    WK-FECHA-MES              PIC X(10)      VALUE SPACES.
      *  05    FILLER                    PIC X(3)       VALUE " 19".
         05    WK-FECHA-ANO              PIC 9(4)       VALUE ZEROS.
      ******************************************************************
       01  AREA-LINK-NOMINA.
           03 CON-COD-EMPRESA           PIC 9(7)       VALUE ZEROS.
           03 CON-ANO                   PIC 9(4)       VALUE ZEROS.
           03 CON-PERIODO               PIC 9(3)       VALUE ZEROS.
           03 CON-USERNAME              PIC X(3)       VALUE SPACES.
           03 CON-PASSW                 PIC X(8)       VALUE SPACES.
           03 CON-PROGRAMA              PIC 9(3)       VALUE ZEROS.
      ******************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.
               COPY "..\DCL\LICENCIA.DCL".
               COPY "..\DCL\DINOMBRE.DCL".
               COPY "..\DCL\CONTRATO.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CONERROR.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\NOMIMENU.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-MENU.
           MOVE  "*P R O G R A M A*"  TO TIT-APLICACION.
           ACCEPT WK-FECHA-HOY FROM DATE
           IF WK-ANO           >= 0 OR
              WK-ANO           < 2000
              ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.
           PERFORM I-STATUS-EMPRESAS THRU F-STATUS-EMPRESAS
           PERFORM I-STATUS-HOJAVIDA THRU F-STATUS-HOJAVIDA.
           PERFORM I-LABEL-CONTRATO  THRU F-LABEL-CONTRATO
           PERFORM I-STATUS-CONTRATO THRU F-STATUS-CONTRATO
           PERFORM I-STATUS-DINOMBRE THRU F-STATUS-DINOMBRE
           PERFORM I-ANALIZE-DATOS   THRU F-ANALIZE-DATOS
           PERFORM I-PANTALLA-2      THRU F-PANTALLA-2.
           PERFORM I-ACEPTA-DATOS    THRU F-ACEPTA-DATOS.
           PERFORM I-MENU            THRU F-MENU
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-MENU.

      *

       I-ANALIZE-DATOS.
           IF  CON-COD-EMPRESA       =      ZEROS
                   MOVE 01           TO     CON-COD-EMPRESA.
           MOVE    CON-COD-EMPRESA   TO     WIN-CODIGO-EMPRESA
           OPEN    INPUT USUARIOS.
           IF      USUARIOS-STATUS = "35"   OR
                   USUARIOS-STATUS = "94"
                   PERFORM I-GENERA-USUARIO THRU F-GENERA-USUARIO
                   GO      I-MAIN-MENU.
           OPEN    INPUT EMPRESAS
           PERFORM I-LEE-EMPRESAS-I THRU F-LEE-EMPRESAS-I
           CLOSE   EMPRESAS
           COMPUTE CON-PERIODO       =      WIN-NOMINA +  1
           MOVE    WIN-FECHA-ANO     TO     CON-ANO
           MOVE    CON-COD-EMPRESA   TO     W-EMPRESA1
           MOVE    CON-ANO           TO     W-ANO WK-ANO.
           IF      CON-PERIODO       <      01
                   MOVE 01           TO     CON-PERIODO.
           PERFORM I-PANTALLA        THRU   F-PANTALLA.
           MOVE    "NO" TO SG-BLINK
           MOVE    "NO" TO SG-BEEP.
           MOVE    SG-COLOR-TABLE(8) TO     SG-FCOLOR.
           MOVE    SG-COLOR-TABLE(4) TO     SG-BCOLOR.
           MOVE    CON-PERIODO       TO     W-PERIODO.
       F-ANALIZE-DATOS.

       I-PANTALLA-2.
               DISPLAY PSW-2                 LINE  2 POSITION 17
                                             CONTROL SG-CONTROL-STRING.
               DISPLAY PSW-3                 LINE  3 POSITION 17
               DISPLAY PSW-4                 LINE  4 POSITION 17
               DISPLAY PSW-5                 LINE  5 POSITION 17
               DISPLAY PSW-6                 LINE  6 POSITION 17
               DISPLAY PSW-7                 LINE  7 POSITION 17
               DISPLAY PSW-8                 LINE  8 POSITION 17
               DISPLAY PSW-9                 LINE  9 POSITION 17
               DISPLAY PSW-11                LINE 11 POSITION 12
               DISPLAY PSW-12                LINE 12 POSITION 12
               DISPLAY PSW-13                LINE 13 POSITION 12
               DISPLAY PSW-14                LINE 14 POSITION 12
               DISPLAY PSW-15                LINE 15 POSITION 12
               DISPLAY PSW-16                LINE 16 POSITION 12
               DISPLAY PSW-17                LINE 17 POSITION 12
               DISPLAY PSW-18                LINE 18 POSITION 12
               MOVE "HIGH" TO SG-INTENSITY
               MOVE "NO" TO SG-BEEP.
               MOVE SG-COLOR-TABLE(8)        TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(4)        TO SG-BCOLOR.
               DISPLAY W-EMPRESA1            LINE 06 POSITION 50
                                             CONTROL SG-CONTROL-STRING.
               DISPLAY W-ANO                 LINE 07 POSITION 50
               DISPLAY W-PERIODO             LINE 08 POSITION 50.
       F-PANTALLA-2.

       I-ACEPTA-DATOS.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING.
           IF      CUP              GO   CAB-5.
         CAB-0.
           PERFORM I-KEY-LICENCIA   THRU F-KEY-LICENCIA.
           IF    ESC OR CUP         GO   F-ACEPTA-DATOS.
         CAB-1.
           PERFORM I-ACEPTA-MENU    THRU F-ACEPTA-MENU.
           IF    CUP                GO   CAB-0.
           IF    ESC                GO   F-ACEPTA-DATOS.
         CAB-1A.
           PERFORM I-ACEPTA-USER    THRU F-ACEPTA-USER.
           IF    CUP                GO   CAB-1.
           IF    ESC                GO   F-ACEPTA-DATOS.
         CAB-2.
           PERFORM I-PASSWORD       THRU F-PASSWORD.
           IF    CUP                GO   CAB-1A.
           IF    ESC                GO   F-ACEPTA-DATOS.
         CAB-3.
           PERFORM I-ACEPTA-EMPRESA THRU F-ACEPTA-EMPRESA.
           IF    CUP                GO   CAB-2.
           IF    ESC                GO   F-ACEPTA-DATOS.
         CAB-4.
           PERFORM I-ACEPTA-ANO     THRU F-ACEPTA-ANO.
           IF    CUP                GO   CAB-3.
           IF    ESC                GO F-ACEPTA-DATOS.
         CAB-5.
           PERFORM I-ACEPTA-PERIODO THRU F-ACEPTA-PERIODO.
           IF    CUP                 GO CAB-4.
       F-ACEPTA-DATOS. EXIT.

       I-KEY-LICENCIA.
           DISPLAY "************" LINE 03 POSITION 50.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT  KEY-LICENCIA LINE 03 POSITION 50 TAB NO BEEP OFF
                   ON EXCEPTION   WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY "************" LINE 03 POSITION 50.
           IF CUP  OR ESC GO F-KEY-LICENCIA.
           IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  KEY-LICENCIA   =  ZEROS
               DISPLAY "LICENCIA NO PUEDE SER IGUAL A CEROS"
               LINE 24 POSITION 1
               GO I-KEY-LICENCIA.
           OPEN INPUT LICENCIA
           READ LICENCIA WITH NO LOCK
                INVALID  KEY  DISPLAY " LICENCIA NO EXISTE          "
                  LINE 24 POSITION 01
                CLOSE LICENCIA GO TO I-KEY-LICENCIA.
           CLOSE LICENCIA.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
       F-KEY-LICENCIA.

       I-ACEPTA-MENU.
           MOVE    ZEROS   TO   WK-EXCEPTION.
           DISPLAY SPACE   LINE 04 POSITION 50 SIZE 8.
           MOVE "CONTABLE" TO W-MENU
           ACCEPT  W-MENU  LINE 04 POSITION 50 UPDATE TAB NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-MENU =  SPACES
               MOVE "CONTABLE" TO W-MENU
               DISPLAY "LA APLICACION DEFAULT ES CONTABLE"
                       LINE 24 POSITION 1
               HIGH REVERSE BEEP
               GO TO I-ACEPTA-MENU.
           PERFORM I-STATUS-CONTRATO THRU F-STATUS-CONTRATO
           PERFORM I-STATUS-HOJAVIDA THRU F-STATUS-HOJAVIDA.
       F-ACEPTA-MENU.

       I-ACEPTA-USER.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACE LINE 05 POSITION 50 SIZE 8.
           MOVE "WIN" TO CON-USERNAME
           ACCEPT CON-USERNAME LINE 05 POSITION 50 TAB NO BEEP UPDATE
                  ON EXCEPTION WK-EX
                     PERFORM 999-EXCEPTION.
           IF CUP OR ESC GO F-ACEPTA-USER.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           MOVE CON-USERNAME TO COD-USR.
           MOVE SPACES       TO COD-PRG.
           READ USUARIOS WITH NO LOCK INVALID KEY
               DISPLAY "USUARIO ERRADO" LINE 24 POSITION 1
                                        HIGH REVERSE BEEP
               GO TO I-ACEPTA-USER.
           DISPLAY NOM-USUARIO LINE 24 POSITION 30.
       F-ACEPTA-USER.

       I-PASSWORD.
           MOVE    ZEROS     TO   WK-EXCEPTION.
           DISPLAY SPACE     LINE 05 POSITION 54 SIZE 8.
           ACCEPT  CON-PASSW LINE 05 POSITION 54 TAB NO BEEP OFF
                   ON EXCEPTION   WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY "********" LINE 05 POSITION 54.
           IF CUP  OR ESC GO F-PASSWORD.
           IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CON-PASSW = CLAVE-USR
              NEXT SENTENCE
           ELSE
              DISPLAY "PASWORD ERRADO" LINE 24 POSITION 1
                                        HIGH REVERSE BEEP
              GO TO I-PASSWORD.
       F-PASSWORD.

       I-ACEPTA-EMPRESA.
           DISPLAY " F1 help Empresas " LINE 24 POSITION 01.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT  W-EMPRESA1  LINE 06 POSITION 50 CONVERT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  F1  OPEN INPUT EMPRESAS
                   PERFORM I-HELP    THRU F-HELP UNTIL ESC
                   PERFORM I-MOSTRAR THRU F-MOSTRAR
                   CLOSE EMPRESAS.
           IF CUP  OR ESC GO F-ACEPTA-EMPRESA.
           IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-EMPRESA1                      =  ZEROS
               MOVE CON-COD-EMPRESA            TO  W-EMPRESA1.
           MOVE W-EMPRESA1                     TO  WIN-CODIGO-EMPRESA
           OPEN I-O EMPRESAS
           READ EMPRESAS WITH NO LOCK
                INVALID  KEY  DISPLAY " EMPRESA NO EXISTE "
                  LINE 24 POSITION 25 REVERSE
                CLOSE EMPRESAS GO TO I-ACEPTA-EMPRESA.
           CLOSE EMPRESAS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80
           PERFORM I-NOMBRE THRU F-NOMBRE.
           DISPLAY WK-NOMBRE2 LINE 24 POSITION 15.
           IF WIN-INACTIVA-EMP = "S"
             PERFORM I-ADVERTENCIA THRU F-ADVERTENCIA
             GO TO I-FIN.
       F-ACEPTA-EMPRESA.

       I-ACEPTA-ANO.
           MOVE ZEROS TO WK-EXCEPTION.
           ACCEPT W-ANO    LINE 07 POSITION 50
                  UPDATE   CONVERT NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF CUP  OR ESC GO F-ACEPTA-ANO.
           IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-ANO = WK-ANO NEXT SENTENCE
           ELSE
               DISPLAY
              "* Crtl-Fin Para Modificar el A�O *"
                LINE 25 POSITION 01 BEEP REVERSE BLINK
               IF CEND
                  COMPUTE WK-DIF  =  W-ANO - WK-ANO
                  IF      WK-DIF  >  2
                          DISPLAY SPACES LINE 25 POSITION 01 SIZE 80
                          DISPLAY "ERROR EN ANO" LINE 25 POSITION 01
                                                                BEEP
                          GO      I-ACEPTA-ANO
                  ELSE
                      NEXT SENTENCE
               ELSE    GO   I-ACEPTA-ANO.
           IF  W-ANO                        =  ZEROS
               MOVE CON-ANO                TO  W-ANO.
       F-ACEPTA-ANO.

       I-ACEPTA-PERIODO.
               MOVE ZEROS TO WK-EXCEPTION.
               ACCEPT W-PERIODO   LINE 08 POSITION 50
                      CONVERT NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF CUP  OR ESC GO F-ACEPTA-PERIODO.
           IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-PERIODO                    =  ZEROS
               MOVE CON-PERIODO            TO  W-PERIODO.
           OPEN I-O EMPRESAS
           MOVE W-EMPRESA1  TO  WIN-CODIGO-EMPRESA
                                CON-COD-EMPRESA
           READ EMPRESAS    WITH NO LOCK
           MOVE W-ANO       TO  CON-ANO
                                WIN-FECHA-ANO
           MOVE W-PERIODO              TO  CON-PERIODO
           REWRITE REC-EMPRESAS
           CLOSE EMPRESAS
           CLOSE USUARIOS.
       F-ACEPTA-PERIODO.

       I-MENU.
           OPEN INPUT MENU
           MOVE SPACES                 TO  ME-IDENT
           MOVE 01                     TO  ME-LINEA
           READ MENU WITH NO LOCK       INVALID  KEY
                    GO TO 030-BUSCAR-MENU.
           MOVE SPACES                 TO  W-IDENT
               GO TO 030-BUSCAR-MENU.
       020-TOMA-MENU.
               MOVE "HIGH" TO SG-INTENSITY

               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.

               DISPLAY " " CONTROL SG-CONTROL-STRING.
               DISPLAY SPACES LINE 23 POSITION 1 SIZE 80
               DISPLAY SPACES LINE 24 POSITION 1 SIZE 80
               DISPLAY SPACES LINE 25 POSITION 1 SIZE 80
               DISPLAY "                   " LINE 23       POSITION 1
               DISPLAY "                   " LINE 24       POSITION 1
               DISPLAY "                   " LINE 25       POSITION 1
               DISPLAY "                   " LINE 23       POSITION 62
               DISPLAY "                   " LINE 24       POSITION 62
               DISPLAY "                   "  LINE 25       POSITION 62

               DISPLAY WK-RAYA-ARRIBA               LINE 23 POSITION  20
               DISPLAY "�"                          LINE 24 POSITION  20
               DISPLAY "�"                          LINE 24 POSITION 61
               DISPLAY WK-RAYA-ABAJO                LINE 25 POSITION  20
               DISPLAY " OPCION:"          LINE 24 POSITION 21
               DISPLAY " FIN <--PARA TERMINAR"      LINE 24 POSITION 40
                                                    BLINK
               ACCEPT W-IDENT                       LINE 24 POSITION 30
                                                    TAB
                                                    NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  W-IDENT                      =  "FIN"
               GO TO I-FIN-MODE.
       030-BUSCAR-MENU.
               MOVE "LOW" TO SG-INTENSITY
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
               DISPLAY " " CONTROL SG-CONTROL-STRING.
               MOVE ZEROS                  TO  SW1
                                               SW2
               MOVE W-IDENT                TO  ME-IDENT
               MOVE ZEROS                  TO  ME-LINEA
               START MENU               KEY >  ME-CLAVE
                                      INVALID  KEY
               MOVE SPACES                 TO  W-IDENT
               GO TO 030-BUSCAR-MENU.
       040-LEER-MENU.
               READ MENU NEXT WITH NO LOCK     AT  END
               GO TO 070-RETORNO.
           IF  ME-IDENT                 NOT =  W-IDENT
           AND SW1                          =  ZERO
               MOVE W-ID                   TO  W-IDENT
               GO TO 030-BUSCAR-MENU.
           IF  ME-IDENT                 NOT = W-IDENT
               GO TO 070-RETORNO.
               MOVE 1                      TO  SW1
               MOVE ME-TIPO                TO  W-TIPO.
           IF  W-TIPO                       =  "I"
               GO TO 060-INSTRUCCION.
       050-FORMATO.
           IF  SW2                          =  ZERO
               MOVE 1                      TO  SW2
               DISPLAY " "                          ERASE.
               MOVE ME-IDENT               TO  W-ID
               DISPLAY "�������������������" LINE ME-LINEA POSITION 1
               DISPLAY ME-DATO   LINE ME-LINEA  POSITION 20
               DISPLAY "�������������������" LINE ME-LINEA POSITION 62
           IF  ME-LINEA                     =  2
               DISPLAY WIN-EMPRESA  LINE  2 POSITION 21 SIZE 40
                                     BLINK.
           IF  ME-LINEA                     =  3
               DISPLAY  CON-ANO    LINE 3 POSITION 21.
           GO TO 040-LEER-MENU.
       060-INSTRUCCION.
               MOVE "HIGH" TO SG-INTENSITY
               DISPLAY " " CONTROL SG-CONTROL-STRING.
               DISPLAY PSW-1               LINE  2 POSITION 17
                             ERASE
               DISPLAY PSW-11              LINE 11 POSITION 12
               DISPLAY PSW-12              LINE 12 POSITION 12
               DISPLAY PSW-13              LINE 13 POSITION 12
               DISPLAY PSW-14              LINE 14 POSITION 12
               DISPLAY PSW-15              LINE 15 POSITION 12
               DISPLAY PSW-16              LINE 16 POSITION 12
               DISPLAY PSW-17              LINE 17 POSITION 12
               DISPLAY PSW-18              LINE 18 POSITION 12
               MOVE ZEROS    TO AREA-WORKING   W-VARIABLES-NOMINA
                                AREAS-DE-TRABAJO-NUMERICAS
               MOVE SPACES   TO AREAS-DE-TRABAJO-ALFANUMERICAS.
               PERFORM I-ERROR-CONERROR  THRU F-ERROR-CONERROR.
               CALL ME-DATO USING AREA-LINK-NOMINA
               CANCEL ME-DATO
               MOVE "HIGH" TO SG-INTENSITY
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
               DISPLAY " " CONTROL SG-CONTROL-STRING.
               GO TO 040-LEER-MENU.
       070-RETORNO.
               MOVE ZEROS                  TO  SW2
           IF  W-TIPO                       =  "D"
               GO TO 020-TOMA-MENU.
               MOVE W-ID                   TO  W-IDENT
               GO TO 030-BUSCAR-MENU.
       F-MENU.

       I-PANTALLA.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           PERFORM I-COLOR-PA THRU F-COLOR-PA.
       F-PANTALLA.

      *
       I-HELP.
           MOVE 01 TO I.
           MOVE 05 TO I-LINEA.
           MOVE ZEROS TO WIN-CLAVE FIN-OK.
           PERFORM I-START-EMPRESA   THRU F-START-EMPRESA.
           PERFORM I-WINDOW-EMPRESA1S THRU F-WINDOW-EMPRESA1S.
           PERFORM I-DISPLAY-EMPR    THRU F-DISPLAY-EMPR
                   UNTIL I > 5.
        LEE-EMPR.
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
                MOVE 05 TO I-LINEA
                MOVE ZEROS TO FIN-OK
                MOVE W-EMPRESA1 TO WIN-CLAVE
                PERFORM I-START-EMPRESA   THRU F-START-EMPRESA
                PERFORM I-WINDOW-EMPRESA1S THRU F-WINDOW-EMPRESA1S
                PERFORM I-DISPLAY-EMPR    THRU F-DISPLAY-EMPR
                        UNTIL I > 5
                IF FIN-OK = 1
                   DISPLAY SPACE LINE 19 POSITION 72 REVERSE
                   GO LEE-EMPR
                ELSE
                   GO LEE-EMPR.
       F-HELP.
      *
       I-START-EMPRESA.
           START EMPRESAS KEY NOT LESS WIN-CLAVE.
       F-START-EMPRESA.
      *
       I-WINDOW-EMPRESA1S.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY "�������������������������������������������������ͻ"
                        LINE  2 POSITION 25 CONTROL SG-CONTROL-STRING
                                                       REVERSE.
           DISPLAY "� COD. �    EMPRESA                               �"
                        LINE  3 POSITION 25 REVERSE.
           DISPLAY "�������������������������������������������������͹"
                        LINE  4 POSITION 25 REVERSE.
           DISPLAY "�      �                                          �"
                        LINE  5 POSITION 25 REVERSE.
           DISPLAY "�      �                                          �"
                        LINE  6 POSITION 25 REVERSE.
           DISPLAY "�      �                                          �"
                        LINE  7 POSITION 25 REVERSE.
           DISPLAY "�      �                                          �"
                        LINE  8 POSITION 25 REVERSE.
           DISPLAY "�      �                                         �"
                        LINE  9 POSITION 25 REVERSE.
           DISPLAY "�������������������������������������������������ͼ"
                        LINE 10 POSITION 25 REVERSE.
       F-WINDOW-EMPRESA1S.
      *
       I-DISPLAY-EMPR.
           PERFORM   I-LEE-EMPRESAS   THRU F-LEE-EMPRESAS.
           IF   FIN-OK = 1
                DISPLAY "***" LINE    I-LINEA POSITION 28 REVERSE
                DISPLAY "Fin Archivo" LINE I-LINEA POSITION 34 REVERSE
                MOVE 6 TO I
                GO   F-DISPLAY-EMPR.
           MOVE WIN-EMPRESA TO WK-EMPRESAS.
           DISPLAY   WIN-CODIGO-EMPRESA LINE I-LINEA POSITION 28.
           DISPLAY   WK-EMPRESAS        LINE I-LINEA POSITION 34.
           IF I = 2  MOVE WIN-CODIGO-EMPRESA TO W-EMPRESA1.
           ADD   1   TO  I.
           ADD   1   TO  I-LINEA.
       F-DISPLAY-EMPR.
      *
       I-LEE-EMPRESAS-I.
           READ EMPRESAS WITH NO LOCK
                INVALID  KEY
                DISPLAY "ERROR DE LOGICA * EMPRESA"
                        LINE 12 POSITION 12 ERASE STOP RUN.
       F-LEE-EMPRESAS-I.

       I-LEE-EMPRESAS.
           MOVE ZEROS TO FIN-OK.
           READ EMPRESAS NEXT RECORD WITH NO LOCK
                AT END MOVE 1 TO FIN-OK.
       F-LEE-EMPRESAS.
      *
       I-MOSTRAR.
           PERFORM I-PANTALLA THRU F-PANTALLA.
           MOVE "NO" TO SG-BLINK
           MOVE "NO" TO SG-BEEP.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.

               MOVE CON-PERIODO            TO  W-PERIODO
               DISPLAY PSW-2                        LINE  2 POSITION 17
                                             CONTROL SG-CONTROL-STRING.
               DISPLAY PSW-3                        LINE  3 POSITION 17
               DISPLAY PSW-4                        LINE  4 POSITION 17
               DISPLAY PSW-5                        LINE  5 POSITION 17
               DISPLAY PSW-6                        LINE  6 POSITION 17
               DISPLAY PSW-7                        LINE  7 POSITION 17
               DISPLAY PSW-8                        LINE  8 POSITION 17
               DISPLAY PSW-9                        LINE  9 POSITION 17
               DISPLAY PSW-11                       LINE 11 POSITION 12
               DISPLAY PSW-12                       LINE 12 POSITION 12
               DISPLAY PSW-13                       LINE 13 POSITION 12
               DISPLAY PSW-14                       LINE 14 POSITION 12
               DISPLAY PSW-15                       LINE 15 POSITION 12
               DISPLAY PSW-16                       LINE 16 POSITION 12
               DISPLAY PSW-17                       LINE 17 POSITION 12
               DISPLAY PSW-18                       LINE 18 POSITION 12
               MOVE "HIGH" TO SG-INTENSITY
               MOVE "NO" TO SG-BEEP.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
               DISPLAY KEY-LICENCIA LINE 03 POSITION 50
               DISPLAY W-MENU    LINE 04 POSITION 50
               DISPLAY CON-USERNAME LINE 05 POSITION 50
               DISPLAY W-EMPRESA1 LINE 06 POSITION 50
                                 CONTROL SG-CONTROL-STRING.
               DISPLAY W-ANO     LINE 07 POSITION 50
               DISPLAY W-PERIODO LINE 08 POSITION 50.
               DISPLAY " F1 help Empresas " LINE 24 POSITION 01 REVERSE.
       F-MOSTRAR.

       I-NOMBRE.
           MOVE WIN-EMPRESA TO WK-NOMBRE1.
           MOVE 1 TO I.
         LOOP-1N.
           IF I > 50 GO TO F-NOMBRE.
           IF TAB-NOM1(I) = SPACE
              ADD 1 TO I
              IF TAB-NOM1(I) = SPACE
                  SUBTRACT 2 FROM I
                  GO TO CONT-1N.
           ADD 1 TO I.
           GO TO LOOP-1N.
         CONT-1N.
           MOVE SPACES TO WK-NOMBRE2.
           COMPUTE J = (50 - I) / 2.
           MOVE 1 TO K.
           DISPLAY I LINE 24 POSITION 01.
           DISPLAY J LINE 24 POSITION 05.
         LOOP-2N.
           IF K > I GO TO F-NOMBRE.
           MOVE TAB-NOM1(K) TO TAB-NOM2(J).
           ADD 1 TO J.
           ADD 1 TO K.
           GO TO LOOP-2N.
       F-NOMBRE.
      *
       I-GENERA-USUARIO.
           MOVE SPACES TO USUARIOS-STATUS.
           OPEN OUTPUT USUARIOS CLOSE USUARIOS.
           OPEN I-O    USUARIOS.
           MOVE SPACES       TO COD-PRG.
           MOVE "WIN"        TO COD-USR.
           MOVE "GABRIEL"   TO CLAVE-USR.
           MOVE "A"          TO TIPO-USR.
           MOVE "ADMINISTRADOR" TO NOM-USUARIO.
           WRITE REG-USER.
           MOVE  "PROG000"   TO COD-PRG.
           MOVE  "WIN"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE USUARIOS" TO NOM-USUARIO.
           WRITE REG-USER.

           MOVE  "PROG100"   TO COD-PRG.
           MOVE  "WIN"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE EMPRESAS" TO NOM-USUARIO.
           WRITE REG-USER.
           MOVE  "PROG005"   TO COD-PRG.
           MOVE  "WIN"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE CLAVES  " TO NOM-USUARIO.
           WRITE REG-USER.
           CLOSE USUARIOS.

           MOVE SPACES       TO COD-PRG.
           MOVE "JJT"        TO COD-USR.
           MOVE "GABRIEL"   TO CLAVE-USR.
           MOVE "A"          TO TIPO-USR.
           MOVE "ADMINISTRADOR" TO NOM-USUARIO.
           WRITE REG-USER.
           MOVE  "PROG000"   TO COD-PRG.
           MOVE  "JJT"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE USUARIOS" TO NOM-USUARIO.
           WRITE REG-USER.

           MOVE  "PROG100"   TO COD-PRG.
           MOVE  "JJT"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE EMPRESAS" TO NOM-USUARIO.
           WRITE REG-USER.
           MOVE  "PROG005"   TO COD-PRG.
           MOVE  "JJT"       TO COD-USR.
           MOVE  "GABRIEL"   TO CLAVE-USR.
           MOVE  "ACTUALIZACION DE CLAVES  " TO NOM-USUARIO.
           WRITE REG-USER.
           CLOSE USUARIOS.
       F-GENERA-USUARIO.
      *
       I-STATUS-HOJAVIDA.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT HOJAVIDA
               MOVE 1                      TO  SW-ABRE
               IF  HOJAVIDA-STATUS              =  "35" OR
                   HOJAVIDA-STATUS              =  "94"
                   OPEN OUTPUT HOJAVIDA
               ELSE
                   IF  HOJAVIDA-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-HOJAVIDA
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE HOJAVIDA.
       F-STATUS-HOJAVIDA.

       I-STATUS-DINOMBRE.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT DINOMBRE
               MOVE 1                      TO  SW-ABRE
               IF  DINOMBRE-STATUS              =  "35" OR
                   DINOMBRE-STATUS              =  "94"
                   OPEN OUTPUT DINOMBRE
               ELSE
                   IF  DINOMBRE-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-DINOMBRE
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE DINOMBRE.
       F-STATUS-DINOMBRE.

       I-STATUS-CONTRATO.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CONTRATO
               MOVE 1                      TO  SW-ABRE
               IF  CONTRATO-STATUS              =  "35" OR
                   CONTRATO-STATUS              =  "94"
                   OPEN OUTPUT CONTRATO
               ELSE
                   IF  CONTRATO-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CONTRATO
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE CONTRATO.
       F-STATUS-CONTRATO.

       I-STATUS-EMPRESAS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT EMPRESAS
               MOVE 1                      TO  SW-ABRE
               IF  EMPRESAS-STATUS              =  "35" OR
                   EMPRESAS-STATUS              =  "94"
                   OPEN OUTPUT EMPRESAS
                   CLOSE EMPRESAS
                   OPEN I-O  EMPRESAS
                   MOVE 1 TO WIN-CODIGO-EMPRESA
                             WIN-MESSAL-ACT WIN-MESSUB-ACT
                             WIN-DIASAL-ACT WIN-DIASUB-ACT

      *            ACCEPT WK-FECHA-HOY       FROM  DATE
                   MOVE WK-ANO     TO  WIN-FECHA-ANO
                                       WIN-ANOSAL-ACT WIN-ANOSUB-ACT
                   MOVE WK-MES     TO  WIN-FECHA-MES
                   MOVE WK-DIA     TO  WIN-FECHA-DIA
                   WRITE REC-EMPRESAS
               ELSE
                   IF  EMPRESAS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-EMPRESAS
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE EMPRESAS.

       F-STATUS-EMPRESAS.

      *****************************************************************
      * Punto pro label, se utiliza para el direccionamiento de       *
      * archivo CONTRATO en aplicacion de nomina.                     *
      *****************************************************************
       I-LABEL-CONTRATO.
           MOVE 1                  TO LAB-MAESCIA.
           MOVE 1                  TO LAB-CONTRATO-CIA.
       F-LABEL-CONTRATO. EXIT.
      *
       I-ADVERTENCIA.
            DISPLAY "������������������������������������������������ͻ"
            LINE 07 POSITION 17 BEEP REVERSE
            DISPLAY "�              A T E N C I O N  !                �"
            LINE 08 POSITION 17 BEEP REVERSE
            DISPLAY "�            SUCURSAL NO ESTA ACTIVA!            �"
            LINE 09 POSITION 17 BEEP REVERSE.
            DISPLAY "������������������������������������������������ͼ"
            LINE 10 POSITION 17 BEEP REVERSE
            "    <ENTER>   CONTINUE "
            LINE 15 POSITION 19 BLINK.
            PERFORM ACEPTA-TRUCO.
       F-ADVERTENCIA.  EXIT.

       I-FIN-MODE.
               CLOSE MENU.
       I-FIN.
               MOVE "HIGH" TO SG-INTENSITY
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
               DISPLAY " " ERASE CONTROL SG-CONTROL-STRING.
               STOP RUN.
       F-FIN-MODE.

              COPY "..\PRO\PANTALLA.PRO".
              COPY "..\PRO\COLOR200.PRO".
              COPY "..\PRO\TRUQUITO.PRO".
              COPY "..\PRO\CONERROR.PRO".
              COPY "..\PLB\CONERROR.PLB".


