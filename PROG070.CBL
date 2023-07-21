       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG070.
      ******************************************************************
      *        CORPORACIONES     MAY 1 DE 2002                         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\NOMICORP.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\NOMICORP.FD".
      *        COPY "..\FD\TABLAEMP.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG070".
       77      ID-PRG              PIC X(7)  VALUE "PROG070".
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\NOMICORP.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMICORP.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.

       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREA-PANTALLA   THRU F-CREA-PANTALLA
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
      *        MOVE WIN-CODIGO-EMPRESA     TO  LAB-TABLAEMP-CIA
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
       F-LABEL-ARCHIVOS.

       I-ABRE-ARCHIVOS.
               OPEN I-O   NOMICORP.
               OPEN INPUT CNCATCTA.
       F-ABRE-ARCHIVOS.

       I-MENU-OPCIONES.
           PERFORM I-BLANQUEA  THRU F-BLANQUEA.
           PERFORM I-FUNCIONES THRU F-FUNCIONES.
           MOVE ZEROS        TO  WK-EXCEPTION FIN-OK
           ACCEPT W-OPCION   LINE 21 POSITION 55
                  UPDATE  NO BEEP ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           DISPLAY SPACE LINE 21 POSITION 11 SIZE 56.
           DISPLAY "[ ESC ] / F2    TERMINA" LINE 21 POSITION 25.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
           IF ESC
              PERFORM  I-FIN-MODE THRU F-FIN-MODE
           ELSE
             IF F5
                PERFORM  I-CREAR THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF F6
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE THRU F-MODIFY-MODE
                           UNTIL ESC OR CUP
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF F7
                   PERFORM  I-CONSUL-MODE THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF F8
                      PERFORM  I-ELIMINA THRU F-ELIMINA
                               UNTIL ESC OR CUP
                      MOVE ZEROS TO WK-EXCEPTION.
       F-MENU-OPCIONES.


       I-CREAR.
           MOVE 01 TO SW-OPCION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-CREAR.
           IF      ESC          GO   F-CREAR.
           PERFORM I-LEE-NOMICORP THRU F-LEE-NOMICORP.
           IF  SW-1  =  ZEROS
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-EXISTE         THRU F-EXISTE
               GO I-CREAR
           ELSE
               MOVE CORP-CODIGO TO W-CODIGO
               MOVE SPACE       TO REC-NOMICORP
               MOVE W-CODIGO    TO CORP-CODIGO
               PERFORM I-BLANQUEA THRU F-BLANQUEA
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP
                  PERFORM I-BLANQUEA THRU F-BLANQUEA
                  GO I-CREAR
               ELSE
                  PERFORM I-BLANQUEA   THRU F-BLANQUEA.
       F-CREAR.


       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-ELIMINA.
           IF      ESC          GO   F-ELIMINA.
           PERFORM I-LEE-NOMICORP THRU F-LEE-NOMICORP.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN.
           PERFORM I-BLANQUEA THRU F-BLANQUEA.
       F-ELIMINA.

       I-MODIFY-MODE.
           MOVE 02 TO SW-OPCION.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-MODIFY-MODE.
           IF      ESC          GO   F-MODIFY-MODE.
           PERFORM I-LEE-NOMICORP THRU F-LEE-NOMICORP.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP
                  PERFORM I-BLANQUEA THRU F-BLANQUEA
                  GO I-MODIFY-MODE
               ELSE
                  PERFORM I-BLANQUEA   THRU F-BLANQUEA.
       F-MODIFY-MODE.

       I-CONSUL-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES       LINE 23 POSITION 1 SIZE 80.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-CONSUL-MODE.
           IF      ESC          GO   F-CONSUL-MODE.
           PERFORM I-LEE-NOMICORP THRU F-LEE-NOMICORP.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-CONSUL-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CONSULTA   THRU F-CONSULTA
                   UNTIL ESC OR F2 OR SW-1 = 1.
           PERFORM I-BLANQUEA THRU F-BLANQUEA.
       F-CONSUL-MODE.
      *
       I-GRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 10.             DISPLAY
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 24 POSITION 10.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              WRITE REC-NOMICORP INVALID KEY STOP " ".
           DISPLAY SPACE LINE 23 POSITION 10 SIZE 60.
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-GRABAR.
      *
       I-REGRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 10.             DISPLAY
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 24 POSITION 10.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "G"
              REWRITE REC-NOMICORP INVALID KEY STOP " ".
           DISPLAY SPACE LINE 23 POSITION 10 SIZE 60.
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-REGRABAR.
      *
       I-ELIMINAR.
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 10.             DISPLAY
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 24 POSITION 10.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE NOMICORP INVALID KEY STOP " ".
           DISPLAY SPACE LINE 23 POSITION 10 SIZE 60.
           DISPLAY SPACE LINE 24 POSITION 10 SIZE 60.
       F-ELIMINAR.

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
                   IF      CUP  GO   F-DATOS.
         I-DIR.
           PERFORM I-DIRECCION  THRU F-DIRECCION.
                   IF      CUP  GO   I-DATOS.
         I-TEL.
           PERFORM I-TELEFONO   THRU F-TELEFONO.
                   IF      CUP  GO   I-DIR.
         I-CTAS.
           MOVE  1 TO I.
           MOVE 15 TO I-LINEA.
           MOVE 23 TO J-LINEA.
           PERFORM I-CUENTAS    THRU F-CUENTAS
             VARYING I FROM 1 BY 1 UNTIL I > 5.
           IF      CUP          GO   I-TEL.

           IF SW-OPCION = 1
                  MOVE ZEROS  TO WK-EXCEPTION
                  MOVE SPACES TO W-TRUCO
                  PERFORM I-GRABAR THRU F-GRABAR
                         UNTIL W-TRUCO = "G" OR CDN OR CUP
                  IF CUP
                     GO I-CTAS
                  ELSE
                     NEXT SENTENCE
           ELSE
              IF SW-OPCION = 2
                  MOVE ZEROS  TO WK-EXCEPTION
                  MOVE SPACES TO W-TRUCO
                  PERFORM I-REGRABAR THRU F-REGRABAR
                         UNTIL W-TRUCO = "G" OR CDN OR CUP
                  IF CUP
                     GO I-CTAS.
       F-DATOS.
      *
       I-MUESTRA-CAMPOS.
           DISPLAY CORP-CODIGO         LINE 10 POSITION 25
           DISPLAY CORP-NOMBRE         LINE 10 POSITION 44
           DISPLAY CORP-DIRECCION      LINE 12 POSITION 24
           DISPLAY CORP-TELEFONO       LINE 13 POSITION 24 .
           MOVE  1 TO K.
           MOVE 15 TO K-LINEA.
           PERFORM I-MUESTRA-CTAS THRU F-MUESTRA-CTAS
                   UNTIL K > 5.
       F-MUESTRA-CAMPOS.

       I-MUESTRA-CTAS.
           MOVE 23 TO  M-LINEA.
           MOVE    CORP-CTA-CTE (K)  TO   WI-Z12
           DISPLAY WI-Z12  LINE K-LINEA POSITION M-LINEA .
           MOVE 37 TO  M-LINEA.
           MOVE    CORP-CTA-CONTABLE (K) TO   WI-Z10
           DISPLAY WI-Z10           LINE K-LINEA POSITION M-LINEA
           MOVE 53 TO  M-LINEA.
           MOVE    CORP-CONS-CHEQUE  (K) TO   WI-Z10
           DISPLAY WI-Z10            LINE K-LINEA POSITION M-LINEA
           ADD   1 TO  K-LINEA.
           ADD   1 TO  K.
       F-MUESTRA-CTAS.

       I-CODIGO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CORP-CODIGO                  LINE 10 POSITION 25
           ACCEPT  CORP-CODIGO                  LINE 10 POSITION 25
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  CORP-CODIGO = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           DISPLAY WK-ESPACIOS                LINE 24 POSITION 1.
       F-CODIGO.

       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CORP-NOMBRE                  LINE 10 POSITION 44
           ACCEPT  CORP-NOMBRE                  LINE 10 POSITION 44
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-NOMBRE.
           IF  CORP-NOMBRE = SPACE GO I-NOMBRE.
       F-NOMBRE.

       I-DIRECCION.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CORP-DIRECCION               LINE 12 POSITION 24
           ACCEPT  CORP-DIRECCION               LINE 12 POSITION 24
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-DIRECCION.

       I-TELEFONO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CORP-TELEFONO                LINE 13 POSITION 24
           ACCEPT  CORP-TELEFONO                LINE 13 POSITION 24
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-TELEFONO.
      *
       I-CUENTAS.
           PERFORM I-CTA-CTE    THRU  F-CTA-CTE.
           IF CUP
              IF I-LINEA = 15
                 MOVE  5 TO I
                 GO    F-CUENTAS
              ELSE
                 COMPUTE I       = I       - 1
                 COMPUTE I-LINEA = I-LINEA - 1
                 GO I-CONS.
           IF CORP-CTA-CTE (I) = ZEROS OR
              CORP-CTA-CTE (I) = SPACE
              IF I < 5
                 MOVE    I TO J
                 COMPUTE K =  I + 1
                 PERFORM I-VALIDA-CTA THRU F-VALIDA-CUENTA
                                           UNTIL   K  >  5
                 PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
                 GO I-CONT
              ELSE
                 GO F-CUENTAS.
         I-CONT.
           MOVE    37   TO J-LINEA.
           PERFORM I-COD-CONT   THRU  F-COD-CONT.
           IF CUP
              MOVE 23 TO J-LINEA
              GO   I-CUENTAS.
         I-CONS.
           MOVE    53   TO J-LINEA.
           PERFORM I-CONS-CHEQ  THRU  F-CONS-CHEQ.
           IF CUP
              MOVE 37 TO J-LINEA
              GO I-CONT.
           ADD   1 TO I-LINEA.
           MOVE 23 TO J-LINEA.
       F-CUENTAS.
      *
       I-VALIDA-CTA.
           MOVE CORP-CTA-CTE      (K) TO CORP-CTA-CTE      (J)
           MOVE CORP-CTA-CONTABLE (K) TO CORP-CTA-CONTABLE (J)
           MOVE CORP-CONS-CHEQUE  (K) TO CORP-CONS-CHEQUE  (J)
           MOVE ZEROS                 TO CORP-CTA-CTE      (K)
           MOVE ZEROS                 TO CORP-CTA-CONTABLE (K)
           MOVE ZEROS                 TO CORP-CONS-CHEQUE  (K).
           ADD  1  TO  K.
           ADD  1  TO  J.
       F-VALIDA-CUENTA.
      *
       I-CTA-CTE.
           COMPUTE J-LINEA = J-LINEA - 7.
           DISPLAY I LINE I-LINEA POSITION J-LINEA.
           COMPUTE J-LINEA = J-LINEA + 7.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CORP-CTA-CTE (I)    TO  WI-Z12.
           DISPLAY WI-Z12                  LINE I-LINEA POSITION J-LINEA
           ACCEPT  CORP-CTA-CTE (I)        LINE I-LINEA POSITION J-LINEA
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE    CORP-CTA-CTE (I)    TO  WI-Z12.
           DISPLAY WI-Z12                  LINE I-LINEA POSITION J-LINEA
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  I = 1
               IF CORP-CTA-CTE (I) = ZEROS OR
                  CORP-CTA-CTE (I) = SPACE
                  GO   I-CTA-CTE.
       F-CTA-CTE.
      *
       I-COD-CONT.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CORP-CTA-CONTABLE (I) TO  WI-Z10.
           DISPLAY WI-Z10                LINE I-LINEA POSITION J-LINEA
           ACCEPT  CORP-CTA-CONTABLE (I) LINE I-LINEA POSITION J-LINEA
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE    CORP-CTA-CONTABLE (I) TO  WI-Z10 CTAMAE.
           IF      CORP-CTA-CONTABLE (I)   = ZEROS  GO COD-1.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF SW-EOF = 1 OR ESTACTA = "S"
              DISPLAY "CTA INCONSISTENTE  EN CATALOGO DE CUENTAS"
              LINE 24 POSITION 15 REVERSE BEEP
              GO I-COD-CONT.
        COD-1.
           DISPLAY WI-Z10                LINE I-LINEA POSITION J-LINEA
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-COD-CONT.
      *
       I-CONS-CHEQ.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CORP-CONS-CHEQUE (I) TO  WI-Z10.
           DISPLAY WI-Z10                LINE I-LINEA POSITION J-LINEA
           ACCEPT  CORP-CONS-CHEQUE (I) LINE I-LINEA POSITION J-LINEA
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE    CORP-CONS-CHEQUE (I) TO  WI-Z10.
           DISPLAY WI-Z10                LINE I-LINEA POSITION J-LINEA
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CONS-CHEQ.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESPACIOS  LINE 24 POSITION  1.
           DISPLAY W-OPCION     LINE  4 POSITION 13
           IF  W-OPCION                     =  "C"
               DISPLAY "   CONSULTA    "
                                    LINE  4 POSITION 15 REVERSE.
           IF  W-OPCION                      =  "M"
               DISPLAY " MODIFICACION  "
                                    LINE  4 POSITION 15 REVERSE.
               DISPLAY WK-ESPACIOS  LINE 23 POSITION  1.
       F-DISPLAY-OPCION.

       I-FUNCIONES.
           DISPLAY
           "บ       [ ESC ]  TERMINA           OPCION  [ _ ]          บ"
                                               LINE 21 POSITION 10.             DISPLAY
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
           DISPLAY "CREA            MODIFICA           CONSULTA
      -             "  ELIMINA"      LINE 24 POSITION 14.
           DISPLAY "[ F5 ]"        LINE 24 POSITION 07 REVERSE.
           DISPLAY "[ F6 ]"        LINE 24 POSITION 23 REVERSE.
           DISPLAY "[ F7 ]"        LINE 24 POSITION 42 REVERSE.
           DISPLAY "[ F8 ]"        LINE 24 POSITION 61 REVERSE.
       F-FUNCIONES.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************
       I-LEE-NOMICORP.
               MOVE 0  TO   SW-1.
               READ NOMICORP WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-NOMICORP.
      *
       I-LEE-CNCATCTA.
               MOVE 0  TO   SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID  KEY
                    MOVE  1  TO  SW-EOF
                    GO F-LEE-CNCATCTA.
               DISPLAY NOMCTA  LINE 24 POSITION 15 SIZE 40 REVERSE.
       F-LEE-CNCATCTA.
      *
       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
      *    EVALUE   W-TRUCO
           IF W-TRUCO =  "M"
              REWRITE REC-NOMICORP.
           IF CDN
              REWRITE REC-NOMICORP
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
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
       F-CONSULTA.

       I-LEE-SIGUIENTE.
           MOVE 0 TO SW-1.
           READ NOMICORP NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
       F-LEE-SIGUIENTE.


       120-FT.

       I-BLANQUEA.
      *    DISPLAY SPACE LINE 10 POSITION 23 SIZE 12.
           DISPLAY SPACE LINE 10 POSITION 43 SIZE 25.
           DISPLAY SPACE LINE 12 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 13 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 15 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 16 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 17 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 18 POSITION 23 SIZE 45.
           DISPLAY SPACE LINE 19 POSITION 23 SIZE 45.
       F-BLANQUEA.

       I-CREA-PANTALLA.
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
            DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING .
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 06 POSITION 10 .
           DISPLAY
           "บ                                     Fecha :             บ"
                                               LINE 07 POSITION 10 .
           DISPLAY
           "บ                                     Hora  :             บ"
                                               LINE 08 POSITION 10 .
           DISPLAY
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
                                               LINE 09 POSITION 10 .
           DISPLAY
           "บ CODIGO    :            NOMBRE :                         บ"
                                               LINE 10 POSITION 10 .
           DISPLAY
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
                                               LINE 11 POSITION 10 .
           DISPLAY
           "บ DIRECCION :                                             บ"
                                               LINE 12 POSITION 10 .
           DISPLAY
           "บ TELEFONO  :                                             บ"
                                               LINE 13 POSITION 10 .
           DISPLAY
           "บ            No. Cuenta   Cod.Contable    Consecutivo     บ"
                                               LINE 14 POSITION 10 .
           DISPLAY
           "บ                                                         บ"
                                               LINE 15 POSITION 10 .
           DISPLAY
           "บ                                                         บ"
                                               LINE 16 POSITION 10 .
           DISPLAY
           "บ                                                         บ"
                                               LINE 17 POSITION 10 .
           DISPLAY
           "บ                                                         บ"
                                               LINE 18 POSITION 10 .
           DISPLAY
           "บ                                                         บ"
                                               LINE 19 POSITION 10 .
           DISPLAY
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
                                               LINE 20 POSITION 10 .
           DISPLAY
           "บ       [ ESC ]  TERMINA           OPCION  [ _ ]          บ"
                                               LINE 21 POSITION 10 .             DISPLAY
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 22 POSITION 10 .
           DISPLAY WEMPRESA                    LINE 07 POSITION 11 .
           DISPLAY "C.A.V"                     LINE 08 POSITION 20 .
           DISPLAY WK-MASCARA-FECHA            LINE 07 POSITION 56 .
           DISPLAY WK-MASCARA-HORA             LINE 08 POSITION 56 .
       F-CREA-PANTALLA.

       I-FIN-MODE.
               CLOSE NOMICORP
                     CNCATCTA.
               EXIT PROGRAM.
               STOP RUN.
       F-FIN-MODE.


               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\AUDITE.PRO".
               COPY "..\PRO\PANTALLA.PRO".


