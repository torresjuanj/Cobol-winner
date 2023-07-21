       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG125.
      ******************************************************************
      * TABLA DE RETEFUENTE       ABR-27-2002                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\RETENFTE.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\RETENFTE.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG125".
       77      ID-PRG              PIC X(7)  VALUE "PROG125".
       77      W-VARIABLES-NOMINA  PIC X(8)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA     PIC 9(6)       OCCURS 25 TIMES.
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\RETENFTE.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\RETENFTE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
       01      WK-AREA-TABLAS.
        03     WK-CODIGO-TABLA           PIC X(3)       VALUE "053".
        03     WK-NOMBRE-TABLA           PIC X(30)      VALUE
               "  ** RETENCION EN LA FUENTE **".
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\RETENFTE.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       010-INICIO.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           MOVE WIN-EMPRESA            TO   WK-COMPANIA
           MOVE "¹"                    TO   WK-RAYAS-U
           MOVE "Ì"                    TO   WK-RAYAS-P
           MOVE "¶"                    TO   WK-RAYA-U
           MOVE "Ç"                    TO   WK-RAYA-P
           PERFORM I-STATUS-RETENFTE THRU F-STATUS-RETENFTE.
           OPEN I-O RETENFTE.
       015-CEROS.
               PERFORM 140-BORRAR-PANTALLA THRU
                       140-SALE-BORRAR-PANTALLA.
       015-INICIO.
               MOVE ZEROS                  TO  I-ERROR
               START RETENFTE KEY           >  RETENFTE-KEY00
                                      INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  RETENFTE-STATUS              =  "23"
               NEXT SENTENCE
           ELSE
               PERFORM RUTINA-ERROR-RETENFTE.
       020-PANTALLA.
               DISPLAY WK-RAYAS-ARRIBA              LINE  1 ERASE
               DISPLAY "º"                          LINE  2 POSITION  1
               DISPLAY WK-COMPANIA                  LINE  2 POSITION  3
               DISPLAY "WINNER FINANCIAL  º"        LINE  2 POSITION 62
               DISPLAY "º DATE :"                   LINE  3 POSITION  1
               DISPLAY WK-MASCARA-FECHA             LINE  3 POSITION 10
               DISPLAY WK-NOMBRE-TABLA              LINE  3 POSITION 26
               DISPLAY "TIME:"                      LINE  3 POSITION 68
               DISPLAY WK-MASCARA-HORA              LINE  3 POSITION 74
               DISPLAY "º"                          LINE  3 POSITION 80
               DISPLAY "º"                          LINE  4 POSITION  1
               DISPLAY "OPCION:"                    LINE  4 POSITION 28
               DISPLAY "º"                          LINE  4 POSITION 80
               DISPLAY WK-RAYAS                     LINE  5 POSITION  1
               DISPLAY "º CATEG."                   LINE  6 POSITION  1
               DISPLAY "VALOR MINIMO"               LINE  6 POSITION 10
               DISPLAY "VALOR MAXIMO"               LINE  6 POSITION 26
               DISPLAY "PORCENTAJE"                 LINE  6 POSITION 44
               DISPLAY "VALOR APORTE"               LINE  6 POSITION 58
               DISPLAY "º"                          LINE  6 POSITION 80
               DISPLAY WK-RAYAS                     LINE  7 POSITION  1
               DISPLAY "º"                          LINE  8 POSITION  1
               DISPLAY "º"                          LINE  8 POSITION 80
               DISPLAY "º"                          LINE  9 POSITION  1
               DISPLAY "º"                          LINE  9 POSITION 80
               DISPLAY "º"                          LINE 10 POSITION  1
               DISPLAY "º"                          LINE 10 POSITION 80
               DISPLAY "º"                          LINE 11 POSITION  1
               DISPLAY "º"                          LINE 11 POSITION 80
               DISPLAY "º"                          LINE 12 POSITION  1
               DISPLAY "º"                          LINE 12 POSITION 80
               DISPLAY "º"                          LINE 13 POSITION  1
               DISPLAY "º"                          LINE 13 POSITION 80
               DISPLAY "º"                          LINE 14 POSITION  1
               DISPLAY "º"                          LINE 14 POSITION 80
               DISPLAY "º"                          LINE 15 POSITION  1
               DISPLAY "º"                          LINE 15 POSITION 80
               DISPLAY "º"                          LINE 16 POSITION  1
               DISPLAY "º"                          LINE 16 POSITION 80
               DISPLAY "º"                          LINE 17 POSITION  1
               DISPLAY "º"                          LINE 17 POSITION 80
               DISPLAY "º"                          LINE 18 POSITION  1
               DISPLAY "º"                          LINE 18 POSITION 80
               DISPLAY "º"                          LINE 19 POSITION  1
               DISPLAY "º"                          LINE 19 POSITION 80
               DISPLAY WK-RAYAS-ABAJO               LINE 20 POSITION  1
               DISPLAY WK-RAYA-ARRIBA               LINE 21 POSITION  1
               DISPLAY "³"                          LINE 22 POSITION  1
               DISPLAY " F2 "                       LINE 22 POSITION  3
                                                    REVERSE
               DISPLAY "Terminar"                   LINE 22 POSITION  8
               DISPLAY " A "                        LINE 22 POSITION 19
                                                    REVERSE
               DISPLAY "Adicion "                   LINE 22 POSITION 23
               DISPLAY " C "                        LINE 22 POSITION 33
                                                    REVERSE
               DISPLAY "Consulta"                   LINE 22 POSITION 37
               DISPLAY " M "                        LINE 22 POSITION 48
                                                    REVERSE
               DISPLAY "Modificacion"               LINE 22 POSITION 52
               DISPLAY " R "                        LINE 22 POSITION 67
                                                    REVERSE
               DISPLAY "Retiro"                     LINE 22 POSITION 71
               DISPLAY "³"                          LINE 22 POSITION 80
               DISPLAY WK-RAYA-ABAJO                LINE 23 POSITION  1.
       025-OPCION.
               DISPLAY W-ESPALF                     LINE  4 POSITION 37
               DISPLAY W-ESPALF                     LINE  4 POSITION 48
               PERFORM 115-PANTALLA-CONSULTA
               PERFORM 015-CEROS
               MOVE ZEROS                  TO  I-ERROR
               START RETENFTE KEY           >  RETENFTE-KEY00
                                      INVALID  KEY
               MOVE 1                      TO  I-ERROR.
           IF  I-ERROR                  NOT =  ZEROS
           IF  RETENFTE-STATUS              =  "23"
               NEXT SENTENCE
           ELSE
               PERFORM RUTINA-ERROR-RETENFTE.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-OPCION             LINE  4 POSITION 37 NO BEEP
                                           ON EXCEPTION WK-EX
                                           PERFORM 999-EXCEPTION.
           IF  CUP
               GO TO 025-OPCION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-OPCION                     =  "A"
           OR  W-OPCION                     =  "C"
           OR  W-OPCION                     =  "M"
           OR  W-OPCION                     =  "R"
               MOVE SPACES                 TO  SW-CONSULTA
               GO TO 025-SIGUE-OPCION
           ELSE
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               DISPLAY " OPCION ERRADA "            LINE 24 POSITION 1
                                                    REVERSE
               DISPLAY " TECLEE UNICAMENTE  A / C / M / R "
                                                    LINE 24 POSITION 18
                                                    REVERSE
               GO TO 025-OPCION.
       025-SIGUE-OPCION.
               DISPLAY W-OPCION                     LINE  4 POSITION 37
           IF  W-OPCION                     =  "A"
               DISPLAY " A D I C I O N "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY "   CONSULTA    "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "M"
               DISPLAY " MODIFICACION  "            LINE  4 POSITION 41
                                                    REVERSE
           ELSE
           IF  W-OPCION                      =  "R"
               DISPLAY "  R E T I R O  "            LINE  4 POSITION 41
                                                    REVERSE.
               MOVE 8                      TO  I-LINEA
               MOVE 8                      TO  K-LINEA
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION  1.
       030-CODIGO-CATEGORIA.
           IF  W-OPCION                     =  "M"
           IF  I-LINEA                  NOT >  K-LINEA
             MOVE CODIGO-PANTALLA (I-LINEA) TO
                  RETENFTE-CODIGO-CATEGORIA.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE RETENFTE-CODIGO-CATEGORIA TO W-RETENFTE-CODIGO-CATEGORIA
                                               WI-Z6
           DISPLAY WI-Z6                   LINE I-LINEA POSITION  3

           ACCEPT W-RETENFTE-CODIGO-CATEGORIA
                  LINE I-LINEA POSITION  3
                      UPDATE  CONVERT TAB  NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.

           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-ESPACIOS LINE 24 POSITION  1
           IF  W-RETENFTE-CODIGO-CATEGORIA       =  ZEROS
            MOVE RETENFTE-CODIGO-CATEGORIA TO
                 W-RETENFTE-CODIGO-CATEGORIA.
           MOVE W-RETENFTE-CODIGO-CATEGORIA TO  WI-Z6
                RETENFTE-CODIGO-CATEGORIA
           DISPLAY WI-Z6                    LINE I-LINEA POSITION 3
           IF  CUP
           IF  W-OPCION                 NOT =  "M"
           OR  I-LINEA                      =  8
               GO TO 025-OPCION
           ELSE
               SUBTRACT 1                FROM  I-LINEA
               GO TO 030-CODIGO-CATEGORIA.
           IF  W-RETENFTE-CODIGO-CATEGORIA       =  ZEROS
               DISPLAY " CATEGORIA RTFTE. EN CEROS "
                  LINE 24 POSITION  1 REVERSE
               GO TO 030-CODIGO-CATEGORIA.
           MOVE ZEROS                  TO  SW1
           MOVE W-RETENFTE-CODIGO-CATEGORIA TO RETENFTE-CODIGO-CATEGORIA
           READ RETENFTE    WITH NO LOCK INVALID  KEY
               MOVE 2                      TO  SW1.
           IF  SW1                          =  ZEROS
           IF  W-OPCION                     =  "A"
               DISPLAY " CATEGORIA RTFTE  DUPLICADA "
                                                    LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-CATEGORIA
           ELSE
               GO TO 035-DISPLAY-DATOS.
           IF  W-OPCION                     =  "C"
               START RETENFTE KEY           >  RETENFTE-KEY00
                                      INVALID  KEY
               GO TO 115-FIN-CONSULTAR.
           IF  W-OPCION                     =  "C"
               MOVE 7                      TO  I-LINEA
               GO TO 115-CONSULTAR-SIGUIENTE.
               PERFORM 140-BORRAR-PANTALLA THRU
                       140-SALE-BORRAR-PANTALLA
               PERFORM 110-BORRAR
               MOVE 1                      TO  I
           IF  W-OPCION                 NOT =  "A"
               DISPLAY " CATEGORIA RTFTE. NO EXISTE "
                                                    LINE 24 POSITION  1
                                                    REVERSE
               GO TO 030-CODIGO-CATEGORIA.
           MOVE W-RETENFTE-CODIGO-CATEGORIA TO
                RETENFTE-CODIGO-CATEGORIA WI-Z6
           DISPLAY WI-Z6  LINE I-LINEA POSITION  3
               GO TO 040-VALOR-MINIMO.
       035-DISPLAY-DATOS.
           MOVE RETENFTE-CODIGO-CATEGORIA TO W-RETENFTE-CODIGO-CATEGORIA
           MOVE RETENFTE-PORCENTAJE       TO W-RETENFTE-PORCENTAJE
           MOVE RETENFTE-VALOR-MINIMO     TO W-RETENFTE-VALOR-MINIMO
           MOVE RETENFTE-VALOR-MAXIMO     TO W-RETENFTE-VALOR-MAXIMO
           MOVE RETENFTE-VALOR-APORTE     TO W-RETENFTE-VALOR-APORTE
           MOVE RETENFTE-PORC-INCAP       TO W-RETENFTE-PORC-INCAP
           MOVE RETENFTE-CODIGO-CATEGORIA TO WI-Z6
           DISPLAY WI-Z6                  LINE I-LINEA POSITION  3
               MOVE W-RETENFTE-VALOR-MINIMO     TO  WI-Z8P2
               DISPLAY WI-Z8P2            LINE I-LINEA POSITION 10
               MOVE W-RETENFTE-VALOR-MAXIMO     TO  WI-Z8P2
               DISPLAY WI-Z8P2            LINE I-LINEA POSITION 26
               MOVE W-RETENFTE-VALOR-APORTE     TO  WI-Z8P2
               DISPLAY WI-Z8P2            LINE I-LINEA POSITION 55
               MOVE W-RETENFTE-PORC-INCAP TO  WI-Z2P4
               DISPLAY WI-Z2P4            LINE I-LINEA POSITION 47.
       040-VALOR-MINIMO.
           IF  W-OPCION                     =  "R"
               GO TO 105-ELIMINAR.
           IF  W-OPCION                     =  "C"
               GO TO 115-CONSULTAR.

               MOVE ZEROS                  TO  WK-EXCEPTION
               MOVE W-RETENFTE-VALOR-MINIMO    TO  WI-Z8P2
               DISPLAY WI-Z8P2                      LINE I-LINEA
                                                    POSITION 10

            ACCEPT W-RETENFTE-VALOR-MINIMO    LINE I-LINEA POSITION 10
                                             UPDATE CONVERT TAB NO BEEP
                                             ON EXCEPTION WK-EX
                                             PERFORM 999-EXCEPTION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-RETENFTE-VALOR-MINIMO           =  ZEROS
               MOVE RETENFTE-VALOR-MINIMO TO  W-RETENFTE-VALOR-MINIMO.
           IF  DEL
               MOVE ZEROS                 TO  W-RETENFTE-VALOR-MINIMO.
               MOVE W-RETENFTE-VALOR-MINIMO TO  RETENFTE-VALOR-MINIMO
               MOVE W-RETENFTE-VALOR-MINIMO TO  WI-Z8P2
               DISPLAY WI-Z8P2              LINE I-LINEA POSITION 10
           IF  CUP
               GO TO 030-CODIGO-CATEGORIA.
       040-VALOR-MAXIMO.
           MOVE ZEROS                   TO  WK-EXCEPTION
           MOVE W-RETENFTE-VALOR-MAXIMO     TO  WI-Z8P2
           DISPLAY WI-Z8P2              LINE I-LINEA POSITION 26
           ACCEPT W-RETENFTE-VALOR-MAXIMO   LINE I-LINEA POSITION 26
                  UPDATE CONVERT TAB NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.

           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-RETENFTE-VALOR-MAXIMO     =  ZEROS
               MOVE RETENFTE-VALOR-MAXIMO  TO  W-RETENFTE-VALOR-MAXIMO.
           IF  DEL
               MOVE ZEROS                  TO  W-RETENFTE-VALOR-MAXIMO.
           MOVE W-RETENFTE-VALOR-MAXIMO    TO  RETENFTE-VALOR-MAXIMO
           MOVE W-RETENFTE-VALOR-MAXIMO    TO  WI-Z8P2
           DISPLAY WI-Z8P2                 LINE I-LINEA POSITION 26
           IF  CUP
               GO TO 040-VALOR-MINIMO.
       040-PORCENTAJE.
           MOVE ZEROS                      TO  WK-EXCEPTION
           MOVE W-RETENFTE-PORC-INCAP      TO  WI-Z2P4
           DISPLAY WI-Z2P4                 LINE I-LINEA POSITION 47
           ACCEPT W-RETENFTE-PORC-INCAP    LINE I-LINEA POSITION 47
                  UPDATE CONVERT TAB NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-RETENFTE-PORC-INCAP       =  ZEROS
               MOVE RETENFTE-PORC-INCAP    TO  W-RETENFTE-PORC-INCAP.
           IF  DEL
               MOVE ZEROS                  TO  W-RETENFTE-PORC-INCAP.
           MOVE W-RETENFTE-PORC-INCAP      TO  RETENFTE-PORC-INCAP
           MOVE W-RETENFTE-PORC-INCAP      TO  WI-Z2P4
           DISPLAY WI-Z2P4                 LINE I-LINEA POSITION 55.
           IF  CUP
               GO TO 040-VALOR-MAXIMO.
       040-VALOR-APORTE.
           MOVE ZEROS                      TO  WK-EXCEPTION
           MOVE W-RETENFTE-VALOR-APORTE    TO  WI-Z8P2
           DISPLAY WI-Z8P2                 LINE I-LINEA POSITION 55
           ACCEPT W-RETENFTE-VALOR-APORTE  LINE I-LINEA POSITION 55
                  UPDATE CONVERT TAB NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF  F2
               PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-RETENFTE-VALOR-APORTE           =  ZEROS
               MOVE RETENFTE-VALOR-APORTE  TO  W-RETENFTE-VALOR-APORTE.
           IF  DEL
               MOVE ZEROS                   TO  W-RETENFTE-VALOR-APORTE.
               MOVE W-RETENFTE-VALOR-APORTE TO  RETENFTE-VALOR-APORTE
               MOVE W-RETENFTE-VALOR-APORTE TO  WI-Z8P2
               DISPLAY WI-Z8P2              LINE I-LINEA POSITION 55
           IF  CUP
               GO TO 040-PORCENTAJE.
       040-ARMAR-REG.
           MOVE W-RETENFTE-CODIGO-CATEGORIA TO RETENFTE-CODIGO-CATEGORIA
           MOVE W-RETENFTE-PORCENTAJE       TO  RETENFTE-PORCENTAJE
           MOVE W-RETENFTE-PORC-INCAP       TO  RETENFTE-PORC-INCAP
           MOVE W-RETENFTE-VALOR-MINIMO     TO  RETENFTE-VALOR-MINIMO
           MOVE W-RETENFTE-VALOR-MAXIMO     TO  RETENFTE-VALOR-MAXIMO
           MOVE W-RETENFTE-VALOR-APORTE     TO  RETENFTE-VALOR-APORTE.
       080-GRABACION.
           IF  W-OPCION                     =  "M"
               GO TO 090-REGRABACION.
           DISPLAY " PRESIONE <A> ADICIONAR "   LINE 24 POSITION 1
                                                    REVERSE
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 27
                                                    REVERSE
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP
               GO TO 040-PORCENTAJE.
           IF  F1
               GO TO 025-OPCION.
           IF  W-TRUCO                  NOT =  "A"
               GO TO 080-GRABACION.
               WRITE REC-RETENFTE
               GO TO 090-FIN-ACTUALIZACION.
       090-REGRABACION.
           DISPLAY " PRESIONE <M> MODIFICAR "   LINE 24 POSITION 1
                                                    REVERSE
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 27
                                                    REVERSE
           DISPLAY " <> CONTINUAR SIN MODIFICAR "
                                                    LINE 24 POSITION 44
                                                    REVERSE
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  CUP
               GO TO 040-PORCENTAJE.
           IF  F1
               GO TO 025-OPCION.
           IF  CDN
               PERFORM 035-DISPLAY-DATOS
               GO TO 090-FIN-ACTUALIZACION.

           IF  W-TRUCO                  NOT =  "M"
               GO TO 090-REGRABACION.
               REWRITE REC-RETENFTE.
       090-FIN-ACTUALIZACION.
           PERFORM 140-BORRAR-PANTALLA    THRU 140-SALE-BORRAR-PANTALLA
           MOVE RETENFTE-CODIGO-CATEGORIA TO  CODIGO-PANTALLA (I-LINEA)
           PERFORM 015-CEROS
           DISPLAY WK-ESPACIOS            LINE 24 POSITION 1
           ADD 1                          TO  I-LINEA
           IF  I-LINEA                    >  K-LINEA
               MOVE I-LINEA               TO  K-LINEA.
           IF  I-LINEA                      =  20
               PERFORM 115-PANTALLA-CONSULTA.
           GO TO 030-CODIGO-CATEGORIA.
       105-ELIMINAR.
           DISPLAY " PRESIONE <R> RETIRAR "     LINE 24 POSITION 1
                                                    REVERSE
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
                                                    REVERSE
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1
               GO TO 025-OPCION.
           IF  W-TRUCO                  NOT =  "R"
               GO TO 105-ELIMINAR.
               DELETE RETENFTE
               GO TO 090-FIN-ACTUALIZACION.
       115-CONSULTAR.
           IF  I-LINEA                      <  19
               GO TO 115-CONSULTAR-SIGUIENTE.
           DISPLAY " PRESIONE <> CONSULTAR "   LINE 24 POSITION 1
                                                    REVERSE
           DISPLAY " <F1> OPCIONES "            LINE 24 POSITION 27
                                                    REVERSE
           PERFORM ACEPTA-TRUCO
           IF  CDN
               GO TO 115-CONSULTAR-SIGUIENTE.
           IF  F1
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               GO TO 025-OPCION
           ELSE
               GO TO 115-CONSULTAR.
       115-PANTALLA-CONSULTA.
           IF  W-OPCION                     =  "C"
               MOVE 7                      TO  I-LINEA
           ELSE
               MOVE 8                      TO  I-LINEA.
               MOVE 8                      TO  K-LINEA
               PERFORM 115-BORRAR
               VARYING J-LINEA           FROM  8 BY 1
               UNTIL J-LINEA                >  19.
       115-CONSULTAR-SIGUIENTE.
           IF  I-LINEA                      =  19
               GO TO 115-PANTALLA-CONSULTA.
               READ RETENFTE               NEXT  RECORD
                            WITH NO LOCK   AT  END
               GO TO 115-FIN-CONSULTAR.
           ADD 1                       TO  I-LINEA
           GO TO 035-DISPLAY-DATOS.
       115-FIN-CONSULTAR.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           DISPLAY " NO HAY MAS REGISTROS "     LINE 24 POSITION 1
                                                    REVERSE
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           GO TO 025-OPCION.
       115-BORRAR.
           MOVE ZEROS                  TO   CODIGO-PANTALLA (J-LINEA)
           DISPLAY W-SPACES            LINE J-LINEA POSITION 2.
       I-FIN-MODE.
           CLOSE RETENFTE.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

       110-BORRAR.
           DISPLAY W-SPACES            LINE I-LINEA POSITION 2
               DISPLAY WK-ESPACIOS     LINE 24 POSITION 1.
       140-BORRAR-PANTALLA.
           MOVE ZEROS                  TO  W-RETENFTE-PORCENTAJE
                                           W-RETENFTE-VALOR-MINIMO
                                           W-RETENFTE-VALOR-MAXIMO
                                           W-RETENFTE-VALOR-APORTE
                                           W-RETENFTE-PORC-INCAP.
       140-SALE-BORRAR-PANTALLA.
           EXIT.

           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\STA\RETENFTE.STA".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\PANTALLA.PRO".

