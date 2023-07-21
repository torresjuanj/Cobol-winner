      *****************************************************************
      * This application was written by Juan Jose Torres
      * and is protected under copyrigth and trademarks.
      * Licensed under MIT to be public at github for demostration 
      * purposes.
      * Winner Financial (r) - 1997-2003                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG001.
      ******************************************************************
      *            CREACION Y MANTENIMIENTO MENUS                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\NOMIMENU.DYC".
           COPY "..\DYC\LICENCIA.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\NOMIMENU.FD".
           COPY "..\FD\LICENCIA.FD".
       WORKING-STORAGE SECTION.
       77  SW1                          PIC 9           VALUE ZEROS.
       77  SW2                          PIC 9           VALUE ZEROS.
       77  SWE                          PIC 9           VALUE ZEROS.
       77  I-POS                        PIC 99          VALUE ZEROS.
       77  I-DATO                       PIC 99          VALUE ZEROS.
       77  J-DATO                       PIC 99          VALUE ZEROS.
       77  K-DATO                       PIC 99          VALUE ZEROS.
       77  W-IDENT                      PIC X(8)        VALUE SPACES.
       77  I-LINEA                      PIC 99          VALUE ZEROS.
       77  W-LINEA                      PIC 99          VALUE ZEROS.
       77  W-LIN                        PIC 99          VALUE ZEROS.
       77  W-TIPO                       PIC X           VALUE SPACES.
       77  W-DATO-AUX                   PIC X(40)       VALUE SPACES.
       77  W-MENU                       PIC X(12)       VALUE SPACES.
       77  W-GUARDA-PROGRAMA            PIC X(8)        VALUE SPACES.
       01  NOMIMENU-STATUS              PIC XX          VALUE SPACES.
           COPY "..\LBL\LICENCIA.LBL".
           COPY "..\WRK\EMPRESAS.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
       01  W-DATO-PROGRAMA.
           03 W-PROGRAMA                PIC X(8)        VALUE SPACES.
           03 FILLER                    PIC X(34)       VALUE SPACES.

       01  W-DATO-01.
           03 W-FILLER-1                PIC X           VALUE "�".
           03 W-DATO                    PIC X(40)       VALUE SPACES.
           03 W-DATO-R                  REDEFINES W-DATO.
              05 W-LETRA-DATO           PIC X
                                        OCCURS 40 TIMES.
           03 W-FILLER-2                PIC X           VALUE "�".

       01  W-DATO-01-ANT.
           03 W-FILLER-ANT-1            PIC X           VALUE "�".
           03 W-DATO-ANT                PIC X(40)       VALUE SPACES.
           03 W-DATO-ANT-R              REDEFINES W-DATO-ANT.
              05 W-LETRA-DATO-ANT       PIC X
                                        OCCURS 40 TIMES.
           03 W-FILLER-ANT-2            PIC X           VALUE "�".

       01  W-DATO-WINNER.
           03 FILLER                    PIC X           VALUE "�".
           03 FILLER                    PIC X(40)       VALUE SPACES.
           03 FILLER                    PIC X           VALUE "�".

       01  WK-RAYA.
           03 WK-RAYA-P                 PIC X.
           03 FILLER                    PIC X(40)       VALUE ALL "�".
           03 WK-RAYA-U                 PIC X.

       01  WK-RAYA-ARRIBA.
           03 FILLER                    PIC X           VALUE "�".
           03 FILLER                    PIC X(40)       VALUE ALL "�".
           03 FILLER                    PIC X           VALUE "�".

       01  WK-RAYA-ABAJO.
           03 FILLER                    PIC X           VALUE "�".
           03 FILLER                    PIC X(40)       VALUE ALL "�".
           03 FILLER                    PIC X           VALUE "�".

       01  WK-RAYAS.
           03 WK-RAYAS-P                PIC X           VALUE "�".
           03 WK-RAYAS-DATO             PIC X(40)       VALUE ALL "�".
           03 WK-RAYAS-U                PIC X           VALUE "�".

       01  WK-RAYAS-ARRIBA.
           03 FILLER                    PIC X           VALUE "�".
           03 FILLER                    PIC X(40)       VALUE ALL "�".
           03 FILLER                    PIC X           VALUE "�".

       01  WK-RAYAS-ABAJO.
           03 FILLER                    PIC X           VALUE "�".
           03 FILLER                    PIC X(40)       VALUE ALL "�".
           03 FILLER                    PIC X           VALUE "�".

       PROCEDURE DIVISION.
       DECLARATIVES.
                   COPY "..\DCL\NOMIMENU.DCL".
                   COPY "..\DCL\LICENCIA.DCL".
       END DECLARATIVES.
       INICIO-PROGRAMA SECTION.
       INICIAR.
               DISPLAY "NOMBRE MENU"                LINE  1 POSITION  1
                                                    ERASE
               DISPLAY "FORMATO = Nombre del formato de pantalla"
                                                    LINE 18 POSITION  1
               DISPLAY "TIPO    = D (Dato)   I (Instruccionaf8)"
                                                    LINE 19 POSITION  1
               DISPLAY "PASSWORD= X(8)  formato=blancos"
                                                    LINE 20 POSITION  1
               DISPLAY "F2-FIN  F3-FORMATO"         LINE 22 POSITION  1
           ACCEPT W-MENU                        LINE  1 POSITION 20
                  TAB
                  NO BEEP
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF  W-MENU                       =  SPACES
               GO TO INICIAR.
           IF AF8
               PERFORM I-RECIBE-LICENCIA THRU F-RECIBE-LICENCIA.
               DISPLAY "MANTENIMIENTO MENU 'CR' CREACION "
                               LINE  1 POSITION  1 ERASE
               ACCEPT W-IDENT  LINE  1 POSITION 36
                      TAB
                      NO BEEP
                      UPDATE
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  W-IDENT                      =  "CR"
               OPEN OUTPUT MENU
               CLOSE MENU.
               OPEN I-O MENU.
               PERFORM 900-CUADRO.
       ACEPTA-FORMATO.
               DISPLAY "FORMATO=        "  LINE 24 POSITION  3
               MOVE ZEROS                  TO  WK-EXCEPTION
                                               W-LIN
               ACCEPT W-IDENT                       LINE 24 POSITION 12
                      TAB
                      NO BEEP PROMPT
                      UPDATE
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               GO TO 130-FIN.
               MOVE W-IDENT                TO  ME-IDENT
               MOVE ZEROS                  TO  ME-LINEA
               MOVE SPACES                 TO  W-TIPO
               MOVE ZEROS                  TO  W-LINEA
               START MENU KEY               >  ME-CLAVE
                                      INVALID  KEY
               GO TO ACEPTA-TIPO.
               READ  MENU                      NEXT
                                           AT  END
               GO TO ACEPTA-TIPO.
           IF  W-IDENT                  NOT =  ME-IDENT
               GO TO ACEPTA-TIPO.
           IF  ME-TIPO                      =  "I"
               GO TO 110-RUTINA-TIPO-I.
               GO TO SIGUE-FORMATO.
       MUESTRA-FORMATO.
               READ MENU                       NEXT
                                           AT  END
               GO TO ACEPTA-LINEA.
       SIGUE-FORMATO.
           IF  ME-IDENT NOT                 =  W-IDENT
               GO TO ACEPTA-LINEA.
               DISPLAY ME-DATO             LINE ME-LINEA POSITION 20
               MOVE ME-TIPO                TO  W-TIPO
               GO TO MUESTRA-FORMATO.
       ACEPTA-TIPO.
               DISPLAY "TIPO=  "           LINE 24 POSITION 23
               MOVE ZEROS                  TO  WK-EXCEPTION.
               ACCEPT W-TIPO               LINE 24 POSITION 28
                      TAB
                      NO BEEP PROMPT
                      UPDATE
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2
               GO TO 130-FIN.
           IF  F3
               GO TO ACEPTA-FORMATO.
           IF  W-TIPO                       =  "D"
           OR  W-TIPO                       =  "I"
               NEXT SENTENCE
           ELSE
               DISPLAY " TIPO ERRADO * I=Instruccion D=Dato "
                                           LINE 25 POSITION 20
               GO TO ACEPTA-TIPO.
               DISPLAY WK-RAYA-ABAJO       LINE 25 POSITION 20
           IF  W-TIPO                       =  "I"
               GO TO 110-RUTINA-TIPO-I.
           IF  W-TIPO                       =  "D"
               GO TO 120-RUTINA-TIPO-D.
       ACEPTA-LINEA.
               DISPLAY "LINEA="            LINE 24 POSITION 31
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LINEA              LINE 24 POSITION 37
                      CONVERT
                      TAB
                      UPDATE PROMPT
                      NO BEEP
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  ERR
               GO TO ACEPTA-LINEA.
           IF  F2
               GO TO 130-FIN.
           IF  F3
               GO TO ACEPTA-FORMATO.
           IF  CDN
               GO TO PRIMERA-LINEA.
           IF  CUP
               GO TO ULTIMA-LINEA.
           IF  PGUP
               GO TO PRIMERA-LINEA.
           IF  PGDN
               GO TO ULTIMA-LINEA.
           IF  AF10
               GO TO BORRAR-MENU.
           IF  W-LINEA                      <  2
           OR  W-LINEA                      >  22
               GO TO ACEPTA-LINEA.
           IF  W-LINEA                      =  4
               GO TO ACEPTA-LINEA.
               GO TO BUSCAR-LINEA.
       BORRAR-MENU.
               MOVE W-IDENT                TO  ME-IDENT
               MOVE ZEROS                  TO  ME-LINEA
               MOVE SPACES                 TO  W-TIPO
               MOVE ZEROS                  TO  W-LINEA
               START MENU KEY               >  ME-CLAVE
                                      INVALID  KEY
               GO TO ACEPTA-FORMATO.
       SIGUE-BORRAR-MENU.
               READ  MENU                      NEXT
                                           AT  END
               GO TO ACEPTA-FORMATO.
           IF  ME-IDENT NOT                 =  W-IDENT
               GO TO ACEPTA-FORMATO.
               DELETE MENU            INVALID  KEY
               GO TO MUESTRA-FORMATO.
               GO TO SIGUE-BORRAR-MENU.
       BUSCAR-LINEAS.
               MOVE W-LIN                  TO  W-LINEA
                                               ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT.
       BUSCAR-LINEA.
               MOVE W-IDENT                TO  ME-IDENT
               MOVE W-LINEA                TO  ME-LINEA
               DISPLAY W-LINEA                      LINE 24 POSITION 37
               MOVE ZEROS                  TO  SW1
               READ MENU                       INVALID KEY
               MOVE 1                      TO  SW1.
           IF  SW1                          =  ZEROS
               DISPLAY ME-DATO                 LINE W-LINEA POSITION  20
               MOVE ME-DATO                TO  W-DATO-01
                                               W-DATO-01-ANT
               MOVE ME-LINEA               TO  W-LIN
                                               W-LINEA.
               GO TO 080-TOMA-DATO.
       ANTERIOR-LINEA.
               SUBTRACT 1                FROM  W-LIN
           IF  W-LIN                        =  4
               SUBTRACT 1 FROM W-LIN.
           IF  W-LIN                        <  3
               NEXT SENTENCE
           ELSE
               GO TO BUSCAR-LINEAS.
       ULTIMA-LINEA.
               MOVE 21                     TO  W-LIN
               GO TO BUSCAR-LINEAS.
       PROXIMA-LINEA.
               ADD 1                       TO  W-LIN
           IF  W-LIN                        =  4
               ADD 1                       TO  W-LIN.
           IF  W-LIN                        >  21
               NEXT SENTENCE
           ELSE
               GO TO BUSCAR-LINEAS.
       PRIMERA-LINEA.
               MOVE 3                      TO  W-LIN
               GO TO BUSCAR-LINEAS.
       080-TOMA-DATO.
               MOVE ZEROS                  TO  WK-EXCEPTION.
               MOVE W-DATO                 TO  W-DATO-AUX
                                               W-DATO-ANT
               PERFORM 800-ACEPTA-DATO THRU
                       800-SALE-ACEPTA-DATO
               DISPLAY W-DATO-01           LINE W-LINEA POSITION  20
           IF  F2
               GO TO 130-FIN.
           IF  F3
               GO TO ACEPTA-FORMATO.
               DISPLAY W-DATO              LINE W-LINEA POSITION 21
               MOVE W-IDENT                TO  ME-IDENT
               MOVE W-LINEA                TO  ME-LINEA
               MOVE W-TIPO                 TO  ME-TIPO
               MOVE W-DATO-01              TO  ME-DATO
           IF  W-DATO-01                    =  SPACES
               GO TO 100-ELIMINAR.
           IF  SW1                          =  ZEROS
               GO TO 090-REGRABAR.
               WRITE REC-MENU
                     INVALID KEY
               MOVE 1                      TO  SW1.
       085-CONTINUAR.
           IF  CDN
               GO TO PROXIMA-LINEA.
           IF  CUP
               GO TO ANTERIOR-LINEA.
           IF  PGUP
               GO TO PRIMERA-LINEA.
           IF  PGDN
               GO TO ULTIMA-LINEA.
           IF  NEW-LINE
               GO TO PROXIMA-LINEA.
               GO TO ACEPTA-LINEA.
       090-REGRABAR.
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1.
               GO TO 085-CONTINUAR.
       100-ELIMINAR.
               DELETE MENU            INVALID  KEY
               MOVE 1                      TO  SW1.
               GO TO 085-CONTINUAR.
       110-RUTINA-TIPO-I.
               DISPLAY "PROGRAMA="         LINE 24 POSITION 23
               MOVE W-IDENT                TO  ME-IDENT.
               MOVE ZEROS                  TO  ME-LINEA
               MOVE ZEROS                  TO  SW2.
               START MENU KEY               >  ME-CLAVE
                     INVALID KEY
               MOVE 1                      TO  SW2.
           IF  SW2 =  ZEROS
               READ MENU NEXT
                    AT END
               MOVE 1                      TO  SW2.
           IF  SW2                          =  ZEROS
           IF  W-IDENT                      =  ME-IDENT
               MOVE ME-DATO                TO  W-DATO-PROGRAMA
           ELSE
               MOVE 1                      TO  SW2
               MOVE SPACES                 TO  W-DATO-PROGRAMA.
               MOVE W-PROGRAMA             TO  W-GUARDA-PROGRAMA
               DISPLAY W-PROGRAMA          LINE 24 POSITION 32
               ACCEPT  W-PROGRAMA          LINE 24 POSITION 32
                       TAB
                       NO BEEP  PROMPT
                       UPDATE
                       ON EXCEPTION WK-EX
                       PERFORM 999-EXCEPTION.
               MOVE W-DATO-PROGRAMA        TO  ME-DATO
               MOVE "I"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE 01                     TO  ME-LINEA
           IF  W-PROGRAMA                   =  SPACES
               MOVE W-GUARDA-PROGRAMA      TO  W-PROGRAMA.
           IF  DEL
               MOVE SPACES                 TO  W-PROGRAMA.
           IF  W-PROGRAMA                   =  SPACES
           AND SW2                          =  ZEROS
               GO TO 115-BORRA-PROGRAMA.
           IF  SW2                          =  ZEROS
               GO TO 115-REGRABA-PROGRAMA.
           IF  SW2                          =  1
               WRITE REC-MENU         INVALID  KEY
               MOVE 1                      TO  SW1.
       110-CONTINUAR.
               DISPLAY "                 "  LINE 24 POSITION 23
               GO TO ACEPTA-FORMATO.
       115-BORRA-PROGRAMA.
               DELETE MENU            INVALID  KEY
               MOVE 1                      TO  SW1.
               GO TO 110-CONTINUAR.
       115-REGRABA-PROGRAMA.
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1.
               GO TO 110-CONTINUAR.
       120-RUTINA-TIPO-D.
               MOVE W-IDENT                TO  ME-IDENT.
               MOVE ZEROS                  TO  ME-LINEA
               MOVE ZEROS                  TO  SW2.
               START MENU KEY               >  ME-CLAVE
                     INVALID KEY
               MOVE 1                      TO  SW2.
           IF  SW2                          =  ZEROS
               READ MENU NEXT
                    AT END
               MOVE 1                      TO  SW2.
           IF  SW2 =  ZEROS
           IF  W-IDENT NOT                  =  ME-IDENT
               MOVE 1                      TO  SW2
          ELSE
               GO TO SIGUE-FORMATO.
           IF  SW2                          =  1
               MOVE WK-RAYAS-ARRIBA        TO  ME-DATO
               MOVE "D"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE 01                     TO  ME-LINEA
               WRITE REC-MENU
               MOVE W-DATO-WINNER          TO  ME-DATO
               MOVE "D"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE 02                     TO  ME-LINEA
               WRITE REC-MENU
               MOVE SPACES                 TO  W-DATO
               PERFORM 120-RUTINA-TIPO-GRABA-LINEAS
                       VARYING W-LIN     FROM  3  BY  1
                       UNTIL   W-LIN        >  21
               MOVE WK-RAYAS-ABAJO         TO  ME-DATO
               MOVE "D"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE 22                     TO  ME-LINEA
               WRITE REC-MENU
               MOVE WK-RAYAS               TO  ME-DATO
               MOVE "D"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE 04                     TO  ME-LINEA
               REWRITE REC-MENU.
               GO TO ACEPTA-LINEA.
       120-RUTINA-TIPO-GRABA-LINEAS.
               MOVE W-DATO-01              TO  ME-DATO
               MOVE "D"                    TO  ME-TIPO
               MOVE W-IDENT                TO  ME-IDENT
               MOVE W-LIN                  TO  ME-LINEA
               WRITE REC-MENU.
       130-FIN.
               CLOSE MENU.
       135-FIN-FIN.
               EXIT    PROGRAM.
       135-FT. STOP RUN.
       800-ACEPTA-DATO.
               ACCEPT  W-DATO  LINE W-LINEA POSITION 21
                      TAB
                      NO BEEP
                      UPDATE
                      ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  SF1
               PERFORM 800-INSERTAR-LINEA
           ELSE
           IF  SF2
               PERFORM 800-BORRAR-LINEA.
           MOVE "�"                    TO  W-FILLER-1
           MOVE "�"                    TO  W-FILLER-2
           DISPLAY W-DATO-01           LINE W-LINEA POSITION  20.
       800-SALE-ACEPTA-DATO.
               EXIT.
       800-BORRAR-LINEA.
               COMPUTE I-LINEA              =  W-LINEA + 1
               PERFORM 800-BORRAR-LINEA-1
                       UNTIL   I-LINEA      =  21
               MOVE I-LINEA                TO  ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT
               MOVE SPACES                 TO  W-DATO
               MOVE W-DATO-01              TO  ME-DATO
               DISPLAY ME-DATO             LINE ME-LINEA POSITION  20
           IF  SW1                          =  ZEROS
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1
           ELSE
               WRITE REC-MENU         INVALID  KEY
               MOVE 1                      TO  SW1.
               MOVE W-LINEA                TO  ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT
               MOVE ZEROS                  TO  SW1
               READ MENU              INVALID  KEY
               MOVE 1                      TO  SW1.
               MOVE ME-DATO                TO  W-DATO-01.
       800-BORRAR-LINEA-1.
               MOVE W-IDENT                TO  ME-IDENT
               MOVE I-LINEA                TO  ME-LINEA
               MOVE ZEROS                  TO  SW1
               READ MENU              INVALID  KEY
               MOVE 1                      TO  SW1.
               SUBTRACT 1                FROM  ME-LINEA
           IF  ME-LINEA                     =  4
               SUBTRACT 1                FROM  ME-LINEA.
           IF  ME-LINEA                 NOT =  4
           IF  SW1                          =  ZEROS
               DISPLAY ME-DATO          LINE ME-LINEA POSITION  20
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1.
               ADD  1                      TO  I-LINEA.
       800-SALE-BORRAR-LINEA.
               EXIT.
       800-INSERTAR-LINEA.
               MOVE W-LINEA                TO  ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT
               MOVE W-DATO-01              TO  ME-DATO
               DISPLAY ME-DATO             LINE ME-LINEA POSITION  20
           IF  SW1                          =  ZEROS
               REWRITE REC-MENU
                       INVALID KEY
               MOVE 1                      TO  SW1
           ELSE
               WRITE REC-MENU
                     INVALID KEY
               MOVE 1                      TO  SW1.
               MOVE 20                     TO  I-LINEA
               PERFORM 800-INSERTAR-LINEA-1
                       UNTIL   I-LINEA      <  W-LINEA
               MOVE W-LINEA                TO  ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT
               MOVE ZEROS                  TO  SW1
               READ MENU              INVALID  KEY
               MOVE 1                      TO  SW1.
               MOVE W-LINEA                TO  ME-LINEA
               MOVE W-IDENT                TO  ME-IDENT
               MOVE SPACES                 TO  W-DATO
               MOVE W-DATO-01              TO  ME-DATO
               DISPLAY ME-DATO             LINE ME-LINEA POSITION  20
           IF  SW1                          =  ZEROS
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1
           ELSE
               WRITE REC-MENU         INVALID  KEY
               MOVE 1                      TO  SW1.
       800-INSERTAR-LINEA-1.
               MOVE W-IDENT                TO  ME-IDENT
               MOVE I-LINEA                TO  ME-LINEA
               MOVE ZEROS                  TO  SW1
               READ MENU              INVALID  KEY
               MOVE 1                      TO  SW1.
               ADD  1                      TO  ME-LINEA
           IF  ME-LINEA                     =  4
               ADD  1                      TO  ME-LINEA.
           IF  I-LINEA                  NOT =  4
           IF  SW1                          =  ZEROS
               DISPLAY ME-DATO          LINE ME-LINEA POSITION  20
               REWRITE REC-MENU       INVALID  KEY
               MOVE 1                      TO  SW1.
               SUBTRACT 1                FROM  I-LINEA.
       800-SALE-INSERTAR-LINEA.
               EXIT.
       900-CUADRO.
               DISPLAY WK-RAYAS-ARRIBA  LINE  1 POSITION  20
                                        ERASE
               PERFORM 900-CUADRO-1
                       VARYING W-LIN     FROM  2  BY  1
                       UNTIL W-LIN          >  21.
               DISPLAY WK-RAYAS-ABAJO       LINE 22 POSITION  20
               DISPLAY WK-RAYAS             LINE  4 POSITION  20
               DISPLAY WK-RAYA-ARRIBA       LINE 23 POSITION  20
               DISPLAY "�"                  LINE 24 POSITION  20
               DISPLAY "�"                  LINE 24 POSITION 61
               DISPLAY WK-RAYA-ABAJO        LINE 25 POSITION  20
               DISPLAY " F2 "               LINE 24 POSITION 44
                                                            REVERSE
               DISPLAY "FIN "               LINE 24 POSITION 49
               DISPLAY " F3 "               LINE 24 POSITION 55
                                                            REVERSE
               DISPLAY "FORMATO"            LINE 24 POSITION 61.
       900-CUADRO-1.
               DISPLAY "�"                  LINE W-LIN POSITION 20.
               DISPLAY "�"                  LINE W-LIN POSITION 61.
       999-EXCEPTION.
               MOVE WK-EX                  TO  WK-EX1.

       I-RECIBE-LICENCIA.
               OPEN OUTPUT LICENCIA
               CLOSE LICENCIA.
               OPEN I-O LICENCIA.

           ACCEPT KEY-LICENCIA                  LINE  1 POSITION 50
                  TAB
                  NO BEEP OFF
                  ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           WRITE REC-LICENCIA.
           CLOSE LICENCIA.
       F-RECIBE-LICENCIA.
