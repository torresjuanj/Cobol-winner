       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG660.
      ******************************************************************
      *  CONTROL DE LOS CHEQUES      ABR-27-2002                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CHCONTRO.DYC".
               COPY "..\DYC\AUDICHEQ.DYC".
               COPY "..\DYC\NOMIBANK.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\CONTREXT.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CHCONTRO.FD".
               COPY "..\FD\AUDICHEQ.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CONTREXT.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)      VALUE "PROG660".
       77      ID-PRG              PIC X(7)      VALUE "PROG660".
       77      W-VARIABLES-NOMINA  PIC X(7)      VALUE SPACES.
       77      EGRE-INICIAL        PIC 9(4)      VALUE ZEROS.
       77      EGRE-FINAL          PIC 9(4)      VALUE ZEROS.
       77      WI-Z99              PIC ZZZZZ,ZZZ,ZZZ.ZZ VALUE ZEROS.
       77      WI-Z91              PIC ZZZZ,ZZZ,ZZZ     VALUE ZEROS.
       77      X2                  PIC 9(1)             VALUE ZEROS.
       77      KK                  PIC 9(1)             VALUE ZEROS.
       77      TAB-CHEQUE          PIC 9(04) VALUE ZEROS.
       77      TAB-CONSEC          PIC 9(04) VALUE ZEROS.
       77      WRK-VALOR1          PIC Z(12).99 VALUE ZEROS.
       77      WRK-VALOR2          PIC Z(12).99 VALUE ZEROS.
       77      TAB-ESTADO          PIC X(12) VALUE SPACES.
       77      TAB-DPTO            PIC X(12) VALUE SPACES.
       77      WK-ESP              PIC X(80) VALUE SPACES.

       01  W-TABLA-CHEQUES.
        03  TABLA-LIBRO-AUXILIAR OCCURS 18 TIMES.
         05 T-CONSEC       PIC X(07).
         05 T-FEC-ELABORO  PIC X(8).
         05 T-EGRESO       PIC X(7).
         05 T-EMPRESA      PIC X(7).
         05 T-NOM-EMPRESA  PIC X(20).
         05 T-GIRADO       PIC X(19).
         05 T-DESDE        PIC 9999.
         05 T-HASTA        PIC 9999.
         05 T-VALOR        PIC X(11).
         05 T-ESTADO       PIC 9.
         05 T-FEC-ENTREGA  PIC 9(08).
         05 T-ENTREGO      PIC X(20).
         05 T-RECIBIO      PIC X(20).
         05 T-DPTO         PIC X.
         05 T-LUGAR        PIC X.

               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CHCONTRO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\HOJAVIDA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\NOMIBANK.LBL".
               COPY "..\LBL\AUDICHEQ.LBL".

               COPY "..\LBL\CONTREXT.LBL".

               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\COMANDOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\CHCONTRO.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
       01      AREA-INFORME.
        03 LDET1.
           05 FILLER         PIC X(01)     VALUE "!".
           05 I-CONSEC       PIC ZZZ,Z99.
           05 FILLER         PIC X(01)     VALUE " ".
           05 I-FEC-ELABORO  PIC ZZZZZZZZ.
           05 FILLER         PIC X(01)     VALUE " ".
           05 I-EGRESO       PIC ZZZ,ZZZ.
           05 FILLER         PIC X(03)     VALUE SPACES.
           05 I-EMPRESA      PIC Z(07).
           05 FILLER         PIC X(01)     VALUE "-".
           05 I-NOM-EMPRESA  PIC X(29).
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-GIRADO       PIC X(30).
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-DESDE        PIC 9999.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-HASTA        PIC 9999.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-VALOR        PIC ZZZ,ZZZ,ZZ9.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-ESTADO       PIC 9.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-FEC-ENTREGA  PIC ZZZZBZZBZZ.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-ENTREGO      PIC X(29).
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-RECIBIO      PIC X(29).
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-DPTO         PIC X.
           05 FILLER         PIC X(01)     VALUE SPACES.
           05 I-LUGAR        PIC X.
           05 FILLER         PIC X(01)     VALUE SPACES.
      *
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CHCONTRO.DCL".
               COPY "..\DCL\NOMIBANK.DCL".
               COPY "..\DCL\HOJAVIDA.DCL".
               COPY "..\DCL\AUDICAMP.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\CONTREXT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS         THRU F-USUARIOS.
           IF      SW-USER = 1        EXIT PROGRAM.
           PERFORM I-PANTALLA-1B      THRU F-PANTALLA-1B
           PERFORM I-DATOS-LABEL      THRU F-DATOS-LABEL.
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-STATUS-CHCONTRO  THRU F-STATUS-CHCONTRO.
           PERFORM I-STATUS-CONTREXT  THRU F-STATUS-CONTREXT.
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           PERFORM I-MENU-OPCIONES    THRU F-MENU-OPCIONES  UNTIL ESC.
           PERFORM I-FIN-MODE         THRU F-FIN-MODE.
       F-MAIN-PROCESS.

       I-LABEL-ARCHIVOS.
               MOVE "CONTROL DE CHEQUES"    TO  TIT-APLICACION
               MOVE WIN-EMPRESA             TO  WK-COMPANIA
               MOVE "π"                     TO  WK-RAYAS-U
               MOVE "Ã"                     TO  WK-RAYAS-P
               MOVE "∂"                     TO  WK-RAYA-U
               MOVE "«"                     TO  WK-RAYA-P.
               MOVE 16                      TO  POS-DOWN.
               PERFORM I-LABEL-CHCONTRO   THRU  F-LABEL-CHCONTRO.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN I-O   CHCONTRO.
               OPEN INPUT HOJAVIDA CNNITCED EMPRESAS.
       F-ABRE-ARCHIVOS.
      *
       I-FIN-MODE.
           CLOSE CHCONTRO
                 HOJAVIDA
                 EMPRESAS
                 CNNITCED.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       MENU OPCIONES                                     ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999                 TO WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE SG-COLOR-TABLE(8)   TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2)   TO SG-BCOLOR.
           MOVE ZEROS               TO FIN-OK
           PERFORM I-SELECCIONA   THRU F-SELECCIONA
                                 UNTIL SALIDA OR ESC.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 999                TO WK-EXCEPTION
              PERFORM I-WINDOW-FIN  THRU F-WINDOW-FIN
              PERFORM I-SALIR       THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM I-FIN-MODE THRU F-FIN-MODE
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
           IF  PROSS-XKK = "N"
               NEXT SENTENCE
           ELSE
           MOVE ZEROS TO SW-ROMPE SW-EMPR WK-EMP-ROMPE.
           IF  W-OPCION  =  "A" AND PROSS-AKK = "S"
               PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
               PERFORM I-ADICIONA     THRU F-ADICIONA
                                           UNTIL ESC
               MOVE ZEROS               TO WK-EXCEPTION
           ELSE
             IF  W-OPCION  =  "C" AND PROSS-CKK = "S"
                 PERFORM I-CONSU            THRU F-CONSU
                 PERFORM I-CONSULTA         THRU F-CONSULTA
                                                 UNTIL ESC
                 MOVE ZEROS                   TO WK-EXCEPTION
             ELSE
               IF  W-OPCION  =  "M" AND  PROSS-MKK = "S"
                   PERFORM I-OPCION-MODIFICA THRU F-OPCION-MODIFICA
                   PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
                   PERFORM I-MODIFICA        THRU F-MODIFICA
                                                  UNTIL ESC
                   MOVE ZEROS                  TO WK-EXCEPTION
               ELSE
                 IF  W-OPCION  =  "R" AND PROSS-RKK = "S"
                      PERFORM I-WINDOW-DATOS  THRU F-WINDOW-DATOS
                      PERFORM I-RETIRA        THRU F-RETIRA UNTIL ESC
                      MOVE    ZEROS             TO WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-CONSU.
            PERFORM I-OPCION-CONSULTA  THRU F-OPCION-CONSULTA
            IF      ESC                  GO F-CONSU.
            PERFORM I-OPCION-CONSULTA2 THRU F-OPCION-CONSULTA2
            IF      ESC                  GO F-CONSU.
            PERFORM I-OPCION-TIPO      THRU F-OPCION-TIPO.
            IF      ESC                  GO F-CONSU.
            PERFORM I-WINDOW-DATOS     THRU F-WINDOW-DATOS.
       F-CONSU.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       I-ADICIONA                                        ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-ADICIONA.
           MOVE  1                     TO X1.
           MOVE SPACES                 TO REG-CHCONTRO.
           PERFORM I-CHEQUE          THRU F-CHEQUE.
           IF      ESC                 GO F-ADICIONA.
           IF UU = 1
                MOVE SPACES            TO REG-CHCONTRO.
           MOVE   TAB-CHEQUE           TO CHEQUE-DESDE-CHCONTRO.
           IF UU = 2  AND W-OPCION = "A"
              PERFORM I-CHEQUEO THRU F-CHEQUEO.
           IF UU = 2  AND W-OPCION = "A"  GO I-ADICIONA.
           MOVE SPACES                    TO REG-CHCONTRO.
           MOVE   TAB-CHEQUE              TO CHEQUE-DESDE-CHCONTRO.
           PERFORM I-DATOS-CAB          THRU F-DATOS-CAB.
           IF      CUP
                PERFORM I-WINDOW-DATOS THRU F-WINDOW-DATOS
                GO I-ADICIONA.
           IF      ESC OR  CUP         GO F-ADICIONA.
           IF W-TRUCO = "G"
              PERFORM I-GRABACION    THRU F-GRABACION.
           PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS.
       F-ADICIONA.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       I-CONSULTA                                        ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-CONSULTA.
           PERFORM I-DISPLAY-OPCION   THRU F-DISPLAY-OPCION.
           MOVE    01                   TO SW-OPCION.
           PERFORM I-PANTALLA-GEN     THRU F-PANTALLA-GEN.
           IF   ANT1 = 1
                PERFORM I-DESDE-C     THRU F-DESDE-C
           ELSE
                PERFORM I-NOMBRE-C    THRU F-NOMBRE-C.
           IF   ESC                  GO TO F-CONSULTA.
           IF   ANT1 = 1
                PERFORM I-START-ARCHIVO    THRU F-START-ARCHIVO
           ELSE
                PERFORM I-START-ARCHIVO-N  THRU F-START-ARCHIVO-N.
           PERFORM I-CEROS-VARIABLES  THRU F-CEROS-VARIABLES
           PERFORM I-CONSULTAR        THRU F-CONSULTAR
                                             UNTIL FIN-OK = 1 OR
                                             ESC OR F1.
           IF F1
              MOVE 01   TO SW-LECTURA
              GO TO I-CONSULTA.
      *    IF ESC OR FIN-OK = 1        GO TO F-CONSULTA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-MUESTRA-TABLA    THRU F-MUESTRA-TABLA
                                     UNTIL CDN OR ESC OR F1.
           IF ESC OR FIN-OK = 1        GO TO F-CONSULTA.

           IF ESC GO TO F-CONSULTA.
           MOVE 01   TO SW-LECTURA.
       F-CONSULTA.
      *
       I-CONSULTAR.
           DISPLAY SPACES LINE 25 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS MAYORES ..." LINE 25 POSITION 01.
           PERFORM I-LEE-CHCONTRO    THRU F-LEE-CHCONTRO
           IF      FIN-OK = 1       GO TO F-CONSULTAR.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-CONSULTAR. EXIT.
      *
       I-CEROS-VARIABLES.
           MOVE 0  TO FIN-OK
           MOVE 06 TO CON-LIN.
           MOVE 01 TO SW-LECTURA.
       F-CEROS-VARIABLES. EXIT.
      *
       I-MUEVE-INFORME-1.
           DISPLAY SPACES LINE 25 POSITION 1 SIZE 80.
           MOVE CONSEC-CHCONTRO         TO   I-CONSEC
           MOVE FEC-ELABORO-CHCONTRO    TO   I-FEC-ELABORO
           MOVE EGRESO-CHCONTRO         TO   I-EGRESO
           MOVE EMPRESA-CHCONTRO        TO   I-EMPRESA
                                             WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS     THRU   F-LEE-EMPRESAS.
           MOVE WIN-EMPRESA             TO   I-NOM-EMPRESA

           MOVE NOMBRE-CHCONTRO       TO I-GIRADO.

           MOVE ENTREGO-CHCONTRO      TO HOJAVIDA-CEDULA CODIM
           PERFORM I-LEE-HOJAVIDA   THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              MOVE HOJAVIDA-NOMBRE       TO I-ENTREGO
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              MOVE NOMBM TO I-ENTREGO.

           MOVE RECIBIO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              MOVE HOJAVIDA-NOMBRE     TO I-RECIBIO
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              MOVE NOMBM TO I-RECIBIO.

           MOVE CHEQUE-DESDE-CHCONTRO   TO   I-DESDE
           MOVE CHEQUE-HASTA-CHCONTRO   TO   I-HASTA
           MOVE VALOR-CHCONTRO          TO   I-VALOR
           MOVE ESTADO-CHCONTRO         TO   I-ESTADO
           MOVE FEC-ENTREGA-CHCONTRO    TO   I-FEC-ENTREGA
           MOVE DPTO-CHCONTRO           TO   I-DPTO
           MOVE LUGAR-CHCONTRO          TO   I-LUGAR
           PERFORM I-EVALUE-IMPRESION THRU   F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1. EXIT.
      *
       I-EVALUE-IMPRESION.
           IF SW-LECTURA > 18
             MOVE ZEROS TO WK-EXCEPTION
             PERFORM I-MUESTRA-TABLA THRU F-MUESTRA-TABLA
             UNTIL CDN OR ESC OR F1
             PERFORM I-LIMPIA-TABLA THRU F-LIMPIA-TABLA
              VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18
             MOVE 01 TO SW-LECTURA
             IF ESC GO TO F-EVALUE-IMPRESION END-IF
            END-IF
           MOVE I-CONSEC         TO   T-CONSEC      (SW-LECTURA)
           MOVE I-FEC-ELABORO    TO   T-FEC-ELABORO (SW-LECTURA)
           MOVE I-EGRESO         TO   T-EGRESO      (SW-LECTURA)
           MOVE I-EMPRESA        TO   T-EMPRESA     (SW-LECTURA)
           MOVE I-NOM-EMPRESA    TO   T-NOM-EMPRESA (SW-LECTURA)
           MOVE I-GIRADO         TO   T-GIRADO      (SW-LECTURA)
           MOVE I-DESDE          TO   T-DESDE       (SW-LECTURA)
           MOVE I-HASTA          TO   T-HASTA       (SW-LECTURA)
           MOVE I-VALOR          TO   T-VALOR       (SW-LECTURA)
           MOVE I-ESTADO         TO   T-ESTADO      (SW-LECTURA)
           MOVE I-FEC-ENTREGA    TO   T-FEC-ENTREGA (SW-LECTURA)
           MOVE I-ENTREGO        TO   T-ENTREGO     (SW-LECTURA)
           MOVE I-RECIBIO        TO   T-RECIBIO     (SW-LECTURA)
           MOVE I-DPTO           TO   T-DPTO        (SW-LECTURA)
           MOVE I-LUGAR          TO   T-LUGAR       (SW-LECTURA)
           ADD 1 TO SW-LECTURA.
       F-EVALUE-IMPRESION.
      *
       I-LIMPIA-TABLA.
           MOVE SPACES TO TABLA-LIBRO-AUXILIAR (SW-LECTURA).
       F-LIMPIA-TABLA. EXIT.
      *
       I-MUESTRA-TABLA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-MUESTRA-TABLAS THRU F-MUESTRA-TABLAS.
         RETORNO-MUESTRA.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF SW-OPCION = 01
            DISPLAY
            "[CONTROL][->] Derecha [] Abajo [ESC] Regresa [F2] Salir"
             LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO UNTIL CRIGHT OR CDN OR F2 OR ESC OR F1
           ELSE
            IF SW-OPCION > 01 AND SW-OPCION < 03
             DISPLAY
             "[CONTROL][->] Derecha [] Abajo [CONTROL][<-] Izquierda [E
      -    "SC] Regresa [F2] Salir"
             LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO UNTIL CLEFT OR CDN OR CRIGHT OR F2
                        OR F1 OR ESC
            ELSE
             DISPLAY
            "[] Abajo [CONTROL][<-] Izquierda [ESC] Regresa [F2] Salir"
             LINE 25 POSITION 01
             PERFORM ACEPTA-TRUCO UNTIL CLEFT OR CDN OR F2
                                        OR ESC OR ESC.
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF F1  GO TO F-MUESTRA-TABLA.
           IF CDN GO TO F-MUESTRA-TABLA.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF ESC GO TO F-MUESTRA-TABLA.
           IF CRIGHT ADD  01 TO SW-OPCION END-IF
           IF CLEFT COMPUTE SW-OPCION = SW-OPCION - 01 END-IF
           PERFORM I-MUESTRA-TABLAS THRU F-MUESTRA-TABLAS.
           GO TO RETORNO-MUESTRA.
       F-MUESTRA-TABLA. EXIT.
      *
       I-MUESTRA-TABLAS.
           IF SW-OPCION = 01
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING I FROM 06 BY 1 UNTIL I > 23
            MOVE 6 TO CON-LIN
            PERFORM I-PRIMER-PANTALLA THRU F-PRIMER-PANTALLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18.
           IF SW-OPCION = 02
            PERFORM I-LIMPIA-PANTALLA THRU F-LIMPIA-PANTALLA
              VARYING I FROM 06 BY 1 UNTIL I > 23
            MOVE 6 TO CON-LIN
            PERFORM I-SEGUND-PANTALLA THRU F-SEGUND-PANTALLA
             VARYING SW-LECTURA FROM 1 BY 1 UNTIL SW-LECTURA > 18.
       F-MUESTRA-TABLAS. EXIT.

      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       I-MODIFICA                                        ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-MODIFICA.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80
           PERFORM I-DISPLAY-OPCION        THRU F-DISPLAY-OPCION
           PERFORM I-CHEQUE                THRU F-CHEQUE.
           IF      ESC                       GO F-MODIFICA.
           PERFORM I-MOSTRAR-CAB           THRU F-MOSTRAR-CAB
           PERFORM I-GUARDA-INAC-VACACION  THRU F-GUARDA-INAC-VACACION.
           PERFORM I-DATOS-CAB             THRU F-DATOS-CAB.
           IF      CUP
                   PERFORM I-WINDOW-DATOS  THRU F-WINDOW-DATOS
                   MOVE SPACES               TO REG-CHCONTRO
                   GO   I-MODIFICA.
           IF      ESC                       GO F-MODIFICA.
           IF W-TRUCO = "G"
              PERFORM I-REGRABACION        THRU F-REGRABACION.
           PERFORM I-WINDOW-DATOS          THRU F-WINDOW-DATOS.
           MOVE SPACES                       TO REG-CHCONTRO.
       F-MODIFICA.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       I-RETIRA                                          ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-RETIRA.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80
           PERFORM I-DISPLAY-OPCION        THRU F-DISPLAY-OPCION
           PERFORM I-CHEQUE                THRU F-CHEQUE.
           IF      ESC                       GO F-RETIRA.
           PERFORM I-MOSTRAR-CAB           THRU F-MOSTRAR-CAB
           PERFORM I-GUARDA-INAC-VACACION  THRU F-GUARDA-INAC-VACACION.
           PERFORM I-ELIMINAR              THRU F-ELIMINAR
           PERFORM I-WINDOW-DATOS          THRU F-WINDOW-DATOS.
           MOVE ZEROS                        TO MM.
       F-RETIRA.
      *
       I-ELIMINAR.
           DISPLAY " PRESIONE <R> RETIRAR "       LINE 25 POSITION 1
           DISPLAY " <F1> CANCELAR  <ESC> SALIR"  LINE 25 POSITION 25
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESP                       LINE 25 POSITION 1
           IF   ESC OR F1   GO F-ELIMINAR.
           IF   F2    PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "R"  GO I-ELIMINAR.
           MOVE      "R"                   TO AUDICAMP-OPCION.
           PERFORM   I-AUDITORIA-BORRA   THRU F-AUDITORIA-BORRA.
           DELETE CHCONTRO.
       F-ELIMINAR.
      *
       I-ELIMINA-GRUPO.
           MOVE   10    TO  I.
           PERFORM I-CHEQUE-INICIAL  THRU F-CHEQUE-INICIAL.
           IF ESC   GO I-ELIMINA-1.
       I-ELI-2.
           DISPLAY " PRESIONE <R> RETIRAR "     LINE 25 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 25 POSITION 25
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESP                  LINE 25 POSITION 1
           IF   F1    GO I-ELIMINA-1.
           IF   F2    PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "R"  GO I-ELI-2.
           IF   CUP   GO I-ELIMINA-GRUPO.
           MOVE  ZEROS    TO SW-EOF.
           MOVE EGRE-INICIAL         TO  CHEQUE-DESDE-CHCONTRO.
           START CHCONTRO  KEY NOT LESS  KEY01-CHCONTRO INVALID KEY
                 DISPLAY "ERROR START CHCONTRO"  LINE 23 POSITION 1
                 GO I-ELIMINA-1.
           PERFORM I-LECTURA-SEC  THRU  F-LECTURA-SEC.
        I-ELIMINA-1.
           CLOSE CHCONTRO
           OPEN I-O   CHCONTRO.
           PERFORM I-WINDOW-DATOS   THRU F-WINDOW-DATOS.
       F-ELIMINA-GRUPO.
      *
       I-CHEQUE-INICIAL.
           DISPLAY "CHEQUE INICIAL : " LINE 15 POSITION 25.
           DISPLAY "CHEQUE FINAL   : " LINE 16 POSITION 25.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-INICIAL LINE 15 POSITION 45
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-CHEQUE-FINAL.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-INICIAL LINE 15 POSITION 45  REVERSE
           IF  EGRE-INICIAL = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 25 POSITION 1
                  REVERSE BEEP GO I-CHEQUE-INICIAL.
           DISPLAY WK-ESP LINE 25 POSITION 1.
       I-CHEQUE-FINAL.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-FINAL   LINE 16 POSITION 45
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-CHEQUE-INICIAL.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-FINAL   LINE 16 POSITION 45  REVERSE
           IF  EGRE-FINAL   = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 25 POSITION 1
                  REVERSE BEEP GO I-CHEQUE-FINAL.
       F-CHEQUE-INICIAL.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-RUTINAS ANEXAS          ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-MOSTRAR-CAB.
           PERFORM I-COLOR1      THRU       F-COLOR1.
           DISPLAY CONSEC-CHCONTRO          LINE 7  POSITION 23 LOW.
           DISPLAY CHEQUE-DESDE-CHCONTRO    LINE 9  POSITION 23 LOW.
           DISPLAY CHEQUE-HASTA-CHCONTRO    LINE 9  POSITION 57 LOW.
           MOVE    VALOR-CHCONTRO             TO WI-Z99.
           DISPLAY WI-Z99                   LINE 10 POSITION 23 LOW.
           DISPLAY EGRESO-CHCONTRO          LINE 10 POSITION 57 LOW.
           MOVE    EMPRESA-CHCONTRO      TO WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS   THRU F-LEE-EMPRESAS
           DISPLAY EMPRESA-CHCONTRO LINE 11 POSITION 23 LOW
           DISPLAY WIN-EMPRESA      LINE 11 POSITION 30 LOW SIZE 30.
           DISPLAY WIN-EMPRESA      LINE 25 POSITION 01 LOW SIZE 30.
           DISPLAY WIN-DIRECCION    LINE 25 POSITION 35 LOW
           MOVE    GIRADO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM WI-Z91
           DISPLAY WI-Z91              LINE 12 POSITION 23 LOW
           PERFORM I-LEE-HOJAVIDA  THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 12 POSITION 40 LOW SIZE 30
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              DISPLAY NOMBM          LINE 12 POSITION 40 LOW SIZE 30.

           DISPLAY DPTO-CHCONTRO     LINE 13 POSITION 57 LOW
           DISPLAY LUGAR-CHCONTRO    LINE 07 POSITION 55 LOW
           IF DPTO-CHCONTRO  = "N"  MOVE "NOMINAS"      TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "T"  MOVE "CONTABILIDAD" TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "S"  MOVE "OTROS PAGOS"  TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "L"  MOVE "LIQUIDACION"  TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "P"  MOVE "SUBSIDIO"     TO TAB-DPTO.
           DISPLAY TAB-DPTO  LINE 13 POSITION 59 LOW.
           DISPLAY FEC-ELABORO-CHCONTRO    LINE 14 POSITION 57 LOW
           IF X1 = 2
               PERFORM I-COLOR2      THRU       F-COLOR2.
           DISPLAY ESTADO-CHCONTRO   LINE 13 POSITION 23 LOW
           IF ESTADO-CHCONTRO  = "0"  MOVE "EN CAJA"      TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "6"  MOVE "ENVIADO EMP." TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "7"  MOVE "ENTREGADO"    TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "9"  MOVE "ANULADO"      TO TAB-ESTADO.
           DISPLAY TAB-ESTADO LINE 13  POSITION 25 LOW.

           DISPLAY FEC-ENTREGA-CHCONTRO    LINE 14 POSITION 23 LOW
           MOVE    ENTREGO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM WI-Z91
           DISPLAY WI-Z91              LINE 15 POSITION 23 LOW
           PERFORM I-LEE-HOJAVIDA  THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 15 POSITION 40 LOW SIZE 30
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              DISPLAY NOMBM          LINE 15 POSITION 40 LOW SIZE 30.

           MOVE RECIBIO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM WI-Z91
           DISPLAY WI-Z91              LINE 16 POSITION 23 LOW
           PERFORM I-LEE-HOJAVIDA  THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 16 POSITION 40 LOW SIZE 30
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              DISPLAY NOMBM          LINE 16 POSITION 40 LOW SIZE 30.
       F-MOSTRAR-CAB.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       CAPURA DE DATOS POR TECLADO                       ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-DATOS-CAB.
        CAB-1.
           IF X2  =  1
           PERFORM I-LUGAR       THRU F-LUGAR
                   GO CAB-13.
           IF      ESC             GO F-DATOS-CAB.
        CAB-2.
           IF X1  =  1
           PERFORM I-FINAL       THRU F-FINAL.
           IF      ESC OR CUP      GO F-DATOS-CAB.
        CAB-3.
           IF X1  =  1
           PERFORM I-VALOR       THRU  F-VALOR .
           IF      CUP             GO CAB-2.
           IF      ESC             GO F-DATOS-CAB.
        CAB-4.
           IF X1  =  1
           PERFORM I-EGRESO      THRU  F-EGRESO.
           IF      CUP             GO CAB-3.
           IF      ESC             GO F-DATOS-CAB.
        CAB-5.
           IF X1  =  1
           PERFORM I-EMPRESA     THRU F-EMPRESA.
           IF      CUP             GO CAB-4.
           IF      ESC             GO F-DATOS-CAB.
        CAB-6.
           IF X1  =  1
           PERFORM I-GIRADO      THRU F-GIRADO.
           IF      CUP             GO CAB-5.
           IF      ESC             GO F-DATOS-CAB.
        CAB-7.
           PERFORM I-ESTADO      THRU F-ESTADO.
           IF      CUP             GO CAB-6.
           IF      ESC             GO F-DATOS-CAB.
        CAB-8.
           IF X1  =  1
           PERFORM I-DPTO        THRU F-DPTO.
           IF      CUP             GO CAB-7.
           IF      ESC             GO F-DATOS-CAB.
        CAB-9.
           PERFORM I-FEC-ENTREGA THRU F-FEC-ENTREGA.
           IF      CUP             GO CAB-8.
           IF      ESC             GO F-DATOS-CAB.
        CAB-10.
           IF X1  =  1
           PERFORM I-FEC-ELABORO THRU F-FEC-ELABORO.
           IF      CUP             GO CAB-9.
           IF      ESC             GO F-DATOS-CAB.
        CAB-11.
           PERFORM I-ENTREGO     THRU F-ENTREGO.
           IF      CUP             GO CAB-10.
           IF      ESC             GO F-DATOS-CAB.
        CAB-12.
           PERFORM I-RECIBIO     THRU F-RECIBIO.
           IF      CUP             GO CAB-11.
           IF      ESC             GO F-DATOS-CAB.
        CAB-13.
           MOVE    ZEROS       TO   WK-EXCEPTION
           MOVE    SPACES      TO   W-TRUCO
           DISPLAY " PRESIONE :       <G> GRABAR        <ESC> CANCELAR
      -    "       <> REGRESAR         " LINE 24 POSITION 01 LOW
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESP                  LINE 25 POSITION 1
           IF  ESC                GO F-DATOS-CAB.
           IF  CUP                GO CAB-12.
           IF  F2                 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO NOT = "G"  GO TO CAB-13.
       F-DATOS-CAB.
      *
       I-DESDE-C.
           MOVE    ZEROS                 TO  WK-EXCEPTION
           DISPLAY TAB-CHEQUE          LINE  6 POSITION 10.
           ACCEPT  TAB-CHEQUE          LINE  6 POSITION 10 REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY TAB-CHEQUE          LINE  6 POSITION 10 LOW.
           IF  ESC GO F-DESDE-C.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-DESDE-C.
      *
       I-NOMBRE-C.
           MOVE    ZEROS               TO  WK-EXCEPTION
           DISPLAY W-NOMBRE          LINE  6 POSITION 49.
           ACCEPT  W-NOMBRE          LINE  6 POSITION 49 REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY W-NOMBRE          LINE  6 POSITION 49 LOW.
           IF  ESC GO F-NOMBRE-C.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-NOMBRE-C.
      *
       I-CHEQUE.
           MOVE ZEROS                    TO  WK-EXCEPTION
           MOVE ZEROS                    TO  UU
           DISPLAY TAB-CHEQUE          LINE  9 POSITION 23.
           ACCEPT  TAB-CHEQUE          LINE  9 POSITION 23 REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY TAB-CHEQUE          LINE  9 POSITION 23 LOW.
           IF CEND
              PERFORM I-LABEL-AUDICAMP      THRU F-LABEL-AUDICAMP
              MOVE  01                        TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                      TO SW-1
              PERFORM I-MUESTRA-AUDITORIA   THRU F-MUESTRA-AUDITORIA
                                                 UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS        THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB         THRU F-MOSTRAR-CAB
              GO TO I-CHEQUE.
           IF  ESC GO F-CHEQUE.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES LINE 25 POSITION 01 SIZE 80.
           MOVE   TAB-CHEQUE              TO CHEQUE-DESDE-CHCONTRO.

      *        NAYI

           IF MM = 0   OR W-OPCION = "A"
              PERFORM I-START-CHCONTRO     THRU F-START-CHCONTRO.
           IF         W-OPCION = "A"         GO F-CHEQUE.
           IF  UU = 1  PERFORM I-NEXISTE   THRU F-NEXISTE
                       MOVE ZEROS            TO MM
                       GO I-CHEQUE.
           IF      CDN
              PERFORM I-LEE-CHCONTRO-SEC   THRU F-LEE-CHCONTRO-SEC
              MOVE    CHEQUE-DESDE-CHCONTRO  TO TAB-CHEQUE
              MOVE    CONSEC-CHCONTRO        TO W-CONTA
              MOVE    1                      TO MM
              PERFORM I-WINDOW-DATOS       THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB        THRU F-MOSTRAR-CAB
              GO      I-CHEQUE.

           PERFORM I-LEE-CHCONTRO-1     THRU F-LEE-CHCONTRO-1.
           IF SW-2 = 1  MOVE 0 TO MM
                        GO  I-CHEQUE.
       F-CHEQUE.
      *
       I-CHEQUEO.
              DISPLAY "CHEQUE YA EXISTE ... <F8> PARA GRABAR "
                       LINE 25 POSITION 01 LOW BLINK
              PERFORM I-MOSTRAR-CAB      THRU F-MOSTRAR-CAB.
              MOVE    VALOR-CHCONTRO       TO  WI-Z99
              DISPLAY WI-Z99             LINE 25 POSITION 45 LOW
              DISPLAY ESTADO-CHCONTRO    LINE 25 POSITION 64 LOW
              PERFORM ACEPTA-TRUCO
              PERFORM I-WINDOW-DATOS     THRU F-WINDOW-DATOS
              IF F8   MOVE ZEROS   TO UU.
       F-CHEQUEO.
      *
       I-OPCION-MODIFICA.
              PERFORM I-AYUDA-MODIFICA  THRU F-AYUDA-MODIFICA.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO X1 X2.
              IF F3   MOVE 1  TO X1 GO F-OPCION-MODIFICA.
              IF F5   MOVE 2  TO X1 GO F-OPCION-MODIFICA.
              IF F4  AND MOD-USR(2) = "N"  MOVE 1  TO X2
                      GO F-OPCION-MODIFICA.
              IF   ESC           GO F-OPCION-MODIFICA.
              IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO      I-OPCION-MODIFICA.
       F-OPCION-MODIFICA.
      *
       I-OPCION-CONSULTA.
              MOVE    ZEROS                 TO  WK-EXCEPTION ANT1
              PERFORM I-AYUDA-CONSULTA  THRU F-AYUDA-CONSULTA.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO LL.
              IF W-TRUCO = "C"   MOVE 3  TO LL GO F-OPCION-CONSULTA.
              IF W-TRUCO = "E"   MOVE 4  TO LL GO F-OPCION-CONSULTA.
              IF W-TRUCO = "A"   MOVE 5  TO LL GO F-OPCION-CONSULTA.
              IF W-TRUCO = "T"   MOVE 6  TO LL GO F-OPCION-CONSULTA.
              IF   ESC           GO F-OPCION-CONSULTA.
              IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO      I-OPCION-CONSULTA.
       F-OPCION-CONSULTA.
      *
       I-OPCION-CONSULTA2.
              MOVE    ZEROS                 TO WK-EXCEPTION ANT2
              PERFORM I-AYUDA-CONSULTA2   THRU F-AYUDA-CONSULTA2.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO ANT2.
              IF W-TRUCO = "C"  AND MOD-USR(1) = "S"
                 MOVE 3  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "N"  MOVE 4  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "L"  MOVE 5  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "P"  MOVE 6  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "S"  MOVE 7  TO ANT2 GO F-OPCION-CONSULTA2.
              IF W-TRUCO = "T"  MOVE 8  TO ANT2 GO F-OPCION-CONSULTA2.
              IF   ESC           GO F-OPCION-CONSULTA2.
              IF   F2           PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO      I-OPCION-CONSULTA2.
       F-OPCION-CONSULTA2.
      *
       I-OPCION-TIPO.
              MOVE    ZEROS                 TO  WK-EXCEPTION ANT1
              DISPLAY "< C > POR No. CHEQUE        < N >  POR NOMBRE"
                      LINE 24 POSITION 20.
              PERFORM ACEPTA-TRUCO
              MOVE    ZEROS   TO ANT1.
              IF W-TRUCO = "C"  MOVE 1  TO ANT1 GO F-OPCION-TIPO.
              IF W-TRUCO = "N"  MOVE 2  TO ANT1 GO F-OPCION-TIPO.
              IF   ESC           GO F-OPCION-TIPO.
              IF F2   PERFORM I-FIN-MODE THRU F-FIN-MODE.
              GO      I-OPCION-TIPO.
       F-OPCION-TIPO.
      *
       I-FINAL.
           MOVE ZEROS                   TO  WK-EXCEPTION
           IF  CHEQUE-HASTA-CHCONTRO    =  ZEROS
               MOVE CHEQUE-DESDE-CHCONTRO TO CHEQUE-HASTA-CHCONTRO.
           DISPLAY CHEQUE-HASTA-CHCONTRO LINE  9 POSITION 57 LOW.
           ACCEPT  CHEQUE-HASTA-CHCONTRO LINE  9 POSITION 57
                   UPDATE PROMPT NO BEEP REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY CHEQUE-HASTA-CHCONTRO    LINE 9 POSITION 57 LOW.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  02                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-FINAL.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-FINAL.
           IF  CHEQUE-HASTA-CHCONTRO    =  ZEROS  GO I-FINAL.
       F-FINAL.
      *
       I-VALOR.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY VALOR-CHCONTRO LINE 10 POSITION 23 LOW
           ACCEPT  VALOR-CHCONTRO LINE 10 POSITION 23
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    VALOR-CHCONTRO      TO WI-Z99.
           DISPLAY WI-Z99             LINE 10 POSITION 23 LOW
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  06                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-VALOR.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-VALOR.
           IF VALOR-CHCONTRO = 0 GO I-VALOR.
       F-VALOR.
      *
       I-EGRESO.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY EGRESO-CHCONTRO LINE 10 POSITION 57.
           ACCEPT  EGRESO-CHCONTRO LINE 10 POSITION 57
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY EGRESO-CHCONTRO    LINE 10 POSITION 57 LOW.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  04                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-EGRESO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-EGRESO.
           IF EGRESO-CHCONTRO = 0 GO I-EGRESO.
       F-EGRESO.
      *
       I-EMPRESA.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY EMPRESA-CHCONTRO LINE 11 POSITION 23.
           ACCEPT  EMPRESA-CHCONTRO LINE 11 POSITION 23
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY EMPRESA-CHCONTRO    LINE 11 POSITION 23 LOW.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  05                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-EMPRESA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-EMPRESA.
           MOVE EMPRESA-CHCONTRO     TO WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS
           IF SW-EOF = 1 GO I-EMPRESA.
           DISPLAY WIN-EMPRESA    LINE 11 POSITION 30 SIZE 30 LOW.
       F-EMPRESA.
      *
       I-GIRADO.
           PERFORM I-COLOR1         THRU  F-COLOR1.
           MOVE ZEROS                 TO  WK-EXCEPTION
           DISPLAY GIRADO-CHCONTRO LINE 12 POSITION 23.
           ACCEPT  GIRADO-CHCONTRO LINE 12 POSITION 23
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    GIRADO-CHCONTRO    TO WI-Z91
           DISPLAY WI-Z91               LINE 12 POSITION 23 LOW.
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT1   THRU F-AYUDA-NIT1
               OPEN I-O CNNITCED
               GO I-GIRADO.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  03                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-GIRADO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-GIRADO.
           PERFORM I-MOSTRAR      THRU F-MOSTRAR.
           IF  CUP GO F-GIRADO.
           IF GIRADO-CHCONTRO = ZEROS GO I-GIRADO.
           MOVE GIRADO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
           PERFORM I-LEE-HOJAVIDA THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 12 POSITION 40 LOW SIZE 30
              MOVE    HOJAVIDA-NOMBRE     TO NOMBRE-CHCONTRO
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              MOVE    NOMBM            TO NOMBRE-CHCONTRO
              DISPLAY NOMBM          LINE 12 POSITION 40 LOW SIZE 30.
           IF SW-EOF = 1 GO I-GIRADO.
       F-GIRADO.
      *
       I-ESTADO.
           PERFORM I-AYUDA-ESTADO  THRU F-AYUDA-ESTADO.
           IF X1 = 2
              PERFORM I-COLOR2         THRU  F-COLOR2
           ELSE
              PERFORM I-COLOR1         THRU  F-COLOR1.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY ESTADO-CHCONTRO LINE 13 POSITION 23.
           ACCEPT  ESTADO-CHCONTRO LINE 13 POSITION 23
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY ESTADO-CHCONTRO    LINE 13 POSITION 23 LOW
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  07                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-ESTADO.
           IF  F2  PERFORM I-FIN-MODE   THRU F-FIN-MODE.
           IF  CUP GO F-ESTADO.
           IF  CUP  OR ESC GO F-ESTADO.
           IF ESTADO-CHCONTRO =  0  OR
              ESTADO-CHCONTRO =  6  OR
              ESTADO-CHCONTRO =  7  OR
              ESTADO-CHCONTRO =  9
              NEXT SENTENCE
         ELSE
              GO I-ESTADO.
           IF ESTADO-CHCONTRO  = "0"  MOVE "EN CAJA"      TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "6"  MOVE "ENVIADO EMP." TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "7"  MOVE "ENTREGADO"    TO TAB-ESTADO.
           IF ESTADO-CHCONTRO  = "9"  MOVE "ANULADO"      TO TAB-ESTADO.
           DISPLAY   TAB-ESTADO       LINE 13  POSITION 25 LOW.
       F-ESTADO.
      *
       I-DPTO.
           PERFORM I-AYUDA-DPTO  THRU F-AYUDA-DPTO.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY DPTO-CHCONTRO LINE 13 POSITION 57.
           ACCEPT  DPTO-CHCONTRO LINE 13 POSITION 57
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY DPTO-CHCONTRO    LINE 13 POSITION 57 LOW
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  08                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-DPTO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-DPTO.
           IF  CUP GO F-DPTO.
           IF DPTO-CHCONTRO = "N" OR
              DPTO-CHCONTRO = "T" OR
              DPTO-CHCONTRO = "S" OR
              DPTO-CHCONTRO = "L" OR
              DPTO-CHCONTRO = "P"
              NEXT SENTENCE
         ELSE
              GO I-DPTO.
           IF DPTO-CHCONTRO  = "N"  MOVE "NOMINAS"      TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "T"  MOVE "CONTABILIDAD" TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "S"  MOVE "OTROS PAGOS"  TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "L"  MOVE "LIQUIDACION"  TO TAB-DPTO.
           IF DPTO-CHCONTRO  = "P"  MOVE "SUBSIDIO"     TO TAB-DPTO.
           DISPLAY TAB-DPTO  LINE 13 POSITION 59 LOW.
       F-DPTO.
      *
       I-LUGAR.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY LUGAR-CHCONTRO  LINE 07 POSITION 55.
           ACCEPT  LUGAR-CHCONTRO  LINE 07 POSITION 55
                   REVERSE UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY LUGAR-CHCONTRO   LINE 07 POSITION 55 LOW
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  14                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-LUGAR.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-LUGAR.
           IF  CUP GO F-LUGAR.
           IF LUGAR-CHCONTRO = "B" OR
              LUGAR-CHCONTRO = " "
              NEXT SENTENCE
         ELSE
              GO I-LUGAR.
       F-LUGAR.
      *
       I-FEC-ENTREGA.
           IF X1 = 2
              PERFORM I-COLOR2         THRU  F-COLOR2
           ELSE
              PERFORM I-COLOR1         THRU  F-COLOR1.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY FEC-ENTREGA-CHCONTRO LINE 14 POSITION 23.
           ACCEPT  FEC-ENTREGA-CHCONTRO LINE 14 POSITION 23
                   REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF F10
              MOVE WK-FECHA-DESDE       TO FEC-ENTREGA-CHCONTRO.
           DISPLAY FEC-ENTREGA-CHCONTRO    LINE 14 POSITION 23 LOW
           IF CEND
              PERFORM I-LABEL-AUDICAMP    THRU F-LABEL-AUDICAMP
              MOVE  10                      TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                    TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                         UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS      THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB       THRU F-MOSTRAR-CAB
              GO TO I-FEC-ENTREGA.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-FEC-ENTREGA.
           IF  CUP  OR ESC GO F-FEC-ENTREGA.
           IF FEC-ENTREGA-CHCONTRO = ZEROS OR
              FEC-ENTREGA-CHCONTRO = SPACES  GO F-FEC-ENTREGA.
           IF MES-ENT-CHCONTRO <  1  OR MES-ENT-CHCONTRO >  12
              GO I-FEC-ENTREGA.
           IF DIA-ENT-CHCONTRO <  1  OR DIA-ENT-CHCONTRO >  31
              GO I-FEC-ENTREGA.
           MOVE FEC-ENTREGA-CHCONTRO  TO WK-FECHA-DESDE.
       F-FEC-ENTREGA.
    *
       I-FEC-ELABORO.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY FEC-ELABORO-CHCONTRO LINE 14 POSITION 57.
           ACCEPT  FEC-ELABORO-CHCONTRO LINE 14 POSITION 57
                   REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY FEC-ELABORO-CHCONTRO    LINE 14 POSITION 57 LOW.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  09                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-FEC-ELABORO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP  OR ESC GO F-FEC-ELABORO.
           IF  CUP GO F-FEC-ELABORO.
           IF FEC-ELABORO-CHCONTRO = ZEROS GO I-FEC-ELABORO.
           IF MES-ELA-CHCONTRO <  1  OR MES-ELA-CHCONTRO >  12
              GO I-FEC-ELABORO.
           IF DIA-ELA-CHCONTRO <  1  OR DIA-ELA-CHCONTRO >  31
              GO I-FEC-ELABORO.
       F-FEC-ELABORO.
      *
       I-ENTREGO.
           IF X1 = 2
              PERFORM I-COLOR2         THRU F-COLOR2
           ELSE
              PERFORM I-COLOR1         THRU  F-COLOR1.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY ENTREGO-CHCONTRO LINE 15 POSITION 23.
           ACCEPT  ENTREGO-CHCONTRO LINE 15 POSITION 23
                   REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF F10
              MOVE W-NIT                TO ENTREGO-CHCONTRO.
           MOVE    ENTREGO-CHCONTRO    TO WI-Z91
           DISPLAY WI-Z91               LINE 15 POSITION 23 LOW.
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT1   THRU F-AYUDA-NIT1
               OPEN I-O CNNITCED
               GO I-ENTREGO.
           IF CEND
              PERFORM I-LABEL-AUDICAMP THRU F-LABEL-AUDICAMP
              MOVE  11                   TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                 TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS    THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB     THRU F-MOSTRAR-CAB
              GO TO I-ENTREGO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           PERFORM I-MOSTRAR      THRU F-MOSTRAR.
           IF  CUP GO F-ENTREGO.
           IF  CUP  OR ESC GO F-ENTREGO.

           MOVE ENTREGO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
           PERFORM I-LEE-HOJAVIDA  THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 15 POSITION 40 LOW SIZE 30
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              DISPLAY NOMBM          LINE 15 POSITION 40 LOW SIZE 30.
           IF SW-EOF = 1 GO I-ENTREGO.
           MOVE      ENTREGO-CHCONTRO  TO W-NIT.
       F-ENTREGO.
      *
       I-RECIBIO.
           IF X1 = 2
              PERFORM I-COLOR2         THRU  F-COLOR2
           ELSE
              PERFORM I-COLOR1         THRU  F-COLOR1.
           MOVE ZEROS                    TO  WK-EXCEPTION
           DISPLAY RECIBIO-CHCONTRO LINE 16 POSITION 23.
           ACCEPT  RECIBIO-CHCONTRO LINE 16 POSITION 23
                   REVERSE
                   UPDATE PROMPT NO BEEP
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF F10
              MOVE WK-CEDULA-INI        TO RECIBIO-CHCONTRO.
           MOVE    RECIBIO-CHCONTRO    TO WI-Z91
           DISPLAY WI-Z91               LINE 16 POSITION 23 LOW.
           IF  F1
               CLOSE CNNITCED
               PERFORM I-AYUDA-NIT1   THRU F-AYUDA-NIT1
               OPEN I-O CNNITCED
               GO I-RECIBIO.
           IF CEND
              PERFORM I-LABEL-AUDICAMP    THRU F-LABEL-AUDICAMP
              MOVE  12                      TO LAB-AUDICAMP-ORDCAM
              MOVE ZEROS                    TO SW-1
              PERFORM I-MUESTRA-AUDITORIA THRU F-MUESTRA-AUDITORIA
                                          UNTIL ESC OR SW-1 = 1
              PERFORM I-WINDOW-DATOS       THRU F-WINDOW-DATOS
              PERFORM I-MOSTRAR-CAB        THRU F-MOSTRAR-CAB
              GO TO I-RECIBIO.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           PERFORM I-MOSTRAR       THRU F-MOSTRAR.
           IF  CUP  OR ESC GO F-RECIBIO.
           IF  CUP GO F-RECIBIO.
           MOVE RECIBIO-CHCONTRO     TO HOJAVIDA-CEDULA CODIM
           PERFORM I-LEE-HOJAVIDA  THRU F-LEE-HOJAVIDA
           IF  SW-EOF = 0
              DISPLAY HOJAVIDA-NOMBRE   LINE 16 POSITION 40 LOW SIZE 30
           ELSE
              PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
              IF SW-EOF = 0
              DISPLAY NOMBM          LINE 16 POSITION 40 LOW SIZE 30.
           IF SW-EOF = 1 GO I-RECIBIO.
           MOVE      RECIBIO-CHCONTRO  TO WK-CEDULA-INI.
       F-RECIBIO.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €           I-DATOS-LABEL ENTRADA     ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-DATOS-LABEL.
           IF      CUP               GO  F-DATOS-LABEL.
         LABEL-1.
           PERFORM I-BANCOS        THRU  F-BANCOS.
                   IF               ESC  GO F-DATOS-LABEL.
         LABEL-2.
           PERFORM I-CONFIRMA-COMP THRU  F-CONFIRMA-COMP.
           IF CUP  GO LABEL-1.
       F-DATOS-LABEL.
      *
       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 45
                                                    REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESP                  LINE 25 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 12
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 25 POSITION 1
                                                    REVERSE BLINK
                                                    GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 48
                                                    REVERSE.
               DISPLAY WK-ESP                  LINE 25 POSITION 1.
       F-ACEPTA-MES.
      *
       I-BANCOS.
           MOVE       ZEROS             TO   WK-EXCEPTION.
           MOVE W-BANCO1 TO WI-Z12C.
           DISPLAY WI-Z2                LINE 09 POSITION 42
           ACCEPT W-BANCO1              LINE 09 POSITION 42
                   UPDATE PROMPT NO BEEP HIGH REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP  GO F-BANCOS.
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  F1  PERFORM
                  I-HELP-BANCOS  THRU F-HELP-BANCOS.
           IF  W-BANCO1    = ZEROS
               DISPLAY "Codigo del Banco no puede ser Ceros"
               LINE 25 POSITION 01 REVERSE BEEP
               GO I-BANCOS.
           MOVE W-BANCO1         TO WI-Z2   BANC-CLAVE.
           PERFORM I-LEE-BANCOS THRU F-LEE-BANCOS.
           IF SW-EOF = 1
              DISPLAY "BANCO - INEXISTENTE  -  INTENTE NUEVAMENTE :"
              LINE 25 POSITION    01  REVERSE
              GO  I-BANCOS.
           DISPLAY BANC-NOMBRE  LINE 09 POSITION 48 SIZE 15 REVERSE.
           DISPLAY SPACES LINE 25 POSITION 01 SIZE 80.
       F-BANCOS.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       GRABACION Y REGRABACION                           ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-GRABACION.
           MOVE     10                  TO  W-COMP.
           PERFORM I-BUSCA-CONTREXT   THRU  F-BUSCA-CONTREXT.
           MOVE    WK-CONSEC            TO  CONSEC-CHCONTRO.
           DISPLAY "GRABANDO... ESPERE UN MOMENTO POR FAVOR"
                LINE 25 POSITION 1 REVERSE
            MOVE    WK-ANO              TO  ANO-GRA-CHCONTRO
            MOVE    WK-MES              TO  MES-GRA-CHCONTRO
            MOVE    WK-DIA              TO  DIA-GRA-CHCONTRO.
            MOVE    CON-USERNAME        TO  USUARIO-CHCONTRO.
            WRITE   REG-CHCONTRO.
       F-GRABACION.
      *
       I-REGRABACION.
           DISPLAY "MODIFICANDO... ESPERE UN MOMENTO POR FAVOR"
                LINE 25 POSITION 1 REVERSE
           MOVE      "M"                   TO AUDICAMP-OPCION.
           PERFORM   I-AUDITORIA-CAMPOS  THRU F-AUDITORIA-CAMPOS.
           REWRITE   REG-CHCONTRO.
       F-REGRABACION.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       RUTINA MANIPULACION DE ARCHIVOS                   ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-LEE-BANCOS.
           OPEN INPUT NOMIBANK.
           MOVE ZEROS TO SW-EOF.
           READ NOMIBANK WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
           CLOSE NOMIBANK.
       F-LEE-BANCOS.
      *
       I-LEE-CHCONTRO-1.
           MOVE ZEROS  TO SW-2.
           READ CHCONTRO WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-2.
       F-LEE-CHCONTRO-1.
      *
       I-LEE-CHCONTRO.
           IF   FIN-OK = 1 GO F-LEE-CHCONTRO.
           MOVE 0  TO   FIN-OK.
           READ CHCONTRO NEXT RECORD WITH NO LOCK AT END
               MOVE 1 TO FIN-OK
               GO F-LEE-CHCONTRO.
      *Controla el Dpto.
           IF MOD-USR(1) = "N" AND DPTO-CHCONTRO = "T"
                                   GO I-LEE-CHCONTRO.
           IF ANT2 = 8  GO I-EST.
           IF ANT2 = 3 AND DPTO-CHCONTRO =  "T" GO I-EST.
           IF ANT2 = 4 AND DPTO-CHCONTRO =  "N" GO I-EST.
           IF ANT2 = 5 AND DPTO-CHCONTRO =  "L" GO I-EST.
           IF ANT2 = 6 AND DPTO-CHCONTRO =  "S" GO I-EST.
           IF ANT2 = 7 AND DPTO-CHCONTRO =  "P" GO I-EST.
           GO I-LEE-CHCONTRO.
      *Controla el estado.
       I-EST.
           IF LL = 6  GO F-LEE-CHCONTRO.
           IF LL = 3 AND ESTADO-CHCONTRO =  0 GO F-LEE-CHCONTRO.
           IF LL = 4 AND ESTADO-CHCONTRO =  6 OR
              LL = 4 AND ESTADO-CHCONTRO =  7 GO F-LEE-CHCONTRO.
           IF LL = 5 AND ESTADO-CHCONTRO =  9 GO F-LEE-CHCONTRO.
           GO I-LEE-CHCONTRO.
       F-LEE-CHCONTRO.
      *
       I-LECTURA-SEC.
           IF  SW-EOF   =   1
                      GO  F-LECTURA-SEC.
           MOVE ZEROS         TO  SW-EOF.
           READ CHCONTRO NEXT  RECORD WITH NO LOCK AT END
                     MOVE 1 TO SW-EOF
                     GO F-LECTURA-SEC.
           IF CHEQUE-DESDE-CHCONTRO < EGRE-INICIAL  GO I-LECTURA-SEC.
           IF CHEQUE-DESDE-CHCONTRO > EGRE-FINAL    MOVE 1 TO SW-EOF
                                                    GO F-LECTURA-SEC.
           DELETE CHCONTRO.
           GO I-LECTURA-SEC.
       F-LECTURA-SEC.
      *
       I-LEE-CHCONTRO-SEC.
               IF  SW-1  =  1   AND W-OPCION = "M" OR
                   SW-1  =  1   AND W-OPCION = "R"
                   MOVE ZEROS    TO CHEQUE-DESDE-CHCONTRO
                   PERFORM I-START-ARCHIVO THRU F-START-ARCHIVO.
               MOVE 0  TO   SW-1.
               READ CHCONTRO NEXT RECORD WITH NO LOCK AT END
                    MOVE 1   TO  SW-1.
       F-LEE-CHCONTRO-SEC.
      *
       I-START-ARCHIVO.
           MOVE   ZEROS               TO SW-1.
           MOVE  TAB-CHEQUE           TO KEY02-CHCONTRO.
           START CHCONTRO  KEY NOT LESS  KEY02-CHCONTRO INVALID KEY
                     MOVE   1    TO SW-1.
       F-START-ARCHIVO.
      *
       I-START-ARCHIVO-N.
           MOVE   ZEROS             TO   SW-1.
           MOVE  W-NOMBRE           TO   KEY03-CHCONTRO.
           START CHCONTRO  KEY NOT LESS  KEY03-CHCONTRO INVALID KEY
                     MOVE   1   TO SW-1.
       F-START-ARCHIVO-N.
      *
       I-START-CHCONTRO.
           MOVE   ZEROS  TO  UU.
           MOVE  TAB-CHEQUE          TO  CHEQUE-DESDE-CHCONTRO.
           START CHCONTRO  KEY NOT LESS  KEY02-CHCONTRO INVALID KEY
                 MOVE    1    TO UU
                 GO F-START-CHCONTRO.
       I-STAR.
           READ CHCONTRO NEXT  RECORD WITH NO LOCK AT END
                     MOVE 1 TO UU.
           IF CHEQUE-DESDE-CHCONTRO = TAB-CHEQUE
              MOVE   CONSEC-CHCONTRO  TO W-CONTA
                                MOVE 2 TO UU
                                GO F-START-CHCONTRO.
           IF CHEQUE-DESDE-CHCONTRO > TAB-CHEQUE
                                MOVE 1 TO UU
                                GO F-START-CHCONTRO.
           GO I-STAR.
       F-START-CHCONTRO.
      *
       I-LEE-CNNITCED.
           MOVE 0 TO SW-EOF.
           READ CNNITCED WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED. EXIT.
      *
       I-LEE-HOJAVIDA.
           MOVE 0 TO SW-EOF.
           READ HOJAVIDA WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-HOJAVIDA. EXIT.
      *
       I-LEE-EMPRESAS.
           MOVE 0 TO SW-EOF.
           READ EMPRESAS WITH NO LOCK INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-EMPRESAS. EXIT.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥       PANTALLAS DEL PROGRAMA                            ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR        "
                     LINE 25 POSITION 1 REVERSE
           DISPLAY " [F2] TERMINAR " LINE 25 POSITION 32 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-CONFIRMA-COMP.
           IF  W-TRUCO                  NOT =  "C"
               GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESP  LINE 25 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-DISPLAY-OPCION.
           DISPLAY WK-ESP  LINE 25 POSITION  1.
           IF  W-OPCION = "A"
               DISPLAY "< ADICIONA >"
                                    LINE  3 POSITION 65 BLINK.
           IF  W-OPCION = "M"
               DISPLAY "< MODIFICA >"
                                    LINE  3 POSITION 65 BLINK.
           IF  W-OPCION = "C"
               DISPLAY "< CONSULTA >"
                                    LINE  3 POSITION 65 BLINK.
           IF  W-OPCION = "R"
               DISPLAY "< ELIMINAR >"
                                    LINE  3 POSITION 65 BLINK.
            DISPLAY "< <  CAPTURA CONTROL DE CHEQUES  > >"
                            LINE 2 POSITION  22.
       F-DISPLAY-OPCION.
      *
       I-MOSTRAR.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY "
      -    "                            " LINE 18 POSITION 01.
           DISPLAY "  .       .                  .                    .
      -    "                  .         " LINE 19 POSITION 01 BLINK.
           DISPLAY "                      .               .
      -    "      .                    ." LINE 20 POSITION 01.
           DISPLAY "
      -    "                            " LINE 21 POSITION 01.
           DISPLAY "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
      -    "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞" LINE 22 POSITION 01 LOW.
           DISPLAY "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
      -    "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞" LINE 23 POSITION 01 LOW.
           DISPLAY
           "PRESIONE:   <> Regresar    <>Siguiente   <ESC> Cancelar
      -    "  <F1>  Ayuda       "  LINE 24 POSITION 01.
           PERFORM I-COLOR1         THRU  F-COLOR1.
       F-MOSTRAR.
      *
       I-WINDOW-DATOS.
           MOVE "            MOVIMIENTO A MODIFICAR "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY
           "PRESIONE:   <> Regresar    <>Siguiente   <ESC> Cancelar
      -    "  <F1>  Ayuda       "  LINE 24 POSITION 01.
           DISPLAY "BANCO -->  "        LINE 03 POSITION 28 REVERSE.
           DISPLAY BANC-NOMBRE  LINE 03 POSITION 38 SIZE 15 REVERSE.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY
            "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -     "ÕÕÕÕÕÕÕÕÕª"        LINE 06 POSITION 07
            "∫  CONSECUTIVO:                      LUGAR:
      -     "         ∫"        LINE 07 POSITION 07
            "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -     "ÕÕÕÕÕÕÕÕÕπ"        LINE 08 POSITION 07
            "∫ CHEQUE DESDE:                     CHEQUE HASTA:
      -     "         ∫"        LINE 09 POSITION 07
            "∫        VALOR:                     COMP.EGRESO :
      -     "         ∫"        LINE 10 POSITION 07
            "∫      EMPRESA:
      -     "         ∫"        LINE 11 POSITION 07
            "∫ BENEFICIARIO:
      -     "         ∫"        LINE 12 POSITION 07
            "∫       ESTADO:                     PROCEDENCIA :
      -     "         ∫"        LINE 13 POSITION 07
            "∫FECHA ENTREGA:          <AAMMDD>   FECHA GIRADO:
      -     "         ∫"        LINE 14 POSITION 07
            "∫QUIEN ENTREGO:
      -     "         ∫"        LINE 15 POSITION 07
            "∫QUIEN RECIBIO:
      -     "         ∫"        LINE 16 POSITION 07
            "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕº"         LINE 17 POSITION 07.
           PERFORM I-DISPLAY-OPCION   THRU  F-DISPLAY-OPCION.
           PERFORM I-COLOR1           THRU  F-COLOR1.
       F-WINDOW-DATOS.
      *
       I-PANTALLA-1B.
           MOVE "     MOVIMIENTO A MODIFICAR "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY "< <  CAPTURA CONTROL DE CHEQUES  > >"
                          LINE 2 POSITION  22 REVERSE.
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 07 POSITION 20
               DISPLAY "∫                                           ∫"
                                               LINE 08 POSITION 20
               DISPLAY "∫ CODIGO DEL BANCO:                         ∫"
                                               LINE 09 POSITION 20
               DISPLAY "∫ << [F1]  AYUDA >>                         ∫"
                                               LINE 10 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 11 POSITION 20
               DISPLAY WK-ESP             LINE 25 POSITION 1.
       F-PANTALLA-1B.
      *
       I-PANTALLA-GEN.
           DISPLAY
           "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª" LINE 01 POSITION 01
           "∫
      -    "                   ∫" LINE 02 POSITION 01
           "∫
      -    "                   ∫" LINE 03 POSITION 01
           "∫
      -    "                   ∫" LINE 04 POSITION 01
           "ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ" LINE 05 POSITION 01
           "∫
      -    "                   ∫" LINE 06 POSITION 01
           "∫
      -    "                   ∫" LINE 07 POSITION 01
           "∫
      -    "                   ∫" LINE 08 POSITION 01
           "∫
      -    "                   ∫" LINE 09 POSITION 01
           "∫
      -    "                   ∫" LINE 10 POSITION 01
           "∫
      -    "                   ∫" LINE 11 POSITION 01
           "∫
      -    "                   ∫" LINE 12 POSITION 01
           "∫
      -    "                   ∫" LINE 13 POSITION 01
           "∫
      -    "                   ∫" LINE 14 POSITION 01
           "∫
      -    "                   ∫" LINE 15 POSITION 01
           "∫
      -    "                   ∫" LINE 16 POSITION 01
           "∫
      -    "                   ∫" LINE 17 POSITION 01
           "∫
      -    "                   ∫" LINE 18 POSITION 01
           "∫
      -    "                   ∫" LINE 19 POSITION 01
           "∫
      -    "                   ∫" LINE 20 POSITION 01
           "∫
      -    "                   ∫" LINE 21 POSITION 01
           "∫
      -    "                   ∫" LINE 22 POSITION 01
           "∫
      -    "                   ∫" LINE 23 POSITION 01
           "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -    "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº" LINE 24 POSITION 01.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02
           SPACES SIZE 78 LINE 02 POSITION 02
           "∞∞∞C O N T R O L   D E   C H E Q U E S∞∞∞"
                                LINE 01 POSITION 20 REVERSE
           "∞∞∞BANCO: ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
      -    "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞" LINE 02 POSITION 02 REVERSE
           W-LONG LINE 02 POSITION 15 REVERSE
           "  NRO.∞CHEQU∞CHEQU∞ FECHA∞ COMP.∞∞∞∞∞∞∞∞∞∞∞∞∞D∞       NOMBRE
      -    "      ∞EMP∞ FECHA∞" LINE 03 POSITION 02 REVERSE

           "CONSEC∞DESDE∞HASTA∞GIRADO∞EGRESO∞   VALOR   ∞P∞    BENEFICIA
      -    "RIO   ∞EMP∞ENTREGA" LINE 04 POSITION 02 REVERSE.
            MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-PANTALLA-GEN. EXIT.
      *
       I-PRIMER-PANTALLA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02
           SPACES SIZE 78 LINE 02 POSITION 02
           "∞∞∞C O N T R O L   D E   C H E Q U E S∞∞∞"
                                LINE 01 POSITION 20 REVERSE
           "∞∞∞BANCO: ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞
      -    "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞" LINE 02 POSITION 02 REVERSE
           W-LONG LINE 02 POSITION 15 REVERSE
           "  NRO.∞CHEQU∞CHEQU∞ FECHA∞ COMP.∞∞∞∞∞∞∞∞∞∞∞∞∞D∞       NOMBRE
      -    "      ∞EMP∞ FECHA∞" LINE 03 POSITION 02 REVERSE

           "CONSEC∞DESDE∞HASTA∞GIRADO∞EGRESO∞   VALOR   ∞P∞    BENEFICIA
      -    "RIO   ∞EMP∞ENTREGA" LINE 04 POSITION 02 REVERSE.
            MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           DISPLAY
           T-CONSEC      (SW-LECTURA) LINE CON-LIN POSITION 02
           T-DESDE       (SW-LECTURA) LINE CON-LIN POSITION 10
           T-VALOR       (SW-LECTURA)
                         LINE CON-LIN POSITION 35
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           T-HASTA       (SW-LECTURA) LINE CON-LIN POSITION 15 LOW
           T-FEC-ELABORO (SW-LECTURA) LINE CON-LIN POSITION 20 LOW
           T-EGRESO      (SW-LECTURA) LINE CON-LIN POSITION 27 LOW
           T-DPTO        (SW-LECTURA) LINE CON-LIN POSITION 47 LOW
           T-GIRADO      (SW-LECTURA) LINE CON-LIN POSITION 49 LOW
           T-EMPRESA     (SW-LECTURA) LINE CON-LIN POSITION 69 LOW
           T-FEC-ENTREGA (SW-LECTURA) LINE CON-LIN POSITION 73 LOW
           ADD 1 TO CON-LIN.
       F-PRIMER-PANTALLA. EXIT.
      *
       I-SEGUND-PANTALLA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           SPACES SIZE 78 LINE 04 POSITION 02
           SPACES SIZE 78 LINE 03 POSITION 02

           "NRO   ∞CHEQU∞      NOMBRE        ∞   PERSONA QUE      ∞
      -    "PERSONA QUE    ∞S∞" LINE 03 POSITION 02 REVERSE
           "CONSEC∞DESDE∞   DE LA EMPRESA    ∞ ENTREGO EL CHEQUE  ∞
      -    "  RECIBIO      ∞W∞" LINE 04 POSITION 02 REVERSE.

            MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR
            MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
            DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
            DISPLAY
            T-CONSEC        (SW-LECTURA) LINE CON-LIN POSITION 02
            T-DESDE         (SW-LECTURA) LINE CON-LIN POSITION 10

           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR
           DISPLAY " " LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING
           DISPLAY
           T-NOM-EMPRESA   (SW-LECTURA) LINE CON-LIN POSITION 15 LOW
           T-ENTREGO       (SW-LECTURA) LINE CON-LIN POSITION 36 LOW
           T-RECIBIO       (SW-LECTURA) LINE CON-LIN POSITION 56 LOW
           T-ESTADO        (SW-LECTURA) LINE CON-LIN POSITION 77 LOW
           T-LUGAR         (SW-LECTURA) LINE CON-LIN POSITION 79 LOW
           ADD 1 TO CON-LIN.
       F-SEGUND-PANTALLA. EXIT.
      *
       I-STATUS-CHCONTRO.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CHCONTRO
               MOVE 1                      TO  SW-ABRE
               IF  CHCONTRO-STATUS              =  "35"
                   OPEN OUTPUT CHCONTRO
               ELSE
                   IF  CHCONTRO-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CHCONTRO
                       PERFORM I-FIN-MODE      THRU F-FIN-MODE.
               CLOSE CHCONTRO.
       F-STATUS-CHCONTRO.
      *
       I-WIN-CTAS.

       F-WIN-CTAS.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01.
       F-NEXISTE.
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 25 POSITION 01 LOW.
       F-EXISTE.
      *
       I-AYUDA-ESTADO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "∫ <0> ≥ En caja (Sin Entregar).     ∫" LINE 18 POSITION 25
           "∫ <6> ≥ Enviado empresa en Mision   ∫" LINE 19 POSITION 25
           "∫ <7> ≥ Entregado al Beneficiario   ∫" LINE 20 POSITION 25
           "∫ <9> ≥ Anulado                     ∫" LINE 21 POSITION 25
           PERFORM I-COLOR1         THRU  F-COLOR1.
       F-AYUDA-ESTADO.
      *
       I-AYUDA-DPTO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING

           "∫ <N> ≥ Nominas                     ∫" LINE 18 POSITION 25
           "∫ <T> ≥ Contabilidad                ∫" LINE 19 POSITION 25
           "∫ <S> ≥ Pagos Adicionales nomina    ∫" LINE 20 POSITION 25
           "∫ <L> ≥ Liq.Prestaciones Sociales   ∫" LINE 21 POSITION 25
           "∫ <P> ≥ Subsidio Familiar           ∫" LINE 22 POSITION 25
           PERFORM I-COLOR1      THRU       F-COLOR1.
       F-AYUDA-DPTO.
      *
       I-COLOR1.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-COLOR1.
      *
       I-COLOR2.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-COLOR2.
      *
       I-AYUDA-MODIFICA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "…ÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                        LINE 16 POSITION 12
           "∫      ∫            OPCIONES DE MODIFICACION              ∫"
                        LINE 17 POSITION 12
           "ÃÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ"
                        LINE 18 POSITION 12
           "∫ <F3> ∫ Modifica todos los datos                         ∫"
                        LINE 19 POSITION 12
           "∫ <F4> ∫ Modifica Lugar                                   ∫"
                        LINE 20 POSITION 12
           "∫ <F5> ∫ Modifica los datos finales: fecha de entrega,    ∫"
                        LINE 21 POSITION 12
           "∫      ∫ quien entrego el cheque, quien lo recibio, sw.   ∫"
                        LINE 22 POSITION 12
           "»ÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                        LINE 23 POSITION 12.

           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-AYUDA-MODIFICA.
      *
       I-AYUDA-CONSULTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "…ÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                        LINE 16 POSITION 12
           "∫      ∫      ESTADO DE LOS CHEQUES                       ∫"
                        LINE 17 POSITION 12
           "ÃÕÕÕÕÕÕŒÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ"
                        LINE 18 POSITION 12
           "∫ < C >∫ Cheques que estan en caja                        ∫"
                        LINE 19 POSITION 12
           "∫ < E >∫ Cheques Entregados al Beneficiario o empresa.    ∫"
                        LINE 20 POSITION 12
           "∫ < A >∫ Cheques Anulados.                                ∫"
                        LINE 21 POSITION 12
           "∫ < T >∫ Todos los cheques.                               ∫"
                        LINE 22 POSITION 12
           "»ÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                        LINE 23 POSITION 12.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-AYUDA-CONSULTA.
      *
       I-AYUDA-CONSULTA2.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "…ÕÕÕÕÕÕÀÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                        LINE 16 POSITION 12
           "∫ < C >∫ Contabilidad.                                    ∫"
                        LINE 17 POSITION 12
           "∫ < N >∫ Nominas.                                         ∫"
                        LINE 18 POSITION 12
           "∫ < L >∫ Liquidaciones.                                   ∫"
                        LINE 19 POSITION 12
           "∫ < P >∫ Pagos adicionales de nomina.                     ∫"
                        LINE 20 POSITION 12
           "∫ < S >∫ Subsidio Familiar                                ∫"
                        LINE 21 POSITION 12
           "∫ < T >∫ Todos                                            ∫"
                        LINE 22 POSITION 12
           "»ÕÕÕÕÕÕ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                        LINE 23 POSITION 12.
           MOVE SG-COLOR-TABLE(1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(4) TO SG-BCOLOR.
           DISPLAY SPACE LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
       F-AYUDA-CONSULTA2.
      *

       I-LIMPIA-PANTALLA.
           DISPLAY SPACES SIZE 78 LINE I POSITION 02.
       F-LIMPIA-PANTALLA. EXIT.
      *
      ***************************************************************
      *                RUTINAS EXTERNAS DEL PROGRAMA                *
      ***************************************************************
               COPY "..\PRO\HELBANKO.PRO".
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\CONTREXT.PRO".
               COPY "..\PRO\AUDICHEQ.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\HELPNIT1.PRO".

               COPY "..\PLB\AUDICHEQ.PLB".
               COPY "..\PLB\CHCONTRO.PLB".
               COPY "..\STA\AUDICAMP.STA".

